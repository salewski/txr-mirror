/* Copyright 2011
 * Kaz Kylheku <kaz@kylheku.com>
 * Vancouver, Canada
 * All rights reserved.
 *
 * BSD License:
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 *   1. Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *   2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in
 *      the documentation and/or other materials provided with the
 *      distribution.
 *   3. The name of the author may not be used to endorse or promote
 *      products derived from this software without specific prior
 *      written permission.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <assert.h>
#include <setjmp.h>
#include <dirent.h>
#include <wchar.h>
#include "config.h"
#ifdef HAVE_VALGRIND
#include <valgrind/memcheck.h>
#endif
#include "lib.h"
#include "stream.h"
#include "hash.h"
#include "txr.h"
#include "eval.h"
#include "gc.h"

#define PROT_STACK_SIZE         1024
#define HEAP_SIZE               16384

typedef struct heap {
  struct heap *next;
  obj_t block[HEAP_SIZE];
} heap_t;

typedef struct mach_context {
  jmp_buf buf;
} mach_context_t;

#define save_context(X) setjmp((X).buf)

int opt_gc_debug;
#ifdef HAVE_VALGRIND
int opt_vg_debug;
#endif
static val *gc_stack_bottom;

static val *prot_stack[PROT_STACK_SIZE];
static val **prot_stack_limit = prot_stack + PROT_STACK_SIZE;
static val **top = prot_stack;

static val free_list, *free_tail = &free_list;
static heap_t *heap_list;
static val heap_min_bound, heap_max_bound;

int gc_enabled = 1;

#if EXTRA_DEBUGGING
static val break_obj;
#endif

val prot1(val *loc)
{
  assert (top < prot_stack_limit);
  *top++ = loc;
  return nil; /* for use in macros */
}

void rel1(val *loc)
{
  /* protect and release calls must nest. */
  if (*--top != loc)
    abort();
}

void protect(val *first, ...)
{
  val *next = first;
  va_list vl;
  va_start (vl, first);

  while (next) {
    prot1(next);
    next = va_arg(vl, val *);
  }

  va_end (vl);
}

void release(val *last, ...)
{
  val *next = last;
  va_list vl;
  va_start (vl, last);

  while (next) {
    rel1(next);
    next = va_arg(vl, val *);
  }

  va_end (vl);
}

static void more(void)
{
  heap_t *heap = (heap_t *) chk_malloc(sizeof *heap);
  obj_t *block = heap->block, *end = heap->block + HEAP_SIZE;

  assert (free_list == 0);

  if (end > heap_max_bound)
    heap_max_bound = end;

  if (block < heap_min_bound)
    heap_min_bound = block;

  while (block < end) {
    block->t.next = free_list;
    block->t.type = (type_t) FREE;
    free_list = block++;
  }

  free_tail = &heap->block[0].t.next;

  heap->next = heap_list;
  heap_list = heap;

#ifdef HAVE_VALGRIND
  if (opt_vg_debug)
    VALGRIND_MAKE_MEM_NOACCESS(&heap->block, sizeof heap->block);
#endif
}

val make_obj(void)
{
  int tries;

  if (opt_gc_debug)
    gc();

  for (tries = 0; tries < 3; tries++) {
    if (free_list) {
      val ret = free_list;
#ifdef HAVE_VALGRIND
      if (opt_vg_debug)
        VALGRIND_MAKE_MEM_DEFINED(free_list, sizeof *free_list);
#endif
      free_list = free_list->t.next;
#ifdef HAVE_VALGRIND
      if (opt_vg_debug)
        VALGRIND_MAKE_MEM_UNDEFINED(ret, sizeof *ret);
#endif
      return ret;
    }

    free_tail = &free_list;

    switch (tries) {
    case 0: gc(); break;
    case 1: more(); break;
    }
  }

  return 0;
}

static void finalize(val obj)
{
  switch (obj->t.type) {
  case CONS:
    return;
  case STR:
    free(obj->st.str);
    obj->st.str = 0;
    return;
  case CHR:
  case NUM:
  case LIT:
  case SYM:
  case PKG:
  case FUN:
    return;
  case VEC:
    free(obj->v.vec-2);
    obj->v.vec = 0;
    return;
  case LCONS:
  case LSTR:
    return;
  case COBJ:
    obj->co.ops->destroy(obj);
    return;
  case ENV:
    return;
  case BGNUM:
    mp_clear(mp(obj));
    return;
  }

  assert (0 && "corrupt type field");
}

void cobj_destroy_stub_op(val obj)
{
}

void cobj_destroy_free_op(val obj)
{
  free(obj->co.handle);
}

static void mark_obj(val obj)
{
  type_t t;

#if 1
tail_call:
#define mark_obj_tail(o) do { obj = (o); goto tail_call; } while (0)
#else
#define mark_obj_tail(o) return mark_obj(o)
#endif

  if (!is_ptr(obj))
    return;

  t = obj->t.type;

  if ((t & REACHABLE) != 0)
    return;

  if ((t & FREE) != 0)
    abort();

  obj->t.type = (type_t) (obj->t.type | REACHABLE);

#if EXTRA_DEBUGGING
  if (obj == break_obj)
    breakpt();
#endif

  switch (t) {
  case CONS:
    mark_obj(obj->c.car);
    mark_obj_tail(obj->c.cdr);
  case STR:
    mark_obj(obj->st.len);
    mark_obj_tail(obj->st.alloc);
  case CHR:
  case NUM:
  case LIT:
  case BGNUM:
    return;
  case SYM:
    mark_obj(obj->s.name);
    mark_obj(obj->s.value);
    mark_obj_tail(obj->s.package);
  case PKG:
    mark_obj(obj->pk.name);
    mark_obj_tail(obj->pk.symhash);
  case FUN:
    mark_obj(obj->f.env);
    if (obj->f.functype == FINTERP)
      mark_obj_tail(obj->f.f.interp_fun);
    return;
  case VEC:
    {
      val alloc_size = obj->v.vec[-2];
      val fill_ptr = obj->v.vec[-1];
      cnum i, fp = c_num(fill_ptr);

      mark_obj(alloc_size);
      mark_obj(fill_ptr);

      for (i = 0; i < fp; i++)
        mark_obj(obj->v.vec[i]);
    }
    return;
  case LCONS:
    mark_obj(obj->lc.func);
    mark_obj(obj->lc.car);
    mark_obj_tail(obj->lc.cdr);
  case LSTR:
    mark_obj(obj->ls.prefix);
    mark_obj(obj->ls.opts);
    mark_obj_tail(obj->ls.list);
  case COBJ:
    obj->co.ops->mark(obj);
    mark_obj_tail(obj->co.cls);
  case ENV:
    mark_obj(obj->e.vbindings);
    mark_obj(obj->e.fbindings);
    mark_obj_tail(obj->e.up_env);
  }

  assert (0 && "corrupt type field");
}

void cobj_mark_op(val obj)
{
}

static int in_heap(val ptr)
{
  heap_t *heap;

  if (!is_ptr(ptr))
    return 0;

  if (ptr < heap_min_bound || ptr >= heap_max_bound)
    return 0;

  for (heap = heap_list; heap != 0; heap = heap->next) {
    if (ptr >= heap->block && ptr < heap->block + HEAP_SIZE)
      if (((char *) ptr - (char *) heap->block) % sizeof (obj_t) == 0)
        return 1;
  }

  return 0;
}

static void mark_mem_region(val *low, val *high)
{
  if (low > high) {
    val *tmp = high;
    high = low;
    low = tmp;
  }

  while (low < high) {
    val maybe_obj = *low;
#ifdef HAVE_VALGRIND
    VALGRIND_MAKE_MEM_DEFINED(&maybe_obj, sizeof maybe_obj);
#endif
    if (in_heap(maybe_obj)) {
#ifdef HAVE_VALGRIND
      if (opt_vg_debug)
        VALGRIND_MAKE_MEM_DEFINED(&maybe_obj->t.type, sizeof maybe_obj->t.type);
#endif
      type_t t = maybe_obj->t.type;
      if ((t & FREE) == 0) {
        mark_obj(maybe_obj);
      } else {
#ifdef HAVE_VALGRIND
        if (opt_vg_debug)
          VALGRIND_MAKE_MEM_NOACCESS(maybe_obj, sizeof *maybe_obj);
#endif
      }
    }
    low++;
  }
}

static void mark(mach_context_t *pmc, val *gc_stack_top)
{
  val **rootloc;

  /*
   * First, scan the officially registered locations.
   */
  for (rootloc = prot_stack; rootloc != top; rootloc++)
    mark_obj(**rootloc);

  /*
   * Then the machine context
   */
  mark_mem_region((val *) pmc, (val *) (pmc + 1));

  /*
   * Finally, the stack.
   */
  mark_mem_region(gc_stack_top, gc_stack_bottom);
}

static void sweep(void)
{
  heap_t *heap;
  int gc_dbg = opt_gc_debug;
#ifdef HAVE_VALGRIND
  int vg_dbg = opt_vg_debug;
#else
  int vg_dbg = 0;
#endif

  for (heap = heap_list; heap != 0; heap = heap->next) {
    obj_t *block, *end;

#ifdef HAVE_VALGRIND
    if (vg_dbg)
        VALGRIND_MAKE_MEM_DEFINED(&heap->block, sizeof heap->block);
#endif

    for (block = heap->block, end = heap->block + HEAP_SIZE;
         block < end;
         block++)
    {
      if ((block->t.type & (REACHABLE | FREE)) == (REACHABLE | FREE))
        abort();

      if (block->t.type & REACHABLE) {
        block->t.type = (type_t) (block->t.type & ~REACHABLE);
        continue;
      }

      if (block->t.type & FREE) {
#ifdef HAVE_VALGRIND
        if (vg_dbg)
            VALGRIND_MAKE_MEM_NOACCESS(block, sizeof *block);
#endif
        continue;
      }

      if (0 && gc_dbg) {
        format(std_error, lit("~a: finalizing: "), progname, nao);
        obj_print(block, std_error);
        put_char(chr('\n'), std_error);
      }
      finalize(block);
      block->t.type = (type_t) (block->t.type | FREE);
      /* If debugging is turned on, we want to catch instances
         where a reachable object is wrongly freed. This is difficult
         to do if the object is recycled soon after.
         So when debugging is on, the free list is FIFO
         rather than LIFO, which increases our chances that the
         code which is still using the object will trip on
         the freed object before it is recycled. */
      if (gc_dbg || vg_dbg) {
#ifdef HAVE_VALGRIND
        if (vg_dbg && free_tail != &free_list)
          VALGRIND_MAKE_MEM_DEFINED(free_tail, sizeof *free_tail);
#endif
        *free_tail = block;
        block->t.next = nil;
#ifdef HAVE_VALGRIND
        if (vg_dbg) {
          if (free_tail != &free_list)
            VALGRIND_MAKE_MEM_NOACCESS(free_tail, sizeof *free_tail);
          VALGRIND_MAKE_MEM_NOACCESS(block, sizeof *block);
        }
#endif
        free_tail = &block->t.next;
      } else {
        block->t.next = free_list;
        free_list = block;
      }
    }
  }
}

void gc(void)
{
  val gc_stack_top = nil;

  if (gc_enabled) {
    mach_context_t mc;
    save_context(mc);
    gc_enabled = 0;
    mark(&mc, &gc_stack_top);
    hash_process_weak();
    sweep();
    gc_enabled = 1;
  }
}

int gc_state(int enabled)
{
  int old = gc_enabled;
  gc_enabled = enabled;
  return old;
}

void gc_init(val *stack_bottom)
{
  gc_stack_bottom = stack_bottom;
}

void gc_mark(val obj)
{
  mark_obj(obj);
}

int gc_is_reachable(val obj)
{
  type_t t;

  if (!is_ptr(obj))
    return 1;

  t = obj->t.type;

  return (t & REACHABLE) != 0;
}

/*
 * Useful functions for gdb'ing.
 */
void unmark(void)
{
  heap_t *heap;

  for (heap = heap_list; heap != 0; heap = heap->next) {
    val block, end;
    for (block = heap->block, end = heap->block + HEAP_SIZE;
         block < end;
         block++)
    {
      block->t.type = (type_t) (block->t.type & ~(FREE | REACHABLE));
    }
  }
}

void dheap(heap_t *heap, int start, int end);

void dheap(heap_t *heap, int start, int end)
{
  int i;
  for (i = start; i < end; i++)
    format(std_output, lit("(~a ~s)\n"), num(i), &heap->block[i], nao);
}

/*
 * This function does nothing.
 * gc_hint(x) just takes the address of local variable x
 * and passes it to this function. This prevents the compiler
 * from caching the value across function calls.
 * This is needed for situations where
 * - a compiler caches a variable in a register, but not entirely (the variable
 *   has a backing memory location); and 
 * - that location contains a stale old value of the variable, which cannot be
 *   garbage-collected as a result; and
 * - this causes a problem, like unbounded memory growth.
 */
void gc_hint_func(val *val)
{
  (void) val;
}
