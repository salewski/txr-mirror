/* Copyright 2009
 * Kaz Kylheku <kkylheku@gmail.com>
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
#include "lib.h"
#include "stream.h"
#include "hash.h"
#include "txr.h"
#include "gc.h"

#define PROT_STACK_SIZE         1024
#define HEAP_SIZE               16384
#define REACHABLE               0x100
#define FREE                    0x200

typedef struct heap {
  struct heap *next;
  obj_t block[HEAP_SIZE];
} heap_t;

int opt_gc_debug;
static obj_t **gc_stack_bottom;

static obj_t **prot_stack[PROT_STACK_SIZE];
static obj_t ***prot_stack_limit = prot_stack + PROT_STACK_SIZE;
static obj_t ***top = prot_stack;

static obj_t *free_list, **free_tail = &free_list;
static heap_t *heap_list;

int gc_enabled = 1;

obj_t *prot1(obj_t **loc)
{
  assert (top < prot_stack_limit);
  *top++ = loc;
  return nil; /* for use in macros */
}

void rel1(obj_t **loc)
{
  /* protect and release calls must nest. */
  if (*--top != loc)
    abort();
}

void protect(obj_t **first, ...)
{
  obj_t **next = first;
  va_list vl;
  va_start (vl, first);

  while (next) {
    prot1(next);
    next = va_arg(vl, obj_t **);
  }

  va_end (vl);
}

void release(obj_t **last, ...)
{
  obj_t **next = last;
  va_list vl;
  va_start (vl, last);

  while (next) {
    rel1(next);
    next = va_arg(vl, obj_t **);
  }

  va_end (vl);
}

static void more()
{
  heap_t *heap = (heap_t *) chk_malloc(sizeof *heap);
  obj_t *block = heap->block, *end = heap->block + HEAP_SIZE;

  assert (free_list == 0);

  while (block < end) {
    block->t.next = free_list;
    block->t.type = FREE;
    free_list = block++;
  }

  free_tail = &heap->block[0].t.next;

  heap->next = heap_list;
  heap_list = heap;
}

obj_t *make_obj(void)
{
  int try;

  if (opt_gc_debug)
    gc();

  for (try = 0; try < 3; try++) {
    if (free_list) {
      obj_t *ret = free_list;
      free_list = free_list->t.next;
      return ret;
    }

    free_tail = &free_list;

    switch (try) {
    case 0: gc(); break;
    case 1: more(); break;
    }
  }

  return 0;
}

static void finalize(obj_t *obj)
{
  switch (obj->t.type) {
  case CONS:
    return;
  case STR:
    if (!opt_gc_debug) {
      free(obj->st.str);
      obj->st.str = 0;
    }
    return;
  case CHR:
  case NUM:
  case LIT:
  case SYM:
  case FUN:
    return;
  case VEC:
    if (!opt_gc_debug) {
      free(obj->v.vec-2);
      obj->v.vec = 0;
    }
    return;
  case LCONS:
  case LSTR:
    return;
  case COBJ:
    if (obj->co.ops->destroy)
      obj->co.ops->destroy(obj);
    return;
  }

  assert (0 && "corrupt type field");
}

static void mark_obj(obj_t *obj)
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

  obj->t.type |= REACHABLE;

  switch (t) {
  case CONS:
    mark_obj(obj->c.car);
    mark_obj_tail(obj->c.cdr);
  case STR:
    mark_obj_tail(obj->st.len);
  case CHR:
  case NUM:
  case LIT:
    return;
  case SYM:
    mark_obj(obj->s.name);
    mark_obj_tail(obj->s.val);
  case FUN:
    mark_obj(obj->f.env);
    if (obj->f.functype == FINTERP)
      mark_obj_tail(obj->f.f.interp_fun);
    return;
  case VEC:
    {
      obj_t *alloc_size = obj->v.vec[-2];
      obj_t *fill_ptr = obj->v.vec[-1];
      long i, fp = c_num(fill_ptr);

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
    if (obj->co.ops->mark)
      obj->co.ops->mark(obj);
    mark_obj_tail(obj->co.cls);
  }

  assert (0 && "corrupt type field");
}

static int in_heap(obj_t *ptr)
{
  heap_t *heap;

  for (heap = heap_list; heap != 0; heap = heap->next) {
    if (ptr >= heap->block && ptr < heap->block + HEAP_SIZE)
      if (((char *) ptr - (char *) heap->block) % sizeof (obj_t) == 0)
        return 1;
  }

  return 0;
}

static void mark_mem_region(obj_t **low, obj_t **high)
{
  if (low > high) {
    obj_t **tmp = high;
    high = low;
    low = tmp;
  }

  while (low < high) {
    obj_t *maybe_obj = *low;
    if (in_heap(maybe_obj)) {
      type_t t = maybe_obj->t.type;
      if ((t & FREE) == 0)
        mark_obj(maybe_obj);
    }
    low++;
  }
}

static void mark(void)
{
  obj_t *gc_stack_top;
  obj_t ***rootloc;

  /*
   * First, scan the officially registered locations.
   */

  for (rootloc = prot_stack; rootloc != top; rootloc++) {
    if (*rootloc) /* stack may have nulls */
      mark_obj(**rootloc);
  }

  mark_mem_region(&gc_stack_top, gc_stack_bottom);
}

static void sweep(void)
{
  heap_t *heap;
  int dbg = opt_gc_debug;

  for (heap = heap_list; heap != 0; heap = heap->next) {
    obj_t *block, *end;
    for (block = heap->block, end = heap->block + HEAP_SIZE;
         block < end;
         block++)
    {
      if ((block->t.type & (REACHABLE | FREE)) == (REACHABLE | FREE))
        abort();

      if (block->t.type & REACHABLE) {
        block->t.type &= ~REACHABLE;
        continue;
      }

      if (block->t.type & FREE)
        continue;

      if (0 && dbg) {
        fwprintf(stderr, L"%ls: finalizing: ", progname);
        obj_print(block, std_error);
        putwc('\n', stderr);
      }
      finalize(block);
      block->t.type |= FREE;
      /* If debugging is turned on, we want to catch instances
         where a reachable object is wrongly freed. This is difficult
         to do if the object is recycled soon after.
         So when debugging is on, the free list is FIFO
         rather than LIFO, which increases our chances that the
         code which is still using the object will trip on
         the freed object before it is recycled. */
      if (dbg) {
        *free_tail = block;
        block->t.next = nil;
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
  if (gc_enabled) {
    jmp_buf jmp;
    setjmp(jmp);
    gc_enabled = 0;
    mark();
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

void gc_init(obj_t **stack_bottom)
{
  gc_stack_bottom = stack_bottom;
}

void gc_mark(obj_t *obj)
{
  mark_obj(obj);
}

int gc_is_reachable(obj_t *obj)
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
    obj_t *block, *end;
    for (block = heap->block, end = heap->block + HEAP_SIZE;
         block < end;
         block++)
    {
      block->t.type &= ~(FREE | REACHABLE);
    }
  }
}
