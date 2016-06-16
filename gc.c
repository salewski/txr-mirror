/* Copyright 2009-2016
 * Kaz Kylheku <kaz@kylheku.com>
 * Vancouver, Canada
 * All rights reserved.
 *
 * Redistribution of this software in source and binary forms, with or without
 * modification, is permitted provided that the following two conditions are met.
 *
 * Use of this software in any manner constitutes agreement with the disclaimer
 * which follows the two conditions.
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DAMAGES, HOWEVER CAUSED,
 * AND UNDER ANY THEORY OF LIABILITY, ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <assert.h>
#include <dirent.h>
#include <wchar.h>
#include <signal.h>
#include "config.h"
#if HAVE_VALGRIND
#include <valgrind/memcheck.h>
#endif
#include "lib.h"
#include "stream.h"
#include "hash.h"
#include "txr.h"
#include "eval.h"
#include "gc.h"
#include "signal.h"

#define PROT_STACK_SIZE         1024
#define HEAP_SIZE               16384
#define CHECKOBJ_VEC_SIZE       (8 * HEAP_SIZE)
#define MUTOBJ_VEC_SIZE         (HEAP_SIZE / 4)
#define FULL_GC_INTERVAL        40
#define FRESHOBJ_VEC_SIZE       (8 * HEAP_SIZE)
#define DFL_MALLOC_DELTA_THRESH (64L * 1024 * 1024)

typedef struct heap {
  struct heap *next;
  obj_t block[HEAP_SIZE];
} heap_t;

typedef struct mach_context {
  struct jmp buf;
} mach_context_t;

#define save_context(X) jmp_save(&(X).buf)

int opt_gc_debug;
#if HAVE_VALGRIND
int opt_vg_debug;
#endif
static val *gc_stack_bottom;

static val *prot_stack[PROT_STACK_SIZE];
static val **prot_stack_limit = prot_stack + PROT_STACK_SIZE;
val **gc_prot_top = prot_stack;

static val free_list, *free_tail = &free_list;
static heap_t *heap_list;
static val heap_min_bound, heap_max_bound;

alloc_bytes_t gc_bytes;
static alloc_bytes_t prev_malloc_bytes;
alloc_bytes_t opt_gc_delta = DFL_MALLOC_DELTA_THRESH;

int gc_enabled = 1;

static struct fin_reg {
  struct fin_reg *next;
  val obj;
  val fun;
  int reachable;
} *final_list, **final_tail = &final_list;

#if CONFIG_GEN_GC
static val checkobj[CHECKOBJ_VEC_SIZE];
static int checkobj_idx;
static val mutobj[MUTOBJ_VEC_SIZE];
static int mutobj_idx;
static val freshobj[FRESHOBJ_VEC_SIZE];
static int freshobj_idx;
int full_gc;
#endif

#if EXTRA_DEBUGGING
val break_obj;
#endif

val prot1(val *loc)
{
  assert (gc_prot_top < prot_stack_limit);
  assert (loc != 0);
  *gc_prot_top++ = loc;
  return nil; /* for use in macros */
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

static void more(void)
{
  heap_t *heap = coerce(heap_t *, chk_malloc_gc_more(sizeof *heap));
  obj_t *block = heap->block, *end = heap->block + HEAP_SIZE;

  if (free_list == 0)
    free_tail = &heap->block[0].t.next;

  if (end > heap_max_bound)
    heap_max_bound = end;

  if (block < heap_min_bound)
    heap_min_bound = block;

  while (block < end) {
    block->t.next = free_list;
    block->t.type = convert(type_t, FREE);
    free_list = block++;
  }

  heap->next = heap_list;
  heap_list = heap;

#if HAVE_VALGRIND
  if (opt_vg_debug)
    VALGRIND_MAKE_MEM_NOACCESS(&heap->block, sizeof heap->block);
#endif
}

val make_obj(void)
{
  int tries;
  alloc_bytes_t malloc_delta = malloc_bytes - prev_malloc_bytes;
  assert (!async_sig_enabled);

#if CONFIG_GEN_GC
  if ((opt_gc_debug || freshobj_idx >= FRESHOBJ_VEC_SIZE ||
       malloc_delta >= opt_gc_delta) &&
      gc_enabled)
  {
    gc();
  }

  if (freshobj_idx >= FRESHOBJ_VEC_SIZE)
    full_gc = 1;
#else
  if ((opt_gc_debug || malloc_delta >= opt_gc_delta) && gc_enabled) {
    gc();
  }
#endif

  for (tries = 0; tries < 3; tries++) {
    if (free_list) {
      val ret = free_list;
#if HAVE_VALGRIND
      if (opt_vg_debug)
        VALGRIND_MAKE_MEM_DEFINED(free_list, sizeof *free_list);
#endif
      free_list = free_list->t.next;

      if (free_list == 0)
        free_tail = &free_list;
#if HAVE_VALGRIND
      if (opt_vg_debug)
        VALGRIND_MAKE_MEM_UNDEFINED(ret, sizeof *ret);
#endif
#if CONFIG_GEN_GC
      ret->t.gen = 0;
      if (!full_gc)
        freshobj[freshobj_idx++] = ret;
#endif
      gc_bytes += sizeof (obj_t);
      return ret;
    }

#if CONFIG_GEN_GC
    if (!full_gc && freshobj_idx < FRESHOBJ_VEC_SIZE) {
      more();
      continue;
    }
#endif

    switch (tries) {
    case 0:
      if (gc_enabled) {
        gc();
        break;
      }
      /* fallthrough */
    case 1:
      more();
      break;
    }
  }

  abort();
}

static void finalize(val obj)
{
  switch (obj->t.type) {
  case NIL:
  case CONS:
  case CHR:
  case NUM:
  case LIT:
  case PKG:
  case FUN:
  case LCONS:
  case ENV:
  case FLNUM:
  case RNG:
    return;
  case SYM:
    free(obj->s.slot_cache);
    obj->s.slot_cache = 0;
    return;
  case STR:
    free(obj->st.str);
    obj->st.str = 0;
    return;
  case LSTR:
    free(obj->ls.props);
    obj->ls.props = 0;
    return;
  case VEC:
    free(obj->v.vec-2);
    obj->v.vec = 0;
    return;
  case COBJ:
    obj->co.ops->destroy(obj);
    obj->co.handle = 0;
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

#if CONFIG_GEN_GC
  if (!full_gc && obj->t.gen > 0)
    return;
#endif

  if ((t & FREE) != 0)
    abort();

#if CONFIG_GEN_GC
  if (obj->t.gen == -1)
    obj->t.gen = 0;  /* Will be promoted to generation 1 by sweep_one */
#endif

  obj->t.type = convert(type_t, t | REACHABLE);

#if EXTRA_DEBUGGING
  if (obj == break_obj)
    breakpt();
#endif

  switch (t) {
  case NIL:
  case CHR:
  case NUM:
  case LIT:
  case BGNUM:
  case FLNUM:
    return;
  case CONS:
    mark_obj(obj->c.car);
    mark_obj_tail(obj->c.cdr);
  case STR:
    mark_obj(obj->st.len);
    mark_obj_tail(obj->st.alloc);
  case SYM:
    mark_obj(obj->s.name);
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
    mark_obj(obj->ls.props->limit);
    mark_obj(obj->ls.props->term);
    mark_obj_tail(obj->ls.list);
  case COBJ:
    obj->co.ops->mark(obj);
    mark_obj_tail(obj->co.cls);
  case ENV:
    mark_obj(obj->e.vbindings);
    mark_obj(obj->e.fbindings);
    mark_obj_tail(obj->e.up_env);
  case RNG:
    mark_obj(obj->rn.from);
    mark_obj_tail(obj->rn.to);
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
      if ((coerce(char *, ptr) - coerce(char *, heap->block)) % sizeof (obj_t) == 0)
        return 1;
  }

  return 0;
}

static void mark_obj_maybe(val maybe_obj)
{
#if HAVE_VALGRIND
  VALGRIND_MAKE_MEM_DEFINED(&maybe_obj, sizeof maybe_obj);
#endif
  if (in_heap(maybe_obj)) {
#if HAVE_VALGRIND
    if (opt_vg_debug)
      VALGRIND_MAKE_MEM_DEFINED(maybe_obj, SIZEOF_PTR);
#endif
    type_t t = maybe_obj->t.type;
    if ((t & FREE) == 0) {
      mark_obj(maybe_obj);
    } else {
#if HAVE_VALGRIND
      if (opt_vg_debug)
        VALGRIND_MAKE_MEM_NOACCESS(maybe_obj, sizeof *maybe_obj);
#endif
    }
  }
}

static void mark_mem_region(val *low, val *high)
{
  if (low > high) {
    val *tmp = high;
    high = low;
    low = tmp;
  }

  for (; low < high; low++)
    mark_obj_maybe(*low);
}

static void mark(mach_context_t *pmc, val *gc_stack_top)
{
  val **rootloc;

  /*
   * First, scan the officially registered locations.
   */
  for (rootloc = prot_stack; rootloc != gc_prot_top; rootloc++)
    mark_obj(**rootloc);

#if CONFIG_GEN_GC
  /*
   * Mark the additional objects indicated for marking.
   */
  if (!full_gc)
  {
    int i;
    for (i = 0; i < checkobj_idx; i++)
      mark_obj(checkobj[i]);
    for (i = 0; i < mutobj_idx; i++)
      mark_obj(mutobj[i]);
  }
#endif

  /*
   * Then the machine context
   */
  mark_mem_region(coerce(val *, pmc), coerce(val *, (pmc + 1)));

  /*
   * Finally, the stack.
   */
  mark_mem_region(gc_stack_top, gc_stack_bottom);
}

static int sweep_one(obj_t *block)
{
#if HAVE_VALGRIND
  const int vg_dbg = opt_vg_debug;
#else
  const int vg_dbg = 0;
#endif

#if EXTRA_DEBUGGING
  if (block == break_obj)
    breakpt();
#endif

#if CONFIG_GEN_GC
  if (!full_gc && block->t.gen > 0)
    abort();
#endif

  if ((block->t.type & (REACHABLE | FREE)) == (REACHABLE | FREE))
    abort();

  if (block->t.type & REACHABLE) {
#if CONFIG_GEN_GC
    block->t.gen = 1;
#endif
    block->t.type = convert(type_t, block->t.type & ~REACHABLE);
    return 0;
  }

  if (block->t.type & FREE) {
#if HAVE_VALGRIND
    if (vg_dbg)
      VALGRIND_MAKE_MEM_NOACCESS(block, sizeof *block);
#endif
    return 1;
  }

  finalize(block);
  block->t.type = convert(type_t, block->t.type | FREE);

  /* If debugging is turned on, we want to catch instances
     where a reachable object is wrongly freed. This is difficult
     to do if the object is recycled soon after.
     So when debugging is on, the free list is FIFO
     rather than LIFO, which increases our chances that the
     code which is still using the object will trip on
     the freed object before it is recycled. */
  if (vg_dbg || opt_gc_debug) {
#if HAVE_VALGRIND
    if (vg_dbg && free_tail != &free_list)
      VALGRIND_MAKE_MEM_DEFINED(free_tail, sizeof *free_tail);
#endif
    *free_tail = block;
    block->t.next = nil;
#if HAVE_VALGRIND
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

  return 1;
}

static int_ptr_t sweep(void)
{
  int_ptr_t free_count = 0;
  heap_t *heap;
#if HAVE_VALGRIND
  const int vg_dbg = opt_vg_debug;
#endif

#if CONFIG_GEN_GC
  if (!full_gc) {
    int i;

    /* No need to mark block defined via Valgrind API; everything
       in the freshobj is an allocated node! */
    for (i = 0; i < freshobj_idx; i++)
      free_count += sweep_one(freshobj[i]);

    /* Generation 1 objects that were indicated for dangerous
       mutation must have their REACHABLE flag flipped off,
       and must be returned to gen 1. */
    for (i = 0; i < mutobj_idx; i++)
      sweep_one(mutobj[i]);

    return free_count;
  }

#endif

  for (heap = heap_list; heap != 0; heap = heap->next) {
    obj_t *block, *end;

#if HAVE_VALGRIND
    if (vg_dbg)
        VALGRIND_MAKE_MEM_DEFINED(&heap->block, sizeof heap->block);
#endif

    for (block = heap->block, end = heap->block + HEAP_SIZE;
         block < end;
         block++)
    {
      free_count += sweep_one(block);
    }
  }

  return free_count;
}

static int is_reachable(val obj)
{
  type_t t;

#if CONFIG_GEN_GC
  if (!full_gc && obj->t.gen > 0)
    return 1;
#endif

  t = obj->t.type;

  return (t & REACHABLE) != 0;
}

static void prepare_finals(void)
{
  struct fin_reg *f;

  if (!final_list)
    return;

  for (f = final_list; f; f = f->next)
    f->reachable = is_reachable(f->obj);

  for (f = final_list; f; f = f->next) {
    if (!f->reachable) {
#if CONFIG_GEN_GC
      f->obj->t.gen = 0;
#endif
      mark_obj(f->obj);
    }
    mark_obj(f->fun);
  }
}

static void call_finals(void)
{
  struct fin_reg *new_list = 0, *old_list = final_list;
  struct fin_reg **tail = &new_list, *f, *next;

  if (!final_list)
    return;

  final_list = 0;
  final_tail = &final_list;

  for (f = old_list; f; f = next) {
    next = f->next;

    if (!f->reachable) {
      funcall1(f->fun, f->obj);
      free(f);
    } else {
      *tail = f;
      tail = &f->next;
    }
  }

  *tail = 0;
  *final_tail = new_list;
  final_tail = tail;
}

void gc(void)
{
  val gc_stack_top = nil;
#if CONFIG_GEN_GC
  int exhausted = (free_list == 0);
  int full_gc_next_time = 0;
  static int gc_counter;
#endif
  int swept;
  mach_context_t mc;

  assert (gc_enabled);

#if CONFIG_GEN_GC
  if (malloc_bytes - prev_malloc_bytes >= opt_gc_delta)
    full_gc = 1;
#endif

  save_context(mc);
  gc_enabled = 0;
  rcyc_empty();
  mark(&mc, &gc_stack_top);
  hash_process_weak();
  prepare_finals();
  swept = sweep();
#if CONFIG_GEN_GC
#if 0
  printf("sweep: freed %d full_gc == %d exhausted == %d\n",
         (int) swept, full_gc, exhausted);
#endif
  if (++gc_counter >= FULL_GC_INTERVAL ||
      freshobj_idx >= FRESHOBJ_VEC_SIZE)
  {
    full_gc_next_time = 1;
    gc_counter = 0;
  }

  if (exhausted && full_gc && swept < 3 * HEAP_SIZE / 4)
    more();
#else
  if (swept < 3 * HEAP_SIZE / 4)
    more();
#endif

#if CONFIG_GEN_GC
  checkobj_idx = 0;
  mutobj_idx = 0;
  freshobj_idx = 0;
  full_gc = full_gc_next_time;
#endif
  call_finals();
  gc_enabled = 1;
  prev_malloc_bytes = malloc_bytes;
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

void gc_conservative_mark(val maybe_obj)
{
  mark_obj_maybe(maybe_obj);
}

void gc_mark_mem(val *low, val *high)
{
  mark_mem_region(low, high);
}

int gc_is_reachable(val obj)
{
  return is_ptr(obj) ? is_reachable(obj) : 1;
}

#if CONFIG_GEN_GC

val gc_set(loc lo, val obj)
{
  val *ptr = valptr(lo);

  if (lo.obj && is_ptr(obj) && lo.obj->t.gen == 1 && obj->t.gen == 0 && !full_gc) {
    if (checkobj_idx < CHECKOBJ_VEC_SIZE) {
      obj->t.gen = -1;
      checkobj[checkobj_idx++] = obj;
    } else if (gc_enabled) {
      gc();
      /* obj can't be in gen 0 because there are no baby objects after gc */
    } else {
      /* We have no space to in checkobj record this backreference, and gc is
         not available to promote obj to gen 1. We must schedule a full gc. */
      full_gc = 1;
    }
  }

  *ptr = obj;
  return obj;
}

val gc_mutated(val obj)
{
  /* We care only about mature generation objects that have not
     already been noted. And if a full gc is coming, don't bother. */
  if (full_gc || obj->t.gen <= 0)
    return obj;
  /* Store in mutobj array *before* triggering gc, otherwise
     baby objects referenced by obj could be reclaimed! */
  if (mutobj_idx < MUTOBJ_VEC_SIZE) {
    obj->t.gen = -1;
    mutobj[mutobj_idx++] = obj;
  } else if (gc_enabled) {
    gc();
  } else {
    full_gc = 1;
  }

  return obj;
}


val gc_push(val obj, loc plist)
{
  return gc_set(plist, cons(obj, deref(plist)));
}

#endif

static val gc_set_delta(val delta)
{
  opt_gc_delta = c_num(delta);
  return nil;
}

static val gc_wrap(void)
{
  if (gc_enabled) {
    gc();
    return t;
  }
  return nil;
}

val gc_finalize(val obj, val fun, val rev_order_p)
{
  type_check(fun, FUN);

  rev_order_p = default_bool_arg(rev_order_p);

  if (is_ptr(obj)) {
    struct fin_reg *f = coerce(struct fin_reg *, chk_malloc(sizeof *f));
    f->obj = obj;
    f->fun = fun;
    f->reachable = 0;

    if (rev_order_p) {
      if (!final_list)
        final_tail = &f->next;
      f->next = final_list;
      final_list = f;
    } else {
      f->next = 0;
      *final_tail = f;
      final_tail = &f->next;
    }
  }
  return obj;
}

val gc_call_finalizers(val obj)
{
  struct fin_reg *new_list = 0, *old_list = final_list;
  struct fin_reg **tail = &new_list, *f, *next;
  val ret = nil;

  if (!final_list)
    return ret;

  final_list = 0;
  final_tail = &final_list;

  for (f = old_list; f; f = next) {
    next = f->next;

    if (f->obj == obj) {
      funcall1(f->fun, f->obj);
      free(f);
      ret = t;
    } else {
      *tail = f;
      tail = &f->next;
    }
  }

  *tail = 0;
  *final_tail = new_list;
  final_tail = tail;
  return ret;
}

void gc_late_init(void)
{
  reg_fun(intern(lit("gc"), system_package), func_n0(gc_wrap));
  reg_fun(intern(lit("gc-set-delta"), system_package), func_n1(gc_set_delta));
  reg_fun(intern(lit("finalize"), user_package), func_n3o(gc_finalize, 2));
  reg_fun(intern(lit("call-finalizers"), user_package),
          func_n1(gc_call_finalizers));
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
      block->t.type = convert(type_t, block->t.type & ~(FREE | REACHABLE));
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

void gc_report_copies(val *pvar)
{
  val *opvar = pvar;
  val obj = *pvar++;

  for (; pvar < gc_stack_bottom; pvar++) {
    if (*pvar == obj)
      printf("%p found at %p (offset %d)\n",
             convert(void *, obj), convert(void *, pvar),
             convert(int, pvar - opvar));
  }
}

void gc_free_all(void)
{
  {
    heap_t *iter = heap_list;

    while (iter) {
      heap_t *next = iter->next;
      obj_t *block, *end;

#if HAVE_VALGRIND
      if (opt_vg_debug)
        VALGRIND_MAKE_MEM_DEFINED(&next->block, sizeof next->block);
#endif

      for (block = iter->block, end = iter->block + HEAP_SIZE;
           block < end;
           block++)
      {
        type_t t = block->t.type;
        if ((t & FREE) != 0)
          continue;
        finalize(block);
      }

      free(iter);
      iter = next;
    }
  }

  {
    struct fin_reg *iter = final_list;

    while (iter) {
      struct fin_reg *next = iter->next;
      free(iter);
      iter = next;
    }
  }
}
