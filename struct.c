/* Copyright 2015-2016
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

#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <dirent.h>
#include <stdarg.h>
#include <stdlib.h>
#include <limits.h>
#include <signal.h>
#include "config.h"
#include ALLOCA_H
#include "lib.h"
#include "hash.h"
#include "eval.h"
#include "signal.h"
#include "unwind.h"
#include "stream.h"
#include "gc.h"
#include "args.h"
#include "cadr.h"
#include "txr.h"
#include "struct.h"

#define max(a, b) ((a) > (b) ? (a) : (b))

#define STATIC_SLOT_BASE 0x10000000

struct struct_type {
  val self;
  val name;
  cnum id;
  cnum nslots;
  cnum nstslots;
  cnum eqmslot;
  val super;
  struct struct_type *super_handle;
  val slots;
  val stinitfun;
  val initfun;
  val boactor;
  val postinitfun;
  val dvtypes;
  val *stslot;
};

struct struct_inst {
  struct struct_type *type;
  cnum id : sizeof (cnum) * CHAR_BIT - 1 ;
  unsigned lazy : 1;
  val slot[1];
};

val struct_type_s, meth_s;

static cnum struct_id_counter;
static val struct_type_hash;
static val slot_hash;
static val struct_type_finalize_f;

static val struct_type_finalize(val obj);
static_forward(struct cobj_ops struct_type_ops);
static_forward(struct cobj_ops struct_inst_ops);

static val make_struct_type_compat(val name, val super, val slots,
                                   val initfun, val boactor);
static val call_super_method(val inst, val sym, struct args *);
static val call_super_fun(val type, val sym, struct args *);

void struct_init(void)
{
  protect(&struct_type_hash, &slot_hash, &struct_type_finalize_f,
          convert(val *, 0));
  struct_type_s = intern(lit("struct-type"), user_package);
  meth_s = intern(lit("meth"), user_package);
  struct_type_hash = make_hash(nil, nil, nil);
  slot_hash = make_hash(nil, nil, t);
  struct_type_finalize_f = func_n1(struct_type_finalize);

  if (opt_compat && opt_compat <= 117)
    reg_fun(intern(lit("make-struct-type"), user_package),
            func_n5(make_struct_type_compat));
  else
    reg_fun(intern(lit("make-struct-type"), user_package),
            func_n8o(make_struct_type, 7));

  reg_fun(intern(lit("make-struct-type"), system_package),
          func_n8(make_struct_type));
  reg_fun(intern(lit("find-struct-type"), user_package),
          func_n1(find_struct_type));
  reg_fun(intern(lit("struct-type-p"), user_package), func_n1(struct_type_p));
  reg_fun(intern(lit("super"), user_package), func_n1(super));
  reg_fun(intern(lit("make-struct"), user_package), func_n2v(make_struct));
  reg_fun(intern(lit("make-lazy-struct"), user_package),
          func_n2(make_lazy_struct));
  reg_fun(intern(lit("copy-struct"), user_package), func_n1(copy_struct));
  reg_fun(intern(lit("replace-struct"), user_package), func_n2(replace_struct));
  reg_fun(intern(lit("clear-struct"), user_package), func_n2o(clear_struct, 1));
  reg_fun(intern(lit("reset-struct"), user_package), func_n1(reset_struct));
  reg_fun(intern(lit("slot"), user_package), func_n2(slot));
  reg_fun(intern(lit("slotset"), user_package), func_n3(slotset));
  reg_fun(intern(lit("static-slot"), user_package), func_n2(static_slot));
  reg_fun(intern(lit("static-slot-set"), user_package),
          func_n3(static_slot_set));
  reg_fun(intern(lit("static-slot-ensure"), user_package),
          func_n4o(static_slot_ensure, 3));
  reg_fun(intern(lit("call-super-method"), user_package),
          func_n2v(call_super_method));
  reg_fun(intern(lit("call-super-fun"), user_package),
          func_n2v(call_super_fun));
  reg_fun(intern(lit("slotp"), user_package), func_n2(slotp));
  if (opt_compat && opt_compat <= 118)
    reg_fun(intern(lit("slot-p"), user_package), func_n2(slotp));
  reg_fun(intern(lit("static-slot-p"), user_package), func_n2(static_slot_p));
  reg_fun(intern(lit("structp"), user_package), func_n1(structp));
  reg_fun(intern(lit("struct-type"), user_package), func_n1(struct_type));
  reg_fun(intern(lit("method"), user_package), func_n2(method));
  reg_fun(intern(lit("super-method"), user_package), func_n2(super_method));
  reg_fun(intern(lit("uslot"), user_package), func_n1(uslot));
  reg_fun(intern(lit("umethod"), user_package), func_n1(umethod));
}

static noreturn void no_such_struct(val ctx, val sym)
{
  uw_throwf(error_s, lit("~a: ~s does not name a struct type"),
            ctx, sym, nao);
}

static val struct_type_finalize(val obj)
{
  struct struct_type *st = coerce(struct struct_type *, obj->co.handle);
  val id = num(st->id);
  val slot;

  for (slot = st->slots; slot; slot = cdr(slot))
    remhash(slot_hash, cons(car(slot), id));

  return nil;
}

static void call_stinitfun_chain(struct struct_type *st, val stype)
{
  if (st) {
    if (st->super)
      call_stinitfun_chain(st->super_handle, stype);
    if (st->stinitfun)
      funcall1(st->stinitfun, stype);
  }
}

static struct struct_type *stype_handle(val *pobj, val ctx)
{
  val obj = *pobj;

  switch (type(obj)) {
  case SYM:
    {
      val stype = find_struct_type(obj);
      if (!stype)
        no_such_struct(ctx, obj);
      *pobj = stype;
      return coerce(struct struct_type *, cobj_handle(stype, struct_type_s));
    }
  case COBJ:
    if (obj->co.cls == struct_type_s)
      return coerce(struct struct_type *, obj->co.handle);
    /* fallthrough */
  default:
    uw_throwf(error_s, lit("~a: ~s isn't a struct type"),
              ctx, obj, nao);
  }
}


val make_struct_type(val name, val super,
                     val static_slots, val slots,
                     val static_initfun, val initfun, val boactor,
                     val postinitfun)
{
  val self = lit("make-struct-type");

  if (super && symbolp(super)) {
    val supertype = gethash(struct_type_hash, super);
    if (!super)
      no_such_struct(self, super);
    super = supertype;
  } else if (super) {
    class_check(super, struct_type_s);
  }

  if (!bindable(name)) {
    uw_throwf(error_s, lit("~a: name ~s is not a bindable symbol"),
              self, name, nao);
  } else if (!all_satisfy(slots, func_n1(bindable), nil)) {
    uw_throwf(error_s, lit("~a: slots must be bindable symbols"),
              self, nao);
  } else if (!eql(length(uniq(slots)), length(slots))) {
    uw_throwf(error_s, lit("~a: slot names must not repeat"),
              self, nao);
  } else if (struct_id_counter == NUM_MAX) {
    uw_throwf(error_s, lit("~a: struct ID overflow"), self, nao);
  } else {
    struct struct_type *st = coerce(struct struct_type *,
                                    chk_malloc(sizeof *st));
    struct struct_type *su = if3(super, stype_handle(&super, self), 0);
    val super_slots = if2(su, su->slots);
    val all_slots = uniq(append2(super_slots, append2(static_slots, slots)));
    val stype = cobj(coerce(mem_t *, st), struct_type_s, &struct_type_ops);
    val id = num_fast(++struct_id_counter);
    val iter;
    cnum sl, stsl;
    val null_ptr = 0;

    st->self = stype;
    st->name = name;
    st->id = c_num(id);
    st->nslots = st->nstslots = st->eqmslot = 0;
    st->slots = all_slots;
    st->super = super;
    st->stslot = 0;
    st->super_handle = su;
    st->stinitfun = static_initfun;
    st->initfun = initfun;
    st->boactor = boactor;
    st->postinitfun = default_bool_arg(postinitfun);
    st->dvtypes = nil;

    gc_finalize(stype, struct_type_finalize_f, nil);

    for (sl = 0, stsl = STATIC_SLOT_BASE, iter = all_slots;
         iter;
         iter = cdr(iter))
    {
      val slot = car(iter);
      val new_tslot_p = memq(slot, static_slots);
      int inherited_p = !new_tslot_p && !memq(slot, slots);
      val ts_p = if3(inherited_p,
                     static_slot_p(super, slot),
                     memq(slot, static_slots));

      if (ts_p)
        sethash(slot_hash, cons(slot, id), num(stsl++));
      else
        sethash(slot_hash, cons(slot, id), num_fast(sl++));

      if (sl >= STATIC_SLOT_BASE)
        uw_throwf(error_s, lit("~a: too many slots"), self, nao);
    }

    stsl -= STATIC_SLOT_BASE;
    st->stslot = coerce(val *, chk_manage_vec(0, 0, stsl, sizeof (val),
                                              coerce(mem_t *, &null_ptr)));
    st->nslots = sl;
    st->nstslots = stsl;

    sethash(struct_type_hash, name, stype);

    if (super) {
      mpush(stype, mkloc(su->dvtypes, super));
      memcpy(st->stslot, su->stslot, sizeof (val) * su->nstslots);
    }

    call_stinitfun_chain(st, stype);

    return stype;
  }
}

static val make_struct_type_compat(val name, val super, val slots,
                                   val initfun, val boactor)
{
  return make_struct_type(name, super, nil, slots, nil, initfun, boactor, nil);
}

val find_struct_type(val sym)
{
  return gethash(struct_type_hash, sym);
}

val struct_type_p(val obj)
{
  return tnil(typeof(obj) == struct_type_s);
}

val super(val type)
{
  if (structp(type)) {
    struct struct_inst *si = coerce(struct struct_inst *, type->co.handle);
    return si->type->super;
  } else {
    struct struct_type *st = stype_handle(&type, lit("super"));
    return st->super;
  }
}

static void struct_type_print(val obj, val out, val pretty)
{
  struct struct_type *st = coerce(struct struct_type *, obj->co.handle);
  format(out, lit("#<struct-type ~s>"), st->name, nao);
}

static void struct_type_destroy(val obj)
{
  struct struct_type *st = coerce(struct struct_type *, obj->co.handle);
  free(st->stslot);
  free(st);
}

static void struct_type_mark(val obj)
{
  struct struct_type *st = coerce(struct struct_type *, obj->co.handle);
  cnum stsl;

  gc_mark(st->name);
  gc_mark(st->super);
  gc_mark(st->slots);
  gc_mark(st->stinitfun);
  gc_mark(st->initfun);
  gc_mark(st->boactor);
  gc_mark(st->postinitfun);
  gc_mark(st->dvtypes);

  for (stsl = 0; stsl < st->nstslots; stsl++)
    gc_mark(st->stslot[stsl]);
}

static void call_initfun_chain(struct struct_type *st, val strct)
{
  if (st) {
    if (st->super)
      call_initfun_chain(st->super_handle, strct);
    if (st->initfun)
      funcall1(st->initfun, strct);
  }
}

static void call_postinitfun_chain(struct struct_type *st, val strct)
{
  if (st) {
    if (st->postinitfun)
      funcall1(st->postinitfun, strct);
    if (st->super)
      call_postinitfun_chain(st->super_handle, strct);
  }
}

val make_struct(val type, val plist, struct args *args)
{
  val self = lit("make-struct");
  struct struct_type *st = stype_handle(&type, self);
  cnum nslots = st->nslots, sl;
  size_t size = offsetof(struct struct_inst, slot) + sizeof (val) * nslots;
  struct struct_inst *si = coerce(struct struct_inst *, chk_malloc(size));
  val sinst;
  volatile val inited = nil;

  if (args_more(args, 0) && !st->boactor) {
    free(si);
    uw_throwf(error_s,
              lit("~a: args present, but ~s has no boa constructor"),
              self, type, nao);
  }

  for (sl = 0; sl < nslots; sl++)
    si->slot[sl] = nil;
  si->type = st;
  si->id = st->id;
  si->lazy = 0;

  sinst = cobj(coerce(mem_t *, si), st->name, &struct_inst_ops);

  bug_unless (type == st->self);

  uw_simple_catch_begin;

  call_initfun_chain(st, sinst);

  for (; plist; plist = cddr(plist))
    slotset(sinst, car(plist), cadr(plist));

  if (args_more(args, 0)) {
    args_decl(args_copy, max(args->fill + 1, ARGS_MIN));
    args_add(args_copy, sinst);
    args_cat_zap(args_copy, args);
    generic_funcall(st->boactor, args_copy);
  }

  call_postinitfun_chain(st, sinst);

  inited = t;

  uw_unwind {
    if (!inited)
      gc_call_finalizers(sinst);
  }

  uw_catch_end;

  return sinst;
}

static void lazy_struct_init(val sinst, struct struct_inst *si)
{
  val self = lit("make-lazy-struct");
  struct struct_type *st = si->type;
  volatile val inited = nil;
  val cell = funcall(si->slot[0]);
  cons_bind (plist, args, cell);

  si->lazy = 0;
  si->slot[0] = nil;

  if (args && !st->boactor) {
    uw_throwf(error_s,
              lit("~a: args present, but ~s has no boa constructor"),
              self, type, nao);
  }

  uw_simple_catch_begin;

  call_initfun_chain(st, sinst);

  for (; plist; plist = cddr(plist))
    slotset(sinst, car(plist), cadr(plist));

  if (args) {
    args_decl_list(argv, ARGS_MIN, cons(sinst, args));
    generic_funcall(st->boactor, argv);
  }

  call_postinitfun_chain(st, sinst);

  inited = t;

  uw_unwind {
    if (!inited)
      gc_call_finalizers(sinst);
  }

  uw_catch_end;
}

INLINE void check_init_lazy_struct(val sinst, struct struct_inst *si)
{
  if (si->lazy)
    lazy_struct_init(sinst, si);
}

val make_lazy_struct(val type, val argfun)
{
  val self = lit("make-lazy-struct");
  struct struct_type *st = stype_handle(&type, self);
  cnum nslots = st->nslots, sl;
  cnum nalloc = nslots ? nslots : 1;
  size_t size = offsetof(struct struct_inst, slot) + sizeof (val) * nalloc;
  struct struct_inst *si = coerce(struct struct_inst *, chk_malloc(size));
  val sinst;

  for (sl = 0; sl < nslots; sl++)
    si->slot[sl] = nil;
  si->type = st;
  si->id = st->id;
  si->lazy = 1;

  sinst = cobj(coerce(mem_t *, si), st->name, &struct_inst_ops);

  bug_unless (type == st->self);

  si->slot[0] = argfun;

  return sinst;
}

static struct struct_inst *struct_handle(val obj, val ctx)
{
  if (cobjp(obj) && obj->co.ops == &struct_inst_ops)
    return coerce(struct struct_inst *, obj->co.handle);
  uw_throwf(error_s, lit("~a: ~s isn't a structure"),
            ctx, obj, nao);
}

val copy_struct(val strct)
{
  const val self = lit("copy-struct");
  val copy;
  struct struct_inst *si = struct_handle(strct, self);
  struct struct_type *st = si->type;
  cnum nslots = st->nslots;
  size_t size = offsetof(struct struct_inst, slot) + sizeof (val) * nslots;
  struct struct_inst *si_copy = coerce(struct struct_inst *, chk_malloc(size));
  check_init_lazy_struct(strct, si);
  memcpy(si_copy, si, size);
  copy = cobj(coerce(mem_t *, si_copy), st->name, &struct_inst_ops);
  gc_hint(strct);
  return copy;
}

val clear_struct(val strct, val value)
{
  const val self = lit("clear-struct");
  struct struct_inst *si = struct_handle(strct, self);
  struct struct_type *st = si->type;
  val clear_val = default_bool_arg(value);
  cnum i;

  check_init_lazy_struct(strct, si);

  for (i = 0; i < st->nslots; i++)
    si->slot[i] = clear_val;

  return strct;
}

val replace_struct(val target, val source)
{
  const val self = lit("replace-struct");
  struct struct_inst *tsi = struct_handle(target, self);
  struct struct_inst *ssi = struct_handle(source, self);
  struct struct_type *sst = ssi->type;
  cnum nslots = sst->nslots;
  size_t size = offsetof(struct struct_inst, slot) + sizeof (val) * nslots;
  struct struct_inst *ssi_copy = coerce(struct struct_inst *, chk_malloc(size));

  check_init_lazy_struct(source, ssi);
  check_init_lazy_struct(target, tsi);

  memcpy(ssi_copy, ssi, size);
  free(tsi);
  target->co.handle = coerce(mem_t *, ssi_copy);
  target->co.cls = source->co.cls;
  return target;
}

val reset_struct(val strct)
{
  const val self = lit("reset-struct");
  struct struct_inst *si = struct_handle(strct, self);
  struct struct_type *st = si->type;
  cnum i;

  check_init_lazy_struct(strct, si);

  for (i = 0; i < st->nslots; i++)
    si->slot[i] = nil;

  call_initfun_chain(st, strct);

  return strct;
}

static int cache_set_lookup(slot_cache_entry_t *set, cnum id)
{
  if (set[0].id == id)
    return set[0].slot;

  if (set[1].id == id) {
    slot_cache_entry_t tmp = set[0];
    set[0] = set[1];
    set[1] = tmp;
    return set[0].slot;
  }

  if (set[2].id == id) {
    slot_cache_entry_t tmp = set[1];
    set[1] = set[2];
    set[2] = tmp;
    return set[1].slot;
  }

  if (set[3].id == id) {
    slot_cache_entry_t tmp = set[2];
    set[2] = set[3];
    set[3] = tmp;
    return set[2].slot;
  }

  return -1;
}

static void cache_set_insert(slot_cache_entry_t *set, cnum id, cnum slot)
{
  int entry;

  if (set[0].id == 0)
    entry = 0;
  else if (set[1].id == 0)
    entry = 1;
  else if (set[2].id == 0)
    entry = 2;
  else
    entry = 3;

  set[entry].id = id;
  set[entry].slot = slot;
}

static loc lookup_slot(val inst, struct struct_inst *si, val sym)
{
  slot_cache_t slot_cache = sym->s.slot_cache;
  cnum id = si->id;

  if (slot_cache != 0) {
    slot_cache_set_t *set = &slot_cache[id % SLOT_CACHE_SIZE];
    cnum slot = cache_set_lookup(*set, id);

    if (slot >= STATIC_SLOT_BASE) {
      struct struct_type *st = si->type;
      return mkloc(st->stslot[slot - STATIC_SLOT_BASE], st->self);
    } else if (slot >= 0) {
      check_init_lazy_struct(inst, si);
      return mkloc(si->slot[slot], inst);
    } else {
      val key = cons(sym, num_fast(id));
      val sl = gethash(slot_hash, key);
      cnum slnum = coerce(cnum, sl) >> TAG_SHIFT;
      if (sl) {
        cache_set_insert(*set, id, slnum);
        if (slnum >= STATIC_SLOT_BASE) {
          struct struct_type *st = si->type;
          return mkloc(st->stslot[slnum - STATIC_SLOT_BASE], st->self);
        }
        check_init_lazy_struct(inst, si);
        return mkloc(si->slot[slnum], inst);
      }
    }
  } else {
    slot_cache = coerce(slot_cache_t,
                        chk_calloc(SLOT_CACHE_SIZE,
                                   sizeof (slot_cache_set_t)));
    slot_cache_set_t *set = &slot_cache[id % SLOT_CACHE_SIZE];
    val key = cons(sym, num_fast(id));
    val sl = gethash(slot_hash, key);
    cnum slnum = coerce(cnum, sl) >> TAG_SHIFT;

    sym->s.slot_cache = slot_cache;

    if (sl) {
      cache_set_insert(*set, id, slnum);
      if (slnum >= STATIC_SLOT_BASE) {
        struct struct_type *st = si->type;
        return mkloc(st->stslot[slnum - STATIC_SLOT_BASE], st->self);
      }
      check_init_lazy_struct(inst, si);
      return mkloc(si->slot[slnum], inst);
    }
  }

  return nulloc;
}

static loc lookup_static_slot(val stype, struct struct_type *st, val sym)
{
  slot_cache_t slot_cache = sym->s.slot_cache;
  cnum id = st->id;

  if (slot_cache != 0) {
    slot_cache_set_t *set = &slot_cache[id % SLOT_CACHE_SIZE];
    cnum slot = cache_set_lookup(*set, id);

    if (slot >= STATIC_SLOT_BASE) {
      return mkloc(st->stslot[slot - STATIC_SLOT_BASE], stype);
    } else if (slot < 0) {
      val key = cons(sym, num_fast(id));
      val sl = gethash(slot_hash, key);
      cnum slnum = coerce(cnum, sl) >> TAG_SHIFT;
      if (sl) {
        cache_set_insert(*set, id, slnum);
        if (slnum >= STATIC_SLOT_BASE)
          return mkloc(st->stslot[slnum - STATIC_SLOT_BASE], stype);
      }
    }
  } else {
    slot_cache = coerce(slot_cache_t,
                        chk_calloc(SLOT_CACHE_SIZE,
                                   sizeof (slot_cache_set_t)));
    slot_cache_set_t *set = &slot_cache[id % SLOT_CACHE_SIZE];
    val key = cons(sym, num_fast(id));
    val sl = gethash(slot_hash, key);
    cnum slnum = coerce(cnum, sl) >> TAG_SHIFT;

    sym->s.slot_cache = slot_cache;

    if (sl) {
      cache_set_insert(*set, id, slnum);
      if (slnum >= STATIC_SLOT_BASE)
        return mkloc(st->stslot[slnum - STATIC_SLOT_BASE], stype);
    }
  }

  return nulloc;
}

static noreturn void no_such_slot(val ctx, val type, val slot)
{
  uw_throwf(error_s, lit("~a: ~s has no slot named ~s"),
            ctx, type, slot, nao);
}

val slot(val strct, val sym)
{
  const val self = lit("slot");
  struct struct_inst *si = struct_handle(strct, self);

  if (symbolp(sym)) {
    loc ptr = lookup_slot(strct, si, sym);
    if (!nullocp(ptr))
      return deref(ptr);
  }

  no_such_slot(self, si->type->self, sym);
}

val slotset(val strct, val sym, val newval)
{
  const val self = lit("slotset");
  struct struct_inst *si = struct_handle(strct, self);

  if (symbolp(sym)) {
    loc ptr = lookup_slot(strct, si, sym);
    if (!nullocp(ptr))
      return set(ptr, newval);
  }

  no_such_slot(self, si->type->self, sym);
}

val static_slot(val stype, val sym)
{
  val self = lit("static-slot");
  struct struct_type *st = stype_handle(&stype, self);

  if (symbolp(sym)) {
    loc ptr = lookup_static_slot(stype, st, sym);
    if (!nullocp(ptr))
      return deref(ptr);
  }

  no_such_slot(self, stype, sym);
}

val static_slot_set(val stype, val sym, val newval)
{
  val self = lit("static-slot-set");
  struct struct_type *st = stype_handle(&stype, self);

  if (symbolp(sym)) {
    loc ptr = lookup_static_slot(stype, st, sym);
    if (!nullocp(ptr)) {
      if (st->eqmslot == -1)
        st->eqmslot = 0;
      return set(ptr, newval);
    }
  }

  no_such_slot(self, stype, sym);
}

val static_slot_ensure(val stype, val sym, val newval, val no_error_p)
{
  val self = lit("static-slot-ensure");
  struct struct_type *st = stype_handle(&stype, self);
  loc ptr;

  if (!bindable(sym))
    uw_throwf(error_s, lit("~a: ~s isn't a valid slot name"),
              self, sym, nao);

  no_error_p = default_bool_arg(no_error_p);

  if (st->eqmslot == -1)
    st->eqmslot = 0;

  if (nullocp((ptr = lookup_static_slot(stype, st, sym)))) {
    if (!memq(sym, st->slots)) {
      st->stslot = coerce(val *, chk_manage_vec(coerce(mem_t *, st->stslot),
                                                st->nstslots, st->nstslots + 1,
                                                sizeof (val),
                                                coerce(mem_t *, &newval)));
      set(mkloc(st->slots, stype), append2(st->slots, cons(sym, nil)));
      sethash(slot_hash, cons(sym, num_fast(st->id)),
              num(st->nstslots++ + STATIC_SLOT_BASE));
    } else {
      if (!no_error_p)
        uw_throwf(error_s, lit("~a: ~s is an instance slot of ~s"),
                  self, sym, stype, nao);
    }
  } else {
    set(ptr, newval);
  }

  {
    val iter;
    for (iter = st->dvtypes; iter; iter = cdr(iter))
      static_slot_ensure(car(iter), sym, newval, t);
  }

  return newval;
}

static val call_super_method(val inst, val sym, struct args *args)
{
  val type = struct_type(inst);
  val suptype = super(type);

  if (suptype) {
    val meth = static_slot(suptype, sym);
    args_decl(args_copy, max(args->fill + 1, ARGS_MIN));
    args_add(args_copy, inst);
    args_cat_zap(args_copy, args);
    return generic_funcall(meth, args_copy);
  }

  uw_throwf(error_s, lit("call-super-method: ~s has no supertype"),
            suptype, nao);
}

static val call_super_fun(val type, val sym, struct args *args)
{
  val suptype = super(type);

  if (suptype) {
    val fun = static_slot(suptype, sym);
    return generic_funcall(fun, args);
  }

  uw_throwf(error_s, lit("call-super-fun: ~s has no supertype"),
            type, nao);
}

val slotp(val type, val sym)
{
  struct struct_type *st = stype_handle(&type, lit("slotp"));
  return tnil(memq(sym, st->slots));
}

val static_slot_p(val type, val sym)
{
  struct struct_type *st = stype_handle(&type, lit("static-slot-p"));

  if (memq(sym, st->slots)) {
    val key = cons(sym, num_fast(st->id));
    val sl = gethash(slot_hash, key);
    cnum slnum = coerce(cnum, sl) >> TAG_SHIFT;

    if (sl && slnum >= STATIC_SLOT_BASE)
      return t;
  }

  return nil;
}

val structp(val obj)
{
  return tnil(cobjp(obj) && obj->co.ops == &struct_inst_ops);
}

val struct_type(val strct)
{
  const val self = lit("struct-type");
  struct struct_inst *si = struct_handle(strct, self);
  return si->type->self;
}

static val method_fun(val env, varg args)
{
  cons_bind (fun, strct, env);
  args_decl(args_copy, max(args->fill + 1, ARGS_MIN));
  args_add(args_copy, strct);
  args_cat_zap(args_copy, args);
  return generic_funcall(fun, args_copy);
}

val method(val strct, val slotsym)
{
  return func_f0v(cons(slot(strct, slotsym), strct), method_fun);
}

val super_method(val strct, val slotsym)
{
  val super_slot = static_slot(super(struct_type(strct)), slotsym);
  return func_f0v(cons(super_slot, strct), method_fun);
}

static val uslot_fun(val sym, val strct)
{
  val self = lit("uslot");
  struct struct_inst *si = struct_handle(strct, self);

  if (symbolp(sym)) {
    loc ptr = lookup_slot(strct, si, sym);
    if (!nullocp(ptr))
      return deref(ptr);
  }

  no_such_slot(self, si->type->self, sym);
}

val uslot(val slot)
{
  return func_f1(slot, uslot_fun);
}

static val umethod_fun(val sym, struct args *args)
{
  val self = lit("umethod");

  if (args->argc == 0) {
    uw_throwf(error_s, lit("~a: object argument required to call ~s"),
              self, env, nao);
  } else {
    val strct = args->arg[0];
    struct struct_inst *si = struct_handle(strct, self);

    if (symbolp(sym)) {
      loc ptr = lookup_slot(strct, si, sym);
      if (!nullocp(ptr))
        return generic_funcall(deref(ptr), args);
    }

    no_such_slot(self, si->type->self, sym);
  }
}

val umethod(val slot)
{
  return func_f0v(slot, umethod_fun);
}

static void struct_inst_print(val obj, val out, val pretty)
{
  struct struct_inst *si = coerce(struct struct_inst *, obj->co.handle);
  struct struct_type *st = si->type;
  val save_mode = test_set_indent_mode(out, num_fast(indent_off),
                                       num_fast(indent_data));
  val save_indent, iter, once;

  put_string(lit("#S("), out);
  obj_print_impl(st->name, out, pretty);
  save_indent = inc_indent(out, one);

  for (iter = st->slots, once = t; iter; iter = cdr(iter)) {
    val sym = car(iter);
    if (!static_slot_p(st->self, sym)) {
      if (once) {
        put_char(chr(' '), out);
        once = nil;
      } else {
        width_check(out, chr(' '));
      }
      obj_print_impl(sym, out, pretty);
      put_char(chr(' '), out);
      obj_print_impl(slot(obj, sym), out, pretty);
    }
  }
  put_char(chr(')'), out);
  set_indent_mode(out, save_mode);
  set_indent(out, save_indent);
}

static void struct_inst_mark(val obj)
{
  struct struct_inst *si = coerce(struct struct_inst *, obj->co.handle);
  struct struct_type *st = si->type;
  cnum sl, nslots = st->nslots;

  if (si->lazy)
    nslots = 1;

  for (sl = 0; sl < nslots; sl++)
    gc_mark(si->slot[sl]);
  gc_mark(st->self);
}

static val struct_inst_equal(val left, val right)
{
  struct struct_inst *ls = coerce(struct struct_inst *, left->co.handle);
  struct struct_inst *rs = coerce(struct struct_inst *, right->co.handle);
  struct struct_type *st = ls->type;
  cnum nslots = st->nslots, sl;

  if (st != rs->type)
    return nil;

  check_init_lazy_struct(left, ls);
  check_init_lazy_struct(right, rs);

  for (sl = 0; sl < nslots; sl++)
    if (!equal(ls->slot[sl], rs->slot[sl]))
      return nil;

  gc_hint(left);
  gc_hint(right);
  return t;
}

static cnum struct_inst_hash(val obj)
{
  struct struct_inst *si = coerce(struct struct_inst *, obj->co.handle);
  struct struct_type *st = si->type;
  cnum nslots = st->nslots, sl, out = c_num(hash_equal(st->self));

  check_init_lazy_struct(obj, si);

  for (sl = 0; sl < nslots; sl++) {
    val hash = hash_equal(si->slot[sl]);
    out += c_num(hash);
    out &= NUM_MAX;
  }

  return out;
}

static val get_equal_method(val stype, struct struct_type *st)
{
  if (st->eqmslot == -1) {
    return nil;
  } else if (st->eqmslot) {
    return st->stslot[st->eqmslot];
  } else {
    loc ptr = lookup_static_slot(stype, st, equal_s);
    if (!nullocp(ptr)) {
      st->eqmslot = valptr(ptr) - st->stslot;
      return deref(ptr);
    }
    st->eqmslot = -1;
    return nil;
  }
}

static val struct_inst_equalsub(val obj)
{
  struct struct_inst *si = coerce(struct struct_inst *, obj->co.handle);
  struct struct_type *st = si->type;
  val equal_method = get_equal_method(obj, st);
  if (equal_method) {
    val sub = funcall1(equal_method, obj);
    if (nilp(sub)) {
      uw_throwf(error_s, lit("equal method on type ~s returned nil"),
                st->self, nao);
    }
    return sub;
  }
  return nil;
}

val method_name(val fun)
{
  val sth_iter = hash_begin(struct_type_hash);
  val sth_cell;

  while ((sth_cell = hash_next(sth_iter))) {
    val sym = car(sth_cell);
    val stype = cdr(sth_cell);
    val sl_iter;
    struct struct_type *st = coerce(struct struct_type *, stype->co.handle);

    for (sl_iter = st->slots; sl_iter; sl_iter = cdr(sl_iter)) {
      val slot = car(sl_iter);
      loc ptr = lookup_static_slot(stype, st, slot);

      if (!nullocp(ptr) && deref(ptr) == fun) {
        val sstype;

        while ((sstype = super(stype)) != nil) {
          struct struct_type *sst = coerce(struct struct_type *,
                                           sstype->co.handle);
          loc sptr = lookup_static_slot(sstype, sst, slot);
          if (!nullocp(sptr) && deref(sptr) == fun) {
            stype = sstype;
            sym = sst->name;
            continue;
          }

          break;
        }

        return list(meth_s, sym, slot, nao);
      }
    }
  }

  return nil;
}

static_def(struct cobj_ops struct_type_ops =
           cobj_ops_init(eq, struct_type_print, struct_type_destroy,
                         struct_type_mark, cobj_hash_op))

static_def(struct cobj_ops struct_inst_ops =
           cobj_ops_init_ex(struct_inst_equal, struct_inst_print,
                            cobj_destroy_free_op, struct_inst_mark,
                            struct_inst_hash, struct_inst_equalsub))
