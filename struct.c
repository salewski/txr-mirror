/* Copyright 2015-2017
 * Kaz Kylheku <kaz@kylheku.com>
 * Vancouver, Canada
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <dirent.h>
#include <stdarg.h>
#include <stdlib.h>
#include <limits.h>
#include <signal.h>
#include <assert.h>
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
#include "lisplib.h"
#include "struct.h"

#define max(a, b) ((a) > (b) ? (a) : (b))

#define STATIC_SLOT_BASE 0x10000000

struct stslot {
  val home_type;
  cnum home_offs;
  val *home;
  val store;
};

#define stslot_loc(s) mkloc(*(s)->home, (s)->home_type)
#define stslot_place(s) (*(s)->home)

struct struct_type {
  val self;
  val name;
  cnum id;
  cnum nslots;
  cnum nstslots;
  struct stslot *eqmslot;
  val super;
  struct struct_type *super_handle;
  val slots;
  val stinitfun;
  val initfun;
  val boactor;
  val postinitfun;
  val dvtypes;
  struct stslot *stslot;
};

struct struct_inst {
  struct struct_type *type;
  cnum id : sizeof (cnum) * CHAR_BIT - 1 ;
  unsigned lazy : 1;
  unsigned dirty : 1;
  val slot[1];
};

val struct_type_s, meth_s, print_s, make_struct_lit_s;
val slot_s, static_slot_s;

static cnum struct_id_counter;
static val struct_type_hash;
static val slot_hash;
static val struct_type_finalize_f;
static val slot_type_hash;
static val static_slot_type_hash;

static val struct_type_finalize(val obj);
static_forward(struct cobj_ops struct_type_ops);
static_forward(struct cobj_ops struct_inst_ops);

static val make_struct_type_compat(val name, val super, val slots,
                                   val initfun, val boactor);
static val call_super_method(val inst, val sym, struct args *);
static val call_super_fun(val type, val sym, struct args *);

void struct_init(void)
{
  protect(&struct_type_hash, &slot_hash, &slot_type_hash,
          &static_slot_type_hash, &struct_type_finalize_f,
          convert(val *, 0));
  struct_type_s = intern(lit("struct-type"), user_package);
  meth_s = intern(lit("meth"), user_package);
  print_s = intern(lit("print"), user_package);
  make_struct_lit_s = intern(lit("make-struct-lit"), system_package);
  slot_s = intern(lit("slot"), system_package);
  static_slot_s = intern(lit("static-slot"), system_package);
  struct_type_hash = make_hash(nil, nil, nil);
  slot_hash = make_hash(nil, nil, t);
  slot_type_hash = make_hash(nil, nil, nil);
  slot_type_hash = make_hash(nil, nil, nil);
  static_slot_type_hash = make_hash(nil, nil, nil);
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
  reg_fun(intern(lit("struct-from-plist"), user_package), func_n1v(struct_from_plist));
  reg_fun(intern(lit("struct-from-args"), user_package), func_n1v(struct_from_args));
  reg_fun(intern(lit("make-lazy-struct"), user_package),
          func_n2(make_lazy_struct));
  reg_fun(make_struct_lit_s, func_n2(make_struct_lit));
  reg_fun(intern(lit("copy-struct"), user_package), func_n1(copy_struct));
  reg_fun(intern(lit("replace-struct"), user_package), func_n2(replace_struct));
  reg_fun(intern(lit("clear-struct"), user_package), func_n2o(clear_struct, 1));
  reg_fun(intern(lit("reset-struct"), user_package), func_n1(reset_struct));
  reg_fun(intern(lit("slot"), user_package), func_n2(slot));
  reg_fun(intern(lit("slotset"), user_package), func_n3(slotset));
  reg_fun(intern(lit("static-slot"), user_package), func_n2(static_slot));
  reg_fun(intern(lit("static-slot-set"), user_package),
          func_n3(static_slot_set));
  reg_fun(intern(lit("test-dirty"), user_package), func_n1(test_dirty));
  reg_fun(intern(lit("test-clear-dirty"), user_package), func_n1(test_clear_dirty));
  reg_fun(intern(lit("clear-dirty"), user_package), func_n1(clear_dirty));
  reg_fun(intern(lit("static-slot-ensure"), user_package),
          func_n4o(static_slot_ensure, 3));
  reg_fun(intern(lit("static-slot-home"), user_package),
          func_n2(static_slot_home));
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
  reg_fun(intern(lit("method"), user_package), func_n2v(method_args));
  reg_fun(intern(lit("super-method"), user_package), func_n2(super_method));
  reg_fun(intern(lit("uslot"), user_package), func_n1(uslot));
  reg_fun(intern(lit("umethod"), user_package), func_n1v(umethod));
  reg_fun(intern(lit("slots"), user_package), func_n1(slots));
  reg_fun(intern(lit("slot-types"), system_package), func_n1(slot_types));
  reg_fun(intern(lit("static-slot-types"), system_package), func_n1(static_slot_types));
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
    if (st->super && opt_compat && opt_compat <= 151)
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

static void static_slot_home_fixup(struct struct_type *st)
{
  cnum i;
  for (i = 0; i < st->nstslots; i++) {
    struct stslot *s = &st->stslot[i];
    if (s->home_type == st->self) {
      s->home = &s->store;
    } else {
      struct struct_type *shome = coerce(struct struct_type *,
                                         s->home_type->co.handle);
      *s = shome->stslot[s->home_offs];
      s->store = nil;
    }
  }
}

val make_struct_type(val name, val super,
                     val static_slots, val slots,
                     val static_initfun, val initfun, val boactor,
                     val postinitfun)
{
  val self = lit("make-struct-type");

  if (super && symbolp(super)) {
    val supertype = find_struct_type(super);
    if (!supertype)
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
    cnum stsl_upb = c_num(plus(length(static_slots),
                               num(if3(su, su->nstslots, 0))));
    struct stslot null_ptr = { nil, 0, 0, nil };

    st->self = stype;
    st->name = name;
    st->id = c_num(id);
    st->nslots = st->nstslots = 0;
    st->eqmslot = 0;
    st->slots = all_slots;
    st->super = super;
    st->stslot = 0;
    st->super_handle = su;
    st->stinitfun = static_initfun;
    st->initfun = initfun;
    st->boactor = boactor;
    st->postinitfun = default_null_arg(postinitfun);
    st->dvtypes = nil;

    gc_finalize(stype, struct_type_finalize_f, nil);

    st->stslot = coerce(struct stslot *,
                        chk_manage_vec(0, 0, stsl_upb, sizeof *st->stslot,
                                       coerce(mem_t *, &null_ptr)));

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

      if (ts_p) {
        cnum n = stsl++ - STATIC_SLOT_BASE;
        struct stslot *ss = &st->stslot[n];
        val key = if2(su, cons(slot, num_fast(su->id)));
        val msl = if2(su, gethash(slot_hash, key));
        cnum m = (coerce(cnum, msl) >> TAG_SHIFT) - STATIC_SLOT_BASE;

        if (!inherited_p || (opt_compat && opt_compat <= 151)) {
          ss->home_type = stype;
          ss->home_offs = n;
          ss->home = &ss->store;
          ss->store = if2(msl, stslot_place(&su->stslot[m]));
        } else {
          *ss = su->stslot[m];
          ss->store = nil;
        }
        sethash(slot_hash, cons(slot, id), num(n + STATIC_SLOT_BASE));
        static_slot_type_reg(slot, name);
      } else {
        sethash(slot_hash, cons(slot, id), num_fast(sl++));
        slot_type_reg(slot, name);
      }

      if (sl >= STATIC_SLOT_BASE)
        uw_throwf(error_s, lit("~a: too many instance slots"), self, nao);

      if (stsl >= NUM_MAX)
        uw_throwf(error_s, lit("~a: too many static slots"), self, nao);
    }

    stsl -= STATIC_SLOT_BASE;
    st->stslot = coerce(struct stslot *,
                        chk_manage_vec(coerce(mem_t *, st->stslot), stsl_upb,
                                       stsl, sizeof *st->stslot,
                                       coerce(mem_t *, &null_ptr)));
    st->nslots = sl;
    st->nstslots = stsl;
    static_slot_home_fixup(st);

    sethash(struct_type_hash, name, stype);

    if (super)
      mpush(stype, mkloc(su->dvtypes, super));

    call_stinitfun_chain(st, stype);

    uw_purge_deferred_warning(cons(struct_type_s, name));

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
  uses_or2;
  return or2(gethash(struct_type_hash, sym),
             if2(lisplib_try_load(sym),
                 gethash(struct_type_hash, sym)));
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

static void struct_type_print(val obj, val out, val pretty, struct strm_ctx *c)
{
  struct struct_type *st = coerce(struct struct_type *, obj->co.handle);
  (void) c;
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

  for (stsl = 0; stsl < st->nstslots; stsl++) {
    struct stslot *sl = &st->stslot[stsl];

    if (sl->home_type == st->self)
      gc_mark(sl->store);
    else
      assert (sl->store == nil);
  }
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
    int derived_first = (opt_compat && opt_compat <= 148);

    if (derived_first && st->postinitfun)
      funcall1(st->postinitfun, strct);
    if (st->super)
      call_postinitfun_chain(st->super_handle, strct);
    if (!derived_first && st->postinitfun)
      funcall1(st->postinitfun, strct);
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
  si->dirty = 1;

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

val struct_from_plist(val type, struct args *plist)
{
  val list = args_get_list(plist);
  args_decl(boa, 0);
  return make_struct(type, list, boa);
}

val struct_from_args(val type, struct args *boa)
{
  return make_struct(type, nil, boa);
}

static void lazy_struct_init(val sinst, struct struct_inst *si)
{
  val self = lit("make-lazy-struct");
  struct struct_type *st = si->type;
  volatile val inited = nil;
  val cell = funcall(si->slot[0]);
  cons_bind (plist, args, cell);

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
  if (si->lazy) {
    si->lazy = 0;
    lazy_struct_init(sinst, si);
  }
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

val make_struct_lit(val type, val plist)
{
  args_decl(args, 0);
  val strct;

  if (opt_compat && opt_compat <= 154) {
    strct = make_struct(type, plist, args);
  } else {
    strct = make_struct(type, nil, args);
    for (; plist; plist = cddr(plist))
      slotset(strct, car(plist), cadr(plist));
  }

  return strct;
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
  val clear_val = default_null_arg(value);
  cnum i;

  check_init_lazy_struct(strct, si);

  for (i = 0; i < st->nslots; i++)
    si->slot[i] = clear_val;

  mut(strct);

  return strct;
}

val replace_struct(val target, val source)
{
  const val self = lit("replace-struct");

  if (target != source) {
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
    mut(target);
  }

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
      struct stslot *stsl = &st->stslot[slot - STATIC_SLOT_BASE];
      return stslot_loc(stsl);
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
          struct stslot *stsl = &st->stslot[slnum - STATIC_SLOT_BASE];
          return stslot_loc(stsl);
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
        struct stslot *stsl = &st->stslot[slnum - STATIC_SLOT_BASE];
        return stslot_loc(stsl);
      }
      check_init_lazy_struct(inst, si);
      return mkloc(si->slot[slnum], inst);
    }
  }

  return nulloc;
}

static struct stslot *lookup_static_slot_desc(struct struct_type *st, val sym)
{
  slot_cache_t slot_cache = sym->s.slot_cache;
  cnum id = st->id;

  if (slot_cache != 0) {
    slot_cache_set_t *set = &slot_cache[id % SLOT_CACHE_SIZE];
    cnum slot = cache_set_lookup(*set, id);

    if (slot >= STATIC_SLOT_BASE) {
      return &st->stslot[slot - STATIC_SLOT_BASE];
    } else if (slot < 0) {
      val key = cons(sym, num_fast(id));
      val sl = gethash(slot_hash, key);
      cnum slnum = coerce(cnum, sl) >> TAG_SHIFT;
      if (sl) {
        cache_set_insert(*set, id, slnum);
        if (slnum >= STATIC_SLOT_BASE)
          return &st->stslot[slnum - STATIC_SLOT_BASE];
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
        return &st->stslot[slnum - STATIC_SLOT_BASE];
    }
  }

  return 0;
}

static loc lookup_static_slot(struct struct_type *st, val sym)
{
  struct stslot *stsl = lookup_static_slot_desc(st, sym);
  return stsl ? stslot_loc(stsl) : nulloc;
}

static loc lookup_slot_load(val inst, struct struct_inst *si, val sym)
{
  loc ptr = lookup_slot(inst, si, sym);
  if (nullocp(ptr)) {
    lisplib_try_load(sym);
    return lookup_slot(inst, si, sym);
  }
  return ptr;
}

static loc lookup_static_slot_load(struct struct_type *st, val sym)
{
  loc ptr = lookup_static_slot(st, sym);
  if (nullocp(ptr)) {
    lisplib_try_load(sym);
    return lookup_static_slot(st, sym);
  }
  return ptr;
}

static struct stslot *lookup_static_slot_desc_load(struct struct_type *st,
                                                   val sym)
{
  struct stslot *stsl = lookup_static_slot_desc(st, sym);
  if (stsl == 0) {
    lisplib_try_load(sym);
    return lookup_static_slot_desc(st, sym);
  }
  return stsl;
}

static noreturn void no_such_slot(val ctx, val type, val slot)
{
  uw_throwf(error_s, lit("~a: ~s has no slot named ~s"),
            ctx, type, slot, nao);
}

static noreturn void no_such_static_slot(val ctx, val type, val slot)
{
  uw_throwf(error_s, lit("~a: ~s has no static slot named ~s"),
            ctx, type, slot, nao);
}

val slot(val strct, val sym)
{
  const val self = lit("slot");
  struct struct_inst *si = struct_handle(strct, self);

  if (sym && symbolp(sym)) {
    loc ptr = lookup_slot_load(strct, si, sym);
    if (!nullocp(ptr))
      return deref(ptr);
  }

  no_such_slot(self, si->type->self, sym);
}

val maybe_slot(val strct, val sym)
{
  const val self = lit("slot");
  struct struct_inst *si = struct_handle(strct, self);

  if (sym && symbolp(sym)) {
    loc ptr = lookup_slot_load(strct, si, sym);
    if (!nullocp(ptr))
      return deref(ptr);
  }

  return nil;
}

val slotset(val strct, val sym, val newval)
{
  const val self = lit("slotset");
  struct struct_inst *si = struct_handle(strct, self);

  if (sym && symbolp(sym)) {
    loc ptr = lookup_slot(strct, si, sym);
    if (!nullocp(ptr)) {
      if (!si->dirty) {
        if (valptr(ptr) >= &si->slot[0] &&
            valptr(ptr) < &si->slot[si->type->nslots])
        {
          si->dirty = 1;
        }
      }
      return set(ptr, newval);
    }
  }

  no_such_slot(self, si->type->self, sym);
}

val static_slot(val stype, val sym)
{
  val self = lit("static-slot");
  struct struct_type *st = stype_handle(&stype, self);

  if (symbolp(sym)) {
    loc ptr = lookup_static_slot_load(st, sym);
    if (!nullocp(ptr))
      return deref(ptr);
  }

  no_such_static_slot(self, stype, sym);
}

val static_slot_set(val stype, val sym, val newval)
{
  val self = lit("static-slot-set");
  struct struct_type *st = stype_handle(&stype, self);

  if (symbolp(sym)) {
    loc ptr = lookup_static_slot(st, sym);
    if (!nullocp(ptr)) {
      if (st->eqmslot == coerce(struct stslot *, -1))
        st->eqmslot = 0;
      return set(ptr, newval);
    }
  }

  no_such_static_slot(self, stype, sym);
}

val test_dirty(val strct)
{
  const val self = lit("test-dirty");
  struct struct_inst *si = struct_handle(strct, self);
  return tnil(si->dirty);
}

val test_clear_dirty(val strct)
{
  const val self = lit("test-clear-dirty");
  struct struct_inst *si = struct_handle(strct, self);
  val ret = tnil(si->dirty);
  si->dirty = 0;
  return ret;
}

val clear_dirty(val strct)
{
  const val self = lit("clear-dirty");
  struct struct_inst *si = struct_handle(strct, self);
  si->dirty = 0;
  return strct;
}

static void static_slot_home_fixup_rec(struct struct_type *st)
{
  static_slot_home_fixup(st);

  {
    val iter;

    for (iter = st->dvtypes; iter; iter = cdr(iter)) {
      val stype = car(iter);
      struct struct_type *st = coerce(struct struct_type *, stype->co.handle);
      static_slot_home_fixup_rec(st);
    }
  }
}

static void static_slot_rewrite_rec(struct struct_type *st,
                                    struct stslot *from,
                                    struct stslot *to)
{
  cnum i;
  val iter;

  for (iter = st->dvtypes; iter; iter = cdr(iter)) {
    val stype = car(iter);
    struct struct_type *st = coerce(struct struct_type *, stype->co.handle);
    static_slot_rewrite_rec(st, from, to);
  }

  for (i = 0; i < st->nstslots; i++) {
    struct stslot *s = &st->stslot[i];

    if (s->home_type == from->home_type &&
        s->home == from->home &&
        s->home_offs == from->home_offs)
    {
      *s = *to;
    }
  }
}


static val static_slot_ens_rec(val stype, val sym, val newval,
                               val no_error_p, val self,
                               struct stslot *inh_stsl)
{
  struct struct_type *st = stype_handle(&stype, self);
  struct stslot *stsl = lookup_static_slot_desc(st, sym);

  if (!bindable(sym))
    uw_throwf(error_s, lit("~a: ~s isn't a valid slot name"),
              self, sym, nao);

  no_error_p = default_null_arg(no_error_p);

  if (st->eqmslot == coerce(struct stslot *, -1))
    st->eqmslot = 0;

  if (stsl != 0 && opt_compat && opt_compat <= 151) {
    set(stslot_loc(stsl), newval);
  } else if (stsl != 0 && inh_stsl != 0) {
    return newval;
  } else if (stsl != 0 && stsl->home_type == stype) {
    set(stslot_loc(stsl), newval);
    return newval;
  } else if (stsl == 0 && memq(sym, st->slots)) {
    if (!no_error_p)
      uw_throwf(error_s, lit("~a: ~s is an instance slot of ~s"),
                self, sym, stype, nao);
    return newval;
  } else if (stsl != 0) {
    struct stslot to;
    to.store = nil;
    to.home_type = stype;
    to.home = &stsl->store;
    to.home_offs = stsl - st->stslot;
    static_slot_rewrite_rec(st, stsl, &to);
    set(stslot_loc(stsl), newval);
    return newval;
  } else {
    struct stslot null_ptr = { nil, 0, 0, nil };

    if (st->nstslots >= NUM_MAX)
      uw_throwf(error_s, lit("~a: too many static slots"), self, nao);

    st->stslot = coerce(struct stslot *,
                        chk_manage_vec(coerce(mem_t *, st->stslot),
                                       st->nstslots, st->nstslots + 1,
                                       sizeof *st->stslot,
                                       coerce(mem_t *, &null_ptr)));
    static_slot_home_fixup_rec(st);
    set(mkloc(st->slots, stype), append2(st->slots, cons(sym, nil)));
    stsl = &st->stslot[st->nstslots];

    if (inh_stsl == 0) {
      stsl->home_type = stype;
      stsl->home_offs = st->nstslots;
      stsl->home = &stsl->store;
      set(stslot_loc(stsl), newval);
      if (!opt_compat || opt_compat > 151)
        inh_stsl = stsl;
    } else {
      stsl->store = nil;
      stsl->home_type = inh_stsl->home_type;
      stsl->home_offs = inh_stsl->home_offs;
      stsl->home = inh_stsl->home;
    }

    sethash(slot_hash, cons(sym, num_fast(st->id)),
            num(st->nstslots++ + STATIC_SLOT_BASE));
    static_slot_type_reg(sym, st->name);
  }

  {
    val iter;

    for (iter = st->dvtypes; iter; iter = cdr(iter))
      static_slot_ens_rec(car(iter), sym, newval, t, self, inh_stsl);

    return newval;
  }
}

val static_slot_ensure(val stype, val sym, val newval, val no_error_p)
{
  val self = lit("static-slot-ensure");

  if (!bindable(sym))
    uw_throwf(error_s, lit("~a: ~s isn't a valid slot name"),
              self, sym, nao);

  if (trace_loaded) {
    struct struct_type *st = stype_handle(&stype, self);
    val name = list(meth_s, st->name, sym, nao);
    trace_check(name);
  }

  no_error_p = default_null_arg(no_error_p);
  return static_slot_ens_rec(stype, sym, newval, no_error_p, self, 0);
}

val static_slot_home(val stype, val sym)
{
  val self = lit("static-slot-home");
  struct struct_type *st = stype_handle(&stype, self);
  struct stslot *stsl = lookup_static_slot_desc_load(st, sym);
  if (stsl) {
    val home = stsl->home_type;
    struct struct_type *sh = stype_handle(&home, self);
    return sh->name;
  }
  no_such_static_slot(self, stype, sym);
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
  val self = lit("call-super-fun");
  struct struct_type *st = stype_handle(&type, self);
  val suptype = st->super;

  if (suptype) {
    val fun = static_slot(suptype, sym);
    return generic_funcall(fun, args);
  }

  uw_throwf(error_s, lit("~a: ~s has no supertype"),
            self, type, nao);
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

val slots(val stype)
{
  struct struct_type *st = stype_handle(&stype, lit("slots"));
  return st->slots;
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

static val method_args_fun(val env, varg args)
{
  cons_bind (curried_args, fun_strct, env);
  cons_bind (fun, strct, fun_strct);
  cnum ca_len = c_num(length(curried_args));
  args_decl(args_call, max(args->fill + 1 + ca_len, ARGS_MIN));
  args_add(args_call, strct);
  args_add_list(args_call, curried_args);
  args_normalize(args_call, ca_len + 1);
  args_cat_zap(args_call, args);
  return generic_funcall(fun, args_call);
}

val method(val strct, val slotsym)
{
  return func_f0v(cons(slot(strct, slotsym), strct), method_fun);
}

val method_args(val strct, val slotsym, struct args *args)
{
  if (!args_more(args, 0))
    return func_f0v(cons(slot(strct, slotsym), strct), method_fun);
  else
    return func_f0v(cons(args_get_list(args),
                         cons(slot(strct, slotsym), strct)), method_args_fun);
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

  if (sym && symbolp(sym)) {
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

  if (!args_more(args, 0)) {
    uw_throwf(error_s, lit("~a: object argument required to call ~s"),
              self, env, nao);
  } else {
    val strct = args_at(args, 0);

    struct struct_inst *si = struct_handle(strct, self);

    if (sym && symbolp(sym)) {
      loc ptr = lookup_slot(strct, si, sym);
      if (!nullocp(ptr))
        return generic_funcall(deref(ptr), args);
    }

    no_such_slot(self, si->type->self, sym);
  }
}

static val umethod_args_fun(val env, struct args *args)
{
  val self = lit("umethod");
  cons_bind (sym, curried_args, env);

  if (!args_more(args, 0)) {
    uw_throwf(error_s, lit("~a: object argument required to call ~s"),
              self, env, nao);
  } else {
    cnum ca_len = c_num(length(curried_args));
    cnum index = 0;
    val strct = args_get(args, &index);
    args_decl(args_call, max(args->fill + ca_len, ARGS_MIN));
    args_add(args_call, strct);
    args_add_list(args_call, curried_args);
    args_normalize(args_call, ca_len + 1);
    args_cat_zap_from(args_call, args, index);

    struct struct_inst *si = struct_handle(strct, self);

    if (sym && symbolp(sym)) {
      loc ptr = lookup_slot(strct, si, sym);
      if (!nullocp(ptr))
        return generic_funcall(deref(ptr), args_call);
    }

    no_such_slot(self, si->type->self, sym);
  }
}

val umethod(val slot, struct args *args)
{
  if (!args_more(args, 0))
    return func_f0v(slot, umethod_fun);
  else
    return func_f0v(cons(slot, args_get_list(args)), umethod_args_fun);
}

static void struct_inst_print(val obj, val out, val pretty,
                              struct strm_ctx *ctx)
{
  struct struct_inst *si = coerce(struct struct_inst *, obj->co.handle);
  struct struct_type *st = si->type;
  val save_mode = test_set_indent_mode(out, num_fast(indent_off),
                                       num_fast(indent_data));
  val save_indent, iter, once;
  int compat = opt_compat && opt_compat <= 154;

  if (!compat || pretty) {
    loc ptr = lookup_static_slot_load(st, print_s);
    if (!nullocp(ptr)) {
      if (compat)
        funcall2(deref(ptr), obj, out);
      else if (funcall3(deref(ptr), obj, out, pretty) != colon_k)
        return;
    }
  }

  put_string(lit("#S("), out);
  obj_print_impl(st->name, out, pretty, ctx);
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
      obj_print_impl(sym, out, pretty, ctx);
      put_char(chr(' '), out);
      obj_print_impl(slot(obj, sym), out, pretty, ctx);
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

static cnum struct_inst_hash(val obj, int *count)
{
  struct struct_inst *si = coerce(struct struct_inst *, obj->co.handle);
  struct struct_type *st = si->type;
  cnum nslots = st->nslots, sl, out = equal_hash(st->self, count);

  check_init_lazy_struct(obj, si);

  for (sl = 0; sl < nslots; sl++) {
    cnum hash = equal_hash(si->slot[sl], count);
    out += hash;
    out &= NUM_MAX;
  }

  return out;
}

static val get_equal_method(struct struct_type *st)
{
  if (st->eqmslot == coerce(struct stslot *, -1)) {
    return nil;
  } else if (st->eqmslot) {
    struct stslot *stsl = st->eqmslot;
    return stslot_place(stsl);
  } else {
    struct stslot *stsl = lookup_static_slot_desc(st, equal_s);
    if (stsl != 0) {
      st->eqmslot = stsl;
      return stslot_place(stsl);
    }
    st->eqmslot = coerce(struct stslot *, -1);
    return nil;
  }
}

static val struct_inst_equalsub(val obj)
{
  struct struct_inst *si = coerce(struct struct_inst *, obj->co.handle);
  struct struct_type *st = si->type;
  val equal_method = get_equal_method(st);
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
      loc ptr = lookup_static_slot(st, slot);

      if (!nullocp(ptr) && deref(ptr) == fun) {
        val sstype;

        while ((sstype = super(stype)) != nil) {
          struct struct_type *sst = coerce(struct struct_type *,
                                           sstype->co.handle);
          loc sptr = lookup_static_slot(sst, slot);
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

val get_slot_syms(val package, val is_current, val method_only)
{
  val result_hash = make_hash(nil, nil, nil);
  val sth_iter = hash_begin(struct_type_hash);
  val sth_cell;

  while ((sth_cell = hash_next(sth_iter))) {
    val stype = cdr(sth_cell);
    val sl_iter;
    struct struct_type *st = coerce(struct struct_type *, stype->co.handle);

    for (sl_iter = st->slots; sl_iter; sl_iter = cdr(sl_iter)) {
      val slot = car(sl_iter);

      if (gethash(result_hash, slot))
        continue;

      if (!is_current && symbol_package(slot) != package)
        continue;

      if (!symbol_visible(package, slot))
        continue;

      if (method_only) {
        loc ptr = lookup_static_slot(st, slot);
        if (nullocp(ptr))
          continue;
        if (!functionp(deref(ptr)))
          continue;
      }

      sethash(result_hash, slot, t);
    }
  }

  return result_hash;
}

val slot_types(val slot)
{
  return gethash(slot_type_hash, slot);
}

val static_slot_types(val slot)
{
  return gethash(static_slot_type_hash, slot);
}

val slot_type_reg(val slot, val strct)
{
  val typelist = gethash(slot_type_hash, slot);

  if (!memq(strct, typelist)) {
    sethash(slot_type_hash, slot, cons(strct, typelist));
    uw_purge_deferred_warning(cons(slot_s, slot));
  }

  return slot;
}

val static_slot_type_reg(val slot, val strct)
{
  val typelist = gethash(static_slot_type_hash, slot);

  if (!memq(strct, typelist)) {
    sethash(slot_type_hash, slot, cons(strct, typelist));
    uw_purge_deferred_warning(cons(static_slot_s, slot));
    uw_purge_deferred_warning(cons(slot_s, slot));
  }

  return slot;
}

static_def(struct cobj_ops struct_type_ops =
           cobj_ops_init(eq, struct_type_print, struct_type_destroy,
                         struct_type_mark, cobj_eq_hash_op))

static_def(struct cobj_ops struct_inst_ops =
           cobj_ops_init_ex(struct_inst_equal, struct_inst_print,
                            cobj_destroy_free_op, struct_inst_mark,
                            struct_inst_hash, struct_inst_equalsub))
