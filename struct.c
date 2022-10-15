/* Copyright 2015-2022
 * Kaz Kylheku <kaz@kylheku.com>
 * Vancouver, Canada
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <stdlib.h>
#include <limits.h>
#include <signal.h>
#include <assert.h>
#include "config.h"
#include "alloca.h"
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
#include "autoload.h"
#include "struct.h"

#define max(a, b) ((a) > (b) ? (a) : (b))
#define nelem(array) (sizeof (array) / sizeof (array)[0])
#define uptopow2_0(v) ((v) - 1)
#define uptopow2_1(v) (uptopow2_0(v) | uptopow2_0(v) >> 1)
#define uptopow2_2(v) (uptopow2_1(v) | uptopow2_1(v) >> 2)
#define uptopow2_3(v) (uptopow2_2(v) | uptopow2_2(v) >> 4)
#define uptopow2_4(v) (uptopow2_3(v) | uptopow2_3(v) >> 8)
#define uptopow2_5(v) (uptopow2_4(v) | uptopow2_4(v) >> 16)
#define uptopow2(v) (uptopow2_5(v) + 1)

#define STATIC_SLOT_BASE 0x10000000

#define SLOT_CACHE_SIZE 8

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
  cnum nsupers;
  cnum ndsupers;
  val supers;
  struct struct_type **sus;
  struct struct_type **dsus;
  val slots;
  val stinitfun;
  val initfun;
  val boactor;
  val postinitfun;
  val dvtypes;
  struct stslot *stslot;
  struct stslot **spslot;
  unsigned dupe : 1;
};

struct struct_inst {
  struct struct_type *type;
  cnum id : NUM_BIT;
  unsigned lazy : 1;
  unsigned dirty : 1;
  val slot[FLEX_ARRAY];
};

val struct_type_s, meth_s, print_s, make_struct_lit_s;
val init_k, postinit_k;
val slot_s, derived_s;

val nullify_s, from_list_s, lambda_set_s;

val iter_begin_s, iter_more_s, iter_item_s, iter_step_s, iter_reset_s;

static val *special_sym[num_special_slots] = {
  &equal_s, &nullify_s, &from_list_s, &lambda_s, &lambda_set_s,
  &length_s, &car_s, &cdr_s, &rplaca_s, &rplacd_s,
  &iter_begin_s, &iter_more_s, &iter_item_s, &iter_step_s, &iter_reset_s
};

static struct cobj_class *struct_type_cls;
struct cobj_class *struct_cls;

static val struct_type_hash;
static val slot_hash;
static val struct_type_finalize_f;
static val slot_type_hash;
static val static_slot_type_hash;

static val struct_type_finalize(val obj);
static_forward(struct cobj_ops struct_type_ops);

static struct stslot *lookup_static_slot_desc(struct struct_type *st, val sym);
static val make_struct_type_compat(val name, val super,
                                   val slots, val initfun, val boactor);
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
  init_k = intern(lit("init"), keyword_package);
  postinit_k = intern(lit("postinit"), keyword_package);
  slot_s = intern(lit("slot"), user_package);
  derived_s = intern(lit("derived"), user_package);
  nullify_s = intern(lit("nullify"), user_package);
  from_list_s = intern(lit("from-list"), user_package);
  lambda_set_s = intern(lit("lambda-set"), user_package);
  iter_begin_s = intern(lit("iter-begin"), user_package);
  iter_more_s = intern(lit("iter-more"), user_package);
  iter_item_s = intern(lit("iter-item"), user_package);
  iter_step_s = intern(lit("iter-step"), user_package);
  iter_reset_s = intern(lit("iter-reset"), user_package);

  struct_type_cls = cobj_register(struct_type_s);
  struct_cls = cobj_register(struct_s);

  struct_type_hash = make_hash(hash_weak_none, nil);
  slot_hash = make_hash(hash_weak_none, t);
  slot_type_hash = make_hash(hash_weak_none, nil);
  static_slot_type_hash = make_hash(hash_weak_none, nil);
  struct_type_finalize_f = func_n1(struct_type_finalize);

  reg_fun(intern(lit("make-struct-type"), user_package),
          func_n8o(make_struct_type, 7));

  reg_fun(intern(lit("make-struct-type"), system_package),
          func_n8(make_struct_type));
  reg_fun(intern(lit("find-struct-type"), user_package),
          func_n1(find_struct_type));
  reg_fun(intern(lit("struct-type-p"), user_package), func_n1(struct_type_p));
  reg_fun(intern(lit("struct-get-initfun"), user_package), func_n1(struct_get_initfun));
  reg_fun(intern(lit("struct-set-initfun"), user_package), func_n2(struct_set_initfun));
  reg_fun(intern(lit("struct-get-postinitfun"), user_package), func_n1(struct_get_postinitfun));
  reg_fun(intern(lit("struct-set-postinitfun"), user_package), func_n2(struct_set_postinitfun));
  reg_fun(intern(lit("super"), user_package), func_n2o(super, 1));
  reg_fun(intern(lit("make-struct"), user_package), func_n2v(make_struct));
  reg_fun(intern(lit("struct-from-plist"), user_package), func_n1v(struct_from_plist));
  reg_fun(intern(lit("struct-from-args"), user_package), func_n1v(struct_from_args));
  reg_fun(intern(lit("make-lazy-struct"), user_package),
          func_n2(make_lazy_struct));
  reg_fun(make_struct_lit_s, func_n2(make_struct_lit));
  reg_fun(intern(lit("allocate-struct"), user_package), func_n1(allocate_struct));
  reg_fun(intern(lit("copy-struct"), user_package), func_n1(copy_struct));
  reg_fun(intern(lit("replace-struct"), user_package), func_n2(replace_struct));
  reg_fun(intern(lit("clear-struct"), user_package), func_n2o(clear_struct, 1));
  reg_fun(intern(lit("reset-struct"), user_package), func_n1(reset_struct));
  reg_fun(slot_s, func_n2(slot));
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
  reg_fun(intern(lit("static-slot-p"), user_package), func_n2(static_slot_p));
  reg_fun(intern(lit("structp"), user_package), func_n1(structp));
  reg_fun(intern(lit("struct-type"), user_package), func_n1(struct_type));
  reg_fun(intern(lit("struct-type-name"), user_package), func_n1(struct_type_name));
  reg_fun(intern(lit("method"), user_package), func_n2v(method_args));
  reg_fun(intern(lit("super-method"), user_package), func_n2(super_method));
  reg_fun(intern(lit("uslot"), user_package), func_n1(uslot));
  reg_fun(intern(lit("umethod"), user_package), func_n1v(umethod));
  reg_fun(intern(lit("slots"), user_package), func_n1(slots));
  reg_fun(intern(lit("slot-types"), system_package), func_n1(slot_types));
  reg_fun(intern(lit("static-slot-types"), system_package), func_n1(static_slot_types));
}

void struct_compat_fixup(int compat_ver)
{
  if (compat_ver <= 118)
    reg_fun(intern(lit("slot-p"), user_package), func_n2(slotp));

  if (compat_ver <= 117)
    reg_fun(intern(lit("make-struct-type"), user_package),
            func_n5(make_struct_type_compat));
}

static NORETURN void no_such_struct(val ctx, val sym)
{
  uw_throwf(error_s, lit("~a: ~s does not name a struct type"),
            ctx, sym, nao);
}

static val struct_type_finalize(val obj)
{
  struct struct_type *st = coerce(struct struct_type *, obj->co.handle);
  val id = num(st->id);
  val iter;

  for (iter = st->slots; iter; iter = cdr(iter)) {
    val slot = car(iter);
    slot_cache_set_t *slot_cache = slot->s.slot_cache;
    int i, j;

    remhash(slot_hash, cons(slot, id));

    if (slot_cache != 0)
      for (i = 0; i < SLOT_CACHE_SIZE; i++)
        for (j = 0; j < 4; j++)
          if (slot_cache[i][j].id == st->id) {
            slot_cache[i][j].id = 0;
            slot_cache[i][j].slot = 0;
          }
  }

  return nil;
}

static void call_stinitfun_chain(struct struct_type *st, val stype)
{
  if (st) {
    if (st->nsupers == 1 && opt_compat && opt_compat <= 151)
      call_stinitfun_chain(st->sus[0], stype);
    if (st->stinitfun)
      funcall1(st->stinitfun, stype);
  }
}

static struct struct_inst *struct_handle(val obj, val ctx)
{
  if (cobjp(obj) && obj->co.ops == &struct_inst_ops)
    return coerce(struct struct_inst *, obj->co.handle);
  uw_throwf(error_s, lit("~a: ~s isn't a structure"),
            ctx, obj, nao);
}

static struct struct_type *stype_handle_impl(val *pobj, val obj_ok, val ctx)
{
  val obj = *pobj;

  switch (type(obj)) {
  case SYM:
    {
      val stype = find_struct_type(obj);
      if (!stype)
        no_such_struct(ctx, obj);
      *pobj = stype;
      return coerce(struct struct_type *, cobj_handle(ctx, stype,
                                                      struct_type_cls));
    }
  case COBJ:
    if (obj->co.cls == struct_type_cls)
      return coerce(struct struct_type *, obj->co.handle);
    if (obj_ok && obj->co.cls == struct_cls)
      return struct_handle(obj, ctx)->type;
    /* fallthrough */
  default:
    uw_throwf(error_s, lit("~a: ~s isn't a struct type"),
              ctx, obj, nao);
  }
}

static struct struct_type *stype_handle(val *pobj, val ctx)
{
  return stype_handle_impl(pobj, nil, ctx);
}

static struct struct_type *stype_handle_obj(val *pobj, val ctx)
{
  return stype_handle_impl(pobj, t, ctx);
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

static val get_all_supers(val supers, val self)
{
  list_collect_decl (all_supers, ptail);

  ptail = list_collect_append(ptail, supers);

  for (; supers; supers = us_cdr(supers)) {
    val super = us_car(supers);
    struct struct_type *su = stype_handle(&super, self);
    int i;

    ptail = list_collect_append(ptail, su->supers);

    for (i = 0; i < su->nsupers; i++) {
      struct struct_type *ssu = su->sus[i];
      ptail = list_collect_append(ptail, get_all_supers(ssu->supers, self));
    }
  }

  return all_supers;
}

static val get_duplicate_supers(val supers, val self)
{
  list_collect_decl (dup_supers, ptail);
  val all_supers = get_all_supers(supers, self);
  ucnum bloom = 0;
  val iter;

  for (iter = all_supers; iter; iter = us_cdr(iter)) {
    val super = us_car(iter);
    struct struct_type *st = stype_handle(&super, self);
    int pos = st->id % (sizeof bloom * CHAR_BIT);
    ucnum mask = convert(ucnum, 1) << pos;

    if ((mask & bloom) != 0) {
      if (memq(super, all_supers) != iter && !memq(super, dup_supers)) {
        ptail = list_collect(ptail, super);
        st->dupe = 1;
      }
    }

    bloom |= mask;
  }

  return dup_supers;
}

static struct struct_type **get_struct_handles(cnum nsupers, val supers,
                                               val self)
{
  cnum i;
  struct struct_type **sus = coerce(struct struct_type **,
                                    chk_malloc(nsupers * sizeof *sus));
  for (i = 0; i < nsupers; i++, supers = us_cdr(supers)) {
    val super = us_car(supers);
    sus[i] = stype_handle(&super, self);
  }

  return sus;
}

static cnum count_super_stslots(cnum nsupers, struct struct_type **sus,
                                val self)
{
  cnum c = 0, i;

  for (i = 0; i < nsupers; i++) {
    struct struct_type *s = sus[i];
    if (c > INT_PTR_MAX - s->nstslots)
      uw_throwf(error_s, lit("~a: too many static slots among supertypes"),
                self, nao);
    c += s->nstslots;
  }

  return c;
}

static val get_super_slots(cnum nsupers, struct struct_type **sus)
{
  cnum i;
  val slots = nil;

  for (i = 0; i < nsupers; i++)
    slots = append2(slots, sus[i]->slots);

  return slots;
}

static struct struct_type *find_super_for_slot(cnum nsupers,
                                               struct struct_type **sus,
                                               val slot)
{
  cnum i;

  for (i = 0; i < nsupers; i++) {
    if (memq(slot, sus[i]->slots))
      return sus[i];
  }

  return 0;
}

val make_struct_type(val name, val supers,
                     val static_slots, val slots,
                     val static_initfun, val initfun, val boactor,
                     val postinitfun)
{
  val self = lit("make-struct-type");
  val iter;

  autoload_try_struct(name);

  if (built_in_type_p(name))
    uw_throwf(error_s, lit("~a: ~s is a built-in type"),
              self, name, nao);

  if (!listp(supers))
    supers = cons(supers, nil);

  {
    list_collect_decl (stypes, ptail);

    for (iter = supers; iter; iter = cdr(iter)) {
      val super = car(iter);
      if (symbolp(super)) {
        val supertype = find_struct_type(super);
        if (!supertype)
          no_such_struct(self, super);
        ptail = list_collect(ptail, supertype);
      } else {
        class_check(self, super, struct_type_cls);
        ptail = list_collect(ptail, super);
      }
    }

    supers = stypes;
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
  } else {
    struct struct_type *st = coerce(struct struct_type *,
                                    chk_calloc(1, sizeof *st));
    val dup_supers = if3(opt_compat && opt_compat <= 242,
                         nil, get_duplicate_supers(supers, self));
    cnum nsupers = c_num(length(supers), self);
    cnum ndsupers = c_num(length(dup_supers), self);
    struct struct_type **sus = get_struct_handles(nsupers, supers, self);
    struct struct_type **dsus = get_struct_handles(ndsupers, dup_supers, self);
    val id = num_fast(coerce(ucnum, st) / (uptopow2(sizeof *st) / 2));
    val super_slots = get_super_slots(nsupers, sus);
    val all_slots = uniq(append2(super_slots, append2(static_slots, slots)));
    cnum stsl_upb = c_num(plus(length(static_slots),
                               num(count_super_stslots(nsupers, sus, self))),
                          self);
    val stype = cobj(coerce(mem_t *, st), struct_type_cls, &struct_type_ops);
    val iter;
    cnum sl, stsl, i;
    struct stslot null_ptr = { nil, 0, 0, nil };

    st->self = stype;
    st->name = name;
    st->id = c_num(id, self);
    st->slots = all_slots;
    st->nsupers = nsupers;
    st->ndsupers = ndsupers;
    st->supers = supers;
    st->sus = sus;
    st->dsus = dsus;
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
      struct struct_type *su = find_super_for_slot(nsupers, sus, slot);
      val super = if2(su, su->self);
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
        cnum m = c_n(msl) - STATIC_SLOT_BASE;

        if (!inherited_p || (opt_compat && opt_compat <= 151)) {
          ss->home_type = stype;
          ss->home_offs = n;
          ss->home = &ss->store;
          ss->store = if2(m >= 0, stslot_place(&su->stslot[m]));
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

    for (i = 0; i < nsupers; i++) {
      struct struct_type *su = sus[i];
      mpush(stype, mkloc(su->dvtypes, su->self));
    }

    call_stinitfun_chain(st, stype);

    uw_purge_deferred_warning(cons(struct_type_s, name));

    for (i = 0; i < nsupers; i++) {
      struct struct_type *su = sus[i];
      struct stslot *dvmeth = lookup_static_slot_desc(su, derived_s);
      if (dvmeth)
        funcall2(stslot_place(dvmeth), su->self, stype);
    }

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
             if2(autoload_try_struct(sym),
                 gethash(struct_type_hash, sym)));
}

val struct_type_p(val obj)
{
  return cobjclassp(obj, struct_type_cls);
}

val struct_get_initfun(val type)
{
  struct struct_type *st = stype_handle(&type, lit("struct-get-initfun"));
  return st->initfun;
}

val struct_set_initfun(val type, val fun)
{
  struct struct_type *st = stype_handle(&type, lit("struct-set-initfun"));
  set(mkloc(st->initfun, type), fun);
  return fun;
}

val struct_get_postinitfun(val type)
{
  struct struct_type *st = stype_handle(&type, lit("struct-get-postinitfun"));
  return st->postinitfun;
}

val struct_set_postinitfun(val type, val fun)
{
  struct struct_type *st = stype_handle(&type, lit("struct-set-postinitfun"));
  set(mkloc(st->postinitfun, type),  fun);
  return fun;
}

val super(val type, val idx)
{
  val self = lit("super");
  cnum ix = c_num(default_arg(idx, zero), self);

  if (ix < 0)
    uw_throwf(error_s,
              lit("~a: index must be non-negative, ~s given"),
              self, idx, nao);

  {
    struct struct_type *st = stype_handle_obj(&type, self);
    return if2(ix < st->nsupers, st->sus[ix]->self);
  }
}

static void struct_type_print(val obj, val out, val pretty, struct strm_ctx *c)
{
  struct struct_type *st = coerce(struct struct_type *, obj->co.handle);
  (void) pretty;
  (void) c;
  format(out, lit("#<struct-type ~s>"), st->name, nao);
}

static void struct_type_destroy(val obj)
{
  struct struct_type *st = coerce(struct struct_type *, obj->co.handle);
  free(st->stslot);
  free(st->spslot);
  free(st->sus);
  free(st->dsus);
  free(st);
}

static void struct_type_mark(val obj)
{
  struct struct_type *st = coerce(struct struct_type *, obj->co.handle);
  cnum stsl;

  gc_mark(st->name);
  gc_mark(st->supers);
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

static void call_initfun_chain(struct struct_type *st, val strct,
                               struct struct_type *root, ucnum *seen)
{
  if (st) {
    cnum i;

    if (st != root && st->dupe)
      for (i = 0; i < root->ndsupers; i++) {
        if (st == root->dsus[i]) {
          const int bits_ucnum = sizeof *seen * CHAR_BIT;
          cnum index = i / bits_ucnum;
          cnum bit = i % bits_ucnum;
          ucnum mask = convert(ucnum, 1) << bit;
          if ((seen[index] & mask) != 0)
            return;
          seen[index] |= mask;
        }
      }

    for (i = st->nsupers - 1; i >= 0; i--)
      call_initfun_chain(st->sus[i], strct, root, seen);
    if (st->initfun)
      funcall1(st->initfun, strct);
  }
}

static void call_postinitfun_chain(struct struct_type *st, val strct,
                                   struct struct_type *root, ucnum *seen)
{
  if (st) {
    int derived_first = (opt_compat && opt_compat <= 148);
    cnum i;

    if (st != root && st->dupe)
      for (i = 0; i < root->ndsupers; i++) {
        if (st == root->dsus[i]) {
          const int bits_ucnum = sizeof *seen * CHAR_BIT;
          cnum index = i / bits_ucnum;
          cnum bit = i % bits_ucnum;
          ucnum mask = convert(ucnum, 1) << bit;
          if ((seen[index] & mask) != 0)
            return;
          seen[index] |= mask;
        }
      }

    if (derived_first && st->postinitfun)
      funcall1(st->postinitfun, strct);
    for (i = st->nsupers - 1; i >= 0; i--)
      call_postinitfun_chain(st->sus[i], strct, root, seen);
    if (!derived_first && st->postinitfun)
      funcall1(st->postinitfun, strct);
  }
}

val allocate_struct(val type)
{
  val self = lit("allocate-struct");
  struct struct_type *st = stype_handle(&type, self);
  cnum nslots = st->nslots;
  size_t size = offsetof(struct struct_inst, slot) + sizeof (val) * nslots;
  struct struct_inst *si = coerce(struct struct_inst *, chk_calloc(1, size));
  si->type = st;
  si->id = st->id;
  si->lazy = 0;
  si->dirty = 1;
  bug_unless (type == st->self);
  return cobj(coerce(mem_t *, si), struct_cls, &struct_inst_ops);
}

#define alloc_seen(name, size_name)                                     \
  const int bits_ucnum = sizeof (ucnum) * CHAR_BIT;                     \
  size_t nelem_name = (st->ndsupers + bits_ucnum - 1) / bits_ucnum;     \
  size_t size_name = nelem_name * sizeof (ucnum);                       \
  ucnum *name ## tmp = coerce(ucnum *, alloca(size_name));              \
  ucnum *name = (memset(name ## tmp, 0, size_name), name ## tmp)

#define clear_seen(name, size_name)                                     \
  memset(name, 0, size_name)

static val make_struct_impl(val self, val type,
                            struct args *plist, struct args *args)
{
  struct struct_type *st = stype_handle(&type, self);
  cnum nslots = st->nslots;
  size_t size = offsetof(struct struct_inst, slot) + sizeof (val) * nslots;
  struct struct_inst *si = coerce(struct struct_inst *, chk_calloc(1, size));
  val sinst;
  volatile val inited = nil;
  alloc_seen (seen, seensz);

  if (args_more(args, 0) && !st->boactor) {
    free(si);
    uw_throwf(error_s,
              lit("~a: args present, but ~s has no boa constructor"),
              self, type, nao);
  }

  si->type = st;
  si->id = st->id;
  si->dirty = 1;

  sinst = cobj(coerce(mem_t *, si), struct_cls, &struct_inst_ops);

  bug_unless (type == st->self);

  uw_simple_catch_begin;

  call_initfun_chain(st, sinst, st, seen);

  {
    cnum index = 0;
    while (args_more(plist, index)) {
      val slot = args_get(plist, &index);
      val value = args_get(plist, &index);
      slotset(sinst, slot, value);
    }
  }

  if (args_more(args, 0)) {
    args_decl(args_copy, max(args->fill + 1, ARGS_MIN));
    args_add(args_copy, sinst);
    args_cat_zap(args_copy, args);
    generic_funcall(st->boactor, args_copy);
  }

  clear_seen(seen, seensz);

  call_postinitfun_chain(st, sinst, st, seen);

  inited = t;

  uw_unwind {
    if (!inited)
      gc_call_finalizers(sinst);
  }

  uw_catch_end;

  return sinst;
}

val make_struct(val type, val plist, struct args *boa)
{
  args_decl_list(pargs, ARGS_MIN, plist);
  return make_struct_impl(lit("make-struct"), type, pargs, boa);
}

val struct_from_plist(val type, struct args *plist)
{
  args_decl_constsize(boa, ARGS_ABS_MIN);
  return make_struct_impl(lit("struct-from-plist"), type, plist, boa);
}

val struct_from_args(val type, struct args *boa)
{
  args_decl_constsize(pargs, ARGS_ABS_MIN);
  return make_struct_impl(lit("struct-from-args"), type, pargs, boa);
}

static void lazy_struct_init(val sinst, struct struct_inst *si)
{
  val self = lit("make-lazy-struct");
  struct struct_type *st = si->type;
  volatile val inited = nil;
  val cell = funcall(si->slot[0]);
  cons_bind (plist, args, cell);
  alloc_seen (seen, seensz);

  si->slot[0] = nil;

  if (args && !st->boactor) {
    uw_throwf(error_s,
              lit("~a: args present, but ~s has no boa constructor"),
              self, type, nao);
  }

  uw_simple_catch_begin;

  call_initfun_chain(st, sinst, st, seen);

  for (; plist; plist = cddr(plist))
    slotset(sinst, car(plist), cadr(plist));

  if (args) {
    args_decl_list(argv, ARGS_MIN, cons(sinst, args));
    generic_funcall(st->boactor, argv);
  }

  clear_seen(seen, seensz);

  call_postinitfun_chain(st, sinst, st, seen);

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
  cnum nslots = st->nslots;
  cnum nalloc = nslots ? nslots : 1;
  size_t size = offsetof(struct struct_inst, slot) + sizeof (val) * nalloc;
  struct struct_inst *si = coerce(struct struct_inst *, chk_calloc(1, size));
  val sinst;

  si->type = st;
  si->id = st->id;
  si->lazy = 1;
  si->dirty = 1;

  sinst = cobj(coerce(mem_t *, si), struct_cls, &struct_inst_ops);

  bug_unless (type == st->self);

  si->slot[0] = argfun;

  return sinst;
}

val make_struct_lit(val type, val plist)
{
  args_decl_constsize(args, ARGS_ABS_MIN);
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

static struct struct_inst *struct_handle_for_slot(val obj, val ctx, val slot)
{
  if (cobjp(obj) && obj->co.ops == &struct_inst_ops)
    return coerce(struct struct_inst *, obj->co.handle);
  uw_throwf(error_s, lit("~a: attempt to access slot ~s of non-structure ~s"),
            ctx, slot, obj, nao);
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
  copy = cobj(coerce(mem_t *, si_copy), struct_cls, &struct_inst_ops);
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

  setcheck(strct, clear_val);

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
  volatile val inited = nil;
  int compat_190 = opt_compat && opt_compat <= 190;
  alloc_seen (seen, seensz);

  check_init_lazy_struct(strct, si);

  uw_simple_catch_begin;

  for (i = 0; i < st->nslots; i++)
    si->slot[i] = nil;

  call_initfun_chain(st, strct, st, seen);

  if (!compat_190) {
    clear_seen(seen, seensz);
    call_postinitfun_chain(st, strct, st, seen);
  }

  inited = t;

  uw_unwind {
    if (!inited && !compat_190)
      gc_call_finalizers(strct);
  }

  uw_catch_end;

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
  slot_cache_set_t *slot_cache = sym->s.slot_cache;
  cnum id = si->id;

  if (slot_cache == 0) {
    slot_cache = coerce(slot_cache_set_t *,
                        chk_calloc(SLOT_CACHE_SIZE,
                                   sizeof (slot_cache_set_t)));
    sym->s.slot_cache = slot_cache;
  }

  {
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
      cnum slnum = c_n(sl);

      rcyc_cons(key);

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
  }

  return nulloc;
}

static struct stslot *lookup_static_slot_desc(struct struct_type *st, val sym)
{
  slot_cache_set_t *slot_cache = sym->s.slot_cache;
  cnum id = st->id;

  if (slot_cache != 0) {
    slot_cache_set_t *set = &slot_cache[id % SLOT_CACHE_SIZE];
    cnum slot = cache_set_lookup(*set, id);

    if (slot >= STATIC_SLOT_BASE) {
      return &st->stslot[slot - STATIC_SLOT_BASE];
    } else if (slot < 0) {
      val key = cons(sym, num_fast(id));
      val sl = gethash(slot_hash, key);
      cnum slnum = c_n(sl);

      rcyc_cons(key);

      if (sl) {
        cache_set_insert(*set, id, slnum);
        if (slnum >= STATIC_SLOT_BASE)
          return &st->stslot[slnum - STATIC_SLOT_BASE];
      }
    }
  } else {
    slot_cache_set_t *slot_cache = coerce(slot_cache_set_t *,
                                          chk_calloc(SLOT_CACHE_SIZE,
                                                     sizeof *slot_cache));
    slot_cache_set_t *set = &slot_cache[id % SLOT_CACHE_SIZE];
    val key = cons(sym, num_fast(id));
    val sl = gethash(slot_hash, key);
    cnum slnum = c_n(sl);

    sym->s.slot_cache = slot_cache;

    rcyc_cons(key);

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
    autoload_try_slot(sym);
    return lookup_slot(inst, si, sym);
  }
  return ptr;
}

static loc lookup_static_slot_load(struct struct_type *st, val sym)
{
  loc ptr = lookup_static_slot(st, sym);
  if (nullocp(ptr)) {
    autoload_try_slot(sym);
    return lookup_static_slot(st, sym);
  }
  return ptr;
}

static struct stslot *lookup_static_slot_desc_load(struct struct_type *st,
                                                   val sym)
{
  struct stslot *stsl = lookup_static_slot_desc(st, sym);
  if (stsl == 0) {
    autoload_try_slot(sym);
    return lookup_static_slot_desc(st, sym);
  }
  return stsl;
}

static NORETURN void no_such_slot(val ctx, val type, val slot)
{
  uw_throwf(error_s, lit("~a: ~s has no slot named ~s"),
            ctx, type, slot, nao);
}

static NORETURN void no_such_static_slot(val ctx, val type, val slot)
{
  uw_throwf(error_s, lit("~a: ~s has no static slot named ~s"),
            ctx, type, slot, nao);
}

val slot(val strct, val sym)
{
  const val self = lit("slot");
  struct struct_inst *si = struct_handle_for_slot(strct, self, sym);

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
  struct struct_inst *si = struct_handle_for_slot(strct, self, sym);

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
  struct struct_inst *si = struct_handle_for_slot(strct, self, sym);

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

static void invalidate_special_slot_nonexistence(struct struct_type *st)
{
  if (st->spslot != 0) {
    int i;
    for (i = 0; i < num_special_slots; i++) {
      if (st->spslot[i] == coerce(struct stslot *, -1))
        st->spslot[i] = 0;
    }
  }
}

val static_slot_set(val stype, val sym, val newval)
{
  val self = lit("static-slot-set");
  struct struct_type *st = stype_handle(&stype, self);

  if (symbolp(sym)) {
    loc ptr = lookup_static_slot(st, sym);
    if (!nullocp(ptr)) {
      invalidate_special_slot_nonexistence(st);
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

  invalidate_special_slot_nonexistence(st);

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
  val res;

  if (!bindable(sym))
    uw_throwf(error_s, lit("~a: ~s isn't a valid slot name"),
              self, sym, nao);

  if (trace_loaded) {
    struct struct_type *st = stype_handle(&stype, self);
    val name = list(meth_s, st->name, sym, nao);
    trace_check(name);
  }

  no_error_p = default_null_arg(no_error_p);
  res = static_slot_ens_rec(stype, sym, newval, no_error_p, self, 0);

  return res;
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

static val do_super(struct struct_type *st,
                    val inst, val sym, val self,
                    struct args *args)
{
  val type = st->self;
  cnum i;

  for (i = 0; i < st->nsupers; i++) {
    struct struct_type *su = st->sus[i];
    loc ptr = lookup_static_slot_load(su, sym);
    if (!nullocp(ptr)) {
      val meth = deref(ptr);
      if (inst == t) {
        return meth;
      } else if (inst) {
        args_decl(args_copy, max(args->fill + 1, ARGS_MIN));
        args_add(args_copy, inst);
        args_cat_zap(args_copy, args);
        return generic_funcall(meth, args_copy);
      } else {
        return generic_funcall(meth, args);
      }
    }
  }

  if (st->nsupers)
    if (bindable(sym))
      uw_throwf(error_s, lit("~s: slot ~s not found among supertypes of ~s"),
                self, sym, type, nao);
    else
      uw_throwf(error_s, lit("~s: ~s isn't a valid slot name"),
                self, sym);
  else
    uw_throwf(error_s, lit("~s: ~s has no supertype"),
              type, nao);
}

static val call_super_method(val inst, val sym, struct args *args)
{
  val type = struct_type(inst);
  val self = lit("call-super-method");
  struct struct_type *st = stype_handle(&type, self);
  return do_super(st, inst, sym, self, args);
}

static val call_super_fun(val type, val sym, struct args *args)
{
  val self = lit("call-super-fun");
  struct struct_type *st = stype_handle(&type, self);
  return do_super(st, nil, sym, self, args);
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
    cnum slnum = c_n(sl);

    rcyc_cons(key);

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

val struct_type_name(val stype)
{
  struct struct_type *st = stype_handle_obj(&stype, lit("struct-type-name"));
  return st->name;
}

static val do_struct_subtype_p(struct struct_type *sb,
                               struct struct_type *su,
                               val self)
{
  if (sb == su) {
    return t;
  } else {
    cnum i;
    for (i = 0; i < sb->nsupers; i++) {
      if (do_struct_subtype_p(sb->sus[i], su, self))
        return t;
    }

    return nil;
  }
}

val struct_subtype_p(val sub, val sup)
{
  const val self = lit("struct-subtype-p");
  struct struct_type *sb = stype_handle(&sub, self);
  struct struct_type *su = stype_handle(&sup, self);
  return do_struct_subtype_p(sb, su, self);
}

static val method_fun(val env, varg args)
{
  cons_bind (fun, strct, env);
  args_decl(args_copy, max(args->fill + 1, ARGS_MIN));
  args_add(args_copy, strct);
  args_cat_zap(args_copy, args);
  return generic_funcall(fun, args_copy);
}

static val method_args_fun(val dargs, varg args)
{
  val self = lit("method");
  struct args *da = dargs->a.args;
  val fun = dargs->a.car;
  val strct = dargs->a.cdr;
  cnum da_nargs = da->fill + c_num(length(da->list), self);
  args_decl(args_call, max(args->fill + 1 + da_nargs, ARGS_MIN));
  args_add(args_call, strct);
  args_cat(args_call, da);
  args_normalize_exact(args_call, da_nargs + 1);
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
    return func_f0v(dyn_args(args, slot(strct, slotsym), strct),
                    method_args_fun);
}

val super_method(val strct, val slotsym)
{
  val type = struct_type(strct);
  val self = lit("super-method");
  struct struct_type *st = stype_handle(&type, self);
  val meth = do_super(st, t, slotsym, self, 0);
  return func_f0v(cons(meth, strct), method_fun);
}

static val uslot_fun(val sym, val strct)
{
  val self = lit("uslot");
  struct struct_inst *si = struct_handle_for_slot(strct, self, sym);

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
              self, sym, nao);
  } else {
    val strct = args_at(args, 0);

    struct struct_inst *si = struct_handle_for_slot(strct, self, sym);

    if (sym && symbolp(sym)) {
      loc ptr = lookup_slot(strct, si, sym);
      if (!nullocp(ptr))
        return generic_funcall(deref(ptr), args);
    }

    no_such_slot(self, si->type->self, sym);
  }
}

static val umethod_args_fun(val dargs, struct args *args)
{
  val self = lit("umethod");
  val sym = dargs->a.car;
  struct args *da = dargs->a.args;

  if (!args_more(args, 0)) {
    uw_throwf(error_s, lit("~a: object argument required to call ~s"),
              self, sym, nao);
  } else {
    cnum da_nargs = da->fill + c_num(length(da->list), self);
    cnum index = 0;
    val strct = args_get(args, &index);
    struct struct_inst *si = struct_handle_for_slot(strct, self, sym);
    args_decl(args_call, max(args->fill + da_nargs, ARGS_MIN));
    args_add(args_call, strct);
    args_cat(args_call, da);
    args_normalize_exact(args_call, da_nargs + 1);
    args_cat_zap_from(args_call, args, index);

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
    return func_f0v(dyn_args(args, slot, nil), umethod_args_fun);
}

static void struct_inst_print(val obj, val out, val pretty,
                              struct strm_ctx *ctx)
{
  struct struct_inst *si = coerce(struct struct_inst *, obj->co.handle);
  struct struct_type *st = si->type;
  val save_mode = test_neq_set_indent_mode(out, num_fast(indent_foff),
                                           num_fast(indent_data));
  val save_indent, iter, once;
  int force_br = 0;
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
        if (width_check(out, chr(' ')))
          force_br = 1;
      }
      obj_print_impl(sym, out, pretty, ctx);
      put_char(chr(' '), out);
      obj_print_impl(slot(obj, sym), out, pretty, ctx);
    }
  }
  put_char(chr(')'), out);
  if (force_br)
    force_break(out);
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

static ucnum struct_inst_hash(val obj, int *count, ucnum seed)
{
  struct struct_inst *si = coerce(struct struct_inst *, obj->co.handle);
  struct struct_type *st = si->type;
  cnum nslots = st->nslots, sl;
  ucnum out = equal_hash(st->self, count, seed);

  check_init_lazy_struct(obj, si);

  for (sl = 0; sl < nslots; sl++) {
    ucnum hash = equal_hash(si->slot[sl], count, seed);
    out += hash;
    out &= NUM_MAX;
  }

  return out;
}

static val get_special_static_slot(struct struct_type *st,
                                   enum special_slot idx, val stslot)
{
  if (st->spslot == 0)
    st->spslot = coerce(struct stslot **,
                        chk_calloc(num_special_slots, sizeof *st->spslot));

  {
    struct stslot *spsl = st->spslot[idx];

    if (spsl == coerce(struct stslot *, -1)) {
      return nil;
    } else if (spsl) {
      return stslot_place(spsl);
    } else {
      struct stslot *stsl = lookup_static_slot_desc(st, stslot);
      if (stsl != 0) {
        st->spslot[idx] = stsl;
        return stslot_place(stsl);
      }
      st->spslot[idx] = coerce(struct stslot *, -1);
      return nil;
    }
  }
}

static val struct_inst_equalsub(val obj)
{
  struct struct_inst *si = coerce(struct struct_inst *, obj->co.handle);
  struct struct_type *st = si->type;
  val equal_method = get_special_static_slot(st, equal_m, equal_s);
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

static struct struct_type *ancestor_with_static_slot(struct struct_type *st,
                                                     val slot,
                                                     val value)
{
  cnum i;
  loc sptr = lookup_static_slot(st, slot);

  if (nullocp(sptr) || deref(sptr) != value)
    return 0;

  for (i = 0; i < st->nsupers; i++) {
    struct struct_type *sa = ancestor_with_static_slot(st->sus[i], slot, value);
    if (sa)
      return sa;
  }

  return st;
}

val method_name(val fun)
{
  struct hash_iter sthi;
  val sth_cell;

  us_hash_iter_init(&sthi, struct_type_hash);

  while ((sth_cell = hash_iter_next(&sthi))) {
    val sym = us_car(sth_cell);
    val stype = us_cdr(sth_cell);
    val sl_iter;
    struct struct_type *st = coerce(struct struct_type *, stype->co.handle);

    for (sl_iter = st->slots; sl_iter; sl_iter = cdr(sl_iter)) {
      val slot = car(sl_iter);
      struct struct_type *sa = ancestor_with_static_slot(st, slot, fun);
      if (sa)
        return list(meth_s, sa->name, slot, nao);
    }

    if (st->initfun == fun)
      return list(meth_s, sym, init_k, nao);

    if (st->postinitfun == fun)
      return list(meth_s, sym, postinit_k, nao);
  }

  return nil;
}

val slot_types(val slot)
{
  uses_or2;
  return or2(gethash(slot_type_hash, slot),
             if2(autoload_try_slot(slot),
                 gethash(slot_type_hash, slot)));
}

val static_slot_types(val slot)
{
  uses_or2;
  return or2(gethash(static_slot_type_hash, slot),
             if2(autoload_try_slot(slot),
                 gethash(static_slot_type_hash, slot)));
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
    sethash(static_slot_type_hash, slot, cons(strct, typelist));
    uw_purge_deferred_warning(cons(slot_s, slot));
  }

  return slot;
}

val get_special_slot(val obj, enum special_slot spidx)
{
  val slot = *special_sym[spidx];

  if (opt_compat && opt_compat <= 224) {
    return maybe_slot(obj, slot);
  } else {
    struct struct_inst *si = coerce(struct struct_inst *, obj->co.handle);
    return get_special_static_slot(si->type, spidx, slot);
  }
}

val get_special_required_slot(val obj, enum special_slot spidx)
{
  val content = get_special_slot(obj, spidx);
  if (content == nil) {
    val slot = *special_sym[spidx];
    uw_throwf(error_s, lit("~s is missing required ~s slot"),
              obj, slot, nao);
  }
  return content;
}

val get_special_slot_by_type(val stype, enum special_slot spidx)
{
  struct struct_type *st = coerce(struct struct_type *, stype->co.handle);
  val slot = *special_sym[spidx];
  return get_special_static_slot(st, spidx, slot);
}

static_def(struct cobj_ops struct_type_ops =
           cobj_ops_init(eq, struct_type_print, struct_type_destroy,
                         struct_type_mark, cobj_eq_hash_op));

struct cobj_ops struct_inst_ops =
  cobj_ops_init_ex(struct_inst_equal, struct_inst_print,
                   cobj_destroy_free_op, struct_inst_mark,
                   struct_inst_hash, struct_inst_equalsub);
