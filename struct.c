/* Copyright 2015
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
#include <setjmp.h>
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
  val name;
  cnum id;
  cnum nslots;
  cnum nstslots;
  val super;
  struct struct_type *super_handle;
  val slots;
  val stinitfun;
  val initfun;
  val boactor;
  val *stslot;
};

struct struct_inst {
  val type;
  cnum id;
  val slot[1];
};

val struct_type_s;

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
  struct_type_hash = make_hash(nil, nil, nil);
  slot_hash = make_hash(nil, nil, t);
  struct_type_finalize_f = func_n1(struct_type_finalize);

  if (opt_compat && opt_compat <= 117)
    reg_fun(intern(lit("make-struct-type"), user_package),
            func_n5(make_struct_type_compat));
  else
    reg_fun(intern(lit("make-struct-type"), user_package),
            func_n7(make_struct_type));

  reg_fun(intern(lit("make-struct-type"), system_package),
          func_n7(make_struct_type));
  reg_fun(intern(lit("find-struct-type"), user_package),
          func_n1(find_struct_type));
  reg_fun(intern(lit("struct-type-p"), user_package), func_n1(struct_type_p));
  reg_fun(intern(lit("super"), user_package), func_n1(super));
  reg_fun(intern(lit("make-struct"), user_package), func_n2v(make_struct));
  reg_fun(intern(lit("copy-struct"), user_package), func_n1(copy_struct));
  reg_fun(intern(lit("slot"), user_package), func_n2(slot));
  reg_fun(intern(lit("slotset"), user_package), func_n3(slotset));
  reg_fun(intern(lit("static-slot"), user_package), func_n2(static_slot));
  reg_fun(intern(lit("static-slot-set"), user_package),
          func_n3(static_slot_set));
  reg_fun(intern(lit("call-super-method"), user_package),
          func_n2v(call_super_method));
  reg_fun(intern(lit("call-super-fun"), user_package),
          func_n2v(call_super_fun));
  reg_fun(intern(lit("slot-p"), user_package), func_n2(slot_p));
  reg_fun(intern(lit("static-slot-p"), user_package), func_n2(static_slot_p));
  reg_fun(intern(lit("structp"), user_package), func_n1(structp));
  reg_fun(intern(lit("struct-type"), user_package), func_n1(struct_type));
  reg_fun(intern(lit("method"), user_package), func_n2(method));
  reg_fun(intern(lit("super-method"), user_package), func_n2(super_method));
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

val make_struct_type(val name, val super,
                     val static_slots, val slots,
                     val static_initfun, val initfun, val boactor)
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
    struct struct_type *su = if3(super,
                                 coerce(struct struct_type *,
                                        cobj_handle(super, struct_type_s)), 0);
    val super_slots = if2(su, su->slots);
    val all_slots = uniq(append2(super_slots, append2(static_slots, slots)));
    val stype = cobj(coerce(mem_t *, st), struct_type_s, &struct_type_ops);
    val id = num_fast(++struct_id_counter);
    val iter;
    cnum sl, stsl;

    st->name = name;
    st->id = c_num(id);
    st->nslots = st->nstslots = 0;
    st->slots = all_slots;
    st->super = super;
    st->stslot = 0;
    st->super_handle = su;
    st->stinitfun = static_initfun;
    st->initfun = initfun;
    st->boactor = boactor;

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
    st->stslot = coerce(val *, chk_calloc(stsl, sizeof *st->stslot));
    st->nslots = sl;
    st->nstslots = stsl;

    sethash(struct_type_hash, name, stype);

    call_stinitfun_chain(st, stype);

    return stype;
  }
}

static val make_struct_type_compat(val name, val super, val slots,
                                   val initfun, val boactor)
{
  return make_struct_type(name, super, nil, slots, nil, initfun, boactor);
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
  if (type && symbolp(type)) {
    val stype = find_struct_type(type);
    if (!stype)
      no_such_struct(lit("super"), type);
    return super(stype);
  } else {
    struct struct_type *st = coerce(struct struct_type *,
                                    cobj_handle(type, struct_type_s));
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

val make_struct(val type, val plist, struct args *args)
{
  val self = lit("make-struct");

  if (symbolp(type)) {
    val typeobj = gethash(struct_type_hash, type);
    if (!typeobj)
      uw_throwf(error_s, lit("~a: ~s doesn't name a struct type"),
                self, type, nao);
    return make_struct(typeobj, plist, args);
  } else {
    struct struct_type *st = coerce(struct struct_type *,
                                    cobj_handle(type, struct_type_s));
    cnum nslots = st->nslots, sl;
    size_t size = offsetof(struct struct_inst, slot) + sizeof (val) * nslots;
    struct struct_inst *si = coerce(struct struct_inst *, chk_malloc(size));
    val sinst;

    if (args_more(args, 0) && !st->boactor) {
      free(si);
      uw_throwf(error_s,
                lit("~a: args present, but ~s has no boa constructor"),
                self, type, nao);
    }

    for (sl = 0; sl < nslots; sl++)
      si->slot[sl] = nil;
    si->type = nil;
    si->id = st->id;


    sinst = cobj(coerce(mem_t *, si), st->name, &struct_inst_ops);

    si->type = type;

    call_initfun_chain(st, sinst);

    if (args_more(args, 0)) {
      args_decl(args_copy, max(args->fill + 1, ARGS_MIN));
      args_add(args_copy, sinst);
      args_cat_zap(args_copy, args);
      generic_funcall(st->boactor, args_copy);
    }

    for (; plist; plist = cddr(plist))
      slotset(sinst, car(plist), cadr(plist));

    return sinst;
  }
}

static struct struct_inst *struct_handle(val obj, val ctx)
{
  if (cobjp(obj) && obj->co.ops == &struct_inst_ops)
    return coerce(struct struct_inst *, obj->co.handle);
  no_such_struct(ctx, obj);
}

val copy_struct(val strct)
{
  const val self = lit("copy-struct");
  val copy;
  struct struct_inst *si = struct_handle(strct, self);
  struct struct_type *st = coerce(struct struct_type *, si->type->co.handle);
  cnum nslots = st->nslots;
  size_t size = offsetof(struct struct_inst, slot) + sizeof (val) * nslots;
  struct struct_inst *si_copy = coerce(struct struct_inst *, chk_malloc(size));
  memcpy(si_copy, si, size);
  copy = cobj(coerce(mem_t *, si_copy), st->name, &struct_inst_ops);
  gc_hint(strct);
  return copy;
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
      val type = si->type;
      struct struct_type *st = coerce(struct struct_type *, type->co.handle);
      return mkloc(st->stslot[slot - STATIC_SLOT_BASE], type);
    } else if (slot >= 0) {
      return mkloc(si->slot[slot], inst);
    } else {
      val key = cons(sym, num_fast(id));
      val sl = gethash(slot_hash, key);
      cnum slnum = coerce(cnum, sl) >> TAG_SHIFT;
      if (sl) {
        cache_set_insert(*set, id, slnum);
        if (slnum >= STATIC_SLOT_BASE) {
          val type = si->type;
          struct struct_type *st = coerce(struct struct_type *, type->co.handle);
          return mkloc(st->stslot[slnum - STATIC_SLOT_BASE], type);
        }
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
        val type = si->type;
        struct struct_type *st = coerce(struct struct_type *, type->co.handle);
        return mkloc(st->stslot[slnum - STATIC_SLOT_BASE], type);
      }
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

  no_such_slot(self, si->type, sym);
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

  no_such_slot(self, si->type, sym);
}

val static_slot(val stype, val sym)
{
  struct struct_type *st = coerce(struct struct_type *,
                                  cobj_handle(stype, struct_type_s));

  if (symbolp(sym)) {
    loc ptr = lookup_static_slot(stype, st, sym);
    if (!nullocp(ptr))
      return deref(ptr);
  }

  no_such_slot(lit("static-slot"), stype, sym);
}

val static_slot_set(val stype, val sym, val newval)
{
  struct struct_type *st = coerce(struct struct_type *,
                                  cobj_handle(stype, struct_type_s));

  if (symbolp(sym)) {
    loc ptr = lookup_static_slot(stype, st, sym);
    if (!nullocp(ptr))
      return set(ptr, newval);
  }

  no_such_slot(lit("static-slot-set"), stype, sym);
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

val slot_p(val type, val sym)
{
  if (type && symbolp(type)) {
    val stype = find_struct_type(type);
    if (!stype)
      no_such_struct(lit("slot-p"), type);
    return slot_p(stype, sym);
  } else {
    struct struct_type *st = coerce(struct struct_type *,
                                    cobj_handle(type, struct_type_s));
    return tnil(memq(sym, st->slots));
  }
}

val static_slot_p(val type, val sym)
{
  if (type && symbolp(type)) {
    val stype = find_struct_type(type);
    if (!stype)
      no_such_struct(lit("type-slot-p"), type);
    return static_slot_p(stype, sym);
  } else {
    struct struct_type *st = coerce(struct struct_type *,
                                    cobj_handle(type, struct_type_s));
    if (memq(sym, st->slots)) {
      val key = cons(sym, num_fast(st->id));
      val sl = gethash(slot_hash, key);
      cnum slnum = coerce(cnum, sl) >> TAG_SHIFT;

      if (sl && slnum >= STATIC_SLOT_BASE)
        return t;
    }

    return nil;
  }
}

val structp(val obj)
{
  return tnil(cobjp(obj) && obj->co.ops == &struct_inst_ops);
}

val struct_type(val strct)
{
  const val self = lit("struct-type");
  struct struct_inst *si = struct_handle(strct, self);
  return si->type;
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

static void struct_inst_print(val obj, val out, val pretty)
{
  struct struct_inst *si = coerce(struct struct_inst *, obj->co.handle);
  struct struct_type *st = coerce(struct struct_type *, si->type->co.handle);
  val save_mode = test_set_indent_mode(out, num_fast(indent_off),
                                       num_fast(indent_data));
  val save_indent, iter, once;

  put_string(lit("#S("), out);
  obj_print_impl(st->name, out, pretty);
  save_indent = inc_indent(out, one);

  for (iter = st->slots, once = t; iter; iter = cdr(iter)) {
    val sym = car(iter);
    if (!static_slot_p(si->type, sym)) {
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
  struct struct_type *st = coerce(struct struct_type *, si->type->co.handle);
  cnum sl, nslots = st->nslots;

  for (sl = 0; sl < nslots; sl++)
    gc_mark(si->slot[sl]);
  gc_mark(si->type);
}

static val struct_inst_equal(val left, val right)
{
  struct struct_inst *ls = coerce(struct struct_inst *, left->co.handle);
  struct struct_inst *rs = coerce(struct struct_inst *, right->co.handle);
  struct struct_type *st = coerce(struct struct_type *, ls->type->co.handle);
  cnum nslots = st->nslots, sl;

  if (rs->type != ls->type)
    return nil;

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
  struct struct_type *st = coerce(struct struct_type *, si->type->co.handle);
  cnum nslots = st->nslots, sl, out = c_num(hash_equal(si->type));

  for (sl = 0; sl < nslots; sl++) {
    val hash = hash_equal(si->slot[sl]);
    out += c_num(hash);
    out &= NUM_MAX;
  }

  return out;
}

static_def(struct cobj_ops struct_type_ops =
           cobj_ops_init(eq, struct_type_print, struct_type_destroy,
                         struct_type_mark, cobj_hash_op))

static_def(struct cobj_ops struct_inst_ops =
           cobj_ops_init(struct_inst_equal, struct_inst_print,
                         cobj_destroy_free_op, struct_inst_mark,
                         struct_inst_hash))
