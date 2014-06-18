/* Copyright 2009-2014
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
#include <string.h>
#include <wctype.h>
#include <limits.h>
#include <stdarg.h>
#include <dirent.h>
#include <setjmp.h>
#include <errno.h>
#include <wchar.h>
#include <math.h>
#include <time.h>
#include <signal.h>
#include <sys/time.h>
#include <assert.h>
#include "config.h"
#ifdef HAVE_GETENVIRONMENTSTRINGS
#define NOMINMAX
#include <windows.h>
#endif
#include "lib.h"
#include "gc.h"
#include "arith.h"
#include "rand.h"
#include "hash.h"
#include "signal.h"
#include "unwind.h"
#include "stream.h"
#include "utf8.h"
#include "filter.h"
#include "eval.h"
#include "regex.h"

#define max(a, b) ((a) > (b) ? (a) : (b))
#define min(a, b) ((a) < (b) ? (a) : (b))

#if !HAVE_POSIX_SIGS
int async_sig_enabled = 0;
#endif

val packages;

val system_package_var, keyword_package_var, user_package_var;
val system_package_s, keyword_package_s, user_package_s;

val null_s, t, cons_s, str_s, chr_s, fixnum_s, sym_s, pkg_s, fun_s, vec_s;
val lit_s, stream_s, hash_s, hash_iter_s, lcons_s, lstr_s, cobj_s, cptr_s;
val env_s, bignum_s, float_s;
val var_s, expr_s, regex_s, chset_s, set_s, cset_s, wild_s, oneplus_s;
val nongreedy_s, compiled_regex_s;
val quote_s, qquote_s, unquote_s, splice_s;
val sys_qquote_s, sys_unquote_s, sys_splice_s;
val zeroplus_s, optional_s, compl_s, compound_s;
val or_s, and_s, quasi_s, quasilist_s;
val skip_s, trailer_s, block_s, next_s, freeform_s, fail_s, accept_s;
val all_s, some_s, none_s, maybe_s, cases_s, collect_s, until_s, coll_s;
val define_s, output_s, single_s, first_s, last_s, empty_s;
val repeat_s, rep_s, flatten_s, forget_s;
val local_s, merge_s, bind_s, rebind_s, cat_s;
val try_s, catch_s, finally_s, throw_s, defex_s, deffilter_s;
val eof_s, eol_s, assert_s;
val error_s, type_error_s, internal_error_s;
val numeric_error_s, range_error_s;
val query_error_s, file_error_s, process_error_s;
val gensym_counter_s;

val nothrow_k, args_k, colon_k, auto_k;

val null_string;
val nil_string;
val null_list;

val identity_f, equal_f, eql_f, eq_f, gt_f, lt_f, car_f, cdr_f, null_f;

val prog_string;

static val env_list;

mem_t *(*oom_realloc)(mem_t *, size_t);

val identity(val obj)
{
  return obj;
}

static val code2type(int code)
{
  switch ((type_t) code) {
  case NIL: return null_s;
  case CONS: return cons_s;
  case STR: return str_s;
  case LIT: return lit_s;
  case CHR: return chr_s;
  case NUM: return fixnum_s;
  case SYM: return sym_s;
  case PKG: return pkg_s;
  case FUN: return fun_s;
  case VEC: return vec_s;
  case LCONS: return lcons_s;
  case LSTR: return lstr_s;
  case COBJ: return cobj_s;
  case ENV: return env_s;
  case BGNUM: return bignum_s;
  case FLNUM: return float_s;
  }
  return nil;
}

val typeof(val obj)
{
  switch (tag(obj)) {
  case TAG_NUM:
    return fixnum_s;
  case TAG_CHR:
    return chr_s;
  case TAG_LIT:
    return lit_s;
  case TAG_PTR:
    {
      int typecode = type(obj);

      if (typecode == COBJ) {
        return obj->co.cls;
      } else {
        val typesym = code2type(typecode);
        if (!typesym)
          internal_error("corrupt type field");
        return typesym;
      }
    }
  default:
    internal_error("invalid type tag");
  }
}

val type_check(val obj, int typecode)
{
  if (type(obj) != typecode)
    type_mismatch(lit("~s is not of type ~s"), obj, code2type(typecode), nao);
  return t;
}

val type_check2(val obj, int t1, int t2)
{
  if (!is_ptr(obj) || (obj->t.type != t1 && obj->t.type != t2))
    type_mismatch(lit("~s is not of type ~s or ~s"), obj,
                  code2type(t1), code2type(t2), nao);
  return t;
}

val type_check3(val obj, int t1, int t2, int t3)
{
  if (!is_ptr(obj) || (obj->t.type != t1 && obj->t.type != t2
                       && obj->t.type != t3))
    type_mismatch(lit("~s is not of type ~s, ~s nor ~s"), obj,
                  code2type(t1), code2type(t2), code2type(t3), nao);
  return t;
}

val class_check(val cobj, val class_sym)
{
  type_assert (is_ptr(cobj) && cobj->t.type == COBJ &&
               cobj->co.cls == class_sym, 
               (lit("~a is not of type ~a"), cobj, class_sym, nao));
  return t;
}

val car(val cons)
{
  switch (type(cons)) {
  case NIL:
    return nil;
  case CONS:
    return cons->c.car;
  case LCONS:
    if (cons->lc.func == nil) {
      return cons->lc.car;
    } else {
      funcall1(cons->lc.func, cons);
      cons->lc.func = nil;
      return cons->lc.car;
    }
  case VEC:
    if (zerop(cons->v.vec[vec_length]))
      return nil;
    return cons->v.vec[0];
  case STR:
  case LIT:
  case LSTR:
    if (zerop(length_str(cons)))
      return nil;
    return chr_str(cons, zero);
  default:
    type_mismatch(lit("~s is not a cons"), cons, nao);
  }
}

val cdr(val cons)
{
  switch (type(cons)) {
  case NIL:
    return nil;
  case CONS:
    return cons->c.cdr;
  case LCONS:
    if (cons->lc.func == nil) {
      return cons->lc.cdr;
    } else {
      funcall1(cons->lc.func, cons);
      cons->lc.func = nil;
      return cons->lc.cdr;
    }
  case VEC:
  case STR:
  case LIT:
  case LSTR:
    if (le(length(cons), one))
      return nil;
    return sub(cons, one, t);
  default:
    type_mismatch(lit("~s is not a cons"), cons, nao);
  }
}

val rplaca(val cons, val new_car)
{
  switch (type(cons)) {
  case CONS:
    set(mkloc(cons->c.car, cons), new_car);
    return cons;
  case LCONS:
    set(mkloc(cons->lc.car, cons), new_car);
    return cons;
  default:
    type_mismatch(lit("~s is not a cons"), cons, nao);
  }
}


val rplacd(val cons, val new_cdr)
{
  switch (type(cons)) {
  case CONS:
    set(mkloc(cons->c.cdr, cons), new_cdr);
    return cons;
  case LCONS:
    set(mkloc(cons->lc.cdr, cons), new_cdr);
    return cons;
  default:
    type_mismatch(lit("~s is not a cons"), cons, nao);
  }
}

loc car_l(val cons)
{
  switch (type(cons)) {
  case CONS:
    return mkloc(cons->c.car, cons);
  case LCONS:
    if (cons->lc.func) {
      funcall1(cons->lc.func, cons);
      cons->lc.func = nil;
    }
    return mkloc(cons->lc.car, cons);
  default:
    type_mismatch(lit("~s is not a cons"), cons, nao);
  }
}

loc cdr_l(val cons)
{
  switch (type(cons)) {
  case CONS:
    return mkloc(cons->c.cdr, cons);
  case LCONS:
    if (cons->lc.func) {
      funcall1(cons->lc.func, cons);
      cons->lc.func = nil;
    }
    return mkloc(cons->lc.cdr, cons);
  default:
    type_mismatch(lit("~s is not a cons"), cons, nao);
  }
}

val first(val cons)
{
  return car(cons);
}

val rest(val cons)
{
  return cdr(cons);
}

val second(val cons)
{
  return car(cdr(cons));
}

val third(val cons)
{
  return car(cdr(cdr(cons)));
}

val fourth(val cons)
{
  return car(cdr(cdr(cdr(cons))));
}

val fifth(val cons)
{
  return car(cdr(cdr(cdr(cdr(cons)))));
}

val sixth(val cons)
{
  return car(cdr(cdr(cdr(cdr(cdr(cons))))));
}

val conses(val list)
{
  list_collect_decl (out, ptail);

  for (; consp(list); list = cdr(list))
    ptail = list_collect(ptail, list);

  return out;
}

static val lazy_conses_func(val env, val lcons)
{
  val fun = lcons_fun(lcons);
  rplaca(lcons, env);
  func_set_env(fun, env = cdr(env));

  if (env)
    rplacd(lcons, make_lazy_cons(fun));
  else
    rplacd(lcons, nil);
  return nil;
}

val lazy_conses(val list)
{
  if (!list)
    return nil;
  return make_lazy_cons(func_f1(list, lazy_conses_func));
}

val listref(val list, val ind)
{
  if (lt(ind, zero))
    ind = plus(ind, length_list(list));
  for (; gt(ind, zero); ind = minus(ind, one))
    list = cdr(list);
  return car(list);
}

loc listref_l(val list, val ind)
{
  val olist = list;
  val oind = ind;

  if (lt(ind, zero))
    ind = plus(ind, length_list(list));

  for (; gt(ind, zero) && list; ind = minus(ind, one))
    list = cdr(list);
  if (consp(list))
    return car_l(list);

  uw_throwf(error_s, lit("~s has no assignable location at ~s"),
            olist, oind,  nao);
}

loc tail(val cons)
{
  while (cdr(cons))
    cons = cdr(cons);
  return cdr_l(cons);
}

loc lastcons(val list)
{
  loc ret = nulloc;
  while (consp(cdr(list))) {
    ret = cdr_l(list);
    list = cdr(list);
  }
  return ret;
}

val last(val list)
{
  loc p = lastcons(list);
  return nullocp(p) ? deref(p) : list;
}

loc ltail(loc cons)
{
  while (cdr(deref(cons)))
    cons = cdr_l(deref(cons));
  return cons;
}

val pop(val *plist)
{
  val ret = car(*plist);
  *plist = cdr(*plist);
  return ret;
}

val upop(val *plist, val *pundo)
{
  *pundo = *plist;
  return pop(plist);
}

val push(val value, val *plist)
{
  /* Unsafe for mutating object fields: use mpush macro. */
  return *plist = cons(value, *plist);
}

val copy_list(val list)
{
  list_collect_decl (out, ptail);

  list = nullify(list);

  while (consp(list)) {
    ptail = list_collect(ptail, car(list));
    list = cdr(list);
  }

  ptail = list_collect_nconc(ptail, list);

  return out;
}

val make_like(val list, val thatobj)
{
  if (list != thatobj) {
    switch (type(thatobj)) {
    case VEC:
      return vector_list(list);
    case STR:
    case LIT:
    case LSTR:
      if (is_chr(car(list)))
        return cat_str(list, nil);
      break;
    case NIL:
    case CONS:
    case LCONS:
    default:
      break;
    }
  }

  return list;
}

val toseq(val seq)
{
  switch (type(seq)) {
  case VEC:
  case STR:
  case LIT:
  case LSTR:
  case NIL:
  case CONS:
  case LCONS:
    return nullify(seq);
  default:
    return cons(seq, nil);
  }
}

val tolist(val seq)
{
  switch (type(seq)) {
  case VEC:
    return list_vector(seq);
  case STR:
  case LIT:
  case LSTR:
    return list_str(seq);
  case NIL:
  case CONS:
  case LCONS:
  default:
    return seq;
  }
}

val nullify(val seq)
{
  switch (type(seq)) {
  case NIL:
    return nil;
  case CONS:
  case LCONS:
    return seq;
  case LIT:
  case STR:
    return c_str(seq)[0] ? seq : nil;
  case LSTR:
    return if3(length_str_gt(seq, zero), seq, nil);
  case VEC:
    return if3(length_vec(seq) != zero, seq, nil);
  default:
    return seq;
  }
}

val seqp(val obj)
{
  switch (type(obj)) {
  case NIL:
  case CONS:
  case LCONS:
  case VEC:
  case STR:
  case LSTR:
  case LIT:
    return t;
  default:
    return nil;
  }
}

loc list_collect(loc ptail, val obj)
{
  switch (type(deref(ptail))) {
  case NIL:
    set(ptail, cons(obj, nil));
    return ptail;
  case CONS:
  case LCONS:
    ptail = tail(deref(ptail));
    set(ptail, cons(obj, nil));
    return ptail;
  case VEC:
    replace_vec(deref(ptail), cons(obj, nil), t, t);
    return ptail;
  case STR:
  case LIT:
  case LSTR:
    replace_str(deref(ptail), cons(obj, nil), t, t);
    return ptail;
  default:
    uw_throwf(error_s, lit("cannot append ~s to ~s"), obj, deref(ptail), nao);
  }
}

loc list_collect_nconc(loc ptail, val obj)
{
  obj = nullify(obj);

  switch (type(deref(ptail))) {
  case NIL:
    set(ptail, obj);
    return ptail;
  case CONS:
  case LCONS:
    ptail = tail(deref(ptail));
    set(ptail, obj);
    return ptail;
  case VEC:
    replace_vec(deref(ptail), obj, t, t);
    return ptail;
  case STR:
  case LIT:
  case LSTR:
    replace_str(deref(ptail), obj, t, t);
    return ptail;
  default:
    uw_throwf(error_s, lit("cannot nconc ~s to ~s"), obj, deref(ptail), nao);
  }
}

loc list_collect_append(loc ptail, val obj)
{
  obj = nullify(obj);

  switch (type(deref(ptail))) {
  case NIL:
    set(ptail, obj);
    return ptail;
  case CONS:
  case LCONS:
    set(ptail, copy_list(deref(ptail)));
    ptail = tail(deref(ptail));
    set(ptail, obj);
    return ptail;
  case VEC:
    set(ptail, copy_vec(deref(ptail)));
    replace_vec(deref(ptail), obj, t, t);
    return ptail;
  case STR:
  case LIT:
  case LSTR:
    set(ptail, copy_str(deref(ptail)));
    replace_str(deref(ptail), obj, t, t);
    return ptail;
  default:
    uw_throwf(error_s, lit("cannot append ~s to ~s"), obj, deref(ptail), nao);
  }
}

val nreverse(val in)
{
  val rev = nil;

  while (in) {
    val temp = cdr(in);
    set(cdr_l(in), rev);
    rev = in;
    in = temp;
  }

  return rev;
}

val reverse(val in)
{
  val in_orig = in;
  val rev = nil;

  in = nullify(in);

  while (in) {
    rev = cons(car(in), rev);
    in = cdr(in);
  }

  return make_like(rev, in_orig);
}

val append2(val list1, val list2)
{
  list_collect_decl (out, ptail);

  ptail = list_collect_append (ptail, list1);
  ptail = list_collect_append (ptail, list2);

  return out;
}

val appendv(val lists)
{
  list_collect_decl (out, ptail);

  for (; lists; lists = cdr(lists)) {
    val item = car(lists);
    ptail = list_collect_append(ptail, item);
  }

  return out;
}

val nappend2(val list1, val list2)
{
  list_collect_decl (out, ptail);

  ptail = list_collect_nconc (ptail, list1);
  ptail = list_collect_nconc (ptail, list2);

  return out;
}

val sub_list(val list, val from, val to)
{
  val len = nil;

  if (!list)
    return nil;

  if (null_or_missing_p(from))
    from = zero;
  else if (from == t)
    from = nil;
  else if (lt(from, zero)) {
    from = plus(from, len = length(list));
    if (to == zero)
      to = nil;
  }

  if (to == t || null_or_missing_p(to))
    to = nil;
  else if (!null_or_missing_p(to) && lt(to, zero))
    to = plus(to, if3(len, len, len = length(list)));

  if (to && from && gt(from, to)) {
    return nil;
  } else if (!to || (len && ge(to, len)))  {
    val iter, i;

    for (i = zero, iter = list; iter; iter = cdr(iter), i = plus(i, one)) {
      if (from && ge(i, from))
        break;
    }
    return iter;
  } else {
    val iter, i;
    list_collect_decl (out, ptail);

    for (i = zero, iter = list; iter; iter = cdr(iter), i = plus(i, one)) {
      if (ge(i, to))
        break;
      if (from && ge(i, from))
        ptail = list_collect(ptail, car(iter));
    }

    return out;
  }
}

val replace_list(val list, val items, val from, val to)
{
  val len = nil;

  if (vectorp(items))
    items = list_vector(items);
  else if (stringp(items))
    items = list_str(items);
  else if (!listp(items))
    uw_throwf(error_s, lit("replace-list: cannot replace with ~s"), items, nao);

  if (!list)
    return items;

  if (null_or_missing_p(from))
    from = zero;
  else if (from == t)
    from = nil;
  else if (lt(from, zero)) {
    from = plus(from, len ? len : (len = length(list)));
    if (to == zero)
      to = len;
  }

  if (to == t || null_or_missing_p(to))
    to = nil;
  if (to && lt(to, zero))
    to = plus(to, len ? len : (len = length(list)));

  if (!to || (len && ge(to, len)))  {
    if (from && zerop(from)) {
      return (listp(items)) ? items : list_vector(items);
    } else {
      val iter, i;
      list_collect_decl (out, ptail);

      for (i = zero, iter = list; iter; iter = cdr(iter), i = plus(i, one)) {
        if (from && ge(i, from))
          break;
        ptail = list_collect(ptail, car(iter));
      }

      ptail = list_collect_nconc(ptail, if3(listp(items),
                                            items, list_vector(items)));
      return out;
    }
  } else {
    val iter, i;
    list_collect_decl (out, ptail);

    for (i = zero, iter = list; iter; iter = cdr(iter), i = plus(i, one)) {
      if (ge(i, to))
        break;
      if (from && lt(i, from))
        ptail = list_collect(ptail, car(iter));
    }

    ptail = list_collect_nconc(ptail, append2(if3(listp(items), items,
                                                  list_vector(items)),
                                      iter));
    return out;
  }
}

static val lazy_appendv_func(val env, val lcons)
{
  cons_bind (last, lists, env);
  val nonempty = nil;

  while (lists) {
    nonempty = nullify(pop(&lists));
    if (nonempty)
      break;
  }

  rplaca(lcons, last);

  if (atom(nonempty)) {
    rplacd(lcons, nonempty);
    return nil;
  }

  rplacd(env, lists);

  {
    loc ptail = ltail(mkcloc(nonempty));
    rplaca(env, car(deref(ptail)));
    set(ptail, make_lazy_cons(lcons_fun(lcons)));
    rplacd(lcons, nonempty);
  }
  return nil;
}

val lazy_appendv(val lists)
{
  val nonempty = nil;

  while (lists) {
    nonempty = nullify(pop(&lists));
    if (nonempty)
      break;
  }

  if (atom(nonempty))
    return nonempty;

  {
    loc ptail = ltail(mkcloc(nonempty));
    set(ptail, make_lazy_cons(func_f1(cons(car(deref(ptail)), lists),
                                      lazy_appendv_func)));
    return nonempty;
  }
}

val ldiff(val list1, val list2)
{
  val list_orig = list1;
  list_collect_decl (out, ptail);

  list1 = nullify(list1);
  list2 = nullify(list2);

  switch (type(list2)) {
  case STR:
  case LIT:
  case LSTR:
  case VEC:
    while (list1 && !equal(list1, list2)) {
      ptail = list_collect(ptail, car(list1));
      list1 = cdr(list1);
    }
    break;
  default:
    while (list1 && list1 != list2) {
      ptail = list_collect(ptail, car(list1));
      list1 = cdr(list1);
    }
    break;
  }

  return make_like(out, list_orig);
}

val memq(val obj, val list)
{
  val list_orig = list;
  list = nullify(list);
  while (list && car(list) != obj)
    list = cdr(list);
  return make_like(list, list_orig);
}

val memql(val obj, val list)
{
  val list_orig = list;
  list = nullify(list);
  while (list && !eql(car(list), obj))
    list = cdr(list);
  return make_like(list, list_orig);
}

val memqual(val obj, val list)
{
  val list_orig = list;
  list = nullify(list);
  while (list && !equal(car(list), obj))
    list = cdr(list);
  return make_like(list, list_orig);
}

val remq(val obj, val list)
{
  list_collect_decl (out, ptail);
  val list_orig = list;
  val lastmatch = cons(nil, list);

  list = nullify(list);

  for (; list; list = cdr(list)) {
    if (car(list) == obj) {
      ptail = list_collect_nconc(ptail, ldiff(cdr(lastmatch), list));
      lastmatch = list;
    }
  }
  ptail = list_collect_nconc(ptail, cdr(lastmatch));
  return make_like(out, list_orig);
}

val remql(val obj, val list)
{
  list_collect_decl (out, ptail);
  val list_orig = list;
  val lastmatch = cons(nil, list);

  list = nullify(list);

  for (; list; list = cdr(list)) {
    if (eql(car(list), obj)) {
      ptail = list_collect_nconc(ptail, ldiff(cdr(lastmatch), list));
      lastmatch = list;
    }
  }
  ptail = list_collect_nconc(ptail, cdr(lastmatch));
  return make_like(out, list_orig);
}

val remqual(val obj, val list)
{
  list_collect_decl (out, ptail);
  val list_orig = list;
  val lastmatch = cons(nil, list);

  list = nullify(list);

  for (; list; list = cdr(list)) {
    if (equal(car(list), obj)) {
      ptail = list_collect_nconc(ptail, ldiff(cdr(lastmatch), list));
      lastmatch = list;
    }
  }
  ptail = list_collect_nconc(ptail, cdr(lastmatch));
  return make_like(out, list_orig);
}

val remove_if(val pred, val list, val key)
{
  list_collect_decl (out, ptail);
  val list_orig = list;
  val lastmatch = cons(nil, list);

  key = default_arg(key, identity_f);

  list = nullify(list);

  for (; list; list = cdr(list)) {
    val subj = funcall1(key, car(list));
    val satisfies = funcall1(pred, subj);

    if (satisfies) {
      ptail = list_collect_nconc(ptail, ldiff(cdr(lastmatch), list));
      lastmatch = list;
    }
  }
  ptail = list_collect_nconc(ptail, cdr(lastmatch));
  return make_like(out, list_orig);
}

val keep_if(val pred, val list, val key)
{
  list_collect_decl (out, ptail);
  val list_orig = list;
  val lastmatch = cons(nil, list);

  key = default_arg(key, identity_f);

  list = nullify(list);

  for (; list; list = cdr(list)) {
    val subj = funcall1(key, car(list));
    val satisfies = funcall1(pred, subj);

    if (!satisfies) {
      ptail = list_collect_nconc(ptail, ldiff(cdr(lastmatch), list));
      lastmatch = list;
    }
  }
  ptail = list_collect_nconc(ptail, cdr(lastmatch));
  return make_like(out, list_orig);
}

static val rem_lazy_rec(val obj, val list, val env, val func);

static val rem_lazy_func(val env, val lcons)
{
  cons_bind (pred, list, env);
  return cdr(rplacd(lcons, rem_lazy_rec(pred, list, env, lcons_fun(lcons))));
}

static val rem_lazy_rec(val pred, val list, val env, val func)
{
  while (list && funcall1(pred, car(list)))
    list = cdr(list);
  if (!list)
    return nil;
  if (!env)
    func = func_f1(cons(pred, cdr(list)), rem_lazy_func);
  else
    rplacd(env, cdr(list));
  return make_half_lazy_cons(func, car(list));
}

val remq_lazy(val obj, val list)
{
  return rem_lazy_rec(curry_12_1(eq_f, obj), nullify(list), nil, nil);
}

val remql_lazy(val obj, val list)
{
  return rem_lazy_rec(curry_12_1(eql_f, obj), nullify(list), nil, nil);
}

val remqual_lazy(val obj, val list)
{
  return rem_lazy_rec(curry_12_1(equal_f, obj), nullify(list), nil, nil);
}

val remove_if_lazy(val pred, val list, val key)
{
  val pred_key = chain(default_arg(key, identity_f), pred, nao);
  return rem_lazy_rec(pred_key, nullify(list), nil, nil);
}

val keep_if_lazy(val pred, val list, val key)
{
  val pred_key = chain(default_arg(key, identity_f), pred, null_f, nao);
  return rem_lazy_rec(pred_key, nullify(list), nil, nil);
}

val tree_find(val obj, val tree, val testfun)
{
  if (funcall2(default_arg(testfun, equal_f), obj, tree))
    return t;
  else if (consp(tree))
    return some_satisfy(tree, curry_123_2(func_n3(tree_find), 
                                          obj, testfun), nil);
  return nil;
}

val countqual(val obj, val list)
{
  val count = zero;

  list = nullify(list);

  for (; list; list = cdr(list))
    if (equal(car(list), obj))
      count = plus(count, one);

  return count;
}

val countql(val obj, val list)
{
  val count = zero;

  list = nullify(list);

  for (; list; list = cdr(list))
    if (eql(car(list), obj))
      count = plus(count, one);

  return count;
}

val countq(val obj, val list)
{
  val count = zero;

  list = nullify(list);

  for (; list; list = cdr(list))
    if (car(list) == obj)
      count = plus(count, one);

  return count;
}

val count_if(val pred, val list, val key)
{
  val count = zero;

  key = default_arg(key, identity_f);
  list = nullify(list);

  for (; list; list = cdr(list)) {
    val subj = funcall1(key, car(list));
    val satisfies = funcall1(pred, subj);

    if (satisfies)
      count = plus(count, one);
  }

  return count;
}

val some_satisfy(val list, val pred, val key)
{
  pred = default_arg(pred, identity_f);
  key = default_arg(key, identity_f);
  list = nullify(list);

  for (; list; list = cdr(list)) {
    val item;
    if ((item = funcall1(pred, funcall1(key, car(list)))) != nil)
      return item;
  }

  return nil;
}

val all_satisfy(val list, val pred, val key)
{
  val item = t;

  pred = default_arg(pred, identity_f);
  key = default_arg(key, identity_f);
  list = nullify(list);

  for (; list; list = cdr(list)) {
    if ((item = funcall1(pred, funcall1(key, car(list)))) == nil)
      return nil;
  }

  return item;
}

val none_satisfy(val list, val pred, val key)
{
  pred = default_arg(pred, identity_f);
  key = default_arg(key, identity_f);
  list = nullify(list);

  for (; list; list = cdr(list)) {
    if (funcall1(pred, funcall1(key, car(list))))
      return nil;
  }

  return t;
}

val flatten(val list)
{
  if (list == nil)
    return nil;

  if (atom(list))
    return cons(list, nil);

  return mappend(func_n1(flatten), list);
}

/*
 * Return the embedded list whose car is the non-nil atom in the given nested
 * list, updating the escape stack in the process. If no such atom is found in
 * the list, try to retreate up the stack to find it in the surrounding
 * structure, finally returning nil if nothing is found.
 */
static val lazy_flatten_scan(val list, val *escape)
{
  for (;;) { 
    if (list) {
      val a = car(list);
      if (nilp(a)) {
        list = cdr(list);
      } else if (atom(a)) {
        return list;
      } else do {
        push(cdr(list), escape); /* safe mutation: *escape is a local var */
        list = a;
        a = car(list);
      } while (consp(a));
      return list;
    } else if (*escape) {
      list = pop(escape);
    } else {
      return nil;
    }
  }
}

static val lazy_flatten_func(val env, val lcons)
{
  cons_bind (list, escape, env);
  val atom = car(list);
  val next = lazy_flatten_scan(cdr(list), &escape);

  rplaca(lcons, atom);
  rplaca(env, next);
  rplacd(env, escape);

  if (next)
    rplacd(lcons, make_lazy_cons(lcons_fun(lcons)));

  return nil;
}

val lazy_flatten(val list)
{
  if (atom(list)) {
    return cons(list, nil);
  } else {
    val escape = nil;
    val next = lazy_flatten_scan(list, &escape);

    if (!next)
      return nil;

    return make_lazy_cons(func_f1(cons(next, escape), lazy_flatten_func));
  }
}

static val tuples_func(val env, val lcons)
{
  list_collect_decl (out, ptail);
  cons_bind (seq_in, envr, env);
  cons_bind (n, fill, envr);
  val seq = seq_in;
  val count;

  for (count = n; count != zero && seq; count = minus(count, one))
    list_collect(ptail, pop(&seq));

  if (!missingp(fill))
    for (; gt(count, zero); count = minus(count, one))
      list_collect(ptail, fill);

  rplaca(env, seq);

  if (seq)
    rplacd(lcons, make_lazy_cons(lcons_fun(lcons)));
  rplaca(lcons, make_like(out, seq_in));

  return nil;
}

val tuples(val n, val seq, val fill)
{
  seq = nullify(seq);

  if (!seq)
    return nil;

  return make_lazy_cons(func_f1(cons(seq, cons(n, fill)),
                                tuples_func));
}

cnum c_num(val num);

val eql(val left, val right)
{
  /* eql is the same as eq except that numbers
     are compared by value. This means that bignum and
     floatinmg point objects which are distinct are
     treated through the equal function. */
  if (left == right)
    return t;

  switch (type(left)) {
  case BGNUM:
  case FLNUM:
    return equal(left, right);
  default:
    return nil;
  }
}

val equal(val left, val right)
{
  if (left == right)
    return t;

  switch (type(left)) {
  case NIL:
  case CHR:
  case NUM:
    return nil;
  case CONS:
  case LCONS:
    if ((type(right) == CONS || type(right) == LCONS) &&
        equal(car(left), car(right)) &&
        equal(cdr(left), cdr(right)))
    {
      return t;
    }
    return nil;
  case LIT:
    switch (type(right)) {
    case LIT:
      return wcscmp(litptr(left), litptr(right)) == 0 ? t : nil;
    case STR:
      return wcscmp(litptr(left), right->st.str) == 0 ? t : nil;
    case LSTR:
      lazy_str_force(right);
      return equal(left, right->ls.prefix);
    default:
      break;
    }
    return nil;
  case STR:
    switch (type(right)) {
    case LIT:
      return wcscmp(left->st.str, litptr(right)) == 0 ? t : nil;
    case STR:
      return wcscmp(left->st.str, right->st.str) == 0 ? t : nil;
    case LSTR:
      lazy_str_force(right);
      return equal(left, right->ls.prefix);
    default:
      break;
    }
    return nil;
  case SYM:
  case PKG:
  case ENV:
    return right == left ? t : nil;
  case FUN:
    if (type(right) == FUN &&
        left->f.functype == right->f.functype &&
        equal(left->f.env, right->f.env))
    {
      switch (left->f.functype) {
      case FINTERP: return (equal(left->f.f.interp_fun, right->f.f.interp_fun));
      case F0: return (left->f.f.f0 == right->f.f.f0) ? t : nil;
      case F1: return (left->f.f.f1 == right->f.f.f1) ? t : nil;
      case F2: return (left->f.f.f2 == right->f.f.f2) ? t : nil;
      case F3: return (left->f.f.f3 == right->f.f.f3) ? t : nil;
      case F4: return (left->f.f.f4 == right->f.f.f4) ? t : nil;
      case N0: return (left->f.f.n0 == right->f.f.n0) ? t : nil;
      case N1: return (left->f.f.n1 == right->f.f.n1) ? t : nil;
      case N2: return (left->f.f.n2 == right->f.f.n2) ? t : nil;
      case N3: return (left->f.f.n3 == right->f.f.n3) ? t : nil;
      case N4: return (left->f.f.n4 == right->f.f.n4) ? t : nil;
      case N5: return (left->f.f.n5 == right->f.f.n5) ? t : nil;
      case N6: return (left->f.f.n6 == right->f.f.n6) ? t : nil;
      case N7: return (left->f.f.n7 == right->f.f.n7) ? t : nil;
      }
      return nil;
    }
    return nil;
  case VEC:
    if (type(right) == VEC) {
      cnum i, length;
      if (!equal(left->v.vec[vec_length], right->v.vec[vec_length]))
        return nil;
      length = c_num(left->v.vec[vec_length]);
      for (i = 0; i < length; i++) {
        if (!equal(left->v.vec[i], right->v.vec[i]))
          return nil;
      }
      return t;
    }
    return nil;
  case LSTR:
    switch (type(right)) {
    case LIT:
    case STR:
    case LSTR:
      lazy_str_force(left);
      return equal(left->ls.prefix, right);
    default:
      break;
    }
    return nil;
  case BGNUM:
    if (type(right) == BGNUM && mp_cmp(mp(left), mp(right)) == MP_EQ)
      return t;
    return nil;
  case FLNUM:
    if (type(right) == FLNUM && left->fl.n == right->fl.n)
      return t;
    return nil;
  case COBJ:
    if (type(right) == COBJ)
      return left->co.ops->equal(left, right);
    return nil;
  }

  internal_error("unhandled case in equal function");
}

alloc_bytes_t malloc_bytes;

mem_t *chk_malloc(size_t size)
{
  mem_t *ptr = (mem_t *) malloc(size);

  assert (!async_sig_enabled);

  if (size && ptr == 0)
    ptr = (mem_t *) oom_realloc(0, size);
  malloc_bytes += size;
  return ptr;
}

mem_t *chk_malloc_gc_more(size_t size)
{
  mem_t *ptr = (mem_t *) malloc(size);
  assert (!async_sig_enabled);
  if (size && ptr == 0)
    ptr = (mem_t *) oom_realloc(0, size);
  return ptr;
}

mem_t *chk_calloc(size_t n, size_t size)
{
  mem_t *ptr = (mem_t *) calloc(n, size);
  cnum total = (cnum) size * (cnum) n;

  assert (!async_sig_enabled);

  if (size && ptr == 0) {
    ptr = (mem_t *) oom_realloc(0, total);
    memset(ptr, 0, total);
  }
  malloc_bytes += total;
  return ptr;
}

mem_t *chk_realloc(mem_t *old, size_t size)
{
  mem_t *newptr = (mem_t *) realloc(old, size);

  assert (!async_sig_enabled);

  if (size != 0 && newptr == 0)
    newptr = oom_realloc(old, size);
  malloc_bytes += size;
  return newptr;
}

wchar_t *chk_strdup(const wchar_t *str)
{
  size_t nchar = wcslen(str) + 1;
  wchar_t *copy = (wchar_t *) chk_malloc(nchar * sizeof *copy);
  assert (!async_sig_enabled);
  wmemcpy(copy, str, nchar);
  return copy;
}


val cons(val car, val cdr)
{
  val obj = make_obj();
  obj->c.type = CONS;
  obj->c.car = car;
  obj->c.cdr = cdr;
  return obj;
}

val make_lazy_cons(val func)
{
  val obj = make_obj();
  obj->lc.type = LCONS;
  obj->lc.car = obj->lc.cdr = nil;
  obj->lc.func = func;
  return obj;
}

val make_half_lazy_cons(val func, val car)
{
  val obj = make_obj();
  obj->lc.type = LCONS;
  obj->lc.car = car;
  obj->lc.cdr = nil;
  obj->lc.func = func;
  return obj;
}

val lcons_fun(val lcons)
{
  type_check(lcons, LCONS);
  return lcons->lc.func;
}

val list(val first, ...)
{
  va_list vl;
  val list = nil;
  val array[32], *ptr = array;

  if (first != nao) {
    val next = first;

    va_start (vl, first);

    do {
      *ptr++ = next;
      if (ptr == array + 32)
        internal_error("runaway arguments in list function");
      next = va_arg(vl, val);
    } while (next != nao);

    va_end (vl);

    while (ptr > array)
      list = cons(*--ptr, list);
  }

  return list;
}

val consp(val obj)
{
  type_t ty = type(obj);
  return (ty == CONS || ty == LCONS) ? t : nil;
}

val atom(val obj)
{
  return if3(consp(obj), nil, t);
}

val listp(val obj)
{
  return if2(obj == nil || consp(obj), t);
}

val proper_listp(val obj)
{
  while (consp(obj))
    obj = cdr(obj);

  return (obj == nil) ? t : nil;
}

val length_list(val list)
{
  cnum len = 0;
  while (consp(list)) {
    len++;
    list = cdr(list);
  }
  return num(len);
}

val getplist(val list, val key)
{
  for (; list; list = cdr(cdr(list))) {
    val ind = first(list);
    if (ind == key)
      return second(list);
  }

  return nil;
}

val getplist_f(val list, val key, loc found)
{
  for (; list; list = cdr(cdr(list))) {
    val ind = first(list);
    if (ind == key) {
      deref(found) = t;
      return second(list);
    }
  }

  deref(found) = nil;
  return nil;
}

val proper_plist_to_alist(val list)
{
  list_collect_decl (out, ptail);

  for (; list; list = cdr(cdr(list))) {
    val ind = first(list);
    val prop = second(list);
    ptail = list_collect(ptail, cons(ind, prop));
  }

  return out;
}

val improper_plist_to_alist(val list, val boolean_keys)
{
  list_collect_decl (out, ptail);

  for (; list; list = cdr(list)) {
    val ind = first(list);

    if (memqual(ind, boolean_keys)) {
      ptail = list_collect(ptail, cons(ind, t));
    } else {
      val prop = second(list);
      ptail = list_collect(ptail, cons(ind, prop));
      list = cdr(list);
    }
  }

  return out;
}

val num(cnum n)
{
  if (n >= NUM_MIN && n <= NUM_MAX)
    return (val) ((n << TAG_SHIFT) | TAG_NUM);
  return bignum(n);
}

cnum c_num(val num)
{
  switch (type(num)) {
  case CHR: case NUM:
    return ((cnum) num) >> TAG_SHIFT;
  case BGNUM:
    if (in_int_ptr_range(num)) {
      int_ptr_t out;
      mp_get_intptr(mp(num), &out);
      return out;
    }
    uw_throwf(error_s, lit("~s is out of cnum range"), num, nao);
  default:
    type_mismatch(lit("~s is not an integer"), num, nao);
  }
}

val flo(double n)
{
  val obj = make_obj();
  obj->fl.type = FLNUM;
  obj->fl.n = n;
  return obj;
}

double c_flo(val num)
{
  type_check(num, FLNUM);
  return num->fl.n;
}

val fixnump(val num)
{
  return (is_num(num)) ? t : nil;
}

val bignump(val num)
{
  return (type(num) == BGNUM) ? t : nil;
}

val integerp(val num)
{
  switch (tag(num)) {
  case TAG_NUM:
    return t;
  case TAG_PTR:
    if (num == nil)
      return nil;
    if (num->t.type == BGNUM)
      return t;
    /* fallthrough */
  default:
    return nil;
  }
}

val floatp(val num)
{
  return (type(num) == FLNUM) ? t : nil;
}

val numberp(val num)
{
  switch (tag(num)) {
  case TAG_NUM:
    return t;
  case TAG_PTR:
    if (num == nil)
      return nil;
    if (num->t.type == BGNUM || num->t.type == FLNUM)
      return t;
    /* fallthrough */
  default:
    return nil;
  }
}

val plusv(val nlist)
{
  if (!nlist)
    return num(0);
  else if (!cdr(nlist))
    return car(nlist);
  return reduce_left(func_n2(plus), cdr(nlist), car(nlist), nil);
}

val minusv(val minuend, val nlist)
{
  if (nlist)
    return reduce_left(func_n2(minus), nlist, minuend, nil);
  return neg(minuend);
}

val mulv(val nlist)
{
  if (!nlist)
    return one;
  else if (!cdr(nlist))
    return car(nlist);
  return reduce_left(func_n2(mul), cdr(nlist), car(nlist), nil);
}

val logandv(val nlist)
{
  if (!nlist)
    return negone;
  else if (!cdr(nlist))
    return car(nlist);
  return reduce_left(func_n2(logand), cdr(nlist), car(nlist), nil);
}

val logiorv(val nlist)
{
  if (!nlist)
    return zero;
  else if (!cdr(nlist))
    return car(nlist);
  return reduce_left(func_n2(logior), cdr(nlist), car(nlist), nil);
}

val gtv(val first, val rest)
{
  val iter;

  for (iter = rest; iter; iter = cdr(iter)) {
    val elem = car(iter);
    if (!gt(first, elem))
      return nil;
    first = elem;
  }

  return t;
}

val ltv(val first, val rest)
{
  val iter;

  for (iter = rest; iter; iter = cdr(iter)) {
    val elem = car(iter);
    if (!lt(first, elem))
      return nil;
    first = elem;
  }

  return t;
}

val gev(val first, val rest)
{
  val iter;

  for (iter = rest; iter; iter = cdr(iter)) {
    val elem = car(iter);
    if (!ge(first, elem))
      return nil;
    first = elem;
  }

  return t;
}

val lev(val first, val rest)
{
  val iter;

  for (iter = rest; iter; iter = cdr(iter)) {
    val elem = car(iter);
    if (!le(first, elem))
      return nil;
    first = elem;
  }

  return t;
}

val numeqv(val first, val rest)
{
  val iter;

  for (iter = rest; iter; iter = cdr(iter)) {
    val elem = car(iter);
    if (!numeq(first, elem))
      return nil;
    first = elem;
  }

  return t;
}

val numneqv(val list)
{
  val i, j;

  for (i = list; i; i = cdr(i))
    for (j = cdr(i); j; j = cdr(j))
      if (numeq(car(i), car(j)))
        return nil;

  return t;
}

val max2(val anum, val bnum)
{
  return if3(ge(anum, bnum), anum, bnum);
}

val min2(val anum, val bnum)
{
  return if3(le(anum, bnum), anum, bnum);
}

val maxv(val first, val rest)
{
  return reduce_left(func_n2(max2), rest, first, nil);
}

val minv(val first, val rest)
{
  return reduce_left(func_n2(min2), rest, first, nil);
}

val exptv(val nlist)
{
  return reduce_right(func_n2(expt), nlist, one, nil);
}

val string_own(wchar_t *str)
{
  val obj = make_obj();
  obj->st.type = STR;
  obj->st.str = str;
  obj->st.len = nil;
  obj->st.alloc = nil;
  return obj;
}

val string(const wchar_t *str)
{
  val obj = make_obj();
  obj->st.type = STR;
  obj->st.str = (wchar_t *) chk_strdup(str);
  obj->st.len = nil;
  obj->st.alloc = nil;
  return obj;
}

val string_utf8(const char *str)
{
  val obj = make_obj();
  obj->st.type = STR;
  obj->st.str = utf8_dup_from(str);
  obj->st.len = nil;
  obj->st.alloc = nil;
  return obj;
}

val mkstring(val len, val ch)
{
  size_t nchar = c_num(len) + 1;
  wchar_t *str = (wchar_t *) chk_malloc(nchar * sizeof *str);
  val s = string_own(str);
  wmemset(str, c_chr(ch), nchar);
  s->st.len = len;
  s->st.alloc = plus(len, one);
  return s;
}

val mkustring(val len)
{
  cnum l = c_num(len);
  wchar_t *str = (wchar_t *) chk_malloc((l + 1) * sizeof *str);
  val s = string_own(str);
  str[l] = 0;
  s->st.len = len;
  s->st.alloc = plus(len, one);
  return s;
}

val init_str(val str, const wchar_t *data)
{
  wmemcpy(str->st.str, data, c_num(str->st.len));
  return str;
}

val copy_str(val str)
{
  return string(c_str(str));
}

val upcase_str(val str)
{
  val len = length_str(str);
  wchar_t *dst = (wchar_t *) chk_malloc((c_num(len) + 1) * sizeof *dst);
  const wchar_t *src = c_str(str);
  val out = string_own(dst);

  while ((*dst++ = towupper(*src++)))
    ;

  return out;
}

val downcase_str(val str)
{
  val len = length_str(str);
  wchar_t *dst = (wchar_t *) chk_malloc((c_num(len) + 1) * sizeof *dst);
  const wchar_t *src = c_str(str);
  val out = string_own(dst);

  while ((*dst++ = towlower(*src++)))
    ;

  return out;
}

val string_extend(val str, val tail)
{
  type_check(str, STR);
  {
    cnum len = c_num(length_str(str));
    cnum alloc = c_num(str->st.alloc);
    val needed;
    val room = zero;

    if (stringp(tail))
      needed = length_str(tail);
    else if (chrp(tail))
      needed = one;
    else if (fixnump(tail))
      needed = tail;
    else
      uw_throwf(error_s, lit("string-extend: tail ~s bad type"), str, nao);

    room = num(alloc - len - 1);

    while (gt(needed, room) && alloc < NUM_MAX) {
      if (alloc > NUM_MAX / 2) {
        alloc = NUM_MAX;
      } else {
        alloc *= 2;
      }
      room = num(alloc - len - 1);
    }

    if (gt(needed, room))
      uw_throwf(error_s, lit("string-extend: overflow"), nao);

    str->st.str = (wchar_t *) chk_realloc((mem_t *) str->st.str,
                                          alloc * sizeof *str->st.str);
    set(mkloc(str->st.alloc, str), num(alloc));
    set(mkloc(str->st.len, str), plus(str->st.len, needed));

    if (stringp(tail)) {
      wmemcpy(str->st.str + len, c_str(tail), c_num(needed) + 1);
    } else if (chrp(tail)) {
      str->st.str[len] = c_chr(tail);
      str->st.str[len + 1] = 0;
    }
  }

  return str;
}

val stringp(val str)
{
  switch (type(str)) {
  case LIT: 
  case STR: 
  case LSTR:
    return t;
  default:
    return nil;
  }
}

val lazy_stringp(val str)
{
  return type(str) == LSTR ? t : nil;
}

val length_str(val str)
{
  if (tag(str) == TAG_LIT) {
    return num(wcslen(c_str(str)));
  } else {
    type_check2 (str, STR, LSTR);

    if (str->ls.type == LSTR) {
      lazy_str_force(str);
      return length_str(str->ls.prefix);
    }

    if (!str->st.len) {
      set(mkloc(str->st.len, str), num(wcslen(str->st.str)));
      set(mkloc(str->st.alloc, str), plus(str->st.len, one));
    }
    return str->st.len;
  }
}

const wchar_t *c_str(val obj)
{
  if (tag(obj) == TAG_LIT)
    return litptr(obj);

  type_check3(obj, STR, SYM, LSTR);

  switch (obj->t.type) {
  case STR:
    return obj->st.str;
  case SYM:
    return c_str(symbol_name(obj));
  case LSTR:
    lazy_str_force(obj);
    return c_str(obj->ls.prefix);
  default:
    abort();
  }
}

val search_str(val haystack, val needle, val start_num, val from_end)
{
  from_end = default_bool_arg(from_end);
  start_num = default_arg(start_num, zero);

  if (length_str_lt(haystack, start_num)) {
    return nil;
  } else {
    val h_is_lazy = lazy_stringp(haystack);
    cnum start = c_num(start_num);
    cnum good = -1, pos = -1;
    const wchar_t *n = c_str(needle), *h;

    if (!h_is_lazy) {
      do {
        const wchar_t *f;
        h = c_str(haystack);

        f = wcsstr(h + start, n);

        if (f)
          pos = f - h;
        else
          pos = -1;
      } while (pos != -1 && (good = pos) != -1 && from_end && h[start++]);
    } else {
      size_t ln = c_num(length_str(needle));

      do {
        lazy_str_force_upto(haystack, plus(num(start + 1), length_str(needle)));
        h = c_str(haystack->ls.prefix);

        if (!wcsncmp(h + start, n, ln))
          good = start;
      } while (h[start++] && (from_end || good == -1));
    }
    return (good == -1) ? nil : num(good);
  }
}

val search_str_tree(val haystack, val tree, val start_num, val from_end)
{
  if (stringp(tree)) {
    val result = search_str(haystack, tree, start_num, from_end);
    if (result)
      return cons(result, length_str(tree));
  } else if (consp(tree)) {
    val it = nil, minpos = nil, maxlen = nil;

    for (; tree; tree = cdr(tree)) {
      val result = search_str_tree(haystack, car(tree), start_num, from_end);
      if (result) {
        cons_bind (pos, len, result);
        if (!it || lt(pos, minpos) || (pos == minpos && gt(len, maxlen))) {
          minpos = pos;
          maxlen = len;
          it = result;
        }
      }
    }

    return it;
  }

  return nil;
}

val match_str(val bigstr, val str, val pos)
{
  val i, p;

  pos = default_arg(pos, zero);

  if (ge(pos, zero)) {
    for (i = zero;
         length_str_gt(bigstr, p = plus(pos, i)) && length_str_gt(str, i);
         i = plus(i, one))
    {
      if (chr_str(bigstr, p) != chr_str(str, i))
        return nil;
    }

    return length_str_le(str, i) ? t : nil;
  } else {
    pos = plus(pos, length(bigstr));
    pos = plus(minus(pos, length(str)), one);

    for (i = minus(length(str), one);
         ge(i, zero) && ge(p = plus(pos, i), zero);
         i = minus(i, one))
    {
      if (chr_str(bigstr, p) != chr_str(str, i))
        return nil;
    }

    return lt(i, zero) ? t : nil;
  }
}

val match_str_tree(val bigstr, val tree, val pos)
{
  pos = default_arg(pos, zero);

  if (stringp(tree)) {
    if (match_str(bigstr, tree, pos))
      return length_str(tree);
  } else if (consp(tree)) {
    val maxlen = nil;

    for (; tree; tree = cdr(tree)) {
      val result = match_str_tree(bigstr, car(tree), pos);
      if (result && (!maxlen || gt(result, maxlen)))
        maxlen = result;
    }

    return maxlen;
  }

  return nil;
}

static val lazy_sub_str(val lstr, val from, val to)
{
  val len = nil;
  val len_pfx = length_str(lstr->ls.prefix);

  if (from == nil) {
    from = zero;
  } else if (from == t) {
    return null_string;
  } else {
    if (lt(from, zero)) {
      from = plus(from, len = length_str(lstr));
      from = max2(zero, from);

      if (to == zero)
        to = t;
    }

    if (ge(from, len_pfx)) {
      if (!lazy_str_force_upto(lstr, from))
        return null_string;
    }
  }

  if (to == nil || to == t) {
    to = t;
  } else {
    if (lt(to, zero)) {
      to = plus(to, len = length_str(lstr));
      to = max(zero, to);
    }

    if (ge(to, len_pfx))
      if (!lazy_str_force_upto(lstr, minus(to, one)))
        to = t;
  }

  {
    val pfxsub = sub_str(lstr->ls.prefix, from, to);
     
    if (to != t) {
      return pfxsub;
    } else {
      val lsub = make_obj();
      lsub->ls.type = LSTR;
      lsub->ls.prefix = pfxsub;
      lsub->ls.list = lstr->ls.list;
      lsub->ls.opts = lstr->ls.opts;

      return lsub;
    }
  }
}

val sub_str(val str_in, val from, val to)
{
  val len = nil;

  if (lazy_stringp(str_in))
    return lazy_sub_str(str_in, from, to);

  len = length_str(str_in);

  if (null_or_missing_p(from))
    from = zero;
  else if (from == t)
    return null_string;
  else if (lt(from, zero)) {
    from = plus(from, len);
    if (to == zero)
      to = len;
  }

  if (null_or_missing_p(to) || to == t)
    to = len;
  else if (lt(to, zero))
    to = plus(to, len);

  from = max2(zero, min2(from, len));
  to = max2(zero, min2(to, len));

  if (ge(from, to)) {
    return null_string;
  } else {
    size_t nchar = c_num(to) - c_num(from) + 1;
    wchar_t *sub = (wchar_t *) chk_malloc(nchar * sizeof (wchar_t));
    const wchar_t *str = c_str(str_in);
    wcsncpy(sub, str + c_num(from), nchar);
    sub[nchar-1] = 0;
    return string_own(sub);
  }
}

val replace_str(val str_in, val items, val from, val to)
{
  val itseq = toseq(items);
  val len = length_str(str_in);
  val len_it = length(itseq);
  val len_rep;

  if (type(str_in) != STR) {
    uw_throwf(error_s, lit("replace-str: ~s of type ~s is not "
                           "a modifiable string"),
              str_in, typeof(str_in), nao);
  }

  if (null_or_missing_p(from))
    from = zero;
  else if (from == t)
    from = len;
  else if (lt(from, zero)) {
    from = plus(from, len);
    if (to == zero)
      to = len;
  }

  if (null_or_missing_p(to) || to == t)
    to = len;
  else if (lt(to, zero))
    to = plus(to, len);

  from = max2(zero, min2(from, len));
  to = max2(zero, min2(to, len));

  len_rep = minus(to, from);

  if (gt(len_rep, len_it)) {
    val len_diff = minus(len_rep, len_it);
    cnum t = c_num(to);
    cnum l = c_num(len);

    wmemmove(str_in->st.str + t - c_num(len_diff), 
             str_in->st.str + t, (l - t) + 1);
    set(mkloc(str_in->st.len, str_in), minus(len, len_diff));
    to = plus(from, len_it);
  } else if (lt(len_rep, len_it)) {
    val len_diff = minus(len_it, len_rep);
    cnum t = c_num(to);
    cnum l = c_num(len);

    string_extend(str_in, plus(len, len_diff));
    wmemmove(str_in->st.str + t + c_num(len_diff),
             str_in->st.str + t, (l - t) + 1);
    to = plus(from, len_it);
  }
  
  if (zerop(len_it))
    return str_in;
  if (stringp(itseq)) {
    wmemcpy(str_in->st.str + c_num(from), c_str(itseq), c_num(len_it));
  } else {
    val iter;
    cnum f = c_num(from);
    cnum t = c_num(to);
    cnum s;

    if (listp(itseq)) {
      for (iter = itseq; iter && f != t; iter = cdr(iter), f++)
        str_in->st.str[f] = c_chr(car(iter));
    } else if (vectorp(itseq)) {
      for (s = 0; f != t; f++, s++)
        str_in->st.str[f] = c_chr(vecref(itseq, num(s)));
    } else {
      uw_throwf(error_s, lit("replace-str: source object ~s not supported"),
                itseq, nao);
    }
  }
  return str_in;
}


val cat_str(val list, val sep)
{
  cnum total = 0;
  val iter;
  wchar_t *str, *ptr;
  cnum len_sep = (!null_or_missing_p(sep)) ? c_num(length_str(sep)) : 0;

  for (iter = list; iter != nil; iter = cdr(iter)) {
    val item = car(iter);
    if (!item)
      continue;
    if (stringp(item)) {
      total += c_num(length_str(item));
      if (len_sep && cdr(iter))
        total += len_sep;
      continue;
    }
    if (chrp(item)) {
      total += 1;
      if (len_sep && cdr(iter))
        total += len_sep;
      continue;
    }
    uw_throwf(error_s, lit("cat-str: ~s is not a character or string"),
              item, nao);
  }

  str = (wchar_t *) chk_malloc((total + 1) * sizeof *str);

  for (ptr = str, iter = list; iter != nil; iter = cdr(iter)) {
    val item = car(iter);
    cnum len;
    if (!item)
      continue;
    if (stringp(item)) {
      len = c_num(length_str(item));
      wmemcpy(ptr, c_str(item), len);
      ptr += len;
    } else {
      *ptr++ = c_chr(item);
    }

    if (len_sep && cdr(iter)) {
      wmemcpy(ptr, c_str(sep), len_sep);
      ptr += len_sep;
    }
  }
  *ptr = 0;

  return string_own(str);
}

val split_str(val str, val sep)
{
  if (regexp(sep)) {
    list_collect_decl (out, iter);
    val pos = zero;

    do {
      cons_bind (new_pos, len, search_regex(str, sep, pos, 0));

      if (eql(pos, new_pos) && len == zero)
        new_pos = plus(new_pos, one);

      iter = list_collect(iter, sub_str(str, pos, new_pos));
      pos = new_pos;

      if (len) {
        pos = plus(pos, len);
        continue;
      }
      break;
    } while (le(pos, length_str(str)));

    return out;
  } else {
    size_t len_sep = c_num(length_str(sep));

    if (len_sep == 0) {
      return list_str(str);
    } else {
      const wchar_t *cstr = c_str(str);
      const wchar_t *csep = c_str(sep);

      list_collect_decl (out, iter);

      prot1(&str);
      prot1(&sep);

      for (;;) {
        const wchar_t *psep = wcsstr(cstr, csep);
        size_t span = (psep != 0) ? psep - cstr : wcslen(cstr);
        val piece = mkustring(num(span));
        init_str(piece, cstr);
        iter = list_collect(iter, piece);
        cstr += span;
        if (psep != 0) {
          cstr += len_sep;
          continue;
        }
        break;
      }

      rel1(&sep);
      rel1(&str);

      return out;
    }
  }
}

val split_str_set(val str, val set)
{
  const wchar_t *cstr = c_str(str);
  const wchar_t *cset = c_str(set);
  list_collect_decl (out, iter);

  prot1(&str);
  prot1(&set);

  for (;;) {
    size_t span = wcscspn(cstr, cset);
    val piece = mkustring(num(span));
    init_str(piece, cstr);
    iter = list_collect(iter, piece);
    cstr += span;
    if (*cstr) {
      cstr++;
      continue;
    }
    break;
  }

  rel1(&set);
  rel1(&str);

  return out;
}

val tok_str(val str, val tok_regex, val keep_sep)
{
  list_collect_decl (out, iter);
  val pos = zero;

  keep_sep = default_bool_arg(keep_sep);

  for (;;) {
    cons_bind (new_pos, len, search_regex(str, tok_regex, pos, nil));
    val end;

    if (!len) {
      if (keep_sep)
        iter = list_collect(iter, sub_str(str, pos, t));
      break;
    }

    end = plus(new_pos, len);

    if (keep_sep)
      iter = list_collect(iter, sub_str(str, pos, new_pos));

    iter = list_collect(iter, sub_str(str, new_pos, end));

    pos = end;

    if (len == zero)
      pos = plus(pos, one);
  }

  return out;
}

val list_str(val str)
{
  const wchar_t *cstr = c_str(str);
  list_collect_decl (out, iter);
  while (*cstr)
    iter = list_collect(iter, chr(*cstr++));
  return out;
}

val trim_str(val str)
{
  const wchar_t *start = c_str(str);
  const wchar_t *end = start + c_num(length_str(str));

  while (start[0] && iswspace(start[0]))
    start++;

  while (end > start && iswspace(end[-1]))
    end--;

  if (end == start) {
    return null_string;
  } else {
    size_t len = end - start;
    wchar_t *buf = (wchar_t *) chk_malloc((len + 1) * sizeof *buf);
    wmemcpy(buf, start, len);
    buf[len] = 0;
    return string_own(buf);
  }
}

val cmp_str(val astr, val bstr)
{
   switch (TYPE_PAIR(type(astr), type(bstr))) {
   case TYPE_PAIR(LIT, LIT):
   case TYPE_PAIR(STR, STR):
   case TYPE_PAIR(LIT, STR):
   case TYPE_PAIR(STR, LIT):
     return num_fast(wcscmp(c_str(astr), c_str(bstr)));
   case TYPE_PAIR(LSTR, LIT):
   case TYPE_PAIR(LSTR, STR):
   case TYPE_PAIR(LIT, LSTR):
   case TYPE_PAIR(STR, LSTR):
   case TYPE_PAIR(LSTR, LSTR):
     {
       val i;
       for (i = zero;
            length_str_lt(astr, i) && length_str_lt(bstr, i);
            i = plus(i, one))
       {
         val ach = chr_str(astr, i);
         val bch = chr_str(bstr, i);

         if (ach < bch)
           return one;
         else if (ach < bch)
           return one;
       }
       if (length_str_lt(bstr, i))
         return negone;
       if (length_str_lt(astr, i))
         return negone;
       return zero;
     }
   default:
     uw_throwf(error_s, lit("cmp-str: invalid operands ~s ~s"),
               astr, bstr, nao);
  }
}

val str_eq(val astr, val bstr)
{
  return if2(cmp_str(astr, bstr) == zero, t);
}

val str_lt(val astr, val bstr)
{
  return if2(cmp_str(astr, bstr) == negone, t);
}

val str_gt(val astr, val bstr)
{
  return if2(cmp_str(astr, bstr) == one, t);
}

val str_le(val astr, val bstr)
{
  val cmp = cmp_str(astr, bstr);
  return if2(cmp == zero || cmp == negone, t);
}

val str_ge(val astr, val bstr)
{
  val cmp = cmp_str(astr, bstr);
  return if2(cmp == zero || cmp == one, t);
}

val int_str(val str, val base)
{
  const wchar_t *wcs = c_str(str);
  wchar_t *ptr;
  cnum b = c_num(default_arg(base, num_fast(10)));

  /* TODO: detect if we have wcstoll */
  long value = wcstol(wcs, &ptr, b ? b : 10);

  if (value == 0 && ptr == wcs)
    return nil;

  if ((value == LONG_MAX || value == LONG_MIN) && errno == ERANGE) {
    val bignum = make_bignum();
    unsigned char *ucs = utf8_dup_to_uc(wcs);
    mp_err err = mp_read_radix(mp(bignum), ucs, b);

    free(ucs); /* TODO: make wchar_t version of mp_read_radix. */

    if (err != MP_OKAY)
      return nil;

    /* If wcstol overflowed, but the range of long is smaller than
       that of fixnums, that means that the value might not
       actually be a bignum, and so we must normalize.
       We do not need this on our usual target platforms, where NUM_MAX is
       never larger than LONG_MAX. */
    return (LONG_MAX < NUM_MAX) ? normalize(bignum) : bignum;
  } 

  if (value >= NUM_MIN && value <= NUM_MAX)
    return num(value);

  return bignum_from_long(value);
}

val flo_str(val str)
{
  const wchar_t *wcs = c_str(str);
  wchar_t *ptr;

  /* TODO: detect if we have wcstod */
  double value = wcstod(wcs, &ptr);
  if (value == 0 && ptr == wcs)
    return nil;
  if ((value == HUGE_VAL || value == -HUGE_VAL) && errno == ERANGE)
    return nil;
  return flo(value);
}

val num_str(val str)
{
  const wchar_t *wcs = c_str(str);
  const wchar_t *nws = wcs + wcsspn(wcs, L"\f\n\r\t\v");
  const wchar_t *dig = nws + wcsspn(wcs, L"+-");

  if (wcsspn(dig, L"0123456789") == wcsspn(dig, L"0123456789eE."))
    return int_str(str, nil);
  return flo_str(str);
}

val chrp(val chr)
{
  return (is_chr(chr)) ? t : nil;
}

wchar_t c_chr(val chr)
{
  if (!is_chr(chr))
    type_mismatch(lit("~s is not a character"), chr, nao);
  return (wchar_t) ((cnum) chr >> TAG_SHIFT);
}

val chr_isalnum(val ch)
{
  return c_true(iswalnum(c_chr(ch)));
}

val chr_isalpha(val ch)
{
  return c_true(iswalpha(c_chr(ch)));
}

val chr_isascii(val ch)
{
  return c_true(c_chr(ch) >= 0 && c_chr(ch) < 128);
}

val chr_iscntrl(val ch)
{
  return c_true(iswcntrl(c_chr(ch)));
}

val chr_isdigit(val ch)
{
  return c_true(iswdigit(c_chr(ch)));
}

val chr_isgraph(val ch)
{
  return c_true(iswgraph(c_chr(ch)));
}

val chr_islower(val ch)
{
  return c_true(iswlower(c_chr(ch)));
}

val chr_isprint(val ch)
{
  return c_true(iswprint(c_chr(ch)));
}

val chr_ispunct(val ch)
{
  return c_true(iswpunct(c_chr(ch)));
}

val chr_isspace(val ch)
{
  return c_true(iswspace(c_chr(ch)));
}

val chr_isupper(val ch)
{
  return c_true(iswupper(c_chr(ch)));
}

val chr_isxdigit(val ch)
{
  return c_true(iswxdigit(c_chr(ch)));
}

val chr_toupper(val ch)
{
  return chr(towupper(c_chr(ch)));
}

val chr_tolower(val ch)
{
  return chr(towlower(c_chr(ch)));
}

val num_chr(val ch)
{
  return num_fast(c_chr(ch));
}

val chr_num(val num)
{
  cnum n = c_num(num);
  if (n < 0 || n > 0x10FFFF)
    uw_throwf(numeric_error_s,
              lit("chr-num: ~s is out of character range"), num, nao);
  return chr(n);
}

val chr_str(val str, val ind)
{
  cnum index = c_num(ind);

  if (index < 0) {
    ind = plus(length_str(str), ind);
    index = c_num(ind);
  }

  if (index < 0 || !length_str_gt(str, ind))
    uw_throwf(error_s, lit("chr-str: ~s is out of range for string ~s"),
              ind, str, nao);

  if (lazy_stringp(str)) {
    lazy_str_force_upto(str, ind);
    return chr(c_str(str->ls.prefix)[index]);
  } else {
    return chr(c_str(str)[index]);
  }
}

val chr_str_set(val str, val ind, val chr)
{
  cnum index = c_num(ind);

  if (index < 0) {
    ind = plus(length_str(str), ind);
    index = c_num(ind);
  }

  if (index < 0 || !length_str_gt(str, ind))
    uw_throwf(error_s, lit("chr-str-set: ~s is out of range for string ~s"),
              ind, str, nao);


  if (lazy_stringp(str)) {
    lazy_str_force_upto(str, ind);
    str->ls.prefix->st.str[index] = c_chr(chr);
  } else {
    str->st.str[index] = c_chr(chr);
  }

  return chr;
}

val span_str(val str, val set)
{
  const wchar_t *cstr = c_str(str);
  const wchar_t *cset = c_str(set);
  size_t span = wcsspn(cstr, cset);
  return num(span);
}

val compl_span_str(val str, val set)
{
  const wchar_t *cstr = c_str(str);
  const wchar_t *cset = c_str(set);
  size_t span = wcscspn(cstr, cset);
  return num(span);
}

val break_str(val str, val set)
{
  const wchar_t *cstr = c_str(str);
  const wchar_t *cset = c_str(set);
  const wchar_t *brk = wcspbrk(cstr, cset);
  if (!brk)
    return nil;
  return num(brk - cstr);
}

val symbol_name(val sym)
{
  if (sym)
    type_check(sym, SYM);
  return sym ? sym->s.name : nil_string;
}

val symbol_package(val sym)
{
  if (sym == nil)
    return user_package;
  type_check(sym, SYM);
  return sym->s.package;
}

val make_sym(val name)
{
  val obj = make_obj();
  obj->s.type = SYM;
  obj->s.name = name;
  obj->s.package = nil;
  obj->s.value = nil;
  return obj;
}

val gensym(val prefix)
{
  prefix = default_arg(prefix, lit("g"));
  loc gs_loc = lookup_var_l(nil, gensym_counter_s);
  val name = format(nil, lit("~a~,04a"), prefix,
                    set(gs_loc, plus(deref(gs_loc), one)), nao);
  return make_sym(name);
}

val make_package(val name)
{
  if (find_package(name)) {
    uw_throwf(error_s, lit("make-package: ~s exists already"), name, nao);
  } else {
    val obj = make_obj();
    obj->pk.type = PKG;
    obj->pk.name = name;
    obj->pk.symhash = nil; /* make_hash call below could trigger gc! */
    obj->pk.symhash = make_hash(nil, nil, lit("t")); /* don't have t yet! */

    push(cons(name, obj), &packages);
    return obj;
  }
}

val packagep(val obj)
{
  return type(obj) == PKG ? t : nil;
}

val find_package(val name)
{
  return cdr(assoc(name, packages));
}

val delete_package(val package)
{
  if (stringp(package)) {
    val p = find_package(package);
    if (!p)
      uw_throwf(error_s, lit("delete-package: no such package: ~s"), package, nao);
    package = p;
  }

  type_check (package, PKG);
  packages = alist_nremove(packages, package->pk.name);
  return nil;
}

val intern(val str, val package)
{
  val new_p;
  loc place;

  if (null_or_missing_p(package)) {
    package = user_package;
  } else if (stringp(package)) {
    val p = find_package(package);
    if (!p)
      uw_throwf(error_s, lit("intern: ~s no such package"), package, nao);
    package = p;
  }

  type_check (package, PKG);

  place = gethash_l(package->pk.symhash, str, mkcloc(new_p));

  if (!new_p) {
    return deref(place);
  } else {
    val newsym = make_sym(str);
    newsym->s.package = package;
    return set(place, newsym);
  }
}

val rehome_sym(val sym, val package)
{
  if (!sym)
    return nil;

  if (null_or_missing_p(package)) {
    package = user_package;
  } else if (stringp(package)) {
    val p = find_package(package);
    if (!p)
      uw_throwf(error_s, lit("rehome-sym: no such package: ~s"), package, nao);
    package = p;
  }

  type_check (package, PKG);
  type_check (sym, SYM);

  if (sym->s.package)
    remhash(sym->s.package->pk.symhash, symbol_name(sym));
  set(mkloc(sym->s.package, sym), package);
  sethash(package->pk.symhash, symbol_name(sym), sym);
  return sym;
}

val symbolp(val sym)
{
  switch (type(sym)) {
  case NIL:
  case SYM:
    return t;
  default:
    return nil;
  }
}

val keywordp(val sym)
{
  return (symbolp(sym) && symbol_package(sym) == keyword_package) ? t : nil;
}

loc get_user_package(void)
{
  if (nilp(user_package_s))
    return mkcloc(user_package_var);
  return lookup_var_l(nil, user_package_s);
}

loc get_system_package(void)
{
  if (nilp(system_package_s))
    return mkcloc(system_package_var);
  return lookup_var_l(nil, system_package_s);
}

loc get_keyword_package(void)
{
  if (nilp(keyword_package_s))
    return mkcloc(keyword_package_var);
  return lookup_var_l(nil, keyword_package_s);
}

val func_f0(val env, val (*fun)(val))
{
  val obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = F0;
  obj->f.env = env;
  obj->f.f.f0 = fun;
  obj->f.variadic = 0;
  obj->f.fixparam = 0;
  obj->f.optargs = 0;
  return obj;
}

val func_f1(val env, val (*fun)(val, val))
{
  val obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = F1;
  obj->f.env = env;
  obj->f.f.f1 = fun;
  obj->f.variadic = 0;
  obj->f.fixparam = 1;
  obj->f.optargs = 0;
  return obj;
}

val func_f2(val env, val (*fun)(val, val, val))
{
  val obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = F2;
  obj->f.env = env;
  obj->f.f.f2 = fun;
  obj->f.variadic = 0;
  obj->f.fixparam = 2;
  obj->f.optargs = 0;
  return obj;
}

val func_f3(val env, val (*fun)(val, val, val, val))
{
  val obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = F3;
  obj->f.env = env;
  obj->f.f.f3 = fun;
  obj->f.variadic = 0;
  obj->f.fixparam = 3;
  obj->f.optargs = 0;
  return obj;
}

val func_f4(val env, val (*fun)(val, val, val, val, val))
{
  val obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = F4;
  obj->f.env = env;
  obj->f.f.f4 = fun;
  obj->f.variadic = 0;
  obj->f.fixparam = 4;
  obj->f.optargs = 0;
  return obj;
}

val func_n0(val (*fun)(void))
{
  val obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = N0;
  obj->f.env = nil;
  obj->f.f.n0 = fun;
  obj->f.variadic = 0;
  obj->f.fixparam = 0;
  obj->f.optargs = 0;
  return obj;
}

val func_n1(val (*fun)(val))
{
  val obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = N1;
  obj->f.env = nil;
  obj->f.f.n1 = fun;
  obj->f.variadic = 0;
  obj->f.fixparam = 1;
  obj->f.optargs = 0;
  return obj;
}

val func_n2(val (*fun)(val, val))
{
  val obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = N2;
  obj->f.env = nil;
  obj->f.f.n2 = fun;
  obj->f.variadic = 0;
  obj->f.fixparam = 2;
  obj->f.optargs = 0;
  return obj;
}

val func_n3(val (*fun)(val, val, val))
{
  val obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = N3;
  obj->f.env = nil;
  obj->f.f.n3 = fun;
  obj->f.variadic = 0;
  obj->f.fixparam = 3;
  obj->f.optargs = 0;
  return obj;
}

val func_n4(val (*fun)(val, val, val, val))
{
  val obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = N4;
  obj->f.env = nil;
  obj->f.f.n4 = fun;
  obj->f.variadic = 0;
  obj->f.fixparam = 4;
  obj->f.optargs = 0;
  return obj;
}

val func_n5(val (*fun)(val, val, val, val, val))
{
  val obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = N5;
  obj->f.env = nil;
  obj->f.f.n5 = fun;
  obj->f.variadic = 0;
  obj->f.fixparam = 5;
  obj->f.optargs = 0;
  return obj;
}

val func_n6(val (*fun)(val, val, val, val, val, val))
{
  val obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = N6;
  obj->f.env = nil;
  obj->f.f.n6 = fun;
  obj->f.variadic = 0;
  obj->f.fixparam = 6;
  obj->f.optargs = 0;
  return obj;
}

val func_n7(val (*fun)(val, val, val, val, val, val, val))
{
  val obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = N7;
  obj->f.env = nil;
  obj->f.f.n7 = fun;
  obj->f.variadic = 0;
  obj->f.fixparam = 7;
  obj->f.optargs = 0;
  return obj;
}

val func_f0v(val env, val (*fun)(val, val))
{
  val obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = F0;
  obj->f.env = env;
  obj->f.f.f0v = fun;
  obj->f.variadic = 1;
  obj->f.fixparam = 0;
  obj->f.optargs = 0;
  return obj;
}

val func_f1v(val env, val (*fun)(val env, val, val rest))
{
  val obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = F1;
  obj->f.env = env;
  obj->f.f.f1v = fun;
  obj->f.variadic = 1;
  obj->f.fixparam = 1;
  obj->f.optargs = 0;
  return obj;
}

val func_f2v(val env, val (*fun)(val env, val, val, val rest))
{
  val obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = F2;
  obj->f.env = env;
  obj->f.f.f2v = fun;
  obj->f.variadic = 1;
  obj->f.fixparam = 2;
  obj->f.optargs = 0;
  return obj;
}

val func_f3v(val env, val (*fun)(val env, val, val, val, val rest))
{
  val obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = F3;
  obj->f.env = env;
  obj->f.f.f3v = fun;
  obj->f.variadic = 1;
  obj->f.fixparam = 3;
  obj->f.optargs = 0;
  return obj;
}

val func_f4v(val env, val (*fun)(val env, val, val, val, val, val rest))
{
  val obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = F4;
  obj->f.env = env;
  obj->f.f.f4v = fun;
  obj->f.variadic = 1;
  obj->f.fixparam = 4;
  obj->f.optargs = 0;
  return obj;
}

val func_n0v(val (*fun)(val rest))
{
  val obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = N0;
  obj->f.env = nil;
  obj->f.f.n0v = fun;
  obj->f.variadic = 1;
  obj->f.fixparam = 0;
  obj->f.optargs = 0;
  return obj;
}

val func_n1v(val (*fun)(val, val rest))
{
  val obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = N1;
  obj->f.env = nil;
  obj->f.f.n1v = fun;
  obj->f.variadic = 1;
  obj->f.fixparam = 1;
  obj->f.optargs = 0;
  return obj;
}

val func_n2v(val (*fun)(val, val, val rest))
{
  val obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = N2;
  obj->f.env = nil;
  obj->f.f.n2v = fun;
  obj->f.variadic = 1;
  obj->f.fixparam = 2;
  obj->f.optargs = 0;
  return obj;
}

val func_n3v(val (*fun)(val, val, val, val rest))
{
  val obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = N3;
  obj->f.env = nil;
  obj->f.f.n3v = fun;
  obj->f.variadic = 1;
  obj->f.fixparam = 3;
  obj->f.optargs = 0;
  return obj;
}

val func_n4v(val (*fun)(val, val, val, val, val rest))
{
  val obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = N4;
  obj->f.env = nil;
  obj->f.f.n4v = fun;
  obj->f.variadic = 1;
  obj->f.fixparam = 4;
  obj->f.optargs = 0;
  return obj;
}

val func_n5v(val (*fun)(val, val, val, val, val, val rest))
{
  val obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = N5;
  obj->f.env = nil;
  obj->f.f.n5v = fun;
  obj->f.variadic = 1;
  obj->f.fixparam = 5;
  obj->f.optargs = 0;
  return obj;
}

val func_n6v(val (*fun)(val, val, val, val, val, val, val rest))
{
  val obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = N6;
  obj->f.env = nil;
  obj->f.f.n6v = fun;
  obj->f.variadic = 1;
  obj->f.fixparam = 6;
  obj->f.optargs = 0;
  return obj;
}

val func_n7v(val (*fun)(val, val, val, val, val, val, val, val rest))
{
  val obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = N7;
  obj->f.env = nil;
  obj->f.f.n7v = fun;
  obj->f.variadic = 1;
  obj->f.fixparam = 7;
  obj->f.optargs = 0;
  return obj;
}

val func_n0o(val (*fun)(void), int reqargs)
{
  val obj = func_n0(fun);
  obj->f.optargs = 0 - reqargs;
  return obj;
}

val func_n1o(val (*fun)(val), int reqargs)
{
  val obj = func_n1(fun);
  obj->f.optargs = 1 - reqargs;
  return obj;
}

val func_n2o(val (*fun)(val, val), int reqargs)
{
  val obj = func_n2(fun);
  obj->f.optargs = 2 - reqargs;
  return obj;
}

val func_n3o(val (*fun)(val, val, val), int reqargs)
{
  val obj = func_n3(fun);
  obj->f.optargs = 3 - reqargs;
  return obj;
}

val func_n4o(val (*fun)(val, val, val, val), int reqargs)
{
  val obj = func_n4(fun);
  obj->f.optargs = 4 - reqargs;
  return obj;
}

val func_interp(val env, val form)
{
  val obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = FINTERP;
  obj->f.env = env;
  obj->f.f.interp_fun = form;
  obj->f.variadic = 1;
  obj->f.fixparam = 0;
  obj->f.optargs = 0;
  return obj;
}

val func_get_form(val fun)
{
  type_check(fun, FUN);
  if (fun->f.functype != FINTERP)
    uw_throwf(error_s, lit("func-get-form: ~a is not an interpreted function"),
              fun, nao);
  return fun->f.f.interp_fun;
}

val func_get_env(val fun)
{
  type_check(fun, FUN);
  return fun->f.env;
}

val func_set_env(val fun, val env)
{
  type_check(fun, FUN);
  set(mkloc(fun->f.env, fun), env);
  return env;
}

val functionp(val obj)
{
  return type(obj) == FUN ? t : nil;
}

val interp_fun_p(val obj)
{
  return (functionp(obj) && obj->f.functype == FINTERP) ? t : nil;
}

val generic_funcall(val fun, val arg[], int nargs)
{
  int variadic, fixparam, reqargs;

  switch (type(fun)) {
  case FUN:
    break;
  case NIL:
  case CONS:
  case LCONS:
  case VEC:
  case STR:
  case LIT:
  case LSTR:
    switch (nargs) {
    case 0:
      uw_throw(error_s, lit("call: missing required arguments"));
    case 1:
      if (consp(arg[0])) {
        cons_bind (x, y, arg[0]);
        if (atom(y))
          return sub(fun, x, y);
        return sel(fun, arg[0]);
      }
      if (vectorp(arg[0]))
        return sel(fun, arg[0]);
      return ref(fun, arg[0]);
    case 2:
      return sub(fun, arg[0], arg[1]);
    default:
      uw_throw(error_s, lit("call: too many arguments"));
    }
  case SYM:
    {
      val binding = lookup_fun(nil, fun);
      if (!binding) {
        uw_throwf(error_s, lit("call: symbol ~s has no function binding"),
                  fun, nao);
        abort();
      }
      fun = cdr(binding);
    }
    break;
  case COBJ:
    if (fun->co.cls == hash_s) {
      switch (nargs) {
      case 0:
        uw_throw(error_s, lit("call: missing required arguments"));
      case 1:
        return gethash(fun, arg[0]);
      case 2:
        return gethash_n(fun, arg[0], arg[1]);
      default:
        uw_throw(error_s, lit("call: too many arguments"));
      }
    }
    /* fallthrough */
  default:
    type_mismatch(lit("call: ~s is not callable"), fun, nao);
  }

  variadic = fun->f.variadic;
  fixparam = fun->f.fixparam;
  reqargs = fixparam - fun->f.optargs;

  if (!variadic) {
    if (nargs < reqargs)
      uw_throw(error_s, lit("call: missing required arguments"));

    if (nargs > fixparam)
      uw_throw(error_s, lit("call: too many arguments"));

    for (; nargs < fixparam; )
      arg[nargs++] = colon_k;

    switch (fun->f.functype) {
    case F0:
      return fun->f.f.f0(fun->f.env);
    case F1:
      return fun->f.f.f1(fun->f.env, arg[0]);
    case F2:
      return fun->f.f.f2(fun->f.env, arg[0], arg[1]);
    case F3:
      return fun->f.f.f3(fun->f.env, arg[0], arg[1], arg[2]);
    case F4:
      return fun->f.f.f4(fun->f.env, arg[0], arg[1], arg[2], arg[3]);
    case N0:
      return fun->f.f.n0();
    case N1:
      return fun->f.f.n1(arg[0]);
    case N2:
      return fun->f.f.n2(arg[0], arg[1]);
    case N3:
      return fun->f.f.n3(arg[0], arg[1], arg[2]);
    case N4:
      return fun->f.f.n4(arg[0], arg[1], arg[2], arg[3]);
    case N5:
      return fun->f.f.n5(arg[0], arg[1], arg[2], arg[3], arg[4]);
    case N6:
      return fun->f.f.n6(arg[0], arg[1], arg[2], arg[3], arg[4], arg[5]);
    case N7:
      return fun->f.f.n7(arg[0], arg[1], arg[2], arg[3], arg[4], arg[5], arg[6]);
    case FINTERP:
      internal_error("unsupported function type");
    }
  } else {
    val arglist = nil;

    if (nargs < reqargs)
      uw_throw(error_s, lit("call: missing required arguments"));

    for (; nargs < fixparam; )
      arg[nargs++] = colon_k;

    for (; nargs > fixparam; )
      arglist = cons(arg[--nargs], arglist);

    switch (fun->f.functype) {
    case FINTERP:
      return interp_fun(fun->f.env, fun->f.f.interp_fun, arglist);
    case F0:
      return fun->f.f.f0v(fun->f.env, arglist);
    case F1:
      return fun->f.f.f1v(fun->f.env, arg[0], arglist);
    case F2:
      return fun->f.f.f2v(fun->f.env, arg[0], arg[1], arglist);
    case F3:
      return fun->f.f.f3v(fun->f.env, arg[0], arg[1], arg[2], arglist);
    case F4:
      return fun->f.f.f4v(fun->f.env, arg[0], arg[1], arg[2], arg[3], arglist);
    case N0:
      return fun->f.f.n0v(arglist);
    case N1:
      return fun->f.f.n1v(arg[0], arglist);
    case N2:
      return fun->f.f.n2v(arg[0], arg[1], arglist);
    case N3:
      return fun->f.f.n3v(arg[0], arg[1], arg[2], arglist);
    case N4:
      return fun->f.f.n4v(arg[0], arg[1], arg[2], arg[3], arglist);
    case N5:
      return fun->f.f.n5v(arg[0], arg[1], arg[2], arg[3], arg[4], arglist);
    case N6:
      return fun->f.f.n6v(arg[0], arg[1], arg[2], arg[3], arg[4], arg[5], arglist);
    case N7:
      return fun->f.f.n7v(arg[0], arg[1], arg[2], arg[3], arg[4], arg[5], arg[6], arglist);
    }
  }

  internal_error("corrupt function type field");
}

val funcall(val fun)
{
  if (fun->f.optargs || type(fun) != FUN) {
    val arg[32] = { nil };
    return generic_funcall(fun, arg, 0);
  }

  if (fun->f.variadic) {
    switch (fun->f.functype) {
    case FINTERP:
      return interp_fun(fun->f.env, fun->f.f.interp_fun, nil);
    case F0:
      return fun->f.f.f0v(fun->f.env, nil);
    case N0:
      return fun->f.f.n0v(nil);
    default:
      break;
    }
  } else {
    switch (fun->f.functype) {
    case F0:
      return fun->f.f.f0(fun->f.env);
    case N0:
      return fun->f.f.n0();
    default:
      break;
    }
  }
  uw_throw(error_s, lit("call: wrong number of arguments"));
}

val funcall1(val fun, val arg)
{
  if (fun->f.optargs || type(fun) != FUN) {
    val args[32];
    args[0] = arg;
    return generic_funcall(fun, args, 1);
  }

  if (fun->f.variadic) {
    switch (fun->f.functype) {
    case FINTERP:
      return interp_fun(fun->f.env, fun->f.f.interp_fun, cons(arg, nil));
    case F0:
      return fun->f.f.f0v(fun->f.env, cons(arg, nil));
    case N0:
      return fun->f.f.n0v(cons(arg, nil));
    case F1:
      return fun->f.f.f1v(fun->f.env, arg, nil);
    case N1:
      return fun->f.f.n1v(arg, nil);
    default:
      break;
    }
  } else {
    switch (fun->f.functype) {
    case F1:
      return fun->f.f.f1(fun->f.env, arg);
    case N1:
      return fun->f.f.n1(arg);
    default:
      break;
    }
  }
  uw_throw(error_s, lit("call: wrong number of arguments"));
}

val funcall2(val fun, val arg1, val arg2)
{
  if (fun->f.optargs || type(fun) != FUN) {
    val arg[32];
    arg[0] = arg1;
    arg[1] = arg2;
    return generic_funcall(fun, arg, 2);
  }

  if (fun->f.variadic) {
    switch (fun->f.functype) {
    case FINTERP:
      return interp_fun(fun->f.env, fun->f.f.interp_fun,
                        cons(arg1, cons(arg2, nil)));
    case F0:
      return fun->f.f.f0v(fun->f.env, cons(arg1, cons(arg2, nil)));
    case N0:
      return fun->f.f.n0v(cons(arg1, cons(arg2, nil)));
    case F1:
      return fun->f.f.f1v(fun->f.env, arg1, cons(arg2, nil));
    case N1:
      return fun->f.f.n1v(arg1, cons(arg2, nil));
    case F2:
      return fun->f.f.f2v(fun->f.env, arg1, arg2, nil);
    case N2:
      return fun->f.f.n2v(arg1, arg2, nil);
    default:
      break;
    }
  } else {
    switch (fun->f.functype) {
    case F2:
      return fun->f.f.f2(fun->f.env, arg1, arg2);
    case N2:
      return fun->f.f.n2(arg1, arg2);
    default:
      break;
    }
  }
  uw_throw(error_s, lit("call: wrong number of arguments"));
}

val funcall3(val fun, val arg1, val arg2, val arg3)
{
  if (fun->f.optargs || type(fun) != FUN) {
    val arg[32];
    arg[0] = arg1;
    arg[1] = arg2;
    arg[2] = arg3;
    return generic_funcall(fun, arg, 3);
  }

  if (fun->f.variadic) {
    switch (fun->f.functype) {
    case FINTERP:
      return interp_fun(fun->f.env, fun->f.f.interp_fun,
                        cons(arg1, cons(arg2, cons(arg3, nil))));
    case F0:
      return fun->f.f.f0v(fun->f.env, cons(arg1, cons(arg2, cons(arg3, nil))));
    case N0:
      return fun->f.f.n0v(cons(arg1, cons(arg2, cons(arg3, nil))));
    case F1:
      return fun->f.f.f1v(fun->f.env, arg1, cons(arg2, cons(arg3, nil)));
    case N1:
      return fun->f.f.n1v(arg1, cons(arg2, cons(arg3, nil)));
    case F2:
      return fun->f.f.f2v(fun->f.env, arg1, arg2, cons(arg3, nil));
    case N2:
      return fun->f.f.n2v(arg1, arg2, cons(arg3, nil));
    case F3:
      return fun->f.f.f3v(fun->f.env, arg1, arg2, arg3, nil);
    case N3:
      return fun->f.f.n3v(arg1, arg2, arg3, nil);
    default:
      break;
    }
  } else {
    switch (fun->f.functype) {
    case F3:
      return fun->f.f.f3(fun->f.env, arg1, arg2, arg3);
    case N3:
      return fun->f.f.n3(arg1, arg2, arg3);
    default:
      break;
    }
  }
  uw_throw(error_s, lit("call: wrong number of arguments"));
}

val funcall4(val fun, val arg1, val arg2, val arg3, val arg4)
{
  if (fun->f.optargs || type(fun) != FUN) {
    val arg[32];
    arg[0] = arg1;
    arg[1] = arg2;
    arg[2] = arg3;
    arg[3] = arg4;
    return generic_funcall(fun, arg, 4);
  }

  if (fun->f.variadic) {
    switch (fun->f.functype) {
    case FINTERP:
      return interp_fun(fun->f.env, fun->f.f.interp_fun,
                        cons(arg1, cons(arg2, cons(arg3, cons(arg4, nil)))));
    case F0:
      return fun->f.f.f0v(fun->f.env, cons(arg1, cons(arg2, cons(arg3, cons(arg4, nil)))));
    case N0:
      return fun->f.f.n0v(cons(arg1, cons(arg2, cons(arg3, cons(arg4, nil)))));
    case F1:
      return fun->f.f.f1v(fun->f.env, arg1, cons(arg2, cons(arg3, cons(arg4, nil))));
    case N1:
      return fun->f.f.n1v(arg1, cons(arg2, cons(arg3, cons(arg4, nil))));
    case F2:
      return fun->f.f.f2v(fun->f.env, arg1, arg2, cons(arg3, cons(arg4, nil)));
    case N2:
      return fun->f.f.n2v(arg1, arg2, cons(arg3, cons(arg4, nil)));
    case F3:
      return fun->f.f.f3v(fun->f.env, arg1, arg2, arg3, cons(arg4, nil));
    case N3:
      return fun->f.f.n3v(arg1, arg2, arg3, cons(arg4, nil));
    case F4:
      return fun->f.f.f4v(fun->f.env, arg1, arg2, arg3, arg4, nil);
    case N4:
      return fun->f.f.n4v(arg1, arg2, arg3, arg4, nil);
    default:
      break;
    }
  } else {
    switch (fun->f.functype) {
    case F4:
      return fun->f.f.f4(fun->f.env, arg1, arg2, arg3, arg4);
    case N4:
      return fun->f.f.n4(arg1, arg2, arg3, arg4);
    default:
      break;
    }
  }
  uw_throw(error_s, lit("call: wrong number of arguments"));
}

val reduce_left(val fun, val list, val init, val key)
{
  if (null_or_missing_p(key))
    key = identity_f;

  if (missingp(init)) {
    if (list)
      init = pop(&list);
    else
      return funcall(fun);
  }

  for (; list; list = cdr(list))
    init = funcall2(fun, init, funcall1(key, car(list)));

  return init;
}

val reduce_right(val fun, val list, val init, val key)
{
  if (null_or_missing_p(key))
    key = identity_f;

  if (list) {
    if (missingp(init)) {
      if (!rest(list))
        return funcall1(key, first(list));
      if (!rest(rest(list)))
        return funcall2(fun, funcall1(key, first(list)),
                        funcall1(key, second(list)));
      /* fall through: no init, three or more items in list */
    } else {
      if (!rest(list))
        return funcall2(fun, funcall1(key, first(list)), init);
      /* fall through: init, and two or more items in list */
    }
  } else if (!missingp(init)) {
    return init;
  } else {
    return funcall(fun);
  }

  return funcall2(fun, funcall1(key, car(list)),
                       if3(cdr(list), reduce_right(fun, cdr(list), init, key),
                                      init));
}

static val do_curry_12_2(val fcons, val arg2)
{
  return funcall2(car(fcons), cdr(fcons), arg2);
}

val curry_12_2(val fun2, val arg)
{
  return func_f1(cons(fun2, arg), do_curry_12_2);
}

static val do_curry_12_1(val fcons, val arg1)
{
  return funcall2(car(fcons), arg1, cdr(fcons));
}

val curry_12_1(val fun2, val arg2)
{
  return func_f1(cons(fun2, arg2), do_curry_12_1);
}

static val do_curry_123_3(val fcons, val arg3)
{
  return funcall3(car(fcons), car(cdr(fcons)), cdr(cdr(fcons)), arg3);
}

val curry_123_3(val fun3, val arg1, val arg2)
{
  return func_f1(cons(fun3, cons(arg1, arg2)), do_curry_123_3);
}

static val do_curry_123_2(val fcons, val arg2)
{
  return funcall3(car(fcons), car(cdr(fcons)), arg2, cdr(cdr(fcons)));
}

val curry_123_2(val fun3, val arg1, val arg3)
{
  return func_f1(cons(fun3, cons(arg1, arg3)), do_curry_123_2);
}

static val do_curry_123_1(val fcons, val arg1)
{
  return funcall3(car(fcons), arg1, car(cdr(fcons)), cdr(cdr(fcons)));
}

val curry_123_1(val fun3, val arg2, val arg3)
{
  return func_f1(cons(fun3, cons(arg2, arg3)), do_curry_123_1);
}

static val do_curry_123_23(val fcons, val arg2, val arg3)
{
  return funcall3(car(fcons), cdr(fcons), arg2, arg3);
}

val curry_123_23(val fun3, val arg1)
{
  return func_f2(cons(fun3, arg1), do_curry_123_23);
}

static val do_curry_1234_34(val fcons, val arg3, val arg4)
{
  return funcall4(car(fcons), car(cdr(fcons)), cdr(cdr(fcons)), arg3, arg4);
}

val curry_1234_34(val fun4, val arg1, val arg2)
{
  return func_f2(cons(fun4, cons(arg1, arg2)), do_curry_1234_34);
}

static val do_chain(val fun1_list, val args)
{
  val arg = nil;

  fun1_list = nullify(fun1_list);

  if (fun1_list) {
    arg = apply(car(fun1_list), args, nil);
    fun1_list = cdr(fun1_list);
  }

  for (; fun1_list; fun1_list = cdr(fun1_list))
    arg = funcall1(car(fun1_list), arg);

  return arg;
}

val chain(val first_fun, ...)
{
  va_list vl;
  list_collect_decl (out, iter);

  if (first_fun != nao) {
    val next_fun;
    va_start (vl, first_fun);
    iter = list_collect(iter, first_fun);

    while ((next_fun = va_arg(vl, val)) != nao)
      iter = list_collect(iter, next_fun);

    va_end (vl);
  }

  return func_f0v(out, do_chain);
}

val chainv(val funlist)
{
  return func_f0v(nullify(funlist), do_chain);
}

static val do_and(val fun1_list, val args)
{
  fun1_list = nullify(fun1_list);

  for (; fun1_list; fun1_list = cdr(fun1_list))
    if (nilp(apply(car(fun1_list), args, nil)))
      return nil;

  return t;
}

val andf(val first_fun, ...)
{
  va_list vl;
  list_collect_decl (out, iter);

  if (first_fun != nao) {
    val next_fun;
    va_start (vl, first_fun);
    iter = list_collect(iter, first_fun);

    while ((next_fun = va_arg(vl, val)) != nao)
      iter = list_collect(iter, next_fun);

    va_end (vl);
  }

  return func_f0v(out, do_and);
}

val andv(val funlist)
{
  return func_f0v(nullify(funlist), do_and);
}

static val do_swap_12_21(val fun, val left, val right)
{
  return funcall2(fun, right, left);
}

val swap_12_21(val fun)
{
  return func_f2(fun, do_swap_12_21);
}

static val do_or(val fun1_list, val args)
{
  fun1_list = nullify(fun1_list);

  for (; fun1_list; fun1_list = cdr(fun1_list))
    if (apply(car(fun1_list), args, nil))
      return t;

  return nil;
}

val orf(val first_fun, ...)
{
  va_list vl;
  list_collect_decl (out, iter);

  if (first_fun != nao) {
    val next_fun;
    va_start (vl, first_fun);
    iter = list_collect(iter, first_fun);

    while ((next_fun = va_arg(vl, val)) != nao)
      iter = list_collect(iter, next_fun);

    va_end (vl);
  }

  return func_f0v(out, do_or);
}

val orv(val funlist)
{
  return func_f0v(nullify(funlist), do_or);
}

static val do_iff(val env, val args)
{
  cons_bind (condfun, choices, env);
  cons_bind (thenfun, elsefun, choices);

  return if3(apply(condfun, args, nil),
             if2(thenfun, apply(thenfun, args, nil)),
             if2(elsefun, apply(elsefun, args, nil)));
}

val iff(val condfun, val thenfun, val elsefun)
{
  return func_f0v(cons(condfun, cons(thenfun, elsefun)), do_iff);
}

val iffi(val condfun, val thenfun, val elsefun)
{
  elsefun = default_arg(elsefun, identity_f);
  return func_f0v(cons(condfun, cons(thenfun, elsefun)), do_iff);
}

val vector(val length, val initval)
{
  int i;
  cnum alloc_plus = c_num(length) + 2;
  size_t size = alloc_plus * sizeof (val);
  val *v = ((cnum) (size / sizeof *v) == alloc_plus)
           ? (val *) chk_malloc(size)
           : (val *) uw_throwf(error_s, lit("vector: length ~a is too large"),
                               length, nao);
  val vec = make_obj();
  initval = default_bool_arg(initval);
#if HAVE_VALGRIND
  vec->v.vec_true_start = v;
#endif
  v += 2;
  vec->v.type = VEC;
  vec->v.vec = v;
  v[vec_alloc] = length;
  v[vec_length] = length;
  for (i = 0; i < alloc_plus - 2; i++)
    vec->v.vec[i] = initval;
  return vec;
}

val vectorp(val vec)
{
  return type(vec) == VEC ? t : nil;
}

val vec_set_length(val vec, val length)
{
  type_check(vec, VEC);

  {
    cnum new_length = c_num(length);
    cnum old_length = c_num(vec->v.vec[vec_length]);
    cnum old_alloc = c_num(vec->v.vec[vec_alloc]);
    cnum length_delta = new_length - old_length;
    cnum alloc_delta = new_length - old_alloc;

    if (new_length > (cnum) ((size_t) -1/sizeof (val) - 2))
      uw_throwf(error_s, lit("vec-set-length: cannot extend to length ~s"),
                length, nao);

    if (alloc_delta > 0) {
      cnum new_alloc = max(new_length, 2*old_alloc);
      val *newvec = (val *) chk_realloc((mem_t *) (vec->v.vec - 2),
                                        (new_alloc + 2) * sizeof *newvec);
      vec->v.vec = newvec + 2;
      set(mkloc(vec->v.vec[vec_alloc], vec), num(new_alloc));
#if HAVE_VALGRIND
      vec->v.vec_true_start = newvec;
#endif
    }

    if (length_delta > 0) {
      cnum i;
      for (i = old_length; i < new_length; i++)
        vec->v.vec[i] = nil;
    }

    set(mkloc(vec->v.vec[vec_length], vec), length);
  }

  return vec;
}

val vecref(val vec, val ind)
{
  cnum index = c_num(ind);
  cnum len = c_num(length_vec(vec));
  if (index < 0)
    index = len + index;
  if (index < 0 || index >= len)
    uw_throwf(error_s, lit("vecref: ~s is out of range for vector ~s"),
              ind, vec, nao);
  return vec->v.vec[index];
}

loc vecref_l(val vec, val ind)
{
  cnum index = c_num(ind);
  cnum len = c_num(length_vec(vec));
  if (index < 0)
    index = len + index;
  if (index < 0 || index >= len)
    uw_throwf(error_s, lit("vecref: ~s is out of range for vector ~s"),
              ind, vec, nao);
  return mkloc(vec->v.vec[index], vec);
}

val vec_push(val vec, val item)
{
  val length = length_vec(vec);
  vec_set_length(vec, plus(length, one));
  set(vecref_l(vec, length), item);
  return length;
}

val length_vec(val vec)
{
  type_check(vec, VEC);
  return vec->v.vec[vec_length];
}

val size_vec(val vec)
{
  type_check(vec, VEC);
  return vec->v.vec[vec_alloc];
}

val vector_list(val list)
{
  val vec = vector(zero, nil);
 
  if (!listp(list))
    uw_throwf(error_s, lit("vector-list: list expected, not ~s"), list, nao);

  for (; consp(list); list = cdr(list))
    vec_push(vec, car(list));

  return vec;
}

val list_vector(val vec)
{
  list_collect_decl (list, ptail);
  int i, len;

  type_check(vec, VEC);

  len = c_num(vec->v.vec[vec_length]);

  for (i = 0; i < len; i++)
    ptail = list_collect(ptail, vec->v.vec[i]);

  return list;
}

val copy_vec(val vec_in)
{
  val length = length_vec(vec_in);
  cnum alloc_plus = c_num(length) + 2;
  val vec = make_obj();
  val *v = (val *) chk_malloc(alloc_plus * sizeof *v);
#if HAVE_VALGRIND
  vec->v.vec_true_start = v;
#endif
  v += 2;
  vec->v.type = VEC;
  vec->v.vec = v;
  v[vec_alloc] = length;
  v[vec_length] = length;
  memcpy(vec->v.vec, vec_in->v.vec, (alloc_plus - 2) * sizeof *vec->v.vec);
  return vec;
}

val sub_vec(val vec_in, val from, val to)
{
  val len = length_vec(vec_in);

  if (null_or_missing_p(from))
    from = zero;
  else if (from == t)
    from = len;
  else if (lt(from, zero)) {
    from = plus(from, len);
    if (to == zero)
      to = len;
  }

  if (null_or_missing_p(to) || to == t)
    to = len;
  else if (lt(to, zero))
    to = plus(to, len);

  from = max2(zero, min2(from, len));
  to = max2(zero, min2(to, len));

  if (ge(from, to)) {
    return vector(zero, nil);
  } else {
    cnum cfrom = c_num(from);
    size_t nelem = c_num(to) - cfrom;
    val vec = make_obj();
    val *v = (val *) chk_malloc((nelem + 2) * sizeof *v);
#if HAVE_VALGRIND
    vec->v.vec_true_start = v;
#endif
    v += 2;
    vec->v.type = VEC;
    vec->v.vec = v;
    v[vec_length] = v[vec_alloc] = num(nelem);
    memcpy(vec->v.vec, vec_in->v.vec + cfrom, nelem * sizeof *vec->v.vec);
    return vec;
  }
}

val replace_vec(val vec_in, val items, val from, val to)
{
  val it_seq = toseq(items);
  val len = length_vec(vec_in);
  val len_it = length(it_seq);
  val len_rep;

  if (null_or_missing_p(from))
    from = zero;
  else if (from == t)
    from = len;
  else if (lt(from, zero)) {
    from = plus(from, len);
    if (to == zero)
      to = len;
  }

  if (null_or_missing_p(to) || to == t)
    to = len;
  else if (lt(to, zero))
    to = plus(to, len);

  from = max2(zero, min2(from, len));
  to = max2(zero, min2(to, len));

  len_rep = minus(to, from);

  if (gt(len_rep, len_it)) {
    val len_diff = minus(len_rep, len_it);
    cnum t = c_num(to);
    cnum l = c_num(len);

    memmove(vec_in->v.vec + t - c_num(len_diff), 
            vec_in->v.vec + t,
            (l - t) * sizeof vec_in->v.vec);

    vec_in->v.vec[vec_length] = minus(len, len_diff);
    to = plus(from, len_it);
  } else if (lt(len_rep, len_it)) {
    val len_diff = minus(len_it, len_rep);
    cnum t = c_num(to);
    cnum l = c_num(len);

    vec_set_length(vec_in, plus(len, len_diff));

    memmove(vec_in->v.vec + t + c_num(len_diff), 
            vec_in->v.vec + t,
            (l - t) * sizeof vec_in->v.vec);
    to = plus(from, len_it);
  }
  
  if (zerop(len_it))
    return vec_in;
  if (vectorp(it_seq)) {
    memcpy(vec_in->v.vec + c_num(from), it_seq->v.vec, 
           sizeof *vec_in->v.vec * c_num(len_it));
    mut(vec_in);
  } else if (stringp(it_seq)) {
    cnum f = c_num(from);
    cnum t = c_num(to);
    cnum s;
    const wchar_t *str = c_str(it_seq);

    for (s = 0; f != t; f++, s++)
      vec_in->v.vec[f] = chr(str[s]);
  } else {
    val iter;
    cnum f = c_num(from);
    cnum t = c_num(to);

    for (iter = it_seq; iter && f != t; iter = cdr(iter), f++)
      vec_in->v.vec[f] = car(iter);
    mut(vec_in);
  }
  return vec_in;
}

val cat_vec(val list)
{
  size_t total = 0;
  val iter;
  val vec, *v;

  list = nullify(list);

  for (iter = list; iter != nil; iter = cdr(iter)) {
    size_t newtot = total + c_num(length_vec(car(iter)));
    if (newtot < total)
      goto toobig;
    total = newtot;
  }

  if (total > ((size_t) -1)/(sizeof (val)) - 2)
    goto toobig;

  vec = make_obj();
  v = (val *) chk_malloc((total + 2) * sizeof *v);

#if HAVE_VALGRIND
  vec->v.vec_true_start = v;
#endif
  v += 2;
  vec->v.type = VEC;
  vec->v.vec = v;
  v[vec_length] = v[vec_alloc] = num(total);

  for (iter = list; iter != nil; iter = cdr(iter)) {
    val item = car(iter);
    cnum len = c_num(item->v.vec[vec_length]);
    memcpy(v, item->v.vec, len * sizeof *v);
    v += len;
  }

  return vec;
toobig:
  uw_throwf(error_s, lit("cat-vec: resulting vector too large"), nao);
}

static val simple_lazy_stream_func(val stream, val lcons)
{
  if (set(mkloc(lcons->lc.car, lcons), get_line(stream)) != nil)
    set(mkloc(lcons->lc.cdr, lcons), make_lazy_cons(lcons->lc.func));
  else
    lcons->lc.cdr = nil;

  return nil;
}

static val lazy_stream_cont(val stream, val func, val env)
{
  val next = get_line(stream);

  if (!next) {
    close_stream(stream, t);
    return nil;
  }

  rplacd(env, next);
  return make_lazy_cons(func);
}

static val lazy_stream_func(val env, val lcons)
{
  val stream = car(env);
  val prefetched_line = cdr(env);

  set(mkloc(lcons->lc.car, lcons), prefetched_line);
  set(mkloc(lcons->lc.cdr, lcons), lazy_stream_cont(stream, lcons->lc.func, env));

  return prefetched_line;
}

val lazy_stream_cons(val stream)
{
  if (real_time_stream_p(stream)) {
    return make_lazy_cons(func_f1(stream, simple_lazy_stream_func));
  } else {
    val first = get_line(stream);

    if (!first) {
      close_stream(stream, t);
      return nil;
    }

    return make_lazy_cons(func_f1(cons(stream, first),
                                  lazy_stream_func));
  }
}

val lazy_str(val lst, val term, val limit)
{
  val obj = make_obj();
  obj->ls.type = LSTR;

  /* Must init before calling something that can gc! */
  obj->ls.opts = obj->ls.list = obj->ls.prefix = nil;

  term = default_arg(term, lit("\n"));
  limit = default_bool_arg(limit);

  if (nilp(lst)) {
    obj->ls.prefix = null_string;
    obj->ls.list = nil;
  } else {
    set(mkloc(obj->ls.prefix, obj), cat_str(list(first(lst), term, nao), nil));
    set(mkloc(obj->ls.list, obj), rest(lst));
    limit = if2(limit, minus(limit, one));
  }

  set(mkloc(obj->ls.opts, obj), cons(term, limit));

  return obj;
}

val lazy_str_force(val lstr)
{
  val lim;
  type_check(lstr, LSTR);
  lim = cdr(lstr->ls.opts);

  while ((!lim || gt(lim, zero)) && lstr->ls.list) {
    val next = pop(&lstr->ls.list);
    val term = car(lstr->ls.opts);
    set(mkloc(lstr->ls.prefix, lstr), cat_str(list(lstr->ls.prefix, next, term, nao), nil));
    if (lim)
      lim = minus(lim, one);
  }

  if (lim)
    set(cdr_l(lstr->ls.opts), lim);

  return lstr->ls.prefix;
}

val lazy_str_force_upto(val lstr, val index)
{
  uses_or2;
  val lim;
  type_check(lstr, LSTR);
  lim = cdr(lstr->ls.opts);

  while (ge(index, length_str(lstr->ls.prefix)) && lstr->ls.list &&
         or2(null(lim),gt(lim,zero)))
  {
    val next = pop(&lstr->ls.list);
    val term = car(lstr->ls.opts);
    set(mkloc(lstr->ls.prefix, lstr), cat_str(list(lstr->ls.prefix, next, term, nao), nil));
    if (lim)
      lim = minus(lim, one);
  }

  if (lim)
    set(cdr_l(lstr->ls.opts), lim);
  return lt(index, length_str(lstr->ls.prefix));
}

val length_str_gt(val str, val len)
{
  if (is_lit(str)) {
    const wchar_t *cstr = c_str(str);
    size_t clen = c_num(len);
    const wchar_t *nult = wmemchr(cstr, 0, clen + 1);
    return nult == 0 ? t : nil;
  } else {
    type_check2 (str, STR, LSTR);

    switch (str->t.type) {
    case STR:
      return gt(length_str(str), len);
    case LSTR:
      lazy_str_force_upto(str, len);
      return gt(length_str(str->ls.prefix), len);
    default:
      internal_error("unexpected type value");
    }
  }
}

val length_str_ge(val str, val len)
{
  if (is_lit(str)) {
    const wchar_t *cstr = c_str(str);
    size_t clen = c_num(len);
    const wchar_t *nult = wmemchr(cstr, 0, clen);
    return nult == 0 ? t : nil;
  } else {
    type_check2 (str, STR, LSTR);

    switch (str->t.type) {
    case STR:
      return ge(length_str(str), len);
    case LSTR:
      lazy_str_force_upto(str, len);
      return ge(length_str(str->ls.prefix), len);
    default:
      internal_error("unexpected type value");
    }
  }
}

val length_str_lt(val str, val len)
{
  if (is_lit(str)) {
    const wchar_t *cstr = c_str(str);
    size_t clen = c_num(len);
    const wchar_t *nult = wmemchr(cstr, 0, clen);
    return nult != 0 ? t : nil;
  } else {
    type_check2 (str, STR, LSTR);

    switch (str->t.type) {
    case STR:
      return lt(length_str(str), len);
    case LSTR:
      lazy_str_force_upto(str, len);
      return lt(length_str(str->ls.prefix), len);
    default:
      internal_error("unexpected type value");
    }
  }
}

val length_str_le(val str, val len)
{
  if (is_lit(str)) {
    const wchar_t *cstr = c_str(str);
    size_t clen = c_num(len);
    const wchar_t *nult = wmemchr(cstr, 0, clen + 1);
    return nult != 0 ? t : nil;
  } else {
    type_check2 (str, STR, LSTR);

    switch (str->t.type) {
    case STR:
      return le(length_str(str), len);
    case LSTR:
      lazy_str_force_upto(str, len);
      return le(length_str(str->ls.prefix), len);
    default:
      internal_error("unexpected type value");
    }
  }
}

val lazy_str_get_trailing_list(val lstr, val index)
{
  type_check(lstr, LSTR);

  /* Force lazy string up through the index position */
  if (ge(index, length_str(lstr->ls.prefix)))
    lazy_str_force_upto(lstr, index);

  {
    uses_or2;
    val split_suffix = split_str(sub_str(lstr->ls.prefix, index, nil),
                                 or2(car(lstr->ls.opts), lit("\n")));

    return nappend2(split_suffix, lstr->ls.list);
  }
}

val cobj(mem_t *handle, val cls_sym, struct cobj_ops *ops)
{
  val obj = make_obj();
  obj->co.type = COBJ;
  obj->co.handle = handle;
  obj->co.ops = ops;
  obj->co.cls = cls_sym;
  return obj;
}

val cobjp(val obj)
{
  return type(obj) == COBJ ? t : nil;
}

mem_t *cobj_handle(val cobj, val cls_sym)
{
  class_check(cobj, cls_sym);
  return cobj->co.handle;
}

void cobj_print_op(val obj, val out)
{
  put_string(lit("#<"), out);
  obj_print(obj->co.cls, out);
  format(out, lit(": ~p>"), obj->co.handle, nao);
}

static val cptr_equal_op(val left, val right)
{
  return (left->co.handle == right->co.handle) ? t : nil;
}

static struct cobj_ops cptr_ops = {
  cptr_equal_op,
  cobj_print_op,
  cobj_destroy_stub_op,
  cobj_mark_op,
  cobj_hash_op
};

val cptr(mem_t *ptr)
{
  return cobj(ptr, cptr_s, &cptr_ops);
}

mem_t *cptr_get(val cptr)
{
  return cobj_handle(cptr, cptr_s);
}

val assoc(val key, val list)
{
  list = nullify(list);

  while (list) {
    val elem = car(list);
    if (equal(car(elem), key))
      return elem;
    list = cdr(list);
  }

  return nil;
}

val assql(val key, val list)
{
  list = nullify(list);

  while (list) {
    val elem = car(list);
    if (eql(car(elem), key))
      return elem;
    list = cdr(list);
  }

  return nil;
}

val acons(val car, val cdr, val list)
{
  return cons(cons(car, cdr), list);
}

val acons_new(val key, val value, val list)
{
  val existing = assoc(key, list);

  if (existing) {
    set(cdr_l(existing), value);
    return list;
  } else {
    return cons(cons(key, value), list);
  }
}

val acons_new_c(val key, loc new_p, loc list)
{
  val existing = assoc(key, deref(list));

  if (existing) {
    if (!nullocp(new_p))
      deref(new_p) = nil;
    return existing;
  } else {
    val nc = cons(key, nil);
    set(list, cons(nc, deref(list)));
    if (!nullocp(new_p))
      deref(new_p) = t;
    return nc;
  }
}

val aconsql_new(val key, val value, val list)
{
  val existing = assql(key, list);

  if (existing) {
    set(cdr_l(existing), value);
    return list;
  } else {
    return cons(cons(key, value), list);
  }
}

val aconsql_new_c(val key, loc new_p, loc list)
{
  val existing = assql(key, deref(list));

  if (existing) {
    if (!nullocp(new_p))
      deref(new_p) = nil;
    return existing;
  } else {
    val nc = cons(key, nil);
    set(list, cons(nc, deref(list)));
    if (!nullocp(new_p))
      deref(new_p) = t;
    return nc;
  }
}


static val alist_remove_test(val item, val key)
{
  return equal(car(item), key);
}

val alist_remove(val list, val keys)
{
  return set_diff(list, keys, func_n2(alist_remove_test), nil);
}

val alist_remove1(val list, val key)
{
  return alist_remove(list, cons(key, nil));
}

val alist_nremove(val list, val keys)
{
  loc plist = mkcloc(list);

  while (deref(plist)) {
    if (memqual(car(car(deref(plist))), keys))
      deref(plist) = cdr(deref(plist));
    else
      plist = cdr_l(deref(plist));
  }

  return list;
}

val alist_nremove1(val list, val key)
{
  loc plist = mkcloc(list);

  while (deref(plist)) {
    if (equal(car(car(deref(plist))), key))
      deref(plist) = cdr(deref(plist));
    else
      plist = cdr_l(deref(plist));
  }

  return list;
}

val copy_cons(val c)
{
  return cons(car(c), cdr(c));
}

val copy_alist(val list)
{
  return mapcar(func_n1(copy_cons), list);
}

val mapcar(val fun, val list)
{
  list_collect_decl (out, iter);
  val list_orig = list;

  list = nullify(list);

  for (; list; list = cdr(list))
    iter = list_collect(iter, funcall1(fun, car(list)));

  return make_like(out, list_orig);
}

val mapcon(val fun, val list)
{
  list_collect_decl (out, iter);
  val list_orig = list;

  list = nullify(list);

  for (; list; list = cdr(list))
    iter = list_collect_nconc(iter, funcall1(fun, list));

  return make_like(out, list_orig);
}

val mappend(val fun, val list)
{
  list_collect_decl (out, iter);
  val list_orig = list;

  list = nullify(list);

  for (; list; list = cdr(list))
    iter = list_collect_append(iter, funcall1(fun, car(list)));

  return make_like(out, list_orig);
}

val merge(val list1, val list2, val lessfun, val keyfun)
{
  list_collect_decl (out, ptail);

  while (list1 && list2) {
    val el1 = funcall1(keyfun, first(list1));
    val el2 = funcall1(keyfun, first(list2));

    if (funcall2(lessfun, el1, el2)) {
      val next = cdr(list1);
      deref(cdr_l(list1)) = nil;
      ptail = list_collect_nconc(ptail, list1);
      list1 = next;
    } else {
      val next = cdr(list2);
      deref(cdr_l(list2)) = nil;
      ptail = list_collect_nconc(ptail, list2);
      list2 = next;
    }
  }

  if (list1)
    ptail = list_collect_nconc(ptail, list1);
  else
    ptail = list_collect_nconc(ptail, list2);

  return out;
}

static val sort_list(val list, val lessfun, val keyfun)
{
  if (list == nil)
    return nil;
  if (!cdr(list))
    return list;
  if (!cdr(cdr(list))) {
    if (funcall2(lessfun, funcall1(keyfun, first(list)),
                          funcall1(keyfun, second(list))))
    {
      return list;
    } else {
      val cons2 = cdr(list);
      /* This assignent is a dangerous mutation since the list
         may contain mixtures of old and new objects, and
         so we could be reversing a newer->older pointer
         relationship. */
      set(cdr_l(cons2), list);
      deref(cdr_l(list)) = nil;
      return cons2;
    }
  }

  {
    val bisect, iter;
    val list2;

    for (iter = cdr(cdr(list)), bisect = list; iter;
         bisect = cdr(bisect), iter = cdr(cdr(iter)))
      ; /* empty */

    list2 = cdr(bisect);
    deref(cdr_l(bisect)) = nil;

    return merge(sort_list(list, lessfun, keyfun),
                 sort_list(list2, lessfun, keyfun),
                 lessfun, keyfun);
  }
}

static void swap(val vec, val i, val j)
{
  if (i != j) {
    val temp = ref(vec, i);
    refset(vec, i, ref(vec, j));
    refset(vec, j, temp);
  }
}

static void quicksort(val vec, val lessfun, val keyfun, cnum from, cnum to)
{
  if (to - from >= 2) {
    cnum pivot = from + (to - from) / 2;
    cnum i, j;
    val pval = ref(vec, num_fast(pivot));
    val pkval = funcall1(keyfun, pval);

    swap(vec, num_fast(pivot), num_fast(to - 1));

    for (j = from, i = from; i < to - 1; i++)
      if (funcall2(lessfun, funcall1(keyfun, ref(vec, num_fast(i))), pkval))
        swap(vec, num_fast(i), num_fast(j++));

    swap(vec, num_fast(j), num_fast(to - 1));

    quicksort(vec, lessfun, keyfun, from, j);
    quicksort(vec, lessfun, keyfun, j + 1, to);
  }
}

static void sort_vec(val vec, val lessfun, val keyfun)
{
  cnum len = c_num(length(vec));
  quicksort(vec, lessfun, keyfun, 0, len);
}

val sort(val seq_in, val lessfun, val keyfun)
{
  val seq_orig = seq_in;
  val seq = nullify(seq_in);

  if (!seq)
    return make_like(nil, seq_orig);

  keyfun = default_arg(keyfun, identity_f);

  if (consp(seq)) {
    /* The list could have a mixture of generation 0 and 1
       objects. Sorting the list could reverse some of the
       pointers between the generations resulting in a backpointer.
       Thus we better inform the collector about this object. */
    return sort_list(seq, lessfun, keyfun);
  }

  sort_vec(seq, lessfun, keyfun);
  return seq;
}

static val multi_sort_less(val funcs_cons, val llist, val rlist)
{
  cons_bind (funcs, key_funcs, funcs_cons);
  val less = t;

  while (funcs) {
    val func = pop(&funcs);
    val test = pop(&key_funcs);
    val left = if3(test, funcall1(test, pop(&llist)), pop(&llist));
    val right = if3(test, funcall1(test, pop(&rlist)), pop(&rlist));

    if (funcall2(func, left, right))
      break;

    if (funcall2(func, right, left)) {
      less = nil;
      break;
    }
  }

  return less;
}

val multi_sort(val lists, val funcs, val key_funcs)
{
  val tuples = mapcarv(func_n0v(identity), nullify(lists));

  key_funcs = default_bool_arg(key_funcs);

  if (functionp(funcs))
    funcs = cons(funcs, nil);

  tuples = sort_list(tuples, func_f2(cons(funcs, key_funcs),
                                     multi_sort_less), identity_f);

  return mapcarv(func_n0v(identity), tuples);
}

val find(val item, val list, val testfun, val keyfun)
{
  testfun = default_arg(testfun, equal_f);
  keyfun = default_arg(keyfun, identity_f);

  list = nullify(list);

  for (; list; list = cdr(list)) {
    val elem = car(list);
    val key = funcall1(keyfun, elem);

    if (funcall2(testfun, item, key))
      return elem;
  }

  return nil;
}

val find_max(val seq_in, val testfun, val keyfun)
{
  val seq = nullify(seq_in);
  val maxkey;
  val maxelt;

  if (!seq)
    return nil;

  testfun = default_arg(testfun, gt_f);
  keyfun = default_arg(keyfun, identity_f);

  maxelt = car(seq_in);
  maxkey = funcall1(keyfun, maxelt);

  for (seq = cdr(seq); seq; seq = cdr(seq)) {
    val elt = car(seq);
    val key = funcall1(keyfun, elt);
    if (funcall2(testfun, key, maxkey)) {
      maxkey = key;
      maxelt = elt;
    }
  }

  return maxelt;
}

val find_min(val seq, val testfun, val keyfun)
{
  return find_max(seq, default_arg(testfun, lt_f), keyfun);
}

val find_if(val pred, val list, val key)
{
  key = default_arg(key, identity_f);
  list = nullify(list);

  for (; list; list = cdr(list)) {
    val item = car(list);
    val subj = funcall1(key, item);

    if (funcall1(pred, subj))
      return item;
  }

  return nil;
}

val posqual(val obj, val list)
{
  val pos = zero;

  list = nullify(list);

  for (; list; list = cdr(list), pos = plus(pos, one))
    if (equal(car(list), obj))
      return pos;

  return nil;
}

val posql(val obj, val list)
{
  val pos = zero;

  list = nullify(list);

  for (; list; list = cdr(list), pos = plus(pos, one))
    if (eql(car(list), obj))
      return pos;

  return nil;
}

val posq(val obj, val list)
{
  val pos = zero;

  list = nullify(list);

  for (; list; list = cdr(list), pos = plus(pos, one))
    if (car(list) == obj)
      return pos;

  return nil;
}

val pos(val item, val list, val testfun, val keyfun)
{
  val pos = zero;
  testfun = default_arg(testfun, equal_f);
  keyfun = default_arg(keyfun, identity_f);
  list = nullify(list);

  for (; list; list = cdr(list), pos = plus(pos, one)) {
    val elem = car(list);
    val key = funcall1(keyfun, elem);

    if (funcall2(testfun, item, key))
      return pos;
  }

  return nil;
}


val pos_if(val pred, val list, val key)
{
  val pos = zero;
  key = default_arg(key, identity_f);
  list = nullify(list);

  for (; list; list = cdr(list), pos = plus(pos, one)) {
    val item = car(list);
    val subj = funcall1(key, item);

    if (funcall1(pred, subj))
      return pos;
  }

  return nil;
}

val pos_max(val seq_in, val testfun, val keyfun)
{
  val pos = zero;
  val seq = nullify(seq_in);
  val maxkey;
  val maxpos = zero;

  if (!seq)
    return nil;

  testfun = default_arg(testfun, gt_f);
  keyfun = default_arg(keyfun, identity_f);

  maxkey = funcall1(keyfun, car(seq));

  for (seq = cdr(seq); seq; seq = cdr(seq)) {
    val key = funcall1(keyfun, car(seq));
    pos = plus(pos, one);
    if (funcall2(testfun, key, maxkey)) {
      maxkey = key;
      maxpos = pos;
    }
  }

  return maxpos;
}

val pos_min(val seq, val testfun, val keyfun)
{
  return pos_max(seq, default_arg(testfun, lt_f), keyfun);
}

val set_diff(val list1, val list2, val testfun, val keyfun)
{
  list_collect_decl (out, ptail);
  val list_orig = list1;

  list1 = nullify(list1);
  list2 = nullify(list2);

  testfun = default_arg(testfun, equal_f);
  keyfun = default_arg(keyfun, identity_f);

  for (; list1; list1 = cdr(list1)) {
    /* optimization: list2 is a tail of list1, and so we
       are done, unless the application has a weird test function. */
    if (list1 == list2) {
      break;
    } else {
      val item = car(list1);
      val list1_key = funcall1(keyfun, item);

      if (!find(list1_key, list2, testfun, keyfun))
        ptail = list_collect(ptail, item);
    }
  }

  return make_like(out, list_orig);
}

val copy(val seq)
{
  switch (type(seq)) {
  case NIL:
    return nil;
  case CONS:
  case LCONS:
    return copy_list(seq);
  case LIT:
  case STR:
  case LSTR:
    return copy_str(seq);
  case VEC:
    return copy_vec(seq);
  case COBJ:
    if (seq->co.cls == hash_s)
      return copy_hash(seq);
    /* fallthrough */
  default:
    type_mismatch(lit("copy: ~s is not a sequence"), seq, nao);
  }
}

val length(val seq)
{
  switch (type(seq)) {
  case NIL:
    return num(0);
  case CONS:
  case LCONS:
    return length_list(seq);
  case LIT:
  case STR:
  case LSTR:
    return length_str(seq);
  case VEC:
    return length_vec(seq);
  case COBJ:
    if (seq->co.cls == hash_s)
      return hash_count(seq);
    /* fallthrough */
  default:
    type_mismatch(lit("length: ~s is not a sequence"), seq, nao);
  }
}

val empty(val seq)
{
  switch (type(seq)) {
  case NIL:
    return t;
  case CONS:
  case LCONS:
    return nil;
  case LIT:
  case STR:
    return if2(c_str(seq)[0] == 0, t);
  case LSTR:
    return length_str_le(seq, zero);
  case VEC:
    return eq(length_vec(seq), zero);
  case COBJ:
    if (seq->co.cls == hash_s)
      return eq(hash_count(seq), zero);
    /* fallthrough */
  default:
    type_mismatch(lit("empty: ~s is not a sequence"), seq, nao);
  }
}

val sub(val seq, val from, val to)
{
  switch (type(seq)) {
  case NIL:
    return nil;
  case CONS:
  case LCONS:
    return sub_list(seq, from, to);
  case LIT:
  case STR:
    return sub_str(seq, from, to);
  case VEC:
    return sub_vec(seq, from, to);
  default:
    type_mismatch(lit("sub: ~s is not a sequence"), cons, nao);
  }
}

val ref(val seq, val ind)
{
  switch (type(seq)) {
  case NIL:
    return nil;
  case CONS:
  case LCONS:
    return listref(seq, ind);
  case LIT:
  case STR:
  case LSTR:
    return chr_str(seq, ind);
  case VEC:
    return vecref(seq, ind);
  default:
    type_mismatch(lit("ref: ~s is not a sequence"), cons, nao);
  }
}

val refset(val seq, val ind, val newval)
{
  switch (type(seq)) {
  case NIL:
  case CONS:
  case LCONS:
    return set(listref_l(seq, ind), newval);
  case LIT:
  case STR:
  case LSTR:
    return chr_str_set(seq, ind, newval);
  case VEC:
    return set(vecref_l(seq, ind), newval);
  default:
    type_mismatch(lit("ref: ~s is not a sequence"), cons, nao);
  }
  return newval;
}

val replace(val seq, val items, val from, val to)
{
  switch (type(seq)) {
  case NIL:
  case CONS:
  case LCONS:
    return replace_list(seq, items, from, to);
  case LIT:
  case STR:
  case LSTR:
    return replace_str(seq, items, from, to);
  case VEC:
    return replace_vec(seq, items, from, to);
  default:
    type_mismatch(lit("replace: ~s is not a sequence"), cons, nao);
  }
}

val update(val seq, val fun)
{
  switch (type(seq)) {
  case NIL:
    break;
  case CONS:
  case LCONS:
    {
      val iter = seq;

      while (consp(iter)) {
        rplaca(iter, funcall1(fun, car(iter)));
        iter = cdr(iter);
      }
    }
    break;
  case LIT:
  case STR:
  case LSTR:
  case VEC:
    {
      val len = length(seq);
      val i;
      for (i = zero; lt(i, len); i = plus(i, one))
        refset(seq, i, funcall1(fun, ref(seq, i)));
    }
    break;
  case COBJ:
    if (hashp(seq))
      return hash_update(seq, fun);
    /* fallthrough */
  default:
    type_mismatch(lit("replace: ~s is not a sequence"), cons, nao);
  }

  return seq;
}

static val search_list(val seq, val key, val testfun, val keyfun)
{
  val iter, siter, kiter;
  val pos = zero;

  switch (type(key)) {
  case NIL:
    return pos;
  case CONS:
  case LCONS:
  case LIT:
  case STR:
  case LSTR:
  case VEC:
    /* TODO: optimize me */
    for (iter = seq; iter; iter = cdr(iter)) {
      for (siter = iter, kiter = key;
           siter && kiter;
           siter = cdr(siter), kiter = cdr(kiter))
      {
        if (!funcall2(testfun,
                      funcall1(keyfun, car(siter)),
                      funcall1(keyfun, car(kiter))))
        {
          pos = plus(pos, one);
          break;
        }
      }

      if (!kiter)
        return pos;

      if (!siter)
        break;
    }
    break;
  default:
    type_mismatch(lit("search: ~s is not a sequence"), cons, nao);
  }

  return nil;
}

val search(val seq, val key, val testfun, val keyfun)
{
  testfun = default_arg(testfun, equal_f);
  keyfun = default_arg(keyfun, identity_f);
  seq = nullify(seq);

  switch (type(seq)) {
  case NIL:
    return if3(length(key) == zero, zero, nil);
  case CONS:
  case LCONS:
  case LIT:
  case STR:
  case LSTR:
  case VEC:
    /* TODO: optimize me */
    return search_list(seq, key, testfun, keyfun);
  default:
    type_mismatch(lit("search: ~s is not a sequence"), cons, nao);
  }

  return seq;
}

val where(val seq_in, val func)
{
  list_collect_decl (out, ptail);
  val seq = nullify(seq_in);
  val idx = zero;

  for (; seq; seq = cdr(seq), idx = plus(idx, one)) {
    val elt = car(seq);
    if (funcall1(func, elt))
      list_collect(ptail, idx);
  }

  return out;
}

val sel(val seq_in, val where_in)
{
  list_collect_decl (out, ptail);
  val seq = nullify(seq_in);
  val where = nullify(where_in);

  switch (type(seq)) {
  case NIL:
    return nil;
  case CONS:
  case LCONS:
    {
      val idx = zero;

      for (; seq && where; seq = cdr(seq), idx = plus(idx, one)) {
        val wh;

        do {
          wh = car(where);
          where = cdr(where);
        } while (lt(wh, idx));

        if (eql(wh, idx))
          list_collect (ptail, car(seq));
      }
    }
    break;
  default:
    {
      val len = length(seq);
      for (; where; where = cdr(where)) {
        val wh = car(where);
        if (ge(wh, len))
          break;
        list_collect (ptail, ref(seq, car(where)));
      }
    }
    break;
  }
  return make_like(out, seq_in);
}

val env(void)
{
  if (env_list) {
    return env_list;
  } else {
    list_collect_decl (out, ptail);
#if HAVE_ENVIRON
    extern char **environ;
    char **iter = environ;

    for (; *iter != 0; iter++)
      ptail = list_collect(ptail, string_utf8(*iter));

    return env_list = out;
#elif HAVE_GETENVIRONMENTSTRINGS
    wchar_t *env = GetEnvironmentStringsW();
    wchar_t *iter = env;

    if (iter == 0)
      uw_throwf(error_s, lit("out of memory"), nao);

    for (; *iter; iter += wcslen(iter) + 1)
      ptail = list_collect(ptail, string(iter));

    FreeEnvironmentStringsW(env);

    return env_list = out;
#else
    uw_throwf(error_s, lit("environment strings not available"), nao);
#endif
  }
}

static void obj_init(void)
{
  /*
   * No need to GC-protect the convenience variables which hold the interned
   * symbols, because the interned_syms list holds a reference to all the
   * symbols.
   */

  protect(&packages, &system_package_var, &keyword_package_var,
          &user_package_var, &null_string, &nil_string,
          &null_list, &equal_f, &eq_f, &eql_f, &gt_f, &lt_f,
          &car_f, &cdr_f, &null_f,
          &identity_f, &prog_string, &env_list,
          (val *) 0);

  nil_string = lit("nil");
  null_string = lit("");
  null_list = cons(nil, nil);

  hash_s = make_sym(lit("hash"));
  system_package = make_package(lit("sys"));
  keyword_package = make_package(lit("keyword"));
  user_package = make_package(lit("usr"));

  rehome_sym(hash_s, user_package);

  /* nil can't be interned because it's not a SYM object;
     it works as a symbol because the nil case is handled by
     symbol-manipulating function. */
  rplacd(gethash_c(user_package->pk.symhash, nil_string, nulloc), nil);

  /* t can't be interned, because intern needs t in order to do its job. */
  t = cdr(rplacd(gethash_c(user_package->pk.symhash,
                           lit("t"), nulloc), make_sym(lit("t"))));
  set(mkloc(t->s.package, t), user_package);

  null_s = intern(lit("null"), user_package);
  cons_s = intern(lit("cons"), user_package);
  str_s = intern(lit("str"), user_package);
  lit_s = intern(lit("lit"), user_package);
  chr_s = intern(lit("chr"), user_package);
  fixnum_s = intern(lit("fixnum"), user_package);
  sym_s = intern(lit("sym"), user_package);
  pkg_s = intern(lit("pkg"), user_package);
  fun_s = intern(lit("fun"), user_package);
  vec_s = intern(lit("vec"), user_package);
  stream_s = intern(lit("stream"), user_package);
  hash_s = intern(lit("hash"), user_package);
  hash_iter_s = intern(lit("hash-iter"), user_package);
  lcons_s = intern(lit("lcons"), user_package);
  lstr_s = intern(lit("lstr"), user_package);
  cobj_s = intern(lit("cobj"), user_package);
  cptr_s = intern(lit("cptr"), user_package);
  env_s = intern(lit("env"), user_package);
  bignum_s = intern(lit("bignum"), user_package);
  float_s = intern(lit("float"), user_package);
  var_s = intern(lit("var"), system_package);
  expr_s = intern(lit("expr"), system_package);
  regex_s = intern(lit("regex"), system_package);
  nongreedy_s = intern(lit("ng0+"), user_package);
  compiled_regex_s = intern(lit("compiled-regex"), system_package);
  quote_s = intern(lit("quote"), user_package);
  qquote_s = intern(lit("qquote"), user_package);
  unquote_s = intern(lit("unquote"), user_package);
  splice_s = intern(lit("splice"), user_package);
  sys_qquote_s = intern(lit("qquote"), system_package);
  sys_unquote_s = intern(lit("unquote"), system_package);
  sys_splice_s = intern(lit("splice"), system_package);
  chset_s = intern(lit("chset"), system_package);
  set_s = intern(lit("set"), user_package);
  cset_s = intern(lit("cset"), user_package);
  wild_s = intern(lit("wild"), user_package);
  oneplus_s = intern(lit("1+"), user_package);
  zeroplus_s = intern(lit("0+"), user_package);
  optional_s = intern(lit("?"), user_package);
  compl_s = intern(lit("~"), user_package);
  compound_s = intern(lit("compound"), user_package);
  or_s = intern(lit("or"), user_package);
  and_s = intern(lit("and"), user_package);
  quasi_s = intern(lit("quasi"), system_package);
  quasilist_s = intern(lit("quasilist"), system_package);
  skip_s = intern(lit("skip"), user_package);
  trailer_s = intern(lit("trailer"), user_package);
  block_s = intern(lit("block"), user_package);
  next_s = intern(lit("next"), user_package);
  freeform_s = intern(lit("freeform"), user_package);
  fail_s = intern(lit("fail"), user_package);
  accept_s = intern(lit("accept"), user_package);
  all_s = intern(lit("all"), user_package);
  some_s = intern(lit("some"), user_package);
  none_s = intern(lit("none"), user_package);
  maybe_s = intern(lit("maybe"), user_package);
  cases_s = intern(lit("cases"), user_package);
  collect_s = intern(lit("collect"), user_package);
  until_s = intern(lit("until"), user_package);
  coll_s = intern(lit("coll"), user_package);
  define_s = intern(lit("define"), user_package);
  output_s = intern(lit("output"), user_package);
  single_s = intern(lit("single"), user_package);
  first_s = intern(lit("first"), user_package);
  last_s = intern(lit("last"), user_package);
  empty_s = intern(lit("empty"), user_package);
  repeat_s = intern(lit("repeat"), user_package);
  rep_s = intern(lit("rep"), user_package);
  flatten_s = intern(lit("flatten"), user_package);
  forget_s = intern(lit("forget"), user_package);
  local_s = intern(lit("local"), user_package);
  merge_s = intern(lit("merge"), user_package);
  bind_s = intern(lit("bind"), user_package);
  rebind_s = intern(lit("rebind"), user_package);
  cat_s = intern(lit("cat"), user_package);
  try_s = intern(lit("try"), user_package);
  catch_s = intern(lit("catch"), user_package);
  finally_s = intern(lit("finally"), user_package);
  throw_s = intern(lit("throw"), user_package);
  defex_s = intern(lit("defex"), user_package);
  deffilter_s = intern(lit("deffilter"), user_package);
  eof_s = intern(lit("eof"), user_package);
  eol_s = intern(lit("eol"), user_package);
  error_s = intern(lit("error"), user_package);
  type_error_s = intern(lit("type_error"), user_package);
  internal_error_s = intern(lit("internal_error"), user_package);
  numeric_error_s = intern(lit("numeric_error"), user_package);
  range_error_s = intern(lit("range_error"), user_package);
  query_error_s = intern(lit("query_error"), user_package);
  file_error_s = intern(lit("file_error"), user_package);
  process_error_s = intern(lit("process_error"), user_package);
  assert_s = intern(lit("assert"), user_package);

  args_k = intern(lit("args"), keyword_package);
  nothrow_k = intern(lit("nothrow"), keyword_package);
  colon_k = intern(lit(""), keyword_package);
  auto_k = intern(lit("auto"), keyword_package);

  equal_f = func_n2(equal);
  eq_f = func_n2(eq);
  eql_f = func_n2(eql);
  gt_f = func_n2(gt);
  lt_f = func_n2(lt);
  identity_f = func_n1(identity);
  car_f = func_n1(car);
  cdr_f = func_n1(cdr);
  null_f = func_n1(null);
  prog_string = string(progname);
}

val obj_print(val obj, val out)
{
  out = default_arg(out, std_output);

  switch (type(obj)) {
  case NIL:
    put_string(lit("nil"), out);
    return obj;
  case CONS:
  case LCONS:
    {
      val sym = car(obj);

      if (sym == quote_s) {
        put_char(chr('\''), out);
        obj_print(second(obj), out);
      } else if (sym == sys_qquote_s) {
        put_char(chr('^'), out);
        obj_print(second(obj), out);
      } else if (sym == sys_unquote_s) {
        put_char(chr(','), out);
        obj_print(second(obj), out);
      } else if (sym == sys_splice_s) {
        put_string(lit(",*"), out);
        obj_print(second(obj), out);
      } else if (sym == vector_lit_s) {
        put_string(lit("#"), out);
        obj_print(second(obj), out);
      } else if (sym == hash_lit_s) {
        put_string(lit("#H"), out);
        obj_print(rest(obj), out);
      } else if (sym == var_s && (symbolp(second(obj)) || integerp(second(obj)))
                 && !cdr(cdr(obj)))
      {
        put_char(chr('@'), out);
        obj_print(second(obj), out);
      } else if (sym == expr_s) {
        put_char(chr('@'), out);
        obj_print(rest(obj), out);
      } else {
        val iter;
        val closepar = chr(')');
        if (sym == dwim_s && consp(cdr(obj))) {
          put_char(chr('['), out);
          obj = cdr(obj);
          closepar = chr(']');
        } else {
          put_char(chr('('), out);
        }

        if (sym == lambda_s && cdr(obj) && symbolp(second(obj))) {
          obj_print(sym, out);
          if (second(obj)) {
            put_string(lit(" (. "), out);
            obj_print(second(obj), out);
            put_char(chr(')'), out);
          } else {
            put_string(lit(" ()"), out);
          }
          iter = (cdr(obj));
          goto finish;
        }

        for (iter = obj; consp(iter); iter = cdr(iter)) {
          obj_print(car(iter), out);
finish:
          if (nilp(cdr(iter))) {
            put_char(closepar, out);
          } else if (consp(cdr(iter))) {
            put_char(chr(' '), out);
          } else {
            put_string(lit(" . "), out);
            obj_print(cdr(iter), out);
            put_char(closepar, out);
          }
        }
      }
    }
    return obj;
  case LIT:
  case STR:
    {
      const wchar_t *ptr;
      int semi_flag = 0;
      put_char(chr('"'), out);

      for (ptr = c_str(obj); *ptr; ptr++) {
        if (semi_flag && iswxdigit(*ptr))
          put_char(chr(';'), out);
        semi_flag = 0;
        switch (*ptr) {
        case '\a': put_string(lit("\\a"), out); break;
        case '\b': put_string(lit("\\b"), out); break;
        case '\t': put_string(lit("\\t"), out); break;
        case '\n': put_string(lit("\\n"), out); break;
        case '\v': put_string(lit("\\v"), out); break;
        case '\f': put_string(lit("\\f"), out); break;
        case '\r': put_string(lit("\\r"), out); break;
        case '"': put_string(lit("\\\""), out); break;
        case '\\': put_string(lit("\\\\"), out); break;
        case 27: put_string(lit("\\e"), out); break;
        default:
          if (*ptr >= ' ') {
            put_char(chr(*ptr), out);
          } else {
            format(out, lit("\\x~,02X"), num(*ptr), nao);
            semi_flag = 1;
          }
        }
      }
      put_char(chr('"'), out);
    }
    return obj;
  case CHR:
    {
      wchar_t ch = c_chr(obj);

      put_string(lit("#\\"), out);
      switch (ch) {
      case '\0': put_string(lit("nul"), out); break;
      case '\a': put_string(lit("alarm"), out); break;
      case '\b': put_string(lit("backspace"), out); break;
      case '\t': put_string(lit("tab"), out); break;
      case '\n': put_string(lit("newline"), out); break;
      case '\v': put_string(lit("vtab"), out); break;
      case '\f': put_string(lit("page"), out); break;
      case '\r': put_string(lit("return"), out); break;
      case 27: put_string(lit("esc"), out); break;
      case ' ': put_string(lit("space"), out); break;
      case 0xDC00: put_string(lit("pnul"), out); break;
      default:
        if ((ch < 0x20) || (ch >= 0x80 && ch < 0xA0))
          format(out, lit("x~,02x"), num(ch), nao);
        else if ((ch >= 0xD800 && ch < 0xE000) || ch == 0xFFFE || ch == 0xFFFF)
          format(out, lit("x~,04x"), num(ch), nao);
        else if (ch >= 0xFFFF)
          format(out, lit("x~,06x"), num(ch), nao);
        else
          put_char(chr(ch), out);
      }
    }
    return obj;
  case NUM:
  case BGNUM:
  case FLNUM:
    format(out, lit("~s"), obj, nao);
    return obj;
  case SYM:
    if (obj->s.package != user_package) {
      if (!obj->s.package)
        put_char(chr('#'), out);
      else if (obj->s.package != keyword_package)
        put_string(obj->s.package->pk.name, out);
      put_char(chr(':'), out);
    }
    put_string(symbol_name(obj), out);
    return obj;
  case PKG:
    format(out, lit("#<package: ~s>"), obj->pk.name, nao);
    return obj;
  case FUN:
    format(out, lit("#<function: type ~a>"), num(obj->f.functype), nao);
    return obj;
  case VEC:
    {
      cnum i, length = c_num(obj->v.vec[vec_length]);
      put_string(lit("#("), out);
      for (i = 0; i < length; i++) {
        obj_print(obj->v.vec[i], out);
        if (i < length - 1)
          put_char(chr(' '), out);
      }
      put_char(chr(')'), out);
    }
    return obj;
  case LSTR:
    if (obj->ls.list)
      format(out, lit("#<lazy-string: ~s (~s ...)>"), obj->ls.prefix,
             obj->ls.list, nao);
    else
      obj_print(obj->ls.prefix, out);
    return obj;
  case COBJ:
    obj->co.ops->print(obj, out);
    return obj;
  case ENV:
    format(out, lit("#<environment: ~p>"), (void *) obj, nao);
    return obj;
  }

  format(out, lit("#<garbage: ~p>"), (void *) obj, nao);
  return obj;
}

val obj_pprint(val obj, val out)
{
  out = default_arg(out, std_output);

  switch (type(obj)) {
  case NIL:
    put_string(lit("nil"), out);
    return obj;
  case CONS:
  case LCONS:
    {
      val sym = car(obj);

      if (sym == quote_s) {
        put_char(chr('\''), out);
        obj_print(second(obj), out);
      } else if (sym == sys_qquote_s) {
        put_char(chr('^'), out);
        obj_print(second(obj), out);
      } else if (sym == sys_unquote_s) {
        put_char(chr(','), out);
        obj_pprint(second(obj), out);
      } else if (sym == sys_splice_s) {
        put_string(lit(",*"), out);
        obj_pprint(second(obj), out);
      } else if (sym == vector_lit_s) {
        put_string(lit("#"), out);
        obj_print(second(obj), out);
      } else if (sym == hash_lit_s) {
        put_string(lit("#H"), out);
        obj_print(rest(obj), out);
      } else if (sym == var_s && (symbolp(second(obj)) || integerp(second(obj)))
                 && !cdr(cdr(obj)))
      {
        put_char(chr('@'), out);
        obj_print(second(obj), out);
      } else if (sym == expr_s) {
        put_char(chr('@'), out);
        obj_print(rest(obj), out);
      } else {
        val iter;
        val closepar = chr(')');
        if (sym == dwim_s && consp(cdr(obj))) {
          put_char(chr('['), out);
          obj = cdr(obj);
          closepar = chr(']');
        } else {
          put_char(chr('('), out);
        }

        if (sym == lambda_s && cdr(obj) && symbolp(second(obj))) {
          obj_print(sym, out);
          if (second(obj)) {
            put_string(lit(" (. "), out);
            obj_pprint(second(obj), out);
            put_char(chr(')'), out);
          } else {
            put_string(lit(" ()"), out);
          }
          iter = (cdr(obj));
          goto finish;
        }

        for (iter = obj; consp(iter); iter = cdr(iter)) {
          obj_pprint(car(iter), out);
finish:
          if (nilp(cdr(iter))) {
            put_char(closepar, out);
          } else if (consp(cdr(iter))) {
            put_char(chr(' '), out);
          } else {
            put_string(lit(" . "), out);
            obj_pprint(cdr(iter), out);
            put_char(closepar, out);
          }
        }
      }
    }
    return obj;
  case LIT:
  case STR:
    put_string(obj, out);
    return obj;
  case CHR:
    put_char(obj, out);
    return obj;
  case NUM:
  case BGNUM:
  case FLNUM:
    format(out, lit("~s"), obj, nao);
    return obj;
  case SYM:
    put_string(symbol_name(obj), out);
    return obj;
  case PKG:
    format(out, lit("#<package: ~s>"), obj->pk.name, nao);
    return obj;
  case FUN:
    format(out, lit("#<function: type ~a>"), num(obj->f.functype), nao);
    return obj;
  case VEC:
    {
      cnum i, length = c_num(obj->v.vec[vec_length]);
      put_string(lit("#("), out);
      for (i = 0; i < length; i++) {
        obj_pprint(obj->v.vec[i], out);
        if (i < length - 1)
          put_char(chr(' '), out);
      }
      put_char(chr(')'), out);
    }
    return obj;
  case LSTR:
    if (obj->ls.list)
      format(out, lit("#<lazy-string: ~s (~s ...)>"), obj->ls.prefix,
             obj->ls.list, nao);
    else
      obj_pprint(obj->ls.prefix, out);
    return obj;
  case COBJ:
    obj->co.ops->print(obj, out);
    return obj;
  case ENV:
    format(out, lit("#<environment: ~p>"), (void *) obj, nao);
    return obj;
  }

  format(out, lit("#<garbage: ~p>"), (void *) obj, nao);
  return obj;
}

val tostring(val obj)
{
  val ss = make_string_output_stream();
  obj_print(obj, ss);
  return get_string_from_stream(ss);
}

val tostringp(val obj)
{
  val ss = make_string_output_stream();
  obj_pprint(obj, ss);
  return get_string_from_stream(ss);
}

val time_sec(void)
{
  struct timeval tv;
  if (gettimeofday(&tv, 0) == -1)
    return nil;
  return num(tv.tv_sec);
}

val time_sec_usec(void)
{
  struct timeval tv;
  if (gettimeofday(&tv, 0) == -1)
    return nil;
  return cons(num(tv.tv_sec), num(tv.tv_usec));
}

#if !HAVE_GMTIME_R
/*
 * Ugly hacks for MingW, which uses the Microsft C Run Time Library, 
 * whic in turn is stuck in the Dark Ages * without _r functions.
 */
struct tm *gmtime_r(const time_t *timep, struct tm *result);
struct tm *localtime_r(const time_t *timep, struct tm *result);

struct tm *gmtime_r(const time_t *timep, struct tm *result)
{
  struct tm *hack = gmtime(timep);
  *result = *hack;
  return hack;
}

struct tm *localtime_r(const time_t *timep, struct tm *result)
{
  struct tm *hack = localtime(timep);
  *result = *hack;
  return hack;
}
#endif

static val string_time(struct tm *(*break_time_fn)(const time_t *, struct tm *),
                       char *format, time_t time)
{
  char buffer[512] = "";
  struct tm broken_out_time;

  if (break_time_fn(&time, &broken_out_time) == 0)
    return nil;

#if HAVE_TM_ZONE
  if (strcmp(broken_out_time.TM_ZONE, "GMT") == 0)
    broken_out_time.TM_ZONE = "UTC";
#endif

  if (strftime(buffer, sizeof buffer, format, &broken_out_time) == 0)
    buffer[0] = 0;

  {
    wchar_t *wctime = utf8_dup_from(buffer);
    return string_own(wctime);
  }
}

val time_string_local(val time, val format)
{
  time_t secs = c_num(time);
  const wchar_t *wcfmt = c_str(format);
  char *u8fmt = utf8_dup_to(wcfmt);
  val timestr = string_time(localtime_r, u8fmt, secs);
  free(u8fmt);
  return timestr;
}

val time_string_utc(val time, val format)
{
  time_t secs = c_num(time);
  const wchar_t *wcfmt = c_str(format);
  char *u8fmt = utf8_dup_to(wcfmt);
  val timestr = string_time(gmtime_r, u8fmt, secs);
  free(u8fmt);
  return timestr;
}

static val broken_time_list(struct tm *tms)
{
  return list(num(tms->tm_year + 1900),
              num_fast(tms->tm_mon + 1),
              num_fast(tms->tm_mday),
              num_fast(tms->tm_hour),
              num_fast(tms->tm_min),
              num_fast(tms->tm_sec),
              tms->tm_isdst ? t : nil,
              nao);
}

val time_fields_local(val time)
{
  struct tm tms;
  time_t secs = c_num(time);

  if (localtime_r(&secs, &tms) == 0)
    return nil;

  return broken_time_list(&tms);
}

val time_fields_utc(val time)
{
  struct tm tms;
  time_t secs = c_num(time);

  if (gmtime_r(&secs, &tms) == 0)
    return nil;

  return broken_time_list(&tms);
}

static val make_time_impl(time_t (*pmktime)(struct tm *),
                          val year, val month, val day,
                          val hour, val minute, val second,
                          val isdst)
{
  struct tm local = { 0 };
  time_t time;

  local.tm_year = c_num(year) - 1900;
  local.tm_mon = c_num(month) - 1;
  local.tm_mday = c_num(day);
  local.tm_hour = c_num(hour);
  local.tm_min = c_num(minute);
  local.tm_sec = c_num(second);

  if (!isdst)
    local.tm_isdst = 0;
  else if (isdst == auto_k)
    local.tm_isdst = -1;
  else
    local.tm_isdst = 1;

  time = pmktime(&local);

  return time == -1 ? nil : num(time);
}

val make_time(val year, val month, val day,
              val hour, val minute, val second,
              val isdst)
{
  return make_time_impl(mktime, year, month, day, hour, minute, second, isdst);
}

#if !HAVE_TIMEGM

#if !HAVE_SETENV
static void
setenv(const char *name, const char *value, int overwrite)
{
  int len = strlen(name)+1+strlen(value)+1;
  char *str = (char *) chk_malloc(len);
  (void) overwrite;
  sprintf(str, "%s=%s", name, value);
  putenv(str);
} 

static void
unsetenv(const char *name)
{
  setenv(name, "", 1);
}
#endif

static time_t timegm_hack(struct tm *tm)
{
    time_t ret;
    char *tz;

    tz = getenv("TZ");
    setenv("TZ", "UTC", 1);
#if HAVE_TZSET
    tzset();
#endif
    ret = mktime(tm);
    if (tz)
        setenv("TZ", tz, 1);
    else
        unsetenv("TZ");
#if HAVE_TZSET
    tzset();
#endif

    env_list = nil;
    return ret;
}
#endif

val make_time_utc(val year, val month, val day,
                  val hour, val minute, val second,
                  val isdst)
{
#if HAVE_TIMEGM
  time_t (*pmktime)(struct tm *) = timegm;
#else
  time_t (*pmktime)(struct tm *) = timegm_hack;
#endif

  return make_time_impl(pmktime, year, month, day, hour, minute, second, isdst);
}

void init(const wchar_t *pn, mem_t *(*oom)(mem_t *, size_t),
          val *stack_bottom)
{
  int gc_save;
  progname = pn;
  gc_save = gc_state(0);

  oom_realloc = oom;
  gc_init(stack_bottom);
  obj_init();
  arith_init();
  uw_init();
  eval_init();
  rand_init();
  stream_init();
#if HAVE_POSIX_SIGS
  sig_init();
#endif
  filter_init();
  hash_init();
  regex_init();

  gc_state(gc_save);
}

void dump(val obj, val out)
{
  obj_print(obj, out);
  put_char(chr('\n'), out);
}

/*
 * Handy function for debugging in gdb,
 * so we don't have to keep typing:
 * (gdb) p dump(something, stdout)
 */
void d(val obj)
{
  int save = gc_state(0);
  dump(obj, std_output);
  gc_state(save);
}

/*
 * Function for software breakpoints.
 * Debugging routines call here when a breakpoint
 * is requested. For this to work, set a breakpoint
 * on this function.
 */
void breakpt(void)
{
}
