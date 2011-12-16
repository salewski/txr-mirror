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
#include <string.h>
#include <wctype.h>
#include <assert.h>
#include <limits.h>
#include <stdarg.h>
#include <dirent.h>
#include <setjmp.h>
#include <errno.h>
#include <wchar.h>
#include "config.h"
#ifdef HAVE_GETENVIRONMENTSTRINGS
#define NOMINMAX
#include <windows.h>
#endif
#include "lib.h"
#include "gc.h"
#include "arith.h"
#include "hash.h"
#include "unwind.h"
#include "stream.h"
#include "utf8.h"
#include "filter.h"
#include "eval.h"

#define max(a, b) ((a) > (b) ? (a) : (b))
#define min(a, b) ((a) < (b) ? (a) : (b))

val packages;

val system_package, keyword_package, user_package;

val null, t, cons_s, str_s, chr_s, fixnum_s, sym_s, pkg_s, fun_s, vec_s;
val stream_s, hash_s, hash_iter_s, lcons_s, lstr_s, cobj_s, cptr_s;
val env_s, bignum_s;
val var_s, expr_s, regex_s, chset_s, set_s, cset_s, wild_s, oneplus_s;
val nongreedy_s, compiled_regex_s;
val quote_s, qquote_s, unquote_s, splice_s;
val zeroplus_s, optional_s, compl_s, compound_s, or_s, and_s, quasi_s;
val skip_s, trailer_s, block_s, next_s, freeform_s, fail_s, accept_s;
val all_s, some_s, none_s, maybe_s, cases_s, collect_s, until_s, coll_s;
val define_s, output_s, single_s, first_s, last_s, empty_s;
val repeat_s, rep_s, flatten_s, forget_s;
val local_s, merge_s, bind_s, cat_s;
val try_s, catch_s, finally_s, throw_s, defex_s, deffilter_s;
val eof_s, eol_s;
val error_s, type_error_s, internal_error_s;
val numeric_error_s, range_error_s;
val query_error_s, file_error_s, process_error_s;

val nothrow_k, args_k;

val null_string;
val nil_string;
val null_list;

val identity_f, equal_f, eql_f, eq_f, car_f, cdr_f;

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
  case CONS: return cons_s;
  case STR: return str_s;
  case LIT: return str_s;
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
    return str_s;
  case TAG_PTR:
    if (obj == nil) {
      return null;
    } else if (obj->t.type == COBJ) {
      return obj->co.cls;
    } else {
      val type = code2type(obj->t.type);
      if (!type)
        internal_error("corrupt type field");
      return type;
    }
  default:
    internal_error("invalid type tag");
  }
}

val type_check(val obj, int type)
{
  if (!is_ptr(obj) || obj->t.type != type)
    type_mismatch(lit("~s is not of type ~s"), obj, code2type(type), nao);
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
  type_check (cobj, COBJ);
  type_assert (cobj->co.cls == class_sym, (lit("~a is not a cobj of class ~a"),
                                           cobj, class_sym));
  return t;
}

val car(val cons)
{
  if (cons == nil)
    return nil;
  else switch (type(cons)) {
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
  default:
    type_mismatch(lit("~s is not a cons"), cons, nao);
  }
}

val cdr(val cons)
{
  if (cons == nil)
    return nil;
  else switch (type(cons)) {
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
  default:
    type_mismatch(lit("~s is not a cons"), cons, nao);
  }
}

val rplaca(val cons, val new_car)
{
  switch (type(cons)) {
  case CONS:
    return cons->c.car = new_car;
  case LCONS:
    return cons->lc.car = new_car;
  default:
    type_mismatch(lit("~s is not a cons"), cons, nao);
  }
}


val rplacd(val cons, val new_cdr)
{
  switch (type(cons)) {
  case CONS:
    return cons->c.cdr = new_cdr;
  case LCONS:
    return cons->lc.cdr = new_cdr;
  default:
    type_mismatch(lit("~s is not a cons"), cons, nao);
  }
}

val *car_l(val cons)
{
  switch (type(cons)) {
  case CONS:
    return &cons->c.car;
  case LCONS:
    funcall1(cons->lc.func, cons);
    return &cons->lc.car;
  default:
    type_mismatch(lit("~s is not a cons"), cons, nao);
  }
}

val *cdr_l(val cons)
{
  switch (type(cons)) {
  case CONS:
    return &cons->c.cdr;
  case LCONS:
    funcall1(cons->lc.func, cons);
    return &cons->lc.cdr;
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

val *tail(val cons)
{
  while (cdr(cons))
    cons = cdr(cons);
  return cdr_l(cons);
}

val pop(val *plist)
{
  val ret = car(*plist);
  *plist = cdr(*plist);
  return ret;
}

val push(val value, val *plist)
{
  return *plist = cons(value, *plist);
}

val copy_list(val list)
{
  list_collect_decl (out, ptail);

  while (consp(list)) {
    list_collect(ptail, car(list));
    list = cdr(list);
  }

  list_collect_append(ptail, list);

  return out;
}

val nreverse(val in)
{
  val rev = nil;

  while (in) {
    val temp = cdr(in);
    *cdr_l(in) = rev;
    rev = in;
    in = temp;
  }

  return rev;
}

val reverse(val in)
{
  val rev = nil;

  while (in) {
    rev = cons(car(in), rev);
    in = cdr(in);
  }

  return rev;
}

val append2(val list1, val list2)
{
  list_collect_decl (out, ptail);

  list_collect_append (ptail, list1);
  list_collect_append (ptail, list2);

  return out;
}

val appendv(val lists)
{
  list_collect_decl (out, ptail);

  for (; lists; lists = cdr(lists)) {
    val item = car(lists);
    if (!listp(*ptail))
      uw_throwf(error_s, lit("append: ~s is not a list"), *ptail, nao);
    list_collect_append(ptail, item);
  }

  return out;
}

val nappend2(val list1, val list2)
{
  list_collect_decl (out, ptail);

  list_collect_nconc (ptail, list1);
  list_collect_nconc (ptail, list2);

  return out;
}

val ldiff(val list1, val list2)
{
  list_collect_decl (out, tail);

  while (list1 && list1 != list2) {
    list_collect (tail, car(list1));
    list1 = cdr(list1);
  }

  return out;
}

val memq(val obj, val list)
{
  while (list && car(list) != obj)
    list = cdr(list);
  return list;
}

val memqual(val obj, val list)
{
  while (list && !equal(car(list), obj))
    list = cdr(list);
  return list;
}

val tree_find(val obj, val tree, val testfun)
{
  if (funcall2(testfun, obj, tree))
    return t;
  else if (consp(tree))
    return some_satisfy(tree, curry_123_2(func_n3(tree_find), 
                                          obj, testfun), nil);
  return nil;
}

val some_satisfy(val list, val pred, val key)
{
  if (!key)
    key = identity_f;

  for (; list; list = cdr(list)) {
    if (funcall1(pred, funcall1(key, car(list))))
      return t;
  }

  return nil;
}

val all_satisfy(val list, val pred, val key)
{
  if (!key)
    key = identity_f;

  for (; list; list = cdr(list)) {
    if (!funcall1(pred, funcall1(key, car(list))))
      return nil;
  }

  return t;
}

val none_satisfy(val list, val pred, val key)
{
  if (!key)
    key = identity_f;

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

cnum c_num(val num);

val eql(val left, val right)
{
  /* eql is same as eq for now, but when we get bignums,
     eql will compare different bignum objects which are
     the same number as equal. */
  if (is_ptr(left) && type(left) == BGNUM)
    return equal(left, right);
  return eq(left, right);
}

val equal(val left, val right)
{
  /* Bitwise equality is equality, period. */
  if (left == right)
    return t;

  /* Objects are not bitwise equal. If either
     is nil, then they are not equal,
     since nil uses bitwise equality. */
  if (left == nil || right == nil)
    return nil;

  switch (type(left)) {
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
  case COBJ:
    if (type(right) == COBJ)
      return left->co.ops->equal(left, right);
    return nil;
  }

  internal_error("unhandled case in equal function");
}

val cobj_equal_op(val left, val right)
{
  return eq(left, right);
}

mem_t *chk_malloc(size_t size)
{
  mem_t *ptr = (mem_t *) malloc(size);
  if (size && ptr == 0)
    ptr = (mem_t *) oom_realloc(0, size);
  return ptr;
}

mem_t *chk_calloc(size_t n, size_t size)
{
  mem_t *ptr = (mem_t *) calloc(n, size);
  if (size && ptr == 0) {
    ptr = (mem_t *) oom_realloc(0, size);
    memset(ptr, 0, n * size);
  }
  return ptr;
}

mem_t *chk_realloc(mem_t *old, size_t size)
{
  mem_t *newptr = (mem_t *) realloc(old, size);
  if (size != 0 && newptr == 0)
    newptr = oom_realloc(old, size);
  return newptr;
}

wchar_t *chk_strdup(const wchar_t *str)
{
  size_t nchar = wcslen(str) + 1;
  wchar_t *copy = (wchar_t *) chk_malloc(nchar * sizeof *copy);
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
  if (!obj) {
    return nil;
  } else {
    type_t ty = type(obj);
    return (ty == CONS || ty == LCONS) ? t : nil;
  }
}

val nullp(val obj)
{
  return obj == 0 ? t : nil;
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
    if (eq(ind, key))
      return second(list);
  }

  return nil;
}

val getplist_f(val list, val key, val *found)
{
  for (; list; list = cdr(cdr(list))) {
    val ind = first(list);
    if (eq(ind, key)) {
      *found = t;
      return second(list);
    }
  }

  *found = nil;
  return nil;
}

val proper_plist_to_alist(val list)
{
  list_collect_decl (out, tail);

  for (; list; list = cdr(cdr(list))) {
    val ind = first(list);
    val prop = second(list);
    list_collect (tail, cons(ind, prop));
  }

  return out;
}

val improper_plist_to_alist(val list, val boolean_keys)
{
  list_collect_decl (out, tail);

  for (; list; list = cdr(list)) {
    val ind = first(list);

    if (memqual(ind, boolean_keys)) {
      list_collect (tail, cons(ind, t));
    } else {
      val prop = second(list);
      list_collect (tail, cons(ind, prop));
      list = cdr(list);
    }
  }

  return out;
}

val num(cnum n)
{
  numeric_assert (n >= NUM_MIN && n <= NUM_MAX);
  return (val) ((n << TAG_SHIFT) | TAG_NUM);
}

cnum c_num(val num)
{
  switch (tag(num)) {
  case TAG_CHR: case TAG_NUM:
    return ((cnum) num) >> TAG_SHIFT;
  default:
    type_mismatch(lit("~s is not a fixnum"), num, nao);
  }
}

val fixnump(val num)
{
  return (is_num(num)) ? t : nil;
}

val bignump(val num)
{
  return (is_ptr(num) && type(num) == BGNUM) ? t : nil;
}

val numberp(val num)
{
  switch (tag(num)) {
  case TAG_NUM:
    return t;
  case TAG_PTR:
    if (num->t.type == BGNUM)
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

val max2(val anum, val bnum)
{
  return if3(gt(anum, bnum), anum, bnum);
}

val min2(val anum, val bnum)
{
  return if3(lt(anum, bnum), anum, bnum);
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
    else
      uw_throwf(error_s, lit("string_extend: tail ~s bad type"), str, nao);

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
      uw_throwf(error_s, lit("string_extend: overflow"), nao);

    str->st.str = (wchar_t *) chk_realloc((mem_t *) str->st.str,
                                          alloc * sizeof *str->st.str);
    str->st.alloc = num(alloc);
    str->st.len = plus(str->st.len, needed);

    if (stringp(tail)) {
      wmemcpy(str->st.str + len, c_str(tail), c_num(needed) + 1);
    } else {
      str->st.str[len] = c_chr(tail);
      str->st.str[len + 1] = 0;
    }
  }

  return str;
}

val stringp(val str)
{
  switch (tag(str)) {
  case TAG_LIT:
    return t;
  case TAG_PTR:
    if (str == nil)
      return nil;
    switch (type(str)) {
    case STR: case LSTR:
      return t;
    default:
      break;
    }
  }
  return nil;
}

val lazy_stringp(val str)
{
  return (is_ptr(str) && (type(str) == LSTR)) ? t : nil;
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
      str->st.len = num(wcslen(str->st.str));
      str->st.alloc = plus(str->st.len, one);
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
        if (!it || lt(pos, minpos) || (eq(pos, minpos) && gt(len, maxlen))) {
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

val sub_str(val str_in, val from, val to)
{
  if (from == nil || lt(from, zero))
    from = zero;
  if (to == nil)
    to = length_str(str_in);
  else if (lt(to, zero))
    to = zero;
  if (length_str_lt(str_in, from))
    from = length_str(str_in);
  if (length_str_lt(str_in, to))
    to = length_str(str_in);

  if (ge(from, to)) {
    return null_string;
  } else {
    size_t nchar = c_num(to) - c_num(from) + 1;
    wchar_t *sub = (wchar_t *) chk_malloc(nchar * sizeof (wchar_t));
    const wchar_t *str = c_str(lazy_stringp(str_in)
                               ? str_in->ls.prefix : str_in);
    wcsncpy(sub, str + c_num(from), nchar);
    sub[nchar-1] = 0;
    return string_own(sub);
  }
}

val cat_str(val list, val sep)
{
  cnum total = 0;
  val iter;
  wchar_t *str, *ptr;
  cnum len_sep = sep ? c_num(length_str(sep)) : 0;

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
    return nil;
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
  const wchar_t *cstr = c_str(str);
  const wchar_t *csep = c_str(sep);
  size_t len_sep = c_num(length_str(sep));

  prot1(&str);
  prot1(&sep);

  list_collect_decl (out, iter);

  for (;;) {
    const wchar_t *psep = wcsstr(cstr, csep);
    size_t span = (psep != 0) ? psep - cstr : wcslen(cstr);
    val piece = mkustring(num(span));
    init_str(piece, cstr);
    list_collect (iter, piece);
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
    list_collect (iter, piece);
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

val string_lt(val astr, val bstr)
{
  int cmp = wcscmp(c_str(astr), c_str(bstr));
  return cmp == -1 ? t : nil;
}

val int_str(val str, val base)
{
  const wchar_t *wcs = c_str(str);
  wchar_t *ptr;
  cnum b = c_num(base);

  /* TODO: detect if we have wcstoll */
  long value = wcstol(wcs, &ptr, b ? b : 10);
  if (value == 0 && ptr == wcs)
    return nil;
  if (((value == LONG_MAX || value == LONG_MIN) && errno == ERANGE) ||
      (value < NUM_MIN || value > NUM_MAX))
  {
    val bignum = make_bignum();
    unsigned char *ucs = utf8_dup_to_uc(wcs);
    mp_err err = mp_read_radix(mp(bignum), ucs, b);

    free(ucs); /* TODO: make wchar_t version of mp_read_radix. */

    if (err != MP_OKAY)
      return nil;

    return bignum;
  }

  return num(value);
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

val chr_str(val str, val index)
{
  bug_unless (length_str_gt(str, index));

  if (lazy_stringp(str)) {
    lazy_str_force_upto(str, index);
    return chr(c_str(str->ls.prefix)[c_num(index)]);
  } else {
    return chr(c_str(str)[c_num(index)]);
  }
}

val chr_str_set(val str, val index, val chr)
{
  bug_unless (length_str_gt(str, index));

  if (lazy_stringp(str)) {
    lazy_str_force_upto(str, index);
    str->ls.prefix->st.str[c_num(index)] = c_chr(chr);
  } else {
    str->st.str[c_num(index)] = c_chr(chr);
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

val make_package(val name)
{
  if (find_package(name)) {
    uw_throwf(error_s, lit("make_package: ~a exists already"), name, nao);
  } else {
    val obj = make_obj();
    obj->pk.type = PKG;
    obj->pk.name = name;
    obj->pk.symhash = make_hash(nil, nil, lit("t")); /* don't have t yet! */

    push(cons(name, obj), &packages);
    return obj;
  }
}

val find_package(val name)
{
  return cdr(assoc(name, packages));
}

val intern(val str, val package)
{
  val new_p;
  val *place;

  if (nullp(package)) {
    package = user_package;
  } else if (stringp(package)) {
    package = find_package(str);
    if (!package)
      uw_throwf(error_s, lit("make_package: ~a exists already"), str, nao);
  }

  type_check (package, PKG);

  place = gethash_l(package->pk.symhash, str, &new_p);

  if (!new_p) {
    return *place;
  } else {
    val newsym = make_sym(str);
    newsym->s.package = package;
    return *place = newsym;
  }
}

static val rehome_sym(val sym, val package)
{
  if (!sym)
    return nil;
  type_check (package, PKG);
  type_check (sym, SYM);

  if (sym->s.package)
    remhash(sym->s.package->pk.symhash, symbol_name(sym));
  sym->s.package = package;
  sethash(package->pk.symhash, symbol_name(sym), sym);
  return sym;
}

val symbolp(val sym)
{
  return (sym == nil || (is_ptr(sym) && sym->s.type == SYM)) ? t : nil;
}

val keywordp(val sym)
{
  return (symbolp(sym) && symbol_package(sym) == keyword_package) ? t : nil;
}

val func_f0(val env, val (*fun)(val))
{
  val obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = F0;
  obj->f.env = env;
  obj->f.f.f0 = fun;
  obj->f.variadic = 0;
  obj->f.minparam = 0;
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
  obj->f.minparam = 1;
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
  obj->f.minparam = 2;
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
  obj->f.minparam = 3;
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
  obj->f.minparam = 4;
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
  obj->f.minparam = 0;
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
  obj->f.minparam = 1;
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
  obj->f.minparam = 2;
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
  obj->f.minparam = 3;
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
  obj->f.minparam = 4;
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
  obj->f.minparam = 0;
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
  obj->f.minparam = 1;
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
  obj->f.minparam = 2;
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
  obj->f.minparam = 3;
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
  obj->f.minparam = 4;
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
  obj->f.minparam = 0;
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
  obj->f.minparam = 1;
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
  obj->f.minparam = 2;
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
  obj->f.minparam = 3;
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
  obj->f.minparam = 4;
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
  obj->f.minparam = 0;
  return obj;
}

val functionp(val obj)
{
  if (!obj) {
    return nil;
  } else {
    type_t ty = type(obj);
    return (ty == FUN) ? t : nil;
  }
}

val funcall(val fun)
{
  type_check(fun, FUN);

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
  uw_throwf(error_s, lit("funcall: wrong number of arguments"));
}

val funcall1(val fun, val arg)
{
  type_check(fun, FUN);

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
  uw_throw(error_s, lit("funcall1: wrong number of arguments"));
}

val funcall2(val fun, val arg1, val arg2)
{
  type_check(fun, FUN);

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
  uw_throw(error_s, lit("funcall2: wrong number of arguments"));
}

val funcall3(val fun, val arg1, val arg2, val arg3)
{
  type_check(fun, FUN);

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
  uw_throw(error_s, lit("funcall3: wrong number of arguments"));
}

val funcall4(val fun, val arg1, val arg2, val arg3, val arg4)
{
  type_check(fun, FUN);

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
  uw_throw(error_s, lit("funcall4: wrong number of arguments"));
}


val reduce_left(val fun, val list, val init, val key)
{
  if (!key)
    key = identity_f;

  for (; list; list = cdr(list))
    init = funcall2(fun, init, funcall1(key, car(list)));

  return init;
}

val reduce_right(val fun, val list, val init, val key)
{
  if (!key)
    key = identity_f;

  if (nullp(list))
    return init;
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

static val do_chain(val fun1_list, val arg)
{
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
    list_collect (iter, first_fun);

    while ((next_fun = va_arg(vl, val)) != nao)
      list_collect (iter, next_fun);

    va_end (vl);
  }

  return func_f1(out, do_chain);
}

static val do_and(val fun1_list, val arg)
{
  for (; fun1_list; fun1_list = cdr(fun1_list))
    if (nullp(funcall1(car(fun1_list), arg)))
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
    list_collect (iter, first_fun);

    while ((next_fun = va_arg(vl, val)) != nao)
      list_collect (iter, next_fun);

    va_end (vl);
  }

  return func_f1(out, do_and);
}

static val do_swap_12_21(val fun, val left, val right)
{
  return funcall2(fun, right, left);
}

val swap_12_21(val fun)
{
  return func_f2(fun, do_swap_12_21);
}

static val do_or(val fun1_list, val arg)
{
  for (; fun1_list; fun1_list = cdr(fun1_list))
    if (funcall1(car(fun1_list), arg))
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
    list_collect (iter, first_fun);

    while ((next_fun = va_arg(vl, val)) != nao)
      list_collect (iter, next_fun);

    va_end (vl);
  }

  return func_f1(out, do_or);
}

val vector(val length)
{
  int i;
  cnum alloc_plus = c_num(length) + 2;
  val vec = make_obj();
  val *v = (val *) chk_malloc(alloc_plus * sizeof *v);
#ifdef HAVE_VALGRIND
  vec->v.vec_true_start = v;
#endif
  v += 2;
  vec->v.type = VEC;
  vec->v.vec = v;
  v[vec_alloc] = length;
  v[vec_length] = length;
  for (i = 0; i < alloc_plus - 2; i++)
    vec->v.vec[i] = nil;
  return vec;
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

    if (alloc_delta > 0) {
      cnum new_alloc = max(new_length, 2*old_alloc);
      val *newvec = (val *) chk_realloc((mem_t *) (vec->v.vec - 2),
                                        (new_alloc + 2) * sizeof *newvec);
      vec->v.vec = newvec + 2;
      vec->v.vec[vec_alloc] = num(new_alloc);
#ifdef HAVE_VALGRIND
      vec->v.vec_true_start = newvec;
#endif
    }

    if (length_delta > 0) {
      cnum i;
      for (i = old_length; i < new_length; i++)
        vec->v.vec[i] = nil;
    }

    vec->v.vec[vec_length] = length;
  }

  return vec;
}

val vecref(val vec, val ind)
{
  type_check(vec, VEC);
  range_bug_unless (c_num(ind) < c_num(vec->v.vec[vec_length]));
  return vec->v.vec[c_num(ind)];
}

val *vecref_l(val vec, val ind)
{
  type_check(vec, VEC);
  range_bug_unless (c_num(ind) < c_num(vec->v.vec[vec_length]));
  return vec->v.vec + c_num(ind);
}

val vec_push(val vec, val item)
{
  val length = length_vec(vec);
  vec_set_length(vec, plus(length, one));
  *vecref_l(vec, length) = item;
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
  val vec = vector(zero);
 
  if (!listp(list))
    uw_throwf(error_s, lit("vector_list: list expected, not ~s"), list, nao);

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
    list_collect(ptail, vec->v.vec[i]);

  return list;
}

static val lazy_stream_func(val env, val lcons)
{
  val stream = car(env);
  val next = cdr(env) ? pop(cdr_l(env)) : get_line(stream);
  val ahead = get_line(stream);

  lcons->lc.car = next;
  lcons->lc.cdr = if2(ahead, make_lazy_cons(lcons->lc.func));
  lcons->lc.func = nil;

  if (!next || !ahead)
    close_stream(stream, t);

  if (ahead)
    push(ahead, cdr_l(env));

  return next;
}

val lazy_stream_cons(val stream)
{
  val first = get_line(stream);

  if (!first) {
    close_stream(stream, t);
    return nil;
  }

  return make_lazy_cons(func_f1(cons(stream, cons(first, nil)),
                                lazy_stream_func));
}

val lazy_str(val lst, val term, val limit)
{
  uses_or2;
  val obj = make_obj();
  obj->ls.type = LSTR;

  /* Must init before calling something that can gc! */
  obj->ls.opts = obj->ls.list = obj->ls.prefix = nil;

  term = or2(term, string(L"\n"));

  if (nullp(lst)) {
    obj->ls.prefix = null_string;
    obj->ls.list = nil;
  } else {
    obj->ls.prefix = cat_str(list(first(lst), term, nao), nil);
    obj->ls.list = rest(lst);
    limit = if2(limit, minus(limit, one));
  }

  obj->ls.opts = cons(term, limit);

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
    lstr->ls.prefix = cat_str(list(lstr->ls.prefix, next, term, nao), nil);
    if (lim)
      lim = minus(lim, one);
  }

  if (lim)
    *cdr_l(lstr->ls.opts) = lim;

  return lstr->ls.prefix;
}

val lazy_str_force_upto(val lstr, val index)
{
  uses_or2;
  val lim;
  type_check(lstr, LSTR);
  lim = cdr(lstr->ls.opts);

  while (ge(index, length_str(lstr->ls.prefix)) && lstr->ls.list &&
         or2(nullp(lim),gt(lim,zero)))
  {
    val next = pop(&lstr->ls.list);
    val term = car(lstr->ls.opts);
    lstr->ls.prefix = cat_str(list(lstr->ls.prefix, next, term, nao), nil);
    if (lim)
      lim = minus(lim, one);
  }

  if (lim)
    *cdr_l(lstr->ls.opts) = lim;
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
                                    or2(car(lstr->ls.opts), string(L"\n")));

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
  if (!obj) {
    return nil;
  } else {
    type_t ty = type(obj);
    return (ty == COBJ) ? t : nil;
  }
}

mem_t *cobj_handle(val cobj, val cls_sym)
{
  class_check(cobj, cls_sym);
  return cobj->co.handle;
}

void cobj_print_op(val obj, val out)
{
  put_string(out, lit("#<"));
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
  while (list) {
    val elem = car(list);
    if (equal(car(elem), key))
      return elem;
    list = cdr(list);
  }

  return nil;
}

val assq(val key, val list)
{
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
    *cdr_l(existing) = value;
    return list;
  } else {
    return cons(cons(key, value), list);
  }
}

val *acons_new_l(val key, val *new_p, val *list)
{
  val existing = assoc(key, *list);

  if (existing) {
    if (new_p)
      *new_p = nil;
    return cdr_l(existing);
  } else {
    val nc = cons(key, nil);
    *list = cons(nc, *list);
    if (new_p)
      *new_p = t;
    return cdr_l(nc);
  }
}

val aconsq_new(val key, val value, val list)
{
  val existing = assq(key, list);

  if (existing) {
    *cdr_l(existing) = value;
    return list;
  } else {
    return cons(cons(key, value), list);
  }
}

val *aconsq_new_l(val key, val *new_p, val *list)
{
  val existing = assq(key, *list);

  if (existing) {
    if (new_p)
      *new_p = nil;
    return cdr_l(existing);
  } else {
    val nc = cons(key, nil);
    *list = cons(nc, *list);
    if (new_p)
      *new_p = t;
    return cdr_l(nc);
  }
}


static val alist_remove_test(val item, val key)
{
  return eq(car(item), key);
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
  val *plist = &list;

  while (*plist) {
    if (memq(car(car(*plist)), keys))
      *plist = cdr(*plist);
    else
      plist = cdr_l(*plist);
  }

  return list;
}

val alist_nremove1(val list, val key)
{
  val *plist = &list;

  while (*plist) {
    if (eq(car(car(*plist)), key))
      *plist = cdr(*plist);
    else
      plist = cdr_l(*plist);
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

  for (; list; list = cdr(list))
    list_collect (iter, funcall1(fun, car(list)));

  return out;
}

val mapcon(val fun, val list)
{
  list_collect_decl (out, iter);

  for (; list; list = cdr(list))
    list_collect_nconc (iter, funcall1(fun, list));

  return out;
}

val mappend(val fun, val list)
{
  list_collect_decl (out, iter);

  for (; list; list = cdr(list))
    list_collect_append (iter, funcall1(fun, car(list)));

  return out;
}

val merge(val list1, val list2, val lessfun, val keyfun)
{
  list_collect_decl (out, ptail);

  while (list1 && list2) {
    val el1 = funcall1(keyfun, first(list1));
    val el2 = funcall1(keyfun, first(list2));

    if (funcall2(lessfun, el1, el2)) {
      val next = cdr(list1);
      *cdr_l(list1) = nil;
      list_collect_append(ptail, list1);
      list1 = next;
    } else {
      val next = cdr(list2);
      *cdr_l(list2) = nil;
      list_collect_append(ptail, list2);
      list2 = next;
    }
  }

  if (list1)
    list_collect_append(ptail, list1);
  else
    list_collect_append(ptail, list2);

  return out;
}

static val do_sort(val list, val lessfun, val keyfun)
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
      *cdr_l(cons2) = list;
      *cdr_l(list) = nil;
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
    *cdr_l(bisect) = nil;

    return merge(do_sort(list, lessfun, keyfun),
                 do_sort(list2, lessfun, keyfun),
                 lessfun, keyfun);
  }
}

val sort(val list, val lessfun, val keyfun)
{
  if (!keyfun)
    keyfun = identity_f;

  return do_sort(list, lessfun, keyfun);
}

val find(val list, val key, val testfun, val keyfun)
{
  for (; list; list = cdr(list)) {
    val item = car(list);
    val list_key = funcall1(keyfun, item);

    if (funcall2(testfun, key, list_key))
      return item;
  }

  return nil;
}

val set_diff(val list1, val list2, val testfun, val keyfun)
{
  list_collect_decl (out, ptail);

  if (!keyfun)
    keyfun = identity_f;

  if (!testfun)
    testfun = equal_f;

  for (; list1; list1 = cdr(list1)) {
    /* optimization: list2 is a tail of list1, and so we
       are done, unless the application has a weird test function. */
    if (list1 == list2) {
      break;
    } else {
      val item = car(list1);
      val list1_key = funcall1(keyfun, item);

      if (!find(list2, list1_key, testfun, keyfun))
        list_collect (ptail, item);
    }
  }

  return out;
}

val length(val seq)
{
  if (seq == nil)
    return num(0);
  else switch (type(seq)) {
  case CONS:
  case LCONS:
    return length_list(seq);
  case LIT:
  case STR:
    return length_str(seq);
  case VEC:
    return length_vec(seq);
  default:
    type_mismatch(lit("~s is not a sequence"), cons, nao);
  }
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
      list_collect (ptail, string_utf8(*iter));

    return env_list = out;
#elif HAVE_GETENVIRONMENTSTRINGS
    wchar_t *env = GetEnvironmentStringsW();
    wchar_t *iter = env;

    if (iter == 0)
      uw_throwf(error_s, lit("out of memory"), nao);

    for (; *iter; iter += wcslen(iter) + 1)
      list_collect (ptail, string(iter));

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

  protect(&packages, &system_package, &keyword_package,
          &user_package, &null_string, &nil_string,
          &null_list, &equal_f, &eq_f, &eql_f, &car_f, &cdr_f,
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
  *gethash_l(user_package->pk.symhash, nil_string, 0) = nil;

  /* t can't be interned, because gethash_l needs t in order to do its job. */
  t = *gethash_l(user_package->pk.symhash, lit("t"), 0) = make_sym(lit("t"));
  t->s.package = user_package;

  null = intern(lit("null"), user_package);
  cons_s = intern(lit("cons"), user_package);
  str_s = intern(lit("str"), user_package);
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
  var_s = intern(lit("var"), system_package);
  expr_s = intern(lit("expr"), system_package);
  regex_s = intern(lit("regex"), system_package);
  nongreedy_s = intern(lit("nongreedy"), system_package);
  compiled_regex_s = intern(lit("compiled-regex"), system_package);
  quote_s = intern(lit("quote"), system_package);
  qquote_s = intern(lit("qquote"), system_package);
  unquote_s = intern(lit("unquote"), system_package);
  splice_s = intern(lit("splice"), system_package);
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

  args_k = intern(lit("args"), keyword_package);
  nothrow_k = intern(lit("nothrow"), keyword_package);

  equal_f = func_n2(equal);
  eq_f = func_n2(eq);
  eql_f = func_n2(eql);
  identity_f = func_n1(identity);
  car_f = func_n1(car);
  cdr_f = func_n1(cdr);
  prog_string = string(progname);
}

val obj_print(val obj, val out)
{
  if (obj == nil) {
    put_string(out, lit("nil"));
    return obj;
  }

  switch (type(obj)) {
  case CONS:
  case LCONS:
    {
      val sym = car(obj);

      if (sym == quote_s || sym == qquote_s) {
        put_char(out, chr('\''));
        obj_print(second(obj), out);
      } else if (sym == unquote_s) {
        put_char(out, chr(','));
        obj_print(second(obj), out);
      } else if (sym == splice_s) {
        put_string(out, lit(",*"));
        obj_print(second(obj), out);
      } else {
        val iter;
        put_char(out, chr('('));
        for (iter = obj; consp(iter); iter = cdr(iter)) {
          obj_print(car(iter), out);
          if (nullp(cdr(iter))) {
            put_char(out, chr(')'));
          } else if (consp(cdr(iter))) {
            put_char(out, chr(' '));
          } else {
            put_string(out, lit(" . "));
            obj_print(cdr(iter), out);
            put_char(out, chr(')'));
          }
        }
      }
    }
    return obj;
  case LIT:
  case STR:
    {
      const wchar_t *ptr;
      put_char(out, chr('"'));
      for (ptr = c_str(obj); *ptr; ptr++) {
        switch (*ptr) {
        case '\a': put_string(out, lit("\\a")); break;
        case '\b': put_string(out, lit("\\b")); break;
        case '\t': put_string(out, lit("\\t")); break;
        case '\n': put_string(out, lit("\\n")); break;
        case '\v': put_string(out, lit("\\v")); break;
        case '\f': put_string(out, lit("\\f")); break;
        case '\r': put_string(out, lit("\\r")); break;
        case '"': put_string(out, lit("\\\"")); break;
        case '\\': put_string(out, lit("\\\\")); break;
        case 27: put_string(out, lit("\\e")); break;
        default:
          if (iswprint(*ptr))
            put_char(out, chr(*ptr));
          else
            format(out, lit("\\~03o"), num(*ptr), nao);
        }
      }
      put_char(out, chr('"'));
    }
    return obj;
  case CHR:
    {
      wchar_t ch = c_chr(obj);

      put_string(out, lit("#\\"));
      switch (ch) {
      case '\0': put_string(out, lit("nul")); break;
      case '\a': put_string(out, lit("alarm")); break;
      case '\b': put_string(out, lit("backspace")); break;
      case '\t': put_string(out, lit("tab")); break;
      case '\n': put_string(out, lit("newline")); break;
      case '\v': put_string(out, lit("vtab")); break;
      case '\f': put_string(out, lit("page")); break;
      case '\r': put_string(out, lit("return")); break;
      case 27: put_string(out, lit("esc")); break;
      case ' ': put_string(out, lit("space")); break;
      default:
        if (iswprint(ch))
          put_char(out, chr(ch));
        else
          format(out, lit("x~x"), num(ch), nao);
      }
    }
    return obj;
  case NUM:
  case BGNUM:
    format(out, lit("~s"), obj, nao);
    return obj;
  case SYM:
    if (obj->s.package != user_package) {
      if (!obj->s.package)
        put_char(out, chr('#'));
      else if (obj->s.package != keyword_package)
        put_string(out, obj->s.package->pk.name);
      put_char(out, chr(':'));
    }
    put_string(out, symbol_name(obj));
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
      put_string(out, lit("#("));
      for (i = 0; i < length; i++) {
        obj_print(obj->v.vec[i], out);
        if (i < length - 1)
          put_char(out, chr(' '));
      }
      put_char(out, chr(')'));
    }
    return obj;
  case LSTR:
    obj_print(obj->ls.prefix, out);
    put_string(out, lit("#<... lazy string>"));
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
  if (obj == nil) {
    put_string(out, lit("nil"));
    return obj;
  }

  switch (type(obj)) {
  case CONS:
  case LCONS:
    {
      val sym = car(obj);

      if (sym == quote_s || sym == qquote_s) {
        put_char(out, chr('\''));
        obj_pprint(second(obj), out);
      } else if (sym == unquote_s) {
        put_char(out, chr(','));
        obj_pprint(second(obj), out);
      } else if (sym == splice_s) {
        put_string(out, lit(",*"));
        obj_pprint(second(obj), out);
      } else {
        val iter;
        put_char(out, chr('('));
        for (iter = obj; consp(iter); iter = cdr(iter)) {
          obj_pprint(car(iter), out);
          if (nullp(cdr(iter))) {
            put_char(out, chr(')'));
          } else if (consp(cdr(iter))) {
            put_char(out, chr(' '));
          } else {
            put_string(out, lit(" . "));
            obj_pprint(cdr(iter), out);
            put_char(out, chr(')'));
          }
        }
      }
    }
    return obj;
  case LIT:
  case STR:
    put_string(out, obj);
    return obj;
  case CHR:
    put_char(out, obj);
    return obj;
  case NUM:
  case BGNUM:
    format(out, lit("~s"), obj, nao);
    return obj;
  case SYM:
    put_string(out, symbol_name(obj));
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
      put_string(out, lit("#("));
      for (i = 0; i < length; i++) {
        obj_pprint(obj->v.vec[i], out);
        if (i < length - 1)
          put_char(out, chr(' '));
      }
      put_char(out, chr(')'));
    }
    return obj;
  case LSTR:
    obj_pprint(obj->ls.prefix, out);
    put_string(out, lit("..."));
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
  stream_init();
  eval_init();
  filter_init();

  gc_state(gc_save);
}

void dump(val obj, val out)
{
  obj_print(obj, out);
  put_char(out, chr('\n'));
}

/*
 * Handy function for debugging in gdb,
 * so we don't have to keep typing:
 * (gdb) p dump(something, stdout)
 */
void d(val obj)
{
  dump(obj, std_output);
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
