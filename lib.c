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
#include <string.h>
#include <ctype.h>
#include <assert.h>
#include <limits.h>
#include <stdarg.h>
#include <dirent.h>
#include "lib.h"
#include "gc.h"

#define max(a, b) ((a) > (b) ? (a) : (b))

obj_t *interned_syms;

obj_t *null, *t, *cons_t, *str_t, *chr_t, *num_t, *sym_t, *fun_t, *vec_t;
obj_t *stream_t, *lcons_t, *var, *regex, *set, *cset, *wild, *oneplus;
obj_t *zeroplus, *optional, *compound, *or;
obj_t *skip, *block, *next, *fail, *accept;
obj_t *all, *some, *none, *maybe, *collect, *until, *coll;
obj_t *output, *single, *frst, *lst, *empty, *repeat, *rep;
obj_t *flattn, *forget, *mrge, *bind, *cat, *dir;

obj_t *zero, *one, *two, *negone, *maxint, *minint;
obj_t *null_string;
obj_t *nil_string;
obj_t *null_list;

obj_t *identity_f;
obj_t *equal_f;

const char *progname;
void *(*oom_realloc)(void *, size_t);


obj_t *identity(obj_t *obj)
{
  return obj;
}

static obj_t *identity_tramp(obj_t *env, obj_t *obj)
{
  (void) env;
  return identity(obj);
}

static obj_t *equal_tramp(obj_t *env, obj_t *, obj_t *);

obj_t *typeof(obj_t *obj)
{
  if (obj == nil)
    return null;
  switch (obj->t.type) {
  case CONS: return cons_t;
  case STR: return str_t;
  case CHR: return chr_t;
  case NUM: return num_t;
  case SYM: return sym_t;
  case FUN: return fun_t;
  case VEC: return vec_t;
  case STREAM: return stream_t;
  case LCONS: return lcons_t;
  case COBJ: return obj->co.cls;
  }
  assert (0 && "corrupt type field");
}

obj_t *car(obj_t *cons)
{
  if (cons == nil)
    return nil;
  else switch (cons->t.type) {
  case CONS:
    return cons->c.car;
  case LCONS:
    if (cons->lc.func == nil) {
      return cons->lc.car;
    } else {
      if (!funcall1(cons->lc.func, cons))
        return nil;
      return cons->lc.car;
    }
  default:
    assert (0 && "corrupt type field");
  }
}

obj_t *cdr(obj_t *cons)
{
  if (cons == nil)
    return nil;
  else switch (cons->t.type) {
  case CONS:
    return cons->c.cdr;
  case LCONS:
    if (cons->lc.func == nil) {
      return cons->lc.cdr;
    } else {
      if (!funcall1(cons->lc.func, cons))
        return nil;
      return cons->lc.cdr;
    }
  default:
    assert (0 && "corrupt type field");
  }
}

obj_t **car_l(obj_t *cons)
{
  switch (cons->t.type) {
  case CONS:
    return &cons->c.car;
  case LCONS:
    funcall1(cons->lc.func, cons);
    return &cons->lc.car;
  default:
    assert (0 && "corrupt type field");
  }
}

obj_t **cdr_l(obj_t *cons)
{
  switch (cons->t.type) {
  case CONS:
    return &cons->c.cdr;
  case LCONS:
    funcall1(cons->lc.func, cons);
    return &cons->lc.cdr;
  default:
    assert (0 && "corrupt type field");
  }
}

obj_t *first(obj_t *cons)
{
  return car(cons);
}

obj_t *rest(obj_t *cons)
{
  return cdr(cons);
}

obj_t *second(obj_t *cons)
{
  return car(cdr(cons));
}

obj_t *third(obj_t *cons)
{
  return car(cdr(cdr(cons)));
}

obj_t *fourth(obj_t *cons)
{
  return car(cdr(cdr(cdr(cons))));
}

obj_t *fifth(obj_t *cons)
{
  return car(cdr(cdr(cdr(cdr(cons)))));
}

obj_t *sixth(obj_t *cons)
{
  return car(cdr(cdr(cdr(cdr(cdr(cons))))));
}

obj_t **tail(obj_t *cons)
{
  while (cdr(cons))
    cons = cdr(cons);
  return cdr_l(cons);
}

obj_t *copy_list(obj_t *list)
{
  list_collect_decl (out, tail);

  while (consp(list)) {
    list_collect(tail, car(list));
    list = cdr(list);
  }

  list_collect_terminate(tail, list);

  return out;
}

obj_t *nreverse(obj_t *in)
{
  obj_t *rev = nil;

  while (in) {
    obj_t *temp = cdr(in);
    *cdr_l(in) = rev;
    rev = in;
    in = temp;
  }

  return rev;
}

obj_t *reverse(obj_t *in)
{
  obj_t *rev = nil;

  while (in) {
    rev = cons(car(in), rev);
    in = cdr(in);
  }

  return rev;
}

obj_t *append2(obj_t *list1, obj_t *list2)
{
  list_collect_decl (out, tail);

  while (list1) {
    list_collect(tail, car(list1));
    list1 = cdr(list1);
  }

  list_collect_terminate(tail, list2);
  return out;
}

obj_t *nappend2(obj_t *list1, obj_t *list2)
{
  obj_t *temp, *iter;

  if (list1 == nil)
    return list2;

  for (iter = list1; (temp = cdr(iter)) != nil; iter = temp)
    ; /* empty */

  *cdr_l(iter) = list2;
  return list1;
}

obj_t *flatten_helper(obj_t *env, obj_t *item)
{
  return flatten(item);
}

obj_t *memq(obj_t *obj, obj_t *list)
{
  while (list && car(list) != obj)
    list = cdr(list);
  return list;
}

obj_t *tree_find(obj_t *obj, obj_t *tree)
{
  if (equal(obj, tree))
    return t;
  else if (consp(tree))
    return some_satisfy(tree, bind2(func_n2(tree_find), obj), nil);
  return nil;
}

obj_t *some_satisfy(obj_t *list, obj_t *pred, obj_t *key)
{
  if (!key)
    key = identity_f;

  for (; list; list = cdr(list)) {
    if (funcall1(pred, funcall1(key, car(list))))
      return t;
  }

  return nil;
}

obj_t *all_satisfy(obj_t *list, obj_t *pred, obj_t *key)
{
  if (!key)
    key = identity_f;

  for (; list; list = cdr(list)) {
    if (!funcall1(pred, funcall1(key, car(list))))
      return nil;
  }

  return t;
}

obj_t *none_satisfy(obj_t *list, obj_t *pred, obj_t *key)
{
  if (!key)
    key = identity_f;

  for (; list; list = cdr(list)) {
    if (funcall1(pred, funcall1(key, car(list))))
      return nil;
  }

  return t;
}

obj_t *flatten(obj_t *list)
{
  if (atom(list))
    return cons(list, nil);

  return mappend(func_f1(nil, flatten_helper), list);
}

long c_num(obj_t *num);

obj_t *equal(obj_t *left, obj_t *right)
{
  if (left == nil && right == nil)
    return t;

  if (left == nil || right == nil)
    return nil;

  switch (left->t.type) {
  case CONS:
  case LCONS:
    if ((right->t.type == CONS || left->t.type == LCONS) &&
        equal(car(left), car(right)) &&
        equal(cdr(left), cdr(right)))
    {
      return t;
    }
    return nil;
  case STR:
    if (right->t.type == STR &&
        strcmp(left->st.str, right->st.str) == 0)
      return t;
    return nil;
  case CHR:
    if (right->t.type == CHR &&
        left->ch.ch == right->ch.ch)
      return t;
    return nil;
  case NUM:
    if (right->t.type == NUM &&
        left->n.val == right->n.val)
      return t;
    return nil;
  case SYM:
    return right == left ? t : nil;
  case FUN:
    if (right->t.type == FUN &&
        left->f.functype == right->f.functype)
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
    if (right->t.type == VEC) {
      long i, fill;
      if (!equal(left->v.vec[vec_fill], right->v.vec[vec_fill]))
        return nil;
      fill = c_num(left->v.vec[vec_fill]);
      for (i = 0; i < fill; i++) {
        if (!equal(left->v.vec[i], right->v.vec[i]))
          return nil;
      }
      return t;
    }
    return nil;
  case STREAM:
    return nil; /* Different stream objects never equal. */
  case COBJ:
    if (right->t.type == COBJ)
      return left->co.ops->equal(left, right);
    return nil;
  }

  assert (0 && "notreached");
  return nil;
}

static obj_t *equal_tramp(obj_t *env, obj_t *left, obj_t *right)
{
  (void) env;
  return equal(left, right);
}

void *chk_malloc(size_t size)
{
  void *ptr = malloc(size);
  if (size && ptr == 0)
    ptr = oom_realloc(0, size);
  return ptr;
}

void *chk_realloc(void *old, size_t size)
{
  void *newptr = realloc(old, size);
  if (size != 0 && newptr == 0)
    newptr = oom_realloc(old, size);
  return newptr;
}

void *chk_strdup(const char *str)
{
  size_t size = strlen(str) + 1;
  char *copy = chk_malloc(size);
  memcpy(copy, str, size);
  return copy;
}


obj_t *cons(obj_t *car, obj_t *cdr)
{
  obj_t *obj = make_obj();
  obj->c.type = CONS;
  obj->c.car = car;
  obj->c.cdr = cdr;
  return obj;
}

obj_t *list(obj_t *first, ...)
{
  va_list vl;
  obj_t *list = nil;
  obj_t *array[32], **ptr = array;

  if (first != nao) {
    obj_t *next = first;

    va_start (vl, first);

    do {
      *ptr++ = next;
      if (ptr == array + 32)
        abort();
      next = va_arg(vl, obj_t *);
    } while (next != nao);

    while (ptr > array)
      list = cons(*--ptr, list);
  }

  return list;
}

obj_t *consp(obj_t *obj)
{
  if (!obj)
    return nil;
  return (obj->t.type == CONS || obj->t.type == LCONS) ? t : nil;
}

obj_t *nullp(obj_t *obj)
{
  return obj == 0 ? t : nil;
}

obj_t *atom(obj_t *obj)
{
  return (obj == nil || (obj->t.type != CONS && obj->t.type != LCONS))
         ? t : nil;
}

obj_t *listp(obj_t *obj)
{
  return (obj == nil || obj->t.type == CONS || obj->t.type == LCONS)
         ? t : nil;
}

obj_t *length(obj_t *list)
{
  long len = 0;
  while (consp(list)) {
    len++;
    list = cdr(list);
  }
  return num(len);
}

obj_t *num(long val)
{
  obj_t *obj = make_obj();
  obj->n.type = NUM;
  obj->n.val = val;
  return obj;
}

long c_num(obj_t *num)
{
  assert (num && num->t.type == NUM);
  return num->n.val;
}

obj_t *nump(obj_t *num)
{
  return (num && num->n.type == NUM) ? t : nil;
}

obj_t *plus(obj_t *anum, obj_t *bnum)
{
  long a = c_num(anum);
  long b = c_num(bnum);

  assert (a <= 0 || b <= 0 || LONG_MAX - b >= a);
  assert (a >= 0 || b >= 0 || LONG_MIN - b >= a);

  return num(a + b);
}

obj_t *minus(obj_t *anum, obj_t *bnum)
{
  long a = c_num(anum);
  long b = c_num(bnum);

  assert (b != LONG_MIN || LONG_MIN == -LONG_MAX);
  assert (a <= 0 || -b <= 0 || LONG_MAX + b >= a);
  assert (a >= 0 || -b >= 0 || LONG_MIN + b >= a);

  return num(a - b);
}

obj_t *neg(obj_t *anum)
{
  long n = c_num(anum);
  return num(-n);
}

obj_t *zerop(obj_t *num)
{
  return c_num(num) == 0 ? t : nil;
}

obj_t *gt(obj_t *anum, obj_t *bnum)
{
  return c_num(anum) > c_num(bnum) ? t : nil;
}

obj_t *lt(obj_t *anum, obj_t *bnum)
{
  return c_num(anum) < c_num(bnum) ? t : nil;
}

obj_t *ge(obj_t *anum, obj_t *bnum)
{
  return c_num(anum) >= c_num(bnum) ? t : nil;
}

obj_t *le(obj_t *anum, obj_t *bnum)
{
  return c_num(anum) <= c_num(bnum) ? t : nil;
}

obj_t *numeq(obj_t *anum, obj_t *bnum)
{
  return c_num(anum) == c_num(bnum) ? t : nil;
}

obj_t *max2(obj_t *anum, obj_t *bnum)
{
  return c_num(anum) > c_num(bnum) ? anum : bnum;
}

obj_t *min2(obj_t *anum, obj_t *bnum)
{
  return c_num(anum) < c_num(bnum) ? anum : bnum;
}

obj_t *string(char *str)
{
  obj_t *obj = make_obj();
  obj->st.type = STR;
  obj->st.str = str;
  obj->st.len = nil;
  return obj;
}

obj_t *mkstring(obj_t *len, obj_t *ch)
{
  char *str = chk_malloc(c_num(len) + 1);
  memset(str, c_chr(ch), c_num(len));
  str[c_num(len)] = 0;
  return string(str);
}

obj_t *copy_str(obj_t *str)
{
  return string(strdup(c_str(str)));
}

obj_t *stringp(obj_t *str)
{
  return (str && str->st.type == STR) ? t : nil;
}

obj_t *length_str(obj_t *str)
{
  assert (str && str->t.type == STR);
  if (!str->st.len)
    str->st.len = num(strlen(str->st.str));
  return str->st.len;
}

const char *c_str(obj_t *str)
{
  assert (str && str->t.type == STR);
  return str->st.str;
}

obj_t *search_str(obj_t *haystack, obj_t *needle, obj_t *start_num,
                  obj_t *from_end)
{
  const char *h = c_str(haystack);
  long len = c_num(length_str(haystack));
  long start = c_num(start_num);

  if (start > len) {
    return nil;
  } else {
    const char *n = c_str(needle), *good = 0, *pos, *from = h + start;

    do {
      pos = strstr(from, n);
    } while (pos && (good = pos) && from_end && *(from = pos + 1));
    return (good == 0) ? nil : num(good - h);
  }
}

obj_t *search_str_tree(obj_t *haystack, obj_t *tree, obj_t *start_num,
                       obj_t *from_end)
{
  if (stringp(tree)) {
    obj_t *result = search_str(haystack, tree, start_num, from_end);
    if (result)
      return cons(result, length_str(tree));
  } else if (consp(tree)) {
    while (tree) {
      obj_t *result = search_str_tree(haystack, car(tree), start_num, from_end);
      if (result)
        return result;
      tree = cdr(tree);
    }
  }

  return nil;
}

obj_t *sub_str(obj_t *str_in, obj_t *from_num, obj_t *to_num)
{
  const char *str = c_str(str_in);
  size_t len = c_num(length_str(str_in));
  long from = c_num(from_num);
  long to = to_num ? c_num(to_num) : len;

  if (to < 0)
    to = 0;
  if (from < 0)
    from = 0;
  if (from > len)
    from = len;
  if (to > len)
    to = len;

  if (from >= to) {
    return null_string;
  } else {
    size_t size = to - from + 1;
    char *sub = chk_malloc(size);
    strncpy(sub, str + from, size);
    sub[size-1] = 0;
    return string(sub);
  }
}

obj_t *cat_str(obj_t *list, obj_t *sep)
{
  long total = 0;
  obj_t *iter;
  char *str, *ptr;
  long len_sep = sep ? c_num(length_str(sep)) : 0;

  for (iter = list; iter != nil; iter = cdr(iter)) {
    obj_t *item = car(iter);
    if (!item)
      continue;
    if (!stringp(item))
      return nil;
    total += c_num(length_str(item));
    if (len_sep && cdr(iter))
      total += len_sep;
  }

  str = chk_malloc(total + 1);

  for (ptr = str, iter = list; iter != nil; iter = cdr(iter)) {
    obj_t *item = car(iter);
    long len;
    if (!item)
      continue;
    len = c_num(length_str(item));
    memcpy(ptr, c_str(item), len);
    ptr += len;
    if (len_sep && cdr(iter)) {
      memcpy(ptr, c_str(sep), len_sep);
      ptr += len_sep;
    }
  }
  *ptr = 0;

  return string(str);
}

obj_t *trim_str(obj_t *str)
{
  const char *start = c_str(str);
  const char *end = start + c_num(length_str(str));

  while (start[0] && isspace(start[0]))
    start++;

  while (end > start && isspace(end[-1]))
    end--;

  if (end == start) {
    return null_string;
  } else {
    size_t len = end - start;
    char *new = chk_malloc(len + 1);
    memcpy(new, start, len);
    new[len] = 0;
    return string(new);
  }
}

obj_t *string_lt(obj_t *astr, obj_t *bstr)
{
  int cmp = strcmp(c_str(astr), c_str(bstr));
  return cmp == -1 ? t : nil;
}

obj_t *chr(int ch)
{
  obj_t *obj = make_obj();
  obj->ch.type = CHR;
  obj->ch.ch = ch;
  return obj;
}

int c_chr(obj_t *chr)
{
  assert (chr && chr->t.type == CHR);
  return chr->ch.ch;
}

obj_t *sym_name(obj_t *sym)
{
  assert (sym && sym->t.type == SYM);
  return sym->s.name;
}

obj_t *make_sym(obj_t *name)
{
  obj_t *obj = make_obj();
  obj->s.type = SYM;
  obj->s.name = name;
  obj->s.val = nil;
  return obj;
}

obj_t *intern(obj_t *str)
{
  obj_t *iter;

  for (iter = interned_syms; iter != nil; iter = cdr(iter)) {
    obj_t *sym = car(iter);
    if (equal(sym_name(sym), str))
      return sym;
  }

  interned_syms = cons(make_sym(str), interned_syms);
  return car(interned_syms);
}

obj_t *symbolp(obj_t *sym)
{
  return (sym == nil || sym->s.type == SYM) ? t : nil;
}

obj_t *symbol_name(obj_t *sym)
{
  assert (sym == nil || sym->t.type == SYM);
  return sym ? sym->s.name : nil_string;
}

obj_t *func_f0(obj_t *env, obj_t *(*fun)(obj_t *))
{
  obj_t *obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = F0;
  obj->f.env = env;
  obj->f.f.f0 = fun;
  return obj;
}

obj_t *func_f1(obj_t *env, obj_t *(*fun)(obj_t *, obj_t *))
{
  obj_t *obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = F1;
  obj->f.env = env;
  obj->f.f.f1 = fun;
  return obj;
}

obj_t *func_f2(obj_t *env, obj_t *(*fun)(obj_t *, obj_t *, obj_t *))
{
  obj_t *obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = F2;
  obj->f.env = env;
  obj->f.f.f2 = fun;
  return obj;
}

obj_t *func_f3(obj_t *env, obj_t *(*fun)(obj_t *, obj_t *, obj_t *, obj_t *))
{
  obj_t *obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = F3;
  obj->f.env = env;
  obj->f.f.f3 = fun;
  return obj;
}

obj_t *func_f4(obj_t *env, obj_t *(*fun)(obj_t *, obj_t *, obj_t *, obj_t *,
                                         obj_t *))
{
  obj_t *obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = F4;
  obj->f.env = env;
  obj->f.f.f4 = fun;
  return obj;
}

obj_t *func_n0(obj_t *(*fun)(void))
{
  obj_t *obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = N0;
  obj->f.env = nil;
  obj->f.f.n0 = fun;
  return obj;
}

obj_t *func_n1(obj_t *(*fun)(obj_t *))
{
  obj_t *obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = N1;
  obj->f.env = nil;
  obj->f.f.n1 = fun;
  return obj;
}

obj_t *func_n2(obj_t *(*fun)(obj_t *, obj_t *))
{
  obj_t *obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = N2;
  obj->f.env = nil;
  obj->f.f.n2 = fun;
  return obj;
}

obj_t *func_n3(obj_t *(*fun)(obj_t *, obj_t *, obj_t *))
{
  obj_t *obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = N3;
  obj->f.f.n3 = fun;
  return obj;
}

obj_t *func_n4(obj_t *(*fun)(obj_t *, obj_t *, obj_t *, obj_t *))
{
  obj_t *obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = N4;
  obj->f.f.n4 = fun;
  return obj;
}


obj_t *apply(obj_t *fun, obj_t *arglist)
{
  obj_t *arg[4], **p = arg;

  assert (fun && fun->f.type == FUN);
  assert (arglist == nil || consp(arglist));

  *p++ = car(arglist); arglist = cdr(arglist);
  *p++ = car(arglist); arglist = cdr(arglist);
  *p++ = car(arglist); arglist = cdr(arglist);
  *p++ = car(arglist); arglist = cdr(arglist);

  switch (fun->f.functype) {
  case F0:
    return fun->f.f.f0(fun);
  case F1:
    return fun->f.f.f1(fun, arg[0]);
  case F2:
    return fun->f.f.f2(fun, arg[0], arg[1]);
  case F3:
    return fun->f.f.f3(fun, arg[0], arg[1], arg[2]);
  case F4:
    return fun->f.f.f4(fun, arg[0], arg[1], arg[2], arg[3]);
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
  case FINTERP:
    abort();
  }

  assert (0 && "bad functype");
}

obj_t *funcall(obj_t *fun)
{
  assert (fun && fun->f.type == FUN);

  switch (fun->f.functype) {
  case F0:
    return fun->f.f.f0(fun->f.env);
  case N0:
    return fun->f.f.n0();
  default:
    abort();
  }
}

obj_t *funcall1(obj_t *fun, obj_t *arg)
{
  assert (fun && fun->f.type == FUN);

  switch (fun->f.functype) {
  case F1:
    return fun->f.f.f1(fun->f.env, arg);
  case N1:
    return fun->f.f.n1(arg);
  default:
    abort();
  }
}

obj_t *funcall2(obj_t *fun, obj_t *arg1, obj_t *arg2)
{
  assert (fun && fun->f.type == FUN);

  switch (fun->f.functype) {
  case F2:
    return fun->f.f.f2(fun->f.env, arg1, arg2);
  case N2:
    return fun->f.f.n2(arg1, arg2);
  default:
    abort();
  }
}

obj_t *reduce_left(obj_t *fun, obj_t *list, obj_t *init, obj_t *key)
{
  if (!key)
    key = identity_f;

  for (; list; list = cdr(list))
    init = funcall2(fun, init, funcall1(key, car(list)));

  return init;
}

obj_t *do_bind2(obj_t *fcons, obj_t *arg2)
{
  return funcall2(car(fcons), cdr(fcons), arg2);
}

obj_t *bind2(obj_t *fun2, obj_t *arg)
{
  return func_f1(cons(fun2, arg), do_bind2);
}

obj_t *do_bind2other(obj_t *fcons, obj_t *arg1)
{
  return funcall2(car(fcons), arg1, cdr(fcons));
}

obj_t *bind2other(obj_t *fun2, obj_t *arg2)
{
  return func_f1(cons(fun2, arg2), do_bind2other);
}


static obj_t *do_chain(obj_t *fun1_list, obj_t *arg)
{
  for (; fun1_list; fun1_list = cdr(fun1_list))
    arg = funcall1(car(fun1_list), arg);

  return arg;
}

obj_t *chain(obj_t *fun1_list)
{
  return func_f1(fun1_list, do_chain);
}

obj_t *vector(obj_t *alloc)
{
  long alloc_plus = c_num(alloc) + 2;
  obj_t *vec = make_obj();
  obj_t **v = chk_malloc(alloc_plus * sizeof *v);
  vec->v.type = VEC;
  vec->v.vec = v + 2;
  v[0] = alloc;
  v[1] = zero;
  return vec;
}

obj_t *vec_get_fill(obj_t *vec)
{
  assert (vec && vec->v.type == VEC);
  return vec->v.vec[vec_fill];
}

obj_t *vec_set_fill(obj_t *vec, obj_t *fill)
{
  assert (vec && vec->v.type == VEC);

  {
    long new_fill = c_num(fill);
    long old_fill = c_num(vec->v.vec[vec_fill]);
    long old_alloc = c_num(vec->v.vec[vec_alloc]);
    long fill_delta = new_fill - old_fill;
    long alloc_delta = new_fill - old_alloc;

    if (alloc_delta > 0) {
      long new_alloc = max(new_fill, 2*old_alloc);
      obj_t **newvec = chk_realloc(vec->v.vec - 2,
                                   (new_alloc + 2)*sizeof *newvec);
      vec->v.vec = newvec + 2;
      vec->v.vec[vec_alloc] = num(new_alloc);
    }

    if (fill_delta > 0) {
      long i;
      for (i = old_fill; i < new_fill; i++)
        vec->v.vec[i] = nil;
    }

    vec->v.vec[vec_fill] = fill;
  }

  return vec;
}


obj_t **vecref_l(obj_t *vec, obj_t *ind)
{
  assert (vec && vec->v.type == VEC);
  assert (c_num(ind) < c_num(vec->v.vec[vec_fill]));
  return vec->v.vec + c_num(ind);
}

obj_t *vec_push(obj_t *vec, obj_t *item)
{
  obj_t *fill = vec_get_fill(vec);
  vec_set_fill(vec, plus(fill, one));
  *vecref_l(vec, fill) = item;
  return fill;
}


static obj_t *stdio_line_read(struct stream *sm)
{
  if (sm->handle == 0) {
    return nil;
  } else {
    char *line = snarf_line((FILE *) sm->handle);

    if (!line)
      return nil;

    return string(line);
  }
}

static obj_t *stdio_line_write(struct stream *sm, obj_t *obj)
{
  assert (obj->t.type == STR);
  if (sm->handle == 0)
    return nil;
  if (fputs(c_str(obj), (FILE *) sm->handle) == EOF)
    return nil;
  if (putc('\n', (FILE *) sm->handle) == EOF)
    return nil;
  return t;
}

static obj_t *stdio_close(struct stream *sm)
{
  FILE *f = (FILE *) sm->handle;

  if (f != 0 && f != stdin && f != stdout) {
    fclose((FILE *) sm->handle);
    sm->handle = 0;
    return t;
  }
  return nil;
}

static struct stream_ops stdio_line_stream_ops = {
  stdio_line_read, stdio_line_write, stdio_close
};

obj_t *stdio_line_stream(FILE *f, obj_t *label)
{
  obj_t *sm = make_obj();
  sm->sm.type = STREAM;
  sm->sm.handle = f;
  sm->sm.ops = &stdio_line_stream_ops;
  sm->sm.label_pushback = label;
  assert (atom(label));
  return sm;
}

static obj_t *pipe_close(struct stream *sm)
{
  if (sm->handle != 0) {
    pclose((FILE *) sm->handle);
    sm->handle = 0;
    return t;
  }
  return nil;
}

static struct stream_ops pipe_line_stream_ops = {
  stdio_line_read, stdio_line_write, pipe_close
};

obj_t *pipe_line_stream(FILE *f, obj_t *label)
{
  obj_t *sm = make_obj();
  sm->sm.type = STREAM;
  sm->sm.handle = f;
  sm->sm.ops = &pipe_line_stream_ops;
  sm->sm.label_pushback = label;
  assert (atom(label));
  return sm;
}

obj_t *dirent_read(struct stream *sm)
{
  if (sm->handle == 0) {
    return nil;
  } else {
    for (;;) {
      struct dirent *e = readdir(sm->handle);
      if (!e)
        return nil;
      if (!strcmp(e->d_name, ".") || !strcmp(e->d_name, ".."))
        continue;
      return string(chk_strdup(e->d_name));
    }
  }
}

obj_t *dirent_close(struct stream *sm)
{
  if (sm->handle != 0) {
    closedir((DIR *) sm->handle);
    sm->handle = 0;
    return t;
  }

  return nil;
}

static struct stream_ops dirent_stream_ops = {
  dirent_read, 0, dirent_close
};

obj_t *dirent_stream(DIR *d, obj_t *label)
{
  obj_t *sm = make_obj();
  sm->sm.type = STREAM;
  sm->sm.handle = d;
  sm->sm.ops = &dirent_stream_ops;
  sm->sm.label_pushback = label;
  assert (atom(label));
  return sm;
}

obj_t *stream_get(obj_t *sm)
{
  assert (sm->sm.type == STREAM);

  if (consp(sm->sm.label_pushback)) {
    obj_t *ret = car(sm->sm.label_pushback);
    sm->sm.label_pushback = cdr(sm->sm.label_pushback);
    return ret;
  }

  return sm->sm.ops->read(&sm->sm);
}

obj_t *stream_pushback(obj_t *sm, obj_t *obj)
{
  assert (sm->sm.type == STREAM);
  sm->sm.label_pushback = cons(obj, sm->sm.label_pushback);
  return obj;
}

obj_t *stream_put(obj_t *sm, obj_t *obj)
{
  assert (sm->sm.type == STREAM);
  return sm->sm.ops->write(&sm->sm, obj);
}

obj_t *stream_close(obj_t *sm)
{
  assert (sm->sm.type == STREAM);
  return sm->sm.ops->close(&sm->sm);
}


static obj_t *make_lazycons(obj_t *func)
{
  obj_t *obj = make_obj();
  obj->lc.type = LCONS;
  obj->lc.car = obj->lc.cdr = nil;
  obj->lc.func = func;
  return obj;
}

static obj_t *lazy_stream_func(obj_t *stream, obj_t *lcons)
{
  obj_t *next = stream_get(stream);
  obj_t *ahead = stream_get(stream);

  lcons->lc.car = next;
  lcons->lc.cdr = if2(ahead, make_lazycons(lcons->lc.func));
  lcons->lc.func = nil;

  if (!next || !ahead)
    stream_close(stream);

  if (ahead)
    stream_pushback(stream, ahead);

  return next;
}

obj_t *lazy_stream_cons(obj_t *stream)
{
  obj_t *first = stream_get(stream);

  if (!first) {
    stream_close(stream);
    return nil;
  }

  stream_pushback(stream, first);

  return make_lazycons(func_f1(stream, lazy_stream_func));
}

obj_t *cobj(void *handle, obj_t *cls_sym, struct cobj_ops *ops)
{
  obj_t *obj = make_obj();
  obj->co.type = COBJ;
  obj->co.handle = handle;
  obj->co.ops = ops;
  obj->co.cls = cls_sym;
  return obj;
}

void cobj_print_op(obj_t *obj, FILE *out)
{
  fprintf(out, "#<");
  obj_print(obj->co.cls, out);
  fprintf(out, ": %p>", obj->co.handle);
}

obj_t *assoc(obj_t *list, obj_t *key)
{
  while (list) {
    obj_t *elem = car(list);
    if (equal(car(elem), key))
      return elem;
    list = cdr(list);
  }

  return nil;
}

obj_t *acons_new(obj_t *list, obj_t *key, obj_t *value)
{
  obj_t *existing = assoc(list, key);

  if (existing) {
    *cdr_l(existing) = value;
    return list;
  } else {
    return cons(cons(key, value), list);
  }
}

obj_t *alist_remove(obj_t *list, obj_t *keys)
{
  obj_t **plist = &list;

  while (*plist) {
    if (memq(car(car(*plist)), keys))
      *plist = cdr(*plist);
    else
      plist = cdr_l(*plist);
  }

  return list;
}

obj_t *mapcar(obj_t *fun, obj_t *list)
{
  list_collect_decl (out, iter);

  for (; list; list = cdr(list))
    list_collect (iter, funcall1(fun, car(list)));

  return out;
}

obj_t *mappend(obj_t *fun, obj_t *list)
{
  list_collect_decl (out, iter);

  for (; list; list = cdr(list))
    list_collect_append (iter, funcall1(fun, car(list)));

  return out;
}

obj_t *merge(obj_t *list1, obj_t *list2, obj_t *lessfun, obj_t *keyfun)
{
  list_collect_decl (out, ptail);

  while (list1 && list2) {
    obj_t *el1 = funcall1(keyfun, first(list1));
    obj_t *el2 = funcall1(keyfun, first(list2));

    if (funcall2(lessfun, el1, el2)) {
      obj_t *next = cdr(list1);
      *cdr_l(list1) = nil;
      list_collect_append(ptail, list1);
      list1 = next;
    } else {
      obj_t *next = cdr(list2);
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

static obj_t *do_sort(obj_t *list, obj_t *lessfun, obj_t *keyfun)
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
      obj_t *cons2 = cdr(list);
      *cdr_l(cons2) = list;
      *cdr_l(list) = nil;
      return cons2;
    }
  }

  {
    obj_t *bisect, *iter;
    obj_t *list2;

    for (iter = cdr(cdr(list)), bisect = list; iter;
         bisect = cdr(bisect), iter = cdr(cdr(iter)))
      ; /* empty */

    list2 = cdr(bisect);
    *cdr_l(bisect) = nil;

    return merge(sort(list, lessfun, keyfun),
                 sort(list2, lessfun, keyfun),
                 lessfun, keyfun);
  }
}

obj_t *sort(obj_t *list, obj_t *lessfun, obj_t *keyfun)
{
  if (!keyfun)
    keyfun = identity_f;

  return do_sort(list, lessfun, keyfun);
}

static void obj_init(void)
{
  int gc_save = gc_state(0);

  /*
   * No need to GC-protect the convenience variables which hold the interned
   * symbols, because the interned_syms list holds a reference to all the
   * symbols.
   */

  protect(&interned_syms, &zero, &one,
          &two, &negone, &maxint, &minint,
          &null_string, &nil_string,
          &null_list, &equal_f,
          &identity_f, 0);

  null = intern(string(strdup("null")));
  t = intern(string(strdup("t")));
  cons_t = intern(string(strdup("cons")));
  str_t = intern(string(strdup("str")));
  chr_t = intern(string(strdup("chr")));
  num_t = intern(string(strdup("num")));
  sym_t = intern(string(strdup("sym")));
  fun_t = intern(string(strdup("fun")));
  vec_t = intern(string(strdup("vec")));
  stream_t = intern(string(strdup("stream")));
  lcons_t = intern(string(strdup("lcons")));
  var = intern(string(strdup("var")));
  regex = intern(string(strdup("regex")));
  set = intern(string(strdup("set")));
  cset = intern(string(strdup("cset")));
  wild = intern(string(strdup("wild")));
  oneplus = intern(string(strdup("1+")));
  zeroplus = intern(string(strdup("0+")));
  optional = intern(string(strdup("?")));
  compound = intern(string(strdup("compound")));
  or = intern(string(strdup("or")));
  skip = intern(string(strdup("skip")));
  block = intern(string(strdup("block")));
  next = intern(string(strdup("next")));
  fail = intern(string(strdup("fail")));
  accept = intern(string(strdup("accept")));
  all = intern(string(strdup("all")));
  some = intern(string(strdup("some")));
  none = intern(string(strdup("none")));
  maybe = intern(string(strdup("maybe")));
  collect = intern(string(strdup("collect")));
  until = intern(string(strdup("until")));
  coll = intern(string(strdup("coll")));
  output = intern(string(strdup("output")));
  single = intern(string(strdup("single")));
  frst = intern(string(strdup("first")));
  lst = intern(string(strdup("last")));
  empty = intern(string(strdup("empty")));
  repeat = intern(string(strdup("repeat")));
  rep = intern(string(strdup("rep")));
  flattn = intern(string(strdup("flatten")));
  forget = intern(string(strdup("forget")));
  mrge = intern(string(strdup("merge")));
  bind = intern(string(strdup("bind")));
  cat = intern(string(strdup("cat")));
  dir = intern(string(strdup("dir")));

  zero = num(0);
  one  = num(1);
  two = num(2);
  negone = num(-1);
  maxint = num(LONG_MAX);
  minint = num(LONG_MIN);

  null_string = string(strdup(""));
  nil_string = string(strdup("NIL"));

  null_list = cons(nil, nil);

  equal_f = func_f2(nil, equal_tramp);
  identity_f = func_f1(nil, identity_tramp);

  gc_state(gc_save);
}

void obj_print(obj_t *obj, FILE *out)
{
  if (obj == nil) {
    fputs("nil", out);
    return;
  }

  switch (obj->t.type) {
  case CONS:
  case LCONS:
    {
      obj_t *iter;
      putc('(', out);
      for (iter = obj; consp(iter); iter = cdr(iter)) {
        obj_print(car(iter), out);
        if (nullp(cdr(iter))) {
          putc(')', out);
        } else if (consp(cdr(iter))) {
          putc(' ', out);
        } else {
          fputs(" . ", out);
          obj_print(cdr(iter), out);
          putc(')', out);
        }
      }
    }
    break;
  case STR:
    {
      const char *ptr;
      putc('"', out);
      for (ptr = obj->st.str; *ptr; ptr++) {
        switch (*ptr) {
        case '\a': fputs("\\a", out); break;
        case '\b': fputs("\\b", out); break;
        case '\t': fputs("\\t", out); break;
        case '\n': fputs("\\n", out); break;
        case '\v': fputs("\\v", out); break;
        case '\f': fputs("\\f", out); break;
        case '\r': fputs("\\r", out); break;
        case '"': fputs("\\\"", out); break;
        case '\\': fputs("\\\\", out); break;
        case 27: fputs("\\e", out); break;
        default:
          if (iscntrl(*ptr))
            fprintf(out, "\\%03o", (int) *ptr);
          else
            putc(*ptr, out);
        }
      }
      putc('"', out);
    }
    break;
  case CHR:
    {
      int ch = obj->ch.ch;

      putc('\'', out);
      switch (ch) {
      case '\a': fputs("\\a", out); break;
      case '\b': fputs("\\b", out); break;
      case '\t': fputs("\\t", out); break;
      case '\n': fputs("\\n", out); break;
      case '\v': fputs("\\v", out); break;
      case '\f': fputs("\\f", out); break;
      case '\r': fputs("\\r", out); break;
      case '"': fputs("\\\"", out); break;
      case '\\': fputs("\\\\", out); break;
      case 27: fputs("\\e", out); break;
      default:
        if (iscntrl(ch))
          fprintf(out, "\\%03o", ch);
        else
          putc(ch, out);
      }
      putc('\'', out);
    }
    break;
  case NUM:
    fprintf(out, "%ld", c_num(obj));
    break;
  case SYM:
    fputs(c_str(symbol_name(obj)), out);
    break;
  case FUN:
    fprintf(out, "#<function: f%d>", (int) obj->f.functype);
    break;
  case VEC:
    {
      long i, fill = c_num(obj->v.vec[vec_fill]);
      fputs("#(", out);
      for (i = 0; i < fill; i++) {
        obj_print(obj->v.vec[i], out);
        if (i < fill - 1)
          putc(' ', out);
      }
      putc(')', out);
    }
    break;
  case STREAM:
    fprintf(out, "#<stream: ");
    {
      obj_t *iter;
      /* skip stream pushback items to find label */
      for (iter = obj->sm.label_pushback; consp(iter); iter = cdr(iter))
        ;
      obj_print(iter, out);
    }
    fprintf(out, ", %p>", (void *) obj->sm.handle);
    break;
  case COBJ:
    obj->co.ops->print(obj, out);
    break;
  }
}

void init(const char *pn, void *(*oom)(void *, size_t))
{
  progname = pn;
  obj_init();
}

void dump(obj_t *obj, FILE *out)
{
  obj_print(obj, out);
  putc('\n', out);
}

/*
 * Handy function for debugging in gdb,
 * so we don't have to keep typing:
 * (gdb) p dump(something, stdout)
 */
void d(obj_t *obj)
{
  dump(obj, stdout);
}

char *snarf_line(FILE *in)
{
  const size_t min_size = 512;
  size_t size = 0;
  size_t fill = 0;
  char *buf = 0;

  for (;;) {
    int ch = getc(in);

    if (ch == EOF && buf == 0)
      break;

    if (fill >= size) {
      size_t newsize = size ? size * 2 : min_size;
      buf = chk_realloc(buf, newsize);
      size = newsize;
    }

    if (ch == '\n') {
      buf[fill++] = 0;
      break;
    }
    buf[fill++] = ch;
  }

  if (buf)
    buf = chk_realloc(buf, fill);

  return buf;
}

obj_t *snarf(FILE *in)
{
  list_collect_decl (list, iter);
  char *str;

  while ((str = snarf_line(in)) != 0)
    list_collect (iter, string(str));

  return list;
}
