/* Copyright 2010-2015
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

#include <wctype.h>
#include <wchar.h>
#include <signal.h>
#include "config.h"
#include "lib.h"
#include "signal.h"
#include "unwind.h"
#include "eval.h"
#include "hash.h"
#include "combi.h"

static val perm_while_fun(val state)
{
  val p = vecref(state, zero);
  cnum k = c_num(vecref(state, one));
  val c = vecref(state, two);
  cnum n = c_num(length(p));
  cnum i, j;

  for (i = k - 1, j = n - k + 1; i >= 0; i--, j++) {
    cnum ci = c_num(c->v.vec[i]) + 1;

    if (ci >= j) {
      if (i == 0)
        return nil;
      c->v.vec[i] = zero;
    } else {
      c->v.vec[i] = num_fast(ci);
      break;
    }
  }

  return t;
}

static cnum perm_index(cnum n, val b)
{
  cnum i, j;

  for (i = 0, j = 0; i < n; i++, j++) {
    while (b->v.vec[j])
      j++;
  }

  while (b->v.vec[j])
    j++;

  return j;
}

static void perm_gen_fun_common(val state, val out,
                                void (*fill)(val out, cnum i, val v))
{
  val p = vecref(state, zero);
  val kk = vecref(state, one);
  val c = vecref(state, two);
  val nn = length(p);
  val b = vector(nn, nil);
  cnum k = c_num(kk);
  cnum i;

  for (i = 0; i < k; i++) {
    cnum ci = c_num(c->v.vec[i]);
    cnum j = perm_index(ci, b);
    fill(out, i, p->v.vec[j]);
    b->v.vec[j] = t;
  }
}

static val perm_init_common(val p, val k_null)
{
  uses_or2;
  val n = length(p);
  val k = or2(k_null, n);

  if (gt(k, n)) {
    return nil;
  } else {
    val state = vector(three, nil);
    val c = vector(k, zero);
    set(vecref_l(state, zero), p);
    set(vecref_l(state, one), k);
    set(vecref_l(state, two), c);
    deref(vecref_l(c, negone)) = negone;
    return state;
  }
}

static void perm_vec_gen_fill(val out, cnum i, val v)
{
  out->v.vec[i] = v;
}

static val perm_vec_gen_fun(val state)
{
  val kk = vecref(state, one);
  val out = vector(kk, nil);
  perm_gen_fun_common(state, out, perm_vec_gen_fill);
  return out;
}

static val perm_vec(val p, val k)
{
  k = default_arg(k, length_vec(p));

  if (k == zero) {
    return cons(vector(zero, nil), nil);
  } else {
    val state = perm_init_common(p, k);
    if (!state)
      return nil;
    return generate(func_f0(state, perm_while_fun),
                    func_f0(state, perm_vec_gen_fun));
  }
}

static void perm_list_gen_fill(val out, cnum i, val v)
{
  val tail = cdr(out);
  val nc = cons(v, nil);
  if (tail)
    rplacd(tail, nc);
  else
    rplaca(out, nc);
  rplacd(out, nc);
}

static val perm_list_gen_fun(val state)
{
  val out = cons(nil, nil);
  perm_gen_fun_common(state, out, perm_list_gen_fill);
  return car(out);
}

static val perm_list(val p, val k)
{
  if (k == zero || (!k && !p)) {
    return cons(nil, nil);
  } else {
    val state = perm_init_common(vec_list(p), k);
    if (!state)
      return nil;
    return generate(func_f0(state, perm_while_fun),
                    func_f0(state, perm_list_gen_fun));
  }
}

static void perm_str_gen_fill(val out, cnum i, val v)
{
  out->st.str[i] = c_chr(v);
}

static val perm_str_gen_fun(val state)
{
  val kk = vecref(state, one);
  val out = mkustring(kk);
  perm_gen_fun_common(state, out, perm_str_gen_fill);
  out->st.str[c_num(kk)] = 0;
  return out;
}

static val perm_str(val p, val k)
{
  k = default_arg(k, length_str(p));

  if (k == zero) {
    return cons(string(L""), nil);
  } else {
    val state = perm_init_common(vec_list(list_str(p)), k);
    if (!state)
      return nil;
    return generate(func_f0(state, perm_while_fun),
                    func_f0(state, perm_str_gen_fun));
  }
}

val perm(val seq, val k)
{
  if (null_or_missing_p(k)) {
    k = nil;
  } else {
    if (!integerp(k))
      type_mismatch(lit("perm: ~s is not an integer"), k, nao);

    if (lt(k, zero))
      uw_throwf(numeric_error_s, lit("perm: ~s is not a positive integer"),
                k, nao);
  }

  switch (type(seq)) {
  case CONS:
  case LCONS:
  case NIL:
    return perm_list(seq, k);
  case VEC:
    return perm_vec(seq, k);
  case STR:
  case LSTR:
  case LIT:
    return perm_str(seq, k);
  default:
    type_mismatch(lit("perm: ~s is not a sequence"), seq, nao);
  }
}

static val rperm_while_fun(val env)
{
  val vec = cdr(env);
  return consp(vecref(vec, zero));
}

static val rperm_gen_fun(val env)
{
  cons_bind (list, vec, env);
  list_collect_decl(out, ptail);
  cnum i;
  cnum len = c_num(length_vec(vec));

  for (i = 0; i < len; i++)
    ptail = list_collect(ptail, car(vec->v.vec[i]));

  for (i = len-1; i >= 0; i--) {
    pop(&vec->v.vec[i]);
    if (atom(vec->v.vec[i]) && i > 0)
      vec->v.vec[i] = list;
    else
      break;
  }

  return out;
}

static val rperm_list(val list, val k)
{
  val vec = vector(k, list);
  val env = cons(list, vec);
  return generate(func_f0(env, rperm_while_fun),
                  func_f0(env, rperm_gen_fun));
}

static val rperm_vec_gen_fun(val env)
{
  val list = rperm_gen_fun(env);
  return vec_list(list);
}

static val rperm_vec(val ve, val k)
{
  val list = list_vec(ve);
  val vec = vector(k, list);
  val env = cons(list, vec);
  return generate(func_f0(env, rperm_while_fun),
                  func_f0(env, rperm_vec_gen_fun));
}

static val rperm_str_gen_fun(val env)
{
  val list = rperm_gen_fun(env);
  return cat_str(list, nil);
}

static val rperm_str(val str, val k)
{
  val list = list_str(str);
  val vec = vector(k, list);
  val env = cons(list, vec);
  return generate(func_f0(env, rperm_while_fun),
                  func_f0(env, rperm_str_gen_fun));
}

val rperm(val seq, val k)
{
  if (!integerp(k))
    type_mismatch(lit("rperm: ~s is not an integer"), k, nao);

  if (lt(k, zero))
    uw_throwf(numeric_error_s, lit("rperm: ~s is not a positive integer"),
              k, nao);

  switch (type(seq)) {
  case NIL:
    if (zerop(k))
      return cons(nil, nil);
    return nil;
  case CONS:
  case LCONS:
    if (zerop(k))
      return cons(nil, nil);
    return rperm_list(seq, k);
  case VEC:
    if (zerop(k))
      return cons(vector(zero, nil), nil);
    return rperm_vec(seq, k);
  case STR:
  case LSTR:
  case LIT:
    if (zerop(k))
      return cons(string(L""), nil);
    return rperm_str(seq, k);
  default:
    type_mismatch(lit("rperm: ~s is not a sequence"), seq, nao);
  }
}


static val k_conses(val list, val k)
{
  val iter = list, i = k;
  list_collect_decl (out, ptail);

  for (; consp(iter) && gt(i, zero); iter = cdr(iter), i = minus(i, one))
    ptail = list_collect(ptail, iter);

  return (i != zero) ? nil : out;
}

static val comb_while_fun(val state)
{
  return car(state);
}

static void comb_gen_fun_common(val state)
{
  val iter;
  val prev = nil;

  for (iter = state; consp(iter); iter = cdr(iter)) {
    val curr = first(iter);
    val curr_rest = rest(curr);
    if (curr_rest != prev && consp(curr_rest)) {
      set(car_l(iter), curr_rest);
      return;
    } else if (rest(iter)) {
      val next = second(iter);
      val next_rest = rest(next);
      val next_rest_rest = rest(next_rest);
      prev = curr;
      if (next_rest != curr && consp(next_rest_rest))
        prev = set(car_l(iter), next_rest_rest);
    }
  }

  deref(car_l(state)) = nil;
}

static val comb_list_gen_fun(val state)
{
  val out = nreverse(mapcar(car_f, state));
  comb_gen_fun_common(state);
  return out;
}

static val comb_list(val list, val k)
{
  val state = nreverse(k_conses(list, k));
  return state ? generate(func_f0(state, comb_while_fun),
                          func_f0(state, comb_list_gen_fun))
               : nil;
}

static val comb_vec_gen_fun(val state)
{
  val nn = length_list(state);
  cnum i, n = c_num(nn);
  val iter, out = vector(nn, nil);

  for (iter = state, i = n - 1; i >= 0; iter = cdr(iter), i--)
    out->v.vec[i] = car(car(iter));

  comb_gen_fun_common(state);
  return out;
}

static val comb_vec(val vec, val k)
{
  val state = nreverse(k_conses(list_vec(vec), k));
  return generate(func_f0(state, comb_while_fun),
                  func_f0(state, comb_vec_gen_fun));
}

static val comb_str_gen_fun(val state)
{
  val nn = length_list(state);
  cnum i, n = c_num(nn);
  val iter, out = mkustring(nn);

  out->st.str[n] = 0;

  for (iter = state, i = n - 1; i >= 0; iter = cdr(iter), i--)
    out->st.str[i] = c_chr(car(car(iter)));

  comb_gen_fun_common(state);
  return out;
}

static val comb_str(val str, val k)
{
  val state = nreverse(k_conses(list_str(str), k));
  return generate(func_f0(state, comb_while_fun),
                  func_f0(state, comb_str_gen_fun));
}

static val comb_hash_while_fun(val state)
{
  return car(car(state));
}

static val comb_hash_gen_fun(val hstate)
{
  cons_bind (state, hash, hstate);
  val iter, out = make_similar_hash(hash);

  for (iter = state; iter; iter = cdr(iter)) {
    val pair = car(car(iter));
    sethash(out, car(pair), cdr(pair));
  }

  comb_gen_fun_common(state);
  return out;
}


static val comb_hash(val hash, val k)
{
  val hstate = cons(nreverse(k_conses(hash_alist(hash), k)), hash);
  return generate(func_f0(hstate, comb_hash_while_fun),
                  func_f0(hstate, comb_hash_gen_fun));
}

val comb(val seq, val k)
{
  if (!integerp(k))
    type_mismatch(lit("comb: ~s is not an integer"), k, nao);

  if (lt(k, zero))
    uw_throwf(numeric_error_s, lit("comb: ~s is not a positive integer"),
              k, nao);

  switch (type(seq)) {
  case CONS:
  case LCONS:
  case NIL:
    if (k == zero)
      return cons(nil, nil);
    return comb_list(seq, k);
  case VEC:
    if (k == zero)
      return cons(vector(zero, nil), nil);
    if (gt(k, length(seq)))
      return nil;
    return comb_vec(seq, k);
  case STR:
  case LSTR:
  case LIT:
    if (k == zero)
      return cons(string(L""), nil);
    if (gt(k, length(seq)))
      return nil;
    return comb_str(seq, k);
  default:
    if (hashp(seq)) {
      if (k == zero)
        return cons(make_similar_hash(seq), nil);
      if (gt(k, hash_count(seq)))
        return nil;
      return comb_hash(seq, k);
    }
    type_mismatch(lit("comb: ~s is not a sequence"), seq, nao);
  }
}

static val rcomb_while_fun(val state)
{
  return car(state);
}

static void rcomb_gen_fun_common(val state)
{
  val iter;
  val next;

  for (iter = state, next = cdr(state);
       consp(iter);
       iter = next, next = cdr(iter))
  {
    val curr = first(iter);
    val curr_rest = rest(curr);

    if (consp(curr_rest)) {
      val jter;
      for (jter = state; jter != next; jter = cdr(jter))
        set(car_l(jter), curr_rest);
      return;
    } else if (next) {
      val next = second(iter);
      if (curr != next)
        set(car_l(iter), rest(next));
    }
  }

  deref(car_l(state)) = nil;
}

static val rcomb_list_gen_fun(val state)
{
  val out = nreverse(mapcar(car_f, state));
  rcomb_gen_fun_common(state);
  return out;
}

static val rcomb_list(val list, val k)
{
  val state = nreverse(list_vec(vector(k, list)));
  return generate(func_f0(state, rcomb_while_fun),
                  func_f0(state, rcomb_list_gen_fun));
}

static val rcomb_vec_gen_fun(val state)
{
  val nn = length_list(state);
  cnum i, n = c_num(nn);
  val iter, out = vector(nn, nil);

  for (iter = state, i = n - 1; i >= 0; iter = cdr(iter), i--)
    out->v.vec[i] = car(car(iter));

  rcomb_gen_fun_common(state);
  return out;
}

static val rcomb_vec(val vec, val k)
{
  val state = nreverse(list_vec(vector(k, list_vec(vec))));
  return generate(func_f0(state, rcomb_while_fun),
                  func_f0(state, rcomb_vec_gen_fun));
}

static val rcomb_str_gen_fun(val state)
{
  val nn = length_list(state);
  cnum i, n = c_num(nn);
  val iter, out = mkustring(nn);

  out->st.str[n] = 0;

  for (iter = state, i = n - 1; i >= 0; iter = cdr(iter), i--)
    out->st.str[i] = c_chr(car(car(iter)));

  rcomb_gen_fun_common(state);
  return out;
}

static val rcomb_str(val str, val k)
{
  val state = nreverse(list_vec(vector(k, list_str(str))));
  return generate(func_f0(state, rcomb_while_fun),
                  func_f0(state, rcomb_str_gen_fun));
}

val rcomb(val seq, val k)
{
  if (!integerp(k))
    type_mismatch(lit("rcomb: ~s is not an integer"), k, nao);

  if (lt(k, zero))
    uw_throwf(numeric_error_s, lit("rcomb: ~s is not a positive integer"),
              k, nao);

  switch (type(seq)) {
  case CONS:
  case LCONS:
  case NIL:
    if (k == zero)
      return cons(nil, nil);
    return rcomb_list(seq, k);
  case VEC:
    if (k == zero)
      return cons(vector(zero, nil), nil);
    return rcomb_vec(seq, k);
  case STR:
  case LSTR:
  case LIT:
    if (k == zero)
      return cons(string(L""), nil);
    return rcomb_str(seq, k);
  default:
    type_mismatch(lit("rcomb: ~s is not a sequence"), seq, nao);
  }
}
