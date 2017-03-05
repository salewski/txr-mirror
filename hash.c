/* Copyright 2009-2017
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
#include <dirent.h>
#include <stdarg.h>
#include <stdlib.h>
#include <limits.h>
#include <signal.h>
#include "config.h"
#include ALLOCA_H
#include "lib.h"
#include "gc.h"
#include "args.h"
#include "signal.h"
#include "unwind.h"
#include "stream.h"
#include "eval.h"
#include "cadr.h"
#include "hash.h"

typedef enum hash_flags {
  hash_weak_none = 0,
  hash_weak_keys = 1,
  hash_weak_vals = 2,
  hash_weak_both = 3
} hash_flags_t;

struct hash {
  hash_flags_t flags;
  struct hash *next;
  val table;
  cnum modulus;
  cnum count;
  val userdata;
  int usecount;
  cnum (*hash_fun)(val, int *);
  val (*equal_fun)(val, val);
  val (*assoc_fun)(val key, cnum hash, val list);
  val (*acons_new_c_fun)(val key, cnum hash, loc new_p, loc list);
};

struct hash_iter {
  struct hash_iter *next;
  val hash;
  cnum chain;
  val cons;
};

val weak_keys_k, weak_vals_k, equal_based_k, userdata_k;

/*
 * Dynamic lists built up during gc.
 */
static struct hash *reachable_weak_hashes;
static struct hash_iter *reachable_iters;

static int hash_str_limit = 128, hash_rec_limit = 32;

/* C99 inline instantiations. */
#if __STDC_VERSION__ >= 199901L
loc gethash_l(val hash, val key, loc new_p);
#endif

/*
 * This is is an adaptation of hashpjw, from Compilers: Principles, Techniques
 * and Tools, Aho, Sethi, Ulman, 1988. P. 436.  The register is wider by
 * a few bits, and we bring down five overflow bits instead of four.
 * We don't reduce the final result modulo a small prime, but leave it
 * as it is; let the hashing routines do their own reduction.
 */
static unsigned long hash_c_str(const wchar_t *str)
{
  int count = hash_str_limit;
  unsigned long h = 0;
  while (*str && count--) {
    unsigned long g;
    h = (h << 4) + *str++;
    g = h & 0x7C000000;
    h = h ^ (g >> 26) ^ g;
  }
  return h;
}

static cnum hash_double(double n)
{
#ifdef HAVE_UINTPTR_T
  uint_ptr_t h = 0;
#else
  unsigned long h = 0;
#endif

  mem_t *p = coerce(mem_t *, &n), *q = p + sizeof(double);

  while (p < q) {
    h = h << 8 | h >> (8 * sizeof h - 1);
    h += *p++;
  }

  return h & NUM_MAX;
}

cnum equal_hash(val obj, int *count)
{
  if ((*count)-- <= 0)
    return 0;

  switch (type(obj)) {
  case NIL:
    return NUM_MAX;
  case LIT:
    return hash_c_str(litptr(obj)) & NUM_MAX;
  case CONS:
    return (equal_hash(obj->c.car, count)
            + 32 * (equal_hash(obj->c.cdr, count) & (NUM_MAX / 16))) & NUM_MAX;
  case STR:
    return hash_c_str(obj->st.str) & NUM_MAX;
  case CHR:
    return c_chr(obj) & NUM_MAX;
  case NUM:
    return c_num(obj) & NUM_MAX;
  case SYM:
  case PKG:
  case ENV:
    switch (sizeof (mem_t *)) {
    case 4:
      return (coerce(cnum, obj) >> 4) & NUM_MAX;
    case 8: default:
      return (coerce(cnum, obj) >> 5) & NUM_MAX;
    }
    break;
  case FUN:
    return (coerce(cnum, obj->f.f.interp_fun)
            + equal_hash(obj->f.env, count)) & NUM_MAX;
  case VEC:
    {
      val length = obj->v.vec[vec_length];
      cnum i, h = equal_hash(obj->v.vec[vec_length], count);
      cnum len = c_num(length);

      for (i = 0; i < len; i++) {
        h = 32 * (h & (NUM_MAX / 16));
        h += equal_hash(obj->v.vec[i], count);
        h &= NUM_MAX;
      }

      return h;
    }
  case LCONS:
    return (equal_hash(car(obj), count)
            + 32 * (equal_hash(cdr(obj), count) & (NUM_MAX / 16))) & NUM_MAX;
  case LSTR:
    lazy_str_force_upto(obj, num(hash_str_limit - 1));
    return equal_hash(obj->ls.prefix, count);
  case BGNUM:
    return mp_hash(mp(obj)) & NUM_MAX;
  case FLNUM:
    return hash_double(obj->fl.n);
  case COBJ:
    if (obj->co.ops->equalsub) {
      val sub = obj->co.ops->equalsub(obj);
      if (sub)
        return equal_hash(sub, count);
    }
    return obj->co.ops->hash(obj, count) & NUM_MAX;
  case RNG:
    return (equal_hash(obj->rn.from, count)
            + 32 * (equal_hash(obj->rn.to, count) & (NUM_MAX / 16))) & NUM_MAX;
  }

  internal_error("unhandled case in equal function");
}

static cnum eql_hash(val obj, int *count)
{
  switch (tag(obj)) {
  case TAG_PTR:
    switch (type(obj)) {
    case NIL:
      return NUM_MAX;
    case BGNUM:
      return mp_hash(mp(obj)) & NUM_MAX;
    case FLNUM:
      return hash_double(obj->fl.n);
    case RNG:
      return (eql_hash(obj->rn.from, count)
              + 32 * (eql_hash(obj->rn.to, count) & (NUM_MAX / 16))) & NUM_MAX;
    default:
      switch (sizeof (mem_t *)) {
      case 4:
        return (coerce(cnum, obj) >> 4) & NUM_MAX;
      case 8: default:
        return (coerce(cnum, obj) >> 5) & NUM_MAX;
      }
    }
  case TAG_CHR:
    return c_chr(obj) & NUM_MAX;
  case TAG_NUM:
    return c_num(obj) & NUM_MAX;
  case TAG_LIT:
    switch (sizeof (mem_t *)) {
    case 4:
      return (coerce(cnum, obj) >> 2) & NUM_MAX;
    case 8: default:
      return (coerce(cnum, obj) >> 3) & NUM_MAX;
    }
  }
  /* notreached */
  abort();
}

cnum cobj_hash_op(val obj, int *count)
{
  (void) count;

  switch (sizeof (mem_t *)) {
  case 4:
    return (coerce(cnum, obj) >> 4) & NUM_MAX;
  case 8: default:
    return (coerce(cnum, obj) >> 5) & NUM_MAX;
  }
  /* notreached */
  abort();
}

static val hash_equal_op(val left, val right)
{
  uses_or2;
  struct hash *l = coerce(struct hash *, left->co.handle);
  struct hash *r = coerce(struct hash *, right->co.handle);
  val liter, riter, lcell, rcell;
  val free_conses = nil;
  val pending = nil;

  if (l->hash_fun != r->hash_fun)
    return nil;

  if (l->count != r->count)
    return nil;

  if (!equal(l->userdata, r->userdata))
    return nil;

  if (l->count == 0)
    return t;

  liter = hash_begin(left);
  riter = hash_begin(right);

  while ((lcell = hash_next(liter)) && ((rcell = hash_next(riter)))) {
    val ncons = or2(pop(&free_conses), cons(nil, nil));
    val found;

    /*
     * Short circuit the logic if we have two identical cells;
     * no need to go through the pending list.
     */

    if (l->equal_fun(car(lcell), car(rcell)) && equal(cdr(lcell), cdr(rcell)))
      continue;

    /*
     * Try to find a cell matching the left cell on the pending list by key.
     * If it is found, and the associated datum is equal, then remove it from
     * the list.  If it is found and the data is not equal, then we have found
     * a difference between the hash tables, and conclude they are different.
     * If there is no match, then we insert the cell into the pending list.
     */
    found = l->assoc_fun(car(lcell), lcell->ch.hash, pending);

    if (found && !equal(cdr(found), cdr(lcell))) {
      return nil;
    } else if (found) {
      val loc = memq(found, pending);
      pending = nappend2(ldiff(pending, loc), cdr(loc));
      set(cdr_l(loc), free_conses);
      free_conses = loc;
    } else {
      ncons = or2(pop(&free_conses), cons(nil, nil));
      set(car_l(ncons), lcell);
      set(cdr_l(ncons), pending);
      pending = ncons;
    }

    /*
     * The logic is mirrored for the right cell.
     */
    found = l->assoc_fun(car(rcell), rcell->ch.hash, pending);

    if (found && !equal(cdr(found), cdr(rcell))) {
      return nil;
    } else if (found) {
      val loc = memq(found, pending);
      pending = nappend2(ldiff(pending, loc), cdr(loc));
      set(cdr_l(loc), free_conses);
      free_conses = loc;
    } else {
      ncons = or2(pop(&free_conses), cons(nil, nil));
      set(car_l(ncons), rcell);
      set(cdr_l(ncons), pending);
      pending = ncons;
    }
  }

  /*
   * The hashes are equal if and only if the pending list
   * balances down to zero.
   */
  return eq(pending, nil);
}

static cnum hash_hash_op(val obj, int *count)
{
  cnum out = 0;
  struct hash *h = coerce(struct hash *, obj->co.handle);
  val iter, cell;

  if ((*count)-- <= 0)
    return 0;

  switch (sizeof (mem_t *)) {
  case 4:
    out += coerce(cnum, h->hash_fun) >> 4;
  case 8: default:
    out += coerce(cnum, h->hash_fun) >> 5;
  }

  out += equal_hash(h->userdata, count);
  out &= NUM_MAX;

  iter = hash_begin(obj);

  while ((cell = hash_next(iter)) != nil) {
    out += equal_hash(cell, count);
    out &= NUM_MAX;
  }

  return out;
}

static void hash_print_op(val hash, val out, val pretty, struct strm_ctx *ctx)
{
  struct hash *h = coerce(struct hash *, hash->co.handle);
  int need_space = 0;
  val save_mode = test_set_indent_mode(out, num_fast(indent_off),
                                       num_fast(indent_data));
  val save_indent;

  put_string(lit("#H("), out);

  save_indent = inc_indent(out, zero);

  put_char(chr('('), out);

  if (h->hash_fun == equal_hash) {
    obj_print_impl(equal_based_k, out, pretty, ctx);
    need_space = 1;
  }
  if (h->flags != hash_weak_none) {
    if (need_space)
      put_char(chr(' '), out);
    need_space = 1;
    switch (h->flags) {
    case hash_weak_both:
      obj_print_impl(weak_keys_k, out, pretty, ctx);
      put_char(chr(' '), out);
      /* fallthrough */
    case hash_weak_vals:
      obj_print_impl(weak_vals_k, out, pretty, ctx);
      break;
    case hash_weak_keys:
      obj_print_impl(weak_keys_k, out, pretty, ctx);
      break;
    default:
      break;
    }
  }
  if (h->userdata) {
    if (need_space)
      put_char(chr(' '), out);
    obj_print_impl(userdata_k, out, pretty, ctx);
    put_char(chr(' '), out);
    obj_print_impl(h->userdata, out, pretty, ctx);
  }
  put_string(lit(")"), out);
  {
    val iter = hash_begin(hash), cell;
    while ((cell = hash_next(iter))) {
      val key = car(cell);
      val value = cdr(cell);
      width_check(out, chr(' '));

      put_string(lit("("), out);
      obj_print_impl(key, out, pretty, ctx);

      if (value) {
        put_string(lit(" "), out);
        obj_print_impl(value, out, pretty, ctx);
      }

      put_string(lit(")"), out);
    }
  }
  put_string(lit(")"), out);

  set_indent_mode(out, save_mode);
  set_indent(out, save_indent);
}

static void hash_mark(val hash)
{
  struct hash *h = coerce(struct hash *, hash->co.handle);
  cnum i;

  gc_mark(h->userdata);

  /* Use counts will be re-calculated by a scan of the
     hash iterators which are still reachable. */
  h->usecount = 0;

  switch (h->flags) {
  case hash_weak_none:
    /* If the hash is not weak, we can simply mark the table
       vector and we are done. */
    gc_mark(h->table);
    break;
  case hash_weak_keys:
    /* Keys are weak: mark the values only. */
    for (i = 0; i < h->modulus; i++) {
      val ind = num_fast(i);
      val chain = vecref(h->table, ind);
      val iter;

      for (iter = chain; iter != nil; iter = cdr(iter)) {
        val entry = car(iter);
        gc_mark(cdr(entry));
      }
    }
    h->next = reachable_weak_hashes;
    reachable_weak_hashes = h;
    break;
  case hash_weak_vals:
    /* Values are weak: mark the keys only. */

    for (i = 0; i < h->modulus; i++) {
      val ind = num_fast(i);
      val chain = vecref(h->table, ind);
      val iter;

      for (iter = chain; iter != nil; iter = cdr(iter)) {
        val entry = car(iter);
        gc_mark(car(entry));
      }
    }
    h->next = reachable_weak_hashes;
    reachable_weak_hashes = h;
    break;
  case hash_weak_both:
    /* Values and keys are weak: don't mark anything. */
    h->next = reachable_weak_hashes;
    reachable_weak_hashes = h;
    break;
  }
}

static struct cobj_ops hash_ops = cobj_ops_init(hash_equal_op,
                                                hash_print_op,
                                                cobj_destroy_free_op,
                                                hash_mark,
                                                hash_hash_op);

static void hash_grow(struct hash *h, val hash)
{
  cnum i;
  cnum new_modulus = 2 * h->modulus;
  val new_table = vector(num_fast(new_modulus), nil);

  bug_unless (new_modulus > h->modulus);

  for (i = 0; i < h->modulus; i++) {
    val conses = vecref(h->table, num_fast(i));

    while (conses) {
      val entry = car(conses);
      val next = cdr(conses);
      loc pchain = vecref_l(new_table,
                            num_fast(entry->ch.hash % new_modulus));
      set(cdr_l(conses), deref(pchain));
      set(pchain, conses);
      conses = next;
    }
  }

  h->modulus = new_modulus;
  h->table = new_table;
  set(mkloc(h->table, hash), new_table);
}

static val hash_assoc(val key, cnum hash, val list)
{
  list = nullify(list);

  while (list) {
    val elem = car(list);
    if (elem->ch.hash == hash && equal(car(elem), key))
      return elem;
    list = cdr(list);
  }

  return nil;
}

static val hash_assql(val key, cnum hash, val list)
{
  list = nullify(list);

  while (list) {
    val elem = car(list);
    if (elem->ch.hash == hash && eql(car(elem), key))
      return elem;
    list = cdr(list);
  }

  return nil;
}

static val hash_acons_new_c(val key, cnum hash, loc new_p, loc list)
{
  val existing = hash_assoc(key, hash, deref(list));

  if (existing) {
    if (!nullocp(new_p))
      deref(new_p) = nil;
    return existing;
  } else {
    val nc = cons(key, nil);
    nc->ch.hash = hash;
    set(list, cons(nc, deref(list)));
    if (!nullocp(new_p))
      deref(new_p) = t;
    return nc;
  }
}

static val hash_aconsql_new_c(val key, cnum hash, loc new_p, loc list)
{
  val existing = hash_assql(key, hash, deref(list));

  if (existing) {
    if (!nullocp(new_p))
      deref(new_p) = nil;
    return existing;
  } else {
    val nc = cons(key, nil);
    nc->ch.hash = hash;
    set(list, cons(nc, deref(list)));
    if (!nullocp(new_p))
      deref(new_p) = t;
    return nc;
  }
}

val make_hash(val weak_keys, val weak_vals, val equal_based)
{
  if (weak_keys && equal_based) {
    uw_throwf(error_s,
              lit("make-hash: bad combination :weak-keys with :equal-based"),
              nao);
  } else {
    int flags = ((weak_vals != nil) << 1) | (weak_keys != nil);
    struct hash *h = coerce(struct hash *, chk_malloc(sizeof *h));
    val mod = num_fast(256);
    val table = vector(mod, nil);
    val hash = cobj(coerce(mem_t *, h), hash_s, &hash_ops);

    h->flags = convert(hash_flags_t, flags);
    h->modulus = c_num(mod);
    h->count = 0;
    h->table = table;
    h->userdata = nil;

    h->usecount = 0;
    h->hash_fun = equal_based ? equal_hash : eql_hash;
    h->equal_fun = equal_based ? equal : eql;
    h->assoc_fun = equal_based ? hash_assoc : hash_assql;
    h->acons_new_c_fun = equal_based ? hash_acons_new_c : hash_aconsql_new_c;

    return hash;
  }
}

val make_similar_hash(val existing)
{
  struct hash *ex = coerce(struct hash *, cobj_handle(existing, hash_s));
  struct hash *h = coerce(struct hash *, chk_malloc(sizeof *h));
  val mod = num_fast(256);
  val table = vector(mod, nil);
  val hash = cobj(coerce(mem_t *, h), hash_s, &hash_ops);

  h->modulus = c_num(mod);
  h->count = 0;
  h->table = table;
  h->userdata = ex->userdata;

  h->flags = ex->flags;
  h->usecount = 0;
  h->hash_fun = ex->hash_fun;
  h->equal_fun = ex->equal_fun;
  h->assoc_fun = ex->assoc_fun;
  h->acons_new_c_fun = ex->acons_new_c_fun;

  return hash;
}

val copy_hash(val existing)
{
  struct hash *ex = coerce(struct hash *, cobj_handle(existing, hash_s));
  struct hash *h = coerce(struct hash *, chk_malloc(sizeof *h));
  val mod = num_fast(ex->modulus);
  val table = vector(mod, nil);
  val hash = cobj(coerce(mem_t *, h), hash_s, &hash_ops);
  val iter;

  h->modulus = ex->modulus;
  h->count = ex->count;
  h->table = table;
  h->userdata = ex->userdata;

  h->flags = ex->flags;
  h->usecount = 0;
  h->hash_fun = ex->hash_fun;
  h->assoc_fun = ex->assoc_fun;
  h->acons_new_c_fun = ex->acons_new_c_fun;

  for (iter = zero; lt(iter, mod); iter = plus(iter, one))
    set(vecref_l(h->table, iter), copy_alist(vecref(ex->table, iter)));

  return hash;
}

val gethash_c(val hash, val key, loc new_p)
{
  struct hash *h = coerce(struct hash *, cobj_handle(hash, hash_s));
  int lim = hash_rec_limit;
  cnum hv = h->hash_fun(key, &lim);
  loc pchain = vecref_l(h->table, num_fast(hv % h->modulus));
  val old = deref(pchain);
  val cell = h->acons_new_c_fun(key, hv, new_p, pchain);
  if (old != deref(pchain) && ++h->count > 2 * h->modulus && h->usecount == 0)
    hash_grow(h, hash);
  return cell;
}

val gethash_e(val hash, val key)
{
  struct hash *h = coerce(struct hash *, cobj_handle(hash, hash_s));
  int lim = hash_rec_limit;
  cnum hv = h->hash_fun(key, &lim);
  val chain = vecref(h->table, num_fast(hv % h->modulus));
  return h->assoc_fun(key, hv, chain);
}

val gethash(val hash, val key)
{
  val found = gethash_e(hash, key);
  return cdr(found);
}

val inhash(val hash, val key, val init)
{
  val cell;

  if (missingp(init)) {
    gethash_f(hash, key, mkcloc(cell));
  } else {
    val new_p;
    cell = gethash_c(hash, key, mkcloc(new_p));
    if (new_p)
      rplacd(cell, init);
  }

  return cell;
}

val gethash_f(val hash, val key, loc found)
{
  set(found, gethash_e(hash, key));
  return cdr(deref(found));
}

val gethash_n(val hash, val key, val notfound_val)
{
  val existing = gethash_e(hash, key);
  return if3(existing, cdr(existing), default_bool_arg(notfound_val));
}

val sethash(val hash, val key, val value)
{
  val new_p;
  rplacd(gethash_c(hash, key, mkcloc(new_p)), value);
  return value;
}

val pushhash(val hash, val key, val value)
{
  val new_p;
  mpush(value, gethash_l(hash, key, mkcloc(new_p)));
  return new_p;
}

val remhash(val hash, val key)
{
  struct hash *h = coerce(struct hash *, cobj_handle(hash, hash_s));
  int lim = hash_rec_limit;
  cnum hv = h->hash_fun(key, &lim);
  loc pchain = vecref_l(h->table, num_fast(hv % h->modulus));
  val existing = h->assoc_fun(key, hv, deref(pchain));

  if (existing) {
    val cell = memq(existing, deref(pchain));
    set(pchain, nappend2(ldiff(deref(pchain), cell), cdr(cell)));
    h->count--;
    bug_unless (h->count >= 0);
    return cdr(existing);
  }

  return nil;
}

val clearhash(val hash)
{
  struct hash *h = coerce(struct hash *, cobj_handle(hash, hash_s));
  val mod = num_fast(256);
  val table = vector(mod, nil);
  cnum oldcount = h->count;
  h->modulus = c_num(mod);
  h->count = 0;
  h->table = table;
  return oldcount ? num(oldcount) : nil;
}

val hash_count(val hash)
{
  struct hash *h = coerce(struct hash *, cobj_handle(hash, hash_s));
  return num_fast(h->count);
}

val get_hash_userdata(val hash)
{
  struct hash *h = coerce(struct hash *, cobj_handle(hash, hash_s));
  return h->userdata;
}

val set_hash_userdata(val hash, val data)
{
  struct hash *h = coerce(struct hash *, cobj_handle(hash, hash_s));
  val olddata = h->userdata;
  set(mkloc(h->userdata, hash), data);
  return olddata;
}

val hashp(val obj)
{
  return typeof(obj) == hash_s ? t : nil;
}

static void hash_iter_mark(val hash_iter)
{
  struct hash_iter *hi = coerce(struct hash_iter *, hash_iter->co.handle);
  if (hi->hash)
    gc_mark(hi->hash);
  gc_mark(hi->cons);
  hi->next = reachable_iters;
  reachable_iters = hi;
}

static struct cobj_ops hash_iter_ops = cobj_ops_init(eq,
                                                     cobj_print_op,
                                                     cobj_destroy_free_op,
                                                     hash_iter_mark,
                                                     cobj_hash_op);

val hash_begin(val hash)
{
  val hi_obj;
  struct hash *h = coerce(struct hash *, cobj_handle(hash, hash_s));
  struct hash_iter *hi = coerce(struct hash_iter *, chk_malloc(sizeof *hi));

  hi->next = 0;
  hi->hash = nil;
  hi->chain = -1;
  hi->cons = nil;
  hi_obj = cobj(coerce(mem_t *, hi), hash_iter_s, &hash_iter_ops);
  hi->hash = hash;
  h->usecount++;
  return hi_obj;
}

val hash_next(val iter)
{
  struct hash_iter *hi = coerce(struct hash_iter *, cobj_handle(iter, hash_iter_s));
  val hash = hi->hash;
  struct hash *h = hash ? coerce(struct hash *, hash->co.handle) : 0;

  if (!h)
    return nil;
  if (hi->cons)
    hi->cons = cdr(hi->cons);
  while (nilp(hi->cons)) {
    if (++hi->chain >= h->modulus) {
      hi->hash = nil;
      h->usecount--;
      return nil;
    }
    set(mkloc(hi->cons, iter), vecref(h->table, num_fast(hi->chain)));
  }
  return car(hi->cons);
}

val maphash(val fun, val hash)
{
  val iter = hash_begin(hash);
  val cell;
  while ((cell = hash_next(iter)) != nil)
    funcall2(fun, car(cell), cdr(cell));
  return nil;
}

val hash_eql(val obj)
{
  int lim = 0;
  return num_fast(eql_hash(obj, &lim));
}

val hash_equal(val obj)
{
  int lim = hash_rec_limit;
  return num_fast(equal_hash(obj, &lim));
}

/*
 * Called from garbage collector. Hash module must process all weak tables
 * that were visited during the marking phase, maintained in the list
 * reachable_weak_hashes.
 */
static void do_weak_tables(void)
{
  struct hash *h;
  cnum i;

  for (h = reachable_weak_hashes; h != 0; h = h->next) {
    /* The table of a weak hash was spuriously reached by conservative GC;
       it's a waste of time doing weak processing, since all keys and
       values have been transitively marked as reachable; and so we
       won't find anything to remove. */
    if (gc_is_reachable(h->table))
      continue;

    switch (h->flags) {
    case hash_weak_none:
      /* what is this doing here */
      break;
    case hash_weak_keys:
      /* Sweep through all entries. Delete any which have keys
         that are garbage. */
      for (i = 0; i < h->modulus; i++) {
        val ind = num_fast(i);
        val *pchain = valptr(vecref_l(h->table, ind));
        val *iter;

        for (iter = pchain; !gc_is_reachable(*iter); ) {
          val entry = car(*iter);
          if (!gc_is_reachable(entry) && !gc_is_reachable(car(entry))) {
            *iter = cdr(*iter);
#if CONFIG_EXTRA_DEBUGGING
            if (car(entry) == break_obj)
              breakpt();
#endif
          } else {
            iter = valptr(cdr_l(*iter));
          }
        }
      }
      /* Garbage is gone now. Seal things by marking the vector. */
      gc_mark(h->table);
      break;
    case hash_weak_vals:
      /* Sweep through all entries. Delete any which have values
         that are garbage. */
      for (i = 0; i < h->modulus; i++) {
        val ind = num_fast(i);
        val *pchain = valptr(vecref_l(h->table, ind));
        val *iter;

        for (iter = pchain; !gc_is_reachable(*iter); ) {
          val entry = car(*iter);
          if (!gc_is_reachable(entry) && !gc_is_reachable(cdr(entry))) {
            *iter = cdr(*iter);
#if CONFIG_EXTRA_DEBUGGING
            if (cdr(entry) == break_obj)
              breakpt();
#endif
          } else {
            iter = valptr(cdr_l(*iter));
          }
        }
      }
      /* Garbage is gone now. Seal things by marking the vector. */
      gc_mark(h->table);
      break;
    case hash_weak_both:
      /* Sweep through all entries. Delete any which have keys
         or values that are garbage. */
      for (i = 0; i < h->modulus; i++) {
        val ind = num_fast(i);
        val *pchain = valptr(vecref_l(h->table, ind));
        val *iter;

        for (iter = pchain; !gc_is_reachable(*iter); ) {
          val entry = car(*iter);
          if (!gc_is_reachable(entry) &&
              (!gc_is_reachable(car(entry)) || !gc_is_reachable(cdr(entry))))
          {
            *iter = cdr(*iter);
#if CONFIG_EXTRA_DEBUGGING
            if (!gc_is_reachable(car(entry)) && car(entry) == break_obj)
              breakpt();
            if (!gc_is_reachable(cdr(entry)) && cdr(entry) == break_obj)
              breakpt();
#endif
          } else {
            iter = valptr(cdr_l(*iter));
          }
        }
      }
      /* Garbage is gone now. Seal things by marking the vector. */
      gc_mark(h->table);
      break;
    }
  }

  /* Done with weak processing; clear out the list in preparation for
     the next gc round. */
  reachable_weak_hashes = 0;
}

static void do_iters(void)
{
  struct hash_iter *hi;

  for (hi = reachable_iters; hi != 0; hi = hi->next) {
    val hash = hi->hash;

    if (!hash)
      continue;

#if CONFIG_GEN_GC
    /* If the hash is a tenured object, we do not touch it.
       It wasn't marked and so its usecount wasn't reset to zero. */
    if (!full_gc && hash->t.gen > 0)
      continue;
#endif

    {
      struct hash *h = coerce(struct hash *, hash->co.handle);
      h->usecount++;
    }
  }

  reachable_iters = 0;
}

void hash_process_weak(void)
{
  do_weak_tables();
  do_iters();
}

val hashv(struct args *args)
{
  val arglist = args_get_list(args);
  val wkeys = memq(weak_keys_k, arglist);
  val wvals = memq(weak_vals_k, arglist);
  val equal = memq(equal_based_k, arglist);
  val userdata = cadr(memq(userdata_k, arglist));
  val hash = make_hash(wkeys, wvals, equal);
  if (userdata)
    set_hash_userdata(hash, userdata);
  return hash;
}

val hashl(val arglist)
{
  args_decl_list(args, ARGS_MIN, arglist);
  return hashv(args);
}

val hash_construct(val hashl_args, val pairs)
{
  val hash = hashl(hashl_args);

  pairs = nullify(pairs);

  for (; pairs; pairs = cdr(pairs)) {
    val pair = car(pairs);
    sethash(hash, first(pair), second(pair));
  }

  return hash;
}

val hash_from_pairs_v(val pairs, struct args *hashv_args)
{
  return hash_construct(args_get_list(hashv_args), pairs);
}

val hash_list(val keys, struct args *hashv_args)
{
  val hash = hashv(hashv_args);

  keys = nullify(keys);

  for (; keys; keys = cdr(keys)) {
    val key = car(keys);
    sethash(hash, key, key);
  }

  return hash;
}

val group_by(val func, val seq, struct args *hashv_args)
{
  val hash = hashv(hashv_args);

  if (vectorp(seq)) {
    cnum i, len;

    for (i = 0, len = c_num(length(seq)); i < len; i++) {
      val v = vecref(seq, num_fast(i));
      pushhash(hash, funcall1(func, v), v);
    }
  } else {
    for (; seq; seq = cdr(seq)) {
      val v = car(seq);
      pushhash(hash, funcall1(func, v), v);
    }
  }

  {
    val iter = hash_begin(hash);
    val cell;

    while ((cell = hash_next(iter)) != nil)
      rplacd(cell, nreverse(cdr(cell)));

    return hash;
  }
}

val group_reduce(val hash, val by_fun, val reduce_fun, val seq,
                 val initval, val filter_fun)
{
  initval = default_bool_arg(initval);

  if (vectorp(seq)) {
    cnum i, len;

    for (i = 0, len = c_num(length(seq)); i < len; i++) {
      val v = vecref(seq, num_fast(i));
      val key = funcall1(by_fun, v);
      val new_p;
      val cell = gethash_c(hash, key, mkcloc(new_p));

      if (new_p)
        rplacd(cell, funcall2(reduce_fun, initval, v));
      else
        rplacd(cell, funcall2(reduce_fun, cdr(cell), v));
    }
  } else {
    for (; seq; seq = cdr(seq)) {
      val v = car(seq);
      val key = funcall1(by_fun, v);
      val new_p;
      val cell = gethash_c(hash, key, mkcloc(new_p));

      if (new_p)
        rplacd(cell, funcall2(reduce_fun, initval, v));
      else
        rplacd(cell, funcall2(reduce_fun, cdr(cell), v));
    }
  }

  if (!null_or_missing_p(filter_fun)) {
    val iter = hash_begin(hash);
    val cell;

    while ((cell = hash_next(iter)) != nil)
      rplacd(cell, funcall1(filter_fun, cdr(cell)));
  }

  return hash;
}

static val hash_keys_lazy(val iter, val lcons)
{
  val cell = hash_next(iter);
  set(mkloc(lcons->lc.cdr, lcons), if2(cell, make_half_lazy_cons(lcons->lc.func, car(cell))));
  return nil;
}

val hash_keys(val hash)
{
  val iter = hash_begin(hash);
  val cell = hash_next(iter);
  if (!cell)
    return nil;
  return make_half_lazy_cons(func_f1(iter, hash_keys_lazy), car(cell));
}

static val hash_values_lazy(val iter, val lcons)
{
  val cell = hash_next(iter);
  set(mkloc(lcons->lc.cdr, lcons), if2(cell, make_half_lazy_cons(lcons->lc.func, cdr(cell))));
  return nil;
}

val hash_values(val hash)
{
  val iter = hash_begin(hash);
  val cell = hash_next(iter);
  if (!cell)
    return nil;
  return make_half_lazy_cons(func_f1(iter, hash_values_lazy), cdr(cell));
}

static val hash_pairs_lazy(val iter, val lcons)
{
  val cell = hash_next(iter);
  set(mkloc(lcons->lc.cdr, lcons), if2(cell, make_half_lazy_cons(lcons->lc.func,
                                                                 cons(car(cell),
                                                                      cons(cdr(cell),
                                                                           nil)))));
  return nil;
}

val hash_pairs(val hash)
{
  val iter = hash_begin(hash);
  val cell = hash_next(iter);
  if (!cell)
    return nil;
  return make_half_lazy_cons(func_f1(iter, hash_pairs_lazy),
                             cons(car(cell), cons(cdr(cell), nil)));
}

static val hash_alist_lazy(val iter, val lcons)
{
  val cell = hash_next(iter);
  set(mkloc(lcons->lc.cdr, lcons), if2(cell, make_half_lazy_cons(lcons->lc.func, cell)));
  return nil;
}

val hash_alist(val hash)
{
  val iter = hash_begin(hash);
  val cell = hash_next(iter);
  if (!cell)
    return nil;
  return make_half_lazy_cons(func_f1(iter, hash_alist_lazy), cell);
}

val hash_uni(val hash1, val hash2, val join_func)
{
  struct hash *h1 = coerce(struct hash *, cobj_handle(hash1, hash_s));
  struct hash *h2 = coerce(struct hash *, cobj_handle(hash2, hash_s));

  if (h1->hash_fun != h2->hash_fun)
    uw_throwf(error_s, lit("hash-uni: ~a and ~a are incompatible hashes"), hash1, hash2, nao);

  {
    val hout = make_similar_hash(hash1);
    val hiter, entry;

    for (hiter = hash_begin(hash2), entry = hash_next(hiter);
         entry;
         entry = hash_next(hiter))
    {
      sethash(hout, car(entry), cdr(entry));
    }

    for (hiter = hash_begin(hash1), entry = hash_next(hiter);
         entry;
         entry = hash_next(hiter))
    {
      if (missingp(join_func)) {
        sethash(hout, car(entry), cdr(entry));
      } else {
        loc ptr = gethash_l(hout, car(entry), nulloc);
        set(ptr, funcall2(join_func, cdr(entry), deref(ptr)));
      }
    }

    return hout;
  }
}

val hash_diff(val hash1, val hash2)
{
  struct hash *h1 = coerce(struct hash *, cobj_handle(hash1, hash_s));
  struct hash *h2 = coerce(struct hash *, cobj_handle(hash2, hash_s));

  if (h1->hash_fun != h2->hash_fun)
    uw_throwf(error_s, lit("hash-diff: ~a and ~a are incompatible hashes"), hash1, hash2, nao);

  {
    val hout = copy_hash(hash1);
    val hiter, entry;

    for (hiter = hash_begin(hash2), entry = hash_next(hiter);
         entry;
         entry = hash_next(hiter))
    {
      remhash(hout, car(entry));
    }

    return hout;
  }
}

val hash_isec(val hash1, val hash2, val join_func)
{
  struct hash *h1 = coerce(struct hash *, cobj_handle(hash1, hash_s));
  struct hash *h2 = coerce(struct hash *, cobj_handle(hash2, hash_s));

  if (h1->hash_fun != h2->hash_fun)
    uw_throwf(error_s, lit("hash-uni: ~a and ~a are incompatible hashes"), hash1, hash2, nao);

  {
    val hout = make_similar_hash(hash1);
    val hiter, entry;

    for (hiter = hash_begin(hash1), entry = hash_next(hiter);
         entry;
         entry = hash_next(hiter))
    {
      val found;
      val data2 = gethash_f(hash2, car(entry), mkcloc(found));
      if (found) {
        if (missingp(join_func))
          sethash(hout, car(entry), cdr(entry));
        else
          sethash(hout, car(entry), funcall2(join_func, cdr(entry), data2));
      }
    }

    return hout;
  }
}

val hash_subset(val hash1, val hash2)
{
  val hiter, entry;

  for (hiter = hash_begin(hash1), entry = hash_next(hiter);
       entry;
       entry = hash_next(hiter))
  {
    if (!inhash(hash2, car(entry), colon_k))
      return nil;
  }

  return t;
}

val hash_proper_subset(val hash1, val hash2)
{
  return and2(hash_subset(hash1, hash2),
              null(numeq(hash_count(hash1), hash_count(hash2))));
}

val hash_update(val hash, val fun)
{
  val iter = hash_begin(hash);
  val cell;
  while ((cell = hash_next(iter)) != nil) {
    loc ptr = cdr_l(cell);
    set(ptr, funcall1(fun, deref(ptr)));
  }
  return hash;
}

val hash_update_1(val hash, val key, val fun, val init)
{
  if (missingp(init)) {
    val cons;
    val data = gethash_f(hash, key, mkcloc(cons));
    if (cons)
      rplacd(cons, funcall1(fun, data));
    return data;
  } else {
    val new_p;
    loc place = gethash_l(hash, key, mkcloc(new_p));
    if (new_p)
      set(place, funcall1(fun, init));
    else
      set(place, funcall1(fun, deref(place)));
    return deref(place);
  }
}

val hash_revget(val hash, val value, val test, val keyfun)
{
  val iter = hash_begin(hash);
  val cell;

  test = default_arg(test, eql_f);
  keyfun = default_arg(keyfun, identity_f);

  while ((cell = hash_next(iter)) != nil) {
    if (funcall2(test, value, funcall1(keyfun, cdr(cell))))
      return car(cell);
  }

  return nil;
}

static val set_hash_str_limit(val lim)
{
  val old = num(hash_str_limit);
  hash_str_limit = c_num(lim);
  return old;
}

static val set_hash_rec_limit(val lim)
{
  val old = num(hash_rec_limit);
  hash_rec_limit = c_num(lim);
  return old;
}

void hash_init(void)
{
  weak_keys_k = intern(lit("weak-keys"), keyword_package);
  weak_vals_k = intern(lit("weak-vals"), keyword_package);
  equal_based_k = intern(lit("equal-based"), keyword_package);
  userdata_k = intern(lit("userdata"), keyword_package);
  val ghu = func_n1(get_hash_userdata);

  reg_fun(intern(lit("make-hash"), user_package), func_n3(make_hash));
  reg_fun(intern(lit("make-similar-hash"), user_package), func_n1(make_similar_hash));
  reg_fun(intern(lit("copy-hash"), user_package), func_n1(copy_hash));
  reg_fun(intern(lit("hash"), user_package), func_n0v(hashv));
  reg_fun(hash_construct_s, func_n2(hash_construct));
  reg_fun(intern(lit("hash-from-pairs"), user_package), func_n1v(hash_from_pairs_v));
  reg_fun(intern(lit("hash-list"), user_package), func_n1v(hash_list));
  reg_fun(intern(lit("gethash"), user_package), func_n3o(gethash_n, 2));
  reg_fun(intern(lit("inhash"), user_package), func_n3o(inhash, 2));
  reg_fun(intern(lit("sethash"), user_package), func_n3(sethash));
  reg_fun(intern(lit("pushhash"), user_package), func_n3(pushhash));
  reg_fun(intern(lit("remhash"), user_package), func_n2(remhash));
  reg_fun(intern(lit("clearhash"), user_package), func_n1(clearhash));
  reg_fun(intern(lit("hash-count"), user_package), func_n1(hash_count));
  reg_fun(intern(lit("get-hash-userdata"), user_package), ghu);
  reg_fun(intern(lit("hash-userdata"), user_package), ghu);
  reg_fun(intern(lit("set-hash-userdata"), user_package),
          func_n2(set_hash_userdata));
  reg_fun(intern(lit("hashp"), user_package), func_n1(hashp));
  reg_fun(intern(lit("maphash"), user_package), func_n2(maphash));
  reg_fun(intern(lit("hash-eql"), user_package), func_n1(hash_eql));
  reg_fun(intern(lit("hash-equal"), user_package), func_n1(hash_equal));
  reg_fun(intern(lit("hash-keys"), user_package), func_n1(hash_keys));
  reg_fun(intern(lit("hash-values"), user_package), func_n1(hash_values));
  reg_fun(intern(lit("hash-pairs"), user_package), func_n1(hash_pairs));
  reg_fun(intern(lit("hash-alist"), user_package), func_n1(hash_alist));
  reg_fun(intern(lit("hash-uni"), user_package), func_n3o(hash_uni, 2));
  reg_fun(intern(lit("hash-diff"), user_package), func_n2(hash_diff));
  reg_fun(intern(lit("hash-isec"), user_package), func_n3o(hash_isec, 2));
  reg_fun(intern(lit("hash-subset"), user_package), func_n2(hash_subset));
  reg_fun(intern(lit("hash-proper-subset"), user_package), func_n2(hash_proper_subset));
  reg_fun(intern(lit("group-by"), user_package), func_n2v(group_by));
  reg_fun(intern(lit("group-reduce"), user_package),
          func_n6o(group_reduce, 4));
  reg_fun(intern(lit("hash-update"), user_package), func_n2(hash_update));
  reg_fun(intern(lit("hash-update-1"), user_package),
          func_n4o(hash_update_1, 3));
  reg_fun(intern(lit("hash-revget"), user_package), func_n4o(hash_revget, 2));
  reg_fun(intern(lit("hash-begin"), user_package), func_n1(hash_begin));
  reg_fun(intern(lit("hash-next"), user_package), func_n1(hash_next));
  reg_fun(intern(lit("set-hash-str-limit"), system_package),
          func_n1(set_hash_str_limit));
  reg_fun(intern(lit("set-hash-rec-limit"), system_package),
          func_n1(set_hash_rec_limit));
}
