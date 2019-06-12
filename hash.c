/* Copyright 2009-2019
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
#include <stdarg.h>
#include <stdlib.h>
#include <limits.h>
#include <signal.h>
#include "config.h"
#include "alloca.h"
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#include "lib.h"
#include "gc.h"
#include "args.h"
#include "txr.h"
#include "signal.h"
#include "unwind.h"
#include "stream.h"
#include "eval.h"
#include "itypes.h"
#include "arith.h"
#include "hash.h"

typedef enum hash_flags {
  hash_weak_none = 0,
  hash_weak_keys = 1,
  hash_weak_vals = 2,
  hash_weak_both = 3
} hash_flags_t;

struct hash_ops {
  ucnum (*hash_fun)(val, int *, ucnum);
  val (*equal_fun)(val, val);
  val (*assoc_fun)(val key, cnum hash, val list);
  val (*acons_new_c_fun)(val key, cnum hash, loc new_p, loc list);
};

#define hash_ops_init(hash, equal, assoc, acons) \
  { hash, equal, assoc, acons }

struct hash {
  ucnum seed;
  hash_flags_t flags;
  struct hash *next;
  val table;
  cnum modulus;
  cnum count;
  val userdata;
  int usecount;
  struct hash_ops *hops;
};

struct hash_iter {
  struct hash_iter *next;
  val hash;
  cnum chain;
  val cons;
};

#define hash_seed (deref(lookup_var_l(nil, hash_seed_s)))

static_forward(struct hash_ops hash_eql_ops);
static_forward(struct hash_ops hash_equal_ops);

val weak_keys_k, weak_vals_k, equal_based_k, eql_based_k, userdata_k;
val hash_seed_s;

/*
 * Dynamic lists built up during gc.
 */
static struct hash *reachable_weak_hashes;
static struct hash_iter *reachable_iters;

static int hash_str_limit = INT_MAX, hash_rec_limit = 32;

static u32_t randbox[] = {
  0x49848f1bU, 0xe6255dbaU, 0x36da5bdcU, 0x47bf94e9U,
  0x8cbcce22U, 0x559fc06aU, 0xd268f536U, 0xe10af79aU,
  0xc1af4d69U, 0x1d2917b5U, 0xec4c304dU, 0x9ee5016cU,
  0x69232f74U, 0xfead7bb3U, 0xe9089ab6U, 0xf012f6aeU,
};

static u32_t hash_c_str(const wchar_t *str, u32_t seed)
{
  int count = hash_str_limit;
  u32_t acc = seed;
  u32_t ch;

  while (count-- && (ch = *str++) != 0) {
    acc ^= ch;
    acc ^= randbox[acc & 0xf];
    acc = acc >> 1 | acc << (32 - 1);
  }

  acc ^= randbox[acc & 0xf];
  acc = acc >> 1 | acc << (32 - 1);
  acc ^= randbox[acc & 0xf];
  acc = acc >> 1 | acc << (32 - 1);
  acc ^= randbox[acc & 0xf];
  acc = acc >> 1 | acc << (32 - 1);
  acc ^= randbox[acc & 0xf];

  return acc;
}

static u32_t hash_buf(const mem_t *ptr, ucnum size, u32_t seed)
{
  const u32_t *buf = coerce(const u32_t *, ptr);
  int count = hash_str_limit;
  u32_t acc = seed;

  for (; size >= sizeof *buf && count--; size -= sizeof *buf, buf++) {
    u32_t in = *buf;
    acc ^= in;
    acc ^= randbox[acc & 0xf];
    acc = acc >> 1 | acc << (32 - 1);
  }

  if (size != 0) {
    const mem_t *tail = coerce(const mem_t *, ptr);
    u32_t in = 0;

    switch (size) {
    case 3:
      in |= convert(u32_t, tail[2]) << 16;
    case 2:
      in |= convert(u32_t, tail[1]) << 8;
    case 1:
      in |= convert(u32_t, tail[0]);
    }

    acc ^= in;
    acc ^= randbox[acc & 0xf];
    acc = acc >> 1 | acc << (32 - 1);
  }

  acc ^= randbox[acc & 0xf];
  acc = acc >> 1 | acc << (32 - 1);
  acc ^= randbox[acc & 0xf];
  acc = acc >> 1 | acc << (32 - 1);
  acc ^= randbox[acc & 0xf];
  acc = acc >> 1 | acc << (32 - 1);
  acc ^= randbox[acc & 0xf];

  return acc;
}

static ucnum hash_double(double n)
{
  union hack {
    volatile double d;
    volatile ucnum a[sizeof (double) / sizeof (ucnum)];
  } u;
  ucnum h = 0;
  unsigned i;

  u.d = n;

  for (i = 0; i < sizeof u.a / sizeof u.a[0]; i++)
    h += u.a[i];

  return h;
}

ucnum equal_hash(val obj, int *count, ucnum seed)
{
  if ((*count)-- <= 0)
    return 0;

  switch (type(obj)) {
  case NIL:
    return convert(ucnum, -1);
  case LIT:
    return hash_c_str(litptr(obj), seed);
  case CONS:
    return equal_hash(obj->c.car, count, seed)
            + 2 * equal_hash(obj->c.cdr, count, seed);
  case STR:
    return hash_c_str(obj->st.str, seed);
  case CHR:
    return c_chr(obj);
  case NUM:
    return c_num(obj);
  case SYM:
  case PKG:
  case ENV:
    switch (CHAR_BIT * sizeof (mem_t *)) {
    case 32:
      return coerce(ucnum, obj) >> 4;
    case 64: default:
      return coerce(ucnum, obj) >> 5;
    }
    break;
  case FUN:
    return coerce(ucnum, obj->f.f.interp_fun)
            + equal_hash(obj->f.env, count, seed);
  case VEC:
    {
      val length = obj->v.vec[vec_length];
      ucnum h = equal_hash(obj->v.vec[vec_length], count, seed);
      cnum i, len = c_num(length);

      for (i = 0; i < len; i++) {
        h *= 2;
        h += equal_hash(obj->v.vec[i], count, seed);
      }

      return h;
    }
  case LCONS:
    return equal_hash(car(obj), count, seed)
            + 2 * equal_hash(cdr(obj), count, seed);
  case LSTR:
    lazy_str_force_upto(obj, num(hash_str_limit - 1));
    return equal_hash(obj->ls.prefix, count, seed);
  case BGNUM:
    return mp_hash(mp(obj));
  case FLNUM:
    return hash_double(obj->fl.n);
  case COBJ:
  case CPTR:
    if (obj->co.ops->equalsub) {
      val sub = obj->co.ops->equalsub(obj);
      if (sub)
        return equal_hash(sub, count, seed);
    }
    return obj->co.ops->hash(obj, count, seed);
  case RNG:
    return equal_hash(obj->rn.from, count, seed)
            + 2 * equal_hash(obj->rn.to, count, seed);
  case BUF:
    return hash_buf(obj->b.data, c_unum(obj->b.len), seed);
  }

  internal_error("unhandled case in equal function");
}

static ucnum eql_hash(val obj, int *count)
{
  switch (tag(obj)) {
  case TAG_PTR:
    switch (type(obj)) {
    case NIL:
      return convert(ucnum, -1);
    case BGNUM:
      return mp_hash(mp(obj));
    case FLNUM:
      return hash_double(obj->fl.n);
    case RNG:
      return eql_hash(obj->rn.from, count) + 2 * eql_hash(obj->rn.to, count);
    default:
      switch (CHAR_BIT * sizeof (mem_t *)) {
      case 32:
        return coerce(ucnum, obj) >> 4;
      case 64: default:
        return coerce(ucnum, obj) >> 5;
      }
    }
  case TAG_CHR:
    return c_chr(obj);
  case TAG_NUM:
    return c_num(obj);
  case TAG_LIT:
    switch (CHAR_BIT * sizeof (mem_t *)) {
    case 32:
      return coerce(ucnum, obj) >> 2;
    case 64: default:
      return coerce(ucnum, obj) >> 3;
    }
  }
  /* notreached */
  abort();
}

static ucnum eql_hash_op(val obj, int *count, ucnum seed)
{
  (void) seed;
  return eql_hash(obj, count);
}

ucnum cobj_eq_hash_op(val obj, int *count, ucnum seed)
{
  (void) count;
  (void) seed;

  switch (CHAR_BIT * sizeof (mem_t *)) {
  case 32:
    return coerce(ucnum, obj) >> 4;
  case 64: default:
    return coerce(ucnum, obj) >> 5;
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

  if (l->hops != r->hops)
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

    if (l->hops->equal_fun(us_car(lcell), us_car(rcell)) &&
        equal(us_cdr(lcell), us_cdr(rcell)))
      continue;

    /*
     * Try to find a cell matching the left cell on the pending list by key.
     * If it is found, and the associated datum is equal, then remove it from
     * the list.  If it is found and the data is not equal, then we have found
     * a difference between the hash tables, and conclude they are different.
     * If there is no match, then we insert the cell into the pending list.
     */
    found = l->hops->assoc_fun(us_car(lcell), lcell->ch.hash, pending);

    if (found && !equal(us_cdr(found), us_cdr(lcell))) {
      return nil;
    } else if (found) {
      val loc = memq(found, pending);
      pending = nappend2(ldiff(pending, loc), us_cdr(loc));
      us_rplacd(loc, free_conses);
      free_conses = loc;
    } else {
      ncons = or2(pop(&free_conses), cons(nil, nil));
      us_rplaca(ncons, lcell);
      us_rplacd(ncons, pending);
      pending = ncons;
    }

    /*
     * The logic is mirrored for the right cell.
     */
    found = l->hops->assoc_fun(us_car(rcell), rcell->ch.hash, pending);

    if (found && !equal(us_cdr(found), us_cdr(rcell))) {
      return nil;
    } else if (found) {
      val loc = memq(found, pending);
      pending = nappend2(ldiff(pending, loc), us_cdr(loc));
      us_rplacd(loc, free_conses);
      free_conses = loc;
    } else {
      ncons = or2(pop(&free_conses), cons(nil, nil));
      us_rplaca(ncons, rcell);
      us_rplacd(ncons, pending);
      pending = ncons;
    }
  }

  /*
   * The hashes are equal if and only if the pending list
   * balances down to zero.
   */
  return eq(pending, nil);
}

static ucnum hash_hash_op(val obj, int *count, ucnum seed)
{
  ucnum out = 0;
  struct hash *h = coerce(struct hash *, obj->co.handle);
  val iter, cell;

  if ((*count)-- <= 0)
    return 0;

  switch (CHAR_BIT * sizeof (mem_t *)) {
  case 32:
    out += coerce(ucnum, h->hops) >> 4;
  case 64: default:
    out += coerce(ucnum, h->hops) >> 5;
  }

  out += equal_hash(h->userdata, count, seed);
  out &= NUM_MAX;

  iter = hash_begin(obj);

  while ((cell = hash_next(iter)) != nil) {
    out += equal_hash(cell, count, seed);
    out &= NUM_MAX;
  }

  return out;
}

static void hash_print_op(val hash, val out, val pretty, struct strm_ctx *ctx)
{
  struct hash *h = coerce(struct hash *, hash->co.handle);
  int need_space = 0, force_br = 0;
  val save_mode = test_set_indent_mode(out, num_fast(indent_off),
                                       num_fast(indent_data));
  val save_indent;

  put_string(lit("#H("), out);

  save_indent = inc_indent(out, zero);

  put_char(chr('('), out);

  if (opt_compat && opt_compat <= 188) {
    if (h->hops == &hash_equal_ops)
      obj_print_impl(equal_based_k, out, pretty, ctx);
    need_space = 1;
  } else {
    if (h->hops == &hash_eql_ops)
      obj_print_impl(eql_based_k, out, pretty, ctx);
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
    cnum max_len = ctx->strm->max_length;
    cnum max_count = max_len;

    while ((cell = hash_next(iter))) {
      val key = us_car(cell);
      val value = us_cdr(cell);
      if (width_check(out, chr(' ')))
        force_br = 1;

      if (max_len && --max_count < 0) {
        put_string(lit("..."), out);
        break;
      }

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

  if (force_br)
    force_break(out);

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

      for (iter = chain; iter != nil; iter = us_cdr(iter)) {
        val entry = us_car(iter);
        gc_mark(us_cdr(entry));
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

      for (iter = chain; iter != nil; iter = us_cdr(iter)) {
        val entry = us_car(iter);
        gc_mark(us_car(entry));
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
  val new_table;

  if (new_modulus > NUM_MAX)
    return;

  new_table = vector(num_fast(new_modulus), nil);

  for (i = 0; i < h->modulus; i++) {
    val conses = vecref(h->table, num_fast(i));

    while (conses) {
      val entry = us_car(conses);
      val next = us_cdr(conses);
      loc pchain = vecref_l(new_table,
                            num_fast(entry->ch.hash % new_modulus));
      us_rplacd(conses, deref(pchain));
      set(pchain, conses);
      conses = next;
    }
  }

  h->modulus = new_modulus;
  h->table = new_table;
  setcheck(hash, new_table);
}

static val hash_assoc(val key, cnum hash, val list)
{
  while (list) {
    val elem = us_car(list);
    if (elem->ch.hash == hash && equal(us_car(elem), key))
      return elem;
    list = us_cdr(list);
  }

  return nil;
}

static val hash_assql(val key, cnum hash, val list)
{
  while (list) {
    val elem = us_car(list);
    if (elem->ch.hash == hash && eql(us_car(elem), key))
      return elem;
    list = us_cdr(list);
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

static_def(struct hash_ops hash_eql_ops = hash_ops_init(eql_hash_op, eql,
                                                        hash_assql,
                                                        hash_aconsql_new_c));

static_def(struct hash_ops hash_equal_ops = hash_ops_init(equal_hash, equal,
                                                          hash_assoc,
                                                          hash_acons_new_c));

val make_seeded_hash(val weak_keys, val weak_vals, val equal_based, val seed)
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

    h->seed = convert(u32_t, c_unum(default_arg(seed,
                                                if3(hash_seed_s,
                                                    hash_seed, zero))));
    h->flags = convert(hash_flags_t, flags);
    h->modulus = c_num(mod);
    h->count = 0;
    h->table = table;
    h->userdata = nil;

    h->usecount = 0;
    h->hops = equal_based ? &hash_equal_ops : &hash_eql_ops;

    return hash;
  }
}

val make_hash(val weak_keys, val weak_vals, val equal_based)
{
  return make_seeded_hash(weak_keys, weak_vals, equal_based, nil);
}

val make_similar_hash(val existing)
{
  val self = lit("make-similar-hash");
  struct hash *ex = coerce(struct hash *, cobj_handle(self, existing, hash_s));
  struct hash *h = coerce(struct hash *, chk_malloc(sizeof *h));
  val mod = num_fast(256);
  val table = vector(mod, nil);
  val hash = cobj(coerce(mem_t *, h), hash_s, &hash_ops);

  h->modulus = c_num(mod);
  h->count = 0;
  h->table = table;
  h->userdata = ex->userdata;

  h->seed = ex->seed;
  h->flags = ex->flags;
  h->usecount = 0;
  h->hops = ex->hops;

  return hash;
}

static val copy_hash_chain(val chain)
{
  list_collect_decl(out, ptail);

  for (; chain; chain = us_cdr(chain)) {
    val entry = us_car(chain);
    val nentry = cons(us_car(entry), us_cdr(entry));
    nentry->ch.hash = entry->ch.hash;
    ptail = list_collect(ptail, nentry);
  }

  return out;
}

val copy_hash(val existing)
{
  val self = lit("copy-hash");
  struct hash *ex = coerce(struct hash *, cobj_handle(self, existing, hash_s));
  struct hash *h = coerce(struct hash *, chk_malloc(sizeof *h));
  val mod = num_fast(ex->modulus);
  val table = vector(mod, nil);
  val hash = cobj(coerce(mem_t *, h), hash_s, &hash_ops);
  val iter;

  h->modulus = ex->modulus;
  h->count = ex->count;
  h->table = table;
  h->userdata = ex->userdata;

  h->seed = ex->seed;
  h->flags = ex->flags;
  h->usecount = 0;
  h->hops = ex->hops;

  for (iter = zero; lt(iter, mod); iter = plus(iter, one))
    set(vecref_l(h->table, iter), copy_hash_chain(vecref(ex->table, iter)));

  return hash;
}

val gethash_c(val self, val hash, val key, loc new_p)
{
  struct hash *h = coerce(struct hash *, cobj_handle(self, hash, hash_s));
  int lim = hash_rec_limit;
  cnum hv = h->hops->hash_fun(key, &lim, h->seed);
  loc pchain = vecref_l(h->table, num_fast(hv % h->modulus));
  val old = deref(pchain);
  val cell = h->hops->acons_new_c_fun(key, hv, new_p, pchain);
  if (old != deref(pchain) && ++h->count > 2 * h->modulus && h->usecount == 0)
    hash_grow(h, hash);
  return cell;
}

val gethash_e(val self, val hash, val key)
{
  struct hash *h = coerce(struct hash *, cobj_handle(self, hash, hash_s));
  int lim = hash_rec_limit;
  cnum hv = h->hops->hash_fun(key, &lim, h->seed);
  val chain = vecref(h->table, num_fast(hv % h->modulus));
  return h->hops->assoc_fun(key, hv, chain);
}

val gethash(val hash, val key)
{
  val self = lit("gethash");
  val found = gethash_e(self, hash, key);
  return if2(found, us_cdr(found));
}

val inhash(val hash, val key, val init)
{
  val self = lit("inhash");

  if (missingp(init)) {
    return gethash_e(self, hash, key);
  } else {
    val new_p;
    val cell = gethash_c(self, hash, key, mkcloc(new_p));
    if (new_p)
      us_rplacd(cell, init);
    return cell;
  }
}

val gethash_n(val hash, val key, val notfound_val)
{
  val self = lit("gethash-n");
  val existing = gethash_e(self, hash, key);
  return if3(existing, us_cdr(existing), default_null_arg(notfound_val));
}

val sethash(val hash, val key, val value)
{
  val self = lit("sethash");
  us_rplacd(gethash_c(self, hash, key, nulloc), value);
  return value;
}

val pushhash(val hash, val key, val value)
{
  val self = lit("pushhash");
  val new_p;
  mpush(value, gethash_l(self, hash, key, mkcloc(new_p)));
  return new_p;
}

val remhash(val hash, val key)
{
  val self = lit("remhash");
  struct hash *h = coerce(struct hash *, cobj_handle(self, hash, hash_s));
  int lim = hash_rec_limit;
  cnum hv = h->hops->hash_fun(key, &lim, h->seed);
  val *pchain = valptr(vecref_l(h->table, num_fast(hv % h->modulus)));
  val existing = h->hops->assoc_fun(key, hv, *pchain);

  if (existing) {
    for (; *pchain; pchain = us_cdr_p(*pchain)) {
      if (us_car(*pchain) == existing) {
        *pchain = us_cdr(*pchain);
        break;
      }
    }
    h->count--;
    bug_unless (h->count >= 0);
    return us_cdr(existing);
  }

  return nil;
}

val clearhash(val hash)
{
  val self = lit("clearhash");
  struct hash *h = coerce(struct hash *, cobj_handle(self, hash, hash_s));
  val mod = num_fast(256);
  val table = vector(mod, nil);
  cnum oldcount = h->count;
  h->modulus = c_num(mod);
  h->count = 0;
  h->table = table;
  setcheck(hash, table);
  return oldcount ? num(oldcount) : nil;
}

val hash_count(val hash)
{
  val self = lit("hash-count");
  struct hash *h = coerce(struct hash *, cobj_handle(self, hash, hash_s));
  return num_fast(h->count);
}

val us_hash_count(val hash)
{
  struct hash *h = coerce(struct hash *, hash->co.handle);
  return num_fast(h->count);
}

val get_hash_userdata(val hash)
{
  val self = lit("get-hash-userdata");
  struct hash *h = coerce(struct hash *, cobj_handle(self, hash, hash_s));
  return h->userdata;
}

val set_hash_userdata(val hash, val data)
{
  val self = lit("set-hash-userdata");
  struct hash *h = coerce(struct hash *, cobj_handle(self, hash, hash_s));
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
                                                     cobj_eq_hash_op);

val hash_begin(val hash)
{
  val self = lit("hash-begin");
  val hi_obj;
  struct hash *h = coerce(struct hash *, cobj_handle(self, hash, hash_s));
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
  val self = lit("hash-next");
  struct hash_iter *hi = coerce(struct hash_iter *,
                                cobj_handle(self, iter, hash_iter_s));
  val hash = hi->hash;
  struct hash *h = hash ? coerce(struct hash *, hash->co.handle) : 0;

  if (!h)
    return nil;
  if (hi->cons)
    hi->cons = us_cdr(hi->cons);
  while (nilp(hi->cons)) {
    if (++hi->chain >= h->modulus) {
      hi->hash = nil;
      h->usecount--;
      return nil;
    }
    set(mkloc(hi->cons, iter), vecref(h->table, num_fast(hi->chain)));
  }
  return us_car(hi->cons);
}

val hash_peek(val iter)
{
  val self = lit("hash-peek");
  struct hash_iter *hi = coerce(struct hash_iter *,
                                cobj_handle(self, iter, hash_iter_s));
  val hash = hi->hash;
  struct hash *h = hash ? coerce(struct hash *, hash->co.handle) : 0;
  cnum chain = hi->chain;
  val cell = hi->cons;

  if (!h)
    return nil;
  if (cell) {
    val peek = us_cdr(cell);
    if (peek)
      return us_car(peek);
  }
  do {
    if (++chain >= h->modulus)
      return nil;
    cell = vecref(h->table, num_fast(chain));
  } while (!cell);
  return us_car(cell);
}

val maphash(val fun, val hash)
{
  val iter = hash_begin(hash);
  val cell;
  while ((cell = hash_next(iter)) != nil)
    funcall2(fun, us_car(cell), us_cdr(cell));
  return nil;
}

val hash_eql(val obj)
{
  int lim = 0;
  return num_fast(eql_hash(obj, &lim));
}

val hash_equal(val obj, val seed)
{
  int lim = hash_rec_limit;
  return num_fast(equal_hash(obj, &lim, if3(missingp(seed), 0, c_unum(seed))));
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
          val entry = us_car(*iter);
          if (!gc_is_reachable(entry) && !gc_is_reachable(us_car(entry))) {
            *iter = us_cdr(*iter);
#if CONFIG_EXTRA_DEBUGGING
            if (us_car(entry) == break_obj)
              breakpt();
#endif
          } else {
            iter = us_cdr_p(*iter);
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
          val entry = us_car(*iter);
          if (!gc_is_reachable(entry) && !gc_is_reachable(us_cdr(entry))) {
            *iter = us_cdr(*iter);
#if CONFIG_EXTRA_DEBUGGING
            if (us_cdr(entry) == break_obj)
              breakpt();
#endif
          } else {
            iter = us_cdr_p(*iter);
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
          val entry = us_car(*iter);
          if (!gc_is_reachable(entry) &&
              (!gc_is_reachable(us_car(entry)) || !gc_is_reachable(us_cdr(entry))))
          {
            *iter = us_cdr(*iter);
#if CONFIG_EXTRA_DEBUGGING
            if (!gc_is_reachable(us_car(entry)) && us_car(entry) == break_obj)
              breakpt();
            if (!gc_is_reachable(us_cdr(entry)) && us_cdr(entry) == break_obj)
              breakpt();
#endif
          } else {
            iter = us_cdr_p(*iter);
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

static val equal_based_p(val equal, val eql, val wkeys)
{
  if (opt_compat && opt_compat <= 187)
    return equal;

  if (equal && eql)
    uw_throwf(error_s,
              lit("make-hash: mutually exclusive :equal-based and :eql-based"),
              nao);

  if (wkeys) {
    if (equal)
      uw_throwf(error_s,
                lit("make-hash: mutually exclusive :equal-based and :weak-keys"),
                nao);
    else
      eql = t;
  }

  return null(eql);
}

val hashv(struct args *args)
{
  val wkeys = nil, wvals = nil, equal = nil, eql = nil, userdata = nil;
  struct args_bool_key akv[] = {
    { weak_keys_k, nil, &wkeys },
    { weak_vals_k, nil, &wvals },
    { equal_based_k, nil, &equal },
    { eql_based_k, nil, &eql },
    { userdata_k, t, &userdata }
  };
  val hash = (args_keys_extract(args, akv, sizeof akv / sizeof akv[0]),
              make_hash(wkeys, wvals, equal_based_p(equal, eql, wkeys)));
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

val hash_from_alist_v(val alist, struct args *hashv_args)
{
  val hash = hashv(hashv_args);

  alist = nullify(alist);

  for (; alist; alist = cdr(alist)) {
    val pair = car(alist);
    sethash(hash, car(pair), cdr(pair));
  }

  return hash;
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
  val self = lit("group-by");
  val hash = hashv(hashv_args);

  if (vectorp(seq)) {
    cnum i, len;

    for (i = 0, len = c_fixnum(length(seq), self); i < len; i++) {
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
      us_rplacd(cell, nreverse(us_cdr(cell)));

    return hash;
  }
}

val group_reduce(val hash, val by_fun, val reduce_fun, val seq,
                 val initval, val filter_fun)
{
  val self = lit("group-reduce");
  initval = default_null_arg(initval);

  if (vectorp(seq)) {
    cnum i, len;

    for (i = 0, len = c_fixnum(length(seq), self); i < len; i++) {
      val v = vecref(seq, num_fast(i));
      val key = funcall1(by_fun, v);
      val new_p;
      loc pcdr = gethash_l(self, hash, key, mkcloc(new_p));

      if (new_p)
        set(pcdr, funcall2(reduce_fun, initval, v));
      else
        set(pcdr, funcall2(reduce_fun, deref(pcdr), v));
    }
  } else {
    for (; seq; seq = cdr(seq)) {
      val v = car(seq);
      val key = funcall1(by_fun, v);
      val new_p;
      loc pcdr = gethash_l(self, hash, key, mkcloc(new_p));

      if (new_p)
        set(pcdr, funcall2(reduce_fun, initval, v));
      else
        set(pcdr, funcall2(reduce_fun, deref(pcdr), v));
    }
  }

  if (!null_or_missing_p(filter_fun)) {
    val iter = hash_begin(hash);
    val cell;

    while ((cell = hash_next(iter)) != nil)
      us_rplacd(cell, funcall1(filter_fun, us_cdr(cell)));
  }

  return hash;
}

static val hash_keys_lazy(val iter, val lcons)
{
  val cell = hash_next(iter);
  us_rplacd(lcons, if2(cell, make_lazy_cons_car(us_lcons_fun(lcons), us_car(cell))));
  return nil;
}

val hash_keys(val hash)
{
  val iter = hash_begin(hash);
  val cell = hash_next(iter);
  if (!cell)
    return nil;
  return make_lazy_cons_car(func_f1(iter, hash_keys_lazy), us_car(cell));
}

static val hash_values_lazy(val iter, val lcons)
{
  val cell = hash_next(iter);
  us_rplacd(lcons, if2(cell, make_lazy_cons_car(us_lcons_fun(lcons), us_cdr(cell))));
  return nil;
}

val hash_values(val hash)
{
  val iter = hash_begin(hash);
  val cell = hash_next(iter);
  if (!cell)
    return nil;
  return make_lazy_cons_car(func_f1(iter, hash_values_lazy), us_cdr(cell));
}

static val hash_pairs_lazy(val iter, val lcons)
{
  val cell = hash_next(iter);
  us_rplacd(lcons, if2(cell,
                       make_lazy_cons_car(us_lcons_fun(lcons),
                                          cons(us_car(cell),
                                               cons(us_cdr(cell),
                                                    nil)))));
  return nil;
}

val hash_pairs(val hash)
{
  val iter = hash_begin(hash);
  val cell = hash_next(iter);
  if (!cell)
    return nil;
  return make_lazy_cons_car(func_f1(iter, hash_pairs_lazy),
                            cons(us_car(cell), cons(us_cdr(cell), nil)));
}

static val hash_alist_lazy(val iter, val lcons)
{
  val cell = hash_next(iter);
  us_rplacd(lcons, if2(cell, make_lazy_cons_car(us_lcons_fun(lcons), cell)));
  return nil;
}

val hash_alist(val hash)
{
  val iter = hash_begin(hash);
  val cell = hash_next(iter);
  if (!cell)
    return nil;
  return make_lazy_cons_car(func_f1(iter, hash_alist_lazy), cell);
}

val hash_uni(val hash1, val hash2, val join_func)
{
  val self = lit("hash-uni");
  struct hash *h1 = coerce(struct hash *, cobj_handle(self, hash1, hash_s));
  struct hash *h2 = coerce(struct hash *, cobj_handle(self, hash2, hash_s));

  if (h1->hops != h2->hops)
    uw_throwf(error_s, lit("~a: ~s and ~s are incompatible hashes"),
              self, hash1, hash2, nao);

  {
    val hout = make_similar_hash(hash1);
    val hiter, entry;

    for (hiter = hash_begin(hash2), entry = hash_next(hiter);
         entry;
         entry = hash_next(hiter))
    {
      sethash(hout, us_car(entry), us_cdr(entry));
    }

    for (hiter = hash_begin(hash1), entry = hash_next(hiter);
         entry;
         entry = hash_next(hiter))
    {
      if (missingp(join_func)) {
        sethash(hout, us_car(entry), us_cdr(entry));
      } else {
        val new_p;
        loc ptr = gethash_l(self, hout, us_car(entry), mkcloc(new_p));
        if (new_p)
          sethash(hout, us_car(entry), us_cdr(entry));
        else
          set(ptr, funcall2(join_func, us_cdr(entry), deref(ptr)));
      }
    }

    return hout;
  }
}

val hash_diff(val hash1, val hash2)
{
  val self = lit("hash-diff");
  struct hash *h1 = coerce(struct hash *, cobj_handle(self, hash1, hash_s));
  struct hash *h2 = coerce(struct hash *, cobj_handle(self, hash2, hash_s));

  if (h1->hops != h2->hops)
    uw_throwf(error_s, lit("~a: ~s and ~a are incompatible hashes"),
              self, hash1, hash2, nao);

  {
    val hout = copy_hash(hash1);
    val hiter, entry;

    for (hiter = hash_begin(hash2), entry = hash_next(hiter);
         entry;
         entry = hash_next(hiter))
    {
      remhash(hout, us_car(entry));
    }

    return hout;
  }
}

val hash_symdiff(val hash1, val hash2)
{
  val self = lit("hash-symdiff");
  struct hash *h1 = coerce(struct hash *, cobj_handle(self, hash1, hash_s));
  struct hash *h2 = coerce(struct hash *, cobj_handle(self, hash2, hash_s));

  if (h1->hops != h2->hops)
    uw_throwf(error_s, lit("~a: ~s and ~a are incompatible hashes"),
              self, hash1, hash2, nao);

  {
    val hout = make_similar_hash(hash1);
    val hiter, entry;

    for (hiter = hash_begin(hash1), entry = hash_next(hiter);
         entry;
         entry = hash_next(hiter))
    {
      if (!gethash_e(self, hash2, us_car(entry)))
        sethash(hout, us_car(entry), us_cdr(entry));
    }

    for (hiter = hash_begin(hash2), entry = hash_next(hiter);
         entry;
         entry = hash_next(hiter))
    {
      if (!gethash_e(self, hash1, us_car(entry)))
        sethash(hout, us_car(entry), us_cdr(entry));
    }

    return hout;
  }
}

val hash_isec(val hash1, val hash2, val join_func)
{
  val self = lit("hash-isec");
  struct hash *h1 = coerce(struct hash *, cobj_handle(self, hash1, hash_s));
  struct hash *h2 = coerce(struct hash *, cobj_handle(self, hash2, hash_s));

  if (h1->hops != h2->hops)
    uw_throwf(error_s, lit("~a: ~s and ~s are incompatible hashes"),
              self, hash1, hash2, nao);

  {
    val hout = make_similar_hash(hash1);
    val hiter, entry;

    for (hiter = hash_begin(hash1), entry = hash_next(hiter);
         entry;
         entry = hash_next(hiter))
    {
      val found = gethash_e(self, hash2, us_car(entry));
      if (found) {
        if (missingp(join_func))
          sethash(hout, us_car(entry), us_cdr(entry));
        else
          sethash(hout, us_car(entry), funcall2(join_func, us_cdr(entry), us_cdr(found)));
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
    if (!inhash(hash2, us_car(entry), colon_k))
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
    loc ptr = mkloc(*us_cdr_p(cell), cell);
    set(ptr, funcall1(fun, deref(ptr)));
  }
  return hash;
}

val hash_update_1(val hash, val key, val fun, val init)
{
  val self = lit("hash-update-1");

  if (missingp(init)) {
    val cons = gethash_e(self, hash, key);
    if (cons) {
      val data = us_cdr(cons);
      us_rplacd(cons, funcall1(fun, data));
      return data;
    }
    return nil;
  } else {
    val new_p;
    loc place = gethash_l(self, hash, key, mkcloc(new_p));
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
    if (funcall2(test, value, funcall1(keyfun, us_cdr(cell))))
      return us_car(cell);
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

static val gen_hash_seed(void)
{
    val time = time_sec_usec();
    ucnum sec = convert(ucnum, c_num(car(time)));
    ucnum usec = convert(ucnum, c_num(cdr(time)));
#if HAVE_UNISTD_H
    ucnum pid = convert(ucnum, getpid());
#else
    ucnum pid = 0;
#endif
    return unum(sec ^ (usec << 12) ^ pid);
}

void hash_init(void)
{
  weak_keys_k = intern(lit("weak-keys"), keyword_package);
  weak_vals_k = intern(lit("weak-vals"), keyword_package);
  equal_based_k = intern(lit("equal-based"), keyword_package);
  eql_based_k = intern(lit("eql-based"), keyword_package);
  userdata_k = intern(lit("userdata"), keyword_package);
  hash_seed_s = intern(lit("*hash-seed*"), user_package);
  val ghu = func_n1(get_hash_userdata);

  reg_var(hash_seed_s, zero);

  reg_fun(intern(lit("make-hash"), user_package), func_n4o(make_seeded_hash, 3));
  reg_fun(intern(lit("make-similar-hash"), user_package), func_n1(make_similar_hash));
  reg_fun(intern(lit("copy-hash"), user_package), func_n1(copy_hash));
  reg_fun(intern(lit("hash"), user_package), func_n0v(hashv));
  reg_fun(hash_construct_s, func_n2(hash_construct));
  reg_fun(intern(lit("hash-from-pairs"), user_package), func_n1v(hash_from_pairs_v));
  reg_fun(intern(lit("hash-from-alist"), user_package), func_n1v(hash_from_alist_v));
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
  reg_fun(intern(lit("hash-equal"), user_package), func_n2o(hash_equal, 1));
  reg_fun(intern(lit("hash-keys"), user_package), func_n1(hash_keys));
  reg_fun(intern(lit("hash-values"), user_package), func_n1(hash_values));
  reg_fun(intern(lit("hash-pairs"), user_package), func_n1(hash_pairs));
  reg_fun(intern(lit("hash-alist"), user_package), func_n1(hash_alist));
  reg_fun(intern(lit("hash-uni"), user_package), func_n3o(hash_uni, 2));
  reg_fun(intern(lit("hash-diff"), user_package), func_n2(hash_diff));
  reg_fun(intern(lit("hash-symdiff"), user_package), func_n2(hash_symdiff));
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
  reg_fun(intern(lit("hash-peek"), user_package), func_n1(hash_peek));
  reg_fun(intern(lit("set-hash-str-limit"), system_package),
          func_n1(set_hash_str_limit));
  reg_fun(intern(lit("set-hash-rec-limit"), system_package),
          func_n1(set_hash_rec_limit));
  reg_fun(intern(lit("gen-hash-seed"), user_package), func_n0(gen_hash_seed));
}
