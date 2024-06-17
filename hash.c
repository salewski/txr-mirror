/* Copyright 2009-2024
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
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
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
#include "sysif.h"
#include "time.h"
#include "hash.h"

typedef enum hash_type {
  hash_type_eq,
  hash_type_eql,
  hash_type_equal
} hash_type_t;

struct hash_ops {
  ucnum (*hash_fun)(val, int *, ucnum);
  val (*equal_fun)(val, val);
  val (*assoc_fun)(val key, ucnum hash, val list);
};

#define hash_ops_init(hash, equal, assoc) \
  { hash, equal, assoc }

struct hash {
  ucnum seed;
  hash_weak_opt_t wkopt;
  struct hash *next;
  val table;
  ucnum mask;
  ucnum count;
  val userdata;
  int usecount;
  val tblstack;
  struct hash_ops *hops;
};

#define hash_seed (deref(lookup_var_l(nil, hash_seed_s)))

static_forward(struct hash_ops hash_eq_ops);
static_forward(struct hash_ops hash_eql_ops);
static_forward(struct hash_ops hash_equal_ops);

val weak_keys_k, weak_vals_k, weak_and_k, weak_or_k, userdata_k;
val equal_based_k, eql_based_k, eq_based_k;
val hash_seed_s;

struct cobj_class *hash_cls, *hash_iter_cls;
/*
 * Dynamic lists built up during gc.
 */
static struct hash *reachable_weak_hashes;
static struct hash_iter *reachable_iters;

static int hash_traversal_limit = 32;

#if SIZEOF_PTR == 8

static u64_t randbox[] = {
  0x41f00c9949848f1bU, 0x16f1d887e6255dbaU,
  0x6921f21236da5bdcU, 0x1878975147bf94e9U,
  0x97b6024b8cbcce22U, 0x9a803523559fc06aU,
  0x45d41914d268f536U, 0x40b0bc43e10af79aU,
  0x90e011a9c1af4d69U, 0x6ac090fe1d2917b5U,
  0x142a6ebeec4c304dU, 0x08fb26fc9ee5016cU,
  0x3f04e1d969232f74U, 0x6177e1befead7bb3U,
  0x778108b4e9089ab6U, 0x6a6e2ab4f012f6aeU
};

static u64_t hash_c_str(const wchar_t *str, u64_t seed, int *pcount)
{
  int count = *pcount << 2;
  u64_t acc = seed;
  u64_t ch;

  while (count-- && (ch = *str++) != 0) {
    acc ^= ch;
    acc ^= randbox[acc & 0xf];
    acc = acc >> 1 | acc << (64 - 1);
  }

  acc ^= randbox[acc & 0xf];
  acc = acc >> 1 | acc << (64 - 1);
  acc ^= randbox[acc & 0xf];
  acc = acc >> 1 | acc << (64 - 1);
  acc ^= randbox[acc & 0xf];
  acc = acc >> 1 | acc << (64 - 1);
  acc ^= randbox[acc & 0xf];

  *pcount = count >> 2;

  return acc;
}

static u64_t hash_buf(const mem_t *ptr, ucnum size, u64_t seed, int *pcount)
{
  const u64_t *buf = coerce(const u64_t *, ptr);
  int count = *pcount << 2;
  u64_t acc = seed;

  for (; size >= sizeof *buf && count--; size -= sizeof *buf, buf++) {
    u64_t in = *buf;
    acc ^= in;
    acc ^= randbox[acc & 0xf];
    acc = acc >> 1 | acc << (64 - 1);
  }

  if (size != 0) {
    const mem_t *tail = coerce(const mem_t *, ptr);
    u64_t in = 0;

    switch (size) {
    case 7:
      in |= convert(u64_t, tail[6]) << 48;
      break;
      /* fallthrough */
    case 6:
      in |= convert(u64_t, tail[5]) << 40;
      break;
      /* fallthrough */
    case 5:
      in |= convert(u64_t, tail[4]) << 32;
      break;
    case 4:
      in |= convert(u64_t, tail[3]) << 24;
      break;
    case 3:
      in |= convert(u64_t, tail[2]) << 16;
      /* fallthrough */
    case 2:
      in |= convert(u64_t, tail[1]) << 8;
      /* fallthrough */
    case 1:
      in |= convert(u64_t, tail[0]);
      break;
    }

    acc ^= in;
    acc ^= randbox[acc & 0xf];
    acc = acc >> 1 | acc << (64 - 1);
  }

  acc ^= randbox[acc & 0xf];
  acc = acc >> 1 | acc << (64 - 1);
  acc ^= randbox[acc & 0xf];
  acc = acc >> 1 | acc << (64 - 1);
  acc ^= randbox[acc & 0xf];
  acc = acc >> 1 | acc << (64 - 1);
  acc ^= randbox[acc & 0xf];

  *pcount = count >> 2;

  return acc;
}

#elif SIZEOF_PTR == 4

static u32_t randbox[] = {
  0x49848f1bU, 0xe6255dbaU, 0x36da5bdcU, 0x47bf94e9U,
  0x8cbcce22U, 0x559fc06aU, 0xd268f536U, 0xe10af79aU,
  0xc1af4d69U, 0x1d2917b5U, 0xec4c304dU, 0x9ee5016cU,
  0x69232f74U, 0xfead7bb3U, 0xe9089ab6U, 0xf012f6aeU,
};

static u32_t hash_c_str(const wchar_t *str, u32_t seed, int *pcount)
{
  int count = *pcount << 2;
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

  *pcount = count >> 2;

  return acc;
}

static u32_t hash_buf(const mem_t *ptr, ucnum size, u32_t seed, int *pcount)
{
  const u32_t *buf = coerce(const u32_t *, ptr);
  int count = *pcount << 2;
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
      /* fallthrough */
    case 2:
      in |= convert(u32_t, tail[1]) << 8;
      /* fallthrough */
    case 1:
      in |= convert(u32_t, tail[0]);
      break;
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

  *pcount = count >> 2;

  return acc;
}

#else
#error portme
#endif

static ucnum hash_double(double n)
{
  union hack {
    volatile double d;
    volatile ucnum a[sizeof (double) / sizeof (ucnum)];
  } u;
  ucnum h = 0;
  unsigned i;

  if (n != 0.0) {
    u.d = n;

    for (i = 0; i < sizeof u.a / sizeof u.a[0]; i++)
      h += u.a[i];
  }

  return h;
}

ucnum equal_hash(val obj, int *count, ucnum seed)
{
  val self = lit("hash-equal");

  if ((*count)-- <= 0)
    return 0;

  switch (type(obj)) {
  case NIL:
    return UINT_PTR_MAX;
  case LIT:
    return hash_c_str(litptr(obj), seed, count);
  case CONS:
    return equal_hash(obj->c.car, count, seed)
            + equal_hash(obj->c.cdr, count, seed + (CONS << 8));
  case STR:
    return hash_c_str(obj->st.str, seed, count);
  case CHR:
    return c_ch(obj);
  case NUM:
    return c_u(obj);
  case SYM:
  case PKG:
  case ENV:
  case DARG:
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
      cnum i, len = c_num(length, self);
      ucnum lseed;

      for (i = 0, lseed = seed; i < len; i++, lseed += seed) {
        h *= 2;
        h += equal_hash(obj->v.vec[i], count, lseed);
        if ((*count)-- <= 0)
          break;
      }

      return h;
    }
  case LCONS:
    return equal_hash(car(obj), count, seed)
            + equal_hash(cdr(obj), count, seed + (CONS << 8));
  case LSTR:
    lazy_str_force_upto(obj, num(*count - 1));
    return equal_hash(obj->ls.prefix, count, seed);
  case BGNUM:
    return mp_hash(mp(obj)) * if3(seed, seed, 1);
  case FLNUM:
    return hash_double(c_f(obj)) * if3(seed, seed, 1);
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
            + equal_hash(obj->rn.to, count, seed + (RNG << 8));
  case BUF:
    return hash_buf(obj->b.data, c_unum(obj->b.len, self), seed, count);
  case TNOD:
    return equal_hash(obj->tn.left, count, (seed + TNOD))
            + equal_hash(obj->tn.right, count, seed + (TNOD << 8))
            + equal_hash(obj->tn.key, count, seed + (TNOD << 16));
  }

  internal_error("unhandled case in equal function");
}

static ucnum eql_hash(val obj, int *count)
{
  if ((*count)-- <= 0)
    return 0;

  switch (tag(obj)) {
  case TAG_PTR:
    switch (type(obj)) {
    case NIL:
      return UINT_PTR_MAX;
    case BGNUM:
      return mp_hash(mp(obj));
    case FLNUM:
      return hash_double(c_f(obj));
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
    return c_ch(obj);
  case TAG_NUM:
    return c_u(obj);
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

static ucnum eq_hash(val obj)
{
  switch (tag_ex(obj)) {
  case TAG_PTR:
    switch (CHAR_BIT * sizeof (mem_t *)) {
    case 32:
      return coerce(ucnum, obj) >> 4;
    case 64: default:
      return coerce(ucnum, obj) >> 5;
    }
  case TAG_CHR:
    return c_ch(obj);
  case TAG_NUM:
    return c_u(obj);
  case TAG_LIT:
    switch (CHAR_BIT * sizeof (mem_t *)) {
    case 32:
      return coerce(ucnum, obj) >> 2;
    case 64: default:
      return coerce(ucnum, obj) >> 3;
    }
#if CONFIG_NAN_BOXING
  case TAG_FLNUM:
    return coerce(ucnum, obj);
#endif
  }
  /* notreached */
  abort();
}

static ucnum hash_find_slot(struct hash *h, val key, ucnum hcode)
{
  val table = h->table;
  val *vec = table->v.vec;
  val (*equal_fun)(val, val) = h->hops->equal_fun;
  ucnum mask = h->mask, start = hcode & mask, i = start;

  do {
    val cell = vec[i];

    if (cell == nil)
      break;

    if (cell->ch.hash == hcode && equal_fun(us_car(cell), key))
      return i;

    i = (i + 1) & h->mask;
  } while (i != start);

  return UINT_PTR_MAX;
}

static val hash_lookup(struct hash *h, val key, ucnum hcode)
{
  val table = h->table;
  val *vec = table->v.vec;

  val (*equal_fun)(val, val) = h->hops->equal_fun;
  ucnum mask = h->mask, start = hcode & mask, i = start;

  do {
    val cell = vec[i];

    if (cell == nil)
      break;

    if (cell->ch.hash == hcode && equal_fun(us_car(cell), key))
      return cell;

    i = (i + 1) & h->mask;
  } while (i != start);

  return nil;
}

static void hash_grow(val hash, struct hash *h, ucnum mask)
{
  ucnum j, nmask = (mask << 1) | 1;
  val ntable;
  val table = h->table;
  val *vec = h->table->v.vec;
  val *nvec;

  if (nmask > NUM_MAX - 1)
    uw_throwf(error_s, lit("hash table overflow"), nao);

  if (h->usecount > 0) {
    push(table, &h->tblstack);
    setcheck(hash, h->tblstack);
  } else {
    h->tblstack = nil;
  }

  ntable = vector(num_fast(nmask + 1), nil);
  nvec = ntable->v.vec;

  h->table = ntable;
  h->mask = nmask;

  setcheck(hash, ntable);

  for (j = 0; j <= mask; j++) {
    val cell = vec[j];

    if (cell) {
      ucnum hcode = cell->ch.hash;
      ucnum start = hcode & nmask, i = start;

      for (;; i = (i + 1) & nmask) {
        if (nvec[i] == nil) {
          nvec[i] = cell;
          break;
        }
      }
    }
  }
}

static val hash_insert(val hash, struct hash *h, val key, ucnum hcode, loc new_p)
{
  val table = h->table;
  val *vec = table->v.vec;
  val (*equal_fun)(val, val) = h->hops->equal_fun;
  ucnum mask = h->mask, start = hcode & mask, i = start;

  do {
    val cell = vec[i];

    if (cell == nil) {
      val ncell = cons(key, nil);
      ncell->ch.hash = hcode;
      vec[i] = ncell;
      setcheck(table, ncell);
      if (!nullocp(new_p))
        deref(new_p) = t;
      if (++h->count > h->mask >> 1)
        hash_grow(hash, h, mask);
      return ncell;
    }

    if (cell->ch.hash == hcode && equal_fun(us_car(cell), key)) {
      if (!nullocp(new_p))
        deref(new_p) = nil;
      return cell;
    }

    i = (i + 1) & h->mask;
  } while (i != start);

  hash_grow(hash, h, mask);

  return hash_insert(hash, h, key, hcode, new_p);
}

static val hash_remove(struct hash *h, ucnum victim)
{
  val table = h->table;
  val *vec = table->v.vec;
  ucnum wipe = victim, i = wipe, mask = h->mask;
  val cell = vec[i];
  val ret = nil;
  val vicentry = vec[victim];

  if (cell == nil)
    return ret;

  ret = us_cdr(cell);

  i = (i + 1) & mask;

  while (i != wipe) {
    cell = vec[i];

    if (cell == nil) {
      break;
    } else {
      ucnum hcode = cell->ch.hash;
      ucnum iprobe = hcode & mask;

      if ((i < wipe) ^ (iprobe <= wipe) ^ (iprobe > i)) {
        vec[wipe] = vec[i];
        wipe = i;
      }
      i = (i + 1) & h->mask;
    }
  }

  vec[wipe] = nil;
  bug_unless (h->count > 0);
  h->count--;

  if (h->usecount) {
    val tblit;

    for (tblit = h->tblstack; tblit; tblit = us_cdr(tblit)) {
      val stbl = us_car(tblit);
      val *svec = stbl->v.vec;
      ucnum smask = c_unum(svec[vec_length], nil) - 1;
      ucnum start = victim & smask;
      ucnum end = (victim + smask) & smask;

      for (i = start; i != end; i = (i + 1) & smask) {
        if (svec[i] == vicentry) {
          svec[i] = nil;
          break;
        }
      }
    }
  } else if (h->tblstack) {
    h->tblstack = nil;
  }

  return ret;
}

static ucnum eql_hash_op(val obj, int *count, ucnum seed)
{
  (void) seed;
  return eql_hash(obj, count);
}

static ucnum eq_hash_op(val obj, int *count, ucnum seed)
{
  (void) seed;
  (void) count;
  return eq_hash(obj);
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
  val lcell, rcell;
  val free_conses = nil;
  val pending = nil;
  struct hash_iter lhi, rhi;

  if (l->hops != r->hops)
    return nil;

  if (l->count != r->count)
    return nil;

  if (!equal(l->userdata, r->userdata))
    return nil;

  if (l->count == 0)
    return t;

  us_hash_iter_init(&lhi, left);
  us_hash_iter_init(&rhi, right);

  while ((lcell = hash_iter_next(&lhi)) && ((rcell = hash_iter_next(&rhi)))) {
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
  val cell;
  struct hash_iter hi;

  if ((*count)-- <= 0)
    return 0;

  switch (CHAR_BIT * sizeof (mem_t *)) {
  case 32:
    out += coerce(ucnum, h->hops) >> 4;
    break;
  case 64: default:
    out += coerce(ucnum, h->hops) >> 5;
    break;
  }

  out += equal_hash(h->userdata, count, seed);
  out &= NUM_MAX;

  us_hash_iter_init(&hi, obj);

  while ((*count)-- > 0 && (cell = hash_iter_next(&hi)) != nil) {
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
    if (h->hops == &hash_eq_ops) {
      obj_print_impl(eq_based_k, out, pretty, ctx);
      need_space = 1;
    } else if (h->hops == &hash_equal_ops) {
      obj_print_impl(equal_based_k, out, pretty, ctx);
      need_space = 1;
    }
  } else {
    if (h->hops == &hash_eql_ops) {
      obj_print_impl(eql_based_k, out, pretty, ctx);
      need_space = 1;
    } else if (h->hops == &hash_eq_ops) {
      obj_print_impl(eq_based_k, out, pretty, ctx);
      need_space = 1;
    }
  }

  if (h->wkopt != hash_weak_none) {
    if (need_space)
      put_char(chr(' '), out);
    need_space = 1;
    switch (h->wkopt) {
    case hash_weak_or:
      obj_print_impl(weak_keys_k, out, pretty, ctx);
      put_char(chr(' '), out);
      /* fallthrough */
    case hash_weak_vals:
      obj_print_impl(weak_vals_k, out, pretty, ctx);
      break;
    case hash_weak_keys:
      obj_print_impl(weak_keys_k, out, pretty, ctx);
      break;
    case hash_weak_and:
      obj_print_impl(weak_and_k, out, pretty, ctx);
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
  put_char(chr(')'), out);
  {
    val cell;
    struct hash_iter hi;
    cnum max_len = ctx->strm->max_length;
    cnum max_count = max_len;

    us_hash_iter_init(&hi, hash);

    while ((cell = hash_iter_next(&hi))) {
      val key = us_car(cell);
      val value = us_cdr(cell);
      if (width_check(out, chr(' ')))
        force_br = 1;

      if (max_len && --max_count < 0) {
        put_string(lit("..."), out);
        break;
      }

      put_char(chr('('), out);
      obj_print_impl(key, out, pretty, ctx);

      if (value) {
        put_char(chr(' '), out);
        obj_print_impl(value, out, pretty, ctx);
      }

      put_char(chr(')'), out);
    }
  }
  put_char(chr(')'), out);

  if (force_br)
    force_break(out);

  set_indent_mode(out, save_mode);
  set_indent(out, save_indent);
}

static void hash_mark(val hash)
{
  struct hash *h = coerce(struct hash *, hash->co.handle);
  val table = h->table;
  val *vec = table->v.vec;
  ucnum mask = h->mask;

  gc_mark(h->userdata);

  if (h->count == 0 || h->tblstack) {
    gc_mark(table);
    gc_mark(h->tblstack);
    return;
  }

  /* Use counts will be re-calculated by a scan of the
     hash iterators which are still reachable. */
  h->usecount = 0;

  switch (h->wkopt) {
    ucnum i;
  case hash_weak_none:
    gc_mark(table);
    return;
  case hash_weak_keys:
    /* Mark values only. Don't mark the table. */
    for (i = 0; i <= mask; i++) {
      val entry = vec[i];
      if (!entry)
        continue;
      gc_mark(us_cdr(entry));
    }
    break;
  case hash_weak_vals:
    /* Mark keys only. Don't mark the table. */
    for (i = 0; i <= mask; i++) {
      val entry = vec[i];
      if (!entry)
        continue;
      gc_mark(us_car(entry));
    }
    break;
  case hash_weak_or:
    /* mark nothing */
    break;
  case hash_weak_and:
    /* Mark key if value is reachable and vice versa. */
    for (i = 0; i <= mask; i++) {
      val entry = vec[i];
      if (!entry)
        continue;
      if (gc_is_reachable(us_car(entry)))
        gc_mark(us_cdr(entry));
      else if (gc_is_reachable(us_cdr(entry)))
        gc_mark(us_car(entry));
    }
    break;
  }

  h->next = reachable_weak_hashes;
  reachable_weak_hashes = h;
}

static struct cobj_ops hash_ops = cobj_ops_init(hash_equal_op,
                                                hash_print_op,
                                                cobj_destroy_free_op,
                                                hash_mark,
                                                hash_hash_op,
                                                copy_hash);

static val hash_assoc(val key, ucnum hash, val list)
{
  while (list) {
    val elem = us_car(list);
    if (elem->ch.hash == hash && equal(us_car(elem), key))
      return elem;
    list = us_cdr(list);
  }

  return nil;
}

static val hash_assql(val key, ucnum hash, val list)
{
  while (list) {
    val elem = us_car(list);
    if (elem->ch.hash == hash && eql(us_car(elem), key))
      return elem;
    list = us_cdr(list);
  }

  return nil;
}

static val hash_assq(val key, ucnum hash, val list)
{
  while (list) {
    val elem = us_car(list);
    if (elem->ch.hash == hash && us_car(elem) == key)
      return elem;
    list = us_cdr(list);
  }

  return nil;
}


static_def(struct hash_ops hash_eq_ops = hash_ops_init(eq_hash_op, eql,
                                                       hash_assq));

static_def(struct hash_ops hash_eql_ops = hash_ops_init(eql_hash_op, eql,
                                                        hash_assql));

static_def(struct hash_ops hash_equal_ops = hash_ops_init(equal_hash, equal,
                                                          hash_assoc));

static val do_make_hash(hash_weak_opt_t wkopt, hash_type_t type, val seed)
{
  val self = lit("make-hash");

  if (type == hash_type_equal &&
      wkopt != hash_weak_none && wkopt != hash_weak_vals)
  {
    uw_throwf(error_s,
              lit("make-hash: bad combination :weak-keys with :equal-based"),
              nao);
  } else {
    struct hash *h = coerce(struct hash *, chk_malloc(sizeof *h));
    val mod = num_fast(256);
    val table = vector(mod, nil);
    val hash = cobj(coerce(mem_t *, h), hash_cls, &hash_ops);

    h->seed = c_unum(default_arg(seed, if3(hash_seed_s, hash_seed, zero)),
                     self);
    h->wkopt = wkopt;
    h->mask = c_unum(mod, self) - 1;
    h->count = 0;
    h->table = table;
    h->userdata = nil;

    h->usecount = 0;
    h->tblstack = nil;

    switch (type) {
    case hash_type_eq:
      h->hops = &hash_eq_ops;
      break;
    case hash_type_eql:
      h->hops = &hash_eql_ops;
      break;
    case hash_type_equal:
    default:
      h->hops = &hash_equal_ops;
      break;
    }

    return hash;
  }
}

static hash_weak_opt_t weak_opt_from_flags(val weak_keys, val weak_vals)
{
  if (weak_keys) {
    if (weak_keys == weak_and_k)
      return hash_weak_and;
    if (weak_keys == weak_or_k)
      return hash_weak_or;
  }

  switch (!!weak_vals << 1 | !!weak_keys) {
  case 0: return hash_weak_none;
  case 1: return hash_weak_keys;
  case 2: return hash_weak_vals;
  case 3: return hash_weak_or;
  default:
    /* notreached */
    abort();
  }
}

val make_seeded_hash(val weak_keys, val weak_vals, val equal_based, val seed)
{
  return do_make_hash(weak_opt_from_flags(weak_keys, weak_vals),
                      if3(equal_based, hash_type_equal, hash_type_eql),
                      seed);
}

val make_hash(hash_weak_opt_t wkopt, val equal_based)
{
  return do_make_hash(wkopt,
                      if3(equal_based, hash_type_equal, hash_type_eql),
                      nil);
}

val make_eq_hash(hash_weak_opt_t wkopt)
{
  return do_make_hash(wkopt, hash_type_eq, nil);
}

val make_similar_hash(val existing)
{
  val self = lit("make-similar-hash");
  struct hash *ex = coerce(struct hash *, cobj_handle(self, existing, hash_cls));
  struct hash *h = coerce(struct hash *, chk_malloc(sizeof *h));
  val mod = num_fast(256);
  val table = vector(mod, nil);
  val hash = cobj(coerce(mem_t *, h), hash_cls, &hash_ops);

  h->mask = c_unum(mod, self) - 1;
  h->count = 0;
  h->table = table;
  h->userdata = ex->userdata;

  h->seed = ex->seed;
  h->wkopt = ex->wkopt;
  h->usecount = 0;
  h->tblstack = 0;
  h->hops = ex->hops;

  return hash;
}

val copy_hash(val existing)
{
  val self = lit("copy-hash");
  struct hash *ex = coerce(struct hash *, cobj_handle(self, existing, hash_cls));
  struct hash *h = coerce(struct hash *, chk_malloc(sizeof *h));
  val mod = num_fast(ex->mask + 1);
  val table = vector(mod, nil);
  val hash = cobj(coerce(mem_t *, h), hash_cls, &hash_ops);
  val *exvec = ex->table->v.vec;
  val *vec = table->v.vec;
  ucnum i;

  h->mask = ex->mask;
  h->count = ex->count;
  h->table = table;
  h->userdata = ex->userdata;

  h->seed = ex->seed;
  h->wkopt = ex->wkopt;
  h->usecount = 0;
  h->tblstack = 0;
  h->hops = ex->hops;

  for (i = 0; i <= h->mask; i++) {
    val cell = exvec[i];
    if (cell) {
      val ncell = cons(us_car(cell), us_cdr(cell));
      ncell->ch.hash = cell->ch.hash;
      vec[i] = ncell;
      setcheck(table, ncell);
    }
  }

  return hash;
}

val gethash_c(val self, val hash, val key, loc new_p)
{
  struct hash *h = coerce(struct hash *, cobj_handle(self, hash, hash_cls));
  int lim = hash_traversal_limit;
  ucnum hv = h->hops->hash_fun(key, &lim, h->seed);
  return hash_insert(hash, h, key, hv, new_p);
}

val gethash_e(val self, val hash, val key)
{
  struct hash *h = coerce(struct hash *, cobj_handle(self, hash, hash_cls));
  int lim = hash_traversal_limit;
  ucnum hv = h->hops->hash_fun(key, &lim, h->seed);
  return hash_lookup(h, key, hv);
}

val gethash_d(val hash, val key)
{
  return gethash_e(lit("gethash-d"), hash, key);
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
  struct hash *h = coerce(struct hash *, cobj_handle(self, hash, hash_cls));
  int lim = hash_traversal_limit;
  ucnum hv = h->hops->hash_fun(key, &lim, h->seed);
  ucnum victim = hash_find_slot(h, key, hv);
  return (victim != UINT_PTR_MAX) ? hash_remove(h, victim) : nil;
}

val clearhash(val hash)
{
  val self = lit("clearhash");
  struct hash *h = coerce(struct hash *, cobj_handle(self, hash, hash_cls));
  val mod = num_fast(256);
  val table = vector(mod, nil);
  ucnum oldcount = h->count;
  h->mask = c_unum(mod, self) - 1;
  h->count = 0;
  h->table = table;
  setcheck(hash, table);
  return oldcount ? num(oldcount) : nil;
}

val hash_count(val hash)
{
  val self = lit("hash-count");
  struct hash *h = coerce(struct hash *, cobj_handle(self, hash, hash_cls));
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
  struct hash *h = coerce(struct hash *, cobj_handle(self, hash, hash_cls));
  return h->userdata;
}

val set_hash_userdata(val hash, val data)
{
  val self = lit("set-hash-userdata");
  struct hash *h = coerce(struct hash *, cobj_handle(self, hash, hash_cls));
  val olddata = h->userdata;
  set(mkloc(h->userdata, hash), data);
  return olddata;
}

val hashp(val obj)
{
  return cobjclassp(obj, hash_cls);
}

static void hash_iter_mark(val hash_iter)
{
  struct hash_iter *hi = coerce(struct hash_iter *, hash_iter->co.handle);
  gc_mark(hi->hash);
  gc_mark(hi->table);
  hi->next = reachable_iters;
  reachable_iters = hi;
}

static struct cobj_ops hash_iter_ops = cobj_ops_init(eq,
                                                     cobj_print_op,
                                                     cobj_destroy_free_op,
                                                     hash_iter_mark,
                                                     cobj_eq_hash_op,
                                                     0);

void hash_iter_init(struct hash_iter *hi, val hash, val self)
{
  struct hash *h = coerce(struct hash *, cobj_handle(self, hash, hash_cls));
  hi->next = 0;
  hi->hash = hash;
  hi->table = h->table;
  hi->mask = h->mask;
  hi->index = 0;
  h->usecount++;
}

void us_hash_iter_init(struct hash_iter *hi, val hash)
{
  struct hash *h = coerce(struct hash *, hash->co.handle);
  hi->next = 0;
  hi->hash = hash;
  hi->table = h->table;
  hi->mask = h->mask;
  hi->index = 0;
  h->usecount++;
}

static val hash_iter_next_impl(struct hash_iter *hi)
{
  val hash = hi->hash;
  struct hash *h = hash ? coerce(struct hash *, hash->co.handle) : 0;
  ucnum mask = hi->mask;

  if (!h)
    return nil;

  while (hi->index <= mask) {
    val cell = hi->table->v.vec[hi->index++];
    if (cell)
      return cell;
  }

  hi->hash = nil;
  if (--h->usecount <= 0)
    h->tblstack = nil;

  return nil;
}

val hash_iter_next(struct hash_iter *hi)
{
  return hash_iter_next_impl(hi);
}

val hash_iter_peek(struct hash_iter *hi)
{
  val hash = hi->hash;
  struct hash *h = hash ? coerce(struct hash *, hash->co.handle) : 0;
  ucnum mask = hi->mask, index = hi->index;

  if (!h)
    return nil;

  while (index <= mask) {
    val cell = hi->table->v.vec[index++];
    if (cell)
      return cell;
  }

  return nil;
}

val hash_begin(val hash)
{
  val self = lit("hash-begin");
  val hi_obj;
  struct hash_iter *hi = coerce(struct hash_iter *, chk_malloc(sizeof *hi));
  hash_iter_init(hi, hash, self);
  hi_obj = cobj(coerce(mem_t *, hi), hash_iter_cls, &hash_iter_ops);
  gc_hint(hash);
  return hi_obj;
}

val hash_next(val iter)
{
  val self = lit("hash-next");
  struct hash_iter *hi = coerce(struct hash_iter *,
                                cobj_handle(self, iter, hash_iter_cls));
  return hash_iter_next_impl(hi);
}


val hash_peek(val iter)
{
  val self = lit("hash-peek");
  struct hash_iter *hi = coerce(struct hash_iter *,
                                cobj_handle(self, iter, hash_iter_cls));
  return hash_iter_peek(hi);
}

val hash_reset(val iter, val hash)
{
  val self = lit("hash-reset");
  struct hash_iter *hi = coerce(struct hash_iter *,
                                cobj_handle(self, iter, hash_iter_cls));

  if (hi->hash) {
    struct hash *h = coerce(struct hash *, hash->co.handle);
    if (--h->usecount <= 0)
      h->tblstack = nil;
  }

  if (hash) {
    hash_iter_init(hi, hash, self);
    if (hi->table)
      setcheck(iter, hash);
  } else {
    memset(hi, 0, sizeof *hi);
  }
  return iter;
}

val maphash(val fun, val hash)
{
  val self = lit("maphash");
  struct hash_iter hi;
  val cell;

  hash_iter_init(&hi, hash, self);

  gc_hint(hash);

  while ((cell = hash_iter_next(&hi)) != nil)
    funcall2(fun, us_car(cell), us_cdr(cell));

  return nil;
}

val hash_eql(val obj)
{
  int lim = hash_traversal_limit;
  return num_fast(eql_hash(obj, &lim));
}

val hash_equal(val obj, val seed)
{
  val self = lit("hash-equal");
  int lim = hash_traversal_limit;
  return num_fast(equal_hash(obj, &lim,
                             if3(missingp(seed), 0, c_unum(seed, self))));
}

/*
 * Called from garbage collector. Hash module must process all weak tables
 * that were visited during the marking phase, maintained in the list
 * reachable_weak_hashes.
 */
static void do_weak_tables(void)
{
  struct hash *h = reachable_weak_hashes;

  reachable_weak_hashes = 0;

  for (; h != 0; h = h->next) {
    ucnum i, c = 0;
    val table = h->table;
    val *vec = table->v.vec;
    ucnum mask = h->mask;

    /* The table of a weak hash was spuriously reached by conservative GC;
       it's a waste of time doing weak processing, since all keys and
       values have been transitively marked as reachable; and so we
       won't find anything to remove. */
    if (gc_is_reachable(table))
      continue;

    h->count = UINT_PTR_MAX;

    switch (h->wkopt) {
    case hash_weak_none:
      /* what is this doing here */
      break;
    case hash_weak_keys:
      /* Sweep through all entries. Delete any which have keys
         that are garbage. */
      for (i = 0; i <= mask; i++) {
        val entry = vec[i];

        if (entry) {
          if (!gc_is_reachable(us_car(entry))) {
            hash_remove(h, i--);
#if CONFIG_EXTRA_DEBUGGING
            if (us_car(entry) == break_obj)
              breakpt();
#endif
          } else {
            gc_mark(entry);
            c++;
          }
        }
      }
      break;
    case hash_weak_vals:
      /* Sweep through all entries. Delete any which have values
         that are garbage. */
      for (i = 0; i <= mask; i++) {
        val entry = vec[i];

        if (entry) {
          if (!gc_is_reachable(us_cdr(entry))) {
            hash_remove(h, i--);
#if CONFIG_EXTRA_DEBUGGING
            if (us_cdr(entry) == break_obj)
              breakpt();
#endif
          } else {
            gc_mark(entry);
            c++;
          }
        }
      }
      break;
    case hash_weak_and:
      /* Sweep through all entries. Delete any which have keys
         and values that are garbage. */
      for (i = 0; i <= mask; i++) {
        val entry = vec[i];

        if (entry) {
          if (!gc_is_reachable(us_car(entry)) && !gc_is_reachable(us_cdr(entry))) {
            hash_remove(h, i--);
#if CONFIG_EXTRA_DEBUGGING
            if (!gc_is_reachable(us_car(entry)) && us_car(entry) == break_obj)
              breakpt();
            if (!gc_is_reachable(us_cdr(entry)) && us_cdr(entry) == break_obj)
              breakpt();
#endif
          } else {
            gc_mark(entry);
            c++;
          }
        }
      }
      break;
    case hash_weak_or:
      /* Sweep through all entries. Delete any which have keys
         or values that are garbage. */
      for (i = 0; i <= mask; i++) {
        val entry = vec[i];

        if (entry) {
          if (!gc_is_reachable(us_car(entry)) || !gc_is_reachable(us_cdr(entry))) {
            hash_remove(h, i--);
#if CONFIG_EXTRA_DEBUGGING
            if (!gc_is_reachable(us_car(entry)) && us_car(entry) == break_obj)
              breakpt();
            if (!gc_is_reachable(us_cdr(entry)) && us_cdr(entry) == break_obj)
              breakpt();
#endif
          } else {
            gc_mark(entry);
            c++;
          }
        }
      }
      break;
    }

    /* Garbage is gone now. Seal things by marking the vector. */
    gc_mark_norec(table);
    gc_mark(vec[vec_alloc]);
    gc_mark(vec[vec_length]);
    h->count = c;
  }

  /* More weak hashes were discovered during weak processing.
     Do another round. */
  if (reachable_weak_hashes)
    do_weak_tables();
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

static val equal_based_p(val equal, val eql, val eq, val wkeys)
{
  val mutex = lit("make-hash: mutually exclusive ~s and ~s");

  if (opt_compat && opt_compat <= 187)
    return equal;

  if (equal && eql)
    uw_throwf(error_s, mutex, equal_based_k, eql_based_k, nao);

  if (equal && eq)
    uw_throwf(error_s, mutex, equal_based_k, eq_based_k, nao);

  if (eql && eq)
    uw_throwf(error_s, mutex, eql_based_k, eq_based_k, nao);

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

val hashv(varg args)
{
  val self = lit("hash");
  val wkeys = nil, wvals = nil, equal = nil, eql = nil, wand = nil, wor = nil;
  val eq = nil, userdata = nil;
  struct args_bool_key akv[] = {
    { weak_keys_k, nil, &wkeys },
    { weak_vals_k, nil, &wvals },
    { equal_based_k, nil, &equal },
    { eql_based_k, nil, &eql },
    { eq_based_k, nil, &eq },
    { weak_and_k, nil, &wand },
    { weak_or_k, nil, &wor },
    { userdata_k, t, &userdata }
  };
  hash_weak_opt_t wkopt = hash_weak_none;

  args_keys_extract(args, akv, sizeof akv / sizeof akv[0]);

  if (wand && wor)
    uw_throwf(error_s, lit("~a: both ~s and ~s specified"),
              self, weak_and_k, weak_or_k, nao);

  if (wand)
    wkopt = hash_weak_and;
  else if (wor)
    wkopt = hash_weak_or;
  else
    wkopt = weak_opt_from_flags(wkeys, wvals);

  {
    val ebp = equal_based_p(equal, eql, eq, wkeys);
    val hash = if3(eq,
                   make_eq_hash(wkopt),
                   make_hash(wkopt, ebp));
    if (userdata)
      set_hash_userdata(hash, userdata);
    return hash;
  }
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

val hash_from_pairs_v(val pairs, varg hashv_args)
{
  return hash_construct(args_get_list(hashv_args), pairs);
}

val hash_from_alist_v(val alist, varg hashv_args)
{
  val hash = hashv(hashv_args);

  alist = nullify(alist);

  for (; alist; alist = cdr(alist)) {
    val pair = car(alist);
    sethash(hash, car(pair), cdr(pair));
  }

  return hash;
}

val hash_map(val fun, val seq, varg hashv_args)
{
  val self = lit("hash-map");
  seq_iter_t iter;
  val hash = hashv(hashv_args), elem;
  seq_iter_init(self, &iter, seq);

  while (seq_get(&iter, &elem))
    sethash(hash, elem, funcall1(fun, elem));

  return hash;
}

val hash_props(varg plist)
{
  val self = lit("hash-props");
  args_decl_constsize(args, ARGS_MIN);
  val hash = hashv(args);
  cnum index = 0;

  while (args_two_more(plist, index)) {
    val key = args_get(plist, &index);
    val value = args_get(plist, &index);
    sethash(hash, key, value);
  }

  if (args_more(plist, index))
    uw_throwf(error_s, lit("~a: unpaired ~s argument"),
              self, args_get(plist, &index), nao);

  return hash;
}

val hash_list(val keys, varg hashv_args)
{
  val hash = hashv(hashv_args);

  keys = nullify(keys);

  for (; keys; keys = cdr(keys)) {
    val key = car(keys);
    sethash(hash, key, key);
  }

  return hash;
}

val hash_zip(val keys, val vals, varg hashv_args)
{
  val self = lit("hash-zip");
  seq_iter_t key_iter, val_iter;
  val k, v;
  val hash = hashv(hashv_args);

  seq_iter_init(self, &key_iter, keys);
  seq_iter_init(self, &val_iter, vals);

  while (seq_get(&key_iter, &k) && seq_get(&val_iter, &v))
    sethash(hash, k, v);

  return hash;
}

val group_by(val func, val seq, varg hashv_args)
{
  val self = lit("group-by");
  val hash = hashv(hashv_args);
  seq_iter_t iter;
  val elem;

  seq_iter_init(self, &iter, seq);

  while (seq_get(&iter, &elem))
    pushhash(hash, funcall1(func, elem), elem);

  {
    struct hash_iter hi;
    val cell;

    us_hash_iter_init(&hi, hash);

    while ((cell = hash_iter_next(&hi)) != nil)
      us_rplacd(cell, nreverse(us_cdr(cell)));

    return hash;
  }
}

val group_map(val by_fun, val filter_fun, val seq, varg hashv_args)
{
  val hash = group_by(by_fun, seq, hashv_args);
  return hash_update(hash, filter_fun);
}

val group_reduce(val hash, val by_fun, val reduce_fun, val seq,
                 val initval, val filter_fun)
{
  val self = lit("group-reduce");
  seq_iter_t iter;
  val elem;

  initval = default_null_arg(initval);

  seq_iter_init(self, &iter, seq);

  while (seq_get(&iter, &elem))
  {
    val key = funcall1(by_fun, elem);
    val new_p;
    loc pcdr = gethash_l(self, hash, key, mkcloc(new_p));

    if (new_p)
      set(pcdr, funcall2(reduce_fun, initval, elem));
    else
      set(pcdr, funcall2(reduce_fun, deref(pcdr), elem));
  }

  if (!null_or_missing_p(filter_fun))
    hash_update(hash, filter_fun);

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

val hash_uni(val hash1, val hash2, val joinfun, val map1fun, val map2fun)
{
  val self = lit("hash-uni");
  struct hash *h1 = coerce(struct hash *, cobj_handle(self, hash1, hash_cls));
  struct hash *h2 = coerce(struct hash *, cobj_handle(self, hash2, hash_cls));

  if (h1->hops != h2->hops)
    uw_throwf(error_s, lit("~a: ~s and ~s are incompatible hashes"),
              self, hash1, hash2, nao);

  {
    val hout = make_similar_hash(hash1);
    val entry;
    struct hash_iter hi;

    hash_iter_init(&hi, hash2, self);

    for (entry = hash_iter_next(&hi); entry; entry = hash_iter_next(&hi)) {
      val rentry = us_cdr(entry);
      if (!missingp(map2fun))
        rentry = funcall1(map2fun, rentry);
      sethash(hout, us_car(entry), rentry);
    }

    hash_iter_init(&hi, hash1, self);

    for (entry = hash_iter_next(&hi); entry; entry = hash_iter_next(&hi)) {
      val lentry = us_cdr(entry);
      if (!missingp(map1fun))
        lentry = funcall1(map1fun, lentry);

      if (missingp(joinfun)) {
        sethash(hout, us_car(entry), lentry);
      } else {
        val new_p;
        loc ptr = gethash_l(self, hout, us_car(entry), mkcloc(new_p));
        if (new_p) {
          sethash(hout, us_car(entry), lentry);
        } else {
          set(ptr, funcall2(joinfun, lentry, deref(ptr)));
        }
      }
    }

    return hout;
  }
}

val hash_join(val hash1, val hash2, val joinfun, val h1dfl, val h2dfl)
{
  val self = lit("hash-join");
  struct hash *h1 = coerce(struct hash *, cobj_handle(self, hash1, hash_cls));
  struct hash *h2 = coerce(struct hash *, cobj_handle(self, hash2, hash_cls));

  if (h1->hops != h2->hops)
    uw_throwf(error_s, lit("~a: ~s and ~s are incompatible hashes"),
              self, hash1, hash2, nao);

  {
    val hout = make_similar_hash(hash1);
    val h1ent, h2ent;
    struct hash_iter hi;

    hash_iter_init(&hi, hash1, self);

    for (h1ent = hash_iter_next(&hi); h1ent; h1ent = hash_iter_next(&hi)) {
      val h1val = us_cdr(h1ent);
      val key = us_car(h1ent);
      val h2ent = gethash_e(self, hash2, key);
      val h2val = if3(h2ent, us_cdr(h2ent), h2dfl);

      sethash(hout, key, funcall2(joinfun, h1val, h2val));
    }

    hash_iter_init(&hi, hash2, self);

    for (h2ent = hash_iter_next(&hi); h2ent; h2ent = hash_iter_next(&hi)) {
      val h2val = us_cdr(h2ent);
      val key = us_car(h2ent);
      val h1ent = gethash_e(self, hash1, us_car(h2ent));
      val h1val = if3(h1ent, us_cdr(h1ent), h1dfl);

      sethash(hout, key, funcall2(joinfun, h1val, h2val));
    }

    return hout;
  }
}

val hash_diff(val hash1, val hash2)
{
  val self = lit("hash-diff");
  struct hash *h1 = coerce(struct hash *, cobj_handle(self, hash1, hash_cls));
  struct hash *h2 = coerce(struct hash *, cobj_handle(self, hash2, hash_cls));

  if (h1->hops != h2->hops)
    uw_throwf(error_s, lit("~a: ~s and ~a are incompatible hashes"),
              self, hash1, hash2, nao);

  {
    val hout = copy_hash(hash1);
    val entry;
    struct hash_iter hi;

    hash_iter_init(&hi, hash2, self);

    for (entry = hash_iter_next(&hi); entry; entry = hash_iter_next(&hi)) {
      remhash(hout, us_car(entry));
    }

    return hout;
  }
}

val hash_symdiff(val hash1, val hash2)
{
  val self = lit("hash-symdiff");
  struct hash *h1 = coerce(struct hash *, cobj_handle(self, hash1, hash_cls));
  struct hash *h2 = coerce(struct hash *, cobj_handle(self, hash2, hash_cls));

  if (h1->hops != h2->hops)
    uw_throwf(error_s, lit("~a: ~s and ~a are incompatible hashes"),
              self, hash1, hash2, nao);

  {
    val hout = make_similar_hash(hash1);
    val entry;
    struct hash_iter hi;

    hash_iter_init(&hi, hash1, self);

    for (entry = hash_iter_next(&hi); entry; entry = hash_iter_next(&hi)) {
      if (!gethash_e(self, hash2, us_car(entry)))
        sethash(hout, us_car(entry), us_cdr(entry));
    }

    hash_iter_init(&hi, hash2, self);

    for (entry = hash_iter_next(&hi); entry; entry = hash_iter_next(&hi)) {
      if (!gethash_e(self, hash1, us_car(entry)))
        sethash(hout, us_car(entry), us_cdr(entry));
    }

    return hout;
  }
}

val hash_isec(val hash1, val hash2, val joinfun)
{
  val self = lit("hash-isec");
  struct hash *h1 = coerce(struct hash *, cobj_handle(self, hash1, hash_cls));
  struct hash *h2 = coerce(struct hash *, cobj_handle(self, hash2, hash_cls));

  if (h1->hops != h2->hops)
    uw_throwf(error_s, lit("~a: ~s and ~s are incompatible hashes"),
              self, hash1, hash2, nao);

  {
    val hout = make_similar_hash(hash1);
    val entry;
    struct hash_iter hi;

    hash_iter_init(&hi, hash1, self);

    for (entry = hash_iter_next(&hi); entry; entry = hash_iter_next(&hi)) {
      val found = gethash_e(self, hash2, us_car(entry));
      if (found) {
        if (missingp(joinfun))
          sethash(hout, us_car(entry), us_cdr(entry));
        else
          sethash(hout, us_car(entry), funcall2(joinfun, us_cdr(entry), us_cdr(found)));
      }
    }

    return hout;
  }
}

val hash_subset(val hash1, val hash2)
{
  val self = lit("hash-subset");
  val entry;
  struct hash_iter hi;

  hash_iter_init(&hi, hash1, self);

  for (entry = hash_iter_next(&hi); entry; entry = hash_iter_next(&hi)) {
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
  val self = lit("hash-update");
  val cell;
  struct hash_iter hi;

  hash_iter_init(&hi, hash, self);

  while ((cell = hash_iter_next(&hi)) != nil) {
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
  val self = lit("hash-revget");
  val cell;
  struct hash_iter hi;

  hash_iter_init(&hi, hash, self);

  if (opt_compat && opt_compat <= 248)
    test = default_arg(test, eql_f);
  else
    test = default_arg(test, equal_f);

  keyfun = default_arg(keyfun, identity_f);

  while ((cell = hash_iter_next(&hi)) != nil) {
    if (funcall2(test, value, funcall1(keyfun, us_cdr(cell))))
      return us_car(cell);
  }

  return nil;
}

val hash_keys_of(val hash, val value, val test, val keyfun)
{
  val self = lit("hash-keys-of");
  val cell;
  struct hash_iter hi;
  list_collect_decl(out, ptail);

  hash_iter_init(&hi, hash, self);

  test = default_arg(test, equal_f);
  keyfun = default_arg(keyfun, identity_f);

  while ((cell = hash_iter_next(&hi)) != nil) {
    if (funcall2(test, value, funcall1(keyfun, us_cdr(cell))))
      ptail = list_collect(ptail, us_car(cell));
  }

  return out;
}

val hash_invert(val hash, val joinfun, val unitfun, varg hashv_args)
{
  val self = lit("hash-invert");
  val hout = hashv(hashv_args);
  val jfun = default_arg(joinfun, identity_star_f);
  val ufun = default_arg(unitfun, identity_f);
  val cell;
  struct hash_iter hi;

  hash_iter_init(&hi, hash, self);

  if (jfun == identity_star_f && ufun == identity_f) {
    while ((cell = hash_iter_next(&hi)) != nil) {
      us_cons_bind(k, v, cell);
      sethash(hout, v, k);
    }
  } else {
    while ((cell = hash_iter_next(&hi)) != nil) {
      us_cons_bind(k, v, cell);
      val new_p;
      loc place = gethash_l(self, hout, v, mkcloc(new_p));
      val ukey = funcall1(ufun, k);
      if (new_p)
        set(place, ukey);
      else
        set(place, funcall2(jfun, deref(place), ukey));
    }
  }

  return hout;
}

static val set_hash_traversal_limit(val lim)
{
  val self = lit("set-hash-traversal-limit");
  val old = num(hash_traversal_limit);
  hash_traversal_limit = c_num(lim, self);
  return old;
}

static val gen_hash_seed(void)
{
  val self = lit("gen-hash-seed");
  val time = time_sec_nsec();
  ucnum sec = convert(ucnum, c_time(car(time), self));
  ucnum nsec = c_unum(cdr(time), self);
#if HAVE_UNISTD_H
  ucnum pid = convert(ucnum, getpid());
#else
  ucnum pid = 0;
#endif
#if SIZEOF_PTR == 8
  return unum((sec << 32) ^ (pid << 16) ^ nsec);
#else
  return unum(sec ^ (nsec << 12) ^ pid);
#endif
}

void hash_early_init(void)
{
  hash_cls = cobj_register(nil);
  hash_iter_cls = cobj_register(nil);
}

void hash_init(void)
{
  val ghu = func_n1(get_hash_userdata);

  weak_keys_k = intern(lit("weak-keys"), keyword_package);
  weak_vals_k = intern(lit("weak-vals"), keyword_package);
  weak_and_k = intern(lit("weak-and"), keyword_package);
  weak_or_k = intern(lit("weak-or"), keyword_package);
  equal_based_k = intern(lit("equal-based"), keyword_package);
  eql_based_k = intern(lit("eql-based"), keyword_package);
  eq_based_k = intern(lit("eq-based"), keyword_package);
  userdata_k = intern(lit("userdata"), keyword_package);
  hash_seed_s = intern(lit("*hash-seed*"), user_package);

  hash_cls->cls_sym = hash_s;
  hash_iter_cls->cls_sym = hash_iter_s;

  reg_var(hash_seed_s, zero);

  reg_fun(intern(lit("make-hash"), user_package), func_n4o(make_seeded_hash, 3));
  reg_fun(intern(lit("make-similar-hash"), user_package), func_n1(make_similar_hash));
  reg_fun(intern(lit("copy-hash"), user_package), func_n1(copy_hash));
  reg_fun(intern(lit("hash"), user_package), func_n0v(hashv));
  reg_fun(hash_construct_s, func_n2(hash_construct));
  reg_fun(intern(lit("hash-from-pairs"), user_package), func_n1v(hash_from_pairs_v));
  reg_fun(intern(lit("hash-from-alist"), user_package), func_n1v(hash_from_alist_v));
  reg_fun(intern(lit("hash-map"), user_package), func_n2v(hash_map));
  reg_fun(intern(lit("hash-props"), user_package), func_n0v(hash_props));
  reg_fun(intern(lit("hash-list"), user_package), func_n1v(hash_list));
  reg_fun(intern(lit("hash-zip"), user_package), func_n2v(hash_zip));
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
  reg_fun(intern(lit("hash-uni"), user_package), func_n5o(hash_uni, 2));
  reg_fun(intern(lit("hash-join"), user_package), func_n5o(hash_join, 3));
  reg_fun(intern(lit("hash-diff"), user_package), func_n2(hash_diff));
  reg_fun(intern(lit("hash-symdiff"), user_package), func_n2(hash_symdiff));
  reg_fun(intern(lit("hash-isec"), user_package), func_n3o(hash_isec, 2));
  reg_fun(intern(lit("hash-subset"), user_package), func_n2(hash_subset));
  reg_fun(intern(lit("hash-proper-subset"), user_package), func_n2(hash_proper_subset));
  reg_fun(intern(lit("group-by"), user_package), func_n2v(group_by));
  reg_fun(intern(lit("group-map"), user_package), func_n3v(group_map));
  reg_fun(intern(lit("group-reduce"), user_package),
          func_n6o(group_reduce, 4));
  reg_fun(intern(lit("hash-update"), user_package), func_n2(hash_update));
  reg_fun(intern(lit("hash-update-1"), user_package),
          func_n4o(hash_update_1, 3));
  reg_fun(intern(lit("hash-revget"), user_package), func_n4o(hash_revget, 2));
  reg_fun(intern(lit("hash-keys-of"), user_package), func_n4o(hash_keys_of, 2));
  reg_fun(intern(lit("hash-invert"), user_package), func_n3ov(hash_invert, 1));
  reg_fun(intern(lit("hash-begin"), user_package), func_n1(hash_begin));
  reg_fun(intern(lit("hash-next"), user_package), func_n1(hash_next));
  reg_fun(intern(lit("hash-peek"), user_package), func_n1(hash_peek));
  reg_fun(intern(lit("hash-reset"), user_package), func_n2(hash_reset));
  reg_fun(intern(lit("set-hash-traversal-limit"), system_package),
          func_n1(set_hash_traversal_limit));
  reg_fun(intern(lit("gen-hash-seed"), user_package), func_n0(gen_hash_seed));
}
