/* Copyright 2012
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
#include <string.h>
#include <dirent.h>
#include <stdarg.h>
#include <stdlib.h>
#include <assert.h>
#include <setjmp.h>
#include <limits.h>
#include "config.h"
#include "lib.h"
#include "gc.h"
#include "unwind.h"
#include "stream.h"
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
  cnum (*hash_fun)(val);
  val (*assoc_fun)(val key, val list);
  val *(*acons_new_l_fun)(val key, val *new_p, val *list);
};

struct hash_iter {
  val hash;
  cnum chain;
  val cons;
};

val weak_keys_k, weak_vals_k, equal_based_k;

/*
 * Dynamic list built up during gc.
 */
static struct hash *reachable_weak_hashes;

/*
 * Cache of equal hashing function
 * These variables are deliberately not GC-protected.
 */
static val last_equal_key;
static cnum last_equal_hash = NUM_MAX;

/*
 * This is is an adaptation of hashpjw, from Compilers: Principles, Techniques
 * and Tools, Aho, Sethi, Ulman, 1988. P. 436.  The register is wider by
 * a few bits, and we bring down five overflow bits instead of four.
 * We don't reduce the final result modulo a small prime, but leave it
 * as it is; let the hashing routines do their own reduction.
 */
static unsigned long hash_c_str(const wchar_t *str)
{
  unsigned long h = 0;
  while (*str) {
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

  mem_t *p = (mem_t *) &n, *q = p + sizeof(double);

  while (p < q) {
    h = h << 8 | h >> (8 * sizeof h - 1);
    h += *p++;
  }

  return h & NUM_MAX;
}

static cnum equal_hash(val obj)
{
  if (obj == last_equal_key)
    return last_equal_hash;

  last_equal_key = obj;

  switch (type(obj)) {
  case NIL:
    return last_equal_hash = NUM_MAX;
  case LIT:
    return last_equal_hash = hash_c_str(litptr(obj)) & NUM_MAX;
  case CONS:
    return last_equal_hash = (equal_hash(obj->c.car) +
                              equal_hash(obj->c.cdr)) & NUM_MAX;
  case STR:
    return last_equal_hash = hash_c_str(obj->st.str) & NUM_MAX;
  case CHR:
    return last_equal_hash = c_chr(obj) & NUM_MAX;
  case NUM:
    return last_equal_hash = c_num(obj) & NUM_MAX;
  case SYM:
  case PKG:
  case ENV:
    switch (sizeof (mem_t *)) {
    case 4:
      return last_equal_hash = (((cnum) obj) >> 4) & NUM_MAX;
    case 8: default:
      return last_equal_hash = (((cnum) obj) >> 5) & NUM_MAX;
    }
    break;
  case FUN:
    return last_equal_hash = ((cnum) obj->f.f.interp_fun +
                              equal_hash(obj->f.env)) & NUM_MAX;
  case VEC:
    {
      val length = obj->v.vec[vec_length];
      cnum i, h = equal_hash(obj->v.vec[vec_length]);
      cnum len = c_num(length);

      for (i = 0; i < len; i++)
        h = (h + equal_hash(obj->v.vec[i])) & NUM_MAX;

      return last_equal_hash = h;
    }
  case LCONS:
    return (equal_hash(car(obj)) + equal_hash(cdr(obj))) & NUM_MAX;
  case LSTR:
    lazy_str_force(obj);
    return equal_hash(obj->ls.prefix);
  case BGNUM:
    return last_equal_hash = mp_hash(mp(obj)) & NUM_MAX;
  case FLNUM:
    return last_equal_hash = hash_double(obj->fl.n);
  case COBJ:
    return last_equal_hash = obj->co.ops->hash(obj) & NUM_MAX;
  }

  internal_error("unhandled case in equal function");
}

static cnum eql_hash(val obj)
{
  switch (tag(obj)) {
  case TAG_PTR:
    switch (type(obj)) {
    case NIL:
      return NUM_MAX;
    case BGNUM:
      if (obj == last_equal_key)
        return last_equal_hash;
      return last_equal_hash = mp_hash(mp(obj)) & NUM_MAX;
    case FLNUM:
      if (obj == last_equal_key)
        return last_equal_hash;
      return last_equal_hash = hash_double(obj->fl.n);
    default:
      switch (sizeof (mem_t *)) {
      case 4:
        return (((cnum) obj) >> 4) & NUM_MAX;
      case 8: default:
        return (((cnum) obj) >> 5) & NUM_MAX;
      }
    }
  case TAG_CHR:
    return c_chr(obj) & NUM_MAX;
  case TAG_NUM:
    return c_num(obj) & NUM_MAX;
  case TAG_LIT:
    switch (sizeof (mem_t *)) {
    case 4:
      return (((cnum) obj) >> 2) & NUM_MAX;
    case 8: default:
      return (((cnum) obj) >> 3) & NUM_MAX;
    }
  }
  /* notreached */
  abort();
}

cnum cobj_hash_op(val obj)
{
  switch (sizeof (mem_t *)) {
  case 4:
    return (((cnum) obj) >> 4) & NUM_MAX;
  case 8: default:
    return (((cnum) obj) >> 5) & NUM_MAX;
  }
  /* notreached */
  abort();
}

static val print_key_val(val out, val key, val value)
{
  format(out, lit(" (~s ~s)"), key, value, nao);
  return nil;
}

static void hash_print_op(val hash, val out)
{
  struct hash *h = (struct hash *) hash->co.handle;
  int need_space = 0;

  put_string(lit("#H(("), out);
  if (h->hash_fun == equal_hash) {
    obj_print(equal_based_k, out);
    need_space = 1;
  }
  if (h->flags != hash_weak_none) {
    if (need_space)
      put_string(lit(" "), out);
    switch (h->flags) {
    case hash_weak_both:
      obj_print(weak_keys_k, out);
      /* fallthrough */
    case hash_weak_vals:
      obj_print(weak_vals_k, out);
      break;
    case hash_weak_keys:
      obj_print(weak_keys_k, out);
      break;
    default:
      break;
    }
  }
  put_string(lit(")"), out);
  maphash(curry_123_23(func_n3(print_key_val), out), hash);
  put_string(lit(")"), out);
}

static void hash_mark(val hash)
{
  struct hash *h = (struct hash *) hash->co.handle;
  cnum i;

  gc_mark(h->userdata);

  switch (h->flags) {
  case hash_weak_none:
    /* If the hash is not weak, we can simply mark the table
       vector and we are done. */
    gc_mark(h->table);
    break;
  case hash_weak_keys:
    /* Keys are weak: mark the values only. */
    for (i = 0; i < h->modulus; i++) {
      val ind = num(i);
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
      val ind = num(i);
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
    break;
  }
}

static struct cobj_ops hash_ops = {
  cobj_equal_op,
  hash_print_op,
  cobj_destroy_free_op,
  hash_mark,
  cobj_hash_op
};

static void hash_grow(struct hash *h)
{
  cnum i;
  cnum new_modulus = 2 * h->modulus;
  val new_table = vector(num(new_modulus));

  bug_unless (new_modulus > h->modulus);

  for (i = 0; i < h->modulus; i++) {
    val conses = vecref(h->table, num(i));

    while (conses) {
      val entry = car(conses);
      val next = cdr(conses);
      val key = car(entry);
      val *pchain = vecref_l(new_table, num(h->hash_fun(key) % new_modulus));
      *cdr_l(conses) = *pchain;
      *pchain = conses;
      conses = next;
    }
  }

  h->modulus = new_modulus;
  set(h->table, new_table);
}

val make_hash(val weak_keys, val weak_vals, val equal_based)
{
  int flags = ((weak_vals != nil) << 1) | (weak_keys != nil);
  struct hash *h = (struct hash *) chk_malloc(sizeof *h);
  val mod = num(256);
  val table = vector(mod);
  val hash = cobj((mem_t *) h, hash_s, &hash_ops);

  h->flags = (hash_flags_t) flags;
  h->modulus = c_num(mod);
  h->count = 0;
  h->table = table;
  h->userdata = nil;

  h->hash_fun = equal_based ? equal_hash : eql_hash;
  h->assoc_fun = equal_based ? assoc : assq;
  h->acons_new_l_fun = equal_based ? acons_new_l : aconsq_new_l;

  return hash;
}

val *gethash_l(val hash, val key, val *new_p)
{
  struct hash *h = (struct hash *) cobj_handle(hash, hash_s);
  val *pchain = vecref_l(h->table, num(h->hash_fun(key) % h->modulus));
  val old = *pchain;
  val *place = h->acons_new_l_fun(key, new_p, pchain);
  if (old != *pchain && ++h->count > 2 * h->modulus)
    hash_grow(h);
  return place;
}

val gethash(val hash, val key)
{
  struct hash *h = (struct hash *) cobj_handle(hash, hash_s);
  val chain = vecref(h->table, num(h->hash_fun(key) % h->modulus));
  val found = h->assoc_fun(key, chain);
  return cdr(found);
}

val gethash_f(val hash, val key, val *found)
{
  struct hash *h = (struct hash *) cobj_handle(hash, hash_s);
  val chain = vecref(h->table, num(h->hash_fun(key) % h->modulus));
  set(*found, h->assoc_fun(key, chain));
  return cdr(*found);
}

val gethash_n(val hash, val key, val notfound_val)
{
  struct hash *h = (struct hash *) cobj_handle(hash, hash_s);
  val chain = vecref(h->table, num(h->hash_fun(key) % h->modulus));
  val existing = h->assoc_fun(key, chain);
  return if3(existing, cdr(existing), notfound_val);
}

val sethash(val hash, val key, val value)
{
  val new_p;
  set(*gethash_l(hash, key, &new_p), value);
  return new_p;
}

val pushhash(val hash, val key, val value)
{
  val new_p;
  mpush(value, *gethash_l(hash, key, &new_p));
  return new_p;
}

val remhash(val hash, val key)
{
  struct hash *h = (struct hash *) cobj_handle(hash, hash_s);
  val *pchain = vecref_l(h->table, num(h->hash_fun(key) % h->modulus));
  val existing = h->assoc_fun(key, *pchain);

  if (existing) {
    val loc = memq(existing, *pchain);
    *pchain = nappend2(ldiff(*pchain, loc), cdr(loc));
    h->count--;
    bug_unless (h->count >= 0);
  }

  return nil;
}

val hash_count(val hash)
{
  struct hash *h = (struct hash *) cobj_handle(hash, hash_s);
  return num(h->count);
}

val get_hash_userdata(val hash)
{
  struct hash *h = (struct hash *) cobj_handle(hash, hash_s);
  return h->userdata;
}

val set_hash_userdata(val hash, val data)
{
  struct hash *h = (struct hash *) cobj_handle(hash, hash_s);
  val olddata = h->userdata;
  set(h->userdata, data);
  return olddata;
}

val hashp(val obj)
{
  return typeof(obj) == hash_s ? t : nil;
}

static void hash_iter_mark(val hash_iter)
{
  struct hash_iter *hi = (struct hash_iter *) hash_iter->co.handle;
  gc_mark(hi->hash);
  gc_mark(hi->cons);
}

static struct cobj_ops hash_iter_ops = {
  cobj_equal_op,
  cobj_print_op,
  cobj_destroy_free_op,
  hash_iter_mark,
  cobj_hash_op
};

val hash_begin(val hash)
{
  val hi_obj;
  struct hash_iter *hi;
  class_check (hash, hash_s);

  hi = (struct hash_iter *) chk_malloc(sizeof *hi);
  hi->hash = nil;
  hi->chain = -1;
  hi->cons = nil;
  hi_obj = cobj((mem_t *) hi, hash_iter_s, &hash_iter_ops);
  hi->hash = hash;
  return hi_obj;
}

val hash_next(val *iter)
{
  struct hash_iter *hi = (struct hash_iter *) cobj_handle(*iter, hash_iter_s);
  val hash = hi->hash;
  struct hash *h = (struct hash *) hash->co.handle;
  if (hi->cons)
    hi->cons = cdr(hi->cons);
  while (nullp(hi->cons)) {
    if (++hi->chain >= h->modulus) {
      *iter = nil;
      return nil;
    }
    set(hi->cons, vecref(h->table, num(hi->chain)));
  }
  return car(hi->cons);
}

val maphash(val fun, val hash)
{
  val iter = hash_begin(hash);
  val cell;
  while ((cell = hash_next(&iter)) != nil)
    funcall2(fun, car(cell), cdr(cell));
  return nil;
}

val hash_eql(val obj)
{
  return num(eql_hash(obj));
}

val hash_equal(val obj)
{
  return num(equal_hash(obj));
}

/*
 * Called from garbage collector. Hash module must process all weak tables
 * that were visited during the marking phase, maintained in the list
 * reachable_weak_hashes.
 */
void hash_process_weak(void)
{
  struct hash *h;
  cnum i;

  /*
   * First lapse the equal cache. We can do this unconditionally.
   */
  last_equal_key = nil;
  last_equal_hash = NUM_MAX;

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
        val ind = num(i);
        val *pchain = vecref_l(h->table, ind);
        val *iter;

        for (iter = pchain; !gc_is_reachable(*iter); ) {
          val entry = car(*iter);
          if (!gc_is_reachable(entry) && !gc_is_reachable(car(entry)))
            *iter = cdr(*iter);
          else
            iter = cdr_l(*iter);
        }
      }
      /* Garbage is gone now. Seal things by marking the vector. */
      gc_mark(h->table);
      break;
    case hash_weak_vals:
      /* Sweep through all entries. Delete any which have values
         that are garbage. */
      for (i = 0; i < h->modulus; i++) {
        val ind = num(i);
        val *pchain = vecref_l(h->table, ind);
        val *iter;

        for (iter = pchain; !gc_is_reachable(*iter); ) {
          val entry = car(*iter);
          if (!gc_is_reachable(entry) && !gc_is_reachable(cdr(entry)))
            *iter = cdr(*iter);
          else
            iter = cdr_l(*iter);
        }
      }
      /* Garbage is gone now. Seal things by marking the vector. */
      gc_mark(h->table);
      break;
    case hash_weak_both:
      /* Sweep through all entries. Delete any which have keys
         or values that are garbage. */
      for (i = 0; i < h->modulus; i++) {
        val ind = num(i);
        val *pchain = vecref_l(h->table, ind);
        val *iter;

        for (iter = pchain; !gc_is_reachable(*iter); ) {
          val entry = car(*iter);
          if (!gc_is_reachable(entry) &&
              (!gc_is_reachable(car(entry)) || !gc_is_reachable(cdr(entry))))
            *iter = cdr(*iter);
          else
            iter = cdr_l(*iter);
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

val hashv(val args)
{
  val wkeys = memq(weak_keys_k, args);
  val wvals = memq(weak_vals_k, args);
  val equal = memq(equal_based_k, args);
  return make_hash(wkeys, wvals, equal);
}

val hash_construct(val hashv_args, val pairs)
{
  val hash = hashv(hashv_args);

  for (; pairs; pairs = cdr(pairs)) {
    val pair = car(pairs);
    sethash(hash, first(pair), second(pair));
  }

  return hash;
}

void hash_init(void)
{
  weak_keys_k = intern(lit("weak-keys"), keyword_package);
  weak_vals_k = intern(lit("weak-vals"), keyword_package);
  equal_based_k = intern(lit("equal-based"), keyword_package);
}
