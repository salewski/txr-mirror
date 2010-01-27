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
};

/*
 * Dynamic list built up during gc.
 */
static struct hash *reachable_weak_hashes;

/*
 * This is is an adaptation of hashpjw, from Compilers: Principles, Techniques
 * and Tools, Aho, Sethi, Ulman, 1988. P. 436.  The register is wider by
 * a few bits, and we bring down five overflow bits instead of four.
 * We don't reduce the final result modulo a small prime, but leave it
 * as it is; let the hashing routines do their own reduction.
 */
static long hash_c_str(const wchar_t *str)
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

static cnum ll_hash(val obj)
{
  if (obj == nil)
    return NUM_MAX;

  switch (type(obj)) {
  case LIT:
    return hash_c_str(litptr(obj));
  case CONS:
    return (ll_hash(obj->c.car) + ll_hash(obj->c.cdr)) & NUM_MAX;
  case STR:
    return hash_c_str(obj->st.str);
  case CHR:
    return c_chr(obj) + NUM_MAX / 2;
  case NUM:
    return c_num(obj) & NUM_MAX;
  case SYM:
  case PKG:
    return ((cnum) obj) & NUM_MAX;
  case FUN:
    return ((cnum) obj->f.f.interp_fun + ll_hash(obj->f.env)) & NUM_MAX;
  case VEC:
    {
      val fill = obj->v.vec[vec_fill];
      cnum i, h = ll_hash(obj->v.vec[vec_fill]);
      cnum len = c_num(fill);

      for (i = 0; i < len; i++)
        h = (h + ll_hash(obj->v.vec[i])) & NUM_MAX;

      return h;
    }
  case LCONS:
    return (ll_hash(car(obj)) + ll_hash(cdr(obj))) & NUM_MAX;
  case LSTR:
    lazy_str_force(obj);
    return ll_hash(obj->ls.prefix);
  case COBJ:
    return obj->co.ops->hash(obj);
  }

  internal_error("unhandled case in equal function");
}

cnum cobj_hash_op(val obj)
{
  return ((cnum) obj) & NUM_MAX;
}

val hash_obj(val obj)
{
  return num(ll_hash(obj));
}

static void hash_destroy(val hash)
{
  free(hash->co.handle);
}

static void hash_mark(val hash)
{
  struct hash *h = (struct hash *) hash->co.handle;
  cnum i;

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
      val *pchain = vecref_l(h->table, ind);
      val iter;

      for (iter = *pchain; iter != nil; iter = cdr(iter)) {
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
      val *pchain = vecref_l(h->table, ind);
      val iter;

      for (iter = *pchain; iter != nil; iter = cdr(iter)) {
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
  cobj_print_op,
  hash_destroy,
  hash_mark,
  cobj_hash_op
};

static void hash_grow(struct hash *h)
{
  cnum i;
  cnum new_modulus = 2 * h->modulus;
  val new_table = vector(num(new_modulus));

  bug_unless (new_modulus > h->modulus);

  vec_set_fill(new_table, num(new_modulus));

  for (i = 0; i < h->modulus; i++) {
    val conses = *vecref_l(h->table, num(i));

    while (conses) {
      val entry = car(conses);
      val next = cdr(conses);
      val key = car(entry);
      val *pchain = vecref_l(new_table,
                                num(ll_hash(key) % new_modulus));
      *cdr_l(conses) = *pchain;
      *pchain = conses;
      conses = next;
    }
  }

  h->modulus = new_modulus;
  h->table = new_table;
}

val make_hash(val weak_keys, val weak_vals)
{
  int flags = ((weak_vals != nil) << 1) | (weak_keys != nil);
  struct hash *h = (struct hash *) chk_malloc(sizeof *h);
  val mod = num(256);
  val table = vector(mod);
  val hash = cobj((mem_t *) h, hash_s, &hash_ops);

  vec_set_fill(table, mod);

  h->flags = (hash_flags_t) flags;
  h->modulus = c_num(mod);
  h->count = 0;
  h->table = table;

  return hash;
}

val *gethash_l(val hash, val key, val *new_p)
{
  struct hash *h = (struct hash *) hash->co.handle;
  val *pchain = vecref_l(h->table, num(ll_hash(key) % h->modulus));
  val old = *pchain;
  val *place = acons_new_l(pchain, key, new_p);
  if (old != *pchain && ++h->count > 2 * h->modulus)
    hash_grow(h);
  return place;
}

val gethash(val hash, val key)
{
  struct hash *h = (struct hash *) hash->co.handle;
  val chain = *vecref_l(h->table, num(ll_hash(key) % h->modulus));
  val found = assoc(chain, key);
  return cdr(found);
}

val sethash(val hash, val key, val value)
{
  val new_p;
  *gethash_l(hash, key, &new_p) = value;
  return new_p;
}

val remhash(val hash, val key)
{
  struct hash *h = (struct hash *) hash->co.handle;
  val *pchain = vecref_l(h->table, num(ll_hash(key) % h->modulus));
  *pchain = alist_remove1(*pchain, key);
  h->count--;
  bug_unless (h->count >= 0);
  return nil;
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
          if (!gc_is_reachable(entry) && !gc_is_reachable(car(entry)))
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

void hash_init(void)
{
}
