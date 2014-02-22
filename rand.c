/* Copyright 2010-2014
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
#include <wchar.h>
#include <limits.h>
#include <time.h>
#include <signal.h>
#include "config.h"
#include "lib.h"
#include "signal.h"
#include "unwind.h"
#include "gc.h"
#include "arith.h"
#include "rand.h"

#if SIZEOF_INT == 4
typedef unsigned int rand32_t;
#elif SIZEOF_LONG == 4
typedef unsigned long rand32_t;
#endif

/*
 * The algorithm here is WELL 512.
 * (Francois Panneton, Pierre L'Ecuyer.)
 */
struct random_state {
  rand32_t state[16];
  int cur;
};

val random_state;
val random_state_s;

static struct cobj_ops random_state_ops = {
  cobj_equal_op,
  cobj_print_op,
  cobj_destroy_free_op,
  cobj_mark_op,
  cobj_hash_op 
};

static val make_state(void)
{
  struct random_state *r = (struct random_state *) chk_malloc(sizeof *r);
  return cobj((mem_t *) r, random_state_s, &random_state_ops);
}

val random_state_p(val obj)
{
  return typeof(obj) == random_state_s ? t : nil;
}

static rand32_t rand32(struct random_state *r)
{
  #define RSTATE(r,i) ((r)->state[((r)->cur + i) % 16])
  rand32_t s0 = RSTATE(r, 0);
  rand32_t s9 = RSTATE(r, 9);
  rand32_t s13 = RSTATE(r, 13);
  rand32_t s15 = RSTATE(r, 15);

  rand32_t r1 = s0 ^ (s0 << 16) ^ s13 ^ (s13 << 15);
  rand32_t r2 = s9 ^ (s9 >> 11);

  rand32_t ns0 = RSTATE(r, 0) = r1 ^ r2;
  rand32_t ns15 = s15 ^ (s15 << 2) ^ r1 ^ (r1 << 18) ^ r2 ^ (r2 << 28) ^ 
                  ((ns0 ^ (ns0 << 5)) & 0xda442d24ul);

  RSTATE(r, 15) = ns15;
  r->cur = (r->cur + 15) % 16;
  return ns15;
  #undef RSTATE
}

val make_random_state(val seed)
{
  val rs = make_state();
  int i;
  struct random_state *r = (struct random_state *) 
                             cobj_handle(rs, random_state_s);

  r->cur = 0;

  if (bignump(seed)) {
    int i, dig, bit;
    mp_int *m = mp(seed);

    for (i = 0, dig = 0, bit = 0; i < 16 && dig < m->used; i++) {
      r->state[i] = (m->dp[dig] >> bit) & 0xFFFFFFFFul;
      bit += 32;
      if (bit >= MP_DIGIT_BIT)
        dig++, bit = 0;
    }

    for (; i < 16; i++) {
      r->state[i] = 0xAAAAAAAA;
    }
  } else if (fixnump(seed)) {
    cnum s = c_num(seed) & NUM_MAX;

    memset(r->state, 0xAA, sizeof r->state);
    r->state[0] = s & 0xFFFFFFFFul;
#if SIZEOF_PTR >= 8
    r->state[1] = (s >> 32) & 0xFFFFFFFFul;
#elif SIZEOF_PTR >= 16
    r->state[2] = (s >> 64) & 0xFFFFFFFFul;
    r->state[3] = (s >> 96) & 0xFFFFFFFFul;
#endif
  } else if (nilp(seed)) {
    val time = time_sec_usec();
    r->state[0] = (rand32_t) c_num(car(time));
    r->state[1] = (rand32_t) c_num(cdr(time));
    memset(r->state + 2, 0xAA, sizeof r->state - 2 * sizeof r->state[0]);
  } else if (random_state_p(seed)) {
    struct random_state *rseed = (struct random_state *) 
                                   cobj_handle(seed, random_state_s);
    *r = *rseed;
  } else {
    uw_throwf(error_s, lit("make-random-state: seed ~s is not a number"),
              seed, nao);
  }

  for (i = 0; i < 8; i++)
    (void) rand32(r);

  return rs;
}

val random_fixnum(val state)
{
  uses_or2;
  struct random_state *r = (struct random_state *) 
                             cobj_handle(or2(state, random_state),
                                         random_state_s);
  return num(rand32(r) & NUM_MAX);
}

val random(val state, val modulus)
{
  struct random_state *r = (struct random_state *) 
                             cobj_handle(random_state, random_state_s);

  if (bignump(modulus)) {
    mp_int *m = mp(modulus);
    int bits = mp_count_bits(m);
    int rands_needed = (bits + 32 - 1) / 32;
    int msb_rand_bits = bits % 32;
    rand32_t msb_rand_mask = ((rand32_t) -1) >> (32 - msb_rand_bits);
    val out = make_bignum();
    mp_int *om = mp(out);

    for (;;) {
      int i;
      for (i = 0; i < rands_needed; i++) {
        rand32_t rnd = rand32(r);
#if MP_DIGIT_SIZE >= 4
        if (i > 0)
          mp_mul_2d(om, 32, om);
        else
          rnd &= msb_rand_mask;
        mp_add_d(om, rnd, om);
#else
        if (i > 0)
          mp_mul_2d(om, 16, om);
        else
          rnd &= msb_rand_mask;
        mp_add_d(om, rnd & 0xFFFF, om);
        mp_mul_2d(om, 16, om);
        mp_add_d(om, rnd >> 16, om);
#endif
      }
      if (mp_cmp(om, m) != MP_LT) {
        mp_zero(om);
        continue;
      }
      break;
    }

    return normalize(out);
  } else if (fixnump(modulus)) {
    cnum m = c_num(modulus);
    int bits = highest_bit(m);
#if SIZEOF_PTR >= 8
    int rands_needed = (bits + 32 - 1) / 32;
#endif
    int msb_rand_bits = bits % 32;
    rand32_t msb_rand_mask = ((rand32_t) -1) >> (32 - msb_rand_bits);
    if (m <= 0)
      goto invalid;
    for (;;) {
      cnum out = 0;
#if SIZEOF_PTR >= 8
      int i;

      for (i = 0; i < rands_needed; i++) {
        rand32_t rnd = rand32(r);
        out <<= 32;
        if (i == 0)
          rnd &= msb_rand_mask;
        out |= rnd;
      }
#else
      out = rand32(r) & msb_rand_mask;
#endif
      if (out >= m)
        continue;
      return num(out);
    }
  } 
invalid:
  uw_throwf(numeric_error_s, lit("random: invalid modulus ~s"),
      modulus, nao);
}

val rnd(val modulus, val state)
{
  state = default_arg(state, random_state);
  return random(state, modulus);
}

void rand_init(void)
{
  prot1(&random_state);
  random_state_s = intern(lit("random-state"), user_package);
  random_state = make_random_state(num(42));
}
