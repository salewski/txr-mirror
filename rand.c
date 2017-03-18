/* Copyright 2010-2017
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

#include <string.h>
#include <wctype.h>
#include <stdarg.h>
#include <wchar.h>
#include <limits.h>
#include <signal.h>
#include "config.h"
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#include "lib.h"
#include "signal.h"
#include "unwind.h"
#include "arith.h"
#include "rand.h"
#include "eval.h"

#define random_warmup (deref(lookup_var_l(nil, random_warmup_s)))

#if SIZEOF_INT == 4
typedef unsigned int rand32_t;
#elif SIZEOF_LONG == 4
typedef unsigned long rand32_t;
#endif

/*
 * The algorithm here is WELL 512.
 * (Francois Panneton, Pierre L'Ecuyer.)
 */
struct rand_state {
  rand32_t state[16];
  unsigned cur;
};

val random_state_s, random_state_var_s, random_warmup_s;

static struct cobj_ops random_state_ops = cobj_ops_init(eq,
                                                        cobj_print_op,
                                                        cobj_destroy_free_op,
                                                        cobj_mark_op,
                                                        cobj_hash_op);

/* Source: bits from /dev/random on a Linux server */
static rand32_t rand_tab[16] = {
  0x2C272ED6U, 0x4DBD5D69U, 0xC5482819U, 0x142AFCDEU,
  0xF7ABAEB0U, 0x454B47F1U, 0xFC85D2ADU, 0x1A9DB177U,
  0x2619231BU, 0x6B678AE8U, 0xAC450E78U, 0xA0A96B1CU,
  0x88A74E05U, 0xC1CBAEC2U, 0x8170BEADU, 0x29FAF776U
};

static val make_state(void)
{
  struct rand_state *r = coerce(struct rand_state *, chk_malloc(sizeof *r));
  return cobj(coerce(mem_t *, r), random_state_s, &random_state_ops);
}

val random_state_p(val obj)
{
  return typeof(obj) == random_state_s ? t : nil;
}

INLINE rand32_t *rstate(struct rand_state *r, int offs)
{
  return &r->state[(r->cur + offs) % 16];
}

static rand32_t rand32(struct rand_state *r)
{
  rand32_t s0 = *rstate(r, 0);
  rand32_t s9 = *rstate(r, 9);
  rand32_t s13 = *rstate(r, 13);
  rand32_t s15 = *rstate(r, 15);

  rand32_t r1 = s0 ^ (s0 << 16) ^ s13 ^ (s13 << 15);
  rand32_t r2 = s9 ^ (s9 >> 11);

  rand32_t ns0 = *rstate(r, 0) = r1 ^ r2;
  rand32_t ns15 = s15 ^ (s15 << 2) ^ r1 ^ (r1 << 18) ^ r2 ^ (r2 << 28) ^
                  ((ns0 ^ (ns0 << 5)) & 0xda442d24ul);

  *rstate(r, 15) = ns15;
  r->cur = (r->cur + 15) % 16;
  return ns15;
}

val make_random_state(val seed, val warmup)
{
  val rs = make_state();
  int i = 0;
  struct rand_state *r = coerce(struct rand_state *,
                                cobj_handle(rs, random_state_s));

  seed = default_null_arg(seed);
  warmup = default_null_arg(warmup);

  if (bignump(seed)) {
    int dig, bit;
    mp_int *m = mp(seed);

    for (i = 0, dig = 0, bit = 0; i < 16 && dig < m->used; i++) {
      r->state[i] = (m->dp[dig] >> bit) & 0xFFFFFFFFul;
      bit += 32;
      if (bit >= MP_DIGIT_BIT)
        dig++, bit = 0;
    }
  } else if (fixnump(seed)) {
    cnum s = c_num(seed) & NUM_MAX;

    r->state[0] = s & 0xFFFFFFFFul;
    i = 1;
#if SIZEOF_PTR == 8
    s >>= 32;
    r->state[1] = s & 0xFFFFFFFFul;
    i = 2;
#elif SIZEOF_PTR > 8
#error port me!
#endif
  } else if (nilp(seed)) {
    val time = time_sec_usec();
    r->state[0] = convert(rand32_t, c_num(car(time)));
    r->state[1] = convert(rand32_t, c_num(cdr(time)));
#if HAVE_UNISTD_H
    r->state[2] = convert(rand32_t, getpid());
    i = 3;
#else
    i = 2;
#endif
  } else if (random_state_p(seed)) {
    struct rand_state *rseed = coerce(struct rand_state *,
                                      cobj_handle(seed, random_state_s));
    *r = *rseed;
    return rs;
  } else if (vectorp(seed)) {
    if (length(seed) < num_fast(17))
      uw_throwf(error_s, lit("make-random-state: vector ~s too short"),
                seed, nao);

    for (i = 0; i < 16; i++)
      r->state[i] = c_uint_ptr_num(seed->v.vec[i]);

    r->cur = c_num(seed->v.vec[i]);
    return rs;
  } else {
    uw_throwf(error_s, lit("make-random-state: seed ~s is not a number"),
              seed, nao);
  }

  while (i > 0 && r->state[i - 1] == 0)
    i--;

  for (; i < 16; i++)
    r->state[i] = rand_tab[i];

  r->cur = 0;

  {
    uses_or2;
    cnum wu = c_num(or2(warmup, random_warmup));

    for (i = 0; i < wu; i++)
      (void) rand32(r);
  }

  return rs;
}

val random_state_get_vec(val state)
{
  struct rand_state *r = coerce(struct rand_state *,
                                cobj_handle(default_arg(state, random_state),
                                            random_state_s));
  int i;
  val vec = vector(num_fast(17), nil);

  for (i = 0; i < 16; i++)
    vec->v.vec[i] = normalize(bignum_from_uintptr(r->state[i]));

  vec->v.vec[i] = num(r->cur);

  return vec;
}

val random_fixnum(val state)
{
  struct rand_state *r = coerce(struct rand_state *,
                                cobj_handle(default_arg(state, random_state),
                                            random_state_s));
  return num(rand32(r) & NUM_MAX);
}

val random(val state, val modulus)
{
  struct rand_state *r = coerce(struct rand_state *,
                                cobj_handle(state, random_state_s));

  if (bignump(modulus)) {
    mp_int *m = mp(modulus);
    int bits = mp_count_bits(m) - mp_is_pow_two(m);
    int rands_needed = (bits + 32 - 1) / 32;
    int msb_rand_bits = bits % 32;
    rand32_t msb_rand_mask = convert(rand32_t, -1) >> (32 - msb_rand_bits);
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
    if (m == 1) {
      return zero;
    } else if (m > 1) {
      int bits = highest_bit(m - 1);
#if SIZEOF_PTR >= 8
      int rands_needed = (bits + 32 - 1) / 32;
#endif
      int msb_rand_bits = bits % 32;
      rand32_t msb_rand_mask = convert(rand32_t, -1) >> (32 - msb_rand_bits);
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
  }

  uw_throwf(numeric_error_s, lit("random: invalid modulus ~s"),
      modulus, nao);
}

val rnd(val modulus, val state)
{
  state = default_arg(state, random_state);
  return random(state, modulus);
}

void rand_compat_fixup(int compat_ver)
{
  if (compat_ver <= 139) {
    loc l = lookup_var_l(nil, random_state_var_s);
    memset(rand_tab, 0xAA, sizeof rand_tab);
    if (compat_ver <= 114)
      random_state_s = random_state_var_s;
    set(l, make_random_state(num_fast(42), num_fast(8)));
  }
}

void rand_init(void)
{
  random_state_var_s = intern(lit("*random-state*"), user_package);
  random_state_s = intern(lit("random-state"), user_package);
  random_warmup_s = intern(lit("*random-warmup*"), user_package);

  reg_var(random_state_var_s, make_random_state(num_fast(42), num_fast(8)));
  reg_var(random_warmup_s, num_fast(8));

  reg_fun(intern(lit("make-random-state"), user_package),
          func_n2o(make_random_state, 0));
  reg_fun(intern(lit("random-state-get-vec"), user_package),
          func_n1o(random_state_get_vec, 0));
  reg_fun(intern(lit("random-state-p"), user_package), func_n1(random_state_p));
  reg_fun(intern(lit("random-fixnum"), user_package), func_n1o(random_fixnum, 0));
  reg_fun(intern(lit("random"), user_package), func_n2(random));
  reg_fun(intern(lit("rand"), user_package), func_n2o(rnd, 1));
}
