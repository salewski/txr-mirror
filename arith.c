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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <wctype.h>
#include <stdarg.h>
#include <wchar.h>
#include <math.h>
#include <signal.h>
#include <ctype.h>
#include <float.h>
#include "config.h"
#include "lib.h"
#include "signal.h"
#include "unwind.h"
#include "gc.h"
#include "args.h"
#include "eval.h"
#include "arith.h"

#define TAG_PAIR(A, B) ((A) << TAG_SHIFT | (B))
#define NOOP(A, B)
#define CNUM_BIT ((int) sizeof (cnum) * CHAR_BIT)
#define ABS(A) ((A) < 0 ? -(A) : (A))

static mp_int NUM_MAX_MP, INT_PTR_MAX_MP, UINT_PTR_MAX_MP;

val make_bignum(void)
{
  val n = make_obj();
  n->bn.type = BGNUM;
  mp_init(&n->bn.mp);
  return n;
}

static val make_ubignum(void)
{
  val n = make_obj();
  n->bn.type = BGNUM;
  mp_init_minimal(&n->bn.mp);
  return n;
}

val bignum(cnum cn)
{
  val n = make_bignum();
  mp_set_intptr(mp(n), cn);
  return n;
}

val bignum_from_long(long l)
{
#if SIZEOF_LONG <= SIZEOF_PTR
  return bignum(l);
#else
  val n = make_bignum();
  mp_set_int(mp(n), l);
  return n;
#endif
}

val bignum_from_uintptr(uint_ptr_t u)
{
  val n = make_bignum();
  mp_set_uintptr(mp(n), u);
  return n;
}

val num_from_buffer(mem_t *buf, int bytes)
{
  val n = make_bignum();
  mp_read_unsigned_bin(mp(n), buf, bytes);
  return normalize(n);
}

int num_to_buffer(val num, mem_t *buf, int bytes)
{
  switch (type(num)) {
  case CHR: case NUM:
    {
      cnum n = coerce(cnum, num) >> TAG_SHIFT;
      mem_t *ptr = buf + bytes;

      for (; n != 0; n >>= 8) {
        if (ptr == buf)
          return 0;
        *--ptr = n & 0xff;
      }
      while (ptr > buf)
        *--ptr = 0;
    }
    return 1;
  case BGNUM:
    return mp_to_unsigned_buf(mp(num), buf, bytes) == MP_OKAY ? 1 : 0;
  default:
    type_mismatch(lit("~s is not an integer"), num, nao);
  }
}

#if HAVE_DOUBLE_INTPTR_T

static val bignum_dbl_ipt(double_intptr_t di)
{
  val n = make_bignum();
  mp_set_double_intptr(mp(n), di);
  return n;
}

#endif

val normalize(val bignum)
{
  if (mp_cmp_mag(mp(bignum), &NUM_MAX_MP) == MP_GT) {
    return bignum;
  } else {
    cnum fixnum;
    mp_get_intptr(mp(bignum), &fixnum);
    return num(fixnum);
  }
}

val in_int_ptr_range(val bignum)
{
  return (mp_cmp_mag(mp(bignum), &INT_PTR_MAX_MP) == MP_GT) ? nil : t;
}

static val in_uint_ptr_range(val bignum)
{
  return (mp_cmp_z(mp(bignum)) == MP_LT ||
          mp_cmp_mag(mp(bignum), &UINT_PTR_MAX_MP) == MP_GT) ? nil : t;
}

uint_ptr_t c_uint_ptr_num(val num)
{
  switch (type(num)) {
  case CHR: case NUM:
    {
      cnum n = coerce(cnum, num) >> TAG_SHIFT;
      if (n >= 0)
        return n;
    }
    goto range;
  case BGNUM:
    if (in_uint_ptr_range(num)) {
      uint_ptr_t out;
      mp_get_uintptr(mp(num), &out);
      return out;
    }
  range:
    uw_throwf(error_s, lit("~s is out of uint_ptr_t range"), num, nao);
  default:
    type_mismatch(lit("~s is not an integer"), num, nao);
  }
}

int highest_bit(int_ptr_t n)
{
#if SIZEOF_PTR == 8
  if (n & 0x7FFFFFFF00000000) {
    if (n & 0x7FFF000000000000) {
      if (n & 0x7F00000000000000) {
        if (n & 0x7000000000000000) {
          if (n & 0x4000000000000000)
            return 63;
          else
            return (n & 0x2000000000000000) ? 62 : 61;
        } else {
          if (n & 0x0C00000000000000)
            return (n & 0x0800000000000000) ? 60 : 59;
          else
            return (n & 0x0200000000000000) ? 58 : 57;
        }
      } else {
        if (n & 0x00F0000000000000) {
          if (n & 0x00C0000000000000)
            return (n & 0x0080000000000000) ? 56 : 55;
          else
            return (n & 0x0020000000000000) ? 54 : 53;
        } else {
          if (n & 0x000C000000000000)
            return (n & 0x0008000000000000) ? 52 : 51;
          else
            return (n & 0x0002000000000000) ? 50 : 49;
        }
      }
    } else {
      if (n & 0x0000FF0000000000) {
        if (n & 0x0000F00000000000) {
          if (n & 0x0000C00000000000)
            return (n & 0x0000800000000000) ? 48 : 47;
          else
            return (n & 0x0000200000000000) ? 46 : 45;
        } else {
          if (n & 0x00000C0000000000)
            return (n & 0x0000080000000000) ? 44 : 43;
          else
            return (n & 0x0000020000000000) ? 42 : 41;
        }
      } else {
        if (n & 0x000000F000000000) {
          if (n & 0x000000C000000000)
            return (n & 0x0000008000000000) ? 40 : 39;
          else
            return (n & 0x0000002000000000) ? 38 : 37;
        } else {
          if (n & 0x0000000C00000000)
            return (n & 0x0000000800000000) ? 36 : 35;
          else
            return (n & 0x0000000200000000) ? 34 : 33;
        }
      }
    }
  } else {
    if (n & 0x00000000FFFF0000) {
      if (n & 0x00000000FF000000) {
        if (n & 0x00000000F0000000) {
          if (n & 0x00000000C0000000)
            return (n & 0x0000000080000000) ? 32 : 31;
          else
            return (n & 0x0000000020000000) ? 30 : 29;
        } else {
          if (n & 0x000000000C000000)
            return (n & 0x0000000008000000) ? 28 : 27;
          else
            return (n & 0x0000000002000000) ? 26 : 25;
        }
      } else {
        if (n & 0x0000000000F00000) {
          if (n & 0x0000000000C00000)
            return (n & 0x0000000000800000) ? 24 : 23;
          else
            return (n & 0x0000000000200000) ? 22 : 21;
        } else {
          if (n & 0x00000000000C0000)
            return (n & 0x0000000000080000) ? 20 : 19;
          else
            return (n & 0x0000000000020000) ? 18 : 17;
        }
      }
    } else {
      if (n & 0x000000000000FF00) {
        if (n & 0x000000000000F000) {
          if (n & 0x000000000000C000)
            return (n & 0x0000000000008000) ? 16 : 15;
          else
            return (n & 0x0000000000002000) ? 14 : 13;
        } else {
          if (n & 0x0000000000000C00)
            return (n & 0x0000000000000800) ? 12 : 11;
          else
            return (n & 0x0000000000000200) ? 10 : 9;
        }
      } else {
        if (n & 0x00000000000000F0) {
          if (n & 0x00000000000000C0)
            return (n & 0x0000000000000080) ? 8 : 7;
          else
            return (n & 0x0000000000000020) ? 6 : 5;
        } else {
          if (n & 0x000000000000000C)
            return (n & 0x0000000000000008) ? 4 : 3;
          else
            return (n & 0x0000000000000002) ? 2 : (n ? 1 : 0);
        }
      }
    }
  }
#elif SIZEOF_PTR == 4
  if (n & 0x7FFF0000) {
    if (n & 0x7F000000) {
      if (n & 0x70000000) {
        if (n & 0x40000000)
          return 31;
        else
          return (n & 0x20000000) ? 30 : 29;
      } else {
        if (n & 0x0C000000)
          return (n & 0x08000000) ? 28 : 27;
        else
          return (n & 0x02000000) ? 26 : 25;
      }
    } else {
      if (n & 0x00F00000) {
        if (n & 0x00C00000)
          return (n & 0x00800000) ? 24 : 23;
        else
          return (n & 0x00200000) ? 22 : 21;
      } else {
        if (n & 0x000C0000)
          return (n & 0x00080000) ? 20 : 19;
        else
          return (n & 0x00020000) ? 18 : 17;
      }
    }
  } else {
    if (n & 0x0000FF00) {
      if (n & 0x0000F000) {
        if (n & 0x0000C000)
          return (n & 0x00008000) ? 16 : 15;
        else
          return (n & 0x00002000) ? 14 : 13;
      } else {
        if (n & 0x00000C00)
          return (n & 0x00000800) ? 12 : 11;
        else
          return (n & 0x00000200) ? 10 : 9;
      }
    } else {
      if (n & 0x000000F0) {
        if (n & 0x000000C0)
          return (n & 0x00000080) ? 8 : 7;
        else
          return (n & 0x00000020) ? 6 : 5;
      } else {
        if (n & 0x0000000C)
          return (n & 0x00000008) ? 4 : 3;
        else
          return (n & 0x00000002) ? 2 : (n ? 1 : 0);
      }
    }
  }
#else
#error fixme: only 4 or 8 byte pointers supported
#endif
  /* notreached */
  abort();
}

static int highest_significant_bit(int_ptr_t n)
{
  if (n >= 0)
    return highest_bit(n);
  return highest_bit(n ^ INT_PTR_MAX);
}

val plus(val anum, val bnum)
{
tail:
  switch (TAG_PAIR(tag(anum), tag(bnum))) {
  case TAG_PAIR(TAG_NUM, TAG_NUM):
    {
      cnum a = c_num(anum);
      cnum b = c_num(bnum);
      cnum sum = a + b;

      if (sum < NUM_MIN || sum > NUM_MAX)
        return bignum(sum);
      return num_fast(sum);
    }
  case TAG_PAIR(TAG_NUM, TAG_PTR):
    switch (type(bnum)) {
    case BGNUM:
      {
        val n;
        if (anum == zero)
          return bnum;
        n = make_bignum();
        if (sizeof (int_ptr_t) <= sizeof (mp_digit))  {
          cnum a = c_num(anum);
          cnum ap = ABS(a);
          if (a > 0)
            mp_add_d(mp(bnum), ap, mp(n));
          else
            mp_sub_d(mp(bnum), ap, mp(n));
        } else {
          mp_int tmp;
          mp_init(&tmp);
          mp_set_intptr(&tmp, c_num(anum));
          mp_add(mp(bnum), &tmp, mp(n));
          mp_clear(&tmp);
        }
        return normalize(n);
      }
    case FLNUM:
      return flo(c_num(anum) + c_flo(bnum));
    case RNG:
      return rcons(plus(anum, from(bnum)), plus(anum, to(bnum)));
    default:
      break;
    }
    break;
  case TAG_PAIR(TAG_PTR, TAG_NUM):
    switch (type(anum)) {
    case BGNUM:
      {
        val n;
        n = make_bignum();
        if (bnum == zero)
          return anum;
        if (sizeof (int_ptr_t) <= sizeof (mp_digit))  {
          cnum b = c_num(bnum);
          cnum bp = ABS(b);
          if (b > 0)
            mp_add_d(mp(anum), bp, mp(n));
          else
            mp_sub_d(mp(anum), bp, mp(n));
        } else {
          mp_int tmp;
          mp_init(&tmp);
          mp_set_intptr(&tmp, c_num(bnum));
          mp_add(mp(anum), &tmp, mp(n));
          mp_clear(&tmp);
        }
        return normalize(n);
      }
    case FLNUM:
      return flo(c_num(bnum) + c_flo(anum));
    case RNG:
      return rcons(plus(from(anum), bnum), plus(to(anum), bnum));
    default:
      break;
    }
    break;
  case TAG_PAIR(TAG_PTR, TAG_PTR):
    switch (TYPE_PAIR(type(anum), type(bnum))) {
    case TYPE_PAIR(BGNUM, BGNUM):
      {
        val n;
        n = make_bignum();
        mp_add(mp(anum), mp(bnum), mp(n));
        return normalize(n);
      }
    case TYPE_PAIR(FLNUM, FLNUM):
      return flo(c_flo(anum) + c_flo(bnum));
    case TYPE_PAIR(BGNUM, FLNUM):
      anum = flo_int(anum);
      goto tail;
    case TYPE_PAIR(FLNUM, BGNUM):
      bnum = flo_int(bnum);
      goto tail;
    case TYPE_PAIR(RNG, RNG):
      return rcons(plus(from(anum), from(bnum)), plus(to(anum), to(bnum)));
    case TYPE_PAIR(BGNUM, RNG):
    case TYPE_PAIR(FLNUM, RNG):
      return rcons(plus(anum, from(bnum)), plus(anum, to(bnum)));
    case TYPE_PAIR(RNG, BGNUM):
    case TYPE_PAIR(RNG, FLNUM):
      return rcons(plus(from(anum), bnum), plus(to(anum), bnum));
    default:
      break;
    }
    break;
  case TAG_PAIR(TAG_CHR, TAG_NUM):
    {
      wchar_t a = c_chr(anum);
      cnum b = c_num(bnum);
      cnum sum = a + b;

      if (sum < 0 || sum > 0x10FFFF)
        goto char_range;
      return chr(sum);
    }
  case TAG_PAIR(TAG_NUM, TAG_CHR):
    {
      cnum a = c_num(anum);
      wchar_t b = c_chr(bnum);
      cnum sum = a + b;

      if (sum < 0 || sum > 0x10FFFF)
        goto char_range;
      return chr(sum);
    }
  case TAG_PAIR(TAG_CHR, TAG_PTR):
    if (type(bnum) == RNG)
      return rcons(plus(anum, from(bnum)), plus(anum, to(bnum)));
  case TAG_PAIR(TAG_PTR, TAG_CHR):
    if (type(anum) == RNG)
      return rcons(plus(from(anum), bnum), plus(to(anum), bnum));
  }
  uw_throwf(error_s, lit("+: invalid operands ~s ~s"), anum, bnum, nao);
char_range:
  uw_throwf(numeric_error_s,
            lit("+: sum of ~s and ~s is out of character range"),
            anum, bnum, nao);
}

val minus(val anum, val bnum)
{
tail:
  switch (TAG_PAIR(tag(anum), tag(bnum))) {
  case TAG_PAIR(TAG_NUM, TAG_NUM):
  case TAG_PAIR(TAG_CHR, TAG_CHR):
    {
      cnum a = c_num(anum);
      cnum b = c_num(bnum);
      cnum sum = a - b;

      if (sum < NUM_MIN || sum > NUM_MAX)
        return bignum(sum);
      return num_fast(sum);
    }
  case TAG_PAIR(TAG_NUM, TAG_PTR):
    switch (type(bnum)) {
    case BGNUM:
      {
        val n;
        n = make_bignum();
        if (anum == zero) {
          mp_neg(mp(bnum), mp(n));
          return n;
        }
        if (sizeof (int_ptr_t) <= sizeof (mp_digit))  {
          cnum a = c_num(anum);
          cnum ap = ABS(a);
          if (ap > 0)
            mp_sub_d(mp(bnum), ap, mp(n));
          else
            mp_add_d(mp(bnum), ap, mp(n));
          mp_neg(mp(n), mp(n));
        } else {
          mp_int tmp;
          mp_init(&tmp);
          mp_set_intptr(&tmp, c_num(anum));
          mp_sub(mp(bnum), &tmp, mp(n));
          mp_clear(&tmp);
        }
        return normalize(n);
      }
    case FLNUM:
      return flo(c_num(anum) - c_flo(bnum));
    case RNG:
      return rcons(minus(anum, from(bnum)), minus(anum, to(bnum)));
    default:
      break;
    }
    break;
  case TAG_PAIR(TAG_PTR, TAG_NUM):
    switch (type(anum)) {
    case BGNUM:
      {
        val n;
        if (bnum == zero)
          return anum;
        n = make_bignum();
        if (sizeof (int_ptr_t) <= sizeof (mp_digit))  {
          cnum b = c_num(bnum);
          cnum bp = ABS(b);
          if (b > 0)
            mp_sub_d(mp(anum), bp, mp(n));
          else
            mp_add_d(mp(anum), bp, mp(n));
        } else {
          mp_int tmp;
          mp_init(&tmp);
          mp_set_intptr(&tmp, c_num(bnum));
          mp_sub(mp(anum), &tmp, mp(n));
          mp_clear(&tmp);
        }
        return normalize(n);
      }
    case FLNUM:
      return flo(c_flo(anum) - c_num(bnum));
    case RNG:
      return rcons(minus(from(anum), bnum), minus(to(anum), bnum));
    default:
      break;
    }
    break;
  case TAG_PAIR(TAG_PTR, TAG_PTR):
    switch (TYPE_PAIR(type(anum), type(bnum))) {
    case TYPE_PAIR(BGNUM, BGNUM):
      {
        val n;
        n = make_bignum();
        mp_sub(mp(anum), mp(bnum), mp(n));
        return normalize(n);
      }
    case TYPE_PAIR(FLNUM, FLNUM):
      return flo(c_flo(anum) - c_flo(bnum));
    case TYPE_PAIR(BGNUM, FLNUM):
      anum = flo_int(anum);
      goto tail;
    case TYPE_PAIR(FLNUM, BGNUM):
      bnum = flo_int(bnum);
      goto tail;
    case TYPE_PAIR(RNG, RNG):
      return rcons(minus(from(anum), from(bnum)), minus(to(anum), to(bnum)));
    case TYPE_PAIR(BGNUM, RNG):
    case TYPE_PAIR(FLNUM, RNG):
      return rcons(minus(anum, from(bnum)), minus(anum, to(bnum)));
    case TYPE_PAIR(RNG, BGNUM):
    case TYPE_PAIR(RNG, FLNUM):
      return rcons(minus(from(anum), bnum), minus(to(anum), bnum));
    default:
      break;
    }
    break;
  case TAG_PAIR(TAG_CHR, TAG_NUM):
    {
      wchar_t a = c_chr(anum);
      cnum b = c_num(bnum);
      cnum sum = a - b;

      if (sum < 0 || sum > 0x10FFFF)
        uw_throwf(numeric_error_s,
                  lit("-: difference of ~s and ~s is out of character range"),
                  anum, bnum, nao);
      return chr(sum);
    }
  case TAG_PAIR(TAG_CHR, TAG_PTR):
    if (type(bnum) == RNG)
      return rcons(minus(anum, from(bnum)), minus(anum, to(bnum)));
  case TAG_PAIR(TAG_PTR, TAG_CHR):
    if (type(anum) == RNG)
      return rcons(minus(from(anum), bnum), minus(to(anum), bnum));
  }
  uw_throwf(error_s, lit("-: invalid operands ~s ~s"), anum, bnum, nao);
}

val neg(val anum)
{
  switch (type(anum)) {
  case BGNUM:
    {
      val n = make_bignum();
      mp_neg(mp(anum), mp(n));
      return n;
    }
  case FLNUM:
    return flo(-c_flo(anum));
  case NUM:
    return num(-c_num(anum));
  case RNG:
    return rcons(neg(from(anum)), neg(to(anum)));
  default:
    uw_throwf(error_s, lit("-: ~s is not a number"), anum, nao);
  }
}

val abso(val anum)
{
  switch (type(anum)) {
  case BGNUM:
    {
      val n = make_bignum();
      mp_abs(mp(anum), mp(n));
      return n;
    }
  case FLNUM:
    return flo(fabs(c_flo(anum)));
  case NUM:
    {
      cnum n = c_num(anum);
      return num(n < 0 ? -n : n);
    }
  case RNG:
    return rcons(abso(from(anum)), abso(to(anum)));
  default:
    uw_throwf(error_s, lit("abs: ~s is not a number"), anum, nao);
  }
}

val mul(val anum, val bnum)
{
tail:
  switch (TAG_PAIR(tag(anum), tag(bnum))) {
  case TAG_PAIR(TAG_NUM, TAG_NUM):
    {
      cnum a = c_num(anum);
      cnum b = c_num(bnum);
#if HAVE_DOUBLE_INTPTR_T
      double_intptr_t product = a * convert(double_intptr_t, b);
      if (product < NUM_MIN || product > NUM_MAX)
        return bignum_dbl_ipt(product);
      return num_fast(product);
#else
      cnum ap = ABS(a);
      cnum bp = ABS(b);
      if (highest_bit(ap) + highest_bit(bp) < CNUM_BIT - 1) {
        cnum product = a * b;
        if (product >= NUM_MIN && product <= NUM_MAX)
          return num_fast(product);
        return bignum(product);
      } else {
        val n = make_bignum();
        mp_int tmpb;
        mp_init(&tmpb);
        mp_set_intptr(&tmpb, b);
        mp_set_intptr(mp(n), a);
        mp_mul(mp(n), &tmpb, mp(n));
        mp_clear(&tmpb);
        return n;
      }
#endif
    }
  case TAG_PAIR(TAG_NUM, TAG_PTR):
    switch (type(bnum)) {
    case BGNUM:
      {
        val n;
        if (anum == one)
          return bnum;
        n = make_bignum();
        if (sizeof (int_ptr_t) <= sizeof (mp_digit)) {
          cnum a = c_num(anum);
          cnum ap = ABS(a);
          mp_mul_d(mp(bnum), ap, mp(n));
          if (ap < 0)
            mp_neg(mp(n), mp(n));
        } else {
          mp_int tmp;
          mp_init(&tmp);
          mp_set_intptr(&tmp, c_num(anum));
          mp_mul(mp(bnum), &tmp, mp(n));
          mp_clear(&tmp);
        }
        return n;
      }
    case FLNUM:
      return flo(c_num(anum) * c_flo(bnum));
    case RNG:
      return rcons(mul(anum, from(bnum)), mul(anum, to(bnum)));
    default:
      break;
    }
    break;
  case TAG_PAIR(TAG_PTR, TAG_NUM):
    switch (type(anum)) {
    case BGNUM:
      {
        val n;
        if (bnum == one)
          return anum;
        n = make_bignum();
        if (sizeof (int_ptr_t) <= sizeof (mp_digit)) {
          cnum b = c_num(bnum);
          cnum bp = ABS(b);
          mp_mul_d(mp(anum), bp, mp(n));
          if (b < 0)
            mp_neg(mp(n), mp(n));
        } else {
          mp_int tmp;
          mp_init(&tmp);
          mp_set_intptr(&tmp, c_num(bnum));
          mp_mul(mp(anum), &tmp, mp(n));
          mp_clear(&tmp);
        }
        return n;
      }
    case FLNUM:
      return flo(c_flo(anum) * c_num(bnum));
    case RNG:
      return rcons(mul(from(anum), bnum), mul(to(anum), bnum));
    default:
      break;
    }
    break;
  case TAG_PAIR(TAG_PTR, TAG_PTR):
    switch (TYPE_PAIR(type(anum), type(bnum))) {
    case TYPE_PAIR(BGNUM, BGNUM):
      {
        val n;
        n = make_bignum();
        mp_mul(mp(anum), mp(bnum), mp(n));
        return n;
      }
    case TYPE_PAIR(FLNUM, FLNUM):
      return flo(c_flo(anum) * c_flo(bnum));
    case TYPE_PAIR(BGNUM, FLNUM):
      anum = flo_int(anum);
      goto tail;
    case TYPE_PAIR(FLNUM, BGNUM):
      bnum = flo_int(bnum);
      goto tail;
    case TYPE_PAIR(RNG, RNG):
      return rcons(mul(from(anum), from(bnum)), mul(to(anum), to(bnum)));
    case TYPE_PAIR(BGNUM, RNG):
    case TYPE_PAIR(FLNUM, RNG):
      return rcons(mul(anum, from(bnum)), mul(anum, to(bnum)));
    case TYPE_PAIR(RNG, BGNUM):
    case TYPE_PAIR(RNG, FLNUM):
      return rcons(mul(from(anum), bnum), mul(to(anum), bnum));
    default:
      break;
    }
  }
  uw_throwf(error_s, lit("*: invalid operands ~s ~s"), anum, bnum, nao);
}

static val trunc1(val num)
{
  switch (type(num)) {
  case NUM:
  case BGNUM:
    return num;
  case FLNUM:
    {
      double n = c_flo(num);
      return flo(n - fmod(n, 1.0));
    }
  case RNG:
    return rcons(trunc1(from(num)), trunc1(to(num)));
  default:
    break;
  }
  uw_throwf(error_s, lit("trunc: invalid operand ~s"), num, nao);
}

val trunc(val anum, val bnum)
{
  if (missingp(bnum))
    return trunc1(anum);
tail:
  switch (TAG_PAIR(tag(anum), tag(bnum))) {
  case TAG_PAIR(TAG_NUM, TAG_NUM):
    {
      cnum a = c_num(anum);
      cnum b = c_num(bnum);
      cnum ap = ABS(a);
      cnum bp = ABS(b);
      int neg = ((a < 0 && b > 0) || (a > 0 && b < 0));

      if (b == 0)
        goto divzero;

      {
        cnum quot = ap / bp;
        return num(neg ? -quot : quot);
      }
    }
  case TAG_PAIR(TAG_NUM, TAG_PTR):
    switch (type(bnum)) {
    case BGNUM:
      return zero;
    case FLNUM:
      {
        double x = c_num(anum), y = c_flo(bnum);
        if (y == 0.0)
          goto divzero;
        else
          return flo((x - fmod(x, y))/y);
      }
    default:
      break;
    }
    break;
  case TAG_PAIR(TAG_PTR, TAG_NUM):
    switch (type(anum)) {
    case BGNUM:
      {
        val n;
        if (bnum == one)
          return anum;
        n = make_bignum();
        if (sizeof (int_ptr_t) <= sizeof (mp_digit)) {
          cnum b = c_num(bnum);
          cnum bp = ABS(b);
          if (mp_div_d(mp(anum), bp, mp(n), 0) != MP_OKAY)
            goto divzero;
          if (b < 0)
            mp_neg(mp(n), mp(n));
        } else {
          int err;
          mp_int tmp;
          mp_init(&tmp);
          mp_set_intptr(&tmp, c_num(bnum));
          err = mp_div(mp(anum), &tmp, mp(n), 0);
          mp_clear(&tmp);
          if (err != MP_OKAY)
            goto divzero;
        }
        return normalize(n);
      }
    case FLNUM:
      {
        double x = c_flo(anum), y = c_num(bnum);
        if (y == 0.0)
          goto divzero;
        else
          return flo((x - fmod(x, y))/y);
      }
    case RNG:
      return rcons(trunc(from(anum), bnum), trunc(to(anum), bnum));
    default:
      break;
    }
    break;
  case TAG_PAIR(TAG_PTR, TAG_PTR):
    switch (TYPE_PAIR(type(anum), type (bnum))) {
    case TYPE_PAIR(BGNUM, BGNUM):
      {
        val n;
        n = make_bignum();
        if (mp_div(mp(anum), mp(bnum), mp(n), 0) != MP_OKAY)
          goto divzero;
        return normalize(n);
      }
    case TYPE_PAIR(FLNUM, FLNUM):
      {
        double x = c_flo(anum), y = c_flo(bnum);
        if (y == 0.0)
          goto divzero;
        else
          return flo((x - fmod(x, y))/y);
      }
    case TYPE_PAIR(BGNUM, FLNUM):
      anum = flo_int(anum);
      goto tail;
    case TYPE_PAIR(FLNUM, BGNUM):
      bnum = flo_int(bnum);
      goto tail;
    case TYPE_PAIR(RNG, BGNUM):
    case TYPE_PAIR(RNG, FLNUM):
      return rcons(trunc(from(anum), bnum), trunc(to(anum), bnum));
    }
  }
  uw_throwf(error_s, lit("trunc: invalid operands ~s ~s"), anum, bnum, nao);
divzero:
  uw_throw(numeric_error_s, lit("trunc: division by zero"));
}

static double dmod(double a, double b)
{
  if (b < 0.0) {
    double m = fmod(-a, -b);
    return - (m < 0.0 ? m - b : m);
  } else {
    double m = fmod(a, b);
    return m < 0 ? m + b : m;
  }
}

val mod(val anum, val bnum)
{
tail:
  switch (TAG_PAIR(tag(anum), tag(bnum))) {
  case TAG_PAIR(TAG_NUM, TAG_NUM):
    {
      cnum a = c_num(anum);
      cnum b = c_num(bnum);

      if (b == 0)
        goto divzero;

      if (b < 0)
      {
        cnum m = -a % -b;
        return num(- (m < 0 ? m - b : m));
      } else {
        cnum m = a % b;
        return num(m < 0 ? m + b : m);
      }
    }
  case TAG_PAIR(TAG_NUM, TAG_PTR):
    switch (type(bnum)) {
    case BGNUM:
      {
        val n;
        mp_int tmpa;
        mp_err err;
        n = make_bignum();
        mp_init(&tmpa);
        if (mp_cmp_z(mp(bnum)) == MP_LT) {
          mp_int tmpb;
          mp_init(&tmpb);
          mp_neg(mp(bnum), &tmpb);
          mp_set_intptr(&tmpa, -c_num(anum));
          err = mp_mod(&tmpa, &tmpb, mp(n));
          mp_clear(&tmpb);
          mp_neg(mp(n), mp(n));
        } else {
          mp_set_intptr(&tmpa, c_num(anum));
          err = mp_mod(&tmpa, mp(bnum), mp(n));
        }
        mp_clear(&tmpa);
        if (err != MP_OKAY)
          goto divzero;
        return normalize(n);
      }
    case FLNUM:
      return flo(dmod(c_num(anum), c_flo(bnum)));
    default:
      break;
    }
    break;
  case TAG_PAIR(TAG_PTR, TAG_NUM):
    switch (type(anum)) {
    case BGNUM:
      {
        if (sizeof (int_ptr_t) <= sizeof (mp_digit)) {
          cnum b = c_num(bnum);
          mp_digit n;
          mp_err err;
          if (b < 0) {
            mp_int tmpa;
            mp_init(&tmpa);
            mp_neg(mp(anum), &tmpa);
            err = mp_mod_d(&tmpa, -b, &n);
            mp_clear(&tmpa);
            n = -n;
          } else {
            err = mp_mod_d(mp(anum), b, &n);
          }
          if (err != MP_OKAY)
            goto divzero;
          return num(n);
        } else {
          val n = make_bignum();
          mp_int tmpb;
          mp_err err;
          cnum b = c_num(bnum);
          mp_init(&tmpb);
          if (b < 0) {
            mp_int tmpa;
            mp_init(&tmpa);
            mp_neg(mp(anum), &tmpa);
            mp_set_intptr(&tmpb, -b);
            err = mp_mod(&tmpa, &tmpb, mp(n));
            mp_clear(&tmpa);
            mp_neg(mp(n), mp(n));
          } else {
            mp_set_intptr(&tmpb, b);
            err = mp_mod(mp(anum), &tmpb, mp(n));
          }
          mp_clear(&tmpb);
          if (err != MP_OKAY)
            goto divzero;
          return normalize(n);
        }
      }
    case FLNUM:
      return flo(dmod(c_flo(anum), c_num(bnum)));
    default:
      break;
    }
    break;
  case TAG_PAIR(TAG_PTR, TAG_PTR):
    switch (TYPE_PAIR(type(anum), type(bnum))) {
    case (TYPE_PAIR(BGNUM, BGNUM)):
      {
        val n;
        n = make_bignum();
        if (mp_cmp_z(mp(bnum)) == MP_LT) {
          mp_int tmpa, tmpb;
          mp_err err;
          mp_init(&tmpa);
          mp_init(&tmpb);
          mp_neg(mp(anum), &tmpa);
          mp_neg(mp(bnum), &tmpb);
          err = mp_mod(&tmpa, &tmpb, mp(n));
          mp_clear(&tmpa);
          mp_clear(&tmpb);
          if (err != MP_OKAY)
            goto divzero;
          mp_neg(mp(n), mp(n));
        } else {
          if (mp_mod(mp(anum), mp(bnum), mp(n)) != MP_OKAY)
            goto divzero;
        }
        return normalize(n);
      }
    case TYPE_PAIR(FLNUM, FLNUM):
      return flo(dmod(c_flo(anum), c_flo(bnum)));
    case TYPE_PAIR(BGNUM, FLNUM):
      anum = flo_int(anum);
      goto tail;
    case TYPE_PAIR(FLNUM, BGNUM):
      bnum = flo_int(bnum);
      goto tail;
    }
  }
  uw_throwf(error_s, lit("mod: invalid operands ~s ~s"), anum, bnum, nao);
divzero:
  uw_throw(numeric_error_s, lit("mod: division by zero"));
}

val floordiv(val anum, val bnum)
{
  if (missingp(bnum))
    return floorf(anum);
tail:
  switch (TAG_PAIR(tag(anum), tag(bnum))) {
  case TAG_PAIR(TAG_NUM, TAG_NUM):
    {
      cnum a = c_num(anum);
      cnum b = c_num(bnum);
      cnum ap = ABS(a);
      cnum bp = ABS(b);
      int neg = ((a < 0 && b > 0) || (a > 0 && b < 0));

      if (b == 0)
        goto divzero;

      {
        cnum quot = ap / bp;
        if (neg) {
          if (quot * bp != ap)
            return num(-quot - 1);
          return num(-quot);
        }
        return num(quot);
      }
    }
  case TAG_PAIR(TAG_NUM, TAG_PTR):
    switch (type(bnum)) {
    case BGNUM:
      {
        cnum a = c_num(anum);
        if (a == 0)
          return zero;
        if (a < 0 && !ISNEG(mp(bnum)))
          return negone;
        if (a > 0 && ISNEG(mp(bnum)))
          return negone;
        return zero;
      }
    case FLNUM:
      {
        double x = c_num(anum), y = c_flo(bnum);
        if (y == 0.0)
          goto divzero;
        else
          return flo((x - dmod(x, y))/y);
      }
    default:
      break;
    }
    break;
  case TAG_PAIR(TAG_PTR, TAG_NUM):
    switch (type(anum)) {
    case BGNUM:
      {
        val n;
        if (bnum == one)
          return anum;
        n = make_bignum();
        if (sizeof (int_ptr_t) <= sizeof (mp_digit)) {
          cnum b = c_num(bnum);
          cnum bp = ABS(b);
          mp_digit rem;
          if (mp_div_d(mp(anum), bp, mp(n), &rem) != MP_OKAY)
            goto divzero;
          if (b < 0)
            mp_neg(mp(n), mp(n));
          if (rem && ((ISNEG(mp(anum)) && b > 0) ||
                      (!ISNEG(mp(anum)) && b < 0)))
            mp_sub_d(mp(n), 1, mp(n));
        } else {
          int err;
          cnum b = c_num(bnum);
          mp_int tmp, rem;
          mp_init(&tmp);
          mp_init(&rem);
          mp_set_intptr(&tmp, b);
          err = mp_div(mp(anum), &tmp, mp(n), 0);
          mp_clear(&tmp);
          if (err != MP_OKAY) {
            mp_clear(&rem);
            goto divzero;
          }
          if (mp_cmp_z(&rem) != MP_EQ &&
              ((ISNEG(mp(anum)) && b > 0) ||
               (!ISNEG(mp(anum)) && b < 0)))
            mp_sub_d(mp(n), 1, mp(n));
          mp_clear(&rem);
        }
        return normalize(n);
      }
    case FLNUM:
      {
        double x = c_flo(anum), y = c_num(bnum);
        if (y == 0.0)
          goto divzero;
        else
          return flo((x - dmod(x, y))/y);
      }
    case RNG:
      return rcons(floordiv(from(anum), bnum), floordiv(to(anum), bnum));
    default:
      break;
    }
    break;
  case TAG_PAIR(TAG_PTR, TAG_PTR):
    switch (TYPE_PAIR(type(anum), type (bnum))) {
    case TYPE_PAIR(BGNUM, BGNUM):
      {
        val n = make_bignum();
        mp_int rem;
        mp_init(&rem);
        if (mp_div(mp(anum), mp(bnum), mp(n), &rem) != MP_OKAY) {
          mp_clear(&rem);
          goto divzero;
        }
        if (mp_cmp_z(&rem) != MP_EQ &&
              ((ISNEG(mp(anum)) && !ISNEG(mp(bnum))) ||
               (!ISNEG(mp(anum)) && ISNEG(mp(bnum)))))
            mp_sub_d(mp(n), 1, mp(n));
        mp_clear(&rem);
        return normalize(n);
      }
    case TYPE_PAIR(FLNUM, FLNUM):
      {
        double x = c_flo(anum), y = c_flo(bnum);
        if (y == 0.0)
          goto divzero;
        else
          return flo((x - dmod(x, y))/y);
      }
    case TYPE_PAIR(BGNUM, FLNUM):
      anum = flo_int(anum);
      goto tail;
    case TYPE_PAIR(FLNUM, BGNUM):
      bnum = flo_int(bnum);
      goto tail;
    case TYPE_PAIR(RNG, BGNUM):
    case TYPE_PAIR(RNG, FLNUM):
      return rcons(floordiv(from(anum), bnum), floordiv(to(anum), bnum));
    }
  }
  uw_throwf(error_s, lit("floor: invalid operands ~s ~s"), anum, bnum, nao);
divzero:
  uw_throw(numeric_error_s, lit("floor: division by zero"));
}

val ceildiv(val anum, val bnum)
{
  if (missingp(bnum))
    return ceili(anum);
  return neg(floordiv(neg(anum), bnum));
}

static val round1(val num)
{
  switch (type(num)) {
  case NUM:
  case BGNUM:
    return num;
  case FLNUM:
#if HAVE_ROUND
    return flo(round(c_flo(num)));
#else
    {
      double n = c_flo(num);
      return if3(n >= 0,
                 flo(floor(0.5 + n)),
                 flo(-floor(0.5 + fabs(n))));
    }
#endif
  case RNG:
    return rcons(round1(from(num)), round1(to(num)));
  default:
    break;
  }
  uw_throwf(error_s, lit("round: invalid operand ~s"), num, nao);
}


val roundiv(val anum, val bnum)
{
  if (missingp(bnum))
    return round1(anum);

  if (minusp(bnum)) {
    anum = neg(anum);
    bnum = neg(bnum);
  }

  if (rangep(anum)) {
    return rcons(roundiv(from(anum), bnum), roundiv(to(anum), bnum));
  } else if (floatp(anum) || floatp(bnum)) {
    val quot = divi(anum, bnum);
#if HAVE_ROUND
    return flo(round(c_flo(quot)));
#else
    {
      double q = c_flo(quot);
      return if3(q >= 0,
                 flo(floor(0.5 + q)),
                 flo(-ceil(0.5 + fabs(q))));
    }
#endif
  } else {
    val quot = floordiv(anum, bnum);
    val rem = minus(anum, mul(quot, bnum));
    val drem = ash(rem, one);
    return if3(eq(drem, bnum),
               if3(minusp(quot), quot, succ(quot)),
               if3(lt(drem, bnum), quot, succ(quot)));
  }
}

val trunc_rem(val anum, val bnum)
{
  val quot = trunc(anum, bnum);
  val rem = minus(anum, mul(quot, if3(missingp(bnum), one, bnum)));
  return list(quot, rem, nao);
}

val floor_rem(val anum, val bnum)
{
  val quot = floordiv(anum, bnum);
  val rem = minus(anum, mul(quot, if3(missingp(bnum), one, bnum)));
  return list(quot, rem, nao);
}

val ceil_rem(val anum, val bnum)
{
  val quot = ceildiv(anum, bnum);
  val rem = minus(anum, mul(quot, if3(missingp(bnum), one, bnum)));
  return list(quot, rem, nao);
}

val round_rem(val anum, val bnum)
{
  val quot = roundiv(anum, bnum);
  val rem = minus(anum, mul(quot, if3(missingp(bnum), one, bnum)));
  return list(quot, rem, nao);
}

val wrap_star(val start, val end, val num)
{
  val modulus = minus(end, start);
  val num_off = minus(num, start);
  val num_mod = mod(num_off, modulus);
  return plus(start, num_mod);
}

val wrap(val start, val end, val num)
{
  return wrap_star(start, succ(end), num);
}

static val to_float(val func, val num)
{
  switch (type(num)) {
  case NUM:
  case BGNUM:
    return flo_int(num);
  case FLNUM:
    return num;
  default:
    uw_throwf(error_s, lit("~a: invalid operand ~s"), func, num, nao);
  }
}

val divi(val anum, val bnum)
{
  if (missingp(bnum)) {
    double b = c_flo(to_float(lit("/"), anum));
    if (b == 0.0)
      uw_throw(numeric_error_s, lit("/: division by zero"));
    return flo(1.0 / b);
  } else if (type(anum) == RNG) {
    return rcons(divi(from(anum), bnum), divi(to(anum), bnum));
  } else {
    double a = c_flo(to_float(lit("/"), anum));
    double b = c_flo(to_float(lit("/"), bnum));

    if (b == 0.0)
      uw_throw(numeric_error_s, lit("/: division by zero"));

    return flo(a / b);
  }
}

val zerop(val num)
{
  if (num == zero)
    return t;

  switch (type(num)) {
  case NUM:
  case BGNUM:
    return nil;
  case FLNUM:
    return if2(c_flo(num) == 0.0, t);
  case CHR:
    return if2(num == chr(0), t);
  case RNG:
    return and2(zerop(from(num)), zerop(to(num)));
  default:
    uw_throwf(error_s, lit("zerop: ~s is not a number"), num, nao);
  }
}

val plusp(val num)
{
  switch (type(num)) {
  case NUM:
    return if2(c_num(num) > 0, t);
  case BGNUM:
    return if2(mp_cmp_z(mp(num)) == MP_GT, t);
  case FLNUM:
    return if2(c_flo(num) > 0.0, t);
  case CHR:
    return if2(num != chr(0), t);
  default:
    uw_throwf(error_s, lit("plusp: ~s is not a number"), num, nao);
  }
}

val minusp(val num)
{
  switch (type(num)) {
  case NUM:
    return if2(c_num(num) < 0, t);
  case BGNUM:
    return if2(mp_cmp_z(mp(num)) == MP_LT, t);
  case FLNUM:
    return if2(c_flo(num) < 0.0, t);
  case CHR:
    return nil;
  default:
    uw_throwf(error_s, lit("minusp: ~s is not a number"), num, nao);
  }
}

val evenp(val num)
{
  switch (type(num)) {
  case NUM:
    return (c_num(num) % 2 == 0) ? t : nil;
  case BGNUM:
    return mp_iseven(mp(num)) ? t : nil;
  default:
    uw_throwf(error_s, lit("evenp: ~s is not an integer"), num, nao);
    return nil;
  }
}

val oddp(val num)
{
  switch (type(num)) {
  case NUM:
    return (c_num(num) % 2 != 0) ? t : nil;
  case BGNUM:
    return mp_isodd(mp(num)) ? t : nil;
  default:
    uw_throwf(error_s, lit("oddp: ~s is not an integer"), num, nao);
    return nil;
  }
}

val succ(val num)
{
  return plus(num, one);
}

val ssucc(val num)
{
  return plus(num, two);
}

val sssucc(val num)
{
  return plus(num, three);
}

val pred(val num)
{
  return minus(num, one);
}

val ppred(val num)
{
  return minus(num, two);
}

val pppred(val num)
{
  return minus(num, three);
}

val gt(val anum, val bnum)
{
tail:
  switch (TYPE_PAIR(type(anum), type(bnum))) {
  case TYPE_PAIR(NUM, NUM):
  case TYPE_PAIR(CHR, CHR):
  case TYPE_PAIR(NUM, CHR):
  case TYPE_PAIR(CHR, NUM):
    return c_num(anum) > c_num(bnum) ? t : nil;
  case TYPE_PAIR(NUM, BGNUM):
  case TYPE_PAIR(CHR, BGNUM):
    return mp_cmp_z(mp(bnum)) == MP_LT ? t : nil;
  case TYPE_PAIR(BGNUM, NUM):
  case TYPE_PAIR(BGNUM, CHR):
    return mp_cmp_z(mp(anum)) == MP_GT ? t : nil;
  case TYPE_PAIR(BGNUM, BGNUM):
    return mp_cmp(mp(anum), mp(bnum)) == MP_GT ? t : nil;
  case TYPE_PAIR(NUM, FLNUM):
  case TYPE_PAIR(CHR, FLNUM):
    return c_num(anum) > c_flo(bnum) ? t : nil;
  case TYPE_PAIR(FLNUM, NUM):
  case TYPE_PAIR(FLNUM, CHR):
    return c_flo(anum) > c_num(bnum) ? t : nil;
  case TYPE_PAIR(FLNUM, FLNUM):
    return c_flo(anum) > c_flo(bnum) ? t : nil;
  case TYPE_PAIR(FLNUM, BGNUM):
    bnum = flo_int(bnum);
    goto tail;
  case TYPE_PAIR(BGNUM, FLNUM):
    anum = flo_int(anum);
    goto tail;
  case TYPE_PAIR(RNG, RNG):
    {
      val fl = from(anum);
      val fr = from(bnum);

      if (gt(fl, fr))
        return t;

      if (numeq(fl, fr))
        return gt(to(anum), to(bnum));

      return nil;
    }
  }

  uw_throwf(error_s, lit(">: invalid operands ~s ~s"), anum, bnum, nao);
}

val lt(val anum, val bnum)
{
tail:
  switch (TYPE_PAIR(type(anum), type(bnum))) {
  case TYPE_PAIR(NUM, NUM):
  case TYPE_PAIR(CHR, CHR):
  case TYPE_PAIR(NUM, CHR):
  case TYPE_PAIR(CHR, NUM):
    return c_num(anum) < c_num(bnum) ? t : nil;
  case TYPE_PAIR(NUM, BGNUM):
  case TYPE_PAIR(CHR, BGNUM):
    return mp_cmp_z(mp(bnum)) == MP_GT ? t : nil;
  case TYPE_PAIR(BGNUM, NUM):
  case TYPE_PAIR(BGNUM, CHR):
    return mp_cmp_z(mp(anum)) == MP_LT ? t : nil;
  case TYPE_PAIR(BGNUM, BGNUM):
    return mp_cmp(mp(anum), mp(bnum)) == MP_LT ? t : nil;
  case TYPE_PAIR(NUM, FLNUM):
  case TYPE_PAIR(CHR, FLNUM):
    return c_num(anum) < c_flo(bnum) ? t : nil;
  case TYPE_PAIR(FLNUM, NUM):
  case TYPE_PAIR(FLNUM, CHR):
    return c_flo(anum) < c_num(bnum) ? t : nil;
  case TYPE_PAIR(FLNUM, FLNUM):
    return c_flo(anum) < c_flo(bnum) ? t : nil;
  case TYPE_PAIR(FLNUM, BGNUM):
    bnum = flo_int(bnum);
    goto tail;
  case TYPE_PAIR(BGNUM, FLNUM):
    anum = flo_int(anum);
    goto tail;
  case TYPE_PAIR(RNG, RNG):
    {
      val fl = from(anum);
      val fr = from(bnum);

      if (lt(fl, fr))
        return t;

      if (numeq(fl, fr))
        return lt(to(anum), to(bnum));

      return nil;
    }
  }

  uw_throwf(error_s, lit("<: invalid operands ~s ~s"), anum, bnum, nao);
}

val ge(val anum, val bnum)
{
tail:
  switch (TYPE_PAIR(type(anum), type(bnum))) {
  case TYPE_PAIR(NUM, NUM):
  case TYPE_PAIR(CHR, CHR):
  case TYPE_PAIR(NUM, CHR):
  case TYPE_PAIR(CHR, NUM):
    return c_num(anum) >= c_num(bnum) ? t : nil;
  case TYPE_PAIR(NUM, BGNUM):
  case TYPE_PAIR(CHR, BGNUM):
    return mp_cmp_z(mp(bnum)) == MP_LT ? t : nil;
  case TYPE_PAIR(BGNUM, NUM):
  case TYPE_PAIR(BGNUM, CHR):
    return mp_cmp_z(mp(anum)) == MP_GT ? t : nil;
  case TYPE_PAIR(BGNUM, BGNUM):
    switch (mp_cmp(mp(anum), mp(bnum))) {
    case MP_GT: case MP_EQ:
      return t;
    default:
      return nil;
    }
  case TYPE_PAIR(NUM, FLNUM):
  case TYPE_PAIR(CHR, FLNUM):
    return c_num(anum) >= c_flo(bnum) ? t : nil;
  case TYPE_PAIR(FLNUM, NUM):
  case TYPE_PAIR(FLNUM, CHR):
    return c_flo(anum) >= c_num(bnum) ? t : nil;
  case TYPE_PAIR(FLNUM, FLNUM):
    return c_flo(anum) >= c_flo(bnum) ? t : nil;
  case TYPE_PAIR(FLNUM, BGNUM):
    bnum = flo_int(bnum);
    goto tail;
  case TYPE_PAIR(BGNUM, FLNUM):
    anum = flo_int(anum);
    goto tail;
  case TYPE_PAIR(RNG, RNG):
    {
      val fl = from(anum);
      val fr = from(bnum);

      if (gt(fl, fr))
        return t;

      if (numeq(fl, fr))
        return ge(to(anum), to(bnum));

      return nil;
    }
  }

  uw_throwf(error_s, lit(">=: invalid operands ~s ~s"), anum, bnum, nao);
}

val le(val anum, val bnum)
{
tail:
  switch (TYPE_PAIR(type(anum), type(bnum))) {
  case TYPE_PAIR(NUM, NUM):
  case TYPE_PAIR(CHR, CHR):
  case TYPE_PAIR(NUM, CHR):
  case TYPE_PAIR(CHR, NUM):
    return c_num(anum) <= c_num(bnum) ? t : nil;
  case TYPE_PAIR(NUM, BGNUM):
  case TYPE_PAIR(CHR, BGNUM):
    return mp_cmp_z(mp(bnum)) == MP_GT ? t : nil;
  case TYPE_PAIR(BGNUM, NUM):
  case TYPE_PAIR(BGNUM, CHR):
    return mp_cmp_z(mp(anum)) == MP_LT ? t : nil;
  case TYPE_PAIR(BGNUM, BGNUM):
    switch (mp_cmp(mp(anum), mp(bnum))) {
    case MP_LT: case MP_EQ:
      return t;
    default:
      return nil;
    }
  case TYPE_PAIR(NUM, FLNUM):
  case TYPE_PAIR(CHR, FLNUM):
    return c_num(anum) <= c_flo(bnum) ? t : nil;
  case TYPE_PAIR(FLNUM, NUM):
  case TYPE_PAIR(FLNUM, CHR):
    return c_flo(anum) <= c_num(bnum) ? t : nil;
  case TYPE_PAIR(FLNUM, FLNUM):
    return c_flo(anum) <= c_flo(bnum) ? t : nil;
  case TYPE_PAIR(FLNUM, BGNUM):
    bnum = flo_int(bnum);
    goto tail;
  case TYPE_PAIR(BGNUM, FLNUM):
    anum = flo_int(anum);
    goto tail;
  case TYPE_PAIR(RNG, RNG):
    {
      val fl = from(anum);
      val fr = from(bnum);

      if (lt(fl, fr))
        return t;

      if (numeq(fl, fr))
        return le(to(anum), to(bnum));

      return nil;
    }
  }

  uw_throwf(error_s, lit("<=: invalid operands ~s ~s"), anum, bnum, nao);
}

val numeq(val anum, val bnum)
{
tail:
  switch (TYPE_PAIR(type(anum), type(bnum))) {
  case TYPE_PAIR(NUM, NUM):
  case TYPE_PAIR(CHR, CHR):
  case TYPE_PAIR(NUM, CHR):
  case TYPE_PAIR(CHR, NUM):
    return c_num(anum) == c_num(bnum) ? t : nil;
  case TYPE_PAIR(NUM, BGNUM):
  case TYPE_PAIR(CHR, BGNUM):
    return mp_cmp_z(mp(bnum)) == MP_EQ ? t : nil;
  case TYPE_PAIR(BGNUM, NUM):
  case TYPE_PAIR(BGNUM, CHR):
    return mp_cmp_z(mp(anum)) == MP_EQ ? t : nil;
  case TYPE_PAIR(BGNUM, BGNUM):
    return mp_cmp(mp(anum), mp(bnum)) == MP_EQ ? t : nil;
  case TYPE_PAIR(NUM, FLNUM):
  case TYPE_PAIR(CHR, FLNUM):
    return c_num(anum) == c_flo(bnum) ? t : nil;
  case TYPE_PAIR(FLNUM, NUM):
  case TYPE_PAIR(FLNUM, CHR):
    return c_flo(anum) == c_num(bnum) ? t : nil;
  case TYPE_PAIR(FLNUM, FLNUM):
    return c_flo(anum) == c_flo(bnum) ? t : nil;
  case TYPE_PAIR(FLNUM, BGNUM):
    bnum = flo_int(bnum);
    goto tail;
  case TYPE_PAIR(BGNUM, FLNUM):
    anum = flo_int(anum);
    goto tail;
  case TYPE_PAIR(RNG, RNG):
    return and2(numeq(from(anum), from(bnum)),
                numeq(to(anum), to(bnum)));
  }

  uw_throwf(error_s, lit("=: invalid operands ~s ~s"), anum, bnum, nao);
}

val expt(val anum, val bnum)
{
tail:
  switch (TYPE_PAIR(type(anum), type(bnum))) {
  case TYPE_PAIR(NUM, NUM):
    {
      cnum a = c_num(anum);
      cnum b = c_num(bnum);
      mp_int tmpa;
      val n;
      if (b < 0)
        goto negexp;
      if (bnum == zero)
        return one;
      if (bnum == one)
        return anum;
      n = make_bignum();
      mp_init(&tmpa);
      mp_set_intptr(&tmpa, a);
      if (sizeof (int_ptr_t) <= sizeof (mp_digit)) {
        mp_expt_d(&tmpa, b, mp(n));
      } else {
        mp_int tmpb;
        mp_init(&tmpb);
        mp_set_intptr(&tmpb, b);
        mp_expt(&tmpa, &tmpb, mp(n));
        mp_clear(&tmpb);
      }
      mp_clear(&tmpa);
      return normalize(n);
    }
  case TYPE_PAIR(NUM, BGNUM):
    {
      cnum a = c_num(anum);
      mp_int tmpa;
      val n;
      if (mp_cmp_z(mp(bnum)) == MP_LT)
        goto negexp;
      n = make_bignum();
      mp_init(&tmpa);
      mp_set_intptr(&tmpa, a);
      mp_expt(&tmpa, mp(bnum), mp(n));
      mp_clear(&tmpa);
      return normalize(n);
    }
  case TYPE_PAIR(BGNUM, NUM):
    {
      cnum b = c_num(bnum);
      val n;
      if (b < 0)
        goto negexp;
      if (bnum == zero)
        return one;
      if (bnum == one)
        return anum;
      n = make_bignum();
      if (sizeof (int_ptr_t) <= sizeof (mp_digit)) {
        mp_expt_d(mp(anum), b, mp(n));
      } else {
        mp_int tmpb;
        mp_init(&tmpb);
        mp_set_intptr(&tmpb, b);
        mp_expt(mp(anum), &tmpb, mp(n));
        mp_clear(&tmpb);
      }
      return normalize(n);
    }
  case TYPE_PAIR(BGNUM, BGNUM):
    {
      val n;
      if (mp_cmp_z(mp(bnum)) == MP_LT)
        goto negexp;
      n = make_bignum();
      mp_expt(mp(anum), mp(bnum), mp(n));
      normalize(n);
      return n;
    }
  case TYPE_PAIR(NUM, FLNUM):
    /* TODO: error checking */
    return flo(pow(c_num(anum), c_flo(bnum)));
  case TYPE_PAIR(FLNUM, NUM):
    return flo(pow(c_flo(anum), c_num(bnum)));
  case TYPE_PAIR(FLNUM, FLNUM):
    return flo(pow(c_flo(anum), c_flo(bnum)));
  case TYPE_PAIR(BGNUM, FLNUM):
    anum = flo_int(anum);
    goto tail;
  case TYPE_PAIR(FLNUM, BGNUM):
    bnum = flo_int(bnum);
    goto tail;
  }

  uw_throwf(error_s, lit("expt: invalid operands ~s ~s"), anum, bnum, nao);
negexp:
  uw_throw(error_s, lit("expt: negative exponent"));
}

val exptmod(val base, val exp, val mod)
{
  val n;

  if (!integerp(base) || !integerp(exp) || !integerp(mod))
    goto inval;

  if (fixnump(base))
    base = bignum(c_num(base));

  if (fixnump(exp))
    exp = bignum(c_num(exp));

  if (fixnump(mod))
    mod = bignum(c_num(mod));

  n = make_bignum();

  if (mp_exptmod(mp(base), mp(exp), mp(mod), mp(n)) != MP_OKAY)
    goto bad;

  return normalize(n);
inval:
  uw_throwf(error_s, lit("exptmod: non-integral operands ~s ~s ~s"),
            base, exp, mod, nao);
bad:
  uw_throwf(error_s, lit("exptmod: bad operands ~s ~s ~s"),
            base, exp, mod, nao);
}

static int_ptr_t isqrt_fixnum(int_ptr_t a)
{
  int_ptr_t mask = convert(int_ptr_t, 1) << (highest_bit(a) / 2);
  int_ptr_t root = 0;

  for (; mask != 0; mask >>= 1) {
    int_ptr_t next_guess = root | mask;
    if (next_guess * next_guess <= a)
      root = next_guess;
  }

  return root;
}

val isqrt(val anum)
{
  switch (type(anum)) {
  case NUM:
    {
      cnum a = c_num(anum);
      if (a < 0)
        goto negop;
      return num_fast(isqrt_fixnum(c_num(anum)));
    }
  case BGNUM:
    {
      val n = make_bignum();
      if (mp_sqrt(mp(anum), mp(n)) != MP_OKAY)
        goto negop;
      return normalize(n);
    }
  default:
    break;
  }

  uw_throwf(error_s, lit("isqrt: non-integer operand ~s"), anum, nao);
negop:
  uw_throw(error_s, lit("isqrt: negative operand"));
}

val gcd(val anum, val bnum)
{
  val n;

  if (!integerp(anum) || !integerp(bnum))
    goto inval;

  if (anum == zero)
    return bnum;

  if (bnum == zero)
    return anum;

  if (fixnump(anum))
    anum = bignum(c_num(anum));

  if (fixnump(bnum))
    bnum = bignum(c_num(bnum));

  n = make_bignum();

  if (mp_gcd(mp(anum), mp(bnum), mp(n)) != MP_OKAY)
    goto bad;

  return normalize(n);
inval:
  uw_throwf(error_s, lit("gcd: non-integral operands ~s ~s"),
            anum, bnum, nao);
bad:
  uw_throwf(error_s, lit("gcd: operation failed on ~s ~s"),
            anum, bnum, nao);
}

val lcm(val anum, val bnum)
{
  if (anum == zero || bnum == zero) {
    return zero;
  } else {
    val prod = mul(anum, bnum);
    val gcdv = gcd(anum, bnum);
    return abso(trunc(prod, gcdv));
  }
}

val floorf(val num)
{
  switch (type(num)) {
  case NUM:
  case BGNUM:
    return num;
  case FLNUM:
    return flo(floor(c_flo(num)));
  case RNG:
    return rcons(floorf(from(num)), floorf(to(num)));
  default:
    break;
  }
  uw_throwf(error_s, lit("floor: invalid operand ~s"), num, nao);
}

val ceili(val num)
{
  switch (type(num)) {
  case NUM:
  case BGNUM:
    return num;
  case FLNUM:
    return flo(ceil(c_flo(num)));
  case RNG:
    return rcons(ceili(from(num)), ceili(to(num)));
  default:
    break;
  }
  uw_throwf(error_s, lit("ceil: invalid operand ~s"), num, nao);
}

val sine(val num)
{
  return flo(sin(c_flo(to_float(lit("sin"), num))));
}

val cosi(val num)
{
  return flo(cos(c_flo(to_float(lit("cos"), num))));
}

val tang(val num)
{
  return flo(tan(c_flo(to_float(lit("tan"), num))));
}

val asine(val num)
{
  return flo(asin(c_flo(to_float(lit("asin"), num))));
}

val acosi(val num)
{
  return flo(acos(c_flo(to_float(lit("acos"), num))));
}

val atang(val num)
{
  return flo(atan(c_flo(to_float(lit("atan"), num))));
}

val atang2(val y, val x)
{
  return flo(atan2(c_flo(to_float(lit("atan2"), y)),
                   c_flo(to_float(lit("atan2"), x))));
}

val loga(val num)
{
  return flo(log(c_flo(to_float(lit("log"), num))));
}

val logten(val num)
{
  return flo(log10(c_flo(to_float(lit("log"), num))));
}

#if HAVE_LOG2

static void log2_init(void)
{
}

#else

static double l2;

static void log2_init(void)
{
  l2 = log(2.0);
}

static double log2(double x)
{
  return log(x)/l2;
}

#endif

val logtwo(val num)
{
  return flo(log2(c_flo(to_float(lit("log"), num))));
}

val expo(val num)
{
  return flo(exp(c_flo(to_float(lit("exp"), num))));
}

val sqroot(val num)
{
  return flo(sqrt(c_flo(to_float(lit("sqrt"), num))));
}

/*
 * TODO: replace this text-based hack!
 */
val int_flo(val f)
{
  double d = c_flo(f);

  if (d >= INT_PTR_MAX && d <= INT_PTR_MIN) {
    cnum n = d;
    if (n < NUM_MIN || n > NUM_MAX)
      return bignum(n);
    return num_fast(n);
  } else {
    char text[128];
    char mint[128] = "", mfrac[128] = "", *pint = mint;
    int have_point, have_exp;
    int exp = 0, fdigs;

    sprintf(text, "%.64g", d);

    if (!isdigit(text[0]) && (text[0] != '-' || !isdigit(text[1])))
      uw_throwf(error_s,
                lit("int-flo: cannot convert #<bad-float> to integer"),
                nao);

    have_exp = (strchr(text, 'e') != 0);
    have_point = (strchr(text, '.') != 0);

    if (have_exp && have_point)
      sscanf(text, "%127[-0-9].%127[0-9]e%d", mint, mfrac, &exp);
    else if (have_exp)
      sscanf(text, "%127[-0-9]e%d", mint, &exp);
    else if (have_point)
      sscanf(text, "%127[-0-9].%127[0-9]", mint, mfrac);
    else
      return int_str(string_utf8(text), nil);

    if (have_exp && exp < 0)
      return zero;

    fdigs = have_point ? strlen(mfrac) : 0;

    if (exp <= fdigs) {
      fdigs = exp;
      exp = 0;
    } else {
      exp -= fdigs;
    }

    {
      char mintfrac[256];
      val out;
      val e10 = (exp == 0) ? one : expt(num_fast(10), num(exp));
      sprintf(mintfrac, "%s%.*s", pint, fdigs, mfrac);
      out = int_str(string_utf8(mintfrac), nil);
      return mul(out, e10);
    }
  }
}

val flo_int(val i)
{
  if (fixnump(i))
    return flo(c_num(i));

  {
    double d;
    type_check(i, BGNUM);
    if (mp_to_double(mp(i), &d) != MP_OKAY)
      uw_throwf(error_s, lit("flo-int: bignum to float conversion failed"),
                nao);
    return flo(d);
  }
}

val logand(val a, val b)
{
  val c;

  if (zerop(a) || zerop(b))
    return zero;

  switch (TYPE_PAIR(type(a), type(b))) {
  case TYPE_PAIR(NUM, NUM):
    if (a == b) {
      return a;
    } else {
      cnum ac = c_num(a);
      cnum bc = c_num(b);
      return num_fast(ac & bc);
    }
  case TYPE_PAIR(BGNUM, NUM):
    {
      val tmp = a;
      a = b;
      b = tmp;
    }
    /* fallthrough */
  case TYPE_PAIR(NUM, BGNUM):
    a = bignum(c_num(a));
    /* fallthrough */
  case TYPE_PAIR(BGNUM, BGNUM):
    if (a == b)
      return a;
    c = make_ubignum();
    if (mp_and(mp(a), mp(b), mp(c)) != MP_OKAY)
      goto bad;
    return normalize(c);
  default:
    uw_throwf(error_s, lit("logand: non-integral operands ~s ~s"), a, b, nao);
  }

bad:
  uw_throwf(error_s, lit("logand: operation failed on ~s ~s"), a, b, nao);
}

val logior(val a, val b)
{
  val c;

  if (zerop(a) && zerop(b))
    return zero;

  switch (TYPE_PAIR(type(a), type(b))) {
  case TYPE_PAIR(NUM, NUM):
    if (a == b) {
      return a;
    } else {
      cnum ac = c_num(a);
      cnum bc = c_num(b);
      return num_fast(ac | bc);
    }
  case TYPE_PAIR(BGNUM, NUM):
    {
      val tmp = a;
      a = b;
      b = tmp;
    }
    /* fallthrough */
  case TYPE_PAIR(NUM, BGNUM):
    a = bignum(c_num(a));
    /* fallthrough */
  case TYPE_PAIR(BGNUM, BGNUM):
    if (a == b)
      return a;
    c = make_ubignum();
    if (mp_or(mp(a), mp(b), mp(c)) != MP_OKAY)
      goto bad;
    return normalize(c);
  default:
    uw_throwf(error_s, lit("logior: non-integral operands ~s ~s"), a, b, nao);
  }

bad:
  uw_throwf(error_s, lit("logior: operation failed on ~s ~s"), a, b, nao);
}

val logxor(val a, val b)
{
  val c;

  if (zerop(a) && zerop(b))
    return zero;

  switch (TYPE_PAIR(type(a), type(b))) {
  case TYPE_PAIR(NUM, NUM):
    if (a == b) {
      return a;
    } else {
      cnum ac = c_num(a);
      cnum bc = c_num(b);
      return num_fast(ac ^ bc);
    }
  case TYPE_PAIR(BGNUM, NUM):
    {
      val tmp = a;
      a = b;
      b = tmp;
    }
    /* fallthrough */
  case TYPE_PAIR(NUM, BGNUM):
    a = bignum(c_num(a));
    /* fallthrough */
  case TYPE_PAIR(BGNUM, BGNUM):
    if (a == b)
      return a;
    c = make_ubignum();
    if (mp_xor(mp(a), mp(b), mp(c)) != MP_OKAY)
      goto bad;
    return normalize(c);
  default:
    uw_throwf(error_s, lit("logxor: non-integral operands ~s ~s"), a, b, nao);
  }

bad:
  uw_throwf(error_s, lit("logxor: operation failed on ~s ~s"), a, b, nao);
}

val logtest(val a, val b)
{
  /* TODO: optimize */
  return logand(a, b) == zero ? nil : t;
}

static val comp_trunc(val a, val bits)
{
  cnum an, bn;
  val b;
  const cnum num_mask = (NUM_MAX << 1) | 1;
  const int num_bits = CHAR_BIT * sizeof (cnum) - TAG_SHIFT;

  if (!fixnump(bits))
    goto bad2;

  bn = c_num(bits);

  switch (type(a)) {
  case NUM:
    an = c_num(a);
    if (bn < num_bits) {
      cnum mask = num_mask >> (num_bits - bn);
      return num_fast((an & mask) ^ mask);
    }
    a = bignum(an);
    /* fallthrough */
  case BGNUM:
    b = make_ubignum();
    if (mp_trunc_comp(mp(a), mp(b), bn) != MP_OKAY)
      goto bad;
    return normalize(b);
  default:
    goto bad3;
  }

bad:
  uw_throwf(error_s, lit("lognot: operation failed on ~s"), a, nao);

bad2:
  uw_throwf(error_s, lit("lognot: bits value ~s is not a fixnum"), bits, nao);

bad3:
  uw_throwf(error_s, lit("lognot: non-integral operand ~s"), a, nao);
}

val lognot(val a, val bits)
{
  val b;

  if (default_null_arg(bits))
    return comp_trunc(a, bits);

  switch (type(a)) {
  case NUM:
    return num_fast(~c_num(a));
  case BGNUM:
    b = make_ubignum();
    if (mp_comp(mp(a), mp(b)) != MP_OKAY)
      goto bad;
    return normalize(b);
  default:
    uw_throwf(error_s, lit("lognot: non-integral operand ~s"), a, nao);
  }

bad:
  uw_throwf(error_s, lit("lognot: operation failed on ~s"), a, nao);
}

val logtrunc(val a, val bits)
{
  cnum an, bn;
  val b;
  const cnum num_mask = (NUM_MAX << 1) | 1;
  const int num_bits = CHAR_BIT * sizeof (cnum) - TAG_SHIFT;

  if (!fixnump(bits))
    goto bad2;

  bn = c_num(bits);

  switch (type(a)) {
  case NUM:
    an = c_num(a);
    if (bn <= num_bits) {
      cnum mask = num_mask >> (num_bits - bn);
      return num_fast(an & mask);
    }
    a = bignum(an);
    /* fallthrough */
  case BGNUM:
    b = make_ubignum();
    if (mp_trunc(mp(a), mp(b), bn) != MP_OKAY)
      goto bad;
    return normalize(b);
  default:
    goto bad3;
  }

bad:
  uw_throwf(error_s, lit("logtrunc: operation failed on ~s"), a, nao);

bad2:
  uw_throwf(error_s, lit("logtrunc: bits value ~s is not a fixnum"), bits, nao);

bad3:
  uw_throwf(error_s, lit("logtrunc: non-integral operand ~s"), a, nao);
}

val sign_extend(val n, val nbits)
{
  val msb = minus(nbits, one);
  val ntrunc = logtrunc(n, nbits);

  if (bit(ntrunc, msb)) {
    switch (type(ntrunc)) {
    case NUM:
      {
        cnum cn = c_num(ntrunc);
        cnum nb = c_num(nbits);
        return num(cn | (INT_PTR_MAX << nb));
      }
    case BGNUM:
      {
        val out = make_ubignum();
        mp_2comp(mp(ntrunc), mp(out), mp(ntrunc)->used);
        mp_neg(mp(out), mp(out));
        return normalize(out);
      }
    default:
      internal_error("impossible case");
    }
  }
  return ntrunc;
}

val ash(val a, val bits)
{
  cnum an, bn;
  val b;
  int hb;
  const int num_bits = CHAR_BIT * sizeof (cnum) - TAG_SHIFT;

  if (!fixnump(bits))
    goto bad2;

  bn = c_num(bits);

  if (bn == 0) {
    switch (type(a)) {
    case NUM:
    case BGNUM:
      return a;
    default:
      goto bad3;
    }
  } else if (bn > 0) {
    switch (type(a)) {
    case NUM:
      an = c_num(a);
      hb = highest_significant_bit(an);
      if (bn + hb < num_bits)
        return num_fast(an << bn);
      a = bignum(an);
      /* fallthrough */
    case BGNUM:
      b = make_bignum();
      if (mp_shift(mp(a), mp(b), bn) != MP_OKAY)
        goto bad;
      return normalize(b);
    default:
      goto bad3;
    }
  } else {
    switch (type(a)) {
    case NUM:
      bn = -bn;
      an = c_num(a);
      if (bn <= num_bits)
        return num_fast(an >> bn);
      return num_fast(an >> num_bits);
    case BGNUM:
      b = make_bignum();
      if (mp_shift(mp(a), mp(b), bn) != MP_OKAY)
        goto bad;
      return normalize(b);
    default:
      goto bad3;
    }

  }

bad:
  uw_throwf(error_s, lit("ashift: operation failed on ~s"), a, nao);

bad2:
  uw_throwf(error_s, lit("ashift: bits value ~s is not a fixnum"), bits, nao);

bad3:
  uw_throwf(error_s, lit("ashift: non-integral operand ~s"), a, nao);
}

val bit(val a, val bit)
{
  cnum bn;

  if (!fixnump(bit))
    goto bad;

  bn = c_num(bit);

  if (bn < 0)
    goto bad2;

  switch (type(a)) {
  case NUM:
    {
      cnum an = c_num(a);
      if (bn < (SIZEOF_PTR * CHAR_BIT))
        return (an & (convert(cnum, 1) << bn)) ? t : nil;
      return an < 0 ? t : nil;
    }
  case BGNUM:
    {
      mp_err res = mp_bit(mp(a), bn);

      switch (res) {
      case MP_YES:
        return t;
      case MP_NO:
        return nil;
      default:
        goto bad4;
      }
    }
  default:
    goto bad3;
  }

bad:
  uw_throwf(error_s, lit("bit: bit position ~s is not a fixnum"), bit, nao);

bad2:
  uw_throwf(error_s, lit("bit: bit position ~s is negative"), bit, nao);

bad3:
  uw_throwf(error_s, lit("bit: non-integral operand ~s"), a, nao);

bad4:
  uw_throwf(error_s, lit("bit: operation failed on ~s, bit ~s"), a, bit, nao);
}

val maskv(struct args *bits)
{
  cnum index = 0;
  val accum = zero;

  while (args_more(bits, index)) {
    val num = args_get(bits, &index);
    val mask = ash(one, num);
    accum = logior(accum, mask);
  }

  return accum;
}

/*
 * Source:
 * Better Approximations to Cumulative Normal Functions
 * Graeme West
 * 2009
 */
val cum_norm_dist(val arg)
{
  val arg_flo = to_float(lit("cum-norm-dist"), arg);
  double x = c_flo(arg_flo);
  double xabs = fabs(x);

  if (xabs > 37.0) {
    return flo(1.0);
  } else {
    double ex = exp(-(xabs * xabs) / 2.0);
    double retval, accum;

    if (xabs < 7.07106781186547) {
      accum = 3.52624965998911E-02 * xabs + 0.700383064443688;
      accum = accum * xabs + 6.37396220353165;
      accum = accum * xabs + 33.912866078383;
      accum = accum * xabs + 112.079291497871;
      accum = accum * xabs + 221.213596169931;
      accum = accum * xabs + 220.206867912376;

      retval = ex * accum;

      accum = 8.83883476483184E-02 * xabs + 1.75566716318264;
      accum = accum * xabs + 16.064177579207;
      accum = accum * xabs + 86.7807322029461;
      accum = accum * xabs + 296.564248779674;
      accum = accum * xabs + 637.333633378831;
      accum = accum * xabs + 793.826512519948;
      accum = accum * xabs + 440.413735824752;

      retval /= accum;
    } else {
      accum = xabs + 0.65;
      accum = xabs + 4.0 / accum;
      accum = xabs + 3.0 / accum;
      accum = xabs + 2.0 / accum;
      accum = xabs + 1.0 / accum;

      retval = ex / accum / 2.506628274631;
    }

    if (x > 0)
      retval = 1.0 - retval;

    return flo(retval);
  }
}

static val rising_product(val m, val n)
{
  val acc;

  if (lt(n, one))
    return one;

  if (gt(m, n))
    return one;

  if (lt(m, one))
    m = one;

  acc = m;

  m = plus(m, one);

  while (le(m, n)) {
    acc = mul(acc, m);
    m = plus(m, one);
  }

  return acc;
}

val n_choose_k(val n, val k)
{
  val top = rising_product(plus(minus(n, k), one), n);
  val bottom = rising_product(one, k);
  return trunc(top, bottom);
}

val n_perm_k(val n, val k)
{
  return rising_product(plus(minus(n, k), one), n);
}

val tofloat(val obj)
{
  switch (tag(obj)) {
  case TAG_NUM:
    return flo_int(obj);
  case TAG_CHR:
    {
      cnum ch = c_num(obj);
      if (ch >= '0' && ch <= '9')
        return flo(ch - '0');
      return nil;
    }
  case TAG_LIT:
    return flo_str(obj);
  case TAG_PTR:
    switch (type(obj)) {
    case BGNUM:
      return flo_int(obj);
    case FLNUM:
      return obj;
    case STR:
    case LSTR:
    case LIT:
      return flo_str(obj);
    default:
      break;
    }
    /* fallthrough */
  default:
    uw_throwf(error_s, lit("tofloat: ~s is not convertible to float"), obj, nao);
  }
}

val toint(val obj, val base)
{
  switch (tag(obj)) {
  case TAG_NUM:
    return obj;
  case TAG_LIT:
    return int_str(obj, base);
  case TAG_CHR:
    {
      cnum ch = c_num(obj);

      if (ch >= '0' && ch <= '9')
        return num(ch - '0');

      if (iswalpha(ch)) {
        cnum n = 10 + towupper(ch) - 'A';
        cnum b = c_num(default_arg(base, num_fast(10)));

        if (n < b)
          return num(n);
      }
      return nil;
    }
  case TAG_PTR:
    switch (type(obj)) {
    case BGNUM:
      return obj;
    case FLNUM:
      return int_flo(obj);
    case STR:
    case LSTR:
    case LIT:
      return int_str(obj, base);
    default:
      break;
    }
    /* fallthrough */
  default:
    uw_throwf(error_s, lit("toint: ~s is not convertible to integer"), obj, nao);
  }
}

val tofloatz(val obj)
{
  uses_or2;
  if (nilp(obj))
    return flo(0.0);
  return or2(tofloat(obj), flo(0.0));
}

val tointz(val obj, val base)
{
  uses_or2;
  if (nilp(obj))
    return zero;
  return or2(toint(obj, base), zero);
}

val width(val obj)
{
  switch (tag(obj)) {
  case TAG_NUM:
  case TAG_CHR:
    {
      cnum n = c_num(obj);

      if (n < 0) {
        n &= INT_PTR_MAX;
        n ^= INT_PTR_MAX;
        return num_fast(highest_bit(n));
      }
      return num_fast(highest_bit(n));
    }
  case TAG_PTR:
    if (type(obj) == BGNUM) {
      int count;
      if (mp_cmp_z(mp(obj)) == MP_LT) {
        mp_int tmp;
        int i;

        mp_2comp(mp(obj), &tmp, mp(obj)->used);

        for (i = 0; i < tmp.used; i++)
          tmp.dp[i] ^= MP_DIGIT_MAX;

        count = mp_count_bits(&tmp);
        mp_clear(&tmp);
      } else {
        count = mp_count_bits(mp(obj));
      }
      return num(count);
    }
  default:
    break;
  }
  uw_throwf(error_s, lit("integer-length: ~s isn't an integer"), obj, nao);
}

static val bits(val obj)
{
  return normalize(bignum_from_uintptr(coerce(uint_ptr_t, obj)));
}


void arith_init(void)
{
  mp_init(&NUM_MAX_MP);
  mp_set_intptr(&NUM_MAX_MP, NUM_MAX);
  mp_init(&INT_PTR_MAX_MP);
  mp_set_intptr(&INT_PTR_MAX_MP, INT_PTR_MAX);
  mp_init(&UINT_PTR_MAX_MP);
  mp_set_uintptr(&UINT_PTR_MAX_MP, -1);
  log2_init();

  reg_varl(intern(lit("*flo-dig*"), user_package), num_fast(DBL_DIG));
  reg_varl(intern(lit("*flo-max*"), user_package), flo(DBL_MAX));
  reg_varl(intern(lit("*flo-min*"), user_package), flo(DBL_MIN));
  reg_varl(intern(lit("*flo-epsilon*"), user_package), flo(DBL_EPSILON));
  reg_varl(intern(lit("flo-dig"), user_package), num_fast(DBL_DIG));
  reg_varl(intern(lit("flo-max-dig"), user_package), num_fast(FLO_MAX_DIG));
  reg_varl(intern(lit("flo-max"), user_package), flo(DBL_MAX));
  reg_varl(intern(lit("flo-min"), user_package), flo(DBL_MIN));
  reg_varl(intern(lit("flo-epsilon"), user_package), flo(DBL_EPSILON));
  reg_varl(intern(lit("fixnum-min"), user_package), num(NUM_MIN));
  reg_varl(intern(lit("fixnum-max"), user_package), num(NUM_MAX));

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif
  reg_varl(intern(lit("*pi*"), user_package), flo(M_PI));
  reg_varl(intern(lit("%pi%"), user_package), flo(M_PI));
#ifndef M_E
#define M_E 2.71828182845904523536
#endif
  reg_varl(intern(lit("*e*"), user_package), flo(M_E));
  reg_varl(intern(lit("%e%"), user_package), flo(M_E));

  reg_fun(intern(lit("bits"), system_package), func_n1(bits));
}

void arith_free_all(void)
{
  mp_clear(&NUM_MAX_MP);
  mp_clear(&INT_PTR_MAX_MP);
  mp_clear(&UINT_PTR_MAX_MP);
}
