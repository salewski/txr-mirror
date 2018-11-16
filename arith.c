/* Copyright 2010-2018
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
#if HAVE_ROUNDING_CTL_H
#include <fenv.h>
#endif
#include "lib.h"
#include "signal.h"
#include "unwind.h"
#include "gc.h"
#include "args.h"
#include "eval.h"
#include "itypes.h"
#include "txr.h"
#include "arith.h"

#define TAG_PAIR(A, B) ((A) << TAG_SHIFT | (B))
#define NOOP(A, B)
#define CNUM_BIT ((int) sizeof (cnum) * CHAR_BIT)
#define ABS(A) ((A) < 0 ? -(A) : (A))

static mp_int NUM_MAX_MP, INT_PTR_MAX_MP, UINT_PTR_MAX_MP;
static mp_int INT_PTR_MAX_SUCC_MP;

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
  mp_err mpe = mp_read_unsigned_bin(mp(n), buf, bytes);
  if (mpe != MP_OKAY)
    do_mp_error(lit("buffer to number conversion"), mpe);
  return normalize(n);
}

static noreturn void not_number(val self, val obj)
{
  uw_throwf(type_error_s, lit("~a: ~s is not a number"), self, obj, nao);
}

static noreturn void not_integer(val self, val obj)
{
  uw_throwf(type_error_s, lit("~a: ~s is not an integer"), self, obj, nao);
}

static noreturn void invalid_ops(val self, val obj1, val obj2)
{
  uw_throwf(type_error_s, lit("~a: invalid operands ~s ~s"), self,
            obj1, obj2, nao);
}

static noreturn void invalid_op(val self, val obj)
{
  uw_throwf(type_error_s, lit("~a: invalid operand ~s"), self, obj, nao);
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
    not_integer(lit("num-to-buffer"), num);
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
  switch (mp_cmp_mag(mp(bignum), &INT_PTR_MAX_SUCC_MP)) {
  default:
  case MP_GT:
    return nil;
  case MP_EQ:
    if (mp_cmp_z(mp(bignum)) == MP_GT)
      return nil;
    /* fallthrough */
  case MP_LT:
    return t;
  }
}

static val in_uint_ptr_range(val bignum)
{
  return (mp_cmp_z(mp(bignum)) == MP_LT ||
          mp_cmp_mag(mp(bignum), &UINT_PTR_MAX_MP) == MP_GT) ? nil : t;
}

ucnum c_unum(val num)
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
    /* fallthrough */
  range:
    uw_throwf(error_s, lit("~s given, non-negative expected"), num, nao);
  default:
    type_mismatch(lit("~s is not an integer"), num, nao);
  }
}

val unum(ucnum u)
{
  if (u <= INT_PTR_MAX) {
    return num(u);
  } else {
    val n = make_bignum();
    mp_set_uintptr(mp(n), u);
    return n;
  }
}

val bignum_len(val num)
{
  switch (type(num)) {
  case CHR: case NUM:
    return zero;
  case BGNUM:
    return unum(mp(num)->used);
  default:
    not_integer(lit("bignum-len"), num);
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

void do_mp_error(val self, mp_err code)
{
  val errstr = string_utf8(mp_strerror(code));
  uw_throwf(numeric_error_s, lit("~a: ~a"), self, errstr, nao);
}

val plus(val anum, val bnum)
{
  val self = lit("+");

tail:
  switch (TAG_PAIR(tag(anum), tag(bnum))) {
  case TAG_PAIR(TAG_NUM, TAG_NUM):
    {
      cnum a = c_n(anum);
      cnum b = c_n(bnum);
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
        mp_err mpe;
        if (anum == zero)
          return bnum;
        n = make_bignum();
        if (sizeof (int_ptr_t) <= sizeof (mp_digit))  {
          cnum a = c_n(anum);
          cnum ap = ABS(a);
          if (a > 0)
            mpe = mp_add_d(mp(bnum), ap, mp(n));
          else
            mpe = mp_sub_d(mp(bnum), ap, mp(n));
        } else {
          mp_int tmp;
          mp_init(&tmp);
          mp_set_intptr(&tmp, c_n(anum));
          mpe = mp_add(mp(bnum), &tmp, mp(n));
          mp_clear(&tmp);
        }
        if (mpe != MP_OKAY)
          do_mp_error(self, mpe);
        return normalize(n);
      }
    case FLNUM:
      return flo(c_n(anum) + c_flo(bnum, self));
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
        mp_err mpe;
        n = make_bignum();
        if (bnum == zero)
          return anum;
        if (sizeof (int_ptr_t) <= sizeof (mp_digit))  {
          cnum b = c_n(bnum);
          cnum bp = ABS(b);
          if (b > 0)
            mpe = mp_add_d(mp(anum), bp, mp(n));
          else
            mpe = mp_sub_d(mp(anum), bp, mp(n));
        } else {
          mp_int tmp;
          mp_init(&tmp);
          mp_set_intptr(&tmp, c_n(bnum));
          mpe = mp_add(mp(anum), &tmp, mp(n));
          mp_clear(&tmp);
        }
        if (mpe != MP_OKAY)
          do_mp_error(self, mpe);
        return normalize(n);
      }
    case FLNUM:
      return flo(c_n(bnum) + c_flo(anum, self));
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
        mp_err mpe;
        n = make_bignum();
        mpe = mp_add(mp(anum), mp(bnum), mp(n));
        if (mpe != MP_OKAY)
          do_mp_error(self, mpe);
        return normalize(n);
      }
    case TYPE_PAIR(FLNUM, FLNUM):
      return flo(c_flo(anum, self) + c_flo(bnum, self));
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
      cnum b = c_n(bnum);
      cnum sum = a + b;

      if (sum < 0 || sum > 0x10FFFF)
        goto char_range;
      return chr(sum);
    }
  case TAG_PAIR(TAG_NUM, TAG_CHR):
    {
      cnum a = c_n(anum);
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
  invalid_ops(self, anum, bnum);
char_range:
  uw_throwf(numeric_error_s,
            lit("~a: sum of ~s and ~s is out of character range"),
            self, anum, bnum, nao);
}

val minus(val anum, val bnum)
{
  val self = lit("-");

tail:
  switch (TAG_PAIR(tag(anum), tag(bnum))) {
  case TAG_PAIR(TAG_NUM, TAG_NUM):
  case TAG_PAIR(TAG_CHR, TAG_CHR):
    {
      cnum a = c_n(anum);
      cnum b = c_n(bnum);
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
        mp_err mpe;
        n = make_bignum();
        if (anum == zero) {
          mp_neg(mp(bnum), mp(n));
          return n;
        }
        if (sizeof (int_ptr_t) <= sizeof (mp_digit))  {
          cnum a = c_n(anum);
          cnum ap = ABS(a);
          if (ap > 0)
            mpe = mp_sub_d(mp(bnum), ap, mp(n));
          else
            mpe = mp_add_d(mp(bnum), ap, mp(n));
          if (mpe == MP_OKAY)
            mp_neg(mp(n), mp(n));
        } else {
          mp_int tmp;
          mp_init(&tmp);
          mp_set_intptr(&tmp, c_n(anum));
          mpe = mp_sub(mp(bnum), &tmp, mp(n));
          mp_clear(&tmp);
        }
        if (mpe != MP_OKAY)
          do_mp_error(self, mpe);
        return normalize(n);
      }
    case FLNUM:
      return flo(c_n(anum) - c_flo(bnum, self));
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
        mp_err mpe;
        if (bnum == zero)
          return anum;
        n = make_bignum();
        if (sizeof (int_ptr_t) <= sizeof (mp_digit))  {
          cnum b = c_n(bnum);
          cnum bp = ABS(b);
          if (b > 0)
            mpe = mp_sub_d(mp(anum), bp, mp(n));
          else
            mpe = mp_add_d(mp(anum), bp, mp(n));
        } else {
          mp_int tmp;
          mp_init(&tmp);
          mp_set_intptr(&tmp, c_n(bnum));
          mpe = mp_sub(mp(anum), &tmp, mp(n));
          mp_clear(&tmp);
        }
        if (mpe != MP_OKAY)
          do_mp_error(self, mpe);
        return normalize(n);
      }
    case FLNUM:
      return flo(c_flo(anum, self) - c_n(bnum));
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
        mp_err mpe;
        n = make_bignum();
        mpe = mp_sub(mp(anum), mp(bnum), mp(n));
        if (mpe != MP_OKAY)
          do_mp_error(self, mpe);
        return normalize(n);
      }
    case TYPE_PAIR(FLNUM, FLNUM):
      return flo(c_flo(anum, self) - c_flo(bnum, self));
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
      cnum b = c_n(bnum);
      cnum sum = a - b;

      if (sum < 0 || sum > 0x10FFFF)
        uw_throwf(numeric_error_s,
                  lit("~a: difference of ~s and ~s is out of character range"),
                  self, anum, bnum, nao);
      return chr(sum);
    }
  case TAG_PAIR(TAG_CHR, TAG_PTR):
    if (type(bnum) == RNG)
      return rcons(minus(anum, from(bnum)), minus(anum, to(bnum)));
    break;
  case TAG_PAIR(TAG_PTR, TAG_CHR):
    if (type(anum) == RNG)
      return rcons(minus(from(anum), bnum), minus(to(anum), bnum));
    break;
  }

  invalid_ops(self, anum, bnum);
}

val neg(val anum)
{
  val self = lit("-");

  switch (type(anum)) {
  case BGNUM:
    {
      val n = make_bignum();
      mp_neg(mp(anum), mp(n));
      return n;
    }
  case FLNUM:
    return flo(-c_flo(anum, self));
  case NUM:
    return num(-c_n(anum));
  case RNG:
    return rcons(neg(from(anum)), neg(to(anum)));
  default:
    not_number(self, anum);
  }
}

val abso(val anum)
{
  val self = lit("abs");

  switch (type(anum)) {
  case BGNUM:
    {
      val n = make_bignum();
      mp_abs(mp(anum), mp(n));
      return n;
    }
  case FLNUM:
    return flo(fabs(c_flo(anum, self)));
  case NUM:
    {
      cnum n = c_n(anum);
      return num(n < 0 ? -n : n);
    }
  case RNG:
    return rcons(abso(from(anum)), abso(to(anum)));
  default:
    not_number(self, anum);
  }
}

static val signum(val anum)
{
  switch (type(anum)) {
  case BGNUM:
    return if3(ISNEG(mp(anum)), negone, one);
  case FLNUM:
    {
      double a = anum->fl.n;
      return flo(if3(a > 0, 1.0, if3(a < 0, -1.0, 0.0)));
    }
  case NUM:
    {
      cnum a = c_n(anum);
      return if3(a > 0, one, if3(a < 0, negone, zero));
    }
  default:
    not_number(lit("signum"), anum);
  }
}

val mul(val anum, val bnum)
{
  val self = lit("*");

tail:
  switch (TAG_PAIR(tag(anum), tag(bnum))) {
  case TAG_PAIR(TAG_NUM, TAG_NUM):
    {
      cnum a = c_n(anum);
      cnum b = c_n(bnum);
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
        mp_err mpe;
        if (anum == one)
          return bnum;
        n = make_bignum();
        if (sizeof (int_ptr_t) <= sizeof (mp_digit)) {
          cnum a = c_n(anum);
          cnum ap = ABS(a);
          mpe = mp_mul_d(mp(bnum), ap, mp(n));
          if (ap < 0 && mpe == MP_OKAY)
            mp_neg(mp(n), mp(n));
        } else {
          mp_int tmp;
          mp_init(&tmp);
          mp_set_intptr(&tmp, c_n(anum));
          mpe = mp_mul(mp(bnum), &tmp, mp(n));
          mp_clear(&tmp);
        }
        if (mpe != MP_OKAY)
          do_mp_error(self, mpe);
        return n;
      }
    case FLNUM:
      return flo(c_n(anum) * c_flo(bnum, self));
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
        mp_err mpe;
        if (bnum == one)
          return anum;
        n = make_bignum();
        if (sizeof (int_ptr_t) <= sizeof (mp_digit)) {
          cnum b = c_n(bnum);
          cnum bp = ABS(b);
          mpe = mp_mul_d(mp(anum), bp, mp(n));
          if (b < 0 && mpe == MP_OKAY)
            mp_neg(mp(n), mp(n));
        } else {
          mp_int tmp;
          mp_init(&tmp);
          mp_set_intptr(&tmp, c_n(bnum));
          mpe = mp_mul(mp(anum), &tmp, mp(n));
          mp_clear(&tmp);
        }
        if (mpe != MP_OKAY)
          do_mp_error(self, mpe);
        return n;
      }
    case FLNUM:
      return flo(c_flo(anum, self) * c_n(bnum));
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
        mp_err mpe;
        n = make_bignum();
        mpe = mp_mul(mp(anum), mp(bnum), mp(n));
        if (mpe != MP_OKAY)
          do_mp_error(self, mpe);
        return n;
      }
    case TYPE_PAIR(FLNUM, FLNUM):
      return flo(c_flo(anum, self) * c_flo(bnum, self));
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

  invalid_ops(self, anum, bnum);
}

static val trunc1(val self, val num)
{
  switch (type(num)) {
  case NUM:
  case BGNUM:
    return num;
  case FLNUM:
    {
      double n = c_flo(num, self);
      return flo(n - fmod(n, 1.0));
    }
  case RNG:
    return rcons(trunc1(self, from(num)), trunc1(self, to(num)));
  default:
    break;
  }
  invalid_op(self, num);
}

static noreturn void divzero(val self)
{
  uw_throwf(numeric_error_s, lit("~a: division by zero"), self, nao);
}

val trunc(val anum, val bnum)
{
  val self = lit("trunc");

  if (missingp(bnum))
    return trunc1(self, anum);
tail:
  switch (TAG_PAIR(tag(anum), tag(bnum))) {
  case TAG_PAIR(TAG_NUM, TAG_NUM):
    {
      cnum a = c_n(anum);
      cnum b = c_n(bnum);
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
        double x = c_n(anum), y = c_flo(bnum, self);
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
          cnum b = c_n(bnum);
          cnum bp = ABS(b);
          if (mp_div_d(mp(anum), bp, mp(n), 0) != MP_OKAY)
            goto divzero;
          if (b < 0)
            mp_neg(mp(n), mp(n));
        } else {
          int err;
          mp_int tmp;
          mp_init(&tmp);
          mp_set_intptr(&tmp, c_n(bnum));
          err = mp_div(mp(anum), &tmp, mp(n), 0);
          mp_clear(&tmp);
          if (err != MP_OKAY)
            goto divzero;
        }
        return normalize(n);
      }
    case FLNUM:
      {
        double x = c_flo(anum, self), y = c_n(bnum);
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
        double x = c_flo(anum, self), y = c_flo(bnum, self);
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
  invalid_ops(self, anum, bnum);
divzero:
  divzero(self);
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
  val self = lit("mod");

tail:
  switch (TAG_PAIR(tag(anum), tag(bnum))) {
  case TAG_PAIR(TAG_NUM, TAG_NUM):
    {
      cnum a = c_n(anum);
      cnum b = c_n(bnum);

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
          mp_set_intptr(&tmpa, -c_n(anum));
          err = mp_mod(&tmpa, &tmpb, mp(n));
          mp_clear(&tmpb);
          mp_neg(mp(n), mp(n));
        } else {
          mp_set_intptr(&tmpa, c_n(anum));
          err = mp_mod(&tmpa, mp(bnum), mp(n));
        }
        mp_clear(&tmpa);
        if (err != MP_OKAY)
          goto divzero;
        return normalize(n);
      }
    case FLNUM:
      return flo(dmod(c_n(anum), c_flo(bnum, self)));
    default:
      break;
    }
    break;
  case TAG_PAIR(TAG_PTR, TAG_NUM):
    switch (type(anum)) {
    case BGNUM:
      {
        if (sizeof (int_ptr_t) <= sizeof (mp_digit)) {
          cnum b = c_n(bnum);
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
          cnum b = c_n(bnum);
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
      return flo(dmod(c_flo(anum, self), c_n(bnum)));
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
      return flo(dmod(c_flo(anum, self), c_flo(bnum, self)));
    case TYPE_PAIR(BGNUM, FLNUM):
      anum = flo_int(anum);
      goto tail;
    case TYPE_PAIR(FLNUM, BGNUM):
      bnum = flo_int(bnum);
      goto tail;
    }
  }
  invalid_ops(self, anum, bnum);
divzero:
  divzero(self);
}

val floordiv(val anum, val bnum)
{
  val self = lit("floor");

  if (missingp(bnum))
    return floorf(anum);
tail:
  switch (TAG_PAIR(tag(anum), tag(bnum))) {
  case TAG_PAIR(TAG_NUM, TAG_NUM):
    {
      cnum a = c_n(anum);
      cnum b = c_n(bnum);
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
        cnum a = c_n(anum);
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
        double x = c_n(anum), y = c_flo(bnum, self);
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
          cnum b = c_n(bnum);
          cnum bp = ABS(b);
          mp_digit rem;
          mp_err mpe = MP_OKAY;
          if (mp_div_d(mp(anum), bp, mp(n), &rem) != MP_OKAY)
            goto divzero;
          if (b < 0)
            mp_neg(mp(n), mp(n));
          if (rem && ((ISNEG(mp(anum)) && b > 0) ||
                      (!ISNEG(mp(anum)) && b < 0)))
            mpe = mp_sub_d(mp(n), 1, mp(n));
          if (mpe != MP_OKAY)
            do_mp_error(self, mpe);
        } else {
          mp_err mpe;
          cnum b = c_n(bnum);
          mp_int tmp, rem;
          mp_init(&tmp);
          mp_init(&rem);
          mp_set_intptr(&tmp, b);
          mpe = mp_div(mp(anum), &tmp, mp(n), 0);
          mp_clear(&tmp);
          if (mpe != MP_OKAY) {
            mp_clear(&rem);
            goto divzero;
          }
          if (mp_cmp_z(&rem) != MP_EQ &&
              ((ISNEG(mp(anum)) && b > 0) ||
               (!ISNEG(mp(anum)) && b < 0)))
            mpe = mp_sub_d(mp(n), 1, mp(n));
          mp_clear(&rem);
          if (mpe != MP_OKAY)
            do_mp_error(self, mpe);
        }
        return normalize(n);
      }
    case FLNUM:
      {
        double x = c_flo(anum, self), y = c_n(bnum);
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
        mp_err mpe = MP_OKAY;
        mp_init(&rem);
        if (mp_div(mp(anum), mp(bnum), mp(n), &rem) != MP_OKAY) {
          mp_clear(&rem);
          goto divzero;
        }
        if (mp_cmp_z(&rem) != MP_EQ &&
              ((ISNEG(mp(anum)) && !ISNEG(mp(bnum))) ||
               (!ISNEG(mp(anum)) && ISNEG(mp(bnum)))))
            mpe = mp_sub_d(mp(n), 1, mp(n));
        mp_clear(&rem);
        if (mpe != MP_OKAY)
          do_mp_error(self, mpe);
        return normalize(n);
      }
    case TYPE_PAIR(FLNUM, FLNUM):
      {
        double x = c_flo(anum, self), y = c_flo(bnum, self);
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
  invalid_ops(self, anum, bnum);
divzero:
  divzero(self);
}

val ceildiv(val anum, val bnum)
{
  if (missingp(bnum))
    return ceili(anum);
  return neg(floordiv(neg(anum), bnum));
}

static val round1(val self, val num)
{
  switch (type(num)) {
  case NUM:
  case BGNUM:
    return num;
  case FLNUM:
#if HAVE_ROUND
    return flo(round(c_flo(num, self)));
#else
    {
      double n = c_flo(num, self);
      return if3(n >= 0,
                 flo(floor(0.5 + n)),
                 flo(-floor(0.5 + fabs(n))));
    }
#endif
  case RNG:
    return rcons(round1(self, from(num)), round1(self, to(num)));
  default:
    break;
  }
  invalid_op(self, num);
}


val roundiv(val anum, val bnum)
{
  val self = lit("round");

  if (missingp(bnum))
    return round1(self, anum);

  if (minusp(bnum)) {
    anum = neg(anum);
    bnum = neg(bnum);
  }

  if (rangep(anum)) {
    return rcons(roundiv(from(anum), bnum), roundiv(to(anum), bnum));
  } else if (floatp(anum) || floatp(bnum)) {
    val quot = divi(anum, bnum);
#if HAVE_ROUND
    return flo(round(c_flo(quot, self)));
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
    invalid_op(func, num);
  }
}

val divi(val anum, val bnum)
{
  val self = lit("/");

  if (missingp(bnum)) {
    double b = c_flo(to_float(self, anum), self);
    if (b == 0.0)
      goto divzero;
    return flo(1.0 / b);
  } else if (type(anum) == RNG) {
    return rcons(divi(from(anum), bnum), divi(to(anum), bnum));
  } else {
    double a = c_flo(to_float(self, anum), self);
    double b = c_flo(to_float(self, bnum), self);

    if (b == 0.0)
      goto divzero;

    return flo(a / b);
  }
divzero:
  divzero(self);
}

val zerop(val num)
{
  val self = lit("zerop");

  if (num == zero)
    return t;

  switch (type(num)) {
  case NUM:
  case BGNUM:
    return nil;
  case FLNUM:
    return if2(c_flo(num, self) == 0.0, t);
  case CHR:
    return if2(num == chr(0), t);
  case RNG:
    return and2(zerop(from(num)), zerop(to(num)));
  default:
    not_number(self, num);
  }
}

val plusp(val num)
{
  val self = lit("zerop");

  switch (type(num)) {
  case NUM:
    return if2(c_n(num) > 0, t);
  case BGNUM:
    return if2(mp_cmp_z(mp(num)) == MP_GT, t);
  case FLNUM:
    return if2(c_flo(num, self) > 0.0, t);
  case CHR:
    return if2(num != chr(0), t);
  default:
    not_number(self, num);
  }
}

val minusp(val num)
{
  val self = lit("minusp");

  switch (type(num)) {
  case NUM:
    return if2(c_n(num) < 0, t);
  case BGNUM:
    return if2(mp_cmp_z(mp(num)) == MP_LT, t);
  case FLNUM:
    return if2(c_flo(num, self) < 0.0, t);
  case CHR:
    return nil;
  default:
    not_number(self, num);
  }
}

val evenp(val num)
{
  switch (type(num)) {
  case NUM:
    return (c_n(num) % 2 == 0) ? t : nil;
  case BGNUM:
    return mp_iseven(mp(num)) ? t : nil;
  default:
    not_integer(lit("evenp"), num);
  }
}

val oddp(val num)
{
  switch (type(num)) {
  case NUM:
    return (c_n(num) % 2 != 0) ? t : nil;
  case BGNUM:
    return mp_isodd(mp(num)) ? t : nil;
  default:
    not_integer(lit("oddp"), num);
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
  val self = lit(">");
tail:
  switch (TYPE_PAIR(type(anum), type(bnum))) {
  case TYPE_PAIR(NUM, NUM):
  case TYPE_PAIR(CHR, CHR):
  case TYPE_PAIR(NUM, CHR):
  case TYPE_PAIR(CHR, NUM):
    return c_n(anum) > c_n(bnum) ? t : nil;
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
    return c_n(anum) > c_flo(bnum, self) ? t : nil;
  case TYPE_PAIR(FLNUM, NUM):
  case TYPE_PAIR(FLNUM, CHR):
    return c_flo(anum, self) > c_n(bnum) ? t : nil;
  case TYPE_PAIR(FLNUM, FLNUM):
    return c_flo(anum, self) > c_flo(bnum, self) ? t : nil;
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

  invalid_ops(self, anum, bnum);
}

val lt(val anum, val bnum)
{
  val self = lit("<");
tail:
  switch (TYPE_PAIR(type(anum), type(bnum))) {
  case TYPE_PAIR(NUM, NUM):
  case TYPE_PAIR(CHR, CHR):
  case TYPE_PAIR(NUM, CHR):
  case TYPE_PAIR(CHR, NUM):
    return c_n(anum) < c_n(bnum) ? t : nil;
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
    return c_n(anum) < c_flo(bnum, self) ? t : nil;
  case TYPE_PAIR(FLNUM, NUM):
  case TYPE_PAIR(FLNUM, CHR):
    return c_flo(anum, self) < c_n(bnum) ? t : nil;
  case TYPE_PAIR(FLNUM, FLNUM):
    return c_flo(anum, self) < c_flo(bnum, self) ? t : nil;
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

  invalid_ops(self, anum, bnum);
}

val ge(val anum, val bnum)
{
  val self = lit(">=");
tail:
  switch (TYPE_PAIR(type(anum), type(bnum))) {
  case TYPE_PAIR(NUM, NUM):
  case TYPE_PAIR(CHR, CHR):
  case TYPE_PAIR(NUM, CHR):
  case TYPE_PAIR(CHR, NUM):
    return c_n(anum) >= c_n(bnum) ? t : nil;
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
    return c_n(anum) >= c_flo(bnum, self) ? t : nil;
  case TYPE_PAIR(FLNUM, NUM):
  case TYPE_PAIR(FLNUM, CHR):
    return c_flo(anum, self) >= c_n(bnum) ? t : nil;
  case TYPE_PAIR(FLNUM, FLNUM):
    return c_flo(anum, self) >= c_flo(bnum, self) ? t : nil;
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

  invalid_ops(self, anum, bnum);
}

val le(val anum, val bnum)
{
  val self = lit("<=");
tail:
  switch (TYPE_PAIR(type(anum), type(bnum))) {
  case TYPE_PAIR(NUM, NUM):
  case TYPE_PAIR(CHR, CHR):
  case TYPE_PAIR(NUM, CHR):
  case TYPE_PAIR(CHR, NUM):
    return c_n(anum) <= c_n(bnum) ? t : nil;
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
    return c_n(anum) <= c_flo(bnum, self) ? t : nil;
  case TYPE_PAIR(FLNUM, NUM):
  case TYPE_PAIR(FLNUM, CHR):
    return c_flo(anum, self) <= c_n(bnum) ? t : nil;
  case TYPE_PAIR(FLNUM, FLNUM):
    return c_flo(anum, self) <= c_flo(bnum, self) ? t : nil;
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

  invalid_ops(self, anum, bnum);
}

val numeq(val anum, val bnum)
{
  val self = lit("=");
tail:
  switch (TYPE_PAIR(type(anum), type(bnum))) {
  case TYPE_PAIR(NUM, NUM):
  case TYPE_PAIR(CHR, CHR):
  case TYPE_PAIR(NUM, CHR):
  case TYPE_PAIR(CHR, NUM):
    return c_n(anum) == c_n(bnum) ? t : nil;
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
    return c_n(anum) == c_flo(bnum, self) ? t : nil;
  case TYPE_PAIR(FLNUM, NUM):
  case TYPE_PAIR(FLNUM, CHR):
    return c_flo(anum, self) == c_n(bnum) ? t : nil;
  case TYPE_PAIR(FLNUM, FLNUM):
    return c_flo(anum, self) == c_flo(bnum, self) ? t : nil;
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

  invalid_ops(self, anum, bnum);
}

val expt(val anum, val bnum)
{
  val self = lit("expt");

tail:
  switch (TYPE_PAIR(type(anum), type(bnum))) {
  case TYPE_PAIR(NUM, NUM):
    {
      cnum a = c_n(anum);
      cnum b = c_n(bnum);
      mp_int tmpa;
      val n;
      mp_err mpe = MP_OKAY;
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
        mpe = mp_expt_d(&tmpa, b, mp(n));
      } else {
        mp_int tmpb;
        mp_init(&tmpb);
        mp_set_intptr(&tmpb, b);
        mpe = mp_expt(&tmpa, &tmpb, mp(n));
        mp_clear(&tmpb);
      }
      mp_clear(&tmpa);
      if (mpe != MP_OKAY)
        do_mp_error(self, mpe);
      return normalize(n);
    }
  case TYPE_PAIR(NUM, BGNUM):
    {
      cnum a = c_n(anum);
      mp_int tmpa;
      val n;
      mp_err mpe = MP_OKAY;
      if (mp_cmp_z(mp(bnum)) == MP_LT)
        goto negexp;
      n = make_bignum();
      mp_init(&tmpa);
      mp_set_intptr(&tmpa, a);
      mpe = mp_expt(&tmpa, mp(bnum), mp(n));
      mp_clear(&tmpa);
      if (mpe != MP_OKAY)
        do_mp_error(self, mpe);
      return normalize(n);
    }
  case TYPE_PAIR(BGNUM, NUM):
    {
      cnum b = c_n(bnum);
      val n;
      mp_err mpe = MP_OKAY;
      if (b < 0)
        goto negexp;
      if (bnum == zero)
        return one;
      if (bnum == one)
        return anum;
      n = make_bignum();
      if (sizeof (int_ptr_t) <= sizeof (mp_digit)) {
        mpe = mp_expt_d(mp(anum), b, mp(n));
      } else {
        mp_int tmpb;
        mp_init(&tmpb);
        mp_set_intptr(&tmpb, b);
        mpe = mp_expt(mp(anum), &tmpb, mp(n));
        mp_clear(&tmpb);
      }
      if (mpe != MP_OKAY)
        do_mp_error(self, mpe);
      return normalize(n);
    }
  case TYPE_PAIR(BGNUM, BGNUM):
    {
      val n;
      mp_err mpe = MP_OKAY;
      if (mp_cmp_z(mp(bnum)) == MP_LT)
        goto negexp;
      n = make_bignum();
      mpe = mp_expt(mp(anum), mp(bnum), mp(n));
      if (mpe != MP_OKAY)
        do_mp_error(self, mpe);
      normalize(n);
      return n;
    }
  case TYPE_PAIR(NUM, FLNUM):
    /* TODO: error checking */
    return flo(pow(c_n(anum), c_flo(bnum, self)));
  case TYPE_PAIR(FLNUM, NUM):
    return flo(pow(c_flo(anum, self), c_n(bnum)));
  case TYPE_PAIR(FLNUM, FLNUM):
    return flo(pow(c_flo(anum, self), c_flo(bnum, self)));
  case TYPE_PAIR(BGNUM, FLNUM):
    anum = flo_int(anum);
    goto tail;
  case TYPE_PAIR(FLNUM, BGNUM):
    bnum = flo_int(bnum);
    goto tail;
  }

  invalid_ops(self, anum, bnum);
negexp:
  uw_throwf(type_error_s, lit("~a: negative exponent"), self, nao);
}

val exptmod(val base, val exp, val mod)
{
  val self = lit("exptmod");
  mp_err mpe = MP_OKAY;
  val n;

  if (!integerp(base) || !integerp(exp) || !integerp(mod))
    goto inval;

  if (fixnump(base))
    base = bignum(c_n(base));

  if (fixnump(exp))
    exp = bignum(c_n(exp));

  if (fixnump(mod))
    mod = bignum(c_n(mod));

  n = make_bignum();

  if ((mpe = mp_exptmod(mp(base), mp(exp), mp(mod), mp(n))) != MP_OKAY)
    goto bad;

  return normalize(n);
inval:
  uw_throwf(error_s, lit("~a: non-integral operands ~s ~s ~s"),
            self, base, exp, mod, nao);
bad:
  do_mp_error(self, mpe);
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
      cnum a = c_n(anum);
      if (a < 0)
        goto negop;
      return num_fast(isqrt_fixnum(c_n(anum)));
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
    anum = bignum(c_n(anum));

  if (fixnump(bnum))
    bnum = bignum(c_n(bnum));

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

val divides(val d, val n)
{
  if (n == zero) {
    if (!integerp(d))
      uw_throwf(error_s, lit("divides: ~s isn't an integer"),
                d, nao);
    return tnil(!zerop(d));
  }

  if (d == one) {
    if (!integerp(n))
      uw_throwf(error_s, lit("divides: ~s isn't an integer"),
                n, nao);
    return t;
  }

  if (minusp(d))
    d = neg(d);

  return eql(gcd(d, n), d);
}

val floorf(val num)
{
  val self = lit("floor");

  switch (type(num)) {
  case NUM:
  case BGNUM:
    return num;
  case FLNUM:
    return flo(floor(c_flo(num, self)));
  case RNG:
    return rcons(floorf(from(num)), floorf(to(num)));
  default:
    break;
  }

  invalid_op(self, num);
}

val ceili(val num)
{
  val self = lit("ceil");

  switch (type(num)) {
  case NUM:
  case BGNUM:
    return num;
  case FLNUM:
    return flo(ceil(c_flo(num, self)));
  case RNG:
    return rcons(ceili(from(num)), ceili(to(num)));
  default:
    break;
  }

  invalid_op(self, num);
}

val sine(val num)
{
  val self = lit("sin");
  return flo(sin(c_flo(to_float(self, num), self)));
}

val cosi(val num)
{
  val self = lit("cos");
  return flo(cos(c_flo(to_float(self, num), self)));
}

val tang(val num)
{
  val self = lit("tan");
  return flo(tan(c_flo(to_float(self, num), self)));
}

val asine(val num)
{
  val self = lit("asin");
  return flo(asin(c_flo(to_float(self, num), self)));
}

val acosi(val num)
{
  val self = lit("acos");
  return flo(acos(c_flo(to_float(self, num), self)));
}

val atang(val num)
{
  val self = lit("atan");
  return flo(atan(c_flo(to_float(self, num), self)));
}

val atang2(val y, val x)
{
  val self = lit("atan2");
  return flo(atan2(c_flo(to_float(self, y), self),
                   c_flo(to_float(self, x), self)));
}

val loga(val num)
{
  val self = lit("log");
  return flo(log(c_flo(to_float(self, num), self)));
}

val logten(val num)
{
  val self = lit("log10");
  return flo(log10(c_flo(to_float(self, num), self)));
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
  val self = lit("log2");
  return flo(log2(c_flo(to_float(self, num), self)));
}

val expo(val num)
{
  val self = lit("exp");
  return flo(exp(c_flo(to_float(self, num), self)));
}

val sqroot(val num)
{
  val self = lit("sqrt");
  return flo(sqrt(c_flo(to_float(self, num), self)));
}

/*
 * TODO: replace this text-based hack!
 */
val int_flo(val f)
{
  val self = lit("int-flo");
  double d = c_flo(f, self);

  if (d >= INT_PTR_MAX && d <= INT_PTR_MIN - 1) {
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
                lit("~a: cannot convert #<bad-float> to integer"),
                self, nao);

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
  val self = lit("flo-int");

  if (fixnump(i))
    return flo(c_n(i));

  {
    double d;
    type_check(self, i, BGNUM);
    if (mp_to_double(mp(i), &d) != MP_OKAY)
      uw_throwf(error_s, lit("~a: bignum to float conversion failed"),
                self, nao);
    return flo(d);
  }
}

val logand(val a, val b)
{
  val c;

  if (zerop(a) || zerop(b))
    return zero;

  switch (TYPE_PAIR(type(a), type(b))) {
  case TYPE_PAIR(NUM, CHR):
  case TYPE_PAIR(CHR, NUM):
    if (a == b) {
      return a;
    } else {
      cnum ac = c_n(a);
      cnum bc = c_n(b);
      return chr(ac & bc);
    }
  case TYPE_PAIR(NUM, NUM):
    if (a == b) {
      return a;
    } else {
      cnum ac = c_n(a);
      cnum bc = c_n(b);
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
    a = bignum(c_n(a));
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
  case TYPE_PAIR(NUM, CHR):
  case TYPE_PAIR(CHR, NUM):
    if (a == b) {
      return a;
    } else {
      cnum ac = c_n(a);
      cnum bc = c_n(b);
      return chr(ac | bc);
    }
  case TYPE_PAIR(NUM, NUM):
    if (a == b) {
      return a;
    } else {
      cnum ac = c_n(a);
      cnum bc = c_n(b);
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
    a = bignum(c_n(a));
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
  case TYPE_PAIR(NUM, CHR):
  case TYPE_PAIR(CHR, NUM):
    if (a == b) {
      return a;
    } else {
      cnum ac = c_n(a);
      cnum bc = c_n(b);
      return chr(ac ^ bc);
    }
  case TYPE_PAIR(NUM, NUM):
    if (a == b) {
      return a;
    } else {
      cnum ac = c_n(a);
      cnum bc = c_n(b);
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
    a = bignum(c_n(a));
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

  bn = c_n(bits);

  if (bn < 0)
    goto bad4;

  switch (type(a)) {
  case NUM:
    an = c_n(a);
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

bad4:
  uw_throwf(error_s, lit("lognot: negative bits value ~s"), bits, nao);
}

val lognot(val a, val bits)
{
  val b;

  if (default_null_arg(bits))
    return comp_trunc(a, bits);

  switch (type(a)) {
  case NUM:
    return num_fast(~c_n(a));
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
  val self = lit("logtrunc");
  cnum an, bn;
  val b;
  const cnum num_mask = (NUM_MAX << 1) | 1;
  const int num_bits = CHAR_BIT * sizeof (cnum) - TAG_SHIFT;

  if (!fixnump(bits))
    goto bad2;

  bn = c_n(bits);

  if (bn < 0)
    goto bad4;

  switch (type(a)) {
    mp_err mpe;
  case NUM:
    an = c_n(a);
    if (bn <= num_bits) {
      cnum mask = num_mask >> (num_bits - bn);
      return num_fast(an & mask);
    }
    a = bignum(an);
    /* fallthrough */
  case BGNUM:
    b = make_ubignum();
    if ((mpe = mp_trunc(mp(a), mp(b), bn)) != MP_OKAY)
      do_mp_error(self, mpe);
    return normalize(b);
  default:
    goto bad3;
  }

bad2:
  uw_throwf(error_s, lit("~a: bits value ~s is not a fixnum"), self, bits, nao);

bad3:
  uw_throwf(error_s, lit("~a: non-integral operand ~s"), self, a, nao);

bad4:;
  uw_throwf(error_s, lit("~a: negative bits value ~s"), self, bits, nao);
}

val sign_extend(val n, val nbits)
{
  val msb = minus(nbits, one);
  val ntrunc = logtrunc(n, nbits);

  if (bit(ntrunc, msb)) {
    switch (type(ntrunc)) {
    case NUM:
      {
        cnum cn = c_n(ntrunc);
        cnum nb = c_n(nbits);
        return num(cn | (INT_PTR_MAX << nb));
      }
    case BGNUM:
      {
        val out = make_ubignum();
        mp_err mpe;
        mp_2comp(mp(ntrunc), mp(out), mp(ntrunc)->used);
        if ((mpe = mp_trunc(mp(out), mp(out), c_n(nbits))) != MP_OKAY)
          do_mp_error(lit("sign-extend"), mpe);
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
  val self = lit("ash");
  cnum an, bn;
  val b;
  int hb;
  const int num_bits = CHAR_BIT * sizeof (cnum) - TAG_SHIFT;
  mp_err mpe = MP_OKAY;

  if (!fixnump(bits))
    goto bad2;

  bn = c_n(bits);

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
      an = c_n(a);
      hb = highest_significant_bit(an);
      if (bn + hb < num_bits)
        return num_fast(an << bn);
      a = bignum(an);
      /* fallthrough */
    case BGNUM:
      if (bn < INT_MIN || bn > INT_MAX)
        goto bad4;
      b = make_bignum();
      if ((mpe = mp_shift(mp(a), mp(b), bn)) != MP_OKAY)
        break;
      return normalize(b);
    default:
      goto bad3;
    }
  } else {
    switch (type(a)) {
    case NUM:
      bn = -bn;
      an = c_n(a);
      if (bn <= num_bits)
        return num_fast(an >> bn);
      return num_fast(an >> num_bits);
    case BGNUM:
      b = make_bignum();
      if ((mpe = mp_shift(mp(a), mp(b), bn)) != MP_OKAY)
        break;
      return normalize(b);
    default:
      goto bad3;
    }
  }

  do_mp_error(self, mpe);

bad2:
  uw_throwf(error_s, lit("~a: bits value ~s is not a fixnum"), self, bits, nao);

bad3:
  uw_throwf(error_s, lit("~a: non-integral operand ~s"), self, a, nao);

bad4:
  uw_throwf(error_s, lit("~a: bit value too large ~s"), self, bits, nao);
}

val bit(val a, val bit)
{
  val self = lit("bit");
  cnum bn;
  mp_err mpe = MP_OKAY;

  if (!fixnump(bit))
    goto bad;

  bn = c_n(bit);

  if (bn < 0)
    goto bad2;

  switch (type(a)) {
  case NUM:
  case CHR:
    {
      cnum an = c_n(a);
      if (bn < (SIZEOF_PTR * CHAR_BIT))
        return (an & (convert(cnum, 1) << bn)) ? t : nil;
      return an < 0 ? t : nil;
    }
  case BGNUM:
    {
      mpe = mp_bit(mp(a), bn);

      switch (mpe) {
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
  uw_throwf(error_s, lit("~a: bit position ~s is not a fixnum"), self, bit, nao);

bad2:
  uw_throwf(error_s, lit("~a: bit position ~s is negative"), self, bit, nao);

bad3:
  uw_throwf(error_s, lit("~a: non-integral operand ~s"), self, a, nao);

bad4:
  do_mp_error(self, mpe);
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

val logcount(val n)
{
  val self = lit("logcount");

  switch (type(n)) {
  case NUM:
  case CHR:
    {
      int_ptr_t c = c_n(n);
      uint_ptr_t d = c;
      if (c < 0)
        d = ~d;
#if SIZEOF_PTR == 8
      d = ((d & 0xAAAAAAAAAAAAAAAA) >>  1) + (d & 0x5555555555555555);
      d = ((d & 0xCCCCCCCCCCCCCCCC) >>  2) + (d & 0x3333333333333333);
      d = ((d & 0xF0F0F0F0F0F0F0F0) >>  4) + (d & 0x0F0F0F0F0F0F0F0F);
      d = ((d & 0xFF00FF00FF00FF00) >>  8) + (d & 0x00FF00FF00FF00FF);
      d = ((d & 0xFFFF0000FFFF0000) >> 16) + (d & 0x0000FFFF0000FFFF);
      d = ((d & 0xFFFFFFFF00000000) >> 32) + (d & 0x00000000FFFFFFFF);
#elif SIZEOF_PTR == 4
      d = ((d & 0xAAAAAAAA) >>  1) + (d & 0x55555555);
      d = ((d & 0xCCCCCCCC) >>  2) + (d & 0x33333333);
      d = ((d & 0xF0F0F0F0) >>  4) + (d & 0x0F0F0F0F);
      d = ((d & 0xFF00FF00) >>  8) + (d & 0x00FF00FF);
      d = ((d & 0xFFFF0000) >> 16) + (d & 0x0000FFFF);
      return unum(d);
#else
#error fixme: only 4 or 8 byte pointers supported
#endif
    }
  case BGNUM:
    {
      mp_size co = mp_count_ones(mp(n));
      return unum(co);
    }
  default:
    uw_throwf(error_s, lit("~a: non-integral operand ~s"), self, n, nao);
  }
}

/*
 * Source:
 * Better Approximations to Cumulative Normal Functions
 * Graeme West
 * 2009
 */
val cum_norm_dist(val arg)
{
  val self = lit("cum-norm-dist");
  val arg_flo = to_float(self, arg);
  double x = c_flo(arg_flo, self);
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

/*
 * Source:
 * Odeh and Evans approximation.
 * 1974
 */
val inv_cum_norm(val arg)
{
  val self = lit("inv-cum-norm");
  val arg_flo = to_float(self, arg);
  double p = c_flo(arg_flo, self);
  int is_upper_half = (p >= 0.5);
  double r = is_upper_half ? 1 - p : p;
  if (r < 1E-20) {
    return flo(is_upper_half ? 10 : -10);
  } else {
    double y = sqrt(-2*log(r));
    double a = ((((4.53642210148e-5*y + 0.0204231210245)*y +
                  0.342242088547)*y + 1)*y + 0.322232431088);
    double b = ((((0.0038560700634*y + 0.10353775285)*y +
                  0.531103462366)*y + 0.588581570495)*y + 0.099348462606);
    double z = y - a / b;
    return flo(is_upper_half ? z : -z);
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
      cnum ch = c_n(obj);
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
      cnum ch = c_n(obj);

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
      cnum n = c_n(obj);

      if (n < 0) {
        n &= INT_PTR_MAX;
        n ^= INT_PTR_MAX;
        return num_fast(highest_bit(n));
      }
      return num_fast(highest_bit(n));
    }
  case TAG_PTR:
    if (type(obj) == BGNUM) {
      mp_size count;
      if (mp_cmp_z(mp(obj)) == MP_LT) {
        mp_int tmp;
        mp_size i;

        mp_2comp(mp(obj), &tmp, mp(obj)->used);

        for (i = 0; i < tmp.used; i++)
          tmp.dp[i] ^= MP_DIGIT_MAX;

        count = mp_count_bits(&tmp);
        mp_clear(&tmp);
      } else {
        count = mp_count_bits(mp(obj));
      }
      return unum(count);
    }
  default:
    break;
  }
  uw_throwf(error_s, lit("width: ~s isn't an integer"), obj, nao);
}

val bits(val obj)
{
  return normalize(bignum_from_uintptr(coerce(uint_ptr_t, obj)));
}

static val digcommon(int pow, val self, val n, val base_in)
{
  val r = default_arg_strict(base_in, num_fast(10));

  if (!integerp(n) || minusp(n))
    uw_throwf(error_s, lit("~a: value ~s must be positive integer"),
              self, n, nao);
  if (!integerp(r) || lt(r, one))
    uw_throwf(error_s, lit("~a: base ~s must be positive integer"),
              self, r, nao);

  {
    val k = r;
    val p = nil, p0;
    list_collect_decl (out, ptail);

    while (lt(k, n)) {
      push(k, &p);
      k = mul(k, r);
    }

    while ((p0 = pop(&p))) {
      cnum i = 0;
      while (ge(n, p0)) {
        i++;
        n = minus(n, p0);
      }
      ptail = list_collect(ptail, if3(pow,
                                      mul(num_fast(i), p0),
                                      num_fast(i)));
    }

    list_collect(ptail, n);

    return out;
  }
}

val digpow(val n, val base)
{
  return digcommon(1, lit("digpow"), n, base);
}

val digits(val n, val base)
{
  return digcommon(0, lit("digits"), n, base);
}

val poly(val x, val seq)
{
  val self = lit("rpoly");
  val acc = zero;
  seq_info_t si = seq_info(seq);

  if (!numberp(x))
    uw_throwf(error_s, lit("~a: bad argument ~s; number required"),
              self, x, nao);

  switch (si.kind) {
  case SEQ_NIL:
    return acc;
  case SEQ_LISTLIKE:
    while (consp(seq)) {
      val coeff = pop(&seq);
      acc = plus(mul(acc, x), coeff);
    }

    (void) endp(seq);
    return acc;
  case SEQ_VECLIKE:
    {
      cnum len = c_n(length(seq)), i = 0;

      while (i < len) {
        val coeff = ref(seq, num(i++));
        acc = plus(mul(acc, x), coeff);
      }

      return acc;
    }
  default:
    uw_throwf(error_s, lit("~a: bad argument ~s; poly wants a sequence!"),
              self, seq, nao);

  }
}

val rpoly(val x, val seq)
{
  val self = lit("poly");
  val acc = zero;
  val pow = x;
  seq_info_t si = seq_info(seq);

  if (!numberp(x))
    uw_throwf(error_s, lit("~a: bad argument ~s; poly wants a number!"),
              self, x, nao);

  switch (si.kind) {
  case SEQ_NIL:
    return acc;
  case SEQ_LISTLIKE:
    if (consp(seq))
      acc = pop(&seq);

    while (consp(seq)) {
      val coeff = pop(&seq);
      acc = plus(acc, mul(pow, coeff));
      if (seq)
        pow = mul(pow, x);
    }

    (void) endp(seq);
    return acc;
  case SEQ_VECLIKE:
    {
      cnum len = c_n(length(seq)), i = len;

      while (i > 0) {
        val coeff = ref(seq, num(--i));
        acc = plus(mul(acc, x), coeff);
      }

      return acc;
    }
  default:
    uw_throwf(error_s, lit("~a: bad argument ~s; poly wants a sequence!"),
              self, seq, nao);

  }

  return acc;
}

#if HAVE_ROUNDING_CTL_H

static val flo_get_round_mode(void)
{
  return num(fegetround());
}

static val flo_set_round_mode(val mode)
{
  return tnil(!fesetround(c_int(mode, lit("flo-set-round-mode"))));
}

#endif

void arith_init(void)
{
  mp_init(&NUM_MAX_MP);
  mp_set_intptr(&NUM_MAX_MP, NUM_MAX);
  mp_init(&INT_PTR_MAX_MP);
  mp_set_intptr(&INT_PTR_MAX_MP, INT_PTR_MAX);
  mp_init(&UINT_PTR_MAX_MP);
  mp_set_uintptr(&UINT_PTR_MAX_MP, -1);
  mp_init(&INT_PTR_MAX_SUCC_MP);
  mp_set_intptr(&INT_PTR_MAX_SUCC_MP, INT_PTR_MIN - 1);
  mp_neg(&INT_PTR_MAX_SUCC_MP, &INT_PTR_MAX_SUCC_MP);
  log2_init();

  if (opt_compat && opt_compat <= 199) {
    reg_varl(intern(lit("*flo-dig*"), user_package), num_fast(DBL_DIG));
    reg_varl(intern(lit("*flo-max*"), user_package), flo(DBL_MAX));
    reg_varl(intern(lit("*flo-min*"), user_package), flo(DBL_MIN));
    reg_varl(intern(lit("*flo-epsilon*"), user_package), flo(DBL_EPSILON));
  }

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
  reg_varl(intern(lit("%pi%"), user_package), flo(M_PI));
#ifndef M_E
#define M_E 2.71828182845904523536
#endif
  reg_varl(intern(lit("%e%"), user_package), flo(M_E));

  if (opt_compat && opt_compat <= 199) {
    reg_varl(intern(lit("*pi*"), user_package), flo(M_PI));
    reg_varl(intern(lit("*e*"), user_package), flo(M_E));
  }

  reg_fun(intern(lit("signum"), user_package), func_n1(signum));

  reg_fun(intern(lit("bignum-len"), user_package), func_n1(bignum_len));
  reg_fun(intern(lit("divides"), user_package), func_n2(divides));
  reg_fun(intern(lit("bits"), system_package), func_n1(bits));
  reg_fun(intern(lit("digpow"), user_package), func_n2o(digpow, 1));
  reg_fun(intern(lit("digits"), user_package), func_n2o(digits, 1));
  reg_fun(intern(lit("poly"), user_package), func_n2(poly));
  reg_fun(intern(lit("rpoly"), user_package), func_n2(rpoly));

  reg_fun(intern(lit("b<"), system_package), func_n2(lt));
  reg_fun(intern(lit("b>"), system_package), func_n2(gt));
  reg_fun(intern(lit("b<="), system_package), func_n2(le));
  reg_fun(intern(lit("b=>"), system_package), func_n2(ge));
  reg_fun(intern(lit("b="), system_package), func_n2(numeq));
  reg_fun(intern(lit("b+"), system_package), func_n2(plus));
  reg_fun(intern(lit("b-"), system_package), func_n2(minus));
  reg_fun(intern(lit("b*"), system_package), func_n2(mul));
  reg_fun(intern(lit("b/"), system_package), func_n2(divi));
  reg_fun(intern(lit("neg"), system_package), func_n1(neg));

#if HAVE_ROUNDING_CTL_H
  reg_varl(intern(lit("flo-near"), user_package), num(FE_TONEAREST));
  reg_varl(intern(lit("flo-down"), user_package), num(FE_DOWNWARD));
  reg_varl(intern(lit("flo-up"), user_package), num(FE_UPWARD));
  reg_varl(intern(lit("flo-zero"), user_package), num(FE_TOWARDZERO));
  reg_fun(intern(lit("flo-get-round-mode"), user_package),
          func_n0(flo_get_round_mode));
  reg_fun(intern(lit("flo-set-round-mode"), user_package),
          func_n1(flo_set_round_mode));
#endif
}

void arith_free_all(void)
{
  mp_clear(&NUM_MAX_MP);
  mp_clear(&INT_PTR_MAX_MP);
  mp_clear(&UINT_PTR_MAX_MP);
  mp_clear(&INT_PTR_MAX_SUCC_MP);
}
