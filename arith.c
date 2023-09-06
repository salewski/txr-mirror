/* Copyright 2010-2023
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

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <wctype.h>
#include <wchar.h>
#include <math.h>
#include <signal.h>
#include <ctype.h>
#include <float.h>
#include "config.h"
#if HAVE_ROUNDING_CTL_H
#include <fenv.h>
#endif
#include "alloca.h"
#include "lib.h"
#include "signal.h"
#include "unwind.h"
#include "gc.h"
#include "args.h"
#include "eval.h"
#include "itypes.h"
#include "struct.h"
#include "txr.h"
#include "psquare.h"
#include "autoload.h"
#include "arith.h"

#define max(a, b) ((a) > (b) ? (a) : (b))

#define ABS(A) ((A) < 0 ? -(A) : (A))

val plus_s, minus_s, inv_minus_s, neg_s, abs_s, signum_s;
val mul_s, div_s, recip_s, inv_div_s;
val trunc1_s, trunc_s, r_trunc_s, mod_s, r_mod_s;
val zerop_s, plusp_s, minusp_s, evenp_s, oddp_s;
val gt_s, lt_s, ge_s, le_s, numeq_s;
val expt_s, r_expt_s, exptmod_s, isqrt_s, square_s;
val floor_s, floor1_s, r_floor_s;
val ceil_s, ceil1_s, r_ceil_s;
val round_s, round1_s, r_round_s;
val sin_s, cos_s, tan_s, asin_s, acos_s, atan_s, atan2_s, r_atan2_s;
val sinh_s, cosh_s, tanh_s, asinh_s, acosh_s, atanh_s;
val log_s, log2_s, log10_s, exp_s, sqrt_s;
val logand_s, logior_s, logxor_s;
val lognot1_s, lognot_s, r_lognot_s, logtrunc_s, r_logtrunc_s;
val sign_extend_s, ash_s, bit_s, width_s, bitset_s, logcount_s;

val cbrt_s, erf_s, erfc_s, exp10_s, exp2_s, expm1_s;
val gamma_s, j0_s, j1_s, lgamma_s, log1p_s, logb_s;
val nearbyint_s, rint_s, significand_s, tgamma_s, y0_s, y1_s;
val copysign_s, drem_s, fdim_s, fmax_s, fmin_s, hypot_s;
val jn_s, ldexp_s, nextafter_s, remainder_s, scalb_s;
val scalbln_s, yn_s;
val r_copysign_s, r_drem_s, r_fdim_s, r_fmax_s, r_fmin_s, r_hypot_s;
val r_jn_s, r_ldexp_s, r_nextafter_s, r_remainder_s, r_scalb_s;
val r_scalbln_s, r_yn_s;

val tofloat_s, toint_s;

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

static NORETURN void not_number(val self, val obj)
{
  uw_throwf(type_error_s, lit("~a: ~s is not a number"), self, obj, nao);
}

static NORETURN void not_integer(val self, val obj)
{
  uw_throwf(type_error_s, lit("~a: ~s is not an integer"), self, obj, nao);
}

int num_to_buffer(val num, mem_t *buf, int bytes)
{
  switch (type(num)) {
  case CHR: case NUM:
    {
      cnum n = c_n(num);
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

val bignum_dbl_ipt(double_intptr_t di)
{
  val n = make_bignum();
  mp_set_double_intptr(mp(n), di);
  return n;
}

val bignum_dbl_uipt(double_uintptr_t dui)
{
  val n = make_bignum();
  mp_set_double_uintptr(mp(n), dui);
  return n;
}

#endif

val normalize(val bignum)
{
  if (!mp_in_range(mp(bignum), NUM_MAX, 0)) {
    return bignum;
  } else {
    cnum fixnum;
    mp_get_intptr(mp(bignum), &fixnum);
    return num(fixnum);
  }
}

ucnum c_unum(val num, val self)
{
  switch (type(num)) {
  case CHR:
    {
      cnum n = c_ch(num);
      if (n >= 0)
        return n;
    }
    goto range;
  case NUM:
    {
      cnum n = c_n(num);
      if (n >= 0)
        return n;
    }
    goto range;
  case BGNUM:
    if (mp_in_uintptr_range(mp(num))) {
      uint_ptr_t out;
      mp_get_uintptr(mp(num), &out);
      return out;
    }
    /* fallthrough */
  range:
    uw_throwf(error_s, lit("~a: ~s is out of allowed range [0, ~a]"),
              self, num, unum(UINT_PTR_MAX), nao);
  default:
    uw_throwf(type_error_s, lit("~a: ~s is not an integer"), self, num, nao);
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

#if HAVE_DOUBLE_INTPTR_T

dbl_cnum c_dbl_num(val n)
{
  switch (type(n)) {
  case CHR:
    return c_ch(n);
  case NUM:
    return c_n(n);
  case BGNUM:
    if (mp_in_double_intptr_range(mp(n))) {
      double_intptr_t out;
      mp_get_double_intptr(mp(n), &out);
      return out;
    }
    uw_throwf(error_s, lit("~s is out of signed ~a bit range"),
              n, num_fast(SIZEOF_DOUBLE_INTPTR * CHAR_BIT), nao);
  default:
    type_mismatch(lit("~s is not an integer"), n, nao);
  }
}

dbl_ucnum c_dbl_unum(val n)
{
  switch (type(n)) {
  case CHR:
    {
      dbl_cnum cn = c_ch(n);
      if (cn >= 0)
        return cn;
      break;
    }
  case NUM:
    {
      dbl_cnum cn = c_n(n);
      if (cn >= 0)
        return cn;
      break;
    }
  case BGNUM:
    if (mp_in_double_uintptr_range(mp(n))) {
      double_uintptr_t out;
      mp_get_double_uintptr(mp(n), &out);
      return out;
    }
    break;
  default:
    type_mismatch(lit("~s is not an integer"), n, nao);
  }
  uw_throwf(error_s, lit("~s is out of unsigned ~a bit range"),
            n, num_fast(SIZEOF_DOUBLE_INTPTR * CHAR_BIT), nao);
}

#endif

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
#if defined __GNUC__ && SIZEOF_PTR == SIZEOF_INT
  return (n == 0) ? 0 : (CHAR_BIT * SIZEOF_PTR - __builtin_clz(n));
#elif defined __GNUC__ && SIZEOF_PTR == SIZEOF_LONG
  return (n == 0) ? 0 : (CHAR_BIT * SIZEOF_PTR - __builtin_clzl(n));
#elif defined __GNUC__ && SIZEOF_PTR == SIZEOF_LONGLONG_T
  return (n == 0) ? 0 : (CHAR_BIT * SIZEOF_PTR - __builtin_clzll(n));
#elif CHAR_BIT * SIZEOF_PTR == 64
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
#elif CHAR_BIT * SIZEOF_PTR == 32
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
  return highest_bit(-n - 1);
}

static UNUSED NORETURN void not_available(val name)
{
  uw_throwf(file_error_s, lit("~a is not available on this platform"),
            name, nao);
}

void do_mp_error(val self, mp_err code)
{
  val errstr = string_utf8(mp_strerror(code));
  uw_throwf(numeric_error_s, lit("~a: ~a"), self, errstr, nao);
}

static NORETURN void not_struct_error(val self, val obj)
{
  uw_throwf(error_s, lit("~a: ~s isn't a structure"),
            self, obj, nao);
}

static NORETURN void method_error(val self, val obj, val fun)
{
  uw_throwf(error_s, lit("~a: object ~s lacks ~a method"),
            self, obj, fun, nao);
}

static val do_unary_method(val self, val sym, val obj)
{
  val meth = maybe_slot(obj, sym);

  if (!obj_struct_p(obj))
    not_struct_error(self, obj);

  if (!meth)
    method_error(self, obj, sym);

  return funcall1(meth, obj);
}

static val do_binary_method(val self, val sym, val obj, val arg)
{
  val meth = maybe_slot(obj, sym);

  if (!obj_struct_p(obj))
    not_struct_error(self, obj);

  if (!meth)
    method_error(self, obj, sym);

  return funcall2(meth, obj, arg);
}

static val do_ternary_method(val self, val sym, val obj, val arg1, val arg2)
{
  val meth = maybe_slot(obj, sym);

  if (!obj_struct_p(obj))
    not_struct_error(self, obj);

  if (!meth)
    method_error(self, obj, sym);

  return funcall3(meth, obj, arg1, arg2);
}

val plus(val anum, val bnum)
{
  val self = plus_s;

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
    case COBJ:
      return do_binary_method(self, self, bnum, anum);
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
    case COBJ:
      return do_binary_method(self, self, anum, bnum);
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
    case TYPE_PAIR(COBJ, BGNUM):
    case TYPE_PAIR(COBJ, FLNUM):
    case TYPE_PAIR(COBJ, RNG):
    case TYPE_PAIR(COBJ, COBJ):
      return do_binary_method(self, self, anum, bnum);
    case TYPE_PAIR(BGNUM, COBJ):
    case TYPE_PAIR(FLNUM, COBJ):
    case TYPE_PAIR(RNG, COBJ):
      return do_binary_method(self, self, bnum, anum);
    default:
      break;
    }
    break;
  case TAG_PAIR(TAG_CHR, TAG_NUM):
    {
      wchar_t a = c_ch(anum);
      cnum b = c_n(bnum);
      cnum sum = a + b;

      if (sum < 0 || sum > 0x10FFFF)
        goto char_range;
      return chr(sum);
    }
  case TAG_PAIR(TAG_NUM, TAG_CHR):
    {
      cnum a = c_n(anum);
      wchar_t b = c_ch(bnum);
      cnum sum = a + b;

      if (sum < 0 || sum > 0x10FFFF)
        goto char_range;
      return chr(sum);
    }
  case TAG_PAIR(TAG_CHR, TAG_PTR):
    if (type(bnum) == RNG)
      return rcons(plus(anum, from(bnum)), plus(anum, to(bnum)));
    break;
  case TAG_PAIR(TAG_PTR, TAG_CHR):
    if (type(anum) == RNG)
      return rcons(plus(from(anum), bnum), plus(to(anum), bnum));
    break;
  }
  invalid_ops(self, anum, bnum);
char_range:
  uw_throwf(numeric_error_s,
            lit("~a: sum of ~s and ~s is out of character range"),
            self, anum, bnum, nao);
}

val minus(val anum, val bnum)
{
  val self = minus_s;

tail:
  switch (TAG_PAIR(tag(anum), tag(bnum))) {
  case TAG_PAIR(TAG_CHR, TAG_CHR):
    {
      cnum a = c_ch(anum);
      cnum b = c_ch(bnum);
      cnum sum = a - b;

      if (sum < NUM_MIN || sum > NUM_MAX)
        return bignum(sum);
      return num_fast(sum);
    }
  case TAG_PAIR(TAG_NUM, TAG_NUM):
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
    case COBJ:
      return do_binary_method(self, inv_minus_s, bnum, anum);
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
    case COBJ:
      return do_binary_method(self, self, anum, bnum);
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
    case TYPE_PAIR(COBJ, BGNUM):
    case TYPE_PAIR(COBJ, FLNUM):
    case TYPE_PAIR(COBJ, RNG):
    case TYPE_PAIR(COBJ, COBJ):
      return do_binary_method(self, self, anum, bnum);
    case TYPE_PAIR(BGNUM, COBJ):
    case TYPE_PAIR(FLNUM, COBJ):
    case TYPE_PAIR(RNG, COBJ):
      return do_binary_method(self, inv_minus_s, bnum, anum);
    default:
      break;
    }
    break;
  case TAG_PAIR(TAG_CHR, TAG_NUM):
    {
      wchar_t a = c_ch(anum);
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
  val self = minus_s;

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
  case COBJ:
    return do_unary_method(self, neg_s, anum);
  default:
    not_number(self, anum);
  }
}

val abso(val anum)
{
  val self = abs_s;

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
  case COBJ:
    return do_unary_method(self, self, anum);
  default:
    not_number(self, anum);
  }
}

static val signum(val anum)
{
  val self = signum_s;

  switch (type(anum)) {
  case BGNUM:
    return if3(mp_isneg(mp(anum)), negone, one);
  case FLNUM:
    {
      double a = c_f(anum);
      return flo(if3(a > 0, 1.0, if3(a < 0, -1.0, 0.0)));
    }
  case NUM:
    {
      cnum a = c_n(anum);
      return if3(a > 0, one, if3(a < 0, negone, zero));
    }
  case COBJ:
    return do_unary_method(self, self, anum);
  default:
    not_number(self, anum);
  }
}

val mul(val anum, val bnum)
{
  val self = mul_s;

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
      if (highest_bit(ap) + highest_bit(bp) < PTR_BIT - 1) {
        cnum product = a * b;
        if (product >= NUM_MIN && product <= NUM_MAX)
          return num_fast(product);
        return bignum(product);
      } else {
        val n = make_bignum();
        mp_int tmpb;
        mp_err mpe;
        mp_init(&tmpb);
        mp_set_intptr(&tmpb, b);
        mp_set_intptr(mp(n), a);
        mpe = mp_mul(mp(n), &tmpb, mp(n));
        mp_clear(&tmpb);
        if (mpe != MP_OKAY)
          do_mp_error(self, mpe);
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
    case COBJ:
      return do_binary_method(self, self, bnum, anum);
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
    case COBJ:
      return do_binary_method(self, self, anum, bnum);
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
    case TYPE_PAIR(COBJ, BGNUM):
    case TYPE_PAIR(COBJ, FLNUM):
    case TYPE_PAIR(COBJ, RNG):
    case TYPE_PAIR(COBJ, COBJ):
      return do_binary_method(self, self, anum, bnum);
    case TYPE_PAIR(BGNUM, COBJ):
    case TYPE_PAIR(FLNUM, COBJ):
    case TYPE_PAIR(RNG, COBJ):
      return do_binary_method(self, self, bnum, anum);
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
  case COBJ:
    return do_unary_method(self, trunc1_s, num);
  default:
    break;
  }
  invalid_op(self, num);
}

static NORETURN void divzero(val self)
{
  uw_throwf(numeric_error_s, lit("~a: division by zero"), self, nao);
}

val trunc(val anum, val bnum)
{
  val self = trunc_s;

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
    case COBJ:
      return do_binary_method(self, r_trunc_s, bnum, anum);
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
    case COBJ:
      return do_binary_method(self, self, anum, bnum);
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
    case TYPE_PAIR(COBJ, BGNUM):
    case TYPE_PAIR(COBJ, FLNUM):
    case TYPE_PAIR(COBJ, COBJ):
      return do_binary_method(self, self, anum, bnum);
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
  val self = mod_s;

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
    case COBJ:
      return do_binary_method(self, r_mod_s, bnum, anum);
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
    case COBJ:
      return do_binary_method(self, self, anum, bnum);
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
    case TYPE_PAIR(COBJ, BGNUM):
    case TYPE_PAIR(COBJ, FLNUM):
    case TYPE_PAIR(COBJ, COBJ):
      return do_binary_method(self, self, anum, bnum);
    case TYPE_PAIR(BGNUM, COBJ):
    case TYPE_PAIR(FLNUM, COBJ):
      return do_binary_method(self, r_mod_s, bnum, anum);
    }
  }
  invalid_ops(self, anum, bnum);
divzero:
  divzero(self);
}

val floordiv(val anum, val bnum)
{
  val self = floor_s;

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
        if (a < 0 && !mp_isneg(mp(bnum)))
          return negone;
        if (a > 0 && mp_isneg(mp(bnum)))
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
    case COBJ:
      return do_binary_method(self, r_floor_s, bnum, anum);
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
          if (rem && ((mp_isneg(mp(anum)) && b > 0) ||
                      (!mp_isneg(mp(anum)) && b < 0)))
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
              ((mp_isneg(mp(anum)) && b > 0) ||
               (!mp_isneg(mp(anum)) && b < 0)))
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
    case COBJ:
      return do_binary_method(self, self, anum, bnum);
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
              ((mp_isneg(mp(anum)) && !mp_isneg(mp(bnum))) ||
               (!mp_isneg(mp(anum)) && mp_isneg(mp(bnum)))))
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
    case TYPE_PAIR(COBJ, BGNUM):
    case TYPE_PAIR(COBJ, FLNUM):
    case TYPE_PAIR(COBJ, RNG):
    case TYPE_PAIR(COBJ, COBJ):
      return do_binary_method(self, self, anum, bnum);
    case TYPE_PAIR(BGNUM, COBJ):
    case TYPE_PAIR(FLNUM, COBJ):
    case TYPE_PAIR(RNG, COBJ):
      return do_binary_method(self, r_floor_s, bnum, anum);
    }
  }
  invalid_ops(self, anum, bnum);
divzero:
  divzero(self);
}

val ceildiv(val anum, val bnum)
{
  val self = ceil_s;
  if (missingp(bnum))
    return ceili(anum);
  if (type(anum) == COBJ)
    return do_binary_method(self, self, anum, bnum);
  if (type(bnum) == COBJ)
    return do_binary_method(self, r_ceil_s, bnum, anum);
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
  case COBJ:
    return do_unary_method(self, round1_s, num);
  default:
    break;
  }
  invalid_op(self, num);
}


val roundiv(val anum, val bnum)
{
  val self = round_s;
  type_t ta, tb;

  if (missingp(bnum))
    return round1(self, anum);

  ta = type(anum);
  tb = type(bnum);

  if (ta == COBJ)
    return do_binary_method(self, self, anum, bnum);

  if (tb == COBJ)
    return do_binary_method(self, r_round_s, bnum, anum);

  if (minusp(bnum)) {
    anum = neg(anum);
    bnum = neg(bnum);
  }

  if (ta == RNG) {
    return rcons(roundiv(from(anum), bnum), roundiv(to(anum), bnum));
  } else if (ta == FLNUM || tb == FLNUM) {
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
  val self = div_s;

  if (missingp(bnum)) {
    if (cobjp(anum)) {
      return do_unary_method(self, recip_s, anum);
    } else {
      double b = c_flo(to_float(self, anum), self);
      if (b == 0.0)
        goto divzero;
      return flo(1.0 / b);
    }
  } else if (type(anum) == RNG) {
    return rcons(divi(from(anum), bnum), divi(to(anum), bnum));
  } else if (type(bnum) == COBJ) {
    return do_binary_method(self, inv_div_s, bnum, anum);
  } else if (type(anum) == COBJ) {
    return do_binary_method(self, self, anum, bnum);
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
  val self = zerop_s;

  if (num == zero)
    return t;

  switch (type(num)) {
  case NUM:
  case BGNUM:
    return nil;
  case FLNUM:
    return tnil(c_flo(num, self) == 0.0);
  case CHR:
    return tnil(num == chr(0));
  case RNG:
    return and2(zerop(from(num)), zerop(to(num)));
  case COBJ:
    return do_unary_method(self, self, num);
  default:
    not_number(self, num);
  }
}

val nzerop(val num)
{
  val self = lit("nzerop");

  if (num == zero)
    return nil;

  switch (type(num)) {
  case NUM:
  case BGNUM:
    return t;
  case FLNUM:
    return tnil(c_flo(num, self) != 0.0);
  case CHR:
    return tnil(num != chr(0));
  case RNG:
    return tnil(nzerop(from(num)) || nzerop(to(num)));
  case COBJ:
    return tnil(!do_unary_method(self, zerop_s, num));
  default:
    not_number(self, num);
  }
}

val plusp(val num)
{
  val self = plusp_s;

  switch (type(num)) {
  case NUM:
    return tnil(c_n(num) > 0);
  case BGNUM:
    return tnil(mp_cmp_z(mp(num)) == MP_GT);
  case FLNUM:
    return tnil(c_flo(num, self) > 0.0);
  case CHR:
    return tnil(num != chr(0));
  case COBJ:
    return do_unary_method(self, self, num);
  default:
    not_number(self, num);
  }
}

val minusp(val num)
{
  val self = minusp_s;

  switch (type(num)) {
  case NUM:
    return tnil(c_n(num) < 0);
  case BGNUM:
    return tnil(mp_cmp_z(mp(num)) == MP_LT);
  case FLNUM:
    return tnil(c_flo(num, self) < 0.0);
  case CHR:
    return nil;
  case COBJ:
    return do_unary_method(self, self, num);
  default:
    not_number(self, num);
  }
}

val evenp(val num)
{
  val self = evenp_s;

  switch (type(num)) {
  case NUM:
    return (c_n(num) % 2 == 0) ? t : nil;
  case BGNUM:
    return mp_iseven(mp(num)) ? t : nil;
  case COBJ:
    return do_unary_method(self, self, num);
  default:
    not_integer(self, num);
  }
}

val oddp(val num)
{
  val self = oddp_s;

  switch (type(num)) {
  case NUM:
    return (c_n(num) % 2 != 0) ? t : nil;
  case BGNUM:
    return mp_isodd(mp(num)) ? t : nil;
  case COBJ:
    return do_unary_method(self, self, num);
  default:
    not_integer(self, num);
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

static void seq_lt_compat_check(seq_iter_t *ita, seq_iter_t *itb,
                                val a, val b, val self)
{
  if (ita->inf.kind == SEQ_NOTSEQ || ita->inf.kind == SEQ_HASHLIKE ||
      itb->inf.kind == SEQ_NOTSEQ || itb->inf.kind == SEQ_HASHLIKE)
  {
    uw_throwf(error_s, lit("~a: invalid operands ~s ~s"), self, a, b, nao);
  }
}

static val seq_lt(val self, val aseq, val bseq)
{
  seq_iter_t ita, itb;
  seq_iter_init(self, &ita, aseq);
  seq_iter_init(self, &itb, bseq);

  seq_lt_compat_check(&ita, &itb, aseq, bseq, self);

  for (;;) {
    val aelem, belem;
    switch (seq_peek(&ita, &aelem) << 1 | seq_peek(&itb, &belem)) {
    case 0:
      return nil;
    case 1:
      return t;
    case 2:
      return nil;
    case 3:
      if (lt(aelem, belem))
        return t;
      if (!numeq(aelem, belem))
        return nil;
      seq_geti(&ita);
      seq_geti(&itb);
      break;
    default:
      internal_error("bad return value from iterator peek");
    }
  }
}

static val seq_le(val self, val aseq, val bseq)
{
  seq_iter_t ita, itb;
  seq_iter_init(self, &ita, aseq);
  seq_iter_init(self, &itb, bseq);

  seq_lt_compat_check(&ita, &itb, aseq, bseq, self);

  for (;;) {
    val aelem, belem;
    switch (seq_peek(&ita, &aelem) << 1 | seq_peek(&itb, &belem)) {
    case 0:
      return t;
    case 1:
      return t;
    case 2:
      return nil;
    case 3:
      if (!le(aelem, belem))
        return nil;
      seq_geti(&ita);
      seq_geti(&itb);
      break;
    default:
      internal_error("bad return value from iterator peek");
    }
  }
}

val gt(val anum, val bnum)
{
  val self = gt_s;
tail:
  switch (TYPE_PAIR(type(anum), type(bnum))) {
  case TYPE_PAIR(NUM, NUM):
    return c_n(anum) > c_n(bnum) ? t : nil;
  case TYPE_PAIR(CHR, CHR):
    return c_ch(anum) > c_ch(bnum) ? t : nil;
  case TYPE_PAIR(NUM, CHR):
    return c_n(anum) > c_ch(bnum) ? t : nil;
  case TYPE_PAIR(CHR, NUM):
    return c_ch(anum) > c_n(bnum) ? t : nil;
  case TYPE_PAIR(NUM, BGNUM):
  case TYPE_PAIR(CHR, BGNUM):
    return mp_cmp_z(mp(bnum)) == MP_LT ? t : nil;
  case TYPE_PAIR(BGNUM, NUM):
  case TYPE_PAIR(BGNUM, CHR):
    return mp_cmp_z(mp(anum)) == MP_GT ? t : nil;
  case TYPE_PAIR(BGNUM, BGNUM):
    return mp_cmp(mp(anum), mp(bnum)) == MP_GT ? t : nil;
  case TYPE_PAIR(NUM, FLNUM):
    return c_n(anum) > c_flo(bnum, self) ? t : nil;
  case TYPE_PAIR(CHR, FLNUM):
    return c_ch(anum) > c_flo(bnum, self) ? t : nil;
  case TYPE_PAIR(FLNUM, NUM):
    return c_flo(anum, self) > c_n(bnum) ? t : nil;
  case TYPE_PAIR(FLNUM, CHR):
    return c_flo(anum, self) > c_ch(bnum) ? t : nil;
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
  case TYPE_PAIR(COBJ, NUM):
  case TYPE_PAIR(COBJ, CHR):
  case TYPE_PAIR(COBJ, BGNUM):
  case TYPE_PAIR(COBJ, FLNUM):
  case TYPE_PAIR(COBJ, RNG):
  case TYPE_PAIR(COBJ, COBJ):
    return do_binary_method(self, self, anum, bnum);
  case TYPE_PAIR(NUM, COBJ):
  case TYPE_PAIR(CHR, COBJ):
  case TYPE_PAIR(BGNUM, COBJ):
  case TYPE_PAIR(FLNUM, COBJ):
  case TYPE_PAIR(RNG, COBJ):
    return do_binary_method(self, lt_s, bnum, anum);
  default:
    return seq_lt(self, bnum, anum);
  }
}

val lt(val anum, val bnum)
{
  val self = lt_s;
tail:
  switch (TYPE_PAIR(type(anum), type(bnum))) {
  case TYPE_PAIR(NUM, NUM):
    return c_n(anum) < c_n(bnum) ? t : nil;
  case TYPE_PAIR(CHR, CHR):
    return c_ch(anum) < c_ch(bnum) ? t : nil;
  case TYPE_PAIR(NUM, CHR):
    return c_n(anum) < c_ch(bnum) ? t : nil;
  case TYPE_PAIR(CHR, NUM):
    return c_ch(anum) < c_n(bnum) ? t : nil;
  case TYPE_PAIR(NUM, BGNUM):
  case TYPE_PAIR(CHR, BGNUM):
    return mp_cmp_z(mp(bnum)) == MP_GT ? t : nil;
  case TYPE_PAIR(BGNUM, NUM):
  case TYPE_PAIR(BGNUM, CHR):
    return mp_cmp_z(mp(anum)) == MP_LT ? t : nil;
  case TYPE_PAIR(BGNUM, BGNUM):
    return mp_cmp(mp(anum), mp(bnum)) == MP_LT ? t : nil;
  case TYPE_PAIR(NUM, FLNUM):
    return c_n(anum) < c_flo(bnum, self) ? t : nil;
  case TYPE_PAIR(CHR, FLNUM):
    return c_ch(anum) < c_flo(bnum, self) ? t : nil;
  case TYPE_PAIR(FLNUM, NUM):
    return c_flo(anum, self) < c_n(bnum) ? t : nil;
  case TYPE_PAIR(FLNUM, CHR):
    return c_flo(anum, self) < c_ch(bnum) ? t : nil;
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
  case TYPE_PAIR(COBJ, NUM):
  case TYPE_PAIR(COBJ, CHR):
  case TYPE_PAIR(COBJ, BGNUM):
  case TYPE_PAIR(COBJ, FLNUM):
  case TYPE_PAIR(COBJ, RNG):
  case TYPE_PAIR(COBJ, COBJ):
    return do_binary_method(self, self, anum, bnum);
  case TYPE_PAIR(NUM, COBJ):
  case TYPE_PAIR(CHR, COBJ):
  case TYPE_PAIR(BGNUM, COBJ):
  case TYPE_PAIR(FLNUM, COBJ):
  case TYPE_PAIR(RNG, COBJ):
    return do_binary_method(self, gt_s, bnum, anum);
  default:
    return seq_lt(self, anum, bnum);
  }
}

val ge(val anum, val bnum)
{
  val self = ge_s;
tail:
  switch (TYPE_PAIR(type(anum), type(bnum))) {
  case TYPE_PAIR(NUM, NUM):
    return c_n(anum) >= c_n(bnum) ? t : nil;
  case TYPE_PAIR(CHR, CHR):
    return c_ch(anum) >= c_ch(bnum) ? t : nil;
  case TYPE_PAIR(NUM, CHR):
    return c_n(anum) >= c_ch(bnum) ? t : nil;
  case TYPE_PAIR(CHR, NUM):
    return c_ch(anum) >= c_n(bnum) ? t : nil;
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
    return c_n(anum) >= c_flo(bnum, self) ? t : nil;
  case TYPE_PAIR(CHR, FLNUM):
    return c_ch(anum) >= c_flo(bnum, self) ? t : nil;
  case TYPE_PAIR(FLNUM, NUM):
    return c_flo(anum, self) >= c_n(bnum) ? t : nil;
  case TYPE_PAIR(FLNUM, CHR):
    return c_flo(anum, self) >= c_ch(bnum) ? t : nil;
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
  case TYPE_PAIR(COBJ, NUM):
  case TYPE_PAIR(COBJ, CHR):
  case TYPE_PAIR(COBJ, BGNUM):
  case TYPE_PAIR(COBJ, FLNUM):
  case TYPE_PAIR(COBJ, RNG):
  case TYPE_PAIR(COBJ, COBJ):
    return do_binary_method(self, self, anum, bnum);
  case TYPE_PAIR(NUM, COBJ):
  case TYPE_PAIR(CHR, COBJ):
  case TYPE_PAIR(BGNUM, COBJ):
  case TYPE_PAIR(FLNUM, COBJ):
  case TYPE_PAIR(RNG, COBJ):
    return do_binary_method(self, le_s, bnum, anum);
  default:
    return seq_le(self, bnum, anum);
  }
}

val le(val anum, val bnum)
{
  val self = le_s;
tail:
  switch (TYPE_PAIR(type(anum), type(bnum))) {
  case TYPE_PAIR(NUM, NUM):
    return c_n(anum) <= c_n(bnum) ? t : nil;
  case TYPE_PAIR(CHR, CHR):
    return c_ch(anum) <= c_ch(bnum) ? t : nil;
  case TYPE_PAIR(NUM, CHR):
    return c_n(anum) <= c_ch(bnum) ? t : nil;
  case TYPE_PAIR(CHR, NUM):
    return c_ch(anum) <= c_n(bnum) ? t : nil;
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
    return c_n(anum) <= c_flo(bnum, self) ? t : nil;
  case TYPE_PAIR(CHR, FLNUM):
    return c_ch(anum) <= c_flo(bnum, self) ? t : nil;
  case TYPE_PAIR(FLNUM, NUM):
    return c_flo(anum, self) <= c_n(bnum) ? t : nil;
  case TYPE_PAIR(FLNUM, CHR):
    return c_flo(anum, self) <= c_ch(bnum) ? t : nil;
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
  case TYPE_PAIR(COBJ, NUM):
  case TYPE_PAIR(COBJ, CHR):
  case TYPE_PAIR(COBJ, BGNUM):
  case TYPE_PAIR(COBJ, FLNUM):
  case TYPE_PAIR(COBJ, RNG):
  case TYPE_PAIR(COBJ, COBJ):
    return do_binary_method(self, self, anum, bnum);
  case TYPE_PAIR(NUM, COBJ):
  case TYPE_PAIR(CHR, COBJ):
  case TYPE_PAIR(BGNUM, COBJ):
  case TYPE_PAIR(FLNUM, COBJ):
  case TYPE_PAIR(RNG, COBJ):
    return do_binary_method(self, ge_s, bnum, anum);
  default:
    return seq_le(self, anum, bnum);
  }
}

static val seq_numeq(val self, val aseq, val bseq)
{
  seq_iter_t ita, itb;
  seq_iter_init(self, &ita, aseq);
  seq_iter_init(self, &itb, bseq);

  if (ita.inf.kind == SEQ_VECLIKE && itb.inf.kind == SEQ_VECLIKE) {
    if (length(aseq) != length(bseq))
      return nil;
  }

  for (;;) {
    val aelem, belem;
    switch (seq_peek(&ita, &aelem) + seq_peek(&itb, &belem)) {
    case 0:
      return t;
    case 1:
      return nil;
    case 2:
      if (!numeq(aelem, belem))
        return nil;
      seq_geti(&ita);
      seq_geti(&itb);
      break;
    default:
      internal_error("bad return value from iterator peek");
    }
  }
}

val numeq(val anum, val bnum)
{
  val self = numeq_s;
tail:
  switch (TYPE_PAIR(type(anum), type(bnum))) {
  case TYPE_PAIR(NUM, NUM):
    return c_n(anum) == c_n(bnum) ? t : nil;
  case TYPE_PAIR(CHR, CHR):
    return c_ch(anum) == c_ch(bnum) ? t : nil;
  case TYPE_PAIR(NUM, CHR):
    return c_n(anum) == c_ch(bnum) ? t : nil;
  case TYPE_PAIR(CHR, NUM):
    return c_ch(anum) == c_n(bnum) ? t : nil;
  case TYPE_PAIR(NUM, BGNUM):
  case TYPE_PAIR(CHR, BGNUM):
    return mp_cmp_z(mp(bnum)) == MP_EQ ? t : nil;
  case TYPE_PAIR(BGNUM, NUM):
  case TYPE_PAIR(BGNUM, CHR):
    return mp_cmp_z(mp(anum)) == MP_EQ ? t : nil;
  case TYPE_PAIR(BGNUM, BGNUM):
    return mp_cmp(mp(anum), mp(bnum)) == MP_EQ ? t : nil;
  case TYPE_PAIR(NUM, FLNUM):
    return c_n(anum) == c_flo(bnum, self) ? t : nil;
  case TYPE_PAIR(CHR, FLNUM):
    return c_ch(anum) == c_flo(bnum, self) ? t : nil;
  case TYPE_PAIR(FLNUM, NUM):
    return c_flo(anum, self) == c_n(bnum) ? t : nil;
  case TYPE_PAIR(FLNUM, CHR):
    return c_flo(anum, self) == c_ch(bnum) ? t : nil;
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
  case TYPE_PAIR(COBJ, NUM):
  case TYPE_PAIR(COBJ, CHR):
  case TYPE_PAIR(COBJ, BGNUM):
  case TYPE_PAIR(COBJ, FLNUM):
  case TYPE_PAIR(COBJ, RNG):
  case TYPE_PAIR(COBJ, COBJ):
    return do_binary_method(self, self, anum, bnum);
  case TYPE_PAIR(NUM, COBJ):
  case TYPE_PAIR(CHR, COBJ):
  case TYPE_PAIR(BGNUM, COBJ):
  case TYPE_PAIR(FLNUM, COBJ):
  case TYPE_PAIR(RNG, COBJ):
    return do_binary_method(self, self, bnum, anum);
  default:
    return seq_numeq(self, anum, bnum);
  }
}

val expt(val anum, val bnum)
{
  val self = expt_s;

tail:
  switch (TYPE_PAIR(type(anum), type(bnum))) {
  case TYPE_PAIR(NUM, NUM):
    {
      cnum a = c_n(anum);
      cnum b = c_n(bnum);
      mp_int tmpa;
      val n;
      mp_err mpe = MP_OKAY;
      if (b < 0) {
        if (anum == zero)
          goto divzero;
        return flo(pow(a, b));
      }
      if (b == 0)
        return one;
      if (b == 1)
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
      if (mp_cmp_z(mp(bnum)) == MP_LT) {
        if (anum == zero)
          goto divzero;
        bnum = flo_int(bnum);
        goto tail;
      }
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
      if (b < 0) {
        if (mp_cmp_z(mp(anum)) == MP_LT)
          goto divzero;
        anum = flo_int(anum);
        goto tail;
      }
      if (b == 0)
        return one;
      if (b == 1)
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
      if (mp_cmp_z(mp(bnum)) == MP_LT) {
        if (mp_cmp_z(mp(anum)) == MP_LT)
          goto divzero;
        anum = flo_int(anum);
        bnum = flo_int(bnum);
        goto tail;
      }
      n = make_bignum();
      mpe = mp_expt(mp(anum), mp(bnum), mp(n));
      if (mpe != MP_OKAY)
        do_mp_error(self, mpe);
      normalize(n);
      return n;
    }
  case TYPE_PAIR(NUM, FLNUM):
    {
      cnum a = c_n(anum);
      double b = c_flo(bnum, self);

      if (b == 0.0)
        return flo(1.0);
      if (a == 0 && b < 0)
        goto divzero;
      return flo(pow(a, b));
    }
  case TYPE_PAIR(FLNUM, NUM):
    {
      double a = c_flo(anum, self);
      cnum b = c_n(bnum);

      if (b == 0)
        return flo(1.0);
      if (a == 0 && b < 0)
        goto divzero;
      return flo(pow(a, b));
    }
    return flo(pow(c_flo(anum, self), c_n(bnum)));
  case TYPE_PAIR(FLNUM, FLNUM):
    {
      double a = c_flo(anum, self);
      double b = c_flo(bnum, self);
      if (b == 0.0)
        return flo(1.0);
      if (a == 0 && b < 0)
        goto divzero;
      return flo(pow(a, b));
    }
  case TYPE_PAIR(BGNUM, FLNUM):
    anum = flo_int(anum);
    goto tail;
  case TYPE_PAIR(FLNUM, BGNUM):
    bnum = flo_int(bnum);
    goto tail;
  case TYPE_PAIR(COBJ, NUM):
  case TYPE_PAIR(COBJ, CHR):
  case TYPE_PAIR(COBJ, BGNUM):
  case TYPE_PAIR(COBJ, FLNUM):
  case TYPE_PAIR(COBJ, RNG):
  case TYPE_PAIR(COBJ, COBJ):
    return do_binary_method(self, self, anum, bnum);
  case TYPE_PAIR(NUM, COBJ):
  case TYPE_PAIR(CHR, COBJ):
  case TYPE_PAIR(BGNUM, COBJ):
  case TYPE_PAIR(FLNUM, COBJ):
  case TYPE_PAIR(RNG, COBJ):
    return do_binary_method(self, r_expt_s, bnum, anum);
  }

  invalid_ops(self, anum, bnum);
divzero:
  divzero(self);
}

val exptmod(val base, val exp, val mod)
{
  val self = exptmod_s;
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
  if (cobjp(base))
    return do_ternary_method(self, self, base, exp, mod);

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
  val self = isqrt_s;

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
  case COBJ:
    return do_unary_method(self, self, anum);
  default:
    break;
  }

  uw_throwf(error_s, lit("~s: non-integer operand ~s"), self, anum, nao);
negop:
  uw_throwf(error_s, lit("~s: negative operand"), self, nao);
}

val square(val anum)
{
  val self = square_s;

  switch (type(anum)) {
  case NUM:
    {
      cnum a = c_n(anum);
#if HAVE_DOUBLE_INTPTR_T
      double_intptr_t product = a * convert(double_intptr_t, a);
      if (product < NUM_MIN || product > NUM_MAX)
        return bignum_dbl_ipt(product);
      return num_fast(product);
#else
      cnum ap = ABS(a);
      if (2 * highest_bit(ap) < PTR_BIT - 1) {
        cnum product = a * a;
        if (product >= NUM_MIN && product <= NUM_MAX)
          return num_fast(product);
        return bignum(product);
      } else {
        val n = make_bignum();
        mp_err mpe;
        mp_set_intptr(mp(n), a);
        mpe = mp_sqr(mp(n), mp(n));
        if (mpe != MP_OKAY)
          do_mp_error(self, mpe);
        return n;
      }
#endif
    }
  case BGNUM:
    {
      val n = make_bignum();
      mp_err mpe = mp_sqr(mp(anum), mp(n));
      if (mpe != MP_OKAY)
        do_mp_error(self, mpe);
      return n;
    }
  case FLNUM:
    {
      double a = c_flo(anum, self);
      return flo(a * a);
    }
  case RNG:
    return rcons(square(from(anum)), square(to(anum)));
  case COBJ:
    return do_unary_method(self, self, anum);
  default:
    break;
  }

  uw_throwf(error_s, lit("~a: invalid operand ~s"), self, anum, nao);
}

val gcd(val anum, val bnum)
{
  val self = lit("gcd");
  ucnum ua, ub;

  switch (TYPE_PAIR(type(anum), type(bnum))) {
  case TYPE_PAIR(BGNUM, BGNUM):
    if (mp_in_uintptr_range(mp(anum)) && mp_in_uintptr_range(mp(bnum))) {
      ua = c_unum(anum, self);
      ub = c_unum(bnum, self);
      goto both_ucnum;
    } else {
      val n = make_bignum();
      if (mp_gcd(mp(anum), mp(bnum), mp(n)) != MP_OKAY)
        break;
      return normalize(n);
    }
  case TYPE_PAIR(NUM, NUM):
    if (anum == zero)
      return bnum;
    if (bnum == zero)
      return anum;
    ua = c_u(anum);
    ub = c_u(bnum);
  both_ucnum:
    {
      int k = 0;
      while ((ua & 1) == 0 && (ub & 1) == 0) {
        ua >>= 1;
        ub >>= 1;
        k++;
      }
      while ((ub & 1) == 0)
        ub >>= 1;
      while ((ua & 1) == 0)
        ua >>= 1;
      for (;;) {
        if (ua > ub) {
          ucnum tmp = ua;
          ua = ub;
          ub = tmp;
        }
        ub -= ua;
        if (ub == 0)
          break;
        while ((ub & 1) == 0)
          ub >>= 1;
      }
      return unum(ua << k);
    }
  case TYPE_PAIR(NUM, BGNUM):
    {
      val tmp = anum;
      anum = bnum;
      bnum = tmp;
    }
    /* fallthrough */
  case TYPE_PAIR(BGNUM, NUM):
    if (mp_in_uintptr_range(mp(anum))) {
      ua = c_unum(anum, self);
      ub = c_u(bnum);
      goto both_ucnum;
    } else {
      mp_int bn;
      val n = make_bignum();

      mp_init(&bn);
      mp_set_intptr(&bn, c_u(bnum));
      if (mp_gcd(mp(anum), &bn, mp(n)) != MP_OKAY) {
        mp_clear(&bn);
        break;
      }
      mp_clear(&bn);
      return normalize(n);
    }
  default:
    uw_throwf(error_s, lit("gcd: non-integral operands ~s ~s"),
              anum, bnum, nao);
  }

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
  case COBJ:
    return do_unary_method(self, floor1_s, num);
  default:
    break;
  }

  invalid_op(self, num);
}

val ceili(val num)
{
  val self = ceil_s;

  switch (type(num)) {
  case NUM:
  case BGNUM:
    return num;
  case FLNUM:
    return flo(ceil(c_flo(num, self)));
  case RNG:
    return rcons(ceili(from(num)), ceili(to(num)));
  case COBJ:
    return do_unary_method(self, ceil1_s, num);
  default:
    break;
  }

  invalid_op(self, num);
}

val sine(val num)
{
  val self = sin_s;
  if (cobjp(num))
    return do_unary_method(self, self, num);
  return flo(sin(c_flo(to_float(self, num), self)));
}

val cosi(val num)
{
  val self = cos_s;
  if (cobjp(num))
    return do_unary_method(self, self, num);
  return flo(cos(c_flo(to_float(self, num), self)));
}

val tang(val num)
{
  val self = tan_s;
  if (cobjp(num))
    return do_unary_method(self, self, num);
  return flo(tan(c_flo(to_float(self, num), self)));
}

val asine(val num)
{
  val self = asin_s;
  if (cobjp(num))
    return do_unary_method(self, self, num);
  return flo(asin(c_flo(to_float(self, num), self)));
}

val acosi(val num)
{
  val self = acos_s;
  if (cobjp(num))
    return do_unary_method(self, self, num);
  return flo(acos(c_flo(to_float(self, num), self)));
}

val atang(val num)
{
  val self = atan_s;
  if (cobjp(num))
    return do_unary_method(self, self, num);
  return flo(atan(c_flo(to_float(self, num), self)));
}

val atang2(val y, val x)
{
  val self = atan2_s;
  if (cobjp(y))
    return do_binary_method(self, self, y, x);
  if (cobjp(x))
    return do_binary_method(self, r_atan2_s, x, y);
  return flo(atan2(c_flo(to_float(self, y), self),
                   c_flo(to_float(self, x), self)));
}

#if !HAVE_HYPERBOLICS

double sinh(double x)
{
  return (exp(x) - exp(-x)) / 2;
}

double cosh(double x)
{
  if (x == 0)
    return 1;
  return (exp(x) + exp(-x)) / 2;
}

double tanh(double x)
{
  double e2x = exp(2*x);
  return (e2x - 1) / (e2x + 1);
}

double asinh(double x)
{
  return log(x + sqrt(1 + x*x));
}

double acosh(double x)
{
  return log(x + sqrt(x + 1)*sqrt(x - 1));
}

double atanh(double x)
{
  return (log(1 + x) - log(1 - x))/2;
}

#endif

val sineh(val num)
{
  val self = sinh_s;
  if (cobjp(num))
    return do_unary_method(self, self, num);
  return flo(sinh(c_flo(to_float(self, num), self)));
}

val cosih(val num)
{
  val self = cosh_s;
  if (cobjp(num))
    return do_unary_method(self, self, num);
  return flo(cosh(c_flo(to_float(self, num), self)));
}

val tangh(val num)
{
  val self = tanh_s;
  if (cobjp(num))
    return do_unary_method(self, self, num);
  return flo(tanh(c_flo(to_float(self, num), self)));
}

val asineh(val num)
{
  val self = asinh_s;
  if (cobjp(num))
    return do_unary_method(self, self, num);
  return flo(asinh(c_flo(to_float(self, num), self)));
}

val acosih(val num)
{
  val self = acosh_s;
  if (cobjp(num))
    return do_unary_method(self, self, num);
  return flo(acosh(c_flo(to_float(self, num), self)));
}

val atangh(val num)
{
  val self = atanh_s;
  if (cobjp(num))
    return do_unary_method(self, self, num);
  return flo(atanh(c_flo(to_float(self, num), self)));
}

val loga(val num)
{
  val self = log_s;
  if (cobjp(num))
    return do_unary_method(self, self, num);
  return flo(log(c_flo(to_float(self, num), self)));
}

val logten(val num)
{
  val self = log10_s;
  if (cobjp(num))
    return do_unary_method(self, self, num);
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
  val self = log2_s;
  if (cobjp(num))
    return do_unary_method(self, self, num);
  return flo(log2(c_flo(to_float(self, num), self)));
}

val expo(val num)
{
  val self = exp_s;
  if (cobjp(num))
    return do_unary_method(self, self, num);
  return flo(exp(c_flo(to_float(self, num), self)));
}

val sqroot(val num)
{
  val self = sqrt_s;
  if (cobjp(num))
    return do_unary_method(self, self, num);
  return flo(sqrt(c_flo(to_float(self, num), self)));
}

static val cbrt_wrap(val num)
{
  val self = cbrt_s;
  if (cobjp(num))
    return do_unary_method(self, self, num);
#if HAVE_CBRT
  return flo(cbrt(c_flo(to_float(self, num), self)));
#else
  not_available(self);
#endif
}

static val erf_wrap(val num)
{
  val self = erf_s;
  if (cobjp(num))
    return do_unary_method(self, self, num);
#if HAVE_ERF
  return flo(erf(c_flo(to_float(self, num), self)));
#else
  not_available(self);
#endif
}

static val erfc_wrap(val num)
{
  val self = erfc_s;
  if (cobjp(num))
    return do_unary_method(self, self, num);
#if HAVE_ERFC
  return flo(erfc(c_flo(to_float(self, num), self)));
#else
  not_available(self);
#endif
}

static val exp10_wrap(val num)
{
  val self = exp10_s;
  if (cobjp(num))
    return do_unary_method(self, self, num);
#if HAVE_EXP10
  return flo(exp10(c_flo(to_float(self, num), self)));
#else
  not_available(self);
#endif
}

static val exp2_wrap(val num)
{
  val self = exp2_s;
  if (cobjp(num))
    return do_unary_method(self, self, num);
#if HAVE_EXP2
  return flo(exp2(c_flo(to_float(self, num), self)));
#else
  not_available(self);
#endif
}

static val expm1_wrap(val num)
{
  val self = expm1_s;
  if (cobjp(num))
    return do_unary_method(self, self, num);
#if HAVE_EXPM1
  return flo(expm1(c_flo(to_float(self, num), self)));
#else
  not_available(self);
#endif
}

static val gamma_wrap(val num)
{
  val self = gamma_s;
  if (cobjp(num))
    return do_unary_method(self, self, num);
#if HAVE_GAMMA
  return flo(gamma(c_flo(to_float(self, num), self)));
#else
  not_available(self);
#endif
}

static val j0_wrap(val num)
{
  val self = j0_s;
  if (cobjp(num))
    return do_unary_method(self, self, num);
#if HAVE_J0
  return flo(j0(c_flo(to_float(self, num), self)));
#else
  not_available(self);
#endif
}

static val j1_wrap(val num)
{
  val self = j1_s;
  if (cobjp(num))
    return do_unary_method(self, self, num);
#if HAVE_J1
  return flo(j1(c_flo(to_float(self, num), self)));
#else
  not_available(self);
#endif
}

static val lgamma_wrap(val num)
{
  val self = lgamma_s;
  if (cobjp(num))
    return do_unary_method(self, self, num);
#if HAVE_LGAMMA
  return flo(lgamma(c_flo(to_float(self, num), self)));
#else
  not_available(self);
#endif
}

static val log1p_wrap(val num)
{
  val self = log1p_s;
  if (cobjp(num))
    return do_unary_method(self, self, num);
#if HAVE_LOG1P
  return flo(log1p(c_flo(to_float(self, num), self)));
#else
  not_available(self);
#endif
}

static val logb_wrap(val num)
{
  val self = logb_s;
  if (cobjp(num))
    return do_unary_method(self, self, num);
#if HAVE_LOGB
  return flo(logb(c_flo(to_float(self, num), self)));
#else
  not_available(self);
#endif
}

static val nearbyint_wrap(val num)
{
  val self = nearbyint_s;
  if (cobjp(num))
    return do_unary_method(self, self, num);
#if HAVE_NEARBYINT
  return flo(nearbyint(c_flo(to_float(self, num), self)));
#else
  not_available(self);
#endif
}

static val rint_wrap(val num)
{
  val self = rint_s;
  if (cobjp(num))
    return do_unary_method(self, self, num);
#if HAVE_RINT
  return flo(rint(c_flo(to_float(self, num), self)));
#else
  not_available(self);
#endif
}

static val significand_wrap(val num)
{
  val self = significand_s;
  if (cobjp(num))
    return do_unary_method(self, self, num);
#if HAVE_SIGNIFICAND
  return flo(significand(c_flo(to_float(self, num), self)));
#else
  not_available(self);
#endif
}

static val tgamma_wrap(val num)
{
  val self = tgamma_s;
  if (cobjp(num))
    return do_unary_method(self, self, num);
#if HAVE_TGAMMA
  return flo(tgamma(c_flo(to_float(self, num), self)));
#else
  not_available(self);
#endif
}

static val y0_wrap(val num)
{
  val self = y0_s;
  if (cobjp(num))
    return do_unary_method(self, self, num);
#if HAVE_Y0
  return flo(y0(c_flo(to_float(self, num), self)));
#else
  not_available(self);
#endif
}

static val y1_wrap(val num)
{
  val self = y1_s;
  if (cobjp(num))
    return do_unary_method(self, self, num);
#if HAVE_Y1
  return flo(y1(c_flo(to_float(self, num), self)));
#else
  not_available(self);
#endif
}

static val copysign_wrap(val anum, val bnum)
{
  val self = copysign_s;
  if (cobjp(anum))
    return do_binary_method(self, self, anum, bnum);
  if (cobjp(bnum))
    return do_binary_method(self, r_copysign_s, bnum, anum);
#if HAVE_COPYSIGN
  return flo(copysign(c_flo(to_float(self, anum), self),
                      c_flo(to_float(self, bnum), self)));
#else
  not_available(self);
#endif
}

static val drem_wrap(val anum, val bnum)
{
  val self = drem_s;
  if (cobjp(anum))
    return do_binary_method(self, self, anum, bnum);
  if (cobjp(bnum))
    return do_binary_method(self, r_drem_s, bnum, anum);
#if HAVE_DREM
  return flo(drem(c_flo(to_float(self, anum), self),
                  c_flo(to_float(self, bnum), self)));
#else
  not_available(self);
#endif
}

static val fdim_wrap(val anum, val bnum)
{
  val self = fdim_s;
  if (cobjp(anum))
    return do_binary_method(self, self, anum, bnum);
  if (cobjp(bnum))
    return do_binary_method(self, r_fdim_s, bnum, anum);
#if HAVE_FDIM
  return flo(fdim(c_flo(to_float(self, anum), self),
                  c_flo(to_float(self, bnum), self)));
#else
  not_available(self);
#endif
}

static val fmax_wrap(val anum, val bnum)
{
  val self = fmax_s;
  if (cobjp(anum))
    return do_binary_method(self, self, anum, bnum);
  if (cobjp(bnum))
    return do_binary_method(self, r_fmax_s, bnum, anum);
#if HAVE_FMAX
  return flo(fmax(c_flo(to_float(self, anum), self),
                  c_flo(to_float(self, bnum), self)));
#else
  not_available(self);
#endif
}

static val fmin_wrap(val anum, val bnum)
{
  val self = fmin_s;
  if (cobjp(anum))
    return do_binary_method(self, self, anum, bnum);
  if (cobjp(bnum))
    return do_binary_method(self, r_fmin_s, bnum, anum);
#if HAVE_FMIN
  return flo(fmin(c_flo(to_float(self, anum), self),
                  c_flo(to_float(self, bnum), self)));
#else
  not_available(self);
#endif
}

static val hypot_wrap(val anum, val bnum)
{
  val self = hypot_s;
  if (cobjp(anum))
    return do_binary_method(self, self, anum, bnum);
  if (cobjp(bnum))
    return do_binary_method(self, r_hypot_s, bnum, anum);
#if HAVE_HYPOT
  return flo(hypot(c_flo(to_float(self, anum), self),
                   c_flo(to_float(self, bnum), self)));
#else
  not_available(self);
#endif
}

static val jn_wrap(val anum, val bnum)
{
  val self = jn_s;
  if (cobjp(anum))
    return do_binary_method(self, self, anum, bnum);
  if (cobjp(bnum))
    return do_binary_method(self, r_jn_s, bnum, anum);
#if HAVE_JN
  return flo(jn(c_flo(to_float(self, anum), self),
                c_flo(to_float(self, bnum), self)));
#else
  not_available(self);
#endif
}

static val ldexp_wrap(val anum, val bnum)
{
  val self = ldexp_s;
  if (cobjp(anum))
    return do_binary_method(self, self, anum, bnum);
  if (cobjp(bnum))
    return do_binary_method(self, r_ldexp_s, bnum, anum);
#if HAVE_LDEXP
  return flo(ldexp(c_flo(to_float(self, anum), self),
                   c_flo(to_float(self, bnum), self)));
#else
  not_available(self);
#endif
}

static val nextafter_wrap(val anum, val bnum)
{
  val self = nextafter_s;
  if (cobjp(anum))
    return do_binary_method(self, self, anum, bnum);
  if (cobjp(bnum))
    return do_binary_method(self, r_nextafter_s, bnum, anum);
#if HAVE_NEXTAFTER
  return flo(nextafter(c_flo(to_float(self, anum), self),
                       c_flo(to_float(self, bnum), self)));
#else
  not_available(self);
#endif
}

static val remainder_wrap(val anum, val bnum)
{
  val self = remainder_s;
  if (cobjp(anum))
    return do_binary_method(self, self, anum, bnum);
  if (cobjp(bnum))
    return do_binary_method(self, r_remainder_s, bnum, anum);
#if HAVE_REMAINDER
  return flo(remainder(c_flo(to_float(self, anum), self),
                       c_flo(to_float(self, bnum), self)));
#else
  not_available(self);
#endif
}

static val scalb_wrap(val anum, val bnum)
{
  val self = scalb_s;
  if (cobjp(anum))
    return do_binary_method(self, self, anum, bnum);
  if (cobjp(bnum))
    return do_binary_method(self, r_scalb_s, bnum, anum);
#if HAVE_SCALB
  return flo(scalb(c_flo(to_float(self, anum), self),
                   c_flo(to_float(self, bnum), self)));
#else
  not_available(self);
#endif
}

static val scalbln_wrap(val anum, val bnum)
{
  val self = scalbln_s;
  if (cobjp(anum))
    return do_binary_method(self, self, anum, bnum);
  if (cobjp(bnum))
    return do_binary_method(self, r_scalbln_s, bnum, anum);
#if HAVE_SCALBLN
  return flo(scalbln(c_flo(to_float(self, anum), self),
                     c_flo(to_float(self, bnum), self)));
#else
  not_available(self);
#endif
}

static val yn_wrap(val anum, val bnum)
{
  val self = yn_s;
  if (cobjp(anum))
    return do_binary_method(self, self, anum, bnum);
  if (cobjp(bnum))
    return do_binary_method(self, r_yn_s, bnum, anum);
#if HAVE_YN
  return flo(yn(c_flo(to_float(self, anum), self),
                c_flo(to_float(self, bnum), self)));
#else
  not_available(self);
#endif
}


/*
 * TODO: replace this text-based hack!
 */
val int_flo(val f)
{
  val self = lit("int-flo");
  double d = c_flo(f, self);
#if SIZEOF_PTR >= 8
  cnum margin = 512;
  ucnum umargin = 1024;
#else
  cnum margin = 0;
  ucnum umargin = 0;
#endif

  if (d >= INT_PTR_MIN && d <= INT_PTR_MAX - margin) {
    cnum n = d;
    if (n < NUM_MIN || n > NUM_MAX)
      return bignum(n);
    return num_fast(n);
  } else if (d >= 0 && d <= UINT_PTR_MAX - umargin) {
    ucnum n = d;
    return unum(n);
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

#if CONFIG_LOCALE_TOLERANCE
    have_point = (strchr(text, dec_point) != 0);

    if (have_exp && have_point)
      sscanf(text, "%127[-0-9]%*1[^0-9e]%127[0-9]e%d", mint, mfrac, &exp);
    else if (have_exp)
      sscanf(text, "%127[-0-9]e%d", mint, &exp);
    else if (have_point)
      sscanf(text, "%127[-0-9]%*1[^0-9]*[%127[0-9]", mint, mfrac);
    else
      return int_str(string_utf8(text), nil);
#else
    have_point = (strchr(text, '.') != 0);

    if (have_exp && have_point)
      sscanf(text, "%127[-0-9].%127[0-9]e%d", mint, mfrac, &exp);
    else if (have_exp)
      sscanf(text, "%127[-0-9]e%d", mint, &exp);
    else if (have_point)
      sscanf(text, "%127[-0-9].%127[0-9]", mint, mfrac);
    else
      return int_str(string_utf8(text), nil);
#endif

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
  val self = logand_s;
  val c;

  switch (TYPE_PAIR(type(a), type(b))) {
  case TYPE_PAIR(NUM, CHR):
    {
      cnum ac = c_n(a);
      cnum bc = c_ch(b);
      return chr(ac & bc);
    }
  case TYPE_PAIR(CHR, NUM):
    {
      cnum ac = c_ch(a);
      cnum bc = c_n(b);
      return chr(ac & bc);
    }
  case TYPE_PAIR(NUM, NUM):
    {
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
  case TYPE_PAIR(COBJ, NUM):
  case TYPE_PAIR(COBJ, BGNUM):
  case TYPE_PAIR(COBJ, COBJ):
    return do_binary_method(self, self, a, b);
  case TYPE_PAIR(NUM, COBJ):
  case TYPE_PAIR(BGNUM, COBJ):
    return do_binary_method(self, self, b, a);
  default:
    uw_throwf(error_s, lit("~a: non-integral operands ~s ~s"), self, a, b, nao);
  }

bad:
  uw_throwf(error_s, lit("~a: operation failed on ~s ~s"), self, a, b, nao);
}

val logior(val a, val b)
{
  val self = logior_s;
  val c;

  switch (TYPE_PAIR(type(a), type(b))) {
  case TYPE_PAIR(NUM, CHR):
    {
      cnum ac = c_n(a);
      cnum bc = c_ch(b);
      return chr(ac | bc);
    }
  case TYPE_PAIR(CHR, NUM):
    {
      cnum ac = c_ch(a);
      cnum bc = c_n(b);
      return chr(ac | bc);
    }
  case TYPE_PAIR(NUM, NUM):
    {
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
  case TYPE_PAIR(COBJ, NUM):
  case TYPE_PAIR(COBJ, BGNUM):
  case TYPE_PAIR(COBJ, COBJ):
    return do_binary_method(self, self, a, b);
  case TYPE_PAIR(NUM, COBJ):
  case TYPE_PAIR(BGNUM, COBJ):
    return do_binary_method(self, self, b, a);
  default:
    uw_throwf(error_s, lit("~a: non-integral operands ~s ~s"), self, a, b, nao);
  }

bad:
  uw_throwf(error_s, lit("~a: operation failed on ~s ~s"), self, a, b, nao);
}

val logxor(val a, val b)
{
  val self = logxor_s;
  val c;

  switch (TYPE_PAIR(type(a), type(b))) {
  case TYPE_PAIR(NUM, CHR):
    {
      cnum ac = c_n(a);
      cnum bc = c_ch(b);
      return chr(ac ^ bc);
    }
  case TYPE_PAIR(CHR, NUM):
    {
      cnum ac = c_ch(a);
      cnum bc = c_n(b);
      return chr(ac ^ bc);
    }
  case TYPE_PAIR(NUM, NUM):
    {
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
      return zero;
    c = make_ubignum();
    if (mp_xor(mp(a), mp(b), mp(c)) != MP_OKAY)
      goto bad;
    return normalize(c);
  case TYPE_PAIR(COBJ, NUM):
  case TYPE_PAIR(COBJ, BGNUM):
  case TYPE_PAIR(COBJ, COBJ):
    return do_binary_method(self, self, a, b);
  case TYPE_PAIR(NUM, COBJ):
  case TYPE_PAIR(BGNUM, COBJ):
    return do_binary_method(self, self, b, a);
  default:
    uw_throwf(error_s, lit("~a: non-integral operands ~s ~s"), self, a, b, nao);
  }

bad:
  uw_throwf(error_s, lit("~a: operation failed on ~s ~s"), self, a, b, nao);
}

val logxor_old(val a, val b)
{
  val c;

  if (zerop(a) && zerop(b))
    return zero;

  switch (TYPE_PAIR(type(a), type(b))) {
  case TYPE_PAIR(NUM, CHR):
    if (a == b) {
      return a;
    } else {
      cnum ac = c_n(a);
      cnum bc = c_ch(b);
      return chr(ac ^ bc);
    }
  case TYPE_PAIR(CHR, NUM):
    if (a == b) {
      return a;
    } else {
      cnum ac = c_ch(a);
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
  val self = lognot1_s;
  cnum an, bn;
  val b;
  const cnum num_mask = (NUM_MAX << 1) | 1;

  if (!fixnump(bits))
    goto bad2;

  bn = c_n(bits);

  if (bn < 0)
    goto bad4;

  switch (type(a)) {
  case NUM:
    an = c_n(a);
    if (bn < NUM_BIT) {
      cnum mask = num_mask >> (NUM_BIT - bn);
      return num_fast((an & mask) ^ mask);
    }
    a = bignum(an);
    /* fallthrough */
  case BGNUM:
    b = make_ubignum();
    if (mp_trunc_comp(mp(a), mp(b), bn) != MP_OKAY)
      goto bad;
    return normalize(b);
  case COBJ:
    return do_binary_method(self, lognot_s, a, bits);
  default:
    goto bad3;
  }

bad:
  uw_throwf(error_s, lit("~a: operation failed on ~s"), self, a, nao);

bad2:
  if (cobjp(a))
    return do_binary_method(self, lognot_s, a, bits);
  if (cobjp(bits))
    return do_binary_method(self, r_lognot_s, bits, a);
  uw_throwf(error_s, lit("~a: bits value ~s is not a fixnum"), bits, nao);

bad3:
  uw_throwf(error_s, lit("~a: non-integral operand ~s"), self, a, nao);

bad4:
  uw_throwf(error_s, lit("~a: negative bits value ~s"), self, bits, nao);
}

val lognot(val a, val bits)
{
  val self = lognot1_s;
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
  case COBJ:
    return do_unary_method(self, self, a);
  default:
    uw_throwf(error_s, lit("~a: non-integral operand ~s"), self, a, nao);
  }

bad:
  uw_throwf(error_s, lit("~a: operation failed on ~s"), self, a, nao);
}

val logtrunc(val a, val bits)
{
  val self = logtrunc_s;
  cnum an, bn;
  val b;
  const cnum num_mask = (NUM_MAX << 1) | 1;

  if (!fixnump(bits))
    goto bad2;

  bn = c_n(bits);

  if (bn < 0)
    goto bad4;

  switch (type(a)) {
    mp_err mpe;
  case NUM:
    an = c_n(a);
    if (bn <= NUM_BIT) {
      cnum mask = num_mask >> (NUM_BIT - bn);
      return num_fast(an & mask);
    }
    a = bignum(an);
    /* fallthrough */
  case BGNUM:
    b = make_ubignum();
    if ((mpe = mp_trunc(mp(a), mp(b), bn)) != MP_OKAY)
      do_mp_error(self, mpe);
    return normalize(b);
  case COBJ:
    return do_binary_method(self, self, a, bits);
  default:
    goto bad3;
  }

bad2:
  if (cobjp(a))
    return do_binary_method(self, self, a, bits);
  if (cobjp(bits))
    return do_binary_method(self, r_logtrunc_s, bits, a);
  uw_throwf(error_s, lit("~a: bits value ~s is not a fixnum"), self, bits, nao);

bad3:
  uw_throwf(error_s, lit("~a: non-integral operand ~s"), self, a, nao);

bad4:;
  uw_throwf(error_s, lit("~a: negative bits value ~s"), self, bits, nao);
}

val sign_extend(val n, val nbits)
{
  val self = sign_extend_s;

  if (cobjp(n)) {
      return do_binary_method(self, self, n, nbits);
  } else {
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
            do_mp_error(self, mpe);
          mp_neg(mp(out), mp(out));
          return normalize(out);
        }
      case COBJ:
        ntrunc = do_binary_method(self, self, ntrunc, nbits);
        break;
      default:
        internal_error("impossible case");
      }
    }
    return ntrunc;
  }
}

val ash(val a, val bits)
{
  val self = ash_s;
  type_t ta = type(a);
  cnum bn;
  mp_err mpe = MP_OKAY;

  if (ta == COBJ)
    return do_binary_method(self, self, a, bits);

  if (!fixnump(bits))
    goto bad2;

  bn = c_n(bits);

  if (bn == 0) {
    switch (ta) {
    case NUM:
    case BGNUM:
      return a;
    default:
      goto bad3;
    }
  } else if (bn > 0) {
    switch (ta) {
    case NUM:
      {
        cnum an = c_n(a);
        int hb = highest_significant_bit(an);
        if (bn + hb < NUM_BIT) {
#if HAVE_UBSAN
          return num_fast(an * (convert(cnum, 1) << bn));
#else
          return num_fast(an << bn);
#endif
        } else {
          val b = make_bignum();
          mp_int tmp;
          mp_init(&tmp);
          mp_set_intptr(&tmp, an);
          mpe = mp_shift(&tmp, mp(b), bn);
          mp_clear(&tmp);
          if (mpe != MP_OKAY)
            break;
          return normalize(b);
        }
      }
    case BGNUM:
      if (bn < INT_MIN || bn > INT_MAX) {
        goto bad4;
      } else {
        val b = make_bignum();
        if ((mpe = mp_shift(mp(a), mp(b), bn)) != MP_OKAY)
          break;
        return normalize(b);
      }
    default:
      goto bad3;
    }
  } else {
    switch (ta) {
    case NUM:
      {
        cnum an = c_n(a);
        bn = -bn;
        if (bn <= NUM_BIT)
          return num_fast(an >> bn);
        return num_fast(an >> NUM_BIT);
      }
    case BGNUM:
      {
        val b = make_bignum();
        if ((mpe = mp_shift(mp(a), mp(b), bn)) != MP_OKAY)
          break;
        return normalize(b);
      }
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
  val self = bit_s;
  type_t ta = type(a);
  cnum bn;
  mp_err mpe = MP_OKAY;

  if (ta == COBJ)
    return do_binary_method(self, self, a, bit);

  if (!fixnump(bit))
    goto bad;

  bn = c_n(bit);

  if (bn < 0)
    goto bad2;

  switch (ta) {
  case NUM:
    {
      cnum an = c_n(a);
      if (bn < (SIZEOF_PTR * CHAR_BIT))
        return (an & (convert(cnum, 1) << bn)) ? t : nil;
      return an < 0 ? t : nil;
    }
  case CHR:
    {
      cnum an = c_ch(a);
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

val maskv(varg bits)
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

val bitset(val n)
{
  val self = bitset_s;
  list_collect_decl (out, ptail);

  switch (type(n)) {
  case NUM:
    {
      cnum c = c_n(n);
      ucnum d = c;
      int p = 0;

      if (c < 0)
        d = ~d;

      for (; d; d >>= 1, p++)
        if (d & 1)
          ptail = list_collect(ptail, num_fast(p));

      return out;
    }
  case CHR:
    {
      cnum c = c_ch(n);
      ucnum d = c;
      int p = 0;

      if (c < 0)
        d = ~d;

      for (; d; d >>= 1, p++)
        if (d & 1)
          ptail = list_collect(ptail, num_fast(p));

      return out;
    }
  case BGNUM:
    {
      mp_int *mn = mp(n);

      if (mp_cmp_z(mn) == MP_LT) {
        mp_int tmp;
        mp_size i = 0;
        ucnum p = 0;
        mp_2comp(mn, &tmp, mn->used);
        for (; i < tmp.used; i++) {
          mp_digit m;
          mp_digit d = tmp.dp[i];
          for (m = 1; m; m <<= 1, p++)
            if ((d & m) == 0)
              ptail = list_collect(ptail, unum(p));
        }
        mp_clear(&tmp);
      } else {
        mp_size i = 0;
        ucnum p = 0;
        for (; i < mn->used; i++) {
          mp_digit m;
          mp_digit d = mn->dp[i];
          for (m = 1; m; m <<= 1, p++)
            if ((d & m) != 0)
              ptail = list_collect(ptail, unum(p));
        }
      }

      return out;
    }
  case COBJ:
    return do_unary_method(self, self, n);
  default:
    uw_throwf(error_s, lit("~a: non-integral operand ~s"), self, n, nao);
  }
}

val logcount(val n)
{
  val self = logcount_s;

  switch (type(n)) {
  case CHR:
    return logcount(num_fast(c_ch(n)));
  case NUM:
    {
      int_ptr_t c = c_n(n);
      uint_ptr_t d = c;
      if (c < 0)
        d = ~d;
#if CHAR_BIT * SIZEOF_PTR == 64
      d = ((d & 0xAAAAAAAAAAAAAAAA) >>  1) + (d & 0x5555555555555555);
      d = ((d & 0xCCCCCCCCCCCCCCCC) >>  2) + (d & 0x3333333333333333);
      d = ((d & 0xF0F0F0F0F0F0F0F0) >>  4) + (d & 0x0F0F0F0F0F0F0F0F);
      d = ((d & 0xFF00FF00FF00FF00) >>  8) + (d & 0x00FF00FF00FF00FF);
      d = ((d & 0xFFFF0000FFFF0000) >> 16) + (d & 0x0000FFFF0000FFFF);
      d = ((d & 0xFFFFFFFF00000000) >> 32) + (d & 0x00000000FFFFFFFF);
#elif CHAR_BIT * SIZEOF_PTR == 32
      d = ((d & 0xAAAAAAAA) >>  1) + (d & 0x55555555);
      d = ((d & 0xCCCCCCCC) >>  2) + (d & 0x33333333);
      d = ((d & 0xF0F0F0F0) >>  4) + (d & 0x0F0F0F0F);
      d = ((d & 0xFF00FF00) >>  8) + (d & 0x00FF00FF);
      d = ((d & 0xFFFF0000) >> 16) + (d & 0x0000FFFF);
#else
#error portme
#endif
      return unum(d);
    }
  case BGNUM:
    {
      mp_err co = mp_count_ones(mp(n));
      if (co < 0)
        internal_error("problem in bignum arithmetic");
      return unum(co);
    }
  case COBJ:
    return do_unary_method(self, self, n);
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
  val self = tofloat_s;

  switch (tag(obj)) {
  case TAG_NUM:
    return flo_int(obj);
  case TAG_CHR:
    {
      cnum ch = c_ch(obj);
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
    if (type(obj) == COBJ)
      return do_unary_method(self, self, obj);
    uw_throwf(error_s, lit("~s: ~s is not convertible to float"),
              self, obj, nao);
  }
}

val toint(val obj, val base)
{
  val self = toint_s;

  switch (tag(obj)) {
  case TAG_NUM:
    return obj;
  case TAG_LIT:
    return int_str(obj, base);
  case TAG_CHR:
    {
      cnum ch = c_ch(obj);

      if (ch >= '0' && ch <= '9')
        return num(ch - '0');

      if (iswalpha(ch)) {
        cnum n = 10 + towupper(ch) - 'A';
        cnum b = c_num(default_arg(base, num_fast(10)), self);

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
    if (type(obj) == COBJ)
      return do_unary_method(self, self, obj);
    uw_throwf(error_s, lit("~a: ~s is not convertible to integer"),
              self, obj, nao);
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
  val self = width_s;

  switch (type(obj)) {
  case CHR:
    return width(num_fast(c_ch(obj)));
  case NUM:
    {
      cnum n = c_n(obj);

      if (n < 0) {
        n &= INT_PTR_MAX;
        n ^= INT_PTR_MAX;
        return num_fast(highest_bit(n));
      }
      return num_fast(highest_bit(n));
    }
  case BGNUM:
    {
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
  case COBJ:
    return do_unary_method(self, self, obj);
  default:
    break;
  }
  uw_throwf(error_s, lit("~a: ~s isn't an integer"), self, obj, nao);
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
  if (!integerp(r) || le(r, one))
    uw_throwf(error_s, lit("~a: base ~s must be positive integer"),
              self, r, nao);

  {
    val k = r;
    val p = nil;
    list_collect_decl (out, ptail);

    while (le(k, n)) {
      push(k, &p);
      k = mul(k, r);
    }

    while (p) {
      val p0 = rcyc_pop(&p);
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
  val self = lit("poly");
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
    uw_throwf(error_s, lit("~a: bad argument ~s; poly wants a list or vector!"),
              self, seq, nao);

  }
}

val rpoly(val x, val seq)
{
  val self = lit("rpoly");
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
    uw_throwf(error_s, lit("~a: bad argument ~s; poly wants a list or vector!"),
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

val num(cnum n)
{
  return (n >= NUM_MIN && n <= NUM_MAX) ? num_fast(n) : bignum(n);
}

cnum c_num(val n, val self)
{
  switch (type(n)) {
  case CHR:
    return c_ch(n);
  case NUM:
    return c_n(n);
  case BGNUM:
    if (mp_in_intptr_range(mp(n))) {
      int_ptr_t out;
      mp_get_intptr(mp(n), &out);
      return out;
    }
    uw_throwf(error_s, lit("~a: ~s is out of allowed range [~s, ~s]"),
              self, n, num(INT_PTR_MIN), num(INT_PTR_MAX), nao);
  default:
    uw_throwf(type_error_s, lit("~a: ~s is not an integer"), self, n, nao);
  }
}

cnum c_fixnum(val num, val self)
{
  switch (type(num)) {
  case CHR:
    return c_ch(num);
  case NUM:
    return c_n(num);
  default:
    type_mismatch(lit("~a: ~s is not fixnum integer or character"),
                  self, num, nao);
  }
}

#if HAVE_FPCLASSIFY
INLINE int bad_float(double d)
{
  switch (fpclassify(d)) {
  case FP_ZERO:
  case FP_NORMAL:
  case FP_SUBNORMAL:
    return 0;
  default:
    return 1;
  }
}
#else
#define bad_float(d) (0)
#endif

#if CONFIG_NAN_BOXING && defined __GNUC__
#pragma GCC diagnostic ignored "-Wstrict-aliasing"
#endif

val flo(double n)
{
  if (bad_float(n)) {
    uw_throw(numeric_error_s, lit("out-of-range floating-point result"));
  } else {
#if CONFIG_NAN_BOXING
    ucnum u = *(ucnum *) &n + NAN_FLNUM_DELTA;
    return coerce(val, u);
#else
    val obj = make_obj();
    obj->fl.type = FLNUM;
    obj->fl.n = n;
    return obj;
#endif
  }
}

#if CONFIG_NAN_BOXING && defined __GNUC__
#pragma GCC diagnostic warning "-Wstrict-aliasing"
#endif

double c_flo(val num, val self)
{
#if CONFIG_NAN_BOXING
  if (is_flo(num))
    return c_f(num);
  throw_mismatch(self, num, FLNUM);
#else
  type_check(self, num, FLNUM);
  return num->fl.n;
#endif
}

val fixnump(val num)
{
  return (is_num(num)) ? t : nil;
}

val bignump(val num)
{
  return (type(num) == BGNUM) ? t : nil;
}

val integerp(val num)
{
  switch (tag_ex(num)) {
  case TAG_NUM:
    return t;
  case TAG_PTR:
    if (num == nil)
      return nil;
    if (num->t.type == BGNUM)
      return t;
    /* fallthrough */
  default:
    return nil;
  }
}

val floatp(val num)
{
  return (type(num) == FLNUM) ? t : nil;
}

val numberp(val num)
{
  switch (tag_ex(num)) {
  case TAG_NUM:
    return t;
#if CONFIG_NAN_BOXING
  case TAG_FLNUM:
    return t;
#endif
  case TAG_PTR:
    if (num == nil)
      return nil;
    if (num->t.type == BGNUM || num->t.type == FLNUM)
      return t;
    /* fallthrough */
  default:
    return nil;
  }
}

val arithp(val obj)
{
  switch (type(obj)) {
  case NUM:
  case BGNUM:
  case FLNUM:
  case CHR:
  case RNG:
    return t;
  default:
    if (obj_struct_p(obj) && get_special_slot(obj, plus_m))
      return t;
  }

  return nil;
}

val nary_op(val self, val (*bfun)(val, val),
            val (*ufun)(val self, val),
            varg args, val emptyval)
{
  val acc, next;
  cnum index = 0;

  if (!args_more(args, index))
    return emptyval;

  acc = args_get(args, &index);

  if (!args_more(args, index))
    return ufun(self, acc);

  do {
    next = args_get(args, &index);
    acc = bfun(acc, next);
  } while (args_more(args, index));

  return acc;
}

val nary_simple_op(val (*bfun)(val, val),
                   varg args, val firstval)
{
  val acc = firstval, next;
  cnum index = 0;

  while (args_more(args, index)) {
    next = args_get(args, &index);
    acc = bfun(acc, next);
  }

  return acc;
}

static val nary_op_seq(val self, val (*bfun)(val, val),
                       val (*ufun)(val self, val),
                       val seq, val emptyval)
{
  seq_iter_t item_iter;
  val acc, next;
  seq_iter_init(self, &item_iter, seq);

  if (!seq_get(&item_iter, &acc))
    return emptyval;

  if (!seq_get(&item_iter, &next))
    return ufun(self, acc);

  do {
    acc = bfun(acc, next);
  } while (seq_get(&item_iter, &next));

  return acc;
}

static val nary_op_seq_keyfun(val self, val (*bfun)(val, val),
                              val (*ufun)(val self, val),
                              val seq, val emptyval, val keyfun)
{
  seq_iter_t item_iter;
  val acc, next;
  seq_iter_init(self, &item_iter, seq);

  if (!seq_get(&item_iter, &acc))
    return emptyval;

  acc = funcall1(keyfun, acc);

  if (!seq_get(&item_iter, &next))
    return ufun(self, acc);

  do {
    next = funcall1(keyfun, next);
    acc = bfun(acc, next);
  } while (seq_get(&item_iter, &next));

  return acc;
}

static val unary_num(val self, val arg)
{
  switch (type(arg)) {
  case NUM:
  case BGNUM:
  case FLNUM:
  case COBJ:
    return arg;
  default:
    uw_throwf(error_s, lit("~a: ~s isn't a number"), self, arg, nao);
  }
}

static val unary_arith(val self, val arg)
{
  switch (type(arg)) {
  case NUM:
  case CHR:
  case BGNUM:
  case FLNUM:
  case COBJ:
    return arg;
  default:
    uw_throwf(error_s, lit("~a: invalid argument ~s"), self, arg, nao);
  }
}

static val unary_int(val self, val arg)
{
  if (!integerp(arg))
    uw_throwf(error_s, lit("~a: ~s isn't an integer"), self, arg, nao);
  return arg;
}

val plusv(varg nlist)
{
  return nary_op(plus_s, plus, unary_arith, nlist, zero);
}

val minusv(val minuend, varg nlist)
{
  val acc = minuend, next;
  cnum index = 0;

  if (!args_more(nlist, index))
    return neg(acc);

  do {
    next = args_get(nlist, &index);
    acc = minus(acc, next);
  } while (args_more(nlist, index));

  return acc;
}

val mulv(varg nlist)
{
  return nary_op(mul_s, mul, unary_num, nlist, one);
}

val divv(val dividend, varg nlist)
{
  val acc = dividend, next;
  cnum index = 0;

  if (!args_more(nlist, index))
    return divi(acc, colon_k);

  do {
    next = args_get(nlist, &index);
    acc = divi(acc, next);
  } while (args_more(nlist, index));

  return acc;
}

val logandv(varg nlist)
{
  return nary_op(logand_s, logand, unary_int, nlist, negone);
}

val logiorv(varg nlist)
{
  return nary_op(logior_s, logior, unary_int, nlist, zero);
}

val gtv(val first, varg rest)
{
  cnum index = 0;

  while (args_more(rest, index)) {
    val elem = args_get(rest, &index);
    if (!gt(first, elem))
      return nil;
    first = elem;
  }

  if (index == 0)
    (void) unary_arith(gt_s, first);

  return t;
}

val ltv(val first, varg rest)
{
  cnum index = 0;

  while (args_more(rest, index)) {
    val elem = args_get(rest, &index);
    if (!lt(first, elem))
      return nil;
    first = elem;
  }

  if (index == 0)
    (void) unary_arith(lt_s, first);

  return t;
}

val gev(val first, varg rest)
{
  cnum index = 0;

  while (args_more(rest, index)) {
    val elem = args_get(rest, &index);
    if (!ge(first, elem))
      return nil;
    first = elem;
  }

  if (index == 0)
    (void) unary_arith(ge_s, first);

  return t;
}

val lev(val first, varg rest)
{
  cnum index = 0;

  while (args_more(rest, index)) {
    val elem = args_get(rest, &index);
    if (!le(first, elem))
      return nil;
    first = elem;
  }

  if (index == 0)
    (void) unary_arith(le_s, first);

  return t;
}

val numeqv(val first, varg rest)
{
  cnum index = 0;

  while (args_more(rest, index)) {
    val elem = args_get(rest, &index);
    if (!numeq(first, elem))
      return nil;
    first = elem;
  }

  if (index == 0)
    (void) unary_arith(numeq_s, first);

  return t;
}

val numneqv(varg args)
{
  if (!args->list) {
    cnum i, j, n = args->fill;

    if (n == 1) {
      (void) unary_arith(lit("/="), args->arg[0]);
      return t;
    }

    for (i = 0; i < n; i++)
      for (j = i + 1; j < n; j++)
        if (numeq(args->arg[i], args->arg[j]))
          return nil;

    return t;
  } else {
    val i, j;
    val list = args_get_list(args);

    if (list && !cdr(list)) {
      (void) unary_arith(lit("/="), car(list));
      return t;
    }

    for (i = list; i; i = cdr(i))
      for (j = cdr(i); j; j = cdr(j))
        if (numeq(car(i), car(j)))
          return nil;
    return t;
  }
}

val sum(val seq, val keyfun)
{
  return if3(missingp(keyfun),
             nary_op_seq(plus_s, plus, unary_arith, seq, zero),
             nary_op_seq_keyfun(plus_s, plus, unary_arith, seq, zero, keyfun));
}

val prod(val seq, val keyfun)
{
  return if3(missingp(keyfun),
             nary_op_seq(mul_s, mul, unary_num, seq, one),
             nary_op_seq_keyfun(mul_s, mul, unary_num, seq, one, keyfun));
}

static val rexpt(val right, val left)
{
  return expt(left, right);
}

val exptv(varg nlist)
{
  val self = lit("exptv");
  cnum nargs = args_count(nlist, self);
  args_decl(rnlist, max(ARGS_MIN, nargs));
  args_copy_reverse(rnlist, nlist, nargs);
  return nary_op(expt_s, rexpt, unary_num, rnlist, one);
}

static val abso_self(val self, val arg)
{
  (void) self;
  return abso(arg);
}

val gcdv(varg nlist)
{
  return nary_op(lit("gcd"), gcd, abso_self, nlist, zero);
}

val lcmv(varg nlist)
{
  return nary_op(lit("lcm"), lcm, abso_self, nlist, zero);
}

static struct cobj_ops psq_ops = cobj_ops_init(cobj_equal_handle_op,
                                               cptr_print_op,
                                               cobj_destroy_free_op,
                                               cobj_mark_op,
                                               cobj_handle_hash_op);

static val quant_fun(val psqo, varg args)
{
  val self = lit("quantile");
  cnum idx = 0;
  struct psquare *psq = coerce(struct psquare *, psqo->cp.handle);

  while (args_more(args, idx)) {
    val arg = args_get(args, &idx);
    if (numberp(arg)) {
      double s = c_flo(to_float(self, arg), self);
      psq_add_sample(psq, s, self);
    } else {
      seq_iter_t item_iter;
      val sample;
      seq_iter_init(self, &item_iter, arg);

      while (seq_get(&item_iter, &sample)) {
        double s = c_flo(to_float(self, sample), self);
        psq_add_sample(psq, s, self);
      }
    }
  }

  return flo(psq_get_estimate(psq));
}

val quantile(val pv, val grsize_in, val rate_in)
{
  val self = lit("quantile");
  double p = c_flo(to_float(self, pv), self);
  struct psquare *psq = coerce(struct psquare *, chk_malloc(sizeof *psq));
  val psqo = cptr_typed(coerce(mem_t *, psq), nil, &psq_ops);
  if (missingp(grsize_in)) {
    psq_init(psq, p);
  } else {
    ucnum grsize = c_unum(grsize_in, self);
    double rate = if3(missingp(rate_in),
                      0.999,
                      c_flo(to_float(self, rate_in), self));
    psq_init_grouped(psq, p, grsize, rate);
  }
  return func_f0v(psqo, quant_fun);
}

static val arith_set_entries(val fun)
{
  val name[] = {
    lit("cbrt"),lit("erf"), lit("erfc"), lit("exp10"),
    lit("exp2"),lit("expm1"), lit("gamma"), lit("j0"),
    lit("j1"),lit("lgamma"), lit("log1p"), lit("logb"),
    lit("nearbyint"),lit("rint"), lit("significand"), lit("tgamma"),
    lit("y0"),lit("y1"), lit("copysign"), lit("drem"),
    lit("fdim"),lit("fmax"), lit("fmin"), lit("hypot"),
    lit("jn"),lit("ldexp"), lit("nextafter"),
    lit("remainder"),lit("scalb"), lit("scalbln"), lit("yn"),
    lit("r-copysign"), lit("r-drem"),
    lit("r-fdim"),lit("r-fmax"), lit("r-fmin"), lit("r-hypot"),
    lit("r-jn"),lit("r-ldexp"), lit("r-nextafter"),
    lit("r-remainder"),lit("r-scalb"), lit("r-scalbln"), lit("r-yn"),
    nil
  };
  autoload_set(al_fun, name, fun);
  return nil;
}

static val arith_instantiate(void)
{
  cbrt_s = intern(lit("cbrt"), user_package);
  erf_s = intern(lit("erf"), user_package);
  erfc_s = intern(lit("erfc"), user_package);
  exp10_s = intern(lit("exp10"), user_package);
  exp2_s = intern(lit("exp2"), user_package);
  expm1_s = intern(lit("expm1"), user_package);
  gamma_s = intern(lit("gamma"), user_package);
  j0_s = intern(lit("j0"), user_package);
  j1_s = intern(lit("j1"), user_package);
  lgamma_s = intern(lit("lgamma"), user_package);
  log1p_s = intern(lit("log1p"), user_package);
  logb_s = intern(lit("logb"), user_package);
  nearbyint_s = intern(lit("nearbyint"), user_package);
  rint_s = intern(lit("rint"), user_package);
  significand_s = intern(lit("significand"), user_package);
  tgamma_s = intern(lit("tgamma"), user_package);
  y0_s = intern(lit("y0"), user_package);
  y1_s = intern(lit("y1"), user_package);
  copysign_s = intern(lit("copysign"), user_package);
  drem_s = intern(lit("drem"), user_package);
  fdim_s = intern(lit("fdim"), user_package);
  fmax_s = intern(lit("fmax"), user_package);
  fmin_s = intern(lit("fmin"), user_package);
  hypot_s = intern(lit("hypot"), user_package);
  jn_s = intern(lit("jn"), user_package);
  ldexp_s = intern(lit("ldexp"), user_package);
  nextafter_s = intern(lit("nextafter"), user_package);
  remainder_s = intern(lit("remainder"), user_package);
  scalb_s = intern(lit("scalb"), user_package);
  scalbln_s = intern(lit("scalbln"), user_package);
  yn_s = intern(lit("yn"), user_package);
  r_copysign_s = intern(lit("r-copysign"), user_package);
  r_drem_s = intern(lit("r-drem"), user_package);
  r_fdim_s = intern(lit("r-fdim"), user_package);
  r_fmax_s = intern(lit("r-fmax"), user_package);
  r_fmin_s = intern(lit("r-fmin"), user_package);
  r_hypot_s = intern(lit("r-hypot"), user_package);
  r_jn_s = intern(lit("r-jn"), user_package);
  r_ldexp_s = intern(lit("r-ldexp"), user_package);
  r_nextafter_s = intern(lit("r-nextafter"), user_package);
  r_remainder_s = intern(lit("r-remainder"), user_package);
  r_scalb_s = intern(lit("r-scalb"), user_package);
  r_scalbln_s = intern(lit("r-scalbln"), user_package);
  r_yn_s = intern(lit("r-yn"), user_package);

  reg_fun(cbrt_s, func_n1(cbrt_wrap));
  reg_fun(erf_s, func_n1(erf_wrap));
  reg_fun(erfc_s, func_n1(erfc_wrap));
  reg_fun(exp10_s, func_n1(exp10_wrap));
  reg_fun(exp2_s, func_n1(exp2_wrap));
  reg_fun(expm1_s, func_n1(expm1_wrap));
  reg_fun(gamma_s, func_n1(gamma_wrap));
  reg_fun(j0_s, func_n1(j0_wrap));
  reg_fun(j1_s, func_n1(j1_wrap));
  reg_fun(lgamma_s, func_n1(lgamma_wrap));
  reg_fun(log1p_s, func_n1(log1p_wrap));
  reg_fun(logb_s, func_n1(logb_wrap));
  reg_fun(nearbyint_s, func_n1(nearbyint_wrap));
  reg_fun(rint_s, func_n1(rint_wrap));
  reg_fun(significand_s, func_n1(significand_wrap));
  reg_fun(tgamma_s, func_n1(tgamma_wrap));
  reg_fun(y0_s, func_n1(y0_wrap));
  reg_fun(y1_s, func_n1(y1_wrap));
  reg_fun(copysign_s, func_n2(copysign_wrap));
  reg_fun(drem_s, func_n2(drem_wrap));
  reg_fun(fdim_s, func_n2(fdim_wrap));
  reg_fun(fmax_s, func_n2(fmax_wrap));
  reg_fun(fmin_s, func_n2(fmin_wrap));
  reg_fun(hypot_s, func_n2(hypot_wrap));
  reg_fun(jn_s, func_n2(jn_wrap));
  reg_fun(ldexp_s, func_n2(ldexp_wrap));
  reg_fun(nextafter_s, func_n2(nextafter_wrap));
  reg_fun(remainder_s, func_n2(remainder_wrap));
  reg_fun(scalb_s, func_n2(scalb_wrap));
  reg_fun(scalbln_s, func_n2(scalbln_wrap));
  reg_fun(yn_s, func_n2(yn_wrap));
  return nil;
}

void arith_init(void)
{
  log2_init();

  plus_s = intern(lit("+"), user_package);
  minus_s = intern(lit("-"), user_package);
  inv_minus_s = intern(lit("--"), user_package);
  neg_s = intern(lit("neg"), user_package);
  abs_s = intern(lit("abs"), user_package);
  signum_s = intern(lit("signum"), user_package);
  mul_s = intern(lit("*"), user_package);
  div_s = intern(lit("/"), user_package);
  recip_s = intern(lit("recip"), user_package);
  inv_div_s = intern(lit("//"), user_package);
  trunc1_s = intern(lit("trunc1"), user_package);
  trunc_s = intern(lit("trunc"), user_package);
  r_trunc_s = intern(lit("r-trunc"), user_package);
  mod_s = intern(lit("mod"), user_package);
  r_mod_s = intern(lit("r-mod"), user_package);
  zerop_s = intern(lit("zerop"), user_package);
  plusp_s = intern(lit("plusp"), user_package);
  minusp_s = intern(lit("minusp"), user_package);
  evenp_s = intern(lit("evenp"), user_package);
  oddp_s = intern(lit("oddp"), user_package);
  gt_s = intern(lit(">"), user_package);
  lt_s = intern(lit("<"), user_package);
  ge_s = intern(lit(">="), user_package);
  le_s = intern(lit("<="), user_package);
  numeq_s = intern(lit("="), user_package);
  expt_s = intern(lit("expt"), user_package);
  r_expt_s = intern(lit("r-expt"), user_package);
  exptmod_s = intern(lit("exptmod"), user_package);
  isqrt_s = intern(lit("isqrt"), user_package);
  square_s = intern(lit("square"), user_package);
  floor_s = intern(lit("floor"), user_package);
  floor1_s = intern(lit("floor1"), user_package);
  r_floor_s = intern(lit("r-floor"), user_package);
  ceil_s = intern(lit("ceil"), user_package);
  ceil1_s = intern(lit("ceil1"), user_package);
  r_ceil_s = intern(lit("r-ceil"), user_package);
  round_s = intern(lit("round"), user_package);
  round1_s = intern(lit("round1"), user_package);
  r_round_s = intern(lit("r-round"), user_package);
  sin_s = intern(lit("sin"), user_package);
  cos_s = intern(lit("cos"), user_package);
  tan_s = intern(lit("tan"), user_package);
  asin_s = intern(lit("asin"), user_package);
  acos_s = intern(lit("acos"), user_package);
  atan_s = intern(lit("atan"), user_package);
  atan2_s = intern(lit("atan2"), user_package);
  r_atan2_s = intern(lit("r-atan2"), user_package);
  sinh_s = intern(lit("sinh"), user_package);
  cosh_s = intern(lit("cosh"), user_package);
  tanh_s = intern(lit("tanh"), user_package);
  asinh_s = intern(lit("asinh"), user_package);
  acosh_s = intern(lit("acosh"), user_package);
  atanh_s = intern(lit("atanh"), user_package);
  log_s = intern(lit("log"), user_package);
  log2_s = intern(lit("log2"), user_package);
  log10_s = intern(lit("log10"), user_package);
  exp_s = intern(lit("exp"), user_package);
  sqrt_s = intern(lit("sqrt"), user_package);
  logand_s = intern(lit("logand"), user_package);
  logior_s = intern(lit("logior"), user_package);
  logxor_s = intern(lit("logxor"), user_package);
  lognot1_s = intern(lit("lognot1"), user_package);
  lognot_s = intern(lit("lognot"), user_package);
  r_lognot_s = intern(lit("r-lognot"), user_package);
  logtrunc_s = intern(lit("logtrunc"), user_package);
  r_logtrunc_s = intern(lit("r-logtrunc"), user_package);
  sign_extend_s = intern(lit("sign-extend"), user_package);
  ash_s = intern(lit("ash"), user_package);
  bit_s = intern(lit("bit"), user_package);
  width_s = intern(lit("width"), user_package);
  bitset_s = intern(lit("bitset"), user_package);
  logcount_s = intern(lit("logcount"), user_package);
  tofloat_s = intern(lit("tofloat"), user_package);
  toint_s = intern(lit("toint"), user_package);

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

  reg_fun(tofloat_s, func_n1(tofloat));
  reg_fun(toint_s, func_n2o(toint, 1));
  reg_fun(intern(lit("tofloatz"), user_package), func_n1(tofloatz));
  reg_fun(intern(lit("tointz"), user_package), func_n2o(tointz, 1));
  reg_fun(plus_s, func_n0v(plusv));
  reg_fun(minus_s, func_n1v(minusv));
  reg_fun(mul_s, func_n0v(mulv));
  reg_fun(intern(lit("sum"), user_package), func_n2o(sum, 1));
  reg_fun(intern(lit("prod"), user_package), func_n2o(prod, 1));
  reg_fun(abs_s, func_n1(abso));
  reg_fun(trunc_s, func_n2o(trunc, 1));
  reg_fun(mod_s, func_n2(mod));
  reg_fun(zerop_s, func_n1(zerop));
  reg_fun(intern(lit("nzerop"), user_package), func_n1(nzerop));
  reg_fun(plusp_s, func_n1(plusp));
  reg_fun(minusp_s, func_n1(minusp));
  reg_fun(evenp_s, func_n1(evenp));
  reg_fun(oddp_s, func_n1(oddp));
  reg_fun(intern(lit("succ"), user_package), func_n1(succ));
  reg_fun(intern(lit("ssucc"), user_package), func_n1(ssucc));
  reg_fun(intern(lit("sssucc"), user_package), func_n1(sssucc));
  reg_fun(intern(lit("pred"), user_package), func_n1(pred));
  reg_fun(intern(lit("ppred"), user_package), func_n1(ppred));
  reg_fun(intern(lit("pppred"), user_package), func_n1(pppred));
  reg_fun(gt_s, func_n1v(gtv));
  reg_fun(lt_s, func_n1v(ltv));
  reg_fun(ge_s, func_n1v(gev));
  reg_fun(le_s, func_n1v(lev));
  reg_fun(numeq_s, func_n1v(numeqv));
  reg_fun(intern(lit("/="), user_package), func_n0v(numneqv));
  reg_fun(intern(lit("wrap"), user_package), func_n3(wrap));
  reg_fun(intern(lit("wrap*"), user_package), func_n3(wrap_star));
  reg_fun(div_s, func_n1v(divv));
  reg_fun(expt_s, func_n0v(exptv));
  reg_fun(exptmod_s, func_n3(exptmod));
  reg_fun(isqrt_s, func_n1(isqrt));
  reg_fun(square_s, func_n1(square));
  reg_fun(intern(lit("gcd"), user_package), func_n0v(gcdv));
  reg_fun(intern(lit("lcm"), user_package), func_n0v(lcmv));
  reg_fun(floor_s, func_n2o(floordiv, 1));
  reg_fun(ceil_s, func_n2o(ceildiv, 1));
  reg_fun(round_s, func_n2o(roundiv, 1));
  reg_fun(intern(lit("trunc-rem"), user_package), func_n2o(trunc_rem, 1));
  reg_fun(intern(lit("floor-rem"), user_package), func_n2o(floor_rem, 1));
  reg_fun(intern(lit("ceil-rem"), user_package), func_n2o(ceil_rem, 1));
  reg_fun(intern(lit("round-rem"), user_package), func_n2o(round_rem, 1));
  reg_fun(sin_s, func_n1(sine));
  reg_fun(cos_s, func_n1(cosi));
  reg_fun(tan_s, func_n1(tang));
  reg_fun(asin_s, func_n1(asine));
  reg_fun(acos_s, func_n1(acosi));
  reg_fun(atan_s, func_n1(atang));
  reg_fun(atan2_s, func_n2(atang2));
  reg_fun(sinh_s, func_n1(sineh));
  reg_fun(cosh_s, func_n1(cosih));
  reg_fun(tanh_s, func_n1(tangh));
  reg_fun(asinh_s, func_n1(asineh));
  reg_fun(acosh_s, func_n1(acosih));
  reg_fun(atanh_s, func_n1(atangh));
  reg_fun(log_s, func_n1(loga));
  reg_fun(log10_s, func_n1(logten));
  reg_fun(log2_s, func_n1(logtwo));
  reg_fun(exp_s, func_n1(expo));
  reg_fun(sqrt_s, func_n1(sqroot));
  reg_fun(logand_s, func_n0v(logandv));
  reg_fun(logior_s, func_n0v(logiorv));
  reg_fun(logxor_s, func_n2(logxor));
  reg_fun(intern(lit("logtest"), user_package), func_n2(logtest));
  reg_fun(lognot_s, func_n2o(lognot, 1));
  reg_fun(logtrunc_s, func_n2(logtrunc));
  reg_fun(sign_extend_s, func_n2(sign_extend));
  reg_fun(ash_s, func_n2(ash));
  reg_fun(bit_s, func_n2(bit));
  reg_fun(intern(lit("mask"), user_package), func_n0v(maskv));
  reg_fun(width_s, func_n1(width));
  reg_fun(logcount_s, func_n1(logcount));
  reg_fun(bitset_s, func_n1(bitset));
  reg_fun(intern(lit("cum-norm-dist"), user_package), func_n1(cum_norm_dist));
  reg_fun(intern(lit("inv-cum-norm"), user_package), func_n1(inv_cum_norm));
  reg_fun(intern(lit("n-choose-k"), user_package), func_n2(n_choose_k));
  reg_fun(intern(lit("n-perm-k"), user_package), func_n2(n_perm_k));
  reg_fun(intern(lit("fixnump"), user_package), func_n1(fixnump));
  reg_fun(intern(lit("bignump"), user_package), func_n1(bignump));
  reg_fun(intern(lit("floatp"), user_package), func_n1(floatp));
  reg_fun(intern(lit("integerp"), user_package), func_n1(integerp));
  reg_fun(intern(lit("numberp"), user_package), func_n1(numberp));
  reg_fun(intern(lit("arithp"), user_package), func_n1(arithp));

  reg_fun(signum_s, func_n1(signum));

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
  reg_fun(neg_s, func_n1(neg));

  reg_fun(intern(lit("quantile"), user_package), func_n3o(quantile, 1));
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

  autoload_reg(arith_instantiate, arith_set_entries);
}

void arith_compat_fixup(int compat_ver)
{
  if (compat_ver <= 202)
    reg_fun(logxor_s, func_n2(logxor_old));

  if (compat_ver <= 199) {
    reg_varl(intern(lit("*pi*"), user_package), flo(M_PI));
    reg_varl(intern(lit("*e*"), user_package), flo(M_E));
    reg_varl(intern(lit("*flo-dig*"), user_package), num_fast(DBL_DIG));
    reg_varl(intern(lit("*flo-max*"), user_package), flo(DBL_MAX));
    reg_varl(intern(lit("*flo-min*"), user_package), flo(DBL_MIN));
    reg_varl(intern(lit("*flo-epsilon*"), user_package), flo(DBL_EPSILON));
  }
}

void arith_free_all(void)
{
}
