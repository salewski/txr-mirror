/* This file is generated using txr arith.txr > arith.c!
 *
 * Copyright 2011
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
#include <assert.h>
#include <limits.h>
#include <stdarg.h>
#include <dirent.h>
#include <setjmp.h>
#include <wchar.h>
#include <limits.h>
#include "config.h"
#include "lib.h"
#include "unwind.h"
#include "gc.h"
#include "arith.h"

#define TAG_PAIR(A, B) ((A) << TAG_SHIFT | (B))
#define NOOP(A, B)
#define CNUM_BIT ((int) sizeof (cnum) * CHAR_BIT)
#define ABS(A) ((A) < 0 ? -(A) : (A))

static mp_int NUM_MAX_MP;

val make_bignum(void)
{
  val n = make_obj();
  n->bn.type = BGNUM;
  mp_init(&n->bn.mp);
  return n;
}

static val bignum(cnum cn)
{
  val n = make_bignum();
  mp_set_intptr(mp(n), cn);
  return n;
}

static val bignum_dbl_ipt(double_intptr_t di)
{
  val n = make_bignum();
  mp_set_double_intptr(mp(n), di);
  return n;
}

static val normalize(val bignum)
{
  if (mp_cmp_mag(mp(bignum), &NUM_MAX_MP) == MP_GT) {
    return bignum;
  } else {
    cnum fixnum;
    mp_get_intptr(mp(bignum), &fixnum);
    return num(fixnum);
  }
}

static int highest_bit(int_ptr_t n)
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

val plus(val anum, val bnum)
{
  int tag_a = tag(anum);
  int tag_b = tag(bnum);

  switch (TAG_PAIR(tag_a, tag_b)) {
  case TAG_PAIR(TAG_NUM, TAG_NUM): 
    {
      cnum a = c_num(anum);
      cnum b = c_num(bnum);
      cnum sum = a + b;

      if (sum < NUM_MIN || sum > NUM_MAX)
        return bignum(sum);
      return num(sum);
    } 
  case TAG_PAIR(TAG_NUM, TAG_PTR):
    {
      val n;
      type_check(bnum, BGNUM);
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
  case TAG_PAIR(TAG_PTR, TAG_NUM):
    {
      val n;
      type_check(anum, BGNUM);
      n = make_bignum();
      if (sizeof (int_ptr_t) <= sizeof (mp_digit))  {
        cnum b = c_num(bnum);
        cnum bp = ABS(b);
        if (b > 0)
          mp_add_d(mp(anum), bp, mp(n));
        else
          mp_sub_d(mp(bnum), bp, mp(n));
      } else {
        mp_int tmp;
        mp_init(&tmp);
        mp_set_intptr(&tmp, c_num(bnum));
        mp_add(mp(anum), &tmp, mp(n));
        mp_clear(&tmp);
      }
      return normalize(n);
    }
  case TAG_PAIR(TAG_PTR, TAG_PTR):
    {
      val n;
      type_check(anum, BGNUM);
      type_check(bnum, BGNUM);
      n = make_bignum();
      mp_add(mp(anum), mp(bnum), mp(n));
      return normalize(n);
    }
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
      cnum a = c_chr(anum);
      wchar_t b = c_num(bnum);
      cnum sum = a + b;

      if (sum < 0 || sum > 0x10FFFF)
        goto char_range;
      return chr(sum);
    }
  }
  uw_throwf(error_s, lit("plus: invalid operands ~s ~s"), anum, bnum, nao);
char_range:
  uw_throwf(numeric_error_s, 
            lit("plus: sum of ~s ~s is out of character range"),
            anum, bnum, nao);
  abort();
}

val minus(val anum, val bnum)
{
  int tag_a = tag(anum);
  int tag_b = tag(bnum);

  switch (TAG_PAIR(tag_a, tag_b)) {
  case TAG_PAIR(TAG_NUM, TAG_NUM): 
    {
      cnum a = c_num(anum);
      cnum b = c_num(bnum);
      cnum sum = a - b;

      if (sum < NUM_MIN || sum > NUM_MAX)
        return bignum(sum);
      return num(sum);
    } 
  case TAG_PAIR(TAG_NUM, TAG_PTR):
    {
      val n;
      type_check(bnum, BGNUM);
      n = make_bignum();
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
  case TAG_PAIR(TAG_PTR, TAG_NUM):
    {
      val n;
      type_check(anum, BGNUM);
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
  case TAG_PAIR(TAG_PTR, TAG_PTR):
    {
      val n;
      type_check(anum, BGNUM);
      type_check(bnum, BGNUM);
      n = make_bignum();
      mp_sub(mp(anum), mp(bnum), mp(n));
      return normalize(n);
    }
  case TAG_PAIR(TAG_CHR, TAG_NUM):
    {
      wchar_t a = c_chr(anum);
      cnum b = c_num(bnum);
      cnum sum = a - b;

      if (sum < 0 || sum > 0x10FFFF)
        uw_throwf(numeric_error_s, 
                  lit("minus: sum of ~s ~s is out of character range"),
                  anum, bnum, nao);
      return chr(sum);
    }
  }
  uw_throwf(error_s, lit("minus: invalid operands ~s ~s"), anum, bnum, nao);
  abort();
}

val neg(val anum)
{
  if (bignump(anum)) {
    val n = make_bignum();
    mp_neg(mp(anum), mp(n));
    return n;
  } else {
    cnum n = c_num(anum);
    return num(-n);
  }
}

val abso(val anum)
{
  if (bignump(anum)) {
    val n = make_bignum();
    mp_abs(mp(anum), mp(n));
    return n;
  } else {
    cnum n = c_num(anum);
    return num(n < 0 ? n : n);
  }
}

val mul(val anum, val bnum)
{
  int tag_a = tag(anum);
  int tag_b = tag(bnum);

  switch (TAG_PAIR(tag_a, tag_b)) {
  case TAG_PAIR(TAG_NUM, TAG_NUM): 
    {
      cnum a = c_num(anum);
      cnum b = c_num(bnum);
#if HAVE_DOUBLE_INTPTR_T
      double_intptr_t product = a * (double_intptr_t) b;
      if (product < NUM_MIN || product > NUM_MAX)
        return bignum_dbl_ipt(product);
      return num(product);
#else
      cnum ap = ABS(a);
      cnum bp = ABS(b);
      if (highest_bit(ap) + highest_bit(bp) < CNUM_BIT - 1) {
        cnum product = a * b;
        if (product >= NUM_MIN && product <= NUM_MAX)
          return num(a * b);
        return bignum(a * b);
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
    {
      val n;
      type_check(bnum, BGNUM);
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
  case TAG_PAIR(TAG_PTR, TAG_NUM):
    {
      val n;
      type_check(anum, BGNUM);
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
  case TAG_PAIR(TAG_PTR, TAG_PTR):
    {
      val n;
      type_check(anum, BGNUM);
      type_check(bnum, BGNUM);
      n = make_bignum();
      mp_mul(mp(anum), mp(bnum), mp(n));
      return n;
    }
  }
  uw_throwf(error_s, lit("mul: invalid operands ~s ~s"), anum, bnum, nao);
  abort();
}

val trunc(val anum, val bnum)
{
  int tag_a = tag(anum);
  int tag_b = tag(bnum);

  switch (TAG_PAIR(tag_a, tag_b)) {
  case TAG_PAIR(TAG_NUM, TAG_NUM):
    {
      cnum a = c_num(anum);
      cnum b = c_num(bnum);
      cnum ap = ABS(a);
      cnum bp = ABS(b);
      int neg = ((a < 0 && b > 0) || (a > 0 && b < 0));

      if (b == 0)
        uw_throw(numeric_error_s, lit("trunc: division by zero"));

      {
        cnum quot = ap / bp;
        return num(neg ? -quot : quot);
      }
    }
  case TAG_PAIR(TAG_NUM, TAG_PTR):
    type_check(bnum, BGNUM);
    return zero;
  case TAG_PAIR(TAG_PTR, TAG_NUM):
    {
      val n;
      type_check(anum, BGNUM);
      n = make_bignum();
      if (sizeof (int_ptr_t) <= sizeof (mp_digit)) {
        cnum b = c_num(bnum);
        cnum bp = ABS(b);
        if (mp_div_d(mp(anum), bp, mp(n), 0) != MP_OKAY)
          uw_throw(numeric_error_s, lit("trunc: division by zero"));
        if (b < 0)
          mp_neg(mp(n), mp(n));
      } else {
        mp_int tmp;
        mp_init(&tmp);
        mp_set_intptr(&tmp, c_num(bnum));
        if (mp_div(mp(anum), &tmp, mp(n), 0) != MP_OKAY) {
          mp_clear(&tmp);
          uw_throw(numeric_error_s, lit("trunc: division by zero"));
        }
        mp_clear(&tmp);
      }
      return normalize(n);
    }
  case TAG_PAIR(TAG_PTR, TAG_PTR):
    {
      val n;
      type_check(anum, BGNUM);
      type_check(bnum, BGNUM);
      n = make_bignum();
      if (mp_div(mp(anum), mp(bnum), mp(n), 0) != MP_OKAY)
          uw_throw(numeric_error_s, lit("trunc: division by zero"));
      return normalize(n);
    }
  }
  uw_throwf(error_s, lit("trunc: invalid operands ~s ~s"), anum, bnum, nao);
  abort();
}

val mod(val anum, val bnum)
{
  int tag_a = tag(anum);
  int tag_b = tag(bnum);

  switch (TAG_PAIR(tag_a, tag_b)) {
  case TAG_PAIR(TAG_NUM, TAG_NUM):
    {
      cnum a = c_num(anum);
      cnum b = c_num(bnum);

      if (b == 0)
        uw_throw(numeric_error_s, lit("mod: division by zero"));

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
    {
      val n;
      mp_int tmpa;
      mp_err err;
      type_check(bnum, BGNUM);
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
        uw_throw(numeric_error_s, lit("mod: division by zero"));
      return normalize(n);
    }
  case TAG_PAIR(TAG_PTR, TAG_NUM):
    {
      type_check(anum, BGNUM);
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
          uw_throw(numeric_error_s, lit("mod: division by zero"));
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
          uw_throw(numeric_error_s, lit("mod: division by zero"));
        return normalize(n);
      }
    }
  case TAG_PAIR(TAG_PTR, TAG_PTR):
    {
      val n;
      type_check(anum, BGNUM);
      type_check(bnum, BGNUM);
      n = make_bignum();
      if (mp_cmp_z(mp(bnum)) == MP_LT) {
        mp_int tmpa, tmpb;
        mp_err err;
        mp_init(&tmpa);
        mp_init(&tmpb);
        mp_neg(mp(anum), &tmpa);
        mp_neg(mp(bnum), &tmpb);
        err = mp_mod(&tmpa, &tmpb, mp(n));
        if (err != MP_OKAY)
          uw_throw(numeric_error_s, lit("mod: division by zero"));
        mp_clear(&tmpa);
        mp_clear(&tmpb);
        mp_neg(mp(n), mp(n));
      } else {
        if (mp_mod(mp(anum), mp(bnum), mp(n)) != MP_OKAY)
            uw_throw(numeric_error_s, lit("mod: division by zero"));
      }
      return normalize(n);
    }
  }
  uw_throwf(error_s, lit("mod: invalid operands ~s ~s"), anum, bnum, nao);
  abort();
}

val zerop(val num)
{
  if (num == zero)
    return t;

  if (!fixnump(num) && !bignump(num))
    uw_throwf(error_s, lit("zerof: ~s is not a number"), num, nao);
  return nil;
}

val evenp(val num)
{
  switch (tag(num)) {
  case TAG_NUM:
    return (c_num(num) % 2 == 0) ? t : nil;
  case TAG_PTR:
    if (num->t.type == BGNUM)
      return mp_iseven(mp(num)) ? t : nil;
    /* fallthrough */
  default:
    uw_throwf(error_s, lit("evenp: ~s is not a number"), num, nao);
    return nil;
  }
}

val oddp(val num)
{
  switch (tag(num)) {
  case TAG_NUM:
    return (c_num(num) % 2 != 0) ? t : nil;
  case TAG_PTR:
    if (num->t.type == BGNUM)
      return mp_isodd(mp(num)) ? t : nil;
    /* fallthrough */
  default:
    uw_throwf(error_s, lit("oddp: ~s is not a number"), num, nao);
    return nil;
  }
}

val gt(val anum, val bnum)
{
  int tag_a = tag(anum);
  int tag_b = tag(bnum);

  switch (TAG_PAIR(tag_a, tag_b)) {
  case TAG_PAIR(TAG_NUM, TAG_NUM):
  case TAG_PAIR(TAG_CHR, TAG_CHR):
  case TAG_PAIR(TAG_NUM, TAG_CHR):
  case TAG_PAIR(TAG_CHR, TAG_NUM):
    return c_num(anum) > c_num(bnum) ? t : nil;
  case TAG_PAIR(TAG_NUM, TAG_PTR):
  case TAG_PAIR(TAG_CHR, TAG_PTR):
    type_check(bnum, BGNUM);
    return mp_cmp_z(mp(bnum)) == MP_LT ? t : nil;
  case TAG_PAIR(TAG_PTR, TAG_NUM):
  case TAG_PAIR(TAG_PTR, TAG_CHR):
    type_check(anum, BGNUM);
    return mp_cmp_z(mp(anum)) == MP_GT ? t : nil;
  case TAG_PAIR(TAG_PTR, TAG_PTR):
    type_check(anum, BGNUM);
    return mp_cmp(mp(anum), mp(bnum)) == MP_GT ? t : nil;
  }

  uw_throwf(error_s, lit("gt: invalid operands ~s ~s"), anum, bnum, nao);
  abort();
}

val lt(val anum, val bnum)
{
  int tag_a = tag(anum);
  int tag_b = tag(bnum);

  switch (TAG_PAIR(tag_a, tag_b)) {
  case TAG_PAIR(TAG_NUM, TAG_NUM):
  case TAG_PAIR(TAG_CHR, TAG_CHR):
  case TAG_PAIR(TAG_NUM, TAG_CHR):
  case TAG_PAIR(TAG_CHR, TAG_NUM):
    return c_num(anum) < c_num(bnum) ? t : nil;
  case TAG_PAIR(TAG_NUM, TAG_PTR):
  case TAG_PAIR(TAG_CHR, TAG_PTR):
    type_check(bnum, BGNUM);
    return mp_cmp_z(mp(bnum)) == MP_GT ? t : nil;
  case TAG_PAIR(TAG_PTR, TAG_NUM):
  case TAG_PAIR(TAG_PTR, TAG_CHR):
    type_check(anum, BGNUM);
    return mp_cmp_z(mp(anum)) == MP_LT ? t : nil;
  case TAG_PAIR(TAG_PTR, TAG_PTR):
    type_check(anum, BGNUM);
    return mp_cmp(mp(anum), mp(bnum)) == MP_LT ? t : nil;
  }

  uw_throwf(error_s, lit("lt: invalid operands ~s ~s"), anum, bnum, nao);
  abort();
}

val ge(val anum, val bnum)
{
  int tag_a = tag(anum);
  int tag_b = tag(bnum);

  switch (TAG_PAIR(tag_a, tag_b)) {
  case TAG_PAIR(TAG_NUM, TAG_NUM):
  case TAG_PAIR(TAG_CHR, TAG_CHR):
  case TAG_PAIR(TAG_NUM, TAG_CHR):
  case TAG_PAIR(TAG_CHR, TAG_NUM):
    return c_num(anum) >= c_num(bnum) ? t : nil;
  case TAG_PAIR(TAG_NUM, TAG_PTR):
  case TAG_PAIR(TAG_CHR, TAG_PTR):
    type_check(bnum, BGNUM);
    return mp_cmp_z(mp(bnum)) == MP_LT ? t : nil;
  case TAG_PAIR(TAG_PTR, TAG_NUM):
  case TAG_PAIR(TAG_PTR, TAG_CHR):
    type_check(anum, BGNUM);
    return mp_cmp_z(mp(anum)) == MP_GT ? t : nil;
  case TAG_PAIR(TAG_PTR, TAG_PTR):
    type_check(anum, BGNUM);
    switch (mp_cmp(mp(anum), mp(bnum))) {
    case MP_GT: case MP_EQ:
      return t;
    default:
      return nil;
    }
  }

  uw_throwf(error_s, lit("ge: invalid operands ~s ~s"), anum, bnum, nao);
  abort();
}

val le(val anum, val bnum)
{
  int tag_a = tag(anum);
  int tag_b = tag(bnum);

  switch (TAG_PAIR(tag_a, tag_b)) {
  case TAG_PAIR(TAG_NUM, TAG_NUM):
  case TAG_PAIR(TAG_CHR, TAG_CHR):
  case TAG_PAIR(TAG_NUM, TAG_CHR):
  case TAG_PAIR(TAG_CHR, TAG_NUM):
    return c_num(anum) <= c_num(bnum) ? t : nil;
  case TAG_PAIR(TAG_NUM, TAG_PTR):
  case TAG_PAIR(TAG_CHR, TAG_PTR):
    type_check(bnum, BGNUM);
    return mp_cmp_z(mp(bnum)) == MP_GT ? t : nil;
  case TAG_PAIR(TAG_PTR, TAG_NUM):
  case TAG_PAIR(TAG_PTR, TAG_CHR):
    type_check(anum, BGNUM);
    return mp_cmp_z(mp(anum)) == MP_LT ? t : nil;
  case TAG_PAIR(TAG_PTR, TAG_PTR):
    type_check(anum, BGNUM);
    switch (mp_cmp(mp(anum), mp(bnum))) {
    case MP_LT: case MP_EQ:
      return t;
    default:
      return nil;
    }
  }

  uw_throwf(error_s, lit("lt: invalid operands ~s ~s"), anum, bnum, nao);
  abort();
}

val expt(val anum, val bnum)
{
  int tag_a = tag(anum);
  int tag_b = tag(bnum);

  switch (TAG_PAIR(tag_a, tag_b)) {
  case TAG_PAIR(TAG_NUM, TAG_NUM):
    {
      cnum a = c_num(anum);
      cnum b = c_num(bnum);
      mp_int tmpa;
      val n;
      if (b < 0)
        uw_throw(error_s, lit("expt: negative exponent"));
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
  case TAG_PAIR(TAG_NUM, TAG_PTR):
    {
      cnum a = c_num(anum);
      mp_int tmpa;
      val n;
      type_check(bnum, BGNUM);
      if (mp_cmp_z(mp(bnum)) == MP_LT)
        uw_throw(error_s, lit("expt: negative exponent"));
      n = make_bignum();
      mp_init(&tmpa);
      mp_set_intptr(&tmpa, a);
      mp_expt(&tmpa, mp(bnum), mp(n));
      mp_clear(&tmpa);
      return normalize(n);
    }
  case TAG_PAIR(TAG_PTR, TAG_NUM):
    {
      cnum b = c_num(bnum);
      val n;
      type_check(anum, BGNUM);
      if (b < 0)
        uw_throw(error_s, lit("expt: negative exponent"));
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
  case TAG_PAIR(TAG_PTR, TAG_PTR):
    {
      val n;
      type_check(anum, BGNUM);
      type_check(bnum, BGNUM);
      if (mp_cmp_z(mp(bnum)) == MP_LT)
        uw_throw(error_s, lit("expt: negative exponent"));
      n = make_bignum();
      mp_expt(mp(anum), mp(bnum), mp(n));
      normalize(n);
      return n;
    }
  }

  uw_throwf(error_s, lit("expt: invalid operands ~s ~s"), anum, bnum, nao);
  abort();
}

val exptmod(val base, val exp, val mod)
{
  val n;

  if (!numberp(base) || !numberp(exp) || !numberp(mod))
    goto inval;

  if (fixnump(base))
    base = bignum(c_num(base));

  if (fixnump(exp))
    exp = bignum(c_num(exp));

  if (fixnump(mod))
    mod = bignum(c_num(mod));

  n = make_bignum();

  if (mp_exptmod(mp(base), mp(exp), mp(mod), mp(n)) != MP_OKAY)
    goto inval;

  return n;
inval:
  uw_throwf(error_s, lit("exptmod: invalid operands ~s ~s ~s"),
            base, exp, mod, nao);
  abort();
}

static int_ptr_t isqrt_fixnum(int_ptr_t a)
{
  int_ptr_t mask = (int_ptr_t) 1 << (highest_bit(a) / 2);
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
  if (fixnump(anum)) {
    cnum a = c_num(anum);
    if (a < 0)
      uw_throw(error_s, lit("sqrt: negative operand"));
    return num_fast(isqrt_fixnum(c_num(anum)));
  } else if (bignump(anum)) {
    val n = make_bignum();
    if (mp_sqrt(mp(anum), mp(n)) != MP_OKAY)
      uw_throw(error_s, lit("sqrt: negative operand"));
    return normalize(n);
  }
  uw_throwf(error_s, lit("sqrt: invalid operand ~s"), anum, nao);
}

val gcd(val anum, val bnum)
{
  val n;

  if (!numberp(anum) || !numberp(bnum))
    goto inval;

  if (fixnump(anum))
    anum = bignum(c_num(anum));

  if (fixnump(bnum))
    bnum = bignum(c_num(bnum));

  n = make_bignum();

  if (mp_gcd(mp(anum), mp(bnum), mp(n)) != MP_OKAY)
    goto inval;

  return n;
inval:
  uw_throwf(error_s, lit("gcd: invalid operands ~s ~s ~s"),
            anum, bnum, nao);
  abort();
}

void arith_init(void)
{
  mp_init(&NUM_MAX_MP);
  mp_set_intptr(&NUM_MAX_MP, NUM_MAX);
}
