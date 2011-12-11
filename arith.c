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
  switch (mp_cmp_mag(mp(bignum), &NUM_MAX_MP)) {
  case MP_EQ:
  case MP_GT:
    return bignum;
  default:
    {
      cnum fixnum;
      mp_get_intptr(mp(bignum), &fixnum);
      return num(fixnum);
    }
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
        mp_add_d(mp(bnum), c_num(anum), mp(n));
        NOOP(mp(n), mp(n));
      } else {
        mp_int tmp;
        mp_init(&tmp);
        mp_set_intptr(&tmp, c_num(anum));
        mp_add(mp(bnum), &tmp, mp(n));
      }
      return normalize(n);
    }
  case TAG_PAIR(TAG_PTR, TAG_NUM):
    {
        val n;
        type_check(anum, BGNUM);
        n = make_bignum();
        if (sizeof (int_ptr_t) <= sizeof (mp_digit))  {
          mp_add_d(mp(anum), c_num(bnum), mp(n));
        } else {
          mp_int tmp;
          mp_init(&tmp);
          mp_set_intptr(&tmp, c_num(bnum));
          mp_add(mp(anum), &tmp, mp(n));
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
  }
  uw_throwf(error_s, lit("plus: invalid operands ~s ~s"), anum, bnum, nao);
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
        mp_sub_d(mp(bnum), c_num(anum), mp(n));
        mp_neg(mp(n), mp(n));
      } else {
        mp_int tmp;
        mp_init(&tmp);
        mp_set_intptr(&tmp, c_num(anum));
        mp_sub(mp(bnum), &tmp, mp(n));
      }
      return normalize(n);
    }
  case TAG_PAIR(TAG_PTR, TAG_NUM):
    {
        val n;
        type_check(anum, BGNUM);
        n = make_bignum();
        if (sizeof (int_ptr_t) <= sizeof (mp_digit))  {
          mp_sub_d(mp(anum), c_num(bnum), mp(n));
        } else {
          mp_int tmp;
          mp_init(&tmp);
          mp_set_intptr(&tmp, c_num(bnum));
          mp_sub(mp(anum), &tmp, mp(n));
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
      cnum ap = (a < 0) ? -a : a;
      cnum bp = (b < 0) ? -b : b;
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
        mp_mul_d(mp(bnum), c_num(anum), mp(n));
      } else {
        mp_int tmp;
        mp_init(&tmp);
        mp_set_intptr(&tmp, c_num(anum));
        mp_mul(mp(bnum), &tmp, mp(n));
      }
      return n;
    }
  case TAG_PAIR(TAG_PTR, TAG_NUM):
    {
      val n;
      type_check(bnum, BGNUM);
      n = make_bignum();
      if (sizeof (int_ptr_t) <= sizeof (mp_digit)) {
        mp_mul_d(mp(anum), c_num(bnum), mp(n));
      } else {
        mp_int tmp;
        mp_init(&tmp);
        mp_set_intptr(&tmp, c_num(bnum));
        mp_mul(mp(anum), &tmp, mp(n));
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

void arith_init(void)
{
  mp_init(&NUM_MAX_MP);
  mp_set_intptr(&NUM_MAX_MP, NUM_MAX);
}
