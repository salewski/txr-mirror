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
      int bit = CNUM_BIT - 3, amaxbit = 0, bmaxbit = 0;
      cnum mask = (cnum) 1 << (CNUM_BIT - 4);
      for (; mask && (ap || bp); mask >>= 1, bit--) {
        if ((ap & mask)) {
          amaxbit = bit;
          ap = 0;
        }
        if ((bp & mask)) {
          bmaxbit = bit;
          bp = 0;
        }
      }
      if (amaxbit + bmaxbit < CNUM_BIT - 1) {
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
