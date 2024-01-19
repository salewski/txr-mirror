/* Copyright 2017-2024
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

#include <stddef.h>
#include <wchar.h>
#include <signal.h>
#include <stdlib.h>
#include "config.h"
#include "lib.h"
#include "signal.h"
#include "unwind.h"
#include "arith.h"
#include "itypes.h"

#if HAVE_I8
i8_t c_i8(val n, val self)
{
  cnum v = c_num(n, self);
  if (v < -128 || v > 127)
    uw_throwf(error_s, lit("~a: value ~s out of signed 8 bit range"),
              self, n, nao);
  return v;
}

u8_t c_u8(val n, val self)
{
  cnum v = c_num(n, self);
  if (v < 0 || v > 255)
    uw_throwf(error_s, lit("~a: value ~s out of unsigned 8 bit range"),
              self, n, nao);
  return v;
}
#endif

#if HAVE_I16
i16_t c_i16(val n, val self)
{
  cnum v = c_num(n, self);
  if (v < -0x8000 || v > 0x7FFF)
    uw_throwf(error_s, lit("~a: value ~s is out of signed 16 bit range"),
              self, n, nao);
  return v;
}

u16_t c_u16(val n, val self)
{
  cnum v = c_num(n, self);
  if (v < 0 || v > 0xFFFF)
    uw_throwf(error_s, lit("~a: value ~s is out of unsigned 16 bit range"),
              self, n, nao);
  return v;
}
#endif

#if HAVE_I32
i32_t c_i32(val n, val self)
{
  cnum v = c_num(n, self);
  if (v < (-convert(cnum, 0x7FFFFFFF) - 1) || v > 0x7FFFFFFF)
    uw_throwf(error_s, lit("~a: value ~s is out of signed 32 bit range"),
              self, n, nao);
  return v;
}

u32_t c_u32(val n, val self)
{
  uint_ptr_t v = c_unum(n, self);
  if (v > 0xFFFFFFFF)
    uw_throwf(error_s, lit("~a: value ~s is out of unsigned 32 bit range"),
              self, n, nao);
  return v;
}
#endif

#if HAVE_I64

#if CHAR_BIT * SIZEOF_PTR >= 64
i64_t c_i64(val n, val self)
{
  cnum v = c_num(n, self);
  if (v < (- (cnum) 0x7FFFFFFFFFFFFFFF - 1) || v > (cnum) 0x7FFFFFFFFFFFFFFF)
    uw_throwf(error_s, lit("~a: value ~s is out of signed 64 bit range"),
              self, n, nao);
  return v;
}

u64_t c_u64(val n, val self)
{
  ucnum v = c_unum(n, self);
  if (v > (ucnum) 0xFFFFFFFFFFFFFFFF)
    uw_throwf(error_s, lit("~a: value ~s is out of unsigned 64 bit range"),
              self, n, nao);
  return v;
}
#elif HAVE_DOUBLE_INTPTR_T && CHAR_BIT * SIZEOF_DOUBLE_INTPTR >= 64
i64_t c_i64(val n, val self)
{
  dbl_cnum v = c_dbl_num(n);
  if (v < (- convert(dbl_cnum, 0x7FFFFFFFFFFFFFFF) - 1) ||
      v > convert(dbl_cnum, 0x7FFFFFFFFFFFFFFF))
  {
    uw_throwf(error_s, lit("~a: value ~s is out of signed 64 bit range"),
              self, n, nao);
  }
  return v;
}

u64_t c_u64(val n, val self)
{
  dbl_ucnum v = c_dbl_unum(n);
  if (v > convert(dbl_ucnum, 0xFFFFFFFFFFFFFFFF))
  {
    uw_throwf(error_s, lit("~a: value ~s is out of unsigned 64 bit range"),
              self, n, nao);
  }
  return v;
}
#else
i64_t c_i64(val n, val self)
{
  val low32 = logtrunc(n, num_fast(32));
  val high32 = ash(n, num_fast(-32));
  return convert(i64_t, c_i32(high32, self)) << 32 | c_u32(low32, self);
}

u64_t c_u64(val n, val self)
{
  val low32 = logtrunc(n, num_fast(32));
  val high32 = ash(n, num_fast(-32));
  return convert(u64_t, c_u32(high32, self)) << 32 | c_u32(low32, self);
}
#endif

val num_64(i64_t n)
{
#if CHAR_BIT * SIZEOF_PTR >= 64
  return num(n);
#elif HAVE_DOUBLE_INTPTR_T && CHAR_BIT * SIZEOF_DOUBLE_INTPTR >= 64
  return normalize(bignum_dbl_ipt(n));
#else
  cnum hi32 = n >> 32;
  ucnum lo32 = n & 0xFFFFFFFFU;
  return logior(ash(num(hi32), num_fast(32)), unum(lo32));
#endif
}

val unum_64(u64_t n)
{
#if CHAR_BIT * SIZEOF_PTR >= 64
  return unum(n);
#elif HAVE_DOUBLE_INTPTR_T && CHAR_BIT * SIZEOF_DOUBLE_INTPTR >= 64
  return normalize(bignum_dbl_uipt(n));
#else
  ucnum hi32 = n >> 32;
  ucnum lo32 = n & 0xFFFFFFFFU;
  return logior(ash(unum(hi32), num_fast(32)), unum(lo32));
#endif
}

#endif

char c_char(val n, val self)
{
#if CHAR_MAX == UCHAR_MAX
  return c_u8(n, self);
#else
  return c_i8(n, self);
#endif
}

unsigned char c_uchar(val n, val self)
{
  return c_u8(n, self);
}

short c_short(val n, val self)
{
  cnum v = c_num(n, self);
  if (v < SHRT_MIN || v > SHRT_MAX)
    uw_throwf(error_s, lit("~a: value ~s is out of short int range"),
              self, n, nao);
  return v;
}

unsigned short c_ushort(val n, val self)
{
  cnum v = c_num(n, self);
  if (v < 0 || v > USHRT_MAX)
    uw_throwf(error_s, lit("~a: value ~s is out of unsigned short range"),
              self, n, nao);
  return v;
}

int c_int(val n, val self)
{
  cnum v = c_num(n, self);
  if (v < INT_MIN || v > INT_MAX)
    uw_throwf(error_s, lit("~a: value ~s is out of int range"),
              self, n, nao);
  return v;
}

unsigned int c_uint(val n, val self)
{
  uint_ptr_t v = c_unum(n, self);
  if (v > UINT_MAX)
    uw_throwf(error_s, lit("~a: value ~s is out of unsigned int range"),
              self, n, nao);
  return v;
}

long c_long(val n, val self)
{
#if SIZEOF_LONG <= SIZEOF_PTR
  cnum v = c_num(n, self);
  if (v < LONG_MIN || v > LONG_MAX)
    uw_throwf(error_s, lit("~a: value ~s is out of long int range"),
              self, n, nao);
  return v;
#elif SIZEOF_LONG == SIZEOF_PTR && HAVE_I64
  return c_i64(n, self);
#else
#error portme
#endif
}

unsigned long c_ulong(val n, val self)
{
#if SIZEOF_LONG <= SIZEOF_PTR
  uint_ptr_t v = c_unum(n, self);
  if (v > ULONG_MAX)
    uw_throwf(error_s, lit("~a: value ~s is out of unsigned long range"),
              self, n, nao);
  return v;
#elif SIZEOF_LONG == SIZEOF_PTR && HAVE_I64
  return c_u64(n, self);
#else
#error portme
#endif
}

size_t c_size(val n, val self)
{
  switch (sizeof (size_t)) {
  case sizeof (unsigned):
    return c_uint(n, self);
#if SIZEOF_LONG != SIZEOF_INT
  case sizeof (unsigned long):
    return c_ulong(n, self);
#endif
  default:
    abort();
  }
}
