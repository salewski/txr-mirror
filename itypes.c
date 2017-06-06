/* Copyright 2017
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

#include <stddef.h>
#include <wchar.h>
#include <signal.h>
#include "config.h"
#include "lib.h"
#include "signal.h"
#include "unwind.h"
#include "arith.h"
#include "itypes.h"

int itypes_little_endian;

#if HAVE_I8
i8_t c_i8(val n, val self)
{
  cnum v = c_num(n);
  if (v < -128 || v > 127)
    uw_throwf(error_s, lit("~a: value ~s out of signed 8 bit range"),
              self, n, nao);
  return v;
}

u8_t c_u8(val n, val self)
{
  cnum v = c_num(n);
  if (v < 0 || v > 255)
    uw_throwf(error_s, lit("~a: value ~s out of unsigned 8 bit range"),
              self, n, nao);
  return v;
}
#endif

#if HAVE_I16
i16_t c_i16(val n, val self)
{
  cnum v = c_num(n);
  if (v < -0x8000 || v > 0x7FFF)
    uw_throwf(error_s, lit("~a: value ~s is out of signed 16 bit range"),
              self, n, nao);
  return v;
}

u16_t c_u16(val n, val self)
{
  cnum v = c_num(n);
  if (v < 0 || v > 0xFFFF)
    uw_throwf(error_s, lit("~a: value ~s is out of unsigned 16 bit range"),
              self, n, nao);
  return v;
}
#endif

#if HAVE_I32
i32_t c_i32(val n, val self)
{
  cnum v = c_num(n);
  if (v < (-convert(cnum, 0x7FFFFFFF) - 1) || v > 0x7FFFFFFF)
    uw_throwf(error_s, lit("~a: value ~s is out of signed 32 bit range"),
              self, n, nao);
  return v;
}

u32_t c_u32(val n, val self)
{
  uint_ptr_t v = c_unum(n);
  if (v < 0 || v > 0xFFFFFFFF)
    uw_throwf(error_s, lit("~a: value ~s is out of unsigned 32 bit range"),
              self, n, nao);
  return v;
}
#endif

#if HAVE_I64
#if SIZEOF_PTR == 8
i64_t c_i64(val n, val self)
{
  cnum v = c_num(n);
  if (v < (- (cnum) 0x7FFFFFFFFFFFFFFF - 1) || v > (cnum) 0x7FFFFFFFFFFFFFFF)
    uw_throwf(error_s, lit("~a: value ~s is out of signed 64 bit range"),
              self, n, nao);
  return v;
}

u64_t c_u64(val n, val self)
{
  ucnum v = c_unum(n);
  if (v > (ucnum) 0xFFFFFFFFFFFFFFFF)
    uw_throwf(error_s, lit("~a: value ~s is out of unsigned 64 bit range"),
              self, n, nao);
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
#endif

char c_char(val n, val self)
{
#if CHAR_MAX == UCHAR_MAX
  return c_u8(n, self);
#else
  return c_i8(n, self);
#endif
}

signed char c_schar(val n, val self)
{
  return c_i8(n, self);
}

unsigned char c_uchar(val n, val self)
{
  return c_u8(n, self);
}

short c_short(val n, val self)
{
  cnum v = c_num(n);
  if (v < SHRT_MIN || v > SHRT_MAX)
    uw_throwf(error_s, lit("~a: value ~s is out of short int range"),
              self, n, nao);
  return v;
}

unsigned short c_ushort(val n, val self)
{
  cnum v = c_num(n);
  if (v < 0 || v > USHRT_MAX)
    uw_throwf(error_s, lit("~a: value ~s is out of unsigned short range"),
              self, n, nao);
  return v;
}

int c_int(val n, val self)
{
  cnum v = c_num(n);
  if (v < INT_MIN || v > INT_MAX)
    uw_throwf(error_s, lit("~a: value ~s is out of int range"),
              self, n, nao);
  return v;
}

unsigned int c_uint(val n, val self)
{
  uint_ptr_t v = c_unum(n);
  if (v < 0 || v > UINT_MAX)
    uw_throwf(error_s, lit("~a: value ~s is out of unsigned int range"),
              self, n, nao);
  return v;
}

long c_long(val n, val self)
{
#if SIZEOF_LONG <= SIZEOF_PTR
  cnum v = c_num(n);
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
  uint_ptr_t v = c_unum(n);
  if (v < 0 || v > ULONG_MAX)
    uw_throwf(error_s, lit("~a: value ~s is out of unsigned long range"),
              self, n, nao);
  return v;
#elif SIZEOF_LONG == SIZEOF_PTR && HAVE_I64
  return c_u64(n, self);
#else
#error portme
#endif
}

extern int itypes_little_endian;
void itypes_init(void);

void itypes_init()
{
  union u {
    volatile unsigned ui;
    volatile unsigned char uc[sizeof (unsigned)];
  } u = { 0xff };

  itypes_little_endian = (u.uc[0] = 0xff);
}
