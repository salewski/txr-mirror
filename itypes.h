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

#if CHAR_BIT == 8
#define HAVE_I8 1
typedef unsigned char u8_t;
typedef signed char i8_t;
#endif

#if CHAR_BIT == 16
#define HAVE_I16 1
typedef unsigned char u16_t;
typedef signed char i16_t;
#elif (SIZEOF_SHORT * CHAR_BIT) == 16
#define HAVE_I16 1
typedef unsigned short u16_t;
typedef short i16_t;
#elif (SIZEOF_INT * CHAR_BIT) == 16
#define HAVE_I16 1
typedef unsigned u16_t;
typedef int i16_t;
#endif

#if CHAR_BIT == 32
#define HAVE_I32 1
typedef unsigned char u32_t;
typedef signed char i32_t;
#elif (SIZEOF_SHORT * CHAR_BIT) == 32
#define HAVE_I32 1
typedef unsigned short u32_t;
typedef short i32_t;
#elif (SIZEOF_INT * CHAR_BIT) == 32
#define HAVE_I32 1
typedef unsigned u32_t;
typedef int i32_t;
#elif (SIZEOF_LONG * CHAR_BIT) == 32
#define HAVE_I32 1
typedef unsigned long u32_t;
typedef long i32_t;
#endif

#if (SIZEOF_INT * CHAR_BIT) == 64
#define HAVE_I64 1
typedef unsigned u64_t;
typedef int i64_t;
#elif (SIZEOF_LONG * CHAR_BIT) == 64
#define HAVE_I64 1
typedef unsigned long u64_t;
typedef long i64_t;
#elif HAVE_ULONGLONG_T && (SIZEOF_LONGLONG_T * CHAR_BIT) == 64
#define HAVE_I64 1
typedef ulonglong_t u64_t;
typedef longlong_t i64_t;
#endif

#if HAVE_I8
i8_t c_i8(val, val self);
u8_t c_u8(val, val self);
#endif

#if HAVE_I16
i16_t c_i16(val, val self);
u16_t c_u16(val, val self);
#endif

#if HAVE_I32
i32_t c_i32(val, val self);
u32_t c_u32(val, val self);
#endif

#if HAVE_I64
i64_t c_i64(val, val self);
u64_t c_u64(val, val self);
#endif

char c_char(val, val self);
signed char c_schar(val, val self);
unsigned char c_uchar(val, val self);

short c_short(val, val self);
unsigned short c_ushort(val, val self);

int c_int(val, val self);
unsigned int c_uint(val, val self);

long c_long(val, val self);
unsigned long c_ulong(val, val self);

extern int itypes_little_endian;
void itypes_init(void);
