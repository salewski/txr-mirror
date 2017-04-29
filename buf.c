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
#include <limits.h>
#include <float.h>
#include <string.h>
#include <stdlib.h>
#include <signal.h>
#include <stdio.h>
#include <dirent.h>
#include "config.h"
#include "lib.h"
#include "gc.h"
#include "itypes.h"
#include "signal.h"
#include "unwind.h"
#include "eval.h"
#include "stream.h"
#include "arith.h"
#include "buf.h"

static cnum buf_check_len(val len, val self)
{
  cnum l = c_num(len);
  if (l < 0)
    uw_throwf(error_s, lit("~a: negative length ~s specified"),
              self, len, nao);
  return l;
}

static cnum buf_check_alloc_size(val alloc_size, cnum len, val self)
{
  cnum ah = c_num(alloc_size);
  if (ah < len)
    uw_throwf(error_s, lit("~a: alloc size size ~s lower than length"),
              self, alloc_size, nao);
  return ah;
}

static cnum buf_check_index(val index, val self)
{
  cnum ix = c_num(index);
  if (ix < 0)
    uw_throwf(error_s, lit("~a: negative byte index ~s specified"),
              self, index, nao);
  return ix;
}

val make_buf(val len, val init_val, val alloc_size)
{
  val self = lit("make-buf");
  cnum l = buf_check_len(len, self);
  val alloc = if3(null_or_missing_p(alloc_size), len, alloc_size);
  cnum size = buf_check_alloc_size(alloc, l, self);
  cnum iv = c_u8(default_arg(init_val, zero), self);
  mem_t *data = if3(iv == 0,
                    chk_calloc(size, 1),
                    chk_malloc(size));
  val obj = make_obj();

  obj->b.type = BUF;
  obj->b.data = data;
  obj->b.len = len;
  obj->b.size = num(size);

  if (iv != 0)
    memset(data, (unsigned char) iv, c_num(len));

  return obj;
}

val make_borrowed_buf(val len, mem_t *data)
{
  val obj = make_obj();

  obj->b.type = BUF;
  obj->b.data = data;
  obj->b.len = len;
  obj->b.size = nil;

  return obj;
}

val make_duplicate_buf(val len, mem_t *data)
{
  val obj = make_obj();

  obj->b.type = BUF;
  obj->b.data = chk_copy_obj(data, c_num(len));
  obj->b.len = len;
  obj->b.size = nil;

  return obj;
}

static struct buf *buf_handle(val buf, val ctx)
{
  if (type(buf) == BUF)
    return coerce(struct buf *, buf);
  uw_throwf(error_s, lit("~a: ~s isn't a buffer"),
            ctx, buf, nao);
}

static void buf_grow(struct buf *b, val init_val, val self)
{
  cnum len = c_num(b->len);
  cnum oldsize = c_num(b->size), size = oldsize;
  cnum iv = c_u8(default_arg(init_val, zero), self);

  while (size < len) {
    cnum delta = size / 4;
    if (INT_PTR_MAX - delta >= size)
      size += size / 4;
    else
      size = len;
  }

  if (size > oldsize) {
    b->data = chk_realloc(b->data, size);
    b->size = num(size);
    memset(b->data + oldsize, (unsigned char) iv, size - oldsize);
  }
}

static void buf_shrink(struct buf *b)
{
  cnum oldsize = c_num(b->size);
  cnum len = c_num(b->len);

  if (len != oldsize) {
    b->data = chk_realloc(b->data, len);
    b->size = b->len;
  }
}

val buf_trim(val buf)
{
  val self = lit("buf-trim");
  struct buf *b = buf_handle(buf, self);
  val oldsize = b->size;
  if (!oldsize)
    uw_throwf(error_s, lit("~a: ~s is a fixed buffer"),
              self, buf, nao);
  buf_shrink(b);
  return oldsize;
}

static val buf_do_set_len(val buf, struct buf *b, val len,
                          val init_val, val self)
{
  val oldlen = b->len;
  if (!b->size)
    uw_throwf(error_s, lit("~a: ~s is a fixed buffer"),
              self, buf, nao);
  (void) buf_check_len(len, self);
  b->len = len;
  buf_grow(b, init_val, self);
  return oldlen;
}

val buf_set_length(val buf, val len, val init_val)
{
  val self = lit("buf-set-len");
  struct buf *b = buf_handle(buf, self);
  return buf_do_set_len(buf, b, len, init_val, self);
}

val length_buf(val buf)
{
  val self = lit("buf-set-len");
  struct buf *b = buf_handle(buf, self);
  return b->len;
}

mem_t *buf_get(val buf, val self)
{
  struct buf *b = buf_handle(buf, self);
  return b->data;
}

mem_t **buf_addr_of(val buf, val self)
{
  struct buf *b = buf_handle(buf, self);
  return &b->data;
}

void buf_fill(val buf, mem_t *src, val self)
{
  struct buf *b = buf_handle(buf, self);
  memcpy(b->data, src, c_num(b->len));
}

static void buf_put_bytes(val buf, val pos, mem_t *ptr, cnum size, val self)
{
  struct buf *b = buf_handle(buf, self);
  cnum p = buf_check_index(pos, self);
  if (p >= c_num(b->len))
    buf_do_set_len(buf, b, plus(pos, num_fast(size)), nil, self);
  memcpy(b->data + p, ptr, size);
}

#if HAVE_I8
val buf_put_i8(val buf, val pos, val num)
{
  val self = lit("buf-put-i8");
  struct buf *b = buf_handle(buf, self);
  cnum p = buf_check_index(pos, self);
  i8_t v = c_i8(num, self);
  if (p >= c_num(b->len))
    buf_do_set_len(buf, b, succ(pos), nil, self);
  b->data[p] = v;
  return num;
}

val buf_put_u8(val buf, val pos, val num)
{
  val self = lit("buf-put-u8");
  struct buf *b = buf_handle(buf, self);
  cnum p = buf_check_index(pos, self);
  cnum v = c_u8(num, self);
  if (p >= c_num(b->len))
    buf_do_set_len(buf, b, succ(pos), nil, self);
  b->data[p] = v;
  return num;
}
#endif

#if HAVE_I16
val buf_put_i16(val buf, val pos, val num)
{
  val self = lit("buf-put-i16");
  i16_t n = c_i16(num, self);
  buf_put_bytes(buf, pos, coerce(mem_t *, &n), sizeof n, self);
  return num;
}

val buf_put_u16(val buf, val pos, val num)
{
  val self = lit("buf-put-u16");
  u16_t n = c_u16(num, self);
  buf_put_bytes(buf, pos, coerce(mem_t *, &n), sizeof n, self);
  return num;
}
#endif

#if HAVE_I32
val buf_put_i32(val buf, val pos, val num)
{
  val self = lit("buf-put-i32");
  i32_t n = c_i32(num, self);
  buf_put_bytes(buf, pos, coerce(mem_t *, &n), sizeof n, self);
  return num;
}

val buf_put_u32(val buf, val pos, val num)
{
  val self = lit("buf-put-u32");
  u32_t n = c_u32(num, self);
  buf_put_bytes(buf, pos, coerce(mem_t *, &n), sizeof n, self);
  return num;
}
#endif

#if HAVE_I64
val buf_put_i64(val buf, val pos, val num)
{
  val self = lit("buf-put-i64");
  i64_t n = c_i64(num, self);
  buf_put_bytes(buf, pos, coerce(mem_t *, &n), sizeof n, self);
  return num;
}

val buf_put_u64(val buf, val pos, val num)
{
  val self = lit("buf-put-u64");
  u64_t n = c_u64(num, self);
  buf_put_bytes(buf, pos, coerce(mem_t *, &n), sizeof n, self);
  return num;
}
#endif

val buf_put_char(val buf, val pos, val num)
{
  val self = lit("buf-put-char");
  struct buf *b = buf_handle(buf, self);
  cnum p = buf_check_index(pos, self);
  char v = c_char(num, self);
  if (p >= c_num(b->len))
    buf_do_set_len(buf, b, succ(pos), nil, self);
  b->data[p] = v;
  return num;
}

val buf_put_uchar(val buf, val pos, val num)
{
  val self = lit("buf-put-uchar");
  struct buf *b = buf_handle(buf, self);
  cnum p = buf_check_index(pos, self);
  unsigned char v = c_char(num, self);
  if (p >= c_num(b->len))
    buf_do_set_len(buf, b, succ(pos), nil, self);
  b->data[p] = v;
  return num;
}

val buf_put_short(val buf, val pos, val num)
{
  val self = lit("buf-put-short");
  short n = c_short(num, self);
  buf_put_bytes(buf, pos, coerce(mem_t *, &n), sizeof n, self);
  return num;
}

val buf_put_ushort(val buf, val pos, val num)
{
  val self = lit("buf-put-ushort");
  unsigned short n = c_short(num, self);
  buf_put_bytes(buf, pos, coerce(mem_t *, &n), sizeof n, self);
  return num;
}

val buf_put_int(val buf, val pos, val num)
{
  val self = lit("buf-put-int");
  int n = c_int(num, self);
  buf_put_bytes(buf, pos, coerce(mem_t *, &n), sizeof n, self);
  return num;
}

val buf_put_uint(val buf, val pos, val num)
{
  val self = lit("buf-put-uint");
  unsigned n = c_uint(num, self);
  buf_put_bytes(buf, pos, coerce(mem_t *, &n), sizeof n, self);
  return num;
}

val buf_put_long(val buf, val pos, val num)
{
  val self = lit("buf-put-long");
  long n = c_long(num, self);
  buf_put_bytes(buf, pos, coerce(mem_t *, &n), sizeof n, self);
  return num;
}

val buf_put_ulong(val buf, val pos, val num)
{
  val self = lit("buf-put-ulong");
  unsigned long n = c_ulong(num, self);
  buf_put_bytes(buf, pos, coerce(mem_t *, &n), sizeof n, self);
  return num;
}

val buf_put_float(val buf, val pos, val num)
{
  val self = lit("buf-put-float");
  double n;
  double f = c_flo(num);
  if (f > FLT_MAX || f < FLT_MIN)
    uw_throwf(error_s, lit("~a: ~s is out of float range"), self, num, nao);

  n = f;

  buf_put_bytes(buf, pos, coerce(mem_t *, &n), sizeof n, self);
  return num;
}

val buf_put_double(val buf, val pos, val num)
{
  val self = lit("buf-put-double");
  double n = c_flo(num);
  buf_put_bytes(buf, pos, coerce(mem_t *, &n), sizeof n, self);
  return num;
}

val buf_put_cptr(val buf, val pos, val cptr)
{
  val self = lit("buf-put-cptr");
  mem_t *p = cptr_get(cptr);
  buf_put_bytes(buf, pos, coerce(mem_t *, &p), sizeof p, self);
  return cptr;
}

static void buf_get_bytes(val buf, val pos, mem_t *ptr, cnum size, val self)
{
  struct buf *b = buf_handle(buf, self);
  cnum p = buf_check_index(pos, self);
  cnum e = p + size;
  cnum l = c_num(b->len);

  if (e >= l || e < 0)
    uw_throwf(error_s, lit("~a: attempted read past buffer end"), self, nao);

  memcpy(ptr, b->data + p, size);
}

#if HAVE_I8
val buf_get_i8(val buf, val pos)
{
  val self = lit("buf-get-i8");
  struct buf *b = buf_handle(buf, self);
  cnum p = buf_check_index(pos, self);
  if (p >= c_num(b->len))
    uw_throwf(error_s, lit("~a: attempted read past buffer end"), self, nao);
  return num_fast((i8_t) b->data[p]);
}

val buf_get_u8(val buf, val pos)
{
  val self = lit("buf-get-u8");
  struct buf *b = buf_handle(buf, self);
  cnum p = buf_check_index(pos, self);
  if (p >= c_num(b->len))
    uw_throwf(error_s, lit("~a: attempted read past buffer end"), self, nao);
  return num_fast((u8_t) b->data[p]);
}
#endif

#if HAVE_I16
val buf_get_i16(val buf, val pos)
{
  val self = lit("buf-get-i16");
  i16_t n;
  buf_get_bytes(buf, pos, coerce(mem_t *, &n), sizeof n, self);
  return num_fast(n);
}

val buf_get_u16(val buf, val pos)
{
  val self = lit("buf-get-u16");
  u16_t n;
  buf_get_bytes(buf, pos, coerce(mem_t *, &n), sizeof n, self);
  return num_fast(n);
}
#endif

#if HAVE_I32
val buf_get_i32(val buf, val pos)
{
  val self = lit("buf-get-i32");
  i32_t n;
  buf_get_bytes(buf, pos, coerce(mem_t *, &n), sizeof n, self);
  return num(n);
}

val buf_get_u32(val buf, val pos)
{
  val self = lit("buf-get-u32");
  u32_t n;
  buf_get_bytes(buf, pos, coerce(mem_t *, &n), sizeof n, self);
  return unum(n);
}
#endif

#if HAVE_I64
val buf_get_i64(val buf, val pos)
{
  val self = lit("buf-get-i64");
  i64_t n;
  buf_get_bytes(buf, pos, coerce(mem_t *, &n), sizeof n, self);

  if (sizeof (i64_t) <= sizeof (cnum)) {
    return num(n);
  } else {
    val high = num(n >> 32);
    val low = unum(n & 0xFFFFFFFF);
    return logior(ash(high, num_fast(32)), low);
  }
}

val buf_get_u64(val buf, val pos)
{
  val self = lit("buf-get-u64");
  u64_t n;
  buf_get_bytes(buf, pos, coerce(mem_t *, &n), sizeof n, self);

  if (sizeof (u64_t) <= sizeof (uint_ptr_t)) {
    return unum(n);
  } else {
    val high = unum(n >> 32);
    val low = unum(n & 0xFFFFFFFF);
    return logior(ash(high, num_fast(32)), low);
  }
}
#endif

val buf_get_char(val buf, val pos)
{
#if CHAR_MAX == UCHAR_MAX
  return buf_get_u8(buf, pos);
#else
  return buf_get_i8(buf, pos);
#endif
}

val buf_get_uchar(val buf, val pos)
{
  return buf_get_u8(buf, pos);
}

val buf_get_short(val buf, val pos)
{
  val self = lit("buf-get-short");
  short n;
  buf_get_bytes(buf, pos, coerce(mem_t *, &n), sizeof n, self);
#if SIZEOF_SHORT < SIZEOF_PTR
  return num_fast(n);
#else
  return num(n);
#endif
}

val buf_get_ushort(val buf, val pos)
{
  val self = lit("buf-get-ushort");
  unsigned short n;
  buf_get_bytes(buf, pos, coerce(mem_t *, &n), sizeof n, self);
#if SIZEOF_SHORT < SIZEOF_PTR
  return num_fast(n);
#else
  return unum(n);
#endif
}

val buf_get_int(val buf, val pos)
{
  val self = lit("buf-get-int");
  int n;
  buf_get_bytes(buf, pos, coerce(mem_t *, &n), sizeof n, self);
  return num(n);
}

val buf_get_uint(val buf, val pos)
{
  val self = lit("buf-get-uint");
  unsigned n;
  buf_get_bytes(buf, pos, coerce(mem_t *, &n), sizeof n, self);
  return unum(n);
}

val buf_get_long(val buf, val pos)
{
  val self = lit("buf-get-long");
  long n;
  buf_get_bytes(buf, pos, coerce(mem_t *, &n), sizeof n, self);
  return num(n);
}

val buf_get_ulong(val buf, val pos)
{
  val self = lit("buf-get-long");
  unsigned long n;
  buf_get_bytes(buf, pos, coerce(mem_t *, &n), sizeof n, self);
  return unum(n);
}

val buf_get_float(val buf, val pos)
{
  val self = lit("buf-get-float");
  float n;
  buf_get_bytes(buf, pos, coerce(mem_t *, &n), sizeof n, self);
  return flo(n);
}

val buf_get_double(val buf, val pos)
{
  val self = lit("buf-get-double");
  double n;
  buf_get_bytes(buf, pos, coerce(mem_t *, &n), sizeof n, self);
  return flo(n);
}

val buf_get_cptr(val buf, val pos)
{
  val self = lit("buf-get-cptr");
  mem_t *p;
  buf_get_bytes(buf, pos, coerce(mem_t *, &p), sizeof p, self);
  return cptr(p);
}

val buf_print(val buf, val stream_in)
{
  val stream = default_arg(stream_in, std_output);
  struct buf *b = buf_handle(buf, lit("buf-print"));
  cnum len = c_num(b->len), count = 0;
  mem_t *data = b->data;
  val save_mode = test_set_indent_mode(stream, num_fast(indent_off),
                                       num_fast(indent_data));
  val save_indent;

  put_string(lit("#b'"), stream);

  save_indent = inc_indent(stream, zero);

  while (len-- > 0) {
    format(stream, lit("~,02x"), num_fast(*data++), nao);
    if ((++count & 7) == 0 && len)
      width_check(stream, chr(' '));
  }

  set_indent(stream, save_indent);
  set_indent_mode(stream, save_mode);

  return put_char(chr('\''), stream);
}

val buf_pprint(val buf, val stream_in)
{
  val stream = default_arg(stream_in, std_output);
  struct buf *b = buf_handle(buf, lit("buf-print"));
  cnum len = c_num(b->len);
  mem_t *data = b->data;

  while (len-- > 0)
    put_byte(num_fast(*data++), stream);

  return t;
}

void buf_init(void)
{
  reg_fun(intern(lit("make-buf"), user_package), func_n3o(make_buf, 1));
  reg_fun(intern(lit("buf-trim"), user_package), func_n1(buf_trim));
  reg_fun(intern(lit("buf-set-length"), user_package), func_n3o(buf_set_length, 2));
  reg_fun(intern(lit("length-buf"), user_package), func_n1(length_buf));

#if HAVE_I8
  reg_fun(intern(lit("buf-put-i8"), user_package), func_n3(buf_put_i8));
  reg_fun(intern(lit("buf-put-u8"), user_package), func_n3(buf_put_u8));
#endif

#if HAVE_I16
  reg_fun(intern(lit("buf-put-i16"), user_package), func_n3(buf_put_i16));
  reg_fun(intern(lit("buf-put-u16"), user_package), func_n3(buf_put_u16));
#endif

#if HAVE_I32
  reg_fun(intern(lit("buf-put-i32"), user_package), func_n3(buf_put_i32));
  reg_fun(intern(lit("buf-put-u32"), user_package), func_n3(buf_put_u32));
#endif

#if HAVE_I64
  reg_fun(intern(lit("buf-put-i64"), user_package), func_n3(buf_put_i64));
  reg_fun(intern(lit("buf-put-u64"), user_package), func_n3(buf_put_u64));
#endif

  reg_fun(intern(lit("buf-put-char"), user_package), func_n3(buf_put_char));
  reg_fun(intern(lit("buf-put-uchar"), user_package), func_n3(buf_put_uchar));
  reg_fun(intern(lit("buf-put-short"), user_package), func_n3(buf_put_short));
  reg_fun(intern(lit("buf-put-ushort"), user_package), func_n3(buf_put_ushort));
  reg_fun(intern(lit("buf-put-int"), user_package), func_n3(buf_put_int));
  reg_fun(intern(lit("buf-put-uint"), user_package), func_n3(buf_put_uint));
  reg_fun(intern(lit("buf-put-long"), user_package), func_n3(buf_put_long));
  reg_fun(intern(lit("buf-put-ulong"), user_package), func_n3(buf_put_ulong));
  reg_fun(intern(lit("buf-put-float"), user_package), func_n3(buf_put_float));
  reg_fun(intern(lit("buf-put-double"), user_package), func_n3(buf_put_double));
  reg_fun(intern(lit("buf-put-cptr"), user_package), func_n3(buf_put_cptr));

#if HAVE_I8
  reg_fun(intern(lit("buf-get-i8"), user_package), func_n2(buf_get_i8));
  reg_fun(intern(lit("buf-get-u8"), user_package), func_n2(buf_get_u8));
#endif

#if HAVE_I16
  reg_fun(intern(lit("buf-get-i16"), user_package), func_n2(buf_get_i16));
  reg_fun(intern(lit("buf-get-u16"), user_package), func_n2(buf_get_u16));
#endif

#if HAVE_I32
  reg_fun(intern(lit("buf-get-i32"), user_package), func_n2(buf_get_i32));
  reg_fun(intern(lit("buf-get-u32"), user_package), func_n2(buf_get_u32));
#endif

#if HAVE_I64
  reg_fun(intern(lit("buf-get-i64"), user_package), func_n2(buf_get_i64));
  reg_fun(intern(lit("buf-get-u64"), user_package), func_n2(buf_get_u64));
#endif

  reg_fun(intern(lit("buf-get-char"), user_package), func_n2(buf_get_char));
  reg_fun(intern(lit("buf-get-uchar"), user_package), func_n2(buf_get_uchar));
  reg_fun(intern(lit("buf-get-short"), user_package), func_n2(buf_get_short));
  reg_fun(intern(lit("buf-get-ushort"), user_package), func_n2(buf_get_ushort));
  reg_fun(intern(lit("buf-get-int"), user_package), func_n2(buf_get_int));
  reg_fun(intern(lit("buf-get-uint"), user_package), func_n2(buf_get_uint));
  reg_fun(intern(lit("buf-get-long"), user_package), func_n2(buf_get_long));
  reg_fun(intern(lit("buf-get-ulong"), user_package), func_n2(buf_get_ulong));
  reg_fun(intern(lit("buf-get-float"), user_package), func_n2(buf_get_float));
  reg_fun(intern(lit("buf-get-double"), user_package), func_n2(buf_get_double));
  reg_fun(intern(lit("buf-get-cptr"), user_package), func_n2(buf_get_cptr));
}
