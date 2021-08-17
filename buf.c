/* Copyright 2017-2021
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

#include <wchar.h>
#include <limits.h>
#include <float.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <signal.h>
#include <stdio.h>
#include "config.h"
#include "lib.h"
#include "gc.h"
#include "itypes.h"
#include "signal.h"
#include "unwind.h"
#include "eval.h"
#include "stream.h"
#include "arith.h"
#include "utf8.h"
#include "buf.h"

static cnum buf_check_len(val len, val self)
{
  cnum l = c_num(len, self);
  if (l < 0)
    uw_throwf(error_s, lit("~a: negative length ~s specified"),
              self, len, nao);
  return l;
}

static cnum buf_check_alloc_size(val alloc_size, cnum len, val self)
{
  cnum ah = c_num(alloc_size, self);
  if (ah < len)
    uw_throwf(error_s, lit("~a: alloc size size ~s lower than length"),
              self, alloc_size, nao);
  return ah;
}

static cnum buf_check_index(struct buf *b, val index, val self)
{
  cnum ix = c_num(index, self);
  if (ix < 0)
    ix = c_num(plus(b->len, index), self);
  if (ix < 0)
    uw_throwf(error_s, lit("~a: negative byte index ~s specified"),
              self, index, nao);
  return ix;
}

val make_buf(val len, val init_val, val alloc_size)
{
  val self = lit("make-buf");
  cnum blen = buf_check_len(len, self);
  val alloc = if3(null_or_missing_p(alloc_size), len, alloc_size);
  cnum size = buf_check_alloc_size(alloc, blen, self);
  cnum iv = c_u8(default_arg(init_val, zero), self);
  mem_t *data = if3(iv == 0 && size == blen,
                    chk_calloc(size, 1),
                    chk_malloc(size));
  val obj = make_obj();

  obj->b.type = BUF;
  obj->b.data = data;
  obj->b.len = len;
  obj->b.size = num(size);

  if (iv != 0 || size != blen)
    memset(data, convert(unsigned char, iv), blen);

  return obj;
}

val bufp(val object)
{
  return tnil(type(object) == BUF);
}

val init_borrowed_buf(obj_t *obj, val len, mem_t *data)
{
  obj->b.type = BUF;
  obj->b.data = data;
  obj->b.len = len;
  obj->b.size = nil;

  return obj;
}

val make_borrowed_buf(val len, mem_t *data)
{
  return init_borrowed_buf(make_obj(), len, data);
}

val make_duplicate_buf(val len, mem_t *data)
{
  val self = lit("make-duplicate-buf");
  val obj = make_obj();

  obj->b.type = BUF;
  obj->b.data = chk_copy_obj(data, c_num(len, self));
  obj->b.len = len;
  obj->b.size = len;

  return obj;
}

val make_owned_buf(val len, mem_t *data)
{
  val buf = make_borrowed_buf(len, data);
  buf->b.size = len;
  return buf;
}

static struct buf *buf_handle(val buf, val ctx)
{
  if (type(buf) == BUF)
    return coerce(struct buf *, buf);
  uw_throwf(error_s, lit("~a: ~s isn't a buffer"),
            ctx, buf, nao);
}

val copy_buf(val buf)
{
  struct buf *b = buf_handle(buf, lit("copy-buf"));
  return if3(b->size,
             make_duplicate_buf(b->len, b->data),
             make_borrowed_buf(b->len, b->data));
}

static void buf_shrink(struct buf *b)
{
  val self = lit("buf-trim");
  val len = b->len;

  if (len == zero)
    len = succ(len); /* avoid reallocing to zero length; i.e. freeing */

  if (len != b->size) {
    b->data = chk_realloc(b->data, c_unum(len, self));
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

static val buf_do_set_len(val buf, struct buf *b, val newlen,
                          val init_val, val self)
{
  val oldlen = b->len;
  cnum olen = c_num(oldlen, self), len = c_num(newlen, self);
  cnum oldsize = c_num(b->size, self), size = oldsize;
  cnum iv = c_u8(default_arg(init_val, zero), self);

  if (!b->size)
    uw_throwf(error_s, lit("~a: ~s is a fixed buffer"),
              self, buf, nao);
  (void) buf_check_len(newlen, self);

  b->len = newlen;

  if (size < len) {
    if (size > INT_PTR_MAX - INT_PTR_MAX / 5) {
      size = INT_PTR_MAX;
    } else {
      size = size + size / 4;
      if (size < len)
        size = len;
    }
  }

  if (size > oldsize) {
    b->data = chk_realloc(b->data, size);
    b->size = num(size);
  }

  if (len > olen)
    memset(b->data + olen, convert(unsigned char, iv), len - olen);

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
  val self = lit("length-buf");
  struct buf *b = buf_handle(buf, self);
  return b->len;
}

val buf_alloc_size(val buf)
{
  val self = lit("buf-alloc-size");
  struct buf *b = buf_handle(buf, self);
  return b->size;
}

mem_t *buf_get(val buf, val self)
{
  struct buf *b = buf_handle(buf, self);
  return b->data;
}

val sub_buf(val buf, val from, val to)
{
  val self = lit("sub-buf");
  struct buf *b = buf_handle(buf, lit("sub"));
  val len = b->len;

  if (null_or_missing_p(from))
    from = zero;
  else if (from == t)
    from = len;
  else if (minusp(from)) {
    from = plus(from, len);
    if (to == zero)
      to = len;
  }

  if (null_or_missing_p(to) || to == t)
    to = len;
  else if (minusp(to))
    to = plus(to, len);

  from = max2(zero, min2(from, len));
  to = max2(zero, min2(to, len));

  if (ge(from, to)) {
    return make_buf(zero, nil, zero);
  } else if (from == 0 && to == len) {
    return buf;
  } else {
    return make_duplicate_buf(minus(to, from), b->data + c_num(from, self));
  }
}

val replace_buf(val buf, val items, val from, val to)
{
  val self = lit("replace");
  val len = length_buf(buf);

  if (missingp(from)) {
    from = zero;
  } else if (from == t) {
    from = len;
  } else if (!integerp(from)) {
    seq_iter_t wh_iter, item_iter;
    val wh, item;
    seq_iter_init(self, &wh_iter, from);
    seq_iter_init(self, &item_iter, items);

    if (!missingp(to))
      uw_throwf(error_s,
                lit("~a: to-arg not applicable when from-arg is a list"),
                self, nao);

    while (seq_get(&wh_iter, &wh) && seq_get(&item_iter, &item)) {
      if (ge(wh, len))
        break;
      buf_put_uchar(buf, wh, item);
    }

    return buf;
  } else if (minusp(from)) {
    from = plus(from, len);
    if (to == zero)
      to = len;
  }

  if (null_or_missing_p(to) || to == t)
    to = len;
  else if (minusp(to))
    to = plus(to, len);

  from = max2(zero, min2(from, len));
  to = max2(zero, min2(to, len));

  {
    val len_rep = minus(to, from);
    val len_it = length(items);

    if (gt(len_rep, len_it)) {
      val len_diff = minus(len_rep, len_it);
      cnum t = c_num(to, self);
      cnum l = c_num(len, self);

      memmove(buf->b.data + t - c_num(len_diff, self),
              buf->b.data + t,
              l - t);

      buf_set_length(buf, minus(len, len_diff), zero);
      to = plus(from, len_it);
    } else if (lt(len_rep, len_it)) {
      val len_diff = minus(len_it, len_rep);
      cnum t = c_num(to, self);
      cnum l = c_num(len, self);

      buf_set_length(buf, plus(len, len_diff), zero);

      memmove(buf->b.data + t + c_num(len_diff, self),
              buf->b.data + t,
              l - t);
      to = plus(from, len_it);
    }

    if (zerop(len_it))
      return buf;
    if (bufp(items)) {
      memmove(buf->b.data + c_num(from, self), items->b.data, c_num(len_it, self));
    } else {
      seq_iter_t item_iter;
      seq_iter_init(self, &item_iter, items);
      cnum f = c_num(from, self);
      cnum t = c_num(to, self);

      for (; f != t; f++) {
        val item = seq_geti(&item_iter);
        buf_put_uchar(buf, num(f), item);
      }
    }
  }

  return buf;
}

val buf_list(val list)
{
  val self = lit("buf-list");;
  val len = length(list);
  val buf = make_buf(zero, zero, len);
  seq_iter_t iter;
  val elem;
  cnum i;

  for (i = 0, seq_iter_init(self, &iter, list); seq_get(&iter, &elem); i++)
    buf->b.data[i] = c_uchar(elem, self);

  buf->b.len = len;

  return buf;
}

static void buf_move_bytes(val buf, val pos, mem_t *ptr, cnum size, val self)
{
  struct buf *b = buf_handle(buf, self);
  cnum p = buf_check_index(b, pos, self);
  val req_len = plus(num(p), num(size));
  if (gt(req_len, b->len))
    buf_do_set_len(buf, b, req_len, nil, self);
  memmove(b->data + p, ptr, size);
}

val buf_put_buf(val dbuf, val sbuf, val pos)
{
  val self = lit("buf-put-buf");
  struct buf *sb = buf_handle(sbuf, self);
  buf_move_bytes(dbuf, pos, sb->data, c_num(sb->len, self), self);
  return sbuf;
}

void buf_put_bytes(val buf, val pos, mem_t *ptr, cnum size, val self)
{
  struct buf *b = buf_handle(buf, self);
  cnum p = buf_check_index(b, pos, self);
  val req_len = plus(num(p), num(size));
  if (gt(req_len, b->len))
    buf_do_set_len(buf, b, req_len, nil, self);
  memcpy(b->data + p, ptr, size);
}

#if HAVE_I8
val buf_put_i8(val buf, val pos, val num)
{
  val self = lit("buf-put-i8");
  struct buf *b = buf_handle(buf, self);
  cnum p = buf_check_index(b, pos, self);
  i8_t v = c_i8(num, self);
  if (p >= c_num(b->len, self))
    buf_do_set_len(buf, b, succ(pos), nil, self);
  b->data[p] = v;
  return num;
}

val buf_put_u8(val buf, val pos, val num)
{
  val self = lit("buf-put-u8");
  struct buf *b = buf_handle(buf, self);
  cnum p = buf_check_index(b, pos, self);
  cnum v = c_u8(num, self);
  if (p >= c_num(b->len, self))
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
  cnum p = buf_check_index(b, pos, self);
  char v = c_char(num, self);
  if (p >= c_num(b->len, self))
    buf_do_set_len(buf, b, succ(pos), nil, self);
  b->data[p] = v;
  return num;
}

val buf_put_uchar(val buf, val pos, val num)
{
  val self = lit("buf-put-uchar");
  struct buf *b = buf_handle(buf, self);
  cnum p = buf_check_index(b, pos, self);
  unsigned char v = c_uchar(num, self);
  if (p >= c_num(b->len, self))
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
  double f = c_flo(num, self);
  if (f > FLT_MAX || f < FLT_MIN)
    uw_throwf(error_s, lit("~a: ~s is out of float range"), self, num, nao);

  n = f;

  buf_put_bytes(buf, pos, coerce(mem_t *, &n), sizeof n, self);
  return num;
}

val buf_put_double(val buf, val pos, val num)
{
  val self = lit("buf-put-double");
  double n = c_flo(num, self);
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

void buf_get_bytes(val buf, val pos, mem_t *ptr, cnum size, val self)
{
  struct buf *b = buf_handle(buf, self);
  cnum p = buf_check_index(b, pos, self);
  cnum e = p + size;
  cnum l = c_num(b->len, self);

  if (e > l || e < 0)
    uw_throwf(error_s, lit("~a: attempted read past buffer end"), self, nao);

  memcpy(ptr, b->data + p, size);
}

#if HAVE_I8
val buf_get_i8(val buf, val pos)
{
  val self = lit("buf-get-i8");
  struct buf *b = buf_handle(buf, self);
  cnum p = buf_check_index(b, pos, self);
  if (p >= c_num(b->len, self))
    uw_throwf(error_s, lit("~a: attempted read past buffer end"), self, nao);
  return num_fast(convert(i8_t, b->data[p]));
}

val buf_get_u8(val buf, val pos)
{
  val self = lit("buf-get-u8");
  struct buf *b = buf_handle(buf, self);
  cnum p = buf_check_index(b, pos, self);
  if (p >= c_num(b->len, self))
    uw_throwf(error_s, lit("~a: attempted read past buffer end"), self, nao);
  return num_fast(convert(u8_t, b->data[p]));
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
  val self = lit("buf-print");
  val stream = default_arg(stream_in, std_output);
  struct buf *b = buf_handle(buf, self);
  cnum len = c_num(b->len, self), count = 0;
  mem_t *data = b->data;
  val save_mode = test_neq_set_indent_mode(stream, num_fast(indent_foff),
                                           num_fast(indent_data));
  val save_indent;
  int fb = 0;

  put_string(lit("#b'"), stream);

  save_indent = inc_indent(stream, zero);

  while (len-- > 0) {
    format(stream, lit("~,02x"), num_fast(*data++), nao);
    if ((++count & 7) == 0 && len)
      if (width_check(stream, chr(' ')))
        fb = 1;
  }

  set_indent(stream, save_indent);
  set_indent_mode(stream, save_mode);

  if (fb)
    force_break(stream);

  return put_char(chr('\''), stream);
}

val buf_pprint(val buf, val stream_in)
{
  val self = lit("buf-pprint");
  val stream = default_arg(stream_in, std_output);
  struct buf *b = buf_handle(buf, self);
  cnum len = c_num(b->len, self);
  mem_t *data = b->data;

  while (len-- > 0)
    put_byte(num_fast(*data++), stream);

  return t;
}

void buf_hex(val buf, char *hex, size_t sz, int caps)
{
  val self = lit("buf-hex");
  struct buf *b = buf_handle(buf, self);
  size_t i;
  unsigned char *data = b->data;
  const char *fmt = caps ? "%02X" : "%02x";

  *hex = 0;

  for (i = 0; i < sz - 1; i += 2)
    hex += sprintf(hex, fmt, *data++);
}

struct buf_strm {
  struct strm_base a;
  utf8_decoder_t ud;
  int is_byte_oriented;
  val buf;
  val pos;
  val unget_c;
};

static void buf_strm_mark(val stream)
{
  struct buf_strm *b = coerce(struct buf_strm *, stream->co.handle);
  strm_base_mark(&b->a);
  gc_mark(b->buf);
  gc_mark(b->pos);
  gc_mark(b->unget_c);
}

static int buf_strm_put_byte_callback(int b, mem_t *ctx)
{
  struct buf_strm *s = coerce(struct buf_strm *, ctx);
  (void) buf_put_uchar(s->buf, s->pos, num_fast(b));
  s->pos = succ(s->pos);
  return 1;
}

static val buf_strm_put_string(val stream, val str)
{
  val self = lit("put-string");
  struct buf_strm *s = coerce(struct buf_strm *, stream->co.handle);
  const wchar_t *p = c_str(str, self);

  while (*p) {
    (void) utf8_encode(*p++, buf_strm_put_byte_callback, coerce(mem_t *, s));
  }

  return t;
}

static val buf_strm_put_char(val stream, val ch)
{
  struct buf_strm *s = coerce(struct buf_strm *, stream->co.handle);
  (void) utf8_encode(c_chr(ch), buf_strm_put_byte_callback, coerce(mem_t *, s));
  return t;
}

static val buf_strm_put_byte(val stream, int b)
{
  struct buf_strm *s = coerce(struct buf_strm *, stream->co.handle);
  (void) buf_strm_put_byte_callback(b, coerce(mem_t *, s));
  return t;
}


static int buf_strm_get_byte_callback(mem_t *ctx)
{
  val self = lit("get-byte");
  struct buf_strm *s = coerce(struct buf_strm *, ctx);
  struct buf *b = buf_handle(s->buf, self);
  cnum p = buf_check_index(b, s->pos, self);
  s->pos = num(p + 1);
  return (p >= c_num(b->len, self)) ? EOF : b->data[p];
}

static val buf_strm_get_char(val stream)
{
  struct buf_strm *s = coerce(struct buf_strm *, stream->co.handle);

  if (s->unget_c) {
    return rcyc_pop(&s->unget_c);
  } else {
    wint_t ch;

    if (s->is_byte_oriented) {
      ch = buf_strm_get_byte_callback(coerce(mem_t *, s));
      if (ch == 0)
        ch = 0xDC00;
    } else {
      ch = utf8_decode(&s->ud, buf_strm_get_byte_callback,
                       coerce(mem_t *, s));
    }

    return (ch != WEOF) ? chr(ch) : nil;
  }
}

static val buf_strm_get_byte(val stream)
{
  struct buf_strm *s = coerce(struct buf_strm *, stream->co.handle);
  int byte = buf_strm_get_byte_callback(coerce(mem_t *, s));
  return byte == EOF ? nil : num_fast(byte);
}

static val buf_strm_unget_char(val stream, val ch)
{
  struct buf_strm *s = coerce(struct buf_strm *, stream->co.handle);
  mpush(ch, mkloc(s->unget_c, stream));
  return ch;
}

static val buf_strm_unget_byte(val stream, int byte)
{
  val self = lit("unget-byte");
  struct buf_strm *s = coerce(struct buf_strm *, stream->co.handle);
  struct buf *b = buf_handle(s->buf, self);
  cnum p = c_num(s->pos, self);

  if (p <= 0) {
    uw_throwf(file_error_s,
              lit("~a: cannot push back past start of stream ~s"),
              self, stream, nao);
  }

  b->data[--p] = byte;
  s->pos = num(p);
  return num_fast(byte);
}

static val buf_strm_seek(val stream, val offset, enum strm_whence whence)
{
  val self = lit("seek-stream");
  struct buf_strm *s = coerce(struct buf_strm *, stream->co.handle);
  struct buf *b = buf_handle(s->buf, self);
  val npos;

  switch (whence) {
  case strm_start:
    npos = offset;
    break;
  case strm_cur:
    if (offset == zero)
      return s->pos;
    npos = plus(s->pos, offset);
    break;
  case strm_end:
    npos = minus(b->len, offset);
    break;
  default:
    internal_error("invalid whence value");
  }

  if (minusp(npos))
    uw_throwf(error_s, lit("~a: cannot seek to negative position ~s"),
              self, npos, nao);

  s->pos = npos;
  return t;
}

static val buf_strm_truncate(val stream, val len)
{
  struct buf_strm *s = coerce(struct buf_strm *, stream->co.handle);
  buf_set_length(s->buf, len, zero);
  return t;
}

static val buf_strm_get_prop(val stream, val ind)
{
  struct buf_strm *s = coerce(struct buf_strm *, stream->co.handle);

  if (ind == name_k) {
    return lit("buf-stream");
  } else if (ind == byte_oriented_k) {
    return tnil(s->is_byte_oriented);
  }

  return nil;
}

static val buf_strm_set_prop(val stream, val ind, val prop)
{
  struct buf_strm *s = coerce(struct buf_strm *, stream->co.handle);

  if (ind == byte_oriented_k) {
    s->is_byte_oriented = prop ? 1 : 0;
    return t;
  }

  return nil;
}


static val buf_strm_get_error(val stream)
{
  val self = lit("get-error");
  struct buf_strm *s = coerce(struct buf_strm *, stream->co.handle);
  struct buf *b = buf_handle(s->buf, self);
  return ge(s->pos, b->len);
}

static val buf_strm_get_error_str(val stream)
{
  return errno_to_string(buf_strm_get_error(stream));
}

static struct strm_ops buf_strm_ops =
  strm_ops_init(cobj_ops_init(eq,
                              stream_print_op,
                              stream_destroy_op,
                              buf_strm_mark,
                              cobj_eq_hash_op),
                wli("buf-stream"),
                buf_strm_put_string,
                buf_strm_put_char,
                buf_strm_put_byte,
                generic_get_line,
                buf_strm_get_char,
                buf_strm_get_byte,
                buf_strm_unget_char,
                buf_strm_unget_byte,
                0,
                0,
                0,
                0,
                buf_strm_seek,
                buf_strm_truncate,
                buf_strm_get_prop,
                buf_strm_set_prop,
                buf_strm_get_error,
                buf_strm_get_error_str,
                0,
                0);

static struct buf_strm *buf_strm(val stream, val self)
{
  struct buf_strm *s = coerce(struct buf_strm *,
                              cobj_handle(self, stream, stream_cls));

  type_assert (stream->co.ops == &buf_strm_ops.cobj_ops,
               (lit("~a: ~a is not a buffer stream"), self, stream, nao));
  return s;
}

val make_buf_stream(val buf_opt)
{
  val stream;
  val buf = default_arg(buf_opt, make_buf(zero, zero, num_fast(64)));
  struct buf_strm *s = coerce(struct buf_strm *, chk_malloc(sizeof *s));

  strm_base_init(&s->a);
  utf8_decoder_init(&s->ud);
  s->buf = nil;
  s->pos = zero;
  s->is_byte_oriented = 0;
  s->unget_c = nil;
  stream = cobj(coerce(mem_t *, s), stream_cls, &buf_strm_ops.cobj_ops);
  s->buf = buf;

  return stream;
}

val get_buf_from_stream(val stream)
{
  val self = lit("get-buf-from-stream");
  struct buf_strm *s = buf_strm(stream, self);
  return s->buf;
}

void buf_swap32(val buf)
{
  val self = lit("buf-swap32");
  struct buf *b = buf_handle(buf, self);
  mem_t *data = b->data, *end = data + c_num(b->len, self);

  for (; data + 3 < end; data += 4) {
    u32_t sw32 = *coerce(u32_t *, data);
    sw32 = ((sw32 & 0xFF00FF00U) >>  8) | ((sw32 & 0x00FF00FFU) <<  8);
    sw32 = ((sw32 & 0xFFFF0000U) >> 16) | ((sw32 & 0x0000FFFFU) << 16);
    *coerce(u32_t *, data) = sw32;
  }
}

static val buf_str(val str, val null_term)
{
  val self = lit("buf-str");
  size_t sz;
  val nt = default_null_arg(null_term);
  unsigned char *u8 = utf8_dup_to_buf(c_str(str, self), &sz, nt != nil);
  return make_owned_buf(unum(sz), u8);
}

static val str_buf(val buf, val null_term)
{
  val self = lit("str-buf");
  struct buf *b = buf_handle(buf, self);
  val nt = default_null_arg(null_term);
  size_t blen = c_unum(b->len, self);
  size_t len = (nt && blen > 0 && !b->data[blen-1]) ? blen - 1 : blen;
  wchar_t *str = utf8_dup_from_buf(coerce(const char *, b->data), len);
  return string_own(str);
}

static val buf_int(val num)
{
  val self = lit("buf-int");

  switch (type(num)) {
  case NUM: case CHR:
    num = bignum(c_num(num, self));
    /* fallthrough */
  case BGNUM:
    {
      val wi = width(num);
      val bits = succ(wi);
      val bytes = ash(plus(bits, num_fast(7)), num_fast(-3));
      val bitsround = ash(bytes, num_fast(3));
      val un = logtrunc(num, bitsround);
      val ube = if3(bignump(un), un, bignum(c_num(un, self)));
      mp_int *m = mp(ube);
      size_t numsize = mp_unsigned_bin_size(m);
      size_t bufsize = c_unum(bytes, self);
      mem_t *data = chk_malloc(bufsize);
      data[0] = 0;
      mp_to_unsigned_bin(m, data + (bufsize - numsize));
      return make_owned_buf(unum(bufsize), data);
    }
  default:
    uw_throwf(type_error_s, lit("~a: ~s isn't an integer or character"),
              self, num, nao);
  }
}

static val buf_uint(val num)
{
  val self = lit("buf-uint");

  switch (type(num)) {
  case NUM: case CHR:
    num = bignum(c_num(num, self));
    /* fallthrough */
  case BGNUM:
    {
      mp_int *m = mp(num);
      if (!mp_isneg(m)) {
        size_t size = mp_unsigned_bin_size(m);
        mem_t *data = chk_malloc(size);
        mp_to_unsigned_bin(m, data);
        return make_owned_buf(unum(size), data);
      }
    }
    uw_throwf(type_error_s, lit("~a: ~s isn't a non-negative integer"),
              self, num, nao);
  default:
    uw_throwf(type_error_s, lit("~a: ~s isn't an integer or character"),
              self, num, nao);
  }
}

static val int_buf(val buf)
{
  val self = lit("int-buf");
  struct buf *b = buf_handle(buf, self);
  ucnum size = c_unum(b->len, self);
  ucnum bits = size * 8;
  val ubn = make_bignum();
  mp_err mpe = mp_read_unsigned_bin(mp(ubn), b->data, size);
  if (mpe != MP_OKAY)
    do_mp_error(self, mpe);
  return sign_extend(normalize(ubn), unum(bits));
}

static val uint_buf(val buf)
{
  val self = lit("uint-buf");
  struct buf *b = buf_handle(buf, self);
  ucnum size = c_unum(b->len, self);
  val ubn = make_bignum();
  mp_err mpe = mp_read_unsigned_bin(mp(ubn), b->data, size);
  if (mpe != MP_OKAY)
    do_mp_error(self, mpe);
  return normalize(ubn);
}

unsigned char *utf8_dup_to_buf(const wchar_t *, size_t *pnbytes,
                               int null_term);
void buf_init(void)
{
  reg_fun(intern(lit("make-buf"), user_package), func_n3o(make_buf, 1));
  reg_fun(intern(lit("bufp"), user_package), func_n1(bufp));
  reg_fun(intern(lit("buf-trim"), user_package), func_n1(buf_trim));
  reg_fun(intern(lit("buf-set-length"), user_package), func_n3o(buf_set_length, 2));
  reg_fun(intern(lit("length-buf"), user_package), func_n1(length_buf));
  reg_fun(intern(lit("buf-alloc-size"), user_package), func_n1(buf_alloc_size));
  reg_fun(intern(lit("copy-buf"), user_package), func_n1(copy_buf));
  reg_fun(intern(lit("sub-buf"), user_package), func_n3(sub_buf));
  reg_fun(intern(lit("replace-buf"), user_package), func_n4(replace_buf));
  reg_fun(intern(lit("buf-list"), user_package), func_n1(buf_list));
  reg_fun(intern(lit("buf-put-buf"), user_package), func_n3(buf_put_buf));

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

  reg_fun(intern(lit("make-buf-stream"), user_package), func_n1o(make_buf_stream, 0));
  reg_fun(intern(lit("get-buf-from-stream"), user_package), func_n1(get_buf_from_stream));

  reg_fun(intern(lit("buf-str"), user_package), func_n2o(buf_str, 1));
  reg_fun(intern(lit("str-buf"), user_package), func_n2o(str_buf, 1));
  reg_fun(intern(lit("buf-int"), user_package), func_n1(buf_int));
  reg_fun(intern(lit("buf-uint"), user_package), func_n1(buf_uint));
  reg_fun(intern(lit("int-buf"), user_package), func_n1(int_buf));
  reg_fun(intern(lit("uint-buf"), user_package), func_n1(uint_buf));

  fill_stream_ops(&buf_strm_ops);
}
