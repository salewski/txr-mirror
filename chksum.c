/* Copyright 2019-2021
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
 * 2. Redistributions in binary form must reproduce the above copyright notice, *    this list of conditions and the following disclaimer in the documentation *    and/or other materials provided with the distribution.
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
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <signal.h>
#include <stdio.h>
#include "config.h"
#include "lib.h"
#include "itypes.h"
#include "signal.h"
#include "unwind.h"
#include "eval.h"
#include "stream.h"
#include "utf8.h"
#include "buf.h"
#include "chksums/sha256.h"
#include "chksums/crc32.h"
#include "chksums/md5.h"
#include "chksum.h"

static val sha256_ctx_s, md5_ctx_s;
static struct cobj_class *sha256_ctx_cls, *md5_ctx_cls;

static void sha256_stream_impl(val stream, val nbytes, unsigned char *hash,
                               val self)
{
  SHA256_t s256;
  val buf = iobuf_get();
  val bfsz = length_buf(buf);
  SHA256_init(&s256);

  if (null_or_missing_p(nbytes)) {
    for (;;) {
      val read = fill_buf(buf, zero, stream);
      cnum rd = c_num(read, self);

      if (!rd)
        break;

      SHA256_update(&s256, buf->b.data, rd);
    }
  } else {
    while (ge(nbytes, bfsz)) {
      val read = fill_buf(buf, zero, stream);
      cnum rd = c_num(read, self);

      if (zerop(read))
        break;

      SHA256_update(&s256, buf->b.data, rd);
      nbytes = minus(nbytes, read);
    }

    buf_set_length(buf, nbytes, nil);

    {
      val read = fill_buf(buf, zero, stream);
      cnum rd = c_num(read, self);
      if (rd)
        SHA256_update(&s256, buf->b.data, rd);
    }
  }

  SHA256_final(&s256, hash);
  iobuf_put(buf);
}

static val chksum_ensure_buf(val self, val buf_in,
                             val len, unsigned char **phash,
                             val hash_name)
{
  if (null_or_missing_p(buf_in)) {
    *phash = chk_malloc(c_unum(len, self));
    return make_owned_buf(len, *phash);
  } else {
    *phash = buf_get(buf_in, self);
    if (lt(length_buf(buf_in), len))
      uw_throwf(error_s, lit("~s: buffer ~s too small for ~a hash"),
                self, buf_in, hash_name, nao);
    return buf_in;
  }
}

val sha256_stream(val stream, val nbytes, val buf_in)
{
  val self = lit("sha256-stream");
  unsigned char *hash;
  val buf = chksum_ensure_buf(self, buf_in, num_fast(SHA256_DIGEST_LENGTH),
                              &hash, lit("SHA-256"));
  sha256_stream_impl(stream, nbytes, hash, self);
  return buf;
}

static void sha256_szmax_upd(SHA256_t *ps256, mem_t *data, ucnum len)
{
  const size_t szmax = convert(size_t, -1) / 4 + 1;
  while (len >= szmax) {
    SHA256_update(ps256, data, szmax);
    data += szmax;
    len -= szmax;
  }
  if (len > 0)
    SHA256_update(ps256, data, len);
}

static void sha256_buf(val buf, unsigned char *hash, val self)
{
  SHA256_t s256;
  SHA256_init(&s256);
  sha256_szmax_upd(&s256, buf->b.data, c_unum(buf->b.len, self));
  SHA256_final(&s256, hash);
}

static void sha256_str(val str, unsigned char *hash, val self)
{
  char *s = utf8_dup_to(c_str(str, self));
  SHA256_t s256;
  SHA256_init(&s256);
  SHA256_update(&s256, coerce(const unsigned char *, s), strlen(s));
  free(s);
  SHA256_final(&s256, hash);
}

val sha256(val obj, val buf_in)
{
  val self = lit("sha256");
  unsigned char *hash;
  val buf = chksum_ensure_buf(self, buf_in, num_fast(SHA256_DIGEST_LENGTH),
                              &hash, lit("SHA-256"));

  switch (type(obj)) {
  case STR:
  case LSTR:
  case LIT:
    sha256_str(obj, hash, self);
    return buf;
  case BUF:
    sha256_buf(obj, hash, self);
    return buf;
  default:
    uw_throwf(error_s, lit("~a: cannot hash ~s, only buffer and strings"),
              self, obj, nao);
  }
}

static struct cobj_ops sha256_ops = cobj_ops_init(cobj_equal_handle_op,
                                                  cobj_print_op,
                                                  cobj_destroy_free_op,
                                                  cobj_mark_op,
                                                  cobj_handle_hash_op);
val sha256_begin(void)
{
  SHA256_t *ps256 = coerce(SHA256_t *, chk_malloc(sizeof *ps256));
  SHA256_init(ps256);
  return cobj(coerce(mem_t *, ps256), sha256_ctx_cls, &sha256_ops);
}

static int sha256_utf8_byte_callback(int b, mem_t *ctx)
{
  SHA256_t *ps256 = coerce(SHA256_t *, ctx);
  unsigned char uc = b;
  SHA256_update(ps256, &uc, 1);
  return 1;
}

val sha256_hash(val ctx, val obj)
{
  val self = lit("sha256-hash");
  SHA256_t *ps256 = coerce(SHA256_t *, cobj_handle(self, ctx, sha256_ctx_cls));

  switch (type(obj)) {
  case STR:
  case LSTR:
  case LIT:
    {
      char *str = utf8_dup_to(c_str(obj, self));
      SHA256_update(ps256, coerce(const unsigned char *, str), strlen(str));
      free(str);
    }
    break;
  case BUF:
    sha256_szmax_upd(ps256, obj->b.data, c_unum(obj->b.len, self));
    break;
  case CHR:
    utf8_encode(c_chr(obj), sha256_utf8_byte_callback, coerce(mem_t *, ps256));
    break;
  case NUM:
    {
      cnum n = c_num(obj, self);
      unsigned char uc = n;
      if (n < 0 || n > 255)
        uw_throwf(error_s, lit("~a: byte value ~s out of range"),
                  self, obj, nao);
      SHA256_update(ps256, &uc, 1);
    }
    break;
  default:
    uw_throwf(error_s, lit("~a: cannot hash ~s, only buffer and strings"),
              self, obj, nao);
  }

  return obj;
}

val sha256_end(val ctx, val buf_in)
{
  val self = lit("sha256-end");
  unsigned char *hash;
  SHA256_t *ps256 = coerce(SHA256_t *, cobj_handle(self, ctx, sha256_ctx_cls));
  val buf = chksum_ensure_buf(self, buf_in, num_fast(SHA256_DIGEST_LENGTH),
                              &hash, lit("SHA-256"));

  SHA256_final(ps256, hash);
  SHA256_init(ps256);
  return buf;
}

val crc32_stream(val stream, val nbytes, val init)
{
  val self = lit("crc32-stream");
  val buf = iobuf_get();
  val bfsz = length_buf(buf);
  u32_t crc = if3(missingp(init), 0, c_u32(init, self));

  if (null_or_missing_p(nbytes)) {
    for (;;) {
      val read = fill_buf(buf, zero, stream);
      cnum rd = c_num(read, self);

      if (!rd)
        break;

      crc = crc32_cont(buf->b.data, rd, crc);
    }
  } else {
    while (ge(nbytes, bfsz)) {
      val read = fill_buf(buf, zero, stream);
      cnum rd = c_num(read, self);

      if (zerop(read))
        break;

      crc = crc32_cont(buf->b.data, rd, crc);
      nbytes = minus(nbytes, read);
    }

    buf_set_length(buf, nbytes, nil);

    {
      val read = fill_buf(buf, zero, stream);
      cnum rd = c_num(read, self);
      if (rd)
        crc = crc32_cont(buf->b.data, rd, crc);
    }
  }

  iobuf_put(buf);
  return unum(crc);
}

static val crc32_buf(val buf, val init, val self)
{
  ucnum len = c_unum(buf->b.len, self);
  mem_t *data = buf->b.data;
  const size_t szmax = convert(size_t, -1) / 4 + 1;
  u32_t crc = if3(missingp(init), 0, c_u32(init, self));

  while (len >= szmax) {
    crc = crc32_cont(data, szmax, crc);
    data += szmax;
    len -= szmax;
  }

  if (len > 0)
    crc = crc32_cont(data, len, crc);

  return unum(crc);
}

static val crc32_str(val str, val init)
{
  val s = make_byte_input_stream(str);
  return crc32_stream(s, nil, init);
}


val crc32(val obj, val init)
{
  val self = lit("sha256");

  switch (type(obj)) {
  case STR:
  case LSTR:
  case LIT:
    return crc32_str(obj, init);
  case BUF:
    return crc32_buf(obj, init, self);
  default:
    uw_throwf(error_s, lit("~a: cannot hash ~s, only buffer and strings"),
              self, obj, nao);
  }
}

static void md5_stream_impl(val stream, val nbytes, unsigned char *hash,
                            val self)
{
  MD5_t md5;
  val buf = iobuf_get();
  val bfsz = length_buf(buf);
  MD5_init(&md5);

  if (null_or_missing_p(nbytes)) {
    for (;;) {
      val read = fill_buf(buf, zero, stream);
      cnum rd = c_num(read, self);

      if (!rd)
        break;

      MD5_update(&md5, buf->b.data, rd);
    }
  } else {
    while (ge(nbytes, bfsz)) {
      val read = fill_buf(buf, zero, stream);
      cnum rd = c_num(read, self);

      if (zerop(read))
        break;

      MD5_update(&md5, buf->b.data, rd);
      nbytes = minus(nbytes, read);
    }

    buf_set_length(buf, nbytes, nil);

    {
      val read = fill_buf(buf, zero, stream);
      cnum rd = c_num(read, self);
      if (rd)
        MD5_update(&md5, buf->b.data, rd);
    }
  }

  MD5_final(&md5, hash);
  iobuf_put(buf);
}

val md5_stream(val stream, val nbytes, val buf_in)
{
  val self = lit("md5-stream");
  unsigned char *hash;
  val buf = chksum_ensure_buf(self, buf_in, num_fast(MD5_DIGEST_LENGTH),
                              &hash, lit("MD5"));
  md5_stream_impl(stream, nbytes, hash, self);
  return buf;
}

static void md5_szmax_upd(MD5_t *pmd5, mem_t *data, ucnum len)
{
  const size_t szmax = convert(size_t, -1) / 4 + 1;
  while (len >= szmax) {
    MD5_update(pmd5, data, szmax);
    data += szmax;
    len -= szmax;
  }
  if (len > 0)
    MD5_update(pmd5, data, len);
}

static void md5_buf(val buf, unsigned char *hash, val self)
{
  MD5_t md5;
  MD5_init(&md5);
  md5_szmax_upd(&md5, buf->b.data, c_unum(buf->b.len, self));
  MD5_final(&md5, hash);
}

static void md5_str(val str, unsigned char *hash, val self)
{
  char *s = utf8_dup_to(c_str(str, self));
  MD5_t md5;
  MD5_init(&md5);
  MD5_update(&md5, coerce(const unsigned char *, s), strlen(s));
  free(s);
  MD5_final(&md5, hash);
}

val md5(val obj, val buf_in)
{
  val self = lit("md5");
  unsigned char *hash;
  val buf = chksum_ensure_buf(self, buf_in, num_fast(MD5_DIGEST_LENGTH),
                              &hash, lit("MD5"));

  switch (type(obj)) {
  case STR:
  case LSTR:
  case LIT:
    md5_str(obj, hash, self);
    return buf;
  case BUF:
    md5_buf(obj, hash, self);
    return buf;
  default:
    uw_throwf(error_s, lit("~a: cannot hash ~s, only buffer and strings"),
              self, obj, nao);
  }
}

static struct cobj_ops md5_ops = cobj_ops_init(cobj_equal_handle_op,
                                                  cobj_print_op,
                                                  cobj_destroy_free_op,
                                                  cobj_mark_op,
                                                  cobj_handle_hash_op);
val md5_begin(void)
{
  MD5_t *pmd5 = coerce(MD5_t *, chk_malloc(sizeof *pmd5));
  MD5_init(pmd5);
  return cobj(coerce(mem_t *, pmd5), md5_ctx_cls, &md5_ops);
}

static int md5_utf8_byte_callback(int b, mem_t *ctx)
{
  MD5_t *ps256 = coerce(MD5_t *, ctx);
  unsigned char uc = b;
  MD5_update(ps256, &uc, 1);
  return 1;
}

val md5_hash(val ctx, val obj)
{
  val self = lit("md5-hash");
  MD5_t *pmd5 = coerce(MD5_t *, cobj_handle(self, ctx, md5_ctx_cls));

  switch (type(obj)) {
  case STR:
  case LSTR:
  case LIT:
    {
      char *str = utf8_dup_to(c_str(obj, self));
      MD5_update(pmd5, coerce(const unsigned char *, str), strlen(str));
      free(str);
    }
    break;
  case BUF:
    md5_szmax_upd(pmd5, obj->b.data, c_unum(obj->b.len, self));
    break;
  case CHR:
    utf8_encode(c_chr(obj), md5_utf8_byte_callback, coerce(mem_t *, pmd5));
    break;
  case NUM:
    {
      cnum n = c_num(obj, self);
      unsigned char uc = n;
      if (n < 0 || n > 255)
        uw_throwf(error_s, lit("~a: byte value ~s out of range"),
                  self, obj, nao);
      MD5_update(pmd5, &uc, 1);
    }
    break;
  default:
    uw_throwf(error_s, lit("~a: cannot hash ~s, only buffer and strings"),
              self, obj, nao);
  }

  return obj;
}

val md5_end(val ctx, val buf_in)
{
  val self = lit("md5-end");
  unsigned char *hash;
  MD5_t *pmd5 = coerce(MD5_t *, cobj_handle(self, ctx, md5_ctx_cls));
  val buf = chksum_ensure_buf(self, buf_in, num_fast(MD5_DIGEST_LENGTH),
                              &hash, lit("SHA-256"));

  MD5_final(pmd5, hash);
  MD5_init(pmd5);
  return buf;
}

void chksum_init(void)
{
  sha256_ctx_s = intern(lit("sha256-ctx"), user_package);
  md5_ctx_s = intern(lit("md5-ctx"), user_package);
  sha256_ctx_cls = cobj_register(sha256_ctx_s);
  md5_ctx_cls = cobj_register(md5_ctx_s);
  reg_fun(intern(lit("sha256-stream"), user_package), func_n3o(sha256_stream, 1));
  reg_fun(intern(lit("sha256"), user_package), func_n2o(sha256, 1));
  reg_fun(intern(lit("sha256-begin"), user_package), func_n0(sha256_begin));
  reg_fun(intern(lit("sha256-hash"), user_package), func_n2(sha256_hash));
  reg_fun(intern(lit("sha256-end"), user_package), func_n2o(sha256_end, 1));
  reg_fun(intern(lit("crc32-stream"), user_package), func_n3o(crc32_stream, 1));
  reg_fun(intern(lit("crc32"), user_package), func_n2o(crc32, 1));
  reg_fun(intern(lit("md5-stream"), user_package), func_n3o(md5_stream, 1));
  reg_fun(intern(lit("md5"), user_package), func_n2o(md5, 1));
  reg_fun(intern(lit("md5-begin"), user_package), func_n0(md5_begin));
  reg_fun(intern(lit("md5-hash"), user_package), func_n2(md5_hash));
  reg_fun(intern(lit("md5-end"), user_package), func_n2o(md5_end, 1));
}
