/* Copyright 2019-2023
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

#define chksum_impl(nameroot, ctx_type,                       \
                   hash_name, digest_len, chksum_init,        \
                   chksum_update, chksum_final)               \
                                                              \
  static void nameroot ## _stream_impl(val stream,            \
                                       val nbytes,            \
                                       unsigned char *hash,   \
                                       val self)              \
  {                                                           \
    ctx_type ctx;                                             \
    val buf = iobuf_get();                                    \
    val bfsz = length_buf(buf);                               \
    chksum_init(&ctx);                                        \
                                                              \
    if (null_or_missing_p(nbytes)) {                          \
      for (;;) {                                              \
        val read = fill_buf(buf, zero, stream);               \
        cnum rd = c_num(read, self);                          \
                                                              \
        if (!rd)                                              \
          break;                                              \
                                                              \
        chksum_update(&ctx, buf->b.data, rd);                 \
      }                                                       \
    } else {                                                  \
      while (ge(nbytes, bfsz)) {                              \
        val read = fill_buf(buf, zero, stream);               \
        cnum rd = c_num(read, self);                          \
                                                              \
        if (zerop(read))                                      \
          break;                                              \
                                                              \
        chksum_update(&ctx, buf->b.data, rd);                 \
        nbytes = minus(nbytes, read);                         \
      }                                                       \
                                                              \
      buf_set_length(buf, nbytes, nil);                       \
                                                              \
      {                                                       \
        val read = fill_buf(buf, zero, stream);               \
        cnum rd = c_num(read, self);                          \
        if (rd)                                               \
          chksum_update(&ctx, buf->b.data, rd);               \
      }                                                       \
    }                                                         \
                                                              \
    chksum_final(&ctx, hash);                                 \
    iobuf_put(buf);                                           \
  }                                                           \
                                                              \
  val nameroot ## _stream(val stream, val nbytes, val buf_in) \
  {                                                           \
    val self = lit(#nameroot "-stream");                      \
    unsigned char *hash;                                      \
    val buf = chksum_ensure_buf(self, buf_in,                 \
                                num_fast(digest_len),         \
                                &hash, lit(hash_name));       \
    nameroot ## _stream_impl(stream, nbytes, hash, self);     \
    return buf;                                               \
  }                                                           \
                                                              \
  static void nameroot ## _szmax_upd(ctx_type *pctx,          \
                                      mem_t *data, ucnum len) \
  {                                                           \
    const size_t szmax = convert(size_t, -1) / 4 + 1;         \
    while (len >= szmax) {                                    \
      chksum_update(pctx, data, szmax);                       \
      data += szmax;                                          \
      len -= szmax;                                           \
    }                                                         \
    if (len > 0)                                              \
      chksum_update(pctx, data, len);                         \
  }                                                           \
                                                              \
  static void nameroot ## _buf(val buf, unsigned char *hash,  \
                               val self)                      \
  {                                                           \
    ctx_type ctx;                                             \
    chksum_init(&ctx);                                        \
    nameroot ## _szmax_upd(&ctx, buf->b.data,                 \
                           c_unum(buf->b.len, self));         \
    chksum_final(&ctx, hash);                                 \
  }                                                           \
                                                              \
  static void nameroot ## _str(val str, unsigned char *hash,  \
                         val self)                            \
  {                                                           \
    char *s = utf8_dup_to(c_str(str, self));                  \
    ctx_type ctx;                                             \
    chksum_init(&ctx);                                        \
    chksum_update(&ctx, coerce(const unsigned char *, s),     \
                 strlen(s));                                  \
    free(s);                                                  \
    chksum_final(&ctx, hash);                                 \
  }                                                           \
                                                              \
  val nameroot(val obj, val buf_in)                           \
  {                                                           \
    val self = lit(#nameroot);                                \
    unsigned char *hash;                                      \
    val buf = chksum_ensure_buf(self, buf_in,                 \
                                num_fast(digest_len),         \
                                &hash, lit(hash_name));       \
    switch (type(obj)) {                                      \
    case STR:                                                 \
    case LSTR:                                                \
    case LIT:                                                 \
      nameroot ## _str(obj, hash, self);                      \
      return buf;                                             \
    case BUF:                                                 \
      nameroot ## _buf(obj, hash, self);                      \
      return buf;                                             \
    default:                                                  \
      uw_throwf(error_s,                                      \
                lit("~a: cannot hash ~s, "                    \
                    "only buffer and strings"),               \
                self, obj, nao);                              \
    }                                                         \
  }                                                           \
                                                              \
  static struct cobj_ops nameroot ## _ops =                   \
     cobj_ops_init(cobj_equal_handle_op,                      \
                   cobj_print_op,                             \
                   cobj_destroy_free_op,                      \
                   cobj_mark_op,                              \
                   cobj_handle_hash_op);                      \
                                                              \
  val nameroot ## _begin(void)                                \
  {                                                           \
    ctx_type *pctx = coerce(ctx_type *,                       \
                            chk_malloc(sizeof *pctx));        \
    chksum_init(pctx);                                        \
    return cobj(coerce(mem_t *, pctx),                        \
                nameroot ## _ctx_cls, &nameroot ## _ops);     \
  }                                                           \
                                                              \
  static int nameroot ## _utf8_byte_callback(int b,           \
                                             mem_t *ctx)      \
  {                                                           \
    ctx_type *pctx = coerce(ctx_type *, ctx);                 \
    unsigned char uc = b;                                     \
    chksum_update(pctx, &uc, 1);                              \
    return 1;                                                 \
  }                                                           \
                                                              \
  val nameroot ## _hash(val ctx, val obj)                     \
  {                                                           \
    val self = lit(#nameroot "-hash");                        \
    ctx_type *pctx = coerce(ctx_type *,                       \
                            cobj_handle(self, ctx,            \
                                        nameroot ##           \
                                        _ctx_cls));           \
    switch (type(obj)) {                                      \
    case STR:                                                 \
    case LSTR:                                                \
    case LIT:                                                 \
      {                                                       \
        char *str = utf8_dup_to(c_str(obj, self));            \
        chksum_update(pctx,                                   \
                      coerce(const unsigned char *, str),     \
                      strlen(str));                           \
        free(str);                                            \
      }                                                       \
      break;                                                  \
    case BUF:                                                 \
      nameroot ## _szmax_upd(pctx, obj->b.data,               \
                            c_unum(obj->b.len, self));        \
      break;                                                  \
    case CHR:                                                 \
      utf8_encode(c_ch(obj),                                  \
                  nameroot ## _utf8_byte_callback,            \
                  coerce(mem_t *, pctx));                     \
      break;                                                  \
    case NUM:                                                 \
      {                                                       \
        cnum n = c_num(obj, self);                            \
        unsigned char uc = n;                                 \
        if (n < 0 || n > 255)                                 \
          uw_throwf(error_s,                                  \
                    lit("~a: byte value ~s out of range"),    \
                    self, obj, nao);                          \
        chksum_update(pctx, &uc, 1);                          \
      }                                                       \
      break;                                                  \
    default:                                                  \
      uw_throwf(error_s, lit("~a: cannot hash ~s, "           \
                             "only buffer and strings"),      \
                self, obj, nao);                              \
    }                                                         \
                                                              \
    return obj;                                               \
  }                                                           \
                                                              \
  val nameroot ## _end(val ctx, val buf_in)                   \
  {                                                           \
    val self = lit(#nameroot "-end");                         \
    unsigned char *hash;                                      \
    ctx_type *pctx = coerce(ctx_type *,                       \
                            cobj_handle(self, ctx,            \
                                        nameroot ##           \
                                        _ctx_cls));           \
    val buf = chksum_ensure_buf(self, buf_in,                 \
                                num_fast(digest_len),         \
                                &hash, lit(hash_name));       \
    chksum_final(pctx, hash);                                 \
    chksum_init(pctx);                                        \
    return buf;                                               \
  }

chksum_impl(sha256, SHA256_t, "SHA-256", SHA256_DIGEST_LENGTH,
            SHA256_init, SHA256_update, SHA256_final);

chksum_impl(md5, MD5_t, "MD5", MD5_DIGEST_LENGTH,
            MD5_init, MD5_update, MD5_final);

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


static val crc32(val obj, val init)
{
  val self = lit("crc32");

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
