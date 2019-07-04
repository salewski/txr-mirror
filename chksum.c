/* Copyright 2019
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
#include "chksums/sha256.h"
#include "chksums/crc32.h"
#include "chksum.h"

val sha256_stream(val stream, val nbytes)
{
  SHA256_t s256;
  unsigned char *hash = chk_malloc(SHA256_DIGEST_LENGTH);
  val buf = iobuf_get();
  val bfsz = length_buf(buf);
  SHA256_init(&s256);

  if (null_or_missing_p(nbytes)) {
    for (;;) {
      val read = fill_buf(buf, zero, stream);
      cnum rd = c_num(read);

      if (!rd)
        break;

      SHA256_update(&s256, buf->b.data, rd);
    }
  } else {
    while (ge(nbytes, bfsz)) {
      val read = fill_buf(buf, zero, stream);
      cnum rd = c_num(read);

      if (zerop(read))
        break;

      SHA256_update(&s256, buf->b.data, rd);
      nbytes = minus(nbytes, read);
    }

    buf_set_length(buf, nbytes, nil);

    {
      val read = fill_buf(buf, zero, stream);
      cnum rd = c_num(read);
      if (rd)
        SHA256_update(&s256, buf->b.data, rd);
    }
  }

  SHA256_final(&s256, hash);
  iobuf_put(buf);
  return make_borrowed_buf(num_fast(SHA256_DIGEST_LENGTH), hash);
}

static val sha256_buf(val buf, val self)
{
  SHA256_t s256;
  unsigned char *hash = chk_malloc(SHA256_DIGEST_LENGTH);
  SHA256_init(&s256);
  ucnum len = c_unum(buf->b.len);
  mem_t *data = buf->b.data;
  const size_t szmax = convert(size_t, -1) / 4 + 1;

  while (len >= szmax) {
    SHA256_update(&s256, data, szmax);
    data += szmax;
    len -= szmax;
  }

  if (len > 0)
    SHA256_update(&s256, data, len);

  SHA256_final(&s256, hash);
  return make_borrowed_buf(num_fast(SHA256_DIGEST_LENGTH), hash);
}

static val sha256_str(val str, val self)
{
  val s = make_byte_input_stream(str);
  return sha256_stream(s, nil);
}

val sha256(val obj)
{
  val self = lit("sha256");

  switch (type(obj)) {
  case STR:
  case LSTR:
  case LIT:
    return sha256_str(obj, self);
  case BUF:
    return sha256_buf(obj, self);
  default:
    uw_throwf(error_s, lit("~a: cannot hash ~s, only buffer and strings"),
              self, obj, nao);
  }
}

val crc32_stream(val stream, val nbytes)
{
  u32_t crc = 0;
  val bfsz = num_fast(BUFSIZ);
  val buf = make_buf(bfsz, nil, nil);

  if (null_or_missing_p(nbytes)) {
    for (;;) {
      val read = fill_buf(buf, zero, stream);
      cnum rd = c_num(read);

      if (!rd)
        break;

      crc = crc32_cont(buf->b.data, rd, crc);
    }
  } else {
    while (ge(nbytes, bfsz)) {
      val read = fill_buf(buf, zero, stream);
      cnum rd = c_num(read);

      if (zerop(read))
        break;

      crc = crc32_cont(buf->b.data, rd, crc);
      nbytes = minus(nbytes, read);
    }

    buf_set_length(buf, nbytes, nil);

    {
      val read = fill_buf(buf, zero, stream);
      cnum rd = c_num(read);
      if (rd)
        crc = crc32_cont(buf->b.data, rd, crc);
    }
  }

  return unum(crc);
}

static val crc32_buf(val buf, val self)
{
  ucnum len = c_unum(buf->b.len);
  mem_t *data = buf->b.data;
  const size_t szmax = convert(size_t, -1) / 4 + 1;
  u32_t crc = 0;

  while (len >= szmax) {
    crc = crc32_cont(data, szmax, crc);
    data += szmax;
    len -= szmax;
  }

  if (len > 0)
    crc = crc32_cont(data, len, crc);

  return unum(crc);
}

static val crc32_str(val str, val self)
{
  val s = make_byte_input_stream(str);
  return crc32_stream(s, nil);
}


val crc32(val obj)
{
  val self = lit("sha256");

  switch (type(obj)) {
  case STR:
  case LSTR:
  case LIT:
    return crc32_str(obj, self);
  case BUF:
    return crc32_buf(obj, self);
  default:
    uw_throwf(error_s, lit("~a: cannot hash ~s, only buffer and strings"),
              self, obj, nao);
  }
}

void chksum_init(void)
{
  reg_fun(intern(lit("sha256-stream"), user_package), func_n2o(sha256_stream, 1));
  reg_fun(intern(lit("sha256"), user_package), func_n1(sha256));
  reg_fun(intern(lit("crc32-stream"), user_package), func_n2o(crc32_stream, 1));
  reg_fun(intern(lit("crc32"), user_package), func_n1(crc32));
}
