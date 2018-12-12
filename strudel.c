/* Copyright 2017-2018
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
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <wchar.h>
#include <signal.h>
#include "config.h"
#include "lib.h"
#include "stream.h"
#include "gc.h"
#include "eval.h"
#include "struct.h"
#include "strudel.h"

struct strudel_base { /* stru-ct del-egate :) */
  struct strm_base a;
  val obj;
};

static void strudel_base_mark(struct strudel_base *sb)
{
  strm_base_mark(&sb->a);
  gc_mark(sb->obj);
}

static void strudel_mark_op(val stream)
{
  struct strudel_base *sb = coerce(struct strudel_base *, stream->co.handle);
  strudel_base_mark(sb);
}

static val strudel_put_string(val stream, val str)
{
  struct strudel_base *sb = coerce(struct strudel_base *, stream->co.handle);
  val obj = sb->obj;
  val meth = slot(obj, put_string_s);
  return funcall2(meth, obj, str);
}

static val strudel_put_char(val stream, val ch)
{
  struct strudel_base *sb = coerce(struct strudel_base *, stream->co.handle);
  val obj = sb->obj;
  val meth = slot(obj, put_char_s);
  return funcall2(meth, obj, ch);
}

static val strudel_put_byte(val stream, int byte)
{
  struct strudel_base *sb = coerce(struct strudel_base *, stream->co.handle);
  val obj = sb->obj;
  val meth = slot(obj, put_byte_s);
  return funcall2(meth, obj, num_fast(byte));
}

static val strudel_get_line(val stream)
{
  struct strudel_base *sb = coerce(struct strudel_base *, stream->co.handle);
  val obj = sb->obj;
  val meth = slot(obj, get_line_s);
  return funcall1(meth, obj);
}

static val strudel_get_char(val stream)
{
  struct strudel_base *sb = coerce(struct strudel_base *, stream->co.handle);
  val obj = sb->obj;
  val meth = slot(obj, get_char_s);
  return funcall1(meth, obj);
}

static val strudel_get_byte(val stream)
{
  struct strudel_base *sb = coerce(struct strudel_base *, stream->co.handle);
  val obj = sb->obj;
  val meth = slot(obj, get_byte_s);
  return funcall1(meth, obj);
}

static val strudel_unget_char(val stream, val ch)
{
  struct strudel_base *sb = coerce(struct strudel_base *, stream->co.handle);
  val obj = sb->obj;
  val meth = slot(obj, unget_char_s);
  return funcall2(meth, obj, ch);
}

static val strudel_unget_byte(val stream, int byte)
{
  struct strudel_base *sb = coerce(struct strudel_base *, stream->co.handle);
  val obj = sb->obj;
  val meth = slot(obj, unget_byte_s);
  return funcall2(meth, obj, num_fast(byte));
}

static val strudel_put_buf(val stream, val buf, cnum pos)
{
  struct strudel_base *sb = coerce(struct strudel_base *, stream->co.handle);
  val obj = sb->obj;
  val meth = slot(obj, put_buf_s);
  return funcall3(meth, obj, buf, num(pos));
}

static val strudel_fill_buf(val stream, val buf, cnum pos)
{
  struct strudel_base *sb = coerce(struct strudel_base *, stream->co.handle);
  val obj = sb->obj;
  val meth = slot(obj, fill_buf_s);
  return funcall3(meth, obj, buf, num(pos));
}

static val strudel_close(val stream, val throw_on_error)
{
  struct strudel_base *sb = coerce(struct strudel_base *, stream->co.handle);
  val obj = sb->obj;
  val meth = slot(obj, close_s);
  return funcall2(meth, obj, throw_on_error);
}

static val strudel_flush(val stream)
{
  struct strudel_base *sb = coerce(struct strudel_base *, stream->co.handle);
  val obj = sb->obj;
  val meth = slot(obj, flush_s);
  return funcall1(meth, obj);
}

static val strudel_seek(val stream, val off, enum strm_whence whence)
{
  struct strudel_base *sb = coerce(struct strudel_base *, stream->co.handle);
  val wh;
  val obj = sb->obj;
  val meth = slot(obj, seek_s);

  switch (whence) {
  default:
  case strm_start:
    wh = from_start_k;
    break;
  case strm_cur:
    wh = from_current_k;
    break;
  case strm_end:
    wh = from_end_k;
    break;
  }

  return funcall3(meth, obj, off, wh);
}

static val strudel_truncate(val stream, val len)
{
  struct strudel_base *sb = coerce(struct strudel_base *, stream->co.handle);
  val obj = sb->obj;
  val meth = slot(obj, truncate_s);
  return funcall2(meth, obj, len);
}

static val strudel_get_prop(val stream, val ind)
{
  struct strudel_base *sb = coerce(struct strudel_base *, stream->co.handle);
  val obj = sb->obj;
  val meth = slot(obj, get_prop_s);
  return funcall2(meth, obj, ind);
}

static val strudel_set_prop(val stream, val ind, val value)
{
  struct strudel_base *sb = coerce(struct strudel_base *, stream->co.handle);
  val obj = sb->obj;
  val meth = slot(obj, set_prop_s);
  return funcall3(meth, obj, ind, value);
}

static val strudel_get_error(val stream)
{
  struct strudel_base *sb = coerce(struct strudel_base *, stream->co.handle);
  val obj = sb->obj;
  val meth = slot(obj, get_error_s);
  return funcall1(meth, obj);
}

static val strudel_get_error_str(val stream)
{
  struct strudel_base *sb = coerce(struct strudel_base *, stream->co.handle);
  val obj = sb->obj;
  val meth = slot(obj, get_error_str_s);
  return funcall1(meth, obj);
}

static val strudel_clear_error(val stream)
{
  struct strudel_base *sb = coerce(struct strudel_base *, stream->co.handle);
  val obj = sb->obj;
  val meth = slot(obj, clear_error_s);
  return funcall1(meth, obj);
}

static val strudel_get_fd(val stream)
{
  struct strudel_base *sb = coerce(struct strudel_base *, stream->co.handle);
  val obj = sb->obj;
  val meth = slot(obj, get_fd_s);
  return funcall1(meth, obj);
}

static struct strm_ops strudel_ops =
  strm_ops_init(cobj_ops_init(eq,
                              stream_print_op,
                              stream_destroy_op,
                              strudel_mark_op,
                              cobj_eq_hash_op),
                wli("struct-delegate-stream"),
                strudel_put_string, strudel_put_char, strudel_put_byte,
                strudel_get_line, strudel_get_char, strudel_get_byte,
                strudel_unget_char, strudel_unget_byte,
                strudel_put_buf, strudel_fill_buf,
                strudel_close, strudel_flush, strudel_seek,
                strudel_truncate, strudel_get_prop, strudel_set_prop,
                strudel_get_error, strudel_get_error_str,
                strudel_clear_error, strudel_get_fd);

val make_struct_delegate_stream(val target_obj)
{
  struct strudel_base *sb = coerce(struct strudel_base *,
                                   chk_malloc(sizeof *sb));
  val stream;
  strm_base_init(&sb->a);
  sb->obj = nil;
  stream = cobj(coerce(mem_t *, sb), stream_s, &strudel_ops.cobj_ops);
  sb->obj = target_obj;
  return stream;
}

void strudel_init(void)
{
  reg_fun(intern(lit("make-struct-delegate-stream"), user_package), func_n1(make_struct_delegate_stream));
  fill_stream_ops(&strudel_ops);
}
