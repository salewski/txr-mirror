/* Copyright 2009-2021
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

#define UTF8_DECL_OPENDIR
#include <stdio.h>
#include <string.h>
#include <stddef.h>
#include <dirent.h>
#include <stdarg.h>
#include <stdlib.h>
#include <errno.h>
#include <ctype.h>
#include <wchar.h>
#include <wctype.h>
#include <signal.h>
#include "config.h"
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef __CYGWIN__
#include <sys/utsname.h>
#endif
#if HAVE_FCNTL
#include <fcntl.h>
#endif
#include <float.h>
#if HAVE_SYS_WAIT
#include <sys/wait.h>
#endif
#if HAVE_WINDOWS_H
#include <windows.h>
#endif
#if HAVE_WSPAWN || HAVE_SPAWN
#include <process.h>
#endif
#include "alloca.h"
#include "lib.h"
#include "gc.h"
#include "signal.h"
#include "unwind.h"
#include "args.h"
#include "sysif.h"
#include "stream.h"
#include "utf8.h"
#include "eval.h"
#include "regex.h"
#include "txr.h"
#include "buf.h"

/* Adhere to ISO C rules about direction switching on update streams. */
#ifndef __gnu_linux__
#define CONFIG_STDIO_STRICT 1
#endif

val stdin_s, stdout_s, stddebug_s, stderr_s, stdnull_s;

val put_string_s, put_char_s, put_byte_s, get_line_s, get_char_s;
val get_byte_s, unget_char_s, unget_byte_s, put_buf_s, fill_buf_s;
val flush_s, seek_s, truncate_s, get_prop_s, set_prop_s;
val get_error_s, get_error_str_s, clear_error_s, get_fd_s;

val print_flo_precision_s, print_flo_digits_s, print_flo_format_s;
val pprint_flo_format_s, print_base_s, print_circle_s;

val from_start_k, from_current_k, from_end_k;
val real_time_k, name_k, addr_k, fd_k, byte_oriented_k;
val format_s;

val stdio_stream_s;

#if HAVE_SOCKETS
val socket_error_s;
#endif

const wchli_t *path_sep_chars = wli("/");

val shell, shell_arg;

void strm_base_init(struct strm_base *s)
{
  static struct strm_base init = { indent_off, 60, 10, 0, 0, 0, 0, 0, 0 };
  *s = init;
}

void strm_base_cleanup(struct strm_base *s)
{
  bug_unless (s->ctx == 0);
}

void strm_base_mark(struct strm_base *s)
{
  (void) s;
}

void stream_print_op(val stream, val out, val pretty, struct strm_ctx *ctx)
{
  val name = stream_get_prop(stream, name_k);
  (void) pretty;
  (void) ctx;
  format(out, lit("#<~a ~p>"), name, stream, nao);
}

void stream_destroy_op(val stream)
{
  struct strm_base *s = coerce(struct strm_base *, stream->co.handle);
  strm_base_cleanup(s);
  free(s);
}

void stream_mark_op(val stream)
{
  struct strm_base *s = coerce(struct strm_base *, stream->co.handle);
  strm_base_mark(s);
}

static NORETURN void unimpl(val stream, val op)
{
  uw_throwf(file_error_s, lit("~a: not supported by stream ~s"),
            op, stream, nao);
  abort();
}

static NORETURN val unimpl_put_string(val stream, val str)
{
  (void) str;
  unimpl(stream, lit("put-string"));
}

static NORETURN val unimpl_put_char(val stream, val ch)
{
  (void) ch;
  unimpl(stream, lit("put-char"));
}

static NORETURN val unimpl_put_byte(val stream, int byte)
{
  (void) byte;
  unimpl(stream, lit("put-byte"));
}

static NORETURN val unimpl_get_line(val stream)
{
  unimpl(stream, lit("get-line"));
}

static NORETURN val unimpl_get_char(val stream)
{
  unimpl(stream, lit("get-char"));
}

static NORETURN val unimpl_get_byte(val stream)
{
  unimpl(stream, lit("get-byte"));
}

static NORETURN val unimpl_unget_char(val stream, val ch)
{
  (void) ch;
  unimpl(stream, lit("unget-char"));
}

static NORETURN val unimpl_unget_byte(val stream, int byte)
{
  (void) byte;
  unimpl(stream, lit("unget-byte"));
}

static NORETURN ucnum unimpl_put_buf(val stream, mem_t *ptr, ucnum len, ucnum pos)
{
  (void) ptr;
  (void) len;
  (void) pos;
  unimpl(stream, lit("put-buf"));
}

static NORETURN ucnum unimpl_fill_buf(val stream, mem_t *ptr, ucnum len, ucnum pos)
{
  (void) ptr;
  (void) len;
  (void) pos;
  unimpl(stream, lit("fill-buf"));
}

static NORETURN val unimpl_seek(val stream, val off, enum strm_whence whence)
{
  (void) off;
  (void) whence;
  unimpl(stream, lit("seek-stream"));
}

static NORETURN val unimpl_truncate(val stream, val len)
{
  (void) len;
  unimpl(stream, lit("truncate-stream"));
}

static NORETURN val unimpl_get_sock_family(val stream)
{
  unimpl(stream, lit("sock-family"));
}

static NORETURN val unimpl_get_sock_type(val stream)
{
  unimpl(stream, lit("sock-type"));
}

static NORETURN val unimpl_get_sock_peer(val stream)
{
  unimpl(stream, lit("sock-peer"));
}

static NORETURN val unimpl_set_sock_peer(val stream, val peer)
{
  (void) peer;
  unimpl(stream, lit("sock-set-peer"));
}

static val null_put_string(val stream, val str)
{
  (void) stream;
  (void) str;
  return nil;
}

static val null_put_char(val stream, val ch)
{
  (void) stream;
  (void) ch;
  return nil;
}

static val null_put_byte(val stream, int byte)
{
  (void) stream;
  (void) byte;
  return nil;
}

static val null_get_line(val stream)
{
  (void) stream;
  return nil;
}

static val null_get_char(val stream)
{
  (void) stream;
  return nil;
}

static val null_get_byte(val stream)
{
  (void) stream;
  return nil;
}

static val null_close(val stream, val throw_on_error)
{
  (void) stream;
  (void) throw_on_error;
  return nil;
}

static val null_flush(val stream)
{
  (void) stream;
  return nil;
}

static val null_seek(val stream, val off, enum strm_whence whence)
{
  (void) stream;
  (void) off;
  (void) whence;
  return nil;
}

static val null_get_prop(val stream, val ind)
{
  if (ind == name_k) {
    struct strm_ops *ops = coerce(struct strm_ops *, stream->co.ops);
    return static_str(ops->name);
  }

  return nil;
}

static val null_set_prop(val stream, val ind, val value)
{
  (void) stream;
  (void) ind;
  (void) value;
  return nil;
}

static val null_get_error(val stream)
{
  (void) stream;
  return nil;
}

static val null_get_error_str(val stream)
{
  (void) stream;
  return nil;
}

static val null_clear_error(val stream)
{
  (void) stream;
  return nil;
}

static val null_get_fd(val stream)
{
  (void) stream;
  return nil;
}

static ucnum generic_put_buf(val stream, mem_t *ptr, ucnum len, ucnum pos)
{
  struct strm_ops *ops = coerce(struct strm_ops *, stream->co.ops);
  ucnum i;

  if (pos >= len)
    return len;

  for (i = pos; i < len; i++)
    ops->put_byte(stream, *ptr++);

  if (i > len)
    i = len;

  return i;
}

static ucnum generic_fill_buf(val stream, mem_t *ptr, ucnum len, ucnum pos)
{
  val self = lit("fill-buf");
  struct strm_ops *ops = coerce(struct strm_ops *, stream->co.ops);
  ucnum i;

  for (i = pos; i < len; i++) {
    val byte = ops->get_byte(stream);
    if (!byte)
      break;
    *ptr++ = c_num(byte, self);
  }

  if (i > len)
    i = len;

  return i;
}

void fill_stream_ops(struct strm_ops *ops)
{
  if (!ops->put_string)
    ops->put_string = unimpl_put_string;
  if (!ops->put_char)
    ops->put_char = unimpl_put_char;
  if (!ops->put_byte)
    ops->put_byte = unimpl_put_byte;
  if (!ops->get_line)
    ops->get_line = unimpl_get_line;
  if (!ops->get_char)
    ops->get_char = unimpl_get_char;
  if (!ops->get_byte)
    ops->get_byte = unimpl_get_byte;
  if (!ops->unget_char)
    ops->unget_char = unimpl_unget_char;
  if (!ops->unget_byte)
    ops->unget_byte = unimpl_unget_byte;
  if (!ops->put_buf)
    ops->put_buf = (ops->get_byte ? generic_put_buf : unimpl_put_buf);
  if (!ops->fill_buf)
    ops->fill_buf = (ops->put_byte ? generic_fill_buf : unimpl_fill_buf);
  if (!ops->close)
    ops->close = null_close;
  if (!ops->flush)
    ops->flush = null_flush;
  if (!ops->seek)
    ops->seek = unimpl_seek;
  if (!ops->truncate)
    ops->truncate = unimpl_truncate;
  if (!ops->get_prop)
    ops->get_prop = null_get_prop;
  if (!ops->set_prop)
    ops->set_prop = null_set_prop;
  if (!ops->get_error)
    ops->get_error = null_get_error;
  if (!ops->get_error_str)
    ops->get_error_str = null_get_error_str;
  if (!ops->clear_error)
    ops->clear_error = null_clear_error;
  if (!ops->get_fd)
    ops->get_fd = null_get_fd;
  if (!ops->get_sock_family)
    ops->get_sock_family = unimpl_get_sock_family;
  if (!ops->get_sock_type)
    ops->get_sock_type = unimpl_get_sock_type;
  if (!ops->get_sock_peer)
    ops->get_sock_peer = unimpl_get_sock_peer;
  if (!ops->set_sock_peer)
    ops->set_sock_peer = unimpl_set_sock_peer;
}

static struct strm_ops null_ops =
  strm_ops_init(cobj_ops_init(eq,
                              stream_print_op,
                              stream_destroy_op,
                              stream_mark_op,
                              cobj_eq_hash_op),
                wli("null-stream"),
                null_put_string, null_put_char, null_put_byte, null_get_line,
                null_get_char, null_get_byte,
                unimpl_unget_char, unimpl_unget_byte,
                unimpl_put_buf, unimpl_fill_buf,
                null_close, null_flush, null_seek, unimpl_truncate,
                null_get_prop, null_set_prop,
                null_get_error, null_get_error_str, null_clear_error,
                null_get_fd);

val make_null_stream(void)
{
  struct strm_base *s = coerce(struct strm_base *, chk_malloc(sizeof *s));
  strm_base_init(s);
  return cobj(coerce(mem_t *, s), stream_s, &null_ops.cobj_ops);
}

#if CONFIG_STDIO_STRICT
enum stdio_op { stdio_none, stdio_read, stdio_write };
#endif

struct stdio_handle {
  struct strm_base a;
  FILE *f;
  val descr;
  val unget_c;
  utf8_decoder_t ud;
  val err;
  char *buf;
#if HAVE_FORK_STUFF
  pid_t pid;
#else
  int pid;
#endif
  val mode; /* used by tail */
  unsigned is_rotated : 8; /* used by tail */
  unsigned is_real_time : 8;
  unsigned is_byte_oriented : 8;
#if CONFIG_STDIO_STRICT
  enum stdio_op last_op;
#endif
#if HAVE_SOCKETS
  val family;
  val type;
  val peer;
  val addr;
#endif
};

static void stdio_stream_print(val stream, val out, val pretty,
                               struct strm_ctx *ctx)
{
  struct stdio_handle *h = coerce(struct stdio_handle *, stream->co.handle);
  struct strm_ops *ops = coerce(struct strm_ops *, stream->co.ops);
  val name = static_str(ops->name);
  val descr = ops->get_prop(stream, name_k);

  (void) pretty;
  (void) ctx;

  if (h->pid)
    format(out, lit("#<~a ~a ~a ~p>"), name, descr, num(h->pid), stream, nao);
  else
    format(out, lit("#<~a ~a ~p>"), name, descr, stream, nao);
}

static void stdio_stream_destroy(val stream)
{
  struct stdio_handle *h = coerce(struct stdio_handle *, stream->co.handle);
  close_stream(stream, nil);
  strm_base_cleanup(&h->a);
  free(h->buf);
  free(h);
}

static void stdio_stream_mark(val stream)
{
  struct stdio_handle *h = coerce(struct stdio_handle *, stream->co.handle);
  strm_base_mark(&h->a);
  gc_mark(h->descr);
  gc_mark(h->mode);
  gc_mark(h->err);
#if HAVE_SOCKETS
  gc_mark(h->family);
  gc_mark(h->type);
  gc_mark(h->peer);
  gc_mark(h->addr);
#endif
}

val errno_to_string(val err)
{
  val self = lit("get-error-str");
  if (is_num(err))
    return errno_to_str(c_num(err, self));
  else if (!err)
    return lit("no error");
  else if (err == t)
    return lit("eof");
  else
    return lit("invalid error code");
}

static val stdio_maybe_read_error(val stream)
{
  struct stdio_handle *h = coerce(struct stdio_handle *, stream->co.handle);
  if (h->f == 0)
    uw_throwf(file_error_s, lit("error reading ~s: file closed"), stream, nao);
  if (ferror(h->f)) {
    val err = num(errno);
    h->err = err;
#ifdef EAGAIN
    if (errno == EAGAIN)
      uw_throwf(timeout_error_s, lit("timed out reading ~s"), stream, nao);
#endif
    uw_throwf(file_error_s, lit("error reading ~s: ~d/~s"),
              stream, err, errno_to_string(err), nao);
  }
  if (feof(h->f))
    h->err = t;
  return nil;
}

static val stdio_maybe_error(val stream, val action)
{
  struct stdio_handle *h = coerce(struct stdio_handle *, stream->co.handle);
  val err = num(errno);
  if (h->f == 0)
    uw_throwf(file_error_s, lit("error ~a ~s: file closed"), action, stream, nao);
  h->err = err;
#ifdef EAGAIN
  if (errno == EAGAIN)
    uw_throwf(timeout_error_s, lit("timed out on ~s"), stream, nao);
#endif
  uw_throwf(file_error_s, lit("error ~a ~s: ~d/~s"),
            action, stream, err, errno_to_string(err), nao);
}

static int se_putc(int ch, FILE *f)
{
  int ret;
  sig_save_enable;
#ifdef __CYGWIN__
  {
    char out[2] = { ch, 0 };
    ret = fputs(out, f) == EOF ? EOF : ch;
  }
#else
  ret = putc(ch, f);
#endif
  sig_restore_enable;
  return ret;
}

static int se_getc(FILE *f)
{
  int ret;
  sig_save_enable;
  ret = getc(f);
  sig_restore_enable;
  return ret;
}

static int se_fflush(FILE *f)
{
  int ret;
  sig_save_enable;
  ret = fflush(f);
  sig_restore_enable;
  return ret;
}

#if CONFIG_STDIO_STRICT
static void stdio_switch(struct stdio_handle *h, enum stdio_op op)
{
  if (h->last_op != op) {
    if (h->f) {
      switch (h->last_op) {
      case stdio_read:
        stdio_fseek(h->f, zero, strm_cur);
        break;
      case stdio_write:
        se_fflush(h->f);
        break;
      default:
        break;
      }
    }

    h->last_op = op;
  }
}
#else
#define stdio_switch(X, Y) ((void) 0)
#endif

static int stdio_put_char_callback(int ch, mem_t *f)
{
  int ret = se_putc(ch, coerce(FILE *, f)) != EOF;
  return ret;
}

static int stdio_get_char_callback(mem_t *f)
{
  return se_getc(coerce(FILE *, f));
}

static val stdio_put_string(val stream, val str)
{
  struct stdio_handle *h = coerce(struct stdio_handle *, stream->co.handle);

  errno = 0;

  if (h->f != 0) {
    const wchar_t *s = c_str(str);

    stdio_switch(h, stdio_write);

    while (*s) {
      if (!utf8_encode(*s++, stdio_put_char_callback, coerce(mem_t *, h->f)))
        return stdio_maybe_error(stream, lit("writing"));
    }
    return t;
  }
  return stdio_maybe_error(stream, lit("writing"));
}

static val stdio_put_char(val stream, val ch)
{
  struct stdio_handle *h = coerce(struct stdio_handle *, stream->co.handle);
  errno = 0;
  stdio_switch(h, stdio_write);
  return h->f != 0 && utf8_encode(c_chr(ch), stdio_put_char_callback,
                                  coerce(mem_t *, h->f))
         ? t : stdio_maybe_error(stream, lit("writing"));
}

static val stdio_put_byte(val stream, int b)
{
  struct stdio_handle *h = coerce(struct stdio_handle *, stream->co.handle);
  errno = 0;
  stdio_switch(h, stdio_write);
  return h->f != 0 && se_putc(b, coerce(FILE *, h->f)) != EOF
         ? t : stdio_maybe_error(stream, lit("writing"));
}

static val stdio_flush(val stream)
{
  struct stdio_handle *h = coerce(struct stdio_handle *, stream->co.handle);
  errno = 0;
#if CONFIG_STDIO_STRICT
  if (h->f && h->last_op != stdio_write)
    return t;
#endif
  return (h->f != 0 && se_fflush(h->f) == 0)
         ? t : stdio_maybe_error(stream, lit("flushing"));
}

static val stdio_seek(val stream, val offset, enum strm_whence whence)
{
  struct stdio_handle *h = coerce(struct stdio_handle *, stream->co.handle);

  errno = 0;

  if (h->f != 0) {
    if (offset == zero && whence == strm_cur) {
      return stdio_ftell(h->f);
    } else {
      if (stdio_fseek(h->f, offset, whence)) {
        utf8_decoder_init(&h->ud);
        h->unget_c = nil;
        return t;
      }
    }
  }

  return stdio_maybe_error(stream, lit("seeking"));
}

static val stdio_get_prop(val stream, val ind)
{
  struct stdio_handle *h = coerce(struct stdio_handle *, stream->co.handle);

  if (ind == real_time_k) {
    return h->is_real_time ? t : nil;
  } else if (ind == name_k) {
    return h->descr;
  } else if (ind == byte_oriented_k) {
    return h->is_byte_oriented ? t : nil;
  }
  return nil;
}

static val stdio_set_prop(val stream, val ind, val prop)
{
  struct stdio_handle *h = coerce(struct stdio_handle *, stream->co.handle);

  if (ind == real_time_k) {
    h->is_real_time = prop ? 1 : 0;
    return t;
  } else if (ind == byte_oriented_k) {
    h->is_byte_oriented = prop ? 1 : 0;
    return t;
  } else if (ind == name_k) {
    h->descr = prop;
  }
  return nil;
}

static val stdio_get_error(val stream)
{
  struct stdio_handle *h = coerce(struct stdio_handle *, stream->co.handle);
  if (h->f != 0 && feof(h->f))
    return t;
  return h->err;
}

static val stdio_get_error_str(val stream)
{
  struct stdio_handle *h = coerce(struct stdio_handle *, stream->co.handle);

  if (h->f != 0 && feof(h->f))
    return lit("eof");

  return errno_to_string(h->err);
}

static val stdio_clear_error(val stream)
{
  struct stdio_handle *h = coerce(struct stdio_handle *, stream->co.handle);
  val ret = h->err;
  if (h->f != 0)
    clearerr(h->f);
  h->err = nil;
  return ret;
}

static val stdio_get_fd(val stream)
{
  val self = lit("stream-fd");
  struct stdio_handle *h = coerce(struct stdio_handle *,
                                  cobj_handle(self, stream, stdio_stream_s));
  return h->f ? num(fileno(h->f)) : nil;
}

val generic_get_line(val stream)
{
  struct strm_ops *ops = coerce(struct strm_ops *, stream->co.ops);
  const size_t min_size = 512;
  size_t size = 0;
  size_t fill = 0;
  wchar_t *volatile buf = 0;
  val out = nil;

  uw_simple_catch_begin;

  for (;;) {
    val chr = ops->get_char(stream);
    wint_t ch = chr ? (wint_t) c_chr(chr) : WEOF;

    if (ch == WEOF && buf == 0)
      break;

    if (fill >= size) {
      size_t newsize = size ? size * 2 : min_size;
      buf = coerce(wchar_t *, chk_grow_vec(coerce(mem_t *, buf),
                                           size, newsize, sizeof *buf));
      size = newsize;
    }

    if (ch == '\n' || ch == WEOF) {
      buf[fill++] = 0;
      break;
    }
    buf[fill++] = ch;
  }

  /* Trim to actual size */
  if (buf) {
    wchar_t *sbuf = coerce(wchar_t *, chk_realloc(coerce(mem_t *, buf),
                                                  fill * sizeof *buf));
    if (sbuf)
      buf = sbuf;

    out = string_own(buf);
    buf = 0;
  }

  uw_unwind {
    free(buf);
  }

  uw_catch_end;

  return out;
}

static val stdio_get_char(val stream)
{
  struct stdio_handle *h = coerce(struct stdio_handle *, stream->co.handle);

  if (h->unget_c)
    return rcyc_pop(&h->unget_c);

  if (h->f) {
    wint_t ch;

    stdio_switch(h, stdio_read);

    if (h->is_byte_oriented) {
      ch = se_getc(h->f);
      if (ch == 0)
        ch = 0xDC00;
    } else {
        ch = utf8_decode(&h->ud, stdio_get_char_callback,
                         coerce(mem_t *, h->f));
    }

    return (ch != WEOF) ? chr(ch) : stdio_maybe_read_error(stream);
  }
  return stdio_maybe_read_error(stream);
}

static val stdio_get_byte(val stream)
{
  struct stdio_handle *h = coerce(struct stdio_handle *, stream->co.handle);

  stdio_switch(h, stdio_read);

  if (h->f) {
    int ch = se_getc(h->f);
    return (ch != EOF) ? num(ch) : stdio_maybe_read_error(stream);
  }
  return stdio_maybe_read_error(stream);
}

static val stdio_unget_char(val stream, val ch)
{
  struct stdio_handle *h = coerce(struct stdio_handle *, stream->co.handle);
  mpush(ch, mkloc(h->unget_c, stream));
  return ch;
}

static val stdio_unget_byte(val stream, int byte)
{
  struct stdio_handle *h = coerce(struct stdio_handle *, stream->co.handle);

  errno = 0;
  return h->f != 0 && ungetc(byte, coerce(FILE *, h->f)) != EOF
         ? num_fast(byte)
         : stdio_maybe_error(stream, lit("writing"));
}

static ucnum stdio_put_buf(val stream, mem_t *ptr, ucnum len, ucnum pos)
{
  val self = lit("put-buf");
  struct stdio_handle *h = coerce(struct stdio_handle *, stream->co.handle);
  if (convert(size_t, len) != len || len > INT_PTR_MAX)
    uw_throwf(error_s, lit("~a: buffer too large"), self, nao);
  if (pos >= len)
    return len;
  errno = 0;
  if (h->f != 0) {
    cnum nwrit = fwrite(ptr + pos, 1, len - pos, h->f);
    if (nwrit > 0)
      return pos + nwrit;
  }
  stdio_maybe_error(stream, lit("writing"));
  return 0;
}

static ucnum stdio_fill_buf(val stream, mem_t *ptr, ucnum len, ucnum pos)
{
  val self = lit("fill-buf");
  struct stdio_handle *h = coerce(struct stdio_handle *, stream->co.handle);
  if (convert(size_t, len) != len || len > INT_PTR_MAX)
    uw_throwf(error_s, lit("~a: buffer too large"), self, nao);
  if (pos >= len)
    return len;
  errno = 0;
  if (h->f != 0) {
    cnum nread = fread(ptr + pos, 1, len - pos, h->f);
    if (nread > 0)
      return pos + nread;
  }
  stdio_maybe_read_error(stream);
  return pos;
}

static val stdio_close(val stream, val throw_on_error)
{
  struct stdio_handle *h = coerce(struct stdio_handle *, stream->co.handle);

  if (h->f != 0 && h->f != stdin && h->f != stdout) {
    int result = fclose(h->f);
    h->f = 0;
    if (result == EOF && throw_on_error) {
      h->err = num(errno);
      uw_throwf(file_error_s, lit("error closing ~s: ~d/~s"),
                stream, num(errno), errno_to_str(errno), nao);
    }
    return result != EOF ? t : nil;
  }
  return nil;
}

#if HAVE_FTRUNCATE || HAVE_CHSIZE
static val stdio_truncate(val stream, val len)
{
  val self = lit("truncate-stream");
  struct stdio_handle *h = coerce(struct stdio_handle *, stream->co.handle);
  cnum l = c_num(len, self);
#if HAVE_FTRUNCATE
  typedef off_t trunc_off_t;
  int (*truncfun)(int, off_t) = ftruncate;
#else
  typedef long trunc_off_t;
  int (*truncfun)(int, long) = chsize;
#endif

  if (convert(cnum, convert(trunc_off_t, l)) != l)
    uw_throwf(error_s, lit("truncate-stream: ~s is too large"), len, nao);

  return (h->f != 0 && truncfun(fileno(h->f), l) == 0)
         ? t
         : stdio_maybe_error(stream, lit("truncating"));
}
#else
#define stdio_truncate unimpl_truncate
#endif

#if HAVE_SOCKETS

static val sock_get_prop(val stream, val ind)
{
  if (ind == addr_k) {
    struct stdio_handle *h = coerce(struct stdio_handle *, stream->co.handle);
    return h->addr;
  }

  if (ind == name_k) {
    struct stdio_handle *h = coerce(struct stdio_handle *, stream->co.handle);

    if (!h->f)
      return h->descr = lit("closed");

    if (h->descr)
      return h->descr;

    if (h->addr)
      return set(mkloc(h->descr, stream),
                 format(nil, lit("passive ~s"), h->addr, nao));

    if (h->peer)
      return set(mkloc(h->descr, stream),
                 format(nil, lit("active ~s"), h->peer, nao));

    return h->descr = lit("disconnected");
  }

  return stdio_get_prop(stream, ind);
}

static val sock_set_prop(val stream, val ind, val prop)
{
  if (ind == addr_k) {
    struct stdio_handle *h = coerce(struct stdio_handle *, stream->co.handle);
    set(mkloc(h->addr, stream), prop);
    h->descr = nil;
    return t;
  }

  return stdio_set_prop(stream, ind, prop);
}

static val stdio_get_sock_family(val stream)
{
  struct stdio_handle *h = coerce(struct stdio_handle *, stream->co.handle);
  return h->family;
}

static val stdio_get_sock_type(val stream)
{
  struct stdio_handle *h = coerce(struct stdio_handle *, stream->co.handle);
  return h->type;
}

static val stdio_get_sock_peer(val stream)
{
  struct stdio_handle *h = coerce(struct stdio_handle *, stream->co.handle);
  return h->peer;
}

static val stdio_set_sock_peer(val stream, val peer)
{
  struct stdio_handle *h = coerce(struct stdio_handle *, stream->co.handle);
  h->descr = nil;
  return set(mkloc(h->peer, stream), peer);
}

#endif

static struct strm_ops stdio_ops =
  strm_ops_init(cobj_ops_init(eq,
                              stdio_stream_print,
                              stdio_stream_destroy,
                              stdio_stream_mark,
                              cobj_eq_hash_op),
                wli("file-stream"),
                stdio_put_string,
                stdio_put_char,
                stdio_put_byte,
                generic_get_line,
                stdio_get_char,
                stdio_get_byte,
                stdio_unget_char,
                stdio_unget_byte,
                stdio_put_buf,
                stdio_fill_buf,
                stdio_close,
                stdio_flush,
                stdio_seek,
                stdio_truncate,
                stdio_get_prop,
                stdio_set_prop,
                stdio_get_error,
                stdio_get_error_str,
                stdio_clear_error,
                stdio_get_fd);

#if HAVE_SOCKETS
static struct strm_ops stdio_sock_ops;
#endif

static FILE *w_fopen_mode(const wchar_t *wname, const wchar_t *mode,
                          const struct stdio_mode m)
{
#if HAVE_FCNTL
  char *name = utf8_dup_to(wname);
  size_t nsiz = strlen(name) + 1;
  int flags = (if3(m.read && m.write, O_RDWR, 0) |
               if3(m.read && !m.write, O_RDONLY, 0) |
               if3(!m.read && m.write,
                   if3(!m.notrunc, O_TRUNC, 0) | O_WRONLY | O_CREAT, 0) |
               if3(m.append, O_APPEND, 0) |
               if3(m.nonblock, O_NONBLOCK, 0));
  char *stkname = coerce(char *, alloca(nsiz));
  int fd;

  memcpy(stkname, name, nsiz);
  free(name);

  sig_save_enable;
  fd = open(stkname, flags, 0666);
  sig_restore_enable;

  return (fd < 0) ? NULL : w_fdopen(fd, mode);
#else
  if (m.notrunc || m.nonblock)
    uw_throwf(file_error_s,
              lit("open-file: specified mode not supported on this system"),
                  nao);
  return w_fopen(wname, mode);
#endif
}


static void tail_calc(unsigned long *state, int *usec, int *mod)
{
  unsigned long count = (*state)++;
  if (count > 32)
    count = 32;
  *usec = 1048576 << (count / 8);
  *mod = 8 >> (count / 8);
  if (*mod == 0)
    *mod = 1;
}

static void tail_strategy(val stream, unsigned long *state)
{
  struct stdio_handle *h = coerce(struct stdio_handle *, stream->co.handle);
  int usec = 0, mod = 0;
  val mode = nil;
  struct stdio_mode m, m_r = stdio_mode_init_r;

  tail_calc(state, &usec, &mod);

  if (h->is_rotated) {
    /* We already know that the file has rotated. The caller
     * has read through to the end of the old open file, and
     * so we can close it now and open a new file.
     */
    fclose(h->f);
    h->f = 0;
    h->is_rotated = 0;
  } else if (h->f != 0) {
    /* We have a file and it hasn't rotated; so sleep on it. */
    sig_save_enable;
    usleep_wrap(num(usec));
    sig_restore_enable;
  }

  /* If the state indicates we should poll for a file rotation,
   * or we have no file ...
   */
  if (h->f == 0 || *state % mod == 0) {
    long save_pos = 0, size;

    if (h->f != 0 && (save_pos = ftell(h->f)) == -1)
      return;

    for (;;) {
      FILE *newf;

      if (!mode)
        mode = normalize_mode(&m, h->mode, m_r);

      /* Try to open the file.
       */
      if (!(newf = w_fopen_mode(c_str(h->descr), c_str(mode), m))) {
        /* If already have the file open previously, and the name
         * does not open any more, then the file has rotated.
         * Have the caller try to read the last bit of data
         * from the old h->f.
         */
        if (h->f) {
          h->is_rotated = 1;
          return;
        }

        /* Unable to open; keep trying. */
        tail_calc(state, &usec, &mod);
        sig_save_enable;
        usleep_wrap(num(usec));
        sig_restore_enable;
        continue;
      }

      /* We opened the new file. If we have no old file,
       * then this is all we have.
       */
      if (!h->f) {
        h->f = newf;
#if CONFIG_STDIO_STRICT
        h->last_op = stdio_none;
#endif
        (void) set_mode_props(m, stream);
        break;
      }

      /* Obtain size of new file. If we can't, then let's just pretend we never
       * opened it and bail out.
       */
      if (fseek(newf, 0, SEEK_END) == -1 || (size = ftell(newf)) == -1) {
        fclose(newf);
        return;
      }

      /* The newly opened file is smaller than the previously opened
       * file. The file has rotated and is quite possibly a new object.
       * We just close newf, and let the caller read the last bit of data from
       * the old stream before cutting over.
       */
      if (size < save_pos) {
        /* TODO: optimize: keep newf in the handle so as not to have to
           re-open it again. */
        h->is_rotated = 1;
        fclose(newf);
        return;
      }

      /* Newly opened file is not smaller. We take a gamble and say
       * that it's the same object as h->f, since rotating files
       * are usually large and it is unlikely that it is a new file
       * which has grown larger than the original. Just in case,
       * though, we take the new file handle. But we do not reset
       * the UTF8 machine.
       */
      if (save_pos)
        fseek(newf, save_pos, SEEK_SET);
      fclose(h->f);
      h->f = newf;
#if CONFIG_STDIO_STRICT
      h->last_op = stdio_none;
#endif
      (void) set_mode_props(m, stream);
      return;
    }

    utf8_decoder_init(&h->ud);
  }
}

static val tail_get_line(val stream)
{
  unsigned long state = 0;
  val ret;

  while ((ret = generic_get_line(stream)) == nil)
    tail_strategy(stream, &state);

  return ret;
}

static val tail_get_char(val stream)
{
  unsigned long state = 0;
  val ret;

  while ((ret = stdio_get_char(stream)) == nil)
    tail_strategy(stream, &state);

  return ret;
}

static val tail_get_byte(val stream)
{
  unsigned long state = 0;
  val ret;

  while ((ret = stdio_get_byte(stream)) == nil)
    tail_strategy(stream, &state);

  return ret;
}

static struct strm_ops tail_ops =
  strm_ops_init(cobj_ops_init(eq,
                              stdio_stream_print,
                              stdio_stream_destroy,
                              stdio_stream_mark,
                              cobj_eq_hash_op),
                wli("tail-stream"),
                stdio_put_string,
                stdio_put_char,
                stdio_put_byte,
                tail_get_line,
                tail_get_char,
                tail_get_byte,
                stdio_unget_char,
                stdio_unget_byte,
                stdio_put_buf,
                stdio_fill_buf,
                stdio_close,
                stdio_flush,
                stdio_seek,
                stdio_truncate,
                stdio_get_prop,
                stdio_set_prop,
                stdio_get_error,
                stdio_get_error_str,
                stdio_clear_error,
                stdio_get_fd);

#if HAVE_FORK_STUFF
static int pipevp_close(FILE *f, pid_t pid)
{
  int status;
  fclose(f);
  sig_save_enable;
  while (waitpid(pid, &status, 0) == -1 && errno == EINTR)
    ;
  sig_restore_enable;
  return status;
}
#endif

static int se_pclose(FILE *f)
{
  int ret;
  sig_save_enable;
  ret = pclose(f);
  sig_restore_enable;
  return ret;
}

static val pipe_close(val stream, val throw_on_error)
{
  struct stdio_handle *h = coerce(struct stdio_handle *, stream->co.handle);

  if (h->f != 0) {
#if HAVE_FORK_STUFF
    int status = h->pid != 0 ? pipevp_close(h->f, h->pid) : se_pclose(h->f);
#else
    int status = se_pclose(h->f);
#endif
    h->f = 0;

    if (status < 0) {
      if (throw_on_error)
        uw_throwf(process_error_s,
                  lit("unable to obtain status of command ~s: ~d/~s"),
                  stream, num(errno), errno_to_str(errno), nao);
    } else {
#if HAVE_SYS_WAIT
      if (throw_on_error) {
        if (WIFSIGNALED(status)) {
          int termsig = WTERMSIG(status);
          uw_throwf(process_error_s, lit("pipe ~s terminated by signal ~a"),
                    stream, num(termsig), nao);
#ifndef WIFCONTINUED
#define WIFCONTINUED(X) 0
#endif
        } else if (WIFSTOPPED(status) || WIFCONTINUED(status)) {
          uw_throwf(process_error_s,
                    lit("processes of closed pipe ~s still running"),
                    stream, nao);
        }
      }
      if (WIFEXITED(status)) {
        int exitstatus = WEXITSTATUS(status);
        return num(exitstatus);
      }
#else
      if (status != 0 && throw_on_error)
        uw_throwf(process_error_s, lit("closing pipe ~s failed"), stream, nao);
#endif
      return status == 0 ? zero : nil;
    }
  }
  return nil;
}

static struct strm_ops pipe_ops =
  strm_ops_init(cobj_ops_init(eq,
                              stdio_stream_print,
                              stdio_stream_destroy,
                              stdio_stream_mark,
                              cobj_eq_hash_op),
                wli("pipe-stream"),
                stdio_put_string,
                stdio_put_char,
                stdio_put_byte,
                generic_get_line,
                stdio_get_char,
                stdio_get_byte,
                stdio_unget_char,
                stdio_unget_byte,
                stdio_put_buf,
                stdio_fill_buf,
                pipe_close,
                stdio_flush,
                0, /* seek: not on pipes */
                0, /* truncate: not on pipes */
                stdio_get_prop,
                stdio_set_prop,
                stdio_get_error,
                stdio_get_error_str,
                stdio_clear_error,
                stdio_get_fd);

static struct stdio_mode do_parse_mode(val mode_str, struct stdio_mode m_dfl)
{
  struct stdio_mode m = stdio_mode_init_blank;
  const wchar_t *ms = c_str(default_arg(mode_str, lit("")));
  int nredir = 0;

  switch (*ms) {
  case 'r':
    ms++;
    m.read = 1;
    break;
  case 'w':
    ms++;
    m.write = 1;
    m.create = 1;
    break;
  case 'a':
    ms++;
    m.write = 1;
    m.append = 1;
    m.notrunc = 1;
    break;
  case 'm':
    ms++;
    m.write = 1;
    m.create = 1;
    m.notrunc = 1;
    break;
  default:
    break;
  }

  if (*ms == '+') {
    ms++;
    if (m.read)
      m.write = 1;
    m.read = 1;
  }

  if (!m.read && !m.write)
    m = m_dfl;

  for (; *ms; ms++) {
    switch (*ms) {
    case 'b':
      m.binary = 1;
      break;
    case 'i':
      m.interactive = 1;
      break;
    case 'l':
      if (m.unbuf) {
        m.malformed = 1;
        return m;
      }
      m.linebuf = 1;
      break;
    case 'u':
      if (m.linebuf) {
        m.malformed = 1;
        return m;
      }
      m.unbuf = 1;
      break;
    case 'n':
      m.nonblock = 1;
      break;
    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
      if (m.unbuf) {
        m.malformed = 1;
        return m;
      }
      m.buforder = *ms - '0';
      break;
    case '>':
      if (nredir >= STDIO_MODE_NREDIRS) {
        m.malformed = 1;
        return m;
      }

      if (ms[1] != '(') {
        if (!isdigit((unsigned char) ms[1]) || !ms[2]) {
          m.malformed = 1;
          return m;
        }

        m.redir[nredir][0] = ms[1] - '0';
        if (isdigit((unsigned char) ms[2])) {
          m.redir[nredir][1] = ms[2] - '0';
        } else switch (ms[2]) {
        case 'n': case 'x':
          m.redir[nredir][1] = ms[2];
          break;
        default:
          m.malformed = 1;
          return m;
        }

        ms += 2;
        nredir++;
        break;
      }

      {
        char code[2];
        char rpar[2];
        unsigned to, from;

        if (swscanf(ms, L">(%u %[nx]%[)]", &to, code, rpar) == 3) {
          m.redir[nredir][0] = to;
          m.redir[nredir][1] = code[0];
        } else if (swscanf(ms, L">(%u %u%[)]", &to, &from, rpar) == 3) {
          m.redir[nredir][0] = to;
          m.redir[nredir][1] = from;
        } else {
          m.malformed = 1;
          return m;
        }

        ms = wcschr(ms, ')');
        nredir++;
        break;
      }
    default:
      m.malformed = 1;
      return m;
    }
  }

  if (nredir < STDIO_MODE_NREDIRS)
    m.redir[nredir][0] = -1;

  return m;
}

struct stdio_mode parse_mode(val mode_str, struct stdio_mode m_dfl)
{
  struct stdio_mode m = do_parse_mode(mode_str, m_dfl);
  if (m.malformed)
    uw_throwf(file_error_s, lit("invalid mode string ~s"), mode_str, nao);
  return m;
}

static val format_mode(const struct stdio_mode m)
{
  wchar_t buf[8], *ptr = buf;

  if (m.malformed)
    return lit("###");

  if (m.append) {
    *ptr++ = 'a';
    if (m.read)
      *ptr++ = '+';
  } else if (m.create) {
    *ptr++ = 'w';
    if (m.read)
      *ptr++ = '+';
  } else {
    *ptr++ = 'r';
    if (m.write)
      *ptr++ = '+';
  }

  if (m.binary)
    *ptr++ = 'b';

#ifdef __CYGWIN__
  if (!m.binary && (opt_compat == 144 || opt_compat == 145))
    *ptr++ = 't';
#endif

  *ptr = 0;
  return string(buf);
}

val normalize_mode(struct stdio_mode *m, val mode_str, struct stdio_mode m_dfl)
{
  *m = do_parse_mode(mode_str, m_dfl);

  if (m->malformed)
    uw_throwf(file_error_s, lit("invalid file open mode ~s"), mode_str, nao);

  return format_mode(*m);
}

val normalize_mode_no_bin(struct stdio_mode *m, val mode_str, struct stdio_mode m_dfl)
{
#ifdef __CYGWIN__
  return normalize_mode(m, mode_str, m_dfl);
#else
  *m = do_parse_mode(mode_str, m_dfl);

  if (m->malformed)
    uw_throwf(file_error_s, lit("invalid file open mode ~s"), mode_str, nao);

  m->binary = 0;

  return format_mode(*m);
#endif
}

val set_mode_props(const struct stdio_mode m, val stream)
{
  if (m.interactive || m.linebuf || m.unbuf || m.buforder != -1) {
    struct stdio_handle *h = coerce(struct stdio_handle *, stream->co.handle);

    if (h->f) {
      int size = m.buforder == -1 ? 0 : 1024 << m.buforder;

      if (h->buf) {
        free(h->buf);
        h->buf = 0;
      }

      if (size)
        h->buf = coerce(char *, chk_malloc(size));

      if (m.write && (m.linebuf || (m.interactive && !m.unbuf)))
        setvbuf(h->f, h->buf, _IOLBF, size);
      else if (m.unbuf)
        setbuf(h->f, 0);
      else if (size)
        setvbuf(h->f, h->buf, _IOFBF, size);
    }

    if (m.interactive)
      stream_set_prop(stream, real_time_k, t);
  }
  return stream;
}

static val make_stdio_stream_common(FILE *f, val descr, struct cobj_ops *ops)
{
  struct stdio_handle *h = coerce(struct stdio_handle *, chk_malloc(sizeof *h));
  val stream = cobj(coerce(mem_t *, h), stdio_stream_s, ops);
  strm_base_init(&h->a);
  h->f = f;
  h->descr = descr;
  h->unget_c = nil;
  utf8_decoder_init(&h->ud);
  h->err = nil;
  h->buf = 0;
  h->pid = 0;
  h->mode = nil;
  h->is_rotated = 0;
#if HAVE_ISATTY
  h->is_real_time = if3(opt_compat && opt_compat <= 105,
                        (h->f != 0 && isatty(fileno(h->f)) == 1), 0);
#else
  h->is_real_time = 0;
#endif
  h->is_byte_oriented = 0;
#if CONFIG_STDIO_STRICT
  h->last_op = stdio_none;
#endif
#if HAVE_SOCKETS
  h->family = nil;
  h->type = nil;
  h->peer = nil;
  h->addr = nil;
#endif
  return stream;
}

val make_stdio_stream(FILE *f, val descr)
{
  return make_stdio_stream_common(f, descr, &stdio_ops.cobj_ops);
}

val make_tail_stream(FILE *f, val descr)
{
  val stream = make_stdio_stream_common(f, descr, &tail_ops.cobj_ops);
  stream_set_prop(stream, real_time_k, t);
  return stream;
}

val make_pipe_stream(FILE *f, val descr)
{
  return make_stdio_stream_common(f, descr, &pipe_ops.cobj_ops);
}

#if HAVE_SOCKETS
val make_sock_stream(FILE *f, val family, val type)
{
  val s = make_stdio_stream_common(f, nil, &stdio_sock_ops.cobj_ops);
  struct stdio_handle *h = coerce(struct stdio_handle *, s->co.handle);
  h->family = family;
  h->type = type;
  return s;
}
#endif

val stream_fd(val stream)
{
  val self = lit("fileno");
  struct strm_ops *ops = coerce(struct strm_ops *,
                                cobj_ops(self, stream, stream_s));
  return ops->get_fd(stream);
}

#if HAVE_SOCKETS
val sock_family(val stream)
{
  val self = lit("sock-family");
  struct strm_ops *ops = coerce(struct strm_ops *,
                                cobj_ops(self, stream, stream_s));
  return ops->get_sock_family(stream);
}

val sock_type(val stream)
{
  val self = lit("sock-type");
  struct strm_ops *ops = coerce(struct strm_ops *,
                                cobj_ops(self, stream, stream_s));
  return ops->get_sock_type(stream);
}

val sock_peer(val stream)
{
  val self = lit("sock-peer");
  struct strm_ops *ops = coerce(struct strm_ops *,
                                cobj_ops(self, stream, stream_s));
  return ops->get_sock_peer(stream);
}

val sock_set_peer(val stream, val peer)
{
  val self = lit("sock-set-peer");
  struct strm_ops *ops = coerce(struct strm_ops *,
                                cobj_ops(self, stream, stream_s));
  return ops->set_sock_peer(stream, peer);
}
#endif

#if HAVE_FORK_STUFF
static val make_pipevp_stream(FILE *f, val descr, pid_t pid)
{
  val stream = make_stdio_stream_common(f, descr, &pipe_ops.cobj_ops);
  struct stdio_handle *h = coerce(struct stdio_handle *, stream->co.handle);
  h->pid = pid;
  return stream;
}
#endif

struct dir_handle {
  struct strm_base a;
  DIR *d;
  val err;
};

static void dir_destroy(val stream)
{
  struct dir_handle *h = coerce(struct dir_handle *, stream->co.handle);
  strm_base_cleanup(&h->a);
  close_stream(stream, nil);
  free(h);
}

static void dir_mark(val stream)
{
  struct dir_handle *h = coerce(struct dir_handle *, stream->co.handle);
  strm_base_mark(&h->a);
  gc_mark(h->err);
}

static val dir_get_line(val stream)
{
  struct dir_handle *h = coerce(struct dir_handle *, stream->co.handle);

  if (h->d == 0) {
    return nil;
  } else {
    for (;;) {
      struct dirent *e = readdir(h->d);
      if (!e) {
        h->err = num(errno);
        return nil;
      }
      if (!strcmp(e->d_name, ".") || !strcmp(e->d_name, ".."))
        continue;
      return string_utf8(e->d_name);
    }
  }
}

static val dir_close(val stream, val throw_on_error)
{
  struct dir_handle *h = coerce(struct dir_handle *, stream->co.handle);
  (void) throw_on_error;

  if (h->d != 0) {
    closedir(coerce(DIR *, h->d));
    h->d = 0;
  }

  return nil;
}

static val dir_get_error(val stream)
{
  struct dir_handle *h = coerce(struct dir_handle *, stream->co.handle);
  return h->err;
}

static val dir_get_error_str(val stream)
{
  struct dir_handle *h = coerce(struct dir_handle *, stream->co.handle);
  return errno_to_string(h->err);
}

static val dir_clear_error(val stream)
{
  struct dir_handle *h = coerce(struct dir_handle *, stream->co.handle);
  val ret = h->err;
  h->err = nil;
  return ret;
}

static struct strm_ops dir_ops =
  strm_ops_init(cobj_ops_init(eq,
                              stream_print_op,
                              dir_destroy,
                              dir_mark,
                              cobj_eq_hash_op),
                wli("dir-stream"),
                0, 0, 0,
                dir_get_line,
                0, 0, 0, 0, 0, 0,
                dir_close,
                0, 0, 0, 0, 0,
                dir_get_error,
                dir_get_error_str,
                dir_clear_error,
                0);

static val make_dir_stream(DIR *dir)
{
  struct dir_handle *h = coerce(struct dir_handle *, chk_malloc(sizeof *h));
  strm_base_init(&h->a);
  h->d = dir;
  h->err = nil;
  return cobj(coerce(mem_t *, h), stream_s, &dir_ops.cobj_ops);
}

struct string_in {
  struct strm_base a;
  val string;
  val pos;
};

static void string_in_stream_mark(val stream)
{
  struct string_in *s = coerce(struct string_in *, stream->co.handle);
  strm_base_mark(&s->a);
  gc_mark(s->string);
  gc_mark(s->pos);
}

static val find_char(val string, val start, val ch)
{
  for (; length_str_gt(string, start); start = succ(start))
    if (chr_str(string, start) == ch)
      return start;

  return nil;
}

static val string_in_get_line(val stream)
{
  struct string_in *s = coerce(struct string_in *, stream->co.handle);

  if (length_str_gt(s->string, s->pos)) {
    val nlpos = find_char(s->string, s->pos, chr('\n'));
    val result = sub_str(s->string, s->pos, nlpos);
    set(mkloc(s->pos, stream), nlpos ? plus(nlpos, one) : length_str(s->string));
    return result;
  }

  return nil;
}

static val string_in_get_char(val stream)
{
  struct string_in *s = coerce(struct string_in *, stream->co.handle);

  if (length_str_gt(s->string, s->pos)) {
    val pos = s->pos;
    set(mkloc(s->pos, stream), plus(pos, one));
    return chr_str(s->string, pos);
  }

  return nil;
}

static val string_in_unget_char(val stream, val ch)
{
  struct string_in *s = coerce(struct string_in *, stream->co.handle);
  val pos = s->pos;

  if (pos == zero)
    uw_throwf(file_error_s,
              lit("unget-char: cannot push past beginning of string"), nao);

  pos = minus(pos, one);

  if (chr_str(s->string, pos) != ch)
    uw_throwf(file_error_s,
              lit("unget-char: ~s doesn't match the character that was read"),
              ch, nao);

  set(mkloc(s->pos, stream), pos);
  return ch;
}

static val string_in_get_prop(val stream, val ind)
{
  if (ind == name_k) {
    struct string_in *s = coerce(struct string_in *, stream->co.handle);
    struct strm_ops *ops = coerce(struct strm_ops *, stream->co.ops);
    val name = static_str(ops->name);
    return format(nil, lit("~a ~s"), name, s->string, nao);
  }

  return nil;
}

static val string_in_get_error(val stream)
{
  struct string_in *s = coerce(struct string_in *, stream->co.handle);
  return if2(ge(s->pos, length_str(s->string)), t);
}

static val string_in_get_error_str(val stream)
{
  return if3(string_in_get_error(stream), lit("eof"), lit("no error"));
}

static struct strm_ops string_in_ops =
  strm_ops_init(cobj_ops_init(eq,
                              stream_print_op,
                              stream_destroy_op,
                              string_in_stream_mark,
                              cobj_eq_hash_op),
                wli("string-input-stream"),
                0, 0, 0,
                string_in_get_line,
                string_in_get_char,
                0,
                string_in_unget_char,
                0, 0, 0, 0, 0,
                0, /* TODO: seek */
                0, /* TODO: truncate */
                string_in_get_prop,
                0,
                string_in_get_error,
                string_in_get_error_str,
                0, 0);

val make_string_input_stream(val string)
{
  struct string_in *s = coerce(struct string_in *, chk_malloc(sizeof *s));
  strm_base_init(&s->a);
  s->string = string;
  s->pos = zero;
  return cobj(coerce(mem_t *, s), stream_s, &string_in_ops.cobj_ops);
}

struct byte_input {
  struct strm_base a;
  unsigned char *buf;
  size_t size;
  size_t index;
};

static void byte_in_stream_destroy(val stream)
{
  struct byte_input *bi = coerce(struct byte_input *, stream->co.handle);
  strm_base_cleanup(&bi->a);
  free(bi->buf);
  bi->buf = 0;
  free(bi);
}

static val byte_in_get_byte(val stream)
{
  struct byte_input *bi = coerce(struct byte_input *, stream->co.handle);

  if (bi->index < bi->size)
    return num(bi->buf[bi->index++]);
  return nil;
}

static val byte_in_unget_byte(val stream, int byte)
{
  struct byte_input *bi = coerce(struct byte_input *, stream->co.handle);

  if (bi->index == 0)
    uw_throwf(file_error_s,
              lit("unget-byte: cannot push past beginning of byte stream"),
              nao);

  bi->buf[--bi->index] = byte;
  return num_fast(byte);
}

static val byte_in_get_error(val stream)
{
  struct byte_input *bi = coerce(struct byte_input *, stream->co.handle);
  return if3(bi->index < bi->size, nil, t);
}

static val byte_in_get_error_str(val stream)
{
  return if3(byte_in_get_error(stream), lit("eof"), lit("no error"));
}

static struct strm_ops byte_in_ops =
  strm_ops_init(cobj_ops_init(eq,
                              stream_print_op,
                              byte_in_stream_destroy,
                              stream_mark_op,
                              cobj_eq_hash_op),
                wli("byte-input-stream"),
                0, 0, 0, 0, 0,
                byte_in_get_byte,
                0,
                byte_in_unget_byte, 0, 0,
                0, 0, 0, 0, 0, 0,
                byte_in_get_error,
                byte_in_get_error_str,
                0, 0);

val make_string_byte_input_stream(val string)
{
  type_assert (stringp(string), (lit("~a is not a string"), string, nao));

  {
    const wchar_t *wstring = c_str(string);
    struct byte_input *bi = coerce(struct byte_input *, chk_malloc(sizeof *bi));
    strm_base_init(&bi->a);
    bi->buf = utf8_dup_to_buf(wstring, &bi->size, 0);
    bi->index = 0;
    return cobj(coerce(mem_t *, bi), stream_s, &byte_in_ops.cobj_ops);
  }
}

struct strlist_in {
  struct strm_base a;
  val string;
  val pos;
  val list;
};

static void strlist_in_stream_mark(val stream)
{
  struct strlist_in *s = coerce(struct strlist_in *, stream->co.handle);
  strm_base_mark(&s->a);
  gc_mark(s->string);
  gc_mark(s->pos);
  gc_mark(s->list);
}

static val strlist_in_get_line(val stream)
{
  struct strlist_in *s = coerce(struct strlist_in *, stream->co.handle);

  if (s->string) {
    val result = sub_str(s->string, s->pos, t);
    s->string = pop(&s->list);
    s->pos = zero;
    return result;
  }

  return nil;
}

static val strlist_in_get_char(val stream)
{
  struct strlist_in *s = coerce(struct strlist_in *, stream->co.handle);

  if (!s->string)
    return nil;

  if (length_str_le(s->string, s->pos)) {
    s->string = pop(&s->list);
    s->pos = zero;
    return chr('\n');
  } else {
    val pos = s->pos;
    set(mkloc(s->pos, stream), plus(pos, one));
    return chr_str(s->string, pos);
  }

  return nil;
}

static val strlist_in_unget_char(val stream, val ch)
{
  struct strlist_in *s = coerce(struct strlist_in *, stream->co.handle);
  val pos = s->pos;

  if (pos == zero) {
    if (ch == chr('\n')) {
      if (s->string)
        mpush(s->string, mkloc(s->list, stream));
      s->string = null_string;
    } else {
      if (!s->string)
        goto mismatch;
      set(mkloc(s->string, stream), scat(nil, ch, s->string, nao));
    }
    return ch;
  }

  pos = minus(pos, one);

  if (chr_str(s->string, pos) != ch)
    goto mismatch;

  set(mkloc(s->pos, stream), pos);
  return ch;
mismatch:
  uw_throwf(file_error_s,
            lit("unget-char: ~s doesn't match the character that was read"),
            ch, nao);
}

static val strlist_in_get_prop(val stream, val ind)
{
  if (ind == name_k) {
    struct strlist_in *s = coerce(struct strlist_in *, stream->co.handle);
    struct strm_ops *ops = coerce(struct strm_ops *, stream->co.ops);
    val name = static_str(ops->name);
    return format(nil, lit("~a ~s"), name, s->string, nao);
  }

  return nil;
}

static val strlist_in_get_error(val stream)
{
  struct strlist_in *s = coerce(struct strlist_in *, stream->co.handle);
  return if2(!s->string, t);
}

static val strlist_in_get_error_str(val stream)
{
  return if3(strlist_in_get_error(stream), lit("eof"), lit("no error"));
}

static struct strm_ops strlist_in_ops =
  strm_ops_init(cobj_ops_init(eq,
                              stream_print_op,
                              stream_destroy_op,
                              strlist_in_stream_mark,
                              cobj_eq_hash_op),
                wli("strlist-input-stream"),
                0, 0, 0,
                strlist_in_get_line,
                strlist_in_get_char,
                0,
                strlist_in_unget_char,
                0, 0, 0, 0, 0,
                0, /* TODO: seek */
                0, /* TODO: truncate */
                strlist_in_get_prop,
                0,
                strlist_in_get_error,
                strlist_in_get_error_str,
                0, 0);

val make_strlist_input_stream(val list)
{
  struct strlist_in *s = coerce(struct strlist_in *, chk_malloc(sizeof *s));
  strm_base_init(&s->a);
  s->string = car(list);
  s->pos = zero;
  s->list = cdr(list);
  return cobj(coerce(mem_t *, s), stream_s, &strlist_in_ops.cobj_ops);
}

struct string_out {
  struct strm_base a;
  wchar_t *buf;
  size_t size;
  size_t fill;
  utf8_decoder_t ud;
  unsigned char byte_buf[4];
  int head, tail;
};

static void string_out_stream_destroy(val stream)
{
  struct string_out *so = coerce(struct string_out *, stream->co.handle);

  strm_base_cleanup(&so->a);
  free(so->buf);
  so->buf = 0;
  free(so);
}

static int string_out_byte_callback(mem_t *ctx)
{
  struct string_out *so = coerce(struct string_out *, ctx);
  if (so->tail >= so->head)
    return EOF;
  return so->byte_buf[so->tail++];
}

static void string_out_extracted_error(val stream)
{
  uw_throwf(file_error_s, lit("output not possible on ~s: string extracted"),
            stream, nao);
}

static val string_out_put_char(val stream, val ch);

static val string_out_byte_flush(struct string_out *so, val stream)
{
  val result = nil;

  if (so->tail < so->head) {
    wint_t ch = utf8_decode(&so->ud, string_out_byte_callback,
                            coerce(mem_t *, so));
    int remaining = so->head - so->tail;
    if (remaining != 0)
      memmove(so->byte_buf, so->byte_buf + so->tail, remaining);
    so->head = so->tail = remaining;
    utf8_decoder_init(&so->ud);
    if (ch == WEOF)
      internal_error("unexpected WEOF from utf8_decode");
    result = string_out_put_char(stream, chr(ch));
    so->tail = 0;
  }
  return result;
}

static val string_out_put_string(val stream, val str)
{
  val self = lit("put-string");
  struct string_out *so = coerce(struct string_out *, stream->co.handle);

  if (so->buf == 0)
    string_out_extracted_error(stream);

  while (so->head != so->tail)
    string_out_byte_flush(so, stream);

  {
    const wchar_t *s = c_str(str);
    size_t len = c_num(length_str(str), self);
    size_t old_size = so->size;
    size_t required_size = len + so->fill + 1;

    if (required_size < len)
      goto oflow;

    while (so->size <= required_size) {
      so->size *= 2;
      if (so->size < old_size)
        goto oflow;
    }

    if (so->size != old_size)
      so->buf = coerce(wchar_t *, chk_grow_vec(coerce(mem_t *, so->buf),
                                               old_size, so->size,
                                               sizeof *so->buf));
    wmemcpy(so->buf + so->fill, s, len + 1);
    so->fill += len;
    return t;
oflow:
    uw_throwf(error_s, lit("~a: string output stream overflow"), self, nao);
  }
}

static val string_out_put_char(val stream, val ch)
{
  wchar_t onech[] = wini(" ");
  wref(onech)[0] = c_chr(ch);
  return string_out_put_string(stream,
                               auto_str(coerce(const wchli_t *, wref(onech))));
}

static val string_out_put_byte(val stream, int ch)
{
  struct string_out *so = coerce(struct string_out *, stream->co.handle);

  if (so->buf == 0)
    string_out_extracted_error(stream);

  so->byte_buf[so->head++] = ch;

  if (so->head >= convert(int, sizeof so->byte_buf))
    return string_out_byte_flush(so, stream);

  return t;
}

static struct strm_ops string_out_ops =
  strm_ops_init(cobj_ops_init(eq,
                              stream_print_op,
                              string_out_stream_destroy,
                              stream_mark_op,
                              cobj_eq_hash_op),
                wli("string-output-stream"),
                string_out_put_string,
                string_out_put_char,
                string_out_put_byte,
                0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, /* TODO: seek; fill-with-spaces semantics if past end. */
                0,
                0, 0, 0, 0, 0, 0);

val make_string_output_stream(void)
{
  struct string_out *so = coerce(struct string_out *, chk_malloc(sizeof *so));
  strm_base_init(&so->a);
  so->size = 128;
  so->buf = chk_wmalloc(so->size);
  so->fill = 0;
  so->buf[0] = 0;
  utf8_decoder_init(&so->ud);
  so->head = so->tail = 0;
  return cobj(coerce(mem_t *, so), stream_s, &string_out_ops.cobj_ops);
}

val get_string_from_stream(val stream)
{
  val self = lit("get-string-from-stream");
  struct string_out *so = coerce(struct string_out *,
                                 cobj_handle(self, stream, stream_s));

  if (stream->co.ops == &string_out_ops.cobj_ops) {
    val out = nil;

    if (!so->buf)
      return out;

    while (so->head != so->tail)
      out = string_out_byte_flush(so, stream);

    /* Trim to actual size */
    so->buf = coerce(wchar_t *, chk_realloc(coerce(mem_t *, so->buf),
                                            (so->fill + 1) * sizeof *so->buf));
    out = string_own(so->buf);
    so->buf = 0;
    return out;
  } else {
    type_assert (stream->co.ops == &string_in_ops.cobj_ops,
                 (lit("~a is not a string stream"), stream, nao));
    {
      struct string_in *si = coerce(struct string_in *, stream->co.handle);
      return si->string;
    }
  }
}

struct strlist_out {
  struct strm_base a;
  val lines;
  val strstream;
};

static void strlist_out_mark(val stream)
{
  struct strlist_out *s = coerce(struct strlist_out *, stream->co.handle);
  strm_base_mark(&s->a);
  gc_mark(s->lines);
  gc_mark(s->strstream);
}

static val strlist_out_put_string(val stream, val str)
{
  struct strlist_out *s = coerce(struct strlist_out *, stream->co.handle);
  val lines = s->lines;
  val strstream = s->strstream;

  for (;;) {
    val length = length_str(str);
    val span_to_newline = compl_span_str(str, lit("\n"));

    if (zerop(length))
      break;

    put_string(sub_str(str, nil, span_to_newline), strstream);

    if (equal(span_to_newline, length))
      break;

    str = sub_str(str, plus(span_to_newline, num(1)), nil);
    push(get_string_from_stream(strstream), &lines);
    strstream = make_string_output_stream();
  }

  if (s->lines != lines) {
    set(mkloc(s->lines, stream), lines);
    set(mkloc(s->strstream, stream), strstream);
  }

  return t;
}

static val strlist_out_put_char(val stream, val ch)
{
  struct strlist_out *s = coerce(struct strlist_out *, stream->co.handle);
  val lines = s->lines;
  val strstream = s->strstream;

  if (ch == chr('\n')) {
    push(get_string_from_stream(strstream), &lines);
    strstream = make_string_output_stream();
    set(mkloc(s->lines, stream), lines);
    set(mkloc(s->strstream, stream), strstream);
  } else {
    put_char(ch, strstream);
  }

  return t;
}

static struct strm_ops strlist_out_ops =
  strm_ops_init(cobj_ops_init(eq,
                              stream_print_op,
                              stream_destroy_op,
                              strlist_out_mark,
                              cobj_eq_hash_op),
                wli("strlist-output-stream"),
                strlist_out_put_string,
                strlist_out_put_char,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);

val make_strlist_output_stream(void)
{
  struct strlist_out *s = coerce(struct strlist_out *, chk_malloc(sizeof *s));
  val stream;
  val strstream = make_string_output_stream();
  strm_base_init(&s->a);
  s->lines = nil;
  s->strstream = nil;
  stream = cobj(coerce(mem_t *, s), stream_s, &strlist_out_ops.cobj_ops);
  s->strstream = strstream;
  return stream;
}

val get_list_from_stream(val stream)
{
  val self = lit("get-list-from-stream");
  struct strlist_out *s = coerce(struct strlist_out *,
                                 cobj_handle(self, stream, stream_s));

  if (stream->co.ops == &strlist_out_ops.cobj_ops) {
    val stray = get_string_from_stream(s->strstream);
    val lines = s->lines;
    if (!zerop(length_str(stray)))
      push(stray, &lines);
    return nreverse(lines);
  }

  type_mismatch(lit("~s is not a string list stream"), stream);
}

struct cat_strm {
  struct strm_base a;
  val streams;
};

static void cat_stream_print(val stream, val out, val pretty,
                             struct strm_ctx *ctx)
{
  struct cat_strm *s = coerce(struct cat_strm *, stream->co.handle);
  struct strm_ops *ops = coerce(struct strm_ops *, stream->co.ops);
  val name = static_str(ops->name);

  (void) pretty;
  (void) ctx;

  format(out, lit("#<~a ~s>"), name, s->streams, nao);
}

static val cat_get_line(val stream)
{
  struct cat_strm *s = coerce(struct cat_strm *, stream->co.handle);
  val streams = s->streams;

  while (streams) {
    val fs = first(streams);
    val line = get_line(fs);
    if (line)
      return line;
    if ((streams = rest(streams)) != nil) {
      close_stream(fs, t);
      set(mkloc(s->streams, stream), streams);
    }
  }

  return nil;
}

static val cat_get_char(val stream)
{
  struct cat_strm *s = coerce(struct cat_strm *, stream->co.handle);
  val streams = s->streams;

  while (streams) {
    val fs = first(streams);
    val ch = get_char(fs);
    if (ch)
      return ch;
    if ((streams = rest(streams)) != nil) {
      close_stream(fs, t);
      set(mkloc(s->streams, stream), streams);
    }
  }

  return nil;
}

static val cat_get_byte(val stream)
{
  struct cat_strm *s = coerce(struct cat_strm *, stream->co.handle);
  val streams = s->streams;

  while (streams) {
    val fs = first(streams);
    val byte = get_byte(fs);
    if (byte)
      return byte;
    if ((streams = rest(streams)) != nil) {
      close_stream(fs, t);
      set(mkloc(s->streams, stream), streams);
    }
  }

  return nil;
}

static val cat_unget_byte(val stream, int byte)
{
  struct cat_strm *s = coerce(struct cat_strm *, stream->co.handle);

  if (!s->streams) {
    uw_throwf(file_error_s,
              lit("unget-byte on catenated stream ~s: stream list empty"),
              stream, nao);
  } else {
    val stream = car(s->streams);
    return unget_byte(num_fast(byte), stream);
  }

  return nil;
}

static val cat_unget_char(val stream, val ch)
{
  struct cat_strm *s = coerce(struct cat_strm *, stream->co.handle);

  if (!s->streams) {
    uw_throwf(file_error_s,
              lit("unget-char on catenated stream ~s: stream list empty"),
              stream, nao);
  } else {
    val stream = car(s->streams);
    return unget_char(ch, stream);
  }
}

static val cat_get_prop(val stream, val ind)
{
  struct cat_strm *s = coerce(struct cat_strm *, stream->co.handle);

  if (s->streams)
    return stream_get_prop(first(s->streams), ind);

  return nil;
}

static void cat_mark(val stream)
{
  struct cat_strm *s = coerce(struct cat_strm *, stream->co.handle);
  strm_base_mark(&s->a);
  gc_mark(s->streams);
}

static val cat_get_error(val stream)
{
  struct cat_strm *s = coerce(struct cat_strm *, stream->co.handle);
  return if3(s->streams, get_error(first(s->streams)), t);
}

static val cat_get_error_str(val stream)
{
  struct cat_strm *s = coerce(struct cat_strm *, stream->co.handle);
  return if3(s->streams, get_error_str(first(s->streams)), lit("eof"));
}

static val cat_clear_error(val stream)
{
  struct cat_strm *s = coerce(struct cat_strm *, stream->co.handle);
  return if2(s->streams, clear_error(first(s->streams)));
}

static struct strm_ops cat_stream_ops =
  strm_ops_init(cobj_ops_init(eq,
                              cat_stream_print,
                              stream_destroy_op,
                              cat_mark,
                              cobj_eq_hash_op),
                wli("catenated-stream"),
                0, 0, 0,
                cat_get_line,
                cat_get_char,
                cat_get_byte,
                cat_unget_char,
                cat_unget_byte,
                0, 0,
                0, 0, 0, 0,
                cat_get_prop,
                0,
                cat_get_error,
                cat_get_error_str,
                cat_clear_error,
                0);

val make_catenated_stream(val stream_list)
{
  struct cat_strm *s = coerce(struct cat_strm *, chk_malloc(sizeof *s));
  val catstrm = nil;
  strm_base_init(&s->a);
  s->streams = nil;
  catstrm = cobj(coerce(mem_t *, s), stream_s, &cat_stream_ops.cobj_ops);
  s->streams = stream_list;
  return catstrm;
}

val make_catenated_stream_v(struct args *streams)
{
  return make_catenated_stream(args_get_list(streams));
}

val catenated_stream_p(val obj)
{
  return if2(streamp(obj), tnil(obj->co.ops == &cat_stream_ops.cobj_ops));
}

val catenated_stream_push(val new_stream, val cat_stream)
{
  type_assert (streamp(new_stream),
               (lit("~a is not a stream"), new_stream, nao));
  type_assert (catenated_stream_p(cat_stream),
               (lit("~a is not a stream"), cat_stream, nao));

  {
    struct cat_strm *s = coerce(struct cat_strm *, cat_stream->co.handle);
    mpush(new_stream, mkloc(s->streams, cat_stream));
    return nil;
  }
}

struct delegate_base {
  struct strm_base a;
  val target_stream;
  struct strm_ops *target_ops;
};

static void delegate_base_mark(struct delegate_base *db)
{
  strm_base_mark(&db->a);
  gc_mark(db->target_stream);
}

static val delegate_put_string(val stream, val str)
{
  struct delegate_base *s = coerce(struct delegate_base *, stream->co.handle);
  return s->target_ops->put_string(s->target_stream, str);
}

static val delegate_put_char(val stream, val ch)
{
  struct delegate_base *s = coerce(struct delegate_base *, stream->co.handle);
  return s->target_ops->put_char(s->target_stream, ch);
}

static val delegate_put_byte(val stream, int byte)
{
  struct delegate_base *s = coerce(struct delegate_base *, stream->co.handle);
  return s->target_ops->put_byte(s->target_stream, byte);
}

static val delegate_get_char(val stream)
{
  struct delegate_base *s = coerce(struct delegate_base *, stream->co.handle);
  return s->target_ops->get_char(s->target_stream);
}

static val delegate_get_byte(val stream)
{
  struct delegate_base *s = coerce(struct delegate_base *, stream->co.handle);
  return s->target_ops->get_byte(s->target_stream);
}

static val delegate_unget_char(val stream, val ch)
{
  struct delegate_base *s = coerce(struct delegate_base *, stream->co.handle);
  return s->target_ops->unget_char(s->target_stream, ch);
}

static val delegate_unget_byte(val stream, int byte)
{
  struct delegate_base *s = coerce(struct delegate_base *, stream->co.handle);
  return s->target_ops->unget_byte(s->target_stream, byte);
}

static ucnum delegate_put_buf(val stream, mem_t *ptr, ucnum len, ucnum pos)
{
  struct delegate_base *s = coerce(struct delegate_base *, stream->co.handle);
  return s->target_ops->put_buf(s->target_stream, ptr, len, pos);
}

static ucnum delegate_fill_buf(val stream, mem_t *ptr, ucnum len, ucnum pos)
{
  struct delegate_base *s = coerce(struct delegate_base *, stream->co.handle);
  return s->target_ops->fill_buf(s->target_stream, ptr, len, pos);
}

static val delegate_close(val stream, val throw_on_error)
{
  struct delegate_base *s = coerce(struct delegate_base *, stream->co.handle);
  return s->target_ops->close(s->target_stream, throw_on_error);
}

static val delegate_flush(val stream)
{
  struct delegate_base *s = coerce(struct delegate_base *, stream->co.handle);
  return s->target_ops->flush(s->target_stream);
}

static val delegate_seek(val stream, val off, enum strm_whence whence)
{
  struct delegate_base *s = coerce(struct delegate_base *, stream->co.handle);
  return s->target_ops->seek(s->target_stream, off, whence);
}

static val delegate_truncate(val stream, val len)
{
  struct delegate_base *s = coerce(struct delegate_base *, stream->co.handle);
  return s->target_ops->truncate(s->target_stream, len);
}

static val delegate_get_prop(val stream, val ind)
{
  struct delegate_base *s = coerce(struct delegate_base *, stream->co.handle);
  return s->target_ops->get_prop(s->target_stream, ind);
}

static val delegate_set_prop(val stream, val ind, val value)
{
  struct delegate_base *s = coerce(struct delegate_base *, stream->co.handle);
  return s->target_ops->set_prop(s->target_stream, ind, value);
}

static val delegate_get_error(val stream)
{
  struct delegate_base *s = coerce(struct delegate_base *, stream->co.handle);
  return s->target_ops->get_error(s->target_stream);
}

static val delegate_get_error_str(val stream)
{
  struct delegate_base *s = coerce(struct delegate_base *, stream->co.handle);
  return s->target_ops->get_error_str(s->target_stream);
}

static val delegate_clear_error(val stream)
{
  struct delegate_base *s = coerce(struct delegate_base *, stream->co.handle);
  return s->target_ops->clear_error(s->target_stream);
}

static val delegate_get_fd(val stream)
{
  struct delegate_base *s = coerce(struct delegate_base *, stream->co.handle);
  return s->target_ops->get_fd(s->target_stream);
}

#if HAVE_SOCKETS

static val delegate_get_sock_family(val stream)
{
  struct delegate_base *s = coerce(struct delegate_base *, stream->co.handle);
  return s->target_ops->get_sock_family(s->target_stream);
}

static val delegate_get_sock_type(val stream)
{
  struct delegate_base *s = coerce(struct delegate_base *, stream->co.handle);
  return s->target_ops->get_sock_type(s->target_stream);
}

static val delegate_get_sock_peer(val stream)
{
  struct delegate_base *s = coerce(struct delegate_base *, stream->co.handle);
  return s->target_ops->get_sock_peer(s->target_stream);
}

static val delegate_set_sock_peer(val stream, val peer)
{
  struct delegate_base *s = coerce(struct delegate_base *, stream->co.handle);
  return s->target_ops->set_sock_peer(s->target_stream, peer);
}

#endif

static val make_delegate_stream(val self, val orig_stream, size_t handle_size,
                                struct cobj_ops *ops)
{
  struct strm_ops *orig_ops = coerce(struct strm_ops *,
                                     cobj_ops(self, orig_stream, stream_s));
  struct delegate_base *db = coerce(struct delegate_base *,
                                    chk_calloc(1, handle_size));
  val delegate_stream;

  strm_base_init(&db->a);
  db->target_stream = nil;
  db->target_ops = orig_ops;

  delegate_stream = cobj(coerce(mem_t *, db), stream_s, ops);

  db->target_stream = orig_stream;

  return delegate_stream;
}

struct record_adapter_base {
  struct delegate_base db;
  val regex;
  val include_match;
};

static void record_adapter_base_mark(struct record_adapter_base *rb)
{
  delegate_base_mark(&rb->db);
  gc_mark(rb->regex);
}

static void record_adapter_mark_op(val stream)
{
  struct record_adapter_base *rb = coerce(struct record_adapter_base *,
                                         stream->co.handle);
  record_adapter_base_mark(rb);
}

static val record_adapter_get_line(val stream)
{
  struct record_adapter_base *rb = coerce(struct record_adapter_base *,
                                         stream->co.handle);
  return read_until_match(rb->regex, rb->db.target_stream, rb->include_match);
}

static struct strm_ops record_adapter_ops =
  strm_ops_init(cobj_ops_init(eq,
                              stream_print_op,
                              stream_destroy_op,
                              record_adapter_mark_op,
                              cobj_eq_hash_op),
                wli("record-adapter"),
                delegate_put_string, delegate_put_char, delegate_put_byte,
                record_adapter_get_line, delegate_get_char, delegate_get_byte,
                delegate_unget_char, delegate_unget_byte,
                delegate_put_buf, delegate_fill_buf,
                delegate_close, delegate_flush, delegate_seek,
                delegate_truncate, delegate_get_prop, delegate_set_prop,
                delegate_get_error, delegate_get_error_str,
                delegate_clear_error, delegate_get_fd);

val record_adapter(val regex, val stream, val include_match)
{
  val self = lit("record-adapter");
  val rec_adapter = make_delegate_stream(self, default_arg(stream, std_input),
                                         sizeof (struct record_adapter_base),
                                         &record_adapter_ops.cobj_ops);
  struct record_adapter_base *rb = coerce(struct record_adapter_base *,
                                          rec_adapter->co.handle);

  rb->regex = regex;
  rb->include_match = default_null_arg(include_match);
  return rec_adapter;
}

val streamp(val obj)
{
  return typep(obj, stream_s);
}

val stream_set_prop(val stream, val ind, val prop)
{
  val self = lit("stream-set-prop");
  struct strm_ops *ops = coerce(struct strm_ops *,
                                cobj_ops(self, stream, stream_s));
  return ops->set_prop(stream, ind, prop);
}

val stream_get_prop(val stream, val ind)
{
  val self = lit("stream-get-prop");
  struct strm_ops *ops = coerce(struct strm_ops *,
                                cobj_ops(self, stream, stream_s));

  if (ind == fd_k && ops->get_fd != null_get_fd)
    return ops->get_fd(stream);

  return ops->get_prop(stream, ind);
}

val real_time_stream_p(val obj)
{
  if (streamp(obj)) {
    struct strm_ops *ops = coerce(struct strm_ops *, obj->co.ops);
    return ops->get_prop(obj, real_time_k);
  }

  return nil;
}

val close_stream(val stream, val throw_on_error)
{
  val self = lit("close-stream");
  struct strm_ops *ops = coerce(struct strm_ops *,
                                cobj_ops(self, stream, stream_s));
  return ops->close(stream, throw_on_error);
}

val get_error(val stream)
{
  val self = lit("get-error");
  struct strm_ops *ops = coerce(struct strm_ops *,
                                cobj_ops(self, stream, stream_s));
  return ops->get_error(stream);
}

val get_error_str(val stream)
{
  val self = lit("get-error-str");
  struct strm_ops *ops = coerce(struct strm_ops *,
                                cobj_ops(self, stream, stream_s));
  return ops->get_error_str(stream);
}

val clear_error(val stream)
{
  val self = lit("clear-error");
  struct strm_ops *ops = coerce(struct strm_ops *,
                                cobj_ops(self, stream, stream_s));
  return ops->clear_error(stream);
}

val get_line(val stream_in)
{
  val self = lit("get-line");
  val stream = default_arg(stream_in, std_input);
  struct strm_ops *ops = coerce(struct strm_ops *,
                                cobj_ops(self, stream, stream_s));
  return ops->get_line(stream);
}

val get_char(val stream_in)
{
  val self = lit("get-char");
  val stream = default_arg(stream_in, std_input);
  struct strm_ops *ops = coerce(struct strm_ops *,
                                cobj_ops(self, stream, stream_s));
  return ops->get_char(stream);
}

val get_byte(val stream_in)
{
  val self = lit("get-byte");
  val stream = default_arg(stream_in, std_input);
  struct strm_ops *ops = coerce(struct strm_ops *,
                                cobj_ops(self, stream, stream_s));
  return ops->get_byte(stream);
}

val get_bytes(val self, val stream_in, mem_t *ptr, ucnum len)
{
  val stream = default_arg(stream_in, std_input);
  struct strm_ops *ops = coerce(struct strm_ops *,
                                cobj_ops(self, stream, stream_s));
  return unum(ops->fill_buf(stream, ptr, len, 0));
}

val unget_char(val ch, val stream_in)
{
  val self = lit("unget-char");
  val stream = default_arg(stream_in, std_input);
  struct strm_ops *ops = coerce(struct strm_ops *,
                                cobj_ops(self, stream, stream_s));
  if (!is_chr(ch))
    type_mismatch(lit("~a: ~s is not a character"), self, ch, nao);
  return ops->unget_char(stream, ch);
}

val unget_byte(val byte, val stream_in)
{
  val self = lit("unget-byte");
  cnum b = c_num(byte, self);
  val stream = default_arg(stream_in, std_input);
  struct strm_ops *ops = coerce(struct strm_ops *,
                                cobj_ops(self, stream, stream_s));

  if (b < 0 || b > 255)
    uw_throwf(file_error_s, lit("~a: stream ~s: byte value ~a out of range"),
              self, stream, byte, nao);

  return ops->unget_byte(stream, b);
}

val put_buf(val buf, val pos_in, val stream_in)
{
  val self = lit("put-buf");
  val stream = default_arg(stream_in, std_output);
  ucnum pos = c_unum(default_arg(pos_in, zero), self);
  ucnum len = c_unum(length_buf(buf), self);
  mem_t *ptr = buf_get(buf, self);
  struct strm_ops *ops = coerce(struct strm_ops *,
                                cobj_ops(self, stream, stream_s));

  return unum(ops->put_buf(stream, ptr, len, pos));
}

val fill_buf(val buf, val pos_in, val stream_in)
{
  val self = lit("fill-buf");
  val stream = default_arg(stream_in, std_input);
  ucnum pos = c_unum(default_arg(pos_in, zero), self);
  ucnum len = c_unum(length_buf(buf), self);
  mem_t *ptr = buf_get(buf, self);
  struct strm_ops *ops = coerce(struct strm_ops *,
                                cobj_ops(self, stream, stream_s));
  return unum(ops->fill_buf(stream, ptr, len, pos));
}

val fill_buf_adjust(val buf, val pos_in, val stream_in)
{
  val self = lit("fill-buf-adjust");
  val stream = default_arg(stream_in, std_input);
  ucnum pos = c_unum(default_arg(pos_in, zero), self);
  val alloc_size = buf_alloc_size(buf);
  ucnum len = c_unum(alloc_size, self);
  mem_t *ptr = buf_get(buf, self);
  val readpos;
  struct strm_ops *ops = coerce(struct strm_ops *,
                                cobj_ops(self, stream, stream_s));
  buf_set_length(buf, alloc_size, zero);
  readpos = unum(ops->fill_buf(stream, ptr, len, pos));
  buf_set_length(buf, readpos, zero);
  return readpos;
}

val get_line_as_buf(val stream_in)
{
  val self = lit("get-line-as-buf");
  val stream = default_arg(stream_in, std_input);
  struct strm_ops *ops = coerce(struct strm_ops *,
                                cobj_ops(self, stream, stream_s));
  val buf = make_buf(zero, nil, num_fast(128));
  unsigned char bytes[128];
  size_t count = 0;

  for (;;) {
    val b = ops->get_byte(stream);
    if (b == nil || b == num('\n'))
      break;
    bytes[count++] = c_num(b, self);

    if (count == sizeof bytes) {
      buf_put_bytes(buf, length_buf(buf), bytes, count, self);
      count = 0;
    }
  }

  if (count > 0)
    buf_put_bytes(buf, length_buf(buf), bytes, count, self);

  buf_trim(buf);
  return buf;
}

struct fmt {
  const char *type;
  const char *dec;
  const char *oct;
  const char *hex;
  const char *HEX;
};

static struct fmt fmt_tab[] = {
  { "short",      "%hd",   "%ho",   "%hx",   "%hX"   },
  { "int",        "%d",    "%o",    "%x",    "%X"    },
  { "long",       "%ld",   "%lo",   "%lx",   "%lX"   },
  { "long long",  "%lld",  "%llo",  "%llx",  "%llX"  },
  { "long long",  "%Ld",   "%Lo",   "%Lx",   "%LX"   },
  { "long long",  "%qd",   "%qo",   "%qx",   "%qX",  },
  { "int64",      "%I64d", "%I64o", "%I64x", "%I64X" },
  { "__int64",    "%I64d", "%I64o", "%I64x", "%I64X" },
  { 0,            0,       0,       0,       0       }
};

static struct fmt *num_fmt;

static void detect_format_string(void)
{
  struct fmt *f;
  char buf[64];
  cnum num = 1234;
  const char *cnum_type = if3(strcmp(INTPTR_TYPE, "longlong_t") == 0,
                              LONGLONG_TYPE, INTPTR_TYPE);

  for (f = fmt_tab; f->type != 0; f++) {
    if (strcmp(cnum_type, f->type) != 0)
      continue;
    memset(buf, 0, sizeof buf);
    if (sprintf(buf, f->dec, num) != 4 || strcmp(buf, "1234") != 0)
      continue;
    memset(buf, 0, sizeof buf);
    if (sprintf(buf, f->oct, num) != 4 || strcmp(buf, "2322") != 0)
      continue;
    memset(buf, 0, sizeof buf);
    if (sprintf(buf, f->hex, num) != 3 || strcmp(buf, "4d2") != 0)
      continue;
    memset(buf, 0, sizeof buf);
    if (sprintf(buf, f->HEX, num) != 3 || strcmp(buf, "4D2") != 0)
      continue;
    num_fmt = f;
    break;
  }

  bug_unless (num_fmt != 0);
}

enum align { al_left, al_center, al_right };

static void vformat_align_pre(val stream, enum align align, int slack)
{
  int i;

  switch (align) {
  case al_right:
    for (i = 0; i < slack; i++)
      put_char(chr(' '), stream);
    break;
  case al_left:
    break;
  case al_center:
    for (i = 0; i < slack/2; i++)
      put_char(chr(' '), stream);
    break;
  }
}

static void vformat_align_post(val stream, enum align align, int slack)
{
  int i;

  switch (align) {
  case al_right:
    break;
  case al_left:
    for (i = 0; i < slack; i++)
      put_char(chr(' '), stream);
    break;
  case al_center:
    for (i = 0; i < (slack+1)/2; i++)
      put_char(chr(' '), stream);
    break;
  }
}

static void vformat_num(val stream, const char *str,
                        int width, enum align align, int zeropad,
                        int precision, int sign)
{
  int sign_char = (str[0] == '-' || str[0] == '+') ? str[0] : 0;
  int digit_len = strlen(str) - (sign_char != 0);
  int padlen = precision > digit_len ? precision - digit_len : 0;
  int total_len = digit_len + padlen + (sign_char || sign);
  int slack = (total_len < width) ? width - total_len : 0;
  int i;

  vformat_align_pre(stream, align, slack);

  if (!zeropad)
    for (i = 0; i < padlen; i++)
      put_char(chr(' '), stream);

  if (sign_char) {
    put_char(chr(sign_char), stream);
    str++;
  } else if (sign) {
    put_char(chr(sign), stream);
  }

  if (zeropad)
    for (i = 0; i < padlen; i++)
      put_char(chr('0'), stream);

  while (*str)
    put_char(chr(*str++), stream);

  vformat_align_post(stream, align, slack);
}

static cnum calc_fitlen(const wchar_t *cstr, int precision, int width)
{
  cnum i, fitlen;
  wchar_t ch;

  if (width == 0 && precision == 0)
    return INT_PTR_MAX;

  for (i = 0, fitlen = 0; (ch = cstr[i]) != 0; i++) {
    int chw = iswcntrl(ch) ? 0 : 1 + wide_display_char_p(ch);

    if (precision && fitlen + chw > precision)
      break;

    if (width && fitlen + chw > width)
      return INT_PTR_MAX;

    fitlen += chw;
  }

  return fitlen;
}

static void vformat_str(val stream, val str, int width, enum align align,
                        int precision)
{
  const wchar_t *cstr = c_str(str);
  cnum fitlen = calc_fitlen(cstr, precision, width);
  cnum slack = (fitlen < width) ? width - fitlen : 0;
  cnum i, w;
  wchar_t wc;

  vformat_align_pre(stream, align, slack);

  if (fitlen == INT_PTR_MAX) {
    put_string(str, stream);
  } else for (i = 0, w = 0; (wc = cstr[i]) != 0; i++) {
    int cw = iswcntrl(wc) ? 0 : 1 + wide_display_char_p(wc);
    if (w + cw > fitlen)
      break;
    put_char(chr(wc), stream);
    w += cw;
  }

  vformat_align_post(stream, align, slack);

  gc_hint(str);
}

val formatv(val stream_in, val fmtstr, struct args *al)
{
  uses_or2;
  val stream = if3(stream_in == t,
                   std_output,
                   or2(stream_in, make_string_output_stream()));
  val save_indent = get_indent(stream);
  val save_mode = nil;
  val self = lit("format");

  uw_simple_catch_begin;

  {
    const wchar_t *fmt = c_str(fmtstr);
    enum {
      vf_init, vf_width, vf_digits, vf_star, vf_precision, vf_spec
    } state = vf_init, saved_state = vf_init;
    int width = 0, precision = 0, precision_p = 0, digits = 0, lt = 0, neg = 0;
    enum align align = al_right;
    int sign = 0, zeropad = 0, dfl_precision = 0;
    int dfl_digits = 0, print_base = 0;
    cnum value;
    cnum arg_ix = 0;

    for (;;) {
      val obj;
      type_t typ;
      wchar_t ch = *fmt++;
      char num_buf[512], *pnum = num_buf;

      switch (state) {
      case vf_init:
        switch (ch) {
        case 0:
          break;
        case '~':
          state = vf_width;
          width = 0;
          align = al_right;
          zeropad = 0;
          precision = 0;
          precision_p = 0;
          digits = 0;
          lt = 0;
          neg = 0;
          continue;
        default:
          put_char(chr(ch), stream);
          continue;
        }
        break;
      case vf_width:
        switch (ch) {
        case '~':
          put_char(chr('~'), stream);
          state = vf_init;
          continue;
        case '<':
          lt = 1;
          align = al_left;
          continue;
        case '^':
          align = al_center;
          continue;
        case ',':
          state = vf_precision;
          continue;
        case '0': case '1': case '2': case '3': case '4': case '5':
        case '6': case '7': case '8': case '9':
          saved_state = state;
          state = vf_digits;
          digits = ch - '0';
          continue;
        case '-':
          neg = 1;
          continue;
        case '*':
          saved_state = state;
          state = vf_star;
          continue;
        default:
          state = vf_spec;
          --fmt;
          continue;
        }
        break;
      case vf_precision:
        switch (ch) {
        case '0':
          if (!zeropad) {
            zeropad = 1;
            continue;
          }
          /* fallthrough */
        case '1': case '2': case '3': case '4': case '5':
        case '6': case '7': case '8': case '9':
          saved_state = state;
          state = vf_digits;
          digits = ch - '0';
          continue;
        case '+': case ' ':
          sign = ch;
          continue;
        case '-':
          sign = '0';
          continue;
        case '*':
          saved_state = state;
          state = vf_star;
          continue;
        default:
          state = vf_spec;
          --fmt;
          continue;
        }
        break;
      case vf_digits:
        switch (ch) {
        case '0': case '1': case '2': case '3': case '4':
        case '5': case '6': case '7': case '8': case '9':
          digits = (digits * 10) + (ch - '0');
          if (digits > 999999)
            uw_throwf(assert_s, lit("~a: ridiculous precision or field"),
                      self, nao);
          continue;
        default:
  do_digits:
          switch (saved_state) {
          case vf_width:
            if (digits < 0) {
              width = -digits;
              align = al_left;
            } else {
              width = digits;
            }
            if (neg)
              align = al_left;
            if (ch == ',') {
              state = vf_precision;
            } else {
              state = vf_spec;
              --fmt;
            }
            continue;
          case vf_precision:
            precision = digits;
            precision_p = 1;
            state = vf_spec;
            --fmt;
            continue;
          default:
            internal_error("unexpected state in formatter");
          }
        }
        break;
      case vf_star:
        obj = args_get_checked(self, al, &arg_ix);
        digits = c_num(obj, self);
        goto do_digits;
        break;
      case vf_spec:
        state = vf_init;
        if (zeropad && !precision_p) {
          zeropad = precision = 0;
          precision_p = 1;
        }
        switch (ch) {
        case 'x': case 'X':
          obj = args_get_checked(self, al, &arg_ix);
          typ = type(obj);
        hex:
          switch (typ) {
          case BGNUM:
            {
              int nchars = mp_radix_size(mp(obj), 16);
              if (nchars >= convert(int, sizeof (num_buf)))
                pnum = coerce(char *, chk_malloc(nchars + 1));
              mp_toradix_case(mp(obj), coerce(unsigned char *, pnum), 16, ch == 'x');
            }
            break;
          case BUF:
            {
              ucnum len = c_unum(length_buf(obj), self);
              ucnum nchars = 2 * len + 1;

              if (len >= INT_PTR_MAX)
                uw_throwf(error_s, lit("~a: ~~~a conversion given "
                                       "too large a buf argument"),
                          self, chr(ch), nao);

              pnum = coerce(char *, chk_malloc(nchars));
              buf_hex(obj, pnum, nchars, ch == 'X');
            }
            break;
          case NUM:
          case CHR:
          default:
            {
              const char *fmt = ch == 'x' ? num_fmt->hex : num_fmt->HEX;
              value = c_num(obj, self);
              if (value < 0) {
                num_buf[0] = '-';
                sprintf(num_buf + 1, fmt, -value);
              } else {
                sprintf(num_buf, fmt, value);
              }
            }
          }
          goto output_num;
        case 'o': case 'b':
          obj = args_get_checked(self, al, &arg_ix);
          typ = type(obj);
        oct:
          if (typ == BGNUM) {
            int rad = ch == 'o' ? 8 : 2;
            int nchars = mp_radix_size(mp(obj), rad);
            if (nchars >= convert(int, sizeof (num_buf)))
              pnum = coerce(char *, chk_malloc(nchars + 1));
            mp_toradix(mp(obj), coerce(unsigned char *, pnum), rad);
          } else if (ch == 'o') {
            cnum value = c_num(obj, self);
            sprintf(num_buf, num_fmt->oct, value);
          } else {
            cnum val = c_num(obj, self);
            int s = (val < 0);
            int i = sizeof num_buf;

            value = s ? -val : val;

            num_buf[--i] = 0;

            while (i > 0) {
              num_buf[--i] = ((value & 1) ? '1' : '0');
              value >>= 1;
              if (!value)
                break;
            }

            if (s && i > 0)
              num_buf[--i] = '-';

            memmove(num_buf, num_buf + i, sizeof num_buf - i);
          }
          goto output_num;
        case 'f': case 'e':
          obj = args_get_checked(self, al, &arg_ix);

          {
            double n;

            switch (type(obj)) {
            case BGNUM:
              obj = flo_int(obj);
              /* fallthrough */
            case FLNUM:
              n = c_flo(obj, lit("format"));
              break;
            case NUM:
              n = convert(double, c_num(obj, self));
              break;
            default:
              uw_throwf(error_s, lit("~a: ~~~a conversion requires "
                                     "numeric arg: ~s given"),
                        self, chr(ch), obj, nao);
            }

            if (!precision_p) {
              if (!dfl_digits)
                dfl_digits = c_num(cdr(lookup_var(nil, print_flo_digits_s)), self);
              precision = dfl_digits;
            }

            /* guard against num_buf overflow */
            if (precision > 128)
              uw_throwf(error_s, lit("~a: excessive precision: ~s"),
                        self, num(precision), nao);

            if (ch == 'e') {
              sprintf(num_buf, "%.*e", precision, n);
              {
#if CONFIG_LOCALE_TOLERANCE
                char *dec = strchr(num_buf, dec_point);
#else
                char *dec = strchr(num_buf, '.');
#endif
                char *exp = strchr(dec ? dec : num_buf, 'e');

                if (exp) {
                  char *scan = ++exp;

                  if (*scan == '-')
                    *exp++ = *scan++;
                  else if (*scan == '+')
                    scan++;

                  while (*scan == '0')
                    scan++;

                  if (!*scan)
                    *exp++ = '0';
                  else
                    while (*scan)
                      *exp++ = *scan++;

                  *exp = 0;
                }
              }
            } else {
              sprintf(num_buf, "%.*f", precision, n);
            }
            if (!isdigit(num_buf[0]) && !isdigit(num_buf[1])) {
              vformat_str(stream, lit("#<bad-float>"), width, align, 0);
              continue;
            }
            precision = (width ? width - 1 : 0);
#if CONFIG_LOCALE_TOLERANCE
            if (dec_point != '.') {
              char *dot = num_buf;
              while ((dot = strchr(dot, dec_point)) != 0)
                *dot++ = '.';
            }
#endif
            goto output_num;
          }
        case 'd':
          obj = args_get_checked(self, al, &arg_ix);
          typ = type(obj);
          goto dec;
        case 'a': case 's':
          obj = args_get_checked(self, al, &arg_ix);
          typ = type(obj);

          if (typ == NUM || typ == BGNUM) {
            if (!print_base)
              print_base = c_num(cdr(lookup_var(nil, print_base_s)), self);
            switch (print_base) {
            case 0:
            case 2:
              ch = 'b';
              goto oct;
            case 8:
              ch = 'o';
              goto oct;
            case 16:
              ch = 'X';
              goto hex;
            case 10:
            default:
              break;
            }
          }
          /* fallthrough */
        dec:
          switch (typ) {
          case NUM:
            value = c_num(obj, self);
            sprintf(num_buf, num_fmt->dec, value);
            goto output_num;
          case BGNUM:
            {
              int nchars = mp_radix_size(mp(obj), 10);
              if (nchars >= convert(int, sizeof (num_buf)))
                pnum = coerce(char *, chk_malloc(nchars + 1));
              mp_toradix(mp(obj), coerce(unsigned char *, pnum), 10);
            }
            goto output_num;
          case FLNUM:
            if (!precision_p) {
              if (!dfl_precision)
                dfl_precision = c_num(cdr(lookup_var(nil,
                                                     print_flo_precision_s)),
                                      self);
              precision = dfl_precision;
            }

            if (precision > 500)
              uw_throwf(error_s, lit("~a: excessive precision: ~s"),
                        self, num(precision), nao);

            sprintf(num_buf, "%.*g", precision, obj->fl.n);

#if CONFIG_LOCALE_TOLERANCE
            if (dec_point != '.') {
              char *dot = num_buf;
              while ((dot = strchr(dot, dec_point)) != 0)
                *dot++ = '.';
            }
#endif

            {
              char *dec = strchr(num_buf, '.');
              char *exp = strchr(dec ? dec : num_buf, 'e');

              if (exp) {
                char *scan = ++exp;

                if (*scan == '-')
                  *exp++ = *scan++;
                else if (*scan == '+')
                  scan++;

                while (*scan == '0')
                  scan++;

                while (*scan)
                  *exp++ = *scan++;

                *exp = 0;
              }

              if (ch == 's' && (!precision_p || precision > 0) && !dec && !exp)
                strcat(num_buf, ".0");
            }

            if (!isdigit(num_buf[0]) && !isdigit(num_buf[1])) {
              vformat_str(stream, lit("#<bad-float>"), width, align, 0);
              continue;
            }

            precision = (width ? width - 1 : 0);
            goto output_num;
          default:
            if (width != 0 || precision_p) {
              val str = format(nil, ch == 'a' ? lit("~a") : lit("~s"),
                               obj, nao);
              vformat_str(stream, str, width, align, precision);
              continue;
            }
          }
          obj_print(obj, stream, if2(ch == 'a', t));
          continue;
        case 'p':
          {
            val ptr = args_get_checked(self, al, &arg_ix);
            value = coerce(cnum, ptr);
            sprintf(num_buf, num_fmt->hex, value);
          }
          goto output_num;
        case '!':
          save_mode = test_neq_set_indent_mode(stream, num_fast(indent_foff),
                                               num_fast(indent_data));
          if (lt)
            set_indent(stream, plus(save_indent, num(width)));
          else
            inc_indent(stream, num(width));
          continue;
        case 0:
          uw_throwf(error_s, lit("missing format directive character"), nao);
        default:
          uw_throwf(error_s, lit("unknown format directive character ~s"),
                    chr(ch), nao);
        output_num:
          {
            vformat_num(stream, pnum, width, align,
                        zeropad, precision, sign);
            if (pnum != num_buf)
              free(pnum);
            continue;
          }
        }
        continue;
      }

      break;
    }

    if (args_more(al, arg_ix))
      uw_throwf(assert_s, lit("~a: excess arguments"), self, nao);
  }


  uw_unwind {
    set_indent(stream, save_indent);
    if (save_mode)
      set_indent_mode(stream, save_mode);
  }

  uw_catch_end;

  gc_hint(fmtstr);

  return (stream_in) ? t : get_string_from_stream(stream);
}

val vformat(val stream, val fmtstr, va_list vl)
{
  val arg;
  args_decl(args, ARGS_MAX);

  while ((arg = va_arg(vl, val)) != nao)
    args_add_checked(lit("format"), args, arg);

  return formatv(stream, fmtstr, args);
}

val vformat_to_string(val fmtstr, va_list vl)
{
  val stream = make_string_output_stream();
  (void) vformat(stream, fmtstr, vl);
  return get_string_from_stream(stream);
}

val format(val stream, val str, ...)
{
  val self = lit("format");
  uses_or2;
  val st = if3(stream == t,
               std_output,
               or2(stream, make_string_output_stream()));
  class_check(self, st, stream_s);

  {
    va_list vl;
    val ret;
    va_start (vl, str);
    ret = vformat(st, str, vl);
    va_end (vl);
    return (stream) ? ret : get_string_from_stream(st);
  }
}

val fmt(val string, struct args *args)
{
  return formatv(nil, string, args);
}

static val put_indent(val stream, struct strm_ops *ops, cnum chars)
{
  while (chars--)
    if (!ops->put_char(stream, chr(' ')))
      return nil;
  return t;
}

val put_string(val string, val stream_in)
{
  val self = lit("put-string");
  val stream = default_arg(stream_in, std_output);
  struct strm_base *s = coerce(struct strm_base *, stream->co.handle);

  if (lazy_stringp(string)) {
    return lazy_str_put(string, stream_in, s);
  } else {
    struct strm_ops *ops = coerce(struct strm_ops *,
                                  cobj_ops(self, stream, stream_s));
    cnum col = s->column;

    const wchar_t *str = c_str(string), *p = str;

    if (s->indent_mode != indent_off && s->indent_mode != indent_foff) {
      while (*str)
        put_char(chr(*str++), stream);
      return t;
    }

    for (; *p; p++) {
      switch (*p) {
      case '\n':
        col = 0;
        s->force_break = 0;
        break;
      case '\t':
        col = (col + 1) | 7;
        break;
      default:
        if (!iswcntrl(*p))
          col += 1 + wide_display_char_p(*p);
        break;
      }
    }

    ops->put_string(stream, string);
    s->column = col;
    return t;
  }
}

val put_char(val ch, val stream_in)
{
  val self = lit("put-char");
  val stream = default_arg(stream_in, std_output);
  struct strm_ops *ops = coerce(struct strm_ops *,
                                cobj_ops(self, stream, stream_s));
  struct strm_base *s = coerce(struct strm_base *, stream->co.handle);
  wint_t cch = c_chr(ch);

  switch (cch) {
  case L'\n':
    ops->put_char(stream, ch);
    s->column = 0;
    s->force_break = 0;
    break;
  case L'\t':
    if (s->column == 0 && s->indent_mode != indent_off &&
        s->indent_mode != indent_foff)
    {
      put_indent(stream, ops, s->indent_chars);
      s->column = s->indent_chars;
    }
    ops->put_char(stream, ch);
    s->column = (s->column + 1) | 7;
    break;
  default:
    if (s->column == 0 && s->indent_mode != indent_off &&
        s->indent_mode != indent_foff)
    {
      put_indent(stream, ops, s->indent_chars);
      s->column = s->indent_chars;
    }
    ops->put_char(stream, ch);

    if (!iswcntrl(cch))
      s->column += 1 + wide_display_char_p(cch);
    break;
  }

  return t;
}

val put_byte(val byte, val stream_in)
{
  val self = lit("put-byte");
  val stream = default_arg(stream_in, std_output);
  struct strm_ops *ops = coerce(struct strm_ops *,
                                cobj_ops(self, stream, stream_s));
  cnum b = c_num(byte, self);

  if (b < 0 || b > 255)
    uw_throwf(file_error_s, lit("~a: stream ~s: byte value ~a out of range"),
              self, stream, byte, nao);

  return ops->put_byte(stream, b);
}

val put_line(val string, val stream)
{
  return (put_string(default_arg(string, null_string), stream), put_char(chr('\n'), stream));
}

val put_strings(val strings, val stream)
{
  strings = nullify(strings);

  for (; strings; strings = cdr(strings))
    put_string(car(strings), stream);
  return t;
}

val put_lines(val lines, val stream)
{
  lines = nullify(lines);

  for (; lines; lines = cdr(lines))
    put_line(car(lines), stream);
  return t;
}

val flush_stream(val stream_in)
{
  val self = lit("flush-stream");
  val stream = default_arg(stream_in, std_output);
  struct strm_ops *ops = coerce(struct strm_ops *,
                                cobj_ops(self, stream, stream_s));
  return ops->flush(stream);
}

val seek_stream(val stream, val offset, val whence)
{
  val self = lit("seek-stream");
  struct strm_ops *ops = coerce(struct strm_ops *,
                                cobj_ops(self, stream, stream_s));
  enum strm_whence w;

  if (whence == from_start_k)
    w = strm_start;
  else if (whence == from_current_k)
    w = strm_cur;
  else if (whence == from_end_k)
    w = strm_end;
  else
    uw_throwf(file_error_s, lit("~a: ~s is not a valid whence argument"),
              self, whence, nao);

  return ops->seek(stream, offset, w);
}

val truncate_stream(val stream, val len)
{
  val self = lit("truncate-stream");
  struct strm_ops *ops = coerce(struct strm_ops *,
                                cobj_ops(self, stream, stream_s));
  if (missingp(len))
    len = ops->seek(stream, zero, strm_cur);
  return ops->truncate(stream, len);
}

val get_indent_mode(val stream)
{
  val self = lit("get-indent-mode");
  struct strm_base *s = coerce(struct strm_base *,
                               cobj_handle(self, stream, stream_s));
  return num_fast(s->indent_mode);
}

val test_set_indent_mode(val stream, val compare, val mode)
{
  val self = lit("test-set-indent-mode");
  struct strm_base *s = coerce(struct strm_base *,
                               cobj_handle(self, stream, stream_s));
  val oldval = num_fast(s->indent_mode);
  if (oldval == compare)
    s->indent_mode = convert(enum indent_mode, c_num(mode, self));
  return oldval;
}

val test_neq_set_indent_mode(val stream, val compare, val mode)
{
  val self = lit("test-neq-set-indent-mode");
  struct strm_base *s = coerce(struct strm_base *,
                               cobj_handle(self, stream, stream_s));
  val oldval = num_fast(s->indent_mode);
  if (oldval != compare)
    s->indent_mode = convert(enum indent_mode, c_num(mode, self));
  return oldval;
}

val set_indent_mode(val stream, val mode)
{
  val self = lit("set-indent-mode");
  struct strm_base *s = coerce(struct strm_base *,
                               cobj_handle(self, stream, stream_s));
  val oldval = num_fast(s->indent_mode);
  s->indent_mode = convert(enum indent_mode, c_num(mode, self));
  return oldval;
}

val get_indent(val stream)
{
  val self = lit("get-indent");
  struct strm_base *s = coerce(struct strm_base *,
                               cobj_handle(self, stream, stream_s));
  return num(s->indent_chars);
}

val set_indent(val stream, val indent)
{
  val self = lit("set-indent");
  struct strm_base *s = coerce(struct strm_base *,
                               cobj_handle(self, stream, stream_s));
  val oldval = num(s->indent_chars);
  s->indent_chars = c_num(indent, self);
  if (s->indent_chars < 0)
    s->indent_chars = 0;
  return oldval;
}

val inc_indent(val stream, val delta)
{
  val self = lit("inc-indent");
  struct strm_base *s = coerce(struct strm_base *,
                               cobj_handle(self, stream, stream_s));
  val oldval = num(s->indent_chars);
  val col = num(s->column);
  s->indent_chars = c_num(plus(delta, col), self);
  if (s->indent_chars < 0)
    s->indent_chars = 0;
  return oldval;
}

val width_check(val stream, val alt)
{
  val self = lit("width-check");
  struct strm_base *s = coerce(struct strm_base *,
                               cobj_handle(self, stream, stream_s));

  if ((s->indent_mode == indent_code &&
       s->column >= s->indent_chars + s->code_width) ||
      (s->indent_mode == indent_data &&
       s->column >= s->indent_chars + s->data_width) ||
      (s->indent_mode != indent_off &&
       s->indent_mode != indent_foff && s->force_break))
  {
    put_char(chr('\n'), stream);
    s->force_break = 0;
    return t;
  } else if (alt) {
    put_char(alt, stream);
  }

  return nil;
}

val force_break(val stream)
{
  val self = lit("force-break");
  struct strm_base *s = coerce(struct strm_base *,
                               cobj_handle(self, stream, stream_s));
  s->force_break = 1;
  return stream;
}

val set_max_length(val stream, val length)
{
  val self = lit("set-max-length");
  struct strm_base *s = coerce(struct strm_base *,
                               cobj_handle(self, stream, stream_s));
  cnum old_max = s->max_length;
  s->max_length = c_num(length, self);
  return num(old_max);
}

val set_max_depth(val stream, val depth)
{
  val self = lit("set-max-depth");
  struct strm_base *s = coerce(struct strm_base *,
                               cobj_handle(self, stream, stream_s));
  cnum old_max = s->max_depth;
  s->max_depth = c_num(depth, self);
  return num(old_max);
}

struct strm_ctx *get_set_ctx(val stream, struct strm_ctx *ctx)
{
  struct strm_base *s = coerce(struct strm_base *, stream->co.handle);
  struct strm_ctx *ret = s->ctx;
  s->ctx = ctx;
  return ret;
}

struct strm_ctx *get_ctx(val stream)
{
  struct strm_base *s = coerce(struct strm_base *, stream->co.handle);
  return s->ctx;
}

val get_string(val stream_in, val nchars, val close_after_p)
{
  val stream = default_arg(stream_in, std_input);
  val strstream = make_string_output_stream();
  nchars = default_null_arg(nchars);
  val ch;

  if (nchars) {
    for (; gt(nchars, zero) && (ch = get_char(stream));
         nchars = minus(nchars, one))
      put_char(ch, strstream);
  } else {
    while ((ch = get_char(stream)))
      put_char(ch, strstream);
  }

  if ((missingp(close_after_p) && (!opt_compat || opt_compat > 102)) ||
      default_arg_strict(close_after_p, t))
    close_stream(stream, t);

  return get_string_from_stream(strstream);
}

val open_directory(val path)
{
  DIR *d = w_opendir(c_str(path));

  if (!d) {
    int eno = errno;
    uw_throwf(errno_to_file_error(eno),
              lit("error opening directory ~s: ~d/~s"),
              path, num(eno), errno_to_str(eno), nao);
  }

  return make_dir_stream(d);
}

val open_file(val path, val mode_str)
{
  struct stdio_mode m, m_r = stdio_mode_init_r;
  val norm_mode = normalize_mode(&m, mode_str, m_r);
  FILE *f = w_fopen_mode(c_str(path), c_str(norm_mode), m);

  if (!f) {
    int eno = errno;
    uw_throwf(errno_to_file_error(eno), lit("error opening ~s: ~d/~s"),
              path, num(eno), errno_to_str(eno), nao);
  }

  return set_mode_props(m, make_stdio_stream(f, path));
}

val open_fileno(val fd, val mode_str)
{
  val self = lit("open-fileno");
  struct stdio_mode m, m_r = stdio_mode_init_r;
  FILE *f = (errno = 0, w_fdopen(c_num(fd, self), c_str(normalize_mode(&m, mode_str, m_r))));

  if (!f) {
    int eno = errno;
    close(c_num(fd, self));
    uw_throwf(errno_to_file_error(eno), lit("error opening descriptor ~a: ~d/~s"),
              fd, num(eno), errno_to_str(eno), nao);
  }

  return set_mode_props(m, make_stdio_stream(f, format(nil,
                                                       lit("fd ~d"),
                                                       fd, nao)));
}

val open_tail(val path, val mode_str, val seek_end_p)
{
  struct stdio_mode m, m_r = stdio_mode_init_r;
  val mode = normalize_mode(&m, mode_str, m_r);
  FILE *f = w_fopen_mode(c_str(path), c_str(mode), m);
  struct stdio_handle *h;
  val stream;
  unsigned long state = 0;

  if (f && default_null_arg(seek_end_p))
    if (fseek(f, 0, SEEK_END) < 0)
      uw_throwf(file_error_s, lit("error seeking to end of ~s: ~d/~s"),
                path, num(errno), errno_to_str(errno), nao);

  stream = make_tail_stream(f, path);
  h = coerce(struct stdio_handle *, stream->co.handle);
  h->mode = mode_str;
  if (!f)
    tail_strategy(stream, &state);
  return set_mode_props(m, stream);
}

struct save_fds {
  volatile int in;
  volatile int out;
  volatile int err;
};

#define FDS_IN  1
#define FDS_OUT 2
#define FDS_ERR 4

static void fds_init(struct save_fds *fds)
{
  fds->in = fds->out = fds->err = -1;
}

static int fds_subst(val stream, int fd_std, val self)
{
  int fd_orig = c_num(stream_fd(stream), self);

  if (fd_orig == fd_std)
    return -1;

  {
    int fd_dup = dup(fd_std);

    if (fd_dup != -1) {
      dup2(fd_orig, fd_std);
      return fd_dup;
    }

    uw_throwf(file_error_s, lit("failed to duplicate file descriptor: ~d/~s"),
              num(errno), errno_to_str(errno), nao);
  }
}

static void fds_swizzle(struct save_fds *fds, int flags, val self)
{
  if ((flags & FDS_IN) != 0)
    fds->in = fds_subst(std_input, STDIN_FILENO, self);

  if ((flags & FDS_OUT) != 0)
    fds->out = fds_subst(std_output, STDOUT_FILENO, self);

  if ((flags & FDS_ERR) != 0)
    fds->err = fds_subst(std_error, STDERR_FILENO, self);
}

static void fds_restore(struct save_fds *fds)
{
  if (fds->in != -1) {
    dup2(fds->in, STDIN_FILENO);
    close(fds->in);
  }

  if (fds->out != -1) {
    dup2(fds->out, STDOUT_FILENO);
    close(fds->out);
  }

  if (fds->err != -1) {
    dup2(fds->err, STDERR_FILENO);
    close(fds->err);
  }
}


val open_command(val path, val mode_str)
{
  val self = lit("open-command");
  struct stdio_mode m, m_r = stdio_mode_init_r;
  val mode = normalize_mode_no_bin(&m, mode_str, m_r);
  int input = m.read != 0;
  struct save_fds sfds;
  FILE *f = 0;

  fds_init(&sfds);

  uw_simple_catch_begin;

  fds_swizzle(&sfds, (input ? FDS_IN : FDS_OUT) | FDS_ERR, self);

  f = w_popen(c_str(path), c_str(mode));

  if (!f) {
    int eno = errno;
    uw_throwf(errno_to_file_error(eno), lit("~a: error opening pipe ~s: ~d/~s"),
              self, path, num(eno), errno_to_str(eno), nao);
  }

  uw_unwind {
    fds_restore(&sfds);
  }

  uw_catch_end;

  return set_mode_props(m, make_pipe_stream(f, path));
}

#if HAVE_FORK_STUFF
static val open_subprocess(val name, val mode_str, val args, val fun)
{
  val self = lit("open-subprocess");
  struct stdio_mode m, m_r = stdio_mode_init_r;
  val mode = normalize_mode(&m, mode_str, m_r);
  int input = m.read != 0;
  int fd[2];
  pid_t pid;
  char **argv = 0;
  val iter;
  int i, nargs;
  struct save_fds sfds;
  val ret = nil;

  args = default_null_arg(args);
  fun = default_null_arg(fun);
  nargs = c_num(length(args), self) + 1;

  if (!name && !fun)
    uw_throwf(error_s, lit("~a: program name and/or function required"), self, nao);

  fds_init(&sfds);

  uw_simple_catch_begin;

  fds_swizzle(&sfds, (input ? FDS_IN : FDS_OUT) | FDS_ERR, self);

  if (nargs < 0 || nargs == INT_MAX)
    uw_throwf(error_s, lit("~a: argument list overflow"), self, nao);

  if (name)
    argv = coerce(char **, chk_xalloc(nargs + 1, sizeof *argv, self));

  if (pipe(fd) == -1) {
    int eno = errno;
    free(argv);
    uw_throwf(errno_to_file_error(eno),
              lit("opening pipe ~s, pipe syscall failed: ~d/~s"),
              name, num(eno), errno_to_str(eno), nao);
  }

  if (argv) {
    for (i = 0, iter = cons(name, args); iter; i++, iter = cdr(iter)) {
      val arg = car(iter);
      argv[i] = utf8_dup_to(c_str(arg));
    }
    argv[i] = 0;
  }

  pid = fork();

  if (pid == -1) {
    if (argv) {
      for (i = 0; i < nargs; i++)
        free(argv[i]);
      free(argv);
    }
    uw_throwf(process_error_s, lit("opening pipe ~s, fork syscall failed: ~d/~s"),
              name, num(errno), errno_to_str(errno), nao);
  }

  if (pid == 0) {
    if (input) {
      dup2(fd[1], STDOUT_FILENO);
      if (fd[1] != STDOUT_FILENO) /* You never know */
        close(fd[1]);
      close(fd[0]);
    } else {
      dup2(fd[0], STDIN_FILENO);
      if (fd[0] != STDIN_FILENO) /* You never know */
        close(fd[0]);
      close(fd[1]);
    }

    for (i = 0; i < STDIO_MODE_NREDIRS; i++) {
      int to = m.redir[i][0];
      int from = m.redir[i][1];

      if (to < 0)
        break;

      switch (from) {
      case 'n':
#if HAVE_FCNTL
        {
          int n = open("/dev/null", O_RDWR);
          if (n > 0) {
            dup2(n, to);
            close(n);
          }
        }
#endif
        break;
      case 'x':
        close(to);
        break;
      default:
        dup2(from, to);
        break;
      }
    }

    if (fun)
      funcall(fun);

    if (argv)
      execvp(argv[0], argv);
    _exit(errno);
  } else {
    int whichfd;
    char *utf8mode = utf8_dup_to(c_str(mode));
    FILE *f;

    if (input) {
      close(fd[1]);
      whichfd = fd[0];
    } else {
      close(fd[0]);
      whichfd = fd[1];
    }

    if (argv) {
      for (i = 0; i < nargs; i++)
        free(argv[i]);
      free(argv);
    }

#if HAVE_FCNTL
    fcntl(whichfd, F_SETFD, FD_CLOEXEC);
#endif

    if ((f = fdopen(whichfd, utf8mode)) == 0) {
      int status;
      kill(pid, SIGINT);
      kill(pid, SIGTERM);
      while (waitpid(pid, &status, 0) == -1 && errno == EINTR)
        ;
      free(utf8mode);
      uw_throwf(file_error_s, lit("opening pipe ~s, fdopen failed: ~d/~s"),
                name, num(errno), errno_to_str(errno), nao);
    }

    free(utf8mode);
    /* TODO: catch potential OOM exception here and kill process. */
    ret = set_mode_props(m, make_pipevp_stream(f, name, pid));
  }

  uw_unwind {
    fds_restore(&sfds);
  }

  uw_catch_end;

  return ret;
}

val open_process(val name, val mode_str, val args)
{
  return open_subprocess(name, mode_str, args, nil);
}
#else

static void string_extend_count(int count, val out, val tail)
{
  int i;
  for (i = 0; i < count; i++)
    string_extend(out, tail);
}

static val win_escape_cmd(val str)
{
  const wchar_t *s;
  val out = string(L"");

  for (s = c_str(str); *s; s++) {
    switch (*s) {
    case ' ': case '\t':
      string_extend(out, lit("\""));
      string_extend(out, chr(*s));
      string_extend(out, lit("\""));
      break;
    default:
      string_extend(out, chr(*s));
    }
  }

  return out;
}

static val win_escape_arg(val str)
{
  int bscount = 0, i;
  const wchar_t *s;
  val out = string(L"");

  for (s = c_str(str); *s; s++) {
    switch (*s) {
    case '"':
      string_extend_count(bscount, out, lit("\\\\"));
      string_extend(out, lit("\\^\""));
      bscount = 0;
      break;
    case '\\':
      bscount++;
      break;
    case '^': case '%': case '!':
    case '\n': case '&': case '|':
    case '<': case '>':
    case '(': case ')':
      for (i = 0; i < bscount; i++)
        string_extend_count(bscount, out, lit("\\"));
      string_extend(out, chr('^'));
      string_extend(out, chr(*s));
      break;
    default:
      for (i = 0; i < bscount; i++)
        string_extend_count(bscount, out, lit("\\"));
      string_extend(out, chr(*s));
      bscount = 0;
      break;
    }
  }

  for (i = 0; i < bscount; i++)
    string_extend(out, lit("\\"));

  return out;
}

static val win_make_cmdline(val args)
{
  val out = string(L"");

  string_extend(out, win_escape_cmd(pop(&args)));
  string_extend(out, chr(' '));

  while (args) {
    string_extend(out, lit("^\""));
    string_extend(out, win_escape_arg(pop(&args)));
    if (args)
      string_extend(out, lit("^\" "));
    else
      string_extend(out, lit("^\""));
  }

  return out;
}

val open_process(val name, val mode_str, val args)
{
  val win_cmdline = win_make_cmdline(cons(name, default_null_arg(args)));
  return open_command(win_cmdline, mode_str);
}
#endif

#if HAVE_WSPAWN || HAVE_SPAWN

#if !HAVE_WSPAWN
static int w_spawnvp(int mode, const wchar_t *wpath, int wargc,
                     const wchar_t **wargv)
{
  char *path = utf8_dup_to(wpath);
  const char **argv = coerce(const char **,
			     chk_malloc(sizeof *argv * (wargc + 1)));
  int i, res;

  for (i = 0; i < wargc; i++)
    argv[i] = utf8_dup_to(wargv[i]);
  argv[i] = 0;

  res = spawnvp(mode, path, argv);

  for (i = 0; i < wargc; i++)
    free(strip_qual(char *, argv[i]));
  free(argv);
  free(path);

  return res;
}
#endif

static val run(val command, val args)
{
  val self = lit("run");
  const wchar_t **wargv = 0;
  val iter;
  int i, nargs, status = 0;
  struct save_fds sfds;

  args = default_null_arg(args);
  nargs = c_num(length(args), self) + 1;

  fds_init(&sfds);

  uw_simple_catch_begin;

  fds_swizzle(&sfds, FDS_IN | FDS_OUT | FDS_ERR, self);

  if (nargs < 0 || nargs == INT_MAX)
    uw_throwf(error_s, lit("~a: argument list overflow"), self, nao);

  wargv = coerce(const wchar_t **, chk_xalloc(nargs + 1, sizeof *wargv, self));

  for (i = 0, iter = cons(command, args); iter; i++, iter = cdr(iter))
    wargv[i] = c_str(car(iter));
  wargv[i] = 0;

#if HAVE_WSPAWN
  status = _wspawnvp(_P_WAIT, c_str(command), wargv);
#else
  status = w_spawnvp(_P_WAIT, c_str(command), nargs, wargv);
#endif

  free(strip_qual(wchar_t **, wargv));

  gc_hint(args);

  uw_unwind {
    fds_restore(&sfds);
  }

  uw_catch_end;

  return (status < 0) ? nil : num(status);
}

static val sh(val command)
{
  return run(lit("cmd.exe"), list(lit("/C"), command, nao));
}

#elif HAVE_FORK_STUFF

static val run(val name, val args)
{
  val self = lit("run");
  pid_t pid;
  char **argv = 0;
  val iter;
  int i, nargs;
  struct save_fds sfds;
  val ret = nil;

  args = default_null_arg(args);
  nargs = c_num(length(args), self) + 1;

  if (nargs < 0 || nargs == INT_MAX)
    uw_throwf(error_s, lit("~a: argument list overflow"), self, nao);

  argv = coerce(char **, chk_xalloc(nargs + 1, sizeof *argv, self));

  for (i = 0, iter = cons(name, args); iter; i++, iter = cdr(iter)) {
    val arg = car(iter);
    argv[i] = utf8_dup_to(c_str(arg));
  }
  argv[i] = 0;

  fds_init(&sfds);

  uw_simple_catch_begin;

  fds_swizzle(&sfds, FDS_IN | FDS_OUT | FDS_ERR, self);

  pid = fork();

  if (pid == -1) {
    for (i = 0; i < nargs; i++)
      free(argv[i]);
    free(argv);
    uw_throwf(process_error_s, lit("opening process ~s, fork syscall failed: ~d/~s"),
              name, num(errno), errno_to_str(errno), nao);
  }

  if (pid == 0) {
    execvp(argv[0], argv);
    _exit(errno);
  } else {
    int status, wres;
    for (i = 0; i < nargs; i++)
      free(argv[i]);
    free(argv);
    while ((wres = waitpid(pid, &status, 0)) == -1 && errno == EINTR)
      ;
    if (wres != -1) {
#if HAVE_SYS_WAIT
      if (WIFEXITED(status)) {
        int exitstatus = WEXITSTATUS(status);
        ret = num(exitstatus);
        goto out;
      }
#endif
      ret = (status == 0 ? zero : nil);
    }
  }

out:
  uw_unwind {
    fds_restore(&sfds);
  }

  uw_catch_end;

  return ret;
}

static val sh(val command)
{
  return run(shell, list(shell_arg, command, nao));
}

#else
#error port me!
#endif

val remove_path(val path, val throw_on_error)
{
  if (w_remove(c_str(path)) < 0) {
    if (default_null_arg(throw_on_error) || errno != ENOENT) {
      int eno = errno;
      uw_throwf(errno_to_file_error(eno), lit("trying to remove ~s: ~d/~s"),
                path, num(eno), errno_to_str(eno), nao);
    }
    return nil;
  }

  return t;
}

val rename_path(val from, val to)
{
  if (w_rename(c_str(from), c_str(to)) < 0) {
    int eno = errno;
    uw_throwf(errno_to_file_error(eno),
              lit("trying to rename ~s to ~s: ~d/~s"),
              from, to, num(eno), errno_to_str(eno), nao);
  }

  return t;
}

static val open_files(val file_list, val substitute_stream, val mode)
{
  substitute_stream = default_null_arg(substitute_stream);
  mode = default_null_arg(mode);

  if (nilp(file_list) && substitute_stream) {
    return substitute_stream;
  } else if (mode) {
    return make_catenated_stream(mapcar(pa_12_1(func_n2o(open_file, 1),
                                                mode), file_list));
  } else {
    return make_catenated_stream(mapcar(func_n2o(open_file, 1), file_list));
  }
}

static val open_files_star(val file_list, val substitute_stream, val mode)
{
  substitute_stream = default_null_arg(substitute_stream);
  mode = default_null_arg(mode);

  if (nilp(file_list) && substitute_stream) {
    return substitute_stream;
  } else if (mode) {
    return make_catenated_stream(lazy_mapcar(pa_12_1(func_n2o(open_file, 1),
                                                     mode), file_list));
  } else {
    return make_catenated_stream(lazy_mapcar(func_n2o(open_file, 1), file_list));
  }
}

static val ap_regex;

val portable_abs_path_p(val path)
{
  val ch;

  if (length(path) == zero)
    return nil;
  if ((ch = chr_str(path, zero)) == chr('/') || ch == chr('\\'))
    return t;

  if (!ap_regex)
    ap_regex = regex_compile(lit("[A-Za-z0-9]+:[/\\\\]"), nil);

  if (match_regex(path, ap_regex, zero))
    return t;

  return nil;
}

val abs_path_p(val path)
{
  const wchar_t *psc = coerce(const wchar_t *, path_sep_chars);

  if (length(path) == zero)
    return nil;

  if (wcschr(psc, c_chr(chr_str(path, zero))))
    return t;

  if (psc[0] != '\\')
    return nil;

  if (!ap_regex)
    ap_regex = regex_compile(lit("[A-Za-z0-9]+:[/\\\\]"), nil);

  if (match_regex(path, ap_regex, zero))
    return t;

  return nil;
}

static val plp_regex;

val pure_rel_path_p(val path)
{
  val ch;
  val len = length_str(path);

  if (len == zero)
    return t;

  if ((ch = chr_str(path, zero)) == chr('/') || ch == chr('\\'))
    return nil;

  if (len == one)
    return ch == chr('.') ? nil : t;

  if (ch == chr('.') &&
      ((ch = chr_str(path, one)) == chr('/') || ch == chr('\\')))
    return nil;

  if (!plp_regex)
    plp_regex = regex_compile(lit("[A-Za-z0-9]+:"), nil);

  if (match_regex(path, plp_regex, zero))
    return nil;

  return t;
}

static void detect_path_separators(void)
{
#ifdef __CYGWIN__
  struct utsname un;

  if (uname(&un) >= 0) {
    if (strncmp(un.sysname, "CYGNAL", 6) == 0)
      path_sep_chars = wli("\\/");
    return;
  }
#endif
}

val base_name(val path, val suff)
{
  val self = lit("base-name");
  const wchar_t *wpath = c_str(path);
  const wchar_t *end = wpath + c_num(length_str(path), self);
  const wchar_t *rsep;
  const wchar_t *psc = coerce(const wchar_t *, path_sep_chars);

  if (end == wpath)
    return null_string;

  while (wpath < end && wcschr(psc, end[-1]))
    end--;

  if (end == wpath)
    return lit("/");

  for (rsep = end;
       wpath < rsep && wcschr(psc, rsep[-1]) == 0;
       rsep--)
    ; /* *empty */


  {
    val base = mkustring(num_fast(end - rsep));
    init_str(base, rsep, self);
    return if3(!null_or_missing_p(suff) && ends_with(suff, base, nil, nil) &&
               neql(length(suff), length(base)),
               sub(base, zero, neg(length(suff))),
               base);
  }
}

val dir_name(val path)
{
  val self = lit("dir-name");
  const wchar_t *wpath = c_str(path);
  const wchar_t *rsep = wpath + c_num(length_str(path), self);
  const wchar_t *psc = coerce(const wchar_t *, path_sep_chars);

  if (rsep == wpath)
    return lit(".");

  if (wcschr(psc, rsep[-1]))
    rsep--;

  if (rsep == wpath) {
    wchar_t root[2] = { psc[0] };
    return string(root);
  }

  for (; rsep > wpath && wcschr(psc, rsep[-1]) == 0; rsep--)
    ; /* *empty */

  if (rsep == wpath + 1) {
    wchar_t root[2] = { psc[0] };
    return string(root);
  }

  if (rsep == wpath)
    return lit(".");


  {
    val base = mkustring(num_fast(rsep - wpath - 1));
    return init_str(base, wpath, self);
  }
}

val path_cat(val dir_name, val base_name)
{
  val dl = length(dir_name);
  val bl = length(base_name);
  val ps = static_str(path_sep_chars);

  if (dl == zero)
    return base_name;

  if (bl == zero)
    return dir_name;

  if (find(chr_str(dir_name, pred(dl)), ps, nil, nil)) {
    val bl0 = chr_str(base_name, zero);

    if (bl == one && bl0 == chr('.'))
      return dir_name;

    if (dl == two && chr_str(dir_name, zero) == chr('.'))
      return base_name;

    if (!find(bl0, ps, nil, nil))
      return scat(nil, dir_name, base_name, nao);

    return scat(nil, dir_name, sub(base_name, one, t), nao);
  }

  if (find(chr_str(base_name, zero), ps, nil, nil))
    return scat(nil, dir_name, base_name, nao);

  if (bl == one && chr_str(base_name, zero) == chr('.'))
    return dir_name;

  if (dl == one && chr_str(dir_name, zero) == chr('.'))
    return base_name;

  return scat(lit("/"), dir_name, base_name, nao);
}

val make_byte_input_stream(val obj)
{
  val self = lit("make-byte-input-stream");

  switch (type(obj)) {
  case LIT:
  case STR:
  case LSTR:
    return make_string_byte_input_stream(obj);
  case BUF:
    return make_buf_stream(obj);
  default:
    uw_throwf(file_error_s, lit("~a: ~s is neither a string nor buffer"),
              self, obj, nao);
  }
}

val tmpfile_wrap(void)
{
  struct stdio_mode m_blank = stdio_mode_init_blank;
  struct stdio_mode m = do_parse_mode(lit("w+b"), m_blank);
  FILE *tf = tmpfile();
  if (tf != 0)
    return set_mode_props(m, make_stdio_stream(tf, lit("tmpfile")));
  uw_throwf(file_error_s, lit("tmpnam failed: ~d/~s"),
            num(errno), errno_to_str(errno), nao);
}

#if HAVE_MKDTEMP

val mkdtemp_wrap(val prefix)
{
  char *tmpl = utf8_dup_to(c_str(scat2(prefix, lit("XXXXXX"))));

  if (mkdtemp(tmpl) != 0) {
    val ret = string_utf8(tmpl);
    free(tmpl);
    return ret;
  }

  free(tmpl);
  uw_throwf(file_error_s, lit("mkdtemp failed: ~d/~s"),
            num(errno), errno_to_str(errno), nao);
}

#endif

#if HAVE_MKSTEMP

val mkstemp_wrap(val prefix, val suffix)
{
  val self = lit("mkstemp");
  val suff = default_arg(suffix, null_string);
  val templ = scat3(prefix, lit("XXXXXX"), suff);
  cnum slen = c_num(length(suff), self);
  char *tmpl = utf8_dup_to(c_str(templ));
  val name;
  int fd;

#if HAVE_MKSTEMPS
  fd = mkstemps(tmpl, slen);
#else
  if (slen > 0) {
    free(tmpl);
    uw_throwf(system_error_s, lit("~a: suffix not supported"), self, nao);
  }
  fd = mkstemp(tmpl);
#endif
  name = string_utf8(tmpl);
  free(tmpl);
  if (fd != -1) {
    val stream = open_fileno(num(fd), lit("w+b"));
    stream_set_prop(stream, name_k, name);
    return stream;
  }
  uw_throwf(file_error_s, lit("~a failed: ~d/~s"),
            self, num(errno), errno_to_str(errno), nao);
}

#endif

static val iobuf_free_list;

val iobuf_get(void)
{
  val buf = iobuf_free_list;

  if (buf) {
    val next = buf->b.len;
    buf->b.len = buf->b.size;
    iobuf_free_list = next;
    return buf;
  } else {
    return make_buf(num_fast(BUFSIZ), nil, nil);
  }
}

void iobuf_put(val buf)
{
  buf->b.len = iobuf_free_list;
  iobuf_free_list = buf;
}

void iobuf_list_empty(void)
{
  iobuf_free_list = nil;
}

void stream_init(void)
{
  prot1(&ap_regex);
  prot1(&plp_regex);

  detect_format_string();
  detect_path_separators();

  from_start_k = intern(lit("from-start"), keyword_package);
  from_current_k = intern(lit("from-current"), keyword_package);
  from_end_k = intern(lit("from-end"), keyword_package);
  real_time_k = intern(lit("real-time"), keyword_package);
  name_k = intern(lit("name"), keyword_package);
  addr_k = intern(lit("addr"), keyword_package);
  fd_k = intern(lit("fd"), keyword_package);
  byte_oriented_k = intern(lit("byte-oriented"), keyword_package);
  format_s = intern(lit("format"), user_package);
  stdio_stream_s = intern(lit("stdio-stream"), user_package);
#if HAVE_SOCKETS
  socket_error_s = intern(lit("socket-error"), user_package);
#endif

  put_string_s = intern(lit("put-string"), user_package);
  put_char_s = intern(lit("put-char"), user_package);
  put_byte_s = intern(lit("put-byte"), user_package);
  get_line_s = intern(lit("get-line"), user_package);
  get_char_s = intern(lit("get-char"), user_package);
  get_byte_s = intern(lit("get-byte"), user_package);
  unget_char_s = intern(lit("unget-char"), user_package);
  unget_byte_s = intern(lit("unget-byte"), user_package);
  put_buf_s = intern(lit("put-buf"), user_package);
  fill_buf_s = intern(lit("fill-buf"), user_package);
  flush_s = intern(lit("flush"), user_package);
  seek_s = intern(lit("seek"), user_package);
  truncate_s = intern(lit("truncate"), user_package);
  get_prop_s = intern(lit("get-prop"), user_package);
  set_prop_s = intern(lit("set-prop"), user_package);
  get_error_s = intern(lit("get-error"), user_package);
  get_error_str_s = intern(lit("get-error-str"), user_package);
  clear_error_s = intern(lit("clear-error"), user_package);
  get_fd_s = intern(lit("get-fd"), user_package);

  reg_var(stdin_s = intern(lit("*stdin*"), user_package),
          make_stdio_stream(stdin, lit("stdin")));
  reg_var(stdout_s = intern(lit("*stdout*"), user_package),
          make_stdio_stream(stdout, lit("stdout")));
  reg_var(stddebug_s = intern(lit("*stddebug*"), user_package),
          make_stdio_stream(stdout, lit("debug")));
  reg_var(stderr_s = intern(lit("*stderr*"), user_package),
          make_stdio_stream(stderr, lit("stderr")));
  reg_var(stdnull_s = intern(lit("*stdnull*"), user_package),
          make_null_stream());

  reg_var(print_flo_precision_s = intern(lit("*print-flo-precision*"),
                                         user_package),
          num_fast(DBL_DIG));
  reg_var(print_flo_digits_s = intern(lit("*print-flo-digits*"),
                                      user_package),
          num_fast(3));
  reg_var(print_flo_format_s = intern(lit("*print-flo-format*"),
                                      user_package),
          lit("~s"));
  reg_var(pprint_flo_format_s = intern(lit("*pprint-flo-format*"),
                                       user_package),
          lit("~a"));
  reg_var(print_base_s = intern(lit("*print-base*"), user_package),
          num_fast(10));
  reg_var(print_circle_s = intern(lit("*print-circle*"), user_package), nil);

#if HAVE_ISATTY
  if (isatty(fileno(stdin)) == 1) {
    stream_set_prop(std_input, real_time_k, t);
    setbuf(stdin, 0);
  }
#endif

  reg_fun(format_s, func_n2v(formatv));
  reg_fun(intern(lit("fmt"), user_package), func_n1v(fmt));
  reg_fun(intern(lit("make-string-input-stream"), user_package), func_n1(make_string_input_stream));
  reg_fun(intern(lit("make-string-byte-input-stream"), user_package), func_n1(make_string_byte_input_stream));
  reg_fun(intern(lit("make-strlist-input-stream"), user_package), func_n1(make_strlist_input_stream));
  reg_fun(intern(lit("make-string-output-stream"), user_package), func_n0(make_string_output_stream));
  reg_fun(intern(lit("get-string-from-stream"), user_package), func_n1(get_string_from_stream));
  reg_fun(intern(lit("make-strlist-output-stream"), user_package), func_n0(make_strlist_output_stream));
  reg_fun(intern(lit("get-list-from-stream"), user_package), func_n1(get_list_from_stream));
  reg_fun(intern(lit("make-byte-input-stream"), user_package), func_n1(make_byte_input_stream));
  reg_fun(intern(lit("close-stream"), user_package), func_n2o(close_stream, 1));
  reg_fun(get_error_s, func_n1(get_error));
  reg_fun(get_error_str_s, func_n1(get_error_str));
  reg_fun(clear_error_s, func_n1(clear_error));
  reg_fun(get_line_s, func_n1o(get_line, 0));
  reg_fun(get_char_s, func_n1o(get_char, 0));
  reg_fun(get_byte_s, func_n1o(get_byte, 0));
  reg_fun(intern(lit("get-string"), user_package), func_n3o(get_string, 0));
  reg_fun(put_string_s, func_n2o(put_string, 1));
  reg_fun(intern(lit("put-line"), user_package), func_n2o(put_line, 0));
  reg_fun(put_char_s, func_n2o(put_char, 1));
  reg_fun(put_byte_s, func_n2o(put_byte, 1));
  reg_fun(intern(lit("put-lines"), user_package), func_n2o(put_lines, 1));
  reg_fun(intern(lit("put-strings"), user_package), func_n2o(put_strings, 1));
  reg_fun(unget_char_s, func_n2o(unget_char, 1));
  reg_fun(unget_byte_s, func_n2o(unget_byte, 1));
  reg_fun(put_buf_s, func_n3o(put_buf, 1));
  reg_fun(fill_buf_s, func_n3o(fill_buf, 1));
  reg_fun(intern(lit("get-line-as-buf"), user_package), func_n1o(get_line_as_buf, 0));
  reg_fun(intern(lit("fill-buf-adjust"), user_package), func_n3o(fill_buf_adjust, 1));
  reg_fun(intern(lit("flush-stream"), user_package), func_n1o(flush_stream, 0));
  reg_fun(intern(lit("seek-stream"), user_package), func_n3(seek_stream));
  reg_fun(intern(lit("truncate-stream"), user_package), func_n2o(truncate_stream, 1));
  reg_fun(intern(lit("streamp"), user_package), func_n1(streamp));
  reg_fun(intern(lit("real-time-stream-p"), user_package), func_n1(real_time_stream_p));
  reg_fun(intern(lit("stream-set-prop"), user_package), func_n3(stream_set_prop));
  reg_fun(intern(lit("stream-get-prop"), user_package), func_n2(stream_get_prop));
  reg_fun(intern(lit("fileno"), user_package), func_n1(stream_fd));
#if HAVE_SOCKETS
  reg_fun(intern(lit("sock-family"), user_package), func_n1(sock_family));
  reg_fun(intern(lit("sock-type"), user_package), func_n1(sock_type));
  reg_fun(intern(lit("sock-peer"), user_package), func_n1(sock_peer));
  reg_fun(intern(lit("sock-set-peer"), user_package), func_n2(sock_set_peer));
#endif
  reg_fun(intern(lit("make-catenated-stream"), user_package), func_n0v(make_catenated_stream_v));
  reg_fun(intern(lit("cat-streams"), user_package), func_n1(make_catenated_stream));
  reg_fun(intern(lit("catenated-stream-p"), user_package), func_n1(catenated_stream_p));
  reg_fun(intern(lit("catenated-stream-push"), user_package), func_n2(catenated_stream_push));
  reg_fun(intern(lit("record-adapter"), user_package), func_n3o(record_adapter, 1));
  reg_fun(intern(lit("open-directory"), user_package), func_n1(open_directory));
  reg_fun(intern(lit("open-file"), user_package), func_n2o(open_file, 1));
  reg_fun(intern(lit("open-fileno"), user_package), func_n2o(open_fileno, 1));
  reg_fun(intern(lit("open-tail"), user_package), func_n3o(open_tail, 1));
  reg_fun(intern(lit("open-command"), user_package), func_n2o(open_command, 1));
  reg_fun(intern(lit("open-pipe"), user_package), func_n2(open_command));
  reg_fun(intern(lit("open-process"), user_package), func_n3o(open_process, 2));
#if HAVE_FORK_STUFF
  reg_fun(intern(lit("open-subprocess"), user_package), func_n4o(open_subprocess, 2));
#endif
  reg_fun(intern(lit("sh"), user_package), func_n1(sh));
  reg_fun(intern(lit("run"), user_package), func_n2o(run, 1));
  reg_fun(intern(lit("remove-path"), user_package), func_n2o(remove_path, 1));
  reg_fun(intern(lit("rename-path"), user_package), func_n2(rename_path));
  reg_fun(intern(lit("open-files"), user_package), func_n3o(open_files, 1));
  reg_fun(intern(lit("open-files*"), user_package), func_n3o(open_files_star, 1));
  reg_fun(intern(lit("portable-abs-path-p"), user_package), func_n1(portable_abs_path_p));
  reg_fun(intern(lit("abs-path-p"), user_package),
          func_n1(if3(opt_compat && opt_compat <= 258,
                      portable_abs_path_p, abs_path_p)));
  reg_fun(intern(lit("pure-rel-path-p"), user_package), func_n1(pure_rel_path_p));
  reg_fun(intern(lit("base-name"), user_package), func_n2o(base_name, 1));
  reg_fun(intern(lit("dir-name"), user_package), func_n1(dir_name));
  reg_fun(intern(lit("path-cat"), user_package), func_n2(path_cat));
  reg_varl(intern(lit("path-sep-chars"), user_package), static_str(path_sep_chars));
  reg_fun(intern(lit("get-indent-mode"), user_package), func_n1(get_indent_mode));
  reg_fun(intern(lit("test-set-indent-mode"), user_package), func_n3(test_set_indent_mode));
  reg_fun(intern(lit("test-neq-set-indent-mode"), user_package), func_n3(test_neq_set_indent_mode));
  reg_fun(intern(lit("set-indent-mode"), user_package), func_n2(set_indent_mode));
  reg_fun(intern(lit("get-indent"), user_package), func_n1(get_indent));
  reg_fun(intern(lit("set-indent"), user_package), func_n2(set_indent));
  reg_fun(intern(lit("inc-indent"), user_package), func_n2(inc_indent));
  reg_fun(intern(lit("width-check"), user_package), func_n2(width_check));
  reg_fun(intern(lit("force-break"), user_package), func_n1(force_break));
  reg_fun(intern(lit("set-max-length"), user_package), func_n2(set_max_length));
  reg_fun(intern(lit("set-max-depth"), user_package), func_n2(set_max_depth));
  reg_varl(intern(lit("indent-off"), user_package), num_fast(indent_off));
  reg_varl(intern(lit("indent-data"), user_package), num_fast(indent_data));
  reg_varl(intern(lit("indent-code"), user_package), num_fast(indent_code));
  reg_varl(intern(lit("indent-foff"), user_package), num_fast(indent_foff));
  reg_fun(intern(lit("tmpfile"), user_package), func_n0(tmpfile_wrap));
#if HAVE_MKDTEMP
  reg_fun(intern(lit("mkdtemp"), user_package), func_n1(mkdtemp_wrap));
#endif
#if HAVE_MKSTEMP
  reg_fun(intern(lit("mkstemp"), user_package), func_n2o(mkstemp_wrap, 1));
#endif

#if HAVE_SOCKETS
  uw_register_subtype(socket_error_s, error_s);
#endif

  fill_stream_ops(&null_ops);
  fill_stream_ops(&stdio_ops);
  fill_stream_ops(&tail_ops);
  fill_stream_ops(&pipe_ops);
  fill_stream_ops(&string_in_ops);
  fill_stream_ops(&byte_in_ops);
  fill_stream_ops(&strlist_in_ops);
  fill_stream_ops(&string_out_ops);
  fill_stream_ops(&strlist_out_ops);
  fill_stream_ops(&dir_ops);
  fill_stream_ops(&cat_stream_ops);

#if HAVE_SOCKETS
  stdio_sock_ops = stdio_ops;
  stdio_sock_ops.name = wli("stream-sock");
  stdio_sock_ops.get_prop = sock_get_prop;
  stdio_sock_ops.set_prop = sock_set_prop;
  stdio_sock_ops.get_sock_family = stdio_get_sock_family;
  stdio_sock_ops.get_sock_type = stdio_get_sock_type;
  stdio_sock_ops.get_sock_peer = stdio_get_sock_peer;
  stdio_sock_ops.set_sock_peer = stdio_set_sock_peer;

  record_adapter_ops.get_sock_family = delegate_get_sock_family;
  record_adapter_ops.get_sock_type = delegate_get_sock_type;
  record_adapter_ops.get_sock_peer = delegate_get_sock_peer;
  record_adapter_ops.set_sock_peer = delegate_set_sock_peer;
#endif


  shell = lit("/bin/sh");
  shell_arg = lit("-c");

#ifdef __CYGWIN__
  {
    const char *sh = getusershell();
    if (sh && strstr(sh, "cmd.exe")) {
      prot1(&shell);
      shell = string_utf8(sh);
      shell_arg = lit("/c");
    }
  }
#endif
}
