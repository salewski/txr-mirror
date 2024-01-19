/* Copyright 2022-2024
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
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <wchar.h>
#include <signal.h>
#include <errno.h>
#include <zlib.h>
#include "config.h"
#if HAVE_SYS_WAIT
#include <sys/wait.h>
#endif
#include "alloca.h"
#include "lib.h"
#include "stream.h"
#include "gc.h"
#include "args.h"
#include "utf8.h"
#include "eval.h"
#include "signal.h"
#include "unwind.h"
#include "sysif.h"
#include "itypes.h"
#include "gzio.h"

struct gzio_handle {
  struct strm_base a;
  gzFile f;
  val descr;
  val unget_c;
  utf8_decoder_t ud;
  val err, errstr;
  char *buf;
  int fd;
#if HAVE_FORK_STUFF
  pid_t pid;
#endif
  unsigned is_byte_oriented : 8;
  unsigned is_output : 8;
};

val gzio_stream_s;

struct cobj_class *gzio_stream_cls;

static void gzio_stream_print(val stream, val out, val pretty,
                              struct strm_ctx *ctx)
{
  struct strm_ops *ops = coerce(struct strm_ops *, stream->co.ops);
  val name = static_str(ops->name);
  val descr = ops->get_prop(stream, name_k);

  (void) pretty;
  (void) ctx;

  format(out, lit("#<~a ~a ~p>"), name, descr, stream, nao);
}

static void gzio_stream_destroy(val stream)
{
  struct gzio_handle *h = coerce(struct gzio_handle *, stream->co.handle);
  close_stream(stream, nil);
  strm_base_cleanup(&h->a);
  free(h->buf);
  free(h);
}

static void gzio_stream_mark(val stream)
{
  struct gzio_handle *h = coerce(struct gzio_handle *, stream->co.handle);
  strm_base_mark(&h->a);
  gc_mark(h->descr);
  gc_mark(h->err);
  gc_mark(h->errstr);
}

static val gzio_maybe_read_error(val stream)
{
  struct gzio_handle *h = coerce(struct gzio_handle *, stream->co.handle);
  const char *gztxt;
  int gzerr;

  if (h->f == 0) {
    uw_throwf(file_error_s, lit("error reading ~s: file closed"), stream, nao);
  } else if (gzeof(h->f)) {
    h->err = t;
    h->errstr = lit("eof");
  } else if ((gztxt = gzerror(h->f, &gzerr)) != 0 && gzerr != Z_OK) {
    if (gzerr == Z_ERRNO) {
      int eno = errno;
      h->err = num(eno);
      h->errstr = nil;
#ifdef EAGAIN
      if (errno == EAGAIN)
        uw_ethrowf(timeout_error_s, lit("timed out reading ~s"), stream, nao);
#endif
      uw_ethrowf(file_error_s, lit("error reading ~s: ~d/~s"),
                 stream, h->err, errno_to_string(h->err), nao);
    } else {
      h->err = negone;
      h->errstr = string_utf8(gztxt);
    }
  } else {
    h->err = nil;
    h->errstr = lit("no error");
  }

  return nil;
}

static val gzio_maybe_error(val stream, val action)
{
  struct gzio_handle *h = coerce(struct gzio_handle *, stream->co.handle);
  val err = num(errno);
  if (h->f == 0)
    uw_ethrowf(file_error_s, lit("error ~a ~s: file closed"), action, stream, nao);
  h->err = err;
#ifdef EAGAIN
  if (errno == EAGAIN)
    uw_ethrowf(timeout_error_s, lit("timed out on ~s"), stream, nao);
#endif
  uw_ethrowf(file_error_s, lit("error ~a ~s: ~d/~s"),
             action, stream, err, errno_to_string(err), nao);
}

static val gzio_get_error(val stream)
{
  struct gzio_handle *h = coerce(struct gzio_handle *, stream->co.handle);
  if (h->f != 0 && gzeof(h->f))
    return t;
  return h->err;
}

static val gzio_get_error_str(val stream)
{
  struct gzio_handle *h = coerce(struct gzio_handle *, stream->co.handle);

  if (h->f != 0 && gzeof(h->f))
    return lit("eof");
  return h->errstr;
}

static val gzio_clear_error(val stream)
{
  struct gzio_handle *h = coerce(struct gzio_handle *, stream->co.handle);
  val ret = h->err;
  if (h->f != 0)
    gzclearerr(h->f);
  h->err = h->errstr = lit("no error");
  return ret;
}

static val gzio_get_fd(val stream)
{
  struct gzio_handle *h = coerce(struct gzio_handle *, stream->co.handle);
  return (h->f && h->fd != -1) ? num(h->fd) : nil;
}

static int se_gzputc(int ch, gzFile f)
{
  int ret;
  sig_save_enable;
  ret = gzputc(f, ch);
  sig_restore_enable;
  return ret;
}

static int se_gzgetc(gzFile f)
{
  int ret;
  sig_save_enable;
  ret = gzgetc(f);
  sig_restore_enable;
  return ret;
}

static int gzio_get_char_callback(mem_t *f)
{
  return se_gzgetc(coerce(gzFile, f));
}

static val gzio_get_char(val stream)
{
  struct gzio_handle *h = coerce(struct gzio_handle *, stream->co.handle);

  if (h->unget_c)
    return rcyc_pop(&h->unget_c);

  if (h->f) {
    wint_t ch;

    if (h->is_byte_oriented) {
      ch = se_gzgetc(h->f);
      if (ch == 0)
        ch = 0xDC00;
    } else {
        ch = utf8_decode(&h->ud, gzio_get_char_callback,
                         coerce(mem_t *, h->f));
    }

    return (ch != WEOF) ? chr(ch) : gzio_maybe_read_error(stream);
  }
  return gzio_maybe_read_error(stream);
}

static val gzio_get_byte(val stream)
{
  struct gzio_handle *h = coerce(struct gzio_handle *, stream->co.handle);

  if (h->f) {
    int ch = se_gzgetc(h->f);
    return (ch != EOF) ? num(ch) : gzio_maybe_read_error(stream);
  }
  return gzio_maybe_read_error(stream);
}

static val gzio_unget_char(val stream, val ch)
{
  struct gzio_handle *h = coerce(struct gzio_handle *, stream->co.handle);
  mpush(ch, mkloc(h->unget_c, stream));
  return ch;
}

static val gzio_unget_byte(val stream, int byte)
{
  struct gzio_handle *h = coerce(struct gzio_handle *, stream->co.handle);

  errno = 0;
  return h->f != 0 && gzungetc(byte, coerce(gzFile, h->f)) != EOF
         ? num_fast(byte)
         : gzio_maybe_error(stream, lit("writing"));
}

static ucnum gzio_fill_buf(val stream, mem_t *ptr, ucnum len, ucnum pos)
{
  val self = lit("fill-buf");
  struct gzio_handle *h = coerce(struct gzio_handle *, stream->co.handle);
  if (convert(size_t, len) != len || len > INT_PTR_MAX)
    uw_throwf(error_s, lit("~a: buffer too large"), self, nao);
  if (pos >= len)
    return len;
  errno = 0;
  if (h->f != 0) {
    cnum nread = gzread(h->f, ptr + pos, len - pos);
    if (nread > 0)
      return pos + nread;
  }
  gzio_maybe_read_error(stream);
  return pos;
}

static val gzio_close(val stream, val throw_on_error)
{
  struct gzio_handle *h = coerce(struct gzio_handle *, stream->co.handle);

  if (h->f != 0) {
    int result = gzclose(h->f);
    h->f = 0;
    if (result != Z_OK) {
      if (default_null_arg(throw_on_error))
        gzio_maybe_error(stream, lit("closing"));
      return nil;
    }
#if HAVE_FORK_STUFF
    if (h->pid != 0)
    {
      int status = 0;
      val self = lit("close-stream");
      sig_save_enable;
      while (waitpid(h->pid, &status, 0) == -1 && errno == EINTR)
        ;
      sig_restore_enable;
      return pipe_close_status_helper(stream, throw_on_error, status, self);
    }
#endif
  }
  return nil;
}

static val num_z_off_t(z_off_t off)
{
  if (sizeof (off) <= sizeof (cnum)) {
    return num(off);
  } else if (NUM_MIN <= off && off <= NUM_MAX) {
    return num(off);
  } else if (sizeof (off) <= sizeof (i64_t)) {
    return num_64(off);
  } else {
    internal_error("portme: unsupported z_off_t size");
  }
}
static z_off_t z_off_t_num(val num, val self)
{
  switch (CHAR_BIT * sizeof(z_off_t)) {
  case 32:
    return c_i32(num, self);
  case 64:
    return c_i64(num, self);
  default:
    internal_error("portme: unsupported z_off_t size");
  }
}
static val gzio_seek(val stream, val offset, enum strm_whence whence)
{
  struct gzio_handle *h = coerce(struct gzio_handle *, stream->co.handle);
  val self = lit("seek-stream");

  errno = 0;

  if (h->f != 0) {
    if (offset == zero && whence == strm_cur) {
      return num_z_off_t(gztell(h->f));
    } else {
      if (gzseek(h->f, z_off_t_num(offset, self), whence) >= 0) {
        if (!h->is_output)
          utf8_decoder_init(&h->ud);
        h->unget_c = nil;
        return t;
      }
    }
  }

  return gzio_maybe_error(stream, lit("seeking"));
}

static int gzio_put_char_callback(int ch, mem_t *f)
{
  int ret = se_gzputc(ch, coerce(gzFile, f)) != EOF;
  return ret;
}

static val gzio_put_string(val stream, val str)
{
  val self = lit("put-string");
  struct gzio_handle *h = coerce(struct gzio_handle *, stream->co.handle);

  errno = 0;

  if (h->f != 0) {
    const wchar_t *s = c_str(str, self);

    while (*s) {
      if (!utf8_encode(*s++, gzio_put_char_callback, coerce(mem_t *, h->f)))
        return gzio_maybe_error(stream, lit("writing"));
    }
    return t;
  }
  return gzio_maybe_error(stream, lit("writing"));
}

static val gzio_put_char(val stream, val ch)
{
  struct gzio_handle *h = coerce(struct gzio_handle *, stream->co.handle);
  errno = 0;
  return h->f != 0 && utf8_encode(c_chr(ch), gzio_put_char_callback,
                                  coerce(mem_t *, h->f))
         ? t : gzio_maybe_error(stream, lit("writing"));
}

static val gzio_put_byte(val stream, int b)
{
  struct gzio_handle *h = coerce(struct gzio_handle *, stream->co.handle);
  errno = 0;
  return h->f != 0 && se_gzputc(b, coerce(gzFile, h->f)) != EOF
         ? t : gzio_maybe_error(stream, lit("writing"));
}

static ucnum gzio_put_buf(val stream, mem_t *ptr, ucnum len, ucnum pos)
{
  val self = lit("put-buf");
  struct gzio_handle *h = coerce(struct gzio_handle *, stream->co.handle);
  if (convert(size_t, len) != len || len > INT_PTR_MAX)
    uw_throwf(error_s, lit("~a: buffer too large"), self, nao);
  if (pos >= len)
    return len;
  errno = 0;
  if (h->f != 0) {
    cnum nwrit = gzwrite(h->f, ptr + pos, len - pos);
    if (nwrit > 0)
      return pos + nwrit;
  }
  gzio_maybe_error(stream, lit("writing"));
  return 0;
}

static val gzio_get_prop(val stream, val ind)
{
  struct gzio_handle *h = coerce(struct gzio_handle *, stream->co.handle);

  if (ind == name_k) {
    return h->descr;
  } else if (ind == byte_oriented_k) {
    return h->is_byte_oriented ? t : nil;
  }
  return nil;
}

static val gzio_set_prop(val stream, val ind, val prop)
{
  struct gzio_handle *h = coerce(struct gzio_handle *, stream->co.handle);

  if (ind == name_k) {
    h->descr = prop;
    return t;
  } else if (ind == byte_oriented_k) {
    h->is_byte_oriented = prop ? 1 : 0;
    return t;
  }

  return nil;
}

static struct strm_ops gzio_ops_rd =
  strm_ops_init(cobj_ops_init(eq,
                              gzio_stream_print,
                              gzio_stream_destroy,
                              gzio_stream_mark,
                              cobj_eq_hash_op),
                wli("gzip-input-stream"),
                0,
                0,
                0,
                generic_get_line,
                gzio_get_char,
                gzio_get_byte,
                gzio_unget_char,
                gzio_unget_byte,
                0,
                gzio_fill_buf,
                gzio_close,
                0,
                gzio_seek,
                0,
                gzio_get_prop,
                gzio_set_prop,
                gzio_get_error,
                gzio_get_error_str,
                gzio_clear_error,
                gzio_get_fd);

static struct strm_ops gzio_ops_wr =
  strm_ops_init(cobj_ops_init(eq,
                              gzio_stream_print,
                              gzio_stream_destroy,
                              gzio_stream_mark,
                              cobj_eq_hash_op),
                wli("gzip-output-stream"),
                gzio_put_string,
                gzio_put_char,
                gzio_put_byte,
                0,
                0,
                0,
                0,
                0,
                gzio_put_buf,
                0,
                gzio_close,
                0,
                gzio_seek,
                0,
                gzio_get_prop,
                gzio_set_prop,
                gzio_get_error,
                gzio_get_error_str,
                gzio_clear_error,
                gzio_get_fd);

void gzio_init(void)
{
  fill_stream_ops(&gzio_ops_rd);
  fill_stream_ops(&gzio_ops_wr);
  gzio_stream_s = intern(lit("gzip-stream"), user_package);
  gzio_stream_cls = cobj_register_super(gzio_stream_s, stream_cls);
}

gzFile w_gzopen_mode(const wchar_t *wname, const wchar_t *wmode,
                     const struct stdio_mode m, val self)
{
  if (m.buforder >= 0 || m.nonblock || m.notrunc || m.unbuf ||
      m.linebuf || m.interactive)
  {
    uw_throwf(file_error_s,
              lit("~a: invalid modes for gzip stream"), self, nao);
  }

  if (m.read && m.write) {
    uw_throwf(file_error_s,
              lit("~a: gzip stream cannot both read and write"), self, nao);
  }

#if HAVE_FCNTL
  {
    int fd = w_open_mode(wname, m);
    return (fd < 0) ? NULL : w_gzdopen_mode(fd, wmode, m, self);
  }
#else
  {
    char *name = utf8_dup_to(wname);
    char *mode = utf8_dup_to(wmode);
    gzFile f = gzopen(name, mode);
    free(name);
    free(mode);
    return f;
  }
#endif
}

gzFile w_gzdopen_mode(int fd, const wchar_t *wmode,
                      const struct stdio_mode m, val self)
{
  if (m.buforder >= 0 || m.nonblock || m.notrunc || m.unbuf ||
      m.linebuf || m.interactive)
  {
    goto badmode;
  }

  if (m.read && m.write) {
    uw_throwf(file_error_s,
              lit("~a: gzip stream cannot both read and write"), self, nao);
  }

  {
    char *mode = utf8_dup_to(wmode);
    gzFile f = gzdopen(fd, mode);
    free(mode);
    if (f)
      return f;
  }

badmode:
  uw_throwf(file_error_s,
            lit("~a: invalid modes for gzip stream"), self, nao);
}

val make_gzio_stream(gzFile f, int fd, val descr, int is_output)
{
  struct gzio_handle *h = coerce(struct gzio_handle *, chk_malloc(sizeof *h));
  val stream = cobj(coerce(mem_t *, h), gzio_stream_cls,
                    if3(is_output,
                        &gzio_ops_wr.cobj_ops, &gzio_ops_rd.cobj_ops));
  strm_base_init(&h->a);
  h->f = f;
  h->fd = fd;
  h->descr = descr;
  h->unget_c = nil;
  utf8_decoder_init(&h->ud);
  h->err = nil;
  h->errstr = lit("no error");
  h->buf = 0;
  h->is_byte_oriented = 0;
  h->is_output = is_output;
#if HAVE_FORK_STUFF
  h->pid = 0;
#endif
  return stream;
}

#if HAVE_FORK_STUFF

val make_gzio_pipe_stream(gzFile f, int fd, val descr, int is_output, pid_t pid)
{
  val stream = make_gzio_stream(f, fd, descr, is_output);
  struct gzio_handle *h = coerce(struct gzio_handle *, stream->co.handle);
  h->pid = pid;
  return stream;
}

#endif
