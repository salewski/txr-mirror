/* Copyright 2009-2015
 * Kaz Kylheku <kaz@kylheku.com>
 * Vancouver, Canada
 * All rights reserved.
 *
 * Redistribution of this software in source and binary forms, with or without
 * modification, is permitted provided that the following two conditions are met.
 *
 * Use of this software in any manner constitutes agreement with the disclaimer
 * which follows the two conditions.
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DAMAGES, HOWEVER CAUSED,
 * AND UNDER ANY THEORY OF LIABILITY, ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <stdio.h>
#include <string.h>
#include <dirent.h>
#include <stdarg.h>
#include <stdlib.h>
#include <setjmp.h>
#include <errno.h>
#include <ctype.h>
#include <wchar.h>
#include <signal.h>
#include "config.h"
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#if HAVE_FCNTL_H
#include <fcntl.h>
#endif
#include <float.h>
#if HAVE_SYS_WAIT
#include <sys/wait.h>
#endif
#if HAVE_WINDOWS_H
#include <windows.h>
#endif
#include "lib.h"
#include "gc.h"
#include "signal.h"
#include "unwind.h"
#include "stream.h"
#include "utf8.h"
#include "eval.h"
#include "regex.h"
#include "txr.h"

val stdin_s, stdout_s, stddebug_s, stderr_s, stdnull_s;

val dev_k, ino_k, mode_k, nlink_k, uid_k;
val gid_k, rdev_k, size_k, blksize_k, blocks_k;
val atime_k, mtime_k, ctime_k;
val from_start_k, from_current_k, from_end_k;
val real_time_k, name_k, fd_k;
val format_s;

void strm_base_init(struct strm_base *s)
{
  static struct strm_base init = { indent_off, 60, 10, 0, 0 };
  *s = init;
}

void strm_base_cleanup(struct strm_base *s)
{
  (void) s;
}

void strm_base_mark(struct strm_base *s)
{
  (void) s;
}

void stream_print_op(val stream, val out, val pretty)
{
  val name = stream_get_prop(stream, name_k);
  (void) pretty;
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

static noreturn void unimpl(val stream, val op)
{
  uw_throwf(file_error_s, lit("~a: not supported by stream ~s\n"),
            op, stream, nao);
  abort();
}

static noreturn val unimpl_put_string(val stream, val str)
{
  unimpl(stream, lit("put-string"));
}

static noreturn val unimpl_put_char(val stream, val ch)
{
  unimpl(stream, lit("put-char"));
}

static noreturn val unimpl_put_byte(val stream, int byte)
{
  unimpl(stream, lit("put-byte"));
}

static noreturn val unimpl_get_line(val stream)
{
  unimpl(stream, lit("get-line"));
}

static noreturn val unimpl_get_char(val stream)
{
  unimpl(stream, lit("get-char"));
}

static noreturn val unimpl_get_byte(val stream)
{
  unimpl(stream, lit("get-byte"));
}

static noreturn val unimpl_unget_char(val stream, val ch)
{
  unimpl(stream, lit("unget-char"));
}

static noreturn val unimpl_unget_byte(val stream, int byte)
{
  unimpl(stream, lit("unget-byte"));
}

static noreturn val unimpl_seek(val stream, cnum off, enum strm_whence whence)
{
  unimpl(stream, lit("seek"));
}

static val null_put_string(val stream, val str)
{
  return nil;
}

static val null_put_char(val stream, val ch)
{
  return nil;
}

static val null_put_byte(val stream, int byte)
{
  return nil;
}

static val null_get_line(val stream)
{
  return nil;
}

static val null_get_char(val stream)
{
  return nil;
}

static val null_get_byte(val stream)
{
  return nil;
}

static val null_close(val stream, val throw_on_error)
{
  return nil;
}

static val null_flush(val stream)
{
  return nil;
}

static val null_seek(val stream, cnum off, enum strm_whence whence)
{
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
  return nil;
}

static val null_get_error(val stream)
{
  return nil;
}

static val null_get_error_str(val stream)
{
  return nil;
}

static val null_clear_error(val stream)
{
  return nil;
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
  if (!ops->close)
    ops->close = null_close;
  if (!ops->flush)
    ops->flush = null_flush;
  if (!ops->seek)
    ops->seek = unimpl_seek;
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
}

static struct strm_ops null_ops =
  strm_ops_init(cobj_ops_init(eq,
                              stream_print_op,
                              stream_destroy_op,
                              stream_mark_op,
                              cobj_hash_op),
                wli("null-stream"),
                null_put_string, null_put_char, null_put_byte, null_get_line,
                null_get_char, null_get_byte,
                unimpl_unget_char, unimpl_unget_byte,
                null_close, null_flush, null_seek, null_get_prop,
                null_set_prop, null_get_error, null_get_error_str,
                null_clear_error);

val make_null_stream(void)
{
  struct strm_base *s = coerce(struct strm_base *, chk_malloc(sizeof *s));
  strm_base_init(s);
  return cobj(convert(mem_t *, s), stream_s, &null_ops.cobj_ops);
}

struct stdio_handle {
  struct strm_base a;
  FILE *f;
  val descr;
  val unget_c;
  utf8_decoder_t ud;
  val err;
#if HAVE_FORK_STUFF
  pid_t pid;
#else
  int pid;
#endif
  val mode; /* used by tail */
  unsigned is_rotated; /* used by tail */
  unsigned is_real_time;
};

static void stdio_stream_print(val stream, val out, val pretty)
{
  struct stdio_handle *h = coerce(struct stdio_handle *, stream->co.handle);
  struct strm_ops *ops = coerce(struct strm_ops *, stream->co.ops);
  val name = static_str(ops->name);

  (void) pretty;

  if (h->pid)
    format(out, lit("#<~a ~s ~s ~p>"), name, h->descr, num(h->pid), stream, nao);
  else
    format(out, lit("#<~a ~s ~p>"), name, h->descr, stream, nao);
}

static void stdio_stream_destroy(val stream)
{
  struct stdio_handle *h = coerce(struct stdio_handle *, stream->co.handle);
  close_stream(stream, nil);
  strm_base_cleanup(&h->a);
  free(h);
}

static void stdio_stream_mark(val stream)
{
  struct stdio_handle *h = coerce(struct stdio_handle *, stream->co.handle);
  strm_base_mark(&h->a);
  gc_mark(h->descr);
  gc_mark(h->mode);
  gc_mark(h->err);
}

static val errno_to_string(val err)
{
  if (err == zero)
    return lit("unspecified error");
  else if (is_num(err))
    return string_utf8(strerror(c_num(err)));
  else if (err)
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
    uw_throwf(file_error_s, lit("error reading ~a: file closed"), stream, nao);
  if (ferror(h->f)) {
    val err = num(errno);
    h->err = err;
    uw_throwf(file_error_s, lit("error reading ~a: ~a/~s"),
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
    uw_throwf(file_error_s, lit("error ~a ~a: file closed"), stream, action, nao);
  h->err = err;
  uw_throwf(file_error_s, lit("error ~a ~a: ~a/~s"),
            stream, action, err, errno_to_string(err), nao);
}

static int se_putc(int ch, FILE *f)
{
  int ret;
  sig_save_enable;
  ret = putc(ch, f);
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
  return h->f != 0 && utf8_encode(c_chr(ch), stdio_put_char_callback,
                                  coerce(mem_t *, h->f))
         ? t : stdio_maybe_error(stream, lit("writing"));
}

static val stdio_put_byte(val stream, int b)
{
  struct stdio_handle *h = coerce(struct stdio_handle *, stream->co.handle);
  errno = 0;
  return h->f != 0 && se_putc(b, coerce(FILE *, h->f)) != EOF
         ? t : stdio_maybe_error(stream, lit("writing"));
}

static val stdio_flush(val stream)
{
  struct stdio_handle *h = coerce(struct stdio_handle *, stream->co.handle);
  errno = 0;
  return (h->f != 0 && se_fflush(h->f) == 0)
         ? t : stdio_maybe_error(stream, lit("flushing"));
}

static val stdio_seek(val stream, cnum offset, enum strm_whence whence)
{
  struct stdio_handle *h = coerce(struct stdio_handle *, stream->co.handle);

  errno = 0;

  if (h->f != 0) {
    if (offset == 0 && whence == strm_cur) {
      long where = ftell(h->f);
      if (where >= 0)
        return num(where);
    } else {
      if (fseek(h->f, offset, whence) == 0) {
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
  } else if (ind == fd_k) {
    return h->f ? num(fileno(h->f)) : nil;
  }
  return nil;
}

static val stdio_set_prop(val stream, val ind, val prop)
{
  if (ind == real_time_k) {
    struct stdio_handle *h = coerce(struct stdio_handle *, stream->co.handle);
    h->is_real_time = prop ? 1 : 0;
    return t;
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

static wchar_t *snarf_line(struct stdio_handle *h)
{
  const size_t min_size = 512;
  size_t size = 0;
  size_t fill = 0;
  wchar_t *buf = 0;

  for (;;) {
    wint_t ch;

    if (h->unget_c) {
      ch = c_chr(h->unget_c);
      h->unget_c = nil;
    } else {
      ch = utf8_decode(&h->ud, stdio_get_char_callback, coerce(mem_t *, h->f));
    }

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
  if (buf)
    buf = coerce(wchar_t *, chk_realloc(coerce(mem_t *, buf),
                                        fill * sizeof *buf));

  return buf;
}

static val stdio_get_line(val stream)
{
  errno = 0;
  if (stream->co.handle == 0) {
    return stdio_maybe_read_error(stream);
  } else {
    struct stdio_handle *h = coerce(struct stdio_handle *, stream->co.handle);
    wchar_t *line = snarf_line(h);
    if (!line)
      return stdio_maybe_read_error(stream);
    return string_own(line);
  }
}

static val stdio_get_char(val stream)
{
  struct stdio_handle *h = coerce(struct stdio_handle *, stream->co.handle);
  val uc = h->unget_c;
  if (uc) {
    h->unget_c = nil;
    return uc;
  }
  if (h->f) {
    wint_t ch = utf8_decode(&h->ud, stdio_get_char_callback,
                            coerce(mem_t *, h->f));
    return (ch != WEOF) ? chr(ch) : stdio_maybe_read_error(stream);
  }
  return stdio_maybe_read_error(stream);
}

static val stdio_get_byte(val stream)
{
  struct stdio_handle *h = coerce(struct stdio_handle *, stream->co.handle);
  if (h->f) {
    int ch = se_getc(h->f);
    return (ch != EOF) ? num(ch) : stdio_maybe_read_error(stream);
  }
  return stdio_maybe_read_error(stream);
}

static val stdio_unget_char(val stream, val ch)
{
  struct stdio_handle *h = coerce(struct stdio_handle *, stream->co.handle);

  if (!is_chr(ch))
    type_mismatch(lit("unget-char: ~s is not a character"), ch, nao);

  if (h->unget_c)
    uw_throwf(file_error_s, lit("unget-char overflow on ~a: "), stream, nao);

  h->unget_c = ch;
  return ch;
}

static val stdio_unget_byte(val stream, int byte)
{
  struct stdio_handle *h = coerce(struct stdio_handle *, stream->co.handle);

  return h->f != 0 && ungetc(byte, coerce(FILE *, h->f)) != EOF
         ? num_fast(byte)
         : stdio_maybe_error(stream, lit("pushing back byte into"));
}

static val stdio_close(val stream, val throw_on_error)
{
  struct stdio_handle *h = coerce(struct stdio_handle *, stream->co.handle);

  if (h->f != 0 && h->f != stdin && h->f != stdout) {
    int result = fclose(h->f);
    h->f = 0;
    if (result == EOF && throw_on_error) {
      h->err = num(errno);
      uw_throwf(file_error_s, lit("error closing ~a: ~a/~s"),
                stream, num(errno), string_utf8(strerror(errno)), nao);
    }
    return result != EOF ? t : nil;
  }
  return nil;
}

static struct strm_ops stdio_ops =
  strm_ops_init(cobj_ops_init(eq,
                              stdio_stream_print,
                              stdio_stream_destroy,
                              stdio_stream_mark,
                              cobj_hash_op),
                wli("file-stream"),
                stdio_put_string,
                stdio_put_char,
                stdio_put_byte,
                stdio_get_line,
                stdio_get_char,
                stdio_get_byte,
                stdio_unget_char,
                stdio_unget_byte,
                stdio_close,
                stdio_flush,
                stdio_seek,
                stdio_get_prop,
                stdio_set_prop,
                stdio_get_error,
                stdio_get_error_str,
                stdio_clear_error);

static void tail_calc(unsigned long *state, int *sec, int *mod)
{
  unsigned long count = (*state)++;
  if (count > 32)
    count = 32;
  *sec = 1 << (count / 8);
  *mod = 8 >> (count / 8);
  if (*mod == 0)
    *mod = 1;
}

#if !HAVE_POSIX_SLEEP && HAVE_WINDOWS_H

int sleep(int sec);

int sleep(int sec)
{
   Sleep(sec * 1000);
   return 0;
}

#endif

static void tail_strategy(val stream, unsigned long *state)
{
  struct stdio_handle *h = coerce(struct stdio_handle *, stream->co.handle);
  int sec = 0, mod = 0;

  tail_calc(state, &sec, &mod);

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
    sleep(sec);
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

      /* Try to open the file.
       */
      if (!(newf = w_fopen(c_str(h->descr), c_str(h->mode)))) {
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
        tail_calc(state, &sec, &mod);
        sig_save_enable;
        sleep(sec);
        sig_restore_enable;
        continue;
      }

      /* We opened the new file. If we have no old file,
       * then this is all we have.
       */
      if (!h->f) {
        h->f = newf;
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
      return;
    }

    utf8_decoder_init(&h->ud);
  }
}

static val tail_get_line(val stream)
{
  unsigned long state = 0;
  val ret;

  while ((ret = stdio_get_line(stream)) == nil)
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
                              cobj_hash_op),
                wli("tail-stream"),
                stdio_put_string,
                stdio_put_char,
                stdio_put_byte,
                tail_get_line,
                tail_get_char,
                tail_get_byte,
                stdio_unget_char,
                stdio_unget_byte,
                stdio_close,
                stdio_flush,
                stdio_seek,
                stdio_get_prop,
                stdio_set_prop,
                stdio_get_error,
                stdio_get_error_str,
                stdio_clear_error);

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
                  lit("unable to obtain status of command ~a: ~a/~s"),
                  stream, num(errno), string_utf8(strerror(errno)), nao);
    } else {
#ifdef HAVE_SYS_WAIT
      if (throw_on_error) {
        if (WIFSIGNALED(status)) {
          int termsig = WTERMSIG(status);
          uw_throwf(process_error_s, lit("pipe ~a terminated by signal ~a"),
                    stream, num(termsig), nao);
#ifndef WIFCONTINUED
#define WIFCONTINUED(X) 0
#endif
        } else if (WIFSTOPPED(status) || WIFCONTINUED(status)) {
          uw_throwf(process_error_s,
                    lit("processes of closed pipe ~a still running"),
                    stream, nao);
        }
      }
      if (WIFEXITED(status)) {
        int exitstatus = WEXITSTATUS(status);
        return num(exitstatus);
      }
#else
      if (status != 0 && throw_on_error)
        uw_throwf(process_error_s, lit("closing pipe ~a failed"), stream, nao);
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
                              cobj_hash_op),
                wli("pipe-stream"),
                stdio_put_string,
                stdio_put_char,
                stdio_put_byte,
                stdio_get_line,
                stdio_get_char,
                stdio_get_byte,
                stdio_unget_char,
                stdio_unget_byte,
                pipe_close,
                stdio_flush,
                0, /* seek: not on pipes */
                stdio_get_prop,
                stdio_set_prop,
                stdio_get_error,
                stdio_get_error_str,
                stdio_clear_error);

struct stdio_mode {
  int malformed;
  int read;
  int write;
  int create;
  int append;
  int binary;
  int interactive;
};

#define stdio_mode_init_trivial(read) { 0, read, 0, 0, 0, 0, 0 }

static struct stdio_mode parse_mode(val mode_str)
{
  struct stdio_mode m = stdio_mode_init_trivial(0);
  const wchar_t *ms = c_str(mode_str);

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
    break;
  default:
    m.malformed = 1;
    return m;
  }

  if (*ms == '+') {
    ms++;
    if (m.read)
      m.write = 1;
    m.read = 1;
  }

  for (; *ms; ms++) {
    switch (*ms) {
    case 'b':
      m.binary = 1;
      break;
    case 'i':
      m.interactive = 1;
      break;
    default:
      m.malformed = 1;
      return m;
    }
  }

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

  *ptr = 0;
  return string(buf);
}

static val normalize_mode(struct stdio_mode *m, val mode_str)
{
  struct stdio_mode blank = stdio_mode_init_trivial(1);

  if (null_or_missing_p(mode_str)) {
    *m = blank;
    return lit("r");
  } else {
    *m = parse_mode(mode_str);

    if (m->malformed)
      uw_throwf(file_error_s, lit("invalid file open mode ~a"), mode_str, nao);

    if (!m->interactive)
      return mode_str;

    return format_mode(*m);
  }
}

static val set_mode_props(const struct stdio_mode m, val stream)
{
  if (m.interactive)
    stream_set_prop(stream, real_time_k, t);
  return stream;
}

static val make_stdio_stream_common(FILE *f, val descr, struct cobj_ops *ops)
{
  struct stdio_handle *h = coerce(struct stdio_handle *, chk_malloc(sizeof *h));
  val stream = cobj(coerce(mem_t *, h), stream_s, ops);
  strm_base_init(&h->a);
  h->f = f;
  h->descr = descr;
  h->unget_c = nil;
  utf8_decoder_init(&h->ud);
  h->err = nil;
  h->pid = 0;
  h->mode = nil;
  h->is_rotated = 0;
#if HAVE_ISATTY
  h->is_real_time = if3(opt_compat && opt_compat <= 105,
                        (h->f != 0 && isatty(fileno(h->f)) == 1), 0);
#else
  h->is_real_time = 0;
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
                              cobj_hash_op),
                wli("dir-stream"),
                0, 0, 0,
                dir_get_line,
                0, 0, 0, 0,
                dir_close,
                0, 0, 0, 0,
                dir_get_error,
                dir_get_error_str,
                dir_clear_error);

val make_dir_stream(DIR *dir)
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
  const wchar_t *str = c_str(string);
  cnum pos = c_num(start);
  cnum len = c_num(length_str(string));
  wchar_t c = c_chr(ch);

  for (; pos < len; pos++) {
    if (str[pos] == c)
      return num(pos);
  }

  return nil;
}

static val string_in_get_line(val stream)
{
  struct string_in *s = coerce(struct string_in *, stream->co.handle);

  if (lt(s->pos, length_str(s->string))) {
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

  if (lt(s->pos, length_str(s->string))) {
    set(mkloc(s->pos, stream), plus(s->pos, one));
    return chr_str(s->string, s->pos);
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
              nao);

  set(mkloc(s->pos, stream), plus(pos, one));
  return ch;
}

static val string_in_get_prop(val stream, val ind)
{
  if (ind == name_k) {
    struct string_in *s = coerce(struct string_in *, stream->co.handle);
    struct strm_ops *ops = coerce(struct strm_ops *, stream->co.ops);
    val name = static_str(ops->name);
    return format(nil, lit("~a ~s ~p"), name, s->string, stream, nao);
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
                              cobj_hash_op),
                wli("string-input-stream"),
                0, 0, 0,
                string_in_get_line,
                string_in_get_char,
                0,
                string_in_unget_char,
                0, 0, 0,
                0, /* TODO: seek */
                string_in_get_prop,
                0,
                string_in_get_error,
                string_in_get_error_str,
                0);

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
                              cobj_hash_op),
                wli("byte-input-stream"),
                0, 0, 0, 0, 0,
                byte_in_get_byte,
                0,
                byte_in_unget_byte,
                0, 0, 0, 0, 0,
                byte_in_get_error,
                byte_in_get_error_str,
                0);

val make_string_byte_input_stream(val string)
{
  type_assert (stringp(string), (lit("~a is not a string"), string, nao));

  {
    struct byte_input *bi = coerce(struct byte_input *, chk_malloc(sizeof *bi));
    unsigned char *utf8 = utf8_dup_to_uc(c_str(string));
    strm_base_init(&bi->a);
    bi->buf = utf8;
    bi->size = strlen(coerce(char *, utf8));
    bi->index = 0;
    return cobj(coerce(mem_t *, bi), stream_s, &byte_in_ops.cobj_ops);
  }
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
  struct string_out *so = coerce(struct string_out *, stream->co.handle);

  if (so == 0)
    return nil;

  while (so->head != so->tail)
    string_out_byte_flush(so, stream);

  {
    const wchar_t *s = c_str(str);
    size_t len = c_num(length_str(str));
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
    uw_throw(error_s, lit("string output stream overflow"));
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

  if (so == 0)
    return nil;

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
                              cobj_hash_op),
                wli("string-output-stream"),
                string_out_put_string,
                string_out_put_char,
                string_out_put_byte,
                0, 0, 0, 0, 0, 0, 0,
                0, /* TODO: seek; fill-with-spaces semantics if past end. */
                0, 0, 0, 0, 0);

val make_string_output_stream(void)
{
  struct string_out *so = coerce(struct string_out *, chk_malloc(sizeof *so));
  strm_base_init(&so->a);
  so->size = 128;
  so->buf = coerce(wchar_t *, chk_malloc(so->size * sizeof so->buf));
  so->fill = 0;
  so->buf[0] = 0;
  utf8_decoder_init(&so->ud);
  so->head = so->tail = 0;
  return cobj(coerce(mem_t *, so), stream_s, &string_out_ops.cobj_ops);
}

val get_string_from_stream(val stream)
{
  struct string_out *so = coerce(struct string_out *,
                                    cobj_handle(stream, stream_s));

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
                              cobj_hash_op),
                wli("strlist-output-stream"),
                strlist_out_put_string,
                strlist_out_put_char,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);

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
  struct strlist_out *s = coerce(struct strlist_out *,
                                 cobj_handle(stream, stream_s));

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

static void cat_stream_print(val stream, val out, val pretty)
{
  struct cat_strm *s = coerce(struct cat_strm *, stream->co.handle);
  struct strm_ops *ops = coerce(struct strm_ops *, stream->co.ops);
  val name = static_str(ops->name);

  (void) pretty;

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
    close_stream(fs, t);
    if ((streams = rest(streams)) != nil)
      set(mkloc(s->streams, stream), streams);
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
    close_stream(fs, t);
    if ((streams = rest(streams)) != nil)
      set(mkloc(s->streams, stream), streams);
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
    close_stream(fs, t);
    if ((streams = rest(streams)) != nil)
      set(mkloc(s->streams, stream), streams);
  }

  return nil;
}

static val cat_unget_byte(val stream, int byte)
{
  struct cat_strm *s = coerce(struct cat_strm *, stream->co.handle);

  if (!s->streams) {
    uw_throwf(file_error_s,
              lit("unget-byte on catenated stream ~a: stream list empty"),
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
              lit("unget-char on catenated stream ~a: stream list empty"),
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
                              cobj_hash_op),
                wli("catenated-stream"),
                0, 0, 0,
                cat_get_line,
                cat_get_char,
                cat_get_byte,
                cat_unget_char,
                cat_unget_byte,
                0, 0, 0,
                cat_get_prop,
                0,
                cat_get_error,
                cat_get_error_str,
                cat_clear_error);

val make_catenated_stream(val stream_list)
{
  struct cat_strm *s = coerce(struct cat_strm *, chk_malloc(sizeof *s));
  strm_base_init(&s->a);
  s->streams = stream_list;
  return cobj(coerce(mem_t *, s), stream_s, &cat_stream_ops.cobj_ops);
}

val catenated_stream_p(val obj)
{
  return if2(streamp(obj), c_true(obj->co.ops == &cat_stream_ops.cobj_ops));
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

val streamp(val obj)
{
  return typeof(obj) == stream_s ? t : nil;
}

val stream_set_prop(val stream, val ind, val prop)
{
  struct strm_ops *ops = coerce(struct strm_ops *, cobj_ops(stream, stream_s));
  return ops->set_prop(stream, ind, prop);
}

val stream_get_prop(val stream, val ind)
{
  struct strm_ops *ops = coerce(struct strm_ops *, cobj_ops(stream, stream_s));
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
  struct strm_ops *ops = coerce(struct strm_ops *, cobj_ops(stream, stream_s));
  return ops->close(stream, throw_on_error);
}

val get_error(val stream)
{
  struct strm_ops *ops = coerce(struct strm_ops *, cobj_ops(stream, stream_s));
  return ops->get_error(stream);
}

val get_error_str(val stream)
{
  struct strm_ops *ops = coerce(struct strm_ops *, cobj_ops(stream, stream_s));
  return ops->get_error_str(stream);
}

val clear_error(val stream)
{
  struct strm_ops *ops = coerce(struct strm_ops *, cobj_ops(stream, stream_s));
  return ops->clear_error(stream);
}

val get_line(val stream_in)
{
  val stream = default_arg(stream_in, std_input);
  struct strm_ops *ops = coerce(struct strm_ops *, cobj_ops(stream, stream_s));
  return ops->get_line(stream);
}

val get_char(val stream_in)
{
  val stream = default_arg(stream_in, std_input);
  struct strm_ops *ops = coerce(struct strm_ops *, cobj_ops(stream, stream_s));
  return ops->get_char(stream);
}

val get_byte(val stream_in)
{
  val stream = default_arg(stream_in, std_input);
  struct strm_ops *ops = coerce(struct strm_ops *, cobj_ops(stream, stream_s));
  return ops->get_byte(stream);
}

val unget_char(val ch, val stream_in)
{
  val stream = default_arg(stream_in, std_input);
  struct strm_ops *ops = coerce(struct strm_ops *, cobj_ops(stream, stream_s));
  return ops->unget_char(stream, ch);
}

val unget_byte(val byte, val stream_in)
{
  cnum b = c_num(byte);
  val stream = default_arg(stream_in, std_input);
  struct strm_ops *ops = coerce(struct strm_ops *, cobj_ops(stream, stream_s));

  if (b < 0 || b > 255)
    uw_throwf(file_error_s, lit("unget-byte on ~a: byte value ~a out of range"),
              stream, byte, nao);

  return ops->unget_byte(stream, b);
}

struct fmt {
  size_t minsize;
  const char *dec;
  const char *oct;
  const char *hex;
  const char *HEX;
};

static struct fmt fmt_tab[] = {
  { sizeof(short),"%hd",   "%ho",   "%hx",   "%hX"   },
  { sizeof(int),  "%d",    "%o",    "%x",    "%X"    },
  { sizeof(long), "%ld",   "%lo",   "%lx",   "%llX"  },
  { sizeof(cnum), "%lld",  "%llo",  "%llx",  "%llX"  },
  { sizeof(cnum), "%Ld",   "%Lo",   "%Lx",   "%llX"  },
  { sizeof(cnum), "%qd",   "%qo",   "%qx",   "%qX",  },
  { sizeof(cnum), "%I64d", "%I64o", "%I64x", "%I64X" },
  { 0,            0,       0,       0,       0       }
};

static struct fmt *num_fmt;

static void detect_format_string(void)
{
  struct fmt *f;
  char buf[64];
  cnum num = 1234;

  for (f = fmt_tab; f->minsize != 0; f++) {
    memset(buf, 0, sizeof buf);
    if (f->minsize != sizeof num)
      continue;
    if (sprintf(buf, f->dec, num) == 4 && strcmp(buf, "1234") == 0) {
      num_fmt = f;
      break;
    }
  }
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

static void vformat_str(val stream, val str, int width, enum align align,
                        int precision)
{
  const wchar_t *cstr = c_str(str);
  int len = c_num(length_str(str));
  int truelen = (precision && precision < len) ? precision : len;
  int slack = (truelen < width) ? width - truelen : 0;
  int i;

  prot1(&str);

  vformat_align_pre(stream, align, slack);

  if (!al_left)
    for (i = 0; i < slack; i++)
      put_char(chr(' '), stream);

  for (i = 0; i < truelen; i++)
    put_char(chr(cstr[i]), stream);

  vformat_align_post(stream, align, slack);

  rel1(&str);
}

val vformat(val stream, val fmtstr, va_list vl)
{
  val save_indent = get_indent(stream);
  val save_mode = nil;

  uw_simple_catch_begin;

  prot1(&fmtstr);

  {
    const wchar_t *fmt = c_str(fmtstr);
    enum {
      vf_init, vf_width, vf_digits, vf_star, vf_precision, vf_spec
    } state = vf_init, saved_state = vf_init;
    int width = 0, precision = 0, precision_p = 0, digits = 0, lt = 0, neg = 0;
    enum align align = al_right;
    int sign = 0, zeropad = 0;
    cnum value;

    for (;;) {
      val obj;
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
          zeropad = 1;
          continue;
        case '1': case '2': case '3': case '4': case '5':
        case '6': case '7': case '8': case '9':
          saved_state = state;
          state = vf_digits;
          digits = ch - '0';
          continue;
        case '+': case ' ':
          sign = ch;
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
            goto toobig;
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
        obj = va_arg(vl, val);
        digits = c_num(obj);
        goto do_digits;
        break;
      case vf_spec:
        state = vf_init;
        switch (ch) {
        case 'x': case 'X':
          obj = va_arg(vl, val);
          if (bignump(obj)) {
            int nchars = mp_radix_size(mp(obj), 16);
            if (nchars >= convert(int, sizeof (num_buf)))
              pnum = coerce(char *, chk_malloc(nchars + 1));
            mp_toradix_case(mp(obj), coerce(unsigned char *, pnum), 16, ch == 'x');
          } else {
            const char *fmt = ch == 'x' ? num_fmt->hex : num_fmt->HEX;
            value = c_num(obj);
            if (value < 0) {
              num_buf[0] = '-';
              sprintf(num_buf + 1, fmt, -value);
            } else {
              sprintf(num_buf, fmt, value);
            }
          }
          goto output_num;
        case 'o':
          obj = va_arg(vl, val);
          if (bignump(obj)) {
            int nchars = mp_radix_size(mp(obj), 8);
            if (nchars >= convert(int, sizeof (num_buf)))
              pnum = coerce(char *, chk_malloc(nchars + 1));
            mp_toradix(mp(obj), coerce(unsigned char *, pnum), 8);
          } else {
            value = c_num(obj);
            sprintf(num_buf, num_fmt->oct, value);
          }
          goto output_num;
        case 'f': case 'e':
          obj = va_arg(vl, val);

          if (obj == nao)
            goto premature;

          {
            double n;

            switch (type(obj)) {
            case BGNUM:
              obj = flo_int(obj);
              /* fallthrough */
            case FLNUM:
              n = c_flo(obj);
              break;
            case NUM:
              n = convert(double, c_num(obj));
              break;
            default:
              uw_throwf(error_s, lit("format: ~~~a conversion requires "
                                     "numeric arg: ~s given\n"),
                        chr(ch), obj, nao);
            }

            if (!precision_p)
              precision = 3;

            /* guard against num_buf overflow */
            if (precision > 128)
              uw_throwf(error_s, lit("excessive precision in format: ~s\n"),
                        num(precision), nao);

            if (ch == 'e') {
              sprintf(num_buf, "%.*e", precision, n);
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
            precision = 0;
            goto output_num;
          }
        case 'a': case 's':
          obj = va_arg(vl, val);
          if (obj == nao)
            goto premature;
          switch (type(obj)) {
          case NUM:
            value = c_num(obj);
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
            if (!precision_p)
              precision = DBL_DIG;

            if (precision > 500)
              uw_throwf(error_s, lit("excessive precision in format: ~s\n"),
                        num(precision), nao);

            sprintf(num_buf, "%.*g", precision, obj->fl.n);

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

            precision = 0;
            goto output_num;
          default:
            if (width != 0 || precision_p) {
              val str = format(nil, ch == 'a' ? lit("~a") : lit("~s"),
                               obj, nao);
              vformat_str(stream, str, width, align, precision);
              continue;
            }
          }
          if (ch == 'a')
            obj_pprint(obj, stream);
          else
            obj_print(obj, stream);
          continue;
        case 'p':
          {
            val ptr = va_arg(vl, val);
            value = coerce(cnum, ptr);
            sprintf(num_buf, num_fmt->hex, value);
          }
          goto output_num;
        case '!':
          save_mode = test_set_indent_mode(stream, num_fast(indent_off),
                                           num_fast(indent_data));
          if (lt)
            set_indent(stream, plus(save_indent, num(width)));
          else
            inc_indent(stream, num(width));
          continue;
        default:
          uw_throwf(error_s, lit("unknown format directive character ~s\n"),
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
  }

  if (0) {
premature:
    internal_error("insufficient arguments for format");
toobig:
    internal_error("ridiculous precision or field width in format");
  }

  if (va_arg(vl, val) != nao)
    internal_error("unterminated format argument list");

  uw_unwind {
    set_indent(stream, save_indent);
    if (save_mode)
      set_indent_mode(stream, save_mode);
  }

  rel1(&fmtstr);

  uw_catch_end;

  return t;
}

val vformat_to_string(val fmtstr, va_list vl)
{
  val stream = make_string_output_stream();
  (void) vformat(stream, fmtstr, vl);
  return get_string_from_stream(stream);
}

val format(val stream, val str, ...)
{
  uses_or2;
  val st = if3(stream == t,
               std_output,
               or2(stream, make_string_output_stream()));
  class_check(st, stream_s);

  {
    va_list vl;
    val ret;
    va_start (vl, str);
    ret = vformat(st, str, vl);
    va_end (vl);
    return (stream) ? ret : get_string_from_stream(st);
  }
}

val formatv(val stream, val string, val args)
{
  val arg[32], *p = arg;

  for (; args && p - arg < 32; args = cdr(args), p++)
    *p = car(args);

  switch (p - arg) {
  case  0: return format(stream, string,  nao);
  case  1: return format(stream, string, arg[0], nao);
  case  2: return format(stream, string, arg[0], arg[1], nao);
  case  3: return format(stream, string, arg[0], arg[1], arg[2], nao);
  case  4: return format(stream, string, arg[0], arg[1], arg[2], arg[3], nao);
  case  5: return format(stream, string, arg[0], arg[1], arg[2], arg[3], arg[4], nao);
  case  6: return format(stream, string, arg[0], arg[1], arg[2], arg[3], arg[4], arg[5], nao);
  case  7: return format(stream, string, arg[0], arg[1], arg[2], arg[3], arg[4], arg[5], arg[6], nao);
  case  8: return format(stream, string, arg[0], arg[1], arg[2], arg[3], arg[4], arg[5], arg[6], arg[7], nao);
  case  9: return format(stream, string, arg[0], arg[1], arg[2], arg[3], arg[4], arg[5], arg[6], arg[7], arg[8], nao);
  case 10: return format(stream, string, arg[0], arg[1], arg[2], arg[3], arg[4], arg[5], arg[6], arg[7], arg[8], arg[9], nao);
  case 11: return format(stream, string, arg[0], arg[1], arg[2], arg[3], arg[4], arg[5], arg[6], arg[7], arg[8], arg[9], arg[10], nao);
  case 12: return format(stream, string, arg[0], arg[1], arg[2], arg[3], arg[4], arg[5], arg[6], arg[7], arg[8], arg[9], arg[10], arg[11], nao);
  case 13: return format(stream, string, arg[0], arg[1], arg[2], arg[3], arg[4], arg[5], arg[6], arg[7], arg[8], arg[9], arg[10], arg[11], arg[12], nao);
  case 14: return format(stream, string, arg[0], arg[1], arg[2], arg[3], arg[4], arg[5], arg[6], arg[7], arg[8], arg[9], arg[10], arg[11], arg[12], arg[13], nao);
  case 15: return format(stream, string, arg[0], arg[1], arg[2], arg[3], arg[4], arg[5], arg[6], arg[7], arg[8], arg[9], arg[10], arg[11], arg[12], arg[13], arg[14], nao);
  case 16: return format(stream, string, arg[0], arg[1], arg[2], arg[3], arg[4], arg[5], arg[6], arg[7], arg[8], arg[9], arg[10], arg[11], arg[12], arg[13], arg[14], arg[15], nao);
  case 17: return format(stream, string, arg[0], arg[1], arg[2], arg[3], arg[4], arg[5], arg[6], arg[7], arg[8], arg[9], arg[10], arg[11], arg[12], arg[13], arg[14], arg[15], arg[16], nao);
  case 18: return format(stream, string, arg[0], arg[1], arg[2], arg[3], arg[4], arg[5], arg[6], arg[7], arg[8], arg[9], arg[10], arg[11], arg[12], arg[13], arg[14], arg[15], arg[16], arg[17], nao);
  case 19: return format(stream, string, arg[0], arg[1], arg[2], arg[3], arg[4], arg[5], arg[6], arg[7], arg[8], arg[9], arg[10], arg[11], arg[12], arg[13], arg[14], arg[15], arg[16], arg[17], arg[18], nao);
  case 20: return format(stream, string, arg[0], arg[1], arg[2], arg[3], arg[4], arg[5], arg[6], arg[7], arg[8], arg[9], arg[10], arg[11], arg[12], arg[13], arg[14], arg[15], arg[16], arg[17], arg[18], arg[19], nao);
  case 21: return format(stream, string, arg[0], arg[1], arg[2], arg[3], arg[4], arg[5], arg[6], arg[7], arg[8], arg[9], arg[10], arg[11], arg[12], arg[13], arg[14], arg[15], arg[16], arg[17], arg[18], arg[19], arg[20], nao);
  case 22: return format(stream, string, arg[0], arg[1], arg[2], arg[3], arg[4], arg[5], arg[6], arg[7], arg[8], arg[9], arg[10], arg[11], arg[12], arg[13], arg[14], arg[15], arg[16], arg[17], arg[18], arg[19], arg[20], arg[21], nao);
  case 23: return format(stream, string, arg[0], arg[1], arg[2], arg[3], arg[4], arg[5], arg[6], arg[7], arg[8], arg[9], arg[10], arg[11], arg[12], arg[13], arg[14], arg[15], arg[16], arg[17], arg[18], arg[19], arg[20], arg[21], arg[22], nao);
  case 24: return format(stream, string, arg[0], arg[1], arg[2], arg[3], arg[4], arg[5], arg[6], arg[7], arg[8], arg[9], arg[10], arg[11], arg[12], arg[13], arg[14], arg[15], arg[16], arg[17], arg[18], arg[19], arg[20], arg[21], arg[22], arg[23], nao);
  case 25: return format(stream, string, arg[0], arg[1], arg[2], arg[3], arg[4], arg[5], arg[6], arg[7], arg[8], arg[9], arg[10], arg[11], arg[12], arg[13], arg[14], arg[15], arg[16], arg[17], arg[18], arg[19], arg[20], arg[21], arg[22], arg[23], arg[24], nao);
  case 26: return format(stream, string, arg[0], arg[1], arg[2], arg[3], arg[4], arg[5], arg[6], arg[7], arg[8], arg[9], arg[10], arg[11], arg[12], arg[13], arg[14], arg[15], arg[16], arg[17], arg[18], arg[19], arg[20], arg[21], arg[22], arg[23], arg[24], arg[25], nao);
  case 27: return format(stream, string, arg[0], arg[1], arg[2], arg[3], arg[4], arg[5], arg[6], arg[7], arg[8], arg[9], arg[10], arg[11], arg[12], arg[13], arg[14], arg[15], arg[16], arg[17], arg[18], arg[19], arg[20], arg[21], arg[22], arg[23], arg[24], arg[25], arg[26], nao);
  case 28: return format(stream, string, arg[0], arg[1], arg[2], arg[3], arg[4], arg[5], arg[6], arg[7], arg[8], arg[9], arg[10], arg[11], arg[12], arg[13], arg[14], arg[15], arg[16], arg[17], arg[18], arg[19], arg[20], arg[21], arg[22], arg[23], arg[24], arg[25], arg[26], arg[27], nao);
  case 29: return format(stream, string, arg[0], arg[1], arg[2], arg[3], arg[4], arg[5], arg[6], arg[7], arg[8], arg[9], arg[10], arg[11], arg[12], arg[13], arg[14], arg[15], arg[16], arg[17], arg[18], arg[19], arg[20], arg[21], arg[22], arg[23], arg[24], arg[25], arg[26], arg[27], arg[28], nao);
  case 30: return format(stream, string, arg[0], arg[1], arg[2], arg[3], arg[4], arg[5], arg[6], arg[7], arg[8], arg[9], arg[10], arg[11], arg[12], arg[13], arg[14], arg[15], arg[16], arg[17], arg[18], arg[19], arg[20], arg[21], arg[22], arg[23], arg[24], arg[25], arg[26], arg[27], arg[28], arg[29], nao);
  case 31: return format(stream, string, arg[0], arg[1], arg[2], arg[3], arg[4], arg[5], arg[6], arg[7], arg[8], arg[9], arg[10], arg[11], arg[12], arg[13], arg[14], arg[15], arg[16], arg[17], arg[18], arg[19], arg[20], arg[21], arg[22], arg[23], arg[24], arg[25], arg[26], arg[27], arg[28], arg[29], arg[30], nao);
  case 32: return format(stream, string, arg[0], arg[1], arg[2], arg[3], arg[4], arg[5], arg[6], arg[7], arg[8], arg[9], arg[10], arg[11], arg[12], arg[13], arg[14], arg[15], arg[16], arg[17], arg[18], arg[19], arg[20], arg[21], arg[22], arg[23], arg[24], arg[25], arg[26], arg[27], arg[28], arg[29], arg[30], arg[31], nao);
  }

  uw_throwf(file_error_s, lit("too many arguments to format"), nao);
  abort();
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
  val stream = default_arg(stream_in, std_output);
  struct strm_ops *ops = coerce(struct strm_ops *, cobj_ops(stream, stream_s));
  struct strm_base *s = coerce(struct strm_base *, stream->co.handle);
  cnum col = s->column;
  const wchar_t *str = c_str(string), *p = str;

  if (s->indent_mode != indent_off) {
    while (*str)
      put_char(chr(*str++), stream);
    return t;
  }

  for (; *p; p++) {
    switch (*p) {
    case '\n':
      col = 0;
      break;
    case '\t':
      col = (col + 1) | 7;
      break;
    default:
      if (iswprint(*p))
        col++;
      break;
    }
  }

  ops->put_string(stream, string);
  s->column = col;
  return t;
}

val put_char(val ch, val stream_in)
{
  val stream = default_arg(stream_in, std_output);
  struct strm_ops *ops = coerce(struct strm_ops *, cobj_ops(stream, stream_s));
  struct strm_base *s = coerce(struct strm_base *, stream->co.handle);
  wint_t cch = c_chr(ch);

  switch (cch) {
  case L'\n':
    ops->put_char(stream, ch);
    s->column = 0;
    break;
  case L'\t':
    if (s->column == 0 && s->indent_mode != indent_off) {
      put_indent(stream, ops, s->indent_chars);
      s->column = s->indent_chars;
    }
    ops->put_char(stream, ch);
    s->column = (s->column + 1) | 7;
    break;
  default:
    if (s->column == 0 && s->indent_mode != indent_off) {
      put_indent(stream, ops, s->indent_chars);
      s->column = s->indent_chars;
    }
    ops->put_char(stream, ch);
    if (iswprint(cch))
      s->column++;
    break;
  }

  return t;
}

val put_byte(val byte, val stream_in)
{
  val stream = default_arg(stream_in, std_output);
  struct strm_ops *ops = coerce(struct strm_ops *, cobj_ops(stream, stream_s));
  cnum b = c_num(byte);

  if (b < 0 || b > 255)
    uw_throwf(file_error_s, lit("put-byte on ~a: byte value ~a out of range"),
              stream, byte, nao);

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

val flush_stream(val stream)
{
  struct strm_ops *ops = coerce(struct strm_ops *, cobj_ops(stream, stream_s));
  return ops->flush(stream);
}

val seek_stream(val stream, val offset, val whence)
{
  struct strm_ops *ops = coerce(struct strm_ops *, cobj_ops(stream, stream_s));
  enum strm_whence w;
  cnum off = c_num(offset);

  if (whence == from_start_k)
    w = strm_start;
  else if (whence == from_current_k)
    w = strm_cur;
  else if (whence == from_end_k)
    w = strm_end;
  else
    uw_throwf(file_error_s, lit("seek: ~a is not a valid whence argument"),
              whence, nao);

  return ops->seek(stream, off, w);
}

val get_indent_mode(val stream)
{
  struct strm_base *s = coerce(struct strm_base *,
                               cobj_handle(stream, stream_s));
  return num_fast(s->indent_mode);
}

val test_set_indent_mode(val stream, val compare, val mode)
{
  struct strm_base *s = coerce(struct strm_base *,
                               cobj_handle(stream, stream_s));
  val oldval = num_fast(s->indent_mode);
  if (oldval == compare)
    s->indent_mode = (enum indent_mode) c_num(mode);
  return oldval;
}

val set_indent_mode(val stream, val mode)
{
  struct strm_base *s = coerce(struct strm_base *,
                               cobj_handle(stream, stream_s));
  val oldval = num_fast(s->indent_mode);
  s->indent_mode = (enum indent_mode) c_num(mode);
  return oldval;
}

val get_indent(val stream)
{
  struct strm_base *s = coerce(struct strm_base *,
                               cobj_handle(stream, stream_s));
  return num(s->indent_chars);
}

val set_indent(val stream, val indent)
{
  struct strm_base *s = coerce(struct strm_base *,
                               cobj_handle(stream, stream_s));
  val oldval = num(s->indent_chars);
  s->indent_chars = c_num(indent);
  if (s->indent_chars < 0)
    s->indent_chars = 0;
  return oldval;
}

val inc_indent(val stream, val delta)
{
  struct strm_base *s = coerce(struct strm_base *,
                               cobj_handle(stream, stream_s));
  val oldval = num(s->indent_chars);
  val col = num(s->column);
  s->indent_chars = c_num(plus(delta, col));
  if (s->indent_chars < 0)
    s->indent_chars = 0;
  return oldval;
}

val width_check(val stream, val alt)
{
  struct strm_ops *ops = coerce(struct strm_ops *, cobj_ops(stream, stream_s));
  struct strm_base *s = coerce(struct strm_base *, stream->co.handle);

  if ((s->indent_mode == indent_code &&
       s->column >= s->indent_chars + s->code_width) ||
      (s->indent_mode == indent_data &&
       s->column >= s->indent_chars + s->data_width))
  {
    if (ops->put_char(stream, chr('\n')) &&
        put_indent(stream, ops, s->indent_chars)) {
      s->column = s->indent_chars;
      return t;
    }
    return nil;
  } else if (alt) {
    ops->put_char(stream, alt);
    s->column++;
    return t;
  }

  return t;
}

val get_string(val stream, val nchars, val close_after_p)
{
  val strstream = make_string_output_stream();
  nchars = default_bool_arg(nchars);
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
      default_arg(close_after_p, t))
    close_stream(stream, t);

  return get_string_from_stream(strstream);
}

static DIR *w_opendir(const wchar_t *wname)
{
  char *name = utf8_dup_to(wname);
  DIR *d = opendir(name);
  free(name);
  return d;
}


val open_directory(val path)
{
  DIR *d = w_opendir(c_str(path));

  if (!d)
    uw_throwf(file_error_s, lit("error opening directory ~a: ~a/~s"),
              path, num(errno), string_utf8(strerror(errno)), nao);

  return make_dir_stream(d);
}

val open_file(val path, val mode_str)
{
  struct stdio_mode m;
  FILE *f = w_fopen(c_str(path), c_str(normalize_mode(&m, mode_str)));

  if (!f)
    uw_throwf(file_error_s, lit("error opening ~a: ~a/~s"),
              path, num(errno), string_utf8(strerror(errno)), nao);

  return set_mode_props(m, make_stdio_stream(f, path));
}

val open_fileno(val fd, val mode_str)
{
  struct stdio_mode m;
  FILE *f = w_fdopen(c_num(fd), c_str(normalize_mode(&m, mode_str)));

  if (!f)
    uw_throwf(file_error_s, lit("error opening descriptor ~a: ~a/~s"),
              fd, num(errno), string_utf8(strerror(errno)), nao);

  return set_mode_props(m, make_stdio_stream(f, format(nil,
                                                       lit("fd ~a"),
                                                       fd, nao)));
}

val open_tail(val path, val mode_str, val seek_end_p)
{
  struct stdio_mode m;
  val mode = normalize_mode(&m, mode_str);
  FILE *f = w_fopen(c_str(path), c_str(mode));
  struct stdio_handle *h;
  val stream;
  unsigned long state = 0;

  if (f && default_bool_arg(seek_end_p))
    if (fseek(f, 0, SEEK_END) < 0)
      uw_throwf(file_error_s, lit("error seeking to end of ~a: ~a/~s"),
                path, num(errno), string_utf8(strerror(errno)), nao);

  stream = make_tail_stream(f, path);
  h = coerce(struct stdio_handle *, stream->co.handle);
  h->mode = mode;
  if (!f)
    tail_strategy(stream, &state);
  return set_mode_props(m, stream);
}

val open_command(val path, val mode_str)
{
  struct stdio_mode m;
  FILE *f = w_popen(c_str(path), c_str(normalize_mode(&m, mode_str)));

  if (!f)
    uw_throwf(file_error_s, lit("error opening pipe ~a: ~a/~s"),
              path, num(errno), string_utf8(strerror(errno)), nao);

  return set_mode_props(m, make_pipe_stream(f, path));
}

#if HAVE_FORK_STUFF
val open_process(val name, val mode_str, val args)
{
  struct stdio_mode m;
  val mode = normalize_mode(&m, mode_str);
  int input = m.read != 0;
  int fd[2];
  pid_t pid;
  char **argv = 0;
  val iter;
  int i, nargs;

  args = default_bool_arg(args);
  nargs = c_num(length(args)) + 1;

  if (pipe(fd) == -1) {
    uw_throwf(file_error_s, lit("opening pipe ~a, pipe syscall failed: ~a/~s"),
              name, num(errno), string_utf8(strerror(errno)), nao);
  }

  argv = coerce(char **, chk_malloc((nargs + 1) * sizeof *argv));

  for (i = 0, iter = cons(name, args); iter; i++, iter = cdr(iter)) {
    val arg = car(iter);
    argv[i] = utf8_dup_to(c_str(arg));
  }
  argv[i] = 0;

  pid = fork();

  if (pid == -1) {
    for (i = 0; i < nargs; i++)
      free(argv[i]);
    free(argv);
    uw_throwf(file_error_s, lit("opening pipe ~a, fork syscall failed: ~a/~s"),
              name, num(errno), string_utf8(strerror(errno)), nao);
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

    for (i = 0; i < nargs; i++)
      free(argv[i]);
    free(argv);

#if HAVE_FCNTL_H
    fcntl(whichfd, F_SETFD, FD_CLOEXEC);
#endif

    if ((f = fdopen(whichfd, utf8mode)) == 0) {
      int status;
      kill(pid, SIGINT);
      kill(pid, SIGTERM);
      while (waitpid(pid, &status, 0) == -1 && errno == EINTR)
        ;
      free(utf8mode);
      uw_throwf(file_error_s, lit("opening pipe ~a, fdopen failed: ~a/~s"),
                name, num(errno), string_utf8(strerror(errno)), nao);
    }

    free(utf8mode);
    /* TODO: catch potential OOM exception here and kill process. */
    return set_mode_props(m, make_pipevp_stream(f, name, pid));
  }
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
  val win_cmdline = win_make_cmdline(cons(name, default_bool_arg(args)));
  return open_command(win_cmdline, mode_str);
}
#endif

static val sh(val command)
{
  char *cmd = utf8_dup_to(c_str(command));
  int status = system(cmd);
  if (status < 0)
    return nil;
#if HAVE_SYS_WAIT
  if (WIFEXITED(status)) {
    int exitstatus = WEXITSTATUS(status);
    return num(exitstatus);
  }
#endif
  return status == 0 ? zero : nil;
}

#if HAVE_FORK_STUFF
static val run(val name, val args)
{
  pid_t pid;
  char **argv = 0;
  val iter;
  int i, nargs;

  args = default_bool_arg(args);
  nargs = c_num(length(args)) + 1;

  argv = coerce(char **, chk_malloc((nargs + 1) * sizeof *argv));

  for (i = 0, iter = cons(name, args); iter; i++, iter = cdr(iter)) {
    val arg = car(iter);
    argv[i] = utf8_dup_to(c_str(arg));
  }
  argv[i] = 0;

  pid = fork();

  if (pid == -1) {
    for (i = 0; i < nargs; i++)
      free(argv[i]);
    free(argv);
    uw_throwf(file_error_s, lit("opening process ~a, fork syscall failed: ~a/~s"),
              name, num(errno), string_utf8(strerror(errno)), nao);
  }

  if (pid == 0) {
    execvp(argv[0], argv);
    _exit(errno);
  } else {
    int status;
    for (i = 0; i < nargs; i++)
      free(argv[i]);
    free(argv);
    while (waitpid(pid, &status, 0) == -1 && errno == EINTR)
      ;
    if (status < 0)
      return nil;
#if HAVE_SYS_WAIT
    if (WIFEXITED(status)) {
      int exitstatus = WEXITSTATUS(status);
      return num(exitstatus);
    }
#endif
    return status == 0 ? zero : nil;
  }
}
#elif HAVE_WSPAWN
static val run(val command, val args)
{
  const wchar_t **wargv = 0;
  val iter;
  int i, nargs, status;

  args = default_bool_arg(args);
  nargs = c_num(length(args)) + 1;

  prot1(&args);

  wargv = coerce(const wchar_t **, chk_malloc((nargs + 2) * sizeof *wargv));

  for (i = 0, iter = cons(command, args); iter; i++, iter = cdr(iter))
    wargv[i] = c_str(car(iter));
  wargv[i] = 0;

  status = _wspawnvp(_P_WAIT, c_str(command), wargv);

  free(strip_qual(wchar_t **, wargv));

  rel1(&args);

  return (status < 0) ? nil : num(status);
}
#else
static val run(val command, val args)
{
  val win_cmdline = win_make_cmdline(cons(command, default_bool_arg(args)));
  return sh(win_cmdline);
}
#endif

val remove_path(val path)
{
  if (w_remove(c_str(path)) < 0)
    uw_throwf(file_error_s, lit("trying to remove ~a: ~a/~s"),
                path, num(errno), string_utf8(strerror(errno)), nao);
  return t;
}

val rename_path(val from, val to)
{
  if (w_rename(c_str(from), c_str(to)) < 0)
    uw_throwf(file_error_s, lit("trying to rename ~a to ~a: ~a/~s"),
                from, to, num(errno), string_utf8(strerror(errno)), nao);
  return t;
}

static val open_files(val file_list, val substitute_stream)
{
  substitute_stream = default_bool_arg(substitute_stream);

  if (nilp(file_list) && substitute_stream) {
    return substitute_stream;
  } else {
    return apply_intrinsic(func_n0v(make_catenated_stream),
                           cons(mapcar(func_n2o(open_file, 1), file_list), nil));

  }
}

static val open_files_star(val file_list, val substitute_stream)
{
  substitute_stream = default_bool_arg(substitute_stream);

  if (nilp(file_list) && substitute_stream) {
    return substitute_stream;
  } else {
    return apply_intrinsic(func_n0v(make_catenated_stream),
                           cons(lazy_mapcar(func_n2o(open_file, 1), file_list), nil));
  }
}

static val ap_regex;

val abs_path_p(val path)
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

void stream_init(void)
{
  prot1(&ap_regex);

  detect_format_string();

  dev_k = intern(lit("dev"), keyword_package);
  ino_k = intern(lit("ino"), keyword_package);
  mode_k = intern(lit("mode"), keyword_package);
  nlink_k = intern(lit("nlink"), keyword_package);
  uid_k = intern(lit("uid"), keyword_package);
  gid_k = intern(lit("gid"), keyword_package);
  rdev_k = intern(lit("rdev"), keyword_package);
  size_k = intern(lit("size"), keyword_package);
  blksize_k = intern(lit("blksize"), keyword_package);
  blocks_k = intern(lit("blocks"), keyword_package);
  atime_k = intern(lit("atime"), keyword_package);
  mtime_k = intern(lit("mtime"), keyword_package);
  ctime_k = intern(lit("ctime"), keyword_package);
  from_start_k = intern(lit("from-start"), keyword_package);
  from_current_k = intern(lit("from-current"), keyword_package);
  from_end_k = intern(lit("from-end"), keyword_package);
  real_time_k = intern(lit("real-time"), keyword_package);
  name_k = intern(lit("name"), keyword_package);
  fd_k = intern(lit("fd"), keyword_package);
  format_s = intern(lit("format"), user_package);

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

#if HAVE_ISATTY
  if (isatty(fileno(stdin)) == 1)
    stream_set_prop(std_input, real_time_k, t);
#endif

  reg_fun(format_s, func_n2v(formatv));
  reg_fun(intern(lit("make-string-input-stream"), user_package), func_n1(make_string_input_stream));
  reg_fun(intern(lit("make-string-byte-input-stream"), user_package), func_n1(make_string_byte_input_stream));
  reg_fun(intern(lit("make-string-output-stream"), user_package), func_n0(make_string_output_stream));
  reg_fun(intern(lit("get-string-from-stream"), user_package), func_n1(get_string_from_stream));
  reg_fun(intern(lit("make-strlist-output-stream"), user_package), func_n0(make_strlist_output_stream));
  reg_fun(intern(lit("get-list-from-stream"), user_package), func_n1(get_list_from_stream));
  reg_fun(intern(lit("close-stream"), user_package), func_n2o(close_stream, 1));
  reg_fun(intern(lit("get-error"), user_package), func_n1(get_error));
  reg_fun(intern(lit("get-error-str"), user_package), func_n1(get_error_str));
  reg_fun(intern(lit("clear-error"), user_package), func_n1(clear_error));
  reg_fun(intern(lit("get-line"), user_package), func_n1o(get_line, 0));
  reg_fun(intern(lit("get-char"), user_package), func_n1o(get_char, 0));
  reg_fun(intern(lit("get-byte"), user_package), func_n1o(get_byte, 0));
  reg_fun(intern(lit("get-string"), user_package), func_n3o(get_string, 0));
  reg_fun(intern(lit("put-string"), user_package), func_n2o(put_string, 1));
  reg_fun(intern(lit("put-line"), user_package), func_n2o(put_line, 0));
  reg_fun(intern(lit("put-char"), user_package), func_n2o(put_char, 1));
  reg_fun(intern(lit("put-byte"), user_package), func_n2o(put_byte, 1));
  reg_fun(intern(lit("put-lines"), user_package), func_n2o(put_lines, 1));
  reg_fun(intern(lit("put-strings"), user_package), func_n2o(put_strings, 1));
  reg_fun(intern(lit("unget-char"), user_package), func_n2o(unget_char, 1));
  reg_fun(intern(lit("unget-byte"), user_package), func_n2o(unget_byte, 1));
  reg_fun(intern(lit("flush-stream"), user_package), func_n1(flush_stream));
  reg_fun(intern(lit("seek-stream"), user_package), func_n3(seek_stream));
  reg_fun(intern(lit("streamp"), user_package), func_n1(streamp));
  reg_fun(intern(lit("real-time-stream-p"), user_package), func_n1(real_time_stream_p));
  reg_fun(intern(lit("stream-set-prop"), user_package), func_n3(stream_set_prop));
  reg_fun(intern(lit("stream-get-prop"), user_package), func_n2(stream_get_prop));
  reg_fun(intern(lit("fileno"), user_package), curry_12_1(func_n2(stream_get_prop), fd_k));
  reg_fun(intern(lit("make-catenated-stream"), user_package), func_n0v(make_catenated_stream));
  reg_fun(intern(lit("cat-streams"), user_package), func_n1(make_catenated_stream));
  reg_fun(intern(lit("catenated-stream-p"), user_package), func_n1(catenated_stream_p));
  reg_fun(intern(lit("catenated-stream-push"), user_package), func_n2(catenated_stream_push));
  reg_fun(intern(lit("open-directory"), user_package), func_n1(open_directory));
  reg_fun(intern(lit("open-file"), user_package), func_n2o(open_file, 1));
  reg_fun(intern(lit("open-fileno"), user_package), func_n2o(open_fileno, 1));
  reg_fun(intern(lit("open-tail"), user_package), func_n3o(open_tail, 1));
  reg_fun(intern(lit("open-command"), user_package), func_n2o(open_command, 1));
  reg_fun(intern(lit("open-pipe"), user_package), func_n2(open_command));
  reg_fun(intern(lit("open-process"), user_package), func_n3o(open_process, 2));
  reg_fun(intern(lit("sh"), user_package), func_n1(sh));
  reg_fun(intern(lit("run"), user_package), func_n2o(run, 1));
  reg_fun(intern(lit("remove-path"), user_package), func_n1(remove_path));
  reg_fun(intern(lit("rename-path"), user_package), func_n2(rename_path));
  reg_fun(intern(lit("open-files"), user_package), func_n2o(open_files, 1));
  reg_fun(intern(lit("open-files*"), user_package), func_n2o(open_files_star, 1));
  reg_fun(intern(lit("abs-path-p"), user_package), func_n1(abs_path_p));

  fill_stream_ops(&null_ops);
  fill_stream_ops(&stdio_ops);
  fill_stream_ops(&tail_ops);
  fill_stream_ops(&pipe_ops);
  fill_stream_ops(&string_in_ops);
  fill_stream_ops(&byte_in_ops);
  fill_stream_ops(&string_out_ops);
  fill_stream_ops(&strlist_out_ops);
  fill_stream_ops(&dir_ops);
  fill_stream_ops(&cat_stream_ops);
}
