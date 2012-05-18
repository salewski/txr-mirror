/* Copyright 2012
 * Kaz Kylheku <kaz@kylheku.com>
 * Vancouver, Canada
 * All rights reserved.
 *
 * BSD License:
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 *   1. Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *   2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in
 *      the documentation and/or other materials provided with the
 *      distribution.
 *   3. The name of the author may not be used to endorse or promote
 *      products derived from this software without specific prior
 *      written permission.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#include <stdio.h>
#include <string.h>
#include <dirent.h>
#include <stdarg.h>
#include <stdlib.h>
#include <assert.h>
#include <setjmp.h>
#include <errno.h>
#include <ctype.h>
#include <wchar.h>
#include <unistd.h>
#include <float.h>
#include "config.h"
#if HAVE_SYS_WAIT
#include <sys/wait.h>
#endif
#include "lib.h"
#include "gc.h"
#include "unwind.h"
#include "stream.h"
#include "utf8.h"

val std_input, std_output, std_debug, std_error;
val output_produced;

struct strm_ops {
  struct cobj_ops cobj_ops;
  val (*put_string)(val, val);
  val (*put_char)(val, val);
  val (*put_byte)(val, int);
  val (*get_line)(val);
  val (*get_char)(val);
  val (*get_byte)(val);
  val (*close)(val, val);
  val (*flush)(val);
};

static void common_destroy(val obj)
{
  (void) close_stream(obj, nil);
}

struct stdio_handle {
  FILE *f;
  val descr;
  utf8_decoder_t ud;
  pid_t pid;
};

static void stdio_stream_print(val stream, val out)
{
  struct stdio_handle *h = (struct stdio_handle *) stream->co.handle;
  if (h->pid)
    format(out, lit("#<~s ~s>"), stream->co.cls, h->descr, nao);
  else
    format(out, lit("#<~s ~s ~s>"), stream->co.cls, h->descr, h->pid, nao);
}

static void stdio_stream_destroy(val stream)
{
  struct stdio_handle *h = (struct stdio_handle *) stream->co.handle;
  common_destroy(stream);
  free(h);
}

static void stdio_stream_mark(val stream)
{
  struct stdio_handle *h = (struct stdio_handle *) stream->co.handle;
  gc_mark(h->descr);
}

static val stdio_maybe_read_error(val stream)
{
  struct stdio_handle *h = (struct stdio_handle *) stream->co.handle;
  if (h->f == 0)
    uw_throwf(file_error_s, lit("error reading ~a: file closed"), stream, nao);
  if (ferror(h->f)) {
    clearerr(h->f);
    uw_throwf(file_error_s, lit("error reading ~a: ~a/~s"),
              stream, num(errno), string_utf8(strerror(errno)), nao);
  }
  return nil;
}

static val stdio_maybe_write_error(val stream)
{
  struct stdio_handle *h = (struct stdio_handle *) stream->co.handle;
  if (h->f == 0)
    uw_throwf(file_error_s, lit("error writing ~a: file closed"), stream, nao);
  clearerr(h->f);
  uw_throwf(file_error_s, lit("error writing ~a: ~a/~s"),
            stream, num(errno), string_utf8(strerror(errno)), nao);
}

static int stdio_put_char_callback(int ch, mem_t *f)
{
  return putc(ch, (FILE *) f) != EOF;
}

static int stdio_get_char_callback(mem_t *f)
{
  return getc((FILE *) f);
}

static val stdio_put_string(val stream, val str)
{
  struct stdio_handle *h = (struct stdio_handle *) stream->co.handle;

  if (stream != std_debug && stream != std_error)
    output_produced = t;

  if (h->f != 0) {
    const wchar_t *s = c_str(str);

    while (*s) {
      if (!utf8_encode(*s++, stdio_put_char_callback, (mem_t *) h->f))
        return stdio_maybe_write_error(stream);
    }
    return t;
  }
  return stdio_maybe_write_error(stream);
}

static val stdio_put_char(val stream, val ch)
{
  struct stdio_handle *h = (struct stdio_handle *) stream->co.handle;

  if (stream != std_debug && stream != std_error)
    output_produced = t;

  return h->f != 0 && utf8_encode(c_chr(ch), stdio_put_char_callback, (mem_t *) h->f)
         ? t : stdio_maybe_write_error(stream);
}

static val stdio_put_byte(val stream, int b)
{
  struct stdio_handle *h = (struct stdio_handle *) stream->co.handle;

  if (stream != std_debug && stream != std_error)
    output_produced = t;

  return h->f != 0 && putc(b, (FILE *) h->f) != EOF
         ? t : stdio_maybe_write_error(stream);
}

static val stdio_flush(val stream)
{
  struct stdio_handle *h = (struct stdio_handle *) stream->co.handle;
  if (fflush(h->f))
    stdio_maybe_write_error(stream);
  return t;
}

static wchar_t *snarf_line(struct stdio_handle *h)
{
  const size_t min_size = 512;
  size_t size = 0;
  size_t fill = 0;
  wchar_t *buf = 0;

  for (;;) {
    wint_t ch = utf8_decode(&h->ud, stdio_get_char_callback, (mem_t *) h->f);

    if (ch == WEOF && buf == 0)
      break;

    if (fill >= size) {
      size_t newsize = size ? size * 2 : min_size;
      buf = (wchar_t *) chk_realloc((mem_t *) buf, newsize * sizeof *buf);
      size = newsize;
    }

    if (ch == '\n' || ch == WEOF) {
      buf[fill++] = 0;
      break;
    }
    buf[fill++] = ch;
  }

  if (buf)
    buf = (wchar_t *) chk_realloc((mem_t *) buf, fill * sizeof *buf);

  return buf;
}

static val stdio_get_line(val stream)
{
  if (stream->co.handle == 0) {
    return stdio_maybe_read_error(stream);
  } else {
    struct stdio_handle *h = (struct stdio_handle *) stream->co.handle;
    wchar_t *line = snarf_line(h);
    if (!line)
      return stdio_maybe_read_error(stream);
    return string_own(line);
  }
}

static val stdio_get_char(val stream)
{
  struct stdio_handle *h = (struct stdio_handle *) stream->co.handle;
  if (h->f) {
    wint_t ch = utf8_decode(&h->ud, stdio_get_char_callback, (mem_t *) h->f);
    return (ch != WEOF) ? chr(ch) : stdio_maybe_read_error(stream);
  }
  return stdio_maybe_read_error(stream);
}

static val stdio_get_byte(val stream)
{
  struct stdio_handle *h = (struct stdio_handle *) stream->co.handle;
  if (h->f) {
    int ch = getc(h->f);
    return (ch != EOF) ? num(ch) : stdio_maybe_read_error(stream);
  }
  return stdio_maybe_read_error(stream);
}

static val stdio_close(val stream, val throw_on_error)
{
  struct stdio_handle *h = (struct stdio_handle *) stream->co.handle;

  if (h->f != 0 && h->f != stdin && h->f != stdout) {
    int result = fclose(h->f);
    h->f = 0;
    if (result == EOF && throw_on_error) {
      uw_throwf(file_error_s, lit("error closing ~a: ~a/~s"),
                stream, num(errno), string_utf8(strerror(errno)), nao);
    }
    return result != EOF ? t : nil;
  }
  return nil;
}

static struct strm_ops stdio_ops = {
  { cobj_equal_op,
    stdio_stream_print,
    stdio_stream_destroy,
    stdio_stream_mark,
    cobj_hash_op },
  stdio_put_string,
  stdio_put_char,
  stdio_put_byte,
  stdio_get_line,
  stdio_get_char,
  stdio_get_byte,
  stdio_close,
  stdio_flush
};

#if HAVE_FORK_STUFF
static int pipevp_close(FILE *f, pid_t pid)
{
  int status;
  fclose(f);
  while (waitpid(pid, &status, 0) == -1 && errno == EINTR)
    ;
  return status;
}
#endif

static val pipe_close(val stream, val throw_on_error)
{
  struct stdio_handle *h = (struct stdio_handle *) stream->co.handle;

  if (h->f != 0) {
#if HAVE_FORK_STUFF
    int status = h->pid != 0 ? pipevp_close(h->f, h->pid) : pclose(h->f);
#else
    int status = pclose(h->f);
#endif
    h->f = 0;

    if (status != 0 && throw_on_error) {
      if (status < 0) {
        uw_throwf(process_error_s,
                  lit("unable to obtain status of command ~a: ~a/~s"),
                  stream, num(errno), string_utf8(strerror(errno)), nao);
#ifdef HAVE_SYS_WAIT
#ifndef WIFCONTINUED
#define WIFCONTINUED(X) 0
#endif
      } else if (WIFEXITED(status)) {
        int exitstatus = WEXITSTATUS(status);
        uw_throwf(process_error_s, lit("pipe ~a terminated with status ~a"),
                  stream, num(exitstatus), nao);
      } else if (WIFSIGNALED(status)) {
        int termsig = WTERMSIG(status);
        uw_throwf(process_error_s, lit("pipe ~a terminated by signal ~a"),
                  stream, num(termsig), nao);

      } else if (WIFSTOPPED(status) || WIFCONTINUED(status)) {
        uw_throwf(process_error_s,
                  lit("processes of closed pipe ~a still running"),
                  stream, nao);
      } else {
        uw_throwf(file_error_s, lit("strange status in when closing pipe ~a"),
                  stream, nao);
#endif
      }
    }

    return status == 0 ? t : nil;
  }
  return nil;
}

static struct strm_ops pipe_ops = {
  { cobj_equal_op,
    stdio_stream_print,
    stdio_stream_destroy,
    stdio_stream_mark,
    cobj_hash_op },
  stdio_put_string,
  stdio_put_char,
  stdio_put_byte,
  stdio_get_line,
  stdio_get_char,
  stdio_get_byte,
  pipe_close,
  stdio_flush,
};

static void string_in_stream_mark(val stream)
{
  val stuff = (val) stream->co.handle;
  gc_mark(stuff);
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
  val pair = (val) stream->co.handle;
  val string = car(pair);
  val pos = cdr(pair);

  if (lt(pos, length_str(string))) {
    val nlpos = find_char(string, pos, chr('\n'));
    val result = sub_str(string, pos, nlpos);
    set(*cdr_l(pair), nlpos ? plus(nlpos, one) : length_str(string));
    return result;
  }

  return nil;
}

static val string_in_get_char(val stream)
{
  val pair = (val) stream->co.handle;
  val string = car(pair);
  val pos = cdr(pair);

  if (lt(pos, length_str(string))) {
    set(*cdr_l(pair), plus(pos, one));
    return chr_str(string, pos);
  }

  return nil;
}

static struct strm_ops string_in_ops = {
  { cobj_equal_op,
    cobj_print_op,
    cobj_destroy_stub_op,
    string_in_stream_mark,
    cobj_hash_op },
  0,
  0,
  0,
  string_in_get_line,
  string_in_get_char,
  0,
  0,
  0
};

struct byte_input {
  unsigned char *buf;
  size_t size;
  size_t index;
};

static void byte_in_stream_destroy(val stream)
{
  struct byte_input *bi = (struct byte_input *) stream->co.handle;

  if (bi) {
    free(bi->buf);
    bi->buf = 0;
    free(bi);
    stream->co.handle = 0;
  }
}

static val byte_in_get_byte(val stream)
{
  struct byte_input *bi = (struct byte_input *) stream->co.handle;

  if (bi->index < bi->size)
    return num(bi->buf[bi->index++]);
  return nil;
}

static struct strm_ops byte_in_ops = {
  { cobj_equal_op,
    cobj_print_op,
    byte_in_stream_destroy,
    cobj_mark_op,
    cobj_hash_op },
  0,
  0,
  0,
  0,
  0,
  byte_in_get_byte,
  0
};


struct string_output {
  wchar_t *buf;
  size_t size;
  size_t fill;
  utf8_decoder_t ud; 
  unsigned char byte_buf[4];
  int head, tail;
};

static void string_out_stream_destroy(val stream)
{
  struct string_output *so = (struct string_output *) stream->co.handle;

  if (so) {
    free(so->buf);
    so->buf = 0;
    free(so);
    stream->co.handle = 0;
  }
}

static int string_out_byte_callback(mem_t *ctx)
{
  struct string_output *so = (struct string_output *) ctx;
  if (so->tail >= so->head)
    return EOF;
  return so->byte_buf[so->tail++];
}

static val string_out_put_char(val stream, val ch);

static val string_out_byte_flush(struct string_output *so, val stream)
{
  val result = nil;

  if (so->tail < so->head) {
    wint_t ch = utf8_decode(&so->ud, string_out_byte_callback, (mem_t *) so);
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
  struct string_output *so = (struct string_output *) stream->co.handle;

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
      return nil;

    while (so->size <= required_size) {
      so->size *= 2;
      if (so->size < old_size)
        return nil;
    }

    if (so->size != old_size)
      so->buf = (wchar_t *) chk_realloc((mem_t *) so->buf,
                                        so->size * sizeof *so->buf);
    wmemcpy(so->buf + so->fill, s, len + 1);
    so->fill += len;
    return t;
  }
}

static val string_out_put_char(val stream, val ch)
{
  wchar_t onech[] = wini(" ");
  wref(onech)[0] = c_chr(ch);
  return string_out_put_string(stream, auto_str((const wchli_t *) wref(onech)));
}

static val string_out_put_byte(val stream, int ch)
{
  struct string_output *so = (struct string_output *) stream->co.handle;

  if (so == 0)
    return nil;

  so->byte_buf[so->head++] = ch;

  if (so->head >= (int) sizeof so->byte_buf)
    return string_out_byte_flush(so, stream);

  return t;
}

static struct strm_ops string_out_ops = {
  { cobj_equal_op,
    cobj_print_op,
    string_out_stream_destroy,
    cobj_mark_op,
    cobj_hash_op },
  string_out_put_string,
  string_out_put_char,
  string_out_put_byte,
  0,
  0,
  0,
  0,
};

static void strlist_mark(val stream)
{
  val stuff = (val) stream->co.handle;
  gc_mark(stuff);
}

static val strlist_out_put_string(val stream, val str)
{
  val cell = (val) stream->co.handle;
  cons_bind (lines, strstream, cell);

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

  set(*car_l(cell), lines);
  set(*cdr_l(cell), strstream);

  return t;
}

static val strlist_out_put_char(val stream, val ch)
{
  val cell = (val) stream->co.handle;
  cons_bind (lines, strstream, cell);

  if (ch == chr('\n')) {
    push(get_string_from_stream(strstream), &lines);
    strstream = make_string_output_stream();
  } else {
    put_char(ch, strstream);
  }

  set(*car_l(cell), lines);
  set(*cdr_l(cell), strstream);

  return t;
}

static struct strm_ops strlist_out_ops = {
  { cobj_equal_op,
    cobj_print_op,
    cobj_destroy_stub_op,
    strlist_mark,
    cobj_hash_op },
  strlist_out_put_string,
  strlist_out_put_char,
  0,
  0,
  0,
  0,
  0,
};

val make_strlist_output_stream(void)
{
  return cobj((mem_t *) cons(nil, make_string_output_stream()), 
              stream_s, &strlist_out_ops.cobj_ops);
}

val get_list_from_stream(val stream)
{
  type_check (stream, COBJ);
  type_assert (stream->co.cls == stream_s,
               (lit("~a is not a stream"), stream, nao));

  if (stream->co.ops == &strlist_out_ops.cobj_ops) {
    val cell = (val) stream->co.handle;
    cons_bind (lines, strstream, cell);
    val stray = get_string_from_stream(strstream);
    if (!zerop(length_str(stray)))
      push(stray, &lines);
    return nreverse(lines);
  }

  type_mismatch(lit("~s is not a string list stream"), stream);
}

static val dir_get_line(val stream)
{
  DIR *handle = (DIR *) stream->co.handle;

  if (handle == 0) {
    return nil;
  } else {
    for (;;) {
      struct dirent *e = readdir(handle);
      if (!e)
        return nil;
      if (!strcmp(e->d_name, ".") || !strcmp(e->d_name, ".."))
        continue;
      return string_utf8(e->d_name);
    }
  }
}

static val dir_close(val stream, val throw_on_error)
{
  if (stream->co.handle != 0) {
    closedir((DIR *) stream->co.handle);
    stream->co.handle = 0;
    return t;
  }

  return nil;
}

static struct strm_ops dir_ops = {
  { cobj_equal_op,
    cobj_print_op,
    common_destroy,
    cobj_mark_op,
    cobj_hash_op },
  0,
  0,
  0,
  dir_get_line,
  0,
  0,
  dir_close
};


val make_stdio_stream(FILE *f, val descr, val input, val output)
{
  struct stdio_handle *h = (struct stdio_handle *) chk_malloc(sizeof *h);
  val stream = cobj((mem_t *) h, stream_s, &stdio_ops.cobj_ops);
  h->f = f;
  h->descr = descr;
  utf8_decoder_init(&h->ud);
  h->pid = 0;
  return stream;
}

val make_pipe_stream(FILE *f, val descr, val input, val output)
{
  struct stdio_handle *h = (struct stdio_handle *) chk_malloc(sizeof *h);
  val stream = cobj((mem_t *) h, stream_s, &pipe_ops.cobj_ops);
  h->f = f;
  h->descr = descr;
  utf8_decoder_init(&h->ud);
  h->pid = 0;
  return stream;
}

#if HAVE_FORK_STUFF
static val make_pipevp_stream(FILE *f, val descr, pid_t pid)
{
  struct stdio_handle *h = (struct stdio_handle *) chk_malloc(sizeof *h);
  val stream = cobj((mem_t *) h, stream_s, &pipe_ops.cobj_ops);
  h->f = f;
  h->descr = descr;
  utf8_decoder_init(&h->ud);
  h->pid = pid;
  return stream;
}
#endif

val make_string_input_stream(val string)
{
  return cobj((mem_t *) cons(string, zero), stream_s, &string_in_ops.cobj_ops);
}

val make_string_byte_input_stream(val string)
{
  type_assert (stringp(string), (lit("~a is not a string"), string, nao));

  {
    struct byte_input *bi = (struct byte_input *) chk_malloc(sizeof *bi);
    unsigned char *utf8 = utf8_dup_to_uc(c_str(string));
    bi->buf = utf8;
    bi->size = strlen((char *) utf8);
    bi->index = 0;
    return cobj((mem_t *) bi, stream_s, &byte_in_ops.cobj_ops);
  }
}

val make_string_output_stream(void)
{
  struct string_output *so = (struct string_output *) chk_malloc(sizeof *so);
  so->size = 128;
  so->buf = (wchar_t *) chk_malloc(so->size * sizeof so->buf);
  so->fill = 0;
  so->buf[0] = 0;
  utf8_decoder_init(&so->ud);
  so->head = so->tail = 0;
  return cobj((mem_t *) so, stream_s, &string_out_ops.cobj_ops);
}

val get_string_from_stream(val stream)
{
  type_check (stream, COBJ);
  type_assert (stream->co.cls == stream_s,
               (lit("~a is not a stream"), stream, nao));

  if (stream->co.ops == &string_out_ops.cobj_ops) {
    struct string_output *so = (struct string_output *) stream->co.handle;
    val out = nil;

    if (!so)
      return out;

    while (so->head != so->tail)
      out = string_out_byte_flush(so, stream);

    stream->co.handle = 0;

    so->buf = (wchar_t *) chk_realloc((mem_t *) so->buf,
                                      (so->fill + 1) * sizeof *so->buf);
    out = string_own(so->buf);
    free(so);
    return out;
  } else if (stream->co.ops == &string_in_ops.cobj_ops) {
    val pair = (val) stream->co.handle;
    return pair ? car(pair) : nil;
  } else {
    abort(); /* not a string input or output stream */
  }
}

val make_dir_stream(DIR *dir)
{
  return cobj((mem_t *) dir, stream_s, &dir_ops.cobj_ops);
}

val streamp(val obj)
{
  return typeof(obj) == stream_s ? t : nil;
}

val close_stream(val stream, val throw_on_error)
{
  type_check (stream, COBJ);
  type_assert (stream->co.cls == stream_s, (lit("~a is not a stream"),
                                            stream, nao));

  {
    struct strm_ops *ops = (struct strm_ops *) stream->co.ops;
    return ops->close ? ops->close(stream, throw_on_error) : nil;
  }
}

val get_line(val stream)
{
  if (!stream)
    stream = std_input;

  type_check (stream, COBJ);
  type_assert (stream->co.cls == stream_s, (lit("~a is not a stream"),
                                            stream, nao));

  {
    struct strm_ops *ops = (struct strm_ops *) stream->co.ops;
    return ops->get_line ? ops->get_line(stream) : nil;
  }
}

val get_char(val stream)
{
  if (!stream)
    stream = std_input;

  type_check (stream, COBJ);
  type_assert (stream->co.cls == stream_s, (lit("~a is not a stream"),
                                            stream, nao));

  {
    struct strm_ops *ops = (struct strm_ops *) stream->co.ops;
    return ops->get_char ? ops->get_char(stream) : nil;
  }
}

val get_byte(val stream)
{
  if (!stream)
    stream = std_input;

  type_check (stream, COBJ);
  type_assert (stream->co.cls == stream_s, (lit("~a is not a stream"),
                                            stream, nao));

  {
    struct strm_ops *ops = (struct strm_ops *) stream->co.ops;
    return ops->get_byte ? ops->get_byte(stream) : nil;
  }
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

static val vformat_num(val stream, const char *str,
                       int width, int left, int zeropad, 
                       int precision, int sign)
{
  int sign_char = (str[0] == '-' || str[0] == '+') ? str[0] : 0;
  int digit_len = strlen(str) - (sign_char != 0);
  int padlen = precision > digit_len ? precision - digit_len : 0;
  int total_len = digit_len + padlen + (sign_char || sign); 
  int slack = (total_len < width) ? width - total_len : 0;
  int i;

  if (!left)
    for (i = 0; i < slack; i++)
      if (!put_char(chr(' '), stream))
        return nil;

  if (!zeropad)
    for (i = 0; i < padlen; i++)
      if (!put_char(chr(' '), stream))
        return nil;

  if (sign_char) {
    put_char(chr(sign_char), stream);
    str++;
  } else if (sign) {
    put_char(chr(sign), stream);
  }

  if (zeropad)
    for (i = 0; i < padlen; i++)
      if (!put_char(chr('0'), stream))
        return nil;

  while (*str)
    if (!put_char(chr(*str++), stream))
      return nil;

  if (left)
    for (i = 0; i < slack; i++)
      if (!put_char(chr(' '), stream))
        return nil;

  return t;
}

static val vformat_str(val stream, val str, int width, int left,
                       int precision)
{
  const wchar_t *cstr = c_str(str);
  int len = c_num(length_str(str));
  int truelen = (precision && precision < len) ? precision : len;
  int slack = (truelen < width) ? width - truelen : 0;
  int i;

  if (!left)
    for (i = 0; i < slack; i++)
      if (!put_char(chr(' '), stream))
        return nil;

  for (i = 0; i < truelen; i++)
    if (!put_char(chr(cstr[i]), stream))
      return nil;

  if (left)
    for (i = 0; i < slack; i++)
      if (!put_char(chr(' '), stream))
        return nil;

  return t;
}

val vformat(val stream, val fmtstr, va_list vl)
{
  type_check (stream, COBJ);
  type_assert (stream->co.cls == stream_s, (lit("~a is not a stream"),
                                            stream, nao));

  {
    const wchar_t *fmt = c_str(fmtstr);
    enum {
      vf_init, vf_width, vf_digits, vf_precision, vf_spec
    } state = vf_init, saved_state = vf_init;
    int width = 0, precision = 0, precision_p = 0, digits = 0;
    int left = 0, sign = 0, zeropad = 0;
    cnum value;
    void *ptr;

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
          left = 0;
          zeropad = 0;
          precision = 0;
          precision_p = 0;
          digits = 0;
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
          left = 1;
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
        case '*':
          obj = va_arg(vl, val);
          width = c_num(obj);
          state = vf_precision;
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
        case '*':
          obj = va_arg(vl, val);
          width = c_num(obj);
          precision = vf_precision;
          precision_p = 1;
          continue;
        default:
          state = vf_spec;
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
          switch (saved_state) {
          case vf_width:
            if (width < 0) {
              width = -digits;
              left = 1;
            } else {
              width = digits;
            }
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
      case vf_spec:
        state = vf_init;
        switch (ch) {
        case 'x':
          obj = va_arg(vl, val);
          if (bignump(obj)) {
            int nchars = mp_radix_size(mp(obj), 16); 
            if (nchars >= (int) sizeof (num_buf))
              pnum = (char *) chk_malloc(nchars + 1);
            mp_toradix_case(mp(obj), (unsigned char *) pnum, 16, 1);
          } else {
            value = c_num(obj);
            sprintf(num_buf, num_fmt->hex, value);
          }
          goto output_num;
        case 'X':
          obj = va_arg(vl, val);
          if (bignump(obj)) {
            int nchars = mp_radix_size(mp(obj), 16); 
            if (nchars >= (int) sizeof (num_buf))
              pnum = (char *) chk_malloc(nchars + 1);
            mp_toradix_case(mp(obj), (unsigned char *) pnum, 16, 0);
          } else {
            value = c_num(obj);
            sprintf(num_buf, num_fmt->HEX, value);
          }
          goto output_num;
        case 'o':
          obj = va_arg(vl, val);
          if (bignump(obj)) {
            int nchars = mp_radix_size(mp(obj), 8); 
            if (nchars >= (int) sizeof (num_buf))
              pnum = (char *) chk_malloc(nchars + 1);
            mp_toradix(mp(obj), (unsigned char *) pnum, 8);
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
              n = (double) c_num(obj);
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

                  while (scan[0] == '0' && scan[1] == '0')
                    scan++;

                  while (*scan)
                    *exp++ = *scan++;

                  *exp = 0;
                }
              }
            } else {
              sprintf(num_buf, "%.*f", precision, n);
            }
            if (!isdigit(num_buf[0]) && !isdigit(num_buf[1])) {
              if (!vformat_str(stream, lit("#<bad-float>"),
                               width, left, 0))
                return nil;
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
              if (nchars >= (int) sizeof (num_buf))
                pnum = (char *) chk_malloc(nchars + 1);
              mp_toradix(mp(obj), (unsigned char *) pnum, 10);
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

                while (*scan == 0)
                  scan++;

                while (*scan)
                  *exp++ = *scan++;

                *exp = 0;
              }

              if (ch == 's' && !precision_p && !dec && !exp)
                  strcat(num_buf, ".0");
            }

            if (!isdigit(num_buf[0]) && !isdigit(num_buf[1])) {
              if (!vformat_str(stream, lit("#<bad-float>"),
                               width, left, 0))
                return nil;
              continue;
            }

            precision = 0;
            goto output_num;
          default:
            if (width != 0) {
              val str = format(nil, ch == 'a' ? lit("~a") : lit("~s"),
                               obj, nao);
              if (!vformat_str(stream, str, width, left, precision))
                return nil;
              continue;
            }
          }
          if (ch == 'a')
            obj_pprint(obj, stream);
          else
            obj_print(obj, stream);
          continue;
        case 'p':
          ptr = va_arg(vl, void *);
          value = (cnum) ptr;
          sprintf(num_buf, num_fmt->hex, value);
          goto output_num;
        default:
          uw_throwf(error_s, lit("unknown format directive character ~s\n"), 
                    chr(ch), nao);
        output_num:
          {
            val res = vformat_num(stream, pnum, width, left,
                                  zeropad, precision, sign);
            if (pnum != num_buf)
              free(pnum);
            if (!res)
              return nil;
            continue;
          }
        }
        continue;
      }

      break;
    }
  }

  if (va_arg(vl, val) != nao)
    internal_error("unterminated format argument list");
  return t;
premature:
  internal_error("insufficient arguments for format");
toobig:
  internal_error("ridiculous precision or field width in format");
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
  type_check (st, COBJ);
  type_assert (st->co.cls == stream_s, (lit("~a is not a stream"), st, nao));

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

val put_string(val string, val stream)
{
  if (!stream)
    stream = std_output;

  type_check (stream, COBJ);
  type_assert (stream->co.cls == stream_s, (lit("~a is not a stream"),
                                            stream, nao));

  {
    struct strm_ops *ops = (struct strm_ops *) stream->co.ops;
    return ops->put_string ? ops->put_string(stream, string) : nil;
  }
}

val put_char(val ch, val stream)
{
  if (!stream)
    stream = std_output;

  type_check (stream, COBJ);
  type_assert (stream->co.cls == stream_s, (lit("~a is not a stream"),
                                            stream, nao));

  {
    struct strm_ops *ops = (struct strm_ops *) stream->co.ops;
    return ops->put_char ? ops->put_char(stream, ch) : nil;
  }
}

val put_byte(val byte, val stream)
{
  cnum b = c_num(byte);

  if (!stream)
    stream = std_output;

  type_check (stream, COBJ);
  type_assert (stream->co.cls == stream_s, (lit("~a is not a stream"),
                                            stream, nao));

  if (b < 0 || b > 255)
    uw_throwf(file_error_s, lit("put-byte on ~a: byte value ~a out of range"),
              stream, byte, nao);

  {
    struct strm_ops *ops = (struct strm_ops *) stream->co.ops;
    return ops->put_char ? ops->put_byte(stream, b) : nil;
  }
}


val put_line(val string, val stream)
{
  return (put_string(string, stream), put_char(chr('\n'), stream));
}

val flush_stream(val stream)
{
  type_check (stream, COBJ);
  type_assert (stream->co.cls == stream_s, (lit("~a is not a stream"),
                                            stream, nao));

  {
    struct strm_ops *ops = (struct strm_ops *) stream->co.ops;
    return ops->flush ? ops->flush(stream) : t;
  }
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
  FILE *f = w_fopen(c_str(path), c_str(mode_str));
  val input = nil, output = nil;

  if (!f)
    uw_throwf(file_error_s, lit("error opening ~a: ~a/~s"),
              path, num(errno), string_utf8(strerror(errno)), nao);

  if (break_str(mode_str, lit("w")))
    output = t;
  if (break_str(mode_str, lit("a")))
    output = t;
  if (break_str(mode_str, lit("r")))
    input = t;
  if (break_str(mode_str, lit("+")))
    input = output = t;

  return make_stdio_stream(f, path, input, output);
}

val open_pipe(val path, val mode_str)
{
  FILE *f = w_popen(c_str(path), c_str(mode_str));
  val input = nil, output = nil;

  if (!f)
    uw_throwf(file_error_s, lit("error opening pipe ~a: ~a/~s"),
              path, num(errno), string_utf8(strerror(errno)), nao);

  if (break_str(mode_str, lit("w")))
    output = t;
  if (break_str(mode_str, lit("a")))
    output = t;
  if (break_str(mode_str, lit("r")))
    input = t;
  if (break_str(mode_str, lit("+")))
    input = output = t;

  return make_pipe_stream(f, path, input, output);
}

#if HAVE_FORK_STUFF
val open_pipevp(val name, val mode_str, val args)
{
  int input = equal(mode_str, lit("r")) || equal(mode_str, lit("rb"));
  int fd[2];
  pid_t pid;
  char **argv = 0, *utf8name = 0;
  val iter;
  int i, nargs = c_num(length(args));

  if (pipe(fd) == -1) {
    uw_throwf(file_error_s, lit("opening pipe ~a, pipe syscall failed: ~a/~s"),
              name, num(errno), string_utf8(strerror(errno)), nao);
  }

  argv = (char **) chk_malloc((nargs + 1) * sizeof *argv);

  for (i = 0, iter = args; iter; i++, iter = cdr(iter)) {
    val arg = car(iter);
    argv[i] = utf8_dup_to(c_str(arg));
  }
  argv[i] = 0;

  utf8name = utf8_dup_to(c_str(name));

  pid = fork();

  if (pid == -1) {
    uw_throwf(file_error_s, lit("opening pipe ~a, fork syscall failed: ~a/~s"),
              name, num(errno), string_utf8(strerror(errno)), nao);
  } 
  
  if (pid == 0) {
    if (input) {
      dup2(fd[1], STDOUT_FILENO);
      close(fd[0]);
    } else {
      dup2(fd[0], STDIN_FILENO);
      close(fd[1]);
    }

    execvp(utf8name, argv);
    _exit(1);
  } else {
    int whichfd;
    char *utf8mode = utf8_dup_to(c_str(mode_str));
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

    if ((f = fdopen(whichfd, utf8mode)) == 0) {
      kill(pid, SIGKILL);
      free(utf8mode);
      uw_throwf(file_error_s, lit("opening pipe ~a, fdopen failed: ~a/~s"),
                name, num(errno), string_utf8(strerror(errno)), nao);
    }

    free(utf8mode);
    /* TODO: catch potential OOM exception here and kill process. */
    return make_pipevp_stream(f, name, pid);
  }
}
#else

static val win_escape_arg(val str)
{
  int bscount = 0, i;
  const wchar_t *s;
  val out = string(L"");

  for (s = c_str(str); *s; s++) {
    switch (*s) {
    case '"':
      for (i = 0; i < bscount; i++)
        string_extend(out, lit("\\\\"));
      string_extend(out, lit("\\\""));
      bscount = 0;
      break;
    case '\\':
      bscount++;
      break;
    default:
      for (i = 0; i < bscount; i++)
        string_extend(out, lit("\\"));
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

  for (; args; args = cdr(args)) {
    string_extend(out, lit("\""));
    string_extend(out, win_escape_arg(car(args)));
    if (cdr(args))
      string_extend(out, lit("\" "));
    else
      string_extend(out, lit("\""));
  }

  return out;
}

val open_pipevp(val name, val mode_str, val args)
{
  val win_cmdline = win_make_cmdline(cons(name, args));
  return open_pipe(win_cmdline, mode_str);
}
#endif

void stream_init(void)
{
  protect(&std_input, &std_output, &std_debug, &std_error, (val *) 0);
  std_input = make_stdio_stream(stdin, string(L"stdin"), t, nil);
  std_output = make_stdio_stream(stdout, string(L"stdout"), nil, t);
  std_debug = make_stdio_stream(stdout, string(L"debug"), nil, t);
  std_error = make_stdio_stream(stderr, string(L"stderr"), nil, t);
  detect_format_string();
}
