/* Copyright 2011
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
#include <wchar.h>
#include <unistd.h>
#include "config.h"
#if HAVE_SYS_WAIT
#include <sys/wait.h>
#endif
#include "lib.h"
#include "gc.h"
#include "unwind.h"
#include "stream.h"
#include "utf8.h"

val std_input, std_output, std_error;

struct strm_ops {
  struct cobj_ops cobj_ops;
  val (*put_string)(val, val);
  val (*put_char)(val, val);
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
  struct utf8_decoder ud;
};

static void stdio_stream_print(val stream, val out)
{
  struct stdio_handle *h = (struct stdio_handle *) stream->co.handle;
  format(out, lit("#<~s ~s>"), stream->co.cls, h->descr, nao);
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
  return h->f != 0 && utf8_encode(c_chr(ch), stdio_put_char_callback, (mem_t *) h->f)
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
  stdio_get_line,
  stdio_get_char,
  stdio_get_byte,
  stdio_close,
  stdio_flush
};

static val pipe_close(val stream, val throw_on_error)
{
  struct stdio_handle *h = (struct stdio_handle *) stream->co.handle;

  if (h->f != 0) {
    int status = pclose(h->f);
    h->f = 0;

    if (status != 0 && throw_on_error) {
      if (status < 0) {
        uw_throwf(process_error_s,
                  lit("unable to obtain status of command ~a: ~a/~s"),
                  stream, num(errno), string_utf8(strerror(errno)), nao);
#ifdef HAVE_SYS_WAIT
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
    *cdr_l(pair) = nlpos ? plus(nlpos, one) : length_str(string);
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
    *cdr_l(pair) = plus(pos, one);
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
  string_in_get_line,
  string_in_get_char,
  0,
  0
};

struct byte_input {
  unsigned char *buf;
  size_t size;
  size_t index;
};

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
    cobj_destroy_stub_op,
    cobj_mark_op,
    cobj_hash_op },
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

static val string_out_put_string(val stream, val str)
{
  struct string_output *so = (struct string_output *) stream->co.handle;

  if (so == 0) {
    return nil;
  } else {
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

static struct strm_ops string_out_ops = {
  { cobj_equal_op,
    cobj_print_op,
    string_out_stream_destroy,
    cobj_mark_op,
    cobj_hash_op },
  string_out_put_string,
  string_out_put_char,
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

    put_string(strstream, sub_str(str, nil, span_to_newline));

    if (equal(span_to_newline, length))
      break;

    str = sub_str(str, plus(span_to_newline, num(1)), nil);
    push(get_string_from_stream(strstream), &lines);
    strstream = make_string_output_stream();
  }

  *car_l(cell) = lines;
  *cdr_l(cell) = strstream;

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
    put_char(strstream, ch);
  }

  *car_l(cell) = lines;
  *cdr_l(cell) = strstream;

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
  return stream;
}

val make_pipe_stream(FILE *f, val descr, val input, val output)
{
  struct stdio_handle *h = (struct stdio_handle *) chk_malloc(sizeof *h);
  val stream = cobj((mem_t *) h, stream_s, &pipe_ops.cobj_ops);
  h->f = f;
  h->descr = descr;
  utf8_decoder_init(&h->ud);
  return stream;
}

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

    stream->co.handle = 0;

    if (!so)
      return out;

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
      if (!put_char(stream, chr(' ')))
        return nil;

  if (!zeropad)
    for (i = 0; i < padlen; i++)
      if (!put_char(stream, chr(' ')))
        return nil;

  if (sign_char) {
    put_char(stream, chr(sign_char));
    str++;
  } else if (sign) {
    put_char(stream, chr(sign));
  }

  if (zeropad)
    for (i = 0; i < padlen; i++)
      if (!put_char(stream, chr('0')))
        return nil;

  while (*str)
    if (!put_char(stream, chr(*str++)))
      return nil;

  if (left)
    for (i = 0; i < slack; i++)
      if (!put_char(stream, chr(' ')))
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
      if (!put_char(stream, chr(' ')))
        return nil;

  for (i = 0; i < truelen; i++)
    if (!put_char(stream, chr(cstr[i])))
      return nil;

  if (left)
    for (i = 0; i < slack; i++)
      if (!put_char(stream, chr(' ')))
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
    int width = 0, precision = 0, digits = 0;
    int left = 0, sign = 0, zeropad = 0;
    cnum value;
    void *ptr;

    for (;;) {
      val obj;
      wchar_t ch = *fmt++;
      char num_buf[64], *pnum = num_buf;

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
          digits = 0;
          continue;
        default:
          put_char(stream, chr(ch));
          continue;
        }
        break;
      case vf_width:
        switch (ch) {
        case '~':
          put_char(stream, chr('~'));
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
        case 'a': case 's':
          obj = va_arg(vl, val);
          if (obj == nao)
            goto premature;
          if (fixnump(obj)) {
            value = c_num(obj);
            sprintf(num_buf, num_fmt->dec, value);
            goto output_num;
          } else if (bignump(obj)) {
            int nchars = mp_radix_size(mp(obj), 10); 
            if (nchars >= (int) sizeof (num_buf))
              pnum = (char *) chk_malloc(nchars + 1);
            mp_toradix(mp(obj), (unsigned char *) pnum, 10);
            goto output_num;
          } else if (width != 0) {
            val str = format(nil, ch == 'a' ? lit("~a") : lit("~s"), obj, nao);
            if (!vformat_str(stream, str, width, left, precision))
              return nil;
            continue;
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

val put_string(val stream, val string)
{
  type_check (stream, COBJ);
  type_assert (stream->co.cls == stream_s, (lit("~a is not a stream"),
                                            stream, nao));

  {
    struct strm_ops *ops = (struct strm_ops *) stream->co.ops;
    return ops->put_string ? ops->put_string(stream, string) : nil;
  }
}

val put_char(val stream, val ch)
{
  type_check (stream, COBJ);
  type_assert (stream->co.cls == stream_s, (lit("~a is not a stream"),
                                            stream, nao));

  {
    struct strm_ops *ops = (struct strm_ops *) stream->co.ops;
    return ops->put_char ? ops->put_char(stream, ch) : nil;
  }
}

val put_line(val stream, val string)
{
  return (put_string(stream, string), put_char(stream, chr('\n')));
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
  char *name = (char *) utf8_dup_to(wname);
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

void stream_init(void)
{
  protect(&std_input, &std_output, &std_error, (val *) 0);
  std_input = make_stdio_stream(stdin, string(L"stdin"), t, nil);
  std_output = make_stdio_stream(stdout, string(L"stdout"), nil, t);
  std_error = make_stdio_stream(stderr, string(L"stderr"), nil, t);
  detect_format_string();
}
