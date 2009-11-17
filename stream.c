/* Copyright 2009
 * Kaz Kylheku <kkylheku@gmail.com>
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

/*
 * Enable code to work around getwc crash in glibc,
 * which happens on FILE * handles from popen.
 */
#define BROKEN_POPEN_GETWC

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
#include "lib.h"
#include "gc.h"
#include "unwind.h"
#include "stream.h"
#include "utf8.h"

obj_t *std_input, *std_output, *std_error;

struct strm_ops {
  struct cobj_ops cobj_ops;
  obj_t *(*put_string)(obj_t *, obj_t *);
  obj_t *(*put_char)(obj_t *, obj_t *);
  obj_t *(*get_line)(obj_t *);
  obj_t *(*get_char)(obj_t *);
  obj_t *(*get_byte)(obj_t *);
  obj_t *(*close)(obj_t *, obj_t *);
};

static obj_t *common_equal(obj_t *self, obj_t *other)
{
  return self == other ? t : nil;
}

static void common_destroy(obj_t *obj)
{
  (void) close_stream(obj, nil);
}

struct stdio_handle {
  FILE *f;
#ifdef BROKEN_POPEN_GETWC
  FILE *f_orig_pipe;
#endif
  obj_t *descr;
};

void stdio_stream_print(obj_t *stream, obj_t *out)
{
  struct stdio_handle *h = (struct stdio_handle *) stream->co.handle;
  format(out, lit("#<~s ~s>"), stream->co.cls, h->descr, nao);
}

void stdio_stream_destroy(obj_t *stream)
{
  struct stdio_handle *h = (struct stdio_handle *) stream->co.handle;
  common_destroy(stream);
  free(h);
}

void stdio_stream_mark(obj_t *stream)
{
  struct stdio_handle *h = (struct stdio_handle *) stream->co.handle;
  gc_mark(h->descr);
}

static obj_t *stdio_maybe_read_error(obj_t *stream)
{
  struct stdio_handle *h = (struct stdio_handle *) stream->co.handle;
  if (ferror(h->f)) {
    clearerr(h->f);
    uw_throwf(file_error, lit("error reading ~a: ~a/~s"),
              stream, num(errno), string_utf8(strerror(errno)), nao);
  }
  return nil;
}

static obj_t *stdio_maybe_write_error(obj_t *stream)
{
  struct stdio_handle *h = (struct stdio_handle *) stream->co.handle;
  if (ferror(h->f)) {
    clearerr(h->f);
    uw_throwf(file_error, lit("error writing ~a: ~a/~s"),
              stream, num(errno), string_utf8(strerror(errno)), nao);
  }
  return nil;
}

static obj_t *stdio_put_string(obj_t *stream, obj_t *s)
{
  struct stdio_handle *h = (struct stdio_handle *) stream->co.handle;
  return (h->f && fputws(c_str(s), h->f) != -1)
         ? t : stdio_maybe_write_error(stream);
}

static obj_t *stdio_put_char(obj_t *stream, obj_t *ch)
{
  struct stdio_handle *h = (struct stdio_handle *) stream->co.handle;
  return (h->f && putwc(c_chr(ch), h->f) != WEOF)
         ? t : stdio_maybe_write_error(stream);
}

static wchar_t *snarf_line(FILE *in)
{
  const size_t min_size = 512;
  size_t size = 0;
  size_t fill = 0;
  wchar_t *buf = 0;

  for (;;) {
    wint_t ch = getwc(in);

    if (ch == WEOF && buf == 0)
      break;

    if (fill >= size) {
      size_t newsize = size ? size * 2 : min_size;
      buf = chk_realloc(buf, newsize * sizeof *buf);
      size = newsize;
    }

    if (ch == '\n' || ch == WEOF) {
      buf[fill++] = 0;
      break;
    }
    buf[fill++] = ch;
  }

  if (buf)
    buf = chk_realloc(buf, fill * sizeof *buf);

  return buf;
}

static obj_t *stdio_get_line(obj_t *stream)
{
  if (stream->co.handle == 0) {
    return nil;
  } else {
    struct stdio_handle *h = (struct stdio_handle *) stream->co.handle;
    wchar_t *line = snarf_line(h->f);
    if (!line)
      return stdio_maybe_read_error(stream);
    return string_own(line);
  }
}

obj_t *stdio_get_char(obj_t *stream)
{
  struct stdio_handle *h = (struct stdio_handle *) stream->co.handle;
  if (h->f) {
    wint_t ch = getwc(h->f);
    return (ch != WEOF) ? chr(ch) : stdio_maybe_read_error(stream);
  }
  return nil;
}

obj_t *stdio_get_byte(obj_t *stream)
{
  struct stdio_handle *h = (struct stdio_handle *) stream->co.handle;
  if (h->f) {
    int ch = getc(h->f);
    return (ch != EOF) ? num(ch) : stdio_maybe_read_error(stream);
  }
  return nil;
}

static obj_t *stdio_close(obj_t *stream, obj_t *throw_on_error)
{
  struct stdio_handle *h = (struct stdio_handle *) stream->co.handle;

  if (h->f != 0 && h->f != stdin && h->f != stdout) {
    int result = fclose(h->f);
    h->f = 0;
    if (result == EOF && throw_on_error) {
      uw_throwf(file_error, lit("error closing ~a: ~a/~s"),
                stream, num(errno), string_utf8(strerror(errno)), nao);
    }
    return result != EOF ? t : nil;
  }
  return nil;
}

static struct strm_ops stdio_ops = {
  { common_equal,
    stdio_stream_print,
    stdio_stream_destroy,
    stdio_stream_mark,
    0 },
  stdio_put_string,
  stdio_put_char,
  stdio_get_line,
  stdio_get_char,
  stdio_get_byte,
  stdio_close
};

static obj_t *pipe_close(obj_t *stream, obj_t *throw_on_error)
{
  struct stdio_handle *h = (struct stdio_handle *) stream->co.handle;

  if (h->f != 0) {
#ifdef BROKEN_POPEN_GETWC
    int status = (fclose(h->f), pclose(h->f_orig_pipe));
    h->f = h->f_orig_pipe = 0;
#else
    int status = pclose(h->f);
    h->f = 0;
#endif

    if (status != 0 && throw_on_error) {
      if (status < 0) {
        uw_throwf(process_error,
                  lit("unable to obtain status of command ~a: ~a/~s"),
                  stream, num(errno), string_utf8(strerror(errno)), nao);
      } else if (WIFEXITED(status)) {
        int exitstatus = WEXITSTATUS(status);
        uw_throwf(process_error, lit("pipe ~a terminated with status ~a"),
                  stream, num(exitstatus), nao);
      } else if (WIFSIGNALED(status)) {
        int termsig = WTERMSIG(status);
        uw_throwf(process_error, lit("pipe ~a terminated by signal ~a"),
                  stream, num(termsig), nao);

      } else if (WIFSTOPPED(status) || WIFCONTINUED(status)) {
        uw_throwf(process_error,
                  lit("processes of closed pipe ~a still running"),
                  stream, nao);
      } else {
        uw_throwf(file_error, lit("strange status in when closing pipe ~a"),
                  stream, nao);
      }
    }

    return status == 0 ? t : nil;
  }
  return nil;
}

static struct strm_ops pipe_ops = {
  { common_equal,
    stdio_stream_print,
    stdio_stream_destroy,
    stdio_stream_mark,
    0 },
  stdio_put_string,
  stdio_put_char,
  stdio_get_line,
  stdio_get_char,
  stdio_get_byte,
  pipe_close
};

void string_in_stream_mark(obj_t *stream)
{
  obj_t *stuff = (obj_t *) stream->co.handle;
  gc_mark(stuff);
}

static obj_t *string_in_get_line(obj_t *stream)
{
  obj_t *pair = (obj_t *) stream->co.handle;
  obj_t *string = car(pair);
  obj_t *pos = cdr(pair);

  /* TODO: broken, should only scan to newline */
  if (lt(pos, length(string))) {
    obj_t *result = sub_str(string, pos, nil);
    *cdr_l(pair) = length_str(string);
    return result;
  }

  return nil;
}

static obj_t *string_in_get_char(obj_t *stream)
{
  obj_t *pair = (obj_t *) stream->co.handle;
  obj_t *string = car(pair);
  obj_t *pos = cdr(pair);

  if (lt(pos, length_str(string))) {
    *cdr_l(pair) = plus(pos, one);
    return chr_str(string, pos);
  }

  return nil;
}

static struct strm_ops string_in_ops = {
  { common_equal,
    cobj_print_op,
    0,
    string_in_stream_mark,
    0 },
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

static obj_t *byte_in_get_byte(obj_t *stream)
{
  struct byte_input *bi = (struct byte_input *) stream->co.handle;

  if (bi->index < bi->size)
    return num(bi->buf[bi->index++]);
  return nil;
}

static struct strm_ops byte_in_ops = {
  { common_equal,
    cobj_print_op,
    0,
    0,
    0 },
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

static void string_out_stream_destroy(obj_t *stream)
{
  struct string_output *so = (struct string_output *) stream->co.handle;

  if (so) {
    free(so->buf);
    so->buf = 0;
    free(so);
    stream->co.handle = 0;
  }
}

static obj_t *string_out_put_string(obj_t *stream, obj_t *str)
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
      so->buf = chk_realloc(so->buf, so->size * sizeof *so->buf);
    wmemcpy(so->buf + so->fill, s, len + 1);
    so->fill += len;
    return t;
  }
}

static obj_t *string_out_put_char(obj_t *stream, obj_t *ch)
{
  wchar_t mini[2];
  mini[0] = c_chr(ch);
  mini[1] = 0;
  return string_out_put_string(stream, auto_str(mini));
}

static struct strm_ops string_out_ops = {
  { common_equal,
    cobj_print_op,
    string_out_stream_destroy,
    0,
    0 },
  string_out_put_string,
  string_out_put_char,
  0,
  0,
  0,
  0,
};

static obj_t *dir_get_line(obj_t *stream)
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

static obj_t *dir_close(obj_t *stream, obj_t *throw_on_error)
{
  if (stream->co.handle != 0) {
    closedir((DIR *) stream->co.handle);
    stream->co.handle = 0;
    return t;
  }

  return nil;
}

static struct strm_ops dir_ops = {
  { common_equal,
    cobj_print_op,
    common_destroy,
    0,
    0 },
  0,
  0,
  dir_get_line,
  0,
  0,
  dir_close
};


obj_t *make_stdio_stream(FILE *f, obj_t *descr, obj_t *input, obj_t *output)
{
  struct stdio_handle *h = (struct stdio_handle *) chk_malloc(sizeof *h);
  obj_t *stream = cobj((void *) h, stream_t, &stdio_ops.cobj_ops);
  h->f = f;
  h->descr = descr;
  return stream;
}

obj_t *make_pipe_stream(FILE *f, obj_t *descr, obj_t *input, obj_t *output)
{
  struct stdio_handle *h = (struct stdio_handle *) chk_malloc(sizeof *h);
  obj_t *stream = cobj((void *) h, stream_t, &pipe_ops.cobj_ops);
#ifdef BROKEN_POPEN_GETWC
  int dup_fd = dup(fileno(f));
  FILE *dup_f = (dup_fd != -1) ? fdopen(dup_fd, output ? "w" : "r") : 0;

  if (dup_fd == -1 || dup_f == 0) {
    int error = errno;
    if (dup_f != 0)
      fclose(dup_f);
    else if (dup_fd != -1)
      close(dup_fd);
    /* Don't leave h uninitialized; it is gc-reachable through stream cobj. */
    h->f = h->f_orig_pipe = 0;
    h->descr = descr;
    uw_throwf(process_error, lit("unable to create pipe ~a: ~a/~s"), descr,
              num(error), string_utf8(strerror(error)), nao);
  }

  h->f_orig_pipe = f;
  h->f = dup_f;
#else
  h->f = f;
#endif
  h->descr = descr;
  return stream;
}

obj_t *make_string_input_stream(obj_t *string)
{
  return cobj((void *) cons(string, zero), stream_t, &string_in_ops.cobj_ops);
}

obj_t *make_string_byte_input_stream(obj_t *string)
{
  type_assert (stringp(string), (lit("~a is not a string"), string, nao));

  {
    struct byte_input *bi = (struct byte_input *) chk_malloc(sizeof *bi);
    unsigned char *utf8 = utf8_dup_to_uc(c_str(string));
    bi->buf = utf8;
    bi->size = strlen((char *) utf8);
    bi->index = 0;
    return cobj(bi, stream_t, &byte_in_ops.cobj_ops);
  }
}

obj_t *make_string_output_stream(void)
{
  struct string_output *so = (struct string_output *) chk_malloc(sizeof *so);
  so->size = 128;
  so->buf = (wchar_t *) chk_malloc(so->size * sizeof so->buf);
  so->fill = 0;
  so->buf[0] = 0;
  return cobj((void *) so, stream_t, &string_out_ops.cobj_ops);
}

obj_t *get_string_from_stream(obj_t *stream)
{
  type_check (stream, COBJ);
  type_assert (stream->co.cls == stream_t,
               (lit("~a is not a stream"), stream, nao));

  if (stream->co.ops == &string_out_ops.cobj_ops) {
    struct string_output *so = (struct string_output *) stream->co.handle;
    obj_t *out = nil;

    stream->co.handle = 0;

    if (!so)
      return out;

    so->buf = chk_realloc(so->buf, (so->fill + 1) * sizeof *so->buf);
    out = string_own(so->buf);
    free(so);
    return out;
  } else if (stream->co.ops == &string_in_ops.cobj_ops) {
    obj_t *pair = (obj_t *) stream->co.handle;
    return pair ? car(pair) : nil;
  } else {
    abort(); /* not a string input or output stream */
  }
}

obj_t *make_dir_stream(DIR *dir)
{
  return cobj((void *) dir, stream_t, &dir_ops.cobj_ops);
}

obj_t *close_stream(obj_t *stream, obj_t *throw_on_error)
{
  type_check (stream, COBJ);
  type_assert (stream->co.cls == stream_t, (lit("~a is not a stream"),
                                            stream, nao));

  {
    struct strm_ops *ops = (struct strm_ops *) stream->co.ops;
    return ops->close ? ops->close(stream, throw_on_error) : nil;
  }
}

obj_t *get_line(obj_t *stream)
{
  type_check (stream, COBJ);
  type_assert (stream->co.cls == stream_t, (lit("~a is not a stream"),
                                            stream, nao));

  {
    struct strm_ops *ops = (struct strm_ops *) stream->co.ops;
    return ops->get_line ? ops->get_line(stream) : nil;
  }
}

obj_t *get_char(obj_t *stream)
{
  type_check (stream, COBJ);
  type_assert (stream->co.cls == stream_t, (lit("~a is not a stream"),
                                            stream, nao));

  {
    struct strm_ops *ops = (struct strm_ops *) stream->co.ops;
    return ops->get_char ? ops->get_char(stream) : nil;
  }
}

obj_t *get_byte(obj_t *stream)
{
  type_check (stream, COBJ);
  type_assert (stream->co.cls == stream_t, (lit("~a is not a stream"),
                                            stream, nao));

  {
    struct strm_ops *ops = (struct strm_ops *) stream->co.ops;
    return ops->get_byte ? ops->get_byte(stream) : nil;
  }
}

static obj_t *vformat_num(obj_t *stream, const char *str,
                          int width, int left, int pad, int precision)
{
  int len = strlen(str);
  int truewidth = (width < precision) ? width : precision;
  int slack = (len < truewidth) ? truewidth - len : 0;
  int padlen = (len < precision) ? precision - len : 0;
  int i;

  if (!left)
    for (i = 0; i < slack; i++)
      if (!put_char(stream, pad ? chr('0') : chr(' ')))
        return nil;

  for (i = 0; i < padlen; i++)
    if (!put_char(stream, pad ? chr('0') : chr(' ')))
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

obj_t *vformat_str(obj_t *stream, obj_t *str, int width, int left,
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

obj_t *vformat(obj_t *stream, obj_t *fmtstr, va_list vl)
{
  type_check (stream, COBJ);
  type_assert (stream->co.cls == stream_t, (lit("~a is not a stream"),
                                            stream, nao));

  {
    const wchar_t *fmt = c_str(fmtstr);
    enum {
      vf_init, vf_width, vf_digits, vf_precision, vf_spec
    } state = vf_init, saved_state = vf_init;
    int width = 0, precision = 0, digits = 0;
    int left = 0, zeropad = 0;
    long val;
    void *ptr;
    char num_buf[64];

    for (;;) {
      obj_t *obj;
      wchar_t ch = *fmt++;

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
          continue;
        case '-':
          left = 1;
          continue;
        case ',':
          state = vf_precision;
          continue;
        case '0':
          saved_state = state;
          state = vf_digits;
          zeropad = 1;
          continue;
        case '1': case '2': case '3': case '4': case '5':
        case '6': case '7': case '8': case '9':
          saved_state = state;
          state = vf_digits;
          digits = ch - '0';
          continue;
        case '*':
          obj = va_arg(vl, obj_t *);
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
        case '0': case '1': case '2': case '3': case '4': case '5':
        case '6': case '7': case '8': case '9':
          saved_state = state;
          state = vf_digits;
          digits = ch - '0';
          continue;
        case '*':
          obj = va_arg(vl, obj_t *);
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
            state = (ch == ',') ? vf_precision : vf_spec;
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
          obj = va_arg(vl, obj_t *);
          val = c_num(obj);
          sprintf(num_buf, "%lx", val);
          goto output_num;
        case 'X':
          obj = va_arg(vl, obj_t *);
          val = c_num(obj);
          sprintf(num_buf, "%lX", val);
          goto output_num;
        case 'o':
          obj = va_arg(vl, obj_t *);
          val = c_num(obj);
          sprintf(num_buf, "%lo", val);
          goto output_num;
        case 'a':
          obj = va_arg(vl, obj_t *);
          if (obj == nao)
            goto premature;
          if (nump(obj)) {
            val = c_num(obj);
            sprintf(num_buf, "%ld", val);
            goto output_num;
          } else if (stringp(obj)) {
            if (!vformat_str(stream, obj, width, left, precision))
              return nil;
            continue;
          }
          obj_pprint(obj, stream);
          continue;
        case 's':
          obj = va_arg(vl, obj_t *);
          if (obj == nao)
            goto premature;
          if (nump(obj)) {
            val = c_num(obj);
            sprintf(num_buf, "%ld", val);
            if (vformat_num(stream, num_buf, 0, 0, 0, 0))
              return nil;
            continue;
          }
          obj_print(obj, stream);
          continue;
        case 'p':
          ptr = va_arg(vl, void *);
          val = (int) ptr;
          sprintf(num_buf, "0x%lx", val);
          goto output_num;
        default:
          abort();
        output_num:
          if (!vformat_num(stream, num_buf, width, left,
                           precision ? 0 : zeropad,
                           precision ? precision : 1))
            return nil;
          continue;
        }
        continue;
      }

      break;
    }
  }


  if (va_arg(vl, obj_t *) != nao)
    internal_error("unterminated format argument list");
  return t;
premature:
  internal_error("insufficient arguments for format");
toobig:
  internal_error("ridiculous precision or field width in format");
}

obj_t *format(obj_t *stream, obj_t *str, ...)
{
  type_check (stream, COBJ);
  type_assert (stream->co.cls == stream_t, (lit("~a is not a stream"),
                                             stream, nao));

  {
    va_list vl;
    obj_t *ret;
    va_start (vl, str);
    ret = vformat(stream, str, vl);
    va_end (vl);
    return ret;
  }
}

obj_t *put_string(obj_t *stream, obj_t *string)
{
  type_check (stream, COBJ);
  type_assert (stream->co.cls == stream_t, (lit("~a is not a stream"),
                                            stream, nao));

  {
    struct strm_ops *ops = (struct strm_ops *) stream->co.ops;
    return ops->put_string ? ops->put_string(stream, string) : nil;
  }
}

obj_t *put_char(obj_t *stream, obj_t *ch)
{
  type_check (stream, COBJ);
  type_assert (stream->co.cls == stream_t, (lit("~a is not a stream"),
                                            stream, nao));

  {
    struct strm_ops *ops = (struct strm_ops *) stream->co.ops;
    return ops->put_char ? ops->put_char(stream, ch) : nil;
  }
}

obj_t *put_line(obj_t *stream, obj_t *string)
{
  return (put_string(stream, string), put_char(stream, chr('\n')));
}

void stream_init(void)
{
  protect(&std_input, &std_output, &std_error, (obj_t **) 0);
  std_input = make_stdio_stream(stdin, string(L"stdin"), t, nil);
  std_output = make_stdio_stream(stdout, string(L"stdout"), nil, t);
  std_error = make_stdio_stream(stderr, string(L"stderr"), nil, t);
}
