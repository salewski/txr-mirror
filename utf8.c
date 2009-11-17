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

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <wchar.h>
#include "lib.h"
#include "utf8.h"

size_t utf8_from_uc(wchar_t *wdst, const unsigned char *src)
{
  size_t nchar = 1;
  enum utf8_state state = utf8_init;
  const unsigned char *backtrack = 0;
  wchar_t wch = 0;

  for (;;) {
    int ch = *src++;

    if (ch == 0) {
      if (state == utf8_init)
        break;
      src = backtrack;
      if (wdst)
        *wdst++ = 0xdc00 | *src;
      nchar++;
      state = utf8_init;
      continue;
    }

    switch (state) {
    case utf8_init:
      if (ch < 0x80) {
        if (wdst)
          *wdst++ = ch;
        nchar++;
      } else if (ch >= 0xc2 && ch <= 0xe0) {
        state = utf8_more1;
        wch = (ch & 0x1f);
      } else if (ch >= 0xe0 && ch <= 0xef) {
        state = utf8_more2;
        wch = (ch & 0xf);
      } else if (ch >= 0xf0 && ch < 0xf5) {
        state = utf8_more3;
        wch = (ch & 0x7);
      } else {
        if (wdst)
          *wdst++ = 0xdc00 | ch;
        nchar++;
      }
      backtrack = src;
      break;
    case utf8_more1:
    case utf8_more2:
    case utf8_more3:
      if (ch >= 0x80 && ch < 0xc0) {
        wch <<= 6;
        wch |= (ch & 0x3f);
        if (--state == utf8_init) {
          if (wdst)
            *wdst++ = wch;
          nchar++;
        }
      } else {
        src = backtrack;
        if (wdst)
          *wdst++ = 0xdc00 | *src;
        nchar++;
        state = utf8_init;
      }
      break;
    }
  }

  if (wdst)
    *wdst++ = 0;
  return nchar;
}

size_t utf8_from(wchar_t *wdst, const char *src)
{
   return utf8_from_uc(wdst, (const unsigned char *) src);
}

size_t utf8_to_uc(unsigned char *dst, const wchar_t *wsrc)
{
  size_t nbyte = 1;
  wchar_t wch;

  while ((wch = *wsrc++)) {
    if (wch < 0x80) {
      nbyte += 1;
      if (dst)
        *dst++ = wch;
    } else if (wch < 0x800) {
      nbyte += 2;
      if (dst) {
        *dst++ = 0xC0 | (wch >> 6);
        *dst++ = 0x80 | (wch & 0x3F);
      }
    } else if (wch < 0x10000) {
      nbyte += 3;
      if (dst) {
        *dst++ = 0xE0 | (wch >> 12);
        *dst++ = 0x80 | ((wch >> 6) & 0x3F);
        *dst++ = 0x80 | (wch & 0x3F);
      }
    } else if (wch < 0x110000) {
      nbyte += 4;
      if (dst) {
        *dst++ = 0xF0 | (wch >> 18);
        *dst++ = 0x80 | ((wch >> 12) & 0x3F);
        *dst++ = 0x80 | ((wch >> 6) & 0x3F);
        *dst++ = 0x80 | (wch & 0x3F);
      }
    }
  }

  if (dst)
    *dst++ = 0;
  return nbyte;
}

size_t utf8_to(char *dst, const wchar_t *wsrc)
{
  return utf8_to_uc((unsigned char *) dst, wsrc);
}

wchar_t *utf8_dup_from_uc(const unsigned char *str)
{
  size_t nchar = utf8_from_uc(0, str);
  wchar_t *wstr = (wchar_t *) chk_malloc(nchar * sizeof *wstr);
  utf8_from_uc(wstr, str);
  return wstr;
}

wchar_t *utf8_dup_from(const char *str)
{
  size_t nchar = utf8_from(0, str);
  wchar_t *wstr = (wchar_t *) chk_malloc(nchar * sizeof *wstr);
  utf8_from(wstr, str);
  return wstr;
}

unsigned char *utf8_dup_to_uc(const wchar_t *wstr)
{
  size_t nbyte = utf8_to_uc(0, wstr);
  unsigned char *str = (unsigned char *) chk_malloc(nbyte);
  utf8_to_uc(str, wstr);
  return str;
}

char *utf8_dup_to(const wchar_t *wstr)
{
  size_t nbyte = utf8_to(0, wstr);
  char *str = (char *) chk_malloc(nbyte);
  utf8_to(str, wstr);
  return str;
}

int utf8_encode(wchar_t wch, int (*put)(int ch, void *ctx), void *ctx)
{
  if (wch < 0x80) {
    return put(wch, ctx);
  } else if (wch < 0x800) {
    return put(0xC0 | (wch >> 6), ctx) &&
           put(0x80 | (wch & 0x3F), ctx);
  } else if (wch < 0x10000) {
    return put(0xE0 | (wch >> 12), ctx) &&
           put(0x80 | ((wch >> 6) & 0x3F), ctx) &&
           put(0x80 | (wch & 0x3F), ctx);
  } else if (wch < 0x110000) {
    return put(0xF0 | (wch >> 18), ctx) &&
           put(0x80 | ((wch >> 12) & 0x3F), ctx) &&
           put(0x80 | ((wch >> 6) & 0x3F), ctx) &&
           put(0x80 | (wch & 0x3F), ctx);
  }

  return 0;
}

void utf8_decoder_init(utf8_decoder_t *ud)
{
  ud->state = utf8_init;
  ud->wch = 0;
  ud->head = ud->tail = ud->back = 0;
}

wint_t utf8_decode(utf8_decoder_t *ud, int (*get)(void *ctx), void *ctx)
{
  for (;;) {
    int ch;

    if (ud->tail != ud->head) {
      ch = ud->buf[ud->tail];
      ud->tail = (ud->tail + 1) % 8;
    } else {
      ch = get(ctx);
      ud->buf[ud->head] = ch;
      ud->head = ud->tail = (ud->head + 1) % 8;
    }

    if (ch == EOF) {
      if (ud->state == utf8_init) {
        return WEOF;
      } else {
        wchar_t wch = 0xdc00 | ud->buf[ud->back];
        ud->tail = ud->back = (ud->back + 1) % 8;
        ud->state = utf8_init;
        return wch;
      }
    }

    switch (ud->state) {
    case utf8_init:
      if (ch < 0x80) {
        ud->back = ud->tail;
        return ch;
      } else if (ch >= 0xc2 && ch <= 0xe0) {
        ud->state = utf8_more1;
        ud->wch = (ch & 0x1f);
      } else if (ch >= 0xe0 && ch <= 0xef) {
        ud->state = utf8_more2;
        ud->wch = (ch & 0xf);
      } else if (ch >= 0xf0 && ch < 0xf5) {
        ud->state = utf8_more3;
        ud->wch = (ch & 0x7);
      } else {
        ud->back = ud->tail;
        return 0xdc00 | ch;
      }
      break;
    case utf8_more1:
    case utf8_more2:
    case utf8_more3:
      if (ch >= 0x80 && ch < 0xc0) {
        ud->wch <<= 6;
        ud->wch |= (ch & 0x3f);
        if (--ud->state == utf8_init) {
          ud->back = ud->tail;
          return ud->wch;
        }
      } else {
        wchar_t wch = 0xdc00 | ud->buf[ud->back];
        ud->tail = ud->back = (ud->back + 1) % 8;
        ud->state = utf8_init;
        return wch;
      }
      break;
    }
  }
}

FILE *w_fopen(const wchar_t *wname, const wchar_t *wmode)
{
  char *name = (char *) utf8_dup_to(wname);
  char *mode = (char *) utf8_dup_to(wmode);
  FILE *f = fopen(name, mode);
  free(name);
  free(mode);
  return f;
}

FILE *w_popen(const wchar_t *wcmd, const wchar_t *wmode)
{
  char *cmd = (char *) utf8_dup_to(wcmd);
  char *mode = (char *) utf8_dup_to(wmode);
  FILE *f = popen(cmd, mode);
  free(cmd);
  free(mode);
  return f;
}
