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
#include "lib.h"

size_t utf8_from(wchar_t *wdst, const unsigned char *src)
{
  size_t nchar = 1;
  enum { init, more1, more2, more3 } state;
  const char *backtrack = 0;
  int ch;
  wchar_t wch = 0;

  for (state = init; (ch = *src); src++) {
    switch (state) {
    case init:
      if (ch < 0x80) {
        if (wdst)
          *wdst++ = ch;
        nchar++;
      } else if (ch >= 0xc2 && ch <= 0xe0) {
        state = more1;
        wch = (ch & 0x1f);
      } else if (ch >= 0xe0 && ch <= 0xef) {
        state = more2;
        wch = (ch & 0xf);
      } else if (ch >= 0xf0 && ch < 0xf5) {
        state = more3;
        wch = (ch & 0x7);
      } else {
        if (wdst)
          *wdst++ = 0xdc00 | ch;
        nchar++;
      }
      backtrack = src;
      break;
    case more1:
    case more2:
    case more3:
      if (ch >= 0x80 && ch < 0xc0) {
        wch <<= 6;
        wch |= (ch & 0x3f);
        if (wdst)
          *wdst++ = wch;
        nchar++;
        state--;
      } else {
        src = backtrack;
        if (wdst)
          *wdst++ = 0xdc00 | *src;
        nchar++;
        state = init;
      }
      break;
    }
  }

  if (state != init) {
    if (wdst)
      *wdst++ = 0xdc00 | *backtrack;
    nchar++;
  }

  if (wdst)
    *wdst++ = 0;
  return nchar;
}

size_t utf8_to(unsigned char *dst, const wchar_t *wsrc)
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

wchar_t *utf8_dup_from(const unsigned char *str)
{
  size_t nchar = utf8_from(0, str);
  wchar_t *wstr = chk_malloc(sizeof *wstr * nchar);
  utf8_from(wstr, str);
  return wstr;
}

unsigned char *utf8_dup_to(const wchar_t *wstr)
{
  size_t nbyte = utf8_to(0, wstr);
  unsigned char *str = chk_malloc(nbyte);
  utf8_to(str, wstr);
  return str;
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
