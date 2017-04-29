/* Copyright 2009-2017
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

size_t utf8_from_buf(wchar_t *, const unsigned char *, size_t nbytes);
size_t utf8_from(wchar_t *, const char *);
size_t utf8_to_buf(unsigned char *dst, const wchar_t *wsrc, int null_term);
size_t utf8_to(char *, const wchar_t *);
wchar_t *utf8_dup_from(const char *);
wchar_t *utf8_dup_from_buf(const char *str, size_t size);
char *utf8_dup_to(const wchar_t *);
unsigned char *utf8_dup_to_buf(const wchar_t *, size_t *pnbytes,
                               int null_term);

enum utf8_state { utf8_init, utf8_more1, utf8_more2, utf8_more3 };

#define UTF8_ADMIT_NUL 1

typedef struct utf8_decoder {
  enum utf8_state state;
  int flags;
  wchar_t wch, wch_min;
  int head, tail, back;
  int buf[8];
} utf8_decoder_t;

int utf8_encode(wchar_t, int (*put)(int ch, mem_t *ctx), mem_t *ctx);
void utf8_decoder_init(utf8_decoder_t *);
wint_t utf8_decode(utf8_decoder_t *,int (*get)(mem_t *ctx), mem_t *ctx);

FILE *w_fopen(const wchar_t *, const wchar_t *);
FILE *w_popen(const wchar_t *, const wchar_t *);
FILE *w_freopen(const wchar_t *, const wchar_t *, FILE *);
FILE *w_fdopen(int, const wchar_t *);
int w_remove(const wchar_t *);
int w_rename(const wchar_t *, const wchar_t *);
