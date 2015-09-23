/* linenoise.h -- VERSION 1.0
 *
 * Guerrilla line editing library against the idea that a line editing lib
 * needs to be 20,000 lines of C code.
 *
 * See linenoise.c for more information.
 *
 * ------------------------------------------------------------------------
 *
 * Copyright (c) 2010-2015, Salvatore Sanfilippo <antirez at gmail dot com>
 * Copyright (c) 2010-2013, Pieter Noordhuis <pcnoordhuis at gmail dot com>
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 *  *  Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *
 *  *  Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

typedef enum lino_error {
    lino_no_error,      /* No error has occurred. */
    lino_error,         /* Unspecified error */
    lino_eof,           /* Line input terminated by Ctrl-D or EOF. */
    lino_ioerr,         /* Line input terminated by I/O error. */
    lino_notty,         /* Input is not a terminal. */
    lino_intr           /* Line innput terminated by Ctrl-C or interrupt */
} lino_error_t;

#define LINO_PAD_CHAR 0xFFFF

typedef struct lino_state lino_t;

#ifndef MEM_T_DEFINED
typedef unsigned char mem_t;
#define MEM_T_DEFINED
#endif

typedef struct lino_os {
    mem_t *(*alloc_fn)(size_t n);
    mem_t *(*realloc_fn)(mem_t *old, size_t size);
    wchar_t *(*wmalloc_fn)(size_t nwchar);
    wchar_t *(*wrealloc_fn)(wchar_t *, size_t nwchar);
    wchar_t *(*wstrdup_fn)(const wchar_t *str);
    void (*free_fn)(void *);
    int (*fileno_fn)(mem_t *stream);
    int (*puts_fn)(mem_t *stream, const wchar_t *str);
    wint_t (*getch_fn)(mem_t *stream);
    wchar_t *(*getl_fn)(mem_t *stream, wchar_t *buf, size_t nchar);
    wchar_t *(*gets_fn)(mem_t *stream, wchar_t *buf, size_t nchar);
    int (*eof_fn)(mem_t *stream);
    mem_t *(*open_fn)(const wchar_t *name, const wchar_t *mode);
    mem_t *(*open8_fn)(const char *name, const wchar_t *mode);
    mem_t *(*fdopen_fn)(int fd, const wchar_t *mode);
    void (*close_fn)(mem_t *stream);
    int (*wide_display_fn)(wchar_t);
} lino_os_t;

#define lino_os_init(alloc, realloc, wmalloc, wrealloc, wstrdup, free,  \
                     fileno, puts, getch, getl, gets, eof,              \
                     open, open8, fdopen, close, wide_disp)             \
{                                                                       \
  alloc, realloc, wmalloc, wrealloc, wstrdup, free,                     \
  fileno, puts, getch, getl, gets, eof, open, open8, fdopen, close,     \
  wide_disp                                                             \
}

typedef struct lino_completions {
    size_t len;
    wchar_t **cvec;
    int substring;
} lino_completions_t;

typedef void lino_compl_cb_t(const wchar_t *, lino_completions_t *, void *ctx);
void lino_set_completion_cb(lino_t *, lino_compl_cb_t *, void *ctx);
void lino_add_completion(lino_completions_t *, const wchar_t *);

void lino_init(lino_os_t *);
lino_t *lino_make(mem_t *istream, mem_t *ostream);
lino_t *lino_copy(lino_t *);
void lino_free(lino_t *);

wchar_t *linenoise(lino_t *, const wchar_t *prompt);
void lino_set_tempfile_suffix(lino_t *, const char *);
lino_error_t lino_get_error(lino_t *);
lino_error_t lino_set_error(lino_t *, lino_error_t); /* returns old */
int lino_hist_add(lino_t *, const wchar_t *line);
int lino_hist_set_max_len(lino_t *, int len);
int lino_hist_save(lino_t *, const wchar_t *filename);
int lino_hist_load(lino_t *, const wchar_t *filename);
void lino_set_result(lino_t *, wchar_t *); /* takes ownership of malloced mem; modifies it */
int lino_clear_screen(lino_t *);
void lino_set_multiline(lino_t *, int ml);
int lino_get_multiline(lino_t *);
void lino_set_selinclusive(lino_t *, int si);
int lino_get_selinculsive(lino_t *);
void lino_set_noninteractive(lino_t *, int ni);
int lino_get_noninteractive(lino_t *);

typedef wchar_t *lino_atom_cb_t(lino_t *, const wchar_t *line, int n, void *ctx);
void lino_set_atom_cb(lino_t *, lino_atom_cb_t *, void *ctx);

typedef int lino_enter_cb_t(const wchar_t *line, void *ctx);
void lino_set_enter_cb(lino_t *, lino_enter_cb_t *, void *ctx);
