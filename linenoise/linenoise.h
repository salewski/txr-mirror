/* linenoise.h -- VERSION 1.0
 *
 * Guerrilla line editing library against the idea that a line editing lib
 * needs to be 20,000 lines of C code.
 *
 * See linenoise.c for more information.
 *
 * ------------------------------------------------------------------------
 *
 * Copyright (c) 2010-2014, Salvatore Sanfilippo <antirez at gmail dot com>
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

typedef struct lino_state lino_t;

typedef struct lino_completions {
  size_t len;
  char **cvec;
} lino_completions_t;

typedef void lino_compl_cb_t(const char *, lino_completions_t *, void *ctx);
void lino_set_completion_cb(lino_t *, lino_compl_cb_t *, void *ctx);
void lino_add_completion(lino_completions_t *, const char *);

lino_t *lino_make(int ifd, int ofd);
lino_t *lino_copy(lino_t *);
void lino_free(lino_t *);

char *linenoise(lino_t *, const char *prompt);
void lino_set_tempfile_suffix(lino_t *, const char *);
lino_error_t lino_get_error(lino_t *);
lino_error_t lino_set_error(lino_t *, lino_error_t); /* returns old */
int lino_hist_add(lino_t *, const char *line);
int lino_hist_set_max_len(lino_t *, int len);
int lino_hist_save(lino_t *, const char *filename);
int lino_hist_load(lino_t *, const char *filename);
int lino_clear_screen(lino_t *);
void lino_set_multiline(lino_t *, int ml);
int lino_get_multiline(lino_t *);

typedef char *lino_atom_cb_t(lino_t *, const char *line, int n, void *ctx);
void lino_set_atom_cb(lino_t *, lino_atom_cb_t *, void *ctx);
