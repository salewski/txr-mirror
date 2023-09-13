/* Copyright 2015-2023
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

#include <wchar.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <glob.h>
#include "config.h"
#include "lib.h"
#include "gc.h"
#include "utf8.h"
#include "eval.h"
#include "signal.h"
#include "unwind.h"
#include "glob.h"
#include "txr.h"

#define GLOB_XNOBRACE (1 << 30)
#define GLOB_XSTAR (1 << 29)

static val s_errfunc;
static uw_frame_t *s_exit_point;

static int super_glob(const char *pattern, int flags,
                      int (*errfunc) (const char *epath, int eerrno),
                      glob_t *pglob);

static int errfunc_thunk(const char *errpath, int errcode)
{
  val result = t;
  uw_frame_t cont_guard;

  uw_push_guard(&cont_guard, 0);

  uw_simple_catch_begin;

  result = funcall2(s_errfunc, string_utf8(errpath), num(errcode));

  uw_unwind {
    s_exit_point = uw_curr_exit_point;
    uw_curr_exit_point = 0; /* stops unwinding */
  }

  uw_catch_end;

  uw_pop_frame(&cont_guard);

  return result ? 1 : 0;
}

val glob_wrap(val pattern, val flags, val errfun)
{
  val self = lit("glob");
  cnum c_flags = c_num(default_arg(flags, zero), self);
  glob_t gl;
  int (*globfn)(const char *, int,
                int (*) (const char *, int),
                glob_t *) = if3((c_flags & GLOB_XSTAR) != 0, super_glob, glob);

  if (s_errfunc)
    uw_throwf(error_s, lit("~a: glob cannot be re-entered from "
                           "its error callback function"), self, nao);

  s_errfunc = default_null_arg(errfun);

  c_flags &= ~(GLOB_APPEND | GLOB_XSTAR | GLOB_XNOBRACE);

  if (globfn == super_glob)
    c_flags &= ~GLOB_BRACE;

  if (stringp(pattern)) {
    char *pat_u8 = utf8_dup_to(c_str(pattern, self));
    (void) globfn(pat_u8, c_flags, s_errfunc ? errfunc_thunk : 0, &gl);
    free(pat_u8);
  } else {
    seq_iter_t iter;
    val elem;
    seq_iter_init(self, &iter, pattern);

    while (seq_get(&iter, &elem)) {
      char *pat_u8 = utf8_dup_to(c_str(elem, self));
      (void) globfn(pat_u8, c_flags, s_errfunc ? errfunc_thunk : 0, &gl);
      if (s_exit_point)
        break;
      c_flags |= GLOB_APPEND;
      free(pat_u8);
    }
  }

  s_errfunc = nil;

  if (s_exit_point) {
    uw_frame_t *ep = s_exit_point;
    s_exit_point = 0;
    globfree(&gl);
    uw_continue(ep);
  }

  {
    size_t i;
    list_collect_decl (out, ptail);

    for (i = 0; i < gl.gl_pathc; i++)
      ptail = list_collect (ptail, string_utf8(gl.gl_pathv[i]));

    globfree(&gl);
    return out;
  }
}

static const char *super_glob_find_inner(const char *pattern)
{
  enum state { init, bsl, cls } st = init, pst;
  int ch;

  for (; (ch = *pattern) != 0; pattern++) {
    switch (st) {
    case init:
      if (strncmp(pattern, "/**/", 4) == 0)
        return pattern + 1;
      switch (ch) {
      case '\\':
        pst = init;
        st = bsl;
        break;
      case '[':
        st = cls;
        break;
      }
      break;
    case bsl:
      st = pst;
      break;
    case cls:
      switch (ch) {
      case '\\':
        pst = cls;
        st = bsl;
        break;
      case ']':
        st = init;
        break;
      }
    }
  }

  return 0;
}

static int super_glob_rec(const char *pattern, int flags,
                          int (*errfunc) (const char *epath, int eerrno),
                          glob_t *pglob, size_t star_limit)
{
  const char *dblstar = 0;

  if (strncmp(pattern, "**/", 3) == 0 || strcmp(pattern, "**") == 0) {
    dblstar = pattern;
  } else if ((dblstar = super_glob_find_inner(pattern)) != 0) {
    /* nothing */
  } else if (strlen(pattern) >= 3) {
    const char *end = pattern + strlen(pattern);
    if (strcmp(end - 3, "/**") == 0)
      dblstar = end - 2;
  }

  if (dblstar == 0) {
    return glob(pattern, flags, errfunc, pglob);
  } else {
    size_t i, base_len = strlen(pattern);
    size_t ds_off = dblstar - pattern;
    size_t tail_off = ds_off + 2;
    size_t limit = star_limit > 10 ? 10 : star_limit;

    for (i = 0; i < limit; i++) {
      size_t space = base_len - 3 + i * 2;
      char *pat_copy = coerce(char *, chk_malloc(space + 2));
      size_t j;
      char *out = pat_copy + ds_off;
      int res;

      strncpy(pat_copy, pattern, ds_off);

      for (j = 0; j < i; j++) {
        if (j > 0)
          *out++ = '/';
        *out++ = '*';
      }

      if (i == 0 && pattern[tail_off] == '/')
        strcpy(out, pattern + tail_off + 1);
      else
        strcpy(out, pattern + tail_off);

      if (i > 0)
        flags |= GLOB_APPEND;

      res = super_glob_rec(pat_copy, flags, errfunc, pglob, star_limit - i);

      free(pat_copy);

      if (res && res != GLOB_NOMATCH)
        return res;
    }

    return 0;
  }
}

static int glob_path_cmp(const void *ls, const void *rs)
{
  const char *lstr = *convert(const char * const *, ls);
  const char *rstr = *convert(const char * const *, rs);

  for (; *lstr && *rstr; lstr++, rstr++)
  {
    if (*lstr == *rstr)
      continue;
    if (*lstr == '/')
      return -1;
    if (*rstr == '/')
      return 1;
    if (*lstr < *rstr)
      return -1;
    if (*lstr > *rstr)
      return 1;
  }

  return lstr ? 1 : rstr ? -1 : 0;
}

static int super_glob(const char *pattern, int flags,
                      int (*errfunc) (const char *epath, int eerrno),
                      glob_t *pglob)
{
  int res = super_glob_rec(pattern, flags | GLOB_NOSORT, errfunc, pglob, 48);

  if (res == 0 && (flags & GLOB_NOSORT) == 0) {
    qsort(pglob->gl_pathv, pglob->gl_pathc,
          sizeof pglob->gl_pathv[0], glob_path_cmp);
  }

  return 0;
}

void glob_init(void)
{
  prot1(&s_errfunc);
  reg_fun(intern(lit("glob"), user_package), func_n3o(glob_wrap, 1));
  reg_varl(intern(lit("glob-err"), user_package), num_fast(GLOB_ERR));
  reg_varl(intern(lit("glob-mark"), user_package), num_fast(GLOB_MARK));
  reg_varl(intern(lit("glob-nosort"), user_package), num_fast(GLOB_NOSORT));
  reg_varl(intern(lit("glob-nocheck"), user_package), num_fast(GLOB_NOCHECK));
  reg_varl(intern(lit("glob-noescape"), user_package), num_fast(GLOB_NOESCAPE));
#ifdef GLOB_PERIOD
  reg_varl(intern(lit("glob-period"), user_package), num_fast(GLOB_PERIOD));
#endif
#ifdef GLOB_ALTDIRFUNC
  reg_varl(intern(lit("glob-altdirfunc"), user_package), num_fast(GLOB_ALTDIRFUNC));
#endif
#ifdef GLOB_BRACE
  reg_varl(intern(lit("glob-brace"), user_package), num_fast(GLOB_BRACE));
#endif
#ifdef GLOB_NOMAGIC
  reg_varl(intern(lit("glob-nomagic"), user_package), num_fast(GLOB_NOMAGIC));
#endif
#ifdef GLOB_TILDE
  reg_varl(intern(lit("glob-tilde"), user_package), num_fast(GLOB_TILDE));
#endif
#ifdef GLOB_TILDE_CHECK
  reg_varl(intern(lit("glob-tilde-check"), user_package), num_fast(GLOB_TILDE_CHECK));
#endif
#ifdef GLOB_ONLYDIR
  reg_varl(intern(lit("glob-onlydir"), user_package), num_fast(GLOB_ONLYDIR));
#endif
  reg_varl(intern(lit("glob-xstar"), system_package), num(GLOB_XSTAR));
  reg_varl(intern(lit("glob-xnobrace"), user_package), num(GLOB_XNOBRACE));
}
