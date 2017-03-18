/* Copyright 2015-2017
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

#include <stdarg.h>
#include <wchar.h>
#include <signal.h>
#include <stdlib.h>
#include <glob.h>
#include "config.h"
#include "lib.h"
#include "gc.h"
#include "utf8.h"
#include "eval.h"
#include "signal.h"
#include "unwind.h"
#include "glob.h"

static val s_errfunc;
static uw_frame_t *s_exit_point;

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

val glob_wrap(val pattern, val flags, val errfunc)
{
  cnum c_flags = c_num(default_arg(flags, zero));
  char *pat_u8 = utf8_dup_to(c_str(pattern));
  glob_t gl;

  if (s_errfunc) {
    free(pat_u8);
    uw_throwf(error_s, lit("glob: glob cannot be re-entered from "
                           "its error callback function"), nao);
  }

  s_errfunc = default_null_arg(errfunc);

  (void) glob(pat_u8, c_flags, s_errfunc ? errfunc_thunk : 0, &gl);

  s_errfunc = nil;
  free(pat_u8);

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
}
