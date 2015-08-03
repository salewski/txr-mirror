/* Copyright 2015
 * Kaz Kylheku <kaz@kylheku.com>
 * Vancouver, Canada
 * All rights reserved.
 *
 * Redistribution of this software in source and binary forms, with or without
 * modification, is permitted provided that the following two conditions are met.
 *
 * Use of this software in any manner constitutes agreement with the disclaimer
 * which follows the two conditions.
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DAMAGES, HOWEVER CAUSED,
 * AND UNDER ANY THEORY OF LIABILITY, ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <setjmp.h>
#include <wchar.h>
#include <signal.h>
#include <glob.h>
#include "config.h"
#include "lib.h"
#include "hash.h"
#include "gc.h"
#include "signal.h"
#include "unwind.h"
#include "utf8.h"
#include "eval.h"
#include "glob.h"

static val s_errfunc;

static int errfunc_thunk(const char *errpath, int errcode)
{
  val result = funcall2(s_errfunc, string_utf8(errpath), num(errcode));
  return result ? 1 : 0;
}

val glob_wrap(val pattern, val flags, val errfunc)
{
  char *pat_u8 = utf8_dup_to(c_str(pattern));
  glob_t gl;

  s_errfunc = default_bool_arg(errfunc);

  (void) glob(pat_u8, c_num(default_arg(flags, zero)),
              s_errfunc ? errfunc_thunk : 0, &gl);

  {
    int i;
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
