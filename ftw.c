/* Copyright 2016
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
#include <stddef.h>
#include <ftw.h>
#include "config.h"
#include ALLOCA_H
#include "lib.h"
#include "gc.h"
#include "args.h"
#include "utf8.h"
#include "eval.h"
#include "signal.h"
#include "unwind.h"
#include "sysif.h"
#include "ftw.h"

static val s_callback;
static uw_frame_t *s_exit_point;

static int ftw_callback(const char *c_path, const struct stat *c_sb,
                        int c_type, struct FTW *fb)
{
  int c_result = 1;
  uw_frame_t cont_guard;

  uw_push_guard(&cont_guard, 0);

  uw_simple_catch_begin;

  sig_check_fast();

  {
    val path = string_utf8(c_path);
    val type = num(c_type);
    val sb = stat_to_struct(*c_sb);
    val level = num(fb->level);
    val base = num(fb->base);
    val result;

    args_decl(args, ARGS_MIN);
    args_add5(args, path, type, sb, level, base);
    result = generic_funcall(s_callback, args);
    c_result = if3(integerp(result), c_num(result), 0);
  }

  uw_unwind {
    s_exit_point = uw_curr_exit_point;
    uw_curr_exit_point = 0; /* stops unwinding */
  }

  uw_catch_end;

  uw_pop_frame(&cont_guard);

  return c_result;
}

val ftw_wrap(val dirpath, val fn, val flags_in, val nopenfd_in)
{
  if (s_callback) {
    uw_throwf(error_s, lit("ftw: cannot be re-entered from "
                           "ftw callback"), nao);
  } else if (dirpath == nil) {
    return t;
  } else if (consp(dirpath)) {
    uses_or2;
    val ret = nil;
    for (; dirpath; dirpath = cdr(dirpath)) {
      val res = ftw_wrap(car(dirpath), fn, flags_in, nopenfd_in);
      if (res != t && res != nil)
        return res;
      ret = or2(ret, res);
    }
    return ret;
  } else {
    int nopenfd = c_num(default_arg(nopenfd_in, num_fast(20)));
    int flags = c_num(default_arg(flags_in, zero));
    char *dirpath_u8 = utf8_dup_to(c_str(dirpath));
    int res = (s_callback = fn,
               nftw(dirpath_u8, ftw_callback, nopenfd, flags));
    s_callback = nil;
    free(dirpath_u8);

    if (s_exit_point) {
      uw_frame_t *ep = s_exit_point;
      s_exit_point = 0;
      uw_continue(ep);
    }

    switch (res) {
    case 0:
      return t;
    case -1:
      return nil;
    default:
      return num(res);
    }
  }
}

void ftw_init(void)
{
  prot1(&s_callback);

  /* ftw flags */
#ifdef FTW_PHYS
  reg_varl(intern(lit("ftw-phys"), user_package), num_fast(FTW_PHYS));
#endif
#ifdef FTW_MOUNT
  reg_varl(intern(lit("ftw-mount"), user_package), num_fast(FTW_MOUNT));
#endif
#ifdef FTW_CHDIR
  reg_varl(intern(lit("ftw-chdir"), user_package), num_fast(FTW_CHDIR));
#endif
#ifdef FTW_DEPTH
  reg_varl(intern(lit("ftw-depth"), user_package), num_fast(FTW_DEPTH));
#endif
#ifdef FTW_ACTIONRETVAL
  reg_varl(intern(lit("ftw-actionretval"), user_package), num_fast(FTW_ACTIONRETVAL));
#endif

  /* callback type codes */
#ifdef FTW_F
  reg_varl(intern(lit("ftw-f"), user_package), num_fast(FTW_F));
#endif
#ifdef FTW_D
  reg_varl(intern(lit("ftw-d"), user_package), num_fast(FTW_D));
#endif
#ifdef FTW_DNR
  reg_varl(intern(lit("ftw-dnr"), user_package), num_fast(FTW_DNR));
#endif
#ifdef FTW_NS
  reg_varl(intern(lit("ftw-ns"), user_package), num_fast(FTW_NS));
#endif
#ifdef FTW_SL
  reg_varl(intern(lit("ftw-sl"), user_package), num_fast(FTW_SL));
#endif
#ifdef FTW_DP
  reg_varl(intern(lit("ftw-dp"), user_package), num_fast(FTW_DP));
#endif
#ifdef FTW_SLN
  reg_varl(intern(lit("ftw-sln"), user_package), num_fast(FTW_SLN));
#endif

  /* callback return values */
#ifdef FTW_CONTINUE
  reg_varl(intern(lit("ftw-continue"), user_package), num_fast(FTW_CONTINUE));
#endif
#ifdef FTW_STOP
  reg_varl(intern(lit("ftw-stop"), user_package), num_fast(FTW_STOP));
#endif
#ifdef FTW_SKIP_SUBTREE
  reg_varl(intern(lit("ftw-skip-subtree"), user_package), num_fast(FTW_SKIP_SUBTREE));
#endif
#ifdef FTW_SKIP_SIBLINGS
  reg_varl(intern(lit("ftw-skip-siblings"), user_package), num_fast(FTW_SKIP_SIBLINGS));
#endif

  reg_fun(intern(lit("ftw"), user_package), func_n4o(ftw_wrap, 2));
}
