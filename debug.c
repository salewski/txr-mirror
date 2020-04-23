/* Copyright 2011-2020
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

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <wchar.h>
#include <signal.h>
#include "config.h"
#include "lib.h"
#include "gc.h"
#include "args.h"
#include "signal.h"
#include "unwind.h"
#include "stream.h"
#include "parser.h"
#include "struct.h"
#include "eval.h"
#include "txr.h"
#include "debug.h"

int opt_debugger;
unsigned debug_state;

static val sys_print_backtrace_s;

static val dbg_clear(val mask)
{
  return unum(debug_clear(c_unum(mask)));
}

static val dbg_set(val mask)
{
  return unum(debug_set(c_unum(mask)));
}

static val dbg_restore(val state)
{
  debug_restore(c_unum(state));
  return nil;
}

void debug_init(void)
{
  sys_print_backtrace_s = intern(lit("print-backtrace"), system_package);
  reg_varl(intern(lit("dbg-enable"), system_package), num_fast(DBG_ENABLE));
  reg_varl(intern(lit("dbg-step"), system_package), num_fast(DBG_STEP));
  reg_varl(intern(lit("dbg-backtrace"), system_package), num_fast(DBG_BACKTRACE));
  reg_varl(intern(lit("dbg-all"), system_package), num_fast(DBG_ALL));
  reg_fun(intern(lit("dbg-clear"), system_package), func_n1(dbg_clear));
  reg_fun(intern(lit("dbg-set"), system_package), func_n1(dbg_set));
  reg_fun(intern(lit("dbg-restore"), system_package), func_n1(dbg_restore));
}

void debug_dump_backtrace(val stream, val prefix)
{
  val fb = lookup_fun(nil, sys_print_backtrace_s);
  if (fb)
    funcall2(cdr(fb), stream, prefix);
  else
    format(nil, lit("~s: no function binding"), sys_print_backtrace_s, nao);
}
