/* Copyright 2011-2015
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
#include <string.h>
#include <errno.h>
#include <dirent.h>
#include <setjmp.h>
#include <stdarg.h>
#include <wchar.h>
#include <signal.h>
#include "config.h"
#include "lib.h"
#include "debug.h"
#include "gc.h"
#include "signal.h"
#include "unwind.h"
#include "stream.h"
#include "parser.h"
#include "txr.h"

int opt_debugger;
int debug_depth;
static int step_mode;
static int next_depth = -1;
static val breakpoints;
static val last_command;
static int cols = 80;

/* C99 inline instantiations. */
#if __STDC_VERSION__ >= 199901L
val debug_check(val form, val bindings, val data, val line,
                val pos, val base);
void debug_init(void);
#endif

static void help(val stream)
{
  put_string(lit("commands:\n"
                 "? - help                               s - step into form\n"
                 "h - help                               n - step over form\n"
                 "c - continue                           f - finish form\n"
                 "v - show variable binding environment  o - show current form\n"
                 "b - set breakpoint by line number      i - show current data\n"
                 "d - delete breakpoint                  w - backtrace\n"
                 "l - list breakpoints                   g - set loglevel\n"),
             stream);
}

static void show_bindings(val env, val stream)
{
  val level = zero;
  put_string(lit("bindings:\n"), stream);

  for (;; level = plus(level, one)) {
    if (nilp(env))
      break;
    else if (consp(env)) {
      format(stream, lit("~s: ~s\n"), level, env, nao);
      break;
    } else if (type(env) == ENV) {
      format(stream, lit("~s: ~s\n"), level, env->e.vbindings, nao);
      env = env->e.up_env;
    } else {
      format(stream, lit("invalid environment object: ~s\n"), env, nao);
      break;
    }
  }
}

val debug(val form, val bindings, val data, val line, val pos, val base)
{
  uses_or2;
  val rl = source_loc(form);
  cons_bind (lineno, file, rl);

  if (consp(data))
    data = car(data);
  else if (data == t)
    data = nil;

  if (!step_mode && !memqual(rl, breakpoints)
      && (debug_depth > next_depth))
  {
    return nil;
  } else {
    val print_form = t;
    val print_data = t;

    for (;;) {
      val input, command;

      if (print_form) {
        format(std_debug, lit("stopped at line ~a of ~a\n"),
               lineno, file, nao);
        format(std_debug, lit("form: ~s\n"), form, nao);
        format(std_debug, lit("depth: ~s\n"), num(debug_depth), nao);
        print_form = nil;
      }

      if (print_data) {
        int lim = cols * 8;

        if (data && pos) {
          val half = num((lim - 8) / 2);
          val full = num((lim - 8));
          val prefix, suffix;

          if (lt(pos, half)) {
            prefix = sub_str(data, zero, pos);
            suffix = sub_str(data, pos, full);
          } else {
            prefix = sub_str(data, minus(pos, half), pos);
            suffix = sub_str(data, pos, plus(pos, half));
          }

          format(std_debug, lit("data (~s:~s):\n~s . ~s\n"),
                 line, plus(pos, base), prefix, suffix, nao);
        } else if (data && length_str_ge(data, num(lim - 2))) {
          format(std_debug, lit("data (~s):\n~s...~s\n"), line,
                 sub_str(data, zero, num(lim/2 - 4)),
                 sub_str(data, num(-(lim/2 - 3)), t), nao);
        } else {
          format(std_debug, lit("data (~s):\n~s\n"), line, data, nao);
        }
        print_data = nil;
      }

      format(std_debug, lit("txr> "), nao);
      flush_stream(std_debug);

      input = split_str_set(or2(get_line(std_input), lit("q")), lit("\t "));
      command = if3(equal(first(input), null_string),
                    or2(last_command, lit("")), first(input));
      last_command = command;

      if (equal(command, lit("?")) || equal(command, lit("h"))) {
        help(std_debug);
        continue;
      } else if (equal(command, null_string)) {
        continue;
      } else if (equal(command, lit("c"))) {
        step_mode = 0;
        next_depth = -1;
        return nil;
      } else if (equal(command, lit("s"))) {
        step_mode = 1;
        return nil;
      } else if (equal(command, lit("n"))) {
        step_mode = 0;
        next_depth = debug_depth;
        return nil;
      } else if (equal(command, lit("f"))) {
        step_mode = 0;
        next_depth = debug_depth - 1;
        return nil;
      } else if (equal(command, lit("v"))) {
        show_bindings(bindings, std_debug);
      } else if (equal(command, lit("o"))) {
        print_form = t;
      } else if (equal(command, lit("i"))) {
        print_data = t;
      } else if (equal(command, lit("b")) || equal(command, lit("d")) ||
                 equal(command, lit("g")))
      {
        if (!rest(input)) {
          format(std_debug, lit("~s needs arguments\n"), command, nao);
          continue;
        } else {
          val n = int_str(second(input), num(10));
          val l = cons(n, or2(third(input), file));

          if (!n) {
            format(std_debug, lit("~s needs <line> [ <file> ]\n"),
                   command, nao);
            continue;
          }

          if (equal(command, lit("b"))) {
            breakpoints = remqual(l, breakpoints);
            push(l, &breakpoints);
          } else if (equal(command, lit("d"))) {
            val breakpoints_old = breakpoints;
            breakpoints = remqual(l, breakpoints);
            if (breakpoints == breakpoints_old)
              format(std_debug, lit("no such breakpoint\n"));
          } else {
            opt_loglevel = c_num(n);
          }
        }
      } else if (equal(command, lit("l"))) {
        format(std_debug, lit("breakpoints: ~s\n"), breakpoints, nao);
      } else if (equal(command, lit("w"))) {
        format(std_debug, lit("backtrace:\n"), nao);
        {
          uw_frame_t *iter;

          for (iter = uw_current_frame(); iter != 0; iter = iter->uw.up) {
            if (iter->uw.type == UW_DBG) {
              if (iter->db.ub_p_a_pairs)
                format(std_debug, lit("(~s ~s ~s)\n"), iter->db.func,
                       iter->db.args, iter->db.ub_p_a_pairs, nao);
              else
                format(std_debug, lit("(~s ~s)\n"), iter->db.func,
                       iter->db.args, nao);
            }
          }
        }
      } else if (equal(command, lit("q"))) {
        uw_throwf(query_error_s, lit("terminated via debugger"), nao);
      } else {
        format(std_debug, lit("unrecognized command: ~a\n"), command, nao);
      }
    }

    return nil;
  }
}

void debug_init(void)
{
  step_mode = 1;
  protect(&breakpoints, &last_command, convert(val *, 0));
  {
    char *columns = getenv("COLUMNS");
    if (columns)
      cols = atoi(columns);
    if (cols < 40)
      cols = 40;
  }
}
