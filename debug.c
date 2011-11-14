#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <dirent.h>
#include <setjmp.h>
#include <stdarg.h>
#include <wchar.h>
#include "config.h"
#include "lib.h"
#include "debug.h"
#include "gc.h"
#include "unwind.h"
#include "stream.h"
#include "parser.h"

int opt_debugger;
static int step_mode;
val breakpoints;
val last_command;

static void help(void)
{
}

val debug(val form, val bindings, val data, val line, val chr)
{
  val lineno = source_loc(form);

  if (!step_mode && !memqual(lineno, breakpoints)) {
    return nil;
  } else {
    format(std_output, lit("stopped at line ~a\n"), lineno, nao);
    format(std_output, lit("form: ~s\n"), form, nao);
    format(std_output, lit("data (~s):\n~s\n"), line, data, nao);
    if (chr)
       format(std_output, lit("(character ~s)\n"), chr, nao);

    for (;;) {
      val input, command;

      format(std_output, lit("txr> "), nao);
      flush_stream(std_output);

      input = split_str_set(get_line(std_input), lit("\t "));
      command = or2(first(input), last_command);
      last_command = command;

      if (equal(command, lit("?")) || equal(command, lit("help"))) {
        help();
        continue;
      } else if (equal(command, lit("c"))) {
        step_mode = 0;
        return nil;
      } else if (equal(command, lit("n"))) {
        step_mode = 1;
        return nil;
      } else if (equal(command, lit("v"))) {
        format(std_output, lit("bindings: ~s\n"), bindings, nao);
      } else if (equal(command, lit("f"))) {
        format(std_output, lit("stopped at line ~a\n"), lineno, nao);
        format(std_output, lit("form: ~s\n"), form, nao);
      } else if (equal(command, lit("d"))) {
        if (data) {
          format(std_output, lit("data (~s):\n~s\n"), line, data, nao);
          if (chr)
             format(std_output, lit("(character ~s)\n"), chr, nao);
        }
      } else if (equal(command, lit("b")) || equal(command, lit("d"))) {
        if (!rest(input)) {
          format(std_output, lit("b needs argument\n"), nao);
          continue;
        } else {
          long n = wcstol(c_str(second(input)), NULL, 10);
          if (equal(command, lit("b")))
            push(num(n), &breakpoints);
        }
      } else if (equal(command, lit("l"))) {
        format(std_output, lit("breakpoints: ~s\n"), breakpoints, nao);
      } else if (equal(command, lit("w"))) {
        format(std_output, lit("backtrace:\n"), nao);
        {
          uw_frame_t *iter;

          for (iter = uw_current_frame(); iter != 0; iter = iter->uw.up) {
            if (iter->uw.type == UW_DBG) {
              format(std_output, lit("(~s ~s)\n"), iter->db.func, iter->db.args, nao);
            }
          }
        }
      } else if (equal(command, lit("q"))) {
        uw_throwf(query_error_s, lit("terminated via debugger"), nao);
      } else {
        format(std_output, lit("unrecognized command: ~a\n"), command, nao);
      }
    }

    return nil;
  }
}

void debug_init(void)
{
  step_mode = 1;
  protect(&breakpoints, &last_command, (val *) 0);
}
