/* Copyright 2013-2015
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
#include <dirent.h>
#include <syslog.h>
#include "config.h"
#include "lib.h"
#include "stream.h"
#include "hash.h"
#include "gc.h"
#include "signal.h"
#include "unwind.h"
#include "utf8.h"
#include "eval.h"
#include "syslog.h"

val prio_k;

void syslog_init(void)
{
  reg_var(intern(lit("log-pid"), user_package), num_fast(LOG_PID));
  reg_var(intern(lit("log-cons"), user_package), num_fast(LOG_CONS));
  reg_var(intern(lit("log-ndelay"), user_package), num_fast(LOG_NDELAY));
  reg_var(intern(lit("log-odelay"), user_package), num_fast(LOG_ODELAY));
  reg_var(intern(lit("log-nowait"), user_package), num_fast(LOG_NOWAIT));
#ifdef LOG_PERROR
  reg_var(intern(lit("log-perror"), user_package), num_fast(LOG_PERROR));
#endif
  reg_var(intern(lit("log-user"), user_package), num_fast(LOG_USER));
  reg_var(intern(lit("log-daemon"), user_package), num_fast(LOG_DAEMON));
  reg_var(intern(lit("log-auth"), user_package), num_fast(LOG_AUTH));
#ifdef LOG_AUTHPRIV
  reg_var(intern(lit("log-authpriv"), user_package), num_fast(LOG_AUTHPRIV));
#endif
  reg_var(intern(lit("log-emerg"), user_package), num_fast(LOG_EMERG));
  reg_var(intern(lit("log-alert"), user_package), num_fast(LOG_ALERT));
  reg_var(intern(lit("log-crit"), user_package), num_fast(LOG_CRIT));
  reg_var(intern(lit("log-err"), user_package), num_fast(LOG_ERR));
  reg_var(intern(lit("log-warning"), user_package), num_fast(LOG_WARNING));
  reg_var(intern(lit("log-notice"), user_package), num_fast(LOG_NOTICE));
  reg_var(intern(lit("log-info"), user_package), num_fast(LOG_INFO));
  reg_var(intern(lit("log-debug"), user_package), num_fast(LOG_DEBUG));
  reg_fun(intern(lit("openlog"), user_package), func_n3o(openlog_wrap, 1));
  reg_fun(intern(lit("closelog"), user_package), func_n0(closelog_wrap));
  reg_fun(intern(lit("setlogmask"), user_package), func_n1(setlogmask_wrap));
  reg_fun(intern(lit("syslog"), user_package), func_n2v(syslog_wrap));

  prio_k = intern(lit("prio"), keyword_package);

  reg_var(intern(lit("*stdlog*"), user_package), make_syslog_stream(num_fast(LOG_INFO)));
}

val openlog_wrap(val wident, val optmask, val facility)
{
  static char *ident;
  char *old_ident = ident;

  optmask = default_arg(optmask, zero);
  facility = default_arg(facility, num_fast(LOG_USER));

  ident = utf8_dup_to(c_str(wident));
  openlog(ident, c_num(optmask), c_num(facility));
  free(old_ident);

  return nil;
}

val setlogmask_wrap(val mask)
{
  return num(setlogmask(c_num(mask)));
}

val syslog_wrap(val prio, val fmt, val args)
{
  val text = formatv(nil, fmt, args);
  char *u8text = utf8_dup_to(c_str(text));
  syslog(c_num(prio), "%s", u8text);
  return nil;
}

val closelog_wrap(void)
{
  closelog();
  return nil;
}

static void syslog_mark(val stream)
{
  val stuff = coerce(val, stream->co.handle);
  gc_mark(stuff);
}

static val syslog_put_string(val stream, val str)
{
  val cell = coerce(val, stream->co.handle);
  cons_bind (prio, strstream, cell);

  for (;;) {
    val length = length_str(str);
    val span_to_newline = compl_span_str(str, lit("\n"));

    if (zerop(length))
      break;

    put_string(sub_str(str, nil, span_to_newline), strstream);

    if (equal(span_to_newline, length))
      break;

    str = sub_str(str, plus(span_to_newline, num(1)), nil);
    syslog_wrap(prio, lit("~a"), list(get_string_from_stream(strstream), nao));
    strstream = make_string_output_stream();
  }

  set(cdr_l(cell), strstream);
  return t;
}

static val syslog_put_char(val stream, val ch)
{
  val cell = coerce(val, stream->co.handle);
  cons_bind (prio, strstream, cell);

  if (ch == chr('\n')) {
    syslog_wrap(prio, lit("~a"), list(get_string_from_stream(strstream), nao));
    strstream = make_string_output_stream();
  } else {
    put_char(ch, strstream);
  }

  set(cdr_l(cell), strstream);
  return t;
}

static val syslog_put_byte(val stream, int ch)
{
  val cell = coerce(val, stream->co.handle);
  cons_bind (prio, strstream, cell);

  if (ch == '\n') {
    syslog_wrap(prio, lit("~a"), list(get_string_from_stream(strstream), nao));
    strstream = make_string_output_stream();
  } else {
    put_byte(num(ch), strstream);
  }

  set(cdr_l(cell), strstream);
  return t;
}

static val syslog_get_prop(val stream, val ind)
{
  if (ind == prio_k) {
    val cell = coerce(val, stream->co.handle);
    return car(cell);
  } else if (ind == name_k) {
    return lit("syslog");
  }
  return nil;
}

static val syslog_set_prop(val stream, val ind, val prop)
{
  if (ind == prio_k) {
    val cell = coerce(val, stream->co.handle);
    set(car_l(cell), prop);
    return t;
  }
  return nil;
}

static struct strm_ops syslog_strm_ops =
  strm_ops_init(cobj_ops_init(eq,
                              cobj_print_op,
                              cobj_destroy_stub_op,
                              syslog_mark,
                              cobj_hash_op),
                syslog_put_string,
                syslog_put_char,
                syslog_put_byte,
                0, 0, 0, 0, 0, 0, 0, 0,
                syslog_get_prop,
                syslog_set_prop);

val make_syslog_stream(val prio)
{
  return cobj(coerce(mem_t *, cons(prio, make_string_output_stream())),
              stream_s, &syslog_strm_ops.cobj_ops);
}
