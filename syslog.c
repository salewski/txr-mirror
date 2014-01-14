/* Copyright 2013-2014
 * Kaz Kylheku <kaz@kylheku.com>
 * Vancouver, Canada
 * All rights reserved.
 *
 * BSD License:
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 *   1. Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *   2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in
 *      the documentation and/or other materials provided with the
 *      distribution.
 *   3. The name of the author may not be used to endorse or promote
 *      products derived from this software without specific prior
 *      written permission.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
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
#include "syslog.h"

val log_pid_v, log_cons_v, log_ndelay_v;
val log_odelay_v, log_nowait_v, log_perror_v;

val log_user_v, log_daemon_v, log_auth_v, log_authpriv_v;

val log_emerg_v, log_alert_v, log_crit_v, log_err_v;
val log_warning_v, log_notice_v, log_info_v, log_debug_v;

val prio_k;

val std_log;

void syslog_init(void)
{
  prot1(&std_log);

  log_pid_v = num(LOG_PID);
  log_cons_v = num(LOG_CONS);
  log_ndelay_v = num(LOG_NDELAY);

  log_odelay_v = num(LOG_ODELAY);
  log_nowait_v = num(LOG_NOWAIT);
#ifdef LOG_PERROR
  log_perror_v = num(LOG_PERROR);
#endif

  log_user_v = num(LOG_USER);
  log_daemon_v = num(LOG_DAEMON);
  log_auth_v = num(LOG_AUTH);
#ifdef LOG_AUTHPRIV
  log_authpriv_v = num(LOG_AUTHPRIV);
#endif

  log_emerg_v = num(LOG_EMERG);
  log_alert_v = num(LOG_ALERT);
  log_crit_v = num(LOG_CRIT);
  log_err_v = num(LOG_ERR);
  log_warning_v = num(LOG_WARNING);
  log_notice_v = num(LOG_NOTICE);
  log_info_v = num(LOG_INFO);
  log_debug_v = num(LOG_DEBUG);

  prio_k = intern(lit("prio"), keyword_package);

  std_log = make_syslog_stream(log_info_v);
}

val openlog_wrap(val wident, val optmask, val facility)
{
  static char *ident;
  char *old_ident = ident;
 
  ident = utf8_dup_to(c_str(wident));
  openlog(ident,
          if3(optmask, c_num(optmask), 0),
          if3(facility, c_num(facility), LOG_USER));
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
  val stuff = (val) stream->co.handle;
  gc_mark(stuff);
}

static val syslog_put_string(val stream, val str)
{
  val cell = (val) stream->co.handle;
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

  set(*cdr_l(cell), strstream);
  return t;
}

static val syslog_put_char(val stream, val ch)
{
  val cell = (val) stream->co.handle;
  cons_bind (prio, strstream, cell);

  if (ch == chr('\n')) {
    syslog_wrap(prio, lit("~a"), list(get_string_from_stream(strstream), nao));
    strstream = make_string_output_stream();
  } else {
    put_char(ch, strstream);
  }

  set(*cdr_l(cell), strstream);
  return t;
}

static val syslog_put_byte(val stream, int ch)
{
  val cell = (val) stream->co.handle;
  cons_bind (prio, strstream, cell);

  if (ch == '\n') {
    syslog_wrap(prio, lit("~a"), list(get_string_from_stream(strstream), nao));
    strstream = make_string_output_stream();
  } else {
    put_byte(num(ch), strstream);
  }

  set(*cdr_l(cell), strstream);
  return t;
}

static val syslog_get_prop(val stream, val ind)
{
  if (ind == prio_k) {
    val cell = (val) stream->co.handle;
    return car(cell);
  } else if (ind == name_k) {
    return lit("syslog");
  }
  return nil;
}

static val syslog_set_prop(val stream, val ind, val prop)
{
  if (ind == prio_k) {
    val cell = (val) stream->co.handle;
    set(*car_l(cell), prop);
    return t;
  }
  return nil;
}

static struct strm_ops syslog_strm_ops = {
  { cobj_equal_op,
    cobj_print_op,
    cobj_destroy_stub_op,
    syslog_mark,
    cobj_hash_op },
  syslog_put_string,
  syslog_put_char,
  syslog_put_byte,
  0, /* get_line */
  0, /* get_char */
  0, /* get_byte */
  0, /* unget_char */
  0, /* unget_byte */
  0, /* close */
  0, /* flush */
  0, /* seek */
  syslog_get_prop,
  syslog_set_prop
};

val make_syslog_stream(val prio)
{
  return cobj((mem_t *) cons(prio, make_string_output_stream()), 
              stream_s, &syslog_strm_ops.cobj_ops);
}
