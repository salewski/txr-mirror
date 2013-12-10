/* Copyright 2012
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
#include <assert.h>
#include <setjmp.h>
#include <wchar.h>
#include <dirent.h>
#include <syslog.h>
#include "config.h"
#include "lib.h"
#include "stream.h"
#include "hash.h"
#include "gc.h"
#include "unwind.h"
#include "utf8.h"
#include "syslog.h"

val log_pid_v, log_cons_v, log_ndelay_v;
val log_odelay_v, log_nowait_v, log_perror_v;

val log_user_v, log_daemon_v, log_auth_v;

val log_emerg_v, log_alert_v, log_crit_v, log_err_v;
val log_warning_v, log_notice_v, log_info_v, log_debug_v;

void syslog_init(void)
{
  log_pid_v = num(LOG_PID);
  log_cons_v = num(LOG_CONS);
  log_ndelay_v = num(LOG_NDELAY);

  log_odelay_v = num(LOG_ODELAY);
  log_nowait_v = num(LOG_NOWAIT);
  log_perror_v = num(LOG_PERROR);

  log_user_v = num(LOG_USER);
  log_daemon_v = num(LOG_DAEMON);
  log_auth_v = num(LOG_AUTH);

  log_emerg_v = num(LOG_EMERG);
  log_alert_v = num(LOG_ALERT);
  log_crit_v = num(LOG_CRIT);
  log_err_v = num(LOG_ERR);
  log_warning_v = num(LOG_WARNING);
  log_notice_v = num(LOG_NOTICE);
  log_info_v = num(LOG_INFO);
  log_debug_v = num(LOG_DEBUG);
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
