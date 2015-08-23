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


extern val log_pid_v, log_cons_v, log_ndelay_v;
extern val log_odelay_v, log_nowait_v, log_perror_v;

extern val log_user_v, log_daemon_v, log_auth_v, log_authpriv_v;

extern val log_emerg_v, log_alert_v, log_crit_v, log_err_v;
extern val log_warning_v, log_notice_v, log_info_v, log_debug_v;

extern val prio_k;

void syslog_init(void);
val openlog_wrap(val ident, val optmask, val facility);
val closelog_wrap(void);
val setlogmask_wrap(val mask);
val syslog_wrap(val prio, val fmt, val args);
val syslog_wrapv(val prio, val fmt, struct args *args);
val make_syslog_stream(val prio);
