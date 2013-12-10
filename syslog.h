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


extern val log_pid_v, log_cons_v, log_ndelay_v;
extern val log_odelay_v, log_nowait_v, log_perror_v;

extern val log_user_v, log_daemon_v, log_auth_v;

extern val log_emerg_v, log_alert_v, log_crit_v, log_err_v;
extern val log_warning_v, log_notice_v, log_info_v, log_debug_v;

extern val prio_k;

extern val std_log;

void syslog_init(void);
val openlog_wrap(val ident, val optmask, val facility);
val setlogmask_wrap(val mask);
val syslog_wrap(val prio, val fmt, val args);
val make_syslog_stream(val prio);
