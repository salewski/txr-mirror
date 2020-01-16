/* Copyright 2013-2020
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

extern val stat_s;
extern val dev_k, ino_k, mode_k, nlink_k, uid_k;
extern val gid_k, rdev_k, size_k, blksize_k, blocks_k;
extern val atime_k, mtime_k, ctime_k;
extern val dev_s, ino_s, mode_s, nlink_s, uid_s;
extern val gid_s, rdev_s, size_s, blksize_s, blocks_s;
extern val atime_s, mtime_s, ctime_s;
extern val atime_nsec_s, mtime_nsec_s, ctime_nsec_s;
extern val path_s;

val errno_to_file_error(int err);
val getenv_wrap(val name);
val at_exit_call(val func);
val at_exit_do_not_call(val func);
val usleep_wrap(val usec);
#if HAVE_FORK_STUFF
val exec_wrap(val file, val args_opt);
#endif
time_t c_time(val time);
val num_time(time_t time);
#if HAVE_SYS_STAT
struct stat;
val stat_to_struct(struct stat st, val path);
#endif
val fstat_wrap(val path);
val stdio_ftell(FILE *);
int stdio_fseek(FILE *, val, int whence);
#if HAVE_GETEUID
void repress_privilege(void);
void drop_privilege(void);
void simulate_setuid_setgid(val open_script);
#else
INLINE void repress_privilege(void) { }
INLINE void drop_privilege(void) { }
INLINE void simulate_setuid_setgid(val open_script) { }
#endif
#if HAVE_UNISTD_H
val getcwd_wrap(void);
#endif
void sysif_init(void);
