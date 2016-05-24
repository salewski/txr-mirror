/* Copyright 2013-2016
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

extern val stat_s;
extern val dev_k, ino_k, mode_k, nlink_k, uid_k;
extern val gid_k, rdev_k, size_k, blksize_k, blocks_k;
extern val atime_k, mtime_k, ctime_k;
extern val dev_s, ino_s, mode_s, nlink_s, uid_s;
extern val gid_s, rdev_s, size_s, blksize_s, blocks_s;
extern val atime_s, mtime_s, ctime_s;

#if !HAVE_FTRUNCATE
typedef long off_t;
#define OFF_T_MAX LONG_MAX
#define OFF_T_MIN LONG_MIN
#else
#define OFF_T_MAX ((((convert(off_t, 1) << \
                      ((sizeof(off_t) * CHAR_BIT) - 2)) - 1) << 1) + 1)
#define OFF_T_MIN (-OFF_T_MAX)
#endif

val getenv_wrap(val name);
val at_exit_call(val func);
val at_exit_do_not_call(val func);
val usleep_wrap(val usec);
#if HAVE_FORK_STUFF
val exec_wrap(val file, val args_opt);
#endif
#if HAVE_SYS_STAT
struct stat;
val stat_to_struct(struct stat st);
#endif
val statp(val path);
val statf(val path);
off_t off_t_num(val num);
val num_off_t(off_t offnum);
val stdio_ftell(FILE *);
val stdio_fseek(FILE *, val, int whence);
#if HAVE_GETEUID
void repress_privilege(void);
void drop_privilege(void);
void simulate_setuid_setgid(val open_script);
#else
INLINE void repress_privilege(void) { }
INLINE void drop_privilege(void) { }
INLINE void simulate_setuid_setgid(val open_script) { }
#endif
void sysif_init(void);
