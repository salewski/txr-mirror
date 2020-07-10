/* Copyright 2010-2020
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

#define UTF8_DECL_OPENDIR
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dirent.h>
#include <wchar.h>
#include <signal.h>
#include <errno.h>
#include <time.h>
#include "config.h"
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#if HAVE_FCNTL
#include <fcntl.h>
#endif
#if HAVE_SYS_WAIT
#include <sys/wait.h>
#endif
#if HAVE_SYS_STAT
#include <sys/stat.h>
#endif
#if HAVE_WINDOWS_H
#include <windows.h>
#endif
#if HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#if HAVE_SYS_TIME
#include <sys/time.h>
#endif
#if HAVE_SYS_SYSMACROS_H
#include <sys/sysmacros.h>
#endif
#if HAVE_POLL
#include <poll.h>
#endif
#if HAVE_PWUID
#include <pwd.h>
#endif
#if HAVE_GRGID
#include <grp.h>
#endif
#if HAVE_FNMATCH
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#include <fnmatch.h>
#endif
#if HAVE_UNAME
#include <sys/utsname.h>
#endif
#if HAVE_DLOPEN
#include <dlfcn.h>
#endif
#if HAVE_CRYPT_R
#include <crypt.h>
#endif
#if HAVE_UTIME
#include <utime.h>
#endif
#include "alloca.h"
#include "lib.h"
#include "stream.h"
#include "hash.h"
#include "signal.h"
#include "utf8.h"
#include "unwind.h"
#include "gc.h"
#include "eval.h"
#include "args.h"
#include "struct.h"
#include "itypes.h"
#include "txr.h"
#include "sysif.h"

val stat_s;
val dev_k, ino_k, mode_k, nlink_k, uid_k;
val gid_k, rdev_k, size_k, blksize_k, blocks_k;
val atime_k, mtime_k, ctime_k;
val dev_s, ino_s, mode_s, nlink_s, uid_s;
val gid_s, rdev_s, size_s, blksize_s, blocks_s;
val atime_s, mtime_s, ctime_s;
val atime_nsec_s, mtime_nsec_s, ctime_nsec_s;
val path_s, dir_s, dirent_s;

#if HAVE_PWUID
val passwd_s, gecos_s, shell_s;
#endif

#if HAVE_GRGID
val group_s, mem_s;
#endif

#if HAVE_UNAME
val utsname_s, sysname_s, nodename_s, release_s, version_s, machine_s;
val domainname_s;
#endif

#if HAVE_FCNTL
val flock_s, type_s, whence_s, start_s, len_s, pid_s;
#endif

#if HAVE_DLOPEN
val dlhandle_s, dlsym_s;
#endif

static val at_exit_list;

static val dirent_st;

static val errno_wrap(val newval)
{
  val self = lit("errno");
  val oldval = num(errno);
  if (default_null_arg(newval))
    errno = c_num(newval, self);
  return oldval;
}

val errno_to_str(int eno)
{
#if HAVE_STRERROR_POSIX
  char buf[128];
  return strerror_r(eno, buf, sizeof buf) >= 0 ? string_utf8(buf) : nil;
#elif HAVE_STRERROR_GNU
  char buf[128];
  return string_utf8(strerror_r(eno, buf, sizeof buf));
#else
  return string_utf8(strerror(eno));
#endif
}

static val strerror_wrap(val errnum)
{
  val self = lit("strerror");
  int eno = c_int(errnum, self);
  return errno_to_str(eno);
}

#if HAVE_STRSIGNAL

static val strsignal_wrap(val signum)
{
  val self = lit("strsignal");
  int sig = c_int(signum, self);
  const char *str = strsignal(sig);
  return if3(str,
             string_utf8(strsignal(sig)),
             format(nil, lit("Unknown signal %s"), signum, nao));
}

#endif

#if HAVE_DAEMON
static val daemon_wrap(val nochdir, val noclose)
{
  int result = daemon(nochdir ? 1 : 0, noclose ? 1 : 0);
  return result == 0 ? t : nil;
}
#endif

static val exit_wrap(val status)
{
  val self = lit("exit");
  int stat;

  if missingp(status)
    stat = EXIT_SUCCESS;
  else if (status == nil)
    stat = EXIT_FAILURE;
  else if (status == t)
    stat = EXIT_SUCCESS;
  else
    stat = c_num(status, self);

  exit(stat);
  /* notreached */
  return nil;
}

val at_exit_call(val func)
{
  push(func, &at_exit_list);
  return func;
}

val at_exit_do_not_call(val func)
{
  val old = at_exit_list;
  at_exit_list = remq(func, old, nil);
  return tnil(old != at_exit_list);
}

static void at_exit_handler(void)
{
  val iter;

  for (iter = at_exit_list; iter; iter = cdr(iter))
    funcall(car(iter));
}

static val abort_wrap(void)
{
  abort();
}

val usleep_wrap(val usec)
{
  val self = lit("usleep");
  val retval;
  cnum u = c_num(usec, self);

  sig_save_enable;

#if HAVE_POSIX_NANOSLEEP
  struct timespec ts;
  ts.tv_sec = u / 1000000;
  ts.tv_nsec = (u % 1000000) * 1000;
  retval = if3(nanosleep(&ts, 0) == 0, t, nil);
#elif HAVE_POSIX_SLEEP && HAVE_POSIX_USLEEP
  retval = if2(sleep(u / 1000000) == 0 &&
               usleep(u % 1000000) == 0, t);
#elif HAVE_WINDOWS_H
  Sleep(u / 1000);
  retval = t;
#else
#error port me!
#endif

  sig_restore_enable;
  return retval;
}

#if HAVE_UNISTD_H

static val getpid_wrap(void)
{
  return num(getpid());
}

#if HAVE_GETPPID
static val getppid_wrap(void)
{
  return num(getppid());
}
#endif

#endif

static val env_hash(void)
{
  val env_strings = env();
  val hash = make_hash(nil, nil, t);

  for (; env_strings; env_strings = cdr(env_strings)) {
    val estr = car(env_strings);
    val eqpos = break_str(estr, lit("="));
    val key = sub(estr, 0, eqpos);
    val val = sub(estr, succ(eqpos), t);
    sethash(hash, key, val);
  }

  return hash;
}

val errno_to_file_error(int err)
{
  switch (err) {
#ifdef ENOENT
  case ENOENT: return path_not_found_s;
#endif
#ifdef EEXIST
  case EEXIST: return path_exists_s;
#endif
#ifdef EPERM
  case EPERM: return path_permission_s;
#endif
#ifdef EACCES
  case EACCES: return path_permission_s;
#endif
  default: return file_error_s;
  }
}

#if HAVE_MKDIR
static val mkdir_wrap(val path, val mode)
{
  val self = lit("mkdir");
  cnum cmode = c_num(default_arg(mode, num_fast(0777)), self);
  char *u8path = utf8_dup_to(c_str(path));
  int err = mkdir(u8path, cmode);
  free(u8path);

  if (err < 0) {
    int eno = errno;
    uw_throwf(errno_to_file_error(eno), lit("mkdir ~a: ~d/~s"),
              path, num(eno), errno_to_str(eno), nao);
  }

  return t;
}
#elif HAVE_WINDOWS_H
static val mkdir_wrap(val path, val mode)
{
  int err = _wmkdir(c_str(path));

  (void) mode;
  if (err < 0) {
    int eno = errno;
    uw_throwf(errno_to_file_error(eno), lit("mkdir ~a: ~d/~s"),
              path, num(eno), errno_to_str(eno), nao);
  }

  return t;
}
#endif

#if HAVE_CHMOD || HAVE_CHOWN || HAVE_SYS_STAT || HAVE_FILE_STAMP_CHANGE
static int get_fd(val stream, val self)
{
  val fd_in = if3(integerp(stream), stream, stream_fd(stream));

  if (stream && !fd_in)
    uw_throwf(file_error_s,
              lit("~a: stream ~s has no :fd property"),
              self, stream, nao);

  if (!stream)
    uw_throwf(file_error_s,
              lit("~a: ~s isn't a stream object"),
              self, stream, nao);

  return c_int(fd_in, self);
}
#endif

#if HAVE_SYS_STAT
static int do_stat(val wpath, struct stat *buf)
{
  if (stringp(wpath)) {
    char *path = utf8_dup_to(c_str(wpath));
    int res = stat(path, buf);
    free(path);
    return res;
  } else {
    val self = lit("stat");
    int fd = get_fd(wpath, self);
    return fstat(fd, buf);
  }
}

#ifdef S_IFLNK
static int do_lstat(val wpath, struct stat *buf)
{
  char *path = utf8_dup_to(c_str(wpath));
  int res = lstat(path, buf);
  free(path);
  return res;
}
#else
#define do_lstat do_stat
#endif
#endif

#if HAVE_MKDIR || HAVE_WINDOWS_H
static val mkdir_nothrow_exists(val path, val mode)
{
  val ret = t;

  uw_catch_begin(cons(file_error_s, nil), esym, eobj);

  ret = mkdir_wrap(path, mode);

  uw_catch (esym, eobj) {
    switch (errno) {
    case EACCES:
    case EPERM:
      ret = num(errno);
      break;
    case EEXIST:
      ret = nil;
#if HAVE_SYS_STAT
      {
        struct stat st;
        int err = do_stat(path, &st);
        if (err == 0 && !S_ISDIR(st.st_mode))
          ret = num(EEXIST);
      }
#endif
      break;
    default:
      uw_throw(esym, eobj);
    }
  }

  uw_unwind;

  uw_catch_end;

  return ret;
}

static val ensure_dir(val path, val mode)
{
  val self = lit("ensure-dir");
#if HAVE_WINDOWS_H
  val sep = lit("\\");
  val sep_set = lit("\\/");
#else
  val sep = lit("/");
  val sep_set = lit("/");
#endif
  val split_path = split_str_set(path, sep_set);
  val partial_path = pop(&split_path);
  val ret = t;

  for (;;) {
    if (length(partial_path) != zero)
      ret = mkdir_nothrow_exists(partial_path, mode);

    if (!split_path)
      break;

    partial_path = scat3(partial_path, sep, pop(&split_path));
  }

  if (integerp(ret)) {
    int eno = c_num(ret, self);
    uw_throwf(errno_to_file_error(eno),
              lit("ensure-dir: ~a: ~d/~s"), path, ret,
              errno_to_str(eno), nao);
  }

  return ret;
}
#endif

#if HAVE_UNISTD_H
static val chdir_wrap(val path)
{
  char *u8path = utf8_dup_to(c_str(path));
  int err = chdir(u8path);
  free(u8path);

  if (err < 0) {
    int eno = errno;
    uw_throwf(errno_to_file_error(eno), lit("chdir ~a: ~d/~s"),
              path, num(eno), errno_to_str(eno), nao);
  }

  return t;
}

val getcwd_wrap(void)
{
  size_t guess = 256;

  for (;;) {
    char *u8buf = coerce(char *, chk_malloc(guess));

    if (getcwd(u8buf, guess) == 0) {
      int eno = errno;
      free(u8buf);
      if (eno != ERANGE) {
        uw_throwf(errno_to_file_error(eno), lit("getcwd: ~d/~s"),
                  num(errno), errno_to_str(errno), nao);
      }
      if (2 * guess > guess)
        guess *= 2;
      else
        uw_throwf(file_error_s, lit("getcwd: weird problem"), nao);
    } else {
      val out = string_utf8(u8buf);
      free(u8buf);
      return out;
    }
  }
}

static val rmdir_wrap(val path)
{
  char *u8path = utf8_dup_to(c_str(path));
  int err = rmdir(u8path);
  free(u8path);

  if (err < 0) {
    int eno = errno;
    uw_throwf(errno_to_file_error(eno), lit("rmdir ~a: ~d/~s"),
              path, num(eno), errno_to_str(eno), nao);
  }

  return t;
}
#endif

#if HAVE_MAKEDEV

static val makedev_wrap(val major, val minor)
{
  val self = lit("makedev");
  return num(makedev(c_num(major, self), c_num(minor, self)));
}

static val minor_wrap(val dev)
{
  val self = lit("minor");
  return num(minor(c_num(dev, self)));
}

static val major_wrap(val dev)
{
  val self = lit("major");
  return num(major(c_num(dev, self)));
}

#endif

#if HAVE_MKNOD

static val mknod_wrap(val path, val mode, val dev)
{
  val self = lit("mknod");
  cnum cmode = c_num(mode, self);
  cnum cdev = c_num(default_arg(dev, zero), self);
  char *u8path = utf8_dup_to(c_str(path));
  int err = mknod(u8path, cmode, cdev);
  free(u8path);

  if (err < 0) {
    int eno = errno;
#if HAVE_MAKEDEV
    uw_throwf(errno_to_file_error(eno), lit("mknod ~a ~a ~a (~d:~d): ~d/~s"),
              path, mode, dev, major_wrap(dev), minor_wrap(dev), num(eno),
              errno_to_str(eno), nao);
#else
    uw_throwf(errno_to_file_error(eno), lit("mknod ~a ~a ~a: ~d/~s"),
              path, mode, dev, num(eno),
              errno_to_str(eno), nao);
#endif
  }

  return t;
}

#endif

#if HAVE_MKFIFO

static val mkfifo_wrap(val path, val mode)
{
  val self = lit("mkfifo");
  cnum cmode = c_num(mode, self);
  char *u8path = utf8_dup_to(c_str(path));
  int err = mkfifo(u8path, cmode);
  free(u8path);

  if (err < 0) {
    int eno = errno;
    uw_throwf(errno_to_file_error(eno), lit("mknod ~a ~a: ~d/~s"),
              path, mode, num(eno),
              errno_to_str(eno), nao);
  }

  return t;
}

#endif

#if HAVE_CHMOD

#define CHM_O 4
#define CHM_G 2
#define CHM_U 1

enum chm_state { chm_who, chm_perm, chm_nxtop, chm_comma };
enum chm_op { chm_add, chm_sub, chm_set };

static val chmod_wrap(val target, val mode)
{
  val self = lit("chmod");
  cnum cmode = 0;
  int err = 0;
  char *u8path = if3(stringp(target), utf8_dup_to(c_str(target)), 0);
  int fd = if3(u8path, -1, get_fd(target, self));

  if (integerp(mode)) {
    cmode = c_num(mode, self);
  } else if (stringp(mode)) {
#if HAVE_SYS_STAT
    struct stat st;
    unsigned who = 0;
    enum chm_state cs = chm_who;
    enum chm_op op = chm_add;

    if (u8path)
      err = stat(u8path, &st);
    else
      err = fstat(fd, &st);

    if (err == 0) {
      const wchar_t *cm = c_str(mode);
      wchar_t ch;
      mode_t srcm = 0, oldm = st.st_mode;

      cmode = oldm;

      while ((ch = *cm++) != 0) {
        switch (cs) {
        case chm_who:
          switch (ch) {
          case 'u': who |= CHM_U; continue;
          case 'g': who |= CHM_G; continue;
          case 'o': who |= CHM_O; continue;
          case 'a': who |= CHM_U | CHM_G | CHM_O; continue;
          case '+': op = chm_add; cs = chm_perm; continue;
          case '-': op = chm_sub; cs = chm_perm; continue;
          case '=': op = chm_set; cs = chm_perm; break;
          default:
            goto inval;
          }
          break;
        case chm_nxtop:
          srcm = 0;
          switch (ch) {
          case '+': op = chm_add; cs = chm_perm; continue;
          case '-': op = chm_sub; cs = chm_perm; continue;
          case '=': op = chm_set; cs = chm_perm; break;
          default: goto perm;
          }
          break;
        perm:
        case chm_perm:
          switch (ch) {
          case 'u': srcm |= (oldm & 0700) >> 6;  cs = chm_comma; break;
          case 'g': srcm |= (oldm & 0070) >> 3;  cs = chm_comma; break;
          case 'o': srcm |= (oldm & 0007);       cs = chm_comma; break;
          case 'r': srcm |= 4;                   cs = chm_nxtop; break;
          case 'w': srcm |= 2;                   cs = chm_nxtop; break;
          case 'x': srcm |= 1;                   cs = chm_nxtop; break;
          case 's': srcm |= 010;                 cs = chm_nxtop; break;
          case 't': srcm |= 020;                 cs = chm_nxtop; break;
          case 'X': srcm |= ((cmode & 0111) != 0 ||
                             S_ISDIR(cmode));    cs = chm_nxtop; break;
          case ',': goto nextmode;
          default:
            goto inval;
          }
          break;
        case chm_comma:
          if (ch != ',')
            goto inval;
        nextmode:
          srcm = 0; who = 0; oldm = cmode; cs = chm_who;
          continue;
        }

        {
          mode_t bits = 0;
          mode_t mask = 0;
          int implicit_all = (who == 0);

          if ((srcm & 020))
            bits |= S_ISVTX;

          if (implicit_all || (who & CHM_U) != 0) {
            mask |= (0700 | S_ISUID);
            if ((srcm & 010))
              bits |= S_ISUID;
            bits |= (srcm & 7) << 6;
          }

          if (implicit_all || (who & CHM_G) != 0) {
            mask |= (0070 | S_ISGID);
            if ((srcm & 010))
              bits |= S_ISGID;
            bits |= (srcm & 7) << 3;
          }

          if (implicit_all || (who & CHM_O) != 0) {
            mask |= 0007;
            bits |= (srcm & 7);
          }

          if (implicit_all) {
            mode_t um = umask(0777);
            umask(um);
            bits &= ~um;
          }

          switch (op) {
          case chm_add: cmode |= bits; break;
          case chm_sub: cmode &= ~bits; break;
          case chm_set:
            if (cs == chm_perm) {
              oldm = cmode;
              cmode &= ~mask;
              if (implicit_all || (who & CHM_O) != 0)
                cmode &= ~S_ISVTX; /* GNU Coreutils 8.28 chmod behavior */
            }
            cmode |= bits;
            break;
          }
        }
      }

      if (cs == chm_who)
        goto inval;
    }
#else
    free(u8path);
    uw_throwf(file_error_s, lit("~s: ~s mode requires stat"),
              self, mode, nao);
#endif
  } else {
inval:
    free(u8path);
    uw_throwf(file_error_s, lit("~s: invalid mode ~s"),
              self, mode, nao);
  }

  if (err == 0) {
    if (u8path) {
      err = chmod(u8path, cmode);
    } else {
      int fd = get_fd(target, self);
      err = fchmod(fd, cmode);
    }
  }

  free(u8path);

  if (err < 0) {
    int eno = errno;
    val error = errno_to_file_error(eno);
    val errstr = errno_to_str(eno);

    if (stringp(mode))
      uw_throwf(error, lit("~a ~a ~a: ~d/~s"),
                self, target, mode, num(eno), errstr, nao);
    else
      uw_throwf(error, lit("~a ~a #o~o: ~d/~s"),
                self, target, mode, num(eno), errstr, nao);
  }

  return t;
}

#endif

#if HAVE_CHOWN

static val do_chown(val target, val uid, val gid, val link_p, val self)
{
  cnum cuid = c_num(uid, self);
  cnum cgid = c_num(gid, self);
  int err;

  if (stringp(target)) {
    char *u8path = utf8_dup_to(c_str(target));
    err = if3(link_p, lchown, chown)(u8path, cuid, cgid);
    free(u8path);
  } else {
    int fd = get_fd(target, self);
    err = fchown(fd, cuid, cgid);
  }

  if (err < 0) {
    int eno = errno;
    uw_throwf(errno_to_file_error(eno), lit("~a ~a ~a ~a: ~d/~s"),
              self, target, uid, gid, num(eno),
              errno_to_str(eno), nao);
  }

  return t;
}

static val chown_wrap(val target, val uid, val gid)
{
  return do_chown(target, uid, gid, nil, lit("chown"));
}

static val lchown_wrap(val target, val uid, val gid)
{
  return do_chown(target, uid, gid, t, lit("lchown"));
}

#endif

#if HAVE_SYMLINK

static val symlink_wrap(val target, val to)
{
  const wchar_t *wtarget = c_str(target);
  const wchar_t *wto = c_str(to);
  char *u8target = utf8_dup_to(wtarget);
  char *u8to = utf8_dup_to(wto);
  int err = symlink(u8target, u8to);
  free(u8target);
  free(u8to);

  if (err < 0) {
    int eno = errno;
    uw_throwf(errno_to_file_error(eno), lit("symlink ~a ~a: ~d/~s"),
              target, to, num(eno), errno_to_str(eno), nao);
  }

  return t;
}

static val link_wrap(val target, val to)
{
  const wchar_t *wtarget = c_str(target);
  const wchar_t *wto = c_str(to);
  char *u8target = utf8_dup_to(wtarget);
  char *u8to = utf8_dup_to(wto);
  int err = link(u8target, u8to);
  free(u8target);
  free(u8to);

  if (err < 0) {
    int eno = errno;
    uw_throwf(errno_to_file_error(eno), lit("link ~a ~a: ~d/~s"),
              target, to, num(eno), errno_to_str(eno), nao);
  }

  return t;
}

static val readlink_wrap(val path)
{
  char *u8path = utf8_dup_to(c_str(path));
  ssize_t guess = 256;

  for (;;) {
    char *u8buf = coerce(char *, chk_malloc(guess));
    ssize_t bytes = readlink(u8path, u8buf, guess);

    if (bytes >= guess) {
      free(u8buf);
      if (2 * guess > guess)
        guess *= 2;
      else
        uw_throwf(file_error_s, lit("readlink: weird problem"), nao);
    } else if (bytes <= 0) {
      int eno = errno;
      free(u8buf);
      uw_throwf(errno_to_file_error(eno), lit("readlink ~a: ~d/~s"),
                path, num(eno), errno_to_str(eno), nao);
    } else {
      val out;
      u8buf[bytes] = 0;
      out = string_utf8(u8buf);
      free(u8buf);
      return out;
    }
  }
}

#endif

#if HAVE_FCNTL

static void flock_pack(val self, val in, struct flock *out)
{
  out->l_type = c_short(slot(in, type_s), self);
  out->l_whence = c_short(slot(in, whence_s), self);
  out->l_start = c_num(slot(in, start_s), self);
  out->l_len = c_num(slot(in, len_s), self);
}

static void flock_unpack(val out, struct flock *in)
{
  slotset(out, type_s, num(in->l_type));
  slotset(out, whence_s, num(in->l_whence));
  slotset(out, start_s, num(in->l_start));
  slotset(out, len_s, num(in->l_len));
  slotset(out, pid_s, num(in->l_pid));
}

static val fcntl_wrap(val fd_in, val cmd_in, val arg_in)
{
  val self = lit("fcntl");
  int fd = c_int(fd_in, self);
  int cmd = c_int(cmd_in, self);
  int res = -1;

  switch (cmd) {
  case F_DUPFD:
#ifdef F_DUPFD_CLOEXEC
  case F_DUPFD_CLOEXEC:
#endif
  case F_SETFD:
  case F_SETFL:
    if (missingp(arg_in)) {
      errno = EINVAL;
    } else {
      long arg = c_long(arg_in, self);
      res = fcntl(fd, cmd, arg);
    }
    break;
  case F_GETFD:
  case F_GETFL:
    res = fcntl(fd, cmd);
    break;
  case F_SETLK:
  case F_SETLKW:
  case F_GETLK:
    if (missingp(arg_in)) {
      errno = EINVAL;
    } else {
      struct flock fl = all_zero_init;
      flock_pack(self, arg_in, &fl);
      res  = fcntl(fd, cmd, &fl);
      if (cmd == F_GETLK)
        flock_unpack(arg_in, &fl);
    }
    break;
  default:
    errno = EINVAL;
    break;
  }

  return num(res);
}

#endif

#if HAVE_FORK_STUFF
static val fork_wrap(void)
{
  pid_t pid = fork();

  if (pid < 0)
    return nil;
  return num(pid);
}

static val wait_wrap(val pid, val flags)
{
  val self = lit("wait");
  cnum p = c_num(default_arg(pid, negone), self);
  cnum f = c_num(default_arg(flags, zero), self);
  int status = 0, result = waitpid(p, &status, f);
  return if2(result >= 0, cons(num(result), num(status)));
}

static val wifexited(val status)
{
  val self = lit("wifexited");
  int s = c_num(if3(consp(status), cdr(status), status), self);
  return tnil(WIFEXITED(s));
}

static val wexitstatus(val status)
{
  val self = lit("wexitstatus");
  int s = c_num(if3(consp(status), cdr(status), status), self);
  return num(WEXITSTATUS(s));
}

static val wifsignaled(val status)
{
  val self = lit("wifsignaled");
  int s = c_num(if3(consp(status), cdr(status), status), self);
  return tnil(WIFSIGNALED(s));
}

static val wtermsig(val status)
{
  val self = lit("wtermsig");
  int s = c_num(if3(consp(status), cdr(status), status), self);
  return num(WTERMSIG(s));
}

#ifdef WCOREDUMP
static val wcoredump(val status)
{
  val self = lit("wcoredump");
  int s = c_num(if3(consp(status), cdr(status), status), self);
  return tnil(WCOREDUMP(s));
}
#endif

static val wifstopped(val status)
{
  val self = lit("wifstopped");
  int s = c_num(if3(consp(status), cdr(status), status), self);
  return tnil(WIFSTOPPED(s));
}

static val wstopsig(val status)
{
  val self = lit("wstopsig");
  int s = c_num(if3(consp(status), cdr(status), status), self);
  return num(WSTOPSIG(s));
}

#ifdef WIFCONTINUED
static val wifcontinued(val status)
{
  val self = lit("wifcontinued");
  int s = c_num(if3(consp(status), cdr(status), status), self);
  return tnil(WIFCONTINUED(s));
}
#endif

static val dup_wrap(val old, val neu)
{
  val self = lit("dupfd");
  if (missingp(neu))
    return num(dup(c_num(old, self)));
  return num(dup2(c_num(old, self), c_num(neu, self)));
}

static val close_wrap(val fd, val throw_on_error)
{
  int res = close(c_int(fd, lit("close")));

  if (res < 0) {
    if (default_null_arg(throw_on_error)) {
      int eno = errno;
      uw_throwf(errno_to_file_error(eno), lit("close ~a: ~d/~s"),
                fd, num(eno), errno_to_str(eno), nao);
    }
    return nil;
  }

  return t;
}

val exec_wrap(val file, val args_opt)
{
  val self = lit("execvp");
  val args = default_null_arg(args_opt);
  int nargs = c_num(length(args), self) + 1;
  char **argv = if3(nargs < 0 || nargs == INT_MAX,
                    (uw_throwf(process_error_s, lit("~a: argument list overflow"),
                               self, nao), convert(char **, 0)),
                    coerce(char **, chk_xalloc(nargs + 1, sizeof *argv, self)));
  val iter;
  int i;

  for (i = 0, iter = cons(file, args); iter; i++, iter = cdr(iter)) {
    val arg = car(iter);
    argv[i] = utf8_dup_to(c_str(arg));
  }
  argv[i] = 0;

  if (execvp(argv[0], argv) < 0)
    uw_throwf(process_error_s, lit("~s ~a: ~d/~s"),
              self, file, num(errno), errno_to_str(errno), nao);
  uw_throwf(process_error_s, lit("~s ~a returned"), self, file, nao);
}

static val exit_star_wrap(val status)
{
  val self = lit("exit*");
  int stat;

  if (status == nil)
    stat = EXIT_FAILURE;
  else if (status == t)
    stat = EXIT_SUCCESS;
  else
    stat = c_num(status, self);

  _exit(stat);
  /* notreached */
  return nil;
}

#endif

time_t c_time(val time, val self)
{
  return if3(convert(time_t, -1) > 0, (time_t) c_unum(time, self), (time_t) c_num(time, self));
}

val num_time(time_t time)
{
  return if3(convert(time_t, -1) > 0, unum(time), num(time));
}

#if HAVE_SYS_STAT
static val stat_to_list(struct stat st)
{
  return list(dev_k, num(st.st_dev),
              ino_k, num(st.st_ino),
              mode_k, num(st.st_mode),
              nlink_k, num(st.st_nlink),
              uid_k, num(st.st_uid),
              gid_k, num(st.st_gid),
              rdev_k, num(st.st_rdev),
              size_k, num(st.st_size),
#if !HAVE_WINDOWS_H
              blksize_k, num(st.st_blksize),
              blocks_k, num(st.st_blocks),
#else
              blksize_k, zero,
              blocks_k, zero,
#endif
              atime_k, num(st.st_atime),
              mtime_k, num(st.st_mtime),
              ctime_k, num(st.st_ctime),
              nao);
}

val stat_to_struct(struct stat st, val path)
{
  args_decl(args, ARGS_MIN);
  val strct = make_struct(stat_s, nil, args);
  slotset(strct, dev_s, num(st.st_dev));
  slotset(strct, ino_s, num(st.st_ino));
  slotset(strct, mode_s, num(st.st_mode));
  slotset(strct, nlink_s, num(st.st_nlink));
  slotset(strct, uid_s, num(st.st_uid));
  slotset(strct, gid_s, num(st.st_gid));
  slotset(strct, rdev_s, num(st.st_rdev));
  slotset(strct, size_s, num(st.st_size));
#if !HAVE_WINDOWS_H
  slotset(strct, blksize_s, num(st.st_blksize));
  slotset(strct, blocks_s, num(st.st_blocks));
#else
  slotset(strct, blksize_s, zero);
  slotset(strct, blocks_s, zero);
#endif
  slotset(strct, atime_s, num_time(st.st_atime));
  slotset(strct, mtime_s, num_time(st.st_mtime));
  slotset(strct, ctime_s, num_time(st.st_ctime));
#if HAVE_STAT_NSEC
  slotset(strct, atime_nsec_s, num(st.st_atim.tv_nsec));
  slotset(strct, mtime_nsec_s, num(st.st_mtim.tv_nsec));
  slotset(strct, ctime_nsec_s, num(st.st_ctim.tv_nsec));
#else
  slotset(strct, atime_nsec_s, zero);
  slotset(strct, mtime_nsec_s, zero);
  slotset(strct, ctime_nsec_s, zero);
#endif
  if (path)
    slotset(strct, path_s, path);

  return strct;
}
#endif

static val stat_impl(val obj, int (*statfn)(val, struct stat *),
                     val name, val path)
{
#if HAVE_SYS_STAT
  struct stat st;
  int res = statfn(obj, &st);

  if (res == -1) {
    int eno = errno;
    uw_throwf(errno_to_file_error(eno), lit("unable to ~a ~a: ~d/~s"),
              name, obj, num(eno), errno_to_str(eno), nao);
  }

  return if3(opt_compat && opt_compat <= 113,
             stat_to_list(st), stat_to_struct(st, path));
#else
  uw_throwf(file_error_s, lit("~a is not implemented"), name, nao);
#endif
}

val stat_wrap(val path)
{
  return stat_impl(path, do_stat, lit("stat"), path);
}

static val lstat_wrap(val path)
{
  return stat_impl(path, do_lstat, lit("lstat"), path);
}

#if HAVE_FILE_STAMP_CHANGE

#if HAVE_FUTIMENS
static long timens(val arg, val self)
{
  return if3(arg == t, UTIME_NOW, if3(arg, c_long(arg, self), UTIME_OMIT));
}
#endif

static val do_utimes(val target, val atime, val atimens,
                     val mtime, val mtimens,
                     val symlink_nofollow, val self)
{
  int res = -1;

  if (stringp(target)) {
    char *u8path = utf8_dup_to(c_str(target));
#if HAVE_FUTIMENS
    int flags = if3(symlink_nofollow, AT_SYMLINK_NOFOLLOW, 0);
    struct timespec times[2];
    times[0].tv_sec = c_time(atime, self);
    times[0].tv_nsec = timens(atimens, self);
    times[1].tv_sec = c_time(mtime, self);
    times[1].tv_nsec = timens(mtimens, self);
    res = utimensat(AT_FDCWD, u8path, times, flags);
#else
    errno = -EINVAL;
    if (integerp(atimens) || integerp(mtimens)) {
      struct timeval times[2];
      times[0].tv_sec = c_time(atime, self);
      times[0].tv_usec = c_long(trunc(atimens, num_fast(1000)), self);
      times[1].tv_sec = c_time(mtime, self);
      times[1].tv_usec = c_long(trunc(mtimens, num_fast(1000)), self);
      if (symlink_nofollow) {
#if HAVE_LUTIMES
        res = lutimes(u8path, times);
#endif
      } else {
#if HAVE_UTIMES
        res = utimes(u8path, times);
#elif HAVE_UTIME
        struct utimbuf utb;
        utb.actime = times[0].tv_sec;
        utb.modtime = times[1].tv_sec;
        res = utime(u8path, &utb);
#endif
      }
    }
#endif
    free(u8path);
  } else {
#if HAVE_FUTIMENS
    struct timespec times[2];
    int fd = get_fd(target, self);
    times[0].tv_sec = c_time(atime, self);
    times[0].tv_nsec = timens(atimens, self);
    times[1].tv_sec = c_time(mtime, self);
    times[1].tv_nsec = timens(mtimens, self);
    res = futimens(fd, times);
#elif HAVE_FUTIMES
    struct timeval times[2];
    int fd = get_fd(target, self);
    errno = -EINVAL;
    if (integerp(atimens) || integerp(mtimens)) {
      times[0].tv_sec = c_time(atime, self);
      times[0].tv_usec = c_long(trunc(atimens, num_fast(1000)), self);
      times[1].tv_sec = c_time(mtime, self);
      times[1].tv_usec = c_long(trunc(mtimens, num_fast(1000)), self);
      res = futimes(fd, times);
    }
#else
    errno = -EINVAL;
#endif
  }

  if (res == -1) {
    int eno = errno;
    uw_throwf(errno_to_file_error(eno), lit("~s: failed: ~d/~s"),
              self, num(eno), errno_to_str(eno), nao);
  }

  return t;
}

static val wrap_utimes(val target, val atime, val atimens,
                       val mtime, val mtimens)
{
  val self = lit("utimes");
  return do_utimes(target, atime, atimens, mtime, mtimens, nil, self);
}

static val wrap_lutimes(val target, val atime, val atimens,
                        val mtime, val mtimens)
{
  val self = lit("lutimes");
  return do_utimes(target, atime, atimens, mtime, mtimens, t, self);
}

#endif


#if HAVE_SYS_STAT

val umask_wrap(val mask)
{
  val self = lit("umask");

  if (missingp(mask)) {
    mode_t m = umask(0777);
    (void) umask(m);
    return num(m);
  }
  return num(umask(c_num(mask, self)));
}

#endif

#if HAVE_PIPE

static val pipe_wrap(void)
{
  int fd[2];
  if (pipe(fd) < 0) {
    int eno = errno;
    uw_throwf(errno_to_file_error(eno), lit("pipe failed: ~d/~s"),
              num(eno), errno_to_str(eno), nao);
  }
  return cons(num(fd[0]), num(fd[1]));
}

#endif

val getenv_wrap(val name)
{
  char *nameu8 = utf8_dup_to(c_str(name));
  char *lookup = getenv(nameu8);
  val result = lookup ? string_utf8(lookup) : nil;
  free(nameu8);
  return result;
}

static val setenv_wrap(val name, val value, val overwrite)
{
  const wchar_t *wname = c_str(name);
  const wchar_t *wvalu = value ? c_str(value) : 0;
  int ovw = default_arg_strict(overwrite, t) != nil;
  char *nameu8 = utf8_dup_to(wname);
  char *valu8 = wvalu ? utf8_dup_to(wvalu) : 0;
  if (valu8)
    setenv(nameu8, valu8, ovw);
  else if (ovw)
    unsetenv(nameu8);
  free(valu8);
  free(nameu8);
  return value;
}

static val unsetenv_wrap(val name)
{
  char *nameu8 = utf8_dup_to(c_str(name));
  unsetenv(nameu8);
  free(nameu8);
  return name;
}

#if HAVE_POLL

static val poll_wrap(val poll_list, val timeout_in)
{
  val self = lit("poll");
  nfds_t i, len = c_num(length(poll_list), self);
  val iter;
  struct pollfd *pfd = coerce(struct pollfd *, alloca(len * sizeof *pfd));
  val timeout = default_arg(timeout_in, negone);
  int res;

  for (i = 0, iter = poll_list; iter; iter = cdr(iter), i++) {
    cons_bind (obj, events, car(iter));

    pfd[i].events = c_num(events, self);

    switch (type(obj)) {
    case NUM:
      pfd[i].fd = c_num(obj, self);
      break;
    case COBJ:
      if (subtypep(obj->co.cls, stream_s)) {
        val fdval = stream_fd(obj);
        if (!fdval) {
          free(pfd);
          uw_throwf(file_error_s,
                    lit("poll: stream ~s doesn't have a file descriptor"),
                    obj, nao);
        }
        pfd[i].fd = c_num(fdval, self);
        break;
      }
      /* fallthrough */
    default:
      free(pfd);
      uw_throwf(file_error_s,
                lit("poll: ~s isn't a stream or file descriptor"),
                obj, nao);
      break;
    }
  }

  sig_save_enable;

  res = poll(pfd, len, c_num(timeout, self));

  sig_restore_enable;

  if (res < 0)
    uw_throwf(file_error_s, lit("poll failed: ~d/~s"),
              num(errno), errno_to_str(errno), nao);

  if (res == 0)
    return nil;

  {
    list_collect_decl (out, ptail);

    for (i = 0, iter = poll_list; iter; iter = cdr(iter), i++) {
      val pair = car(iter);
      if (pfd[i].revents)
        ptail = list_collect(ptail, cons(car(pair), num(pfd[i].revents)));
    }

    return out;
  }
}

#endif

#if HAVE_GETEUID

static val getuid_wrap(void)
{
  return num(getuid());
}

static val geteuid_wrap(void)
{
  return num(geteuid());
}

static val getgid_wrap(void)
{
  return num(getgid());
}

static val getegid_wrap(void)
{
  return num(getegid());
}

static val getgroups_wrap(void)
{
  val self = lit("getgroups");
  gid_t dummy[1];
  int needed = getgroups(0, dummy);

  if (needed == 0) {
    return nil;
  } else if (needed > 0) {
    gid_t *arr = coerce(gid_t *, chk_xalloc(needed, sizeof *arr, self));
    int obtained = getgroups(needed, arr);
    int i;
    list_collect_decl (out, ptail);

    if (obtained <= needed) {
      for (i = 0; i < obtained; i++)
        ptail = list_collect (ptail, num(arr[i]));

      free(arr);
      return out;
    }

    free(arr);
  }

  uw_throwf(system_error_s, lit("~s failed: ~d/~s"),
            self, num(errno), errno_to_str(errno), nao);
  abort();
}

static val setuid_wrap(val nval)
{
  val self = lit("setuid");
  if (setuid(c_num(nval, self)) == -1)
    uw_throwf(system_error_s, lit("setuid failed: ~d/~s"),
              num(errno), errno_to_str(errno), nao);
  return t;
}

static val seteuid_wrap(val nval)
{
  val self = lit("seteuid");
  if (seteuid(c_num(nval, self)) == -1)
    uw_throwf(system_error_s, lit("seteuid failed: ~d/~s"),
              num(errno), errno_to_str(errno), nao);
  return t;
}

static val setgid_wrap(val nval)
{
  val self = lit("setgid");
  if (setgid(c_num(nval, self)) == -1)
    uw_throwf(system_error_s, lit("setgid failed: ~d/~s"),
              num(errno), errno_to_str(errno), nao);
  return t;
}

static val setegid_wrap(val nval)
{
  val self = lit("setegid");
  if (setegid(c_num(nval, self)) == -1)
    uw_throwf(system_error_s, lit("setegid failed: ~d/~s"),
              num(errno), errno_to_str(errno), nao);
  return t;
}

#define RC_MAGIC 0xbe50c001

static uid_t orig_euid, real_uid;
static gid_t orig_egid, real_gid;
static unsigned int repress_called = 0, is_setuid = 1, is_setgid = 1;

void repress_privilege(void)
{
  real_gid = getgid();
  orig_egid = getegid();
  real_uid = getuid();
  orig_euid = geteuid();

  if (real_gid != orig_egid) {
    if (setegid(real_gid))
      panic("setegid failed when trying to repress privilege");
  } else {
    is_setgid = 0;
  }

  if (real_uid != orig_euid) {
    if (seteuid(real_uid))
      panic("setegid failed when trying to repress privilege");
  } else {
    is_setuid = 0;
  }

  repress_called = RC_MAGIC;
}

void drop_privilege(void)
{
  if (repress_called != RC_MAGIC)
    panic("bug in setuid logic: repress_privilege not called");

  if (!is_setuid && !is_setgid)
    return;

#if HAVE_SETRESUID
  {
    if (is_setgid && setresgid(real_gid, real_gid, real_gid) != 0)
      panic("setresgid failed when trying to drop privilege");
    if (is_setuid && setresuid(real_uid, real_uid, real_uid) != 0)
      panic("setresuid failed when trying to drop privilege");
    return;
  }
#else
  {
    /* First, try to regain as much privilege as possible.
     * On some platforms, setuid requires the caller to be effective
     * root in order to change the saved user ID. We don't want this
     * call to fail just because we weren't effective root, even though
     * we have the privilege to be effective root!
     */
    (void) setuid(0);

    if (is_setgid) {
      /* If we are setgid, and cannot set effective gid to real,
       * then abort.
       */
      if (setgid(real_gid) != 0)
        panic("dropping to real group with setgid failed");
    }

    if (is_setuid) {
      if (setuid(real_uid) != 0)
        panic("dropping to to real user id with setuid failed");
      /* If we can re-gain previous effective IDs, then setuid(getuid())
       * didn't actually work; it didn't set the saved ID. We assume
       * that setuid(getuid()) does work for effective root; i.e. only
       * setuid non-root has this problem. And of course, if the
       * real UID is root, we are not "dropping" privileges.
       */
      if (orig_euid != 0 && real_uid != 0) {
        if (seteuid(orig_euid) == 0)
          panic("privilege drop failed: still can regain setuid");
      }
    }

    /* If we can regain setgid privileges, abort */
    if (is_setgid && real_uid != 0 && setegid(orig_egid) == 0)
      panic("privilege drop failed: still can regain setgid");
  }
#endif
}

void simulate_setuid_setgid(val open_script)
{
  val self = lit("txr");

  if (repress_called != RC_MAGIC || (is_setuid && seteuid(orig_euid) != 0))
    abort();

  if (!is_setuid && orig_euid != 0)
    return;

  {
    val fdv = stream_fd(open_script);

    if (fdv) {
      struct stat stb;
      cnum fd = c_num(fdv, self);

      if (fstat(fd, &stb) != 0)
        goto drop;

      if ((stb.st_mode & (S_ISGID | S_IXGRP)) == (S_ISGID | S_IXGRP)) {
        if (setegid(stb.st_gid) == 0)
          is_setgid = 0; /* do not drop effective gid in drop_privilege. */
      }

      if ((stb.st_mode & (S_ISUID | S_IXUSR)) == (S_ISUID | S_IXUSR)) {
        if (seteuid(stb.st_uid) == 0)
          is_setuid = 0; /* do not drop effective uid in drop_privilege. */
      }
    }
  }

drop:
  drop_privilege();
}

#endif

#if HAVE_SETGROUPS

static val setgroups_wrap(val list)
{
  val self = lit("setgroups");
  ucnum len = c_num(length(list), self);

  if (convert(ucnum, convert(size_t, len)) != len) {
    uw_throwf(system_error_s, lit("~a: list too long"), self, nao);
  } else {
    gid_t *arr = coerce(gid_t *, chk_xalloc(len, sizeof *arr, self));
    int i = 0, res;

    for (; list; i++, list = cdr(list)) {
      cnum gid = c_num(car(list), self);
      arr[i] = gid;
    }

    res = setgroups(len, arr);

    free(arr);

    if (res != 0)
      uw_throwf(system_error_s, lit("setgroups failed: ~d/~s"),
                num(errno), errno_to_str(errno), nao);

    return t;
  }
}

#endif

#if HAVE_SETRESUID

static val getresuid_wrap(void)
{
  uid_t r, e, s;
  if (getresuid(&r, &e, &s) != 0)
    uw_throwf(system_error_s, lit("getresuid failed: ~d/~s"),
              num(errno), errno_to_str(errno), nao);
  return list(num(r), num(e), num(s), nao);
}

static val getresgid_wrap(void)
{
  gid_t r, e, s;
  if (getresgid(&r, &e, &s) != 0)
    uw_throwf(system_error_s, lit("getresgid failed: ~d/~s"),
              num(errno), errno_to_str(errno), nao);
  return list(num(r), num(e), num(s), nao);
}

static val setresuid_wrap(val r, val e, val s)
{
  val self = lit("setresuid");
  if (setresuid(c_num(r, self), c_num(e, self), c_num(s, self)) != 0)
    uw_throwf(system_error_s, lit("setresuid failed: ~d/~s"),
              num(errno), errno_to_str(errno), nao);
  return t;
}

static val setresgid_wrap(val r, val e, val s)
{
  val self = lit("setresgid");
  if (setresuid(c_num(r, self), c_num(e, self), c_num(s, self)) != 0)
    uw_throwf(system_error_s, lit("setresuid failed: ~d/~s"),
              num(errno), errno_to_str(errno), nao);
  return t;
}

#endif

#if HAVE_PWUID

static val setpwent_wrap(void)
{
  setpwent();
  return nil;
}

static val endpwent_wrap(void)
{
  endpwent();
  return nil;
}

static void fill_passwd(val to, struct passwd *from)
{
  slotset(to, name_s, string_utf8(from->pw_name));
  slotset(to, passwd_s, string_utf8(from->pw_passwd));
  slotset(to, uid_s, num(from->pw_uid));
  slotset(to, gid_s, num(from->pw_gid));
  slotset(to, gecos_s, string_utf8(from->pw_gecos));
  slotset(to, dir_s, string_utf8(from->pw_dir));
  slotset(to, shell_s, string_utf8(from->pw_shell));
}

static val make_pwstruct(struct passwd *p)
{
  args_decl(args, ARGS_MIN);
  val out = make_struct(passwd_s, nil, args);
  fill_passwd(out, p);
  return out;
}

#endif

#if HAVE_PWUID_R

static val getpwent_wrap(void)
{
  char buf[1024];
  struct passwd pw, *p;
  int res = getpwent_r(&pw, buf, sizeof buf, &p);

  return (res == 0 && p != 0) ? make_pwstruct(&pw) : nil;
}

static val getpwuid_wrap(val uid)
{
  val self = lit("getpwuid");
  char buf[1024];
  struct passwd pw, *p;
  int res = getpwuid_r(c_num(uid, self), &pw, buf, sizeof buf, &p);

  return (res == 0 && p != 0) ? make_pwstruct(&pw) : nil;
}

static val getpwnam_wrap(val wname)
{
  char buf[1024];
  struct passwd pw, *p;
  char *name = utf8_dup_to(c_str(wname));
  int res = getpwnam_r(name, &pw, buf, sizeof buf, &p);

  free(name);
  return (res == 0 && p != 0) ? make_pwstruct(&pw) : nil;
}

#elif HAVE_PWUID

static val getpwent_wrap(void)
{
  struct passwd *p = getpwent();
  return (p != 0) ? make_pwstruct(p) : nil;
}

static val getpwuid_wrap(val uid)
{
  struct passwd *p = getpwuid(c_num(uid, self));
  return (p != 0) ? make_pwstruct(p) : nil;
}

static val getpwnam_wrap(val wname)
{
  char *name = utf8_dup_to(c_str(wname));
  struct passwd *p = getpwnam(name);
  free(name);
  return (p != 0) ? make_pwstruct(p) : nil;
}

#endif

#if HAVE_GRGID

static val setgrent_wrap(void)
{
  setgrent();
  return nil;
}

static val endgrent_wrap(void)
{
  endgrent();
  return nil;
}

static void fill_group(val to, struct group *from)
{
  list_collect_decl (out, ptail);
  int i;

  slotset(to, name_s, string_utf8(from->gr_name));
  slotset(to, passwd_s, string_utf8(from->gr_passwd));
  slotset(to, gid_s, num(from->gr_gid));

  for (i = 0; from->gr_mem[i] != 0; i++)
    ptail = list_collect(ptail, string_utf8(from->gr_mem[i]));

  slotset(to, mem_s, out);
}

static val make_grstruct(struct group *g)
{
  args_decl(args, ARGS_MIN);
  val out = make_struct(group_s, nil, args);
  fill_group(out, g);
  return out;
}


static val getgrent_wrap(void)
{
  struct group *g = getgrent();
  return (g != 0) ? make_grstruct(g) : nil;
}


#endif

#if HAVE_GRGID_R

static val getgrgid_wrap(val uid)
{
  val self = lit("getgrgid");
  char buf[1024];
  struct group gr, *g;
  int res = getgrgid_r(c_num(uid, self), &gr, buf, sizeof buf, &g);

  return (res == 0 && g != 0) ? make_grstruct(&gr) : nil;
}

static val getgrnam_wrap(val wname)
{
  char buf[1024];
  struct group gr, *g;
  char *name = utf8_dup_to(c_str(wname));
  int res = getgrnam_r(name, &gr, buf, sizeof buf, &g);

  free(name);
  return (res == 0 && g != 0) ? make_grstruct(&gr) : nil;
}

#elif HAVE_GRGID

static val getgrgid_wrap(val uid)
{
  struct group *g = getgrgid(c_num(uid, self));
  return (g != 0) ? make_grstruct(g) : nil;
}

static val getgrnam_wrap(val wname)
{
  char *name = utf8_dup_to(c_str(wname));
  struct group *g = getgrnam(name);
  free(name);
  return (g != 0) ? make_grstruct(g) : nil;
}

#endif

#if HAVE_CRYPT || HAVE_CRYPT_R

static int salt_char_p(wchar_t ch)
{
  return ((ch >= 'a' && ch <= 'z') ||
          (ch >= 'A' && ch <= 'Z') ||
          (ch >= '0' && ch <= '9') ||
          (ch == '.') || (ch == '/'));
}

static const wchar_t *validate_salt(const wchar_t *salt)
{
  const wchar_t *s = salt;

  if (salt_char_p(*s)) {
    if (salt_char_p(*++s))
      return salt;
    else
      goto badsalt;
  }

  if (*s++ != '$')
    goto badsalt;

  switch (*s++) {
  case '1': case '5': case '6':
    break;
  case '2':
    if (*s >= 'a' && *s++ <= 'z')
      break;
    /* fallthrough */
  default:
    goto badsalt;
  }

  if (*s++ != '$')
    goto badsalt;

  if (wcsncmp(s, L"rounds=", 7) == 0) {
    size_t ispn = wcsspn(s += 7, L"0123456789");
    s += ispn;
    if (*s++ != '$')
      goto badsalt;
  }

  while (salt_char_p(*s))
    s++;

  if (*s && *s != '$')
    goto badsalt;

  return salt;

badsalt:
  errno = EINVAL;
  return 0;
}

static val crypt_wrap(val wkey, val wsalt)
{
  const wchar_t *cwkey = c_str(wkey);
  const wchar_t *cwsalt = validate_salt(c_str(wsalt));

  if (cwsalt != 0) {
    char *key = utf8_dup_to(cwkey);
    char *salt = utf8_dup_to(cwsalt);
#if HAVE_CRYPT_R
    struct crypt_data cd;
    char *hash = (cd.initialized = 0, crypt_r(key, salt, &cd));
#else
    char *hash = crypt(key, salt);
#endif
    free(key);
    free(salt);
    if (hash != 0)
      return string_utf8(hash);
  }

  uw_throwf(error_s, lit("crypt failed: ~d/~s"), num(errno),
            errno_to_str(errno), nao);
}

#endif

#if HAVE_FSEEKO

static off_t off_t_num(val num, val self)
{
  switch (CHAR_BIT * sizeof(off_t)) {
  case 32:
    return c_i32(num, self);
  case 64:
    return c_i64(num, self);
  default:
    internal_error("portme: unsupported off_t size");
  }
}

static val num_off_t(off_t off)
{
  if (sizeof (off_t) <= sizeof (cnum)) {
    return num(off);
  } else if (NUM_MIN <= off && off <= NUM_MAX) {
    return num(off);
  } else if (sizeof (off_t) <= sizeof (i64_t)) {
    return num_64(off);
  } else {
    internal_error("portme: unsupported off_t size");
  }
}

#endif

val stdio_ftell(FILE *f)
{
#if HAVE_FSEEKO
  return num_off_t(ftello(f));
#else
  return num_off_t(ftell(f));
#endif
}

int stdio_fseek(FILE *f, val off, int whence)
{
  val self = lit("seek-stream");
#if HAVE_FSEEKO
  return fseeko(f, off_t_num(off, self), whence) == 0;
#else
  return fseek(f, c_long(off, self), whence) == 0;
#endif
}

#if HAVE_FNMATCH
static val fnmatch_wrap(val pattern, val string, val flags)
{
  val self = lit("fnmatch");
  const wchar_t *pattern_ws = c_str(pattern);
  const wchar_t *string_ws = c_str(string);
  cnum c_flags = c_num(default_arg(flags, zero), self);
  char *pattern_u8 = utf8_dup_to(pattern_ws);
  char *string_u8 = utf8_dup_to(string_ws);
  int res = fnmatch(pattern_u8, string_u8, c_flags);
  free(string_u8);
  free(pattern_u8);
  switch (res) {
  case 0:
    return t;
  case FNM_NOMATCH:
    return nil;
  default:
    uw_throwf(error_s, lit("fnmatch: error ~a"), num(res), nao);
  }
}
#endif

#if HAVE_UNAME
static val uname_wrap(void)
{
  args_decl(args, ARGS_MIN);
  struct utsname un;
  int res;
  if ((res = uname(&un)) >= 0) {
    val out = make_struct(utsname_s, nil, args);
    slotset(out, sysname_s, string_utf8(un.sysname));
    slotset(out, nodename_s, string_utf8(un.nodename));
    slotset(out, release_s, string_utf8(un.release));
    slotset(out, version_s, string_utf8(un.version));
    slotset(out, machine_s, string_utf8(un.machine));
#if HAVE_UTSNAME_DOMAINNAME
    slotset(out, domainname_s, string_utf8(un.domainname));
#endif
    return out;
  }
  uw_throwf(error_s, lit("uname failed: ~d/~s"), num(errno),
            errno_to_str(errno), nao);
}
#endif

#if HAVE_DLOPEN

static void cptr_dl_destroy_op(val obj)
{
  if (obj->co.handle != 0) {
    (void) dlclose(obj->co.handle);
    obj->co.handle = 0;
  }
}

static struct cobj_ops cptr_dl_ops = cobj_ops_init(cobj_equal_handle_op,
                                                   cptr_print_op,
                                                   cptr_dl_destroy_op,
                                                   cobj_mark_op,
                                                   cobj_handle_hash_op);

static val dlopen_wrap(val name, val flags)
{
  val self = lit("dlopen");
  const wchar_t *name_ws = if3(null_or_missing_p(name),
                               0, c_str(name));
  char *name_u8 = if3(name_ws != 0, utf8_dup_to(name_ws), 0);
  cnum f = if3(missingp(flags), RTLD_LAZY, c_num(flags, self));
  mem_t *ptr = coerce(mem_t *, (dlerror(), dlopen(name_u8, f)));
  free(name_u8);
  if (ptr == 0) {
    char *err = dlerror();
    if (err)
      uw_throwf(error_s, lit("dlopen failed: ~a"), string_utf8(err), nao);
    else
      uw_throwf(error_s, lit("dlopen failed on ~a"), name, nao);
  }
  return cptr_typed(ptr, dlhandle_s, &cptr_dl_ops);
}

static val dlclose_wrap(val cptr)
{
  val self = lit("dlclose");
  mem_t *ptr = cptr_handle(cptr, dlhandle_s, self);
  if (cptr->co.ops != &cptr_dl_ops)
    uw_throwf(error_s, lit("~a: object ~s isn't a handle from dlopen"),
              self, cptr, nao);
  if (ptr != 0) {
    int res = dlclose(ptr);
    cptr->co.handle = 0;
    return tnil(res == 0);
  }
  return nil;
}

static val dlsym_wrap(val dlptr, val name)
{
  val self = lit("dlsym");
  const wchar_t *name_ws = c_str(name);
  char *name_u8 = utf8_dup_to(name_ws);
  mem_t *dl = cptr_handle(dlptr, dlhandle_s, self);
  mem_t *sym = coerce(mem_t *, dlsym(dl, name_u8));
  free(name_u8);
  return cptr_typed(sym, dlsym_s, 0);
}

static void dlsym_error(val dlptr, val name, val self)
{
  char *err = dlerror();
  if (err)
    uw_throwf(error_s, lit("~a: ~a"), self, string_utf8(err), nao);
  else
    uw_throwf(error_s, lit("~a: ~a not found in ~s"),
              self, name, dlptr, nao);
}

static val dlsym_checked(val dlptr, val name)
{
  val self = lit("dlsym-checked");
  val ptr = (dlerror(), dlsym_wrap(dlptr, name));
  if (cptr_handle(ptr, dlsym_s, self) == 0)
    dlsym_error(dlptr, name, self);
  return ptr;
}

#if HAVE_DLVSYM
static val dlvsym_wrap(val dlptr, val name, val ver)
{
  val self = lit("dlvsym");

  if (null_or_missing_p(ver)) {
    return dlsym_wrap(dlptr, name);
  } else {
    const wchar_t *name_ws = c_str(name);
    const wchar_t *ver_ws = c_str(ver);
    char *name_u8 = utf8_dup_to(name_ws);
    char *ver_u8 = utf8_dup_to(ver_ws);
    mem_t *dl = cptr_handle(dlptr, dlhandle_s, self);
    mem_t *sym = coerce(mem_t *, dlvsym(dl, name_u8, ver_u8));
    free(name_u8);
    free(ver_u8);
    return cptr(sym);
  }
}

static val dlvsym_checked(val dlptr, val name, val ver)
{
  val self = lit("dlvsym-checked");
  val ptr = (dlerror(), dlvsym_wrap(dlptr, name, ver));
  if (cptr_handle(ptr, dlsym_s, self) == 0)
    dlsym_error(dlptr, name, self);
  return ptr;
}
#endif

#endif

#if HAVE_REALPATH
static val realpath_wrap(val path)
{
  const wchar_t *path_ws = c_str(path);
  char *path_u8 = utf8_dup_to(path_ws);
  char *rp_u8 = realpath(path_u8, 0);
  val rp = if2(rp_u8, string_utf8(rp_u8));
  free(rp_u8);
  free(path_u8);
  return rp;
}
#endif

#if HAVE_ISATTY
static val isatty_wrap(val spec)
{
  val fdval;
  val self = lit("isatty");

  if (streamp(spec))
    fdval = stream_get_prop(spec, fd_k);
  else
    fdval = spec;

  if (fdval) {
    int fd = c_int(fdval, self);
    return if2(fd && isatty(fd) > 0, t);
  }

  return nil;
}
#endif

struct dir {
  DIR *dir;
  val path;
};

static void opendir_free(val obj)
{
  struct dir *d = coerce(struct dir *, obj->co.handle);
  if (d->dir != 0) {
    closedir(d->dir);
    d->dir = 0;
  }
  free(d);
}

static void opendir_mark(val obj)
{
  struct dir *d = coerce(struct dir *, obj->co.handle);
  gc_mark(d->path);
}

static struct cobj_ops opendir_ops = cobj_ops_init(eq,
                                                   cobj_print_op,
                                                   opendir_free,
                                                   opendir_mark,
                                                   cobj_eq_hash_op);
static val opendir_wrap(val path, val prefix_p)
{
  DIR *dir = w_opendir(c_str(path));

  if (dir == 0) {
    uw_throwf(system_error_s, lit("opendir failed for ~a: ~d/~s"),
              path, num(errno), errno_to_str(errno), nao);
  } else {
    struct dir *d = coerce(struct dir *, chk_malloc(sizeof *d));
    val obj = cobj(coerce(mem_t *, d), dir_s, &opendir_ops);
    d->dir = dir;
    d->path = if2(default_null_arg(prefix_p), path);
    return obj;
  }
}

static val closedir_wrap(val dirobj)
{
  val self = lit("closedir");
  struct dir *d = coerce(struct dir *, cobj_handle(self, dirobj, dir_s));

  if (d->dir != 0) {
    closedir(d->dir);
    d->dir = 0;
    return t;
  }

  return nil;
}

static val readdir_wrap(val dirobj, val dirent_in)
{
  val self = lit("readdir");
  struct dir *d = coerce(struct dir *, cobj_handle(self, dirobj, dir_s));
  struct dirent *dent = if3(d->dir != 0, readdir(d->dir), 0);

  for (;;) {
    if (dent == 0) {
      closedir_wrap(dirobj);
      return nil;
    } else if (!strcmp(dent->d_name, ".") || !strcmp(dent->d_name, "..")) {
      dent = readdir(d->dir);
      continue;
    } else {
      args_decl(args, ARGS_MIN);
      val dirent = default_arg(dirent_in, make_struct(dirent_st, nil, args));
      slotset(dirent, name_s,
              if3(d->path,
                  path_cat(d->path, string_utf8(dent->d_name)),
                  string_utf8(dent->d_name)));
      slotset(dirent, ino_s, num(dent->d_ino));
#ifdef _DIRENT_HAVE_D_TYPE
      slotset(dirent, type_s, num(dent->d_type));
#else
      if (dirent_in == dirent)
        slotset(dirent, type_s, nil);
#endif
      return dirent;
    }
  }
}

void sysif_init(void)
{
  prot1(&at_exit_list);
  prot1(&dirent_st);

  atexit(at_exit_handler);

  stat_s = intern(lit("stat"), user_package);
  dev_k = intern(lit("dev"), keyword_package);
  ino_k = intern(lit("ino"), keyword_package);
  mode_k = intern(lit("mode"), keyword_package);
  nlink_k = intern(lit("nlink"), keyword_package);
  uid_k = intern(lit("uid"), keyword_package);
  gid_k = intern(lit("gid"), keyword_package);
  rdev_k = intern(lit("rdev"), keyword_package);
  size_k = intern(lit("size"), keyword_package);
  blksize_k = intern(lit("blksize"), keyword_package);
  blocks_k = intern(lit("blocks"), keyword_package);
  atime_k = intern(lit("atime"), keyword_package);
  mtime_k = intern(lit("mtime"), keyword_package);
  ctime_k = intern(lit("ctime"), keyword_package);
  dev_s = intern(lit("dev"), user_package);
  ino_s = intern(lit("ino"), user_package);
  mode_s = intern(lit("mode"), user_package);
  nlink_s = intern(lit("nlink"), user_package);
  uid_s = intern(lit("uid"), user_package);
  gid_s = intern(lit("gid"), user_package);
  rdev_s = intern(lit("rdev"), user_package);
  size_s = intern(lit("size"), user_package);
  blksize_s = intern(lit("blksize"), user_package);
  blocks_s = intern(lit("blocks"), user_package);
  atime_s = intern(lit("atime"), user_package);
  mtime_s = intern(lit("mtime"), user_package);
  ctime_s = intern(lit("ctime"), user_package);
  atime_nsec_s = intern(lit("atime-nsec"), user_package);
  mtime_nsec_s = intern(lit("mtime-nsec"), user_package);
  ctime_nsec_s = intern(lit("ctime-nsec"), user_package);
  path_s = intern(lit("path"), user_package);
  dir_s = intern(lit("dir"), user_package);
  dirent_s = intern(lit("dirent"), user_package);
#if HAVE_PWUID
  passwd_s = intern(lit("passwd"), user_package);
  gecos_s = intern(lit("gecos"), user_package);
  shell_s = intern(lit("shell"), user_package);
#endif
#if HAVE_GRGID
  group_s = intern(lit("group"), user_package);
  mem_s = intern(lit("mem"), user_package);
#endif
#if HAVE_UNAME
  utsname_s = intern(lit("utsname"), user_package);
  sysname_s = intern(lit("sysname"), user_package);
  nodename_s = intern(lit("nodename"), user_package);
  release_s = intern(lit("release"), user_package);
  version_s = intern(lit("version"), user_package);
  machine_s = intern(lit("machine"), user_package);
  domainname_s = intern(lit("domainname"), user_package);
#endif
#if HAVE_FCNTL
  flock_s = intern(lit("flock"), user_package);
  type_s = intern(lit("type"), user_package);
  whence_s = intern(lit("whence"), user_package);
  start_s = intern(lit("start"), user_package);
  len_s = intern(lit("len"), user_package);
  pid_s = intern(lit("pid"), user_package);
#endif

  make_struct_type(stat_s, nil, nil,
                   list(dev_s, ino_s, mode_s, nlink_s, uid_s, gid_s,
                        rdev_s, size_s, blksize_s, blocks_s,
                        atime_s, atime_nsec_s, mtime_s, mtime_nsec_s,
                        ctime_s, ctime_nsec_s, path_s, nao),
                   nil, nil, nil, nil);
#if HAVE_PWUID
  make_struct_type(passwd_s, nil, nil,
                   list(name_s, passwd_s, uid_s, gid_s,
                        gecos_s, dir_s, shell_s, nao), nil, nil, nil, nil);
#endif
#if HAVE_GRGID
  make_struct_type(group_s, nil, nil,
                   list(name_s, passwd_s, gid_s, mem_s, nao),
                   nil, nil, nil, nil);
#endif
#if HAVE_UNAME
  make_struct_type(utsname_s, nil, nil,
                   list(sysname_s, nodename_s, release_s,
                        version_s, machine_s, domainname_s, nao),
                   nil, nil, nil, nil);
#endif
#if HAVE_FCNTL
  make_struct_type(flock_s, nil, nil,
                   list(type_s, whence_s, start_s, len_s, pid_s, nao),
                   nil, nil, nil, nil);
#endif

  reg_varl(intern(lit("e2big"), user_package), num_fast(E2BIG));
  reg_varl(intern(lit("eacces"), user_package), num_fast(EACCES));
  reg_varl(intern(lit("eaddrinuse"), user_package), num_fast(EADDRINUSE));
  reg_varl(intern(lit("eaddrnotavail"), user_package), num_fast(EADDRNOTAVAIL));
  reg_varl(intern(lit("eafnosupport"), user_package), num_fast(EAFNOSUPPORT));
  reg_varl(intern(lit("eagain"), user_package), num_fast(EAGAIN));
  reg_varl(intern(lit("ealready"), user_package), num_fast(EALREADY));
  reg_varl(intern(lit("ebadf"), user_package), num_fast(EBADF));
  reg_varl(intern(lit("ebadmsg"), user_package), num_fast(EBADMSG));
  reg_varl(intern(lit("ebusy"), user_package), num_fast(EBUSY));
  reg_varl(intern(lit("ecanceled"), user_package), num_fast(ECANCELED));
  reg_varl(intern(lit("echild"), user_package), num_fast(ECHILD));
  reg_varl(intern(lit("econnaborted"), user_package), num_fast(ECONNABORTED));
  reg_varl(intern(lit("econnrefused"), user_package), num_fast(ECONNREFUSED));
  reg_varl(intern(lit("econnreset"), user_package), num_fast(ECONNRESET));
  reg_varl(intern(lit("edeadlk"), user_package), num_fast(EDEADLK));
  reg_varl(intern(lit("edestaddrreq"), user_package), num_fast(EDESTADDRREQ));
  reg_varl(intern(lit("edom"), user_package), num_fast(EDOM));
  reg_varl(intern(lit("edquot"), user_package), num_fast(EDQUOT));
  reg_varl(intern(lit("eexist"), user_package), num_fast(EEXIST));
  reg_varl(intern(lit("efault"), user_package), num_fast(EFAULT));
  reg_varl(intern(lit("efbig"), user_package), num_fast(EFBIG));
  reg_varl(intern(lit("ehostunreach"), user_package), num_fast(EHOSTUNREACH));
  reg_varl(intern(lit("eidrm"), user_package), num_fast(EIDRM));
  reg_varl(intern(lit("eilseq"), user_package), num_fast(EILSEQ));
  reg_varl(intern(lit("einprogress"), user_package), num_fast(EINPROGRESS));
  reg_varl(intern(lit("eintr"), user_package), num_fast(EINTR));
  reg_varl(intern(lit("einval"), user_package), num_fast(EINVAL));
  reg_varl(intern(lit("eio"), user_package), num_fast(EIO));
  reg_varl(intern(lit("eisconn"), user_package), num_fast(EISCONN));
  reg_varl(intern(lit("eisdir"), user_package), num_fast(EISDIR));
  reg_varl(intern(lit("eloop"), user_package), num_fast(ELOOP));
  reg_varl(intern(lit("emfile"), user_package), num_fast(EMFILE));
  reg_varl(intern(lit("emlink"), user_package), num_fast(EMLINK));
  reg_varl(intern(lit("emsgsize"), user_package), num_fast(EMSGSIZE));
  reg_varl(intern(lit("emultihop"), user_package), num_fast(EMULTIHOP));
  reg_varl(intern(lit("enametoolong"), user_package), num_fast(ENAMETOOLONG));
  reg_varl(intern(lit("enetdown"), user_package), num_fast(ENETDOWN));
  reg_varl(intern(lit("enetreset"), user_package), num_fast(ENETRESET));
  reg_varl(intern(lit("enetunreach"), user_package), num_fast(ENETUNREACH));
  reg_varl(intern(lit("enfile"), user_package), num_fast(ENFILE));
  reg_varl(intern(lit("enobufs"), user_package), num_fast(ENOBUFS));
  reg_varl(intern(lit("enodata"), user_package), num_fast(ENODATA));
  reg_varl(intern(lit("enodev"), user_package), num_fast(ENODEV));
  reg_varl(intern(lit("enoent"), user_package), num_fast(ENOENT));
  reg_varl(intern(lit("enoexec"), user_package), num_fast(ENOEXEC));
  reg_varl(intern(lit("enolck"), user_package), num_fast(ENOLCK));
  reg_varl(intern(lit("enolink"), user_package), num_fast(ENOLINK));
  reg_varl(intern(lit("enomem"), user_package), num_fast(ENOMEM));
  reg_varl(intern(lit("enomsg"), user_package), num_fast(ENOMSG));
  reg_varl(intern(lit("enoprotoopt"), user_package), num_fast(ENOPROTOOPT));
  reg_varl(intern(lit("enospc"), user_package), num_fast(ENOSPC));
  reg_varl(intern(lit("enosr"), user_package), num_fast(ENOSR));
  reg_varl(intern(lit("enostr"), user_package), num_fast(ENOSTR));
  reg_varl(intern(lit("enosys"), user_package), num_fast(ENOSYS));
  reg_varl(intern(lit("enotconn"), user_package), num_fast(ENOTCONN));
  reg_varl(intern(lit("enotdir"), user_package), num_fast(ENOTDIR));
  reg_varl(intern(lit("enotempty"), user_package), num_fast(ENOTEMPTY));
#ifdef ENOTRECOVERABLE
  reg_varl(intern(lit("enotrecoverable"), user_package), num_fast(ENOTRECOVERABLE));
#endif
  reg_varl(intern(lit("enotsock"), user_package), num_fast(ENOTSOCK));
  reg_varl(intern(lit("enotsup"), user_package), num_fast(ENOTSUP));
  reg_varl(intern(lit("enotty"), user_package), num_fast(ENOTTY));
  reg_varl(intern(lit("enxio"), user_package), num_fast(ENXIO));
  reg_varl(intern(lit("eopnotsupp"), user_package), num_fast(EOPNOTSUPP));
  reg_varl(intern(lit("eoverflow"), user_package), num_fast(EOVERFLOW));
#ifdef EOWNERDEAD
  reg_varl(intern(lit("eownerdead"), user_package), num_fast(EOWNERDEAD));
#endif
  reg_varl(intern(lit("eperm"), user_package), num_fast(EPERM));
  reg_varl(intern(lit("epipe"), user_package), num_fast(EPIPE));
  reg_varl(intern(lit("eproto"), user_package), num_fast(EPROTO));
  reg_varl(intern(lit("eprotonosupport"), user_package), num_fast(EPROTONOSUPPORT));
  reg_varl(intern(lit("eprototype"), user_package), num_fast(EPROTOTYPE));
  reg_varl(intern(lit("erange"), user_package), num_fast(ERANGE));
  reg_varl(intern(lit("erofs"), user_package), num_fast(EROFS));
  reg_varl(intern(lit("espipe"), user_package), num_fast(ESPIPE));
  reg_varl(intern(lit("esrch"), user_package), num_fast(ESRCH));
  reg_varl(intern(lit("estale"), user_package), num_fast(ESTALE));
  reg_varl(intern(lit("etime"), user_package), num_fast(ETIME));
  reg_varl(intern(lit("etimedout"), user_package), num_fast(ETIMEDOUT));
  reg_varl(intern(lit("etxtbsy"), user_package), num_fast(ETXTBSY));
  reg_varl(intern(lit("ewouldblock"), user_package), num_fast(EWOULDBLOCK));
  reg_varl(intern(lit("exdev"), user_package), num_fast(EXDEV));

  reg_fun(intern(lit("errno"), user_package), func_n1o(errno_wrap, 0));
  reg_fun(intern(lit("strerror"), user_package), func_n1o(strerror_wrap, 0));
#if HAVE_STRSIGNAL
  reg_fun(intern(lit("strsignal"), user_package), func_n1(strsignal_wrap));
#endif
  reg_fun(intern(lit("exit"), user_package), func_n1o(exit_wrap, 0));
  reg_fun(intern(lit("at-exit-call"), user_package), func_n1(at_exit_call));
  reg_fun(intern(lit("at-exit-do-not-call"), user_package), func_n1(at_exit_do_not_call));
  reg_fun(intern(lit("abort"), user_package), func_n0(abort_wrap));
  reg_fun(intern(lit("usleep"), user_package), func_n1(usleep_wrap));
#if HAVE_UNISTD_H
  reg_fun(intern(lit("getpid"), user_package), func_n0(getpid_wrap));
#if HAVE_GETPPID
  reg_fun(intern(lit("getppid"), user_package), func_n0(getppid_wrap));
#endif
#endif

  reg_fun(intern(lit("env"), user_package), func_n0(env));
  reg_fun(intern(lit("env-hash"), user_package), func_n0(env_hash));

#if HAVE_DAEMON
  reg_fun(intern(lit("daemon"), user_package), func_n2(daemon_wrap));
#endif

#if HAVE_MKDIR || HAVE_WINDOWS_H
  reg_fun(intern(lit("mkdir"), user_package), func_n2o(mkdir_wrap, 1));
  reg_fun(intern(lit("ensure-dir"), user_package), func_n2o(ensure_dir, 1));
#endif

#if HAVE_UNISTD_H
  reg_fun(intern(lit("chdir"), user_package), func_n1(chdir_wrap));
  reg_fun(intern(lit("pwd"), user_package), func_n0(getcwd_wrap));
  reg_fun(intern(lit("rmdir"), user_package), func_n1(rmdir_wrap));
#endif

#if HAVE_MAKEDEV
  reg_fun(intern(lit("makedev"), user_package), func_n2(makedev_wrap));
  reg_fun(intern(lit("minor"), user_package), func_n1(minor_wrap));
  reg_fun(intern(lit("major"), user_package), func_n1(major_wrap));
#endif

#if HAVE_MKNOD
  reg_fun(intern(lit("mknod"), user_package), func_n3o(mknod_wrap, 2));
#endif

#if HAVE_MKFIFO
  reg_fun(intern(lit("mkfifo"), user_package), func_n2(mkfifo_wrap));
#endif

#if HAVE_CHMOD
  reg_fun(intern(lit("chmod"), user_package), func_n2(chmod_wrap));
#endif

#if HAVE_CHOWN
  reg_fun(intern(lit("chown"), user_package), func_n3(chown_wrap));
  reg_fun(intern(lit("lchown"), user_package), func_n3(lchown_wrap));
#endif

#if HAVE_SYMLINK
  reg_fun(intern(lit("symlink"), user_package), func_n2(symlink_wrap));
  reg_fun(intern(lit("link"), user_package), func_n2(link_wrap));
  reg_fun(intern(lit("readlink"), user_package), func_n1(readlink_wrap));
#endif

#if HAVE_FCNTL
  reg_varl(intern(lit("o-accmode"), user_package), num_fast(O_ACCMODE));
  reg_varl(intern(lit("o-rdonly"), user_package), num_fast(O_RDONLY));
  reg_varl(intern(lit("o-wronly"), user_package), num_fast(O_WRONLY));
  reg_varl(intern(lit("o-rdwr"), user_package), num_fast(O_RDWR));
  reg_varl(intern(lit("o-creat"), user_package), num_fast(O_CREAT));
  reg_varl(intern(lit("o-noctty"), user_package), num_fast(O_NOCTTY));
  reg_varl(intern(lit("o-trunc"), user_package), num_fast(O_TRUNC));
  reg_varl(intern(lit("o-append"), user_package), num_fast(O_APPEND));
  reg_varl(intern(lit("o-nonblock"), user_package), num_fast(O_NONBLOCK));
  reg_varl(intern(lit("o-sync"), user_package), num_fast(O_SYNC));
#ifdef O_ASYNC
  reg_varl(intern(lit("o-async"), user_package), num_fast(O_ASYNC));
#endif
#ifdef O_DIRECTORY
  reg_varl(intern(lit("o-directory"), user_package), num_fast(O_DIRECTORY));
#endif
#ifdef O_NOFOLLOW
  reg_varl(intern(lit("o-nofollow"), user_package), num_fast(O_NOFOLLOW));
#endif
#ifdef O_CLOEXEC
  reg_varl(intern(lit("o-cloexec"), user_package), num_fast(O_CLOEXEC));
#endif
#ifdef O_DIRECT
  reg_varl(intern(lit("o-direct"), user_package), num_fast(O_DIRECT));
#endif
#ifdef O_NOATIME
  reg_varl(intern(lit("o-noatime"), user_package), num_fast(O_NOATIME));
#endif
#ifdef O_PATH
  reg_varl(intern(lit("o-path"), user_package), num_fast(O_PATH));
#endif

  reg_varl(intern(lit("f-dupfd"), user_package), num_fast(F_DUPFD));
#ifdef F_DUPFD_CLOEXEC
  reg_varl(intern(lit("f-dupfd-cloexec"), user_package), num_fast(F_DUPFD_CLOEXEC));
#endif
  reg_varl(intern(lit("f-getfd"), user_package), num_fast(F_GETFD));
  reg_varl(intern(lit("f-setfd"), user_package), num_fast(F_SETFD));

  reg_varl(intern(lit("fd-cloexec"), user_package), num_fast(FD_CLOEXEC));

  reg_varl(intern(lit("f-getfl"), user_package), num_fast(F_GETFL));
  reg_varl(intern(lit("f-setfl"), user_package), num_fast(F_SETFL));

  reg_varl(intern(lit("f-getlk"), user_package), num_fast(F_GETLK));
  reg_varl(intern(lit("f-setlk"), user_package), num_fast(F_SETLK));
  reg_varl(intern(lit("f-setlkw"), user_package), num_fast(F_SETLKW));
  reg_varl(intern(lit("f-rdlck"), user_package), num_fast(F_RDLCK));
  reg_varl(intern(lit("f-wrlck"), user_package), num_fast(F_WRLCK));
  reg_varl(intern(lit("f-unlck"), user_package), num_fast(F_UNLCK));
  reg_varl(intern(lit("seek-set"), user_package), num_fast(SEEK_SET));
  reg_varl(intern(lit("seek-cur"), user_package), num_fast(SEEK_CUR));
  reg_varl(intern(lit("seek-end"), user_package), num_fast(SEEK_END));

  reg_fun(intern(lit("fcntl"), user_package), func_n3o(fcntl_wrap, 2));
#endif

#if HAVE_FILE_STAMP_CHANGE
  reg_fun(intern(lit("utimes"), user_package), func_n5(wrap_utimes));
  reg_fun(intern(lit("lutimes"), user_package), func_n5(wrap_lutimes));
#endif

  {
    val fn = func_n1(stat_wrap);
    reg_fun(intern(lit("stat"), user_package), fn);
    reg_fun(intern(lit("fstat"), user_package), fn);
  }
  reg_fun(intern(lit("lstat"), user_package), func_n1(lstat_wrap));

#if HAVE_SYS_STAT
#ifndef S_IFSOCK
#define S_IFSOCK 0
#endif

#ifndef S_IFLNK
#define S_IFLNK 0
#endif

#ifndef S_ISUID
#define S_ISUID 0
#endif

#ifndef S_ISGID
#define S_ISGID 0
#endif

#ifndef S_ISVTX
#define S_ISVTX 0
#endif

#ifndef S_IRWXG
#define S_IRWXG 0
#endif

#ifndef S_IRGRP
#define S_IRGRP 0
#endif

#ifndef S_IWGRP
#define S_IWGRP 0
#endif

#ifndef S_IXGRP
#define S_IXGRP 0
#endif

#ifndef S_IRWXO
#define S_IRWXO 0
#endif

#ifndef S_IROTH
#define S_IROTH 0
#endif

#ifndef S_IWOTH
#define S_IWOTH 0
#endif

#ifndef S_IXOTH
#define S_IXOTH 0
#endif

  reg_varl(intern(lit("s-ifmt"), user_package), num_fast(S_IFMT));
  reg_varl(intern(lit("s-ifsock"), user_package), num_fast(S_IFSOCK));
  reg_varl(intern(lit("s-iflnk"), user_package), num_fast(S_IFLNK));
  reg_varl(intern(lit("s-ifreg"), user_package), num_fast(S_IFREG));
  reg_varl(intern(lit("s-ifblk"), user_package), num_fast(S_IFBLK));
  reg_varl(intern(lit("s-ifdir"), user_package), num_fast(S_IFDIR));
  reg_varl(intern(lit("s-ifchr"), user_package), num_fast(S_IFCHR));
  reg_varl(intern(lit("s-ififo"), user_package), num_fast(S_IFIFO));
  reg_varl(intern(lit("s-isuid"), user_package), num_fast(S_ISUID));
  reg_varl(intern(lit("s-isgid"), user_package), num_fast(S_ISGID));
  reg_varl(intern(lit("s-isvtx"), user_package), num_fast(S_ISVTX));
  reg_varl(intern(lit("s-irwxu"), user_package), num_fast(S_IRWXU));
  reg_varl(intern(lit("s-irusr"), user_package), num_fast(S_IRUSR));
  reg_varl(intern(lit("s-iwusr"), user_package), num_fast(S_IWUSR));
  reg_varl(intern(lit("s-ixusr"), user_package), num_fast(S_IXUSR));
  reg_varl(intern(lit("s-irwxg"), user_package), num_fast(S_IRWXG));
  reg_varl(intern(lit("s-irgrp"), user_package), num_fast(S_IRGRP));
  reg_varl(intern(lit("s-iwgrp"), user_package), num_fast(S_IWGRP));
  reg_varl(intern(lit("s-ixgrp"), user_package), num_fast(S_IXGRP));
  reg_varl(intern(lit("s-irwxo"), user_package), num_fast(S_IRWXO));
  reg_varl(intern(lit("s-iroth"), user_package), num_fast(S_IROTH));
  reg_varl(intern(lit("s-iwoth"), user_package), num_fast(S_IWOTH));
  reg_varl(intern(lit("s-ixoth"), user_package), num_fast(S_IXOTH));
#endif
#if HAVE_POLL
  reg_varl(intern(lit("poll-in"), user_package), num_fast(POLLIN));
  reg_varl(intern(lit("poll-out"), user_package), num_fast(POLLOUT));
  reg_varl(intern(lit("poll-err"), user_package), num_fast(POLLERR));
#ifdef POLLPRI
  reg_varl(intern(lit("poll-pri"), user_package), num_fast(POLLPRI));
#endif
#ifdef POLLRDHUP
  reg_varl(intern(lit("poll-rdhup"), user_package), num_fast(POLLRDHUP));
#endif
#ifdef POLLNVAL
  reg_varl(intern(lit("poll-nval"), user_package), num_fast(POLLNVAL));
#endif
#ifdef POLLRDBAND
  reg_varl(intern(lit("poll-rdband"), user_package), num_fast(POLLRDBAND));
#endif
#ifdef POLLWRBAND
  reg_varl(intern(lit("poll-wrband"), user_package), num_fast(POLLWRBAND));
#endif
#endif

#if HAVE_FORK_STUFF
  reg_fun(intern(lit("fork"), user_package), func_n0(fork_wrap));
  reg_fun(intern(lit("wait"), user_package), func_n2o(wait_wrap, 0));
  reg_fun(intern(lit("exec"), user_package), func_n2o(exec_wrap, 1));
  reg_fun(intern(lit("exit*"), user_package), func_n1(exit_star_wrap));
  reg_fun(intern(lit("w-ifexited"), user_package), func_n1(wifexited));
  reg_fun(intern(lit("w-exitstatus"), user_package), func_n1(wexitstatus));
  reg_fun(intern(lit("w-ifsignaled"), user_package), func_n1(wifsignaled));
  reg_fun(intern(lit("w-termsig"), user_package), func_n1(wtermsig));
#ifdef WCOREDUMP
  reg_fun(intern(lit("w-coredump"), user_package), func_n1(wcoredump));
#endif
  reg_fun(intern(lit("w-ifstopped"), user_package), func_n1(wifstopped));
  reg_fun(intern(lit("w-stopsig"), user_package), func_n1(wstopsig));
#ifdef WIFCONTINUED
  reg_fun(intern(lit("w-ifcontinued"), user_package), func_n1(wifcontinued));
#endif
#ifdef WNOHANG
  reg_varl(intern(lit("w-nohang"), user_package), num_fast(WNOHANG));
#endif
#ifdef WUNTRACED
  reg_varl(intern(lit("w-untraced"), user_package), num_fast(WUNTRACED));
#endif
#ifdef WCONTINUED
  reg_varl(intern(lit("w-continued"), user_package), num_fast(WCONTINUED));
#endif
  reg_fun(intern(lit("dupfd"), user_package), func_n2o(dup_wrap, 1));
  reg_fun(intern(lit("close"), user_package), func_n2o(close_wrap, 1));
#endif
#if HAVE_PIPE
  reg_fun(intern(lit("pipe"), user_package), func_n0(pipe_wrap));
#endif
  reg_fun(intern(lit("getenv"), user_package), func_n1(getenv_wrap));
  reg_fun(intern(lit("setenv"), user_package), func_n3o(setenv_wrap, 2));
  reg_fun(intern(lit("unsetenv"), user_package), func_n1(unsetenv_wrap));

#if HAVE_GETEUID
  reg_fun(intern(lit("getuid"), user_package), func_n0(getuid_wrap));
  reg_fun(intern(lit("geteuid"), user_package), func_n0(geteuid_wrap));
  reg_fun(intern(lit("getgid"), user_package), func_n0(getgid_wrap));
  reg_fun(intern(lit("getegid"), user_package), func_n0(getegid_wrap));
  reg_fun(intern(lit("getgroups"), user_package), func_n0(getgroups_wrap));
  reg_fun(intern(lit("setuid"), user_package), func_n1(setuid_wrap));
  reg_fun(intern(lit("seteuid"), user_package), func_n1(seteuid_wrap));
  reg_fun(intern(lit("setgid"), user_package), func_n1(setgid_wrap));
  reg_fun(intern(lit("setegid"), user_package), func_n1(setegid_wrap));
#endif

#if HAVE_SETGROUPS
  reg_fun(intern(lit("setgroups"), user_package), func_n1(setgroups_wrap));
#endif

#if HAVE_SETRESUID
  reg_fun(intern(lit("getresuid"), user_package), func_n0(getresuid_wrap));
  reg_fun(intern(lit("getresgid"), user_package), func_n0(getresgid_wrap));
  reg_fun(intern(lit("setresuid"), user_package), func_n3(setresuid_wrap));
  reg_fun(intern(lit("setresgid"), user_package), func_n3(setresgid_wrap));
#endif

#if HAVE_PWUID
  reg_fun(intern(lit("setpwent"), user_package), func_n0(setpwent_wrap));
  reg_fun(intern(lit("endpwent"), user_package), func_n0(endpwent_wrap));
  reg_fun(intern(lit("getpwent"), user_package), func_n0(getpwent_wrap));
  reg_fun(intern(lit("getpwuid"), user_package), func_n1(getpwuid_wrap));
  reg_fun(intern(lit("getpwnam"), user_package), func_n1(getpwnam_wrap));
#endif

#if HAVE_GRGID
  reg_fun(intern(lit("setgrent"), user_package), func_n0(setgrent_wrap));
  reg_fun(intern(lit("endgrent"), user_package), func_n0(endgrent_wrap));
  reg_fun(intern(lit("getgrent"), user_package), func_n0(getgrent_wrap));
  reg_fun(intern(lit("getgrgid"), user_package), func_n1(getgrgid_wrap));
  reg_fun(intern(lit("getgrnam"), user_package), func_n1(getgrnam_wrap));
#endif

#if HAVE_CRYPT || HAVE_CRYPT_R
  reg_fun(intern(lit("crypt"), user_package), func_n2(crypt_wrap));
#endif

#if HAVE_POLL
  reg_fun(intern(lit("poll"), user_package), func_n2o(poll_wrap, 1));
#endif

#if HAVE_SYS_STAT
  reg_fun(intern(lit("umask"), user_package), func_n1o(umask_wrap, 0));
#endif

#if HAVE_FNMATCH
  reg_fun(intern(lit("fnmatch"), user_package), func_n3o(fnmatch_wrap, 2));
#endif

#if HAVE_FNMATCH
#ifdef FNM_PATHNAME
  reg_varl(intern(lit("fnm-pathname"), user_package), num_fast(FNM_PATHNAME));
#endif
#ifdef FNM_NOESCAPE
  reg_varl(intern(lit("fnm-noescape"), user_package), num_fast(FNM_NOESCAPE));
#endif
#ifdef FNM_PERIOD
  reg_varl(intern(lit("fnm-period"), user_package), num_fast(FNM_PERIOD));
#endif
#ifdef FNM_LEADING_DIR
  reg_varl(intern(lit("fnm-leading-dir"), user_package), num_fast(FNM_LEADING_DIR));
#endif
#ifdef FNM_CASEFOLD
  reg_varl(intern(lit("fnm-casefold"), user_package), num_fast(FNM_CASEFOLD));
#endif
#ifdef FNM_EXTMATCH
  reg_varl(intern(lit("fnm-extmatch"), user_package), num_fast(FNM_EXTMATCH));
#endif
#endif

#if HAVE_UNAME
  reg_fun(intern(lit("uname"), user_package), func_n0(uname_wrap));
#endif

#if HAVE_DLOPEN
  dlhandle_s = intern(lit("dlhandle"), user_package);
  dlsym_s = intern(lit("dlsym"), user_package);
  reg_fun(intern(lit("dlopen"), user_package), func_n2o(dlopen_wrap, 0));
  reg_fun(intern(lit("dlclose"), user_package), func_n1(dlclose_wrap));
  reg_fun(dlsym_s, func_n2(dlsym_wrap));
  reg_fun(intern(lit("dlsym-checked"), user_package), func_n2(dlsym_checked));
#if HAVE_DLVSYM
  reg_fun(intern(lit("dlvsym"), user_package), func_n3o(dlvsym_wrap, 2));
  reg_fun(intern(lit("dlvsym-checked"), user_package), func_n3o(dlvsym_checked, 2));
#endif
  reg_varl(intern(lit("rtld-lazy"), user_package), num_fast(RTLD_LAZY));
  reg_varl(intern(lit("rtld-now"), user_package), num_fast(RTLD_NOW));
#ifdef RTLD_GLOBAL
  reg_varl(intern(lit("rtld-global"), user_package), num_fast(RTLD_GLOBAL));
#endif
#ifdef RTLD_LOCAL
  reg_varl(intern(lit("rtld-local"), user_package), num_fast(RTLD_LOCAL));
#endif
#ifdef RTLD_NODELETE
  reg_varl(intern(lit("rtld-nodelete"), user_package), num_fast(RTLD_NODELETE));
#endif
#ifdef RTLD_NOLOAD
  reg_varl(intern(lit("rtld-noload"), user_package), num_fast(RTLD_NOLOAD));
#endif
#ifdef RTLD_DEEPBIND
  reg_varl(intern(lit("rtld-deepbind"), user_package), num_fast(RTLD_DEEPBIND));
#endif
#endif

#if HAVE_REALPATH
  reg_fun(intern(lit("realpath"), user_package), func_n1(realpath_wrap));
#endif

#if HAVE_ISATTY
  reg_fun(intern(lit("isatty"), user_package), func_n1(isatty_wrap));
#endif

  dirent_st = make_struct_type(dirent_s, nil, nil,
                               list(name_s, ino_s, type_s, nao),
                               nil, nil, nil, nil);
  reg_fun(intern(lit("opendir"), user_package), func_n2o(opendir_wrap, 1));
  reg_fun(intern(lit("closedir"), user_package), func_n1(closedir_wrap));
  reg_fun(intern(lit("readdir"), user_package), func_n2o(readdir_wrap, 1));

#ifdef DT_BLK
  reg_varl(intern(lit("dt-blk"), user_package), num_fast(DT_BLK));
#endif
#ifdef DT_CHR
  reg_varl(intern(lit("dt-chr"), user_package), num_fast(DT_CHR));
#endif
#ifdef DT_DIR
  reg_varl(intern(lit("dt-dir"), user_package), num_fast(DT_DIR));
#endif
#ifdef DT_FIFO
  reg_varl(intern(lit("dt-fifo"), user_package), num_fast(DT_FIFO));
#endif
#ifdef DT_LNK
  reg_varl(intern(lit("dt-lnk"), user_package), num_fast(DT_LNK));
#endif
#ifdef DT_REG
  reg_varl(intern(lit("dt-reg"), user_package), num_fast(DT_REG));
#endif
#ifdef DT_SOCK
  reg_varl(intern(lit("dt-sock"), user_package), num_fast(DT_SOCK));
#endif
#ifdef DT_UNKNOWN
  reg_varl(intern(lit("dt-unknown"), user_package), num_fast(DT_UNKNOWN));
#endif
}
