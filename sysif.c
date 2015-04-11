/* Copyright 2010-2015
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
#include <setjmp.h>
#include <wchar.h>
#include <signal.h>
#include <dirent.h>
#include <errno.h>
#include <time.h>
#include "config.h"
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#if HAVE_FCNTL_H
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
#if HAVE_MAKEDEV
#include <sys/types.h>
#endif
#include "lib.h"
#include "stream.h"
#include "hash.h"
#include "signal.h"
#include "utf8.h"
#include "unwind.h"
#include "gc.h"
#include "eval.h"
#include "sysif.h"

static val errno_wrap(val newval)
{
  val oldval = num(errno);
  if (default_bool_arg(newval))
    errno = c_num(newval);
  return oldval;
}

#if HAVE_DAEMON
static val daemon_wrap(val nochdir, val noclose)
{
  int result = daemon(nochdir ? 1 : 0, noclose ? 1 : 0);
  return result == 0 ? t : nil;
}
#endif

static val exit_wrap(val status)
{
  int stat;

  if (status == nil)
    stat = EXIT_FAILURE;
  else if (status == t)
    stat = EXIT_SUCCESS;
  else
    stat = c_num(status);

  exit(stat);
  /* notreached */
  return nil;
}

static val abort_wrap(void)
{
  abort();
}

static val usleep_wrap(val usec)
{
  val retval;
  cnum u = c_num(usec);

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
    cons_bind (key, val_cons, split_str(car(env_strings), lit("=")));
    sethash(hash, key, car(val_cons));
  }

  return hash;
}

#if HAVE_MKDIR
static val mkdir_wrap(val path, val mode)
{
  char *u8path = utf8_dup_to(c_str(path));
  int err = mkdir(u8path, c_num(default_arg(mode, num_fast(0777))));
  free(u8path);

  if (err < 0)
    uw_throwf(file_error_s, lit("mkdir ~a: ~a/~s"),
              path, num(errno), string_utf8(strerror(errno)), nao);

  return t;
}
#elif HAVE_WINDOWS_H
static val mkdir_wrap(val path, val mode)
{
  int err = _wmkdir(c_str(path));

  (void) mode;
  if (err < 0)
    uw_throwf(file_error_s, lit("mkdir ~a: ~a/~s"),
              path, num(errno), string_utf8(strerror(errno)), nao);

  return t;
}
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

    partial_path = format(nil, lit("~a~a~a"),
                          partial_path, sep, pop(&split_path), nao);
  }

  if (ret != t)
    uw_throwf(file_error_s,
              lit("ensure-dir: ~a: ~a/~s"), path, ret,
              string_utf8(strerror(c_num(ret))), nao);

  return ret;
}
#endif

#if HAVE_UNISTD_H
static val chdir_wrap(val path)
{
  char *u8path = utf8_dup_to(c_str(path));
  int err = chdir(u8path);
  free(u8path);

  if (err < 0)
    uw_throwf(file_error_s, lit("chdir ~a: ~a/~s"),
              path, num(errno), string_utf8(strerror(errno)), nao);
  return t;
}

static val getcwd_wrap(void)
{
  size_t guess = 256;

  for (;;) {
    char *u8buf = coerce(char *, chk_malloc(guess));

    if (getcwd(u8buf, guess) == 0) {
      free(u8buf);
      if (errno != ERANGE) {
        uw_throwf(file_error_s, lit("getcwd: ~a/~s"),
                  num(errno), string_utf8(strerror(errno)), nao);
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
#endif

#if HAVE_MAKEDEV

static val makedev_wrap(val major, val minor)
{
  return num(makedev(c_num(major), c_num(minor)));
}

static val minor_wrap(val dev)
{
  return num(minor(c_num(dev)));
}

static val major_wrap(val dev)
{
  return num(major(c_num(dev)));
}

#endif

#if HAVE_MKNOD

static val mknod_wrap(val path, val mode, val dev)
{
  char *u8path = utf8_dup_to(c_str(path));
  int err = mknod(u8path, c_num(mode), c_num(default_arg(dev, zero)));
  free(u8path);

  if (err < 0)
#if HAVE_MAKEDEV
    uw_throwf(file_error_s, lit("mknod ~a ~a ~a (~a:~a): ~a/~s"),
              path, mode, dev, major_wrap(dev), minor_wrap(dev), num(errno),
              string_utf8(strerror(errno)), nao);
#else
    uw_throwf(file_error_s, lit("mknod ~a ~a ~a: ~a/~s"),
              path, mode, dev, num(errno),
              string_utf8(strerror(errno)), nao);
#endif

  return t;
}

#endif

#if HAVE_SYMLINK

static val symlink_wrap(val target, val to)
{
  char *u8target = utf8_dup_to(c_str(target));
  char *u8to = utf8_dup_to(c_str(to));
  int err = symlink(u8target, u8to);
  free(u8target);
  free(u8to);
  if (err < 0)
    uw_throwf(file_error_s, lit("symlink ~a ~a: ~a/~s"),
              target, to, num(errno), string_utf8(strerror(errno)), nao);
  return t;
}

static val link_wrap(val target, val to)
{
  char *u8target = utf8_dup_to(c_str(target));
  char *u8to = utf8_dup_to(c_str(to));
  int err = link(u8target, u8to);
  free(u8target);
  free(u8to);
  if (err < 0)
    uw_throwf(file_error_s, lit("link ~a ~a: ~a/~s"),
              target, to, num(errno), string_utf8(strerror(errno)), nao);
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
      free(u8buf);
      uw_throwf(file_error_s, lit("readlink ~a: ~a/~s"),
                path, num(errno), string_utf8(strerror(errno)), nao);
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
  cnum p = c_num(default_arg(pid, negone));
  cnum f = c_num(default_arg(flags, zero));
  int status = 0, result = waitpid(p, &status, f);
  return if2(result >= 0, cons(num(result), num(status)));
}

static val wifexited(val status)
{
  int s = c_num(if3(consp(status), cdr(status), status));
  return c_true(WIFEXITED(s));
}

static val wexitstatus(val status)
{
  int s = c_num(if3(consp(status), cdr(status), status));
  return num(WEXITSTATUS(s));
}

static val wifsignaled(val status)
{
  int s = c_num(if3(consp(status), cdr(status), status));
  return c_true(WIFSIGNALED(s));
}

static val wtermsig(val status)
{
  int s = c_num(if3(consp(status), cdr(status), status));
  return num(WTERMSIG(s));
}

#ifdef WCOREDUMP
static val wcoredump(val status)
{
  int s = c_num(if3(consp(status), cdr(status), status));
  return c_true(WCOREDUMP(s));
}
#endif

static val wifstopped(val status)
{
  int s = c_num(if3(consp(status), cdr(status), status));
  return c_true(WIFSTOPPED(s));
}

static val wstopsig(val status)
{
  int s = c_num(if3(consp(status), cdr(status), status));
  return num(WSTOPSIG(s));
}

#ifdef WIFCONTINUED
static val wifcontinued(val status)
{
  int s = c_num(if3(consp(status), cdr(status), status));
  return c_true(WIFCONTINUED(s));
}
#endif

static val dup_wrap(val old, val new)
{
  if (missingp(new))
    return num(dup(c_num(old)));
  return num(dup2(c_num(old), c_num(new)));
}

#endif

#if HAVE_SYS_STAT
static int w_stat(const wchar_t *wpath, struct stat *buf)
{
  char *path = utf8_dup_to(wpath);
  int res = stat(path, buf);
  free(path);
  return res;
}
#endif

val statf(val path)
{
#if HAVE_SYS_STAT
  struct stat st;
  int res = w_stat(c_str(path), &st);

  if (res == -1)
    uw_throwf(file_error_s, lit("unable to stat ~a: ~a/~s"),
              path, num(errno), string_utf8(strerror(errno)), nao);

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
#else
  uw_throwf(file_error_s, lit("stat is not implemented"), nao);
#endif
}


void sysif_init(void)
{
  reg_fun(intern(lit("errno"), user_package), func_n1o(errno_wrap, 0));
  reg_fun(intern(lit("exit"), user_package), func_n1(exit_wrap));
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
#endif

#if HAVE_MAKEDEV
  reg_fun(intern(lit("makedev"), user_package), func_n2(makedev_wrap));
  reg_fun(intern(lit("minor"), user_package), func_n1(minor_wrap));
  reg_fun(intern(lit("major"), user_package), func_n1(major_wrap));
#endif

#if HAVE_MKNOD
  reg_fun(intern(lit("mknod"), user_package), func_n3(mknod_wrap));
#endif

#if HAVE_SYMLINK
  reg_fun(intern(lit("symlink"), user_package), func_n2(symlink_wrap));
  reg_fun(intern(lit("link"), user_package), func_n2(link_wrap));
  reg_fun(intern(lit("readlink"), user_package), func_n1(readlink_wrap));
#endif

  reg_fun(intern(lit("stat"), user_package), func_n1(statf));

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

  reg_var(intern(lit("s-ifmt"), user_package), num_fast(S_IFMT));
  reg_var(intern(lit("s-ifsock"), user_package), num_fast(S_IFSOCK));
  reg_var(intern(lit("s-iflnk"), user_package), num_fast(S_IFLNK));
  reg_var(intern(lit("s-ifreg"), user_package), num_fast(S_IFREG));
  reg_var(intern(lit("s-ifblk"), user_package), num_fast(S_IFBLK));
  reg_var(intern(lit("s-ifdir"), user_package), num_fast(S_IFDIR));
  reg_var(intern(lit("s-ifchr"), user_package), num_fast(S_IFCHR));
  reg_var(intern(lit("s-ififo"), user_package), num_fast(S_IFIFO));
  reg_var(intern(lit("s-isuid"), user_package), num_fast(S_ISUID));
  reg_var(intern(lit("s-isgid"), user_package), num_fast(S_ISGID));
  reg_var(intern(lit("s-isvtx"), user_package), num_fast(S_ISVTX));
  reg_var(intern(lit("s-irwxu"), user_package), num_fast(S_IRWXU));
  reg_var(intern(lit("s-irusr"), user_package), num_fast(S_IRUSR));
  reg_var(intern(lit("s-iwusr"), user_package), num_fast(S_IWUSR));
  reg_var(intern(lit("s-ixusr"), user_package), num_fast(S_IXUSR));
  reg_var(intern(lit("s-irwxg"), user_package), num_fast(S_IRWXG));
  reg_var(intern(lit("s-irgrp"), user_package), num_fast(S_IRGRP));
  reg_var(intern(lit("s-iwgrp"), user_package), num_fast(S_IWGRP));
  reg_var(intern(lit("s-ixgrp"), user_package), num_fast(S_IXGRP));
  reg_var(intern(lit("s-irwxo"), user_package), num_fast(S_IRWXO));
  reg_var(intern(lit("s-iroth"), user_package), num_fast(S_IROTH));
  reg_var(intern(lit("s-iwoth"), user_package), num_fast(S_IWOTH));
  reg_var(intern(lit("s-ixoth"), user_package), num_fast(S_IXOTH));
#endif

#if HAVE_FORK_STUFF
  reg_fun(intern(lit("fork"), user_package), func_n0(fork_wrap));
  reg_fun(intern(lit("wait"), user_package), func_n2o(wait_wrap, 0));
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
  reg_var(intern(lit("w-nohang"), user_package), num_fast(WNOHANG));
#endif
#ifdef WUNTRACED
  reg_var(intern(lit("w-untraced"), user_package), num_fast(WUNTRACED));
#endif
#ifdef WCONTINUED
  reg_var(intern(lit("w-continued"), user_package), num_fast(WCONTINUED));
#endif
  reg_fun(intern(lit("dupfd"), user_package), func_n2o(dup_wrap, 1));
#endif
}
