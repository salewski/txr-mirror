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
#if HAVE_POLL
#include <poll.h>
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

#if HAVE_CHMOD

static val chmod_wrap(val path, val mode)
{
  char *u8path = utf8_dup_to(c_str(path));
  int err = chmod(u8path, c_num(mode));
  free(u8path);

  if (err < 0)
    uw_throwf(file_error_s, lit("chmod ~a ~a: ~a/~s"),
              path, mode, num(errno), string_utf8(strerror(errno)), nao);
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
  return tnil(WIFEXITED(s));
}

static val wexitstatus(val status)
{
  int s = c_num(if3(consp(status), cdr(status), status));
  return num(WEXITSTATUS(s));
}

static val wifsignaled(val status)
{
  int s = c_num(if3(consp(status), cdr(status), status));
  return tnil(WIFSIGNALED(s));
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
  return tnil(WCOREDUMP(s));
}
#endif

static val wifstopped(val status)
{
  int s = c_num(if3(consp(status), cdr(status), status));
  return tnil(WIFSTOPPED(s));
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
  return tnil(WIFCONTINUED(s));
}
#endif

static val dup_wrap(val old, val neu)
{
  if (missingp(neu))
    return num(dup(c_num(old)));
  return num(dup2(c_num(old), c_num(neu)));
}

static val exec_wrap(val file, val args_opt)
{
  val args = default_bool_arg(args_opt);
  int nargs = c_num(length(args)) + 1;
  char **argv = coerce(char **, chk_malloc((nargs + 1) * sizeof *argv));
  val iter;
  int i;

  for (i = 0, iter = cons(file, args); iter; i++, iter = cdr(iter)) {
    val arg = car(iter);
    argv[i] = utf8_dup_to(c_str(arg));
  }
  argv[i] = 0;

  if (execvp(argv[0], argv) < 0)
    uw_throwf(file_error_s, lit("execvp ~a: ~a/~s"),
                file, num(errno), string_utf8(strerror(errno)), nao);
  uw_throwf(file_error_s, lit("execvp ~a returned"), file, nao);
}

static val exit_star_wrap(val status)
{
  int stat;

  if (status == nil)
    stat = EXIT_FAILURE;
  else if (status == t)
    stat = EXIT_SUCCESS;
  else
    stat = c_num(status);

  _exit(stat);
  /* notreached */
  return nil;
}

#endif

#if HAVE_SYS_STAT
static int w_stat(val wpath, struct stat *buf)
{
  char *path = utf8_dup_to(c_str(wpath));
  int res = stat(path, buf);
  free(path);
  return res;
}

#ifdef S_IFLNK
static int w_lstat(val wpath, struct stat *buf)
{
  char *path = utf8_dup_to(c_str(wpath));
  int res = lstat(path, buf);
  free(path);
  return res;
}
#else
#define w_lstat w_stat
#endif

static int w_fstat(val stream, struct stat *buf)
{
  val fd = stream_get_prop(stream, fd_k);

  if (fd) {
    int res = fstat(c_num(fd), buf);
    return res;
  }

  uw_throwf(file_error_s,
            lit("cannot fstat stream ~s: it has no :fd property"),
            stream, nao);
}
#endif

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

#endif

static val stat_impl(val obj, int (*statfn)(val, struct stat *),
                     val name)
{
#if HAVE_SYS_STAT
  struct stat st;
  int res = statfn(obj, &st);

  if (res == -1)
    uw_throwf(file_error_s, lit("unable to ~a ~a: ~a/~s"),
              name, obj, num(errno), string_utf8(strerror(errno)), nao);

  return stat_to_list(st);
#else
  uw_throwf(file_error_s, lit("~a is not implemented"), name, nao);
#endif
}

static val statp(val path)
{
  return stat_impl(path, w_stat, lit("stat"));
}

static val statl(val path)
{
  return stat_impl(path, w_lstat, lit("lstat"));
}

static val statf(val stream)
{
  return stat_impl(stream, w_fstat, lit("lstat"));
}

#if HAVE_PIPE

static val pipe_wrap(void)
{
  int fd[2];
  if (pipe(fd) < 0)
    uw_throwf(file_error_s, lit("pipe failed: ~a/~s"),
              num(errno), string_utf8(strerror(errno)), nao);
  return cons(num(fd[0]), num(fd[1]));
}

#endif

static val getenv_wrap(val name)
{
  char *nameu8 = utf8_dup_to(c_str(name));
  char *lookup = getenv(nameu8);
  val result = lookup ? string_utf8(lookup) : nil;
  free(nameu8);
  return result;
}

static val setenv_wrap(val name, val value, val overwrite)
{
  char *nameu8 = utf8_dup_to(c_str(name));
  char *valu8 = utf8_dup_to(c_str(value));
  setenv(nameu8, valu8, default_arg(overwrite, t) != nil);
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
  nfds_t i, len = c_num(length(poll_list));
  val iter;
  struct pollfd *pfd = convert(struct pollfd *,
                               chk_calloc(len, sizeof *pfd));
  val timeout = default_arg(timeout_in, negone);
  int res;

  for (i = 0, iter = poll_list; iter; iter = cdr(iter), i++) {
    cons_bind (obj, events, car(iter));

    pfd[i].events = c_num(events);

    switch (type(obj)) {
    case NUM:
      pfd[i].fd = c_num(obj);
      break;
    case COBJ:
      if (obj->co.cls == stream_s) {
        val fdval = stream_get_prop(obj, fd_k);
        if (!fdval) {
          free(pfd);
          uw_throwf(file_error_s,
                    lit("poll: stream ~s doesn't have a file descriptor"),
                    obj, nao);
        }
        pfd[i].fd = c_num(fdval);
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

  res = poll(pfd, len, c_num(timeout));

  if (res < 0) {
    free(pfd);
    uw_throwf(file_error_s, lit("poll failed: ~a/~s"),
              num(errno), string_utf8(strerror(errno)), nao);
  }

  if (res == 0) {
    free(pfd);
    return nil;
  }

  {
    list_collect_decl (out, ptail);

    for (i = 0, iter = poll_list; iter; iter = cdr(iter), i++) {
      val pair = car(iter);
      if (pfd[i].revents)
        ptail = list_collect(ptail, cons(car(pair), num(pfd[i].revents)));
    }

    free(pfd);
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
  gid_t dummy[1];
  int needed = getgroups(0, dummy);

  if (needed == 0) {
    return nil;
  } else if (needed >  0) {
    gid_t *arr = coerce(gid_t *, chk_malloc(needed *sizeof *arr));
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

  uw_throwf(system_error_s, lit("getgroups failed: ~a/~s"),
            num(errno), string_utf8(strerror(errno)), nao);
  abort();
}

static val setuid_wrap(val nval)
{
  if (setuid(c_num(nval)) == -1)
    uw_throwf(system_error_s, lit("setuid failed: ~a/~s"),
              num(errno), string_utf8(strerror(errno)), nao);
  return t;
}

static val seteuid_wrap(val nval)
{
  if (seteuid(c_num(nval)) == -1)
    uw_throwf(system_error_s, lit("seteuid failed: ~a/~s"),
              num(errno), string_utf8(strerror(errno)), nao);
  return t;
}

static val setgid_wrap(val nval)
{
  if (setgid(c_num(nval)) == -1)
    uw_throwf(system_error_s, lit("setgid failed: ~a/~s"),
              num(errno), string_utf8(strerror(errno)), nao);
  return t;
}

static val setegid_wrap(val nval)
{
  if (setegid(c_num(nval)) == -1)
    uw_throwf(system_error_s, lit("setegid failed: ~a/~s"),
              num(errno), string_utf8(strerror(errno)), nao);
  return t;
}

#endif

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

#if HAVE_CHMOD
  reg_fun(intern(lit("chmod"), user_package), func_n2(chmod_wrap));
#endif

#if HAVE_SYMLINK
  reg_fun(intern(lit("symlink"), user_package), func_n2(symlink_wrap));
  reg_fun(intern(lit("link"), user_package), func_n2(link_wrap));
  reg_fun(intern(lit("readlink"), user_package), func_n1(readlink_wrap));
#endif

  reg_fun(intern(lit("stat"), user_package), func_n1(statp));
  reg_fun(intern(lit("lstat"), user_package), func_n1(statl));
  reg_fun(intern(lit("fstat"), user_package), func_n1(statf));

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

#if HAVE_POLL
  reg_fun(intern(lit("poll"), user_package), func_n2o(poll_wrap, 1));
#endif
}
