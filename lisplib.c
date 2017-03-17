/* Copyright 2015-2017
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

#include <stdio.h>
#include <wchar.h>
#include <dirent.h>
#include <stdarg.h>
#include <signal.h>
#include "config.h"
#include "lib.h"
#include "eval.h"
#include "signal.h"
#include "stream.h"
#include "hash.h"
#include "gc.h"
#include "debug.h"
#include "txr.h"
#include "socket.h"
#include "lisplib.h"

val dl_table;
int opt_dbg_autoload;
val trace_loaded;

void set_dlt_entries(val dlt, val *name, val fun)
{
  for (; *name; name++) {
    val sym = intern(*name, user_package);

    if (fun)
      sethash(dlt, sym, fun);
    else
      remhash(dlt, sym);
  }
}

static val place_set_entries(val dlt, val fun)
{
  val name[] = {
    lit("*place-clobber-expander*"), lit("*place-update-expander*"),
    lit("*place-delete-expander*"), lit("*place-macro*"),
    lit("get-update-expander"), lit("get-clobber-expander"),
    lit("get-delete-expander"),
    lit("place-form-p"),
    lit("rlet"), lit("slet"), lit("alet"), lit("with-gensyms"),
    lit("call-update-expander"), lit("call-clobber-expander"),
    lit("call-delete-expander)"),
    lit("with-update-expander"), lit("with-clobber-expander"),
    lit("with-delete-expander"),
    lit("set"), lit("pset"), lit("zap"), lit("flip"), lit("inc"), lit("dec"),
    lit("push"), lit("pop"), lit("swap"), lit("shift"), lit("rotate"),
    lit("pushnew"), lit("del"), lit("lset"), lit("upd"),
    lit("defplace"), lit("define-place-macro"), lit("define-modify-macro"),
    lit("placelet"), lit("placelet*"), lit("define-acessor"),
    lit("with-slots"),
    nil
  };

  set_dlt_entries(dlt, name, fun);
  return nil;
}

static val place_instantiate(val set_fun)
{
  funcall1(set_fun, nil);
  load(format(nil, lit("~aplace.tl"), stdlib_path, nao));
  return nil;
}

static val ver_set_entries(val dlt, val fun)
{
  val name[] = { lit("*lib-version*"), lit("lib-version"), nil };
  set_dlt_entries(dlt, name, fun);
  return nil;
}

static val ver_instantiate(val set_fun)
{
  funcall1(set_fun, nil);
  load(format(nil, lit("~aver.tl"), stdlib_path, nao));
  return nil;
}

static val ifa_set_entries(val dlt, val fun)
{
  val name[] = {
    lit("ifa"), lit("whena"), lit("conda"), lit("condlet"), nil
  };
  set_dlt_entries(dlt, name, fun);
  return nil;
}

static val ifa_instantiate(val set_fun)
{
  funcall1(set_fun, nil);
  load(format(nil, lit("~aifa.tl"), stdlib_path, nao));
  return nil;
}

static val txr_case_set_entries(val dlt, val fun)
{
  val name[] = { lit("txr-if"), lit("txr-when"), lit("txr-case"), nil };
  set_dlt_entries(dlt, name, fun);
  return nil;
}

static val txr_case_instantiate(val set_fun)
{
  funcall1(set_fun, nil);
  load(format(nil, lit("~atxr-case.tl"), stdlib_path, nao));
  return nil;
}

static val with_resources_set_entries(val dlt, val fun)
{
  val name[] = {
    lit("with-resources"),
    lit("with-objects"),
    nil
  };
  set_dlt_entries(dlt, name, fun);
  return nil;
}

static val with_resources_instantiate(val set_fun)
{
  funcall1(set_fun, nil);
  load(format(nil, lit("~awith-resources.tl"), stdlib_path, nao));
  return nil;
}

static val path_test_set_entries(val dlt, val fun)
{
  val name[] = {
    lit("path-exists-p"), lit("path-file-p"), lit("path-dir-p"),
    lit("path-symlink-p"), lit("path-blkdev-p"), lit("path-chrdev-p"),
    lit("path-sock-p"), lit("path-pipe-p"), lit("path-pipe-p"),
    lit("path-setgid-p"), lit("path-setuid-p"), lit("path-sticky-p"),
    lit("path-mine-p"), lit("path-my-group-p"), lit("path-executable-to-me-p"),
    lit("path-writable-to-me-p"), lit("path-readable-to-me-p"),
    lit("path-read-writable-to-me-p"),
    lit("path-newer"), lit("path-older"),
    lit("path-same-object"), lit("path-private-to-me-p"),
    lit("path-strictly-private-to-me-p"),
    nil
  };

  set_dlt_entries(dlt, name, fun);
  return nil;
}

static val path_test_instantiate(val set_fun)
{
  funcall1(set_fun, nil);
  load(format(nil, lit("~apath-test.tl"), stdlib_path, nao));
  return nil;
}

static val struct_set_entries(val dlt, val fun)
{
  val name[] = {
    lit("defstruct"), lit("qref"), lit("uref"), lit("new"), lit("meth"),
    lit("umeth"), lit("usl"), lit("defmeth"), lit("rslot"), nil
  };

  set_dlt_entries(dlt, name, fun);

  if (fun)
    sethash(dlt, struct_lit_s, fun);
  else
    remhash(dlt, struct_lit_s);

  return nil;
}

static val struct_instantiate(val set_fun)
{
  funcall1(set_fun, nil);
  load(format(nil, lit("~astruct.tl"), stdlib_path, nao));
  return nil;
}

static val with_stream_set_entries(val dlt, val fun)
{
  val name[] = {
    lit("with-out-string-stream"),
    lit("with-out-strlist-stream"),
    lit("with-in-string-stream"),
    lit("with-in-string-byte-stream"),
    lit("with-stream"),
    nil
  };
  set_dlt_entries(dlt, name, fun);
  return nil;
}

static val with_stream_instantiate(val set_fun)
{
  funcall1(set_fun, nil);
  load(format(nil, lit("~awith-stream.tl"), stdlib_path, nao));
  return nil;
}

static val hash_set_entries(val dlt, val fun)
{
  val name[] = { lit("with-hash-iter"), nil };
  set_dlt_entries(dlt, name, fun);
  return nil;
}

static val hash_instantiate(val set_fun)
{
  funcall1(set_fun, nil);
  load(format(nil, lit("~ahash.tl"), stdlib_path, nao));
  return nil;
}

static val except_set_entries(val dlt, val fun)
{
  val name[] = {
    lit("catch"), lit("catch*"), lit("handle"), lit("handle*"),
    lit("ignwarn"), lit("macro-time-ignwarn"),
    nil
  };
  set_dlt_entries(dlt, name, fun);
  return nil;
}

static val except_instantiate(val set_fun)
{
  funcall1(set_fun, nil);
  load(format(nil, lit("~aexcept.tl"), stdlib_path, nao));
  return nil;
}

static val type_set_entries(val dlt, val fun)
{
  val name[] = { lit("typecase"), nil };
  set_dlt_entries(dlt, name, fun);
  return nil;
}

static val type_instantiate(val set_fun)
{
  funcall1(set_fun, nil);
  load(format(nil, lit("~atype.tl"), stdlib_path, nao));
  return nil;
}

static val yield_set_entries(val dlt, val fun)
{
  val name[] = {
    lit("obtain"), lit("obtain-block"), lit("yield-from"), lit("yield"),
    lit("obtain*"), lit("obtain*-block"),
    lit("suspend"),
    nil
  };
  set_dlt_entries(dlt, name, fun);
  return nil;
}

static val yield_instantiate(val set_fun)
{
  funcall1(set_fun, nil);
  load(format(nil, lit("~ayield.tl"), stdlib_path, nao));
  return nil;
}

#if HAVE_SOCKETS
static val sock_set_entries(val dlt, val fun)
{
  val name[] = {
    lit("sockaddr"), lit("sockaddr-in"), lit("sockaddr-in6"),
    lit("sockaddr-un"), lit("addrinfo"),
    lit("getaddrinfo"),
    lit("af-unspec"), lit("af-unix"), lit("af-inet"), lit("af-inet6"),
    lit("sock-stream"), lit("sock-dgram"),
    lit("sock-nonblock"), lit("sock-cloexec"),
    lit("ai-passive"), lit("ai-canonname"), lit("ai-numerichost"),
    lit("ai-v4mapped"), lit("ai-all"), lit("ai-addrconfig"),
    lit("ai-numericserv"),
    lit("str-inaddr"), lit("str-in6addr"),
    lit("str-inaddr-net"), lit("str-in6addr-net"),
    lit("open-socket"), lit("open-socket-pair"),
    nil
  };
  set_dlt_entries(dlt, name, fun);
  return nil;
}

static val sock_instantiate(val set_fun)
{
  funcall1(set_fun, nil);
  sock_load_init();
  load(format(nil, lit("~asocket.tl"), stdlib_path, nao));
  return nil;
}

#endif

#if HAVE_TERMIOS

static val termios_set_entries(val dlt, val fun)
{
  val name[] = {
    lit("set-iflags"), lit("set-oflags"), lit("set-cflags"), lit("set-lflags"),
    lit("clear-iflags"), lit("clear-oflags"), lit("clear-cflags"), lit("clear-lflags"),
    lit("go-raw"), lit("go-cbreak"), lit("go-canon"),
    lit("string-encode"), lit("string-decode"), nil
  };
  set_dlt_entries(dlt, name, fun);
  return nil;
}

static val termios_instantiate(val set_fun)
{
  funcall1(set_fun, nil);
  load(format(nil, lit("~atermios.tl"), stdlib_path, nao));
  sock_load_init();
  return nil;
}

#endif

static val awk_set_entries(val dlt, val fun)
{
  val name[] = {
    lit("awk"), nil
  };
  set_dlt_entries(dlt, name, fun);
  return nil;
}

static val awk_instantiate(val set_fun)
{
  funcall1(set_fun, nil);
  load(format(nil, lit("~aawk.tl"), stdlib_path, nao));
  sock_load_init();
  return nil;
}

static val build_set_entries(val dlt, val fun)
{
  val name[] = {
    lit("list-builder"), lit("build-list"), lit("build"), nil
  };
  set_dlt_entries(dlt, name, fun);
  return nil;
}

static val build_instantiate(val set_fun)
{
  funcall1(set_fun, nil);
  load(format(nil, lit("~abuild.tl"), stdlib_path, nao));
  sock_load_init();
  return nil;
}

static val trace_set_entries(val dlt, val fun)
{
  val name[] = {
    lit("*trace-output*"), lit("trace"), lit("untrace"), nil
  };
  set_dlt_entries(dlt, name, fun);
  return nil;
}

static val trace_instantiate(val set_fun)
{
  funcall1(set_fun, nil);
  load(format(nil, lit("~atrace.tl"), stdlib_path, nao));
  trace_loaded = t;
  return nil;
}

static val getopts_set_entries(val dlt, val fun)
{
  val name[] = {
    lit("opt-desc"), lit("opts"),
    lit("opt"), lit("getopts"), lit("opthelp"),
    nil
  };
  set_dlt_entries(dlt, name, fun);
  return nil;
}

static val getopts_instantiate(val set_fun)
{
  funcall1(set_fun, nil);
  load(format(nil, lit("~agetopts.tl"), stdlib_path, nao));
  return nil;
}

static val package_set_entries(val dlt, val fun)
{
  val name[] = {
    lit("defpackage"), lit("in-package"),
    nil
  };
  set_dlt_entries(dlt, name, fun);
  return nil;
}

static val package_instantiate(val set_fun)
{
  funcall1(set_fun, nil);
  load(format(nil, lit("~apackage.tl"), stdlib_path, nao));
  return nil;
}

static val getput_set_entries(val dlt, val fun)
{
  val name[] = {
    lit("file-get"), lit("file-put"), lit("file-append"),
    lit("file-get-string"), lit("file-put-string"), lit("file-append-string"),
    lit("file-get-lines"), lit("file-put-lines"), lit("file-append-lines"),
    lit("command-get"), lit("command-put"),
    lit("command-get-string"), lit("command-put-string"),
    lit("command-get-lines"), lit("command-put-lines"),
    nil
  };
  set_dlt_entries(dlt, name, fun);
  return nil;
}

static val getput_instantiate(val set_fun)
{
  funcall1(set_fun, nil);
  load(format(nil, lit("~agetput.tl"), stdlib_path, nao));
  return nil;
}

static val tagbody_set_entries(val dlt, val fun)
{
  val name[] = {
    lit("tagbody"), lit("go"), lit("prog"), lit("prog*"), nil
  };
  set_dlt_entries(dlt, name, fun);
  return nil;
}

static val tagbody_instantiate(val set_fun)
{
  funcall1(set_fun, nil);
  load(format(nil, lit("~atagbody.tl"), stdlib_path, nao));
  return nil;
}

static val pmac_set_entries(val dlt, val fun)
{
  val name[] = {
    lit("define-param-expander"), nil
  };
  set_dlt_entries(dlt, name, fun);
  return nil;
}

static val pmac_instantiate(val set_fun)
{
  funcall1(set_fun, nil);
  load(format(nil, lit("~apmac.tl"), stdlib_path, nao));
  return nil;
}

static val error_set_entries(val dlt, val fun)
{
  val name[] = {
    lit("compile-error"), lit("compile-warning"), lit("compile-defr-warning"),
    nil
  };
  set_dlt_entries(dlt, name, fun);
  return nil;
}

static val error_instantiate(val set_fun)
{
  funcall1(set_fun, nil);
  load(format(nil, lit("~aerror.tl"), stdlib_path, nao));
  return nil;
}

static val keyparams_set_entries(val dlt, val fun)
{
  val key_k = intern(lit("key"), keyword_package);
  if (fun)
    sethash(dlt, key_k, fun);
  else
    remhash(dlt, key_k);
  return nil;
}

static val keyparams_instantiate(val set_fun)
{
  funcall1(set_fun, nil);
  load(format(nil, lit("~akeyparams.tl"), stdlib_path, nao));
  return nil;
}

val dlt_register(val dlt,
                 val (*instantiate)(val),
                 val (*set_entries)(val, val))
{
  return set_entries(dl_table, func_f0(func_f1(dlt, set_entries), instantiate));
}

void lisplib_init(void)
{
  prot1(&dl_table);
  dl_table = make_hash(nil, nil, nil);
  dlt_register(dl_table, place_instantiate, place_set_entries);
  dlt_register(dl_table, ver_instantiate, ver_set_entries);
  dlt_register(dl_table, ifa_instantiate, ifa_set_entries);
  dlt_register(dl_table, txr_case_instantiate, txr_case_set_entries);
  dlt_register(dl_table, with_resources_instantiate, with_resources_set_entries);
  dlt_register(dl_table, path_test_instantiate, path_test_set_entries);
  dlt_register(dl_table, struct_instantiate, struct_set_entries);
  dlt_register(dl_table, with_stream_instantiate, with_stream_set_entries);
  dlt_register(dl_table, hash_instantiate, hash_set_entries);
  dlt_register(dl_table, except_instantiate, except_set_entries);
  dlt_register(dl_table, type_instantiate, type_set_entries);
  dlt_register(dl_table, yield_instantiate, yield_set_entries);
#if HAVE_SOCKETS
  dlt_register(dl_table, sock_instantiate, sock_set_entries);
#endif
#if HAVE_TERMIOS
  dlt_register(dl_table, termios_instantiate, termios_set_entries);
#endif
  dlt_register(dl_table, awk_instantiate, awk_set_entries);
  dlt_register(dl_table, build_instantiate, build_set_entries);
  dlt_register(dl_table, trace_instantiate, trace_set_entries);
  dlt_register(dl_table, getopts_instantiate, getopts_set_entries);
  dlt_register(dl_table, package_instantiate, package_set_entries);
  dlt_register(dl_table, getput_instantiate, getput_set_entries);
  dlt_register(dl_table, tagbody_instantiate, tagbody_set_entries);
  dlt_register(dl_table, pmac_instantiate, pmac_set_entries);
  dlt_register(dl_table, error_instantiate, error_set_entries);
  dlt_register(dl_table, keyparams_instantiate, keyparams_set_entries);
}

val lisplib_try_load(val sym)
{
  val fun = gethash(dl_table, sym);

  if (fun) {
     debug_state_t ds = debug_set_state(opt_dbg_autoload ? 0 : -1, opt_dbg_autoload);
     val saved_dyn_env = dyn_env;
     dyn_env = make_env(nil, nil, dyn_env);
     env_vbind(dyn_env, package_s, user_package);
     funcall(fun);
     dyn_env = saved_dyn_env;
     debug_restore_state(ds);
     return t;
  }
  return nil;
}
