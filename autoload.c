/* Copyright 2015-2023
 * Kaz Kylheku <kaz@kylheku.com>
 * Vancouver, Canada
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

#include <stdio.h>
#include <wchar.h>
#include <stdarg.h>
#include <signal.h>
#include "config.h"
#include "lib.h"
#include "eval.h"
#include "stream.h"
#include "hash.h"
#include "gc.h"
#include "debug.h"
#include "txr.h"
#include "autoload.h"

int opt_dbg_autoload;
val trace_loaded;

static val autoload_hash[al_max + 1];
static val autoload_reg_hash;

static void autload_set_impl(al_ns_t ns, val *name, val fun, val package)
{
  for (; *name; name++) {
    val sym = intern(*name, package);

    if (fun)
      sethash(autoload_hash[ns], sym, fun);
    else
      remhash(autoload_hash[ns], sym);
  }
}

void autoload_set(al_ns_t ns, val *name, val fun)
{
  autload_set_impl(ns, name, fun, user_package);
}

static void autoload_sys_set(al_ns_t ns, val *name, val fun)
{
  autload_set_impl(ns, name, fun, system_package);
}

static void autoload_key_set(al_ns_t ns, val *name, val fun)
{
  autload_set_impl(ns, name, fun, keyword_package);
}

static void intern_only(val *name)
{
  for (; *name; name++)
    intern(*name, user_package);
}

static val place_set_entries(val fun)
{
  val sys_name[] = {
    lit("get-fun-getter-setter"), lit("get-mb"), lit("get-vb"),
    lit("register-simple-accessor"),
    nil
  };
  val vname[] = {
    lit("*place-clobber-expander*"), lit("*place-update-expander*"),
    lit("*place-delete-expander*"), lit("*place-macro*"),
    nil
  };
  val name[] = {
    lit("get-update-expander"), lit("get-clobber-expander"),
    lit("get-delete-expander"),
    lit("place-form-p"),
    lit("rlet"), lit("slet"), lit("alet"), lit("with-gensyms"),
    lit("call-update-expander"), lit("call-clobber-expander"),
    lit("call-delete-expander"),
    lit("with-update-expander"), lit("with-clobber-expander"),
    lit("with-delete-expander"),
    lit("set"), lit("pset"), lit("zap"), lit("flip"), lit("inc"), lit("dec"),
    lit("pinc"), lit("pdec"),
    lit("push"), lit("pop"), lit("swap"), lit("shift"), lit("rotate"),
    lit("test-set"), lit("test-clear"), lit("compare-swap"),
    lit("test-inc"), lit("test-dec"),
    lit("pushnew"), lit("del"), lit("lset"), lit("upd"),
    lit("defplace"), lit("define-place-macro"), lit("define-modify-macro"),
    lit("placelet"), lit("placelet*"), lit("read-once"),
    lit("define-accessor"), lit("with-slots"),
    lit("macroexpand-place"), lit("macroexpand-1-place"),
    nil
  };

  autoload_sys_set(al_fun, sys_name, fun);
  autoload_set(al_var, vname, fun);
  autoload_set(al_fun, name, fun);
  return nil;
}

static val place_instantiate(void)
{
  load(scat2(stdlib_path, lit("place")));
  return nil;
}

static val ver_set_entries(val fun)
{
  val vname[] = { lit("*lib-version*"), lit("lib-version"), nil };
  autoload_set(al_var, vname, fun);
  return nil;
}

static val ver_instantiate(void)
{
  load(scat2(stdlib_path, lit("ver")));
  return nil;
}

static val ifa_set_entries(val fun)
{
  val name[] = {
    lit("ifa"), lit("whena"), lit("conda"), lit("condlet"), lit("it"), nil
  };
  autoload_set(al_fun, name, fun);
  return nil;
}

static val ifa_instantiate(void)
{
  load(scat2(stdlib_path, lit("ifa")));
  return nil;
}

static val txr_case_set_entries(val fun)
{
  val name[] = { lit("txr-if"), lit("txr-when"), lit("txr-case"), nil };
  autoload_set(al_fun, name, fun);
  return nil;
}

static val txr_case_instantiate(void)
{
  load(scat2(stdlib_path, lit("txr-case")));
  return nil;
}

static val with_resources_set_entries(val fun)
{
  val name[] = {
    lit("with-resources"),
    lit("with-objects"),
    nil
  };
  autoload_set(al_fun, name, fun);
  return nil;
}

static val with_resources_instantiate(void)
{
  load(scat2(stdlib_path, lit("with-resources")));
  return nil;
}

static val path_test_set_entries(val fun)
{
  val name[] = {
    lit("path-exists-p"), lit("path-file-p"), lit("path-dir-p"),
    lit("path-symlink-p"), lit("path-blkdev-p"), lit("path-chrdev-p"),
    lit("path-sock-p"), lit("path-pipe-p"), lit("path-pipe-p"),
    lit("path-setgid-p"), lit("path-setuid-p"), lit("path-sticky-p"),
    lit("path-mine-p"), lit("path-my-group-p"), lit("path-executable-to-me-p"),
    lit("path-writable-to-me-p"), lit("path-readable-to-me-p"),
    lit("path-read-writable-to-me-p"),
    lit("path-safe-sticky-dir"), lit("path-components-safe"),
    lit("path-newer"), lit("path-older"),
    lit("path-same-object"), lit("path-private-to-me-p"),
    lit("path-strictly-private-to-me-p"),
    lit("path-dir-empty"),
    lit("rel-path"), lit("path-equal"),
    nil
  };
  autoload_set(al_fun, name, fun);
  return nil;
}

static val path_test_instantiate(void)
{
  load(scat2(stdlib_path, lit("path-test")));
  return nil;
}

static val struct_set_entries(val fun)
{
  val sys_name[] = {
    lit("define-method"), lit("rslotset"), nil
  };
  val name[] = {
    lit("defstruct"), lit("qref"), lit("uref"), lit("new"), lit("lnew"),
    lit("new*"), lit("lnew*"),
    lit("meth"), lit("umeth"), lit("usl"), lit("defmeth"), lit("rslot"),
    lit("define-struct-prelude"),
    lit("define-struct-clause"), lit("macroexpand-struct-clause"), nil
  };
  val vname[] = {
    lit("*struct-clause-expander*"), nil
  };

  autoload_sys_set(al_fun, sys_name, fun);
  autoload_set(al_fun, name, fun);
  autoload_set(al_var, vname, fun);

  if (fun)
    sethash(autoload_hash[al_fun], struct_lit_s, fun);
  else
    remhash(autoload_hash[al_fun], struct_lit_s);

  return nil;
}

static val struct_instantiate(void)
{
  load(scat2(stdlib_path, lit("struct")));
  return nil;
}

static val with_stream_set_entries(val fun)
{
  val name[] = {
    lit("with-out-string-stream"),
    lit("with-out-strlist-stream"),
    lit("with-out-buf-stream"),
    lit("with-in-string-stream"),
    lit("with-in-string-byte-stream"),
    lit("with-in-buf-stream"),
    lit("with-stream"),
    nil
  };
  autoload_set(al_fun, name, fun);
  return nil;
}

static val with_stream_instantiate(void)
{
  load(scat2(stdlib_path, lit("with-stream")));
  return nil;
}

static val hash_set_entries(val fun)
{
  val name[] = { lit("with-hash-iter"), nil };
  autoload_set(al_fun, name, fun);
  return nil;
}

static val hash_instantiate(void)
{
  load(scat2(stdlib_path, lit("hash")));
  return nil;
}

static val except_set_entries(val fun)
{
  val name[] = {
    lit("catch"), lit("catch*"), lit("catch**"), lit("handle"), lit("handle*"),
    lit("ignwarn"), lit("macro-time-ignwarn"),
    nil
  };
  autoload_set(al_fun, name, fun);
  return nil;
}

static val except_instantiate(void)
{
  load(scat2(stdlib_path, lit("except")));
  return nil;
}

static val type_set_entries(val fun)
{
  val name[] = {
    lit("typecase"), lit("etypecase"), nil
  };
  autoload_set(al_fun, name, fun);
  return nil;
}

static val type_instantiate(void)
{
  load(scat2(stdlib_path, lit("type")));
  return nil;
}

static val yield_set_entries(val fun)
{
  val sys_name[] = {
    lit("obtain-impl"), nil
  };
  val name[] = {
    lit("obtain"), lit("obtain-block"), lit("yield-from"), lit("yield"),
    lit("obtain*"), lit("obtain*-block"),
    lit("suspend"), lit("hlet"), lit("hlet*"),
    nil
  };
  autoload_sys_set(al_fun, sys_name, fun);
  autoload_set(al_fun, name, fun);
  return nil;
}

static val yield_instantiate(void)
{
  load(scat2(stdlib_path, lit("yield")));
  return nil;
}

static val awk_set_entries(val fun)
{
  val sys_sname[] = {
    lit("awk-state"), nil
  };
  val name[] = {
    lit("awk"), nil
  };
  val name_noload[] = {
    lit("rec"), lit("orec"), lit("f"), lit("nf"), lit("nr"), lit("fnr"),
    lit("arg"), lit("fname"), lit("rs"), lit("krs"), lit("fs"), lit("ft"),
    lit("fw"), lit("kfs"), lit("ofs"), lit("ors"), lit("next"), lit("again"),
    lit("next-file"), lit("rng"), lit("-rng"), lit("rng-"), lit("-rng-"),
    lit("--rng"), lit("--rng-"), lit("rng+"), lit("-rng+"), lit("--rng+"),
    lit("ff"), lit("f"), lit("mf"), lit("fconv"), lit("->"), lit("->>"),
    lit("<-"), lit("!>"), lit("<!"), lit("prn"),
    lit("i"), lit("o"), lit("x"), lit("b"), lit("c"), lit("r"),
    lit("iz"), lit("oz"), lit("xz"), lit("bz"), lit("cz"), lit("rz"),
    lit("res"),
    nil
  };
  autoload_sys_set(al_struct, sys_sname, fun);
  autoload_set(al_fun, name, fun);
  intern_only(name_noload);
  return nil;
}

static val awk_instantiate(void)
{
  load(scat2(stdlib_path, lit("awk")));
  return nil;
}

static val build_set_entries(val fun)
{
  val sname[] = {
    lit("list-builder"), nil
  };
  val name[] = {
    lit("build-list"), lit("build"), lit("buildn"), nil
  };
  val name_noload[] = {
    lit("head"), lit("tail"), lit("add"), lit("add*"), lit("pend"),
    lit("pend*"), lit("ncon"), lit("ncon*"), lit("get"),
    lit("del"), lit("del*"), lit("oust"),
    nil
  };
  autoload_set(al_struct, sname, fun);
  autoload_set(al_fun, name, fun);
  intern_only(name_noload);
  return nil;
}

static val build_instantiate(void)
{
  load(scat2(stdlib_path, lit("build")));
  return nil;
}

static val trace_set_entries(val fun)
{
  val sys_name[] = {
    lit("trace"), lit("untrace"), nil
  };
  val vname[] = {
    lit("*trace-output*"), nil
  };
  val name[] = {
    lit("trace"), lit("untrace"), nil
  };
  autoload_sys_set(al_fun, sys_name, fun);
  autoload_set(al_var, vname, fun);
  autoload_set(al_fun, name, fun);
  return nil;
}

static val trace_instantiate(void)
{
  load(scat2(stdlib_path, lit("trace")));
  trace_loaded = t;
  return nil;
}

static val getopts_set_entries(val fun)
{
  val sname[] = {
    lit("opt-desc"), lit("opts"), nil
  };
  val name[] = {
    lit("opt"), lit("getopts"), lit("opthelp"), lit("opthelp-conventions"),
    lit("opthelp-types"), lit("define-option-struct"),
    nil
  };
  val name_noload[] = {
    lit("short"), lit("long"), lit("helptext"), lit("type"),
    lit("in-args"), lit("out-args"), lit("cumul"), lit("opt-error"), nil
  };
  autoload_set(al_struct, sname, fun);
  autoload_set(al_fun, name, fun);
  intern_only(name_noload);
  return nil;
}

static val getopts_instantiate(void)
{
  load(scat2(stdlib_path, lit("getopts")));
  return nil;
}

static val package_set_entries(val fun)
{
  val name[] = {
    lit("defpackage"), lit("in-package"),
    nil
  };
  autoload_set(al_fun, name, fun);
  return nil;
}

static val package_instantiate(void)
{
  load(scat2(stdlib_path, lit("package")));
  return nil;
}

static val getput_set_entries(val fun)
{
  val name[] = {
    lit("get-jsons"), lit("put-jsons"),
    lit("file-get"), lit("file-put"), lit("file-append"),
    lit("file-get-string"), lit("file-put-string"), lit("file-append-string"),
    lit("file-get-lines"), lit("file-put-lines"), lit("file-append-lines"),
    lit("file-get-buf"), lit("file-put-buf"),
    lit("file-place-buf"), lit("file-append-buf"),
    lit("file-get-json"), lit("file-put-json"), lit("file-append-json"),
    lit("file-get-jsons"), lit("file-put-jsons"), lit("file-append-jsons"),
    lit("command-get"), lit("command-put"),
    lit("command-get-string"), lit("command-put-string"),
    lit("command-get-lines"), lit("command-put-lines"),
    lit("command-get-buf"), lit("command-put-buf"),
    lit("command-get-json"), lit("command-put-json"),
    lit("command-get-jsons"), lit("command-put-jsons"),
    lit("close-lazy-streams"),
    nil
  };
  autoload_set(al_fun, name, fun);
  return nil;
}

static val getput_instantiate(void)
{
  load(scat2(stdlib_path, lit("getput")));
  return nil;
}

static val tagbody_set_entries(val fun)
{
  val name[] = {
    lit("tagbody"), lit("go"), lit("prog"), lit("prog*"), nil
  };
  autoload_set(al_fun, name, fun);
  return nil;
}

static val tagbody_instantiate(void)
{
  load(scat2(stdlib_path, lit("tagbody")));
  return nil;
}

static val pmac_set_entries(val fun)
{
  val name[] = {
    lit("define-param-expander"), lit("macroexpand-params"), nil
  };
  autoload_set(al_fun, name, fun);
  return nil;
}

static val pmac_instantiate(void)
{
  load(scat2(stdlib_path, lit("pmac")));
  return nil;
}

static val error_set_entries(val fun)
{
  val sys_name[] = {
    lit("bind-mac-error"), lit("bind-mac-check"),
    lit("lambda-too-many-args"), lit("lambda-too-few-args"),
    lit("lambda-short-apply-list"), lit("lambda-excess-apply-list"),
    nil
  };
  val name[] = {
    lit("compile-error"), lit("compile-warning"), lit("compile-defr-warning"),
    nil
  };
  autoload_sys_set(al_fun, sys_name, fun);
  autoload_set(al_fun, name, fun);
  return nil;
}

static val error_instantiate(void)
{
  load(scat2(stdlib_path, lit("error")));
  return nil;
}

static val keyparams_set_entries(val fun)
{
  val sys_name[] = {
    lit("extract-keys"), nil
  };
  val sys_kname[] = {
    lit("key"), nil
  };
  val name_noload[] = {
    lit("--"),
    nil
  };
  autoload_sys_set(al_fun, sys_name, fun);
  autoload_key_set(al_key, sys_kname, fun);
  intern_only(name_noload);
  return nil;
}

static val keyparams_instantiate(void)
{
  load(scat2(stdlib_path, lit("keyparams")));
  return nil;
}

static val ffi_set_entries(val fun)
{
  val name[] = {
    lit("with-dyn-lib"), lit("deffi"), lit("deffi-type"), lit("deffi-cb"),
    lit("deffi-cb-unsafe"),
    lit("deffi-sym"), lit("deffi-var"), lit("deffi-struct"),
    lit("deffi-union"), lit("typedef"), lit("sizeof"),
    lit("alignof"), lit("offsetof"), lit("arraysize"), lit("elemsize"),
    lit("elemtype"), lit("ffi"), lit("carray-ref"), lit("carray-sub"),
    lit("sub-buf"), lit("znew"),
    nil
  };
  autoload_set(al_fun, name, fun);
  return nil;
}

static val ffi_instantiate(void)
{
  load(scat2(stdlib_path, lit("ffi")));
  return nil;
}

static val doloop_set_entries(val fun)
{
  val name[] = {
    lit("doloop"), lit("doloop*"),
    nil
  };
  autoload_set(al_fun, name, fun);
  return nil;
}

static val doloop_instantiate(void)
{
  load(scat2(stdlib_path, lit("doloop")));
  return nil;
}

static val stream_wrap_set_entries(val fun)
{
  val sname[] = {
    lit("stream-wrap"),
    nil
  };
  val name_noload[] = {
    lit("close"), lit("flush"), lit("seek"), lit("truncate"),
    lit("get-prop"), lit("set-prop"), lit("get-fd"), nil
  };
  autoload_set(al_struct, sname, fun);
  intern_only(name_noload);
  return nil;
}

static val stream_wrap_instantiate(void)
{
  load(scat2(stdlib_path, lit("stream-wrap")));
  return nil;
}

static val asm_set_entries(val fun)
{
  val sys_sname[] = {
    lit("assembler"),
    nil
  };
  val name[] = {
    lit("disassemble"),
    nil
  };
  autoload_sys_set(al_struct, sys_sname, fun);
  autoload_set(al_fun, name, fun);
  return nil;
}

static val asm_instantiate(void)
{
  load(scat2(stdlib_path, lit("asm")));
  return nil;
}

static val compiler_set_entries(val fun)
{
  val sys_name[] = {
    lit("compiler"), lit("*in-compilation-unit*"),
    nil
  };
  val sname[] = {
    lit("compile-opts"),
    nil
  };
  val name[] = {
    lit("compile-toplevel"), lit("compile"), lit("compile-file"),
    lit("compile-update-file"), lit("clean-file"),
    lit("with-compilation-unit"), lit("dump-compiled-objects"),
    lit("with-compile-opts"), lit("compiler-let"),
    nil
  };
  val sys_vname[] = {
    lit("*in-compilation-unit*"),
    nil
  };
  val vname[] = {
    lit("*opt-level*"), lit("*compile-opts*"),
    nil
  };
  val slname[] = {
    lit("shadow-fun"), lit("shadow-var"), lit("shadow-cross"),
    lit("unused"), lit("log-level"), nil
  };
  autoload_sys_set(al_struct, sys_name, fun);
  autoload_set(al_struct, sname, fun);
  autoload_set(al_fun, name, fun);
  autoload_sys_set(al_var, sys_vname, fun);
  autoload_set(al_var, vname, fun);
  autoload_set(al_slot, slname, fun);
  return nil;
}

static val compiler_instantiate(void)
{
  load(scat2(stdlib_path, lit("compiler")));
  return nil;
}

static val debugger_set_entries(val fun)
{
  val sys_name[] = {
    lit("debugger"), lit("print-backtrace"),
    nil
  };
  autoload_sys_set(al_fun, sys_name, fun);
  return nil;
}

static val debugger_instantiate(void)
{
  load(scat2(stdlib_path, lit("debugger")));
  return nil;
}

static val op_set_entries(val fun)
{
  val name[] = {
    lit("op"), lit("do"), lit("lop"), lit("ldo"), lit("ap"), lit("ip"),
    lit("ado"), lit("ido"), lit("ret"), lit("aret"),
    lit("opip"), lit("oand"), lit("flow"),
    nil
  };
  autoload_set(al_fun, name, fun);
  return nil;
}

static val op_instantiate(void)
{
  load(scat2(stdlib_path, lit("op")));
  return nil;
}

static val save_exe_set_entries(val fun)
{
  val name[] = {
    lit("save-exe"),
    nil
  };

  autoload_set(al_fun, name, fun);
  return nil;
}

static val save_exe_instantiate(void)
{
  load(scat2(stdlib_path, lit("save-exe")));
  return nil;
}

static val defset_set_entries(val fun)
{
  val name[] = {
    lit("defset"), lit("sub-list"), lit("sub-vec"), lit("sub-str"),
    lit("left"), lit("right"), lit("key"),
    lit("set-mask"), lit("clear-mask"),
    nil
  };
  autoload_set(al_fun, name, fun);
  return nil;
}

static val defset_instantiate(void)
{
  load(scat2(stdlib_path, lit("defset")));
  return nil;
}

static val copy_file_set_entries(val fun)
{
  val sname[] = {
    lit("copy-path-opts"),
    nil
  };
  val name[] = {
    lit("copy-file"), lit("copy-files"), lit("cat-files"),
    lit("copy-path-rec"), lit("remove-path-rec"),
    lit("chown-rec"), lit("chmod-rec"), lit("touch"),
    nil
  };
  autoload_set(al_struct, sname, fun);
  autoload_set(al_fun, name, fun);
  return nil;
}

static val copy_file_instantiate(void)
{
  load(scat2(stdlib_path, lit("copy-file")));
  return nil;
}

static val arith_each_set_entries(val fun)
{
  val name[] = {
    lit("sum-each"), lit("mul-each"), lit("sum-each*"), lit("mul-each*"),
    lit("each-true"), lit("some-true"), lit("each-false"), lit("some-false"),
    nil
  };
  autoload_set(al_fun, name, fun);
  return nil;
}

static val arith_each_instantiate(void)
{
  load(scat2(stdlib_path, lit("arith-each")));
  return nil;
}

static val each_prod_set_entries(val fun)
{
  val name[] = {
    lit("each-prod"), lit("collect-each-prod"), lit("append-each-prod"),
    lit("sum-each-prod"), lit("mul-each-prod"),
    lit("each-prod*"), lit("collect-each-prod*"), lit("append-each-prod*"),
    lit("sum-each-prod*"), lit("mul-each-prod*"),
    nil
  };
  autoload_set(al_fun, name, fun);
  return nil;
}

static val each_prod_instantiate(void)
{
  load(scat2(stdlib_path, lit("each-prod")));
  return nil;
}

static val quips_set_entries(val fun)
{
  val name[] = {
    lit("quip"), nil
  };
  autoload_set(al_fun, name, fun);
  return nil;
}

static val quips_instantiate(void)
{
  load(scat2(stdlib_path, lit("quips")));
  return nil;
}

static val match_set_entries(val fun)
{
  val name_noload[] = {
    lit("all*"), lit("as"), lit("with"), lit("scan"), lit("sme"), lit("match-error"),
    nil
  };
  val name[] = {
    lit("when-match"), lit("match-case"), lit("if-match"),
    lit("match"), lit("match-ecase"),
    lit("while-match"), lit("while-match-case"), lit("while-true-match-case"),
    lit("lambda-match"), lit("defun-match"), lit("defmatch"),
    lit("macroexpand-match"),
    lit("each-match"), lit("append-matches"),
    lit("keep-matches"), lit("each-match-product"),
    lit("append-match-products"), lit("keep-match-products"),
    nil
  };
  val vname[] = {
    lit("*match-macro*"),
    nil
  };
  val kname[] = {
    lit("match"),
    nil
  };

  autoload_set(al_fun, name, fun);
  autoload_set(al_var, vname, fun);
  autoload_key_set(al_key, kname, fun);
  intern_only(name_noload);
  return nil;
}

static val match_instantiate(void)
{
  load(scat2(stdlib_path, lit("match")));
  return nil;
}

static val doc_set_entries(val fun)
{
  val name[] = {
    lit("doc"), nil
  };
  val vname[] = {
    lit("*doc-url*"), nil
  };
  autoload_set(al_fun, name, fun);
  autoload_set(al_var, vname, fun);
  return nil;
}

static val doc_instantiate(void)
{
  load(scat2(stdlib_path, lit("doc-lookup")));
  return nil;
}

static val pic_set_entries(val fun)
{
  val name[] = {
    lit("pic"),
    nil
  };
  autoload_set(al_fun, name, fun);
  return nil;
}

static val pic_instantiate(void)
{
  load(scat2(stdlib_path, lit("pic")));
  return nil;
}

static val constfun_set_entries(val fun)
{
  val sys_vname[] = {
    lit("%const-foldable%"),
    nil
  };
  autoload_sys_set(al_var, sys_vname, fun);
  return nil;
}

static val constfun_instantiate(void)
{
  load(scat2(stdlib_path, lit("constfun")));
  return nil;
}

static val expander_let_set_entries(val fun)
{
  val sys_name[] = {
    lit("expander-let"),
    nil
  };
  autoload_set(al_fun, sys_name, fun);
  return nil;
}

static val expander_let_instantiate(void)
{
  load(scat2(stdlib_path, lit("expander-let")));
  return nil;
}

val autoload_reg(val (*instantiate)(void),
                 val (*set_entries)(val))
{
  val fun = func_n0(instantiate);
  set_entries(fun);
  return sethash(autoload_reg_hash, fun, t);
}

static void autoload_init_tables(void)
{
  int i;

  prot1(&autoload_reg_hash);

  for (i = 0; i <= al_max; i++) {
    autoload_hash[i] = make_hash(hash_weak_or, nil);
    prot1(&autoload_hash[i]);
  }

  autoload_reg_hash = make_hash(hash_weak_none, nil);
}

void autoload_init(void)
{
  autoload_init_tables();
  autoload_reg(place_instantiate, place_set_entries);
  autoload_reg(ver_instantiate, ver_set_entries);
  autoload_reg(ifa_instantiate, ifa_set_entries);
  autoload_reg(txr_case_instantiate, txr_case_set_entries);
  autoload_reg(with_resources_instantiate, with_resources_set_entries);
  autoload_reg(path_test_instantiate, path_test_set_entries);
  autoload_reg(struct_instantiate, struct_set_entries);
  autoload_reg(with_stream_instantiate, with_stream_set_entries);
  autoload_reg(hash_instantiate, hash_set_entries);
  autoload_reg(except_instantiate, except_set_entries);
  autoload_reg(type_instantiate, type_set_entries);
  autoload_reg(yield_instantiate, yield_set_entries);
  autoload_reg(awk_instantiate, awk_set_entries);
  autoload_reg(build_instantiate, build_set_entries);
  autoload_reg(trace_instantiate, trace_set_entries);
  autoload_reg(getopts_instantiate, getopts_set_entries);
  autoload_reg(package_instantiate, package_set_entries);
  autoload_reg(getput_instantiate, getput_set_entries);
  autoload_reg(tagbody_instantiate, tagbody_set_entries);
  autoload_reg(pmac_instantiate, pmac_set_entries);
  autoload_reg(error_instantiate, error_set_entries);
  autoload_reg(keyparams_instantiate, keyparams_set_entries);
  autoload_reg(ffi_instantiate, ffi_set_entries);
  autoload_reg(doloop_instantiate, doloop_set_entries);
  autoload_reg(stream_wrap_instantiate, stream_wrap_set_entries);
  autoload_reg(asm_instantiate, asm_set_entries);
  autoload_reg(compiler_instantiate, compiler_set_entries);
  autoload_reg(debugger_instantiate, debugger_set_entries);

  if (!opt_compat || opt_compat >= 185)
    autoload_reg(op_instantiate, op_set_entries);

  autoload_reg(save_exe_instantiate, save_exe_set_entries);
  autoload_reg(defset_instantiate, defset_set_entries);
  autoload_reg(copy_file_instantiate, copy_file_set_entries);
  autoload_reg(arith_each_instantiate, arith_each_set_entries);
  autoload_reg(each_prod_instantiate, each_prod_set_entries);
  autoload_reg(quips_instantiate, quips_set_entries);
  autoload_reg(match_instantiate, match_set_entries);
  autoload_reg(doc_instantiate, doc_set_entries);
  autoload_reg(pic_instantiate, pic_set_entries);
  autoload_reg(constfun_instantiate, constfun_set_entries);
  autoload_reg(expander_let_instantiate, expander_let_set_entries);

  reg_fun(intern(lit("autoload-try-fun"), system_package), func_n1(autoload_try_fun));
}

static val autoload_try(al_ns_t ns, val sym)
{
  val fun = gethash(autoload_hash[ns], sym);

  if (fun) {
    val check = gethash(autoload_reg_hash, fun);

    if (check) {
      unsigned ds = debug_clear(opt_dbg_autoload ? 0 : DBG_ENABLE);
      val saved_dyn_env = dyn_env;
      int saved_compat = opt_compat;
      remhash(autoload_reg_hash, fun);
      dyn_env = make_env(nil, nil, dyn_env);
      env_vbind(dyn_env, package_s, system_package);
      env_vbind(dyn_env, package_alist_s, packages);
      opt_compat = 0;
      funcall(fun);
      opt_compat = saved_compat;
      dyn_env = saved_dyn_env;
      debug_restore(ds);
      return t;
    }
  }
  return nil;
}

val autoload_try_fun(val sym)
{
  return autoload_try(al_fun, sym);
}

val autoload_try_var(val sym)
{
  return autoload_try(al_var, sym);
}

val autoload_try_fun_var(val sym)
{
  uses_or2;
  return or2(autoload_try_fun(sym),
             autoload_try_var(sym));
}

val autoload_try_slot(val sym)
{
  return autoload_try(al_slot, sym);
}

val autoload_try_struct(val sym)
{
  return autoload_try(al_struct, sym);
}

val autoload_try_keyword(val sym)
{
  return autoload_try(al_key, sym);
}

void autoload_intern(val *namearray)
{
  intern_only(namearray);
}
