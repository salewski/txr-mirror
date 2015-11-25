/* Copyright 2015
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
#include <wchar.h>
#include <dirent.h>
#include <stdarg.h>
#include "config.h"
#include "lib.h"
#include "eval.h"
#include "parser.h"
#include "stream.h"
#include "hash.h"
#include "gc.h"
#include "debug.h"
#include "txr.h"
#include "lisplib.h"

val dl_table;
int opt_dbg_autoload;

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
    lit("rlet"), lit("with-gensyms"),
    lit("call-update-expander"), lit("call-clobber-expander"),
    lit("call-delete-expander)"),
    lit("with-update-expander"), lit("with-clobber-expander"),
    lit("with-delete-expander"),
    lit("set"), lit("pset"), lit("zap"), lit("flip"), lit("inc"), lit("dec"),
    lit("push"), lit("pop"), lit("swap"), lit("shift"), lit("rotate"),
    lit("pushnew"), lit("del"),
    lit("defplace"), lit("define-place-macro"), lit("define-modify-macro"),
    lit("placelet"), lit("placelet*"), lit("define-acessor"),
    nil
  };

  set_dlt_entries(dlt, name, fun);
  return nil;
}

static val place_instantiate(val set_fun)
{
  funcall1(set_fun, nil);
  load(format(nil, lit("~a/place.tl"), stdlib_path, nao));
  return nil;
}

static val ver_set_entries(val dlt, val fun)
{
  val name[] = { lit("*lib-version*"), nil };
  set_dlt_entries(dlt, name, fun);
  return nil;
}

static val ver_instantiate(val set_fun)
{
  funcall1(set_fun, nil);
  load(format(nil, lit("~a/ver.tl"), stdlib_path, nao));
  return nil;
}

static val ifa_set_entries(val dlt, val fun)
{
  val name[] = { lit("ifa"), lit("conda"), nil };
  set_dlt_entries(dlt, name, fun);
  return nil;
}

static val ifa_instantiate(val set_fun)
{
  funcall1(set_fun, nil);
  load(format(nil, lit("~a/ifa.tl"), stdlib_path, nao));
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
  load(format(nil, lit("~a/txr-case.tl"), stdlib_path, nao));
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
  load(format(nil, lit("~a/with-resources.tl"), stdlib_path, nao));
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
    lit("path-writable-to-me-p"), lit("path-newer"), lit("path-older"),
    lit("path-same-object"), lit("path-private-to-me-p"),
    nil
  };

  set_dlt_entries(dlt, name, fun);
  return nil;
}

static val path_test_instantiate(val set_fun)
{
  funcall1(set_fun, nil);
  load(format(nil, lit("~a/path-test.tl"), stdlib_path, nao));
  return nil;
}

static val struct_set_entries(val dlt, val fun)
{
  val name[] = {
    lit("defstruct"), lit("qref"), lit("new"), lit("meth"),
    lit("umeth"), lit("usl"), lit("defmeth"), nil
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
  load(format(nil, lit("~a/struct.tl"), stdlib_path, nao));
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
  load(format(nil, lit("~a/with-stream.tl"), stdlib_path, nao));
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
  load(format(nil, lit("~a/hash.tl"), stdlib_path, nao));
  return nil;
}

static val except_set_entries(val dlt, val fun)
{
  val name[] = { lit("handle"), nil };
  set_dlt_entries(dlt, name, fun);
  return nil;
}

static val except_instantiate(val set_fun)
{
  funcall1(set_fun, nil);
  load(format(nil, lit("~a/except.tl"), stdlib_path, nao));
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
  load(format(nil, lit("~a/type.tl"), stdlib_path, nao));
  return nil;
}

static val yield_set_entries(val dlt, val fun)
{
  val name[] = {
    lit("obtain"), lit("obtain-block"), lit("yield-from"), lit("yield"),
    lit("suspend"),
    nil
  };
  set_dlt_entries(dlt, name, fun);
  return nil;
}

static val yield_instantiate(val set_fun)
{
  funcall1(set_fun, nil);
  load(format(nil, lit("~a/yield.tl"), stdlib_path, nao));
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
}

val lisplib_try_load(val sym)
{
  val fun = gethash(dl_table, sym);
  debug_state_t ds;
  return if3(fun, (ds = debug_set_state(opt_dbg_autoload ? 0 : -1,
                                        opt_dbg_autoload),
                   funcall(fun),
                   debug_restore_state(ds), t), nil);
}
