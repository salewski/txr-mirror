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
#include "txr.h"
#include "lisplib.h"

val dl_table;

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
    lit("*place-delete-expander*"),
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
    lit("defplace"), lit("define-modify-macro"),
    lit("placelet"), lit("placelet*"),
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
}

val lisplib_try_load(val sym)
{
  val fun = gethash(dl_table, sym);
  return if3(fun, (funcall(fun), t), nil);
}
