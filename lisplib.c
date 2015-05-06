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
#include "place.h"
#include "lisplib.h"

static val dl_table;

static void set_place_dlt_entries(val dlt, val fun)
{
  int i;
  val dl_sym[] = {
    lit("*place-clobber-expander*"), lit("*place-update-expander*"),
    lit("*place-delete-expander*"),
    lit("get-update-expander"), lit("get-clobber-expander"),
    lit("get-delete-expander"),
    lit("rlet"), lit("with-gensyms"),
    lit("call-update-expander"), lit("call-clobber-expander"),
    lit("call-delete-expander)"),
    lit("with-update-expander"), lit("with-clobber-expander"),
    lit("with-delete-expander"),
    lit("set"), lit("zap"), lit("flip"), lit("inc"), lit("dec"),
    lit("push"), lit("pop"), lit("swap"), lit("shift"), lit("rotate"),
    lit("del"),
    nil
  };

  for (i = 0; dl_sym[i]; i++)
    sethash(dlt, intern(dl_sym[i], user_package), fun);
}

static val place_instantiate(val dlt)
{
  set_place_dlt_entries(dlt, nil);
  return eval_intrinsic(lisp_parse(static_str(place_code), std_error, nil), nil);
}

void lisplib_init(void)
{
  prot1(&dl_table);
  dl_table = make_hash(nil, nil, nil);
  set_place_dlt_entries(dl_table, func_f0(dl_table, place_instantiate));
}

val lisplib_try_load(val sym)
{
  val fun = gethash(dl_table, sym);
  return if3(fun, (funcall(fun), t), nil);
}
