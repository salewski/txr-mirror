/* Copyright 2009-2015
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

#include <stddef.h>
#include <setjmp.h>
#include <signal.h>
#include "config.h"
#include "lib.h"
#include "signal.h"
#include "unwind.h"
#include ALLOCA_H
#include "args.h"

val args_cons_list(struct args *args);

cnum args_limit(val name, cnum in)
{
  if (in <= ARGS_MAX)
    return in;
  uw_throwf(assert_s, lit("~a: too many trailing arguments (limit is ~s)"),
            name, num(ARGS_MAX), nao);
}

void args_add_list(struct args *args, val list)
{
  for (; list; list = cdr(list))
    args_add(args, car(list));
}

val args_add_checked(val name, struct args *args, val arg)
{
  if (args->fill >= args->argc)
    uw_throwf(assert_s, lit("~a: argument list size exceeded"), name, nao);
  return args_add(args, arg);
}

val args_cons_list(struct args *args)
{
  cnum i;
  list_collect_decl (out, ptail);

  for (i = 0; i < args->argc; i++)
    ptail = list_collect(ptail, args->arg[i]);

  return args->list = out;
}

val args_get_checked(val name, struct args *args, cnum *arg_index)
{
  if (args->fill == 0 && args->list)
    args_add_list(args, args->list);
  if (*arg_index >= args->fill)
    uw_throwf(assert_s, lit("~a: insufficient arguments"), name, nao);
  return args_get(args, arg_index);
}
