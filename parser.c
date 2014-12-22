/* Copyright 2009-2014
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
#include <assert.h>
#include <limits.h>
#include <dirent.h>
#include <stdlib.h>
#include <stdarg.h>
#include <setjmp.h>
#include <wchar.h>
#include <signal.h>
#include "config.h"
#include "lib.h"
#include "signal.h"
#include "unwind.h"
#include "gc.h"
#include "regex.h"
#include "utf8.h"
#include "match.h"
#include "hash.h"
#include "eval.h"
#include "stream.h"
#include "parser.h"

val parser_s;

static void parser_mark(val obj)
{
  parser_t *p = coerce(parser_t *, obj->co.handle);
  gc_mark(p->stream);
  gc_mark(p->name);
  gc_mark(p->prepared_msg);
  gc_mark(p->syntax_tree);
}

static struct cobj_ops parser_ops = {
  eq,
  cobj_print_op,
  cobj_destroy_free_op,
  parser_mark,
  cobj_hash_op,
};

val parser(val stream, val lineno)
{
  parser_t *p = coerce(parser_t *, chk_malloc(sizeof *p));
  val parser;
  p->lineno = 0;
  p->errors = 0;
  p->stream = nil;
  p->name = nil;
  p->prepared_msg = nil;
  p->syntax_tree = nil;
  p->scanner = 0;
  parser = cobj(coerce(mem_t *, p), parser_s, &parser_ops);
  p->lineno = c_num(default_arg(lineno, one));
  p->stream = stream;
  return parser;
}

void parse_init(void)
{
  parser_s = intern(lit("parser"), user_package);
  parser_l_init();
}
