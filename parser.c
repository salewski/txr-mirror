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

void open_txr_file(val spec_file, val *name, val *stream)
{
  {
    FILE *in = w_fopen(c_str(spec_file), L"r");
    if (in == 0) {
      spec_file = cat_str(list(spec_file, lit("txr"), nao), lit("."));
      in = w_fopen(c_str(spec_file), L"r");
      if (in == 0)
        uw_throwf(file_error_s, lit("unable to open ~a"), spec_file, nao);
    }
    *stream = make_stdio_stream(in, spec_file);
    *name = spec_file;
  }
}

val regex_parse(val string, val error_stream)
{
  uses_or2;
  val parse_string = cat_str(list(lit("@\x01R"), string, nao), nil);
  val save_stream = std_error;
  val stream = make_string_byte_input_stream(parse_string);
  parser_t parser;

  error_stream = default_bool_arg(error_stream);
  std_error = if3(error_stream == t, std_output, or2(error_stream, std_null));

  {
    int gc = gc_state(0);
    val name = if3(std_error != std_null, lit("regex"), lit(""));
    parse(stream, name, &parser);
    gc_state(gc);
  }
  std_error = save_stream;
  return parser.errors ? nil : parser.syntax_tree;
}

val lisp_parse(val source_in, val error_stream, val error_return_val)
{
  uses_or2;
  val source = default_bool_arg(source_in);
  val input_stream = if3(stringp(source),
                         make_string_byte_input_stream(source),
                         or2(source, std_input));
  val secret_token_stream = make_string_byte_input_stream(lit("@\x01" "E"));
  val name = if3(stringp(source),
                 lit("string"),
                 stream_get_prop(input_stream, name_k));
  val stream = make_catenated_stream(list(secret_token_stream, input_stream, nao));
  val saved_dyn = dyn_env;
  parser_t parser;

  dyn_env = make_env(nil, nil, dyn_env);

  error_stream = default_bool_arg(error_stream);
  error_stream = if3(error_stream == t, std_output, or2(error_stream, std_null));
  class_check (error_stream, stream_s);

  env_vbind(dyn_env, stderr_s, error_stream);

  {
    int gc = gc_state(0);
    name = if3(std_error != std_null, name, lit(""));
    parse(stream, name, &parser);
    gc_state(gc);
  }

  dyn_env = saved_dyn;

  if (parser.errors) {
    if (missingp(error_return_val))
      uw_throwf(syntax_error_s, lit("read: syntax error"), nao);
    return error_return_val;
  }

  return parser.syntax_tree;
}

void parse_init(void)
{
  parser_s = intern(lit("parser"), user_package);
  parser_l_init();
}
