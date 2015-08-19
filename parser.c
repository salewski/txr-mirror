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
#include <errno.h>
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
#include "y.tab.h"
#include "parser.h"

val parser_s, unique_s;

static val stream_parser_hash;

static void yy_tok_mark(struct yy_token *tok)
{
  gc_conservative_mark(tok->yy_lval.val);
}

static void parser_mark(val obj)
{
  int i;
  parser_t *p = coerce(parser_t *, obj->co.handle);

  assert (p->parser == nil || p->parser == obj);
  gc_mark(p->stream);
  gc_mark(p->name);
  gc_mark(p->prepared_msg);
  if (p->syntax_tree != nao)
    gc_mark(p->syntax_tree);
  yy_tok_mark(&p->recent_tok);
  for (i = 0; i < 4; i++)
    yy_tok_mark(&p->tok_pushback[i]);
}

static void parser_destroy(val obj)
{
  parser_t *p = coerce(parser_t *, obj->co.handle);
  parser_cleanup(p);
}

static struct cobj_ops parser_ops = {
  eq,
  cobj_print_op,
  parser_destroy,
  parser_mark,
  cobj_hash_op,
};

void parser_common_init(parser_t *p)
{
  int i;

  p->parser = nil;
  p->lineno = 1;
  p->errors = 0;
  p->stream = nil;
  p->name = nil;
  p->prepared_msg = nil;
  p->syntax_tree = nil;
  yylex_init(&p->yyscan);
  p->scanner = convert(scanner_t *, p->yyscan);
  yyset_extra(p, p->scanner);
  p->recent_tok.yy_char = 0;
  p->recent_tok.yy_lval.val = 0;
  for (i = 0; i < 4; i++) {
    p->tok_pushback[i].yy_char = 0;
    p->tok_pushback[i].yy_lval.val = 0;
  }
  p->tok_idx = 0;
}

void parser_cleanup(parser_t *p)
{
  if (p->scanner != 0)
    yylex_destroy(p->scanner);
}

val parser(val stream, val lineno)
{
  parser_t *p = coerce(parser_t *, chk_malloc(sizeof *p));
  val parser;
  parser_common_init(p);
  parser = cobj(coerce(mem_t *, p), parser_s, &parser_ops);
  p->parser = parser;
  p->lineno = c_num(default_arg(lineno, one));
  p->stream = stream;

  return parser;
}

static parser_t *get_parser_impl(val parser)
{
  return coerce(parser_t *, cobj_handle(parser, parser_s));
}

static val ensure_parser(val stream)
{
  val cell = gethash_c(stream_parser_hash, stream, nulloc);
  val pars = cdr(cell);
  if (pars)
    return pars;
  return set(cdr_l(cell), parser(stream, one));
}

static void pushback_token(parser_t *p, struct yy_token *tok)
{
  assert (p->tok_idx < 4);
  p->tok_pushback[p->tok_idx++] = *tok;
}

void prime_parser(parser_t *p, val name, enum prime_parser prim)
{
  struct yy_token sec_tok = { 0 };

  switch (prim) {
  case prime_lisp:
    sec_tok.yy_char = SECRET_ESCAPE_E;
    break;
  case prime_regex:
    sec_tok.yy_char = SECRET_ESCAPE_R;
    break;
  }

  if (p->recent_tok.yy_char)
    pushback_token(p, &p->recent_tok);
  pushback_token(p, &sec_tok);
  prime_scanner(p->scanner, prim);
  set(mkloc(p->name, p->parser), name);
}

void open_txr_file(val spec_file, val *txr_lisp_p, val *name, val *stream)
{
  enum { none, tl, txr } suffix;

  if (match_str(spec_file, lit(".txr"), negone))
    suffix = txr;
  else if (match_str(spec_file, lit(".tl"), negone))
    suffix = tl;
  else
    suffix = none;

  errno = 0;

  {
    val spec_file_try = spec_file;
    FILE *in = w_fopen(c_str(spec_file_try), L"r");

    if (in != 0) {
      switch (suffix) {
      case tl:
        *txr_lisp_p = t;
        break;
      case txr:
        *txr_lisp_p = nil;
        break;
      default:
        break;
      }
    }

#ifdef ENOENT
    if (in == 0 && errno != ENOENT)
      goto except;
    errno = 0;
#endif

    if (suffix == none && in == 0 && !*txr_lisp_p) {
      spec_file_try = cat_str(list(spec_file, lit("txr"), nao), lit("."));
      in = w_fopen(c_str(spec_file_try), L"r");
#ifdef ENOENT
      if (in == 0 && errno != ENOENT)
        goto except;
      errno = 0;
#endif
    }


    if (suffix == none && in == 0) {
      spec_file_try = cat_str(list(spec_file, lit("tl"), nao), lit("."));
      in = w_fopen(c_str(spec_file_try), L"r");
      *txr_lisp_p = t;
    }

    if (in == 0) {
#ifdef ENOENT
except:
#endif
      uw_throwf(file_error_s, lit("unable to open ~a"), spec_file_try, nao);
    }

    *stream = make_stdio_stream(in, spec_file_try);
    *name = spec_file_try;
  }
}

val regex_parse(val string, val error_stream)
{
  uses_or2;
  val save_stream = std_error;
  val stream = make_string_byte_input_stream(string);
  parser_t parser;

  error_stream = default_bool_arg(error_stream);
  std_error = if3(error_stream == t, std_output, or2(error_stream, std_null));

  parser_common_init(&parser);
  parser.stream = stream;

  {
    int gc = gc_state(0);
    parse(&parser, if3(std_error != std_null, lit("regex"), lit("")), prime_regex);
    gc_state(gc);
  }

  parser_cleanup(&parser);
  std_error = save_stream;
  return parser.errors ? nil : parser.syntax_tree;
}

val lisp_parse(val source_in, val error_stream, val error_return_val, val name_in)
{
  uses_or2;
  val source = default_bool_arg(source_in);
  val input_stream = if3(stringp(source),
                         make_string_byte_input_stream(source),
                         or2(source, std_input));
  val name = or2(default_bool_arg(name_in),
                 if3(stringp(source),
                     lit("string"),
                     stream_get_prop(input_stream, name_k)));
  val parser = ensure_parser(input_stream);
  val saved_dyn = dyn_env;
  parser_t *pi = get_parser_impl(parser);

  dyn_env = make_env(nil, nil, dyn_env);

  error_stream = default_bool_arg(error_stream);
  error_stream = if3(error_stream == t, std_output, or2(error_stream, std_null));
  class_check (error_stream, stream_s);

  env_vbind(dyn_env, stderr_s, error_stream);

  {
    int gc = gc_state(0);
    parse(pi, if3(std_error != std_null, name, lit("")), prime_lisp);
    gc_state(gc);
  }

  dyn_env = saved_dyn;

  if (pi->errors || pi->syntax_tree == nao) {
    if (missingp(error_return_val))
      uw_throwf(syntax_error_s, lit("read: ~a: ~a"), name,
                if3(pi->syntax_tree == nao,
                    lit("end of input reached without seeing object"),
                    lit("errors encountered")), nao);
    return error_return_val;
  }

  return pi->syntax_tree;
}

val read_eval_stream(val stream, val error_stream, val hash_bang_support)
{
  val error_val = gensym(nil);
  val name = stream_get_prop(stream, name_k);

  if (hash_bang_support) {
    val firstline = get_line(stream);

    if (firstline && !match_str(firstline, lit("#!"), nil)) {
      val flwnl = cat_str(list(firstline, lit("\n"), nao), nil);
      val string_stream = make_string_byte_input_stream(flwnl);
      stream = make_catenated_stream(list(string_stream, stream, nao));
    }
  }

  for (;;) {
    val form = lisp_parse(stream, error_stream, error_val, name);

    if (form == error_val) {
      if (parser_errors(get_parser(stream)) == zero)
        break;
      return nil;
    }

    (void) eval_intrinsic(form, nil);
  }

  return t;
}

val get_parser(val stream)
{
  return gethash(stream_parser_hash, stream);
}

val parser_errors(val parser)
{
  parser_t *p = coerce(parser_t *, cobj_handle(parser, parser_s));
  return num(p->errors);
}

void parse_init(void)
{
  parser_s = intern(lit("parser"), user_package);
  unique_s = gensym(nil);
  prot1(&stream_parser_hash);
  prot1(&unique_s);
  stream_parser_hash = make_hash(t, t, nil);
  parser_l_init();
}
