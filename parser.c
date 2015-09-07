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
#include <string.h>
#include <stdarg.h>
#include <setjmp.h>
#include <wchar.h>
#include <signal.h>
#include <ctype.h>
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
#if HAVE_TERMIOS
#include "linenoise/linenoise.h"
#endif

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
  yyscan_t yyscan;

  p->parser = nil;
  p->lineno = 1;
  p->errors = 0;
  p->stream = nil;
  p->name = nil;
  p->prepared_msg = nil;
  p->syntax_tree = nil;
  yylex_init(&yyscan);
  p->scanner = convert(scanner_t *, yyscan);
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

val lisp_parse(val source_in, val error_stream, val error_return_val,
               val name_in, val lineno)
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

  if (lineno && !missingp(lineno))
    pi->lineno = c_num(lineno);

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
    val form = lisp_parse(stream, error_stream, error_val, name, colon_k);

    if (form == error_val) {
      if (parser_errors(get_parser(stream)) == zero)
        break;
      return nil;
    }

    (void) eval_intrinsic(form, nil);
  }

  return t;
}

#if HAVE_TERMIOS

static void find_matching_syms(lino_completions_t *cpl,
                               val package, val prefix,
                               val line_prefix, char par,
                               val force_qualify)
{
  val qualify = tnil(force_qualify || package != user_package);
  val pkg_name = if2(qualify,
                     if3(package == keyword_package && !force_qualify,
                         lit(""),
                         package_name(package)));
  val syms;

  for (syms = package_symbols(package); syms; syms = cdr(syms)) {
    val sym = car(syms);
    val name = symbol_name(sym);

    if (match_str(name, prefix, zero)) {
      val compl;

      switch (par) {
      case '(':
        if (!fboundp(sym))
          continue;
        break;
      case '[':
        if (!boundp(sym) && !lookup_fun(nil, sym))
          continue;
        break;
      default:
        break;
      }

      if (qualify)
        compl = format(nil, lit("~a~a:~a"), line_prefix, pkg_name, name, nao);
      else
        compl = format(nil, lit("~a~a"), line_prefix, name, nao);

      {
        char *completion = utf8_dup_to(c_str(compl));
        lino_add_completion(cpl, completion);
        free(completion);
      }

      gc_hint(compl);
    }
  }
}

static void provide_completions(const char *data,
                                lino_completions_t *cpl,
                                void *ctx)
{
  const char *gly = "!$%&*+-<=>?\\_~/";
  const char *ptr = data + strlen(data) - 1;
  const char *sym = 0, *pkg = 0;
  const char *end;
  val keyword = nil;
  val package = nil;

  (void) ctx;

  while ((isalnum(*ptr) || strchr(gly, *ptr)) && (sym = ptr) && ptr > data)
    ptr--;

  if (!sym)
    return;

  end = sym;

  if (*ptr == ':') {
    if (ptr == data) {
      keyword = t;
    } else {
      ptr--;

      while ((isalnum(*ptr) || strchr(gly, *ptr)) && (pkg = ptr) && ptr > data)
        ptr--;

      if (!pkg)
        keyword = t;
    }
  }

  if (keyword) {
    package = keyword_package;
    end = sym - 1;
  } else if (pkg) {
    size_t sz = sym - pkg;
    char *pkg_copy = alloca(sz);

    memcpy(pkg_copy, pkg, sz);
    pkg_copy[sz - 1] = 0;

    {
      val package_name = string_utf8(pkg_copy);
      package = find_package(package_name);
    }

    end = pkg;
  }

  {
    val sym_pfx = string_utf8(sym);
    size_t lsz = end - data + 1;
    char *line_pfxu8 = alloca(lsz);
    memcpy(line_pfxu8, data, lsz);
    line_pfxu8[lsz - 1] = 0;

    {
      val line_pfx = string_utf8(line_pfxu8);
      char prev = (end > data) ? end[-1] : 0;
      char pprev = (end > data + 1) ? end[-2] : 0;
      int quote = (pprev == '^' || pprev == '\'' || pprev == '#');
      int dwim = (prev == '[');
      char par = (!pprev || !quote || dwim) ? prev : 0;

      if (package) {
        find_matching_syms(cpl, package, sym_pfx, line_pfx, par, null(keyword));
      } else {
        val pa;

        for (pa = package_alist(); pa; pa = cdr(pa)) {
          val pair = car(pa);
          find_matching_syms(cpl, cdr(pair), sym_pfx, line_pfx, par, nil);
        }
      }
    }
  }
}

static val repl_intr(val signo, val async_p)
{
  uw_throw(error_s, lit("intr"));
}

val repl(val bindings, val in_stream, val out_stream)
{
  val ifd = stream_get_prop(in_stream, fd_k);
  val ofd = stream_get_prop(out_stream, fd_k);
  lino_t *ls = lino_make(c_num(ifd), c_num(ofd));
  char *line_u8 = 0;
  char *prompt_u8 = 0;
  val repl_env = make_env(bindings, nil, nil);
  val quit_k = intern(lit("quit"), keyword_package);
  val counter_sym = intern(lit("*n"), user_package);
  val var_counter_sym = intern(lit("*v"), user_package);
  val result_hash_sym = intern(lit("*r"), user_package);
  val catch_all = list(t, nao);
  val result_hash = make_hash(nil, nil, nil);
  val done = nil;
  val counter = one;
  val old_sig_handler = set_sig_handler(num(SIGINT), func_n2(repl_intr));

  reg_varl(result_hash_sym, result_hash);

  lino_set_completion_cb(ls, provide_completions, 0);

  while (!done) {
    val prompt = format(nil, lit("~a> "), counter, nao);
    val prev_counter = counter;
    val var_counter = mod(counter, num_fast(100));
    val var_name = format(nil, lit("*~a"), var_counter, nao);
    val var_sym = intern(var_name, user_package);

    char *prompt_u8 = utf8_dup_to(c_str(prompt));

    reg_varl(counter_sym, counter);
    reg_varl(var_counter_sym, var_counter);
    line_u8 = linenoise(ls, prompt_u8);
    free (prompt_u8);
    prompt_u8 = 0;

    if (line_u8 == 0) {
      if (lino_get_error(ls) == lino_eof)
        break;
      put_line(lit("** intr"), out_stream);
      continue;
    }

    if (strspn(line_u8, " \t") == strlen(line_u8))
      continue;

    counter = succ(counter);

    uw_catch_begin (catch_all, exsym, exvals);

    {
      val line = string_utf8(line_u8);
      val form = lisp_parse(line, out_stream, colon_k,
                            lit("line"), prev_counter);
      val value = eval_intrinsic(form, repl_env);
      if (value == quit_k) {
        done = t;
      } else {
        reg_varl(var_sym, value);
        sethash(result_hash, var_counter, value);
        prinl(value, out_stream);
        lino_hist_add(ls, line_u8);
      }
    }

    uw_catch (exsym, exvals) {
      reg_varl(var_sym, nil);
      sethash(result_hash, var_counter, nil);
      lino_hist_add(ls, line_u8);

      if (uw_exception_subtype_p(exsym, syntax_error_s)) {
        /* suppress syntax error exceptions: uninformative in the repl */
      } else if (uw_exception_subtype_p(exsym, error_s)) {
        put_string(lit("** "), out_stream);
        obj_pprint(car(exvals), out_stream);
        if (cdr(exvals)) {
          put_string(lit(" "), out_stream);
          pprinl(cdr(exvals), out_stream);
        } else {
          put_line(nil, nil);
        }
      } else {
        format(out_stream, lit("caught exception: ~s ~s\n"),
               exsym, exvals, nao);
      }
    }

    uw_unwind {
      free(line_u8);
      line_u8 = 0;
    }

    uw_catch_end;

    gc_hint(prompt);
  }

  set_sig_handler(num(SIGINT), old_sig_handler);
  free(prompt_u8);
  free(line_u8);
  lino_free(ls);
  return nil;
}

#endif

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
  stream_parser_hash = make_hash(t, nil, nil);
  parser_l_init();
}
