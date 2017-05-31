/* Copyright 2009-2017
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
#include <assert.h>
#include <limits.h>
#include <dirent.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <wchar.h>
#include <signal.h>
#include <ctype.h>
#include <errno.h>
#include "config.h"
#include ALLOCA_H
#ifdef __CYGWIN__
#include <sys/utsname.h>
#endif
#include "lib.h"
#include "signal.h"
#include "unwind.h"
#include "gc.h"
#include "args.h"
#include "utf8.h"
#include "hash.h"
#include "eval.h"
#include "stream.h"
#include "y.tab.h"
#include "sysif.h"
#include "cadr.h"
#include "struct.h"
#include "parser.h"
#include "txr.h"
#if HAVE_TERMIOS
#include "linenoise/linenoise.h"
#endif

val parser_s, unique_s, circref_s;
val listener_hist_len_s, listener_multi_line_p_s, listener_sel_inclusive_p_s;
val intr_s;

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
  gc_mark(p->circ_ref_hash);
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
  free(p);
}

static struct cobj_ops parser_ops = {
  eq,
  cobj_print_op,
  parser_destroy,
  parser_mark,
  cobj_eq_hash_op,
};

void parser_common_init(parser_t *p)
{
  int i;
  yyscan_t yyscan;

  p->parser = nil;
  p->lineno = 1;
  p->errors = 0;
  p->eof = 0;
  p->stream = nil;
  p->name = nil;
  p->prepared_msg = nil;
  p->circ_ref_hash = nil;
  p->circ_count = 0;
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
  p->scanner = 0;
}

void parser_reset(parser_t *p)
{
  yyscan_t yyscan;
  parser_cleanup(p);
  yylex_init(&yyscan);
  p->scanner = convert(scanner_t *, yyscan);
  yyset_extra(p, p->scanner);
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
  case prime_interactive:
    sec_tok.yy_char = SECRET_ESCAPE_I;
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

void prime_parser_post(parser_t *p, enum prime_parser prim)
{
  p->eof = (p->recent_tok.yy_char == 0);
  if (prim == prime_interactive)
    p->recent_tok.yy_char = 0;
}

int parser_callgraph_circ_check(struct circ_stack *rs, val obj)
{
  for (; rs; rs = rs->up) {
    if (rs->obj == obj)
      return 0;
  }

  return 1;
}

static val patch_ref(parser_t *p, val obj)
{
  if (consp(obj)) {
    val a = pop(&obj);
    if (a == circref_s) {
      val num = car(obj);
      val rep = gethash(p->circ_ref_hash, num);
      if (!rep)
        yyerrorf(p->scanner, lit("dangling #~s# ref"), num, nao);
      if (consp(rep) && car(rep) == circref_s)
        yyerrorf(p->scanner, lit("absurd #~s# ref"), num, nao);
      if (!p->circ_count--)
        yyerrorf(p->scanner, lit("unexpected surplus #~s# ref"), num, nao);
      return rep;
    }
  }
  return nil;
}

static void circ_backpatch(parser_t *p, struct circ_stack *up, val obj)
{
  struct circ_stack cs = { up, obj };

  if (!parser_callgraph_circ_check(up, obj))
    return;

tail:
  if (!p->circ_count)
    return;
  if (!is_ptr(obj))
    return;
  switch (type(obj)) {
  case CONS:
    {
      val a = car(obj);
      val d = cdr(obj);
      val ra = patch_ref(p, a);
      val rd = patch_ref(p, d);

      if (ra)
        rplaca(obj, ra);
      else
        circ_backpatch(p, &cs, a);

      if (rd) {
        rplacd(obj, rd);
        break;
      }

      obj = d;
      goto tail;
    }
  case VEC:
    {
      cnum i;
      cnum l = c_num(length_vec(obj));

      for (i = 0; i < l; i++) {
        val in = num(i);
        val v = vecref(obj, in);
        val rv = patch_ref(p, v);
        if (rv)
          set(vecref_l(obj, in), rv);
        else
          circ_backpatch(p, &cs, v);
        if (!p->circ_count)
          break;
      }

      break;
    }
  case RNG:
    {
      val s = from(obj);
      val e = to(obj);
      val rs = patch_ref(p, s);
      val re = patch_ref(p, e);

      if (rs)
        set_from(obj, rs);
      else
        circ_backpatch(p, &cs, s);

      if (re) {
        set_to(obj, re);
        break;
      }

      obj = e;
      goto tail;
    }
  case COBJ:
    if (hashp(obj)) {
      val u = get_hash_userdata(obj);
      val ru = patch_ref(p, u);
      if (ru)
        set_hash_userdata(obj, ru);
      if (p->circ_count) {
        val iter = hash_begin(obj);
        val cell;
        val pairs = nil;

        while ((cell = hash_next(iter))) {
          circ_backpatch(p, &cs, cell);
          push(cell, &pairs);
        }

        clearhash(obj);

        while (pairs) {
          val cell = pop(&pairs);
          sethash(obj, car(cell), cdr(cell));
        }
      }
    } else if (structp(obj)) {
      val stype = struct_type(obj);
      val iter;

      for (iter = slots(stype); iter; iter = cdr(iter)) {
        val sn = car(iter);
        val sv = slot(obj, sn);
        val rsv = patch_ref(p, sv);
        if (rsv)
          slotset(obj, sn, rsv);
        else
          circ_backpatch(p, &cs, sv);
      }
    }
    break;
  case FUN:
    if (obj->f.functype == FINTERP) {
      val fun = obj->f.f.interp_fun;
      circ_backpatch(p, &cs, car(fun));
      obj = cadr(fun);
      goto tail;
    }
  default:
    break;
  }
  return;
}

void parser_resolve_circ(parser_t *p)
{
  if (p->circ_count == 0)
    return;


  circ_backpatch(p, 0, p->syntax_tree);

  if (p->circ_count > 0)
    yyerrorf(p->scanner, lit("not all #<num># refs replaced in object ~s"),
             p->syntax_tree, nao);
}

void parser_circ_def(parser_t *p, val num, val expr)
{
  if (!p->circ_ref_hash)
    p->circ_ref_hash = make_hash(nil, nil, nil);

  {
    val new_p = nil;
    val cell = gethash_c(p->circ_ref_hash, num, mkcloc(new_p));

    if (!new_p && cdr(cell) != unique_s)
      yyerrorf(p->scanner, lit("duplicate #~s= def"), num, nao);

    rplacd(cell, expr);
  }
}

val parser_circ_ref(parser_t *p, val num)
{
  val obj = if2(p->circ_ref_hash, gethash(p->circ_ref_hash, num));

  if (!obj)
    yyerrorf(p->scanner, lit("dangling #~s# ref"), num, nao);

  if (obj == unique_s && !p->circ_suppress) {
    p->circ_count++;
    return cons(circref_s, cons(num, nil));
  }

  return obj;
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
      spec_file_try = scat(lit("."), spec_file, lit("txr"), nao);
      in = w_fopen(c_str(spec_file_try), L"r");
#ifdef ENOENT
      if (in == 0 && errno != ENOENT)
        goto except;
      errno = 0;
#endif
    }


    if (suffix == none && in == 0) {
      spec_file_try = scat(lit("."), spec_file, lit("tl"), nao);
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

  error_stream = default_null_arg(error_stream);
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

  if (parser.errors)
    uw_throw(syntax_error_s, lit("regex-parse: syntax errors in regex"));

  return parser.syntax_tree;
}

static val lisp_parse_impl(val interactive, val source_in, val error_stream,
                           val error_return_val, val name_in, val lineno)
{
  uses_or2;
  val source = default_null_arg(source_in);
  val input_stream = if3(stringp(source),
                         make_string_byte_input_stream(source),
                         or2(source, std_input));
  val name = or2(default_null_arg(name_in),
                 if3(stringp(source),
                     lit("string"),
                     stream_get_prop(input_stream, name_k)));
  val parser = ensure_parser(input_stream);
  val saved_dyn = dyn_env;
  parser_t *pi = get_parser_impl(parser);
  volatile val parsed = nil;

  uw_simple_catch_begin;

  dyn_env = make_env(nil, nil, dyn_env);

  error_stream = default_null_arg(error_stream);
  error_stream = if3(error_stream == t, std_output, or2(error_stream, std_null));
  class_check (error_stream, stream_s);

  if (lineno && !missingp(lineno))
    pi->lineno = c_num(lineno);

  env_vbind(dyn_env, stderr_s, error_stream);

  for (;;) {
    int gc = gc_state(0);
    enum prime_parser prime = if3(interactive, prime_interactive, prime_lisp);
    parse(pi, if3(std_error != std_null, name, lit("")), prime);
    gc_state(gc);

    if (pi->syntax_tree == nao && pi->errors == 0 && !parser_eof(parser))
      continue;

    break;
  }

  parsed = t;

  uw_unwind {
    dyn_env = saved_dyn;
    if (!parsed) {
      parser_reset(pi);
    }
  }

  uw_catch_end;

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

val lisp_parse(val source_in, val error_stream, val error_return_val,
               val name_in, val lineno)
{
  return lisp_parse_impl(nil, source_in, error_stream, error_return_val,
                         name_in, lineno);
}

val iread(val source_in, val error_stream, val error_return_val,
          val name_in, val lineno)
{
  return lisp_parse_impl(t, source_in, error_stream, error_return_val,
                         name_in, lineno);
}

val read_eval_stream(val stream, val error_stream)
{
  val error_val = gensym(nil);
  val name = stream_get_prop(stream, name_k);

  for (;;) {
    val form = lisp_parse(stream, error_stream, error_val, name, colon_k);
    val parser = get_parser(stream);

    if (form == error_val) {
      if (parser_errors(parser) != zero)
        return nil;
      if (parser_eof(parser))
        break;
      continue;
    }

    (void) eval_intrinsic(form, nil);

    if (parser_eof(parser))
      break;
  }

  return t;
}

#if HAVE_TERMIOS

static void load_rcfile(val name)
{
  val resolved_name;
  val lisp_p = t;
  val stream = nil;
  val catch_syms = cons(error_s, nil);
  val path_private_to_me_p =  intern(lit("path-private-to-me-p"), user_package);
  val path_exists_p =  intern(lit("path-exists-p"), user_package);

  if (!funcall1(path_exists_p, name))
    return;

  uw_catch_begin (catch_syms, sy, va);

  open_txr_file(name, &lisp_p, &resolved_name, &stream);

  if (stream) {
    if (!funcall1(path_private_to_me_p, statf(stream))) {
      format(std_output,
             lit("** possible security problem: ~a is writable to others\n"),
             name, nao);
    } else {
      val saved_dyn_env = set_dyn_env(make_env(nil, nil, dyn_env));
      env_vbind(dyn_env, load_path_s, resolved_name);
      read_eval_stream(stream, std_output);
      dyn_env = saved_dyn_env;
    }
  }

  uw_catch(sy, va)
  {
    (void) va;
    format(std_output, lit("** type ~s exception while loading ~a\n"),
           sy, name, nao);
    format(std_output, lit("** details: ~a\n"), car(va), nao);
  }

  uw_unwind {
    if (stream)
      close_stream(stream, nil);
  }

  uw_catch_end;
}

static val get_visible_syms(val package, int is_cur)
{
  val fblist;

  if (!is_cur || nilp((fblist = package_fallback_list(package)))) {
    return package_symbols(package);
  } else {
    val symhash = copy_hash(package->pk.symhash);

    for (; fblist; fblist = cdr(fblist))
    {
      val fb_pkg = car(fblist);
      val hiter = hash_begin(fb_pkg->pk.symhash);
      val fcell;
      val new_p;
      while ((fcell = hash_next(hiter))) {
        val scell = gethash_c(symhash, car(fcell), mkcloc(new_p));
        if (new_p)
          rplacd(scell, cdr(fcell));
      }
    }
    return hash_values(symhash);
  }
}

static void find_matching_syms(lino_completions_t *cpl,
                               val package, val prefix,
                               val line_prefix, char kind,
                               val force_qualify)
{
  val is_cur = tnil(package == cur_package);
  val qualify = tnil(force_qualify || !is_cur);
  val pkg_name = if2(qualify,
                     if3(package == keyword_package && !force_qualify,
                         lit(""),
                         package_name(package)));
  val syms = ((kind == 'S' || kind == 'M')
              ? hash_keys((get_slot_syms(package, is_cur, tnil(kind == 'M'))))
              : get_visible_syms(package, is_cur != nil));

  for ( ; syms; syms = cdr(syms)) {
    val sym = car(syms);
    val name = symbol_name(sym);
    val found = if3(cpl->substring,
                    search_str(name, prefix, zero, nil),
                    match_str(name, prefix, zero));

    if (found) {
      val comple;

      switch (kind) {
      case '(':
        if (!fboundp(sym) && !mboundp(sym) && !special_operator_p(sym))
          continue;
        break;
      case '[':
        if (!boundp(sym) && !lookup_fun(nil, sym))
          continue;
        break;
      case 'M':
      case 'S':
        break;
      default:
        break;
      }

      if (equal(name, prefix))
        continue;

      if (qualify)
        comple = format(nil, lit("~a~a:~a"), line_prefix, pkg_name, name, nao);
      else
        comple = format(nil, lit("~a~a"), line_prefix, name, nao);

      {
        char *completion = utf8_dup_to(c_str(comple));
        lino_add_completion(cpl, completion);
        free(completion);
      }

      gc_hint(comple);
    }
  }
}

static void provide_completions(const char *data,
                                lino_completions_t *cpl,
                                void *ctx)
{
  const char *gly = "!$%&*+-<=>?\\_~/";
  const char *ptr = data[0] ? data + strlen(data) - 1 : 0;
  const char *sym = 0, *pkg = 0;
  const char *end;
  val keyword = nil;
  val package = nil;

  (void) ctx;

  if (!ptr)
    return;

  while ((isalnum(convert(unsigned char, *ptr)) || strchr(gly, *ptr)) &&
         (sym = ptr) && ptr > data)
    ptr--;

  if (!sym)
    return;

  end = sym;

  if (*ptr == ':') {
    if (ptr == data) {
      keyword = t;
    } else {
      ptr--;

      while ((isalnum(convert(unsigned char, *ptr)) || strchr(gly, *ptr)) &&
             (pkg = ptr) && ptr > data)
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
    char *pkg_copy = convert(char *, alloca(sz));

    memcpy(pkg_copy, pkg, sz);
    pkg_copy[sz - 1] = 0;

    {
      val package_name = string_utf8(pkg_copy);
      package = find_package(package_name);
      if (!package)
        return;
    }

    end = pkg;
  }

  {
    val sym_pfx = string_utf8(sym);
    size_t lsz = end - data + 1;
    char *line_pfxu8 = convert(char *, alloca(lsz));
    memcpy(line_pfxu8, data, lsz);
    line_pfxu8[lsz - 1] = 0;

    {
      uses_or2;
      val line_pfx = string_utf8(line_pfxu8);
      char prev = (end > data) ? end[-1] : 0;
      char pprev = (end > data + 1) ? end[-2] : 0;
      int quote = (pprev == '^' || pprev == '\'' || pprev == '#');
      int ppar = (pprev == '(');
      int dwim = (prev == '[');
      int par = (prev == '(');
      int slot = (prev == '.');
      int meth = (pprev == '.') && (dwim || par);
      char kind = (slot
                   ? 'S'
                   : (meth
                      ? 'M'
                      : (!pprev || (!quote && !ppar) || dwim) ? prev : 0));

      find_matching_syms(cpl, or2(package, cur_package),
                         sym_pfx, line_pfx, kind, if2(package, null(keyword)));
    }
  }
}

static char *provide_atom(lino_t *l, const char *str, int n, void *ctx)
{
  val catch_all = list(t, nao);
  val obj = nao;
  val form;
  val line = string_utf8(str);
  char *out = 0;

  (void) l;
  (void) ctx;

  uw_catch_begin (catch_all, exsym, exvals);

  form = lisp_parse(line, std_null, colon_k, lit("atomcb"), colon_k);

  if (atom(form)) {
    if (n == 1)
      obj = form;
  } else {
    val fform = flatcar(form);
    obj = ref(fform, num(-n));
  }

  if (obj != nao)
      out = utf8_dup_to(c_str(tostring(obj)));

  uw_catch (exsym, exvals) {
    (void) exsym;
    (void) exvals;
  }

  uw_unwind;

  uw_catch_end;

  return out;
}

static val repl_intr(val signo, val async_p)
{
  uw_throw(intr_s, lit("intr"));
}

static val read_eval_ret_last(val env, val counter,
                              val in_stream, val out_stream)
{
  val lineno = one;
  val error_val = gensym(nil);
  val name = format(nil, lit("paste-~a"), counter, nao);
  val value = nil;
  val loading = cdr(lookup_var(dyn_env, load_recursive_s));
  val saved_dyn_env = set_dyn_env(make_env(nil, nil, dyn_env));
  env_vbind(dyn_env, load_recursive_s, t);

  for (;; lineno = succ(lineno)) {
    val form = lisp_parse(in_stream, out_stream, error_val, name, lineno);
    val parser = get_parser(in_stream);

    if (form == error_val) {
      if (parser_errors(parser) != zero || parser_eof(parser))
        break;
      continue;
    }

    value = eval_intrinsic(form, nil);

    if (parser_eof(parser))
      break;
  }

  dyn_env = saved_dyn_env;

  if (!loading)
    uw_release_deferred_warnings();

  prinl(value, out_stream);
  return t;
}

static val get_home_path(void)
{
#ifdef __CYGWIN__
  struct utsname un;

  if (uname(&un) >= 0) {
    if (strncmp(un.sysname, "CYGNAL", 6) == 0)
      return getenv_wrap(lit("USERPROFILE"));
  }
#endif
  return getenv_wrap(lit("HOME"));
}

static val repl_warning(val out_stream, val exc, struct args *rest)
{
  val args = args_get_list(rest);

  if (cdr(args))
    uw_defer_warning(args);
  else
    format(out_stream, lit("** warning: ~!~a\n"), car(args), nao);

  uw_throw(continue_s, nil);
}

val repl(val bindings, val in_stream, val out_stream)
{
  val ifd = stream_get_prop(in_stream, fd_k);
  val ofd = stream_get_prop(out_stream, fd_k);
  lino_t *ls = lino_make(c_num(ifd), c_num(ofd));
  char *line_u8 = 0;
  char *prompt_u8 = 0;
  val quit_k = intern(lit("quit"), keyword_package);
  val read_k = intern(lit("read"), keyword_package);
  val prompt_k = intern(lit("prompt"), keyword_package);
  val p_k = intern(lit("p"), keyword_package);
  val counter_sym = intern(lit("*n"), user_package);
  val var_counter_sym = intern(lit("*v"), user_package);
  val result_hash_sym = intern(lit("*r"), user_package);
  val catch_all = list(t, nao);
  val result_hash = make_hash(nil, nil, nil);
  val done = nil;
  val counter = one;
  val home = get_home_path();
  val histfile = if2(home, format(nil, lit("~a/.txr_history"), home, nao));
  char *histfile_u8 = if3(home, utf8_dup_to(c_str(histfile)), NULL);
  val rcfile = if2(home, format(nil, lit("~a/.txr_profile"), home, nao));
  val old_sig_handler = set_sig_handler(num(SIGINT), func_n2(repl_intr));
  val hist_len_var = lookup_global_var(listener_hist_len_s);
  val multi_line_var = lookup_global_var(listener_multi_line_p_s);
  val sel_inclusive_var = lookup_global_var(listener_sel_inclusive_p_s);
  val rw_f = func_f1v(out_stream, repl_warning);
  val saved_dyn_env = set_dyn_env(make_env(nil, nil, dyn_env));

  env_vbind(dyn_env, stderr_s, out_stream);

  for (; bindings; bindings = cdr(bindings)) {
    val binding = car(bindings);
    reg_varl(car(binding), cdr(binding));
  }

  reg_varl(result_hash_sym, result_hash);

  lino_set_completion_cb(ls, provide_completions, 0);
  lino_set_atom_cb(ls, provide_atom, 0);
  lino_set_tempfile_suffix(ls, ".tl");

  if (rcfile)
    load_rcfile(rcfile);

  lino_hist_set_max_len(ls, c_num(cdr(hist_len_var)));

  if (histfile_u8)
    lino_hist_load(ls, histfile_u8);

  lino_set_noninteractive(ls, opt_noninteractive);

  while (!done) {
    val prompt = format(nil, lit("~d> "), counter, nao);
    val prev_counter = counter;
    val var_counter = mod(counter, num_fast(100));
    val var_name = format(nil, lit("*~d"), var_counter, nao);
    val var_sym = intern(var_name, user_package);
    uw_frame_t uw_handler;

    char *prompt_u8 = utf8_dup_to(c_str(prompt));

    lino_hist_set_max_len(ls, c_num(cdr(hist_len_var)));
    lino_set_multiline(ls, cdr(multi_line_var) != nil);
    lino_set_selinclusive(ls, cdr(sel_inclusive_var) != nil);
    reg_varl(counter_sym, counter);
    reg_varl(var_counter_sym, var_counter);
    line_u8 = linenoise(ls, prompt_u8);
    free (prompt_u8);
    prompt_u8 = 0;

    rplacd(multi_line_var, tnil(lino_get_multiline(ls)));

    if (line_u8 == 0) {
      switch (lino_get_error(ls)) {
      case lino_intr:
        put_line(lit("** intr"), out_stream);
        continue;
      case lino_eof:
        break;
      default:
        put_line(lit("** error reading interactive input"), out_stream);
        break;
      }
      break;
    }

    {
      size_t wsp = strspn(line_u8, " \t\n\r");

      if (line_u8[wsp] == 0) {
        free(line_u8);
        continue;
      }

      if (line_u8[wsp] == ';') {
        lino_hist_add(ls, line_u8);
        free(line_u8);
        continue;
      }
    }

    counter = succ(counter);

    uw_catch_begin (catch_all, exsym, exvals);

    uw_push_handler(&uw_handler, cons(warning_s, nil), rw_f);

    {
      val name = format(nil, lit("expr-~d"), prev_counter, nao);
      val line = string_utf8(line_u8);
      val form = lisp_parse(line, out_stream, colon_k, name, colon_k);
      if (form == quit_k) {
        done = t;
      } else if (form == prompt_k) {
        pprinl(prompt, out_stream);
        counter = prev_counter;
      } else if (form == p_k) {
        pprinl(prev_counter, out_stream);
        counter = prev_counter;
      } else {
        val value = if3(form != read_k,
                        eval_intrinsic(form, nil),
                        read_eval_ret_last(nil, prev_counter,
                                           in_stream, out_stream));
        reg_varl(var_sym, value);
        sethash(result_hash, var_counter, value);
        prinl(value, out_stream);
        lino_set_result(ls, utf8_dup_to(c_str(tostring(value))));
        lino_hist_add(ls, line_u8);
      }
    }

    uw_pop_frame(&uw_handler);

    uw_catch (exsym, exvals) {
      val exinfo = cons(exsym, exvals);
      reg_varl(var_sym, exinfo);
      sethash(result_hash, var_counter, exinfo);
      lino_hist_add(ls, line_u8);

      if (uw_exception_subtype_p(exsym, syntax_error_s)) {
        put_line(lit("** syntax error"), out_stream);
      } else if (uw_exception_subtype_p(exsym, error_s)) {
        error_trace(exsym, exvals, out_stream, lit("**"));
      } else {
        format(out_stream, lit("** ~!~s exception, args: ~!~s\n"),
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

  dyn_env = saved_dyn_env;

  if (histfile_u8)
    lino_hist_save(ls, histfile_u8);

  free(histfile_u8);
  free(prompt_u8);
  free(line_u8);
  lino_free(ls);
  gc_hint(histfile);
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

val parser_eof(val parser)
{
  parser_t *p = coerce(parser_t *, cobj_handle(parser, parser_s));
  return tnil(p->eof);
}

static val circref(val n)
{
  uw_throwf(error_s, lit("unresolved #~s# reference in object syntax"),
            n, nao);
}

void parse_init(void)
{
  parser_s = intern(lit("parser"), user_package);
  circref_s = intern(lit("circref"), system_package);
  intr_s = intern(lit("intr"), user_package);
  listener_hist_len_s = intern(lit("*listener-hist-len*"), user_package);
  listener_multi_line_p_s = intern(lit("*listener-multi-line-p*"), user_package);
  listener_sel_inclusive_p_s = intern(lit("*listener-sel-inclusive-p*"), user_package);
  unique_s = gensym(nil);
  prot1(&stream_parser_hash);
  prot1(&unique_s);
  stream_parser_hash = make_hash(t, nil, nil);
  parser_l_init();
  reg_var(listener_hist_len_s, num_fast(500));
  reg_var(listener_multi_line_p_s, nil);
  reg_var(listener_sel_inclusive_p_s, nil);
  reg_fun(circref_s, func_n1(circref));
}
