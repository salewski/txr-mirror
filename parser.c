/* Copyright 2009-2023
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
#include <assert.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <wchar.h>
#include <signal.h>
#include <wctype.h>
#include <errno.h>
#include "config.h"
#include "alloca.h"
#ifdef __CYGWIN__
#include <sys/utsname.h>
#endif
#if HAVE_SYS_STAT
#include <sys/stat.h>
#endif
#if HAVE_ZLIB
#include <zlib.h>
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
#if HAVE_ZLIB
#include "gzio.h"
#endif
#include "y.tab.h"
#include "sysif.h"
#include "cadr.h"
#include "struct.h"
#include "tree.h"
#include "parser.h"
#include "regex.h"
#include "itypes.h"
#include "arith.h"
#include "buf.h"
#include "vm.h"
#include "ffi.h"
#include "txr.h"
#include "linenoise/linenoise.h"

val parser_s, unique_s, circref_s;
val listener_hist_len_s, listener_multi_line_p_s, listener_sel_inclusive_p_s;
val listener_pprint_s, listener_greedy_eval_s, listener_auto_compound_s;
val rec_source_loc_s, read_unknown_structs_s, read_bad_json_s;
val json_s;
val intr_s;

struct cobj_class *parser_cls;

static lino_t *lino_ctx;
static int repl_level = 0;

static val stream_parser_hash, catch_all, catch_error;

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

static struct cobj_ops parser_ops = cobj_ops_init(eq,
                                                  cobj_print_op,
                                                  parser_destroy,
                                                  parser_mark,
                                                  cobj_eq_hash_op);

void parser_common_init(parser_t *p)
{
  int i;
  yyscan_t yyscan;
  val rec_source_loc_var = lookup_var(nil, rec_source_loc_s);
  val read_unknown_structs_var = lookup_var(nil, read_unknown_structs_s);
  val read_bad_json_var = lookup_var(nil, read_bad_json_s);

  p->parser = nil;
  p->lineno = 1;
  p->errors = 0;
  p->eof = 0;
  p->ignore = 0;
  p->stream = nil;
  p->name = nil;
  p->prepared_msg = nil;
  p->circ_ref_hash = nil;
  p->circ_count = 0;
  p->syntax_tree = nil;
  p->quasi_level = 0;
  yylex_init(&yyscan);
  p->scanner = convert(scanner_t *, yyscan);
  yyset_extra(p, p->scanner);
  p->recent_tok.yy_char = 0;
  p->recent_tok.yy_lex_state = 0;
  p->recent_tok.yy_lval.val = 0;
  for (i = 0; i < 4; i++) {
    p->tok_pushback[i].yy_char = 0;
    p->tok_pushback[i].yy_lex_state = 0;
    p->tok_pushback[i].yy_lval.val = 0;
  }
  p->tok_idx = 0;
  p->rec_source_loc = !nilp(cdr(rec_source_loc_var));
  p->read_unknown_structs = !nilp(cdr(read_unknown_structs_var));
  p->read_bad_json = !nilp(cdr(read_bad_json_var));
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

val parser(val stream, val name, val lineno)
{
  val self = lit("parser");
  parser_t *p = coerce(parser_t *, chk_malloc(sizeof *p));
  val parser;
  parser_common_init(p);
  parser = cobj(coerce(mem_t *, p), parser_cls, &parser_ops);
  p->parser = parser;
  p->lineno = c_num(default_arg(lineno, one), self);
  p->name = name;
  p->stream = stream;

  return parser;
}

parser_t *parser_get_impl(val self, val parser)
{
  return coerce(parser_t *, cobj_handle(self, parser, parser_cls));
}

val ensure_parser(val stream, val name)
{
  uses_or2;
  loc pcdr = gethash_l(lit("internal error"), stream_parser_hash, stream, nulloc);
  val pars = deref(pcdr);
  if (pars)
    return pars;
  return set(pcdr,
             parser(stream, or2(name, stream_get_prop(stream, name_k)), one));
}

static void pushback_token(parser_t *p, struct yy_token *tok)
{
  assert (p->tok_idx < 4);
  p->tok_pushback[p->tok_idx++] = *tok;
}

val parser_set_lineno(val self, val stream, val lineno)
{
  val parser = ensure_parser(stream, nil);
  parser_t *pi = parser_get_impl(self, parser);
  pi->lineno = c_num(lineno, self);
  return stream;
}

void prime_parser(parser_t *p, val name, enum prime_parser prim)
{
  struct yy_token sec_tok = all_zero_init;

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
  case prime_json:
    sec_tok.yy_char = SECRET_ESCAPE_J;
    break;
  }

  if (p->recent_tok.yy_char && prim != prime_json)
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
  val self = lit("parser");
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
      us_cons_bind(a, d, obj);
      val ra = patch_ref(p, a);
      val rd = patch_ref(p, d);

      if (ra)
        us_rplaca(obj, ra);
      else
        circ_backpatch(p, &cs, a);

      if (rd) {
        us_rplacd(obj, rd);
        break;
      }

      obj = d;
      goto tail;
    }
  case VEC:
    {
      cnum i;
      cnum l = c_num(length_vec(obj), self);

      for (i = 0; i < l; i++) {
        val v = obj->v.vec[i];
        val rv = patch_ref(p, v);
        if (rv)
          set(mkloc(obj->v.vec[i], obj), rv);
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
  case TNOD:
    {
      val k = obj->tn.key;
      val l = obj->tn.left;
      val r = obj->tn.right;
      val rk = patch_ref(p, k);
      val rl = patch_ref(p, l);
      val rr = patch_ref(p, r);

      if (rl)
        set(mkloc(obj->tn.left, obj), rl);
      else
        circ_backpatch(p, &cs, l);

      if (rr)
        set(mkloc(obj->tn.right, obj), rr);
      else
        circ_backpatch(p, &cs, r);

      if (rk)
        set(mkloc(obj->tn.key, obj), rk);

      obj = k;
      goto tail;
    }
  case COBJ:
    if (hashp(obj)) {
      val u = get_hash_userdata(obj);
      val ru = patch_ref(p, u);
      cnum old_circ_count = p->circ_count;

      if (ru)
        set_hash_userdata(obj, ru);
      else
        circ_backpatch(p, &cs, u);

      if (old_circ_count > 0) {
        val cell;
        val pairs = nil;
        struct hash_iter hi;

        us_hash_iter_init(&hi, obj);

        while ((cell = hash_iter_next(&hi))) {
          circ_backpatch(p, &cs, cell);
          push(cell, &pairs);
        }

        if (old_circ_count != p->circ_count) {
          clearhash(obj);

          while (pairs) {
            val cell = rcyc_pop(&pairs);
            sethash(obj, us_car(cell), us_cdr(cell));
          }
        } else {
          while (pairs)
            rcyc_pop(&pairs);
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
        if (p->circ_count <= 0)
          break;
      }
    } else if (treep(obj)) {
      val iter = tree_begin(obj, colon_k, colon_k);
      val node;
      val nodes = nil;
      cnum old_circ_count = p->circ_count;

      while ((node = tree_next(iter))) {
        val k = node->tn.key;
        val rk = patch_ref(p, k);
        if (rk)
          set(mkloc(node->tn.key, node), rk);
        else
          circ_backpatch(p, &cs, k);
        push(node, &nodes);
      }

      if (nodes && old_circ_count != p->circ_count) {
        tree_clear(obj);

        while (nodes) {
          val node = rcyc_pop(&nodes);
          tree_insert_node(obj, node, t);
        }
      } else {
        while (nodes)
          rcyc_pop(&nodes);
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
  if (!p->circ_ref_hash) {
    p->circ_ref_hash = make_eq_hash(hash_weak_none);
    setcheck(p->parser, p->circ_ref_hash);
  }

  {
    val new_p = nil;
    loc pcdr = gethash_l(lit("parser"), p->circ_ref_hash, num, mkcloc(new_p));

    if (!new_p && deref(pcdr) != unique_s)
      yyerrorf(p->scanner, lit("duplicate #~s= def"), num, nao);

    set(pcdr, expr);
  }
}

val parser_circ_ref(parser_t *p, val num)
{
  val obj = if2(p->circ_ref_hash, gethash(p->circ_ref_hash, num));

  if (!obj)
    yyerrorf(p->scanner, lit("dangling #~s# ref"), num, nao);

  if (obj == unique_s && !p->ignore) {
    p->circ_count++;
    return cons(circref_s, cons(num, nil));
  }

  return obj;
}

void open_txr_file(val first_try_path, val *txr_lisp_p,
                   val *orig_in_resolved_out, val *stream,
                   val search_dirs, val self)
{
  enum { none, tl, tlo, tlz, txr } suffix;
#if HAVE_ZLIB
  struct stdio_mode m_r = stdio_mode_init_r;
#endif

  if (match_str(first_try_path, lit(".txr"), negone))
    suffix = txr;
  else if (match_str(first_try_path, lit(".tl"), negone))
    suffix = tl;
  else if (match_str(first_try_path, lit(".tlo"), negone))
    suffix = tlo;
  else if (match_str(first_try_path, lit(".tlo.gz"), negone))
    suffix = tlz;
  else if (match_str(first_try_path, lit(".txr_profile"), negone))
    suffix = tl;
  else
    suffix = none;

#if !HAVE_ZLIB
  if (suffix == tlz)
    uw_ethrowf(file_error_s, lit("~s: cannot open ~s files: "
                                 "not built with zlib support"),
               self, nao);
#endif

  errno = 0;

  {
    val try_path = nil;
    FILE *in = 0;
#if HAVE_ZLIB
    gzFile zin = 0;
#else
    const int zin = 0;
#endif

    {
      try_path = first_try_path;
      errno = 0;
#if HAVE_ZLIB
      if (suffix == tlz)
        zin = w_gzopen_mode(c_str(try_path, self), L"r", m_r, self);
      else
#endif
        in = w_fopen(c_str(try_path, self), L"r");

      if (in != 0 || zin != 0) {
        switch (suffix) {
        case tl:
          *txr_lisp_p = t;
          break;
        case tlo: case tlz:
          *txr_lisp_p = chr('o');
          break;
        case txr:
          *txr_lisp_p = nil;
          break;
        default:
          break;
        }
        goto found;
       } else {
#ifdef ENOENT
        if (errno != ENOENT)
          goto except;
#endif
       }
    }

    if (suffix == none && !*txr_lisp_p) {
      try_path = scat(lit("."), first_try_path, lit("txr"), nao);
      if ((in = w_fopen(c_str(try_path, nil), L"r")) != 0)
        goto found;
#ifdef ENOENT
      if (errno != ENOENT)
        goto except;
#endif
    }

    if (suffix == none) {
      {
        try_path = scat(lit("."), first_try_path, lit("tlo"), nao);
        errno = 0;
        if ((in = w_fopen(c_str(try_path, nil), L"r")) != 0) {
          *txr_lisp_p = chr('o');
          goto found;
        }
#ifdef ENOENT
        if (errno != ENOENT)
          goto except;
#endif
      }
#if HAVE_ZLIB
      {
        try_path = scat(lit("."), first_try_path, lit("tlo.gz"), nao);
        errno = 0;
        if ((zin = w_gzopen_mode(c_str(try_path, nil), L"r", m_r, self)) != 0) {
          *txr_lisp_p = chr('o');
          goto found;
        }
#ifdef ENOENT
        if (errno != ENOENT)
          goto except;
#endif
      }
#endif
      {
        try_path = scat(lit("."), first_try_path, lit("tl"), nao);
        errno = 0;
        if ((in = w_fopen(c_str(try_path, nil), L"r")) != 0) {
          *txr_lisp_p = t;
          goto found;
        }
#ifdef ENOENT
        if (errno != ENOENT)
          goto except;
#endif
      }
    }

    if (in == 0 && zin == 0) {
      val try_next;
#ifdef ENOENT
except:
#endif
      if (abs_path_p(*orig_in_resolved_out))
        search_dirs = nil;
      else if (search_dirs == t)
        search_dirs = load_search_dirs;

#ifdef  ENOENT
      if (errno != ENOENT || search_dirs == nil)
#else
      if (search_dirs == nil)
#endif
        uw_ethrowf(errno_to_file_error(errno),
                   lit("~a: unable to open ~a"), self, *orig_in_resolved_out, nao);
      try_next = path_cat(pop(&search_dirs), *orig_in_resolved_out);
      open_txr_file(try_next, txr_lisp_p, orig_in_resolved_out, stream,
                    search_dirs, self);
      return;
    }

found:
    if (in != 0)
      *stream = make_stdio_stream(in, try_path);
#if HAVE_ZLIB
    else
      *stream = make_gzio_stream(zin, -1, try_path, 0);
#endif
    *orig_in_resolved_out = try_path;
  }
}

val regex_parse(val string, val error_stream)
{
  val save_stream = std_error;
  val stream = make_string_byte_input_stream(string);
  parser_t parser;

  error_stream = default_arg_strict(error_stream, std_null);
  std_error = if3(error_stream == t, std_output, error_stream);

  parser_common_init(&parser);
  parser.stream = stream;

  {
    int gc = gc_state(0);
    parse(&parser, if3(std_error != std_null, lit("regex"), null_string),
          prime_regex);
    gc_state(gc);
  }

  parser_cleanup(&parser);
  std_error = save_stream;

  if (parser.errors)
    uw_throw(syntax_error_s, lit("regex-parse: syntax errors in regex"));

  return parser.syntax_tree;
}

static val lisp_parse_impl(val self, enum prime_parser prime,
                           val rlcp_p, val source_in,
                           val error_stream, val error_return_val, val name_in,
                           val lineno)
{
  val source = default_arg_strict(source_in, std_input);
  val str = stringp(source);
  val input_stream = if3(str, make_string_byte_input_stream(source), source);
  val name = default_arg_strict(name_in,
                                if3(str,
                                    lit("string"),
                                    stream_get_prop(input_stream, name_k)));
  val parser = ensure_parser(input_stream, name);
  val saved_dyn = dyn_env;
  parser_t *pi = parser_get_impl(self, parser);
  volatile val parsed = nil;

  if (rlcp_p)
    pi->rec_source_loc = 1;

  uw_simple_catch_begin;

  dyn_env = make_env(nil, nil, dyn_env);

  error_stream = default_arg_strict(error_stream, std_null);
  error_stream = if3(error_stream == t, std_output, error_stream);
  class_check (self, error_stream, stream_cls);

  if (lineno && !missingp(lineno))
    pi->lineno = c_num(lineno, self);

  env_vbind(dyn_env, stderr_s, error_stream);

  for (;;) {
    int gc = gc_state(0);
    parse(pi, if3(std_error != std_null, name, null_string), prime);
    mut(parser);
    gc_state(gc);

    if (pi->syntax_tree == nao && pi->errors == 0 && !pi->eof)
      continue;

    break;
  }

  if (str) {
    int junk = 0;
    if (prime == prime_json) {
      YYSTYPE yyl;
      junk = yylex(&yyl, pi->scanner);
    } else {
      junk = pi->recent_tok.yy_char;
    }

    if (junk)
      yyerrorf(pi->scanner, lit("trailing material after expression"), nao);
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

  gc_hint(parser);

  return pi->syntax_tree;
}

val lisp_parse(val source_in, val error_stream, val error_return_val,
               val name_in, val lineno)
{
  val self = lit("lisp-parse");
  return lisp_parse_impl(self, prime_lisp, t, source_in, error_stream,
                         error_return_val, name_in, lineno);
}

val nread(val source_in, val error_stream, val error_return_val,
          val name_in, val lineno)
{
  val self = lit("nread");
  return lisp_parse_impl(self, prime_lisp, nil, source_in, error_stream,
                         error_return_val, name_in, lineno);
}

val iread(val source_in, val error_stream, val error_return_val,
          val name_in, val lineno)
{
  val self = lit("iread");
  return lisp_parse_impl(self, prime_interactive, nil, source_in, error_stream,
                         error_return_val, name_in, lineno);
}

val get_json(val source_in, val error_stream, val error_return_val,
             val name_in, val lineno)
{
  val self = lit("get-json");
  return lisp_parse_impl(self, prime_json, nil, source_in, error_stream,
                         error_return_val, name_in, lineno);
}

static val read_file_common(val self, val stream, val error_stream, val compiled)
{
  val error_val = gensym(nil);
  val name = stream_get_prop(stream, name_k);
  val first = t;
  val big_endian = nil;
  val parser = ensure_parser(stream, name);
  val not_compiled = null(compiled);
  val version_form = nil;

  if (compiled) {
    parser_t *pi = parser_get_impl(self, parser);
    pi->rec_source_loc = 0;
  }

  for (;;) {
    val form = lisp_parse_impl(self, prime_lisp, not_compiled, stream,
                               error_stream, error_val, name, colon_k);

    if (form == error_val) {
      if (parser_errors(parser) != zero)
        return nil;
      break;
    }

    if (compiled && first) {
      val major = car(form);
      if (neq(major, num_fast(6)) && neq(major, num_fast(7)))
        uw_throwf(error_s,
                  lit("cannot load ~s: version number mismatch"),
                  stream, nao);
      big_endian = caddr(form);
      first = nil;
      version_form = form;
    } else if (compiled) {
      if (consp(car(form))) {
        for (; form; form = cdr(form)) {
          val item = car(form);
          val nlevels = pop(&item);
          val nregs = pop(&item);
          val bytecode = pop(&item);
          val datavec = pop(&item);
          val funvec = car(item);
          val desc = vm_make_desc(nlevels, nregs, bytecode, datavec, funvec);
          if ((big_endian && HAVE_LITTLE_ENDIAN) ||
              (!big_endian && !HAVE_LITTLE_ENDIAN))
            buf_swap32(bytecode);
          (void) vm_execute_toplevel(desc);
          gc_hint(desc);
        }
      } else if (nequal(form, version_form)) {
        uw_throwf(error_s, lit("~s: mismatched version ~s in combined .tlo file"),
                  stream, form, nao);
      }
    } else {
      (void) eval_intrinsic(form, nil, nil);
    }
  }

  return t;
}

val read_eval_stream(val self, val stream, val error_stream)
{
  return read_file_common(self, stream, error_stream, nil);
}

val read_compiled_file(val self, val stream, val error_stream)
{
  return read_file_common(self, stream, error_stream, t);
}

val read_objects_from_string(val string, val error_stream,
                             val error_return_val, val name_in)
{
  val self = lit("read-objects-from-string");
  val stream = make_string_byte_input_stream(string);
  val name = default_arg(name_in, lit("string"));
  val parser = ensure_parser(stream, name);
  list_collect_decl (out, ptail);

  for (;;) {
    val form = lisp_parse_impl(self, prime_lisp, t, stream,
                               error_stream, unique_s, name, colon_k);

    if (form == unique_s) {
      if (parser_errors(parser) != zero)
        return error_return_val;
      break;
    }

    ptail = list_collect(ptail, form);
  }

  return out;
}

val txr_parse(val source_in, val error_stream,
              val error_return_val, val name_in)
{
  val self = lit("txr-parse");
  val source = default_arg_strict(source_in, std_input);
  val input_stream = if3(stringp(source),
                         make_string_byte_input_stream(source),
                         source);
  val name = default_arg_strict(name_in,
                                if3(stringp(source),
                                    lit("string"),
                                    stream_get_prop(input_stream, name_k)));
  int gc = gc_state(0);
  val saved_dyn = dyn_env;
  val parser_obj = ensure_parser(input_stream, name);
  parser_t *pi = parser_get_impl(self, parser_obj);
  val loading = cdr(lookup_var(nil, load_recursive_s));

  uw_simple_catch_begin;

  dyn_env = make_env(nil, nil, dyn_env);
  error_stream = default_arg_strict(error_stream, std_null);
  error_stream = if3(error_stream == t, std_output, error_stream);
  class_check (self, error_stream, stream_cls);

  parse_once(self, input_stream, name);

  uw_unwind {
    dyn_env = saved_dyn;
    mut(parser_obj);
    gc_state(gc);
    if (!loading)
      uw_release_deferred_warnings();
  }

  uw_catch_end;

  if (pi->errors || pi->syntax_tree == nao) {
    if (missingp(error_return_val))
      uw_throwf(syntax_error_s, lit("~a: ~a: ~a"), self, name,
                if3(pi->syntax_tree == nao,
                    lit("end of input reached without seeing object"),
                    lit("errors encountered")), nao);

    return error_return_val;
  }

  return pi->syntax_tree;
}

static void report_file_perm_problem(val name)
{
#ifdef __CYGWIN__
  (void) name;
#else
  format(std_output,
         lit("** security problem: ~a is readable by others\n"),
         name, nao);
#endif
}

static void report_path_perm_problem(val name)
{
#ifdef __CYGWIN__
  (void) name;
#else
  format(std_output,
         lit("** security problem: a component of ~a is writable to others\n"),
         name, nao);
#endif
}

static void load_rcfile(val name, val psafe_s, val ppriv_s)
{
  val self = lit("listener");
  val resolved_name = name;
  val lisp_p = t;
  val stream = nil;

  if (!funcall1(psafe_s, name)) {
    report_path_perm_problem(name);
    return;
  }

  uw_catch_begin (catch_error, sy, va);

  open_txr_file(name, &lisp_p, &resolved_name, &stream, nil, self);

  if (stream) {
    if (!funcall1(ppriv_s, stream)) {
      report_file_perm_problem(name);
    } else {
      val saved_dyn_env = set_dyn_env(make_env(nil, nil, dyn_env));
      env_vbind(dyn_env, load_path_s, resolved_name);
      read_eval_stream(self, stream, std_output);
      dyn_env = saved_dyn_env;
    }
  }

  uw_catch(sy, va)
  {
    (void) va;
    if (stream || sy != path_not_found_s) {
      format(std_output, lit("** type ~s exception while loading ~a\n"),
             sy, name, nao);
      format(std_output, lit("** details: ~a\n"), car(va), nao);
    }
  }

  uw_unwind {
    if (stream)
      close_stream(stream, nil);
  }

  uw_catch_end;
}

#if CONFIG_FULL_REPL

static val get_visible_syms(val package, int include_fallback)
{
  val fblist;

  if (!include_fallback || nilp((fblist = package_fallback_list(package)))) {
    return package_symbols(package);
  } else {
    val symhash = copy_hash(package->pk.symhash);

    for (; fblist; fblist = cdr(fblist))
    {
      val fb_pkg = car(fblist);
      val fcell;
      val new_p;
      struct hash_iter hi;

      us_hash_iter_init(&hi, fb_pkg->pk.symhash);

      while ((fcell = hash_iter_next(&hi))) {
        loc pcdr = gethash_l(lit("listener"), symhash, us_car(fcell), mkcloc(new_p));
        if (new_p)
          set(pcdr, us_cdr(fcell));
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
                         null_string,
                         package_name(package)));
  val syms = get_visible_syms(package, is_cur != nil && !qualify);

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
        if (fboundp(sym) || mboundp(sym) || special_operator_p(sym))
          break;
        continue;
      case 'M':
        if (static_slot_types(sym))
          break;
        continue;
      case 'S':
        if (slot_types(sym))
          break;
        continue;
        break;
      case 'Q':
        if (mboundp(sym) || special_operator_p(sym))
          break;
        /* fallthrough */
      default:
        if (find_struct_type(sym) || ffi_type_p(sym))
          break;
        /* fallthrough */
      case '[':
        if (fboundp(sym) || boundp(sym))
          break;
        continue;
      }

      if (equal(name, prefix))
        continue;

      if (qualify)
        comple = scat(nil, line_prefix, pkg_name, lit(":"), name, nao);
      else
        comple = scat2(line_prefix, name);

      lino_add_completion(cpl, c_str(comple, nil));
      gc_hint(comple);
    }
  }
}

static void provide_completions(const wchar_t *data,
                                lino_completions_t *cpl,
                                void *ctx)
{
  const wchar_t *gly = L"!$%&*+-<=>?\\_~/";
  const wchar_t *ptr = data[0] ? data + wcslen(data) - 1 : 0;
  const wchar_t *sym = 0, *pkg = 0;
  const wchar_t *end;
  val keyword = nil;
  val package = nil;

  (void) ctx;

  uw_catch_begin (catch_error, exsym, exvals);

  if (!ptr)
    goto out;

  while ((iswalnum(convert(wint_t, *ptr)) || wcschr(gly, *ptr) ||
          *ptr >= 0x80) &&
         (sym = ptr) && ptr > data)
    ptr--;

  if (!sym)
    goto out;

  end = sym;

  if (*ptr == ':') {
    if (ptr == data) {
      keyword = t;
    } else {
      ptr--;

      while ((iswalnum(convert(wint_t, *ptr)) || wcschr(gly, *ptr) ||
              *ptr >= 0x80) &&
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
    wchar_t *pkg_copy = convert(wchar_t *, alloca(sizeof *pkg_copy * sz));

    wmemcpy(pkg_copy, pkg, sz);
    pkg_copy[sz - 1] = 0;

    {
      val package_name = string(pkg_copy);
      package = find_package(package_name);
      if (!package)
        return;
    }

    end = pkg;
  }

  {
    val sym_pfx = string(sym);
    size_t lsz = end - data + 1;
    wchar_t *line_pfxs = convert(wchar_t *, alloca(sizeof *line_pfxs * lsz));
    wmemcpy(line_pfxs, data, lsz);
    line_pfxs[lsz - 1] = 0;

    {
      uses_or2;
      val line_pfx = string(line_pfxs);
      char prev = (end > data) ? end[-1] : 0;
      char pprev = (end > data + 1) ? end[-2] : 0;
      int quote = (prev == '^' || prev == '\'');
      int pquote = (pprev == '^' || pprev == '\'' || pprev == '#');
      int ppar = (pprev == '(');
      int dwim = (prev == '[');
      int par = (prev == '(');
      int slot = (prev == '.');
      int meth = (pprev == '.') && (dwim || par);
      char kind = if3(slot, 'S',
                      if3(meth, 'M',
                          if3(quote, 'Q',
                              if3(!pprev || (!pquote && !ppar) || dwim,
                                  prev,  0))));

      find_matching_syms(cpl, or2(package, cur_package),
                         sym_pfx, line_pfx, kind, if2(package, null(keyword)));
    }
  }

out:
  uw_catch (exsym, exvals) {
    (void) exsym;
    (void) exvals;
  }

  uw_unwind;

  uw_catch_end;
}

static wchar_t *provide_atom(lino_t *l, const wchar_t *str, int n, void *ctx)
{
  val obj = nao;
  val form;
  val line = string(str);
  wchar_t *out = 0;

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
    out = chk_strdup(c_str(tostring(obj), nil));

  uw_catch (exsym, exvals) {
    (void) exsym;
    (void) exvals;
  }

  uw_unwind;

  uw_catch_end;

  return out;
}

#endif

static val repl_intr(val signo, val async_p)
{
  (void) signo;
  (void) async_p;
  return uw_rthrow(intr_s, lit("intr"));
}

static val read_eval_ret_last(val env, val counter,
                              val in_stream, val out_stream)
{
  val lineno = one;
  val error_val = gensym(nil);
  val name = format(nil, lit("paste-~a"), counter, nao);
  val value = nil;
  val loading = cdr(lookup_var(nil, load_recursive_s));
  val saved_dyn_env = set_dyn_env(make_env(nil, nil, dyn_env));
  env_vbind(dyn_env, load_recursive_s, t);

  (void) env;

  for (;; lineno = succ(lineno)) {
    val form = lisp_parse(in_stream, out_stream, error_val, name, lineno);

    if (form == error_val)
      break;

    value = eval_intrinsic(form, nil, nil);
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

static val repl_warning(val out_stream, val exc, varg rest)
{
  val args = args_get_list(rest);

  (void) exc;

  if (cdr(args))
    uw_defer_warning(args);
  else
    format(out_stream, lit("** ~!~a\n"), car(args), nao);

  return uw_rthrow(continue_s, nil);
}

static int is_balanced_line(const wchar_t *line, void *ctx)
{
  enum state {
    ST_START, ST_CMNT, ST_PAR, ST_BKT, ST_BRC, ST_HASH,
    ST_LIT, ST_QLIT, ST_RGX, ST_RGXC, ST_RGXE, ST_CHR, ST_ESC, ST_AT,
    ST_HASH_B, ST_BUF
  };
  int count[32], sp = 0;
  enum state state[32];
  wchar_t ch;

  (void) ctx;

  count[sp] = 0;
  state[sp] = ST_START;

  while ((ch = *line++) != 0) {
  again:
    if (sp >= 30)
      return 1;

    count[sp+1] = 0;
    count[sp+2] = 0;

    switch (state[sp]) {
    case ST_START:
    case ST_PAR:
    case ST_BKT:
    case ST_BRC:
      switch (ch) {
      case ';':
        state[++sp] = ST_CMNT;
        break;
      case '#':
        state[++sp] = ST_HASH;
        break;
      case '"':
        state[++sp] = ST_LIT;
        break;
      case '`':
        state[++sp] = ST_QLIT;
        break;
      case '(':
        if (state[sp] == ST_PAR)
          count[sp]++;
        else
          state[++sp] = ST_PAR;
        break;
      case '[':
        if (state[sp] == ST_BKT)
          count[sp]++;
        else
          state[++sp] = ST_BKT;
        break;
      case '{':
        if (state[sp] == ST_BRC)
          count[sp]++;
        else
          state[++sp] = ST_BRC;
        break;
      case ')': case ']': case '}':
        {
          enum state match = ST_START;
          switch (ch) {
          case ')': match = ST_PAR; break;
          case ']': match = ST_BKT; break;
          case '}': match = ST_BRC; break;
          }

          while (sp > 0 && state[sp] != match)
            sp--;
          if (state[sp] != match)
            return 0;
          if (count[sp] == 0)
            sp--;
          else
            count[sp]--;
          break;
        }
      }
      break;
    case ST_CMNT:
      if (ch == '\r')
        sp--;
      break;
    case ST_HASH:
      switch (ch) {
      case '\\':
        state[sp] = ST_CHR;
        break;
      case '/':
        state[sp] = ST_RGX;
        break;
      case 'b':
        state[sp] = ST_HASH_B;
        break;
      case ';':
        --sp;
        break;
      default:
        --sp;
        goto again;
      }
      break;
    case ST_LIT:
      switch (ch) {
      case '"':
        sp--;
        break;
      case '\\':
        state[++sp] = ST_ESC;
        break;
      }
      break;
    case ST_QLIT:
      switch (ch) {
      case '`':
        sp--;
        break;
      case '\\':
        state[++sp] = ST_ESC;
        break;
      case '@':
        state[++sp] = ST_AT;
        break;
      }
      break;
    case ST_RGX:
      switch (ch) {
      case '/':
        sp--;
        break;
      case '[':
        state[++sp] = ST_RGXC;
        break;
      case '(':
        state[++sp] = ST_RGXE;
        break;
      case '\\':
        state[++sp] = ST_ESC;
        break;
      }
      break;
    case ST_RGXC:
      switch (ch) {
      case ']':
        sp--;
        break;
      case '\\':
        state[++sp] = ST_ESC;
        break;
      }
      break;
    case ST_RGXE:
      switch (ch) {
      case ')':
        sp--;
        break;
      case '[':
        state[++sp] = ST_RGXC;
        break;
      case '(':
        state[++sp] = ST_RGXE;
        break;
      case '\\':
        state[++sp] = ST_ESC;
        break;
      }
      break;
    case ST_CHR:
      --sp;
      break;
    case ST_ESC:
      --sp;
      break;
    case ST_AT:
      switch (ch) {
      case '(':
        state[sp] = ST_PAR;
        break;
      case '[':
        state[sp] = ST_BKT;
        break;
      case '{':
        state[sp] = ST_BRC;
        break;
      default:
        sp--;
        break;
      }
      break;
    case ST_HASH_B:
      switch (ch) {
      case '\'':
        state[sp] = ST_BUF;
        break;
      default:
        sp--;
        break;
      }
      break;
    case ST_BUF:
      switch (ch) {
      case '\'':
        sp--;
        break;
      }
      break;
    }
  }

  if (state[sp] == ST_CMNT)
    sp--;

  return sp == 0 && state[sp] == ST_START && count[sp] == 0;
}

static_forward(lino_os_t linenoise_txr_binding);

static void hist_save(lino_t *ls, val in_stream, val out_stream,
                      val histfile, const wchar_t *histfile_w,
                      val hist_len_var)
{
  val self = lit("listener");
  if (histfile_w && lino_have_new_lines(ls)) {
    val histfile_tmp = scat2(histfile, lit(".tmp"));
    const wchar_t *histfile_tmp_w = c_str(histfile_tmp, self);
    lino_t *ltmp = lino_make(coerce(mem_t *, in_stream),
                             coerce(mem_t *, out_stream));
    lino_hist_set_max_len(ltmp, c_num(cdr(hist_len_var), self));
    lino_hist_load(ltmp, histfile_w);
    lino_hist_save(ltmp, histfile_tmp_w, 0);
    if (lino_hist_save(ls, histfile_tmp_w, 1) == 0)
      rename_path(histfile_tmp, histfile);
    else
      put_line(lit("** unable to save history file"), out_stream);
    gc_hint(histfile_tmp);
    lino_free(ltmp);
  }
}

val repl(val bindings, val in_stream, val out_stream, val env)
{
  val self = lit("listener");
  lino_t *ls = if3(repl_level++,
                   lino_ctx,
                   lino_ctx = lino_make(coerce(mem_t *, in_stream),
                                        coerce(mem_t *, out_stream)));
  wchar_t *volatile line_w = 0;
  val quit_k = intern(lit("quit"), keyword_package);
  val read_k = intern(lit("read"), keyword_package);
  val prompt_k = intern(lit("prompt"), keyword_package);
  val prompt_on_k = intern(lit("prompt-on"), keyword_package);
  val p_k = intern(lit("p"), keyword_package);
  val save_k = intern(lit("save"), keyword_package);
  val counter_sym = intern(lit("*n"), user_package);
  val var_counter_sym = intern(lit("*v"), user_package);
  val result_hash_sym = intern(lit("*r"), user_package);
  val pexist_s =  intern(lit("path-exists-p"), user_package);
#ifdef __CYGWIN__
  val ppriv_s = intern(lit("tf"), user_package);
  val psafe_s = ppriv_s;
#else
  val ppriv_s = intern(lit("path-strictly-private-to-me-p"), user_package);
  val psafe_s = intern(lit("path-components-safe"), user_package);
#endif
  val result_hash = make_hash(hash_weak_none, nil);
  val done = nil;
  val counter = one;
  val home = if3(repl_level == 1, get_home_path(), nil);
  val histfile = if2(home, scat2(home, lit("/.txr_history")));
  const wchar_t *histfile_w = if3(home, c_str(histfile, self), NULL);
  val rcfile = if2(home && !opt_noprofile, scat2(home, lit("/.txr_profile")));
  val old_sig_handler = set_sig_handler(num(SIGINT), func_n2(repl_intr));
  val hist_len_var = lookup_global_var(listener_hist_len_s);
#if CONFIG_FULL_REPL
  val multi_line_var = lookup_global_var(listener_multi_line_p_s);
  val sel_inclusive_var = lookup_global_var(listener_sel_inclusive_p_s);
#endif
  val pprint_var = lookup_global_var(listener_pprint_s);
  val greedy_eval = lookup_global_var(listener_greedy_eval_s);
  val auto_parens = lookup_global_var(listener_auto_compound_s);
  val rw_f = func_f1v(out_stream, repl_warning);
  val saved_dyn_env = set_dyn_env(make_env(nil, nil, dyn_env));
  val brackets = mkstring(num_fast(repl_level), chr('>'));
  cnum i;

  env_vbind(dyn_env, stderr_s, out_stream);

  for (; bindings; bindings = cdr(bindings)) {
    val binding = car(bindings);
    reg_varl(car(binding), cdr(binding));
  }

  for (i = 1; i <= 20; i++) {
    val name = format(nil, lit("*-~d"), num_fast(i), nao);
    val sym = intern(name, user_package);
    reg_symacro(sym, list(dwim_s, result_hash_sym,
                          list(macro_time_s,
                               list(mod_s,
                                    list(minus_s, var_counter_sym,
                                         num_fast(i), nao),
                                    num_fast(100), nao), nao), nao));
  }

  reg_varl(result_hash_sym, result_hash);

#if CONFIG_FULL_REPL
  lino_set_completion_cb(ls, provide_completions, 0);
  lino_set_atom_cb(ls, provide_atom, 0);
#endif

  lino_set_enter_cb(ls, is_balanced_line, 0);
  lino_set_tempfile_suffix(ls, ".tl");

  if (rcfile && funcall1(pexist_s, rcfile))
    load_rcfile(rcfile, psafe_s, ppriv_s);

  lino_hist_set_max_len(ls, c_num(cdr(hist_len_var), self));

  if (histfile_w && funcall1(pexist_s, histfile)) {
    if (!funcall1(psafe_s, home)) {
      report_path_perm_problem(home);
    } else if (!funcall1(ppriv_s, histfile)) {
      report_file_perm_problem(histfile);
    }

    lino_hist_load(ls, histfile_w);
  }

#if CONFIG_FULL_REPL
  lino_set_noninteractive(ls, opt_noninteractive);
#endif

  while (!done) {
    val prompt = format(nil, lit("~d~a "), counter, brackets,nao);
    val prev_counter = counter;
    val var_counter = mod(counter, num_fast(100));
    val var_name = format(nil, lit("*~d"), var_counter, nao);
    val var_sym = intern(var_name, user_package);
    uw_frame_t uw_handler;

    lino_hist_set_max_len(ls, c_num(cdr(hist_len_var), self));
#if CONFIG_FULL_REPL
    lino_set_multiline(ls, cdr(multi_line_var) != nil);
    lino_set_selinclusive(ls, cdr(sel_inclusive_var) != nil);
#endif
    reg_varl(counter_sym, counter);
    reg_varl(var_counter_sym, var_counter);

    uw_catch_begin (catch_all, exsym, exvals);

    uw_push_handler(&uw_handler, cons(warning_s, nil), rw_f);

    line_w = linenoise(ls, c_str(prompt, self));

    uw_pop_frame(&uw_handler);

#if CONFIG_FULL_REPL
    rplacd(multi_line_var, tnil(lino_get_multiline(ls)));
#endif

    if (line_w == 0) {
      switch (lino_get_error(ls)) {
      case lino_intr:
        put_line(lit("** intr"), out_stream);
        goto contin;
      case lino_eof:
        break;
      default:
        put_line(lit("** error reading interactive input"), out_stream);
        break;
      }
      done = t;
      goto contin;
    }

    {
      size_t wsp = wcsspn(line_w, L" \t\n\r");

      if (line_w[wsp] == 0)
        goto contin;

      if (line_w[wsp] == ';') {
        lino_hist_add(ls, line_w);
        goto contin;
      }
    }

    counter = succ(counter);

    uw_push_handler(&uw_handler, cons(warning_s, nil), rw_f);

    {
      val name = format(nil, lit("expr-~d"), prev_counter, nao);
      val line = string(line_w);
      val forms = read_objects_from_string(line, std_error, colon_k, name);
      val form = if2(and2(consp(forms), null(cdr(forms))), car(forms));
      if (form == quit_k) {
        done = t;
      } else if (form == prompt_k) {
        pprinl(prompt, out_stream);
        counter = prev_counter;
      } else if (form == prompt_on_k) {
        lino_enable_noninteractive_prompt(ls, 1);
        counter = prev_counter;
      } else if (form == p_k) {
        pprinl(prev_counter, out_stream);
        counter = prev_counter;
      } else if (form == save_k) {
        hist_save(ls, in_stream, out_stream, histfile, histfile_w, hist_len_var);
        counter = prev_counter;
      } else {
        val expr = if2(form != read_k,
                       if3(consp(forms),
                           if3(cdr(auto_parens) && cdr(forms),
                               forms,
                               cons(progn_s, forms)),
                           forms));
        val value = if3(form != read_k,
                        eval_intrinsic(expr, nil, env),
                        read_eval_ret_last(nil, prev_counter,
                                           in_stream, out_stream));
        val pprin = cdr(pprint_var);
        val (*pfun)(val, val) = if3(pprin, pprinl, prinl);
#if CONFIG_FULL_REPL
        val (*tsfun)(val) = if3(pprin, tostringp, tostring);
#endif
        reg_varl(var_sym, value);
        sethash(result_hash, var_counter, value);
        pfun(value, out_stream);
#if CONFIG_FULL_REPL
        lino_set_result(ls, chk_strdup(c_str(tsfun(value), self)));
#endif
        lino_hist_add(ls, line_w);
        if (cdr(greedy_eval)) {
          val error_p = nil;
          while (bindable(value) || consp(value))
          {
            value = eval_intrinsic_noerr(value, nil, &error_p);
            /* env deliberately not passed to eval here */
            if (error_p)
              break;
            pfun(value, out_stream);
          }
        }
      }
    }

    uw_pop_frame(&uw_handler);

    uw_catch (exsym, exvals) {
      val exinfo = cons(exsym, exvals);
      reg_varl(var_sym, exinfo);
      sethash(result_hash, var_counter, exinfo);
      if (line_w)
        lino_hist_add(ls, line_w);

      if (uw_exception_subtype_p(exsym, syntax_error_s)) {
        format(out_stream, lit("** syntax error: ~a\n"), car(exvals), nao);
      } else if (uw_exception_subtype_p(exsym, intr_s)) {
        format(out_stream, lit("** intr\n"), nao);
      } else if (uw_exception_subtype_p(exsym, error_s)) {
        error_trace(exsym, exvals, out_stream, lit("**"));
      } else {
        format(out_stream, lit("** ~!~s exception, args: ~!~s\n"),
               exsym, exvals, nao);
      }
    }

  contin:
    uw_unwind {
      free(line_w);
      line_w = 0;
    }

    uw_catch_end;

    gc_hint(prompt);
  }

  set_sig_handler(num(SIGINT), old_sig_handler);

  dyn_env = saved_dyn_env;

  hist_save(ls, in_stream, out_stream, histfile, histfile_w, hist_len_var);

  free(line_w);
  if (--repl_level == 0) {
    lino_free(lino_ctx);
    lino_ctx = 0;
  }
  gc_hint(histfile);
  return nil;
}

val parser_errors(val parser)
{
  val self = lit("parser-errors");
  parser_t *p = coerce(parser_t *, cobj_handle(self, parser, parser_cls));
  return num(p->errors);
}

val parse_errors(val stream)
{
  val self = lit("parse-errors");
  val errors = nil;
  val parser = gethash(stream_parser_hash, stream);
  if (parser) {
    parser_t *p = coerce(parser_t *, cobj_handle(self, parser, parser_cls));
    if (p->errors)
      errors = num(p->errors);
  }
  return errors;
}

static val circref(val n)
{
  uw_throwf(error_s, lit("unresolved #~s# reference in object syntax"),
            n, nao);
}

static int lino_fileno(mem_t *stream_in)
{
  val self = lit("listener");
  val stream = coerce(val, stream_in);
  return c_num(stream_fd(stream), self);
}

static int lino_puts(mem_t *stream_in, const wchar_t *str_in)
{
  val stream = coerce(val, stream_in);
  wchar_t ch;
  while ((ch = *str_in++))
    if (ch != LINO_PAD_CHAR)
      if (put_char(chr(ch), stream) != t)
        return 0;
  flush_stream(stream);
  return 1;
}

static int lino_puts_file(mem_t *stream_in, const wchar_t *str_in)
{
  val stream = coerce(val, stream_in);
  wchar_t ch;
  while ((ch = *str_in++))
    if (put_char(chr(ch), stream) != t)
      return 0;
  return 1;
}

static wint_t lino_getch(mem_t *stream_in)
{
  val self = lit("listener");
  wint_t ret = WEOF;

  val stream, ch;

  uw_catch_begin (catch_all, sy, va);

  stream = coerce(val, stream_in);
  ch = get_char(stream);

  ret = if3(ch, convert(wint_t, c_num(ch, self)), WEOF);

  uw_catch (sy, va) {
    (void) sy;
    (void) va;
  }

  uw_unwind { }

  uw_catch_end;

  return ret;
}

static wchar_t *lino_getl(mem_t *stream_in, wchar_t *buf, size_t nchar)
{
  val self = lit("listener");
  wchar_t *ptr = buf;
  val stream = coerce(val, stream_in);

  if (nchar == 0)
    return buf;

  while (nchar-- > 1) {
    val ch = get_char(stream);
    if (!ch)
      break;
    if ((*ptr++ = c_num(ch, self)) == '\n')
      break;
  }

  *ptr++ = 0;
  return (ptr == buf + 1) ? 0 : buf;
}

static wchar_t *lino_gets(mem_t *stream_in, wchar_t *buf, size_t nchar)
{
  val self = lit("listener");
  wchar_t *ptr = buf;
  val stream = coerce(val, stream_in);

  if (nchar == 0)
    return buf;

  while (nchar-- > 1) {
    val ch = get_char(stream);
    if (!ch)
      break;
    *ptr++ = c_num(ch, self);
  }

  *ptr++ = 0;
  return (ptr == buf + 1) ? 0 : buf;
}


static int lino_feof(mem_t *stream_in)
{
  val stream = coerce(val, stream_in);
  return get_error(stream) == t;
}

static const wchli_t *lino_mode_str[] = {
  wli("r"), wli("w"), wli("a")
};

static mem_t *lino_open(const wchar_t *name_in, lino_file_mode_t mode_in)
{
  val self = lit("listener");
  val name = string(name_in);
  val mode = static_str(lino_mode_str[mode_in]);
  val ret = 0;
  ignerr_begin;
  ret = open_file(name, mode);
#if HAVE_CHMOD
  if (mode_in == lino_overwrite || mode_in == lino_append)
    (void) fchmod(c_num(stream_fd(ret), self), S_IRUSR | S_IWUSR);
#endif
  ignerr_end;
  return coerce(mem_t *, ret);
}

static mem_t *lino_open8(const char *name_in, lino_file_mode_t mode_in)
{
  val name = string_utf8(name_in);
  val mode = static_str(lino_mode_str[mode_in]);
  mem_t *ret = 0;
  ignerr_begin;
  ret = coerce(mem_t *, open_file(name, mode));
  ignerr_end;
  return ret;
}

static mem_t *lino_fdopen(int fd, lino_file_mode_t mode_in)
{
  val mode = static_str(lino_mode_str[mode_in]);
  return coerce(mem_t *, open_fileno(num(fd), mode, nil));
}

static void lino_close(mem_t *stream)
{
  (void) close_stream(coerce(val, stream), nil);
}

static_def(lino_os_t linenoise_txr_binding =
           lino_os_init(chk_malloc, chk_realloc, chk_wmalloc,
                        chk_wrealloc, chk_strdup, free,
                        lino_fileno, lino_puts, lino_puts_file, lino_getch,
                        lino_getl, lino_gets, lino_feof,
                        lino_open, lino_open8, lino_fdopen, lino_close,
                        wide_display_char_p));

static val me_json(val form, val menv)
{
  (void) menv;
  return cdr(form);
}

void parse_init(void)
{
  parser_s = intern(lit("parser"), user_package);
  circref_s = intern(lit("circref"), system_package);
  intr_s = intern(lit("intr"), user_package);
  listener_hist_len_s = intern(lit("*listener-hist-len*"), user_package);
  listener_multi_line_p_s = intern(lit("*listener-multi-line-p*"), user_package);
  listener_sel_inclusive_p_s = intern(lit("*listener-sel-inclusive-p*"), user_package);
  listener_pprint_s = intern(lit("*listener-pprint-p*"), user_package);
  listener_greedy_eval_s = intern(lit("*listener-greedy-eval-p*"), user_package);
  listener_auto_compound_s = intern(lit("*listener-auto-compound-p*"), user_package);
  rec_source_loc_s = intern(lit("*rec-source-loc*"), user_package);
  read_unknown_structs_s = intern(lit("*read-unknown-structs*"), user_package);
  read_bad_json_s = intern(lit("*read-bad-json*"), user_package);
  json_s = intern(lit("json"), user_package);
  unique_s = gensym(nil);

  parser_cls = cobj_register(parser_s);

  protect(&stream_parser_hash, &unique_s,
          &catch_all, &catch_error, convert(val *, 0));
  stream_parser_hash = make_hash(hash_weak_and, nil);
  catch_all = cons(t, nil);
  catch_error = cons(error_s, nil);

  parser_l_init();

  lino_init(&linenoise_txr_binding);

  reg_var(listener_hist_len_s, num_fast(500));
  reg_var(listener_multi_line_p_s, t);
  reg_var(listener_sel_inclusive_p_s, t);
  reg_var(listener_pprint_s, nil);
  reg_var(listener_greedy_eval_s, nil);
  reg_var(listener_auto_compound_s, nil);
  reg_var(rec_source_loc_s, nil);
  reg_var(read_unknown_structs_s, nil);
  reg_var(read_bad_json_s, nil);
  reg_fun(circref_s, func_n1(circref));
  reg_fun(intern(lit("parse-errors"), user_package), func_n1(parse_errors));
  reg_fun(intern(lit("repl"), system_package), func_n4(repl));
  reg_mac(json_s, func_n2(me_json));
}
