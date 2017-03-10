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

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <dirent.h>
#include <stdarg.h>
#include <wchar.h>
#include <signal.h>
#include "config.h"
#include ALLOCA_H
#include "lib.h"
#include "gc.h"
#include "args.h"
#include "signal.h"
#include "unwind.h"
#include "regex.h"
#include "stream.h"
#include "y.tab.h"
#include "parser.h"
#include "txr.h"
#include "filter.h"
#include "hash.h"
#include "debug.h"
#include "eval.h"
#include "cadr.h"
#include "match.h"

int opt_print_bindings = 0;
int opt_lisp_bindings = 0;
int opt_arraydims = 1;

val decline_k, next_spec_k, repeat_spec_k;
val mingap_k, maxgap_k, gap_k, mintimes_k, maxtimes_k, times_k;
val lines_k, chars_k;
val text_s, choose_s, gather_s, do_s, mod_s, modlast_s;
val line_s, data_s, fuzz_s, load_s;
val include_s, close_s, require_s;
val longest_k, shortest_k, greedy_k;
val vars_k, resolve_k;
val append_k, into_k, var_k, list_k, tlist_k, string_k, env_k, counter_k;
val named_k, continue_k, finish_k, mandatory_k;

val filter_s;

val noval_s;

static val h_directive_table, v_directive_table;
static val non_matching_directive_table, binding_directive_table;

static void debuglf(val form, val fmt, ...)
{
  if (opt_loglevel >= 2) {
    va_list vl;
    va_start (vl, fmt);
    format(std_error, lit("~a: (~a) "), prog_string,
           source_loc_str(form, colon_k), nao);
    vformat(std_error, fmt, vl);
    put_char(chr('\n'), std_error);
    va_end (vl);
  }
}

noreturn static void sem_error(val form, val fmt, ...)
{
  va_list vl;
  val stream = make_string_output_stream();

  va_start (vl, fmt);
  if (form)
    format(stream, lit("(~a) "), source_loc_str(form, colon_k), nao);
  (void) vformat(stream, fmt, vl);
  va_end (vl);

  uw_throw(query_error_s, get_string_from_stream(stream));
  abort();
}

static void typed_error(val type, val form, val fmt, ...)
{
  va_list vl;
  val stream = make_string_output_stream();

  va_start (vl, fmt);
  if (form)
    format(stream, lit("(~a) "), source_loc_str(form, colon_k), nao);
  (void) vformat(stream, fmt, vl);
  va_end (vl);

  uw_throw(type, get_string_from_stream(stream));
  abort();
}

static void dump_shell_string(const wchar_t *str)
{
  int ch;

  put_char(chr('"'), std_output);
  while ((ch = *str++) != 0) {
    switch (ch) {
    case '"': case '`': case '$': case '\\': case '\n':
      put_char(chr('\\'), std_output);
      /* fallthrough */
    default:
      put_char(chr(ch), std_output);
    }
  }
  put_char(chr('"'), std_output);
}

static void dump_byte_string(const char *str)
{
  while (*str)
    put_char(chr(*str++), std_output);
}


static void dump_var(val var, char *pfx1, size_t len1,
              char *pfx2, size_t len2, val value, int level)
{
  if (len1 >= 112 || len2 >= 112)
    internal_error("too much depth in bindings");

  if (listp(value)) {
    val iter;
    int i;
    size_t add1 = 0, add2 = 0;

    for (i = 0, iter = value; iter; iter = cdr(iter), i++) {
      if (level < opt_arraydims) {
        add2 = sprintf(pfx2 + len2, "[%d]", i);
        add1 = 0;
      } else {
        add1 = sprintf(pfx1 + len1, "_%d", i);
        add2 = 0;
      }

      dump_var(var, pfx1, len1 + add1, pfx2, len2 + add2, car(iter), level + 1);
    }
  } else {
    val ss = make_string_output_stream();
    val str;

    pprint(value, ss);
    str = get_string_from_stream(ss);

    put_string(var, std_output);
    dump_byte_string(pfx1);
    dump_byte_string(pfx2);
    put_char(chr('='), std_output);
    dump_shell_string(c_str(str));
    put_char(chr('\n'), std_output);
  }
}

static void dump_bindings(val bindings)
{
  if (opt_lisp_bindings) {
    val iter;
    for (iter = bindings; iter; iter = cdr(iter)) {
      dump(car(iter), std_output);
    }
  } else {
    while (bindings) {
      char pfx1[128], pfx2[128];
      val var = car(car(bindings));
      val value = cdr(car(bindings));
      *pfx1 = 0; *pfx2 = 0;
      dump_var(var, pfx1, 0, pfx2, 0, value, 0);
      bindings = cdr(bindings);
    }
  }
}

static val depth(val obj)
{
  val dep = zero;

  if (obj == nil)
    return one;

  if (atom(obj))
    return zero;

  while (obj) {
    dep = max2(dep, depth(first(obj)));
    obj = rest(obj);
  }

  return plus(dep, one);
}

static val weird_merge(val left, val right)
{
  val left_depth = depth(left);
  val right_depth = depth(right);

  while (lt(left_depth, right_depth) || zerop(left_depth)) {
    left = cons(left, nil);
    left_depth = plus(left_depth, one);
  }

  while (lt(right_depth, left_depth) || zerop(right_depth)) {
    right = cons(right, nil);
    right_depth = plus(right_depth, one);
  }

  return append2(left, right);
}

static val tx_lookup_var(val sym, val bindings)
{
  uses_or2;
  return or2(assoc(sym, bindings),
             if2(!opt_compat || opt_compat <= 138, lookup_var(nil, bindings)));
}

static val tx_lookup_var_ubc(val sym, val bindings, val spec)
{
  val binding = tx_lookup_var(sym, bindings);
  if (binding)
    return binding;
  sem_error(spec, if3(bindable(sym),
                      lit("~s: unbound variable ~s"),
                      lit("~s: bindable symbol expected, not ~s")),
            car(spec), sym, nao);
}

static val dest_set(val spec, val bindings, val pattern, val value)
{
  if (bindable(pattern)) {
    val existing = tx_lookup_var_ubc(pattern, bindings, spec);
    set(cdr_l(existing), value);
  } else if (consp(pattern)) {
    if (first(pattern) == var_s) {
      uw_throwf(query_error_s,
                lit("metavariable @~a syntax cannot be used here"),
                second(pattern), nao);
    }

    if (first(pattern) == expr_s) {
      uw_throwf(query_error_s,
                lit("the @~s syntax cannot be used here"),
                rest(pattern), nao);
    }
    dest_set(spec, bindings, car(pattern), car(value));
    if (cdr(pattern))
      dest_set(spec, bindings, cdr(pattern), cdr(value));
  } else {
    sem_error(spec, lit("cannot set ~s: not a variable"), pattern, nao);
  }

  return nil;
}

static val tleval(val spec, val form, val bindings)
{
  val ret;

  uw_env_begin;

  if (opt_compat && opt_compat <= 121) {
    uw_set_match_context(cons(spec, bindings));
    ret = eval(form, make_env(bindings, nil, nil), form);
  } else {
    val saved_de = set_dyn_env(make_env(bindings, nil, nil));

    uw_set_match_context(cons(spec, bindings));
    ret = eval(form, nil, spec);

    set_dyn_env(saved_de);
  }

  uw_env_end;
  return ret;
}


static val tleval_progn(val spec, val forms, val bindings)
{
  val ret;

  uw_env_begin;

  if (opt_compat && opt_compat <= 121) {
    uw_set_match_context(cons(spec, bindings));
    ret = eval_progn(forms, make_env(bindings, nil, nil), forms);
  } else {
    val saved_de = set_dyn_env(make_env(bindings, nil, nil));

    uw_set_match_context(cons(spec, bindings));
    ret = eval_progn(forms, nil, forms);

    set_dyn_env(saved_de);
  }

  uw_env_end;
  return ret;
}

static val txeval(val spec, val form, val bindings);

static val tleval_144(val spec, val form, val bindings)
{
  return if3(!opt_compat || opt_compat >= 144,
             tleval(spec, form, bindings),
             txeval(spec, form, bindings));
}

static val dest_bind(val spec, val bindings, val pattern,
                     val value, val testfun)
{
  if (symbolp(pattern)) {
    if (bindable(pattern)) {
      val existing = tx_lookup_var(pattern, bindings);
      if (existing) {
        if (tree_find(value, cdr(existing), swap_12_21(testfun)))
          return bindings;
        if (tree_find(cdr(existing), value, testfun))
          return bindings;
        debuglf(spec, lit("variable ~a binding mismatch (~s vs. ~s)"),
                pattern, cdr(existing), value, nao);
        return t;
      }
      return cons(cons(pattern, value), bindings);
    } else {
      return funcall2(testfun, pattern, value) ? bindings : t;
    }
  } else if (consp(pattern)) {
    val piter = pattern, viter = value;
    val lisp_evaled = nil;
    val ret;

    if (first(pattern) == var_s) {
      ret = tleval(spec, second(pattern), bindings);
      lisp_evaled = t;
    }

    if (first(pattern) == expr_s) {
      ret = tleval(spec, second(pattern), bindings);
      lisp_evaled = t;
    }

    if (lisp_evaled)
      return if3(tree_find(value, ret, swap_12_21(testfun)), bindings, t);

    while (consp(piter) && consp(viter))
    {
      bindings = dest_bind(spec, bindings, car(piter), car(viter), testfun);
      if (bindings == t)
        return t;
      piter = cdr(piter);
      viter = cdr(viter);
    }

    if (bindable(piter)) {
      bindings = dest_bind(spec, bindings, piter, viter, testfun);
      if (bindings == t)
        return t;
    } else {
      return funcall2(testfun, piter, viter) ? bindings : t;
    }
    return bindings;
  } else if (tree_find(value, pattern, swap_12_21(testfun))) {
    return bindings;
  }

  return t;
}

static val vars_to_bindings(val spec, val vars, val bindings)
{
  val iter;
  list_collect_decl (fixed_vars, ptail);

  if (vars && !consp(vars))
    sem_error(spec, lit("not a valid variable list: ~a"), vars, nao);

  for (iter = vars; iter; iter = cdr(iter)) {
    val item = car(iter);
    if (bindable(item)) {
      ptail = list_collect(ptail, cons(item, noval_s));
    } else if (consp(item) && bindable(first(item))) {
      ptail = list_collect(ptail, cons(first(item),
                                       tleval_144(spec, second(item), bindings)));
    } else {
      sem_error(spec, lit("not a variable spec: ~a"), item, nao);
    }
  }
  return fixed_vars;
}

typedef struct {
  val bindings, specline, dataline, base, pos, data, data_lineno, file;
} match_line_ctx;

static match_line_ctx ml_all(val bindings, val specline, val dataline, val pos,
                             val data, val data_lineno, val file)
{
  match_line_ctx c;
  c.bindings = bindings;
  c.specline = specline;
  c.dataline = dataline;
  c.base = zero;
  c.pos = pos;
  c.data = data;
  c.data_lineno = data_lineno;
  c.file = file;

  return c;
}

static match_line_ctx ml_specline(match_line_ctx c, val specline)
{
  match_line_ctx nc = c;
  nc.specline = specline;
  return nc;
}

static match_line_ctx ml_specline_pos(match_line_ctx c, val specline, val pos)
{
  match_line_ctx nc = c;
  nc.specline = specline;
  nc.pos = pos;
  return nc;
}


static match_line_ctx ml_bindings_specline(match_line_ctx c, val bindings,
                                           val specline)
{
  match_line_ctx nc = c;
  nc.bindings = bindings;
  nc.specline = specline;
  return nc;
}

static val do_match_line(match_line_ctx *c);
static val match_line(match_line_ctx c);

typedef val (*h_match_func)(match_line_ctx *c);

#define LOG_MISMATCH(KIND)                                              \
  debuglf(elem, lit(KIND " mismatch, position ~a (~a:~d)"),             \
          plus(c->pos, c->base), c->file, c->data_lineno, nao);         \
  debuglf(elem, lit("  ~a"), c->dataline, nao);                         \
  if (c_num(c->pos) < 77)                                               \
    debuglf(elem, lit("  ~*a^"), c->pos, lit(""), nao)

#define LOG_MATCH(KIND, EXTENT)                                         \
  debuglf(elem, lit(KIND " matched, position ~a-~a (~a:~d)"),           \
          plus(c->pos, c->base), EXTENT, c->file, c->data_lineno, nao); \
  debuglf(elem, lit("  ~a"), c->dataline, nao);                         \
  if (c_num(EXTENT) < 77)                                               \
    debuglf(elem, lit("  ~*a~<*a^"), c->pos, lit(""),                   \
              minus(EXTENT, c->pos), lit("^"), nao)

#define elem_bind(elem_var, directive_var, specline)    \
  val elem_var = first(specline);                       \
  val directive_var = first(elem_var)

static val h_text(match_line_ctx *c)
{
  val elem = first(c->specline);
  val texts = rest(elem);
  val new_pos = cdr(match_line(ml_specline(*c, texts)));

  if (new_pos) {
    c->pos = minus(new_pos, c->base);
    return next_spec_k;
  }

  return nil;
}

static void consume_prefix(match_line_ctx *c)
{
  if (lazy_stringp(c->dataline)) {
    const val shift_hiwater = num_fast(4000);
    const val shift_amount = num_fast(3900);

    if (gt(c->pos, shift_hiwater)) {
      c->base = plus(c->base, shift_amount);
      c->pos = minus(c->pos, shift_amount);
      c->dataline = sub_str(c->dataline, shift_amount, t);
    }
  }
}


static val search_match(match_line_ctx *c, val from_end, val spec)
{
  val pos = from_end ? length_str(c->dataline) : c->pos;
  val step = from_end ? negone : one;

  for (; (from_end && ge(pos, c->pos)) ||
         (!from_end && length_str_ge(c->dataline, pos));
       pos = plus(pos, step))
  {
    val new_pos = cdr(match_line(ml_specline_pos(*c, spec, pos)));
    if (new_pos == t) {
      return cons(pos, t);
    } else if (new_pos) {
      new_pos = minus(new_pos, c->base);
      return cons(pos, minus(new_pos, pos));
    }

    consume_prefix(c);
  }

  return nil;
}

static val h_var(match_line_ctx *c)
{
  val elem = pop(&c->specline);
  val sym = second(elem);
  val next = first(c->specline);
  val modifiers = third(elem);
  val modifier = first(modifiers);
  val pair = if2(sym, tx_lookup_var(sym, c->bindings));

  if (sym == t)
    sem_error(elem, lit("t is not a bindable symbol"), nao);

  if (gt(length_list(modifiers), one)) {
    sem_error(elem, lit("multiple modifiers on variable ~s"),
              sym, nao);
  }

  if (pair) {
    /* If the variable already has a binding, we replace
       it with its value, and treat it as a string match.
       The spec looks like ((var <sym>) <next> ...)
       and it must be transformed into
       (<sym-substituted> <next> ...).
       But if the variable is a fix sized field match,
       then we treat that specially: it has to match
       that much text. */
    if (integerp(modifier)) {
      val past = plus(c->pos, modifier);

      if (length_str_lt(c->dataline, past) || lt(past, c->pos))
      {
        LOG_MISMATCH("fixed field size");
        return nil;
      }

      if (!tree_find(trim_str(sub_str(c->dataline, c->pos, past)),
                     cdr(pair), equal_f))
      {
        LOG_MISMATCH("fixed field contents");
        return nil;
      }

      LOG_MATCH("fixed field", past);
      c->pos = past;
      c->specline = rest(c->specline);
    } else {
      c->specline = rlcp(cons(cdr(pair), c->specline), c->specline);
    }
    return repeat_spec_k;
  } else if (consp(modifier) || regexp(modifier)) { /* var bound over text matched by form */
    cons_bind (new_bindings, new_pos,
               match_line(ml_specline(*c, modifiers)));

    if (!new_pos) {
      LOG_MISMATCH("var spanning form");
      return nil;
    }

    new_pos = minus(new_pos, c->base);

    LOG_MATCH("var spanning form", new_pos);
    if (sym)
      c->bindings = acons(sym, sub_str(c->dataline, c->pos, new_pos), new_bindings);
    c->pos = new_pos;
    /* This may have another variable attached */
    if (next) {
      c->specline = rlcp(cons(next, rest(c->specline)), c->specline);
      return repeat_spec_k;
    }
  } else if (integerp(modifier)) { /* fixed field */
    val past = plus(c->pos, modifier);
    if (length_str_lt(c->dataline, past) || lt(past, c->pos))
    {
      LOG_MISMATCH("count based var");
      return nil;
    }
    LOG_MATCH("count based var", past);
    if (sym)
      c->bindings = acons(sym, trim_str(sub_str(c->dataline, c->pos, past)), c->bindings);
    c->pos = past;
    /* This may have another variable attached */
    if (next) {
      c->specline = rlcp(cons(next, rest(c->specline)), c->specline);
      return repeat_spec_k;
    }
  } else if (modifier && modifier != t) {
    sem_error(elem, lit("invalid modifier ~s on variable ~s"),
              modifier, sym, nao);
  } else if (next == nil) { /* no modifier, no elem -> to end of line */
    if (sym)
      c->bindings = acons(sym, sub_str(c->dataline, c->pos, nil), c->bindings);
    c->pos = length_str(c->dataline);
  } else if (type(next) == STR) {
    val find = search_str(c->dataline, next, c->pos, modifier);
    if (!find) {
      LOG_MISMATCH("var delimiting string");
      return nil;
    }
    LOG_MATCH("var delimiting string", find);
    if (sym)
      c->bindings = acons(sym, sub_str(c->dataline, c->pos, find), c->bindings);
    c->pos = plus(find, length_str(next));
  } else if (regexp(next)) {
    val find = search_regex(c->dataline, next, c->pos, modifier);
    val fpos = car(find);
    val flen = cdr(find);
    if (!find) {
      LOG_MISMATCH("var delimiting regex");
      return nil;
    }
    LOG_MATCH("var delimiting regex", fpos);
    if (sym)
      c->bindings = acons(sym, sub_str(c->dataline, c->pos, fpos), c->bindings);
    c->pos = if3(flen == t, t, plus(fpos, flen));
  } else if (consp(next)) {
    val op = first(next);

    if (op == var_s) {
      /* Unbound var followed by var: the following one must either
         be bound, or must specify a regex. */
      val second_sym = second(next);
      val next_modifiers = third(next);
      val next_modifier = first(next_modifiers);
      val pair = if2(second_sym, tx_lookup_var(second_sym, c->bindings));

      if (gt(length_list(next_modifiers), one)) {
        sem_error(elem, lit("multiple modifiers on variable ~s"),
                  second_sym, nao);
      }

      if (!pair && regexp(next_modifier)) {
        val find = search_regex(c->dataline, next_modifier, c->pos, modifier);
        val fpos = car(find);
        val flen = cdr(find);

        if (!find) {
          LOG_MISMATCH("double var regex");
          return nil;
        }

        /* Text from here to start of regex match goes to this
           variable. */
        if (sym)
          c->bindings = acons(sym, sub_str(c->dataline, c->pos, fpos),
                              c->bindings);
        /* Text from start of regex match to end goes to the
           second variable */
        if (second_sym)
          c->bindings = acons(second_sym,
                              sub_str(c->dataline, fpos, plus(fpos, flen)),
                              c->bindings);
        LOG_MATCH("double var regex (first var)", fpos);
        c->pos = fpos;
        LOG_MATCH("double var regex (second var)", plus(fpos, flen));
        c->pos = plus(fpos, flen);
        return next_spec_k;
      } else if (!pair) {
        sem_error(elem, lit("consecutive unbound variables"), nao);
      } else {
      /* Re-generate a new spec in which the next variable
         is replaced by its value, and repeat. */
        val r = rest(c->specline);
        c->specline = rlcp(cons(elem, rlcp(cons(cdr(pair), r), r)), r);
        return repeat_spec_k;
      }
    } else if (op == text_s) {
      val text_only_spec = rlcp(cons(next, nil), next);
      val find = search_match(c, modifier, text_only_spec);
      val fpos = car(find);
      if (!find) {
        LOG_MISMATCH("var delimiting text compound");
        return nil;
      }
      LOG_MATCH("var delimiting text compound", fpos);
      if (sym)
        c->bindings = acons(sym, sub_str(c->dataline, c->pos, fpos), c->bindings);
      c->pos = fpos;
      return repeat_spec_k;
    } else if (consp(op) || stringp(op)) {
      cons_bind (find, len, search_str_tree(c->dataline, next, c->pos, modifier));
      if (!find) {
        LOG_MISMATCH("string");
        return nil;
      }
      if (sym)
        c->bindings = acons(sym, sub_str(c->dataline, c->pos, find), c->bindings);
      c->pos = plus(find, len);
    } else {
      val find = search_match(c, modifier, c->specline);
      val fpos = car(find);
      if (!find) {
        LOG_MISMATCH("var delimiting spec");
        return nil;
      }
      LOG_MATCH("var delimiting spec", fpos);
      if (sym)
        c->bindings = acons(sym, sub_str(c->dataline, c->pos, fpos), c->bindings);
      c->pos = fpos;
      return repeat_spec_k;
    }
  } else {
    sem_error(elem, lit("variable followed by invalid element: ~s"), next, nao);
  }

  return next_spec_k;
}

static val h_skip(match_line_ctx *c)
{
  val elem = first(c->specline);
  val max = tleval_144(elem, second(elem), c->bindings);
  val min = tleval_144(elem, third(elem), c->bindings);
  cnum cmax = if3(max, c_num(max), 0);
  cnum cmin = if3(min, c_num(min), 0);
  val greedy = eq(max, greedy_k);
  val last_good_result = nil, last_good_pos = nil;

  if (!rest(c->specline)) {
    debuglf(elem,
            lit("skip to end of line ~a:~d"), c->file, c->data_lineno, nao);
    return cons(c->bindings, t);
  }

  {
    cnum reps_max = 0, reps_min = 0;

    while (length_str_gt(c->dataline, c->pos) && min && reps_min < cmin) {
      c->pos = plus(c->pos, one);
      reps_min++;
      consume_prefix(c);
    }

    if (min) {
      if (reps_min != cmin) {
        debuglf(elem,
                lit("skipped only ~d/~d chars to ~a:~d:~d"),
                num(reps_min), num(cmin),
                c->file, c->data_lineno, c->pos, nao);
        return nil;
      }

      debuglf(elem, lit("skipped ~d chars to ~a:~d:~d"),
              num(reps_min), c->file, c->data_lineno, c->pos, nao);
    }

    while (greedy || !max || reps_max++ < cmax) {
      val result = match_line(ml_specline(*c, rest(c->specline)));

      if (result) {
        if (greedy) {
          last_good_result = result;
          last_good_pos = c->pos;
        } else {
          LOG_MATCH("skip", c->pos);
          return result;
        }
      }

      if (length_str_le(c->dataline, c->pos))  {
        if (last_good_result) {
          LOG_MATCH("greedy skip", last_good_pos);
          return last_good_result;
        }
        break;
      }

      c->pos = plus(c->pos, one);
      consume_prefix(c);
    }
  }

  LOG_MISMATCH("skip");
  return nil;
}

static val h_block(match_line_ctx *c)
{
  val elem = first(c->specline);
  val name = second(elem);
  val specs = car(third(elem));

  {
    uw_block_begin(name, result);
    result = match_line(ml_specline(*c, specs));
    uw_block_end;

    if (vectorp(result)) {
      val bindings = vecref(result, zero);
      val vpos = vecref(result, one);
      val hpos = vecref(result, two);
      c->bindings = bindings;

      if (hpos && car(vpos) == c->data) {
        debuglf(elem, lit("accept from horiz. context in same line: advancing"),
                nao);
        c->pos = minus(hpos, c->base);
      } else if (hpos) {
        debuglf(elem, lit("accept from horiz. context in diff line"), nao);
      } else {
        debuglf(elem, lit("accept from vertical context"), nao);
      }
      return next_spec_k;
    } else if (result) {
      cons_bind (bindings, new_pos, result);
      c->bindings = bindings;
      c->pos = minus(new_pos, c->base);
      return next_spec_k;
    }
    return nil;
  }
}

static val h_accept_fail(match_line_ctx *c)
{
  val elem = first(c->specline);
  val sym = first(elem);
  val target = second(elem);

  uw_block_return_proto(target,
                        if2(sym == accept_s,
                            vec(c->bindings,
                                if3(c->data, cons(c->data, c->data_lineno), t),
                                plus(c->pos, c->base),
                                nao)),
                        sym);

  /* TODO: uw_block_return could just throw this */
  if (target)
    sem_error(elem, lit("~a: no block named ~a in scope"),
              sym, target, nao);
  else
    sem_error(elem, lit("~a: no anonymous block in scope"),
              sym, nao);
  return nil;
}


static val h_coll(match_line_ctx *c)
{
  val elem = first(c->specline);
  val op_sym = first(elem);
  val coll_specline = second(elem);
  val until_last_specline = third(elem);
  cons_bind (ul_sym, ul_args, until_last_specline);
  cons_bind (ul_opts, spec, ul_args);
  val ul_match = nil;
  val args = fourth(elem);
  val bindings_coll = nil;
  val last_bindings = nil;
  val max = tleval_144(elem, getplist(args, maxgap_k), c->bindings);
  val min = tleval_144(elem, getplist(args, mingap_k), c->bindings);
  val gap = tleval_144(elem, getplist(args, gap_k), c->bindings);
  val times = tleval_144(elem, getplist(args, times_k), c->bindings);
  val mintimes = tleval_144(elem, getplist(args, mintimes_k), c->bindings);
  val maxtimes = tleval_144(elem, getplist(args, maxtimes_k), c->bindings);
  val chars = tleval_144(elem, getplist(args, chars_k), c->bindings);
  val counter_spec = getplist(args, counter_k);
  val consp_counter = consp(counter_spec);
  val counter = if3(consp_counter, first(counter_spec), counter_spec);
  val counter_base = if3(consp_counter,
                         tleval(elem,
                                second(counter_spec),
                                c->bindings), zero);
  val counter_binding = if2(counter, cons(counter, nil));
  val bindings_with_counter = if2(counter, cons(counter_binding, nil));
  val have_vars;
  val vars = getplist_f(args, vars_k, mkcloc(have_vars));
  cnum cmax = if3(gap, c_num(gap), if3(max, c_num(max), 0));
  cnum cmin = if3(gap, c_num(gap), if3(min, c_num(min), 0));
  cnum mincounter = cmin, maxcounter = 0;
  cnum ctimax = if3(times, c_num(times), if3(maxtimes, c_num(maxtimes), 0));
  cnum ctimin = if3(times, c_num(times), if3(mintimes, c_num(mintimes), 0));
  cnum cchars = if3(chars, c_num(chars), 0);
  cnum timescounter = 0, charscounter = 0;
  val iter;

  if (op_sym == rep_s) {
    if (have_vars)
      sem_error(elem, lit("~s: coll takes :vars, rep does not"),
                op_sym, nao);
    have_vars = t;
  }

  if (counter && !bindable(counter))
    sem_error(elem, lit("~s: ~s specified as :counter isn't a bindable symbol"),
              op_sym, counter, nao);


  vars = vars_to_bindings(elem, vars, c->bindings);

  if (((times || maxtimes) && ctimax == 0) || (chars && cchars == 0))
    return next_spec_k;

  for (;;) {
    val new_bindings = nil, new_pos = nil;

    if ((gap || min) && mincounter < cmin)
      goto next_coll;

    if (chars && charscounter++ >= cchars)
      break;

    {
      if (counter) {
        rplacd(counter_binding, plus(num(timescounter), counter_base));
        rplacd(bindings_with_counter, c->bindings);
        cons_set (new_bindings, new_pos,
                  match_line(ml_bindings_specline(*c, bindings_with_counter, coll_specline)));

        if (!new_bindings) {
          rplacd(counter_binding, nil);
          new_bindings = bindings_with_counter;
        }
      } else {
        cons_set (new_bindings, new_pos,
                  match_line(ml_specline(*c, coll_specline)));
      }

      if (until_last_specline) {
        uses_or2;
        cons_bind (until_last_bindings, until_pos,
                   match_line(ml_bindings_specline(*c,
                                                   or2(new_bindings, c->bindings),
                                                   spec)));

        if (until_pos) {
          until_pos = minus(until_pos, c->base);

          LOG_MATCH("until/last", until_pos);
          if (ul_sym == last_s) {
            last_bindings = set_diff(until_last_bindings,
                                     new_bindings, eq_f, nil);
            c->pos = until_pos;
          }
          ul_match = t;
          break;
        } else {
          LOG_MISMATCH("until/last");
        }
      }

      if (new_pos) {
        list_collect_decl (missing, ptail);
        val strictly_new_bindings = set_diff(new_bindings,
                                             c->bindings, eq_f, nil);
        val have_new = strictly_new_bindings;

        new_pos = minus(new_pos, c->base);
        LOG_MATCH("coll", new_pos);

        for (iter = vars; iter; iter = cdr(iter)) {
          cons_bind (var, dfl, car(iter));
          val exists = tx_lookup_var(var, new_bindings);

          if (!exists) {
            if (dfl == noval_s)
              ptail = list_collect(ptail, var);
            else
              strictly_new_bindings = acons(var, dfl, strictly_new_bindings);
          }
        }

        if (have_new && missing)
          sem_error(elem, lit("~s failed to bind ~a"),
                    op_sym, missing, nao);

        for (iter = strictly_new_bindings; iter; iter = cdr(iter))
        {
          val binding = car(iter);
          val vars_binding = assoc(car(binding), vars);

          if (!have_vars || vars_binding) {
            val existing = assoc(car(binding), bindings_coll);
            bindings_coll = acons_new(car(binding),
                                      cons(cdr(binding), cdr(existing)),
                                      bindings_coll);
          }
        }
      }

      if (new_pos && !equal(new_pos, c->pos)) {
        c->pos = new_pos;
        bug_unless (new_pos != t && length_str_ge(c->dataline, c->pos));

        timescounter++;

        if ((times || maxtimes) && timescounter >= ctimax)
          break;

        mincounter = 0;
        maxcounter = 0;
      } else {
next_coll:
        mincounter++;
        if ((gap || max) && ++maxcounter > cmax)
          break;
        c->pos = plus(c->pos, one);
      }

      if (c->pos == t || length_str_le(c->dataline, c->pos))
        break;
    }

    consume_prefix(c);
  }

  if ((times || mintimes) && timescounter < ctimin) {
    debuglf(elem, lit("fewer than ~d iterations collected"),
            num(ctimin), nao);
    return nil;
  }

  if (!ul_match && ul_opts && memq(mandatory_k, ul_opts)) {
    debuglf(elem, lit("~s didn't match mandatory until/last"),
            op_sym, nao);
    return nil;
  }

  if (!bindings_coll)
    debuglf(elem, lit("nothing was collected"), nao);

  for (iter = bindings_coll; iter; iter = cdr(iter)) {
    val pair = car(iter);
    val rev = cons(car(pair), nreverse(cdr(pair)));
    c->bindings = cons(rev, c->bindings);
  }

  if (last_bindings) {
    c->bindings = set_diff(c->bindings, last_bindings, eq_f, car_f);
    c->bindings = nappend2(last_bindings, c->bindings);
  }

  /* If nothing was collected, but vars were specified,
     then bind empty lists for the vars. */
  if (!bindings_coll && vars) {
    for (iter = vars; iter; iter = cdr(iter)) {
      val sym = car(car(iter));
      val exists = tx_lookup_var(sym, c->bindings);
      if (!exists)
        c->bindings = acons(sym, nil, c->bindings);
    }
  }

  return next_spec_k;
}

static val h_parallel(match_line_ctx *c)
{
  uses_or2;
  elem_bind(elem, directive, c->specline);
  val specs = third(elem);
  val plist = fourth(elem);
  val all_match = t;
  val some_match = nil;
  val max_pos = c->pos;
  val choose_shortest = getplist(plist, shortest_k);
  val choose_longest = getplist(plist, longest_k);
  val choose_sym = or2(choose_longest, choose_shortest);
  val choose_bindings = c->bindings, choose_pos = c->pos;
  val choose_minmax = choose_longest ? num(-1) : num(NUM_MAX);
  val resolve = getplist(plist, resolve_k);
  val resolve_ub_vars = nil;
  val resolve_bindings = nil;
  val iter;

  if (choose_longest && choose_shortest)
    sem_error(elem, lit("choose: both :shortest and :longest specified"), nao);

  if (directive == choose_s && !choose_sym)
    sem_error(elem, lit("choose: criterion not specified"), nao);


  if (resolve) {
    for (iter = resolve; iter; iter = cdr(iter)) {
      val var = car(iter);
      if (!tx_lookup_var(var, c->bindings))
        push(var, &resolve_ub_vars);
    }
  }

  for (iter = specs; iter != nil; iter = cdr(iter)) {
    val nested_spec = first(iter);
    cons_bind (new_bindings, new_pos,
               match_line(ml_specline(*c, nested_spec)));

    if (new_pos) {
      some_match = t;
      new_pos = minus(new_pos, c->base);

      if (resolve_ub_vars) {
        val uiter;
        for (uiter = resolve_ub_vars; uiter; uiter = cdr(uiter)) {
          val ubvar = car(uiter);
          val exists = tx_lookup_var(ubvar, new_bindings);

          if (exists)
            resolve_bindings = acons_new(ubvar, cdr(exists), resolve_bindings);
        }

        new_bindings = alist_remove(new_bindings, resolve_ub_vars);
      }

      if (new_pos == t || gt(new_pos, max_pos))
        max_pos = new_pos;

      if (directive == choose_s) {
        val binding = choose_sym ? tx_lookup_var(choose_sym, new_bindings) : nil;
        val value = cdr(binding);

        if (value) {
          val len = length_str(value);

          if ((choose_longest && gt(len, choose_minmax)) ||
              (choose_shortest && lt(len, choose_minmax)))
          {
            choose_minmax = len;
            choose_bindings = new_bindings;
            choose_pos = new_pos;
          }
        }
      } else {
        c->bindings = new_bindings;
      }
      if (directive == cases_s || directive == none_s)
        break;
    } else {
      all_match = nil;
      if (directive == all_s)
        break;
    }
  }

  if (directive == all_s && !all_match) {
    debuglf(elem, lit("all: some clauses didn't match"), nao);
    return nil;
  }

  if ((directive == some_s || directive == cases_s) && !some_match) {
    debuglf(elem, lit("some/cases: no clauses matched"), nao);
    return nil;
  }

  if (directive == none_s && some_match) {
    debuglf(elem, lit("none: some clauses matched"), nao);
    return nil;
  }

  /* No check for maybe, since it always succeeds. */


  if (resolve_bindings)
    c->bindings = nappend2(resolve_bindings, c->bindings);

  if (directive == choose_s) {
    c->bindings = choose_bindings;
    c->pos = choose_pos;
  } else {
    c->pos = max_pos;
  }

  return next_spec_k;
}

static val h_trailer(match_line_ctx *c)
{
  val ret = nil;

  uw_simple_catch_begin;

  {
    val result = match_line(ml_specline(*c, rest(c->specline)));
    cons_bind (new_bindings, new_pos, result);
    val elem = first(c->specline);

    if (!new_pos) {
      LOG_MISMATCH("trailer");
      ret = nil;
    }

    LOG_MATCH("trailer", new_pos);
    ret = cons(new_bindings, plus(c->pos, c->base));
  }

  uw_unwind {
    uw_frame_t *ex = uw_current_exit_point();
    if (ex && ex->uw.type == UW_BLOCK && ex->bl.protocol == accept_s) {
      set(vecref_l(ex->bl.result, one), cons(c->data, c->data_lineno));
      set(vecref_l(ex->bl.result, two), plus(c->pos, c->base));
    }
  }

  uw_catch_end;

  return ret;
}

static val fun_resolve_bindings(val bindings, val ub_p_a_pairs,
                                val new_bindings, val sym, val elem)
{
  val piter;

  for (piter = ub_p_a_pairs; piter; piter = cdr(piter))
  {
    cons_bind (param, arg, car(piter));

    if (symbolp(arg)) {
      val newbind = tx_lookup_var(param, new_bindings);
      if (newbind) {
        bindings = dest_bind(elem, bindings, arg, cdr(newbind), equal_f);
        if (bindings == t) {
          debuglf(elem,
              lit("binding mismatch on ~a "
                  "when returning from ~a"), arg, sym, nao);
          break;
        }
      }
    }
  }

  return bindings;
}

static void fun_intercept_accept(val bindings, val ub_p_a_pairs,
                                 val sym, val elem)
{
  uw_frame_t *ex = uw_current_exit_point();
  if (ex && ex->uw.type == UW_BLOCK && ex->bl.protocol == accept_s) {
    loc ab_loc = vecref_l(ex->bl.result, zero);
    val accept_bindings = deref(ab_loc);
    bindings = fun_resolve_bindings(bindings, ub_p_a_pairs,
                                    accept_bindings, sym, elem);
    if (bindings == t)
      bindings = nil;

    set(ab_loc, bindings);
  }
}

static val h_fun(match_line_ctx *c)
{
  val elem = first(c->specline);
  val sym = first(elem);
  val func = cdr(uw_get_func(sym));

  if (func) {
    val args = rest(elem);
    val params = car(func);
    val ub_p_a_pairs = nil;
    val body = cdr(func);
    val piter, aiter;
    val bindings_cp = copy_list(c->bindings);

    if (!equal(length(args), length_list(params)))
      sem_error(elem, lit("function ~a takes ~d argument(s)"),
                sym, length_list(params), nao);

    for (piter = params, aiter = args; piter;
         piter = cdr(piter), aiter = cdr(aiter))
    {
      val param = car(piter);
      val arg = car(aiter);

      if (arg && bindable(arg)) {
        val val = tx_lookup_var(arg, c->bindings);
        if (val) {
          bindings_cp = cons(cons(param, cdr(val)), bindings_cp);
        } else {
          bindings_cp = alist_nremove1(bindings_cp, param);
          ub_p_a_pairs = cons(cons(param, arg), ub_p_a_pairs);
        }
      } else {
        val val = txeval(elem, arg, c->bindings);
        bindings_cp = cons(cons(param, val), bindings_cp);
      }
    }

    {
      args_decl_list(args, ARGS_MIN, bindings_cp);
      uw_block_begin(nil, result);
      uw_env_begin;
      debug_frame(sym, args, ub_p_a_pairs, c->bindings, c->dataline, c->data_lineno, c->pos);

      uw_simple_catch_begin;

      result = match_line(ml_bindings_specline(*c, bindings_cp, body));

      uw_unwind {
        fun_intercept_accept(c->bindings, ub_p_a_pairs, sym, elem);
      }

      uw_catch_end;

      debug_end;
      uw_env_end;
      uw_block_end;

      if (!result) {
        debuglf(elem, lit("function (~s ~s) failed"), sym, args, nao);
        return nil;
      }

      {
        cons_bind (new_bindings, success, result);

        c->bindings = fun_resolve_bindings(c->bindings, ub_p_a_pairs,
                                           new_bindings, sym, elem);

        if (c->bindings == t)
          return nil;

        c->pos = minus(success, c->base);
      }
    }

    return next_spec_k;
  }

  return decline_k;
}

static val h_eol(match_line_ctx *c)
{
  val elem = first(c->specline);

  if (length_str_le(c->dataline, c->pos)) {
    LOG_MATCH("eol", c->pos);
    return next_spec_k;
  }
  LOG_MISMATCH("eol");
  return nil;
}

static val h_chr(match_line_ctx *c)
{
  val elem = first(c->specline);
  val args = rest(elem);
  val pat = car(args);

  if (!args || rest(args))
      sem_error(elem, lit("chr directive takes one argument"), nao);

  c->bindings = dest_bind(elem, c->bindings, pat, c->pos, eql_f);

  if (c->bindings == t) {
    debuglf(elem, lit("chr mismatch (position ~d vs. ~s)"), c->pos, pat, nao);
    return nil;
  }

  return next_spec_k;
}

typedef struct {
  val spec, files, curfile, bindings, data, data_lineno;
} match_files_ctx;

static match_files_ctx mf_all(val spec, val files, val bindings,
                              val data, val curfile);

static val v_fun(match_files_ctx *c);

static val h_call(match_line_ctx *c)
{
  val saved_specline = c->specline;
  val elem = first(saved_specline);
  val args = rest(elem);
  val funexpr = first(args);
  val funval = tleval_144(c->specline, funexpr, c->bindings);
  val argexprs = rest(args);
  val call = cons(funval, argexprs);
  val new_specline = cons(call, nil);
  val ret;

  c->specline = new_specline;

  ret = h_fun(c);

  c->specline = saved_specline;

  if (ret == decline_k) {
    val spec = cons(new_specline, nil);
    match_files_ctx vc = mf_all(spec, nil, c->bindings, nil, c->file);
    val vresult = v_fun(&vc);

    if (vresult == next_spec_k) {
      c->bindings = vc.bindings;
      return vresult;
    } else if (vresult == decline_k) {
      sem_error(elem, lit("call: function ~s not found"), funval, nao);
    }

    return vresult;
  }

  return ret;
}

static val do_match_line(match_line_ctx *c)
{
  val lfe_save = set_last_form_evaled(nil);

  debug_enter;

  while (c->specline) {
    val elem = first(c->specline);

    set_last_form_evaled(elem);

    if (c->pos == t)
      c->pos = length_str(c->dataline);

    consume_prefix(c);

    debug_check(c->specline, c->bindings, c->dataline, c->data_lineno,
                c->pos, c->base);

    switch (type(elem)) {
    case CONS: /* directive */
      {
        val directive = first(elem);

        if (consp(directive) || stringp(directive)) {
          val len = match_str_tree(c->dataline, elem, c->pos);
          val newpos;

          if (!len) {
            LOG_MISMATCH("string tree");
            debug_return (nil);
          }

          newpos = plus(c->pos, len);
          LOG_MATCH("string tree", newpos);
          c->pos = newpos;
        } else {
          val entry = gethash(h_directive_table, directive);
          if (entry) {
            h_match_func hmf = coerce(h_match_func, cptr_get(entry));
            val result = hmf(c);

            if (result == next_spec_k) {
              break;
            } else if (result == repeat_spec_k) {
              continue;
            } else {
              debug_return (result);
            }
          } else {
            val result = h_fun(c);

            if (result == next_spec_k) {
              break;
            } else if (result == repeat_spec_k) {
              continue;
            } else if (result == decline_k) {
              val spec = rlcp(cons(cons(elem, nil), nil), elem);
              match_files_ctx vc = mf_all(spec, nil, c->bindings,
                                          nil, c->file);
              val vresult = v_fun(&vc);

              if (vresult == next_spec_k) {
                c->bindings = vc.bindings;
                break;
              } else if (vresult == repeat_spec_k) {
                c->bindings = vc.bindings;
                continue;
              } else if (vresult == decline_k) {
                if (gethash(v_directive_table, directive))
                  sem_error(elem, lit("~a only exists as a vertical directive"),
                            directive, nao);
                else
                  sem_error(elem, lit("no such function or directive: ~a"),
                            directive, nao);
              } else {
                debug_return (vresult);
              }
            } else {
              debug_return (result);
            }
          }
        }
      }
      break;
    case STR:
    case LSTR:
    case LIT:
      {
        val newpos;
        if (!match_str(c->dataline, elem, c->pos)) {
          LOG_MISMATCH("string");
          debug_return (nil);
        }
        newpos = plus(c->pos, length_str(elem));
        LOG_MATCH("string", newpos);
        c->pos = newpos;
        break;
      }
    case COBJ:
      if (elem->co.cls == regex_s) {
        val past = match_regex(c->dataline, elem, c->pos);
        if (nilp(past)) {
          LOG_MISMATCH("regex");
          debug_return (nil);
        }
        LOG_MATCH("regex", past);
        c->pos = past;
        break;
      }
      /* fallthrough */
    default:
      sem_error(elem, lit("unsupported object in spec: ~s"), elem, nao);
    }

    c->specline = cdr(c->specline);
  }

  debug_return (cons(c->bindings, plus(c->pos, c->base)));
  debug_leave;

  set_last_form_evaled(lfe_save);
}

static val match_line(match_line_ctx c)
{
  return do_match_line(&c);
}

static val match_line_completely(match_line_ctx c)
{
  val result = do_match_line(&c);

  if (result) {
    val new_pos = cdr(result);

    if (new_pos != t && length_str_gt(c.dataline, minus(new_pos, c.base))) {
      debuglf(c.specline, lit("spec only matches line to position ~d: ~a"),
              new_pos, c.dataline, nao);
      return nil;
    }
  }

  return result;
}

static val tx_subst_vars(val spec, val bindings, val filter)
{
  if (opt_compat && opt_compat <= 128) {
    list_collect_decl(out, iter);
    uw_env_begin;

    uw_set_match_context(cons(spec, bindings));

    while (spec) {
      val elem = first(spec);

      if (consp(elem)) {
        val sym = first(elem);

        if (sym == var_s) {
          val expr = second(elem);
          val modifiers = third(elem);
          val str = txeval(spec, expr, bindings);

          /* If the object is a list, we let format_field deal with the
             conversion to text, because the modifiers influence how
             it is done. */
          if (!stringp(str) && !listp(str))
            str = tostringp(str);

          if (modifiers) {
            spec = cons(format_field(str, modifiers, filter,
                                     curry_123_2(func_n3(txeval), spec, bindings)),
                        rest(spec));
          } else {
            if (listp(str))
              str = cat_str(mapcar(func_n1(tostringp), str), lit(" "));

            spec = cons(filter_string_tree(filter, str), rest(spec));
          }

          continue;
        } else if (sym == quasi_s) {
          val nested = tx_subst_vars(rest(elem), bindings, filter);
          iter = list_collect_append(iter, nested);
          spec = cdr(spec);
          continue;
        } else {
          if (opt_compat && opt_compat < 100) {
            val result = tleval(spec, elem, bindings);
            spec = cons(filter_string_tree(filter, tostringp(result)), rest(spec));
            continue;
          } else {
            val str = tleval(spec, elem, bindings);
            if (listp(str))
              str = cat_str(mapcar(func_n1(tostringp), str), lit(" "));
            else if (!stringp(str))
              str = tostringp(str);
            spec = cons(filter_string_tree(filter, tostringp(str)), rest(spec));
            continue;
          }
        }
      }

      iter = list_collect(iter, elem);
      spec = cdr(spec);
    }

    uw_env_end;
    return out;
  } else {
    val saved_de = set_dyn_env(make_env(bindings, nil, nil));
    val out;

    uw_set_match_context(cons(spec, bindings));
    out = subst_vars(spec, nil, filter);
    set_dyn_env(saved_de);

    return out;
  }
}

static val do_txeval(val spec, val form, val bindings, val allow_unbound)
{
  val ret = nil;
  uw_mark_frame;
  uses_or2;
  uw_catch_begin (cons(query_error_s, nil), exc_sym, exc);

  if (!form)
    uw_fast_return(nil);

  {
    if (!form || regexp(form)) {
      ret = form;
    } else if (bindable(form)) {
      val binding = or2(assoc(form, bindings), lookup_var(nil, form));
      if (!binding) {
        if (allow_unbound)
          ret = noval_s;
        else
          sem_error(spec, lit("unbound variable ~s"), form, nao);
      } else {
        ret = cdr(binding);
      }
    } else if (consp(form)) {
      val sym = first(form);
      if (sym == quasi_s) {
        ret = cat_str(tx_subst_vars(rest(form), bindings, nil), nil);
      } else if (sym == quasilist_s) {
        val iter;
        list_collect_decl (out, tail);
        for (iter = rest(form); iter != nil; iter = cdr(iter))
          tail = list_collect(tail, tx_subst_vars(cdr(car(iter)), bindings, nil));
        ret = out;
      } else if (sym == var_s || sym == expr_s) {
        ret = tleval(spec, second(form), bindings);
      } else {
        ret =  mapcar(curry_123_2(func_n3(txeval), spec, bindings), form);
      }
    } else if (stringp(form)) {
      ret = form;
    } else {
      ret = form;
    }

    uw_catch (exc_sym, exc) {
      val msg = if3(consp(exc), car(exc), exc);

      if (stringp(msg) && !equal(msg, lit("")) &&
          chr_str(msg, zero) == chr('('))
      {
        uw_throw (exc_sym, exc);
      }

      sem_error(spec, lit("~a"), exc, nao);
    }

    uw_unwind { }
  }
  uw_catch_end;

  return ret;
}

static val txeval(val spec, val form, val bindings)
{
  return do_txeval(spec, form, bindings, nil);
}

static val txeval_allow_ub(val spec, val form, val bindings)
{
  return do_txeval(spec, form, bindings, t);
}

static val complex_open(val name, val output, val append,
                        val nothrow, val from_cmdline)
{
  int old_hacky_open = opt_compat && opt_compat <= 142;

  if (streamp(name)) {
    return name;
  } else {
    val fc = car(name);
    val result = nil;

    if (old_hacky_open && fc == chr('$') && output)
      uw_throwf(query_error_s, lit("cannot output to directory: ~a"),
                name, nao);

    uw_catch_begin (if2(nothrow, cons(error_s, nil)), exc_sym, exc);

    if (from_cmdline && fc == chr('-')) {
      result = output ? std_output : std_input;
    } else if (old_hacky_open && fc == chr('!')) {
      result = open_command(cdr(name), output ? lit("w") : lit("r"));
    } else if (old_hacky_open && fc == chr('$')) {
      result = open_directory(cdr(name));
    } else {
      result = open_file(name,
                         output ? append ? lit("a") : lit("w") : lit("r"));
    }

    uw_catch (exc_sym, exc) { (void) exc; }

    uw_unwind { }

    uw_catch_end;

    return result;
  }
}

static val robust_length(val obj)
{
  if (obj == nil)
    return zero;
  if (atom(obj))
    return negone;
  return length_list(obj);
}

static val bind_car(val bind_cons)
{
  return if3(consp(cdr(bind_cons)),
               cons(car(bind_cons), car(cdr(bind_cons))),
               bind_cons);
}

static val bind_cdr(val bind_cons)
{
  return if3(consp(cdr(bind_cons)),
               cons(car(bind_cons), cdr(cdr(bind_cons))),
               bind_cons);
}

static val extract_vars(val output_spec)
{
  list_collect_decl (vars, tai);

  if (consp(output_spec)) {
    val sym = first(output_spec);
    if (sym == var_s) {
      val name = second(output_spec);
      val modifiers = third(output_spec);

      if (bindable(name))
        tai = list_collect(tai, name);
      else
        tai = list_collect_nconc(tai, extract_vars(name));

      for (; modifiers; modifiers = cdr(modifiers)) {
        val mod = car(modifiers);
        if (bindable(mod)) {
          tai = list_collect(tai, mod);
        } else if (consp(mod)) {
          val msym = car(mod);

          if (msym == dwim_s) {
            val arg = second(mod);

            if (bindable(arg)) {
              tai = list_collect(tai, arg);
            } else if (consp(arg) && car(arg) == rcons_s) {
              val f = second(arg);
              val t = third(arg);
              if (bindable(f))
                tai = list_collect(tai, f);
              if (bindable(t))
                tai = list_collect(tai, t);
            }
          }
        }
      }
    } else if (sym != expr_s) {
      for (; output_spec; output_spec = cdr(output_spec))
        tai = list_collect_nconc(tai, extract_vars(car(output_spec)));
    }
  }

  return vars;
}

static val extract_bindings(val bindings, val output_spec, val vars)
{
  list_collect_decl (bindings_out, ptail);
  list_collect_decl (var_list, vtail);

  vtail = list_collect_nconc(vtail, extract_vars(output_spec));

  for (; vars; vars = cdr(vars)) {
    val var = car(vars);
    if (consp(var)) {
      val form = cadr(var);
      val value = tleval(output_spec, form, bindings);
      bindings = cons(cons(car(var), value), bindings);
      vtail = list_collect(vtail, car(var));
    } else {
      vtail = list_collect(vtail, var);
    }
  }

  for (; bindings; bindings = cdr(bindings)) {
    val binding = car(bindings);
    val sym = car(binding);
    if (assoc(sym, bindings_out))
      continue;
    if (memq(sym, var_list))
      ptail = list_collect(ptail, binding);
  }

  for (; var_list; var_list = cdr(var_list)) {
    val sym = car(var_list);
    if (assoc(sym, bindings_out)) {
      continue;
    } else {
      val binding = lookup_var(nil, sym);
      if (binding)
        ptail = list_collect(ptail, binding);
    }
  }

  return bindings_out;
}

static void do_output_line(val bindings, val specline, val filter, val out)
{
  if (specline == t)
    return;

  for (; specline; specline = rest(specline)) {
    val elem = first(specline);

    switch (type(elem)) {
    case CONS:
      {
        val directive = first(elem);

        if (directive == var_s) {
          val str = cat_str(tx_subst_vars(cons(elem, nil),
                                          bindings, filter), nil);
          if (str == nil)
            sem_error(specline, lit("bad substitution: ~a"),
                      second(elem), nao);
          put_string(str, out);
        } else if (directive == rep_s) {
          val clauses = cdr(elem);
          val args = pop(&clauses);
          val main_clauses = pop(&clauses);
          val single_clauses = pop(&clauses);
          val first_clauses = pop(&clauses);
          val last_clauses = pop(&clauses);
          val empty_clauses = pop(&clauses);
          val mod_clauses = pop(&clauses);
          val modlast_clauses = pop(&clauses);
          val counter_spec = getplist(args, counter_k);
          val consp_counter = consp(counter_spec);
          val counter = if3(consp_counter, first(counter_spec), counter_spec);
          val counter_base = if3(consp_counter,
                                 tleval(elem,
                                        second(counter_spec),
                                        bindings), zero);
          val vars = getplist(args, vars_k);
          val bind_cp = extract_bindings(bindings, elem, vars);
          val max_depth = reduce_left(func_n2(max2),
                                      bind_cp, zero,
                                      chain(func_n1(cdr),
                                            func_n1(robust_length),
                                            nao));

          if (counter && !bindable(counter))
            sem_error(elem, lit(":counter requires a bindable symbol, not ~s"),
                      counter, nao);

          if (equal(max_depth, zero) && empty_clauses) {
            do_output_line(nappend2(bind_cp, bindings), empty_clauses, filter, out);
          } else if (equal(max_depth, one) && single_clauses) {
            val bind_a = nappend2(mapcar(func_n1(bind_car), bind_cp), bindings);
            do_output_line(bind_a, single_clauses, filter, out);
          } else if (!zerop(max_depth)) {
            val counter_var = if2(counter, cons(counter, nil));
            val counter_bind = if2(counter, cons(counter_var, nil));
            cnum i;

            for (i = 0; i < c_num(max_depth); i++) {
              val bind_a = nappend2(mapcar(func_n1(bind_car), bind_cp), bindings);
              val bind_d = mapcar(func_n1(bind_cdr), bind_cp);

              if (counter) {
                rplacd(counter_var, plus(num(i), counter_base));
                rplacd(counter_bind, bind_a);
                bind_a = counter_bind;
              }

              if (i == 0 && first_clauses) {
                do_output_line(bind_a, first_clauses, filter, out);
              } else if (i == c_num(max_depth) - 1 &&
                         (last_clauses || modlast_clauses)) {
                if (modlast_clauses) {
                  val iter;
                  list_collect_decl (active_mods, ptail);

                  for (iter = modlast_clauses; iter != nil; iter = cdr(iter)) {
                    val clause = car(iter);
                    val args = first(clause);
                    val n = tleval_144(args, first(args), bind_a);
                    val m = tleval_144(args, second(args), bind_a);

                    if (eql(mod(num(i), m), n))
                      ptail = list_collect_append(ptail, rest(clause));
                  }

                  if (active_mods)
                    do_output_line(bind_a, active_mods, filter, out);
                  else if (last_clauses)
                    do_output_line(bind_a, last_clauses, filter, out);
                  else
                    goto mod_fallback;
                } else {
                  do_output_line(bind_a, last_clauses, filter, out);
                }
              } else if (mod_clauses) mod_fallback: {
                val iter;
                list_collect_decl (active_mods, ptail);

                for (iter = mod_clauses; iter != nil; iter = cdr(iter)) {
                  val clause = car(iter);
                  val args = first(clause);
                  val n = tleval_144(args, first(args), bind_a);
                  val m = tleval_144(args, second(args), bind_a);

                  if (eql(mod(num(i), m), n))
                    ptail = list_collect_append(ptail, rest(clause));
                }

                if (active_mods)
                  do_output_line(bind_a, active_mods, filter, out);
                else
                  do_output_line(bind_a, main_clauses, filter, out);
              } else {
                do_output_line(bind_a, main_clauses, filter, out);
              }

              bind_cp = bind_d;
            }
          }

        } else if (directive == expr_s) {
          if (opt_compat && opt_compat < 100) {
            format(out, lit("~a"),
                   tleval(elem, second(elem), bindings), nao);
          } else {
            val str = cat_str(tx_subst_vars(cdr(elem),
                                            bindings, filter), nil);
            if (str == nil)
              sem_error(specline, lit("bad substitution: ~a"),
                        second(elem), nao);
            put_string(str, out);
          }
        }
      }
      break;
    case STR:
    case LSTR:
    case LIT:
      put_string(elem, out);
      break;
    case 0:
      break;
    default:
      sem_error(specline,
                lit("unsupported object in output spec: ~s"), elem, nao);
    }
  }
}

static void do_output(val bindings, val specs, val filter, val out)
{
  if (specs == t)
    return;

  for (; specs; specs = cdr(specs)) {
    val specline = first(specs);
    val first_elem = first(specline);

    if (consp(first_elem)) {
      val sym = first(first_elem);

      if (sym == repeat_s) {
        val clauses = cdr(first_elem);
        val args = pop(&clauses);
        val main_clauses = pop(&clauses);
        val single_clauses = pop(&clauses);
        val first_clauses = pop(&clauses);
        val last_clauses = pop(&clauses);
        val empty_clauses = pop(&clauses);
        val mod_clauses = pop(&clauses);
        val modlast_clauses = pop(&clauses);
        val counter_spec = getplist(args, counter_k);
        val consp_counter = consp(counter_spec);
        val counter = if3(consp_counter, first(counter_spec), counter_spec);
        val counter_base = if3(consp_counter,
                               tleval(first_elem,
                                      second(counter_spec),
                                      bindings), zero);
        val vars = getplist(args, vars_k);
        val bind_cp = extract_bindings(bindings, first_elem, vars);
        val max_depth = reduce_left(func_n2(max2),
                                    bind_cp, zero,
                                    chain(func_n1(cdr),
                                          func_n1(robust_length),
                                          nao));

        if (equal(max_depth, zero) && empty_clauses) {
          do_output(nappend2(bind_cp, bindings), empty_clauses, filter, out);
        } else if (equal(max_depth, one) && single_clauses) {
          val bind_a = nappend2(mapcar(func_n1(bind_car), bind_cp), bindings);
          do_output(bind_a, single_clauses, filter, out);
        } else if (!zerop(max_depth)) {
          val counter_var = if2(counter, cons(counter, nil));
          val counter_bind = if2(counter, cons(counter_var, nil));
          cnum i;

          for (i = 0; i < c_num(max_depth); i++) {
            val bind_a = nappend2(mapcar(func_n1(bind_car), bind_cp), bindings);
            val bind_d = mapcar(func_n1(bind_cdr), bind_cp);

            if (counter) {
              rplacd(counter_var, plus(num(i), counter_base));
              rplacd(counter_bind, bind_a);
              bind_a = counter_bind;
            }

            if (i == 0 && first_clauses) {
              do_output(bind_a, first_clauses, filter, out);
            } else if (i == c_num(max_depth) - 1 &&
                       (last_clauses || modlast_clauses))
            {
              if (modlast_clauses) {
                val iter;
                list_collect_decl (active_mods, ptail);

                for (iter = modlast_clauses; iter != nil; iter = cdr(iter)) {
                  val clause = car(iter);
                  val args = first(clause);
                  val n = tleval_144(args, first(args), bind_a);
                  val m = tleval_144(args, second(args), bind_a);

                  if (eql(mod(num(i), m), n))
                    ptail = list_collect_append(ptail, rest(clause));
                }

                if (active_mods)
                  do_output(bind_a, active_mods, filter, out);
                else if (last_clauses)
                  do_output(bind_a, last_clauses, filter, out);
                else
                  goto mod_fallback;
              } else {
                do_output(bind_a, last_clauses, filter, out);
              }
            } else if (mod_clauses) mod_fallback: {
              val iter;
              list_collect_decl (active_mods, ptail);

              for (iter = mod_clauses; iter != nil; iter = cdr(iter)) {
                val clause = car(iter);
                val args = first(clause);
                val n = tleval_144(args, first(args), bind_a);
                val m = tleval_144(args, second(args), bind_a);

                if (eql(mod(num(i), m), n))
                  ptail = list_collect_append(ptail, rest(clause));
              }

              if (active_mods)
                do_output(bind_a, active_mods, filter, out);
              else
                do_output(bind_a, main_clauses, filter, out);
            } else {
              do_output(bind_a, main_clauses, filter, out);
            }

            bind_cp = bind_d;
          }
        }
        continue;
      }
    }

    do_output_line(bindings, specline, filter, out);
    put_char(chr('\n'), out);
  }
}

static match_files_ctx mf_all(val spec, val files, val bindings,
                              val data, val curfile)
{
  match_files_ctx c;
  c.spec = spec;
  c.files = files;
  c.curfile = curfile;
  c.bindings = bindings;
  c.data = data;
  c.data_lineno = if3(data, one, zero);
  return c;
}

static match_files_ctx mf_args(match_files_ctx c)
{
  match_files_ctx nc = c;
  nc.data = c.files;
  nc.curfile = lit("args");
  nc.data_lineno = one;
  return nc;
}

static match_files_ctx mf_data(match_files_ctx c, val data, val data_lineno)
{
  match_files_ctx nc = c;
  nc.data = data;
  nc.data_lineno = data_lineno;
  return nc;
}

static match_files_ctx mf_spec(match_files_ctx c, val spec)
{
  match_files_ctx nc = c;
  nc.spec = spec;
  return nc;
}

static match_files_ctx mf_spec_bindings(match_files_ctx c, val spec,
                                        val bindings)
{
  match_files_ctx nc = c;
  nc.spec = spec;
  nc.bindings = bindings;
  return nc;
}

static match_files_ctx mf_file_data(match_files_ctx c, val file,
                                    val data, val data_lineno)
{
  match_files_ctx nc = c;
  nc.files = cons(file, c.files);
  nc.curfile = file;
  nc.data = data;
  nc.data_lineno = data_lineno;
  return nc;
}

static match_files_ctx mf_from_ml(match_line_ctx ml)
{
  return mf_all(cons(ml.specline, nil), nil, ml.bindings, nil, ml.file);
}

static val match_files(match_files_ctx a);

typedef val (*v_match_func)(match_files_ctx *cout);

#define spec_bind(specline, first_spec, spec)           \
  val specline = first(spec);                           \
  val first_spec = first(specline)

static val v_skip(match_files_ctx *c)
{
  spec_bind (specline, first_spec, c->spec);

  if (rest(specline))
    return decline_k;

  c->spec = rest(c->spec);

  if (!c->spec)
    return cons(c->bindings, cons(c->data, c->data_lineno));

  {
    val skipspec = first(first(c->spec));
    val args = rest(first_spec);
    val max = tleval_144(skipspec, first(args), c->bindings);
    val min = tleval_144(skipspec, second(args), c->bindings);
    cnum cmax = if3(max, c_num(max), 0);
    cnum cmin = if3(min, c_num(min), 0);
    val greedy = eq(max, greedy_k);
    volatile val last_good_result = nil;
    volatile val last_good_line = zero;

    {
      cnum reps_max = 0, reps_min = 0;
      uw_block_begin(nil, result);

      while (c->data && min && reps_min < cmin) {
        c->data = rest(c->data);
        c->data_lineno = plus(c->data_lineno, one);
        reps_min++;
      }

      if (min) {
        if (reps_min != cmin) {
          debuglf(skipspec, lit("skipped only ~d/~d lines to ~a:~d"),
                  num(reps_min), num(cmin),
                  c->curfile, c->data_lineno, nao);
          uw_block_return(nil, nil);
        }

        debuglf(skipspec, lit("skipped ~d lines to ~a:~d"),
                num(reps_min), c->curfile,
                c->data_lineno, nao);
      }

      while (greedy || !max || reps_max++ < cmax) {
        result = match_files(*c);

        if (result) {
          if (greedy) {
            last_good_result = result;
            last_good_line = c->data_lineno;
          } else {
            debuglf(skipspec, lit("skip matched ~a:~d"), c->curfile,
                    c->data_lineno, nao);
            break;
          }
        } else {
          debuglf(skipspec, lit("skip didn't match ~a:~d"),
                  c->curfile, c->data_lineno, nao);
        }

        if (!c->data)
          break;

        debuglf(skipspec, lit("skip didn't match ~a:~d"), c->curfile,
                c->data_lineno, nao);

        c->data = rest(c->data);
        c->data_lineno = plus(c->data_lineno, one);
      }

      uw_block_end;

      if (result)
        return result;
      if (last_good_result) {
        debuglf(skipspec, lit("greedy skip matched ~a:~d"),
                c->curfile, last_good_line, nao);
        return last_good_result;
      }
    }

    debuglf(skipspec, lit("skip failed"), nao);
    return nil;
  }
}

static val v_fuzz(match_files_ctx *c)
{
  spec_bind (specline, first_spec, c->spec);

  if (rest(specline))
    return decline_k;

  c->spec = rest(c->spec);

  if (!c->spec)
    return cons(c->bindings, cons(c->data, c->data_lineno));

  {
    val fuzz_spec = first(first(c->spec));
    val args = rest(first_spec);
    val m = tleval_144(fuzz_spec, first(args), c->bindings);
    val n = tleval_144(fuzz_spec, second(args), c->bindings);
    cnum cm = if3(m, c_num(m), 0);
    cnum cn = if3(n, c_num(n), 0);

    {
      cnum reps, good;

      for (reps = 0, good = 0; reps < cn; reps++) {
        match_files_ctx fuzz_ctx = mf_spec(*c, cons(first(c->spec), nil));
        val result = match_files(fuzz_ctx);

        if (result) {
          debuglf(fuzz_spec, lit("fuzz matched ~a:~d"), c->curfile,
                  c->data_lineno, nao);
          good++;
        } else {
          debuglf(fuzz_spec, lit("fuzz didn't match ~a:~d"),
                  c->curfile, c->data_lineno, nao);
        }

        if (!c->data)
          break;
        c->data = rest(c->data);
        c->data_lineno = plus(c->data_lineno, one);
        c->spec = rest(c->spec);
        if (!c->spec) {
          if (good >= cm)
            break;
          debuglf(fuzz_spec, lit("fuzz failed ~a:~d"), c->curfile,
                  c->data_lineno, nao);
          return nil;
        }
      }

      if (reps == cn && good < cm) {
        debuglf(fuzz_spec, lit("fuzz failed ~a:~d"), c->curfile,
                c->data_lineno, nao);
        return nil;
      }

      return match_files(*c);
    }
  }
}

static val v_trailer(match_files_ctx *c)
{
  if (rest(rest(first(c->spec))))
    return decline_k;

  c->spec = rest(c->spec);

  {
    val result = nil;

    uw_simple_catch_begin;

    if (!c->spec)  {
      result = cons(c->bindings, cons(c->data, c->data_lineno));
    } else {
      cons_bind (new_bindings, success, match_files(*c));
      result = if2(success, cons(new_bindings, cons(c->data, c->data_lineno)));
    }

    /*
     * Intercept an block return initiated by accept, and rewrite
     * the data extent part of the result. If we don't do this;
     * then an accept can emanate out of the trailer block and cause
     * the data position to advance into the matched material.
     */
    uw_unwind {
      uw_frame_t *ex = uw_current_exit_point();
      if (ex && ex->uw.type == UW_BLOCK && ex->bl.protocol == accept_s) {
        set(vecref_l(ex->bl.result, one), cons(c->data, c->data_lineno));
        set(vecref_l(ex->bl.result, two), nil);
      }
    }

    uw_catch_end;

    return result;
  }
}

val freeform_prepare(val vals, match_files_ctx *c, match_line_ctx *mlc);

static val v_freeform(match_files_ctx *c)
{
  spec_bind (specline, first_spec, c->spec);

  val args = rest(first_spec);
  val vals = mapcar(curry_123_2(func_n3(tleval_144), first_spec, c->bindings),
                    args);

  if ((c->spec = rest(c->spec)) == nil) {
    sem_error(first_spec,
              lit("freeform must be followed by a query line"), nao);
  } else if (!c->data) {
    debuglf(specline, lit("freeform match failure: no data"), nao);
    return nil;
  } else {
    match_line_ctx mlc;
    val lim = freeform_prepare(vals, c, &mlc);
    c->data = nil;

    {
      cons_bind (new_bindings, success, do_match_line(&mlc));

      if (!success) {
        debuglf(specline, lit("freeform match failure"), nao);
        return nil;
      }

      if (integerp(success)) {
        c->data = lazy_str_get_trailing_list(mlc.dataline, success);
        c->data_lineno = plus(c->data_lineno, one);
      } else if (success == t && lim) {
        c->data = lazy_str_get_trailing_list(mlc.dataline, length_str(mlc.dataline));
        c->data_lineno = plus(c->data_lineno, one);
      }

      c->bindings = new_bindings;
    }
  }

  return next_spec_k;
}

val freeform_prepare(val vals, match_files_ctx *c, match_line_ctx *mlc)
{
  uses_or2;
  val first_spec = first(c->spec);
  val limit = or2(if2(integerp(first(vals)), first(vals)),
                  if2(integerp(second(vals)), second(vals)));
  val term = or2(if2(stringp(first(vals)), first(vals)),
                 if2(stringp(second(vals)), second(vals)));
  val dataline = lazy_str(c->data, term, limit);
  *mlc = ml_all(c->bindings, first_spec, dataline, zero,
                c->data, c->data_lineno, c->curfile);
  return limit;
}

static val maybe_next(match_files_ctx *c, val match_result)
{
  cons_bind (new_bindings, success, match_result);

  if (!success) {
    return nil;
  } else if (success == t) {
    c->data = nil;
  } else {
    cons_bind (new_data, new_line, success);
    c->data = new_data;
    c->data_lineno = new_line;
  }

  c->bindings = new_bindings;
  return next_spec_k;
}

static void v_take_accept(match_files_ctx *c, val specline, val result)
{
  val bindings = vecref(result, zero);
  val vpos = vecref(result, one);
  val hpos = vecref(result, two);
  c->bindings = bindings;

  if (hpos && car(vpos) == c->data) {
    debuglf(specline, lit("accept from horiz. context in same line: advancing"),
            nao);
    c->data = cdr(c->data);
  } else if (hpos) {
    debuglf(specline, lit("accept from horiz. context in diff line"), nao);
  } else {
    debuglf(specline, lit("accept from vertical context"), nao);
    if (vpos == t) {
      c->data = nil;
    } else {
      c->data = car(vpos);
      c->data_lineno = cdr(vpos);
    }
  }
}

static val v_block(match_files_ctx *c)
{
  spec_bind (specline, first_spec, c->spec);

  val args = rest(first_spec);
  val name = first(args);
  val spec = second(args);

  if (rest(specline))
    sem_error(specline, lit("unexpected material after block directive"), nao);

  {
    uw_block_begin(name, result);
    result = match_files(mf_spec(*c, spec));
    uw_block_end;

    if (vectorp(result))
    {
      v_take_accept(c, specline, result);
      return next_spec_k;
    }

    return maybe_next(c, result);
  }
}

static val v_accept_fail(match_files_ctx *c)
{
  spec_bind (specline, first_spec, c->spec);
  val sym = first(first_spec);
  val target = first(rest(first_spec));

  if (rest(specline))
    sem_error(specline, lit("unexpected material after ~a"), sym, nao);

  uw_block_return_proto(target,
                        if2(sym == accept_s,
                            vec(c->bindings,
                                if3(c->data, cons(c->data, c->data_lineno), t),
                                nil,
                                nao)),
                        sym);

  /* TODO: uw_block_return could just throw this */
  if (target)
    sem_error(specline, lit("~a: no block named ~a in scope"),
              sym, target, nao);
  else
    sem_error(specline, lit("~a: no anonymous block in scope"),
              sym, nao);
  return nil;
}

static val v_next_impl(match_files_ctx *c)
{
  spec_bind (specline, first_spec, c->spec);

  if ((c->spec = rest(c->spec)) == nil)
    return cons(c->bindings, cons(c->data, c->data_lineno));

  if (rest(first_spec)) {
    val args = rest(first_spec);
    val source = first(args);
    val meta = nil;

    if (source == args_k) {
      if (rest(args)) {
        sem_error(specline, lit("(next :args) takes no additional arguments"), nao);
      } else {
        cons_bind (new_bindings, success,
                   match_files(mf_args(*c)));

        if (success)
          return cons(new_bindings,
                      if3(c->data, cons(c->data, c->data_lineno), t));
        return nil;
      }
    } else if (source == env_k) {
      if (rest(args)) {
        sem_error(specline, lit("(next :env) takes no additional arguments"), nao);
      } else {
        cons_bind (new_bindings, success,
                   match_files(mf_file_data(*c, lit("env"), env(), one)));

        if (success)
          return cons(new_bindings,
                      if3(c->data, cons(c->data, c->data_lineno), t));
        return nil;
      }
    } else if (consp(source)) {
      val sym = car(source);
      if (sym == var_s || sym == expr_s)
        meta = t;
    } else if (!source) {
      cons_bind (new_bindings, success,
                 match_files(mf_all(c->spec, nil, c->bindings, nil, lit("empty"))));

      if (success)
        return cons(new_bindings,
                    if3(c->data, cons(c->data, c->data_lineno), t));
      return nil;
    }

    if (opt_compat && opt_compat <= 124)
      meta = t;

    if (keywordp(first(args))) {
      source = nil;
    } else {
      pop(&args);
    }

    if (args && !keywordp(first(args)))
      sem_error(specline, lit("next: keyword argument expected, not ~s"), first(args), nao);

    {
      int old_hacky_open = opt_compat && opt_compat <= 142;
      val alist = improper_plist_to_alist(args, list(nothrow_k, nao));
      val from_var = cdr(assoc(var_k, alist));
      val list_expr = cdr(assoc(list_k, alist));
      val tlist_expr = cdr(assoc(tlist_k, alist));
      val string_expr = cdr(assoc(string_k, alist));
      val nothrow = cdr(assoc(nothrow_k, alist));
      val str = if3(meta,
                    txeval(specline, source, c->bindings),
                    tleval(specline, source, c->bindings));

      if (!from_var && !source && !string_expr && !list_expr && !tlist_expr)
        sem_error(specline, lit("next: source required before keyword arguments"), nao);

      {
        int count = (from_var != nil) +
                    (list_expr != nil) +
                    (tlist_expr != nil) +
                    (string_expr != nil);

        if (count > 1)
        {
          sem_error(specline, lit("next: only one of :var, :list, :tlist or :string "
                                  "can be specified"), nao);
        }
      }

      if (from_var) {
        val existing = tx_lookup_var_ubc(from_var, c->bindings, first_spec);

        {
          cons_bind (new_bindings, success,
                     match_files(mf_file_data(*c, lit("var"),
                                 lazy_flatten(cdr(existing)), one)));

          if (success)
            return cons(new_bindings,
                        if3(c->data, cons(c->data, c->data_lineno), t));
          return nil;
        }
      } else if (list_expr) {
        val list_val = if3(opt_compat && opt_compat <= 143,
                           txeval, tleval)(specline, list_expr, c->bindings);
        cons_bind (new_bindings, success,
                   match_files(mf_file_data(*c, lit("var"),
                               lazy_flatten(list_val), one)));

        if (success)
          return cons(new_bindings,
                      if3(c->data, cons(c->data, c->data_lineno), t));
        return nil;
      } else if (tlist_expr) {
        val list_val = txeval(specline, tlist_expr, c->bindings);
        cons_bind (new_bindings, success,
                   match_files(mf_file_data(*c, lit("var"),
                               lazy_flatten(list_val), one)));

        if (success)
          return cons(new_bindings,
                      if3(c->data, cons(c->data, c->data_lineno), t));
        return nil;
      } else if (string_expr) {
        val str_val = tleval_144(specline, string_expr, c->bindings);
        if (!stringp(str_val))
          sem_error(specline, lit(":string arg ~s evaluated to non-string ~s"), string_expr, str_val, nao);

        {
          cons_bind (new_bindings, success,
                     match_files(mf_file_data(*c, lit("var"),
                                 split_str(str_val, lit("\n")), one)));

          if (success)
            return cons(new_bindings,
                        if3(c->data, cons(c->data, c->data_lineno), t));
          return nil;
        }
      } else if (old_hacky_open && nothrow) {
        if (str) {
          c->files = cons(cons(nothrow_k, str), c->files);
        } else {
          c->files = rest(c->files);
          if (!c->files) {
            debuglf(specline, lit("next: out of arguments"), nao);
            return nil;
          }
          c->files = cons(cons(nothrow_k, first(c->files)), rest(c->files));
        }
      } else if (old_hacky_open) {
        if (str) {
          c->files = cons(str, c->files);
        } else {
          c->files = rest(c->files);
          if (!c->files)
            sem_error(specline, lit("next: out of arguments"), nao);
          c->files = cons(cons(nothrow_k, first(c->files)), rest(c->files));
        }
      } else {
        val stream = complex_open(str, nil, nil, nothrow, nil);
        cons_bind (new_bindings, success,
                   match_files(mf_file_data(*c, str,
                                            lazy_stream_cons(stream), one)));

        if (success)
          return cons(new_bindings,
                      if3(c->data, cons(c->data, c->data_lineno), t));
        return nil;
      }
    }
  } else {
    c->files = rest(c->files);
    if (!c->files)
      sem_error(specline, lit("next: out of arguments"), nao);
  }

  /* We recursively process the file list, but the new
     data position we return to the caller must be in the
     original file we we were called with. Hence, we can't
     make a straight tail call here. */
  {
    cons_bind (new_bindings, success, match_files(mf_data(*c, t, nil)));

    if (success)
      return cons(new_bindings,
                  if3(c->data, cons(c->data, c->data_lineno), t));
    return nil;
  }
}

static val v_next(match_files_ctx *c)
{
  val result = nil;

  uw_simple_catch_begin;

  result = v_next_impl(c);

  uw_unwind {
    uw_frame_t *ex = uw_current_exit_point();
    if (ex && ex->uw.type == UW_BLOCK && ex->bl.protocol == accept_s) {
      set(vecref_l(ex->bl.result, one), cons(c->data, c->data_lineno));
      set(vecref_l(ex->bl.result, two), nil);
    }
  }

  uw_catch_end;

  return result;
}

static val v_parallel(match_files_ctx *c)
{
  spec_bind (specline, first_spec, c->spec);

  if (second(first_spec) == t) {
    return decline_k;
  } else {
    uses_or2;
    val sym = first(first_spec);
    val all_match = t;
    val some_match = nil;
    val max_line = zero;
    val max_data = nil;
    val specs = second(first_spec);
    val plist = third(first_spec);
    val choose_shortest = getplist(plist, shortest_k);
    val choose_longest = getplist(plist, longest_k);
    val choose_sym = or2(choose_longest, choose_shortest);
    val choose_bindings = c->bindings, choose_line = zero, choose_data = nil;
    val choose_minmax = choose_longest ? num(-1) : num(NUM_MAX);
    val resolve = getplist(plist, resolve_k);
    val resolve_ub_vars = nil;
    val resolve_bindings = nil;
    val iter;

    if (choose_longest && choose_shortest)
      sem_error(specline, lit("choose: both :shortest and :longest specified"), nao);

    if (sym == choose_s && !choose_sym)
      sem_error(specline, lit("choose: criterion not specified"), nao);

    if (resolve) {
      for (iter = resolve; iter; iter = cdr(iter)) {
        val var = car(iter);
        if (!tx_lookup_var(var, c->bindings))
          push(var, &resolve_ub_vars);
      }
    }

    for (iter = specs; iter != nil; iter = rest(iter))
    {
      val nested_spec = first(iter);
      cons_bind (new_bindings, success,
                 match_files(mf_spec(*c, nested_spec)));

      if (success) {
        some_match = t;

        if (resolve_ub_vars) {
          val uiter;
          for (uiter = resolve_ub_vars; uiter; uiter = cdr(uiter)) {
            val ubvar = car(uiter);
            val exists = tx_lookup_var(ubvar, new_bindings);

            if (exists)
              resolve_bindings = acons_new(ubvar, cdr(exists), resolve_bindings);
          }

          new_bindings = alist_remove(new_bindings, resolve_ub_vars);
        }

        if (sym == choose_s) {
          val binding = choose_sym ? tx_lookup_var(choose_sym, new_bindings) : nil;
          val value = cdr(binding);

          if (value) {
            val len = length_str(value);

            if ((choose_longest && gt(len, choose_minmax)) ||
                (choose_shortest && lt(len, choose_minmax)))
            {
              choose_minmax = len;
              choose_bindings = new_bindings;

              if (success == t) {
                choose_data = t;
              } else {
                cons_bind (new_data, new_line, success);
                choose_data = new_data;
                choose_line = new_line;
              }
            }
          }
        } else {
          /* choose does not propagate bindings between clauses! */
          c->bindings = new_bindings;
        }


        if (success == t) {
          max_data = t;
        } else if (consp(success) && max_data != t) {
          cons_bind (new_data, new_line, success);
          if (gt(new_line, max_line)) {
            max_line = new_line;
            max_data = new_data;
          }
        }
        if (sym == cases_s || sym == none_s)
          break;
      } else {
        all_match = nil;
        if (sym == all_s)
          break;
      }
    }

    if (sym == all_s && !all_match) {
      debuglf(specline, lit("all: some clauses didn't match"), nao);
      return nil;
    }

    if ((sym == some_s || sym == cases_s) && !some_match) {
      debuglf(specline, lit("some/cases: no clauses matched"), nao);
      return nil;
    }

    if (sym == none_s && some_match) {
      debuglf(specline, lit("none: some clauses matched"), nao);
      return nil;
    }

    /* No check for maybe, since it always succeeds. */

    if (resolve_bindings)
      c->bindings = nappend2(resolve_bindings, c->bindings);

    if (choose_sym) {
      if (consp(choose_data)) {
        c->data_lineno = choose_line;
        c->data = choose_data;
      } else if (choose_data == t) {
        c->data = nil;
      }
      c->bindings = choose_bindings;
    } else if (consp(max_data)) {
      c->data_lineno = max_line;
      c->data = max_data;
    } else if (max_data == t) {
      c->data = nil;
    }

    return next_spec_k;
  }
}

static val v_gather(match_files_ctx *c)
{
  spec_bind (specline, first_spec, c->spec);
  val specs = copy_list(second(first_spec));
  val args = third(first_spec);
  val until_last = fourth(first_spec);
  cons_bind (ul_sym, ul_args, until_last);
  cons_bind (ul_opts, ul_spec, ul_args);
  val ul_match = nil;
  val have_vars;
  val vars = vars_to_bindings(specline,
                              getplist_f(args, vars_k, mkcloc(have_vars)),
                              c->bindings);

  while (specs && c->data) {
    list_collect_decl (new_specs, ptail);
    val max_line = zero;
    val max_data = nil;
    val iter, next;
    val orig_bindings = c->bindings;

    for (iter = specs, next = cdr(iter); iter != nil; iter = next, next = cdr(iter)) {
      val nested_spec = first(iter);
      cons_bind (new_bindings, success,
                 match_files(mf_spec(*c, nested_spec)));

      if (!success) {
        deref(cdr_l(iter)) = nil;
        ptail = list_collect_nconc(ptail, iter);
      } else if (success == t) {
        c->bindings = new_bindings;
        max_data = t;
      } else if (consp(success) && max_data != t) {
        cons_bind (new_data, new_line, success);
        c->bindings = new_bindings;
        if (gt(new_line, max_line)) {
          max_line = new_line;
          max_data = new_data;
        }
      }
    }

    if (until_last)
    {
      cons_bind (until_last_bindings, success,
                 match_files(mf_spec(*c, ul_spec)));

      if (success) {
        debuglf(specline, lit("until/last matched ~a:~d"),
                c->curfile, c->data_lineno, nao);
        /* Until discards bindings and position, last keeps them. */
        if (ul_sym == last_s) {
          val last_bindings = set_diff(until_last_bindings, c->bindings, eq_f, nil);
          c->bindings = nappend2(last_bindings, orig_bindings);

          if (success == t) {
            c->data = t;
          } else {
            cons_bind (new_data, new_line, success);
            c->data = new_data;
            c->data_lineno = new_line;
          }
        }
        ul_match = t;
        break;
      }
    }

    specs = new_specs;

    if (consp(max_data)) {
      debuglf(specline, lit("gather advancing from line ~d to ~d"),
              c->data_lineno, max_line, nao);
      c->data_lineno = max_line;
      c->data = max_data;
    } else if (max_data == t) {
      debuglf(specline, lit("gather consumed entire file"), nao);
      c->data = nil;
    } else {
      c->data_lineno = plus(c->data_lineno, one);
      c->data = rest(c->data);
      debuglf(specline, lit("gather advancing by one line to ~d"), c->data_lineno, nao);
    }
  }

  if (!ul_match && ul_opts && memq(mandatory_k, ul_opts)) {
    debuglf(specline, lit("gather didn't match mandatory until/last"), nao);
    return nil;
  }

  if (have_vars) {
    val iter;

    for (iter = vars; iter != nil; iter = cdr(iter)) {
      cons_bind (var, dfl_val, car(iter));
      if (!tx_lookup_var(var, c->bindings)) {
        if (dfl_val == noval_s) {
          debuglf(specline, lit("gather failed to match some required vars"), nao);
          return nil;
        } else {
          c->bindings = acons(var, dfl_val, c->bindings);
        }
      }
    }

    debuglf(specline, lit("gather matched all required vars"), nao);
    return next_spec_k;
  }

  if (specs) {
    debuglf(specline, lit("gather failed to match some specs:"), nao);
    debuglf(specline, lit("~s"), specs, nao);
    return nil;
  }

  return next_spec_k;
}


static val match_expand_vars(val vars)
{
  list_collect_decl (out, ptail);

  for (; vars; vars = cdr(vars)) {
    val var = car(vars);
    if (consp(var)) {
      val sym = car(var);
      val dfl = cadr(var);
      val dfl_ex = expand(dfl, nil);

      ptail = list_collect(ptail, if3(dfl == dfl_ex,
                                      var, list(sym, dfl_ex, nao)));
    } else {
      ptail = list_collect(ptail, var);
    }
  }

  return out;
}

val match_expand_keyword_args(val args)
{
  list_collect_decl (out, ptail);

  if (opt_compat && opt_compat <= 165)
    return args;

  while (consp(args)) {
    val sym = car(args);
    val next = cdr(args);
    val more = consp(next);

    if (more &&
        (sym == maxgap_k || sym == mingap_k || sym == gap_k ||
         sym == times_k || sym == mintimes_k || sym == maxtimes_k ||
         sym == lines_k || sym == vars_k ||
         sym == list_k || sym == string_k))
    {
      val form = car(next);
      val form_ex = if3(sym == vars_k,
                        match_expand_vars(form),
                        expand(form, nil));
      ptail = list_collect(ptail, sym);
      ptail = list_collect(ptail, form_ex);
      args = cdr(next);
    } else if (more &&
               (sym == counter_k)) {
      val form = car(next);
      val vars_like_wrap = cons(form, nil);
      val vars_like_ex = match_expand_vars(vars_like_wrap);

      ptail = list_collect(ptail, sym);
      ptail = list_collect(ptail, car(vars_like_ex));
      args = cdr(next);
      if (consp(form))
        match_reg_var(car(form));
      else
        match_reg_var(form);
    } else if (more &&
               (sym == tlist_k)) {
      ptail = list_collect(ptail, sym);
      ptail = list_collect(ptail, expand_meta(car(next), nil));
      args = cdr(next);
    } else if (more &&
               (sym == var_k)) {
      ptail = list_collect(ptail, sym);
      ptail = list_collect(ptail, car(next));
      args = cdr(next);
    } else {
      ptail = list_collect(ptail, expand(sym, nil));
      args = next;
    }
  }

  return out;
}

val match_expand_elem(val elem)
{
  if (atom(elem)) {
    return elem;
  } else {
    val sym = car(elem);
    val args = cdr(elem);

    if (opt_compat && opt_compat < 166) {
      goto out;
    } else if (sym == skip_s || sym == fuzz_s || sym == load_s ||
               sym == close_s)
    {
      val args_ex = expand_forms(args, nil);
      if (args == args_ex)
        return elem;
      return rlcp(cons(sym, args_ex), elem);
    } else if (sym == call_s) {
      if (atom(args)) {
        return elem;
      } else {
        val arg1 = car(args);
        val arg1_ex = expand(arg1, nil);
        if (arg1 == arg1_ex)
          return elem;
        return rlcp(cons(sym, cons(arg1_ex, cdr(args))), elem);
      }
    } else if (sym == cat_s) {
      if (atom(args) || atom(cdr(args))) {
        return elem;
      } else {
        val arg1 = car(args);
        val arg2 = cadr(args);
        val arg2_ex = expand(arg2, nil);
        if (arg2 == arg2_ex)
          return elem;
        return rlcp(cons(sym, cons(arg1, cons(arg2_ex, cddr(args)))), elem);
      }
    } else if (sym == next_s) {
      val args_ex = match_expand_keyword_args(args);
      if (args == args_ex)
        return elem;
      return rlcp(cons(sym, args_ex), elem);
    } else {
out:
      return rlcp(cons(sym, expand_meta(args, nil)), elem);
    }
  }
}

static val v_collect(match_files_ctx *c)
{
  spec_bind (specline, first_spec, c->spec);
  val op_sym = first(first_spec);
  val coll_spec = second(first_spec);
  val until_last_spec = third(first_spec);
  cons_bind (ul_sym, ul_args, until_last_spec);
  cons_bind (ul_opts, ul_spec, ul_args);
  val ul_match = nil, accept_jump = t;
  val args = fourth(first_spec);
  volatile val bindings_coll = nil;
  volatile val last_bindings = nil;
  val max = tleval_144(specline, getplist(args, maxgap_k), c->bindings);
  val min = tleval_144(specline, getplist(args, mingap_k), c->bindings);
  val gap = tleval_144(specline, getplist(args, gap_k), c->bindings);
  val times = tleval_144(specline, getplist(args, times_k), c->bindings);
  val mintimes = tleval_144(specline, getplist(args, mintimes_k), c->bindings);
  val maxtimes = tleval_144(specline, getplist(args, maxtimes_k), c->bindings);
  val lines = tleval_144(specline, getplist(args, lines_k), c->bindings);
  val counter_spec = getplist(args, counter_k);
  val consp_counter = consp(counter_spec);
  val counter = if3(consp_counter, first(counter_spec), counter_spec);
  val counter_base = if3(consp_counter,
                         tleval(specline,
                                second(counter_spec),
                                c->bindings), zero);
  val counter_binding = if2(counter, cons(counter, nil));
  val bindings_with_counter = if2(counter, cons(counter_binding, nil));
  val have_vars;
  volatile val vars = getplist_f(args, vars_k, mkcloc(have_vars));
  cnum cmax = if3(gap, c_num(gap), if3(max, c_num(max), 0));
  cnum cmin = if3(gap, c_num(gap), if3(min, c_num(min), 0));
  cnum mincounter = cmin, maxcounter = 0;
  cnum ctimax = if3(times, c_num(times), if3(maxtimes, c_num(maxtimes), 0));
  cnum ctimin = if3(times, c_num(times), if3(mintimes, c_num(mintimes), 0));
  volatile cnum timescounter = 0, linescounter = 0;
  cnum ctimes = if3(times, c_num(times), 0);
  cnum clines = if3(lines, c_num(lines), 0);
  val iter;
  uw_mark_frame;
  uw_block_begin(nil, result);

  if (gap && (max || min))
    sem_error(specline, lit("~s: cannot mix :gap with :mingap or :maxgap"),
              op_sym, nao);

  if (op_sym == repeat_s) {
    if (have_vars)
      sem_error(specline, lit("~s: collect takes :vars, repeat does not"),
                op_sym, nao);
    have_vars = t;
  }

  if (counter && !bindable(counter))
    sem_error(specline, lit("~s: ~s specified as :counter isn't a bindable symbol"),
              op_sym, counter, nao);

  vars = vars_to_bindings(specline, vars, c->bindings);

  if ((times && ctimes == 0) || (lines && clines == 0))
    uw_fast_return(next_spec_k);

  result = t;

  while (c->data) {
    val new_bindings = nil, success = nil;

    if ((gap || min) && mincounter < cmin)
      goto next_collect;

    if (lines && linescounter++ >= clines)
      break;

    {
      if (counter) {
        rplacd(counter_binding, plus(num(timescounter), counter_base));
        rplacd(bindings_with_counter, c->bindings);
        cons_set (new_bindings, success,
                  match_files(mf_spec_bindings(*c, coll_spec, bindings_with_counter)));

        if (!new_bindings) {
          rplacd(counter_binding, nil);
          new_bindings = bindings_with_counter;
        }
      } else {
        cons_set (new_bindings, success,
                  match_files(mf_spec(*c, coll_spec)));
      }

      /* Until/last clause sees un-collated bindings from collect. */
      if (until_last_spec)
      {
        uses_or2;
        cons_bind (until_last_bindings, success,
                   match_files(mf_spec_bindings(*c, ul_spec,
                                                or2(new_bindings, c->bindings))));

        if (success) {
          debuglf(specline, lit("until/last matched ~a:~d"),
                  c->curfile, c->data_lineno, nao);
          /* Until discards bindings and position, last keeps them. */
          if (ul_sym == last_s) {
            last_bindings = set_diff(until_last_bindings,
                                     new_bindings, eq_f, nil);
            if (success == t) {
              debuglf(specline, lit("~s: consumed entire file"), op_sym, nao);
              c->data = nil;
            } else {
              cons_bind (new_data, new_line, success);
              c->data = new_data;
              c->data_lineno = new_line;
            }
          }
          ul_match = t;
          break;
        }
      }

      if (success) {
        list_collect_decl (missing, ptail);
        val strictly_new_bindings = set_diff(new_bindings,
                                             c->bindings, eq_f, nil);
        val have_new = strictly_new_bindings;

        debuglf(specline, lit("~s matched ~a:~d"),
                op_sym, c->curfile, c->data_lineno, nao);

        for (iter = vars; iter; iter = cdr(iter)) {
          cons_bind (var, dfl, car(iter));
          val exists = tx_lookup_var(var, new_bindings);

          if (!exists) {
            if (dfl == noval_s)
              ptail = list_collect(ptail, var);
            else
              strictly_new_bindings = acons(var, dfl, strictly_new_bindings);
          }
        }

        if (have_new && missing)
          sem_error(specline, lit("~s failed to bind ~a"),
                    op_sym, missing, nao);

        for (iter = strictly_new_bindings; iter; iter = cdr(iter))
        {
          val binding = car(iter);
          val vars_binding = assoc(car(binding), vars);

          if (!have_vars || vars_binding) {
            val existing = assoc(car(binding), bindings_coll);

            bindings_coll = acons_new(car(binding), cons(cdr(binding), cdr(existing)), bindings_coll);
          }
        }
      }

      if (success) {
        if (consp(success)) {
          cons_bind (new_data, new_line, success);

          bug_unless (ge(new_line, c->data_lineno));

          if (new_line == c->data_lineno) {
            new_data = cdr(new_data);
            new_line = plus(new_line, one);
          }

          debuglf(specline, lit("~s advancing from line ~d to ~d"),
                  op_sym, c->data_lineno, new_line, nao);

          c->data = new_data;
          c->data_lineno = new_line;
          deref(car_l(success)) = nil;
        } else {
          debuglf(specline, lit("~s consumed entire file"), op_sym, nao);
          c->data = nil;
        }
        mincounter = 0;
        maxcounter = 0;

        timescounter++;

        if ((times || maxtimes) && timescounter >= ctimax)
          break;
      } else {
next_collect:
        mincounter++;
        if ((gap || max) && ++maxcounter > cmax)
          break;
        c->data_lineno = plus(c->data_lineno, one);
        c->data = rest(c->data);
      }
    }
  }

  accept_jump = nil;

  uw_block_end;

  if (!result) {
    debuglf(specline, lit("~s explicitly failed"), op_sym, nao);
    return nil;
  }

  if ((times || mintimes) && timescounter < ctimin) {
    debuglf(specline, lit("fewer than ~d iterations collected"),
            num(ctimin), nao);
    return nil;
  }

  if (!ul_match && ul_opts && memq(mandatory_k, ul_opts) && !accept_jump) {
    debuglf(specline, lit("~s didn't match mandatory until/last"),
            op_sym, nao);
    return nil;
  }

  if (!bindings_coll)
    debuglf(specline, lit("nothing was collected"), nao);

  c->bindings = set_diff(c->bindings, bindings_coll, eq_f, car_f);

  for (iter = bindings_coll; iter; iter = cdr(iter)) {
    val pair = car(iter);
    val rev = cons(car(pair), nreverse(cdr(pair)));
    c->bindings = cons(rev, c->bindings);
  }

  if (last_bindings) {
    c->bindings = set_diff(c->bindings, last_bindings, eq_f, car_f);
    c->bindings = nappend2(last_bindings, c->bindings);
  }

  /* If nothing was collected, but vars were specified,
     then bind empty lists for the vars. */
  if (!bindings_coll && vars) {
    for (iter = vars; iter; iter = cdr(iter)) {
      val sym = car(car(iter));
      val exists = tx_lookup_var(sym, c->bindings);
      if (!exists)
        c->bindings = acons(sym, nil, c->bindings);
    }
  }

  return next_spec_k;
}

static val v_flatten(match_files_ctx *c)
{
  spec_bind (specline, first_spec, c->spec);
  val iter;

  for (iter = rest(first_spec); iter; iter = rest(iter)) {
    val sym = first(iter);

    if (!bindable(sym)) {
      sem_error(specline,
                lit("flatten: ~s is not a bindable symbol"), sym, nao);
    } else {
      val existing = tx_lookup_var_ubc(sym, c->bindings, first_spec);
      set(cdr_l(existing), flatten(cdr(existing)));
    }
  }

  return next_spec_k;
}

static val v_forget_local(match_files_ctx *c)
{
  val specline = first(c->spec);
  val first_spec = first(specline);
  c->bindings = alist_remove(c->bindings, rest(first_spec));
  return next_spec_k;
}

static val v_merge(match_files_ctx *c)
{
  spec_bind (specline, first_spec, c->spec);
  val target = first(rest(first_spec));
  val args = rest(rest(first_spec));
  val merged = nil;

  if (!bindable(target))
    sem_error(specline, lit("merge: ~s is not a bindable symbol"),
              target, nao);

  for (; args; args = rest(args)) {
    val arg = first(args);

    if (arg) {
      val arg_eval = txeval(specline, arg, c->bindings);

      if (merged)
        merged = weird_merge(merged, arg_eval);
      else
        merged = arg_eval;
    }
  }

  c->bindings = acons_new(target, merged, c->bindings);

  return next_spec_k;
}

static val v_bind(match_files_ctx *c)
{
  spec_bind (specline, first_spec, c->spec);
  val args = rest(first_spec);
  val pattern = first(args);
  val form = second(args);
  val keywords = rest(rest(args));
  val value = txeval(specline, form, c->bindings);
  val testfun = equal_f;
  val filter_spec = getplist(keywords, filter_k);
  val lfilt_spec = getplist(keywords, lfilt_k);
  val rfilt_spec = getplist(keywords, rfilt_k);

  if (filter_spec && (rfilt_spec || lfilt_spec))
    sem_error(specline, lit("bind: cannot use :filter with :lfilt or :rfilt"), nao);

  if (filter_spec) {
    val filter = get_filter(filter_spec);

    if (!filter) {
      sem_error(specline, lit("bind: ~s specifies unknown filter"),
                filter_spec, nao);
    }

    testfun = curry_1234_34(func_n4(filter_equal), filter, filter);
  } else if (rfilt_spec || lfilt_spec) {
    val rfilt = if3(rfilt_spec, get_filter(rfilt_spec), identity_f);
    val lfilt = if3(lfilt_spec, get_filter(lfilt_spec), identity_f);

    if (!rfilt) {
      sem_error(specline, lit("bind: ~s specifies unknown filter"),
                rfilt_spec, nao);
    }

    if (!lfilt) {
      sem_error(specline, lit("bind: ~s specifies unknown filter"),
                lfilt_spec, nao);
    }

    testfun = curry_1234_34(func_n4(filter_equal), lfilt, rfilt);
  }

  uw_env_begin;
  uw_set_match_context(cons(c->spec, c->bindings));

  c->bindings = dest_bind(specline, c->bindings, pattern, value, testfun);

  uw_env_end;

  if (c->bindings == t)
    return nil;

  return next_spec_k;
}

static val hv_trampoline(match_line_ctx *c)
{
  val ret;
  match_files_ctx mf = mf_from_ml(*c);
  val sym = first(first(c->specline));
  val entry = gethash(v_directive_table, sym);

  if (!entry)
    internal_error("hv_trampoline: missing dispatch table entry");

  {
    v_match_func vmf = coerce(v_match_func, cptr_get(entry));
    ret = vmf(&mf);
    if (ret == next_spec_k)
      c->bindings = mf.bindings;
    return ret;
  }
}

static val v_set(match_files_ctx *c)
{
  spec_bind (specline, first_spec, c->spec);
  val args = rest(first_spec);
  val pattern = first(args);
  val form = second(args);
  val val = txeval(specline, form, c->bindings);

  dest_set(first_spec, c->bindings, pattern, val);

  return next_spec_k;
}

static val v_rebind(match_files_ctx *c)
{
  spec_bind (specline, first_spec, c->spec);
  val args = rest(first_spec);
  val pattern = first(args);
  val form = second(args);
  val val = txeval(specline, form, c->bindings);

  c->bindings = alist_remove(c->bindings, args);
  c->bindings = dest_bind(specline, c->bindings,
                          pattern, val, equal_f);

  return next_spec_k;
}

static val v_cat(match_files_ctx *c)
{
  spec_bind (specline, first_spec, c->spec);
  val sym = second(first_spec);
  val sep_form = third(first_spec);

  if (!bindable(sym)) {
    sem_error(specline, lit("cat: ~s is not a bindable symbol"), sym, nao);
  } else {
    val existing = tx_lookup_var_ubc(sym, c->bindings, first_spec);
    val sep = if3(sep_form, tleval_144(specline, sep_form, c->bindings),
                  lit(" "));
    set(cdr_l(existing), cat_str(flatten(cdr(existing)), sep));
  }

  return next_spec_k;
}

static val v_output(match_files_ctx *c)
{
  spec_bind (specline, first_spec, c->spec);
  val specs = second(first_spec);
  val dest_spec = third(first_spec);
  val nothrow = nil;
  val append = nil;
  val dest = std_output;
  val filter = nil;
  val named_var = nil, continue_expr = nil, finish_expr = nil;
  val alist;
  val stream = nil;

  if (first(dest_spec) == nothrow_k) {
    if (rest(dest_spec))
      sem_error(specline, lit("material after :nothrow in output"), nao);
  } else if (!keywordp(first(dest_spec))) {
    uses_or2;
    val form = first(dest_spec);
    val sym = if2(consp(form), car(form));
    int tx = ((opt_compat && opt_compat <= 142) ||
              (sym == var_s) || (sym == expr_s));
    val val = if3(tx,
                  txeval(specline, form, c->bindings),
                  tleval(specline, form, c->bindings));
    dest = or2(val, dest);
    pop(&dest_spec);
  }

  alist = improper_plist_to_alist(dest_spec, list(nothrow_k, append_k, nao));

  nothrow = cdr(assoc(nothrow_k, alist));
  append = cdr(assoc(append_k, alist));
  named_var = cdr(assoc(named_k, alist));
  continue_expr = cdr(assoc(continue_k, alist));
  finish_expr = cdr(assoc(finish_k, alist));

  if (named_var && continue_expr)
    sem_error(specline, lit(":continue and :named are mutually exclusive"), nao);

  if (named_var && finish_expr)
    sem_error(specline, lit(":named and :finish are mutually exclusive"), nao);

  if (continue_expr && finish_expr)
    sem_error(specline, lit(":continue and :finish are mutually exclusive"), nao);

  {
    val filter_sym = cdr(assoc(filter_k, alist));

    if (filter_sym) {
      filter = get_filter(filter_sym);

      if (!filter)
        sem_error(specline, lit("~s specifies unknown filter"), filter_sym, nao);
    }
  }

  {
    val into_var = cdr(assoc(into_k, alist));

    if (into_var) {
      val stream = make_strlist_output_stream();

      if (!symbolp(into_var))
        sem_error(specline, lit(":into requires a variable, not ~s"), into_var, nao);

      if (named_var)
        sem_error(specline, lit(":into incompatible with :named"), nao);

      if (continue_expr)
        sem_error(specline, lit(":into incompatible with :continue"), nao);

      debuglf(specline, lit("opening string list stream"), nao);
      do_output(c->bindings, specs, filter, stream);
      flush_stream(stream);

      {
        val existing = tx_lookup_var(into_var, c->bindings);
        val list_out = get_list_from_stream(stream);

        if (existing) {
          if (append) {
            set(cdr_l(existing), append2(flatten(cdr(existing)), list_out));
          } else {
            set(cdr_l(existing), list_out);
          }
        } else {
          c->bindings = acons(into_var, list_out, c->bindings);
        }
      }
      return next_spec_k;
    }
  }

  if (continue_expr || finish_expr) {
    uses_or2;
    val which = or2(continue_expr, finish_expr);
    val stream = tleval_144(specline, which, c->bindings);


    if (!streamp(stream))
      sem_error(specline, lit("~s evaluated to ~s which is not a stream"), which, stream, nao);

    do_output(c->bindings, specs, filter, stream);
    flush_stream(stream);

    if (finish_expr)
      close_stream(stream, t);

    return next_spec_k;
  }

  stream = complex_open(dest, t, append, nothrow, nil);

  debuglf(specline, lit("opening data sink ~a"), dest, nao);

  if (!stream) {
    debuglf(specline, lit("could not open ~a: "
                          "treating as failed match due to nothrow"), dest, nao);
    return nil;
  } else {
    do_output(c->bindings, specs, filter, stream);
    flush_stream(stream);

    if (named_var)
      c->bindings = acons(named_var, stream, c->bindings);
    else if (!streamp(dest))
      close_stream(stream, t);
  }

  return next_spec_k;
}

static val v_try(match_files_ctx *c)
{
  spec_bind (specline, first_spec, c->spec);
  val catch_syms = second(first_spec);
  val try_clause = third(first_spec);
  val catch_fin = fourth(first_spec);
  val finally_clause = nil;

  {
    uw_block_begin(nil, result);
    uw_catch_begin(catch_syms, exsym, exvals);

    result = match_files(mf_spec(*c, try_clause));

    uw_catch(exsym, exvals) {
      {
        val iter;

        for (iter = catch_fin; iter; iter = cdr(iter)) {
          val clause = car(iter);
          val type = first(second(clause));
          val params = second(second(clause));
          val body = third(clause);

          if (first(clause) == catch_s) {
            if (uw_exception_subtype_p(exsym, type)) {
              val all_bind = t;
              val piter, viter;

              for (piter = params, viter = exvals;
                   piter && viter;
                   piter = cdr(piter), viter = cdr(viter))
              {
                val param = car(piter);
                val value = car(viter);

                if (value != noval_s) {
                  c->bindings = dest_bind(specline, c->bindings,
                                         param, value, equal_f);

                  if (c->bindings == t) {
                    all_bind = nil;
                    break;
                  }
                }
              }

              if (all_bind) {
                cons_bind (new_bindings, success,
                           match_files(mf_spec(*c, body)));
                if (success) {
                  c->bindings = new_bindings;
                  result = t; /* catch succeeded, so try succeeds */
                  if (consp(success)) {
                    c->data = car(success);
                    c->data_lineno = cdr(success);
                  } else {
                    c->data = nil;
                  }
                }
              }
              break;
            }
          } else if (car(clause) == finally_s) {
            finally_clause = body;
          }
        }
      }
    }

    uw_unwind {
      val iter;
      uw_frame_t *ex = uw_current_exit_point();
      int acc_intercept = (ex && ex->uw.type == UW_BLOCK &&
                           ex->bl.protocol == accept_s);

      /* result may be t, from catch above. */
      if (consp(result)) {
        /* We process it before finally, as part of the unwinding, so
           finally can accumulate more bindings over top of any bindings
           produced by the main clause. */
        cons_bind (new_bindings, success, result);
        if (consp(success)) {
          c->data = car(success);
          c->data_lineno = cdr(success);
        } else {
          c->data = nil;
        }
        c->bindings = new_bindings;
      }

      if (!finally_clause) {
        for (iter = catch_fin; iter; iter = cdr(iter)) {
          val clause = car(iter);
          if (first(clause) == finally_s) {
            finally_clause = third(clause);
            break;
          }
        }
      }

      if (finally_clause && acc_intercept)
        v_take_accept(c, specline, ex->bl.result);

      if (finally_clause) {
        cons_bind (new_bindings, success,
                   match_files(mf_spec(*c, finally_clause)));

        if (success) {
          c->bindings = new_bindings;
          result = t; /* finally succeeds, so try block succeeds */
          if (consp(success)) {
            c->data = car(success);
            c->data_lineno = cdr(success);
          } else {
            c->data = nil;
          }

          if (acc_intercept && finally_clause) {
            ex->bl.result = if2(success,
                                vec(c->bindings,
                                    if3(c->data, cons(c->data, c->data_lineno), t),
                                    nil,
                                    nao));
            ex->bl.protocol = if3(success, accept_s, fail_s);
          }
        }
      }
    }

    uw_catch_end;
    uw_block_end;

    if (!result)
      return nil;

    return next_spec_k;
  }
}

static val h_define(match_line_ctx *c)
{
  val elem = first(c->specline);
  val body = third(elem);
  val args = fourth(elem);
  val name = first(args);
  val params = second(args);
  val existing = uw_get_func(name);
  uw_set_func(name, cons(car(existing), cons(params, body)));
  return next_spec_k;
}

static val v_define(match_files_ctx *c)
{
  spec_bind (specline, first_spec, c->spec);

  if (rest(specline))
    sem_error(specline, lit("unexpected material after define"), nao);

  if (second(first_spec) == t) {
    val elem = first(specline);
    val body = third(elem);
    val args = fourth(elem);
    val name = first(args);
    val params = second(args);
    val existing = uw_get_func(name);
    uw_set_func(name, cons(car(existing), cons(params, body)));
    return next_spec_k;
  } else {
    val args = second(first_spec);
    val body = third(first_spec);
    val name = first(args);
    val params = second(args);
    val existing = uw_get_func(name);

    uw_set_func(name, cons(cons(params, body), cdr(existing)));
    return next_spec_k;
  }
}

static val v_defex(match_files_ctx *c)
{
  spec_bind (specline, first_spec, c->spec);
  val types = rest(first_spec);
  if (!all_satisfy(types, func_n1(symbolp), nil))
    sem_error(specline, lit("defex arguments must all be symbols"),
              nao);
  (void) reduce_left(func_n2(uw_register_subtype), types, nil, nil);

  return next_spec_k;
}

static val v_throw(match_files_ctx *c)
{
  spec_bind (specline, first_spec, c->spec);
  val type = second(first_spec);
  val args = rest(rest(first_spec));
  if (!symbolp(type))
    sem_error(specline, lit("throw: ~a is not a type symbol"),
              type, nao);
  {
    val values = mapcar(curry_123_2(func_n3(txeval_allow_ub),
                                    specline, c->bindings), args);
    uw_throw(type, values);
  }
}

static val v_deffilter(match_files_ctx *c)
{
  spec_bind (specline, first_spec, c->spec);
  val sym = second(first_spec);
  val table = rest(rest(first_spec));

  if (!symbolp(sym))
    sem_error(specline, lit("deffilter: ~a is not a symbol"),
              first(first_spec), nao);

  {
    val table_evaled = txeval(specline, table, c->bindings);

    if (!all_satisfy(table_evaled, andf(func_n1(listp),
                                        chain(func_n1(length_list),
                                              curry_12_1(func_n2(ge), two), nao),
                                        chain(func_n1(rest),
                                              curry_123_1(func_n3(all_satisfy),
                                                          func_n1(stringp),
                                                          nil),
                                              nao),
                                        nao),
                                   nil))
    sem_error(specline,
              lit("deffilter arguments must be lists of at least two strings"),
              nao);

    register_filter(sym, table_evaled);
  }


  /* TODO: warn about replaced filter. */
  return next_spec_k;
}

static val v_filter(match_files_ctx *c)
{
  spec_bind (specline, first_spec, c->spec);
  val filter_spec = second(first_spec);
  val vars = rest(rest(first_spec));
  val filter = get_filter(filter_spec);

  if (!filter)
    sem_error(specline, lit("~s specifies unknown filter"), filter_spec, nao);

  uw_env_begin;
  uw_set_match_context(cons(c->spec, c->bindings));

  for (; vars; vars = cdr(vars)) {
    val var = car(vars);
    val existing = tx_lookup_var_ubc(var, c->bindings, first_spec);
    set(cdr_l(existing), filter_string_tree(filter, cdr(existing)));
  }

  uw_env_end;
  return next_spec_k;
}

static val v_eof(match_files_ctx *c)
{
  if (c->data && car(c->data)) {
    debuglf(c->spec, lit("eof failed to match at ~d"), c->data_lineno, nao);
    return nil;
  }
  return next_spec_k;
}

static val v_fun(match_files_ctx *c)
{
  spec_bind (specline, first_spec, c->spec);
  val sym = first(first_spec);
  val func = car(uw_get_func(sym));

  if (func && !rest(specline)) {
    val args = rest(first_spec);
    val params = car(func);
    val ub_p_a_pairs = nil;
    val body = cdr(func);
    val piter, aiter;
    val bindings_cp = copy_list(c->bindings);

    if (!equal(length_list(args), length_list(params)))
      sem_error(specline, lit("function ~a takes ~a argument(s)"),
                sym, length_list(params), nao);

    for (piter = params, aiter = args; piter;
         piter = cdr(piter), aiter = cdr(aiter))
    {
      val param = car(piter);
      val arg = car(aiter);

      if (bindable(arg)) {
        val val = tx_lookup_var(arg, c->bindings);
        if (val) {
          bindings_cp = cons(cons(param, cdr(val)), bindings_cp);
        } else {
          bindings_cp = alist_nremove1(bindings_cp, param);
          ub_p_a_pairs = cons(cons(param, arg), ub_p_a_pairs);
        }
      } else {
        val val = txeval(specline, arg, c->bindings);
        bindings_cp = cons(cons(param, val), bindings_cp);
      }
    }

    {
      args_decl_list(args, ARGS_MIN, bindings_cp);
      uw_block_begin(nil, result);
      uw_env_begin;
      debug_frame(sym, args, ub_p_a_pairs, c->bindings, if2(consp(c->data), car(c->data)),
                  c->data_lineno, nil);

      uw_simple_catch_begin;

      result = match_files(mf_spec_bindings(*c, body, bindings_cp));

      uw_unwind {
        fun_intercept_accept(c->bindings, ub_p_a_pairs, sym, specline);
      }

      uw_catch_end;

      debug_end;
      uw_env_end;
      uw_block_end;

      if (!result) {
        debuglf(specline, lit("function (~s ~s) failed"), sym, args, nao);
        return nil;
      }

      {
        cons_bind (new_bindings, success, result);

        c->bindings = fun_resolve_bindings(c->bindings, ub_p_a_pairs,
                                           new_bindings, sym, specline);

        if (c->bindings == t)
          return nil;

        if (consp(success)) {
          debuglf(specline,
                  lit("function matched; "
                      "advancing from line ~d to ~d"),
                  c->data_lineno, cdr(success), nao);
          c->data = car(success);
          c->data_lineno = cdr(success);
        } else {
          debuglf(specline, lit("function consumed entire file"),
                  nao);
          c->data = nil;
        }
      }
    }

    return next_spec_k;
  }

  return decline_k;
}

static val v_do(match_files_ctx *c)
{
  spec_bind (specline, first_spec, c->spec);
  val args = rest(first_spec);
  (void) tleval_progn(c->spec, args, c->bindings);
  return next_spec_k;
}

static val v_require(match_files_ctx *c)
{
  spec_bind (specline, first_spec, c->spec);
  val args = rest(first_spec);
  val ret;
  ret = tleval_progn(c->spec, args, c->bindings);
  if (!ret) {
    debuglf(specline, lit("require failed"), nao);
    return ret;
  }
  return next_spec_k;
}

static val v_if(match_files_ctx *c)
{
  spec_bind (specline, first_spec, c->spec);
  val args = rest(first_spec);

  for (; args; args = cdr(args)) {
    cons_bind (expr, spec, car(args));
    if (tleval(c->spec, expr, c->bindings))
      return maybe_next(c, match_files(mf_spec(*c, spec)));
  }

  return next_spec_k;
}

static val v_assert(match_files_ctx *c)
{
  spec_bind (specline, first_spec, c->spec);

  if (rest(specline))
    return decline_k;

  c->spec = rest(c->spec);

  if (!c->spec)
    return cons(c->bindings, cons(c->data, c->data_lineno));

  {
    val args = rest(first_spec);
    val type = pop(&args);
    val result = match_files(*c);

    if (result) {
      return result;
    } else if (type) {
      val values = mapcar(curry_123_2(func_n3(txeval_allow_ub),
                                      specline, c->bindings), args);
      uw_throw(type, values);
    } else {
      if (c->curfile)
        typed_error(assert_s, first_spec, lit("assertion (at ~a:~d)"), c->curfile, c->data_lineno, nao);
      typed_error(assert_s, first_spec, lit("assertion (line ~d)"), c->data_lineno, nao);
    }
  }
  abort();
}

static val v_load(match_files_ctx *c)
{
  uses_or2;
  spec_bind (specline, first_spec, c->spec);
  val sym = first(first_spec);
  val args = rest(first_spec);
  val parent = or2(load_path, null_string);
  val target = tleval(specline, first(args), c->bindings);

  if (rest(specline))
    sem_error(specline, lit("unexpected material after ~s"), sym, nao);

  if (!stringp(target))
    sem_error(specline, lit("~s: path ~s is not a string"), sym, target, nao);

  if (equal(target, null_string))
    sem_error(specline, lit("~s: null string path given"), sym, nao);

  {
    val path = if3(!pure_rel_path_p(target),
                   target,
                   cat_str(nappend2(sub_list(split_str(parent, lit("/")),
                                         zero, negone),
                                    cons(target, nil)), lit("/")));
    val stream, name;
    val txr_lisp_p = nil;
    val ret = nil;
    val saved_dyn_env = dyn_env;
    val rec = cdr(lookup_var(saved_dyn_env, load_recursive_s));

    open_txr_file(path, &txr_lisp_p, &name, &stream);

    uw_simple_catch_begin;

    dyn_env = make_env(nil, nil, dyn_env);
    env_vbind(dyn_env, load_path_s, name);
    env_vbind(dyn_env, load_recursive_s, t);
    env_vbind(dyn_env, package_s, cur_package);

    if (!txr_lisp_p) {
      int gc = gc_state(0);
      parser_t parser;

      parse_once(stream, name, &parser);
      gc_state(gc);

      if (parser.errors)
        sem_error(specline, lit("~s: errors encountered in ~a"), sym, path, nao);

      if (sym == include_s) {
        ret = parser.syntax_tree;
      } else {
        val spec = parser.syntax_tree;
        val result = match_files(mf_spec(*c, spec));

        if (!result) {
          debuglf(specline, lit("load: ~a failed"), path, nao);
        } else {
          cons_bind (new_bindings, success, result);

          c->bindings = new_bindings;

          if (consp(success)) {
            debuglf(specline,
                    lit("load: ~a matched; "
                        "advancing from line ~d to ~d"),
                    path, c->data_lineno, cdr(success), nao);
            c->data = car(success);
            c->data_lineno = cdr(success);
          } else {
            debuglf(specline, lit("load: ~a consumed entire file"), path,
                    nao);
            c->data = nil;
          }

          ret = next_spec_k;
        }
      }
    } else {
      if (!read_eval_stream(stream, std_error, nil)){
        close_stream(stream, nil);
        sem_error(specline, lit("load: ~a contains errors"), path, nao);
      }

      ret = (sym == include_s) ? nil : next_spec_k;
    }

    dyn_env = saved_dyn_env;

    uw_unwind {
      close_stream(stream, nil);
      if (!rec)
        uw_dump_deferred_warnings(std_null);
    }

    uw_catch_end;

    return ret;
  }
}

static val v_close(match_files_ctx *c)
{
  spec_bind (specline, first_spec, c->spec);
  val args = rest(first_spec);
  val stream = tleval(specline, first(args), c->bindings);

  if (rest(specline))
    sem_error(specline, lit("unexpected material after close"), nao);

  if (!streamp(stream))
    sem_error(specline, lit("close: ~s is not a stream"), stream, nao);

  close_stream(stream, t);
  return next_spec_k;
}

static val v_line(match_files_ctx *c)
{
  spec_bind (specline, first_spec, c->spec);
  val args = rest(first_spec);
  val pat = car(args);

  if (!args || rest(args))
      sem_error(specline, lit("line directive takes one argument"), nao);

  c->bindings = dest_bind(specline, c->bindings, pat, c->data_lineno, eql_f);

  if (c->bindings == t) {
    debuglf(specline, lit("line mismatch (line ~d vs. ~s)"), c->data_lineno, pat, nao);
    return nil;
  }

  return next_spec_k;
}

static val v_data(match_files_ctx *c)
{
  spec_bind (specline, first_spec, c->spec);
  val args = rest(first_spec);
  val pat = car(args);

  if (!args || rest(args))
    sem_error(specline, lit("data directive takes one argument"), nao);

  c->bindings = dest_bind(specline, c->bindings, pat, c->data, eql_f);

  if (c->bindings == t) {
    debuglf(specline, lit("data mismatch (data vs. ~s)"), pat, nao);
    return nil;
  }

  return next_spec_k;
}

static val v_name(match_files_ctx *c)
{
  spec_bind (specline, first_spec, c->spec);
  val args = rest(first_spec);
  val pat = car(args);

  if (!args || rest(args))
    sem_error(specline, lit("name directive takes one argument"), nao);

  c->bindings = dest_bind(specline, c->bindings, pat, c->curfile, equal_f);

  if (c->bindings == t) {
    debuglf(specline, lit("name mismatch (~s vs. ~s)"), c->curfile, pat, nao);
    return nil;
  }

  return next_spec_k;
}

static val v_call(match_files_ctx *c)
{
  spec_bind (specline, first_spec, c->spec);
  val exprs = rest(first_spec);
  val funexpr = car(exprs);
  val funval = tleval_144(specline, funexpr, c->bindings);
  val argexprs = cdr(exprs);
  val call = cons(funval, argexprs);
  val spec = cons(cons(call, nil), nil);
  match_files_ctx ctx = mf_spec_bindings(*c, spec, c->bindings);
  val ret = v_fun(&ctx);

  if (ret == nil)
    return nil;

  if (ret == decline_k)
    sem_error(nil, lit("call: function ~s not found"), funval, nao);

  return cons(ctx.bindings, if3(ctx.data, cons(ctx.data, ctx.data_lineno), t));
}

static val h_do(match_line_ctx *c)
{
  val elem = first(c->specline);
  val args = rest(elem);
  (void) tleval_progn(cons(c->specline, nil), args, c->bindings);
  return next_spec_k;
}

static val h_assert(match_line_ctx *c)
{
  val elem = rest(first(c->specline));
  val type = pop(&elem);
  val result = match_line(ml_specline(*c, rest(c->specline)));

  if (result) {
    return result;
  } else if (type) {
    val values = mapcar(curry_123_2(func_n3(txeval_allow_ub),
                                    c->specline, c->bindings), elem);
    uw_throw(type, values);
  } else {
    if (c->file)
      typed_error(assert_s, elem, lit("assertion (at ~a:~d)"), c->file, c->data_lineno, nao);
    typed_error(assert_s, elem, lit("assertion (line ~d)"), c->data_lineno, nao);
  }
  abort();
}

static void open_data_source(match_files_ctx *c)
{
  spec_bind (specline, first_spec, c->spec);
  int non_matching_dir = (consp(first_spec) &&
                          (gethash(non_matching_directive_table,
                                   first(first_spec))) &&
                          !rest(specline));

  /* c->data == t is set up by the top level call to match_files.
   * It indicates that we have not yet opened any data source.
   */

  if (c->data == t && c->files) {
    val source_spec = first(c->files);
    val ss_consp = consp(source_spec);
    val name = ss_consp ? cdr(source_spec) : source_spec;
    val nothrow = tnil(ss_consp && car(source_spec) == nothrow_k);

    if (non_matching_dir) {
      debuglf(first_spec, lit("not opening source ~a "
                              "since query starts with non-matching "
                              "directive."), name, nao);
    } else if (stringp(name)) {
      val stream = complex_open(name, nil, nil, nothrow, t);

      debuglf(specline, lit("opening data source ~a"), name, nao);

      if (!stream) {
        debuglf(first_spec, lit("could not open ~a: "
                                "treating as failed match due to nothrow"), name, nao);
        c->data = nil;
        return;
      }

      c->files = cons(name, cdr(c->files)); /* Get rid of cons and nothrow */
      c->curfile = source_spec;

      if ((c->data = lazy_stream_cons(stream)) != nil)
        c->data_lineno = one;
    } else if (streamp(name)) {
      if ((c->data = lazy_stream_cons(name)))
        c->data_lineno = one;
    } else {
      sem_error(specline, lit("~s doesn't denote a valid data source"), name, nao);
    }
  } else if (c->data == t && c->files == nil) {
    if (opt_compat && opt_compat <= 170) {
      c->data = nil;
    } else if (non_matching_dir) {
      debuglf(first_spec, lit("not opening standard input "
                              "since query starts with non-matching "
                              "directive."), nao);
    } else {
      debuglf(first_spec, lit("opening standard input as data source"), nao);
      c->curfile = lit("-");
      c->data = lazy_stream_cons(std_input);
      c->data_lineno = one;
    }
  }
}

static val match_files(match_files_ctx c)
{
  debug_enter;

  gc_hint(c.data);

  for (; c.spec; c.spec = rest(c.spec),
                 c.data = rest(c.data),
                 c.data_lineno = plus(c.data_lineno, one))
repeat_spec_same_data:
  {
    spec_bind (specline, first_spec, c.spec);

    open_data_source(&c);

    debug_check(specline, c.bindings, c.data, c.data_lineno, nil, nil);

    if (consp(first_spec) && !rest(specline)) {
      val lfe_save = set_last_form_evaled(first_spec);
      val sym = first(first_spec);
      val entry = gethash(v_directive_table, sym);

      if (entry) {
        v_match_func vmf = coerce(v_match_func, cptr_get(entry));
        val result;

        result = vmf(&c);

        set_last_form_evaled(lfe_save);

        if (result == next_spec_k) {
          if ((c.spec = rest(c.spec)) == nil)
            break;
          goto repeat_spec_same_data;
        } else if (result == decline_k) {
          /* go on to other processing below */
        } else {
          debug_return (result);
        }
      } else {
        val result = v_fun(&c);

        set_last_form_evaled(lfe_save);

        if (result == next_spec_k) {
          if ((c.spec = rest(c.spec)) == nil)
            break;
          goto repeat_spec_same_data;
        } else if (result == decline_k) {
          /* go on to other processing below */
        } else {
          debug_return (result);
        }
      }
    }

    open_data_source(&c);

    if (consp(c.data) && car(c.data))
    {
      val dataline = car(c.data);

      cons_bind (new_bindings, success,
                 match_line_completely(ml_all(c.bindings, specline,
                                              dataline, zero,
                                              c.data, c.data_lineno, c.curfile)));

      if (!success)
        debug_return (nil);

      c.bindings = new_bindings;
    } else if (consp(c.data) || nilp(c.data)) {
      debuglf(specline, lit("spec ran out of data"), nao);
      debug_return (nil);
    } else {
      internal_error("bug in data stream opening logic");
    }
  }

  debug_return (cons(c.bindings, if3(c.data, cons(c.data, c.data_lineno), t)));

  debug_leave;
}

val match_filter(val name, val arg, val other_args)
{
  cons_bind (in_spec, in_bindings, uw_get_match_context());
  spec_bind (specline, first_spec, in_spec);
  val in_arg_sym = make_sym(lit("in_arg"));
  val out_arg_sym = make_sym(lit("out_arg"));
  val bindings = cons(cons(in_arg_sym, arg), in_bindings);
  val spec = cons(list(cons(name,
                            cons(in_arg_sym, cons(out_arg_sym, other_args))),
                       nao), nil);
  match_files_ctx c = mf_all(spec, nil, bindings, nil, nil);
  val ret = v_fun(&c);

  (void) first_spec;
  rlcp(car(spec), specline);

  if (ret == nil)
    sem_error(specline, lit("filter: (~s ~s ~s) failed"), name,
              arg, out_arg_sym, nao);

  if (ret == decline_k)
    sem_error(specline, lit("filter: function ~s not found"), name, nao);

  {
    val out = tx_lookup_var(out_arg_sym, c.bindings);
    if (!out)
      sem_error(specline,
                lit("filter: (~s ~s ~s) did not bind ~s"), name,
                arg, out_arg_sym, out_arg_sym, nao);
    return cdr(out);
  }
}

val match_fun(val name, val args, val input, val files)
{
  val call = cons(name, args);
  val spec = cons(cons(call, nil), nil);
  val in_bindings = cdr(uw_get_match_context());
  val data = if3(streamp(input),
                 lazy_stream_cons(input),
                 input);
  /* TODO: pass through source location context */
  match_files_ctx c = mf_all(spec, files, in_bindings, data, nil);
  val ret;

  debug_enter;

  debug_check(call, c.bindings, c.data, c.data_lineno, nil, nil);

  ret = v_fun(&c);

  if (ret == nil)
    debug_return (nil);

  if (ret == decline_k)
    sem_error(nil, lit("match_fun: function ~s not found"), name, nao);

  debug_return (cons(c.bindings, if3(c.data, cons(c.data, c.data_lineno), t)));

  debug_leave;
}

val include(val specline)
{
  val spec = cons(specline, nil);
  match_files_ctx c = mf_all(spec, nil, nil, nil, nil);
  return v_load(&c);
}

val extract(val spec, val files, val predefined_bindings)
{
  val result = match_files(mf_all(spec, files, predefined_bindings,
                                  t, nil));
  cons_bind (bindings, success, result);

  if (opt_print_bindings) {
    if (bindings) {
      bindings = nreverse(bindings);
      rplaca(result, bindings);
      dump_bindings(bindings);
    }

    if (!success)
      put_line(lit("false"), std_output);
  }

  return result;
}

void match_reg_var(val sym)
{
  if (bindable(sym) && !uw_tentative_def_exists(sym)) {
    val tag = cons(var_s, sym);
    uw_register_tentative_def(tag);
  }
}

static void match_reg_var_rec(val sym)
{
  if (consp(sym)) {
    match_reg_var_rec(car(sym));
    match_reg_var_rec(cdr(sym));
    return;
  }
  match_reg_var(sym);
}

void match_reg_params(val params)
{
  for (; params; params = cdr(params)) {
    val var = car(params);
    if (atom(var))
      match_reg_var(var);
    else
      match_reg_var(car(var));
  }
}

void match_reg_elem(val elem)
{
  if (consp(elem)) {
    val sym = car(elem);
    val vpos = gethash(binding_directive_table, sym);
    if (vpos) {
      val var = ref(elem, vpos);
      match_reg_var_rec(var);
    } else if (!gethash(h_directive_table, sym) &&
               !gethash(v_directive_table, sym))
    {
      elem = cdr(elem);
      for (; consp(elem); elem = cdr(elem))
        match_reg_var(car(elem));
      match_reg_var(elem);
    }
  }
}

static void syms_init(void)
{
  decline_k = intern(lit("decline"), keyword_package);
  next_spec_k = intern(lit("next-spec"), keyword_package);
  repeat_spec_k = intern(lit("repeat-spec"), keyword_package);
  mingap_k = intern(lit("mingap"), keyword_package);
  maxgap_k = intern(lit("maxgap"), keyword_package);
  gap_k = intern(lit("gap"), keyword_package);
  mintimes_k = intern(lit("mintimes"), keyword_package);
  maxtimes_k = intern(lit("maxtimes"), keyword_package);
  times_k = intern(lit("times"), keyword_package);
  lines_k = intern(lit("lines"), keyword_package);
  chars_k = intern(lit("chars"), keyword_package);
  text_s = intern(lit("text"), system_package);
  choose_s = intern(lit("choose"), user_package);
  gather_s = intern(lit("gather"), user_package);
  do_s = intern(lit("do"), user_package);
  load_s = intern(lit("load"), user_package);
  include_s = intern(lit("include"), user_package);
  close_s = intern(lit("close"), user_package);
  require_s = intern(lit("require"), user_package);
  longest_k = intern(lit("longest"), keyword_package);
  shortest_k = intern(lit("shortest"), keyword_package);
  greedy_k = intern(lit("greedy"), keyword_package);
  vars_k = intern(lit("vars"), keyword_package);
  resolve_k = intern(lit("resolve"), keyword_package);
  append_k = intern(lit("append"), keyword_package);
  into_k = intern(lit("into"), keyword_package);
  var_k = intern(lit("var"), keyword_package);
  list_k = intern(lit("list"), keyword_package);
  tlist_k = intern(lit("tlist"), keyword_package);
  string_k = intern(lit("string"), keyword_package);
  env_k = intern(lit("env"), keyword_package);
  named_k = intern(lit("named"), keyword_package);
  continue_k = intern(lit("continue"), keyword_package);
  finish_k = intern(lit("finish"), keyword_package);
  mandatory_k = intern(lit("mandatory"), keyword_package);

  filter_s = intern(lit("filter"), user_package);
  noval_s = intern(lit("noval"), system_package);

  mod_s = intern(lit("mod"), user_package);
  modlast_s = intern(lit("modlast"), user_package);
  line_s = intern(lit("line"), user_package);
  data_s = intern(lit("data"), user_package);
  fuzz_s = intern(lit("fuzz"), user_package);
  counter_k = intern(lit("counter"), keyword_package);
}

static void dir_tables_init(void)
{
  protect(&h_directive_table, &v_directive_table,
          &non_matching_directive_table, &binding_directive_table,
          convert(val *, 0));

  h_directive_table = make_hash(nil, nil, nil);
  v_directive_table = make_hash(nil, nil, nil);
  non_matching_directive_table = make_hash(nil, nil, nil);
  binding_directive_table = make_hash(nil, nil, nil);

  sethash(v_directive_table, skip_s, cptr(coerce(mem_t *, v_skip)));
  sethash(v_directive_table, fuzz_s, cptr(coerce(mem_t *, v_fuzz)));
  sethash(v_directive_table, trailer_s, cptr(coerce(mem_t *, v_trailer)));
  sethash(v_directive_table, freeform_s, cptr(coerce(mem_t *, v_freeform)));
  sethash(v_directive_table, block_s, cptr(coerce(mem_t *, v_block)));
  sethash(v_directive_table, accept_s, cptr(coerce(mem_t *, v_accept_fail)));
  sethash(v_directive_table, fail_s, cptr(coerce(mem_t *, v_accept_fail)));
  sethash(v_directive_table, next_s, cptr(coerce(mem_t *, v_next)));
  sethash(v_directive_table, some_s, cptr(coerce(mem_t *, v_parallel)));
  sethash(v_directive_table, all_s, cptr(coerce(mem_t *, v_parallel)));
  sethash(v_directive_table, none_s, cptr(coerce(mem_t *, v_parallel)));
  sethash(v_directive_table, maybe_s, cptr(coerce(mem_t *, v_parallel)));
  sethash(v_directive_table, cases_s, cptr(coerce(mem_t *, v_parallel)));
  sethash(v_directive_table, choose_s, cptr(coerce(mem_t *, v_parallel)));
  sethash(v_directive_table, gather_s, cptr(coerce(mem_t *, v_gather)));
  sethash(v_directive_table, collect_s, cptr(coerce(mem_t *, v_collect)));
  sethash(v_directive_table, repeat_s, cptr(coerce(mem_t *, v_collect)));
  sethash(v_directive_table, flatten_s, cptr(coerce(mem_t *, v_flatten)));
  sethash(v_directive_table, forget_s, cptr(coerce(mem_t *, v_forget_local)));
  sethash(v_directive_table, local_s, cptr(coerce(mem_t *, v_forget_local)));
  sethash(v_directive_table, merge_s, cptr(coerce(mem_t *, v_merge)));
  sethash(v_directive_table, bind_s, cptr(coerce(mem_t *, v_bind)));
  sethash(v_directive_table, rebind_s, cptr(coerce(mem_t *, v_rebind)));
  sethash(v_directive_table, set_s, cptr(coerce(mem_t *, v_set)));
  sethash(v_directive_table, cat_s, cptr(coerce(mem_t *, v_cat)));
  sethash(v_directive_table, output_s, cptr(coerce(mem_t *, v_output)));
  sethash(v_directive_table, define_s, cptr(coerce(mem_t *, v_define)));
  sethash(v_directive_table, try_s, cptr(coerce(mem_t *, v_try)));
  sethash(v_directive_table, defex_s, cptr(coerce(mem_t *, v_defex)));
  sethash(v_directive_table, throw_s, cptr(coerce(mem_t *, v_throw)));
  sethash(v_directive_table, deffilter_s, cptr(coerce(mem_t *, v_deffilter)));
  sethash(v_directive_table, filter_s, cptr(coerce(mem_t *, v_filter)));
  sethash(v_directive_table, eof_s, cptr(coerce(mem_t *, v_eof)));
  sethash(v_directive_table, do_s, cptr(coerce(mem_t *, v_do)));
  sethash(v_directive_table, require_s, cptr(coerce(mem_t *, v_require)));
  sethash(v_directive_table, if_s, cptr(coerce(mem_t *, v_if)));
  sethash(v_directive_table, assert_s, cptr(coerce(mem_t *, v_assert)));
  sethash(v_directive_table, load_s, cptr(coerce(mem_t *, v_load)));
  sethash(v_directive_table, close_s, cptr(coerce(mem_t *, v_close)));
  sethash(v_directive_table, line_s, cptr(coerce(mem_t *, v_line)));
  sethash(v_directive_table, data_s, cptr(coerce(mem_t *, v_data)));
  sethash(v_directive_table, name_s, cptr(coerce(mem_t *, v_name)));
  sethash(v_directive_table, call_s, cptr(coerce(mem_t *, v_call)));

  sethash(h_directive_table, text_s, cptr(coerce(mem_t *, h_text)));
  sethash(h_directive_table, var_s, cptr(coerce(mem_t *, h_var)));
  sethash(h_directive_table, skip_s, cptr(coerce(mem_t *, h_skip)));
  sethash(h_directive_table, block_s, cptr(coerce(mem_t *, h_block)));
  sethash(h_directive_table, accept_s, cptr(coerce(mem_t *, h_accept_fail)));
  sethash(h_directive_table, fail_s, cptr(coerce(mem_t *, h_accept_fail)));
  sethash(h_directive_table, coll_s, cptr(coerce(mem_t *, h_coll)));
  sethash(h_directive_table, rep_s, cptr(coerce(mem_t *, h_coll)));
  sethash(h_directive_table, flatten_s, cptr(coerce(mem_t *, hv_trampoline)));
  sethash(h_directive_table, forget_s, cptr(coerce(mem_t *, hv_trampoline)));
  sethash(h_directive_table, local_s, cptr(coerce(mem_t *, hv_trampoline)));
  sethash(h_directive_table, merge_s, cptr(coerce(mem_t *, hv_trampoline)));
  sethash(h_directive_table, bind_s, cptr(coerce(mem_t *, hv_trampoline)));
  sethash(h_directive_table, rebind_s, cptr(coerce(mem_t *, hv_trampoline)));
  sethash(h_directive_table, set_s, cptr(coerce(mem_t *, hv_trampoline)));
  sethash(h_directive_table, cat_s, cptr(coerce(mem_t *, hv_trampoline)));
  sethash(h_directive_table, filter_s, cptr(coerce(mem_t *, hv_trampoline)));
  sethash(h_directive_table, some_s, cptr(coerce(mem_t *, h_parallel)));
  sethash(h_directive_table, all_s, cptr(coerce(mem_t *, h_parallel)));
  sethash(h_directive_table, none_s, cptr(coerce(mem_t *, h_parallel)));
  sethash(h_directive_table, maybe_s, cptr(coerce(mem_t *, h_parallel)));
  sethash(h_directive_table, cases_s, cptr(coerce(mem_t *, h_parallel)));
  sethash(h_directive_table, choose_s, cptr(coerce(mem_t *, h_parallel)));
  sethash(h_directive_table, trailer_s, cptr(coerce(mem_t *, h_trailer)));
  sethash(h_directive_table, define_s, cptr(coerce(mem_t *, h_define)));
  sethash(h_directive_table, throw_s, cptr(coerce(mem_t *, hv_trampoline)));
  sethash(h_directive_table, eol_s, cptr(coerce(mem_t *, h_eol)));
  sethash(h_directive_table, chr_s, cptr(coerce(mem_t *, h_chr)));
  sethash(h_directive_table, do_s, cptr(coerce(mem_t *, h_do)));
  sethash(h_directive_table, require_s, cptr(coerce(mem_t *, hv_trampoline)));
  sethash(h_directive_table, assert_s, cptr(coerce(mem_t *, h_assert)));
  sethash(h_directive_table, line_s, cptr(coerce(mem_t *, hv_trampoline)));
  sethash(h_directive_table, data_s, cptr(coerce(mem_t *, hv_trampoline)));
  sethash(h_directive_table, name_s, cptr(coerce(mem_t *, hv_trampoline)));
  sethash(h_directive_table, call_s, cptr(coerce(mem_t *, h_call)));

  sethash(non_matching_directive_table, block_s, t);
  sethash(non_matching_directive_table, accept_s, t);
  sethash(non_matching_directive_table, fail_s, t);
  sethash(non_matching_directive_table, next_s, t);
  sethash(non_matching_directive_table, forget_s, t);
  sethash(non_matching_directive_table, local_s, t);
  sethash(non_matching_directive_table, merge_s, t);
  sethash(non_matching_directive_table, bind_s, t);
  sethash(non_matching_directive_table, rebind_s, t);
  sethash(non_matching_directive_table, set_s, t);
  sethash(non_matching_directive_table, cat_s, t);
  sethash(non_matching_directive_table, output_s, t);
  sethash(non_matching_directive_table, define_s, t);
  sethash(non_matching_directive_table, try_s, t);
  sethash(non_matching_directive_table, defex_s, t);
  sethash(non_matching_directive_table, throw_s, t);
  sethash(non_matching_directive_table, deffilter_s, t);
  sethash(non_matching_directive_table, filter_s, t);
  sethash(non_matching_directive_table, require_s, t);
  sethash(non_matching_directive_table, assert_s, t);
  sethash(non_matching_directive_table, do_s, t);
  sethash(non_matching_directive_table, load_s, t);
  sethash(non_matching_directive_table, close_s, t);

  sethash(binding_directive_table, var_s, one);
  sethash(binding_directive_table, merge_s, one);
  sethash(binding_directive_table, bind_s, one);
  sethash(binding_directive_table, rebind_s, one);
  sethash(binding_directive_table, line_s, one);
  sethash(binding_directive_table, chr_s, one);
  sethash(binding_directive_table, data_s, one);
  sethash(binding_directive_table, name_s, one);
}

void match_init(void)
{
  syms_init();
  dir_tables_init();
}
