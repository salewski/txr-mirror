/* Copyright 2009-2014
 * Kaz Kylheku <kaz@kylheku.com>
 * Vancouver, Canada
 * All rights reserved.
 *
 * BSD License:
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 *   1. Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *   2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in
 *      the documentation and/or other materials provided with the
 *      distribution.
 *   3. The name of the author may not be used to endorse or promote
 *      products derived from this software without specific prior
 *      written permission.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <dirent.h>
#include <setjmp.h>
#include <stdarg.h>
#include <wchar.h>
#include <signal.h>
#include "config.h"
#include "lib.h"
#include "gc.h"
#include "signal.h"
#include "unwind.h"
#include "regex.h"
#include "stream.h"
#include "parser.h"
#include "txr.h"
#include "utf8.h"
#include "filter.h"
#include "hash.h"
#include "debug.h"
#include "eval.h"
#include "match.h"

int opt_nobindings = 0;
int opt_lisp_bindings = 0;
int opt_arraydims = 1;

val decline_k, next_spec_k, repeat_spec_k;
val mingap_k, maxgap_k, gap_k, mintimes_k, maxtimes_k, times_k;
val lines_k, chars_k;
val text_s, choose_s, gather_s, do_s, mod_s, modlast_s, fuzz_s, load_s;
val close_s, require_s;
val longest_k, shortest_k, greedy_k;
val vars_k, resolve_k;
val append_k, into_k, var_k, list_k, string_k, env_k, counter_k;
val named_k, continue_k, finish_k;

val filter_s;

val noval_s;

static val h_directive_table, v_directive_table;

static void debuglf(val form, val fmt, ...)
{
  if (opt_loglevel >= 2) {
    va_list vl;
    va_start (vl, fmt);
    format(std_error, lit("~a: (~a) "), prog_string, source_loc_str(form), nao);
    vformat(std_error, fmt, vl);
    put_char(chr('\n'), std_error);
    va_end (vl);
  }
}

static void sem_error(val form, val fmt, ...)
{
  va_list vl;
  val stream = make_string_output_stream();

  va_start (vl, fmt);
  if (form)
    format(stream, lit("(~a) "), source_loc_str(form), nao);
  (void) vformat(stream, fmt, vl);
  va_end (vl);

  uw_throw(query_error_s, get_string_from_stream(stream));
  abort();
}

static void file_err(val form, val fmt, ...)
{
  va_list vl;
  val stream = make_string_output_stream();

  va_start (vl, fmt);
  if (form)
    format(stream, lit("(~a) "), source_loc_str(form), nao);
  (void) vformat(stream, fmt, vl);
  va_end (vl);

  uw_throw(file_error_s, get_string_from_stream(stream));
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

    obj_pprint(value, ss);
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

static val dest_set(val spec, val bindings, val pattern, val value)
{
  if (symbolp(pattern)) {
    val existing = assoc(pattern, bindings);
    if (!bindable(pattern))
      sem_error(spec, lit("~s cannot be used as a variable"), pattern, nao);
    if (!existing)
      sem_error(spec, lit("cannot set unbound variable ~s"), pattern, nao);
    set(*cdr_l(existing), value);
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

static val dest_bind(val spec, val bindings, val pattern,
                     val value, val testfun)
{
  if (symbolp(pattern)) {
    if (bindable(pattern)) {
      val existing = assoc(pattern, bindings);
      if (existing) {
        if (tree_find(value, cdr(existing), swap_12_21(testfun)))
          return bindings;
        if (tree_find(cdr(existing), value, testfun))
          return bindings;
        debuglf(spec, lit("bind variable mismatch: ~a"), pattern, nao);
        return t;
      }
      return cons(cons(pattern, value), bindings);
    } else {
      return funcall2(testfun, pattern, value) ? bindings : t;
    }
  } else if (consp(pattern)) {
    val piter = pattern, viter = value;

    if (first(pattern) == var_s) {
      sem_error(spec, lit("metavariable @~a syntax cannot be used here"), second(pattern), nao);
    }

    if (first(pattern) == expr_s) {
      sem_error(spec, lit("the @~s syntax cannot be used here"), rest(pattern), nao);
    }


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

static val txeval(val spec, val form, val bindings);

static val vars_to_bindings(val spec, val vars, val bindings)
{
  val iter;
  list_collect_decl (fixed_vars, ptail);

  if (vars && !consp(vars)) 
    sem_error(spec, lit("not a valid variable list: ~a"), vars, nao);

  for (iter = vars; iter; iter = cdr(iter)) {
    val item = car(iter);
    if (bindable(item)) {
      list_collect (ptail, cons(item, noval_s));
    } else if (consp(item) && bindable(first(item))) {
      list_collect (ptail, cons(first(item), 
                                txeval(spec, second(item), bindings)));
    } else { 
      sem_error(spec, lit("not a variable spec: ~a"), item, nao);
    }
  }
  return fixed_vars;
}

typedef struct {
  val bindings, specline, dataline, base, pos, data_lineno, file;
} match_line_ctx;

static match_line_ctx ml_all(val bindings, val specline, val dataline,
                             val pos, val data_lineno, val file)
{
  match_line_ctx c;
  c.bindings = bindings;
  c.specline = specline;
  c.dataline = dataline;
  c.base = zero;
  c.pos = pos;
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
  debuglf(elem, lit(KIND " mismatch, position ~a (~a:~a)"),             \
          plus(c->pos, c->base), c->file, c->data_lineno, nao);         \
  debuglf(elem, lit("  ~a"), c->dataline, nao);                         \
  if (c_num(c->pos) < 77)                                               \
    debuglf(elem, lit("  ~*~a^"), c->pos, lit(""), nao)

#define LOG_MATCH(KIND, EXTENT)                                         \
  debuglf(elem, lit(KIND " matched, position ~a-~a (~a:~a)"),           \
          plus(c->pos, c->base), EXTENT, c->file, c->data_lineno, nao); \
  debuglf(elem, lit("  ~a"), c->dataline, nao);                         \
  if (c_num(EXTENT) < 77)                                               \
    debuglf(elem, lit("  ~*~a~<*~a^"), c->pos, lit(""),                 \
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
    c->pos = new_pos;
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


static val search_form(match_line_ctx *c, val needle_form, val from_end)
{
  if (regexp(first(needle_form))) {
    return search_regex(c->dataline, first(needle_form), c->pos, from_end);
  } else {
    val spec = cons(needle_form, nil);
    val pos = from_end ? length_str(c->dataline) : c->pos;
    val step = from_end ? num(-1) : num(1);

    rlcp(spec, needle_form);

    for (; (from_end && ge(pos, c->pos)) || 
           (!from_end && length_str_ge(c->dataline, pos));
         pos = plus(pos, step))
    {
      cons_bind (new_bindings, new_pos,
                 match_line(ml_specline_pos(*c, spec, pos)));
      if (new_pos == t) {
        return cons(pos, t);
      } else if (new_pos) {
        new_pos = minus(new_pos, c->base);
        c->bindings = new_bindings;
        return cons(pos, minus(new_pos, pos));
      }

      consume_prefix(c);
    }

    return nil;
  }
}

static val h_var(match_line_ctx *c)
{
  val elem = first(c->specline);
  val sym = second(elem);
  val pat = third(elem);
  val modifiers = fourth(elem);
  val modifier = first(modifiers);
  val pair = if2(sym, assoc(sym, c->bindings)); /* exists? */

  if (sym == t)
    sem_error(elem, lit("t is not a bindable symbol"), nao);
  
  if (gt(length_list(modifiers), one)) {
    sem_error(elem, lit("multiple modifiers on variable ~s"),
              sym, nao);
  }

  if (pair) {
    /* If the variable already has a binding, we replace
       it with its value, and treat it as a string match.
       The spec looks like ((var <sym> <pat>) ...)
       and it must be transformed into
       (<sym-substituted> <pat> ...) */
    if (pat) {
      c->specline = rlcp(cons(cdr(pair), cons(pat, rest(c->specline))), c->specline);
    } else if (fixnump(modifier)) {
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
      c->specline = cdr(c->specline);
    } else {
      c->specline = rlcp(cons(cdr(pair), rest(c->specline)), c->specline);
    }
    return repeat_spec_k;
  } else if (consp(modifier)) { /* var bound over text matched by form */
    cons_bind (new_bindings, new_pos,
               match_line(ml_specline(*c, modifiers)));

    if (!new_pos) {
      LOG_MISMATCH("var spanning form");
      return nil;
    }

    LOG_MATCH("var spanning form", new_pos);
    if (sym)
      c->bindings = acons(sym, sub_str(c->dataline, c->pos, new_pos), new_bindings);
    c->pos = new_pos;
    /* This may have another variable attached */
    if (pat) {
      c->specline = rlcp(cons(pat, rest(c->specline)), c->specline);
      return repeat_spec_k;
    }
  } else if (fixnump(modifier)) { /* fixed field */
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
    if (pat) {
      c->specline = cons(pat, rest(c->specline));
      return repeat_spec_k;
    }
  } else if (modifier && modifier != t) {
    sem_error(elem, lit("invalid modifier ~s on variable ~s"),
              modifier, sym, nao);
  } else if (pat == nil) { /* no modifier, no elem -> to end of line */
    if (sym)
      c->bindings = acons(sym, sub_str(c->dataline, c->pos, nil), c->bindings);
    c->pos = length_str(c->dataline);
  } else if (type(pat) == STR) {
    val find = search_str(c->dataline, pat, c->pos, modifier);
    if (!find) {
      LOG_MISMATCH("var delimiting string");
      return nil;
    }
    LOG_MATCH("var delimiting string", find);
    if (sym)
      c->bindings = acons(sym, sub_str(c->dataline, c->pos, find), c->bindings);
    c->pos = plus(find, length_str(pat));
  } else if (consp(pat) && first(pat) != var_s) {
    val find = search_form(c, pat, modifier);
    val fpos = car(find);
    val flen = cdr(find);
    if (!find) {
      LOG_MISMATCH("var delimiting form");
      return nil;
    }
    LOG_MATCH("var delimiting form", fpos);
    if (sym)
      c->bindings = acons(sym, sub_str(c->dataline, c->pos, fpos), c->bindings);
    c->pos = if3(flen == t, t, plus(fpos, flen));
  } else if (consp(pat)) {
    /* Unbound var followed by var: the following one must either
       be bound, or must specify a regex. */
    val second_sym = second(pat);
    val next_pat = third(pat);
    val next_modifiers = fourth(pat);
    val next_modifier = first(fourth(pat));
    val pair = if2(second_sym, assoc(second_sym, c->bindings)); /* exists? */

    if (gt(length_list(next_modifiers), one)) {
      sem_error(elem, lit("multiple modifiers on variable ~s"),
                second_sym, nao);
    }

    if (!pair && consp(next_modifier)) {
      val find = search_form(c, next_modifier, modifier);
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
      if (next_pat) {
        c->specline = rlcp(cons(next_pat, rest(c->specline)), c->specline);
        return repeat_spec_k;
      }
    } else if (!pair) {
      sem_error(elem, lit("consecutive unbound variables"), nao);
    } else {
    /* Re-generate a new spec with an edited version of
       the element we just processed, and repeat. */
      val new_elem = list(var_s, sym, cdr(pair), modifier, nao);

      if (next_pat)
         c->specline = cons(new_elem, cons(next_pat, rest(c->specline)));
      else
         c->specline = cons(new_elem, rest(c->specline));
      return repeat_spec_k;
    }
  } else if (consp(pat) && (consp(first(pat)) || stringp(first(pat)))) {
    cons_bind (find, len, search_str(c->dataline, pat, c->pos, modifier));
    if (!find) {
      LOG_MISMATCH("string");
      return nil;
    }
    if (sym)
      c->bindings = acons(sym, sub_str(c->dataline, c->pos, find), c->bindings);
    c->pos = plus(find, len);
  } else {
    sem_error(elem, lit("variable followed by invalid element"), nao);
  }

  return next_spec_k;
}

static val h_skip(match_line_ctx *c)
{
  val elem = first(c->specline);
  val max = txeval(elem, second(elem), c->bindings);
  val min = txeval(elem, third(elem), c->bindings);
  cnum cmax = fixnump(max) ? c_num(max) : 0;
  cnum cmin = fixnump(min) ? c_num(min) : 0;
  val greedy = eq(max, greedy_k);
  val last_good_result = nil, last_good_pos = nil;

  if (!rest(c->specline)) {
    debuglf(elem, 
            lit("skip to end of line ~a:~a"), c->file, c->data_lineno, nao);
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
                lit("skipped only ~a/~a chars to ~a:~a:~a"),
                num(reps_min), num(cmin),
                c->file, c->data_lineno, c->pos, nao);
        return nil;
      }

      debuglf(elem, lit("skipped ~a chars to ~a:~a:~a"),
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

static val h_coll(match_line_ctx *c)
{
  val elem = first(c->specline);
  val coll_specline = second(elem);
  val until_last_specline = third(elem);
  val args = fourth(elem);
  val bindings_coll = nil;
  val last_bindings = nil;
  val max = txeval(elem, getplist(args, maxgap_k), c->bindings);
  val min = txeval(elem, getplist(args, mingap_k), c->bindings);
  val gap = txeval(elem, getplist(args, gap_k), c->bindings);
  val times = txeval(elem, getplist(args, times_k), c->bindings);
  val mintimes = txeval(elem, getplist(args, mintimes_k), c->bindings);
  val maxtimes = txeval(elem, getplist(args, maxtimes_k), c->bindings);
  val chars = txeval(elem, getplist(args, chars_k), c->bindings);
  val have_vars;
  val vars = getplist_f(args, vars_k, &have_vars);
  cnum cmax = fixnump(gap) ? c_num(gap) : (fixnump(max) ? c_num(max) : 0);
  cnum cmin = fixnump(gap) ? c_num(gap) : (fixnump(min) ? c_num(min) : 0);
  cnum mincounter = cmin, maxcounter = 0;
  cnum ctimax = fixnump(times) ? c_num(times) 
                               : (fixnump(maxtimes) ? c_num(maxtimes) : 0);
  cnum ctimin = fixnump(times) ? c_num(times) 
                               : (fixnump(mintimes) ? c_num(mintimes) : 0);
  cnum cchars = fixnump(chars) ? c_num(chars) : 0;
  cnum timescounter = 0, charscounter = 0;
  val iter;

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
      cons_set (new_bindings, new_pos,
                match_line(ml_specline(*c, coll_specline)));

      if (until_last_specline) {
        cons_bind (sym, spec, until_last_specline);
        cons_bind (until_last_bindings, until_pos,
                   match_line(ml_bindings_specline(*c, new_bindings, spec)));

        if (until_pos) {
          LOG_MATCH("until/last", until_pos);
          if (sym == last_s) {
            last_bindings = set_diff(until_last_bindings,
                                     new_bindings, eq_f, nil);
            c->pos = until_pos;
          }
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
        LOG_MATCH("coll", new_pos);

        for (iter = vars; iter; iter = cdr(iter)) {
          cons_bind (var, dfl, car(iter));
          val exists = assoc(var, new_bindings);

          if (!exists) {
            if (dfl == noval_s)
              list_collect (ptail, var);
            else
              strictly_new_bindings = acons(var, dfl, strictly_new_bindings);
          }
        }

        if (have_new && missing)
          sem_error(elem, lit("collect failed to bind ~a"),
                    missing, nao);

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
    debuglf(elem, lit("fewer than ~a iterations collected"),
            num(ctimin), nao);
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
      val exists = assoc(sym, c->bindings);
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
      if (!assoc(var, c->bindings))
        push(var, &resolve_ub_vars);
    }
  }

  for (iter = specs; iter != nil; iter = cdr(iter)) {
    val nested_spec = first(iter);
    cons_bind (new_bindings, new_pos,
               match_line(ml_specline(*c, nested_spec)));

    if (new_pos) {
      some_match = t;

      if (resolve_ub_vars) {
        val uiter;
        for (uiter = resolve_ub_vars; uiter; uiter = cdr(uiter)) {
          val ubvar = car(uiter);
          val exists = assoc(ubvar, new_bindings);

          if (exists)
            resolve_bindings = acons_new(ubvar, cdr(exists), resolve_bindings);
        }

        new_bindings = alist_remove(new_bindings, resolve_ub_vars);
      }

      if (new_pos == t || gt(new_pos, max_pos))
        max_pos = new_pos;

      if (directive == choose_s) {
        val binding = choose_sym ? assoc(choose_sym, new_bindings) : nil;
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
  val result = match_line(ml_specline(*c, rest(c->specline)));
  val new_pos = cdr(result);
  val elem = first(c->specline);

  if (!new_pos) {
    LOG_MISMATCH("trailer");
    return nil;
  }

  LOG_MATCH("trailer", new_pos);
  return cons(c->bindings, plus(c->pos, c->base));
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
      sem_error(elem, lit("function ~a takes ~a argument(s)"),
                sym, length_list(params), nao);

    for (piter = params, aiter = args; piter;
         piter = cdr(piter), aiter = cdr(aiter))
    {
      val param = car(piter);
      val arg = car(aiter);

      if (arg && bindable(arg)) {
        val val = assoc(arg, c->bindings);
        if (val) {
          bindings_cp = acons_new(param, cdr(val), bindings_cp);
        } else {
          bindings_cp = alist_nremove1(bindings_cp, param);
          ub_p_a_pairs = cons(cons(param, arg), ub_p_a_pairs);
        }
      } else {
        val val = txeval(elem, arg, c->bindings);
        bindings_cp = acons_new(param, val, bindings_cp);
      }
    }

    {
      uw_block_begin(nil, result);
      uw_env_begin;
      debug_frame(sym, bindings_cp, ub_p_a_pairs, c->bindings, c->dataline, c->data_lineno, c->pos);

      result = match_line(ml_bindings_specline(*c, bindings_cp, body));

      debug_end;
      uw_env_end;
      uw_block_end;

      if (!result) {
        debuglf(elem, lit("function (~s ~s) failed"), sym, args, nao);
        return nil;
      }

      {
        cons_bind (new_bindings, success, result);

        for (piter = ub_p_a_pairs; piter; piter = cdr(piter))
        {
          cons_bind (param, arg, car(piter));

          if (symbolp(arg)) {
            val newbind = assoc(param, new_bindings);
            if (newbind) {
              c->bindings = dest_bind(elem, c->bindings, 
                                      arg, cdr(newbind), equal_f);
              if (c->bindings == t) {
                debuglf(elem,
                        lit("binding mismatch on ~a "
                            "when returning from ~a"), arg, sym, nao);
                return nil;
              }
            }
          }
        }

        c->pos = success;
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
    return cons(c->bindings, plus(c->pos, c->base));
  }
  LOG_MISMATCH("eol");
  return nil;
}

typedef struct {
  val spec, files, curfile, bindings, data, data_lineno;
} match_files_ctx;

static match_files_ctx mf_all(val spec, val files, val bindings,
                              val data, val data_lineno, val curfile);

static val v_fun(match_files_ctx *c);

static val do_match_line(match_line_ctx *c)
{
  debug_enter;

  for (;;) {
    val elem;

    if (c->specline == nil)
      break;

    if (c->pos == t)
      c->pos = length_str(c->dataline);

    consume_prefix(c);

    elem = first(c->specline);

    debug_check(elem, c->bindings, c->dataline, c->data_lineno,
                c->pos, c->base);

    switch (type(elem)) {
    case CONS: /* directive */
      {
        val directive = first(elem);

        if (regexp(directive)) {
          val past = match_regex(c->dataline, directive, c->pos);
          if (nullp(past)) {
            LOG_MISMATCH("regex");
            debug_return (nil);
          }
          LOG_MATCH("regex", past);
          c->pos = past;
        } else if (consp(directive) || stringp(directive)) {
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
            h_match_func hmf = (h_match_func) cptr_get(entry);
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
                                          nil, num(0), c->file);
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
    default:
      sem_error(elem, lit("unsupported object in spec: ~s"), elem, nao);
    }

    c->specline = cdr(c->specline);
  }

  debug_return (cons(c->bindings, plus(c->pos, c->base)));
  debug_leave;
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
      debuglf(c.specline, lit("spec only matches line to position ~a: ~a"),
              new_pos, c.dataline, nao);
      return nil;
    }
  }

  return result;
}


val format_field(val obj, val modifier, val filter, val eval_fun)
{
  val n = zero, sep = lit(" ");
  val plist = nil;
  val str;

  for (; modifier; pop(&modifier)) {
    val item = first(modifier);
    if (regexp(item)) {
      uw_throw(query_error_s, lit("format_field: regex modifier in output"));
    } else if (keywordp(item)) {
      plist = modifier;
      break;
    } else if (consp(item) && car(item) == dwim_s) {
      val arg_expr = second(item);

      if (consp(arg_expr) && car(arg_expr) == cons_s) {
        val from = funcall1(eval_fun, second(arg_expr));
        val to = funcall1(eval_fun, third(arg_expr));

        obj = sub(obj, from, to);
      } else {
         val arg = funcall1(eval_fun, arg_expr);
         if (bignump(arg) || fixnump(arg)) {
           obj = ref(obj, arg);
         } else {
           uw_throwf(query_error_s, lit("format_field: bad index: ~s"),
                     arg, nao);
         }
      }
    } else {
      val v = funcall1(eval_fun, item);
      if (fixnump(v))
        n = v;
      else if (stringp(v))
        sep = v;
      else
        uw_throwf(query_error_s, lit("format_field: bad modifier object: ~s"),
                  item, nao);
    }
  }

  if (listp(obj))
    str = cat_str(mapcar(func_n1(tostringp), obj), sep);
  else
    str = if3(stringp(obj), obj, tostringp(obj));

  {
    val filter_sym = getplist(plist, filter_k);

    if (filter_sym) {
      filter = get_filter(filter_sym);

      if (!filter) {
        uw_throwf(query_error_s, lit("format_field: ~s specifies unknown filter"),
                  filter_sym, nao);
      }
    }

    if (filter)
      str = filter_string_tree(filter, str);
  }

  {
    val right = lt(n, zero);
    val width = if3(lt(n, zero), neg(n), n);
    val diff = minus(width, length_str(str));

    if (le(diff, zero))
      return str;

    if (ge(length_str(str), width))
      return str;

    {
      val padding = mkstring(diff, chr(' '));

      return if3(right,
                 cat_str(list(padding, str, nao), nil),
                 cat_str(list(str, padding, nao), nil));
    }
  }
}

static val subst_vars(val spec, val bindings, val filter)
{
  list_collect_decl(out, iter);

  while (spec) {
    val elem = first(spec);

    if (consp(elem)) {
      val sym = first(elem);

      if (sym == var_s) {
        val expr = second(elem);
        val pat = third(elem);
        val modifiers = fourth(elem);
        val str = txeval(spec, expr, bindings);

        if (!stringp(str) && !listp(str))
          str = format(nil, lit("~a"), str, nao);

        if (pat)
          spec = cons(pat, rest(spec));

        if (modifiers) {
          spec = cons(format_field(str, modifiers, filter,
                                   curry_123_2(func_n3(txeval), spec, bindings)),
                      rest(spec));
        } else {
          if (listp(str))
            str = cat_str(mapcar(func_n1(tostringp), str), lit(" "));
          else
            str = if3(stringp(str), str, tostringp(str));

          spec = cons(filter_string_tree(filter, str), rest(spec));
        }

        continue;
      } else if (sym == quasi_s) {
        val nested = subst_vars(rest(elem), bindings, filter);
        list_collect_append(iter, nested);
        spec = cdr(spec);
        continue;
      } else if (sym == expr_s) {
        val result = eval(rest(elem), make_env(bindings, nil, nil), elem);
        spec = cons(format(nil, lit("~a"), result, nao), rest(spec));
        continue;
      } else {
        val nested = subst_vars(elem, bindings, filter);
        list_collect_append(iter, nested);
        spec = cdr(spec);
        continue;
      }
    }

    list_collect(iter, elem);
    spec = cdr(spec);
  }

  return out;
}

static val do_txeval(val spec, val form, val bindings, val allow_unbound)
{
  val ret = nil;
  uw_mark_frame;
  uw_catch_begin (cons(query_error_s, nil), exc_sym, exc);

  if (!form)
    uw_fast_return(nil);

  {
    if (!form) {
      ret = form;
    } else if (bindable(form)) {
      val binding = assoc(form, bindings);
      if (!binding) {
        if (allow_unbound)
          ret = noval_s;
        else
          sem_error(spec, lit("unbound variable ~s"), form, nao);
      } else {
        ret = cdr(binding);
      }
    } else if (consp(form)) {
      if (first(form) == quasi_s) {
        uw_env_begin;
        uw_set_match_context(cons(spec, bindings));
        ret = cat_str(subst_vars(rest(form), bindings, nil), nil);
        uw_env_end;
      } else if (regexp(car(form))) {
        ret = form;
      } else if (first(form) == var_s) {
        uw_env_begin;
        uw_set_match_context(cons(spec, bindings));
        ret = eval(second(form), make_env(bindings, nil, nil), form);
        uw_env_end;
      } else if (first(form) == expr_s) {
        uw_env_begin;
        uw_set_match_context(cons(spec, bindings));
        ret = eval(rest(form), make_env(bindings, nil, nil), form);
        uw_env_end;
      } else {
        ret =  mapcar(curry_123_2(func_n3(txeval), spec, bindings), form);
      }
    } else if (stringp(form)) {
      ret = form;
    } else {
      ret = form;
    }

    uw_catch (exc_sym, exc) {
      if (stringp(exc) && !equal(exc, lit("")) && 
          chr_str(exc, zero) == chr('('))
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

enum fpip_close { fpip_fclose, fpip_pclose, fpip_closedir };

typedef struct fpip {
  FILE *f;
  DIR *d;
  enum fpip_close close;
} fpip_t;

static fpip_t complex_open(val name, val output, val append)
{
  fpip_t ret = { 0, 0 };
  const wchar_t *namestr = c_str(name);
  cnum len = c_num(length_str(name));

  if (len == 0)
    return ret;

  if (!wcscmp(namestr, L"-")) {
    ret.close = fpip_fclose;
    ret.f = output ? stdout : stdin;
  } else if (namestr[0] == '!') {
    ret.close = fpip_pclose;
    ret.f = w_popen(namestr+1, output ? L"w" : L"r");
  } else if (namestr[0] == '$') {
    char *name;
    if (output)
      return ret;
    name = utf8_dup_to(namestr+1);
    ret.close = fpip_closedir;
    ret.d = opendir(name);
    free(name);
  } else {
    ret.close = fpip_fclose;
    ret.f = w_fopen(namestr, output ? append ? L"a" : L"w" : L"r");
  }

  return ret;
}

static int complex_open_failed(fpip_t fp)
{
  return fp.f == 0 && fp.d == 0;
}

static val complex_snarf(fpip_t fp, val name)
{
  switch (fp.close) {
  case fpip_fclose:
    return lazy_stream_cons(make_stdio_stream(fp.f, name));
  case fpip_pclose:
    return lazy_stream_cons(make_pipe_stream(fp.f, name));
  case fpip_closedir:
    return lazy_stream_cons(make_dir_stream(fp.d));
  }

  internal_error("bad input source type");
}

static val complex_stream(fpip_t fp, val name)
{
  switch (fp.close) {
  case fpip_fclose:
    return make_stdio_stream(fp.f, name);
  case fpip_pclose:
    return make_pipe_stream(fp.f, name);
  case fpip_closedir:
    uw_throwf(query_error_s, lit("cannot output to directory: ~a"), name, nao);
  }

  internal_error("bad input source type");
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
      if (bindable(second(output_spec)))
        list_collect (tai, second(output_spec));
      else
        list_collect_nconc (tai, extract_vars(second(output_spec)));
    } else if (sym != expr_s) {
      for (; output_spec; output_spec = cdr(output_spec))
        list_collect_nconc(tai, extract_vars(car(output_spec)));
    }
  }

  return vars;
}

static val extract_bindings(val bindings, val output_spec, val vars)
{
  list_collect_decl (bindings_out, ptail);
  val var_list = nappend2(vars, extract_vars(output_spec));

  for (; bindings; bindings = cdr(bindings)) {
    val binding = car(bindings);
    val sym = car(binding);
    if (assoc(sym, bindings_out))
      continue;
    if (memq(sym, var_list))
      list_collect(ptail, binding);
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
          val str = cat_str(subst_vars(cons(elem, nil),
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
          val counter = getplist(args, counter_k);
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
                rplacd(counter_var, num_fast(i));
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
                    val n = txeval(args, first(args), bind_a);
                    val m = txeval(args, second(args), bind_a);

                    if (eql(mod(num_fast(i), m), n))
                      list_collect_append (ptail, rest(clause));
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
                  val n = txeval(args, first(args), bind_a);
                  val m = txeval(args, second(args), bind_a);

                  if (eql(mod(num_fast(i), m), n))
                    list_collect_append (ptail, rest(clause));
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
          format(out, lit("~a"), 
                 eval(rest(elem), make_env(bindings, nil, nil), elem), nao);
        }
      }
      break;
    case STR:
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
        val counter = getplist(args, counter_k);
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
              rplacd(counter_var, num_fast(i));
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
                  val n = txeval(args, first(args), bind_a);
                  val m = txeval(args, second(args), bind_a);

                  if (eql(mod(num_fast(i), m), n))
                    list_collect_append (ptail, rest(clause));
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
                val n = txeval(args, first(args), bind_a);
                val m = txeval(args, second(args), bind_a);

                if (eql(mod(num_fast(i), m), n))
                  list_collect_append (ptail, rest(clause));
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
                              val data, val data_lineno,
                              val curfile)
{
  match_files_ctx c;
  c.spec = spec;
  c.files = files;
  c.curfile = curfile;
  c.bindings = bindings;
  c.data = data;
  c.data_lineno = data_lineno;
  return c;
}

static match_files_ctx mf_args(match_files_ctx c)
{
  match_files_ctx nc = c;
  nc.data = c.files;
  nc.curfile = lit("args");
  nc.data_lineno = num(1);
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
  return mf_all(cons(ml.specline, nil), nil, ml.bindings, nil, num(0), ml.file);
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
    val max = txeval(skipspec, first(args), c->bindings);
    val min = txeval(skipspec, second(args), c->bindings);
    cnum cmax = fixnump(max) ? c_num(max) : 0;
    cnum cmin = fixnump(min) ? c_num(min) : 0;
    val greedy = eq(max, greedy_k);
    volatile val last_good_result = nil;
    volatile val last_good_line = num(0);

    {
      cnum reps_max = 0, reps_min = 0;
      uw_block_begin(nil, result);

      while (c->data && min && reps_min < cmin) {
        c->data = rest(c->data);
        c->data_lineno = plus(c->data_lineno, num(1));
        reps_min++;
      }

      if (min) {
        if (reps_min != cmin) {
          debuglf(skipspec, lit("skipped only ~a/~a lines to ~a:~a"),
                  num(reps_min), num(cmin),
                  c->curfile, c->data_lineno, nao);
          uw_block_return(nil, nil);
        }

        debuglf(skipspec, lit("skipped ~a lines to ~a:~a"),
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
            debuglf(skipspec, lit("skip matched ~a:~a"), c->curfile,
                    c->data_lineno, nao);
            break;
          }
        } else {
          debuglf(skipspec, lit("skip didn't match ~a:~a"),
                  c->curfile, c->data_lineno, nao);
        }

        if (!c->data)
          break;

        debuglf(skipspec, lit("skip didn't match ~a:~a"), c->curfile,
                c->data_lineno, nao);

        c->data = rest(c->data);
        c->data_lineno = plus(c->data_lineno, num(1));
      }

      uw_block_end;

      if (result)
        return result;
      if (last_good_result) {
        debuglf(skipspec, lit("greedy skip matched ~a:~a"), 
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
    val m = txeval(fuzz_spec, first(args), c->bindings);
    val n = txeval(fuzz_spec, second(args), c->bindings);
    cnum cm = fixnump(m) ? c_num(m) : 0;
    cnum cn = fixnump(n) ? c_num(n) : 0;

    {
      cnum reps, good;
  
      for (reps = 0, good = 0; reps < cn; reps++) {
        match_files_ctx fuzz_ctx = mf_spec(*c, cons(first(c->spec), nil));
        val result = match_files(fuzz_ctx);

        if (result) {
          debuglf(fuzz_spec, lit("fuzz matched ~a:~a"), c->curfile,
                  c->data_lineno, nao);
          good++;
        } else {
          debuglf(fuzz_spec, lit("fuzz didn't match ~a:~a"),
                  c->curfile, c->data_lineno, nao);
        }

        if (!c->data)
          break;
        c->data = rest(c->data);
        c->data_lineno = plus(c->data_lineno, num(1));
        c->spec = rest(c->spec);
        if (!c->spec) {
          if (good >= cm)
            break;
          debuglf(fuzz_spec, lit("fuzz failed ~a:~a"), c->curfile,
                  c->data_lineno, nao);
          return nil;
        }
      }

      if (reps == cn && good < cm) {
        debuglf(fuzz_spec, lit("fuzz failed ~a:~a"), c->curfile,
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
      if (ex->uw.type == UW_BLOCK && ex->bl.protocol == accept_s)
        rplacd(ex->bl.result, cons(c->data, c->data_lineno));
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
  val vals = mapcar(curry_123_2(func_n3(txeval), first_spec, c->bindings),
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

      if (fixnump(success)) {
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
  val limit = or2(if2(fixnump(first(vals)), first(vals)),
                  if2(fixnump(second(vals)), second(vals)));
  val term = or2(if2(stringp(first(vals)), first(vals)),
                 if2(stringp(second(vals)), second(vals)));
  val dataline = lazy_str(c->data, term, limit);
  *mlc = ml_all(c->bindings, first_spec, dataline, zero, c->data_lineno, c->curfile);
  return limit;
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

    {
      cons_bind (new_bindings, success, result);

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
                            cons(c->bindings,
                                 if3(c->data, cons(c->data, c->data_lineno),
                                     t))),
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

static val v_next(match_files_ctx *c)
{
  spec_bind (specline, first_spec, c->spec);
  
  if ((c->spec = rest(c->spec)) == nil)
    return cons(c->bindings, cons(c->data, c->data_lineno));

  if (rest(first_spec)) {
    val args = rest(first_spec);
    val source = first(args);

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
    }

    if (source == env_k) {
      if (rest(args)) {
        sem_error(specline, lit("(next :env) takes no additional arguments"), nao);
      } else {
        cons_bind (new_bindings, success,
                   match_files(mf_data(*c, env(), num(1))));

        if (success)
          return cons(new_bindings,
                      if3(c->data, cons(c->data, c->data_lineno), t));
        return nil;
      }
    }
    
    if (keywordp(first(args))) {
      source = nil;
    } else {
      pop(&args);
    }

    if (args && !keywordp(first(args)))
      sem_error(specline, lit("next: keyword argument expected, not ~s"), first(args), nao);

    {
      val alist = improper_plist_to_alist(args, list(nothrow_k, nao));
      val from_var = cdr(assoc(var_k, alist));
      val list_expr = cdr(assoc(list_k, alist));
      val string_expr = cdr(assoc(string_k, alist));
      val nothrow = cdr(assoc(nothrow_k, alist));
      val str = txeval(specline, source, c->bindings);

      if (!from_var && !source && !string_expr && !list_expr)
        sem_error(specline, lit("next: source required before keyword arguments"), nao);

      if ((from_var && string_expr) || (string_expr && list_expr) ||
          (from_var && list_expr))
      {
        sem_error(specline, lit("next: only one of :var, :list or :string can be specified"), nao);
      }

      if (from_var) {
        val existing = assoc(from_var, c->bindings);

        if (!symbolp(from_var)) 
          sem_error(specline, lit(":var requires a variable, not ~s"), from_var, nao);

        if (!existing)
          sem_error(specline, lit(":var specifies unbound variable ~s"), from_var, nao);

        {
          cons_bind (new_bindings, success,
                     match_files(mf_file_data(*c, lit("var"),
                                 lazy_flatten(cdr(existing)), num(1))));

          if (success)
            return cons(new_bindings,
                        if3(c->data, cons(c->data, c->data_lineno), t));
          return nil;
        }
      } else if (list_expr) {
        val list_val = txeval(specline, list_expr, c->bindings);
        cons_bind (new_bindings, success,
                   match_files(mf_file_data(*c, lit("var"),
                               lazy_flatten(list_val), num(1))));

        if (success)
          return cons(new_bindings,
                      if3(c->data, cons(c->data, c->data_lineno), t));
        return nil;
      } else if (string_expr) {
        val str_val = txeval(specline, string_expr, c->bindings);
        if (!stringp(str_val))
          sem_error(specline, lit(":string arg ~s evaluated to non-string ~s"), string_expr, str_val, nao);

        {
          cons_bind (new_bindings, success,
                     match_files(mf_file_data(*c, lit("var"),
                                 split_str(str_val, lit("\n")), num(1))));

          if (success)
            return cons(new_bindings,
                        if3(c->data, cons(c->data, c->data_lineno), t));
          return nil;
        }
      } else if (nothrow) {
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
      } else {
        if (str) {
          c->files = cons(str, c->files);
        } else {
          c->files = rest(c->files);
          if (!c->files)
            sem_error(specline, lit("next: out of arguments"), nao);
          c->files = cons(cons(nothrow_k, first(c->files)), rest(c->files));
        }
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
        if (!assoc(var, c->bindings))
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
            val exists = assoc(ubvar, new_bindings);

            if (exists)
              resolve_bindings = acons_new(ubvar, cdr(exists), resolve_bindings);
          }

          new_bindings = alist_remove(new_bindings, resolve_ub_vars);
        }

        if (sym == choose_s) {
          val binding = choose_sym ? assoc(choose_sym, new_bindings) : nil;
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
  val vars = vars_to_bindings(specline, getplist(args, vars_k), c->bindings);

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
        *cdr_l(iter) = nil;
        list_collect_nconc(ptail, iter);
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
      cons_bind (sym, ul_spec, until_last);
      cons_bind (until_last_bindings, success,
                 match_files(mf_spec(*c, ul_spec)));

      if (success) {
        debuglf(specline, lit("until/last matched ~a:~a"),
                c->curfile, c->data_lineno, nao);
        /* Until discards bindings and position, last keeps them. */
        if (sym == last_s) {
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
        break;
      }
    }

    specs = new_specs;

    if (consp(max_data)) {
      debuglf(specline, lit("gather advancing from line ~a to ~a"),
              c->data_lineno, max_line, nao);
      c->data_lineno = max_line;
      c->data = max_data;
    } else if (max_data == t) {
      debuglf(specline, lit("gather consumed entire file"), nao);
      c->data = nil;
    } else {
      c->data_lineno = plus(c->data_lineno, num(1));
      c->data = rest(c->data);
      debuglf(specline, lit("gather advancing by one line to ~a"), c->data_lineno, nao);
    }
  }

  if (vars) {
    val iter;

    for (iter = vars; iter != nil; iter = cdr(iter)) {
      cons_bind (var, dfl_val, car(iter));
      if (!assoc(var, c->bindings)) {
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

static val v_collect(match_files_ctx *c)
{
  spec_bind (specline, first_spec, c->spec);
  val op_sym = first(first_spec);
  val coll_spec = second(first_spec);
  val until_last_spec = third(first_spec);
  val args = fourth(first_spec);
  volatile val bindings_coll = nil;
  volatile val last_bindings = nil;
  val max = txeval(specline, getplist(args, maxgap_k), c->bindings);
  val min = txeval(specline, getplist(args, mingap_k), c->bindings);
  val gap = txeval(specline, getplist(args, gap_k), c->bindings);
  val times = txeval(specline, getplist(args, times_k), c->bindings);
  val mintimes = txeval(specline, getplist(args, mintimes_k), c->bindings);
  val maxtimes = txeval(specline, getplist(args, maxtimes_k), c->bindings);
  val lines = txeval(specline, getplist(args, lines_k), c->bindings);
  val have_vars;
  volatile val vars = getplist_f(args, vars_k, &have_vars);
  cnum cmax = fixnump(gap) ? c_num(gap) : (fixnump(max) ? c_num(max) : 0);
  cnum cmin = fixnump(gap) ? c_num(gap) : (fixnump(min) ? c_num(min) : 0);
  cnum mincounter = cmin, maxcounter = 0;
  cnum ctimax = fixnump(times) ? c_num(times) 
                               : (fixnump(maxtimes) ? c_num(maxtimes) : 0);
  cnum ctimin = fixnump(times) ? c_num(times) 
                               : (fixnump(mintimes) ? c_num(mintimes) : 0);
  volatile cnum timescounter = 0, linescounter = 0;
  cnum ctimes = fixnump(times) ? c_num(times) : 0;
  cnum clines = fixnump(lines) ? c_num(lines) : 0;
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
      cons_set (new_bindings, success,
                match_files(mf_spec(*c, coll_spec)));

      /* Until/last clause sees un-collated bindings from collect. */
      if (until_last_spec)
      {
        cons_bind (sym, ul_spec, until_last_spec);
        cons_bind (until_last_bindings, success,
                   match_files(mf_spec_bindings(*c, ul_spec, new_bindings)));

        if (success) {
          debuglf(specline, lit("until/last matched ~a:~a"),
                  c->curfile, c->data_lineno, nao);
          /* Until discards bindings and position, last keeps them. */
          if (sym == last_s) {
            last_bindings = set_diff(until_last_bindings,
                                     new_bindings, eq_f, nil);
            if (success == t) {
              debuglf(specline, lit("collect consumed entire file"), nao);
              c->data = nil;
            } else {
              cons_bind (new_data, new_line, success);
              c->data = new_data;
              c->data_lineno = new_line;
            }
          }
          break;
        }
      }

      if (success) {
        list_collect_decl (missing, ptail);
        val strictly_new_bindings = set_diff(new_bindings,
                                             c->bindings, eq_f, nil);
        val have_new = strictly_new_bindings;

        debuglf(specline, lit("collect matched ~a:~a"),
                c->curfile, c->data_lineno, nao);

        for (iter = vars; iter; iter = cdr(iter)) {
          cons_bind (var, dfl, car(iter));
          val exists = assoc(var, new_bindings);

          if (!exists) {
            if (dfl == noval_s) 
              list_collect (ptail, var);
            else
              strictly_new_bindings = acons(var, dfl, strictly_new_bindings);
          }
        }

        if (have_new && missing)
          sem_error(specline, lit("collect failed to bind ~a"),
                    missing, nao);

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
            new_line = plus(new_line, num(1));
          }

          debuglf(specline, lit("collect advancing from line ~a to ~a"),
                  c->data_lineno, new_line, nao);

          c->data = new_data;
          c->data_lineno = new_line;
          *car_l(success) = nil;
        } else {
          debuglf(specline, lit("collect consumed entire file"), nao);
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
        c->data_lineno = plus(c->data_lineno, num(1));
        c->data = rest(c->data);
      }
    }
  }

  uw_block_end;

  if (!result) {
    debuglf(specline, lit("collect explicitly failed"), nao);
    return nil;
  }

  if ((times || mintimes) && timescounter < ctimin) {
    debuglf(specline, lit("fewer than ~a iterations collected"),
            num(ctimin), nao);
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
      val exists = assoc(sym, c->bindings);
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
      val existing = assoc(sym, c->bindings);

      if (existing)
        set(*cdr_l(existing), flatten(cdr(existing)));
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
    v_match_func vmf = (v_match_func) cptr_get(entry);
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

  dest_set(specline, c->bindings, pattern, val);

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
    val existing = assoc(sym, c->bindings);
    if (existing) {
      val sep = if3(sep_form, txeval(specline, sep_form, c->bindings),
                    lit(" "));
      set(*cdr_l(existing), cat_str(flatten(cdr(existing)), sep));
    } else {
      sem_error(specline, lit("cat: unbound variable ~s"), sym, nao);
    }
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
  val dest = lit("-");
  val filter = nil;
  val named_var = nil, continue_expr = nil, finish_expr = nil;
  val alist;
  fpip_t fp;

  if (eq(first(dest_spec), nothrow_k)) {
    if (rest(dest_spec))
      sem_error(specline, lit("material after :nothrow in output"), nao);
  } else if (!keywordp(first(dest_spec))) {
    uses_or2;
    val form = first(dest_spec);
    val val = txeval(specline, form, c->bindings);
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
      uw_env_begin;
      uw_set_match_context(cons(c->spec, c->bindings));
      do_output(c->bindings, specs, filter, stream);
      flush_stream(stream);
      uw_env_end;

      {
        val existing = assoc(into_var, c->bindings);
        val list_out = get_list_from_stream(stream);

        if (existing) {
          if (append) {
            set(*cdr_l(existing), append2(flatten(cdr(existing)), list_out));
          } else {
            set(*cdr_l(existing), list_out);
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
    val stream = txeval(specline, which, c->bindings);


    if (!streamp(stream))
      sem_error(specline, lit("~s evaluated to ~s which is not a stream"), which, stream, nao);

    uw_env_begin;
    uw_set_match_context(cons(c->spec, c->bindings));
    do_output(c->bindings, specs, filter, stream);
    flush_stream(stream);
    uw_env_end;
    if (finish_expr)
      close_stream(stream, t);
    return next_spec_k;
  }

  fp = (errno = 0, complex_open(dest, t, append));

  debuglf(specline, lit("opening data sink ~a"), dest, nao);

  if (complex_open_failed(fp)) {
    if (nothrow) {
      debuglf(specline, lit("could not open ~a: "
                            "treating as failed match due to nothrow"), dest, nao);
      return nil;
    } else if (errno != 0) {
      file_err(specline, lit("could not open ~a (error ~a/~a)"), dest,
               num(errno), string_utf8(strerror(errno)), nao);
    } else {
      file_err(specline, lit("could not open ~a"), dest, nao);
    }
  } else {
    val stream = complex_stream(fp, dest);
    uw_env_begin;
    uw_set_match_context(cons(c->spec, c->bindings));
    do_output(c->bindings, specs, filter, stream);
    flush_stream(stream);
    uw_env_end;

    if (named_var)
      c->bindings = acons(named_var, stream, c->bindings);
    else
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
          val vals = if3(listp(exvals),
                         exvals,
                         cons(exvals, nil));

          if (first(clause) == catch_s) {
            if (uw_exception_subtype_p(exsym, type)) {
              val all_bind = t;
              val piter, viter;

              for (piter = params, viter = vals;
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
    val existing = assoc(var, c->bindings);

    if (!bindable(var))
      sem_error(specline, lit("filter: ~a is not a variable name"), 
                var, nao);

    if (!existing)
      sem_error(specline, lit("filter: variable ~a is unbound"), var, nao);

    set(*cdr_l(existing), filter_string_tree(filter, cdr(existing)));
  }

  uw_env_end;
  return next_spec_k;
}

static val v_eof(match_files_ctx *c)
{
  if (c->data) {
    debuglf(c->spec, lit("eof failed to match at ~a"), c->data_lineno, nao);
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

      if (arg && bindable(arg)) {
        val val = assoc(arg, c->bindings);
        if (val) {
          bindings_cp = acons_new(param, cdr(val), bindings_cp);
        } else {
          bindings_cp = alist_nremove1(bindings_cp, param);
          ub_p_a_pairs = cons(cons(param, arg), ub_p_a_pairs);
        }
      } else {
        val val = txeval(specline, arg, c->bindings);
        bindings_cp = acons_new(param, val, bindings_cp);
      }
    }

    {
      uw_block_begin(nil, result);
      uw_env_begin;
      debug_frame(sym, bindings_cp, ub_p_a_pairs, c->bindings, if2(consp(c->data), car(c->data)),
                  c->data_lineno, nil);
      result = match_files(mf_spec_bindings(*c, body, bindings_cp));
      debug_end;
      uw_env_end;
      uw_block_end;

      if (!result) {
        debuglf(specline, lit("function (~s ~s) failed"), sym, args, nao);
        return nil;
      }

      {
        cons_bind (new_bindings, success, result);

        for (piter = ub_p_a_pairs; piter; piter = cdr(piter))
        {
          cons_bind (param, arg, car(piter));

          if (symbolp(arg)) {
            val newbind = assoc(param, new_bindings);
            if (newbind) {
              c->bindings = dest_bind(specline, c->bindings, 
                                     arg, cdr(newbind), equal_f);
              if (c->bindings == t) {
                debuglf(specline,
                        lit("binding mismatch on ~a "
                            "when returning from ~a"), arg, sym, nao);
                return nil;
              }
            }
          }
        }

        if (consp(success)) {
          debuglf(specline,
                  lit("function matched; "
                      "advancing from line ~a to ~a"),
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
  uw_set_match_context(cons(c->spec, c->bindings));
  (void) eval_progn(args, make_env(c->bindings, nil, nil), specline);
  return next_spec_k;
}

static val v_require(match_files_ctx *c)
{
  spec_bind (specline, first_spec, c->spec);
  val args = rest(first_spec);
  uw_set_match_context(cons(c->spec, c->bindings));
  if (!eval_progn(args, make_env(c->bindings, nil, nil), specline))
    return nil;
  return next_spec_k;
}


static val v_load(match_files_ctx *c)
{
  uses_or2;
  spec_bind (specline, first_spec, c->spec);
  val args = rest(first_spec);
  val parent = or2(cdr(source_loc(specline)), null_string);
  val target = txeval(specline, first(args), c->bindings);

  if (rest(specline))
    sem_error(specline, lit("unexpected material after load"), nao);

  if (!stringp(target))
    sem_error(specline, lit("load: path ~s is not a string"), target, nao);

  if (equal(target, null_string))
    sem_error(specline, lit("load: null string path given"), nao);

  {
    val path = if3(chr_str(target, zero) == chr('/'),
                   target,
                   cat_str(nappend2(sub_list(split_str(parent, lit("/")),
                                         zero, negone),
                                    cons(target, nil)), lit("/")));
    int gc = gc_state(0);
    parse_reset(path);
    yyparse();
    yylex_destroy();
    gc_state(gc);

    if (errors)
      sem_error(specline, lit("load: errors encountered in ~s"), path, nao);

    { 
      val spec = get_spec();
      val result = match_files(mf_spec(*c, spec));

      if (!result) {
        debuglf(specline, lit("load: ~s failed"), path, nao);
        return nil;
      } else {
        cons_bind (new_bindings, success, result);

        c->bindings = new_bindings;

        if (consp(success)) {
          debuglf(specline,
                  lit("load: ~s matched; "
                      "advancing from line ~a to ~a"),
                  path, c->data_lineno, cdr(success), nao);
          c->data = car(success);
          c->data_lineno = cdr(success);
        } else {
          debuglf(specline, lit("load: ~s consumed entire file"), path,
                  nao);
          c->data = nil;
        }

        return next_spec_k;
      }
    }
  }
}

static val v_close(match_files_ctx *c)
{
  spec_bind (specline, first_spec, c->spec);
  val args = rest(first_spec);
  val stream = txeval(specline, first(args), c->bindings);

  if (rest(specline))
    sem_error(specline, lit("unexpected material after close"), nao);

  if (!streamp(stream))
    sem_error(specline, lit("close: ~s is not a stream"), stream, nao);

  close_stream(stream, t);
  return next_spec_k;
}


static val h_do(match_line_ctx *c)
{
  val elem = first(c->specline);
  val args = rest(elem);
  (void) eval_progn(args, make_env(c->bindings, nil, nil), elem);
  return next_spec_k;
}

static val match_files(match_files_ctx c)
{
  debug_enter;

  gc_hint(c.data);

  if (listp(c.data)) { /* recursive call with lazy list */
    ; /* no special initialization */
  } else if (c.files) { /* c.data == t: toplevel call with file list */
    val source_spec = first(c.files);
    val name = consp(source_spec) ? cdr(source_spec) : source_spec;

    if (stringp(name)) {
      fpip_t fp = (errno = 0, complex_open(name, nil, nil));
      spec_bind (specline, first_spec, c.spec);

      if (consp(first_spec) && eq(first(first_spec), next_s) && !rest(specline)) {
        debuglf(first_spec, lit("not opening source ~a "
                                     "since query starts with next directive"), name, nao);
      } else {
        val spec = first(c.spec);
        debuglf(spec, lit("opening data source ~a"), name, nao);

        if (complex_open_failed(fp)) {
          if (consp(source_spec) && car(source_spec) == nothrow_k) {
            debuglf(spec, lit("could not open ~a: "
                              "treating as failed match due to nothrow"), name, nao);
            debug_return (nil);
          } else if (errno != 0)
            file_err(spec, lit("could not open ~a (error ~a/~a)"), name,
                     num(errno), string_utf8(strerror(errno)), nao);
          else
            file_err(spec, lit("could not open ~a"), name, nao);
          debug_return (nil);
        }

        c.files = cons(name, cdr(c.files)); /* Get rid of cons and nothrow */

        if ((c.data = complex_snarf(fp, name)) != nil)
          c.data_lineno = num(1);
      }
    } else if (streamp(name)) {
      if ((c.data = lazy_stream_cons(name)))
        c.data_lineno = num(1);
    } else {
      c.data = nil;
    }
  } else { /* toplevel call with no data or file list */
    c.data = nil;
  }

  for (; c.spec; c.spec = rest(c.spec), 
                 c.data = rest(c.data),
                 c.data_lineno = plus(c.data_lineno, num(1)))
repeat_spec_same_data:
  {
    spec_bind (specline, first_spec, c.spec);

    debug_check(first_spec, c.bindings, c.data, c.data_lineno, nil, nil);

    if (consp(first_spec) && !rest(specline)) {
      val sym = first(first_spec);
      val entry = gethash(v_directive_table, sym);

      if (entry) {
        v_match_func vmf = (v_match_func) cptr_get(entry);
        val result;

        result = vmf(&c);

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

    if (c.data && car(c.data))
    {
      val dataline = car(c.data);

      cons_bind (new_bindings, success,
                 match_line_completely(ml_all(c.bindings, specline,
                                              dataline, zero,
                                              c.data_lineno, c.curfile)));

      if (!success)
        debug_return (nil);

      c.bindings = new_bindings;
    } else {
      debuglf(specline, lit("spec ran out of data"), nao);
      debug_return (nil);
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
  match_files_ctx c = mf_all(spec, nil, bindings, nil, num(0), nil);
  val ret = v_fun(&c);

  (void) first_spec;
  rlcp(car(spec), specline);

  if (ret == nil)
    sem_error(specline, lit("filter: (~s ~s ~s) failed"), name,
              arg, out_arg_sym, nao);

  if (ret == decline_k)
    sem_error(specline, lit("filter: function ~s not found"), name, nao);

  {
    val out = assoc(out_arg_sym, c.bindings);
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
  match_files_ctx c = mf_all(spec, files, in_bindings, data,
                             if3(data, one, zero), nil);
  val ret;

  debug_enter;

  debug_check(call, c.bindings, c.data, c.data_lineno, nil, nil);

  ret = v_fun(&c);

  if (ret == nil)
    return nil;

  if (ret == decline_k)
    sem_error(nil, lit("match_fun: function ~s not found"), name, nao);

  debug_return (cons(c.bindings, if3(c.data, cons(c.data, c.data_lineno), t)));

  debug_leave;
}

int extract(val spec, val files, val predefined_bindings)
{
  cons_bind (bindings, success, match_files(mf_all(spec, files, 
                                                   predefined_bindings,
                                                   t, nil, nil)));

  if ((!output_produced && opt_nobindings <= 0) || opt_nobindings < 0) {
    if (bindings) {
      bindings = nreverse(bindings);
      dump_bindings(bindings);
    }

    if (!success)
      put_line(lit("false"), std_output);
  }

  return success ? 0 : EXIT_FAILURE;
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
  string_k = intern(lit("string"), keyword_package);
  env_k = intern(lit("env"), keyword_package);
  named_k = intern(lit("named"), keyword_package);
  continue_k = intern(lit("continue"), keyword_package);
  finish_k = intern(lit("finish"), keyword_package);

  filter_s = intern(lit("filter"), user_package);
  noval_s = intern(lit("noval"), system_package);

  mod_s = intern(lit("mod"), user_package);
  modlast_s = intern(lit("modlast"), user_package);
  fuzz_s = intern(lit("fuzz"), user_package);
  counter_k = intern(lit("counter"), keyword_package);
}

static void dir_tables_init(void)
{
  h_directive_table = make_hash(nil, nil, nil);
  v_directive_table = make_hash(nil, nil, nil);

  protect(&h_directive_table, &v_directive_table, (val *) 0);

  sethash(v_directive_table, skip_s, cptr((mem_t *) v_skip));
  sethash(v_directive_table, fuzz_s, cptr((mem_t *) v_fuzz));
  sethash(v_directive_table, trailer_s, cptr((mem_t *) v_trailer));
  sethash(v_directive_table, freeform_s, cptr((mem_t *) v_freeform));
  sethash(v_directive_table, block_s, cptr((mem_t *) v_block));
  sethash(v_directive_table, accept_s, cptr((mem_t *) v_accept_fail));
  sethash(v_directive_table, fail_s, cptr((mem_t *) v_accept_fail));
  sethash(v_directive_table, next_s, cptr((mem_t *) v_next));
  sethash(v_directive_table, some_s, cptr((mem_t *) v_parallel));
  sethash(v_directive_table, all_s, cptr((mem_t *) v_parallel));
  sethash(v_directive_table, none_s, cptr((mem_t *) v_parallel));
  sethash(v_directive_table, maybe_s, cptr((mem_t *) v_parallel));
  sethash(v_directive_table, cases_s, cptr((mem_t *) v_parallel));
  sethash(v_directive_table, choose_s, cptr((mem_t *) v_parallel));
  sethash(v_directive_table, gather_s, cptr((mem_t *) v_gather));
  sethash(v_directive_table, collect_s, cptr((mem_t *) v_collect));
  sethash(v_directive_table, repeat_s, cptr((mem_t *) v_collect));
  sethash(v_directive_table, flatten_s, cptr((mem_t *) v_flatten));
  sethash(v_directive_table, forget_s, cptr((mem_t *) v_forget_local));
  sethash(v_directive_table, local_s, cptr((mem_t *) v_forget_local));
  sethash(v_directive_table, merge_s, cptr((mem_t *) v_merge));
  sethash(v_directive_table, bind_s, cptr((mem_t *) v_bind));
  sethash(v_directive_table, rebind_s, cptr((mem_t *) v_rebind));
  sethash(v_directive_table, set_s, cptr((mem_t *) v_set));
  sethash(v_directive_table, cat_s, cptr((mem_t *) v_cat));
  sethash(v_directive_table, output_s, cptr((mem_t *) v_output));
  sethash(v_directive_table, define_s, cptr((mem_t *) v_define));
  sethash(v_directive_table, try_s, cptr((mem_t *) v_try));
  sethash(v_directive_table, defex_s, cptr((mem_t *) v_defex));
  sethash(v_directive_table, throw_s, cptr((mem_t *) v_throw));
  sethash(v_directive_table, deffilter_s, cptr((mem_t *) v_deffilter));
  sethash(v_directive_table, filter_s, cptr((mem_t *) v_filter));
  sethash(v_directive_table, eof_s, cptr((mem_t *) v_eof));
  sethash(v_directive_table, do_s, cptr((mem_t *) v_do));
  sethash(v_directive_table, require_s, cptr((mem_t *) v_require));
  sethash(v_directive_table, load_s, cptr((mem_t *) v_load));
  sethash(v_directive_table, close_s, cptr((mem_t *) v_close));

  sethash(h_directive_table, text_s, cptr((mem_t *) h_text));
  sethash(h_directive_table, var_s, cptr((mem_t *) h_var));
  sethash(h_directive_table, skip_s, cptr((mem_t *) h_skip));
  sethash(h_directive_table, coll_s, cptr((mem_t *) h_coll));
  sethash(h_directive_table, flatten_s, cptr((mem_t *) hv_trampoline));
  sethash(h_directive_table, forget_s, cptr((mem_t *) hv_trampoline));
  sethash(h_directive_table, local_s, cptr((mem_t *) hv_trampoline));
  sethash(h_directive_table, merge_s, cptr((mem_t *) hv_trampoline));
  sethash(h_directive_table, bind_s, cptr((mem_t *) hv_trampoline));
  sethash(h_directive_table, rebind_s, cptr((mem_t *) hv_trampoline));
  sethash(h_directive_table, set_s, cptr((mem_t *) hv_trampoline));
  sethash(h_directive_table, cat_s, cptr((mem_t *) hv_trampoline));
  sethash(h_directive_table, filter_s, cptr((mem_t *) hv_trampoline));
  sethash(h_directive_table, some_s, cptr((mem_t *) h_parallel));
  sethash(h_directive_table, all_s, cptr((mem_t *) h_parallel));
  sethash(h_directive_table, none_s, cptr((mem_t *) h_parallel));
  sethash(h_directive_table, maybe_s, cptr((mem_t *) h_parallel));
  sethash(h_directive_table, cases_s, cptr((mem_t *) h_parallel));
  sethash(h_directive_table, choose_s, cptr((mem_t *) h_parallel));
  sethash(h_directive_table, trailer_s, cptr((mem_t *) h_trailer));
  sethash(h_directive_table, define_s, cptr((mem_t *) h_define));
  sethash(h_directive_table, eol_s, cptr((mem_t *) h_eol));
  sethash(h_directive_table, do_s, cptr((mem_t *) h_do));
  sethash(h_directive_table, require_s, cptr((mem_t *) hv_trampoline));
}

void match_init(void)
{
  syms_init();
  dir_tables_init();
}
