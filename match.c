/* Copyright 2011
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
#include "config.h"
#include "lib.h"
#include "gc.h"
#include "unwind.h"
#include "regex.h"
#include "stream.h"
#include "parser.h"
#include "txr.h"
#include "utf8.h"
#include "filter.h"
#include "hash.h"
#include "debug.h"
#include "match.h"

int output_produced;
int opt_nobindings = 0;
int opt_lisp_bindings = 0;
int opt_arraydims = 1;

val decline_k, next_spec_k, repeat_spec_k;
val mingap_k, maxgap_k, gap_k, mintimes_k, maxtimes_k, times_k;
val lines_k, chars_k;
val choose_s, gather_s;
val longest_k, shortest_k, greedy_k;
val vars_k, resolve_k;
val append_k, into_k, var_k, list_k, string_k, env_k;

val filter_s;

val noval_s;

static val h_directive_table, v_directive_table;

static void debuglf(val form, val fmt, ...)
{
  if (opt_loglevel >= 2) {
    va_list vl;
    va_start (vl, fmt);
    format(std_error, lit("~a: (~a:~a) "), prog_string,
           spec_file_str, source_loc(form), nao);
    vformat(std_error, fmt, vl);
    put_char(std_error, chr('\n'));
    va_end (vl);
  }
}

static void sem_error(val form, val fmt, ...)
{
  va_list vl;
  val stream = make_string_output_stream();

  va_start (vl, fmt);
  if (form)
    format(stream, lit("(~a:~a) "), spec_file_str, source_loc(form), nao);
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
    format(stream, lit("(~a:~a) "), spec_file_str, source_loc(form), nao);
  (void) vformat(stream, fmt, vl);
  va_end (vl);

  uw_throw(file_error_s, get_string_from_stream(stream));
  abort();
}


static void dump_shell_string(const wchar_t *str)
{
  int ch;

  put_char(std_output, chr('"'));
  while ((ch = *str++) != 0) {
    switch (ch) {
    case '"': case '`': case '$': case '\\': case '\n':
      put_char(std_output, chr('\\'));
      /* fallthrough */
    default:
      put_char(std_output, chr(ch));
    }
  }
  put_char(std_output, chr('"'));
}

static void dump_byte_string(const char *str)
{
  while (*str)
    put_char(std_output, chr(*str++));
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

    put_string(std_output, var);
    dump_byte_string(pfx1);
    dump_byte_string(pfx2);
    put_char(std_output, chr('='));
    dump_shell_string(c_str(str));
    put_char(std_output, chr('\n'));
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

static val bindable(val obj)
{
  return (obj && symbolp(obj) && obj != t && !keywordp(obj)) ? t : nil;
}

static val dest_set(val spec, val bindings, val pattern, val value)
{
  if (symbolp(pattern)) {
    val existing = assoc(bindings, pattern);
    if (!bindable(pattern))
      sem_error(spec, lit("~s cannot be used as a variable"), pattern, nao);
    if (!existing)
      sem_error(spec, lit("cannot set unbound variable ~s"), pattern, nao);
    *cdr_l(existing) = value;
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
      val existing = assoc(bindings, pattern);
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

static val eval_form(val spec, val form, val bindings);

static val vars_to_bindings(val spec, val vars, val bindings)
{
  val iter;
  list_collect_decl (fixed_vars, tail);

  if (vars && !consp(vars)) 
    sem_error(spec, lit("not a valid variable list: ~a"), vars, nao);

  for (iter = vars; iter; iter = cdr(iter)) {
    val item = car(iter);
    if (bindable(item)) {
      list_collect (tail, cons(item, noval_s));
    } else if (consp(item) && bindable(first(item))) {
      list_collect (tail, cons(first(item), 
                               cdr(eval_form(spec, second(item), bindings))));
    } else { 
      sem_error(spec, lit("not a variable spec: ~a"), item, nao);
    }
  }
  return fixed_vars;
}

typedef struct {
  val bindings, specline, dataline, pos, data_lineno, file;
} match_line_ctx;

static match_line_ctx ml_all(val bindings, val specline, val dataline,
                             val pos, val data_lineno, val file)
{
  match_line_ctx c = { bindings, specline, dataline, 
                       pos, data_lineno, file };
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

static val match_line(match_line_ctx c);

typedef val (*h_match_func)(match_line_ctx c, match_line_ctx *cout);

#define LOG_MISMATCH(KIND)                                              \
  debuglf(elem, lit(KIND " mismatch, position ~a (~a:~a)"),             \
          c.pos, c.file, c.data_lineno, nao);                           \
  debuglf(elem, lit("  ~a"), c.dataline, nao);                          \
  if (c_num(c.pos) < 77)                                                \
    debuglf(elem, lit("  ~*~a^"), c.pos, lit(""), nao)

#define LOG_MATCH(KIND, EXTENT)                                         \
  debuglf(elem, lit(KIND " matched, position ~a-~a (~a:~a)"),           \
          c.pos, EXTENT, c.file, c.data_lineno, nao);                   \
  debuglf(elem, lit("  ~a"), c.dataline, nao);                          \
  if (c_num(EXTENT) < 77)                                               \
    debuglf(elem, lit("  ~*~a~-*~a^"), c.pos, lit(""),                  \
              minus(EXTENT, c.pos), lit("^"), nao)

#define elem_bind(elem_var, directive_var, specline)    \
  val elem_var = first(specline);                       \
  val directive_var = first(elem_var)

static val search_form(match_line_ctx *c, val needle_form, val from_end)
{
  if (regexp(first(needle_form))) {
    return search_regex(c->dataline, first(needle_form), c->pos, from_end);
  } else {
    val spec = cons(needle_form, nil);
    val pos = from_end ? length_str(c->dataline) : c->pos;
    val step = from_end ? num(-1) : num(1);

    rlcp(spec, needle_form);

    for (; (from_end && ge(pos, c->pos)) || length_str_gt(c->dataline, pos);
         pos = plus(pos, step))
    {
      cons_bind (new_bindings, new_pos,
                 match_line(ml_specline_pos(*c, spec, pos)));
      if (new_pos) {
        c->bindings = new_bindings;
        return cons(pos, minus(new_pos, pos));
      }
    }

    return nil;
  }
}

static val h_var(match_line_ctx c, match_line_ctx *cout)
{
  val elem = first(c.specline);
  val sym = second(elem);
  val pat = third(elem);
  val modifiers = fourth(elem);
  val modifier = first(modifiers);
  val pair = assoc(c.bindings, sym); /* var exists already? */

  if (gt(length(modifiers), one)) {
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
      val loc = source_loc(c.specline);
      c.specline = cons(cdr(pair), cons(pat, rest(c.specline)));
      rl(car(c.specline), loc);
    } else if (nump(modifier)) {
      val past = plus(c.pos, modifier);

      if (length_str_lt(c.dataline, past) || lt(past, c.pos))
      {
        LOG_MISMATCH("fixed field size");
        return nil;
      }

      if (!tree_find(trim_str(sub_str(c.dataline, c.pos, past)),
                     cdr(pair), equal_f))
      {
        LOG_MISMATCH("fixed field contents");
        return nil;
      }

      LOG_MATCH("fixed field", past);
      c.pos = past;
      c.specline = cdr(c.specline);
    } else {
      val loc = source_loc(c.specline);
      c.specline = cons(cdr(pair), rest(c.specline));
      rl(car(c.specline), loc);
    }
    goto repeat;
  } else if (consp(modifier)) { /* var bound over text matched by form */
    cons_bind (new_bindings, new_pos,
               match_line(ml_specline(c, modifiers)));

    if (!new_pos) {
      LOG_MISMATCH("var spanning form");
      return nil;
    }

    LOG_MATCH("var spanning form", new_pos);
    c.bindings = acons(new_bindings, sym, sub_str(c.dataline, c.pos, new_pos));
    c.pos = new_pos;
    /* This may have another variable attached */
    if (pat) {
      val loc = source_loc(c.specline);
      c.specline = cons(pat, rest(c.specline));
      rl(car(c.specline), loc);
      goto repeat;
    }
  } else if (nump(modifier)) { /* fixed field */
    val past = plus(c.pos, modifier);
    if (length_str_lt(c.dataline, past) || lt(past, c.pos))
    {
      LOG_MISMATCH("count based var");
      return nil;
    }
    LOG_MATCH("count based var", past);
    c.bindings = acons(c.bindings, sym, trim_str(sub_str(c.dataline, c.pos, past)));
    c.pos = past;
    /* This may have another variable attached */
    if (pat) {
      c.specline = cons(pat, rest(c.specline));
      goto repeat;
    }
  } else if (modifier && modifier != t) {
    sem_error(elem, lit("invalid modifier ~s on variable ~s"),
              modifier, sym, nao);
  } else if (pat == nil) { /* no modifier, no elem -> to end of line */
    c.bindings = acons(c.bindings, sym, sub_str(c.dataline, c.pos, nil));
    c.pos = length_str(c.dataline);
  } else if (type(pat) == STR) {
    val find = search_str(c.dataline, pat, c.pos, modifier);
    if (!find) {
      LOG_MISMATCH("var delimiting string");
      return nil;
    }
    LOG_MATCH("var delimiting string", find);
    c.bindings = acons(c.bindings, sym, sub_str(c.dataline, c.pos, find));
    c.pos = plus(find, length_str(pat));
  } else if (consp(pat) && first(pat) != var_s) {
    val find = search_form(&c, pat, modifier);
    val fpos = car(find);
    val flen = cdr(find);
    if (!find) {
      LOG_MISMATCH("var delimiting form");
      return nil;
    }
    LOG_MATCH("var delimiting form", fpos);
    c.bindings = acons(c.bindings, sym, sub_str(c.dataline, c.pos, fpos));
    c.pos = plus(fpos, flen);
  } else if (consp(pat)) {
    /* Unbound var followed by var: the following one must either
       be bound, or must specify a regex. */
    val second_sym = second(pat);
    val next_pat = third(pat);
    val next_modifiers = fourth(pat);
    val next_modifier = first(fourth(pat));
    val pair = assoc(c.bindings, second_sym); /* var exists already? */

    if (gt(length(next_modifiers), one)) {
      sem_error(elem, lit("multiple modifiers on variable ~s"),
                second_sym, nao);
    }

    if (!pair && consp(next_modifier)) {
      val find = search_form(&c, next_modifier, modifier);
      val fpos = car(find);
      val flen = cdr(find);

      if (!find) {
        LOG_MISMATCH("double var regex");
        return nil;
      }

      /* Text from here to start of regex match goes to this
         variable. */
      c.bindings = acons(c.bindings, sym, sub_str(c.dataline, c.pos, fpos));
      /* Text from start of regex match to end goes to the
         second variable */
      c.bindings = acons(c.bindings, second_sym, sub_str(c.dataline, fpos, plus(fpos, flen)));
      LOG_MATCH("double var regex (first var)", fpos);
      c.pos = fpos;
      LOG_MATCH("double var regex (second var)", plus(fpos, flen));
      c.pos = plus(fpos, flen);
      if (next_pat) {
        val loc = source_loc(c.specline);
        c.specline = cons(next_pat, rest(c.specline));
        rl(car(c.specline), loc);
        goto repeat;
      }
    } else if (!pair) {
      sem_error(elem, lit("consecutive unbound variables"), nao);
    } else {
    /* Re-generate a new spec with an edited version of
       the element we just processed, and repeat. */
      val new_elem = list(var_s, sym, cdr(pair), modifier, nao);

      if (next_pat)
         c.specline = cons(new_elem, cons(next_pat, rest(c.specline)));
      else
         c.specline = cons(new_elem, rest(c.specline));
      goto repeat;
    }
  } else if (consp(pat) && (consp(first(pat)) || stringp(first(pat)))) {
    cons_bind (find, len, search_str(c.dataline, pat, c.pos, modifier));
    if (!find) {
      LOG_MISMATCH("string");
      return nil;
    }
    c.bindings = acons(c.bindings, sym, sub_str(c.dataline, c.pos, find));
    c.pos = plus(find, len);
  } else {
    sem_error(elem, lit("variable followed by invalid element"), nao);
  }

  *cout = c;
  return next_spec_k;

repeat:
  *cout = c;
  return repeat_spec_k;
}

static val h_skip(match_line_ctx c, match_line_ctx *cout)
{
  val elem = first(c.specline);
  val max = second(elem);
  val min = third(elem);
  cnum cmax = nump(max) ? c_num(max) : 0;
  cnum cmin = nump(min) ? c_num(min) : 0;
  val greedy = eq(max, greedy_k);
  val last_good_result = nil, last_good_pos = nil;

  if (!rest(c.specline)) {
    debuglf(elem, 
            lit("skip to end of line ~a:~a"), c.file, c.data_lineno, nao);
    return cons(c.bindings, t);
  }

  {
    cnum reps_max = 0, reps_min = 0;

    while (length_str_gt(c.dataline, c.pos) && min && reps_min < cmin) {
      c.pos = plus(c.pos, one);
      reps_min++;
    }

    if (min) {
      if (reps_min != cmin) {
        debuglf(elem,
                lit("skipped only ~a/~a chars to ~a:~a:~a"),
                num(reps_min), num(cmin),
                c.file, c.data_lineno, c.pos, nao);
        return nil;
      }

      debuglf(elem, lit("skipped ~a chars to ~a:~a:~a"),
              num(reps_min), c.file, c.data_lineno, c.pos, nao);
    }

    while (greedy || !max || reps_max++ < cmax) {
      val result = match_line(ml_specline(c, rest(c.specline)));

      if (result) {
        if (greedy) {
          last_good_result = result;
          last_good_pos = c.pos;
        } else {
          LOG_MATCH("skip", c.pos);
          return result;
        }
      }

      if (length_str_le(c.dataline, c.pos))  {
        if (last_good_result) {
          LOG_MATCH("greedy skip", last_good_pos);
          return last_good_result;
        }
        break;
      }

      c.pos = plus(c.pos, one);
    }
  }

  LOG_MISMATCH("skip");
  return nil;
}

static val h_coll(match_line_ctx c, match_line_ctx *cout)
{
  val elem = first(c.specline);
  val coll_specline = second(elem);
  val until_last_specline = third(elem);
  val args = fourth(elem);
  val bindings_coll = nil;
  val last_bindings = nil;
  val max = getplist(args, maxgap_k);
  val min = getplist(args, mingap_k);
  val gap = getplist(args, gap_k);
  val times = getplist(args, times_k);
  val mintimes = getplist(args, mintimes_k);
  val maxtimes = getplist(args, maxtimes_k);
  val chars = getplist(args, chars_k);
  val vars = getplist(args, vars_k);
  cnum cmax = nump(gap) ? c_num(gap) : (nump(max) ? c_num(max) : 0);
  cnum cmin = nump(gap) ? c_num(gap) : (nump(min) ? c_num(min) : 0);
  cnum mincounter = cmin, maxcounter = 0;
  cnum ctimax = nump(times) ? c_num(times) 
                            : (nump(maxtimes) ? c_num(maxtimes) : 0);
  cnum ctimin = nump(times) ? c_num(times) 
                            : (nump(mintimes) ? c_num(mintimes) : 0);
  cnum cchars = nump(chars) ? c_num(chars) : 0;
  cnum timescounter = 0, charscounter = 0;
  val iter;

  vars = vars_to_bindings(elem, vars, c.bindings);

  if (((times || maxtimes) && ctimax == 0) || (chars && cchars == 0)) {
    *cout = c;
    return next_spec_k;
  }

  for (;;) {
    val new_bindings = nil, new_pos = nil;

    if ((gap || min) && mincounter < cmin)
      goto next_coll;

    if (chars && charscounter++ >= cchars)
      break;

    {
      cons_set (new_bindings, new_pos,
                match_line(ml_specline(c, coll_specline)));

      if (until_last_specline) {
        cons_bind (sym, spec, until_last_specline);
        cons_bind (until_last_bindings, until_pos,
                   match_line(ml_bindings_specline(c, new_bindings, spec)));

        if (until_pos) {
          LOG_MATCH("until/last", until_pos);
          if (sym == last_s) {
            last_bindings = set_diff(until_last_bindings,
                                     new_bindings, eq_f, nil);
            c.pos = until_pos;
          }
          break;
        } else {
          LOG_MISMATCH("until/last");
        }
      }

      if (new_pos) {
        val strictly_new_bindings = set_diff(new_bindings,
                                             c.bindings, eq_f, nil);
        LOG_MATCH("coll", new_pos);

        for (iter = vars; iter; iter = cdr(iter)) {
          cons_bind (var, dfl, car(iter));
          val exists = assoc(new_bindings, var);

          if (!exists) {
            if (dfl == noval_s)
              sem_error(elem, lit("coll failed to bind ~a"),
                        var, nao);
            else
              strictly_new_bindings = acons(strictly_new_bindings, 
                                            var, dfl);
          }
        }

        for (iter = strictly_new_bindings; iter; iter = cdr(iter))
        {
          val binding = car(iter);
          val vars_binding = assoc(vars, car(binding));

          if (!vars || vars_binding) {
            val existing = assoc(bindings_coll, car(binding));
            bindings_coll = acons_new(bindings_coll, car(binding),
                                      cons(cdr(binding), cdr(existing)));
          }
        }
      }

      if (new_pos && !equal(new_pos, c.pos)) {
        c.pos = new_pos;
        bug_unless (length_str_ge(c.dataline, c.pos));

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
        c.pos = plus(c.pos, one);
      }

      if (length_str_le(c.dataline, c.pos))
        break;
    }
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
    c.bindings = cons(rev, c.bindings);
  }

  if (last_bindings) {
    c.bindings = set_diff(c.bindings, last_bindings, eq_f, car_f);
    c.bindings = nappend2(last_bindings, c.bindings);
  }

  /* If nothing was collected, but vars were specified,
     then bind empty lists for the vars. */
  if (!bindings_coll && vars) {
    for (iter = vars; iter; iter = cdr(iter)) {
      val sym = car(car(iter));
      val exists = assoc(c.bindings, sym);
      if (!exists)
        c.bindings = acons(c.bindings, sym, nil);
    }
  }

  *cout = c;
  return next_spec_k;
}

static val h_parallel(match_line_ctx c, match_line_ctx *cout)
{
  elem_bind(elem, directive, c.specline);
  val specs = third(elem);
  val plist = fourth(elem);
  val all_match = t;
  val some_match = nil;
  val max_pos = c.pos;
  val choose_shortest = getplist(plist, shortest_k);
  val choose_longest = getplist(plist, longest_k);
  val choose_sym = or2(choose_longest, choose_shortest);
  val choose_bindings = c.bindings, choose_pos = c.pos;
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
      if (!assoc(c.bindings, var))
        push(var, &resolve_ub_vars);
    }
  }

  for (iter = specs; iter != nil; iter = cdr(iter)) {
    val nested_spec = first(iter);
    cons_bind (new_bindings, new_pos,
               match_line(ml_specline(c, nested_spec)));

    if (new_pos) {
      some_match = t;

      if (resolve_ub_vars) {
        val uiter;
        for (uiter = resolve_ub_vars; uiter; uiter = cdr(uiter)) {
          val ubvar = car(uiter);
          val exists = assoc(new_bindings, ubvar);

          if (exists)
            resolve_bindings = acons_new(resolve_bindings, ubvar, cdr(exists));
        }

        new_bindings = alist_remove(new_bindings, resolve_ub_vars);
      }

      if (gt(new_pos, max_pos))
        max_pos = new_pos;

      if (directive == choose_s) {
        val binding = choose_sym ? assoc(new_bindings, choose_sym) : nil;
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
        c.bindings = new_bindings;
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
    c.bindings = nappend2(resolve_bindings, c.bindings);

  if (directive == choose_s) {
    c.bindings = choose_bindings;
    c.pos = choose_pos;
  } else {
    c.pos = max_pos;
  }

  *cout = c;
  return next_spec_k;
}

static val h_trailer(match_line_ctx c, match_line_ctx *cout)
{
  val result = match_line(ml_specline(c, rest(c.specline)));
  val new_pos = cdr(result);
  val elem = first(c.specline);

  if (!new_pos) {
    LOG_MISMATCH("trailer");
    return nil;
  }

  LOG_MATCH("trailer", new_pos);
  return cons(c.bindings, c.pos);
}

static val h_fun(match_line_ctx c, match_line_ctx *cout)
{
  val elem = first(c.specline);
  val sym = first(elem);
  val func = cdr(uw_get_func(sym));

  if (func) {
    val args = rest(elem);
    val params = car(func);
    val ub_p_a_pairs = nil;
    val body = cdr(func);
    val piter, aiter;
    val bindings_cp = copy_list(c.bindings);

    if (!equal(length(args), length(params)))
      sem_error(elem, lit("function ~a takes ~a argument(s)"),
                sym, length(params), nao);

    for (piter = params, aiter = args; piter;
         piter = cdr(piter), aiter = cdr(aiter))
    {
      val param = car(piter);
      val arg = car(aiter);

      if (arg && bindable(arg)) {
        val val = assoc(c.bindings, arg);
        if (val) {
          bindings_cp = acons_new(bindings_cp,
                                  param,
                                  cdr(val));
        } else {
          bindings_cp = alist_nremove1(bindings_cp, param);
          ub_p_a_pairs = cons(cons(param, arg), ub_p_a_pairs);
        }
      } else {
        val val = eval_form(elem, arg, c.bindings);
        bindings_cp = acons_new(bindings_cp, param, cdr(val));
      }
    }

    {
      uw_block_begin(nil, result);
      uw_env_begin;
      debug_begin(sym, args, ub_p_a_pairs, c.bindings, c.dataline, c.data_lineno, c.pos);

      result = match_line(ml_bindings_specline(c, bindings_cp, body));

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
            val newbind = assoc(new_bindings, param);
            if (newbind) {
              c.bindings = dest_bind(elem, c.bindings, 
                                     arg, cdr(newbind), equal_f);
              if (c.bindings == t) {
                debuglf(elem,
                        lit("binding mismatch on ~a "
                            "when returning from ~a"), arg, sym, nao);
                return nil;
              }
            }
          }
        }

        if (nump(success))
          c.pos = success;
      }
    }

    *cout = c;
    return next_spec_k;
  }

  return decline_k;
}

static val h_eol(match_line_ctx c, match_line_ctx *cout)
{
  val elem = first(c.specline);

  if (length_str_le(c.dataline, c.pos)) {
    LOG_MATCH("eol", c.pos);
    return cons(c.bindings, t);
  }
  LOG_MISMATCH("eol");
  return nil;
}

typedef struct {
  val spec, files, bindings, data, data_lineno;
} match_files_ctx;

static match_files_ctx mf_all(val spec, val files, val bindings,
                              val data, val data_lineno);

static val v_fun(match_files_ctx *c);

static val match_line(match_line_ctx c)
{
  for (;;) {
    val elem;

    if (c.specline == nil)
      break;

    elem = first(c.specline);

    debug_check(elem, c.bindings, c.dataline, c.data_lineno, c.pos);

    switch (elem ? type(elem) : 0) {
    case CONS: /* directive */
      {
        val directive = first(elem);

        if (regexp(directive)) {
          val past = match_regex(c.dataline, directive, c.pos);
          if (nullp(past)) {
            LOG_MISMATCH("regex");
            return nil;
          }
          LOG_MATCH("regex", past);
          c.pos = past;
        } else if (consp(directive) || stringp(directive)) {
          cons_bind (find, len, search_str_tree(c.dataline, elem, c.pos, nil));
          val newpos;

          if (find == nil || !equal(find, c.pos)) {
            LOG_MISMATCH("string tree");
            return nil;
          }

          newpos = plus(find, len);
          LOG_MATCH("string tree", newpos);
          c.pos = newpos;
        } else {
          val entry = gethash(h_directive_table, directive);
          if (entry) {
            h_match_func hmf = (h_match_func) cptr_get(entry);
            match_line_ctx nc;
            val result = hmf(c, &nc);

            if (result == next_spec_k) {
              c = nc;
              break;
            } else if (result == repeat_spec_k) {
              c = nc;
              continue;
            } else {
              return result;
            }
          } else {
            match_line_ctx nc;
            val result = h_fun(c, &nc);

            if (result == next_spec_k) {
              c = nc;
              break;
            } else if (result == repeat_spec_k) {
              c = nc;
              continue;
            } else if (result == decline_k) {
              val spec = rlcp(cons(cons(elem, nil), nil), elem);
              match_files_ctx vc = mf_all(spec, nil, c.bindings, nil, num(0));
              val vresult = v_fun(&vc);

              if (vresult == next_spec_k) {
                c.bindings = vc.bindings;
                break;
              } else if (vresult == repeat_spec_k) {
                c.bindings = vc.bindings;
                continue;
              } else if (vresult == decline_k) {
                if (gethash(v_directive_table, directive))
                  sem_error(elem, lit("~a only exists as a vertical directive"),
                            directive, nao);
                else
                  sem_error(elem, lit("no such function or directive: ~a"),
                            directive, nao);
              } else {
                return vresult;
              }
            } else {
              return result;
            }
          }
        }
      }
      break;
    case STR:
      {
        val find = search_str(c.dataline, elem, c.pos, nil);
        val newpos;
        if (find == nil || !equal(find, c.pos)) {
          LOG_MISMATCH("string");
          return nil;
        }
        newpos = plus(find, length_str(elem));
        LOG_MATCH("string", newpos);
        c.pos = newpos;
        break;
      }
    default:
      sem_error(elem, lit("unsupported object in spec: ~s"), elem, nao);
    }

    c.specline = cdr(c.specline);
  }

  return cons(c.bindings, c.pos);
}

static val format_field(val string_or_list, val modifier, val filter)
{
  val n = zero;
  val plist = nil;

  if (!stringp(string_or_list))
    return string_or_list;

  for (; modifier; pop(&modifier)) {
    val item = first(modifier);
    if (nump(item))
      n = item;
    if (regexp(item))
      uw_throw(query_error_s, lit("format_field: regex modifier in output"));
    if (keywordp(item)) {
      plist = modifier;
      break;
    }
  }

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
      string_or_list = filter_string(filter, cat_str(list(string_or_list, nao),
                                                     nil));
  }

  {
    val right = lt(n, zero);
    val width = if3(lt(n, zero), neg(n), n);
    val diff = minus(width, length_str(string_or_list));

    if (le(diff, zero))
      return string_or_list;

    if (ge(length_str(string_or_list), width))
      return string_or_list;

    {
      val padding = mkstring(diff, chr(' '));

      return if3(right,
                 cat_str(list(padding, string_or_list, nao), nil),
                 cat_str(list(string_or_list, padding, nao), nil));
    }
  }
}

static val subst_vars(val spec, val bindings, val filter)
{
  list_collect_decl(out, iter);

  while (spec) {
    val elem = first(spec);

    if (consp(elem)) {
      if (first(elem) == var_s) {
        val sym = second(elem);
        val pat = third(elem);
        val modifiers = fourth(elem);
        val pair = assoc(bindings, sym);

        if (pair) {
          if (pat)
            spec = cons(filter_string(filter, cdr(pair)), cons(pat, rest(spec)));
          else if (modifiers)
            spec = cons(format_field(cdr(pair), modifiers, filter), rest(spec));
          else
            spec = cons(filter_string(filter, cdr(pair)), rest(spec));
          continue;
        }
        uw_throwf(query_error_s, lit("unbound variable ~a"),
                                 sym, nao);
      } else if (first(elem) == quasi_s) {
        val nested = subst_vars(rest(elem), bindings, filter);
        list_collect_append(iter, nested);
        spec = cdr(spec);
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

static val eval_form(val spec, val form, val bindings)
{
  val ret = nil;

  uw_catch_begin (cons(query_error_s, nil), exc_sym, exc);
  {
    if (!form) {
      ret = cons(t, form);
    } else if (bindable(form)) {
      ret = assoc(bindings, form);
    } else if (consp(form)) {
      if (first(form) == quasi_s) {
        ret = cons(t, cat_str(subst_vars(rest(form), bindings, nil), nil));
      } else if (regexp(car(form))) {
        ret = cons(t, form);
      } else if (first(form) == var_s) {
        sem_error(spec, lit("metavariable @~a syntax cannot be used here"),
                  second(form), nao);
      } else if (first(form) == expr_s) {
        sem_error(spec, lit("the @~s syntax cannot be used here"),
                  rest(form), nao);
      } else {
        val subforms = mapcar(curry_123_2(func_n3(eval_form), 
                                          spec, bindings), form);

        if (all_satisfy(subforms, identity_f, nil))
          ret = cons(t, mapcar(func_n1(cdr), subforms));
      }
    } else if (stringp(form)) {
      ret = cons(t, form);
    } else {
      ret = cons(t, form);
    }

    uw_catch (exc_sym, exc) {
      if (stringp(exc) && !equal(exc, lit("")) && 
          chr_str(exc, zero) == chr('('))
      {
        uw_throw (exc_sym, exc);
      }

      sem_error(spec, lit("~a"), exc, nao);
    }
  }
  uw_catch_end;

  if (!ret)
    sem_error(spec, lit("unbound variable in form ~s"), form, nao);

  return ret;
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
    output_produced = output ? 1 : 0;
  } else if (namestr[0] == '!') {
    ret.close = fpip_pclose;
    ret.f = w_popen(namestr+1, output ? L"w" : L"r");
  } else if (namestr[0] == '$') {
    char *name;
    if (output)
      return ret;
    name = (char *) utf8_dup_to(namestr+1);
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
    return lazy_stream_cons(make_stdio_stream(fp.f, name, t, nil));
  case fpip_pclose:
    return lazy_stream_cons(make_pipe_stream(fp.f, name, t, nil));
  case fpip_closedir:
    return lazy_stream_cons(make_dir_stream(fp.d));
  }

  internal_error("bad input source type");
}

static val complex_stream(fpip_t fp, val name)
{
  switch (fp.close) {
  case fpip_fclose:
    return make_stdio_stream(fp.f, name, t, nil);
  case fpip_pclose:
    return make_pipe_stream(fp.f, name, t, nil);
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
  return length(obj);
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
    if (first(output_spec) == var_s) {
      list_collect (tai, second(output_spec));
    } else {
      for (; output_spec; output_spec = cdr(output_spec))
        list_collect_nconc(tai, extract_vars(car(output_spec)));
    }
  }

  return vars;
}

static val extract_bindings(val bindings, val output_spec)
{
  list_collect_decl (bindings_out, tail);
  val var_list = extract_vars(output_spec);

  for (; bindings; bindings = cdr(bindings))
    if (memq(car(car(bindings)), var_list))
      list_collect(tail, car(bindings));

  return bindings_out;
}

static void do_output_line(val bindings, val specline, val filter, val out)
{
  for (; specline; specline = rest(specline)) {
    val elem = first(specline);

    switch (elem ? type(elem) : 0) {
    case CONS:
      {
        val directive = first(elem);

        if (directive == var_s) {
          val str = cat_str(subst_vars(cons(elem, nil),
                                       bindings, filter), nil);
          if (str == nil)
            sem_error(specline, lit("bad substitution: ~a"),
                      second(elem), nao);
          put_string(out, str);
        } else if (directive == rep_s) {
          val main_clauses = second(elem);
          val single_clauses = third(elem);
          val first_clauses = fourth(elem);
          val last_clauses = fifth(elem);
          val empty_clauses = sixth(elem);
          val bind_cp = extract_bindings(bindings, elem);
          val max_depth = reduce_left(func_n2(max2),
                                      bind_cp, zero,
                                      chain(func_n1(cdr),
                                            func_n1(robust_length),
                                            nao));

          if (equal(max_depth, zero) && empty_clauses) {
            do_output_line(bindings, empty_clauses, filter, out);
          } else if (equal(max_depth, one) && single_clauses) {
            val bind_a = mapcar(func_n1(bind_car), bind_cp);
            do_output_line(bind_a, single_clauses, filter, out);
          } else if (!zerop(max_depth)) {
            cnum i;

            for (i = 0; i < c_num(max_depth); i++) {
              val bind_a = mapcar(func_n1(bind_car), bind_cp);
              val bind_d = mapcar(func_n1(bind_cdr), bind_cp);

              if (i == 0 && first_clauses) {
                do_output_line(bind_a, first_clauses, filter, out);
              } else if (i == c_num(max_depth) - 1 && last_clauses) {
                do_output_line(bind_a, last_clauses, filter, out);
              } else {
                do_output_line(bind_a, main_clauses, filter, out);
              }

              bind_cp = bind_d;
            }
          }

        } else {
          sem_error(specline, lit("unknown directive: ~a"), directive, nao);
        }
      }
      break;
    case STR:
      put_string(out, elem);
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
  if (equal(specs, null_list))
    return;

  for (; specs; specs = cdr(specs)) {
    val specline = first(specs);
    val first_elem = first(specline);

    if (consp(first_elem)) {
      val sym = first(first_elem);

      if (sym == repeat_s) {
        val main_clauses = second(first_elem);
        val single_clauses = third(first_elem);
        val first_clauses = fourth(first_elem);
        val last_clauses = fifth(first_elem);
        val empty_clauses = sixth(first_elem);
        val bind_cp = extract_bindings(bindings, first_elem);
        val max_depth = reduce_left(func_n2(max2),
                                    bind_cp, zero,
                                    chain(func_n1(cdr),
                                          func_n1(robust_length),
                                          nao));

        if (equal(max_depth, zero) && empty_clauses) {
          do_output(bind_cp, empty_clauses, filter, out);
        } else if (equal(max_depth, one) && single_clauses) {
          val bind_a = mapcar(func_n1(bind_car), bind_cp);
          do_output(bind_a, single_clauses, filter, out);
        } else if (!zerop(max_depth)) {
          cnum i;

          for (i = 0; i < c_num(max_depth); i++) {
            val bind_a = mapcar(func_n1(bind_car), bind_cp);
            val bind_d = mapcar(func_n1(bind_cdr), bind_cp);

            if (i == 0 && first_clauses) {
              do_output(bind_a, first_clauses, filter, out);
            } else if (i == c_num(max_depth) - 1 && last_clauses) {
              do_output(bind_a, last_clauses, filter, out);
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
    put_char(out, chr('\n'));
  }
}

static match_files_ctx mf_all(val spec, val files, val bindings,
                              val data, val data_lineno)
{
  match_files_ctx c = { spec, files, bindings, data, data_lineno };
  return c;
}

static match_files_ctx mf_args(match_files_ctx c)
{
  match_files_ctx nc = c;
  nc.files = cons(string(L"args"), c.files);
  nc.data = c.files;
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
  nc.data = data;
  nc.data_lineno = data_lineno;
  return nc;
}

static match_files_ctx mf_from_ml(match_line_ctx ml)
{
  return mf_all(cons(ml.specline, nil), nil, ml.bindings, nil, num(0));
}

static val match_files(match_files_ctx a);

typedef val (*v_match_func)(match_files_ctx *cout);

#define spec_bind(specline, first_spec, spec)           \
  val specline = first(spec);                           \
  val first_spec = first(specline);

static val v_skip(match_files_ctx *c)
{
  val specline = rest(first(c->spec));

  if (rest(specline))
    return decline_k;

  c->spec = rest(c->spec);

  if (!c->spec)
    return cons(c->bindings, cons(c->data, c->data_lineno));

  {
    val skipspec = first(first(c->spec));
    val first_spec = first(specline);
    val args = rest(first_spec);
    val max = first(args);
    val min = second(args);
    cnum cmax = nump(max) ? c_num(max) : 0;
    cnum cmin = nump(min) ? c_num(min) : 0;
    val greedy = eq(max, greedy_k);
    val last_good_result = nil;
    val last_good_line = num(0);

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
                  first(c->files), c->data_lineno, nao);
          uw_block_return(nil, nil);
        }

        debuglf(skipspec, lit("skipped ~a lines to ~a:~a"),
                num(reps_min), first(c->files),
                c->data_lineno, nao);
      }

      while (greedy || !max || reps_max++ < cmax) {
        result = match_files(*c);

        if (result) {
          if (greedy) {
            last_good_result = result;
            last_good_line = c->data_lineno;
          } else {
            debuglf(skipspec, lit("skip matched ~a:~a"), first(c->files),
                    c->data_lineno, nao);
            break;
          }
        } else {
          debuglf(skipspec, lit("skip didn't match ~a:~a"),
                  first(c->files), c->data_lineno, nao);
        }

        if (!c->data)
          break;

        debuglf(skipspec, lit("skip didn't match ~a:~a"), first(c->files),
                c->data_lineno, nao);

        c->data = rest(c->data);
        c->data_lineno = plus(c->data_lineno, num(1));
      }

      uw_block_end;

      if (result)
        return result;
      if (last_good_result) {
        debuglf(skipspec, lit("greedy skip matched ~a:~a"), 
                first(c->files), last_good_line, nao);
        return last_good_result;
      }
    }

    debuglf(skipspec, lit("skip failed"), nao);
    return nil;
  }
}

static val v_trailer(match_files_ctx *c)
{
  if (rest(rest(first(c->spec))))
    return decline_k;

  c->spec = rest(c->spec);

  if (!c->spec)  {
    return cons(c->bindings, cons(c->data, c->data_lineno));
  } else {
    cons_bind (new_bindings, success, match_files(*c));
    return success ? cons(new_bindings, cons(c->data, c->data_lineno)) : nil;
  }
}

static val v_freeform(match_files_ctx *c)
{
  spec_bind (specline, first_spec, c->spec);

  val args = rest(first_spec);
  val vals = mapcar(func_n1(cdr),
                       mapcar(curry_123_2(func_n3(eval_form),
                                          first_spec, c->bindings), args));

  if ((c->spec = rest(c->spec)) == nil) {
    sem_error(first_spec,
              lit("freeform must be followed by a query line"), nao);
  } else if (!c->data) {
    debuglf(specline, lit("freeform match failure: no data"), nao);
    return nil;
  } else {
    val limit = or2(if2(nump(first(vals)), first(vals)),
                    if2(nump(second(vals)), second(vals)));
    val term = or2(if2(stringp(first(vals)), first(vals)),
                   if2(stringp(second(vals)), second(vals)));
    val ff_specline = first(c->spec);
    val ff_dataline = lazy_str(c->data, term, limit);

    cons_bind (new_bindings, success,
               match_line(ml_all(c->bindings, ff_specline, ff_dataline, zero,
                                 c->data_lineno, first(c->files))));

    if (!success) {
      debuglf(specline, lit("freeform match failure"), nao);
      return nil;
    }

    if (nump(success)) {
      c->data = lazy_str_get_trailing_list(ff_dataline, success);
      c->data_lineno = plus(c->data_lineno, num(1));
    }

    c->bindings = new_bindings;
  }

  return next_spec_k;
}

static val v_block(match_files_ctx *c)
{
  spec_bind (specline, first_spec, c->spec);

  val name = first(rest(first_spec));

  if (rest(specline))
    sem_error(specline, lit("unexpected material after block directive"), nao);

  if ((c->spec = rest(c->spec)) != nil)
  {
    uw_block_begin(name, result);
    result = match_files(*c);
    uw_block_end;
    return result;
  }

  return next_spec_k;
}

static val v_accept_fail(match_files_ctx *c)
{
  spec_bind (specline, first_spec, c->spec);
  val sym = first(first_spec);
  val target = first(rest(first_spec));

  if (rest(specline))
    sem_error(specline, lit("unexpected material after ~a"), sym, nao);

  uw_block_return(target,
                  if2(sym == accept_s,
                      cons(c->bindings,
                           if3(c->data, cons(c->data, c->data_lineno), t))));
  /* TODO: uw_block_return could just throw this */
  if (target)
    sem_error(specline, lit("~a: no block named ~a in scope"),
              sym, target, nao);
  else
    sem_error(specline, lit("%~a: no anonymous block in scope"),
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
      val from_var = cdr(assoc(alist, var_k));
      val list_expr = cdr(assoc(alist, list_k));
      val string_expr = cdr(assoc(alist, string_k));
      val nothrow = cdr(assoc(alist, nothrow_k));
      val eval = eval_form(specline, source, c->bindings);
      val str = cdr(eval);

      if (!from_var && !source && !string_expr && !list_expr)
        sem_error(specline, lit("next: source required before keyword arguments"), nao);

      if ((from_var && string_expr) || (string_expr && list_expr) ||
          (from_var && list_expr))
      {
        sem_error(specline, lit("next: only one of :var, :list or :string can be specified"), nao);
      }

      if (from_var) {
        val existing = assoc(c->bindings, from_var);

        if (!symbolp(from_var)) 
          sem_error(specline, lit(":var requires a variable, not ~s"), from_var, nao);

        if (!existing)
          sem_error(specline, lit(":var specifies unbound variable ~s"), from_var, nao);

        {
          cons_bind (new_bindings, success,
                     match_files(mf_file_data(*c, lit("var"),
                                 flatten(cdr(existing)), num(1))));

          if (success)
            return cons(new_bindings,
                        if3(c->data, cons(c->data, c->data_lineno), t));
          return nil;
        }
      } else if (list_expr) {
        val list_val = cdr(eval_form(specline, list_expr, c->bindings));
        cons_bind (new_bindings, success,
                   match_files(mf_file_data(*c, lit("var"),
                               flatten(list_val), num(1))));

        if (success)
          return cons(new_bindings,
                      if3(c->data, cons(c->data, c->data_lineno), t));
        return nil;
      } else if (string_expr) {
        val str_val = cdr(eval_form(specline, string_expr, c->bindings));
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
        if (!assoc(c->bindings, var))
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
            val exists = assoc(new_bindings, ubvar);

            if (exists)
              resolve_bindings = acons_new(resolve_bindings, ubvar, cdr(exists));
          }

          new_bindings = alist_remove(new_bindings, resolve_ub_vars);
        }

        if (sym == choose_s) {
          val binding = choose_sym ? assoc(new_bindings, choose_sym) : nil;
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
  val vars = vars_to_bindings(specline, getplist(args, vars_k), c->bindings);

  while (specs && c->data) {
    list_collect_decl (new_specs, ptail);
    val max_line = zero;
    val max_data = nil;
    val iter, next;

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
        c->bindings = new_bindings;
        cons_bind (new_data, new_line, success);
        if (gt(new_line, max_line)) {
          max_line = new_line;
          max_data = new_data;
        }
      }
    }

    list_collect_terminate (ptail, nil);
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
      if (!assoc(c->bindings, var)) {
        if (dfl_val == noval_s) {
          debuglf(specline, lit("gather failed to match some required vars"), nao);
          return nil;
        } else {
          c->bindings = acons(c->bindings, var, dfl_val);
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
  val coll_spec = second(first_spec);
  val until_last_spec = third(first_spec);
  val args = fourth(first_spec);
  val bindings_coll = nil;
  val last_bindings = nil;
  val max = getplist(args, maxgap_k);
  val min = getplist(args, mingap_k);
  val gap = getplist(args, gap_k);
  val times = getplist(args, times_k);
  val mintimes = getplist(args, mintimes_k);
  val maxtimes = getplist(args, maxtimes_k);
  val lines = getplist(args, lines_k);
  val vars = getplist(args, vars_k);
  cnum cmax = nump(gap) ? c_num(gap) : (nump(max) ? c_num(max) : 0);
  cnum cmin = nump(gap) ? c_num(gap) : (nump(min) ? c_num(min) : 0);
  cnum mincounter = cmin, maxcounter = 0;
  cnum ctimax = nump(times) ? c_num(times) 
                            : (nump(maxtimes) ? c_num(maxtimes) : 0);
  cnum ctimin = nump(times) ? c_num(times) 
                            : (nump(mintimes) ? c_num(mintimes) : 0);
  cnum timescounter = 0, linescounter = 0;
  cnum ctimes = nump(times) ? c_num(times) : 0;
  cnum clines = nump(lines) ? c_num(lines) : 0;
  val iter;

  if (gap && (max || min))
    sem_error(specline, lit("collect: cannot mix :gap with :mingap or :maxgap"), nao);

  vars = vars_to_bindings(specline, vars, c->bindings);

  if ((times && ctimes == 0) || (lines && clines == 0))
    return next_spec_k;

  uw_block_begin(nil, result);

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
                  first(c->files), c->data_lineno, nao);
          /* Until discards bindings and position, last keeps them. */
          if (sym == last_s) {
            last_bindings = set_diff(until_last_bindings,
                                     new_bindings, eq_f, nil);
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

      if (success) {
        val strictly_new_bindings = set_diff(new_bindings,
                                             c->bindings, eq_f, nil);

        debuglf(specline, lit("collect matched ~a:~a"),
                first(c->files), c->data_lineno, nao);

        for (iter = vars; iter; iter = cdr(iter)) {
          cons_bind (var, dfl, car(iter));
          val exists = assoc(new_bindings, var);

          if (!exists) {
            if (dfl == noval_s) 
              sem_error(specline, lit("collect failed to bind ~a"),
                        var, nao);
            else
              strictly_new_bindings = acons(strictly_new_bindings, 
                                            var, dfl);
          }
        }

        for (iter = strictly_new_bindings; iter; iter = cdr(iter))
        {
          val binding = car(iter);
          val vars_binding = assoc(vars, car(binding));

          if (!vars || vars_binding) {
            val existing = assoc(bindings_coll, car(binding));

            bindings_coll = acons_new(bindings_coll, car(binding),
                                      cons(cdr(binding), cdr(existing)));
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
      val exists = assoc(c->bindings, sym);
      if (!exists)
        c->bindings = acons(c->bindings, sym, nil);
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
      val existing = assoc(c->bindings, sym);

      if (existing)
        *cdr_l(existing) = flatten(cdr(existing));
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
      val arg_eval = eval_form(specline, arg, c->bindings);

      if (merged)
        merged = weird_merge(merged, cdr(arg_eval));
      else
        merged = cdr(arg_eval);
    }
  }

  c->bindings = acons_new(c->bindings, target, merged);

  return next_spec_k;
}

static val v_bind(match_files_ctx *c)
{
  spec_bind (specline, first_spec, c->spec);
  val args = rest(first_spec);
  val pattern = first(args);
  val form = second(args);
  val keywords = rest(rest(args));
  val value = eval_form(specline, form, c->bindings);
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

  c->bindings = dest_bind(specline, c->bindings, pattern,
                         cdr(value), testfun);

  uw_env_end;

  if (c->bindings == t)
    return nil;

  return next_spec_k;
}

static val hv_trampoline(match_line_ctx c, match_line_ctx *cout)
{
  val ret;
  match_files_ctx mf = mf_from_ml(c);
  val sym = first(first(c.specline));
  val entry = gethash(v_directive_table, sym);

  if (!entry)
    internal_error("hv_trampoline: missing dispatch table entry");

  {
    v_match_func vmf = (v_match_func) cptr_get(entry);
    ret = vmf(&mf);
    if (ret == next_spec_k) {
      c.bindings = mf.bindings;
      *cout = c;
    }
    return ret;
  }
}

static val v_set(match_files_ctx *c)
{
  spec_bind (specline, first_spec, c->spec);
  val args = rest(first_spec);
  val pattern = first(args);
  val form = second(args);
  val val = eval_form(specline, form, c->bindings);

  dest_set(specline, c->bindings, pattern, cdr(val));

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
    val existing = assoc(c->bindings, sym);
    if (existing) {
      val sep = if3(sep_form, 
                    cdr(eval_form(specline, sep_form, c->bindings)),
                    lit(" "));
      *cdr_l(existing) = cat_str(flatten(cdr(existing)), sep);
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
  val alist;
  fpip_t fp;

  if (eq(first(dest_spec), nothrow_k)) {
    if (rest(dest_spec))
      sem_error(specline, lit("material after :nothrow in output"), nao);
  } else if (!keywordp(first(dest_spec))) {
    val form = first(dest_spec);
    val val = eval_form(specline, form, c->bindings);
    dest = or2(cdr(val), dest);
    pop(&dest_spec);
  }

  alist = improper_plist_to_alist(dest_spec, list(nothrow_k, append_k, nao));

  nothrow = cdr(assoc(alist, nothrow_k));
  append = cdr(assoc(alist, append_k));

  {
    val filter_sym = cdr(assoc(alist, filter_k));

    if (filter_sym) {
      filter = get_filter(filter_sym);

      if (!filter)
        sem_error(specline, lit("~s specifies unknown filter"), filter_sym, nao);
    }
  }

  {
    val into_var = cdr(assoc(alist, into_k));

    if (into_var) {
      val stream = make_strlist_output_stream();

      if (!symbolp(into_var)) 
        sem_error(specline, lit(":into requires a variable, not ~s"), into_var, nao);

      debuglf(specline, lit("opening string list stream"), nao);
      uw_env_begin;
      uw_set_match_context(cons(c->spec, c->bindings));
      do_output(c->bindings, specs, filter, stream);
      uw_env_end;

      {
        val existing = assoc(c->bindings, into_var);
        val list_out = get_list_from_stream(stream);

        if (existing) {
          if (append) {
            *cdr_l(existing) = append2(flatten(cdr(existing)), list_out);
          } else {
            *cdr_l(existing) = list_out;
          }
        } else {
          c->bindings = acons(c->bindings, into_var, list_out);
        }
      }
      return next_spec_k;
    }
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
    uw_env_end;
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
    uw_do_unwind;

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
                         cons(cons(t, exvals), nil));

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

                if (value) {
                  c->bindings = dest_bind(specline, c->bindings, 
                                         param, cdr(value), equal_f);

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
      uw_do_unwind;
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

static val h_define(match_line_ctx c, match_line_ctx *cout)
{
  val elem = first(c.specline);
  val body = third(elem);
  val args = fourth(elem);
  val name = first(args);
  val params = second(args);
  val existing = uw_get_func(name);
  uw_set_func(name, cons(car(existing), cons(params, body)));
  *cout = c;
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
    val values = mapcar(curry_123_2(func_n3(eval_form), 
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

  if (!all_satisfy(table, func_n1(listp), nil))
    sem_error(specline, 
              lit("deffilter arguments must be lists"),
              nao);

  {
    val table_evaled = mapcar(curry_12_2(func_n2(mapcar), 
                                         chain(curry_123_2(func_n3(eval_form),
                                                           specline, c->bindings),
                                              cdr_f,
                                              nao)),
                              table);

    if (!all_satisfy(table_evaled, andf(func_n1(listp), 
                                        chain(func_n1(length),
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
    val existing = assoc(c->bindings, var);

    if (!bindable(var))
      sem_error(specline, lit("filter: ~a is not a variable name"), 
                var, nao);

    if (!existing)
      sem_error(specline, lit("filter: variable ~a is unbound"), var, nao);

    *cdr_l(existing) = filter_string(filter, cdr(existing));
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

    debug_check(specline, c->bindings, if2(consp(c->data), car(c->data)), c->data_lineno, nil);

    if (!equal(length(args), length(params)))
      sem_error(specline, lit("function ~a takes ~a argument(s)"),
                sym, length(params), nao);

    for (piter = params, aiter = args; piter;
         piter = cdr(piter), aiter = cdr(aiter))
    {
      val param = car(piter);
      val arg = car(aiter);

      if (arg && bindable(arg)) {
        val val = assoc(c->bindings, arg);
        if (val) {
          bindings_cp = acons_new(bindings_cp,
                                  param,
                                  cdr(val));
        } else {
          bindings_cp = alist_nremove1(bindings_cp, param);
          ub_p_a_pairs = cons(cons(param, arg), ub_p_a_pairs);
        }
      } else {
        val val = eval_form(specline, arg, c->bindings);
        bindings_cp = acons_new(bindings_cp, param, cdr(val));
      }
    }

    {
      uw_block_begin(nil, result);
      uw_env_begin;
      debug_begin(sym, args, ub_p_a_pairs, c->bindings, if2(consp(c->data), car(c->data)),
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
            val newbind = assoc(new_bindings, param);
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

static val match_files(match_files_ctx c)
{
  gc_hint(c.data);

  if (listp(c.data)) { /* recursive call with lazy list */
    ; /* no specia initialization */
  } else if (c.files) { /* c.data == t: toplevel call with file list */
    val source_spec = first(c.files);
    val name = consp(source_spec) ? cdr(source_spec) : source_spec;
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
          return nil;
        } else if (errno != 0)
          file_err(spec, lit("could not open ~a (error ~a/~a)"), name,
                   num(errno), string_utf8(strerror(errno)), nao);
        else
          file_err(spec, lit("could not open ~a"), name, nao);
        return nil;
      }

      c.files = cons(name, cdr(c.files)); /* Get rid of cons and nothrow */

      if ((c.data = complex_snarf(fp, name)) != nil)
        c.data_lineno = num(1);
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

    if (consp(first_spec) && !rest(specline)) {
      val sym = first(first_spec);
      val entry = gethash(v_directive_table, sym);

      if (entry) {
        v_match_func vmf = (v_match_func) cptr_get(entry);
        val result;

        debug_check(first_spec, c.bindings, if2(consp(c.data), car(c.data)), c.data_lineno, nil);

        result = vmf(&c);

        if (result == next_spec_k) {
          if ((c.spec = rest(c.spec)) == nil)
            break;
          goto repeat_spec_same_data;
        } else if (result == decline_k) {
          /* go on to other processing below */
        } else {
          return result;
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
          return result;
        }
      }
    }

    if (c.data)
    {
      val dataline = first(c.data);
      cons_bind (new_bindings, success,
                 match_line(ml_all(c.bindings, specline, dataline, zero,
                                   c.data_lineno, first(c.files))));

      if (nump(success) && c_num(success) < c_num(length_str(dataline))) {
        debuglf(specline, lit("spec only matches line to position ~a: ~a"),
                success, dataline, nao);
        return nil;
      }

      if (!success)
        return nil;

      c.bindings = new_bindings;
    } else {
      debuglf(specline, lit("spec ran out of data"), nao);
      return nil;
    }
  }

  return cons(c.bindings, if3(c.data, cons(c.data, c.data_lineno), t));
}

val match_funcall(val name, val arg, val other_args)
{
  cons_bind (in_spec, in_bindings, uw_get_match_context());
  spec_bind (specline, first_spec, in_spec);
  val in_arg_sym = make_sym(lit("in_arg"));
  val out_arg_sym = make_sym(lit("out_arg"));
  val bindings = cons(cons(in_arg_sym, arg), in_bindings);
  val spec = cons(list(cons(name, 
                            cons(in_arg_sym, cons(out_arg_sym, other_args))),
                       nao), nil);
  match_files_ctx c = mf_all(spec, nil, bindings, nil, num(0));
  (void) first_spec;

  val ret = v_fun(&c);

  if (ret == nil)
    sem_error(specline, lit("filter: (~s ~s ~s) failed"), name,
              arg, out_arg_sym, nao);

  if (ret == decline_k)
    sem_error(specline, lit("filter: function ~s not found"), name, nao);

  {
    val out = assoc(c.bindings, out_arg_sym);
    if (!out)
      sem_error(specline,
                lit("filter: (~s ~s ~s) did not bind ~s"), name,
                arg, out_arg_sym, out_arg_sym, nao);
    return cdr(out);
  }
}

int extract(val spec, val files, val predefined_bindings)
{
  cons_bind (bindings, success, match_files(mf_all(spec, files, 
                                                   predefined_bindings,
                                                   t, nil)));

  if (!output_produced) {
    if (!opt_nobindings) {
      if (bindings) {
        bindings = nreverse(bindings);
        dump_bindings(bindings);
      }
    }

    if (!success)
      put_line(std_output, lit("false"));
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
  choose_s = intern(lit("choose"), user_package);
  gather_s = intern(lit("gather"), user_package);
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

  filter_s = intern(lit("filter"), user_package);
  noval_s = intern(lit("noval"), system_package);
}

static void dir_tables_init(void)
{
  h_directive_table = make_hash(nil, nil, nil);
  v_directive_table = make_hash(nil, nil, nil);

  protect(&h_directive_table, &v_directive_table, (val *) 0);

  sethash(v_directive_table, skip_s, cptr((mem_t *) v_skip));
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
  sethash(v_directive_table, flatten_s, cptr((mem_t *) v_flatten));
  sethash(v_directive_table, forget_s, cptr((mem_t *) v_forget_local));
  sethash(v_directive_table, local_s, cptr((mem_t *) v_forget_local));
  sethash(v_directive_table, merge_s, cptr((mem_t *) v_merge));
  sethash(v_directive_table, bind_s, cptr((mem_t *) v_bind));
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

  sethash(h_directive_table, var_s, cptr((mem_t *) h_var));
  sethash(h_directive_table, skip_s, cptr((mem_t *) h_skip));
  sethash(h_directive_table, coll_s, cptr((mem_t *) h_coll));
  sethash(h_directive_table, flatten_s, cptr((mem_t *) hv_trampoline));
  sethash(h_directive_table, forget_s, cptr((mem_t *) hv_trampoline));
  sethash(h_directive_table, local_s, cptr((mem_t *) hv_trampoline));
  sethash(h_directive_table, merge_s, cptr((mem_t *) hv_trampoline));
  sethash(h_directive_table, bind_s, cptr((mem_t *) hv_trampoline));
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
}

void match_init(void)
{
  syms_init();
  dir_tables_init();
}
