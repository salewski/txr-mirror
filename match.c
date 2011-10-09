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
#include <assert.h>
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
#include "match.h"

int output_produced;

val mingap_k, maxgap_k, gap_k, mintimes_k, maxtimes_k, times_k;
val lines_k, chars_k;
val choose_s, longest_k, shortest_k, greedy_k;
val vars_k;

static void debugf(val fmt, ...)
{
  if (opt_loglevel >= 2) {
    va_list vl;
    va_start (vl, fmt);
    format(std_error, lit("~a: "), prog_string, nao);
    vformat(std_error, fmt, vl);
    put_char(std_error, chr('\n'));
    va_end (vl);
  }
}

static void debuglf(val line, val fmt, ...)
{
  if (opt_loglevel >= 2) {
    va_list vl;
    va_start (vl, fmt);
    format(std_error, lit("~a: (~a:~a) "), prog_string,
           spec_file_str, line, nao);
    vformat(std_error, fmt, vl);
    put_char(std_error, chr('\n'));
    va_end (vl);
  }
}

static void sem_error(val line, val fmt, ...)
{
  va_list vl;
  val stream = make_string_output_stream();

  va_start (vl, fmt);
  if (line)
    format(stream, lit("(~a:~a) "), spec_file_str, line, nao);
  (void) vformat(stream, fmt, vl);
  va_end (vl);

  uw_throw(query_error_s, get_string_from_stream(stream));
  abort();
}

static void file_err(val line, val fmt, ...)
{
  va_list vl;
  val stream = make_string_output_stream();

  va_start (vl, fmt);
  if (line)
    format(stream, lit("(~a:~a) "), spec_file_str, line, nao);
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
  if (opt_loglevel >= 2) {
    put_line(std_error, lit("raw_bindings:"));
    dump(bindings, std_error);
  }

  while (bindings) {
    char pfx1[128], pfx2[128];
    val var = car(car(bindings));
    val value = cdr(car(bindings));
    *pfx1 = 0; *pfx2 = 0;
    dump_var(var, pfx1, 0, pfx2, 0, value, 0);
    bindings = cdr(bindings);
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

static val dest_bind(val linenum, val bindings, val pattern, val value)
{
  if (symbolp(pattern)) {
    if (bindable(pattern)) {
      val existing = assoc(bindings, pattern);
      if (existing) {
        if (tree_find(value, cdr(existing)))
          return bindings;
        if (tree_find(cdr(existing), value))
          return bindings;
        debugf(lit("bind variable mismatch: ~a"), pattern, nao);
        return t;
      }
      return cons(cons(pattern, value), bindings);
    } else {
      return equal(pattern, value) ? bindings : t;
    }
  } else if (consp(pattern)) {
    val piter = pattern, viter = value;

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


    while (consp(piter) && consp(viter))
    {
      bindings = dest_bind(linenum, bindings, car(piter), car(viter));
      if (bindings == t)
        return t;
      piter = cdr(piter);
      viter = cdr(viter);
    }

    if (bindable(piter)) {
      bindings = dest_bind(linenum, bindings, piter, viter);
      if (bindings == t)
        return t;
    } else {
      return equal(piter, viter) ? bindings : t;
    }
    return bindings;
  } else if (tree_find(value, pattern)) {
    return bindings;
  }

  return t;
}

static val match_line(val bindings, val specline, val dataline,
                      val pos, val spec_lineno, val data_lineno,
                      val file)
{
#define LOG_MISMATCH(KIND)                                              \
  debuglf(spec_lineno, lit(KIND " mismatch, position ~a (~a:~a)"), pos, \
            file, data_lineno, nao);                                    \
  debuglf(spec_lineno, lit("  ~a"), dataline, nao);                     \
  if (c_num(pos) < 77)                                                  \
    debuglf(spec_lineno, lit("  ~*~a^"), pos, lit(""), nao)

#define LOG_MATCH(KIND, EXTENT)                                         \
  debuglf(spec_lineno, lit(KIND " matched, position ~a-~a (~a:~a)"),    \
            pos, EXTENT, file, data_lineno, nao);                       \
  debuglf(spec_lineno, lit("  ~a"), dataline, nao);                     \
  if (c_num(EXTENT) < 77)                                               \
    debuglf(spec_lineno, lit("  ~*~a~-*~a^"), pos, lit(""),             \
              minus(EXTENT, pos), lit("^"), nao)

  for (;;) {
    val elem;

    if (specline == nil)
      break;

    elem = first(specline);

    switch (elem ? type(elem) : 0) {
    case CONS: /* directive */
      {
        val directive = first(elem);

        if (directive == var_s) {
          val sym = second(elem);
          val pat = third(elem);
          val modifier = fourth(elem);
          val pair = assoc(bindings, sym); /* var exists already? */

          if (gt(length(modifier), one)) {
            sem_error(spec_lineno, lit("multiple modifiers on variable ~s"),
                      sym, nao);
          }

          modifier = car(modifier);

          if (pair) {
            /* If the variable already has a binding, we replace
               it with its value, and treat it as a string match.
               The spec looks like ((var <sym> <pat>) ...)
               and it must be transformed into
               (<sym-substituted> <pat> ...) */
            if (pat) {
              specline = cons(cdr(pair), cons(pat, rest(specline)));
            } else if (nump(modifier)) {
              val past = plus(pos, modifier);

              if (length_str_lt(dataline, past) || lt(past, pos))
              {
                LOG_MISMATCH("fixed field size");
                return nil;
              }

              if (!tree_find(trim_str(sub_str(dataline, pos, past)),
                             cdr(pair)))
              {
                LOG_MISMATCH("fixed field contents");
                return nil;
              }

              LOG_MATCH("fixed field", past);
              pos = past;
              specline = cdr(specline);
            } else {
              specline = cons(cdr(pair), rest(specline));
            }
            continue;
          } else if (consp(modifier)) { /* regex variable */
            val past = match_regex(dataline, car(modifier), pos);
            if (nullp(past)) {
              LOG_MISMATCH("var positive regex");
              return nil;
            }
            LOG_MATCH("var positive regex", past);
            bindings = acons(bindings, sym, sub_str(dataline, pos, past));
            pos = past;
            /* This may have another variable attached */
            if (pat) {
              specline = cons(pat, rest(specline));
              continue;
            }
          } else if (nump(modifier)) { /* fixed field */
            val past = plus(pos, modifier);
            if (length_str_lt(dataline, past) || lt(past, pos))
            {
              LOG_MISMATCH("count based var");
              return nil;
            }
            LOG_MATCH("count based var", past);
            bindings = acons(bindings, sym, trim_str(sub_str(dataline, pos, past)));
            pos = past;
            /* This may have another variable attached */
            if (pat) {
              specline = cons(pat, rest(specline));
              continue;
            }
          } else if (modifier && modifier != t) {
            sem_error(spec_lineno, lit("invalid modifier ~s on variable ~s"),
                      modifier, sym, nao);
          } else if (pat == nil) { /* no modifier, no elem -> to end of line */
            bindings = acons(bindings, sym, sub_str(dataline, pos, nil));
            pos = length_str(dataline);
          } else if (type(pat) == STR) {
            val find = search_str(dataline, pat, pos, modifier);
            if (!find) {
              LOG_MISMATCH("var delimiting string");
              return nil;
            }
            LOG_MATCH("var delimiting string", find);
            bindings = acons(bindings, sym, sub_str(dataline, pos, find));
            pos = plus(find, length_str(pat));
          } else if (consp(pat) && regexp(first(pat))) {
            val find = search_regex(dataline, first(pat), pos, modifier);
            val fpos = car(find);
            val flen = cdr(find);
            if (!find) {
              LOG_MISMATCH("var delimiting regex");
              return nil;
            }
            LOG_MATCH("var delimiting regex", fpos);
            bindings = acons(bindings, sym, sub_str(dataline, pos, fpos));
            pos = plus(fpos, flen);
          } else if (consp(pat) && first(pat) == var_s) {
            /* Unbound var followed by var: the following one must either
               be bound, or must specify a regex. */
            val second_sym = second(pat);
            val next_pat = third(pat);
            val next_modifier = fourth(pat);
            val pair = assoc(bindings, second_sym); /* var exists already? */

            if (gt(length(next_modifier), one)) {
              sem_error(spec_lineno, lit("multiple modifiers on variable ~s"),
                        second_sym, nao);
            }

            next_modifier = car(next_modifier);

            if (!pair && consp(next_modifier)) {
              val find = search_regex(dataline, first(next_modifier), pos, modifier);
              val fpos = car(find);
              val flen = cdr(find);

              if (!find) {
                LOG_MISMATCH("double var regex");
                return nil;
              }

              /* Text from here to start of regex match goes to this
                 variable. */
              bindings = acons(bindings, sym, sub_str(dataline, pos, fpos));
              /* Text from start of regex match to end goes to the
                 second variable */
              bindings = acons(bindings, second_sym, sub_str(dataline, fpos, plus(fpos, flen)));
              LOG_MATCH("double var regex (first var)", fpos);
              pos = fpos;
              LOG_MATCH("double var regex (second var)", plus(fpos, flen));
              pos = plus(fpos, flen);
              if (next_pat) {
                specline = cons(next_pat, rest(specline));
                continue;
              }
            } else if (!pair) {
              sem_error(spec_lineno, lit("consecutive unbound variables"), nao);
            } else {
            /* Re-generate a new spec with an edited version of
               the element we just processed, and repeat. */
              val new_elem = list(var_s, sym, cdr(pair), modifier, nao);

              if (next_pat)
                 specline = cons(new_elem, cons(next_pat, rest(specline)));
              else
                 specline = cons(new_elem, rest(specline));
              continue;
            }
          } else if (consp(pat) && (consp(first(pat)) || stringp(first(pat)))) {
            cons_bind (find, len, search_str(dataline, pat, pos, modifier));
            if (!find) {
              LOG_MISMATCH("string");
              return nil;
            }
            bindings = acons(bindings, sym, sub_str(dataline, pos, find));
            pos = plus(find, len);
          } else {
            sem_error(spec_lineno,
                      lit("variable followed by invalid element"), nao);
          }
        } else if (regexp(directive)) {
          val past = match_regex(dataline, directive, pos);
          if (nullp(past)) {
            LOG_MISMATCH("regex");
            return nil;
          }
          LOG_MATCH("regex", past);
          pos = past;
        } else if (directive == skip_s) {
          val max = second(elem);
          val min = third(elem);
          cnum cmax = nump(max) ? c_num(max) : 0;
          cnum cmin = nump(min) ? c_num(min) : 0;
          val greedy = eq(max, greedy_k);
          val last_good_result = nil, last_good_pos = nil;

          if (!rest(specline)) {
            debuglf(lit("skip to end of line ~a:~a"), file, data_lineno);
            return cons(bindings, t);
          }

          {
            cnum reps_max = 0, reps_min = 0;

            while (length_str_gt(dataline, pos) && min && reps_min < cmin) {
              pos = plus(pos, one);
              reps_min++;
            }

            if (min) {
              if (reps_min != cmin) {
                debuglf(spec_lineno,
                        lit("skipped only ~a/~a chars to ~a:~a:~a"),
                        num(reps_min), num(cmin),
                        file, data_lineno, pos, nao);
                return nil;
              }

              debuglf(spec_lineno, lit("skipped ~a chars to ~a:~a:~a"),
                      num(reps_min), file, data_lineno, pos, nao);
            }

            while (greedy || !max || reps_max++ < cmax) {
              val result = match_line(bindings, rest(specline), dataline, pos,
                                      spec_lineno, data_lineno, file);

              if (result) {
                if (greedy) {
                  last_good_result = result;
                  last_good_pos = pos;
                } else {
                  LOG_MATCH("skip", pos);
                  return result;
                }
              }

              if (length_str_le(dataline, pos))  {
                if (last_good_result) {
                  LOG_MATCH("greedy skip", last_good_pos);
                  return last_good_result;
                }
                break;
              }

              pos = plus(pos, one);
            }
          }

          LOG_MISMATCH("skip");
          return nil;
        } else if (directive == coll_s) {
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

          if (((times || maxtimes) && ctimax == 0) || (chars && cchars == 0))
            break;

          for (;;) {
            val new_bindings = nil, new_pos = nil;

            if ((gap || min) && mincounter < cmin)
              goto next_coll;

            if (chars && charscounter++ >= cchars)
              break;

            {
              cons_set (new_bindings, new_pos,
                        match_line(bindings, coll_specline, dataline, pos,
                                   spec_lineno, data_lineno, file));

              if (until_last_specline) {
                cons_bind (sym, spec, until_last_specline);
                cons_bind (until_last_bindings, until_pos,
                           match_line(new_bindings, spec, 
                                      dataline, pos,
                                      spec_lineno, data_lineno, file));

                if (until_pos) {
                  LOG_MATCH("until/last", until_pos);
                  if (sym == last_s) {
                    last_bindings = set_diff(until_last_bindings,
                                             new_bindings, eq_f, nil);
                    pos = until_pos;
                  }
                  break;
                } else {
                  LOG_MISMATCH("until/last");
                }
              }

              if (new_pos) {
                val strictly_new_bindings = set_diff(new_bindings,
                                                     bindings, eq_f, nil);
                LOG_MATCH("coll", new_pos);

                for (iter = strictly_new_bindings; iter; iter = cdr(iter))
                {
                  val binding = car(iter);
                  val existing = assoc(bindings_coll, car(binding));

                  bindings_coll = acons_new(bindings_coll, car(binding),
                                            cons(cdr(binding), cdr(existing)));
                }
              }

              if (new_pos && !equal(new_pos, pos)) {
                pos = new_pos;
                bug_unless (length_str_ge(dataline, pos));

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
                pos = plus(pos, one);
              }

              if (length_str_le(dataline, pos))
                break;
            }
          }

          if ((times || mintimes) && timescounter < ctimin) {
            debuglf(spec_lineno, lit("fewer than ~a iterations collected"),
                    num(ctimin), nao);
            return nil;
          }

          if (!bindings_coll)
            debuglf(spec_lineno, lit("nothing was collected"), nao);

          for (iter = bindings_coll; iter; iter = cdr(iter)) {
            val pair = car(iter);
            val rev = cons(car(pair), nreverse(cdr(pair)));
            bindings = cons(rev, bindings);
          }

          if (last_bindings) {
            bindings = set_diff(bindings, last_bindings, eq_f, car_f);
            bindings = nappend2(last_bindings, bindings);
          }
        } else if (directive == all_s || directive == some_s ||
                   directive == none_s || directive == maybe_s ||
                   directive == cases_s || directive == choose_s)
        {
          val specs = third(elem);
          val plist = fourth(elem);
          val all_match = t;
          val some_match = nil;
          val max_pos = pos;
          val choose_shortest = getplist(plist, shortest_k);
          val choose_longest = getplist(plist, longest_k);
          val choose_sym = or2(choose_longest, choose_shortest);
          val choose_bindings = bindings, choose_pos = pos;
          val choose_minmax = choose_longest ? num(-1) : num(NUM_MAX);
          val iter;

          if (choose_longest && choose_shortest)
            sem_error(spec_lineno, lit("choose: both :shortest and :longest specified"), nao);

          if (directive == choose_s && !choose_sym)
            sem_error(spec_lineno, lit("choose: criterion not specified"), nao);
          for (iter = specs; iter != nil; iter = cdr(iter)) {
            val nested_spec = first(iter);
            cons_bind (new_bindings, new_pos,
                       match_line(bindings, nested_spec, dataline, pos,
                                  spec_lineno, data_lineno, file));
            if (new_pos) {
              some_match = t;
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
                bindings = new_bindings;
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
            debuglf(spec_lineno, lit("all: some clauses didn't match"), nao);
            return nil;
          }

          if ((directive == some_s || directive == cases_s) && !some_match) {
            debuglf(spec_lineno, lit("some/cases: no clauses matched"), nao);
            return nil;
          }

          if (directive == none_s && some_match) {
            debuglf(spec_lineno, lit("none: some clauses matched"), nao);
            return nil;
          }

          /* No check for maybe, since it always succeeds. */

          if (directive == choose_s) {
            bindings = choose_bindings;
            pos = choose_pos;
          } else {
            pos = max_pos;
          }
        } else if (directive == trailer_s) {
          cons_bind (new_bindings, new_pos,
                     match_line(bindings, rest(specline), dataline, pos,
                                spec_lineno, data_lineno, file));
          (void) new_bindings;
          if (!new_pos) {
            LOG_MISMATCH("trailer");
            return nil;
          }
          LOG_MATCH("trailer", new_pos);
          return cons(bindings, pos);
        } else if (directive == eol_s) {
          if (length_str_le(dataline, pos)) {
            LOG_MATCH("eol", pos);
            return cons(bindings, t);
          }
          LOG_MISMATCH("eol");
          return nil;
        } else if (consp(directive) || stringp(directive)) {
          cons_bind (find, len, search_str_tree(dataline, elem, pos, nil));
          val newpos;

          if (find == nil || !equal(find, pos)) {
            LOG_MISMATCH("string tree");
            return nil;
          }

          newpos = plus(find, len);
          LOG_MATCH("string tree", newpos);
          pos = newpos;
        } else {
          sem_error(spec_lineno, lit("unknown directive: ~a"), directive, nao);
        }
      }
      break;
    case STR:
      {
        val find = search_str(dataline, elem, pos, nil);
        val newpos;
        if (find == nil || !equal(find, pos)) {
          LOG_MISMATCH("string");
          return nil;
        }
        newpos = plus(find, length_str(elem));
        LOG_MATCH("string", newpos);
        pos = newpos;
        break;
      }
    default:
      sem_error(spec_lineno, lit("unsupported object in spec: ~s"), elem, nao);
    }

    specline = cdr(specline);
  }

  return cons(bindings, pos);
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
      filter = get_filter_trie(filter_sym);

      if (!filter) {
        uw_throwf(query_error_s, lit("format_field: filter ~s not known"),
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

static val eval_form(val lineno, val form, val bindings)
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
        sem_error(lineno, lit("metavariable @~a syntax cannot be used here"),
                  second(form), nao);
      } else if (first(form) == expr_s) {
        sem_error(lineno, lit("the @~s syntax cannot be used here"),
                  rest(form), nao);
      } else {
        val subforms = mapcar(curry_123_2(func_n3(eval_form), 
                                          lineno, bindings), form);

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

      sem_error(lineno, lit("~a"), exc, nao);
    }
  }
  uw_catch_end;

  if (!ret)
    sem_error(lineno, lit("unbound variable in form ~s"), form, nao);

  return ret;
}

enum fpip_close { fpip_fclose, fpip_pclose, fpip_closedir };

typedef struct fpip {
  FILE *f;
  DIR *d;
  enum fpip_close close;
} fpip_t;

static fpip_t complex_open(val name, val output)
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
    ret.f = w_fopen(namestr, output ? L"w" : L"r");
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

static void do_output_line(val bindings, val specline,
                           val spec_lineno, val filter, val out)
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
            sem_error(spec_lineno, lit("bad substitution: ~a"),
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
            do_output_line(bindings, empty_clauses, spec_lineno, filter, out);
          } else if (equal(max_depth, one) && single_clauses) {
            val bind_a = mapcar(func_n1(bind_car), bind_cp);
            do_output_line(bind_a, single_clauses, spec_lineno, filter, out);
          } else if (!zerop(max_depth)) {
            cnum i;

            for (i = 0; i < c_num(max_depth); i++) {
              val bind_a = mapcar(func_n1(bind_car), bind_cp);
              val bind_d = mapcar(func_n1(bind_cdr), bind_cp);

              if (i == 0 && first_clauses) {
                do_output_line(bind_a, first_clauses, spec_lineno, filter, out);
              } else if (i == c_num(max_depth) - 1 && last_clauses) {
                do_output_line(bind_a, last_clauses, spec_lineno, filter, out);
              } else {
                do_output_line(bind_a, main_clauses, spec_lineno, filter, out);
              }

              bind_cp = bind_d;
            }
          }

        } else {
          sem_error(spec_lineno, lit("unknown directive: ~a"), directive, nao);
        }
      }
      break;
    case STR:
      put_string(out, elem);
      break;
    case 0:
      break;
    default:
      sem_error(spec_lineno,
                lit("unsupported object in output spec: ~s"), elem, nao);
    }
  }
}

static void do_output(val bindings, val specs, val filter, val out)
{
  if (equal(specs, null_list))
    return;

  for (; specs; specs = cdr(specs)) {
    cons_bind (spec_lineno, specline, first(specs));
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

    do_output_line(bindings, specline, spec_lineno, filter, out);
    put_char(out, chr('\n'));
  }
}

static val match_files(val spec, val files,
                       val bindings, val first_file_parsed,
                       val data_linenum)
{
  val data = nil;
  cnum data_lineno = 0;

  gc_hint(data);

  if (listp(first_file_parsed)) {
    data = first_file_parsed;
    data_lineno = c_num(data_linenum);
    first_file_parsed = nil;
  } else if (files) {
    val source_spec = first(files);
    val name = consp(source_spec) ? cdr(source_spec) : source_spec;
    fpip_t fp = (errno = 0, complex_open(name, nil));
    val first_spec_item = second(first(spec));

    if (consp(first_spec_item) && eq(first(first_spec_item), next_s)) {
      debugf(lit("not opening source ~a "
                 "since query starts with next directive"), name, nao);
    } else {
      debugf(lit("opening data source ~a"), name, nao);

      if (complex_open_failed(fp)) {
        if (consp(source_spec) && car(source_spec) == nothrow_k) {
          debugf(lit("could not open ~a: "
                     "treating as failed match due to nothrow"), name, nao);
          return nil;
        } else if (errno != 0)
          file_err(nil, lit("could not open ~a (error ~a/~a)"), name,
                   num(errno), string_utf8(strerror(errno)), nao);
        else
          file_err(nil, lit("could not open ~a"), name, nao);
        return nil;
      }

      files = cons(name, cdr(files)); /* Get rid of cons and nothrow */

      if ((data = complex_snarf(fp, name)) != nil)
        data_lineno = 1;
    }
  }

  for (; spec;  spec = rest(spec), data = rest(data), data_lineno++)
repeat_spec_same_data:
  {
    val specline = rest(first(spec));
    val spec_linenum = first(first(spec));
    val first_spec = first(specline);

    if (consp(first_spec)) {
      val sym = first(first_spec);


      if (sym == skip_s && rest(specline) == nil) {
        val args = rest(first_spec);
        val max = first(args);
        val min = second(args);
        cnum cmax = nump(max) ? c_num(max) : 0;
        cnum cmin = nump(min) ? c_num(min) : 0;
        val greedy = eq(max, greedy_k);
        val last_good_result = nil;
        cnum last_good_line = 0;

        if ((spec = rest(spec)) == nil)
          break;

        {
          cnum reps_max = 0, reps_min = 0;
          uw_block_begin(nil, result);

          while (data && min && reps_min < cmin) {
            data = rest(data);
            data_lineno++;
            reps_min++;
          }

          if (min) {
            if (reps_min != cmin) {
              debuglf(spec_linenum, lit("skipped only ~a/~a lines to ~a:~a"),
                      num(reps_min), num(cmin),
                      first(files), num(data_lineno), nao);
              uw_block_return(nil, nil);
            }

            debuglf(spec_linenum, lit("skipped ~a lines to ~a:~a"),
                    num(reps_min), first(files),
                    num(data_lineno), nao);
          }

          while (greedy || !max || reps_max++ < cmax) {
            result = match_files(spec, files, bindings,
                                 data, num(data_lineno));

            if (result) {
              if (greedy) {
                last_good_result = result;
                last_good_line = data_lineno;
              } else {
                debuglf(spec_linenum, lit("skip matched ~a:~a"), first(files),
                        num(data_lineno), nao);
                break;
              }
            } else {
              debuglf(spec_linenum, lit("skip didn't match ~a:~a"),
                      first(files), num(data_lineno), nao);
            }

            if (!data)
              break;

            debuglf(spec_linenum, lit("skip didn't match ~a:~a"), first(files),
                    num(data_lineno), nao);

            data = rest(data);
            data_lineno++;
          }

          uw_block_end;

          if (result)
            return result;
          if (last_good_result) {
            debuglf(spec_linenum, lit("greedy skip matched ~a:~a"), 
                    first(files), num(last_good_line), nao);
            return last_good_result;
          }
        }

        debuglf(spec_linenum, lit("skip failed"), nao);
        return nil;
      } else if (sym == trailer_s && !rest(specline)) {
        if ((spec = rest(spec)) == nil)
          break;

        {
          cons_bind (new_bindings, success,
                     match_files(spec, files, bindings,
                                 data, num(data_lineno)));

          if (success)
            return cons(new_bindings, cons(data, num(data_lineno)));
          return nil;
        }
      } else if (sym == freeform_s) {
        val args = rest(first_spec);
        val vals = mapcar(func_n1(cdr),
                             mapcar(curry_123_2(func_n3(eval_form),
                                                spec_linenum, bindings), args));

        if ((spec = rest(spec)) == nil) {
          sem_error(spec_linenum,
                    lit("freeform must be followed by a query line"), nao);
        } else {
          val limit = or2(if2(nump(first(vals)), first(vals)),
                          if2(nump(second(vals)), second(vals)));
          val term = or2(if2(stringp(first(vals)), first(vals)),
                         if2(stringp(second(vals)), second(vals)));
          val ff_specline = rest(first(spec));
          val ff_dataline = lazy_str(data, term, limit);

          cons_bind (new_bindings, success,
                     match_line(bindings, ff_specline, ff_dataline, zero,
                                spec_linenum, num(data_lineno), first(files)));

          if (!success) {
            debuglf(spec_linenum, lit("freeform match failure"), nao);
            return nil;
          }

          if (nump(success)) {
            data = lazy_str_get_trailing_list(ff_dataline, success);
            data_lineno++;
          }

          bindings = new_bindings;
        }

        if ((spec = rest(spec)) == nil)
          break;

        goto repeat_spec_same_data;
      } else if (sym == block_s) {
        val name = first(rest(first_spec));
        if (rest(specline))
          sem_error(spec_linenum,
                    lit("unexpected material after block directive"), nao);
        if ((spec = rest(spec)) == nil)
          break;
        {
          uw_block_begin(name, result);
          result = match_files(spec, files, bindings, data, num(data_lineno));
          uw_block_end;
          return result;
        }
      } else if (sym == fail_s || sym == accept_s) {
        val target = first(rest(first_spec));

        if (rest(specline))
          sem_error(spec_linenum, lit("unexpected material after ~a"), sym, nao);

        uw_block_return(target,
                        if2(sym == accept_s,
                            cons(bindings,
                                 if3(data, cons(data, num(data_lineno)), t))));
        /* TODO: uw_block_return could just throw this */
        if (target)
          sem_error(spec_linenum, lit("~a: no block named ~a in scope"),
                    sym, target, nao);
        else
          sem_error(spec_linenum, lit("%~a: no anonymous block in scope"),
                    sym, nao);
        return nil;
      } else if (sym == next_s) {
        if (rest(first_spec) && rest(specline))
          sem_error(spec_linenum, lit("invalid combination of old "
                                      "and new next syntax"), nao);
        if (rest(specline)) {
          sem_error(spec_linenum, lit("obsolete next syntax: trailing material"), nao);
        }

        if ((spec = rest(spec)) == nil)
          break;

        if (rest(first_spec)) {
          val source = rest(first_spec);
          val keyword = first(source);
          val arg = keyword;

          if (keywordp(keyword)) {
            if (eq(keyword, nothrow_k)) {
              sem_error(spec_linenum, lit("misplaced :nothrow"), nao);
            } else if (eq(keyword, args_k)) {
              val input_name = string(L"args");
              cons_bind (new_bindings, success,
                         match_files(spec, cons(input_name, files),
                                     bindings, files, one));
              if (success)
                return cons(new_bindings,
                    if3(data, cons(data, num(data_lineno)), t));
              return nil;
            }
            arg = second(source);
          }

          {
            val eval = eval_form(spec_linenum, arg, bindings);
            val str = cdr(eval);

            if (eq(second(source), nothrow_k)) {
              if (str) {
                files = cons(cons(nothrow_k, str), files);
              } else {
                files = rest(files);
                if (!files) {
                  debuglf(spec_linenum, lit("next: out of arguments"), nao);
                  return nil;
                }
                files = cons(cons(nothrow_k, first(files)), rest(files));
              }
            } else {
              if (str) {
                files = cons(str, files);
              } else {
                files = rest(files);
                if (!files)
                  sem_error(spec_linenum, lit("next: out of arguments"), nao);
                files = cons(cons(nothrow_k, first(files)), rest(files));
              }
            }
          }
        } else {
          files = rest(files);
          if (!files)
            sem_error(spec_linenum, lit("next: out of arguments"), nao);
        }

        /* We recursively process the file list, but the new
           data position we return to the caller must be in the
           original file we we were called with. Hence, we can't
           make a straight tail call here. */
        {
          cons_bind (new_bindings, success,
                     match_files(spec, files, bindings, t, nil));
          if (success)
            return cons(new_bindings,
                        if3(data, cons(data, num(data_lineno)), t));
          return nil;
        }
      } else if ((sym == some_s || sym == all_s || sym == none_s ||
                  sym == maybe_s || sym == cases_s || sym == choose_s) && 
                 second(first_spec) != t)
      {
        val all_match = t;
        val some_match = nil;
        val max_line = zero;
        val max_data = nil;
        val specs = second(first_spec);
        val plist = third(first_spec);
        val choose_shortest = getplist(plist, shortest_k);
        val choose_longest = getplist(plist, longest_k);
        val choose_sym = or2(choose_longest, choose_shortest);
        val choose_bindings = bindings, choose_line = zero, choose_data = nil;
        val choose_minmax = choose_longest ? num(-1) : num(NUM_MAX);
        val iter;

        if (choose_longest && choose_shortest)
          sem_error(spec_linenum, lit("choose: both :shortest and :longest specified"), nao);

        if (sym == choose_s && !choose_sym)
          sem_error(spec_linenum, lit("choose: criterion not specified"), nao);

        for (iter = specs; iter != nil; iter = rest(iter))
        {
          val nested_spec = first(iter);
          val data_linenum = num(data_lineno);

          cons_bind (new_bindings, success,
                     match_files(nested_spec, files, bindings,
                                 data, data_linenum));

          if (success) {
            some_match = t;

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
              bindings = new_bindings;
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
          debuglf(spec_linenum, lit("all: some clauses didn't match"), nao);
          return nil;
        }

        if ((sym == some_s || sym == cases_s) && !some_match) {
          debuglf(spec_linenum, lit("some/cases: no clauses matched"), nao);
          return nil;
        }

        if (sym == none_s && some_match) {
          debuglf(spec_linenum, lit("none: some clauses matched"), nao);
          return nil;
        }

        /* No check for maybe, since it always succeeds. */

        if (choose_sym) {
          if (consp(choose_data)) {
            data_lineno = c_num(choose_line);
            data = choose_data;
          } else if (choose_data == t) {
            data = nil;
          }
          bindings = choose_bindings;
        } else if (consp(max_data)) {
          data_lineno = c_num(max_line);
          data = max_data;
        } else if (max_data == t) {
          data = nil;
        }

        if ((spec = rest(spec)) == nil)
          break;

        goto repeat_spec_same_data;
      } else if (sym == collect_s) {
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
          sem_error(spec_linenum, lit("collect: cannot mix :gap with :mingap or :maxgap"), nao);

        if (vars) {
          list_collect_decl (fixed_vars, tail);

          if (!consp(vars)) 
            sem_error(spec_linenum, lit("collect: invalid argument to :vars"), nao);
          for (iter = vars; iter; iter = cdr(iter)) {
            val item = car(iter);
            if (bindable(item)) {
              list_collect (tail, cons(item, nil));
            } else if (consp(item) && bindable(first(item))) {
              list_collect (tail, cons(first(item), second(item)));
            } else { 
              sem_error(spec_linenum, lit("not a variable spec: ~a"), item, nao);
            }
          }
          vars = fixed_vars;
        }

        if ((times && ctimes == 0) || (lines && clines == 0)) {
          if ((spec = rest(spec)) == nil) 
            break;

          goto repeat_spec_same_data;
        }

        uw_block_begin(nil, result);

        result = t;

        while (data) {
          val new_bindings = nil, success = nil;

          if ((gap || min) && mincounter < cmin)
            goto next_collect;

          if (lines && linescounter++ >= clines)
            break;

          {
            cons_set (new_bindings, success,
                      match_files(coll_spec, files, bindings,
                                  data, num(data_lineno)));

            /* Until/last clause sees un-collated bindings from collect. */
            if (until_last_spec)
            {
              cons_bind (sym, spec, until_last_spec);
              cons_bind (until_last_bindings, success,
                         match_files(spec, files, new_bindings,
                                     data, num(data_lineno)));

              if (success) {
                debuglf(spec_linenum, lit("until/last matched ~a:~a"),
                        first(files), num(data_lineno), nao);
                /* Until discards bindings and position, last keeps them. */
                if (sym == last_s) {
                  last_bindings = set_diff(until_last_bindings,
                                           new_bindings, eq_f, nil);
                  if (success == t) {
                    data = t;
                  } else {
                    cons_bind (new_data, new_line, success);
                    data = new_data;
                    data_lineno = c_num(new_line);
                  }
                }
                break;
              }
            }

            if (success) {
              val strictly_new_bindings = set_diff(new_bindings,
                                                   bindings, eq_f, nil);

              debuglf(spec_linenum, lit("collect matched ~a:~a"),
                      first(files), num(data_lineno), nao);

              for (iter = vars; iter; iter = cdr(iter)) {
                cons_bind (var, dfl, car(iter));
                val exists = assoc(new_bindings, var);

                if (!exists) {
                  if (!dfl) 
                    sem_error(spec_linenum, lit("collect failed to bind ~a"),
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
                cnum new_lineno = c_num(new_line);

                bug_unless (new_lineno >= data_lineno);

                if (new_lineno == data_lineno) {
                  new_data = cdr(new_data);
                  new_lineno++;
                }

                debuglf(spec_linenum, lit("collect advancing from line ~a to ~a"),
                        num(data_lineno), num(new_lineno), nao);

                data = new_data;
                data_lineno = new_lineno;
                *car_l(success) = nil;
              } else {
                debuglf(spec_linenum, lit("collect consumed entire file"), nao);
                data = nil;
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
              data_lineno++;
              data = rest(data);
            }
          }
        }

        uw_block_end;

        if (!result) {
          debuglf(spec_linenum, lit("collect explicitly failed"), nao);
          return nil;
        }

        if ((times || mintimes) && timescounter < ctimin) {
          debuglf(spec_linenum, lit("fewer than ~a iterations collected"),
                  num(ctimin), nao);
          return nil;
        }

        if (!bindings_coll)
          debuglf(spec_linenum, lit("nothing was collected"), nao);

        bindings = set_diff(bindings, bindings_coll, eq_f, car_f);

        for (iter = bindings_coll; iter; iter = cdr(iter)) {
          val pair = car(iter);
          val rev = cons(car(pair), nreverse(cdr(pair)));
          bindings = cons(rev, bindings);
        }

        if (last_bindings) {
          bindings = set_diff(bindings, last_bindings, eq_f, car_f);
          bindings = nappend2(last_bindings, bindings);
        }

        if ((spec = rest(spec)) == nil)
          break;

        goto repeat_spec_same_data;
      } else if (sym == flatten_s) {
        val iter;

        for (iter = rest(first_spec); iter; iter = rest(iter)) {
          val sym = first(iter);

          if (!bindable(sym)) {
            sem_error(spec_linenum,
                      lit("flatten: ~s is not a bindable symbol"), sym, nao);
          } else {
            val existing = assoc(bindings, sym);

            if (existing)
              *cdr_l(existing) = flatten(cdr(existing));
          }
        }

        if ((spec = rest(spec)) == nil)
          break;

        goto repeat_spec_same_data;
      } else if (sym == forget_s || sym == local_s) {
        bindings = alist_remove(bindings, rest(first_spec));

        if ((spec = rest(spec)) == nil)
          break;

        goto repeat_spec_same_data;
      } else if (sym == merge_s) {
        val target = first(rest(first_spec));
        val args = rest(rest(first_spec));
        val merged = nil;

        if (!bindable(target))
          sem_error(spec_linenum, lit("~a: ~s is not a bindable symbol"),
                    sym, target, nao);

        for (; args; args = rest(args)) {
          val arg = first(args);

          if (arg) {
            val arg_eval = eval_form(spec_linenum, arg, bindings);

            if (merged)
              merged = weird_merge(merged, cdr(arg_eval));
            else
              merged = cdr(arg_eval);
          }
        }

        bindings = acons_new(bindings, target, merged);

        if ((spec = rest(spec)) == nil)
          break;

        goto repeat_spec_same_data;
      } else if (sym == bind_s) {
        val args = rest(first_spec);
        val pattern = first(args);
        val form = second(args);
        val val = eval_form(spec_linenum, form, bindings);

        bindings = dest_bind(spec_linenum, bindings, pattern, cdr(val));

        if (bindings == t)
          return nil;

        if ((spec = rest(spec)) == nil)
          break;

        goto repeat_spec_same_data;
      } else if (sym == cat_s) {
        val iter;

        for (iter = rest(first_spec); iter; iter = rest(iter)) {
          val sym = first(iter);

          if (!bindable(sym)) {
            sem_error(spec_linenum,
                      lit("cat: ~s is not a bindable symbol"), sym, nao);
          } else {
            val existing = assoc(bindings, sym);
            val sep = nil;

            if (rest(specline)) {
              val sub = subst_vars(rest(specline), bindings, nil);
              sep = cat_str(sub, nil);
            }

            if (existing)
              *cdr_l(existing) = cat_str(flatten(cdr(existing)), sep);
          }
        }

        if ((spec = rest(spec)) == nil)
          break;

        goto repeat_spec_same_data;
      } else if (sym == output_s) {
        val specs = second(first_spec);
        val dest_spec = third(first_spec);
        val nothrow = nil;
        val dest = lit("-");
        val filter = nil;
        fpip_t fp;

        if (eq(first(dest_spec), nothrow_k)) {
          if (rest(dest_spec))
            sem_error(spec_linenum, lit("material after :nothrow in output"), nao);
        } else if (!keywordp(first(dest_spec))) {
          val form = first(dest_spec);
          val val = eval_form(spec_linenum, form, bindings);
          dest = or2(cdr(val), dest);
          pop(&dest_spec);
        }

        if (eq(first(dest_spec), nothrow_k)) {
          nothrow = t;
          pop(&dest_spec);
        }

        if (keywordp(first(dest_spec))) {
          val filter_sym = getplist(dest_spec, filter_k);

          if (filter_sym) {
            filter = get_filter_trie(filter_sym);

            if (!filter)
              sem_error(spec_linenum, lit("unknown filter ~s"), filter_sym, nao);
          }
        }

        fp = (errno = 0, complex_open(dest, t));

        debugf(lit("opening data sink ~a"), dest, nao);

        if (complex_open_failed(fp)) {
          if (nothrow) {
            debugf(lit("could not open ~a: "
                       "treating as failed match due to nothrow"), dest, nao);
            return nil;
          } else if (errno != 0) {
            file_err(nil, lit("could not open ~a (error ~a/~a)"), dest,
                     num(errno), string_utf8(strerror(errno)), nao);
          } else {
            file_err(nil, lit("could not open ~a"), dest, nao);
          }
        } else {
          val stream = complex_stream(fp, dest);
          do_output(bindings, specs, filter, stream);
          close_stream(stream, t);
        }

        if ((spec = rest(spec)) == nil)
          break;

        goto repeat_spec_same_data;
      } else if (sym == define_s) {
        val args = second(first_spec);
        val body = third(first_spec);
        val name = first(args);
        val params = second(args);

        if (rest(specline))
          sem_error(spec_linenum,
                    lit("unexpected material after define"), nao);

        uw_set_func(name, cons(params, body));

        if ((spec = rest(spec)) == nil)
          break;

        goto repeat_spec_same_data;
      } else if (sym == try_s) {
        val catch_syms = second(first_spec);
        val try_clause = third(first_spec);
        val catch_fin = fourth(first_spec);
        val finally_clause = nil;

        {
          uw_block_begin(nil, result);
          uw_catch_begin(catch_syms, exsym, exvals);

          {
            result = match_files(try_clause, files, bindings,
                                 data, num(data_lineno));
            uw_do_unwind;
          }

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
                        bindings = dest_bind(spec_linenum, bindings, 
                                             param, cdr(value));

                        if (bindings == t) {
                          all_bind = nil;
                          break;
                        }
                      }
                    }

                    if (all_bind) {
                      cons_bind (new_bindings, success,
                                 match_files(body, files, bindings,
                                             data, num(data_lineno)));
                      if (success) {
                        bindings = new_bindings;
                        result = t; /* catch succeeded, so try succeeds */
                        if (consp(success)) {
                          data = car(success);
                          data_lineno = c_num(cdr(success));
                        } else {
                          data = nil;
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
                data = car(success);
                data_lineno = c_num(cdr(success));
              } else {
                data = nil;
              }
              bindings = new_bindings;
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
                         match_files(finally_clause, files, bindings,
                                     data, num(data_lineno)));
              if (success) {
                bindings = new_bindings;
                result = t; /* finally succeeds, so try block succeeds */
                if (consp(success)) {
                  data = car(success);
                  data_lineno = c_num(cdr(success));
                } else {
                  data = nil;
                }
              }
            }
          }

          uw_catch_end;
          uw_block_end;

          if (!result)
            return nil;

          if ((spec = rest(spec)) == nil)
            break;

          goto repeat_spec_same_data;
        }
      } else if (sym == defex_s) {
        val types = rest(first_spec);
        if (!all_satisfy(types, func_n1(symbolp), nil))
          sem_error(spec_linenum, lit("defex arguments must all be symbols"),
                    nao);
        (void) reduce_left(func_n2(uw_register_subtype), types, nil, nil);
        if ((spec = rest(spec)) == nil)
          break;
        goto repeat_spec_same_data;
      } else if (sym == throw_s) {
        val type = second(first_spec);
        val args = rest(rest(first_spec));
        if (!symbolp(type))
          sem_error(spec_linenum, lit("throw: ~a is not a type symbol"),
                    type, nao);
        {
          val values = mapcar(curry_123_2(func_n3(eval_form), 
                                          spec_linenum, bindings), args);
          uw_throw(type, values);
        }
      } else if (sym == deffilter_s) {
        val sym = second(first_spec);
        val table = rest(rest(first_spec));

        if (!symbolp(sym))
          sem_error(spec_linenum, lit("deffilter: ~a is not a symbol"),
                    first(first_spec), nao);

        if (!all_satisfy(table, andf(func_n1(listp), 
                                     chain(func_n1(length),
                                           curry_12_2(func_n2(eq), two),
                                           nao),
                                     chain(func_n1(first),
                                           func_n1(stringp), 
                                           nao),
                                     chain(func_n1(second),
                                           func_n1(stringp), 
                                           nao), 
                                     nao),
                                  nil))
          sem_error(spec_linenum, 
                    lit("deffilter arguments must be string pairs"),
                    nao);
        register_filter(sym, table);
        /* TODO: warn about replaced filter. */
        if ((spec = rest(spec)) == nil)
          break;
        goto repeat_spec_same_data;
      } else if (sym == eof_s) {
        if (data) {
          debugf(lit("eof failed to match at ~a"), num(data_lineno), nao);
          return nil;
        }
        if ((spec = rest(spec)) == nil)
          break;
        goto repeat_spec_same_data;
      } else {
        val func = uw_get_func(sym);

        if (func) {
          val args = rest(first_spec);
          val params = car(func);
          val ub_p_a_pairs = nil;
          val body = cdr(func);
          val piter, aiter;
          val bindings_cp = copy_alist(bindings);

          if (!equal(length(args), length(params)))
            sem_error(spec_linenum, lit("function ~a takes ~a argument(s)"),
                      sym, length(params), nao);

          for (piter = params, aiter = args; piter;
               piter = cdr(piter), aiter = cdr(aiter))
          {
            val param = car(piter);
            val arg = car(aiter);

            if (arg && bindable(arg)) {
              val val = assoc(bindings, arg);
              if (val) {
                bindings_cp = acons_new(bindings_cp,
                                        param,
                                        cdr(val));
              } else {
                bindings_cp = alist_nremove1(bindings_cp, param);
                ub_p_a_pairs = cons(cons(param, arg), ub_p_a_pairs);
              }
            } else {
              val val = eval_form(spec_linenum, arg, bindings);
              bindings_cp = acons_new(bindings_cp, param, cdr(val));
            }
          }

          {
            uw_block_begin(nil, result);
            uw_env_begin;
            result = match_files(body, files, bindings_cp,
                                 data, num(data_lineno));
            uw_env_end;
            uw_block_end;

            if (!result) {
              debuglf(spec_linenum, lit("function failed"), nao);
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
                    bindings = dest_bind(spec_linenum, bindings, 
                                         arg, cdr(newbind));
                    if (bindings == t) {
                      debuglf(spec_linenum,
                              lit("binding mismatch on ~a "
                                  "when returning from ~a"), arg, sym, nao);
                      return nil;
                    }
                  }
                }
              }

              if (consp(success)) {
                debuglf(spec_linenum,
                        lit("function matched; "
                            "advancing from line ~a to ~a"),
                        num(data_lineno), cdr(success), nao);
                data = car(success);
                data_lineno = c_num(cdr(success));
              } else {
                debuglf(spec_linenum, lit("function consumed entire file"),
                        nao);
                data = nil;
              }
            }
          }

          if ((spec = rest(spec)) == nil)
            break;

          goto repeat_spec_same_data;
        }
      }
    }

    if (data)
    {
      val dataline = first(data);
      cons_bind (new_bindings, success,
                 match_line(bindings, specline, dataline, zero,
                            spec_linenum, num(data_lineno), first(files)));

      if (nump(success) && c_num(success) < c_num(length_str(dataline))) {
        debuglf(spec_linenum, lit("spec only matches line to position ~a: ~a"),
                success, dataline, nao);
        return nil;
      }

      if (!success)
        return nil;

      bindings = new_bindings;
    } else {
      debuglf(spec_linenum, lit("spec ran out of data"), nao);
      return nil;
    }
  }

  return cons(bindings, if3(data, cons(data, num(data_lineno)), t));
}

int extract(val spec, val files, val predefined_bindings)
{
  cons_bind (bindings, success, match_files(spec, files, predefined_bindings,
                                            t, nil));

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

void match_init(void)
{
  mingap_k = intern(lit("mingap"), keyword_package);
  maxgap_k = intern(lit("maxgap"), keyword_package);
  gap_k = intern(lit("gap"), keyword_package);
  mintimes_k = intern(lit("mintimes"), keyword_package);
  maxtimes_k = intern(lit("maxtimes"), keyword_package);
  times_k = intern(lit("times"), keyword_package);
  lines_k = intern(lit("lines"), keyword_package);
  chars_k = intern(lit("chars"), keyword_package);
  choose_s = intern(lit("choose"), user_package);
  longest_k = intern(lit("longest"), keyword_package);
  shortest_k = intern(lit("shortest"), keyword_package);
  greedy_k = intern(lit("greedy"), keyword_package);
  vars_k = intern(lit("vars"), keyword_package);
}
