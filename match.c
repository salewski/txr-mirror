/* Copyright 2009
 * Kaz Kylheku <kkylheku@gmail.com>
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
#include "lib.h"
#include "gc.h"
#include "unwind.h"
#include "regex.h"
#include "stream.h"
#include "parser.h"
#include "txr.h"
#include "utf8.h"
#include "match.h"

int output_produced;

static void debugf(obj_t *fmt, ...)
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

static void debuglf(obj_t *line, obj_t *fmt, ...)
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

static void sem_error(obj_t *line, obj_t *fmt, ...)
{
  va_list vl;
  obj_t *stream = make_string_output_stream();

  va_start (vl, fmt);
  if (line)
    format(stream, lit("(~a:~a) "), spec_file_str, line, nao);
  (void) vformat(stream, fmt, vl);
  va_end (vl);

  uw_throw(query_error, get_string_from_stream(stream));
  abort();
}

static void file_err(obj_t *line, obj_t *fmt, ...)
{
  va_list vl;
  obj_t *stream = make_string_output_stream();

  va_start (vl, fmt);
  if (line)
    format(stream, lit("(~a:~a) "), spec_file_str, line, nao);
  (void) vformat(stream, fmt, vl);
  va_end (vl);

  uw_throw(file_error, get_string_from_stream(stream));
  abort();
}


void dump_shell_string(const wchar_t *str)
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

void dump_byte_string(const char *str)
{
  while (*str)
    put_char(std_output, chr(*str++));
}


void dump_var(obj_t *var, char *pfx1, size_t len1,
              char *pfx2, size_t len2, obj_t *value, int level)
{
  if (len1 >= 112 || len2 >= 112)
    internal_error("too much depth in bindings");

  if (stringp(value) || chrp(value)) {
    put_string(std_output, var);
    dump_byte_string(pfx1);
    dump_byte_string(pfx2);
    put_char(std_output, chr('='));
    if (stringp(value)) {
      dump_shell_string(c_str(value));
    } else {
      wchar_t mini[2];
      mini[0] = c_chr(value);
      mini[1] = 0;
      dump_shell_string(mini);
    }
    put_char(std_output, chr('\n'));
  } else {
    obj_t *iter;
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
  }
}

void dump_bindings(obj_t *bindings)
{
  if (opt_loglevel >= 2) {
    put_line(std_error, lit("raw_bindings:"));
    dump(bindings, std_error);
  }

  while (bindings) {
    char pfx1[128], pfx2[128];
    obj_t *var = car(car(bindings));
    obj_t *value = cdr(car(bindings));
    *pfx1 = 0; *pfx2 = 0;
    dump_var(var, pfx1, 0, pfx2, 0, value, 0);
    bindings = cdr(bindings);
  }
}

obj_t *depth(obj_t *obj)
{
  obj_t *dep = zero;

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

obj_t *weird_merge(obj_t *left, obj_t *right)
{
  obj_t *left_depth = depth(left);
  obj_t *right_depth = depth(right);

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

obj_t *map_leaf_lists(obj_t *func, obj_t *list)
{
  if (atom(list))
    return list;
  if (none_satisfy(list, func_n1(listp), nil))
    return funcall1(func, list);
  return mapcar(bind2(func_n2(map_leaf_lists), func), list);
}

obj_t *dest_bind(obj_t *bindings, obj_t *pattern, obj_t *value)
{
  if (nullp(pattern))
    return bindings;

  if (symbolp(pattern)) {
    obj_t *existing = assoc(bindings, pattern);
    if (existing) {
      if (tree_find(value, cdr(existing)))
        return bindings;
      if (tree_find(cdr(existing), value))
        return bindings;
      debugf(lit("bind variable mismatch: ~a"), pattern, nao);
      return t;
    }
    return cons(cons(pattern, value), bindings);
  }

  if (consp(pattern)) {
    obj_t *piter = pattern, *viter = value;

    while (consp(piter) && consp(viter))
    {
      bindings = dest_bind(bindings, car(piter), car(viter));
      if (bindings == t)
        return t;
      piter = cdr(piter);
      viter = cdr(viter);
    } while (consp(piter) && consp(viter));

    if (symbolp(piter)) {
      bindings = dest_bind(bindings, piter, viter);
      if (bindings == t)
        return t;
    }
  }

  return bindings;
}

obj_t *match_line(obj_t *bindings, obj_t *specline, obj_t *dataline,
                  obj_t *pos, obj_t *spec_lineno, obj_t *data_lineno,
                  obj_t *file)
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
    obj_t *elem;

    if (specline == nil)
      break;

    elem = first(specline);

    switch (elem ? type(elem) : 0) {
    case CONS: /* directive */
      {
        obj_t *directive = first(elem);

        if (directive == var) {
          obj_t *sym = second(elem);
          obj_t *pat = third(elem);
          obj_t *modifier = fourth(elem);
          obj_t *pair = assoc(bindings, sym); /* var exists already? */

          if (pair) {
            /* If the variable already has a binding, we replace
               it with its value, and treat it as a string match.
               The spec looks like ((var <sym> <pat>) ...)
               and it must be transformed into
               (<sym-substituted> <pat> ...) */
            if (pat) {
              specline = cons(cdr(pair), cons(pat, rest(specline)));
            } else if (nump(modifier)) {
              obj_t *past = plus(pos, modifier);

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
          } else if (pat == nil) { /* match to end of line or with regex */
            if (consp(modifier)) {
              obj_t *past = match_regex(dataline, car(modifier), pos);
              if (nullp(past)) {
                LOG_MISMATCH("var positive regex");
                return nil;
              }
              LOG_MATCH("var positive regex", past);
              bindings = acons_new(bindings, sym, sub_str(dataline, pos, past));
              pos = past;
            } else if (nump(modifier)) {
              obj_t *past = plus(pos, modifier);
              if (length_str_lt(dataline, past) || lt(past, pos))
              {
                LOG_MISMATCH("count based var");
                return nil;
              }
              LOG_MATCH("count based var", past);
              bindings = acons_new(bindings, sym, trim_str(sub_str(dataline, pos, past)));
              pos = past;
            } else {
              bindings = acons_new(bindings, sym, sub_str(dataline, pos, nil));
              pos = length_str(dataline);
            }
          } else if (type(pat) == STR) {
            obj_t *find = search_str(dataline, pat, pos, modifier);
            if (!find) {
              LOG_MISMATCH("var delimiting string");
              return nil;
            }
            LOG_MATCH("var delimiting string", find);
            bindings = acons_new(bindings, sym, sub_str(dataline, pos, find));
            pos = plus(find, length_str(pat));
          } else if (consp(pat) && typeof(first(pat)) == regex) {
            obj_t *find = search_regex(dataline, first(pat), pos, modifier);
            obj_t *fpos = car(find);
            obj_t *flen = cdr(find);
            if (!find) {
              LOG_MISMATCH("var delimiting regex");
              return nil;
            }
            LOG_MATCH("var delimiting regex", fpos);
            bindings = acons_new(bindings, sym, sub_str(dataline, pos, fpos));
            pos = plus(fpos, flen);
          } else if (consp(pat) && first(pat) == var) {
            /* Unbound var followed by var: the following one must be bound. */
            obj_t *second_sym = second(pat);
            obj_t *next_pat = third(pat);
            obj_t *pair = assoc(bindings, second_sym); /* var exists already? */

            if (!pair)
              sem_error(spec_lineno, lit("consecutive unbound variables"), nao);

            /* Re-generate a new spec with an edited version of
               the element we just processed, and repeat. */
            {
              obj_t *new_elem = list(var, sym, cdr(pair), modifier, nao);

              if (next_pat)
                 specline = cons(new_elem, cons(next_pat, rest(specline)));
              else
                 specline = cons(new_elem, rest(specline));
            }

            continue;
          } else if (consp(pat) && (consp(first(pat)) || stringp(first(pat)))) {
            cons_bind (find, len, search_str(dataline, pat, pos, modifier));
            if (!find) {
              LOG_MISMATCH("string");
              return nil;
            }
            bindings = acons_new(bindings, sym, sub_str(dataline, pos, find));
            pos = plus(find, len);
          } else {
            sem_error(spec_lineno,
                      lit("variable followed by invalid element"), nao);
          }
        } else if (typeof(directive) == regex) {
          obj_t *past = match_regex(dataline, directive, pos);
          if (nullp(past)) {
            LOG_MISMATCH("regex");
            return nil;
          }
          LOG_MATCH("regex", past);
          pos = past;
        } else if (directive == coll) {
          obj_t *coll_specline = second(elem);
          obj_t *until_specline = third(elem);
          obj_t *bindings_coll = nil;
          obj_t *iter;

          for (;;) {
            cons_bind (new_bindings, new_pos,
                       match_line(bindings, coll_specline, dataline, pos,
                                  spec_lineno, data_lineno, file));

            if (until_specline) {
              cons_bind (until_bindings, until_pos,
                         match_line(bindings, until_specline, dataline, pos,
                                    spec_lineno, data_lineno, file));

              if (until_pos) {
                (void) until_bindings;
                LOG_MATCH("until", until_pos);
                break;
              } else {
                LOG_MISMATCH("until");
              }
            }

            if (new_pos) {
              LOG_MATCH("coll", new_pos);

              for (iter = new_bindings; iter && iter != bindings;
                   iter = cdr(iter))
              {
                obj_t *binding = car(iter);
                obj_t *existing = assoc(bindings_coll, car(binding));

                bindings_coll = acons_new(bindings_coll, car(binding),
                                          cons(cdr(binding), cdr(existing)));
              }
            }

            if (new_pos && !equal(new_pos, pos)) {
              pos = new_pos;
              bug_unless (length_str_ge(dataline, pos));
            } else {
              pos = plus(pos, one);
            }

            if (length_str_le(dataline, pos))
              break;
          }


          if (!bindings_coll)
            debuglf(spec_lineno, lit("nothing was collected"), nao);

          for (iter = bindings_coll; iter; iter = cdr(iter)) {
            obj_t *pair = car(iter);
            obj_t *rev = cons(car(pair), nreverse(cdr(pair)));
            bindings = cons(rev, bindings);
          }
        } else if (consp(directive) || stringp(directive)) {
          cons_bind (find, len, search_str_tree(dataline, elem, pos, nil));
          obj_t *newpos;

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
        obj_t *find = search_str(dataline, elem, pos, nil);
        obj_t *newpos;
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

obj_t *format_field(obj_t *string_or_list, obj_t *spec)
{
  if (!stringp(string_or_list))
    return string_or_list;

  {
    obj_t *right = lt(spec, zero);
    obj_t *width = if3(lt(spec, zero), neg(spec), spec);
    obj_t *diff = minus(width, length_str(string_or_list));

    if (le(diff, zero))
      return string_or_list;

    if (ge(length_str(string_or_list), width))
      return string_or_list;

    {
      obj_t *padding = mkstring(diff, chr(' '));

      return if3(right,
                   cat_str(list(padding, string_or_list, nao), nil),
                   cat_str(list(string_or_list, padding, nao), nil));
    }
  }
}

obj_t *subst_vars(obj_t *spec, obj_t *bindings)
{
  list_collect_decl(out, iter);

  while (spec) {
    obj_t *elem = first(spec);

    if (consp(elem)) {
      if (first(elem) == var) {
        obj_t *sym = second(elem);
        obj_t *pat = third(elem);
        obj_t *modifier = fourth(elem);
        obj_t *pair = assoc(bindings, sym);

        if (pair) {
          if (pat)
            spec = cons(cdr(pair), cons(pat, rest(spec)));
          else if (nump(modifier))
            spec = cons(format_field(cdr(pair), modifier), rest(spec));
          else
            spec = cons(cdr(pair), rest(spec));
          continue;
        }
      } else if (first(elem) == quasi) {
        obj_t *nested = subst_vars(rest(elem), bindings);
        list_collect_append(iter, nested);
        spec = cdr(spec);
        continue;
      } else {
        obj_t *nested = subst_vars(elem, bindings);
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

obj_t *eval_form(obj_t *form, obj_t *bindings)
{
  if (!form)
    return cons(t, form);
  else if (symbolp(form))
    return assoc(bindings, form);
  else if (consp(form)) {
    if (car(form) == quasi) {
      return cons(t, cat_str(subst_vars(rest(form), bindings), nil));
    } else if (regexp(car(form))) {
      return cons(t, form);
    } else {
      obj_t *subforms = mapcar(bind2other(func_n2(eval_form), bindings), form);

      if (all_satisfy(subforms, identity_f, nil))
        return cons(t, mapcar(func_n1(cdr), subforms));
      return nil;
    }
  } if (stringp(form)) {
    return cons(t, form);
  }

  return cons(t, form);
}

typedef struct fpip {
  FILE *f;
  DIR *d;
  enum { fpip_fclose, fpip_pclose, fpip_closedir } close;
} fpip_t;

fpip_t complex_open(obj_t *name, obj_t *output)
{
  fpip_t ret = { 0, 0 };

  const wchar_t *namestr = c_str(name);
  long len = c_num(length_str(name));

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

int complex_open_failed(fpip_t fp)
{
  return fp.f == 0 && fp.d == 0;
}

void complex_close(fpip_t fp)
{
  if (fp.f == 0)
    return;
  switch (fp.close) {
  case fpip_fclose:
    if (fp.f != stdin && fp.f != stdout)
      fclose(fp.f);
    return;
  case fpip_pclose:
    pclose(fp.f);
    return;
  case fpip_closedir:
    closedir(fp.d);
    return;
  }

  internal_error("bad input source type code");
}

obj_t *complex_snarf(fpip_t fp, obj_t *name)
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

obj_t *complex_stream(fpip_t fp, obj_t *name)
{
  switch (fp.close) {
  case fpip_fclose:
    return make_stdio_stream(fp.f, name, t, nil);
  case fpip_pclose:
    return make_pipe_stream(fp.f, name, t, nil);
  case fpip_closedir:
    uw_throwf(query_error, lit("cannot output to directory: ~a"), name, nao);
  }

  internal_error("bad input source type");
}


obj_t *robust_length(obj_t *obj)
{
  if (obj == nil)
    return zero;
  if (atom(obj))
    return negone;
  return length(obj);
}

obj_t *bind_car(obj_t *bind_cons)
{
  return if3(consp(cdr(bind_cons)),
               cons(car(bind_cons), car(cdr(bind_cons))),
               bind_cons);
}

obj_t *bind_cdr(obj_t *bind_cons)
{
  return if3(consp(cdr(bind_cons)),
               cons(car(bind_cons), cdr(cdr(bind_cons))),
               bind_cons);
}

obj_t *extract_vars(obj_t *output_spec)
{
  list_collect_decl (vars, tai);

  if (consp(output_spec)) {
    if (first(output_spec) == var) {
      list_collect (tai, second(output_spec));
    } else {
      for (; output_spec; output_spec = cdr(output_spec))
        list_collect_nconc(tai, extract_vars(car(output_spec)));
    }
  }

  return vars;
}

obj_t *extract_bindings(obj_t *bindings, obj_t *output_spec)
{
  list_collect_decl (bindings_out, tail);
  obj_t *var_list = extract_vars(output_spec);

  for (; bindings; bindings = cdr(bindings))
    if (memq(car(car(bindings)), var_list))
      list_collect(tail, car(bindings));

  return bindings_out;
}

void do_output_line(obj_t *bindings, obj_t *specline,
                    obj_t *spec_lineno, obj_t *out)
{
  for (; specline; specline = rest(specline)) {
    obj_t *elem = first(specline);

    switch (elem ? type(elem) : 0) {
    case CONS:
      {
        obj_t *directive = first(elem);

        if (directive == var) {
          obj_t *str = cat_str(subst_vars(cons(elem, nil), bindings), nil);
          if (str == nil)
            sem_error(spec_lineno, lit("bad substitution: ~a"),
                      second(elem), nao);
          put_string(out, str);
        } else if (directive == rep) {
          obj_t *main_clauses = second(elem);
          obj_t *single_clauses = third(elem);
          obj_t *first_clauses = fourth(elem);
          obj_t *last_clauses = fifth(elem);
          obj_t *empty_clauses = sixth(elem);
          obj_t *bind_cp = extract_bindings(bindings, elem);
          obj_t *max_depth = reduce_left(func_n2(max2),
                                         bind_cp, zero,
                                         chain(list(func_n1(cdr),
                                                    func_n1(robust_length),
                                                    nao)));

          if (equal(max_depth, zero) && empty_clauses) {
            do_output_line(bindings, empty_clauses, spec_lineno, out);
          } else if (equal(max_depth, one) && single_clauses) {
            obj_t *bind_a = mapcar(func_n1(bind_car), bind_cp);
            do_output_line(bind_a, single_clauses, spec_lineno, out);
          } else if (!zerop(max_depth)) {
            long i;

            for (i = 0; i < c_num(max_depth); i++) {
              obj_t *bind_a = mapcar(func_n1(bind_car), bind_cp);
              obj_t *bind_d = mapcar(func_n1(bind_cdr), bind_cp);

              if (i == 0 && first_clauses) {
                do_output_line(bind_a, first_clauses, spec_lineno, out);
              } else if (i == c_num(max_depth) - 1 && last_clauses) {
                do_output_line(bind_a, last_clauses, spec_lineno, out);
              } else {
                do_output_line(bind_a, main_clauses, spec_lineno, out);
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

void do_output(obj_t *bindings, obj_t *specs, obj_t *out)
{
  if (equal(specs, null_list))
    return;

  for (; specs; specs = cdr(specs)) {
    cons_bind (spec_lineno, specline, first(specs));
    obj_t *first_elem = first(specline);

    if (consp(first_elem)) {
      obj_t *sym = first(first_elem);

      if (sym == repeat) {
        obj_t *main_clauses = second(first_elem);
        obj_t *single_clauses = third(first_elem);
        obj_t *first_clauses = fourth(first_elem);
        obj_t *last_clauses = fifth(first_elem);
        obj_t *empty_clauses = sixth(first_elem);
        obj_t *bind_cp = extract_bindings(bindings, first_elem);
        obj_t *max_depth = reduce_left(func_n2(max2),
                                       bind_cp, zero,
                                       chain(list(func_n1(cdr),
                                                  func_n1(robust_length),
                                                  nao)));

        if (equal(max_depth, zero) && empty_clauses) {
          do_output(bind_cp, empty_clauses, out);
        } else if (equal(max_depth, one) && single_clauses) {
          obj_t *bind_a = mapcar(func_n1(bind_car), bind_cp);
          do_output(bind_a, single_clauses, out);
        } else if (!zerop(max_depth)) {
          long i;

          for (i = 0; i < c_num(max_depth); i++) {
            obj_t *bind_a = mapcar(func_n1(bind_car), bind_cp);
            obj_t *bind_d = mapcar(func_n1(bind_cdr), bind_cp);

            if (i == 0 && first_clauses) {
              do_output(bind_a, first_clauses, out);
            } else if (i == c_num(max_depth) - 1 && last_clauses) {
              do_output(bind_a, last_clauses, out);
            } else {
              do_output(bind_a, main_clauses, out);
            }

            bind_cp = bind_d;
          }
        }
        continue;
      }
    }

    do_output_line(bindings, specline, spec_lineno, out);
    put_char(out, chr('\n'));
  }
}

obj_t *match_files(obj_t *spec, obj_t *files,
                   obj_t *bindings, obj_t *first_file_parsed,
                   obj_t *data_linenum)
{
  obj_t *data = nil;
  long data_lineno = 0;

  if (listp(first_file_parsed)) {
    data = first_file_parsed;
    data_lineno = c_num(data_linenum);
    first_file_parsed = nil;
  } else if (files) {
    obj_t *source_spec = first(files);
    obj_t *name = consp(source_spec) ? cdr(source_spec) : source_spec;
    fpip_t fp = (errno = 0, complex_open(name, nil));
    obj_t *first_spec_item = second(first(spec));

    if (consp(first_spec_item) && eq(first(first_spec_item), next)) {
      debugf(lit("not opening source ~a "
                 "since query starts with next directive"), name, nao);
    } else {
      debugf(lit("opening data source ~a"), name, nao);

      if (complex_open_failed(fp)) {
        if (consp(source_spec) && car(source_spec) == nothrow) {
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
    obj_t *specline = rest(first(spec));
    obj_t *dataline = first(data);
    obj_t *spec_linenum = first(first(spec));
    obj_t *first_spec = first(specline);

    if (consp(first_spec)) {
      obj_t *sym = first(first_spec);

      if (sym == skip) {
        obj_t *max = first(rest(first_spec));
        long cmax = nump(max) ? c_num(max) : 0;
        long reps = 0;

        if (rest(specline))
          sem_error(spec_linenum,
                    lit("unexpected material after skip directive"), nao);

        if ((spec = rest(spec)) == nil)
          break;

        {
          uw_block_begin(nil, result);

          while (dataline && (!max || reps++ < cmax)) {
            result = match_files(spec, files, bindings,
                                 data, num(data_lineno));

            if (result) {
              debuglf(spec_linenum, lit("skip matched ~a:~a"), first(files),
                      num(data_lineno), nao);
              break;
            }

            debuglf(spec_linenum, lit("skip didn't match ~a:~a"), first(files),
                    num(data_lineno), nao);
            data = rest(data);
            data_lineno++;
            dataline = first(data);
          }

          uw_block_end;

          if (result)
            return result;
        }

        debuglf(spec_linenum, lit("skip failed"), nao);
        return nil;
      } else if (sym == trailer) {
        if (rest(specline))
          sem_error(spec_linenum,
                    lit("unexpected material after trailer directive"), nao);

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
      } else if (sym == freeform) {
        obj_t *args = rest(first_spec);
        obj_t *vals = mapcar(func_n1(cdr),
                             mapcar(bind2other(func_n2(eval_form),
                                                bindings), args));

        if ((spec = rest(spec)) == nil) {
          sem_error(spec_linenum,
                    lit("freeform must be followed by a query line"), nao);
        } else {
          obj_t *limit = or2(if2(nump(first(vals)), first(vals)),
                             if2(nump(second(vals)), second(vals)));
          obj_t *term = or2(if2(stringp(first(vals)), first(vals)),
                            if2(stringp(second(vals)), second(vals)));
          obj_t *ff_specline = rest(first(spec));
          obj_t *ff_dataline = lazy_str(data, term, limit);

          cons_bind (new_bindings, success,
                     match_line(bindings, ff_specline, ff_dataline, zero,
                                spec_linenum, num(data_lineno), first(files)));

          if (!success) {
            debuglf(spec_linenum, lit("freeform match failure"), nao);
            return nil;
          }

          if (nump(success))
            data = lazy_str_get_trailing_list(ff_dataline, success);

          bindings = new_bindings;
        }

        if ((spec = rest(spec)) == nil)
          break;

        goto repeat_spec_same_data;
      } else if (sym == block) {
        obj_t *name = first(rest(first_spec));
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
      } else if (sym == fail || sym == accept) {
        obj_t *target = first(rest(first_spec));

        if (rest(specline))
          sem_error(spec_linenum, lit("unexpected material after ~a"), sym, nao);

        uw_block_return(target,
                        if2(sym == accept,
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
      } else if (sym == next) {
        if (rest(first_spec) && rest(specline))
          sem_error(spec_linenum, lit("invalid combination of old "
                                      "and new next syntax"), nao);

        if ((spec = rest(spec)) == nil)
          break;

        if (rest(first_spec)) {
          obj_t *source = rest(first_spec);

          if (eq(first(source), nothrow))
            push(nil, &source);
          else if (eq(first(source), args)) {
            obj_t *input_name = string(L"args");
            cons_bind (new_bindings, success,
                match_files(spec, cons(input_name, files),
                  bindings, files, one));
            if (success)
              return cons(new_bindings,
                  if3(data, cons(data, num(data_lineno)), t));
            return nil;
          }

          {
            obj_t *val = eval_form(first(source), bindings);
            obj_t *name = cdr(val);

            if (!val)
              sem_error(spec_linenum, lit("next: unbound variable in form ~a"),
                        first(source), nao);

            if (eq(second(source), nothrow)) {
              if (name) {
                files = cons(cons(nothrow, name), files);
              } else {
                files = rest(files);
                if (!files) {
                  debuglf(spec_linenum, lit("next: out of arguments"), nao);
                  return nil;
                }
                files = cons(cons(nothrow, first(files)), rest(files));
              }
            } else {
              if (name) {
                files = cons(name, files);
              } else {
                files = rest(files);
                if (!files)
                  sem_error(spec_linenum, lit("next: out of arguments"), nao);
                files = cons(cons(nothrow, first(files)), rest(files));
              }
            }
          }
        } else if (rest(specline)) {
          obj_t *sub = subst_vars(rest(specline), bindings);
          obj_t *str = cat_str(sub, nil);
          if (str == nil) {
            sem_error(spec_linenum, lit("bad substitution in next file spec"),
                      nao);
            continue;
          }
          files = cons(cons(nothrow, str), files);
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
      } else if (sym == some || sym == all || sym == none || sym == maybe ||
                 sym == cases)
      {
        obj_t *specs;
        obj_t *all_match = t;
        obj_t *some_match = nil;
        obj_t *max_line = zero;
        obj_t *max_data = nil;

        for (specs = rest(first_spec); specs != nil; specs = rest(specs))
        {
          obj_t *nested_spec = first(specs);
          obj_t *data_linenum = num(data_lineno);

          cons_bind (new_bindings, success,
                     match_files(nested_spec, files, bindings,
                                 data, data_linenum));

          if (success) {
            bindings = new_bindings;
            some_match = t;

            if (success == t) {
              max_data = t;
            } else if (consp(success) && max_data != t) {
              cons_bind (new_data, new_line, success);
              if (gt(new_line, max_line)) {
                max_line = new_line;
                max_data = new_data;
              }
            }
            if (sym == cases)
              break;
          } else {
            all_match = nil;
          }
        }

        if (sym == all && !all_match) {
          debuglf(spec_linenum, lit("all: some clauses didn't match"), nao);
          return nil;
        }

        if ((sym == some || sym == cases) && !some_match) {
          debuglf(spec_linenum, lit("some/cases: no clauses matched"), nao);
          return nil;
        }

        if (sym == none && some_match) {
          debuglf(spec_linenum, lit("none: some clauses matched"), nao);
          return nil;
        }

        /* No check for maybe, since it always succeeds. */

        if (consp(max_data)) {
          data_lineno = c_num(max_line);
          data = max_data;
        } else if (max_data == t) {
          data = nil;
        }

        if ((spec = rest(spec)) == nil)
          break;

        goto repeat_spec_same_data;
      } else if (sym == collect) {
        obj_t *coll_spec = second(first_spec);
        obj_t *until_spec = third(first_spec);
        obj_t *bindings_coll = nil;
        obj_t *iter;

        uw_block_begin(nil, result);

        result = t;

        while (data) {
          cons_bind (new_bindings, success,
                     match_files(coll_spec, files, bindings,
                                 data, num(data_lineno)));

          /* Until clause sees un-collated bindings from collect. */
          if (until_spec)
          {
            cons_bind (discarded_bindings, success,
                       match_files(until_spec, files, new_bindings,
                                   data, num(data_lineno)));

            if (success) {
              (void) discarded_bindings;
              break;
            }
          }

          if (success) {
            debuglf(spec_linenum, lit("collect matched ~a:~a"),
                    first(files), num(data_lineno), nao);

            for (iter = new_bindings; iter && iter != bindings;
                iter = cdr(iter))
            {
              obj_t *binding = car(iter);
              obj_t *existing = assoc(bindings_coll, car(binding));

              bindings_coll = acons_new(bindings_coll, car(binding),
                                        cons(cdr(binding), cdr(existing)));
            }
          }

          if (success) {
            if (consp(success)) {
              cons_bind (new_data, new_line, success);
              long new_lineno = c_num(new_line);

              bug_unless (new_lineno >= data_lineno);

              if (new_lineno == data_lineno) {
                new_data = cdr(new_data);
                new_lineno++;
              }

              debuglf(spec_linenum, lit("collect advancing from line ~a to ~a"),
                      num(data_lineno), num(new_lineno), nao);

              data = new_data;
              data_lineno = new_lineno;
            } else {
              debuglf(spec_linenum, lit("collect consumed entire file"), nao);
              data = nil;
            }
          } else {
            data = rest(data);
            data_lineno++;
          }
        }

        uw_block_end;

        if (!result) {
          debuglf(spec_linenum, lit("collect explicitly failed"), nao);
          return nil;
        }

        if (!bindings_coll)
          debuglf(spec_linenum, lit("nothing was collected"), nao);

        for (iter = bindings_coll; iter; iter = cdr(iter)) {
          obj_t *pair = car(iter);
          obj_t *rev = cons(car(pair), nreverse(cdr(pair)));
          bindings = cons(rev, bindings);
        }

        if ((spec = rest(spec)) == nil)
          break;

        goto repeat_spec_same_data;
      } else if (sym == flattn) {
        obj_t *iter;

        for (iter = rest(first_spec); iter; iter = rest(iter)) {
          obj_t *sym = first(iter);

          if (!symbolp(sym)) {
            sem_error(spec_linenum, lit("non-symbol in flatten directive"),
                      nao);
          } else {
            obj_t *existing = assoc(bindings, sym);

            if (existing)
              *cdr_l(existing) = flatten(cdr(existing));
          }
        }

        if ((spec = rest(spec)) == nil)
          break;

        goto repeat_spec_same_data;
      } else if (sym == forget || sym == local) {
        bindings = alist_remove(bindings, rest(first_spec));

        if ((spec = rest(spec)) == nil)
          break;

        goto repeat_spec_same_data;
      } else if (sym == mrge) {
        obj_t *target = first(rest(first_spec));
        obj_t *args = rest(rest(first_spec));
        obj_t *merged = nil;

        if (!target || !symbolp(target))
          sem_error(spec_linenum, lit("bad merge directive"), nao);

        for (; args; args = rest(args)) {
          obj_t *other_sym = first(args);

          if (other_sym) {
            obj_t *other_lookup = assoc(bindings, other_sym);

            if (!symbolp(other_sym))
              sem_error(spec_linenum, lit("non-symbol in merge directive"),
                        nao);
            else if (!other_lookup)
              sem_error(spec_linenum, lit("merge: nonexistent symbol ~a"),
                        other_sym, nao);

            if (merged)
              merged = weird_merge(merged, cdr(other_lookup));
            else
              merged = cdr(other_lookup);
          }
        }

        bindings = acons_new(bindings, target, merged);

        if ((spec = rest(spec)) == nil)
          break;

        goto repeat_spec_same_data;
      } else if (sym == bind) {
        obj_t *args = rest(first_spec);
        obj_t *pattern = first(args);
        obj_t *form = second(args);
        obj_t *val = eval_form(form, bindings);

        if (!val)
          sem_error(spec_linenum, lit("bind: unbound variable on right side"),
                    nao);

        bindings = dest_bind(bindings, pattern, cdr(val));

        if (bindings == t)
          return nil;

        if ((spec = rest(spec)) == nil)
          break;

        goto repeat_spec_same_data;
      } else if (sym == cat) {
        obj_t *iter;

        for (iter = rest(first_spec); iter; iter = rest(iter)) {
          obj_t *sym = first(iter);

          if (!symbolp(sym)) {
            sem_error(spec_linenum, lit("non-symbol in cat directive"), nao);
          } else {
            obj_t *existing = assoc(bindings, sym);
            obj_t *sep = nil;

            if (rest(specline)) {
              obj_t *sub = subst_vars(rest(specline), bindings);
              sep = cat_str(sub, nil);
            }

            if (existing)
              *cdr_l(existing) = cat_str(flatten(cdr(existing)), sep);
          }
        }

        if ((spec = rest(spec)) == nil)
          break;

        goto repeat_spec_same_data;
      } else if (sym == output) {
        obj_t *specs = second(first_spec);
        obj_t *old_style_dest = third(first_spec);
        obj_t *new_style_dest = fourth(first_spec);
        obj_t *nt = nil;
        obj_t *dest;
        fpip_t fp;

        if (old_style_dest) {
          dest = cat_str(subst_vars(old_style_dest, bindings), nil);
        } else {
          if (eq(first(new_style_dest), nothrow))
            push(nil, &new_style_dest);

          {
            obj_t *form = first(new_style_dest);
            obj_t *val = eval_form(form, bindings);

            if (!val)
              sem_error(spec_linenum,
                        lit("output: unbound variable in form ~a"), form, nao);

            nt = eq(second(new_style_dest), nothrow);
            dest = or2(cdr(val), string(L"-"));
          }
        }

        fp = (errno = 0, complex_open(dest, t));

        debugf(lit("opening data sink ~a"), dest, nao);

        if (complex_open_failed(fp)) {
          if (nt) {
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
          obj_t *stream = complex_stream(fp, dest);
          do_output(bindings, specs, stream);
          close_stream(stream, t);
        }

        if ((spec = rest(spec)) == nil)
          break;

        goto repeat_spec_same_data;
      } else if (sym == define) {
        obj_t *args = second(first_spec);
        obj_t *body = third(first_spec);
        obj_t *name = first(args);
        obj_t *params = second(args);

        if (rest(specline))
          sem_error(spec_linenum,
                    lit("unexpected material after define"), nao);

        uw_set_func(name, cons(params, body));

        if ((spec = rest(spec)) == nil)
          break;

        goto repeat_spec_same_data;
      } else if (sym == try) {
        obj_t *catch_syms = second(first_spec);
        obj_t *try_clause = third(first_spec);
        obj_t *catch_fin = fourth(first_spec);
        obj_t *finally_clause = nil;

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
              obj_t *iter;

              for (iter = catch_fin; iter; iter = cdr(iter)) {
                obj_t *clause = car(iter);
                obj_t *type = first(second(clause));
                obj_t *params = second(second(clause));
                obj_t *body = third(clause);
                obj_t *vals = if3(listp(exvals),
                                  exvals,
                                  cons(cons(t, exvals), nil));

                if (first(clause) == catch) {
                  if (uw_exception_subtype_p(exsym, type)) {
                    obj_t *all_bind = t;
                    obj_t *piter, *viter;

                    for (piter = params, viter = vals;
                         piter && viter;
                         piter = cdr(piter), viter = cdr(viter))
                    {
                      obj_t *param = car(piter);
                      obj_t *val = car(viter);

                      if (val) {
                        bindings = dest_bind(bindings, param, cdr(val));

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
                } else if (car(clause) == finally) {
                  finally_clause = body;
                }
              }
            }
            uw_do_unwind;
          }

          uw_unwind {
            obj_t *iter;

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
                obj_t *clause = car(iter);
                if (first(clause) == finally) {
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
      } else if (sym == defex) {
        obj_t *types = rest(first_spec);
        if (!all_satisfy(types, func_n1(symbolp), nil))
          sem_error(spec_linenum, lit("defex arguments must all be symbols"),
                    nao);
        (void) reduce_left(func_n2(uw_register_subtype), types, nil, nil);
        if ((spec = rest(spec)) == nil)
          break;
        goto repeat_spec_same_data;
      } else if (sym == throw) {
        obj_t *type = second(first_spec);
        obj_t *args = rest(rest(first_spec));
        if (!symbolp(type))
          sem_error(spec_linenum, lit("throw: ~a is not a type symbol"),
                    first(first_spec), nao);
        {
          obj_t *values = mapcar(bind2other(func_n2(eval_form), bindings),
                                 args);
          uw_throw(type, values);
        }
      } else {
        obj_t *func = uw_get_func(sym);

        if (func) {
          obj_t *args = rest(first_spec);
          obj_t *params = car(func);
          obj_t *ub_p_a_pairs = nil;
          obj_t *body = cdr(func);
          obj_t *piter, *aiter;
          obj_t *bindings_cp = copy_alist(bindings);

          if (!equal(length(args), length(params)))
            sem_error(spec_linenum, lit("function ~a takes ~a argument(s)"),
                      sym, length(params), nao);

          for (piter = params, aiter = args; piter;
               piter = cdr(piter), aiter = cdr(aiter))
          {
            obj_t *param = car(piter);
            obj_t *arg = car(aiter);

            if (arg && symbolp(arg)) {
              obj_t *val = eval_form(arg, bindings);
              if (val) {
                bindings_cp = acons_new(bindings_cp,
                                        param,
                                        cdr(val));
              } else {
                bindings_cp = alist_remove1(bindings_cp, param);
                ub_p_a_pairs = cons(cons(param, arg), ub_p_a_pairs);
              }
            } else {
              obj_t *val = eval_form(arg, bindings);
              if (!val)
                sem_error(spec_linenum,
                          lit("unbound variable in function argument form"),
                          nao);
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

              for (piter = ub_p_a_pairs; piter; piter = cdr(aiter))
              {
                cons_bind (param, arg, car(piter));

                if (symbolp(arg)) {
                  obj_t *newbind = assoc(new_bindings, param);
                  if (newbind) {
                    bindings = dest_bind(bindings, arg, cdr(newbind));
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

    if (dataline == nil)
      return nil;

    {
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
    }
  }

  return cons(bindings, if3(data, cons(data, num(data_lineno)), t));
}

int extract(obj_t *spec, obj_t *files, obj_t *predefined_bindings)
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
