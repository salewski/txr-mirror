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

%{

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>
#include <limits.h>
#include <errno.h>
#include <dirent.h>
#include <setjmp.h>
#include "lib.h"
#include "gc.h"
#include "unwind.h"
#include "regex.h"
#include "extract.h"

int yylex(void);
void yyerror(const char *);

obj_t *repeat_rep_helper(obj_t *sym, obj_t *main, obj_t *parts);

static obj_t *parsed_spec;
static int output_produced;

%}

%union {
  char *lexeme;
  union obj *obj;
  char chr;
  long num;
}

%token <lexeme> TEXT IDENT ALL SOME NONE MAYBE AND OR END COLLECT UNTIL COLL
%token <lexeme> OUTPUT REPEAT REP SINGLE FIRST LAST EMPTY
%token <num> NUMBER
%token <chr> REGCHAR

%type <obj> spec clauses clause all_clause some_clause none_clause maybe_clause
%type <obj> collect_clause clause_parts additional_parts output_clause
%type <obj> line elems_opt elems elem var var_op list exprs expr
%type <obj> out_clauses out_clauses_opt out_clause
%type <obj> repeat_clause repeat_parts_opt o_line
%type <obj> o_elems_opt o_elems_opt2 o_elems o_elem rep_elem rep_parts_opt
%type <obj> regex regexpr regbranch
%type <obj> regterm regclass regclassterm regrange
%type <chr> regchar

%nonassoc ALL SOME NONE MAYBE AND OR END COLLECT UNTIL COLL
%nonassoc OUTPUT REPEAT REP FIRST LAST EMPTY
%nonassoc '{' '}' '[' ']' '(' ')'
%right IDENT TEXT NUMBER
%left '|' '/'
%right '*' '?' '+'
%right '^' '.' '\\' REGCHAR

%%

spec : clauses                  { parsed_spec = $1; }
     |                          { parsed_spec = nil; }
     | error                    { parsed_spec = nil;
                                  yybadtoken(yychar, 0); }
     ;

clauses : clause                { $$ = cons($1, nil); }
        | clause clauses        { $$ = cons($1, $2);  }
        ;

clause : all_clause             { $$ = list(num(lineno - 1), $1, nao); }
       | some_clause            { $$ = list(num(lineno - 1), $1, nao); }
       | none_clause            { $$ = list(num(lineno - 1), $1, nao); }
       | maybe_clause           { $$ = list(num(lineno - 1), $1, nao); }
       | collect_clause         { $$ = list(num(lineno - 1), $1, nao); }
       | output_clause          { $$ = list(num(lineno - 1), $1, nao); }
       | line                   { $$ = $1; }
       | repeat_clause          { $$ = nil;
                                  yyerror("repeat outside of output"); }
       ;

all_clause : ALL newl clause_parts      { $$ = cons(all, $3); }
           | ALL newl error             { $$ = nil;
                                          yybadtoken(yychar,
                                                     "all clause"); }
           | ALL newl END               { $$ = nil;
                                          yyerror("empty all clause"); }

           ;

some_clause : SOME newl clause_parts    { $$ = cons(some, $3); }
            | SOME newl error           { $$ = nil;
                                          yybadtoken(yychar,
                                                     "some clause"); }
            | SOME newl END             { $$ = nil;
                                          yyerror("empty some clause"); }
            ;

none_clause : NONE newl clause_parts    { $$ = cons(none, $3); }
            | NONE newl error           { $$ = nil;
                                          yybadtoken(yychar,
                                                     "none clause"); }
            | NONE newl END             { $$ = nil;
                                          yyerror("empty none clause"); }
            ;

maybe_clause : MAYBE newl clause_parts  { $$ = cons(maybe, $3); }
             | MAYBE newl error         { $$ = nil;
                                          yybadtoken(yychar,
                                                     "maybe clause"); }
             | MAYBE newl END           { $$ = nil;
                                          yyerror("empty maybe clause"); }
             ;

collect_clause : COLLECT newl clauses END newl  { $$ = list(collect, $3, nao); }
               | COLLECT newl clauses
                 UNTIL newl clauses END newl    { $$ = list(collect, $3,
                                                            $6, nao); }
               | COLLECT newl error     { $$ = nil;
                                          if (yychar == UNTIL || yychar == END)
                                            yyerror("empty collect");
                                          else
                                            yybadtoken(yychar,
                                                       "collect clause"); }
               ;

clause_parts : clauses additional_parts { $$ = cons($1, $2); }
             ;

additional_parts : END newl                             { $$ = nil; }
                 | AND newl clauses additional_parts    { $$ = cons($3, $4); }
                 | OR newl clauses additional_parts    { $$ = cons($3, $4); }
                 ;

line : elems_opt '\n'           { $$ = $1; }
     ;

elems_opt : elems               { $$ = cons(num(lineno - 1), $1); }
          |                     { $$ = nil; }
          ;

elems : elem                    { $$ = cons($1, nil); }
      | elem elems              { $$ = cons($1, $2); }
      | rep_elem                { $$ = nil;
                                  yyerror("rep outside of output"); }
      ;

elem : TEXT                     { $$ = string($1); }
     | var                      { $$ = $1; }
     | list                     { $$ = $1; }
     | regex                    { $$ = cons(regex_compile($1), $1); }
     | COLL elems END           { $$ = list(coll, $2, nao); }
     | COLL elems
       UNTIL elems END          { $$ = list(coll, $2, $4, nao); }
     | COLL error               { $$ = nil;
                                  yybadtoken(yychar, "coll clause"); }
     ;

output_clause : OUTPUT o_elems '\n'
                out_clauses
                END newl        { $$ = list(output, $4, $2, nao); }
              | OUTPUT newl
                out_clauses
                END newl        { $$ = list(output, $3, nao); }
              | OUTPUT o_elems '\n'
                error           { $$ = nil;
                                  yybadtoken(yychar, "output clause"); }
              | OUTPUT newl
                error           { $$ = nil;
                                  yybadtoken(yychar, "output clause"); }
              ;

out_clauses : out_clause                { $$ = cons($1, nil); }
            | out_clause out_clauses    { $$ = cons($1, $2);  }
            ;

out_clause : repeat_clause              { $$ = list(num(lineno - 1), $1, nao); }
           | o_line                     { $$ = $1; }
           | all_clause                 { $$ = nil;
                                          yyerror("match clause in output"); }
           | some_clause                { $$ = nil;
                                          yyerror("match clause in output"); }
           | none_clause                { $$ = nil;
                                          yyerror("match clause in output"); }
           | maybe_clause               { $$ = nil;
                                          yyerror("match clause in output"); }
           | collect_clause             { $$ = nil;
                                          yyerror("match clause in output"); }
           | output_clause              { $$ = nil;
                                          yyerror("match clause in output"); }
           ;

repeat_clause : REPEAT newl
                out_clauses
                repeat_parts_opt
                END newl                { $$ = repeat_rep_helper(repeat, $3, $4); }
              | REPEAT newl
                error           { $$ = nil;
                                  yybadtoken(yychar, "repeat clause"); }
              ;

repeat_parts_opt : SINGLE newl
                   out_clauses_opt
                   repeat_parts_opt     { $$ = cons(cons(single, $3), $4); }
                 | FIRST newl
                   out_clauses_opt
                   repeat_parts_opt     { $$ = cons(cons(frst, $3), $4); }
                 | LAST newl
                   out_clauses_opt
                   repeat_parts_opt     { $$ = cons(cons(lst, $3), $4); }
                 | EMPTY newl
                   out_clauses_opt
                   repeat_parts_opt     { $$ = cons(cons(empty, $3), $4); }
                 | /* empty */          { $$ = nil; }
                 ;


out_clauses_opt : out_clauses   { $$ = $1; }
                | /* empty */   { $$ = null_list; }

o_line : o_elems_opt '\n'       { $$ = $1; }
       ;

o_elems_opt : o_elems           { $$ = cons(num(lineno - 1), $1); }
            |                   { $$ = nil; }
            ;

o_elems_opt2 : o_elems          { $$ = $1; }
             |                  { $$ = null_list; }
             ;

o_elems : o_elem                { $$ = cons($1, nil); }
        | o_elem o_elems        { $$ = cons($1, $2); }
        ;

o_elem : TEXT                   { $$ = string($1); }
       | var                    { $$ = $1; }
       | rep_elem               { $$ = $1; }
       ;

rep_elem : REP o_elems
           rep_parts_opt END    { $$ = repeat_rep_helper(rep, $2, $3); }
         | REP error            { $$ = nil; yybadtoken(yychar, "rep clause"); }
         ;

rep_parts_opt : SINGLE o_elems_opt2
                rep_parts_opt           { $$ = cons(cons(single, $2), $3); }
              | FIRST o_elems_opt2
                rep_parts_opt           { $$ = cons(cons(frst, $2), $3); }
              | LAST o_elems_opt2
                rep_parts_opt           { $$ = cons(cons(lst, $2), $3); }
              | EMPTY o_elems_opt2
                rep_parts_opt           { $$ = cons(cons(empty, $2), $3); }
              | /* empty */             { $$ = nil; }
              ;


/* This sucks, but factoring '*' into a nonterminal
 * that generates an empty phrase causes reduce/reduce conflicts.
 */
var : IDENT                     { $$ = list(var, intern(string($1)), nao); }
    | IDENT elem                { $$ = list(var, intern(string($1)), $2, nao); }
    | '{' IDENT '}'             { $$ = list(var, intern(string($2)), nao); }
    | '{' IDENT '}' elem        { $$ = list(var, intern(string($2)), $4, nao); }
    | '{' IDENT regex '}'       { $$ = list(var, intern(string($2)),
                                            nil, cons(regex_compile($3), $3),
                                            nao); }
    | '{' IDENT NUMBER '}'      { $$ = list(var, intern(string($2)),
                                            nil, num($3), nao); }
    | var_op IDENT              { $$ = list(var, intern(string($2)),
                                            nil, $1, nao); }
    | var_op IDENT elem         { $$ = list(var, intern(string($2)),
                                            $3, $1, nao); }
    | var_op '{' IDENT '}'      { $$ = list(var, intern(string($3)),
                                            nil, $1, nao); }
    | var_op '{' IDENT '}' elem { $$ = list(var, intern(string($3)),
                                            $5, $1, nao); }
    | IDENT error               { $$ = nil;
                                  yybadtoken(yychar, "variable spec"); }
    | var_op error              { $$ = nil;
                                  yybadtoken(yychar, "variable spec"); }
    ;

var_op : '*'                    { $$ = t; }
       ;

list : '(' exprs ')'            { $$ = $2; }
     | '(' ')'                  { $$ = nil; }
     | '(' error                { $$ = nil;
                                  yybadtoken(yychar, "list expression"); }
     ;

exprs : expr                    { $$ = cons($1, nil); }
      | expr exprs              { $$ = cons($1, $2); }
      | expr '.' expr           { $$ = cons($1, $3); }
      ;

expr : IDENT                    { $$ = intern(string($1)); }
     | NUMBER                   { $$ = num($1); }
     | list                     { $$ = $1; }
     | regex                    { $$ = cons(regex_compile($1), $1); }
     ;

regex : '/' regexpr '/'         { $$ = $2; }
      | '/' '/'                 { $$ = nil; }
      | '/' error               { $$ = nil;
                                  yybadtoken(yychar, "regex"); }
      ;

regexpr : regbranch                     { $$ = $1; }
        | regbranch '|' regbranch       { $$ = list(list(or, $1,
                                                         $3, nao), nao); }
        ;

regbranch : regterm             { $$ = cons($1, nil); }
          | regterm regbranch   { $$ = cons($1, $2); }
          ;

regterm : '[' regclass ']'      { $$ = cons(set, $2); }
        | '[' '^' regclass ']'  { $$ = cons(cset, $3); }
        | '.'                   { $$ = wild; }
        | '^'                   { $$ = chr('^'); }
        | ']'                   { $$ = chr(']'); }
        | '-'                   { $$ = chr('-'); }
        | regterm '*'           { $$ = list(zeroplus, $1, nao); }
        | regterm '+'           { $$ = list(oneplus, $1, nao); }
        | regterm '?'           { $$ = list(optional, $1, nao); }
        | REGCHAR               { $$ = chr($1); }
        | '(' regexpr ')'       { $$ = cons(compound, $2); }
        | '(' error             { $$ = nil;
                                  yybadtoken(yychar, "regex subexpression"); }
        | '[' error             { $$ = nil;
                                  yybadtoken(yychar, "regex character class"); }
        ;

regclass : regclassterm                 { $$ = cons($1, nil); }
         | regclassterm regclass        { $$ = cons($1, $2); }
         ;

regclassterm : regrange         { $$ = $1; }
             | regchar          { $$ = chr($1); }
             ;

regrange : regchar '-' regchar  { $$ = cons(chr($1), chr($3)); }

regchar : '?'                   { $$ = '?'; }
        | '.'                   { $$ = '.'; }
        | '*'                   { $$ = '*'; }
        | '+'                   { $$ = '+'; }
        | '('                   { $$ = '('; }
        | ')'                   { $$ = ')'; }
        | '^'                   { $$ = '^'; }
        | '|'                   { $$ = '|'; }
        | REGCHAR               { $$ = $1; }
        ;

newl : '\n'
     | error '\n'       { yyerror("newline expected after directive");
                          yyerrok; }
     ;

%%

obj_t *repeat_rep_helper(obj_t *sym, obj_t *main, obj_t *parts)
{
  obj_t *single_parts = nil;
  obj_t *first_parts = nil;
  obj_t *last_parts = nil;
  obj_t *empty_parts = nil;
  obj_t *iter;

  for (iter = parts; iter != nil; iter = cdr(iter)) {
    obj_t *part = car(iter);
    obj_t *sym = car(part);
    obj_t *clauses = cdr(part);

    if (sym == single)
      single_parts = nappend2(single_parts, clauses);
    else if (sym == frst)
      first_parts = nappend2(first_parts, clauses);
    else if (sym == lst)
      last_parts = nappend2(last_parts, clauses);
    else if (sym == empty)
      empty_parts = nappend2(empty_parts, clauses);
    else
      abort();
  }

  return list(sym, main, single_parts, first_parts,
              last_parts, empty_parts, nao);
}

obj_t *get_spec(void)
{
  return parsed_spec;
}

void dump_shell_string(const char *str)
{
  int ch;

  putchar('"');
  while ((ch = *str++) != 0) {
    switch (ch) {
    case '"': case '`': case '$': case '\\': case '\n':
      putchar('\\');
      /* fallthrough */
    default:
      putchar(ch);
    }
  }
  putchar('"');
}

void dump_var(const char *name, char *pfx1, size_t len1,
              char *pfx2, size_t len2, obj_t *value, int level)
{
  if (len1 >= 112 || len2 >= 112)
    abort();

  if (stringp(value)) {
    fputs(name, stdout);
    fputs(pfx1, stdout);
    fputs(pfx2, stdout);
    putchar('=');
    dump_shell_string(c_str(value));
    putchar('\n');
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

      dump_var(name, pfx1, len1 + add1, pfx2, len2 + add2, car(iter), level + 1);
    }
  }
}

void dump_bindings(obj_t *bindings)
{
  if (opt_loglevel >= 2) {
    fputs("raw_bindings:\n", stderr);
    dump(bindings, stderr);
  }

  while (bindings) {
    char pfx1[128], pfx2[128];
    obj_t *var = car(car(bindings));
    obj_t *value = cdr(car(bindings));
    const char *name = c_str(symbol_name(var));
    *pfx1 = 0; *pfx2 = 0;
    dump_var(name, pfx1, 0, pfx2, 0, value, 0);
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
      yyerrorf(2, "bind variable mismatch: %s", c_str(symbol_name(pattern)));
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
  yyerrorlf(2, c_num(spec_lineno),                                      \
            "%s mismatch, position %ld (%s:%ld)", (KIND), c_num(pos),   \
            c_str(file), c_num(data_lineno));                           \
  yyerrorlf(2, c_num(spec_lineno), "  %s", c_str(dataline));            \
  if (c_num(pos) < 77)                                                  \
    yyerrorlf(2, c_num(spec_lineno), "  %*s^", (int) c_num(pos), "")

#define LOG_MATCH(KIND, EXTENT)                                         \
  yyerrorlf(2, c_num(spec_lineno),                                      \
            "%s matched, position %ld-%ld (%s:%ld)", (KIND),            \
            c_num(pos), c_num(EXTENT), c_str(file),                     \
            c_num(data_lineno));                                        \
  yyerrorlf(2, c_num(spec_lineno), "  %s", c_str(dataline));            \
  if (c_num(EXTENT) < 77)                                               \
    yyerrorlf(2, c_num(spec_lineno), "  %*s%-*s^", (int) c_num(pos),    \
              "", (int) (c_num(EXTENT) - c_num(pos)), "^")
  for (;;) {
    obj_t *elem;

    if (specline == nil)
      break;

    elem = first(specline);

    switch (elem ? elem->t.type : 0) {
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

              if (c_num(past) > c_num(length_str(dataline)) ||
                  c_num(past) < c_num(pos))
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
              if (c_num(past) > c_num(length_str(dataline)) ||
                  c_num(past) < c_num(pos))
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
          } else if (pat->t.type == STR) {
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

            if (!pair) {
              yyerrorlf(1, c_num(spec_lineno), "consecutive unbound variables");
              return nil;
            }

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
            yyerrorlf(0, c_num(spec_lineno), "variable followed by invalid element");
            return nil;
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
              assert (c_num(pos) <= c_num(length_str(dataline)));
            } else {
              pos = plus(pos, one);
            }

            if (c_num(pos) >= c_num(length_str(dataline)))
              break;
          }


          if (!bindings_coll)
            yyerrorlf(2, c_num(spec_lineno), "nothing was collected");

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
          yyerrorlf(0, c_num(spec_lineno), "unknown directive: %s",
                    c_str(symbol_name(directive)));
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
      yyerrorlf(0, c_num(spec_lineno), "unsupported object in spec");
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

    if (consp(elem) && first(elem) == var) {
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
    }

    list_collect(iter, elem);
    spec = cdr(spec);
  }

  return out;
}

typedef struct fpip {
  FILE *f;
  DIR *d;
  enum { fpip_fclose, fpip_pclose, fpip_closedir } close;
} fpip_t;

fpip_t complex_open(obj_t *name, obj_t *output)
{
  fpip_t ret = { 0 };

  const char *namestr = c_str(name);
  long len = c_num(length_str(name));

  if (len == 0)
    return ret;

  if (!strcmp(namestr, "-")) {
    ret.close = fpip_fclose;
    ret.f = output ? stdout : stdin;
    output_produced = output ? 1 : 0;
  } else if (namestr[0] == '!') {
    ret.close = fpip_pclose;
    ret.f = popen(namestr+1, output ? "w" : "r");
  } else if (namestr[0] == '$') {
    if (output)
      return ret;
    ret.close = fpip_closedir;
    ret.d = opendir(namestr+1);
  } else {
    ret.close = fpip_fclose;
    ret.f = fopen(namestr, output ? "w" : "r");
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

  abort();
}

obj_t *complex_snarf(fpip_t fp, obj_t *name)
{
  switch (fp.close) {
  case fpip_fclose:
    return lazy_stream_cons(stdio_line_stream(fp.f, name));
  case fpip_pclose:
    return lazy_stream_cons(pipe_line_stream(fp.f, name));
  case fpip_closedir:
    return lazy_stream_cons(dirent_stream(fp.d, name));
  }

  abort();
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
                    obj_t *spec_lineno, FILE *out)
{
  for (; specline; specline = rest(specline)) {
    obj_t *elem = first(specline);

    switch (elem ? elem->t.type : 0) {
    case CONS:
      {
        obj_t *directive = first(elem);

        if (directive == var) {
          obj_t *str = cat_str(subst_vars(cons(elem, nil), bindings), nil);
          if (str == nil) {
            yyerrorlf(1, c_num(spec_lineno), "bad substitution: %s",
                      c_str(symbol_name(second(elem))));
            continue;
          }
          fputs(c_str(str), out);
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
          yyerrorlf(0, c_num(spec_lineno), "unknown directive: %s",
                    c_str(symbol_name(directive)));
        }
      }
      break;
    case STR:
      fputs(c_str(elem), out);
      break;
    case 0:
      break;
    default:
      yyerrorlf(0, c_num(spec_lineno), "unsupported object in output spec");
    }
  }
}

void do_output(obj_t *bindings, obj_t *specs, FILE *out)
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
    putc('\n', out);
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
    obj_t *name = first(files);
    fpip_t fp = (errno = 0, complex_open(name, nil));

    yyerrorf(2, "opening data source %s", c_str(name));

    if (complex_open_failed(fp)) {
      if (errno != 0)
        yyerrorf(2, "could not open %s: %s", c_str(name), strerror(errno));
      else
        yyerrorf(2, "could not open %s", c_str(name));
      return nil;
    }

    if ((data = complex_snarf(fp, name)) != nil)
      data_lineno = 1;
  }

  for (; spec;  spec = rest(spec), data = rest(data), data_lineno++)
repeat_spec_same_data:
  {
    obj_t *specline = rest(first(spec));
    obj_t *dataline = first(data);
    obj_t *spec_linenum = first(first(spec));
    obj_t *first_spec = first(specline);
    long spec_lineno = spec_linenum ? c_num(spec_linenum) : 0;

    if (consp(first_spec)) {
      obj_t *sym = first(first_spec);

      if (sym == skip) {
        obj_t *max = first(rest(first_spec));
        long cmax = nump(max) ? c_num(max) : 0;
        long reps = 0;

        if (rest(specline))
          yyerrorlf(1, spec_lineno, "material after skip directive ignored");

        if ((spec = rest(spec)) == nil)
          break;

        {
          uw_block_begin(nil, result);

          while (dataline && (!max || reps++ < cmax)) {
            cons_bind (new_bindings, success,
                       match_files(spec, files, bindings,
                                   data, num(data_lineno)));

            if (success) {
              yyerrorlf(2, spec_lineno, "skip matched %s:%ld",
                        c_str(first(files)), data_lineno);
              result = cons(new_bindings, cons(data, num(data_lineno)));
              break;
            }

            yyerrorlf(2, spec_lineno, "skip didn't match %s:%ld",
                      c_str(first(files)), data_lineno);
            data = rest(data);
            data_lineno++;
            dataline = first(data);
          }

          uw_block_end;

          if (result)
            return result;
        }

        yyerrorlf(2, spec_lineno, "skip failed");
        return nil;
      } else if (sym == trailer) {
        if (rest(specline))
          yyerrorlf(1, spec_lineno, "material after trailer directive ignored");
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
      } else if (sym == block) {
        obj_t *name = first(rest(first_spec));
        if (rest(specline))
          yyerrorlf(1, spec_lineno, "material after block directive ignored");
        if ((spec = rest(spec)) == nil)
          break;
        uw_block_begin(name, result);
        result = match_files(spec, files, bindings, data, num(data_lineno));
        uw_block_end;
        return result;
      } else if (sym == fail || sym == accept) {
        obj_t *target = first(rest(first_spec));

        if (rest(specline))
          yyerrorlf(1, spec_lineno, "material after %s ignored",
                    c_str(symbol_name(sym)));

        uw_block_return(target,
                        if2(sym == accept,
                            cons(bindings,
                                 if3(data, cons(data, num(data_lineno)), t))));
        if (target)
          yyerrorlf(1, spec_lineno, "%s: no block named %s in scope",
                    c_str(symbol_name(sym)), c_str(symbol_name(target)));
        else
          yyerrorlf(1, spec_lineno, "%s: not anonymous block in scope",
                    c_str(symbol_name(sym)));

        return nil;
      } else if (sym == next) {
        if (rest(first_spec))
          yyerrorlf(0, spec_lineno, "next takes no args");

        if ((spec = rest(spec)) == nil)
          break;

        if (rest(specline)) {
          obj_t *sub = subst_vars(rest(specline), bindings);
          obj_t *str = cat_str(sub, nil);
          if (str == nil) {
            yyerrorlf(2, spec_lineno, "bad substitution in next file spec");
            continue;
          }
          files = cons(str, files);
        } else {
          files = rest(files);
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
      } else if (sym == some || sym == all || sym == none || sym == maybe) {
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
          } else {
            all_match = nil;
          }
        }

        if (sym == all && !all_match) {
          yyerrorlf(2, spec_lineno, "all: some clauses didn't match");
          return nil;
        }

        if (sym == some && !some_match) {
          yyerrorlf(2, spec_lineno, "some: no clauses matched");
          return nil;
        }

        if (sym == none && some_match) {
          yyerrorlf(2, spec_lineno, "none: some clauses matched");
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
            yyerrorlf(2, spec_lineno, "collect matched %s:%ld",
                      c_str(first(files)), data_lineno);

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
              yyerrorlf(2, spec_lineno,
                        "collect advancing from line %ld to %ld",
                        data_lineno, c_num(cdr(success)));
              data = car(success);
              data_lineno = c_num(cdr(success));
            } else {
              yyerrorlf(2, spec_lineno, "collect consumed entire file");
              data = nil;
              break;
            }
          } else {
            data = rest(data);
            data_lineno++;
          }
        }

        uw_block_end;

        if (!result) {
          yyerrorlf(2, spec_lineno, "collect explicitly failed");
          return nil;
        }

        if (!bindings_coll)
          yyerrorlf(2, spec_lineno, "nothing was collected");

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
            yyerrorlf(1, spec_lineno, "non-symbol in flatten directive");
            continue;
          } else {
            obj_t *existing = assoc(bindings, sym);

            if (existing)
              *cdr_l(existing) = flatten(cdr(existing));
          }
        }

        if ((spec = rest(spec)) == nil)
          break;

        goto repeat_spec_same_data;
      } else if (sym == forget) {
        bindings = alist_remove(bindings, rest(first_spec));

        if ((spec = rest(spec)) == nil)
          break;

        goto repeat_spec_same_data;
      } else if (sym == mrge) {
        obj_t *target = first(rest(first_spec));
        obj_t *args = rest(rest(first_spec));
        obj_t *exists = assoc(bindings, target);
        obj_t *merged = nil;

        if (!target || !symbolp(target))
          yyerrorlf(1, spec_lineno, "bad merge directive");

        if (exists)
          yyerrorlf(1, spec_lineno, "merge: symbol %s already bound",
                    c_str(symbol_name(target)));

        for (; args; args = rest(args)) {
          obj_t *other_sym = first(args);

          if (other_sym) {
            obj_t *other_lookup = assoc(bindings, other_sym);

            if (!symbolp(other_sym))
              yyerrorlf(1, spec_lineno, "non-symbol in merge directive");
            else if (!other_lookup)
              yyerrorlf(1, spec_lineno, "merge: nonexistent symbol %s",
                        c_str(symbol_name(sym)));

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
        obj_t *var = second(args);
        obj_t *lookup = assoc(bindings, var);

        if (!var || !symbolp(var))
          yyerrorlf(1, spec_lineno, "bind: bad variable spec");
        else if (!lookup)
          yyerrorlf(1, spec_lineno, "bind: unbound source variable");

        bindings = dest_bind(bindings, pattern, cdr(lookup));

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
            yyerrorlf(1, spec_lineno, "non-symbol in cat directive");
            continue;
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
        obj_t *dest_opt = third(first_spec);
        obj_t *dest = dest_opt ? cat_str(subst_vars(dest_opt, bindings), nil)
                               : string(chk_strdup("-"));
        fpip_t fp = (errno = 0, complex_open(dest, t));

        yyerrorf(2, "opening data sink %s", c_str(dest));

        if (complex_open_failed(fp)) {
          if (errno != 0)
            yyerrorf(2, "could not open %s: %s", c_str(dest), strerror(errno));
          else
            yyerrorf(2, "could not open %s", c_str(dest));
        } else {
          do_output(bindings, specs, fp.f);
          complex_close(fp);
        }

        if ((spec = rest(spec)) == nil)
          break;

        goto repeat_spec_same_data;
      }
    }

    if (dataline == nil)
      return nil;

    {
      cons_bind (new_bindings, success,
                 match_line(bindings, specline, dataline, zero,
                            spec_linenum, num(data_lineno), first(files)));

      if (nump(success) && c_num(success) < c_num(length_str(dataline))) {
        yyerrorf(2, "spec only matches line to position %ld: %s",
                 c_num(success), c_str(dataline));
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
      puts("false");
  }

  return success ? 0 : EXIT_FAILURE;
}
