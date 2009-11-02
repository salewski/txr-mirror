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
#include <assert.h>
#include <limits.h>
#include <dirent.h>
#include "lib.h"
#include "regex.h"
#include "parser.h"

int yylex(void);
void yyerror(const char *);

obj_t *repeat_rep_helper(obj_t *sym, obj_t *main, obj_t *parts);
obj_t *define_transform(obj_t *define_form);
obj_t *lit_char_helper(obj_t *litchars);

static obj_t *parsed_spec;

%}

%union {
  char *lexeme;
  union obj *obj;
  char chr;
  long num;
}

%token <lexeme> TEXT IDENT ALL SOME NONE MAYBE CASES AND OR END COLLECT
%token <lexeme> UNTIL COLL OUTPUT REPEAT REP SINGLE FIRST LAST EMPTY DEFINE
%token <lexeme> TRY CATCH FINALLY
%token <num> NUMBER
%token <chr> REGCHAR LITCHAR

%type <obj> spec clauses clauses_opt clause
%type <obj> all_clause some_clause none_clause maybe_clause
%type <obj> cases_clause collect_clause clause_parts additional_parts
%type <obj> output_clause define_clause try_clause catch_clauses_opt
%type <obj> line elems_opt elems elem var var_op
%type <obj> list exprs expr out_clauses out_clauses_opt out_clause
%type <obj> repeat_clause repeat_parts_opt o_line
%type <obj> o_elems_opt o_elems_opt2 o_elems o_elem rep_elem rep_parts_opt
%type <obj> regex regexpr regbranch
%type <obj> regterm regclass regclassterm regrange
%type <obj> strlit chrlit quasilit quasi_items quasi_item litchars
%type <chr> regchar
%nonassoc ALL SOME NONE MAYBE CASES AND OR END COLLECT UNTIL COLL
%nonassoc OUTPUT REPEAT REP FIRST LAST EMPTY DEFINE
%nonassoc '{' '}' '[' ']' '(' ')'
%right IDENT TEXT NUMBER
%left '|' '/'
%right '*' '?' '+'
%right '^' '.' '\\' REGCHAR LITCHAR

%%

spec : clauses                  { parsed_spec = $1; }
     | /* empty */              { parsed_spec = nil; }
     | error                    { parsed_spec = nil;
                                  yybadtoken(yychar, 0); }
     ;

clauses : clause                { $$ = cons($1, nil); }
        | clause clauses        { $$ = cons($1, $2);  }
        ;

clauses_opt : clauses           { $$ = $1; }
            | /* empty */       { $$ = nil; }
            ;

clause : all_clause             { $$ = list(num(lineno - 1), $1, nao); }
       | some_clause            { $$ = list(num(lineno - 1), $1, nao); }
       | none_clause            { $$ = list(num(lineno - 1), $1, nao); }
       | maybe_clause           { $$ = list(num(lineno - 1), $1, nao); }
       | cases_clause           { $$ = list(num(lineno - 1), $1, nao); }
       | collect_clause         { $$ = list(num(lineno - 1), $1, nao); }
       | define_clause          { $$ = list(num(lineno - 1),
                                            define_transform($1), nao); }
       | try_clause             { $$ = list(num(lineno - 1), $1, nao); }
       | output_clause          { $$ = list(num(lineno - 1), $1, nao); }
       | line                   { $$ = $1; }
       | repeat_clause          { $$ = nil;
                                  yyerror("repeat outside of output"); }
       ;

all_clause : ALL newl clause_parts      { $$ = cons(all, $3); }
           | ALL newl error             { $$ = nil;
                                          yybadtoken(yychar,
                                                     "all clause"); }
           | ALL newl END newl          { $$ = nil;
                                          yyerror("empty all clause"); }

           ;

some_clause : SOME newl clause_parts    { $$ = cons(some, $3); }
            | SOME newl error           { $$ = nil;
                                          yybadtoken(yychar,
                                                     "some clause"); }
            | SOME newl END newl       { $$ = nil;
                                          yyerror("empty some clause"); }
            ;

none_clause : NONE newl clause_parts    { $$ = cons(none, $3); }
            | NONE newl error           { $$ = nil;
                                          yybadtoken(yychar,
                                                     "none clause"); }
            | NONE newl END newl        { $$ = nil;
                                          yyerror("empty none clause"); }
            ;

maybe_clause : MAYBE newl clause_parts  { $$ = cons(maybe, $3); }
             | MAYBE newl error         { $$ = nil;
                                          yybadtoken(yychar,
                                                     "maybe clause"); }
             | MAYBE newl END newl      { $$ = nil;
                                          yyerror("empty maybe clause"); }
             ;

cases_clause : CASES newl clause_parts  { $$ = cons(cases, $3); }
             | CASES newl error         { $$ = nil;
                                          yybadtoken(yychar,
                                                     "cases clause"); }
             | CASES newl END newl      { $$ = nil;
                                          yyerror("empty cases clause"); }
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

elem : TEXT                     { $$ = string_own($1); }
     | var                      { $$ = $1; }
     | list                     { $$ = $1; }
     | regex                    { $$ = cons(regex_compile($1), $1); }
     | COLL elems END           { $$ = list(coll, $2, nao); }
     | COLL elems
       UNTIL elems END          { $$ = list(coll, $2, $4, nao); }
     | COLL error               { $$ = nil;
                                  yybadtoken(yychar, "coll clause"); }
     ;

define_clause : DEFINE exprs ')' newl
                clauses_opt
                END newl        { $$ = list(define, $2, $5, nao); }
              | DEFINE ')' newl
                clauses_opt
                END newl        { $$ = list(define, nil, $4, nao); }
              | DEFINE error    { yybadtoken(yychar, "list expression"); }
              | DEFINE exprs ')' newl
                error           { yybadtoken(yychar, "define"); }
              | DEFINE ')' newl
                error           { yybadtoken(yychar, "define"); }
              ;

try_clause : TRY newl
             clauses
             catch_clauses_opt
             END newl           { $$ = list(try,
                                            flatten(mapcar(func_n1(second),
                                                           $4)),
                                            $3, $4, nao); }
           | TRY newl
             error              { $$ = nil;
                                  if (yychar == END || yychar == CATCH ||
                                      yychar == FINALLY)
                                    yyerror("empty try clause");
                                  else
                                    yybadtoken(yychar, "try clause"); }
           | TRY newl
             clauses
             error              { $$ = nil;
                                  yybadtoken(yychar, "try clause"); }
           ;

catch_clauses_opt : CATCH ')' newl
                    clauses_opt
                    catch_clauses_opt   { $$ = cons(list(catch, cons(t, nil),
                                                         $4, nao), $5); }
                  | CATCH exprs ')' newl
                    clauses_opt
                    catch_clauses_opt   { $$ = cons(list(catch, $2, $5, nao),
                                                    $6); }
                  | FINALLY newl
                    clauses_opt         { $$ = cons(list(finally, nil,
                                                         $3, nao),
                                                    nil); }
                  |                     { $$ = nil; }
                  | CATCH ')' newl
                    error               { yybadtoken(yychar, "try clause"); }
                  | CATCH exprs ')' newl
                    error               { yybadtoken(yychar, "try clause"); }
                  | FINALLY newl
                    error               { yybadtoken(yychar, "try clause"); }
                  ;


output_clause : OUTPUT ')' o_elems '\n'
                out_clauses
                END newl        { $$ = list(output, $5, $3, nao); }
              | OUTPUT ')' newl
                out_clauses
                END newl        { $$ = list(output, $4, nao); }
              | OUTPUT exprs ')' newl
                out_clauses
                END newl        { $$ = list(output, $5, nil, $2, nao); }
              | OUTPUT exprs ')' o_elems '\n'
                out_clauses
                END newl        { yyerror("invalid combination of old and "
                                          "new syntax in output directive"); }
              | OUTPUT error    { yybadtoken(yychar, "list expression"); }
              | OUTPUT ')' o_elems '\n'
                error           { $$ = nil;
                                  yybadtoken(yychar, "output clause"); }
              | OUTPUT ')' newl
                error           { $$ = nil;
                                  yybadtoken(yychar, "output clause"); }
              | OUTPUT exprs ')' o_elems '\n'
                error           { $$ = nil;
                                  yybadtoken(yychar, "output clause"); }
              | OUTPUT exprs ')' newl
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
           | cases_clause               { $$ = nil;
                                          yyerror("match clause in output"); }
           | collect_clause             { $$ = nil;
                                          yyerror("match clause in output"); }
           | define_clause              { $$ = nil;
                                          yyerror("match clause in output"); }

           | try_clause                 { $$ = nil;
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

o_elem : TEXT                   { $$ = string_own($1); }
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
var : IDENT                     { $$ = list(var, intern(string_own($1)),
                                            nao); }
    | IDENT elem                { $$ = list(var, intern(string_own($1)),
                                            $2, nao); }
    | '{' IDENT '}'             { $$ = list(var, intern(string_own($2)),
                                            nao); }
    | '{' IDENT '}' elem        { $$ = list(var, intern(string_own($2)),
                                            $4, nao); }
    | '{' IDENT regex '}'       { $$ = list(var, intern(string_own($2)),
                                            nil, cons(regex_compile($3), $3),
                                            nao); }
    | '{' IDENT NUMBER '}'      { $$ = list(var, intern(string_own($2)),
                                            nil, num($3), nao); }
    | var_op IDENT              { $$ = list(var, intern(string_own($2)),
                                            nil, $1, nao); }
    | var_op IDENT elem         { $$ = list(var, intern(string_own($2)),
                                            $3, $1, nao); }
    | var_op '{' IDENT '}'      { $$ = list(var, intern(string_own($3)),
                                            nil, $1, nao); }
    | var_op '{' IDENT '}' elem { $$ = list(var, intern(string_own($3)),
                                            $5, $1, nao); }
    | var_op '{' IDENT regex '}'        { yyerror("longest match "
                                                  "not useable with regex"); }
    | var_op '{' IDENT NUMBER '}'       { yyerror("longest match "
                                                  "not useable with "
                                                  "fixed width match"); }
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

expr : IDENT                    { $$ = intern(string_own($1)); }
     | NUMBER                   { $$ = num($1); }
     | list                     { $$ = $1; }
     | regex                    { $$ = cons(regex_compile($1), $1); }
     | chrlit                   { $$ = $1; }
     | strlit                   { $$ = $1; }
     | quasilit                 { $$ = $1; }
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

strlit : '"' '"'                { $$ = null_string; }
       | '"' litchars '"'       { $$ = lit_char_helper($2); }
       | '"' error              { yybadtoken(yychar, "string literal"); }
       ;

chrlit : '\'' '\''              { yyerror("empty character literal"); }
                                { $$ = nil; }
       | '\'' litchars '\''     { $$ = car($2);
                                  if (cdr($2))
                                    yyerror("multiple characters in "
                                            "character literal"); }
       | '\'' error             { $$ = nil;
                                  yybadtoken(yychar, "character literal"); }
       ;

quasilit : '`' '`'              { $$ = null_string; }
         | '`' quasi_items '`'  { $$ = cons(quasi, $2); }
         | '`' error            { $$ = nil;
                                  yybadtoken(yychar, "string literal"); }
         ;

quasi_items : quasi_item                { $$ = cons($1, nil); }
            | quasi_item quasi_items    { $$ = cons($1, $2); }
            ;

quasi_item : litchars           { $$ = lit_char_helper($1); }
           | TEXT               { $$ = string_own($1); }
           | var                { $$ = $1; }
           | list               { $$ = $1; }
           ;

litchars : LITCHAR              { $$ = cons(chr($1), nil); }
         | LITCHAR litchars     { $$ = cons(chr($1), $2); }
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

obj_t *define_transform(obj_t *define_form)
{
  obj_t *sym = first(define_form);
  obj_t *args = second(define_form);

  if (define_form == nil)
    return nil;

  assert (sym == define);

  if (args == nil) {
    yyerror("define requires arguments");
    return define_form;
  }

  if (!consp(args) || !listp(cdr(args))) {
    yyerror("bad define argument syntax");
    return define_form;
  } else {
    obj_t *name = first(args);
    obj_t *params = second(args);

    if (!symbolp(name)) {
      yyerror("function name must be a symbol");
      return define_form;
    }

    if (!proper_listp(params)) {
      yyerror("invalid function parameter list");
      return define_form;
    }

    if (!all_satisfy(params, func_n1(symbolp), nil))
      yyerror("function parameters must be symbols");
  }

  return define_form;
}

obj_t *lit_char_helper(obj_t *litchars)
{
  obj_t *ret = nil;

  if (litchars) {
    obj_t *len = length(litchars), *iter, *ix;
    ret = mkustring(len);
    for (iter = litchars, ix = zero;
        iter;
        iter = cdr(iter), ix = plus(ix, one))
    {
      chr_str_set(ret, ix, car(iter));
    }
  } else {
    ret = nil;
  }
  return ret;
}

obj_t *get_spec(void)
{
  return parsed_spec;
}

