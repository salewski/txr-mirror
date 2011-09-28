/* Copyright 2011
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
#include <stdlib.h>
#include <wchar.h>
#include "config.h"
#include "lib.h"
#include "regex.h"
#include "utf8.h"
#include "parser.h"

int yylex(void);
void yyerror(const char *);

val repeat_rep_helper(val sym, val main, val parts);
val o_elems_transform(val output_form);
val define_transform(val define_form);
val lit_char_helper(val litchars);

static val parsed_spec;

%}

%union {
  wchar_t *lexeme;
  union obj *obj;
  wchar_t chr;
  cnum num;
}

%token <lexeme> TEXT IDENT KEYWORD ALL SOME NONE MAYBE CASES AND OR END COLLECT
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
%nonassoc LOW /* used for precedence assertion */
%nonassoc ALL SOME NONE MAYBE CASES AND OR END COLLECT UNTIL COLL
%nonassoc OUTPUT REPEAT REP FIRST LAST EMPTY DEFINE
%nonassoc '[' ']' '(' ')'
%right IDENT TEXT NUMBER '{' '}'
%left '-'
%left '|' '/'
%left '&' 
%right '~' '*' '?' '+' '%'
%right '.' '\\' REGCHAR LITCHAR

%%

spec : clauses                  { parsed_spec = $1; }
     | /* empty */              { parsed_spec = nil; }
     | error                    { parsed_spec = nil;
                                  yybadtoken(yychar, nil); }
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

all_clause : ALL newl clause_parts      { $$ = cons(all_s, $3); }
           | ALL newl error             { $$ = nil;
                                          yybadtoken(yychar,
                                                     lit("all clause")); }
           | ALL newl END newl          { $$ = nil;
                                          yyerror("empty all clause"); }

           ;

some_clause : SOME newl clause_parts    { $$ = cons(some_s, $3); }
            | SOME newl error           { $$ = nil;
                                          yybadtoken(yychar,
                                                     lit("some clause")); }
            | SOME newl END newl       { $$ = nil;
                                          yyerror("empty some clause"); }
            ;

none_clause : NONE newl clause_parts    { $$ = cons(none_s, $3); }
            | NONE newl error           { $$ = nil;
                                          yybadtoken(yychar,
                                                     lit("none clause")); }
            | NONE newl END newl        { $$ = nil;
                                          yyerror("empty none clause"); }
            ;

maybe_clause : MAYBE newl clause_parts  { $$ = cons(maybe_s, $3); }
             | MAYBE newl error         { $$ = nil;
                                          yybadtoken(yychar,
                                                     lit("maybe clause")); }
             | MAYBE newl END newl      { $$ = nil;
                                          yyerror("empty maybe clause"); }
             ;

cases_clause : CASES newl clause_parts  { $$ = cons(cases_s, $3); }
             | CASES newl error         { $$ = nil;
                                          yybadtoken(yychar,
                                                     lit("cases clause")); }
             | CASES newl END newl      { $$ = nil;
                                          yyerror("empty cases clause"); }
             ;

collect_clause : COLLECT newl clauses END newl  { $$ = list(collect_s,
                                                            $3, nao); }
               | COLLECT newl clauses
                 UNTIL newl clauses END newl    { $$ = list(collect_s, $3,
                                                            $6, nao); }
               | COLLECT newl error     { $$ = nil;
                                          if (yychar == UNTIL || yychar == END)
                                            yyerror("empty collect");
                                          else
                                            yybadtoken(yychar,
                                                       lit("collect clause")); }
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
     | regex                    { $$ = cons(regex_compile(rest($1)),
                                            rest($1)); }
     | COLL elems END           { $$ = list(coll_s, $2, nao); }
     | COLL elems
       UNTIL elems END          { $$ = list(coll_s, $2, $4, nao); }
     | COLL error               { $$ = nil;
                                  yybadtoken(yychar, lit("coll clause")); }
     ;

define_clause : DEFINE exprs ')' newl
                clauses_opt
                END newl        { $$ = list(define_s, $2, $5, nao); }
              | DEFINE ')' newl
                clauses_opt
                END newl        { $$ = list(define_s, nil, $4, nao); }
              | DEFINE error    { $$ = nil;
                                  yybadtoken(yychar, lit("list expression")); }
              | DEFINE exprs ')' newl
                error           { $$ = nil; yybadtoken(yychar, lit("define")); }
              | DEFINE ')' newl
                error           { $$ = nil;
                                  yybadtoken(yychar, lit("define")); }
              ;

try_clause : TRY newl
             clauses
             catch_clauses_opt
             END newl           { $$ = list(try_s,
                                            flatten(mapcar(func_n1(second),
                                                           $4)),
                                            $3, $4, nao); }
           | TRY newl
             error              { $$ = nil;
                                  if (yychar == END || yychar == CATCH ||
                                      yychar == FINALLY)
                                    yyerror("empty try clause");
                                  else
                                    yybadtoken(yychar, lit("try clause")); }
           | TRY newl
             clauses
             error              { $$ = nil;
                                  yybadtoken(yychar, lit("try clause")); }
           ;

catch_clauses_opt : CATCH ')' newl
                    clauses_opt
                    catch_clauses_opt   { $$ = cons(list(catch_s, cons(t, nil),
                                                         $4, nao), $5); }
                  | CATCH exprs ')' newl
                    clauses_opt
                    catch_clauses_opt   { $$ = cons(list(catch_s, $2, $5, nao),
                                                    $6); }
                  | FINALLY newl
                    clauses_opt         { $$ = cons(list(finally_s, nil,
                                                         $3, nao),
                                                    nil); }
                  |                     { $$ = nil; }
                  | CATCH ')' newl
                    error               { $$ = nil;
                                          yybadtoken(yychar,
                                                     lit("try clause")); }
                  | CATCH exprs ')' newl
                    error               { $$ = nil;
                                          yybadtoken(yychar,
                                                     lit("try clause")); }
                  | FINALLY newl
                    error               { $$ = nil;
                                          yybadtoken(yychar,
                                                     lit("try clause")); }
                  ;


output_clause : OUTPUT ')' o_elems '\n'
                out_clauses
                END newl        { $$ = nil;
                                  yyerror("obsolete output syntax: trailing material"); }
              | OUTPUT ')' newl
                END newl        { $$ = list(output_s, nao); }
              | OUTPUT ')' newl
                out_clauses
                END newl        { $$ = list(output_s, $4, nao); }
              | OUTPUT exprs ')' newl
                out_clauses
                END newl        { $$ = list(output_s, $5, $2, nao); }
              | OUTPUT exprs ')' o_elems '\n'
                out_clauses
                END newl        { $$ = nil;
                                  yyerror("invalid combination of old and "
                                          "new syntax in output directive"); }
              | OUTPUT error    { $$ = nil;
                                  yybadtoken(yychar, lit("list expression")); }
              | OUTPUT ')' o_elems '\n'
                error           { $$ = nil;
                                  yybadtoken(yychar, lit("output clause")); }
              | OUTPUT ')' newl
                error           { $$ = nil;
                                  yybadtoken(yychar, lit("output clause")); }
              | OUTPUT exprs ')' o_elems '\n'
                error           { $$ = nil;
                                  yybadtoken(yychar, lit("output clause")); }
              | OUTPUT exprs ')' newl
                error           { $$ = nil;
                                  yybadtoken(yychar, lit("output clause")); }
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
                END newl                { $$ = repeat_rep_helper(repeat_s, $3, $4); }
              | REPEAT newl
                error           { $$ = nil;
                                  yybadtoken(yychar, lit("repeat clause")); }
              ;

repeat_parts_opt : SINGLE newl
                   out_clauses_opt
                   repeat_parts_opt     { $$ = cons(cons(single_s, $3), $4); }
                 | FIRST newl
                   out_clauses_opt
                   repeat_parts_opt     { $$ = cons(cons(first_s, $3), $4); }
                 | LAST newl
                   out_clauses_opt
                   repeat_parts_opt     { $$ = cons(cons(last_s, $3), $4); }
                 | EMPTY newl
                   out_clauses_opt
                   repeat_parts_opt     { $$ = cons(cons(empty_s, $3), $4); }
                 | /* empty */          { $$ = nil; }
                 ;


out_clauses_opt : out_clauses   { $$ = $1; }
                | /* empty */   { $$ = null_list; }

o_line : o_elems_opt '\n'       { $$ = $1; }
       ;

o_elems_opt : o_elems           { $$ = cons(num(lineno - 1),
                                            o_elems_transform($1)); }
            |                   { $$ = nil; }
            ;

o_elems_opt2 : o_elems          { $$ = o_elems_transform($1); }
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
           rep_parts_opt END    { $$ = repeat_rep_helper(rep_s, $2, $3); }
         | REP error            { $$ = nil;
                                  yybadtoken(yychar, lit("rep clause")); }
         ;

rep_parts_opt : SINGLE o_elems_opt2
                rep_parts_opt           { $$ = cons(cons(single_s, $2), $3); }
              | FIRST o_elems_opt2
                rep_parts_opt           { $$ = cons(cons(first_s, $2), $3); }
              | LAST o_elems_opt2
                rep_parts_opt           { $$ = cons(cons(last_s, $2), $3); }
              | EMPTY o_elems_opt2
                rep_parts_opt           { $$ = cons(cons(empty_s, $2), $3); }
              | /* empty */             { $$ = nil; }
              ;


/* This sucks, but factoring '*' into a nonterminal
 * that generates an empty phrase causes reduce/reduce conflicts.
 */
var : IDENT                     { $$ = list(var_s, intern(string_own($1), nil),
                                            nao); }
    | IDENT elem                { $$ = list(var_s, intern(string_own($1), nil),
                                            $2, nao); }
    | '{' IDENT '}'             { $$ = list(var_s, intern(string_own($2), nil),
                                            nao); }
    | '{' IDENT '}' elem        { $$ = list(var_s, intern(string_own($2), nil),
                                            $4, nao); }
    | '{' IDENT exprs '}'       { $$ = list(var_s, intern(string_own($2), nil),
                                            nil, $3, nao); }
    | '{' IDENT exprs '}' elem  { $$ = list(var_s, intern(string_own($2), nil),
                                            $5, $3, nao); }
    | var_op IDENT              { $$ = list(var_s, intern(string_own($2), nil),
                                            nil, $1, nao); }
    | var_op IDENT elem         { $$ = list(var_s, intern(string_own($2), nil),
                                            $3, $1, nao); }
    | var_op '{' IDENT '}'      { $$ = list(var_s, intern(string_own($3), nil),
                                            nil, $1, nao); }
    | var_op '{' IDENT '}' elem { $$ = list(var_s, intern(string_own($3), nil),
                                            $5, $1, nao); }
    | var_op '{' IDENT regex '}'        { $$ = nil;
                                          yyerror("longest match "
                                                  "not useable with regex"); }
    | var_op '{' IDENT NUMBER '}'       { $$ = nil;
                                          yyerror("longest match "
                                                  "not useable with "
                                                  "fixed width match"); }
    | IDENT error               { $$ = nil;
                                  yybadtoken(yychar, lit("variable spec")); }
    | var_op error              { $$ = nil;
                                  yybadtoken(yychar, lit("variable spec")); }
    ;

var_op : '*'                    { $$ = t; }
       ;

list : '(' exprs ')'            { $$ = $2; }
     | '(' ')'                  { $$ = nil; }
     | '(' error                { $$ = nil;
                                  yybadtoken(yychar, lit("list expression")); }
     ;

exprs : expr                    { $$ = cons($1, nil); }
      | expr exprs              { $$ = cons($1, $2); }
      | expr '.' expr           { $$ = cons($1, $3); }
      ;

expr : IDENT                    { $$ = intern(string_own($1), nil); }
     | KEYWORD                  { $$ = intern(string_own($1),
                                              keyword_package); }
     | NUMBER                   { $$ = num($1); }
     | list                     { $$ = $1; }
     | regex                    { $$ = cons(regex_compile(rest($1)),
                                            rest($1)); }
     | chrlit                   { $$ = $1; }
     | strlit                   { $$ = $1; }
     | quasilit                 { $$ = $1; }
     ;

regex : '/' regexpr '/'         { $$ = cons(regex_s, $2); end_of_regex(); }
      | '/' error               { $$ = nil;
                                  yybadtoken(yychar, lit("regex"));
                                  end_of_regex(); }
      ;

regexpr : regbranch                     { $$ = if3(cdr($1), 
                                                   cons(compound_s, $1),
                                                   car($1)); }
        | regexpr '|' regexpr           { $$ = list(or_s, $1, $3, nao); }
        | regexpr '&' regexpr           { $$ = list(and_s, $1, $3, nao); }
        | '~' regexpr                   { $$ = list(compl_s, $2, nao); }
        | /* empty */ %prec LOW         { $$ = nil; }
        ;

regbranch : regterm %prec LOW   { $$ = cons($1, nil); }
          | regterm regbranch   { $$ = cons($1, $2); }
          | regterm '~' regexpr { $$ = list($1, list(compl_s, $3, nao), nao); }
          ;

regterm : regterm '*'           { $$ = list(zeroplus_s, $1, nao); }
        | regterm '+'           { $$ = list(oneplus_s, $1, nao); }
        | regterm '?'           { $$ = list(optional_s, $1, nao); }
        | regterm '%' regexpr   { $$ = list(nongreedy_s, $1, $3, nao); }
        | '[' regclass ']'      { if (first($2) == chr('^'))
                                  { if (rest($2))
                                      $$ = cons(cset_s, rest($2));
                                    else
                                      $$ = wild_s; }
                                  else
                                    $$ = cons(set_s, $2); }
        | '[' ']'               { $$ = cons(set_s, nil); }
        | '[' error             { $$ = nil;
                                  yybadtoken(yychar,
                                            lit("regex character class")); }
        | '.'                   { $$ = wild_s; }
        | ']'                   { $$ = chr(']'); }
        | '-'                   { $$ = chr('-'); }
        | REGCHAR               { $$ = chr($1); }
        | '(' regexpr ')'       { $$ = $2; }
        | '(' error             { $$ = nil;
                                  yybadtoken(yychar,
                                             lit("regex subexpression")); }
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
        | '|'                   { $$ = '|'; }
        | '~'                   { $$ = '~'; }
        | '&'                   { $$ = '&'; }
        | '%'                   { $$ = '%'; }
        | '/'                   { $$ = '/'; }
        | REGCHAR               { $$ = $1; }
        ;

newl : '\n'
     | error '\n'       { yyerror("newline expected after directive");
                          yyerrok; }
     ;

strlit : '"' '"'                { $$ = null_string; }
       | '"' litchars '"'       { $$ = lit_char_helper($2); }
       | '"' error              { $$ = nil;
                                  yybadtoken(yychar, lit("string literal")); }
       ;

chrlit : '\'' '\''              { $$ = nil;
                                  yyerror("empty character literal"); }
       | '\'' litchars '\''     { $$ = car($2);
                                  if (cdr($2))
                                    yyerror("multiple characters in "
                                            "character literal"); }
       | '\'' error             { $$ = nil;
                                  yybadtoken(yychar,
                                             lit("character literal")); }
       ;

quasilit : '`' '`'              { $$ = null_string; }
         | '`' quasi_items '`'  { $$ = cons(quasi_s, o_elems_transform($2)); }
         | '`' error            { $$ = nil;
                                  yybadtoken(yychar, lit("string literal")); }
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

val repeat_rep_helper(val sym, val main, val parts)
{
  val single_parts = nil;
  val first_parts = nil;
  val last_parts = nil;
  val empty_parts = nil;
  val iter;

  for (iter = parts; iter != nil; iter = cdr(iter)) {
    val part = car(iter);
    val sym = car(part);
    val clauses = cdr(part);

    if (sym == single_s)
      single_parts = nappend2(single_parts, clauses);
    else if (sym == first_s)
      first_parts = nappend2(first_parts, clauses);
    else if (sym == last_s)
      last_parts = nappend2(last_parts, clauses);
    else if (sym == empty_s)
      empty_parts = nappend2(empty_parts, clauses);
    else
      abort();
  }

  return list(sym, main, single_parts, first_parts,
              last_parts, empty_parts, nao);
}

val o_elems_transform(val o_elems)
{
  list_collect_decl(o_elems_out, ptail);
  val iter;

  for (iter = o_elems; iter; iter = cdr(iter)) {
    val elem = car(iter);

    while (consp(elem) && first(elem) == var_s) {
      val sym = second(elem);
      val pat = third(elem);
      val modifiers = fourth(elem);

      list_collect(ptail, list(first(elem), sym, nil, modifiers, nao));
      elem = pat;
    }

    if (elem)
      list_collect(ptail, elem);
  }
  
  return o_elems_out;
}

val define_transform(val define_form)
{
  val sym = first(define_form);
  val args = second(define_form);

  if (define_form == nil)
    return nil;

  assert (sym == define_s);

  if (args == nil) {
    yyerror("define requires arguments");
    return define_form;
  }

  if (!consp(args) || !listp(cdr(args))) {
    yyerror("bad define argument syntax");
    return define_form;
  } else {
    val name = first(args);
    val params = second(args);

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

val lit_char_helper(val litchars)
{
  val ret = nil;

  if (litchars) {
    val len = length(litchars), iter, ix;
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

val get_spec(void)
{
  return parsed_spec;
}

