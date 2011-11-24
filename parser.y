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
#include "match.h"
#include "hash.h"
#include "parser.h"

int yylex(void);
void yyerror(const char *);

static val repeat_rep_helper(val sym, val main, val parts);
static val o_elems_transform(val output_form);
static val define_transform(val define_form);
static val lit_char_helper(val litchars);
static val optimize_text(val text_form);
static wchar_t char_from_name(wchar_t *name);

static val parsed_spec;

%}

%union {
  wchar_t *lexeme;
  union obj *val;
  wchar_t chr;
  cnum num, lineno;
}

%token <lexeme> SPACE TEXT IDENT KEYWORD METAVAR
%token <lineno> ALL SOME NONE MAYBE CASES CHOOSE GATHER
%token <lineno> AND OR END COLLECT
%token <lineno> UNTIL COLL OUTPUT REPEAT REP SINGLE FIRST LAST EMPTY DEFINE
%token <lineno> TRY CATCH FINALLY
%token <lineno> ERRTOK /* deliberately not used in grammar */
%token <lineno> HASH_BACKSLASH

%token <num> NUMBER

%token <chr> REGCHAR LITCHAR
%token <chr> METAPAR SPLICE

%type <val> spec clauses clauses_opt clause
%type <val> all_clause some_clause none_clause maybe_clause
%type <val> cases_clause choose_clause gather_clause collect_clause until_last
%type <val> clause_parts additional_parts
%type <val> output_clause define_clause try_clause catch_clauses_opt
%type <val> line elems_opt elems clause_parts_h additional_parts_h
%type <val> text texts elem var var_op meta_expr
%type <val> list exprs exprs_opt expr out_clauses out_clauses_opt out_clause
%type <val> repeat_clause repeat_parts_opt o_line
%type <val> o_elems_opt o_elems_opt2 o_elems o_elem o_var rep_elem rep_parts_opt
%type <val> regex regexpr regbranch
%type <val> regterm regclass regclassterm regrange
%type <val> strlit chrlit quasilit quasi_items quasi_item litchars
%type <chr> regchar
%type <lineno> '('

%nonassoc LOW /* used for precedence assertion */
%right IDENT '{' '}'
%right ALL SOME NONE MAYBE CASES CHOOSE AND OR END COLLECT UNTIL COLL
%right OUTPUT REPEAT REP FIRST LAST EMPTY DEFINE
%right SPACE TEXT NUMBER
%nonassoc '[' ']' '(' ')'
%left '-' ',' '\'' SPLICE
%left '|' '/'
%left '&' 
%right '~' '*' '?' '+' '%'
%right '.' REGCHAR LITCHAR

%%

spec : clauses                  { parsed_spec = $1; }
     | /* empty */              { parsed_spec = nil; }
     | error '\n'               { parsed_spec = nil;
                                  if (errors >= 8)
                                    YYABORT;
                                  yyerrok;
                                  yybadtoken(yychar, nil); }

     ;

clauses : clause                { $$ = cons($1, nil); }
        | clause clauses        { $$ = cons($1, $2);  }
        ;

clauses_opt : clauses           { $$ = $1; }
            | /* empty */       { $$ = nil; }
            ;

clause : all_clause             { $$ = list($1, nao); rlcp($$, $1); }
       | some_clause            { $$ = list($1, nao); rlcp($$, $1); }
       | none_clause            { $$ = list($1, nao); rlcp($$, $1); }
       | maybe_clause           { $$ = list($1, nao); rlcp($$, $1); }
       | cases_clause           { $$ = list($1, nao); rlcp($$, $1); }
       | choose_clause          { $$ = list($1, nao); rlcp($$, $1); }
       | collect_clause         { $$ = list($1, nao); rlcp($$, $1); }
       | gather_clause          { $$ = list($1, nao); rlcp($$, $1); }
       | define_clause          { $$ = list(define_transform($1), nao);
                                  rlcp(car($$), $1);
                                  rlcp($$, $1); }
       | try_clause             { $$ = list($1, nao); rlcp($$, $1); }
       | output_clause          { $$ = list($1, nao); rlcp($$, $1); }
       | line                   { $$ = $1; }
       | repeat_clause          { $$ = nil;
                                  yyerror("repeat outside of output"); }
       ;

all_clause : ALL newl clause_parts      { $$ = list(all_s, $3, nao);
                                          rl($$, num($1)); }
           | ALL newl error             { $$ = nil;
                                          yybadtoken(yychar,
                                                     lit("all clause")); }
           | ALL newl END newl          { $$ = nil;
                                          yyerror("empty all clause"); }

           ;

some_clause : SOME exprs_opt ')'
              newl clause_parts         { $$ = list(some_s, $5, $2, nao);
                                          rl($$, num($1)); }
            | SOME exprs_opt ')'
              newl error           
                                        { $$ = nil;
                                          yybadtoken(yychar,
                                                     lit("some clause")); }
            | SOME exprs_opt ')'
              newl END newl             { $$ = nil;
                                          yyerror("empty some clause"); }
            ;

none_clause : NONE newl clause_parts    { $$ = list(none_s, $3, nao);
                                          rl($$, num($1)); }
            | NONE newl error           { $$ = nil;
                                          yybadtoken(yychar,
                                                     lit("none clause")); }
            | NONE newl END newl        { $$ = nil;
                                          yyerror("empty none clause"); }
            ;

maybe_clause : MAYBE newl clause_parts  { $$ = list(maybe_s, $3, nao);
                                          rl($$, num($1)); }
             | MAYBE newl error         { $$ = nil;
                                          yybadtoken(yychar,
                                                     lit("maybe clause")); }
             | MAYBE newl END newl      { $$ = nil;
                                          yyerror("empty maybe clause"); }
             ;

cases_clause : CASES newl clause_parts  { $$ = list(cases_s, $3, nao);
                                          rl($$, num($1)); }
             | CASES newl error         { $$ = nil;
                                          yybadtoken(yychar,
                                                     lit("cases clause")); }
             | CASES newl END newl      { $$ = nil;
                                          yyerror("empty cases clause"); }
             ;

choose_clause : CHOOSE exprs_opt ')'
                newl clause_parts       { $$ = list(choose_s, $5, $2, nao);
                                          rl($$, num($1)); }
              | CHOOSE exprs_opt ')'
                newl error              { $$ = nil;
                                          yybadtoken(yychar,
                                                     lit("choose clause")); }
              | CHOOSE exprs_opt ')'
                newl END newl           { $$ = nil;
                                          yyerror("empty choose clause"); }
              ;

gather_clause : GATHER exprs_opt ')'
                newl clause_parts       { $$ = list(gather_s, 
                                                    append2(mapcar(curry_12_1(func_n2(cons), nil),
                                                                   first($5)), rest($5)),
                                                    $2, nao); 
                                          rl($$, num($1)); }
              | GATHER exprs_opt ')'
                newl error              { $$ = nil;
                                          yybadtoken(yychar,
                                                     lit("gather clause")); }
              | GATHER exprs_opt ')'
                newl END newl           { $$ = nil;
                                          yyerror("empty gather clause"); }
              ;


collect_clause : COLLECT exprs_opt ')' newl
                 clauses END newl                { $$ = list(collect_s,
                                                             $5, nil, $2,
                                                             nao);
                                                   rl($$, num($1)); }
               | COLLECT exprs_opt ')'
                 newl clauses until_last
                 newl clauses END newl    { $$ = list(collect_s, $5,
                                                      cons(cdr($6), $8),
                                                      $2, nao);
                                            rl($$, num($1)); 
                                            rl($8, car($6)); }
               | COLLECT exprs_opt ')'
                 newl error             { $$ = nil;
                                          if (yychar == UNTIL ||
                                              yychar == END ||
                                              yychar == LAST)
                                            yyerror("empty collect");
                                          else
                                            yybadtoken(yychar,
                                                       lit("collect clause")); }
               ;

until_last : UNTIL { $$ = cons(num($1), until_s); }
           | LAST  { $$ = cons(num($1), last_s); } 
           ;

clause_parts : clauses additional_parts { $$ = cons($1, $2); }
             ;

additional_parts : END newl                             { $$ = nil; }
                 | AND newl clauses additional_parts    { $$ = cons($3, $4); }
                 | OR newl clauses additional_parts     { $$ = cons($3, $4); }
                 ;

line : elems_opt '\n'           { $$ = $1; }
     ;

elems_opt : elems               { $$ = $1; }
          |                     { $$ = nil; }
          ;

elems : elem                    { $$ = cons($1, nil); 
                                  rlcp($$, $1); }
      | elem elems              { $$ = cons($1, $2);
                                  rlcp($$, $1); }
      | rep_elem                { $$ = nil;
                                  yyerror("rep outside of output"); }
      ;


text : TEXT                     { $$ = rl(string_own($1), num(lineno)); }
     | SPACE                    { if ($1[0] == ' ' && $1[1] == 0)
                                  { val spaces = list(oneplus_s, 
                                                      chr(' '), nao);
                                    $$ = cons(regex_compile(spaces), spaces);
                                    rl($$, num(lineno));
                                    free($1); }
                                  else
                                  { $$ = rl(string_own($1), num(lineno)); }}
     | regex                    { $$ = cons(regex_compile(rest($1)),
                                            rest($1));
                                  rl($$, num(lineno)); }
     ;

texts : text %prec LOW          { $$ = rlcp(cons($1, nil), $1); }
      | text texts              { $$ = rlcp(cons($1, $2), $2); }
      ;

elem : texts                    { $$ = rlcp(cons(text_s, $1), $1);
                                  $$ = rlcp(optimize_text($$), $$); }
     | var                      { $$ = rl($1, num(lineno)); }
     | list                     { $$ = $1; }
     | COLL exprs_opt ')' elems END     { $$ = list(coll_s, $4, nil, $2, nao);
                                          rl($$, num($1)); }
     | COLL exprs_opt ')' elems
       until_last elems END     { $$ = list(coll_s, $4, cons(cdr($5), $6), 
                                            $2, nao);
                                  rl($$, num($1));
                                  rl($6, car($5)); }
     | COLL error               { $$ = nil;
                                  yybadtoken(yychar, lit("coll clause")); }
     | ALL clause_parts_h       { $$ = rl(list(all_s, t, $2, nao), num($1)); }
     | ALL END                  { yyerror("empty all clause"); }
     | SOME exprs_opt ')'
       clause_parts_h           { $$ = rl(list(some_s, t, $4, $2, nao), num($1)); }
     | SOME exprs_opt ')' END   { yyerror("empty some clause"); }
     | NONE clause_parts_h      { $$ = rl(list(none_s, t, $2, nao), num($1)); }
     | NONE END                 { yyerror("empty none clause"); }
     | MAYBE clause_parts_h     { $$ = rl(list(maybe_s, t, $2, nao), num($1)); }
     | MAYBE END                { yyerror("empty maybe clause"); }
     | CASES clause_parts_h     { $$ = rl(list(cases_s, t, $2, nao), num($1)); }
     | CASES END                { yyerror("empty cases clause"); }
     | CHOOSE exprs_opt ')'
       clause_parts_h           { $$ = list(choose_s, t, $4, $2, nao);
                                  rl($$, num($1)); }
     | CHOOSE exprs_opt ')' END { yyerror("empty cases clause"); }
     | DEFINE exprs ')' elems END       { $$ = list(define_s, t, $4, $2, nao);
                                          rl($$, num($1)); }
     ;

clause_parts_h : elems additional_parts_h { $$ = cons($1, $2); }
               ;

additional_parts_h : END                                { $$ = nil; }
                   | AND elems additional_parts_h       { $$ = cons($2, $3); }
                   | OR elems additional_parts_h        { $$ = cons($2, $3); }
                   ;

define_clause : DEFINE exprs ')' newl
                clauses_opt
                END newl        { $$ = list(define_s, $2, $5, nao);
                                  rl($$, num($1)); }
              | DEFINE ')' newl
                clauses_opt
                END newl        { $$ = list(define_s, nil, $4, nao);
                                  rl($$, num($1)); }
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
                                            $3, $4, nao);
                                  rl($$, num($1)); }
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
                                                         $4, nao), $5);
                                          rl($$, num($1)); }
                  | CATCH exprs ')' newl
                    clauses_opt
                    catch_clauses_opt   { $$ = cons(list(catch_s, $2, $5, nao),
                                                    $6);
                                          rl($$, num($1)); }
                  | FINALLY newl
                    clauses_opt         { $$ = cons(list(finally_s, nil,
                                                         $3, nao),
                                                    nil);
                                          rl($$, num($1)); }
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
                END newl        { $$ = rl(list(output_s, nao), num($1)); }
              | OUTPUT ')' newl
                out_clauses
                END newl        { $$ = rl(list(output_s, $4, nao), num($1)); }
              | OUTPUT exprs ')' newl
                out_clauses
                END newl        { $$ = list(output_s, $5, $2, nao);
                                  rl($$, num($1)); }

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

out_clause : repeat_clause              { $$ = cons($1, nil); }
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
           | choose_clause              { $$ = nil;
                                          yyerror("choose clause in output"); }
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
                out_clauses_opt
                repeat_parts_opt
                END newl                { $$ = repeat_rep_helper(repeat_s, $3, $4);
                                          rl($$, num($1)); }
              | REPEAT newl
                error           { $$ = nil;
                                  yybadtoken(yychar, lit("repeat clause")); }
              ;

repeat_parts_opt : SINGLE newl
                   out_clauses_opt
                   repeat_parts_opt     { $$ = cons(cons(single_s, $3), $4);
                                          rl($$, num($1)); }
                 | FIRST newl
                   out_clauses_opt
                   repeat_parts_opt     { $$ = cons(cons(first_s, $3), $4);
                                          rl($$, num($1)); }
                 | LAST newl
                   out_clauses_opt
                   repeat_parts_opt     { $$ = cons(cons(last_s, $3), $4);
                                          rl($$, num($1)); }
                 | EMPTY newl
                   out_clauses_opt
                   repeat_parts_opt     { $$ = cons(cons(empty_s, $3), $4);
                                          rl($$, num($1)); }
                 | /* empty */          { $$ = nil; }
                 ;


out_clauses_opt : out_clauses   { $$ = $1; }
                | /* empty */   { $$ = null_list; }

o_line : o_elems_opt '\n'       { $$ = $1; }
       ;

o_elems_opt : o_elems           { $$ = o_elems_transform($1);
                                  rl($$, num(lineno)); }
            |                   { $$ = nil; }
            ;

o_elems_opt2 : o_elems          { $$ = o_elems_transform($1);
                                  rl($$, num(lineno)); }
             |                  { $$ = null_list; }
             ;

o_elems : o_elem                { $$ = cons($1, nil); }
        | o_elem o_elems        { $$ = cons($1, $2); }
        ;

o_elem : TEXT                   { $$ = string_own($1);
                                  rl($$, num(lineno)); }
       | SPACE                  { $$ = string_own($1);
                                  rl($$, num(lineno)); }
       | o_var                  { $$ = $1; }
       | rep_elem               { $$ = $1; }
       ;

rep_elem : REP o_elems_opt
           rep_parts_opt END    { $$ = repeat_rep_helper(rep_s, 
                                                         o_elems_transform($2),
                                                         $3);
                                  rl($$, num($1)); }
         | REP error            { $$ = nil;
                                  yybadtoken(yychar, lit("rep clause")); }
         ;

rep_parts_opt : SINGLE o_elems_opt2
                rep_parts_opt           { $$ = cons(cons(single_s, $2), $3);
                                          rl($$, num($1)); }
              | FIRST o_elems_opt2
                rep_parts_opt           { $$ = cons(cons(first_s, $2), $3);
                                          rl($$, num($1)); }
              | LAST o_elems_opt2
                rep_parts_opt           { $$ = cons(cons(last_s, $2), $3);
                                          rl($$, num($1)); }
              | EMPTY o_elems_opt2
                rep_parts_opt           { $$ = cons(cons(empty_s, $2), $3);
                                          rl($$, num($1)); }
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

o_var : IDENT                   { $$ = list(var_s, intern(string_own($1), nil),
                                            nao); }
      | IDENT o_elem            { $$ = list(var_s, intern(string_own($1), nil),
                                            $2, nao); }
      | '{' IDENT '}'           { $$ = list(var_s, intern(string_own($2), nil),
                                              nao); }
      | '{' IDENT '}' o_elem    { $$ = list(var_s, intern(string_own($2), nil),
                                              $4, nao); }
      | '{' IDENT exprs '}'     { $$ = list(var_s, intern(string_own($2), nil),
                                              nil, $3, nao); }
      | '{' IDENT exprs '}' o_elem      { $$ = list(var_s, 
                                                    intern(string_own($2), nil),
                                                    $5, $3, nao); }
      | IDENT error               { $$ = nil;
                                    yybadtoken(yychar, lit("variable spec")); }
      ;

var_op : '*'                    { $$ = list(t, nao); }
       ;

list : '(' exprs ')'            { $$ = rl($2, num($1)); }
     | '(' ')'                  { $$ = nil; }
     | ',' expr                 { $$ = rlcp(list(unquote_s, $2, nao), $2); }
     | '\'' expr                { $$ = rlcp(list(qquote_s, $2, nao), $2); }
     | SPLICE expr              { $$ = rlcp(list(splice_s, $2, nao), $2); }
     | '(' error                { $$ = nil;
                                  yybadtoken(yychar, lit("list expression")); }
     ;

meta_expr : METAPAR exprs ')'   { $$ = cons(expr_s, $2); }
          | METAPAR ')'         { $$ = cons(expr_s, nil); }
          | METAPAR error       { $$ = nil;
                                  yybadtoken(yychar, lit("meta expression")); }
          ;
exprs : expr                    { $$ = cons($1, nil); }
      | expr exprs              { $$ = cons($1, $2); }
      | expr '.' expr           { $$ = cons($1, $3); }
      ;

exprs_opt : exprs               { $$ = $1; }
          | /* empty */         { $$ = nil; }
          ;

expr : IDENT                    { $$ = intern(string_own($1), nil); }
     | KEYWORD                  { $$ = intern(string_own($1),
                                              keyword_package); }
     | METAVAR                  { $$ = list(var_s,
                                            intern(string_own($1), nil), nao); }
     | NUMBER                   { $$ = num($1); }
     | list                     { $$ = $1; }
     | meta_expr                { $$ = $1; }
     | regex                    { $$ = cons(regex_compile(rest($1)),
                                            rest($1)); }
     | chrlit                   { $$ = $1; }
     | strlit                   { $$ = $1; }
     | quasilit                 { $$ = $1; }
     ;

regex : '/' regexpr '/'         { $$ = cons(regex_s, $2); end_of_regex();
                                  rl($$, num(lineno)); }
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

chrlit : HASH_BACKSLASH IDENT   { wchar_t ch = char_from_name($2);
                                  val str = string_own($2);
                                  end_of_char();
                                  if (ch == L'!')
                                  { yyerrorf(lit("unknown character name: ~a"),
                                             str, nao); }
                                  $$ = chr(ch); }
       | HASH_BACKSLASH LITCHAR { $$ = chr($2);
                                  end_of_char(); }
       | HASH_BACKSLASH error   { $$ = nil;
                                  yybadtoken(yychar,
                                             lit("character literal")); }
       ;

quasilit : '`' '`'              { $$ = null_string; }
         | '`' quasi_items '`'  { $$ = cons(quasi_s, o_elems_transform($2));
                                  rl($$, num(lineno)); }
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

static val repeat_rep_helper(val sym, val main, val parts)
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

static val o_elems_transform(val o_elems)
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

static val define_transform(val define_form)
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

static val lit_char_helper(val litchars)
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

static val optimize_text(val text_form)
{
  if (all_satisfy(rest(text_form), func_n1(stringp), nil))
    return cat_str(rest(text_form), lit(""));
  return text_form;
}

val rl(val form, val lineno)
{
  sethash(form_to_ln_hash, form, lineno);
  pushhash(ln_to_forms_hash, lineno, form);
  return form;
}

static wchar_t char_from_name(wchar_t *name)
{
  static struct {
    const wchar_t *name;
    const wchar_t ch;
  } map[] = {
    { L"nul", 0 },
    { L"alarm", L'\a' },
    { L"backspace", L'\b' },
    { L"tab", L'\t' },
    { L"linefeed", L'\n' },
    { L"newline", L'\n' },
    { L"vtab", L'\v' },
    { L"page", L'\f' },
    { L"return", L'\r' },
    { L"esc", 27 },
    { L"space", L' ' },
    { 0, 0 },
  };
  int i;

  for (i = 0; map[i].name; i++) {
    if (wcscmp(map[i].name, name) == 0)
      return map[i].ch;
  }

  return L'!'; /* code meaning not found */
}

val get_spec(void)
{
  return parsed_spec;
}

#ifndef YYEOF
#define YYEOF YYEMPTY
#endif

void yybadtoken(int tok, val context)
{
  val problem = nil;

  switch (tok) {
  case ERRTOK:
    return;
  case SPACE:   problem = lit("space"); break;
  case TEXT:    problem = lit("text"); break;
  case IDENT:   problem = lit("identifier"); break;
  case KEYWORD: problem = lit("keyword"); break;
  case METAVAR: problem = lit("metavar"); break;
  case ALL:     problem = lit("\"all\""); break;
  case SOME:    problem = lit("\"some\""); break;
  case NONE:    problem = lit("\"none\""); break;
  case MAYBE:   problem = lit("\"maybe\""); break;
  case CASES:   problem = lit("\"cases\""); break;
  case CHOOSE:  problem = lit("\"choose\""); break;
  case AND:     problem = lit("\"and\""); break;
  case OR:      problem = lit("\"or\""); break;
  case END:     problem = lit("\"end\""); break;
  case COLLECT: problem = lit("\"collect\""); break;
  case UNTIL:   problem = lit("\"until\""); break;
  case COLL:    problem = lit("\"coll\""); break;
  case OUTPUT:  problem = lit("\"output\""); break;
  case REPEAT:  problem = lit("\"repeat\""); break;
  case REP:     problem = lit("\"rep\""); break;
  case SINGLE:  problem = lit("\"single\""); break;
  case FIRST:   problem = lit("\"first\""); break;
  case LAST:    problem = lit("\"last\""); break;
  case EMPTY:   problem = lit("\"empty\""); break;
  case DEFINE:  problem = lit("\"define\""); break;
  case TRY:     problem = lit("\"try\""); break;
  case CATCH:   problem = lit("\"catch\""); break;
  case FINALLY: problem = lit("\"finally\""); break;
  case NUMBER:  problem = lit("\"number\""); break;
  case REGCHAR: problem = lit("regular expression character"); break;
  case LITCHAR: problem = lit("string literal character"); break;
  case METAPAR: problem = lit("@("); break;
  }

  if (problem != 0)
    if (context)
      yyerrorf(lit("misplaced ~a in ~a"), problem, context, nao);
    else
      yyerrorf(lit("unexpected ~a"), problem, nao);
  else
    if (context)
      if (tok == YYEOF || tok == YYEMPTY)
        yyerrorf(lit("unterminated ~a"), context, nao);
      else if (tok == '\n')
        yyerrorf(lit("newline in ~a"), context, nao);
      else
        yyerrorf(lit("misplaced character ~a in ~a"), chr(tok), context, nao);
    else
      if (tok == YYEOF)
        yyerrorf(lit("unexpected end of input"), nao);
      else if (tok == YYEMPTY)
        return;
      else
        yyerrorf(lit("unexpected ~s"), chr(tok), nao);
}

