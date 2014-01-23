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

%{

#include <stdio.h>
#include <assert.h>
#include <limits.h>
#include <dirent.h>
#include <stdlib.h>
#include <setjmp.h>
#include <wchar.h>
#include <signal.h>
#include "config.h"
#include "lib.h"
#include "signal.h"
#include "unwind.h"
#include "regex.h"
#include "utf8.h"
#include "match.h"
#include "hash.h"
#include "eval.h"
#include "parser.h"

int yylex(void);
void yyerror(const char *);

static val sym_helper(wchar_t *lexeme, val meta_allowed);
static val repeat_rep_helper(val sym, val args, val main, val parts);
static val o_elems_transform(val output_form);
static val define_transform(val define_form);
static val lit_char_helper(val litchars);
static val optimize_text(val text_form);
static val unquotes_occur(val quoted_form);
static val choose_quote(val quoted_form);
static wchar_t char_from_name(const wchar_t *name);

static val parsed_spec;

%}

%union {
  wchar_t *lexeme;
  union obj *val;
  wchar_t chr;
  cnum lineno;
}

%token <lexeme> SPACE TEXT SYMTOK
%token <lineno> ALL SOME NONE MAYBE CASES BLOCK CHOOSE GATHER
%token <lineno> AND OR END COLLECT
%token <lineno> UNTIL COLL OUTPUT REPEAT REP SINGLE FIRST LAST EMPTY 
%token <lineno> MOD MODLAST DEFINE TRY CATCH FINALLY
%token <lineno> ERRTOK /* deliberately not used in grammar */
%token <lineno> HASH_BACKSLASH HASH_SLASH DOTDOT HASH_H
%token <lineno> SECRET_ESCAPE_R SECRET_ESCAPE_E

%token <val> NUMBER METANUM

%token <chr> REGCHAR REGTOKEN LITCHAR
%token <chr> METAPAR METABKT SPLICE

%type <val> spec clauses clauses_opt clause
%type <val> all_clause some_clause none_clause maybe_clause block_clause
%type <val> cases_clause choose_clause gather_clause collect_clause until_last
%type <val> collect_repeat
%type <val> clause_parts additional_parts gather_parts additional_gather_parts
%type <val> output_clause define_clause try_clause catch_clauses_opt
%type <val> line elems_opt elems clause_parts_h additional_parts_h
%type <val> text texts elem var var_op modifiers meta_expr vector hash
%type <val> list exprs exprs_opt expr out_clauses out_clauses_opt out_clause
%type <val> repeat_clause repeat_parts_opt o_line
%type <val> o_elems_opt o_elems o_elem o_var rep_elem rep_parts_opt
%type <val> regex lisp_regex regexpr regbranch
%type <val> regterm regtoken regclass regclassterm regrange
%type <val> strlit chrlit quasilit quasi_items quasi_item litchars
%type <chr> regchar
%type <lineno> '(' '['

%nonassoc LOW /* used for precedence assertion */
%right SYMTOK '{' '}'
%right ALL SOME NONE MAYBE CASES CHOOSE AND OR END COLLECT UNTIL COLL
%right OUTPUT REPEAT REP FIRST LAST EMPTY DEFINE
%right SPACE TEXT NUMBER
%nonassoc '[' ']' '(' ')'
%left '-' ',' '\'' SPLICE
%left '|' '/'
%left '&' 
%right '~' '*' '?' '+' '%'
%right '.' REGCHAR REGTOKEN LITCHAR
%right DOTDOT

%%

spec : clauses                  { parsed_spec = $1; }
     | /* empty */              { parsed_spec = nil; }
     | SECRET_ESCAPE_R regexpr  { parsed_spec = $2; end_of_regex(); }
     | SECRET_ESCAPE_E expr     { parsed_spec = $2; YYACCEPT; }
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

clause : all_clause             { $$ = cons($1, nil); rlcp($$, $1); }
       | some_clause            { $$ = cons($1, nil); rlcp($$, $1); }
       | none_clause            { $$ = cons($1, nil); rlcp($$, $1); }
       | maybe_clause           { $$ = cons($1, nil); rlcp($$, $1); }
       | cases_clause           { $$ = cons($1, nil); rlcp($$, $1); }
       | block_clause           { $$ = cons($1, nil); rlcp($$, $1); }
       | choose_clause          { $$ = cons($1, nil); rlcp($$, $1); }
       | collect_clause         { $$ = cons($1, nil); rlcp($$, $1); }
       | gather_clause          { $$ = cons($1, nil); rlcp($$, $1); }
       | define_clause          { $$ = list(define_transform($1), nao);
                                  rlcp(car($$), $1);
                                  rlcp($$, $1); }
       | try_clause             { $$ = cons($1, nil); rlcp($$, $1); }
       | output_clause          { $$ = cons($1, nil); rlcp($$, $1); }
       | line                   { $$ = $1; }
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

block_clause  : BLOCK exprs_opt ')'
                newl clauses_opt
                END newl                { val name = first($2); 
                                          if (gt(length($2), one))
                                            yyerror("block: takes zero or no arguments");
                                          if (name && !bindable(name))
                                            yyerrorf(lit("block: ~s is not a bindable symbol"),
                                                     name, nao);
                                          $$ = list(block_s, name, $5, nao);
                                          rl($$, num($1)); }
              | BLOCK exprs_opt ')'
                newl error              { $$ = nil;
                                          yybadtoken(yychar,
                                                     lit("block clause")); }
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
                newl gather_parts
                END newl                { $$ = list(gather_s, 
                                                    append2(mapcar(curry_12_1(func_n2(cons), nil),
                                                                   first($5)), rest($5)),
                                                    $2, nao); 
                                          rl($$, num($1)); }

              | GATHER exprs_opt ')' 
                newl gather_parts
                until_last newl 
                clauses
                END newl                { $$ = list(gather_s, 
                                                    append2(mapcar(curry_12_1(func_n2(cons), nil),
                                                                   first($5)), rest($5)),
                                                    $2, cons(cdr($6), $8), nao); 
                                          rl($$, num($1)); }

              | GATHER exprs_opt ')'
                newl error              { $$ = nil;
                                          yybadtoken(yychar,
                                                     lit("gather clause")); }
              | GATHER exprs_opt ')'
                newl END newl           { $$ = nil;
                                          yyerror("empty gather clause"); }
              ;

gather_parts : clauses additional_gather_parts  { $$ = cons($1, $2); }
             ;

additional_gather_parts : AND newl clauses additional_gather_parts      { $$ = cons($3, $4); }
                        | OR newl clauses additional_parts              { $$ = cons($3, $4); }
                        | /* empty */                                   { $$ = nil; }
                        ;

collect_clause : collect_repeat exprs_opt ')' newl
                 clauses END newl                { $$ = list(car($1),
                                                             $5, nil, $2,
                                                             nao);
                                                   rl($$, cdr($1)); }
               | collect_repeat exprs_opt ')'
                 newl clauses until_last
                 newl clauses END newl    { $$ = list(car($1), $5,
                                                      cons(cdr($6), $8),
                                                      $2, nao);
                                            rl($$, cdr($1)); 
                                            rl($8, car($6)); }
               | collect_repeat exprs_opt ')'
                 newl error             { $$ = nil;
                                          if (yychar == UNTIL ||
                                              yychar == END ||
                                              yychar == LAST)
                                            yyerror("empty collect");
                                          else
                                            yybadtoken(yychar,
                                                       lit("collect clause")); }
               ;

collect_repeat : COLLECT { $$ = cons(collect_s, num($1)); }
               | REPEAT { $$ = cons(repeat_s, num($1)); }
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
                                    $$ = cons(regex_compile(spaces, nil), spaces);
                                    rl($$, num(lineno));
                                    free($1); }
                                  else
                                  { $$ = rl(string_own($1), num(lineno)); }}
     | regex                    { $$ = cons(regex_compile(rest($1), nil),
                                            rest($1));
                                  rl($$, num(lineno)); }
     ;

texts : text %prec LOW          { $$ = rlcp(cons($1, nil), $1); }
      | text texts              { $$ = rlcp(cons($1, $2), $2); }
      ;

elem : texts                    { $$ = rlcp(cons(text_s, $1), $1);
                                  $$ = rlcp(optimize_text($$), $$); }
     | var                      { $$ = rl($1, num(lineno)); }
     | list                     { val sym = first($1);
                                  if (sym == do_s || sym == require_s)
                                    $$ = rlcp(cons(sym,
                                                   expand_forms(rest($1))),
                                              $1);
                                  else
                                    $$ = $1; }
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
           | define_clause              { $$ = nil;
                                          yyerror("match clause in output"); }

           | try_clause                 { $$ = nil;
                                          yyerror("match clause in output"); }
           | output_clause              { $$ = nil;
                                          yyerror("match clause in output"); }
           ;

repeat_clause : REPEAT exprs_opt ')' newl
                out_clauses_opt
                repeat_parts_opt
                END newl                { $$ = repeat_rep_helper(repeat_s, 
                                                                 $2, $5, $6);
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
                 | MOD exprs_opt ')'
                   newl
                   out_clauses_opt
                   repeat_parts_opt     { $$ = cons(cons(mod_s, 
                                                         cons($2, $5)), $6);
                                          rl($$, num($1)); }
                 | MODLAST exprs_opt ')'
                   newl
                   out_clauses_opt
                   repeat_parts_opt     { $$ = cons(cons(modlast_s,
                                                         cons($2, $5)), $6);
                                          rl($$, num($1)); }
                 | /* empty */          { $$ = nil; }
                 ;


out_clauses_opt : out_clauses   { $$ = $1; }
                | /* empty */   { $$ = nil; }

o_line : o_elems_opt '\n'       { $$ = $1; }
       ;

o_elems_opt : o_elems           { $$ = o_elems_transform($1);
                                  rl($$, num(lineno)); }
            |                   { $$ = nil; }
            ;

o_elems : o_elem                { $$ = cons($1, nil); }
        | o_elem o_elems        { $$ = cons($1, $2); }
        ;

o_elem : TEXT                   { $$ = string_own($1);
                                  rl($$, num(lineno)); }
       | SPACE                  { $$ = string_own($1);
                                  rl($$, num(lineno)); }
       | o_var                  { $$ = $1; }
       | list                   { $$ = rlcp(cons(expr_s, expand($1)), $1); }
       | rep_elem               { $$ = $1; }
       ;

rep_elem : REP exprs_opt ')' o_elems_opt
           rep_parts_opt END    { $$ = repeat_rep_helper(rep_s, 
                                                         $2,
                                                         o_elems_transform($4),
                                                         $5);
                                  rl($$, num($1)); }
         | REP error            { $$ = nil;
                                  yybadtoken(yychar, lit("rep clause")); }
         ;

rep_parts_opt : SINGLE o_elems_opt
                rep_parts_opt           { $$ = cons(cons(single_s, $2), $3);
                                          rl($$, num($1)); }
              | FIRST o_elems_opt
                rep_parts_opt           { $$ = cons(cons(first_s, $2), $3);
                                          rl($$, num($1)); }
              | LAST o_elems_opt
                rep_parts_opt           { $$ = cons(cons(last_s, $2), $3);
                                          rl($$, num($1)); }
              | EMPTY o_elems_opt
                rep_parts_opt           { $$ = cons(cons(empty_s, $2), $3);
                                          rl($$, num($1)); }
              | MOD exprs_opt ')'
                o_elems_opt
                rep_parts_opt           { $$ = cons(cons(mod_s, 
                                                         cons($2, $4)), $5);
                                          rl($$, num($1)); }
              | MODLAST exprs_opt ')'
                o_elems_opt
                rep_parts_opt           { $$ = cons(cons(modlast_s, 
                                                         cons($2, $4)), $5);
                                          rl($$, num($1)); }
              | /* empty */             { $$ = nil; }
              ;


/* This sucks, but factoring '*' into a nonterminal
 * that generates an empty phrase causes reduce/reduce conflicts.
 */
var : SYMTOK                    { $$ = list(var_s, sym_helper($1, nil), nao); }
    | SYMTOK elem               { $$ = list(var_s, sym_helper($1, nil),
                                            $2, nao); }
    | '{' SYMTOK '}'            { $$ = list(var_s, sym_helper($2, nil), nao); }
    | '{' SYMTOK '}' elem       { $$ = list(var_s, sym_helper($2, nil),
                                            $4, nao); }
    | '{' SYMTOK modifiers '}'  { $$ = list(var_s, sym_helper($2, nil),
                                            nil, $3, nao); }
    | '{' SYMTOK modifiers '}' elem
                                { $$ = list(var_s, sym_helper($2, nil),
                                            $5, $3, nao); }
    | var_op SYMTOK             { $$ = list(var_s, sym_helper($2, nil),
                                            nil, $1, nao); }
    | var_op SYMTOK elem        { $$ = list(var_s, sym_helper($2, nil),
                                            $3, $1, nao); }
    | var_op '{' SYMTOK '}'     { $$ = list(var_s, sym_helper($3, nil),
                                            nil, $1, nao); }
    | var_op '{' SYMTOK '}' elem
                                { $$ = list(var_s, sym_helper($3, nil),
                                            $5, $1, nao); }
    | var_op '{' SYMTOK regex '}'       { $$ = nil;
                                          yyerror("longest match "
                                                  "not useable with regex"); }
    | var_op '{' SYMTOK NUMBER '}'      { $$ = nil;
                                          yyerror("longest match "
                                                  "not useable with "
                                                  "fixed width match"); }
    | SYMTOK error              { $$ = nil;
                                  yybadtoken(yychar, lit("variable spec")); }
    | var_op error              { $$ = nil;
                                  yybadtoken(yychar, lit("variable spec")); }
    ;

var_op : '*'                    { $$ = list(t, nao); }
       ;

modifiers : NUMBER              { $$ = cons($1, nil); }
          | regex               { $$ = cons(cons(regex_compile(rest($1), nil), 
                                                 rest($1)), nil);
                                  rlcp($$, $1); }
          | list                { $$ = cons($1, nil); }
          ;

o_var : SYMTOK                  { $$ = list(var_s, sym_helper($1, nil), nao); }
      | SYMTOK o_elem           { $$ = list(var_s, sym_helper($1, nil),
                                            $2, nao); }
      | '{' expr exprs_opt '}' 
                                { $$ = list(var_s, $2, nil, $3, nao); }
      | '{' expr exprs_opt '}' o_elem      
                                { $$ = list(var_s, $2, $5, $3, nao); }
      | SYMTOK error            { $$ = nil;
                                    yybadtoken(yychar, lit("variable spec")); }
      ;

vector : '#' list               { if (unquotes_occur($2))
                                    $$ = rlcp(cons(vector_lit_s,
                                                   cons($2, nil)), $2);
                                  else
                                    $$ = rlcp(vector_list($2), $2); }
       ;

hash : HASH_H list              { if (unquotes_occur($2))
                                    $$ = rlcp(cons(hash_lit_s, $2),
                                              num($1));
                                  else
                                    $$ = rlcp(hash_construct(first($2),
                                                             rest($2)),
                                              num($1)); }
     ;

list : '(' exprs ')'            { $$ = rl($2, num($1)); }
     | '(' ')'                  { $$ = nil; }
     | '[' exprs ']'            { $$ = rl(cons(dwim_s, $2), num($1)); }
     | '[' ']'                  { $$ = rl(cons(dwim_s, nil), num($1)); }
     | ',' expr                 { val expr = $2;
                                  if (consp(expr) && first(expr) == qquote_s)
                                    expr = cons(quote_s, rest(expr));
                                  $$ = rlcp(list(unquote_s, expr, nao), $2); }
     | '\'' expr                { $$ = rlcp(list(choose_quote($2),
                                            $2, nao), $2); }
     | SPLICE expr              { val expr = $2;
                                  if (consp(expr) && first(expr) == qquote_s)
                                    expr = cons(quote_s, rest(expr));
                                  $$ = rlcp(list(splice_s, expr, nao), $2); }
     | '(' error                { $$ = nil;
                                  yybadtoken(yychar, lit("list expression")); }
     | '[' error                { $$ = nil;
                                  yybadtoken(yychar, lit("DWIM expression")); }
     ;

meta_expr : METAPAR exprs ')'   { $$ = rlcp(cons(expr_s, expand($2)), $2); }
          | METABKT exprs ']'   { $$ = rlcp(cons(expr_s, 
                                                 rlcp(expand(cons(dwim_s, $2)),
                                                      $2)), 
                                            $2); }
          | METAPAR ')'         { $$ = rl(cons(expr_s, nil), num(lineno)); }
          | METABKT ']'         { $$ = rl(cons(expr_s, rl(cons(dwim_s, nil), 
                                                          num(lineno))),
                                          num(lineno)); }
          | METAPAR error       { $$ = nil;
                                  yybadtoken(yychar, lit("meta expression")); }
          ;

exprs : expr                    { $$ = rlcp(cons($1, nil), $1); }
      | expr exprs              { $$ = rlcp(cons($1, $2), $1); }
      | expr '.' expr           { $$ = rlcp(cons($1, $3), $1); }
      | expr DOTDOT exprs       { $$ = rlcp(cons(list(cons_s, $1,
                                                      car($3), nao),
                                                 cdr($3)), $1); }
      ;

exprs_opt : exprs               { $$ = $1; }
          | /* empty */         { $$ = nil; }
          ;

expr : SYMTOK                   { $$ = rl(sym_helper($1, t), num(lineno)); }
     | METANUM                  { $$ = cons(var_s, cons($1, nil));
                                  rl($$, num(lineno)); }
     | NUMBER                   { $$ = $1; }
     | list                     { $$ = $1; }
     | vector                   { $$ = $1; }
     | hash                     { $$ = $1; }
     | meta_expr                { $$ = $1; }
     | lisp_regex               { $$ = cons(regex_compile(rest($1), nil),
                                            rest($1));
                                  rlcp($$, $1); }
     | chrlit                   { $$ = rl($1, num(lineno)); }
     | strlit                   { $$ = $1; }
     | quasilit                 { $$ = $1; }
     ;

regex : '/' regexpr '/'         { $$ = cons(regex_s, $2); end_of_regex();
                                  rl($$, num(lineno)); }
      | '/' error               { $$ = nil;
                                  yybadtoken(yychar, lit("regex"));
                                  end_of_regex(); }
      ;

lisp_regex : HASH_SLASH regexpr '/'    
                                { $$ = cons(regex_s, $2); end_of_regex();
                                  rl($$, num(lineno)); }
      | HASH_SLASH error        { $$ = nil;
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
        | regtoken              { $$ = $1; }
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
             | regtoken         { $$ = $1; }
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

regtoken : REGTOKEN             { switch ($1)
                                  { case 's':
                                      $$ = space_k; break;
                                    case 'S':
                                      $$ = cspace_k; break;
                                    case 'd':
                                      $$ = digit_k; break;
                                    case 'D':
                                      $$ = cdigit_k; break;
                                    case 'w':
                                      $$ = word_char_k; break;
                                    case 'W':
                                      $$ = cword_char_k; break; }}

newl : '\n'
     | error '\n'       { yyerror("newline expected after directive");
                          yyerrok; }
     ;

strlit : '"' '"'                { $$ = null_string; }
       | '"' litchars '"'       { $$ = lit_char_helper($2);
                                  rl($$, num(lineno)); }
       | '"' error              { $$ = nil;
                                  yybadtoken(yychar, lit("string literal")); }
       ;

chrlit : HASH_BACKSLASH SYMTOK  { wchar_t ch;
                                  val str = string_own($2);
                                  const wchar_t *cstr = c_str(str);

                                  if (cstr[1] == 0)
                                  { ch = cstr[0]; }
                                  else
                                  { ch = char_from_name(cstr);
                                    if (ch == L'!')
                                    { yyerrorf(lit("unknown character name: ~a"),
                                               str, nao); }}
                                  end_of_char();
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
           | o_var              { $$ = $1; }
           | METANUM            { $$ = cons(var_s, cons($1, nil));
                                  rl($$, num(lineno)); }
           | list               { $$ = rlcp(cons(expr_s, expand($1)), $1); }
           ;

litchars : LITCHAR              { $$ = cons(chr($1), nil); }
         | LITCHAR litchars     { $$ = cons(chr($1), $2); }
         ;

%%

static val sym_helper(wchar_t *lexeme, val meta_allowed)
{
  int leading_at = *lexeme == L'@';
  wchar_t *tokfree = lexeme;
  wchar_t *colon = wcschr(lexeme, L':');
  val sym_name = nil, pkg_name = nil, package = nil, sym;

  if (leading_at) {
    if (!meta_allowed) {
      val tok = string_own(lexeme);
      yyerrorf(lit("~a: meta variable not allowed in this context"), tok, nao);
      return nil;
    }
    lexeme++;
  }

  if (colon != 0)
    *colon = 0;

  if (colon == lexeme) {
    package = keyword_package;
    sym_name = string(colon + 1);
    free(tokfree);
  } else if (colon != 0) {
    pkg_name = string(lexeme);
    package = find_package(pkg_name);
    sym_name = string(colon + 1);
    free(tokfree);
    if (!package) {
      yyerrorf(lit("~a:~a: package ~a not found"), pkg_name, sym_name, pkg_name, nao);
      return nil;
    }
  } else {
    sym_name = string(lexeme);
    free(tokfree);
  }

  sym = intern(sym_name, package);

  return leading_at ? list(var_s, sym, nao) : sym;
}

static val repeat_rep_helper(val sym, val args, val main, val parts)
{
  uses_or2;
  val single_parts = nil, single_parts_p = nil;
  val first_parts = nil, first_parts_p = nil;
  val last_parts = nil, last_parts_p = nil;
  val empty_parts = nil, empty_parts_p = nil;
  val mod_parts = nil, mod_parts_p = nil;
  val modlast_parts = nil, modlast_parts_p = nil;
  val iter;

  for (iter = parts; iter != nil; iter = cdr(iter)) {
    val part = car(iter);
    val sym = car(part);
    val clauses = copy_list(cdr(part));

    if (sym == single_s) {
      single_parts = nappend2(single_parts, clauses);
      single_parts_p = t;
    } else if (sym == first_s) {
      first_parts = nappend2(first_parts, clauses);
      first_parts_p = t;
    } else if (sym == last_s) {
      last_parts = nappend2(last_parts, clauses);
      last_parts_p = t;
    } else if (sym == empty_s) {
      empty_parts = nappend2(empty_parts, clauses);
      empty_parts_p = t;
    } else if (sym == mod_s) {
      mod_parts = cons(clauses, mod_parts);
      mod_parts_p = t;
    } else if (sym == modlast_s) {
      modlast_parts = cons(clauses, modlast_parts);
      modlast_parts_p = t;
    } else {
      abort();
    }
  }

  single_parts = or2(single_parts, single_parts_p);
  first_parts = or2(first_parts, first_parts_p);
  last_parts = or2(last_parts, last_parts_p);
  empty_parts = or2(empty_parts, empty_parts_p);
  mod_parts = or2(nreverse(mod_parts), mod_parts_p);
  modlast_parts = or2(nreverse(modlast_parts), modlast_parts_p);

  return list(sym, args, main, single_parts, first_parts,
              last_parts, empty_parts, nreverse(mod_parts),
              nreverse(modlast_parts), nao);
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

      ptail = list_collect(ptail, list(first(elem), sym, nil, modifiers, nao));
      elem = pat;
    }

    if (elem)
      ptail = list_collect(ptail, elem);
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
    val len = length_list(litchars), iter, ix;
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

static val unquotes_occur(val quoted_form)
{
  uses_or2;

  if (atom(quoted_form)) {
    return nil;
  } else {
    val sym = car(quoted_form);
    if (sym == unquote_s || sym == splice_s)
      return t;
    if (sym == quote_s)
      return nil;
    return or2(unquotes_occur(sym), unquotes_occur(cdr(quoted_form)));
  }
}

static val choose_quote(val quoted_form)
{
  return unquotes_occur(quoted_form) ? qquote_s : quote_s;
}

val rl(val form, val lineno)
{
  sethash(form_to_ln_hash, form, cons(lineno, spec_file_str));
  return form;
}

val rlset(val form, val info)
{
  sethash(form_to_ln_hash, form, info);
  return form;
}

static wchar_t char_from_name(const wchar_t *name)
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
#define YYEOF 0
#endif

void yybadtoken(int tok, val context)
{
  val problem = nil;

  switch (tok) {
  case ERRTOK:
    return;
  case SPACE:   problem = lit("space"); break;
  case TEXT:    problem = lit("text"); break;
  case SYMTOK:  problem = lit("symbol-token"); break;
  case METANUM: problem = lit("metanum"); break;
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
  case NUMBER:  problem = lit("number"); break;
  case REGCHAR: problem = lit("regular expression character"); break;
  case REGTOKEN: problem = lit("regular expression token"); break;
  case LITCHAR: problem = lit("string literal character"); break;
  case METAPAR: problem = lit("@("); break;
  case METABKT: problem = lit("@["); break;
  case DOTDOT: problem = lit(".."); break;
  case HASH_BACKSLASH: problem = lit("#\\"); break;
  case HASH_SLASH:     problem = lit("#/"); break;
  case HASH_H:         problem = lit("#H"); break;
  }

  if (problem != 0)
    if (context)
      yyerrorf(lit("misplaced ~a in ~a"), problem, context, nao);
    else
      yyerrorf(lit("unexpected ~a"), problem, nao);
  else
    if (context) /* Byacc sets yychar to 0 */
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
