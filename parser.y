/* Copyright 2009-2016
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

%{

#include <stddef.h>
#include <stdio.h>
#include <assert.h>
#include <limits.h>
#include <dirent.h>
#include <stdlib.h>
#include <stdarg.h>
#include <setjmp.h>
#include <wchar.h>
#include <signal.h>
#include "config.h"
#include ALLOCA_H
#include "lib.h"
#include "signal.h"
#include "unwind.h"
#include "regex.h"
#include "utf8.h"
#include "match.h"
#include "hash.h"
#include "struct.h"
#include "eval.h"
#include "stream.h"
#include "y.tab.h"
#include "gc.h"
#include "args.h"
#include "cadr.h"
#include "debug.h"
#include "txr.h"
#include "parser.h"

static val sym_helper(parser_t *parser, wchar_t *lexeme, val meta_allowed);
static val repeat_rep_helper(val sym, val args, val main, val parts);
static val define_transform(parser_t *parser, val define_form);
static val lit_char_helper(val litchars);
static val optimize_text(val text_form);
static val unquotes_occur(val quoted_form, int level);
static val expand_meta(val form, val menv);
static val rlrec(parser_t *, val form, val line);
static wchar_t char_from_name(const wchar_t *name);
static val make_expr(parser_t *, val sym, val rest, val lineno);
static val check_for_include(val spec_rev);
static void misplaced_consing_dot_check(scanner_t *scanner, val term_atom_cons);

#if YYBISON
union YYSTYPE;
int yylex(union YYSTYPE *, yyscan_t scanner);
int yyparse(scanner_t *, parser_t *);
#endif

#define rl(form, line) rlrec(parser, form, line)
#define mkexp(sym, rest, lineno) make_expr(parser, sym, rest, lineno)
#define symhlpr(lexeme, meta_allowed) sym_helper(parser, lexeme, meta_allowed)
#define yyerr(msg) yyerror(scnr, parser, msg)
#define yybadtok(tok, context) yybadtoken(parser, tok, context)

%}

%pure-parser
%parse-param{scanner_t *scnr}
%parse-param{parser_t *parser}
%lex-param{yyscan_t scnr}

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
%token <lineno> MOD MODLAST DEFINE TRY CATCH FINALLY IF
%token <lineno> ERRTOK /* deliberately not used in grammar */
%token <lineno> HASH_BACKSLASH HASH_SLASH DOTDOT HASH_H HASH_S HASH_R HASH_SEMI
%token <lineno> WORDS WSPLICE QWORDS QWSPLICE
%token <lineno> SECRET_ESCAPE_R SECRET_ESCAPE_E SECRET_ESCAPE_I

%token <val> NUMBER METANUM
%token <val> HASH_N_EQUALS HASH_N_HASH

%token <chr> REGCHAR REGTOKEN LITCHAR SPLICE CONSDOT LAMBDOT

%type <val> spec hash_semis_n_expr hash_semis_i_expr
%type <val> ignored_i_exprs ignored_n_exprs
%type <val> clauses_rev clauses_opt clause
%type <val> all_clause some_clause none_clause maybe_clause block_clause
%type <val> cases_clause choose_clause gather_clause collect_clause until_last
%type <val> collect_repeat
%type <val> clause_parts additional_parts gather_parts additional_gather_parts
%type <val> output_clause define_clause try_clause catch_clauses_opt
%type <val> if_clause elif_clauses_opt else_clause_opt
%type <val> line elems_opt elems clause_parts_h additional_parts_h
%type <val> text texts elem var var_op modifiers vector hash struct range
%type <val> list exprs exprs_opt n_exprs r_exprs i_expr n_expr n_exprs_opt
%type <val> out_clauses out_clauses_opt out_clause
%type <val> repeat_clause repeat_parts_opt o_line
%type <val> o_elems_opt o_elems o_elem o_var q_var rep_elem rep_parts_opt
%type <val> regex lisp_regex regexpr regbranch
%type <val> regterm regtoken regclass regclassterm regrange
%type <val> strlit chrlit quasilit quasi_items quasi_item litchars wordslit
%type <val> wordsqlit not_a_clause
%type <chr> regchar
%type <val> byacc_fool
%type <lineno> '(' '[' '@'

%nonassoc LOW /* used for precedence assertion */
%right SYMTOK '{' '}'
%right ALL SOME NONE MAYBE CASES CHOOSE AND OR END COLLECT UNTIL COLL
%right OUTPUT REPEAT REP FIRST LAST EMPTY DEFINE IF ELIF ELSE
%right SPACE TEXT NUMBER METANUM HASH_N_EQUALS HASH_N_HASH
%nonassoc '[' ']' '(' ')'
%left '-' ',' '\'' '^' SPLICE '@'
%left '|' '/'
%left '&'
%right '~' '*' '?' '+' '%'
%right '.' CONSDOT LAMBDOT REGCHAR REGTOKEN LITCHAR
%right DOTDOT

%%

spec : clauses_opt              { parser->syntax_tree = $1; }
     | SECRET_ESCAPE_R regexpr  { parser->syntax_tree = $2; end_of_regex(scnr); }
     | SECRET_ESCAPE_E hash_semis_n_expr
                                { parser->syntax_tree = $2; YYACCEPT; }
       byacc_fool               { internal_error("notreached"); }
     | SECRET_ESCAPE_I hash_semis_i_expr
                                { parser->syntax_tree = $2; YYACCEPT; }
       byacc_fool               { internal_error("notreached"); }
     | SECRET_ESCAPE_E          { if (yychar == YYEOF) {
                                    parser->syntax_tree = nao;
                                    YYACCEPT;
                                  } else {
                                    yybadtok(yychar, nil);
                                    parser->syntax_tree = nil;
                                  } }
     | SECRET_ESCAPE_I          { if (yychar == YYEOF) {
                                    parser->syntax_tree = nao;
                                    YYACCEPT;
                                  } else {
                                    yybadtok(yychar, nil);
                                    parser->syntax_tree = nil;
                                  } }
     | error '\n'               { parser->syntax_tree = nil;
                                  if (parser->errors >= 8)
                                    YYABORT;
                                  yyerrok;
                                  yybadtok(yychar, nil); }

     ;

hash_semis_n_expr : ignored_n_exprs n_expr      { $$ = $2; }
                  | n_expr                      { $$ = $1; }
                  ;

ignored_n_exprs : ignored_n_exprs HASH_SEMI     { parser->circ_suppress = 1; }
                  n_expr                        { parser->circ_suppress = 0;
                                                  $$ = nil; }
                | HASH_SEMI                     { parser->circ_suppress = 1; }
                  n_expr                        { parser->circ_suppress = 0;
                                                  $$ = nil; }
                ;

hash_semis_i_expr : ignored_i_exprs i_expr      { $$ = $2; }
                  | i_expr                      { $$ = $1; }
                  ;

ignored_i_exprs : ignored_i_exprs HASH_SEMI     { parser->circ_suppress = 1; }
                  i_expr                        { parser->circ_suppress = 0;
                                                  $$ = nil; }
                | HASH_SEMI                     { parser->circ_suppress = 1; }
                  i_expr                        { parser->circ_suppress = 0;
                                                  $$ = nil; }
                ;

/* Hack needed for Berkeley Yacc */
byacc_fool : n_expr { internal_error("notreached"); }
           | { internal_error("notreached"); }
           ;

clauses_rev : clause                    { $$ = check_for_include(cons($1, nil)); }
            | clauses_rev clause        { $$ = check_for_include(cons($2, $1));  }
            ;

clauses_opt : clauses_rev       { $$ = nreverse($1); }
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
       | define_clause          { $$ = list(define_transform(parser, $1), nao);
                                  rlcp(car($$), $1);
                                  rlcp($$, $1); }
       | try_clause             { $$ = cons($1, nil); rlcp($$, $1); }
       | if_clause              { $$ = cons($1, nil); rlcp($$, $1); }
       | output_clause          { $$ = cons($1, nil); rlcp($$, $1); }
       | line                   { $$ = $1; }
       ;

all_clause : ALL newl clause_parts      { if (nilp($3))
                                            yyerr("empty all clause");
                                          $$ = list(all_s, $3, nao);
                                          rl($$, num($1)); }
           | ALL newl error             { $$ = nil;
                                          yybadtok(yychar, lit("all clause")); }
           ;

some_clause : SOME exprs_opt ')'
              newl clause_parts         { if (nilp($5))
                                            yyerr("empty some clause");
                                          $$ = list(some_s, $5, $2, nao);
                                          rl($$, num($1)); }
            | SOME exprs_opt ')'
              newl error
                                        { $$ = nil;
                                          yybadtok(yychar, lit("some clause")); }
            ;

none_clause : NONE newl clause_parts    { if (nilp($3))
                                            yyerr("empty none clause");
                                          $$ = list(none_s, $3, nao);
                                          rl($$, num($1)); }
            | NONE newl error           { $$ = nil;
                                          yybadtok(yychar, lit("none clause")); }
            ;

maybe_clause : MAYBE newl clause_parts  { if (nilp($3))
                                            yyerr("empty maybe clause");
                                          $$ = list(maybe_s, $3, nao);
                                          rl($$, num($1)); }
             | MAYBE newl error         { $$ = nil;
                                          yybadtok(yychar, lit("maybe clause")); }
             ;

cases_clause : CASES newl clause_parts  { if (nilp($3))
                                            yyerr("empty cases clause");
                                          $$ = list(cases_s, $3, nao);
                                          rl($$, num($1)); }
             | CASES newl error         { $$ = nil;
                                          yybadtok(yychar, lit("cases clause")); }
             ;

block_clause  : BLOCK exprs_opt ')'
                newl clauses_opt
                END newl                { val name = first($2);
                                          if (gt(length($2), one))
                                            yyerr("block: takes zero or no arguments");
                                          if (name && !bindable(name))
                                            yyerrorf(scnr,
                                                     lit("block: ~s is not a bindable symbol"),
                                                     name, nao);
                                          $$ = list(block_s, name, $5, nao);
                                          rl($$, num($1)); }
              | BLOCK exprs_opt ')'
                newl error              { $$ = nil;
                                          yybadtok(yychar, lit("block clause")); }
              ;

choose_clause : CHOOSE exprs_opt ')'
                newl clause_parts       { if (nilp($5))
                                            yyerr("empty choose clause");
                                          $$ = list(choose_s, $5, $2, nao);
                                          rl($$, num($1)); }
              | CHOOSE exprs_opt ')'
                newl error              { $$ = nil;
                                          yybadtok(yychar, lit("choose clause")); }
              ;

gather_clause : GATHER exprs_opt ')'
                newl gather_parts
                END newl                { if (nilp($5))
                                            yyerr("empty gather clause");
                                          $$ = list(gather_s,
                                                    append2(mapcar(curry_12_1(func_n2(cons), nil),
                                                                   first($5)), rest($5)),
                                                    $2, nao);
                                          rl($$, num($1)); }

              | GATHER exprs_opt ')'
                newl gather_parts
                until_last exprs_opt ')' newl
                clauses_opt
                END newl                { if (nilp($5))
                                            yyerr("empty gather clause");
                                          if (nilp($10))
                                            yyerr("empty until/last clause in gather");
                                          $$ = list(gather_s,
                                                    append2(mapcar(curry_12_1(func_n2(cons), nil),
                                                                   first($5)), rest($5)),
                                                    $2, cons(cdr($6),
                                                             cons($7, $10)), nao);
                                          rl($$, num($1)); }

              | GATHER exprs_opt ')'
                newl error              { $$ = nil;
                                          yybadtok(yychar, lit("gather clause")); }
              ;

gather_parts : clauses_opt additional_gather_parts
                                        { $$ = if2($1, cons($1, $2)); }
             ;

additional_gather_parts : AND newl gather_parts { $$ = $3;
                                                  if (nilp($$))
                                                    yyerr("empty and subclause"); }
                        | OR newl gather_parts  { $$ = $3;
                                                  if (nilp($$))
                                                    yyerr("empty or subclause"); }
                        | /* empty */           { $$ = nil; }
                        ;

collect_clause : collect_repeat exprs_opt ')' newl
                 clauses_opt END newl            { if (nilp($5))
                                                     yyerr("empty collect clause");
                                                   $$ = list(car($1),
                                                             $5, nil, $2,
                                                             nao);
                                                   rl($$, cdr($1)); }
               | collect_repeat exprs_opt ')'
                 newl clauses_opt until_last exprs_opt ')'
                 newl clauses_opt END newl
                                          { if (nilp($5))
                                              yyerr("empty collect clause");
                                            if (nilp($10))
                                              yyerr("empty until/last in collect");
                                            $$ = list(car($1), $5,
                                                      cons(cdr($6),
                                                           cons($7, $10)),
                                                      $2, nao);
                                            rl($$, cdr($1));
                                            rl($10, car($6)); }
               | collect_repeat exprs_opt ')'
                 newl error             { $$ = nil;
                                          yybadtok(yychar, lit("collect clause")); }
               ;

collect_repeat : COLLECT { $$ = cons(collect_s, num($1)); }
               | REPEAT { $$ = cons(repeat_s, num($1)); }
               ;

until_last : UNTIL { $$ = cons(num($1), until_s); }
           | LAST  { $$ = cons(num($1), last_s); }
           ;

clause_parts : clauses_opt additional_parts     { $$ = if2($1, cons($1, $2)); }
             ;

additional_parts : END newl                     { $$ = nil; }
                 | AND newl clause_parts        { $$ = $3;
                                                  if (nilp($$))
                                                    yyerr("empty and subclause"); }
                 | OR newl clause_parts         { $$ = $3;
                                                  if (nilp($$))
                                                    yyerr("empty or subclause"); }
                 ;

if_clause : IF n_exprs_opt ')'
            newl clauses_opt
            elif_clauses_opt
            else_clause_opt
            END newl            { if (opt_compat && opt_compat <= 136)
                                  { val xexp = expand_meta($2, nil);
                                    val req = rlcp(cons(require_s, xexp), $2);
                                    val iff = rlcp(cons(cons(cons(req, nil), $5), nil), $2);
                                    val elifs = $6;
                                    val els = cons($7, nil);
                                    val cases = nappend2(nappend2(iff, elifs), els);
                                    $$ = list(cases_s, cases, nao); }
                                  else
                                  { val expr = expand(car($2), nil);
                                    val ifs = $5;
                                    val branch = cons(cons(expr, ifs), nil);
                                    val elifs = $6;
                                    val els = $7;
                                    if (cdr($2))
                                      yyerr("extra expression in if");
                                    $$ = cons(if_s,
                                              nappend2(branch, nappend2(elifs, els)));
                                    rl($$, num($1)); } }
          | IF n_exprs_opt ')'
            newl error          { $$ = nil; yybadtok(yychar, lit("if clause")); }
          ;

elif_clauses_opt : ELIF n_exprs_opt ')' newl
                   clauses_opt
                   elif_clauses_opt  { if (opt_compat && opt_compat <= 136)
                                       { val xexp = expand_meta($2, nil);
                                         val req = rlcp(cons(require_s, xexp), $2);
                                         $$ = cons(cons(cons(req, nil), $5), $6); }
                                       else
                                       { val expr = expand(car($2), nil);
                                         val elifs = $5;
                                         val branch = cons(cons(expr, elifs), nil);
                                         if (cdr($2))
                                           yyerr("extra expression in elif");
                                         $$ = nappend2(branch, $6); } }
                 |                   { $$ = nil; }
                 ;

else_clause_opt : ELSE newl
                  clauses_opt        { if (opt_compat && opt_compat <= 136)
                                       { $$ = $3; }
                                       else
                                       { $$ = cons(cons(t, $3), nil); } }
                  |                  { $$ = nil; }
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
      ;


text : TEXT                     { $$ = rl(string_own($1), num(parser->lineno)); }
     | SPACE                    { if ($1[0] == ' ' && $1[1] == 0)
                                  { val spaces = list(oneplus_s,
                                                      chr(' '), nao);
                                    free($1);
                                    $$ = regex_compile(spaces, nil);
                                    rl($$, num(parser->lineno)); }
                                  else
                                  { $$ = rl(string_own($1), num(parser->lineno)); }}
     | regex                    { $$ = $1;
                                  rl($$, num(parser->lineno)); }
     | EMPTY                    { $$ = null_string; }
     ;

texts : text %prec LOW          { $$ = rlcp(cons($1, nil), $1); }
      | text texts              { $$ = rlcp(cons($1, $2), $2); }
      ;

elem : texts                    { $$ = rlcp(cons(text_s, $1), $1);
                                  $$ = rlcp(optimize_text($$), $$); }
     | var                      { $$ = rl($1, num(parser->lineno)); }
     | list                     { val sym = first($1);
                                  if (sym ==  do_s || sym == require_s)
                                    $$ = rlcp(cons(sym,
                                                   expand_forms(rest($1), nil)),
                                              $1);
                                  else
                                    $$ = rlcp(cons(sym,
                                                   expand_meta(rest($1), nil)),
                                              $1); }
     | COLL exprs_opt ')' elems_opt END { if (nilp($4))
                                            yyerr("empty coll clause");
                                          $$ = list(coll_s, $4, nil, $2, nao);
                                          rl($$, num($1)); }
     | COLL exprs_opt ')' elems_opt
       until_last exprs_opt ')'
       elems_opt END            { if (nilp($4))
                                    yyerr("empty coll clause");
                                  if (nilp($8))
                                    yyerr("empty until/last in coll");
                                  $$ = list(coll_s, $4, cons(cdr($5),
                                                             cons($6, $8)),
                                            $2, nao);
                                  rl($$, num($1));
                                  rl($6, car($5)); }
     | REP exprs_opt ')' elems END     { if (nilp($4))
                                            yyerr("empty rep clause");
                                         $$ = list(rep_s, $4, nil, $2, nao);
                                         rl($$, num($1)); }
     | REP exprs_opt ')' elems
       until_last exprs_opt ')'
       elems END
                                { if (nilp($4))
                                    yyerr("empty rep clause");
                                  if (nilp($8))
                                    yyerr("empty until/last in rep");
                                  $$ = list(rep_s, $4, cons(cdr($5),
                                                            cons($6, $8)),
                                            $2, nao);
                                  rl($$, num($1));
                                  rl($6, car($5)); }
     | COLL error               { $$ = nil;
                                  yybadtok(yychar, lit("coll clause")); }
     | REP error               { $$ = nil;
                                 yybadtok(yychar, lit("rep clause")); }
     | ALL clause_parts_h       { $$ = rl(list(all_s, t, $2, nao), num($1));
                                  if (nilp($2))
                                     yyerr("empty all clause"); }
     | SOME exprs_opt ')'
       clause_parts_h           { $$ = rl(list(some_s, t, $4, $2, nao), num($1));
                                  if (nilp($4))
                                    yyerr("empty some clause"); }
     | NONE clause_parts_h      { $$ = rl(list(none_s, t, $2, nao), num($1));
                                  if (nilp($2))
                                    yyerr("empty none clause"); }
     | MAYBE clause_parts_h     { $$ = rl(list(maybe_s, t, $2, nao), num($1));
                                  if (nilp($2))
                                    yyerr("empty maybe clause"); }
     | CASES clause_parts_h     { $$ = rl(list(cases_s, t, $2, nao), num($1));
                                  if (nilp($2))
                                    yyerr("empty cases clause"); }
     | CHOOSE exprs_opt ')'
       clause_parts_h           { $$ = list(choose_s, t, $4, $2, nao);
                                  rl($$, num($1));
                                  if (nilp($4))
                                    yyerr("empty cases clause"); }
     | DEFINE exprs ')' elems END
                                { $$ = list(define_s, t, $4, $2, nao);
                                  rl($$, num($1)); }
     ;

clause_parts_h : elems_opt additional_parts_h   { $$ = if2($1, cons($1, $2)); }
               ;

additional_parts_h : END                        { $$ = nil; }
                   | AND clause_parts_h         { $$ = $2;
                                                  if (nilp($$))
                                                    yyerr("empty and subclause"); }
                   | OR clause_parts_h          { $$ = $2;
                                                  if (nilp($$))
                                                    yyerr("empty or subclause"); }
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
                                  yybadtok(yychar, lit("list expression")); }
              | DEFINE exprs ')' newl
                error           { $$ = nil; yybadtok(yychar, lit("define")); }
              | DEFINE ')' newl
                error           { $$ = nil;
                                  yybadtok(yychar, lit("define")); }
              ;

try_clause : TRY newl
             clauses_opt
             catch_clauses_opt
             END newl           { if (nilp($3))
                                    yyerr("empty try clause");
                                  $$ = list(try_s,
                                            flatten(mapcar(func_n1(second),
                                                           $4)),
                                            $3, $4, nao);
                                  rl($$, num($1)); }
           | TRY newl
             error              { $$ = nil;
                                  yybadtok(yychar, lit("try clause")); }
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
                                          yybadtok(yychar, lit("catch clause")); }
                  | CATCH exprs ')' newl
                    error               { $$ = nil;
                                          yybadtok(yychar, lit("catch clause")); }
                  | FINALLY newl
                    error               { $$ = nil;
                                          yybadtok(yychar, lit("finally clause")); }
                  ;


output_clause : OUTPUT ')' o_elems '\n'
                out_clauses
                END newl        { $$ = nil;
                                  yyerr("obsolete output syntax: trailing material"); }
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
                                  yyerr("invalid combination of old and "
                                               "new syntax in output directive"); }
              | OUTPUT error    { $$ = nil;
                                  yybadtok(yychar, lit("list expression")); }
              | OUTPUT ')' o_elems '\n'
                error           { $$ = nil;
                                  yybadtok(yychar, lit("output clause")); }
              | OUTPUT ')' newl
                error           { $$ = nil;
                                  yybadtok(yychar, lit("output clause")); }
              | OUTPUT exprs ')' o_elems '\n'
                error           { $$ = nil;
                                  yybadtok(yychar, lit("output clause")); }
              | OUTPUT exprs ')' newl
                error           { $$ = nil;
                                  yybadtok(yychar, lit("output clause")); }
              ;

out_clauses : out_clause                { $$ = cons($1, nil); }
            | out_clause out_clauses    { $$ = cons($1, $2);  }
            ;

out_clause : repeat_clause              { $$ = cons($1, nil); }
           | o_line                     { $$ = $1; }
           ;

repeat_clause : REPEAT n_exprs_opt ')' newl
                out_clauses_opt
                repeat_parts_opt
                END newl                { $$ = repeat_rep_helper(repeat_s,
                                                                 $2, $5, $6);
                                          rl($$, num($1)); }
              | REPEAT newl
                error           { $$ = nil;
                                  yybadtok(yychar, lit("repeat clause")); }
              ;

repeat_parts_opt : SINGLE newl
                   out_clauses_opt
                   repeat_parts_opt     { $$ = cons(cons(single_s, $3), $4);
                                          rl($$, num($1)); }
                 | FIRST newl
                   out_clauses_opt
                   repeat_parts_opt     { $$ = cons(cons(first_s, $3), $4);
                                          rl($$, num($1)); }
                 | LAST exprs_opt ')' newl
                   out_clauses_opt
                   repeat_parts_opt     { if ($2)
                                            yyerrorf(scnr,
                                                     lit("last: in output, "
                                                         "takes no arguments"),
                                                     nao);
                                          $$ = cons(cons(last_s, $5), $6);
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

o_elems_opt : o_elems           { $$ = $1;
                                  rl($$, num(parser->lineno)); }
            |                   { $$ = nil; }
            ;

o_elems : o_elem                { $$ = cons($1, nil); }
        | o_elem o_elems        { $$ = cons($1, $2); }
        | not_a_clause          { $$ = cons($1, nil); }
        | not_a_clause o_elems  { $$ = cons($1, $2); }
        ;

o_elem : TEXT                   { $$ = string_own($1);
                                  rl($$, num(parser->lineno)); }
       | SPACE                  { $$ = string_own($1);
                                  rl($$, num(parser->lineno)); }
       | o_var                  { $$ = $1; }
       | list                   { $$ = rlcp(list(expr_s,
                                                 expand($1, nil), nao), $1); }
       | rep_elem               { $$ = $1; }
       ;

rep_elem : REP n_exprs_opt ')' o_elems_opt
           rep_parts_opt END    { $$ = repeat_rep_helper(rep_s, $2, $4, $5);
                                  rl($$, num($1)); }
         | REP error            { $$ = nil;
                                  yybadtok(yychar, lit("rep clause")); }
         ;

rep_parts_opt : SINGLE o_elems_opt
                rep_parts_opt           { $$ = cons(cons(single_s, $2), $3);
                                          rl($$, num($1)); }
              | FIRST o_elems_opt
                rep_parts_opt           { $$ = cons(cons(first_s, $2), $3);
                                          rl($$, num($1)); }
              | LAST exprs_opt ')'
                o_elems_opt rep_parts_opt { if ($2)
                                            yyerrorf(scnr,
                                                     lit("last: in output, "
                                                         "takes no arguments"),
                                                     nao);
                                          $$ = cons(cons(last_s, $4), $5);
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
var : SYMTOK                    { $$ = list(var_s, symhlpr($1, nil), nao); }
    | '{' SYMTOK '}'            { $$ = list(var_s, symhlpr($2, nil), nao); }
    | '{' SYMTOK modifiers '}'  { $$ = list(var_s, symhlpr($2, nil), $3, nao); }
    | var_op SYMTOK             { $$ = list(var_s, symhlpr($2, nil), $1, nao); }
    | var_op '{' SYMTOK '}'     { $$ = list(var_s, symhlpr($3, nil), $1, nao); }
    | var_op '{' SYMTOK regex '}'       { $$ = nil;
                                          yyerr("longest match "
                                                "not useable with regex"); }
    | var_op '{' SYMTOK NUMBER '}'      { $$ = nil;
                                          yyerr("longest match "
                                                "not useable with "
                                                "fixed width match"); }
    | SYMTOK error              { $$ = nil;
                                  yybadtok(yychar, lit("variable spec")); }
    | var_op error              { $$ = nil;
                                  yybadtok(yychar, lit("variable spec")); }
    ;

var_op : '*'                    { $$ = list(t, nao); }
       ;

modifiers : NUMBER              { $$ = cons($1, nil); }
          | regex               { $$ = cons($1, nil);
                                  rlcp($$, $1); }
          | list                { $$ = rlcp(cons(expand_meta($1, nil),
                                                 nil), $1); }
          ;

o_var : SYMTOK                  { val expr = symhlpr($1, nil);
                                  if (!opt_compat || opt_compat > 128)
                                    expr = expand(expr, nil);
                                  $$ = list(var_s, expr, nao);
                                  rl($$, num(parser->lineno)); }
      | '{' n_expr n_exprs_opt '}'
                                { if (opt_compat && opt_compat <= 128)
                                  { $$ = list(var_s,
                                              expand_meta($2, nil),
                                              expand_meta($3, nil), nao); }
                                  else
                                  { $$ = list(var_s,
                                              expand($2, nil),
                                              expand($3, nil), nao); }
                                  rl($$, num(parser->lineno)); }
      | SYMTOK error            { $$ = nil;
                                  yybadtok(yychar, lit("variable spec")); }
      ;

q_var : '@' '{' n_expr n_exprs_opt '}'
                                { $$ = list(var_s, $3, $4, nao);
                                  rl($$, num(parser->lineno)); }
      | '@' '{' error           { $$ = nil;
                                  yybadtok(yychar, lit("variable spec")); }
      ;


vector : '#' list               { if (unquotes_occur($2, 0))
                                    $$ = rlcp(cons(vector_lit_s,
                                                   cons($2, nil)), $2);
                                  else
                                    $$ = rlcp(vec_list($2), $2); }
       ;

hash : HASH_H list              { if (unquotes_occur($2, 0))
                                    $$ = rl(cons(hash_lit_s, $2), num($1));
                                  else
                                    $$ = rl(hash_construct(first($2),
                                                             rest($2)),
                                            num($1)); }
     ;

struct : HASH_S list            { if (unquotes_occur($2, 0))
                                    $$ = rl(cons(struct_lit_s, $2),
                                              num($1));
                                  else
                                  { val strct = make_struct_lit(first($2),
                                                                rest($2));
                                    $$ = rl(strct, num($1)); } }
       ;

range : HASH_R list             { if (length($2) != two)
                                    yyerr("range literal needs two elements");

                                  if (unquotes_occur($2, 0))
                                    $$ = rl(cons(rcons_s, $2), num($1));
                                  else
                                  { val range = rcons(first($2), second($2));
                                    $$ = rl(range, num($1)); } }
       ;

list : '(' n_exprs ')'          { $$ = rl($2, num($1)); }
     | '(' ')'                  { $$ = nil; }
     | '(' LAMBDOT n_expr ')'   { $$ = $3; }
     | '(' CONSDOT n_expr ')'   { $$ = $3; }
     | '[' n_exprs ']'          { $$ = rl(cons(dwim_s, $2), num($1)); }
     | '[' ']'                  { $$ = rl(cons(dwim_s, nil), num($1)); }
     | '@' n_expr               { if (consp($2))
                                    $$ = rl(cons(expr_s, cons($2, nil)), num($1));
                                  else
                                    $$ = rl(cons(var_s, cons($2, nil)),
                                            num($1)); }
     | '(' error                { $$ = nil;
                                  yybadtok(yychar, lit("list expression")); }
     | '[' error                { $$ = nil;
                                  yybadtok(yychar, lit("DWIM expression")); }
     | '@' error                { $$ = nil;
                                  yybadtok(yychar, lit("meta expression")); }
     ;

exprs : n_exprs                 { $$ = rlcp(expand_meta($1, nil), $1); }
      ;

exprs_opt : exprs               { $$ = $1; }
          | /* empty */         { $$ = nil; }
          ;

n_exprs : r_exprs               { val term_atom = pop(&$1);
                                  val tail_cons = $1;
                                  $$ = nreverse($1);
                                  if (term_atom != unique_s)
                                    rplacd(tail_cons, term_atom); }
        ;

r_exprs : n_expr                { val exprs = cons($1, nil);
                                  rlcp(exprs, $1);
                                  $$ = rlcp(cons(unique_s, exprs), exprs); }
        | HASH_SEMI             { parser->circ_suppress = 1; }
          n_expr                { parser->circ_suppress = 0;
                                  $$ = cons(unique_s, nil); }
        | r_exprs HASH_SEMI     { parser->circ_suppress = 1; }
          n_expr                { parser->circ_suppress = 0;
                                  $$ = $1; }
        | r_exprs n_expr        { uses_or2;
                                  val term_atom_cons = $1;
                                  val exprs = cdr($1);
                                  misplaced_consing_dot_check(scnr, term_atom_cons);
                                  rplacd(term_atom_cons,
                                         rlcp(cons($2, exprs), or2($2, exprs)));
                                  $$ = term_atom_cons; }
        | r_exprs CONSDOT n_expr
                                { val term_atom_cons = $1;
                                  misplaced_consing_dot_check(scnr, term_atom_cons);
                                  rplaca(term_atom_cons, $3);
                                  $$ = $1; }
        | WSPLICE wordslit      { $$ = cons(unique_s, nreverse(rl($2, num($1))));
                                  rlcp($$, cdr($$)); }
        | r_exprs WSPLICE
          wordslit              { val term_atom_cons = $1;
                                  val exprs = cdr($1);
                                  misplaced_consing_dot_check(scnr, term_atom_cons);
                                  rplacd(term_atom_cons,
                                         nappend2(rl(nreverse($3), num($2)),
                                                  exprs));
                                  $$ = term_atom_cons; }
        | QWSPLICE wordsqlit    { $$ = cons(unique_s, rl($2, num($1)));
                                  rlcp($$, cdr($$)); }
        | r_exprs QWSPLICE
          wordsqlit             { val term_atom_cons = $1;
                                  val exprs = cdr($1);
                                  misplaced_consing_dot_check(scnr, term_atom_cons);
                                  rplacd(term_atom_cons,
                                         nappend2(rl(nreverse($3), num($2)),
                                                  exprs));
                                  $$ = term_atom_cons; }
        ;

i_expr : SYMTOK                 { $$ = symhlpr($1, t); }
       | METANUM                { $$ = cons(var_s, cons($1, nil));
                                  rl($$, num(parser->lineno)); }
       | NUMBER                 { $$ = $1; }
       | list                   { $$ = $1; }
       | vector                 { $$ = $1; }
       | hash                   { $$ = $1; }
       | struct                 { $$ = $1; }
       | range                  { $$ = $1; }
       | lisp_regex             { $$ = $1; }
       | chrlit                 { $$ = $1; }
       | strlit                 { $$ = $1; }
       | quasilit               { $$ = $1; }
       | WORDS wordslit         { $$ = rl($2, num($1)); }
       | QWORDS wordsqlit       { $$ = rl(cons(quasilist_s, $2), num($1)); }
       | '\'' i_expr            { $$ = rl(rlcp(list(quote_s, $2, nao), $2),
                                          num(parser->lineno)); }
       | '^' i_expr             { $$ = rl(rlcp(list(sys_qquote_s, $2, nao), $2),
                                          num(parser->lineno)); }
       | ',' i_expr             { $$ = rl(rlcp(list(sys_unquote_s, $2, nao), $2),
                                          num(parser->lineno)); }
       | SPLICE i_expr          { $$ = rl(rlcp(list(sys_splice_s, $2, nao), $2),
                                          num(parser->lineno)); }
       | HASH_N_EQUALS          { parser_circ_def(parser, $1, unique_s); }
         i_expr                 { parser_circ_def(parser, $1, $3);
                                  $$ = $3; }
       | HASH_N_HASH            { $$ = parser_circ_ref(parser, $1); }
       ;

n_expr : SYMTOK                 { $$ = symhlpr($1, t); }
       | METANUM                { $$ = cons(var_s, cons($1, nil));
                                  rl($$, num(parser->lineno)); }
       | NUMBER                 { $$ = $1; }
       | list                   { $$ = $1; }
       | vector                 { $$ = $1; }
       | hash                   { $$ = $1; }
       | struct                 { $$ = $1; }
       | range                  { $$ = $1; }
       | lisp_regex             { $$ = $1; }
       | chrlit                 { $$ = $1; }
       | strlit                 { $$ = $1; }
       | quasilit               { $$ = $1; }
       | WORDS wordslit         { $$ = rl($2, num($1)); }
       | QWORDS wordsqlit       { $$ = rl(cons(quasilist_s, $2), num($1)); }
       | '\'' n_expr            { $$ = rl(rlcp(list(quote_s, $2, nao), $2),
                                          num(parser->lineno)); }
       | '^' n_expr             { $$ = rl(rlcp(list(sys_qquote_s, $2, nao), $2),
                                          num(parser->lineno)); }
       | ',' n_expr             { $$ = rl(rlcp(list(sys_unquote_s, $2, nao), $2),
                                          num(parser->lineno)); }
       | SPLICE n_expr          { $$ = rl(rlcp(list(sys_splice_s, $2, nao), $2),
                                          num(parser->lineno)); }
       | n_expr DOTDOT n_expr   { uses_or2;
                                  $$ = rlcp(list(rcons_s, $1, $3, nao),
                                            or2($1, $3)); }
       | n_expr '.' n_expr      { uses_or2;
                                  if (consp($3) && car($3) == qref_s) {
                                    rplacd($3, rlcp(cons($1, cdr($3)), $1));
                                    rl($$, num(parser->lineno));
                                    $$ = $3;
                                  } else {
                                    $$ = rl(rlcp(list(qref_s, $1, $3, nao),
                                                 or2($1, $3)),
                                                 num(parser->lineno));
                                  } }
       | HASH_N_EQUALS          { parser_circ_def(parser, $1, unique_s); }
         n_expr                 { parser_circ_def(parser, $1, $3);
                                  $$ = $3; }
       | HASH_N_HASH            { $$ = parser_circ_ref(parser, $1); }
       ;

n_exprs_opt : n_exprs           { $$ = $1; }
            | /* empty */       { $$ = nil; }
          ;

regex : '/' regexpr '/'         { $$ = regex_compile($2, nil);
                                  end_of_regex(scnr);
                                  rl($$, num(parser->lineno)); }
      | '/' error               { $$ = nil;
                                  yybadtok(yychar, lit("regex"));
                                  end_of_regex(scnr); }
      ;

lisp_regex : HASH_SLASH regexpr '/'
                                { $$ = regex_compile($2, nil);
                                  end_of_regex(scnr);
                                  rl($$, num(parser->lineno)); }
           | HASH_SLASH error
                                { $$ = nil;
                                  yybadtok(yychar, lit("regex"));
                                  end_of_regex(scnr); }
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
                                  yybadtok(yychar, lit("regex character class")); }
        | '.'                   { $$ = wild_s; }
        | ']'                   { $$ = chr(']'); }
        | '-'                   { $$ = chr('-'); }
        | REGCHAR               { $$ = chr($1); }
        | regtoken              { $$ = $1; }
        | '(' regexpr ')'       { $$ = $2; }
        | '(' error             { $$ = nil;
                                  yybadtok(yychar, lit("regex subexpression")); }
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
     | error '\n'       { yyerr("newline expected after directive");
                          yyerrok; }
     ;

strlit : '"' '"'                { $$ = null_string; }
       | '"' litchars '"'       { $$ = lit_char_helper($2);
                                  rl($$, num(parser->lineno)); }
       | '"' error              { $$ = nil;
                                  yybadtok(yychar, lit("string literal")); }
       ;

chrlit : HASH_BACKSLASH SYMTOK  { wchar_t ch;
                                  val str = string_own($2);
                                  const wchar_t *cstr = c_str(str);

                                  if (cstr[1] == 0)
                                  { ch = cstr[0]; }
                                  else
                                  { ch = char_from_name(cstr);
                                    if (ch == L'!')
                                    { yyerrorf(scnr, lit("unknown character name: ~a"),
                                               str, nao); }}
                                  end_of_char(scnr);
                                  $$ = chr(ch); }
       | HASH_BACKSLASH LITCHAR { $$ = chr($2);
                                  end_of_char(scnr); }
       | HASH_BACKSLASH error   { $$ = nil;
                                  yybadtok(yychar,
                                             lit("character literal")); }
       ;

quasilit : '`' '`'              { $$ = null_string; }
         | '`' quasi_items '`'  { $$ = cons(quasi_s, $2);
                                  rlcp($$, $2);
                                  rl($$, num(parser->lineno)); }
         | '`' error            { $$ = nil;
                                  yybadtok(yychar, lit("quasistring")); }
         ;

quasi_items : quasi_item                { $$ = cons($1, nil);
                                          rl($$, num(parser->lineno)); }
            | quasi_item quasi_items    { $$ = cons($1, $2);
                                          rl($$, num(parser->lineno)); }
            ;

quasi_item : litchars           { $$ = lit_char_helper($1); }
           | TEXT               { $$ = string_own($1); }
           | q_var              { $$ = $1; }
           | METANUM            { $$ = cons(var_s, cons($1, nil));
                                  rl($$, num(parser->lineno)); }
           | '@' n_expr         { if (integerp($2) || symbolp($2))
                                    $$ = rlcp_tree(cons(var_s, cons($2, nil)),
                                                   $2);
                                  else
                                    $$ = $2; }
           ;

litchars : LITCHAR              { $$ = rl(cons(chr($1), nil), num(parser->lineno)); }
         | LITCHAR litchars     { $$ = rl(cons(chr($1), $2), num(parser->lineno)); }
         ;

wordslit : '"'                  { $$ = nil; }
         | ' ' wordslit         { $$ = $2; }
         | litchars wordslit    { val word = lit_char_helper($1);
                                  $$ = rlcp(cons(word, $2), $1); }
         | error                { $$ = nil;
                                  yybadtok(yychar, lit("word list")); }
         ;

wordsqlit : '`'                  { $$ = nil; }
          | ' ' wordsqlit        { $$ = $2; }
          | quasi_items '`'      { val qword = cons(quasi_s, $1);
                                   $$ = rlcp(cons(qword, nil), $1); }
          | quasi_items ' '
            wordsqlit
                                 { val qword = cons(quasi_s, $1);
                                   $$ = rlcp(cons(qword, $3), $1); }
          ;

not_a_clause : ALL              { $$ = mkexp(all_s, nil, num(parser->lineno)); }
             | SOME             { $$ = mkexp(some_s, nil, num(parser->lineno)); }
             | NONE             { $$ = mkexp(none_s, nil, num(parser->lineno)); }
             | MAYBE            { $$ = mkexp(maybe_s, nil, num(parser->lineno)); }
             | CASES            { $$ = mkexp(cases_s, nil, num(parser->lineno)); }
             | AND              { $$ = mkexp(and_s, nil, num(parser->lineno)); }
             | OR               { $$ = mkexp(or_s, nil, num(parser->lineno)); }
             | TRY              { $$ = mkexp(try_s, nil, num(parser->lineno)); }
             | FINALLY          { $$ = mkexp(finally_s, nil, num(parser->lineno)); }
             | ELSE             { $$ = mkexp(intern(lit("else"), nil),
                                             nil, num(parser->lineno)); }
             | ELIF             { $$ = mkexp(intern(lit("elif"), nil),
                                             nil, num(parser->lineno)); }
             | BLOCK
               exprs_opt ')'    { $$ = mkexp(block_s, $2, nil); }
             | CHOOSE
               exprs_opt ')'    { $$ = mkexp(choose_s, $2, nil); }
             | COLLECT
               exprs_opt ')'    { $$ = mkexp(collect_s, $2, nil); }
             | COLL
               exprs_opt ')'    { $$ = mkexp(coll_s, $2, nil); }
             | GATHER
               exprs_opt ')'    { $$ = mkexp(gather_s, $2, nil); }
             | DEFINE
               exprs_opt ')'    { $$ = mkexp(define_s, $2, nil); }
             | CATCH
               exprs_opt ')'    { $$ = mkexp(catch_s, $2, nil); }
             | IF
               exprs_opt ')'    { $$ = mkexp(intern(lit("if"), nil),
                                                 $2, nil); }
             | OUTPUT
                exprs_opt ')'   { yyerr("@(output) doesn't nest"); }

             ;

%%

const int have_yydebug = YYDEBUG;

int yylex(YYSTYPE *, yyscan_t scanner);

/* C99 inline instantiations. */
#if __STDC_VERSION__ >= 199901L
val rlcp(val to, val from);
#endif

void yydebug_onoff(int val)
{
#if YYDEBUG
  yydebug = val;
#endif
}

static val sym_helper(parser_t *parser, wchar_t *lexeme, val meta_allowed)
{
  scanner_t *scnr = parser->scanner;
  int leading_at = *lexeme == L'@';
  wchar_t *tokfree = lexeme;
  wchar_t *colon = wcschr(lexeme, L':');
  val sym;

  if (leading_at) {
    if (!meta_allowed) {
      val tok = string_own(lexeme);
      yyerrorf(scnr, lit("~a: meta variable not allowed in this context"), tok, nao);
      return nil;
    }
    lexeme++;
  }

  if (colon != 0)
    *colon = 0;

  if (colon == lexeme) {
    val sym_name = string(colon + 1);
    scrub_scanner(parser->scanner, SYMTOK, tokfree);
    free(tokfree);
    sym = intern(sym_name, keyword_package_var);
  } else if (colon != 0) {
    val pkg_name = string(lexeme);
    val sym_name = string(colon + 1);
    scrub_scanner(parser->scanner, SYMTOK, tokfree);
    free(tokfree);
    if (equal(pkg_name, lit("#"))) {
      sym = make_sym(sym_name);
    } else {
      val package = find_package(pkg_name);
      if (!package) {
        yyerrorf(scnr, lit("~a:~a: package ~a not found"),
                 pkg_name, sym_name, pkg_name, nao);
        return nil;
      }

      sym = find_symbol(sym_name, package);

      if (sym == zero) {
        if (!package_fallback_list(package)) {
          sym = intern(sym_name, package);
        } else {
          yyerrorf(scnr, lit("~a:~a: cannot intern symbol using qualified symbol syntax,"),
                   pkg_name, sym_name, nao);
          yyerrorf(scnr, lit("~a:~a: because package ~a has a fallback list"),
                   pkg_name, sym_name, pkg_name, nao);
          return nil;
        }
      }
    }
  } else {
    val sym_name = string(lexeme);
    scrub_scanner(parser->scanner, SYMTOK, tokfree);
    free(tokfree);
    sym = intern_fallback(sym_name, cur_package);
  }

  return leading_at ? rl(list(var_s, sym, nao), num(parser->lineno)) : sym;
}

static val expand_repeat_rep_args(val args)
{
  list_collect_decl (out, ptail);
  val exp_pair = nil, exp_pairs = nil;

  for (; args; args = cdr(args)) {
    val arg = car(args);

    if (consp(arg)) {
      if (exp_pairs) {
        list_collect_decl (iout, iptail);
        for (; arg; arg = cdr(arg)) {
          val iarg = car(arg);
          if (consp(iarg))
            iptail = list_collect(iptail, list(first(iarg),
                                               expand(second(iarg), nil),
                                               nao));
          else
            iptail = list_collect(iptail, iarg);
        }
        ptail = list_collect(ptail, iout);
      } else if (exp_pair) {
        ptail = list_collect(ptail, list(first(arg),
                                         expand(second(arg), nil),
                                         nao));
      } else {
        ptail = list_collect(ptail, arg);
      }
    } else if (arg == counter_k) {
      exp_pair = t;
      ptail = list_collect(ptail, arg);
      continue;
    } else if (arg == vars_k) {
      exp_pairs = t;
      ptail = list_collect(ptail, arg);
      continue;
    }

    exp_pair = exp_pairs = nil;
    ptail = list_collect(ptail, arg);
  }

  return out;
}

static val repeat_rep_helper(val sym, val args, val main, val parts)
{
  uses_or2;
  val exp_args = expand_repeat_rep_args(args);
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

  return list(sym, exp_args, main, single_parts, first_parts,
              last_parts, empty_parts, nreverse(mod_parts),
              nreverse(modlast_parts), nao);
}

static val define_transform(parser_t *parser, val define_form)
{
  scanner_t *scnr = parser->scanner;
  val sym = first(define_form);
  val args = second(define_form);

  if (define_form == nil)
    return nil;

  assert (sym == define_s);

  if (args == nil) {
    yyerr("define requires arguments");
    return define_form;
  }

  if (!consp(args) || !listp(cdr(args))) {
    yyerr("bad define argument syntax");
    return define_form;
  } else {
    val name = first(args);
    val params = second(args);

    if (!symbolp(name)) {
      yyerr("function name must be a symbol");
      return define_form;
    }

    if (!proper_list_p(params)) {
      yyerr("invalid function parameter list");
      return define_form;
    }

    if (!all_satisfy(params, func_n1(symbolp), nil))
      yyerr("function parameters must be symbols");
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

static val unquotes_occur(val quoted_form, int level)
{
  uses_or2;

  if (atom(quoted_form)) {
    return nil;
  } else {
    val sym = car(quoted_form);
    if (sym == sys_unquote_s || sym == sys_splice_s)
      return (level == 0) ? t : unquotes_occur(cdr(quoted_form), level - 1);
    if (sym == sys_qquote_s)
      return unquotes_occur(cdr(quoted_form), level + 1);
    return or2(unquotes_occur(sym, level),
               unquotes_occur(cdr(quoted_form), level));
  }
}

static val expand_meta(val form, val menv)
{
  val sym;

  if (atom(form))
    return form;

  menv = default_arg(menv, make_env(nil, nil, nil));

  if ((sym = car(form)) == quasi_s) {
    if (opt_compat && opt_compat <= 128) {
      list_collect_decl (out, ptail);

      for (; consp(form); form = cdr(form)) {
        val subform = car(form);
        if (consp(subform) && car(subform) == expr_s)
          ptail = list_collect(ptail, expand_meta(subform, menv));
        else
          ptail = list_collect(ptail, subform);
      }

      ptail = list_collect_nconc(ptail, form);

      return rlcp(out, form);
    }

    return expand(form, nil);
  }

  if ((sym = car(form)) == expr_s) {
    val exp_x = expand(second(form), menv);
    if (!bindable(exp_x))
      return rlcp(cons(sym, cons(exp_x, nil)), form);
    return rlcp(cons(var_s, cons(exp_x, nil)), form);
  }

  if (sym == var_s) {
    val var_x = expand(second(form), menv);
    if (!bindable(var_x))
      return rlcp(cons(expr_s, var_x), form);
    return rlcp(cons(var_s, cons(var_x, nil)), form);
  }

  {
    list_collect_decl (out, ptail);

    for (; consp(form); form = cdr(form)) {
      loc nptail = list_collect(ptail, expand_meta(car(form), menv));
      rlcp(deref(ptail), form);
      ptail = nptail;
    }

    ptail = list_collect_nconc(ptail, form);

    return out;
  }
}

val rlset(val form, val info)
{
  val cell = gethash_c(form_to_ln_hash, form, nulloc);
  loc place = cdr_l(cell);
  if (nilp(deref(place)))
    set(place, info);
  return form;
}

val rlrec(parser_t *parser, val form, val line)
{
  rlset(form, cons(line, parser->name));
  return form;
}

static val rlcp_tree_rec(val to, val from, struct circ_stack *up)
{
  val ret = to;

  while (consp(to)) {
    val a = car(to);
    struct circ_stack rlcs = { up, a };
    rlcp(to, from);
    if (!parser_callgraph_circ_check(up, a))
      break;
    rlcp_tree_rec(a, from, &rlcs);
    to = cdr(to);
    if (!parser_callgraph_circ_check(up, to))
      break;
  }
  return ret;
}


val rlcp_tree(val to, val from)
{
  return rlcp_tree_rec(to, from, 0);
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
    { L"pnul", 0xDC00 },
    { 0, 0 },
  };
  int i;

  for (i = 0; map[i].name; i++) {
    if (wcscmp(map[i].name, name) == 0)
      return map[i].ch;
  }

  return L'!'; /* code meaning not found */
}

static val make_expr(parser_t *parser, val sym, val rest, val lineno)
{
  val expr = cons(sym, rest);
  val ret = cons(expr_s, cons(expand(expr, nil), nil));

  if (rest) {
    rlcp(expr, rest);
    rlcp(ret, rest);
  } else {
    rl(expr, lineno);
    rl(ret, lineno);
  }

  return ret;
}

static val check_for_include(val spec_rev)
{
  val line = first(spec_rev);

  if (consp(line)) {
    val elem = first(line);
    if (consp(elem)) {
      if (car(elem) == include_s)
        return nappend2(nreverse(include(line)), rest(spec_rev));
    }
  }
  return spec_rev;
}

static void misplaced_consing_dot_check(scanner_t *scanner, val term_atom_cons)
{
  if (car(term_atom_cons) != unique_s) {
    yyerrorf(scanner, lit("misplaced consing dot"), nao);
    rplaca(term_atom_cons, unique_s);
  }
}

#ifndef YYEOF
#define YYEOF 0
#endif

void yybadtoken(parser_t *parser, int tok, val context)
{
  val problem = nil;
  scanner_t *scnr = parser->scanner;

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
  case IF:      problem = lit("\"if\""); break;
  case ELIF:    problem = lit("\"elif\""); break;
  case ELSE:    problem = lit("\"else\""); break;
  case NUMBER:  problem = lit("number"); break;
  case REGCHAR: problem = lit("regular expression character"); break;
  case REGTOKEN: problem = lit("regular expression token"); break;
  case LITCHAR: problem = lit("string literal character"); break;
  case CONSDOT: problem = lit("consing dot"); break;
  case LAMBDOT: problem = lit("consing dot"); break;
  case DOTDOT: problem = lit(".."); break;
  case HASH_BACKSLASH: problem = lit("#\\"); break;
  case HASH_SLASH:     problem = lit("#/"); break;
  case HASH_H:         problem = lit("#H"); break;
  case HASH_S:         problem = lit("#S"); break;
  case HASH_R:         problem = lit("#R"); break;
  case HASH_SEMI:      problem = lit("#;"); break;
  case HASH_N_EQUALS:  problem = lit("#<n>="); break;
  case HASH_N_HASH:    problem = lit("#<n>#"); break;
  case WORDS:   problem = lit("#\""); break;
  case WSPLICE: problem = lit("#*\""); break;
  case QWORDS:         problem = lit("#`"); break;
  case QWSPLICE:       problem = lit("#*`"); break;
  }

  if (problem != 0)
    if (context)
      yyerrorf(scnr, lit("misplaced ~a in ~a"), problem, context, nao);
    else
      yyerrorf(scnr, lit("unexpected ~a"), problem, nao);
  else
    if (context) /* Byacc sets yychar to 0 */
      if (tok == YYEOF || tok == YYEMPTY)
        yyerrorf(scnr, lit("unterminated ~a"), context, nao);
      else if (tok == '\n')
        yyerrorf(scnr, lit("newline in ~a"), context, nao);
      else
        yyerrorf(scnr, lit("misplaced character ~a in ~a"), chr(tok), context, nao);
    else
      if (tok == YYEOF)
        yyerrorf(scnr, lit("unexpected end of input"), nao);
      else if (tok == YYEMPTY)
        return;
      else
        yyerrorf(scnr, lit("unexpected character ~a"), chr(tok), nao);
}

static val warning_continue(val exc, val arg)
{
  uw_throw(continue_s, nil);
}

int parse_once(val stream, val name, parser_t *parser)
{
  int res = 0;
  uw_frame_t uw_handler;
#if CONFIG_DEBUG_SUPPORT
  debug_state_t ds = debug_set_state(opt_dbg_expansion ? 0 : -1,
                                     opt_dbg_expansion);
#endif

  parser_common_init(parser);

  parser->stream = stream;
  parser->name = name;

  uw_push_handler(&uw_handler, cons(warning_s, nil), func_n2(warning_continue));

  uw_catch_begin(cons(error_s, nil), esym, eobj);

  res = yyparse(parser->scanner, parser);

  parser_resolve_circ(parser);

  uw_catch(esym, eobj) {
    yyerrorf(parser->scanner, lit("exception during parse"), nao);
    uw_throw(esym, eobj);
  }

  uw_unwind {
    parser_cleanup(parser);
#if CONFIG_DEBUG_SUPPORT
    debug_restore_state(ds);
#endif
  }

  uw_catch_end;

  uw_pop_frame(&uw_handler);

  return res;
}

int parse(parser_t *parser, val name, enum prime_parser prim)
{
  int res = 0;

  parser->errors = 0;
  parser->prepared_msg = nil;
  parser->circ_ref_hash = nil;
  parser->circ_count = 0;
  parser->circ_suppress = 0;
  parser->syntax_tree = nil;

  prime_parser(parser, name, prim);

  uw_catch_begin(cons(error_s, nil), esym, eobj);

  res = yyparse(parser->scanner, parser);

  prime_parser_post(parser, prim);

  parser_resolve_circ(parser);

  uw_catch(esym, eobj) {
    yyerrorf(parser->scanner, lit("exception during parse"), nao);
    uw_throw(esym, eobj);
  }

  uw_unwind;

  uw_catch_end;

  return res;
}
