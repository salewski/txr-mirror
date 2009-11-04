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
#include <string.h>
#include <stdarg.h>
#include <stdlib.h>
#include <limits.h>
#include <errno.h>
#include <dirent.h>
#include "y.tab.h"
#include "lib.h"
#include "gc.h"
#include "stream.h"
#include "parser.h"

#define YY_NO_UNPUT

#define YY_INPUT(buf, result, max_size)           \
  do {                                            \
    obj_t *c = nil;                               \
    int n, ch = '*';                              \
    for (n = 0; n < max_size &&                   \
                (c = get_char(yyin_stream)) &&    \
                (ch = c_chr(c)) != '\n'; ++n)     \
      buf[n] = (char) ch;                         \
    if (ch == '\n')                               \
      buf[n++] = (char) ch;                       \
    result = n;                                   \
  } while (0)

obj_t *yyin_stream;

long lineno = 1;
int opt_loglevel = 1;   /* 0 - quiet; 1 - normal; 2 - verbose */
int opt_nobindings = 0;
int opt_arraydims = 1;

int errors;

void yyerror(const char *s)
{
  yyerrorf("%s", s);
}

void yyerrorf(const char *s, ...)
{
  if (opt_loglevel >= 1) {
    va_list vl;
    va_start (vl, s);
    fprintf(stderr, "%s: (%s:%ld): ", progname, spec_file, lineno);
    vfprintf(stderr, s, vl);
    putc('\n', stderr);
    va_end (vl);
  }
  errors++;
}

void yybadtoken(int tok, const char *context)
{
  const char *problem = 0;

  switch (tok) {
  case TEXT:    problem = "text"; break;
  case IDENT:   problem = "identifier"; break;
  case ALL:     problem = "\"all\""; break;
  case SOME:    problem = "\"some\""; break;
  case NONE:    problem = "\"none\""; break;
  case MAYBE:   problem = "\"maybe\""; break;
  case CASES:   problem = "\"cases\""; break;
  case AND:     problem = "\"and\""; break;
  case OR:      problem = "\"or\""; break;
  case END:     problem = "\"end\""; break;
  case COLLECT: problem = "\"collect\""; break;
  case UNTIL:   problem = "\"until\""; break;
  case COLL:    problem = "\"coll\""; break;
  case OUTPUT:  problem = "\"output\""; break;
  case REPEAT:  problem = "\"repeat\""; break;
  case REP:     problem = "\"rep\""; break;
  case SINGLE:  problem = "\"single\""; break;
  case FIRST:   problem = "\"first\""; break;
  case LAST:    problem = "\"last\""; break;
  case EMPTY:   problem = "\"empty\""; break;
  case DEFINE:  problem = "\"define\""; break;
  case TRY:     problem = "\"try\""; break;
  case CATCH:   problem = "\"catch\""; break;
  case FINALLY: problem = "\"finally\""; break;
  case NUMBER:  problem = "\"number\""; break;
  case REGCHAR: problem = "regular expression character"; break;
  case LITCHAR: problem = "string literal character"; break;
  }

  if (problem != 0)
    if (context)
      yyerrorf("misplaced %s in %s", problem, context);
    else
      yyerrorf("unexpected %s", problem);
  else
    if (context)
      yyerrorf("unterminated %s", context);
    else
      yyerrorf("unexpected end of input");
}

static int char_esc(int letter)
{
  switch (letter) {
  case 'a': return '\a';
  case 'b': return '\b';
  case 't': return '\t';
  case 'n': return '\n';
  case 'v': return '\v';
  case 'f': return '\f';
  case 'r': return '\r';
  case 'e': return 27;
  case '"': return '"';
  case '\'': return '\'';
  case '`': return '`';
  }

  abort();
}

static int num_esc(char *num)
{
  if (num[0] == 'x') {
    if (strlen(num) > 3)
      yyerror("too many digits in hex character escape");
    return strtol(num + 1, 0, 16);
  } else {
    if (strlen(num) > 3)
      yyerror("too many digits in octal character escape");
    return strtol(num, 0, 8);
  }
}

%}

%option stack

TOK     [a-zA-Z_][a-zA-Z0-9_]*|[+-]?[0-9]+
ID_END  [^a-zA-Z0-9_]
NUM_END [^0-9]
WS      [\t ]*
HEX     [0-9A-Fa-f]
OCT     [0-7]

%x      SPECIAL NESTED REGEX REGCLASS STRLIT CHRLIT QSILIT

%%

<SPECIAL,NESTED>{TOK}   {
                          long val;
                          char *errp;

                          errno = 0;

                          val = strtol(yytext, &errp, 10);

                          if (yy_top_state() == INITIAL
                              || yy_top_state() == QSILIT)
                            yy_pop_state();

                          if (*errp != 0) {
                            /* not a number */
                            yylval.lexeme = strdup(yytext);
                            return IDENT;
                          }

                          if ((val == LONG_MAX || val == LONG_MIN)
                              && errno == ERANGE)
                            yyerror("numeric overflow in token");

                          yylval.num = val;
                          return NUMBER;
                        }

<SPECIAL>\({WS}all{WS}\)        {
                                  yy_pop_state();
                                  return ALL;
                                }

<SPECIAL>\({WS}some{WS}\)       {
                                  yy_pop_state();
                                  return SOME;
                                }

<SPECIAL>\({WS}none{WS}\)       {
                                  yy_pop_state();
                                  return NONE;
                                }

<SPECIAL>\({WS}maybe{WS}\)      {
                                  yy_pop_state();
                                  return MAYBE;
                                }

<SPECIAL>\({WS}cases{WS}\)      {
                                  yy_pop_state();
                                  return CASES;
                                }

<SPECIAL>\({WS}and{WS}\)        {
                                  yy_pop_state();
                                  return AND;
                                }

<SPECIAL>\({WS}or{WS}\)         {
                                  yy_pop_state();
                                  return OR;
                                }

<SPECIAL>\({WS}end{WS}\)        {
                                  yy_pop_state();
                                  return END;
                                }

<SPECIAL>\({WS}collect{WS}\)    {
                                  yy_pop_state();
                                  return COLLECT;
                                }

<SPECIAL>\({WS}coll{WS}\)       {
                                  yy_pop_state();
                                  return COLL;
                                }

<SPECIAL>\({WS}until{WS}\)      {
                                  yy_pop_state();
                                  return UNTIL;
                                }

<SPECIAL>\({WS}output/{ID_END}  {
                                  yy_push_state(NESTED);
                                  return OUTPUT;
                                }

<SPECIAL>\({WS}repeat{WS}\)     {
                                  yy_pop_state();
                                  return REPEAT;
                                }


<SPECIAL>\({WS}rep{WS}\)        {
                                  yy_pop_state();
                                  return REP;
                                }

<SPECIAL>\({WS}single{WS}\)     {
                                  yy_pop_state();
                                  return SINGLE;
                                }

<SPECIAL>\({WS}first{WS}\)      {
                                  yy_pop_state();
                                  return FIRST;
                                }

<SPECIAL>\({WS}last{WS}\)       {
                                  yy_pop_state();
                                  return LAST;
                                }

<SPECIAL>\({WS}empty{WS}\)      {
                                  yy_pop_state();
                                  return EMPTY;
                                }

<SPECIAL>\({WS}define/{ID_END}  {
                                  yy_push_state(NESTED);
                                  return DEFINE;
                                }

<SPECIAL>\({WS}try{WS}\)        {
                                  yy_pop_state();
                                  return TRY;
                                }

<SPECIAL>\({WS}catch/{ID_END}   {
                                  yy_push_state(NESTED);
                                  return CATCH;
                                }

<SPECIAL>\({WS}finally{WS}\)    {
                                  yy_pop_state();
                                  return FINALLY;
                                }

<SPECIAL,NESTED>\{|\(   {
                          yy_push_state(NESTED);
                          if (yy_top_state() == INITIAL
                              || yy_top_state() == QSILIT)
                            yy_pop_state();
                          return yytext[0];
                        }

<SPECIAL,NESTED>\}|\)   {
                          yy_pop_state();
                          if (yy_top_state() == INITIAL
                              || yy_top_state() == QSILIT)
                            yy_pop_state();
                          return yytext[0];
                        }

<SPECIAL,NESTED>[\t ]+  { /* Eat whitespace in directive */ }

<SPECIAL,NESTED>\"      {
                          yy_push_state(STRLIT);
                          return '"';
                        }

<SPECIAL,NESTED>\'      {
                          yy_push_state(CHRLIT);
                          return '\'';
                        }

<SPECIAL,NESTED>`       {
                          yy_push_state(QSILIT);
                          return '`';
                        }

<SPECIAL>@              {
                          yy_pop_state();
                          yylval.lexeme = strdup("@");
                          return TEXT;
                        }

<SPECIAL,NESTED>\n      {
                          lineno++;
                        }

<SPECIAL,NESTED>[/]     {
                          yy_push_state(REGEX);
                          return '/';
                        }

<SPECIAL,NESTED>\.      {
                          yylval.chr = '.';
                          return '.';
                        }

<SPECIAL>[\\][abtnvfre] {
                          char lexeme[2];
                          lexeme[0] = char_esc(yytext[1]);
                          lexeme[1] = 0;
                          yylval.lexeme = strdup(lexeme);
                          yy_pop_state();
                          return TEXT;
                        }

<SPECIAL>[\\](x{HEX}+|{OCT}+)   {
                                  char lexeme[2];
                                  lexeme[0] = num_esc(yytext + 1);
                                  lexeme[1] = 0;
                                  yylval.lexeme = strdup(lexeme);
                                  yy_pop_state();
                                  return TEXT;
                                }

<SPECIAL,NESTED>.       {
                          yyerrorf("bad character in directive: '%c'",
                                   yytext[0]);
                        }

<REGEX>[/]      {
                  yy_pop_state();
                  if (yy_top_state() == INITIAL
                      || yy_top_state() == QSILIT)
                    yy_pop_state();
                  yylval.chr = '/';
                  return '/';
                }


<REGEX>[\\][abtnvfre]   {
                          yylval.chr = char_esc(yytext[1]);
                          return REGCHAR;
                        }

<REGEX>[\\](x{HEX}+|{OCT}+)     {
                                  yylval.chr = num_esc(yytext + 1);
                                  return REGCHAR;
                                }

<REGEX>\n       {
                  lineno++;
                  yyerror("newline in regex");
                }

<REGEX>[.*?+^]  {
                  yylval.chr = yytext[0];
                  return yytext[0];
                }


<REGEX>[\[\]\-] {
                  yylval.chr = yytext[0];
                  return yytext[0];
                }

<REGEX>[()|]    {
                  yylval.chr = yytext[0];
                  return yytext[0];
                }

<REGEX>[\\].    {
                  yylval.chr = yytext[1];
                  return REGCHAR;
                }

<REGEX>.        {
                  yylval.chr = yytext[0];
                  return REGCHAR;
                }

<INITIAL>[^@\n]+        {
                          yylval.lexeme = strdup(yytext);
                          return TEXT;
                        }

<INITIAL>\n     {
                  lineno++;
                  return '\n';
                }

<INITIAL>@{WS}\*        {
                          yy_push_state(SPECIAL);
                          return '*';
                        }

<INITIAL>@      {
                  yy_push_state(SPECIAL);
                }

<INITIAL>^@#.*\n        {
                          /* eat whole line comment */
                          lineno++;
                        }

<INITIAL>@#.*   {
                   /* comment to end of line */
                }

<STRLIT>\"      {
                  yy_pop_state();
                  return yytext[0];
                }

<CHRLIT>\'      {
                  yy_pop_state();
                  return yytext[0];
                }

<QSILIT>`       {
                  yy_pop_state();
                  return yytext[0];
                }

<STRLIT,CHRLIT,QSILIT>[\\][abtnvfre"`'] {
                                          yylval.chr = char_esc(yytext[1]);
                                          return LITCHAR;
                                        }

<STRLIT,CHRLIT>[\\](x{HEX}+|{OCT}+)     {
                                          yylval.chr = num_esc(yytext + 1);
                                          return LITCHAR;
                                        }
<STRLIT>\n              {
                          yyerror("newline in string literal");
                          lineno++;
                          yylval.chr = yytext[0];
                          return LITCHAR;
                        }

<CHRLIT>\n              {
                          yyerror("newline in character literal");
                          lineno++;
                          yylval.chr = yytext[0];
                          return LITCHAR;
                        }

<QSILIT>\n              {
                          yyerror("newline in string quasiliteral");
                          lineno++;
                          yylval.chr = yytext[0];
                          return LITCHAR;
                        }

<QSILIT>@               {
                          yy_push_state(SPECIAL);
                        }

<STRLIT,CHRLIT,QSILIT>. {
                          yylval.chr = yytext[0];
                          return LITCHAR;
                        }

%%
