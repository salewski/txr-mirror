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
#include <wchar.h>
#include "config.h"
#include "lib.h"
#include "y.tab.h"
#include "gc.h"
#include "stream.h"
#include "utf8.h"
#include "parser.h"

#define YY_INPUT(buf, result, max_size)           \
  do {                                            \
    val c = nil;                                  \
    size_t n;                                     \
    int ch = '*';                                 \
    for (n = 0; n < max_size &&                   \
                (c = get_byte(yyin_stream)) &&    \
                (ch = c_num(c)) != '\n'; ++n)     \
      buf[n] = (char) ch;                         \
    if (ch == '\n')                               \
      buf[n++] = (char) ch;                       \
    result = n;                                   \
  } while (0)

val yyin_stream;

cnum lineno = 1;
int opt_loglevel = 1;   /* 0 - quiet; 1 - normal; 2 - verbose */
int opt_nobindings = 0;
int opt_arraydims = 1;

int errors;

void yyerror(const char *s)
{
  yyerrorf(lit("~a"), string_utf8(s), nao);
}

void yyerrorf(val fmt, ...)
{
  if (opt_loglevel >= 1) {
    va_list vl;
    va_start (vl, fmt);
    format(std_error, lit("~a: (~a:~a): "), prog_string,
           spec_file_str, num(lineno), nao);
    vformat(std_error, fmt, vl);
    put_char(std_error, chr('\n'));
    va_end (vl);
  }
  errors++;
}

void yybadtoken(int tok, val context)
{
  val problem = nil;

  switch (tok) {
  case TEXT:    problem = lit("text"); break;
  case IDENT:   problem = lit("identifier"); break;
  case KEYWORD: problem = lit("keyword"); break;
  case ALL:     problem = lit("\"all\""); break;
  case SOME:    problem = lit("\"some\""); break;
  case NONE:    problem = lit("\"none\""); break;
  case MAYBE:   problem = lit("\"maybe\""); break;
  case CASES:   problem = lit("\"cases\""); break;
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
  }

  if (problem != 0)
    if (context)
      yyerrorf(lit("misplaced ~a in ~a"), problem, context, nao);
    else
      yyerrorf(lit("unexpected ~a"), problem, nao);
  else
    if (context)
      yyerrorf(lit("unterminated ~a"), context, nao);
    else
      yyerrorf(lit("unexpected end of input"), nao);
}

static wchar_t char_esc(int letter)
{
  switch (letter) {
  case 'a': return L'\a';
  case 'b': return L'\b';
  case 't': return L'\t';
  case 'n': return L'\n';
  case 'v': return L'\v';
  case 'f': return L'\f';
  case 'r': return L'\r';
  case 'e': return 27;
  case '"': return L'"';
  case '\'': return L'\'';
  case '`': return L'`';
  }

  abort();
}

static wchar_t num_esc(char *num)
{
  if (num[0] == 'x') {
    if (strlen(num) > 7)
      yyerror("too many digits in hex character escape");
    return strtol(num + 1, 0, 16);
  } else {
    if (strlen(num) > 8)
      yyerror("too many digits in octal character escape");
    return strtol(num, 0, 8);
  }
}

%}

%option stack
%option nounput
%option noinput

TOK     :?[a-zA-Z_][a-zA-Z0-9_]*|[+-]?[0-9]+
ID_END  [^a-zA-Z0-9_]
NUM_END [^0-9]
WS      [\t ]*
HEX     [0-9A-Fa-f]
OCT     [0-7]

ASC     [\x00-\x7f]
ASCN    [\x00-\t\v-\x7f]
U       [\x80-\xbf]
U2      [\xc2-\xdf]
U3      [\xe0-\xef]
U4      [\xf0-\xf4]

UANY    {ASC}|{U2}{U}|{U3}{U}{U}|{U4}{U}{U}{U}
UANYN   {ASCN}|{U2}{U}|{U3}{U}{U}|{U4}{U}{U}{U}
UONLY   {U2}{U}|{U3}{U}{U}|{U4}{U}{U}{U}

%x      SPECIAL NESTED REGEX REGCLASS STRLIT CHRLIT QSILIT

%%

<SPECIAL,NESTED>{TOK}   {
                          cnum val;
                          char *errp;


                          if (yy_top_state() == INITIAL
                              || yy_top_state() == QSILIT)
                            yy_pop_state();

                          if (yytext[0] == ':') {
                            yylval.lexeme = utf8_dup_from(yytext + 1);
                            return KEYWORD;
                          }

                          errno = 0;

                          val = strtol(yytext, &errp, 10);

                          if (*errp != 0) {
                            /* not a number */
                            yylval.lexeme = utf8_dup_from(yytext);
                            return IDENT;
                          }

                          if ((val == LONG_MAX || val == LONG_MIN)
                              && errno == ERANGE)
                            yyerror("numeric overflow in token");

                          if (val < NUM_MIN || val > NUM_MAX)
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
                          yylval.lexeme = chk_strdup(L"@");
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
                          wchar_t lexeme[2];
                          lexeme[0] = char_esc(yytext[1]);
                          lexeme[1] = 0;
                          yylval.lexeme = chk_strdup(lexeme);
                          yy_pop_state();
                          return TEXT;
                        }

<SPECIAL>[\\](x{HEX}+|{OCT}+)   {
                                  wchar_t lexeme[2];
                                  lexeme[0] = num_esc(yytext + 1);
                                  lexeme[1] = 0;
                                  yylval.lexeme = chk_strdup(lexeme);
                                  yy_pop_state();
                                  return TEXT;
                                }

<SPECIAL,NESTED>{UANYN} {
                          yyerrorf(lit("bad character in directive: '~a'"),
                                   string_utf8(yytext), nao);
                        }

<SPECIAL,NESTED>.       {
                          yyerrorf(lit("non-UTF-8 byte in directive: "
                                       "'\\x~02x'"),
                                   num((unsigned char) yytext[0]), nao);
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

<REGEX>[.*?+^~&]        {
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

<REGEX>{UANYN}  {
                  wchar_t buf[8];
                  utf8_from(buf, yytext);
                  yylval.chr = buf[0];
                  return REGCHAR;
                }

<REGEX>.        {
                   yyerrorf(lit("non-UTF-8 byte in regex: '\\x~02x'"),
                            num((unsigned char) yytext[0]), nao);
                }

<INITIAL>({UONLY}|[^@\n])+        {
                                    yylval.lexeme = utf8_dup_from(yytext);
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

<STRLIT,CHRLIT,QSILIT>{UANYN} {
                                wchar_t buf[8];
                                utf8_from(buf, yytext);
                                yylval.chr = buf[0];
                                return LITCHAR;
                              }

<STRLIT,CHRLIT,QSILIT>. {
                           yyerrorf(lit("non-UTF-8 byte in regex: '\\x~02x'"),
                                    num((unsigned char) yytext[0]), nao);
                        }

%%
