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
#include <string.h>
#include <stdarg.h>
#include <stdlib.h>
#include <limits.h>
#include <errno.h>
#include <dirent.h>
#include <wchar.h>
#include <setjmp.h>
#include "config.h"
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#include "lib.h"
#include "y.tab.h"
#include "gc.h"
#include "stream.h"
#include "utf8.h"
#include "unwind.h"
#include "hash.h"
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

int errors;
val spec_file_str;

val form_to_ln_hash;

static val prepared_error_message;

void yyerror(const char *s)
{
  yyerrorf(lit("~a"), string_utf8(s), nao);
  if (prepared_error_message) {
    yyerrorf(lit("~a"), prepared_error_message, nao);
    prepared_error_message = nil;
  }
}

void yyerrorf(val fmt, ...)
{
  if (opt_loglevel >= 1) {
    va_list vl;
    va_start (vl, fmt);
    format(std_error, lit("~a: (~a:~a): "), prog_string,
           spec_file_str, num(lineno), nao);
    vformat(std_error, fmt, vl);
    put_char(chr('\n'), std_error);
    va_end (vl);
  }
  errors++;
}

static void yyerrprepf(val fmt, ...)
{
  if (opt_loglevel >= 1) {
    va_list vl;
    va_start (vl, fmt);
    prepared_error_message = vformat_to_string(fmt, vl);
    va_end (vl);
  }
}

static wchar_t char_esc(int letter)
{
  switch (letter) {
  case ' ': return L' ';
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
  case '/': return L'/';
  case '\\': return L'\\';
  }

  internal_error("unhandled escape character");
}

static wchar_t num_esc(char *num)
{
  if (num[0] == 'x') {
    if (strlen(num) > 7)
      yyerror("too many digits in hex character escape");
    return strtol(num + 1, 0, 16);
  } else {
    if (num[0] == 'o')
      num++;
    if (strlen(num) > 8)
      yyerror("too many digits in octal character escape");
    return strtol(num, 0, 8);
  }
}

%}

%option stack
%option nounput
%option noinput

SYM     [a-zA-Z0-9_]+
SGN     [+\-]
EXP     [eE][+\-]?[0-9]+
DIG     [0-9]
XDIG    [0-9A-Fa-f]
NUM     {SGN}?{DIG}+
FLO     {SGN}?({DIG}*[.]{DIG}+{EXP}?|{DIG}+[.]?{EXP})
FLODOT  {SGN}?{DIG}+[.]
XNUM    #x{SGN}?{XDIG}+
BSCHR   [a-zA-Z0-9!$%&*+\-<=>?\\^_~]
BSYM    {BSCHR}({BSCHR}|#)*
NSCHR   [a-zA-Z0-9!$%&*+\-<=>?\\^_~/]
ID_END  [^a-zA-Z0-9!$%&*+\-<=>?\\^_~/]
NSYM    {NSCHR}({NSCHR}|#)*
TOK     :?{SYM}
BTOK    [:@]?{BSYM}
NTOK    [:@]?{NSYM}
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

%x      SPECIAL BRACED NESTED REGEX STRLIT CHRLIT QSILIT

%%

<SPECIAL,NESTED,BRACED>{NUM} {
  val str = string_own(utf8_dup_from(yytext));

  if (yy_top_state() == INITIAL
      || yy_top_state() == QSILIT)
    yy_pop_state();

  yylval.val = int_str(str, num(10));
  return NUMBER;
}

<SPECIAL,NESTED,BRACED>{XNUM} {
  val str = string_own(utf8_dup_from(yytext + 2));

  if (yy_top_state() == INITIAL
      || yy_top_state() == QSILIT)
    yy_pop_state();

  yylval.val = int_str(str, num(16));
  return NUMBER;
}


<SPECIAL,NESTED,BRACED>{FLO} {
  val str = string_own(utf8_dup_from(yytext));

  if (yy_top_state() == INITIAL
      || yy_top_state() == QSILIT)
    yy_pop_state();

  yylval.val = flo_str(str);
  return NUMBER;
}

<SPECIAL,NESTED,BRACED>{FLODOT}/[^.] {
  val str = string_own(utf8_dup_from(yytext));

  if (yy_top_state() == INITIAL
      || yy_top_state() == QSILIT)
    yy_pop_state();

  yylval.val = flo_str(str);
  return NUMBER;
}

<NESTED,QSILIT>@{NUM} {
  val str = string_own(utf8_dup_from(yytext + 1));

  if (yy_top_state() == INITIAL
      || yy_top_state() == QSILIT)
    yy_pop_state();
  yylval.val = int_str(str, num(10));
  return METANUM;
}

<NESTED,QSILIT>@{XNUM} {
  val str = string_own(utf8_dup_from(yytext + 3));

  if (yy_top_state() == INITIAL
      || yy_top_state() == QSILIT)
    yy_pop_state();
  yylval.val = int_str(str, num(16));
  return METANUM;
}

<SPECIAL>{TOK} |
<BRACED>{BTOK} |
<NESTED>{NTOK} {
  if (yy_top_state() == INITIAL
      || yy_top_state() == QSILIT)
    yy_pop_state();

  switch (yytext[0]) {
  case ':':
    yylval.lexeme = utf8_dup_from(yytext + 1);
    return KEYWORD;
  case '@':
    yylval.lexeme = utf8_dup_from(yytext + 1);
    return METAVAR;
  default:
    yylval.lexeme = utf8_dup_from(yytext);
    return IDENT;
  }
}

<BRACED,NESTED>: {
  if (yy_top_state() == INITIAL
      || yy_top_state() == QSILIT)
    yy_pop_state();
  yylval.lexeme = utf8_dup_from("");
  return KEYWORD;
}

<SPECIAL>\({WS}all{WS}\) {
  yy_pop_state();
  yylval.lineno = lineno;
  return ALL;
}

<SPECIAL>\({WS}some/{ID_END} {
  yy_push_state(NESTED);
  yylval.lineno = lineno;
  return SOME;
}

<SPECIAL>\({WS}none{WS}\) {
  yy_pop_state();
  yylval.lineno = lineno;
  return NONE;
}

<SPECIAL>\({WS}maybe{WS}\) {
  yy_pop_state();
  yylval.lineno = lineno;
  return MAYBE;
}

<SPECIAL>\({WS}cases{WS}\) {
  yy_pop_state();
  yylval.lineno = lineno;
  return CASES;
}

<SPECIAL>\({WS}block/{ID_END} {
  yy_push_state(NESTED);
  yylval.lineno = lineno;
  return BLOCK;
}

<SPECIAL>\({WS}choose/{ID_END} {
  yy_push_state(NESTED);
  yylval.lineno = lineno;
  return CHOOSE;
}

<SPECIAL>\({WS}gather/{ID_END} {
  yy_push_state(NESTED);
  yylval.lineno = lineno;
  return GATHER;
}

<SPECIAL>\({WS}and{WS}\) {
  yy_pop_state();
  yylval.lineno = lineno;
  return AND;
}

<SPECIAL>\({WS}or{WS}\) {
  yy_pop_state();
  yylval.lineno = lineno;
  return OR;
}

<SPECIAL>\({WS}end{WS}\) {
  yy_pop_state();
  yylval.lineno = lineno;
  return END;
}

<SPECIAL>\({WS}collect/{ID_END} {
  yy_push_state(NESTED);
  yylval.lineno = lineno;
  return COLLECT;
}

<SPECIAL>\({WS}coll/{ID_END} {
  yy_push_state(NESTED);
  yylval.lineno = lineno;
  return COLL;
}

<SPECIAL>\({WS}until{WS}\) {
  yy_pop_state();
  yylval.lineno = lineno;
  return UNTIL;
}

<SPECIAL>\({WS}output/{ID_END}  {
  yy_push_state(NESTED);
  yylval.lineno = lineno;
  return OUTPUT;
}

<SPECIAL>\({WS}repeat/{ID_END}  {
  yy_push_state(NESTED);
  yylval.lineno = lineno;
  return REPEAT;
}


<SPECIAL>\({WS}rep/{ID_END} {
  yy_push_state(NESTED);
  yylval.lineno = lineno;
  return REP;
}

<SPECIAL>\({WS}single{WS}\) {
  yy_pop_state();
  yylval.lineno = lineno;
  return SINGLE;
}

<SPECIAL>\({WS}first{WS}\) {
  yy_pop_state();
  yylval.lineno = lineno;
  return FIRST;
}

<SPECIAL>\({WS}last{WS}\) {
  yy_pop_state();
  yylval.lineno = lineno;
  return LAST;
}

<SPECIAL>\({WS}empty{WS}\) {
  yy_pop_state();
  yylval.lineno = lineno;
  return EMPTY;
}

<SPECIAL>\({WS}mod/{ID_END} {
  yy_push_state(NESTED);
  yylval.lineno = lineno;
  return MOD;
}

<SPECIAL>\({WS}modlast/{ID_END} {
  yy_push_state(NESTED);
  yylval.lineno = lineno;
  return MODLAST;
}

<SPECIAL>\({WS}define/{ID_END} {
  yy_push_state(NESTED);
  yylval.lineno = lineno;
  return DEFINE;
}

<SPECIAL>\({WS}try{WS}\) {
  yy_pop_state();
  yylval.lineno = lineno;
  return TRY;
}

<SPECIAL>\({WS}catch/{ID_END} {
  yy_push_state(NESTED);
  yylval.lineno = lineno;
  return CATCH;
}

<SPECIAL>\({WS}finally{WS}\) {
  yy_pop_state();
  yylval.lineno = lineno;
  return FINALLY;
}

<SPECIAL>[{] {
  yy_push_state(BRACED);
  yylval.lineno = lineno;
  return yytext[0];
}

<SPECIAL>[(\[] |
<NESTED,BRACED>@?[(\[] {
 yy_push_state(NESTED);
 if (yytext[0] == '@') {
   yylval.chr = yytext[1];
   return yytext[1] == '(' ? METAPAR : METABKT;
 }
 yylval.lineno = lineno;
 return yytext[0];
}

<NESTED>,[*] {
  yylval.chr = '*';
  return SPLICE;
}

<NESTED>[,'] {
  yylval.chr = yytext[0];
  return yytext[0];
}

<BRACED>[}] {
  yy_pop_state();
  if (yy_top_state() == INITIAL
      || yy_top_state() == QSILIT)
    yy_pop_state();
  return yytext[0];
}

<SPECIAL,NESTED>[)\]] {
  yy_pop_state();
  if (yy_top_state() == INITIAL
      || yy_top_state() == QSILIT)
    yy_pop_state();
  return yytext[0];
}

<SPECIAL,NESTED,BRACED>{WS} {
  /* Eat whitespace in directive */
}

<SPECIAL,NESTED,BRACED>\" {
  yy_push_state(STRLIT);
  return '"';
}

<SPECIAL,NESTED,BRACED>#\\ {
  yy_push_state(CHRLIT);
  return HASH_BACKSLASH;
}

<SPECIAL,NESTED,BRACED>#[/] {
  yy_push_state(REGEX);
  return HASH_SLASH;
}

<SPECIAL,NESTED,BRACED>` {
  yy_push_state(QSILIT);
  return '`';
}

<NESTED,BRACED># {
  return '#';
}

<NESTED,BRACED>#H {
  yylval.lineno = lineno;
  return HASH_H;
}

<NESTED>\.\. {
  yylval.lineno = lineno;
  return DOTDOT;
}

<SPECIAL>@ {
  yy_pop_state();
  yylval.lexeme = chk_strdup(L"@");
  return TEXT;
}

<SPECIAL,NESTED,BRACED>\n {
  lineno++;
}

<SPECIAL,BRACED>[/] {
  yy_push_state(REGEX);
  return '/';
}

<SPECIAL,NESTED>\. {
  yylval.chr = '.';
  return '.';
}

<SPECIAL,NESTED,BRACED>[\\]\n{WS} {
  yy_pop_state();
  lineno++;
}

<SPECIAL>[\\][abtnvfre ] {
  wchar_t lexeme[2];
  lexeme[0] = char_esc(yytext[1]);
  lexeme[1] = 0;
  yylval.lexeme = chk_strdup(lexeme);
  yy_pop_state();
  return TEXT;
}

<SPECIAL>[\\](x{HEX}+|{OCT}+) {
  wchar_t lexeme[2];
  lexeme[0] = num_esc(yytext + 1);
  lexeme[1] = 0;
  yylval.lexeme = chk_strdup(lexeme);
  yy_pop_state();
  return TEXT;
}

<SPECIAL>[\\]. {
  yyerrorf(lit("unrecognized escape: \\~a"), chr(yytext[1]), nao);
}

<SPECIAL,NESTED,BRACED>[;].* {
  /* comment */
}

<SPECIAL,NESTED,BRACED>{UANYN} {
  yyerrprepf(lit("bad character in directive: '~a'"),
             string_utf8(yytext), nao);
  return ERRTOK;
}

<SPECIAL,NESTED,BRACED>. {
  yyerrprepf(lit("non-UTF-8 byte in directive: "
                 "'\\x~02x'"),
             num((unsigned char) yytext[0]), nao);
  return ERRTOK;
}

<REGEX>[/] {
  yylval.chr = '/';
  return '/';
}


<REGEX>[\\][abtnvfre\\ ] {
  yylval.chr = char_esc(yytext[1]);
  return REGCHAR;
}

<REGEX>[\\](x{HEX}+|{OCT}+);? {
  yylval.chr = num_esc(yytext + 1);
  return REGCHAR;
}

<REGEX>[\\][sSdDwW] {
  yylval.chr = yytext[1];
  return REGTOKEN;
}

<REGEX>{WS}[\\]\n{WS} {
  lineno++;
}

<REGEX>\n {
  lineno++;
  yyerrprepf(lit("newline in regex"), nao);
  return ERRTOK;
}

<REGEX>[.*?+~&%] {
  yylval.chr = yytext[0];
  return yytext[0];
}

<REGEX>[\[\]\-] {
  yylval.chr = yytext[0];
  return yytext[0];
}

<REGEX>[()|] {
  yylval.chr = yytext[0];
  return yytext[0];
}

<REGEX>[\\]. {
  yylval.chr = yytext[1];
  return REGCHAR;
}

<REGEX>{UANYN}  {
  wchar_t buf[8];
  utf8_from(buf, yytext);
  yylval.chr = buf[0];
  return REGCHAR;
}

<REGEX>. {
  yyerrprepf(lit("non-UTF-8 byte in regex: '\\x~02x'"),
             num((unsigned char) yytext[0]), nao);
  return ERRTOK;
}

<INITIAL>[ ]+ {
  yylval.lexeme = utf8_dup_from(yytext);
  return SPACE;
}

<INITIAL>({UONLY}|[^@\n ])+ {
  yylval.lexeme = utf8_dup_from(yytext);
  return TEXT;
}

<INITIAL>\n {
  lineno++;
  return '\n';
}

<INITIAL>@{WS}\* {
  yy_push_state(SPECIAL);
  return '*';
}

<INITIAL>@ {
  yy_push_state(SPECIAL);
}

<INITIAL>@\x01R {
  yy_push_state(REGEX);
  return SECRET_ESCAPE_R;
}

<INITIAL>^@[#;].*\n {
  /* eat whole line comment */
  lineno++;
}

<INITIAL>@[#;].* {
  /* comment to end of line */
}

<STRLIT>\" {
  yy_pop_state();
  return yytext[0];
}

<QSILIT>` {
  yy_pop_state();
  return yytext[0];
}

<STRLIT,QSILIT>[\\][abtnvfre "`'\\ ] {
  yylval.chr = char_esc(yytext[1]);
  return LITCHAR;
}

<STRLIT,QSILIT>{WS}[\\]\n{WS} {
  lineno++;
}
                                
<STRLIT,QSILIT>[\\](x{HEX}+|{OCT}+);?  {
  yylval.chr = num_esc(yytext+1);
  return LITCHAR;
}

<STRLIT,QSILIT>[\\]. {
  yyerrorf(lit("unrecognized escape: \\~a"), chr(yytext[1]), nao);
}

<CHRLIT>(x{HEX}+|o{OCT}+) {
  yylval.chr = num_esc(yytext);
  return LITCHAR;
}

<CHRLIT>{SYM} {
  yylval.lexeme = utf8_dup_from(yytext);
  return IDENT;
}

<CHRLIT>[^ \t\n] {
  yylval.lexeme = utf8_dup_from(yytext);
  return IDENT; /* hack */
}

<STRLIT>\n {
  yyerrprepf(lit("newline in string literal"), nao);
  lineno++;
  yylval.chr = yytext[0];
  return ERRTOK;
}

<CHRLIT>\n {
  yyerrprepf(lit("newline in character literal"), nao);
  lineno++;
  yylval.chr = yytext[0];
  return ERRTOK;
}

<QSILIT>\n {
  yyerrprepf(lit("newline in string quasiliteral"), nao);
  lineno++;
  yylval.chr = yytext[0];
  return ERRTOK;
}

<QSILIT>@ {
  yy_push_state(SPECIAL);
}

<STRLIT,CHRLIT,QSILIT>{UANYN} {
  wchar_t buf[8];
  utf8_from(buf, yytext);
  yylval.chr = buf[0];
  return LITCHAR;
}

<STRLIT,CHRLIT,QSILIT>. {
  yyerrprepf(lit("non-UTF-8 byte in literal: '\\x~02x'"),
             num((unsigned char) yytext[0]), nao);
  return ERRTOK;
}

%%

void end_of_regex(void)
{
  if (YYSTATE != REGEX)
    internal_error("end_of_regex called in wrong scanner state");

  yy_pop_state();

  if (YYSTATE != INITIAL) {
    if (yy_top_state() == INITIAL || yy_top_state() == QSILIT)
      yy_pop_state();
  }
}

void end_of_char(void)
{
  if (YYSTATE != CHRLIT)
    internal_error("end_of_char called in wrong scanner state");

  yy_pop_state();
}

val source_loc(val form)
{
  return gethash(form_to_ln_hash, form);
}

val source_loc_str(val form)
{
  cons_bind (line, file, gethash(form_to_ln_hash, form));
  return if3(line,
             format(nil, lit("~a:~a"), file, line, nao),
             lit("source location n/a"));
}

void parse_init(void)
{
  protect(&yyin_stream, &prepared_error_message,
          &form_to_ln_hash, (val *) 0);

  form_to_ln_hash = make_hash(t, nil, t);
}

void parse_reset(val spec_file)
{
  errors = 0;
  lineno = 1;
  spec_file_str = spec_file;
  {
    FILE *in = w_fopen(c_str(spec_file_str), L"r");
    if (in == 0) {
      spec_file_str = cat_str(list(spec_file_str, lit("txr"), nao), lit("."));
      in = w_fopen(c_str(spec_file_str), L"r");
      if (in == 0)
        uw_throwf(file_error_s, lit("unable to open ~a"), spec_file, nao);
    }
    yyin_stream = make_stdio_stream(in, spec_file_str);
  }
}

val regex_parse(val string, val error_stream)
{
  uses_or2;
  val parse_string = cat_str(list(lit("@\x01R"), string, nao), nil);
  yyin_stream = make_string_byte_input_stream(parse_string);
  errors = 0;
  lineno = 1;
  val save_stream = std_error;
  std_error = if3(error_stream == t, std_output, or2(error_stream, std_null));
  {
    int gc = gc_state(0);
    spec_file_str = if3(std_error != std_null,
                        format(nil, lit("regex: ~s"), string, nao),
                        lit(""));
    yyparse();
    yylex_destroy();
    gc_state(gc);
  }
  std_error = save_stream;
  return errors ? nil : get_spec();
}
