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

typedef struct yyguts_t scanner_t;

#ifndef YY_TYPEDEF_YY_SCANNER_T
#define YY_TYPEDEF_YY_SCANNER_T
typedef void *yyscan_t;
#endif

typedef struct parser parser_t;

#ifdef SPACE

struct yy_token {
  int yy_char;
  YYSTYPE yy_lval;
};

struct parser {
  val parser;
  cnum lineno;
  int errors;
  val stream;
  val name;
  val prepared_msg;
  val syntax_tree;
  scanner_t *scanner;
  struct yy_token recent_tok;
  struct yy_token tok_pushback[4];
  int tok_idx;
};
#endif

enum prime_parser { prime_lisp, prime_interactive, prime_regex };

extern const int have_yydebug;
extern const wchar_t *spec_file;
extern val form_to_ln_hash;
extern val parser_s;
extern val unique_s;
void yydebug_onoff(int);
void yyerror(scanner_t *scanner, parser_t *, const char *s);
void yyerr(scanner_t *scanner, const char *s);
void yyerrorf(scanner_t *scanner, val s, ...);
void yybadtoken(parser_t *, int tok, val context);
void end_of_regex(scanner_t *scanner);
void end_of_char(scanner_t *scanner);
#ifdef SPACE
int yylex(YYSTYPE *yylval_param, yyscan_t yyscanner);
#endif
int yylex_init(yyscan_t *pscanner);
int yylex_destroy(yyscan_t scanner);
parser_t *yyget_extra(yyscan_t scanner);
void yyset_extra(parser_t *, yyscan_t);
void yyset_hold_char(yyscan_t, int);
void parser_l_init(void);
void open_txr_file(val spec_file, val *txr_lisp_p, val *name, val *stream);
void prime_parser(parser_t *, val name, enum prime_parser);
void prime_parser_post(parser_t *, enum prime_parser);
void prime_scanner(scanner_t *, enum prime_parser);
void scrub_scanner(scanner_t *, int yy_char, wchar_t *lexeme);
int parse_once(val stream, val name, parser_t *parser);
int parse(parser_t *parser, val name, enum prime_parser);
val source_loc(val form);
val source_loc_str(val form, val alt);
val rlset(val form, val info);
void parser_reset(parser_t *);
INLINE val rlcp(val to, val from)
{
  return rlset(to, source_loc(from));
}
val rlcp_tree(val to, val from);
val regex_parse(val string, val error_stream);
val lisp_parse(val source_in, val error_stream, val error_return_val,
               val name_in, val lineno);
val iread(val source_in, val error_stream, val error_return_val,
          val name_in, val lineno);
val read_eval_stream(val stream, val error_stream, val hash_bang_support);
#if HAVE_TERMIOS
val repl(val bindings, val in_stream, val out_stream);
#endif
void parser_common_init(parser_t *);
void parser_cleanup(parser_t *);
val parser(val stream, val lineno);
val get_parser(val stream);
val parser_errors(val parser);
val parser_eof(val parser);
void parse_init(void);
