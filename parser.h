/* Copyright 2009-2015
 * Kaz Kylheku <kaz@kylheku.com>
 * Vancouver, Canada
 * All rights reserved.
 *
 * Redistribution of this software in source and binary forms, with or without
 * modification, is permitted provided that the following two conditions are met.
 *
 * Use of this software in any manner constitutes agreement with the disclaimer
 * which follows the two conditions.
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DAMAGES, HOWEVER CAUSED,
 * AND UNDER ANY THEORY OF LIABILITY, ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

typedef struct yyguts_t scanner_t;

typedef struct {
  cnum lineno;
  int errors;
  val stream;
  val name;
  val prepared_msg;
  val syntax_tree;
  scanner_t *scanner;
} parser_t;

#ifndef YY_TYPEDEF_YY_SCANNER_T
#define YY_TYPEDEF_YY_SCANNER_T
typedef void *yyscan_t;
#endif

extern const wchar_t *spec_file;
extern val form_to_ln_hash;
extern val parser_s;
void yyerror(scanner_t *scanner, parser_t *, const char *s);
void yyerr(scanner_t *scanner, const char *s);
void yyerrorf(scanner_t *scanner, val s, ...);
void yybadtoken(parser_t *, int tok, val context);
void end_of_regex(scanner_t *scanner);
void end_of_char(scanner_t *scanner);
int yylex_init(yyscan_t *pscanner);
int yylex_destroy(yyscan_t scanner);
parser_t *yyget_extra(yyscan_t scanner);
void yyset_extra(parser_t *, yyscan_t);
void parser_l_init(void);
void open_txr_file(val spec_file, val *name, val *stream);
int parse(val stream, val name, parser_t *parser);
val source_loc(val form);
val source_loc_str(val form);
val rlset(val form, val info);
INLINE val rlcp(val to, val from)
{
  return rlset(to, source_loc(from));
}
val rlcp_tree(val to, val from);
val regex_parse(val string, val error_stream);
val lisp_parse(val source, val error_stream, val error_return_val, val name);
val parser(val stream, val lineno);
void parse_init(void);
