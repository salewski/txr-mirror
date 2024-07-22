/* Copyright 2009-2024
 * Kaz Kylheku <kaz@kylheku.com>
 * Vancouver, Canada
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
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
  int yy_lex_state;
};

struct circ_stack {
  struct circ_stack *up;
  val obj;
};

struct parser {
  val parser;
  cnum lineno;
  int errors;
  unsigned char eof;
  unsigned char ignore;
  val stream;
  val name;
  val prepared_msg;
  val syntax_tree;
  int quasi_level;
  val circ_ref_hash;
  cnum circ_count;
  scanner_t *scanner;
  struct yy_token recent_tok;
  struct yy_token tok_pushback[4];
  int tok_idx;
  int rec_source_loc;
  int read_unknown_structs;
  int read_bad_json;
  int read_json_int;
};
#endif

enum prime_parser { prime_lisp, prime_interactive, prime_regex, prime_json };

extern const int have_yydebug;
extern const wchar_t *spec_file;
extern val form_to_ln_hash;
extern val parser_s, unique_s, circref_s;
extern val rec_source_loc_s, read_unknown_structs_s, read_bad_json_s;
extern val json_s;

extern struct cobj_class *parser_cls;

void yydebug_onoff(int);
void yyerror(scanner_t *scanner, parser_t *, const char *s);
void yyerr(scanner_t *scanner, const char *s);
void yyerrorf(scanner_t *scanner, val s, ...);
void yybadtoken(parser_t *, int tok, val context);
void end_of_regex(scanner_t *scanner);
void end_of_char(scanner_t *scanner);
void end_of_buflit(scanner_t *scanner);
void end_of_json(scanner_t *scanner);
void end_of_json_unquote(scanner_t *scanner);
#ifdef SPACE
int yylex(YYSTYPE *yylval_param, yyscan_t yyscanner);
#endif
int yylex_init(yyscan_t *pscanner);
int yylex_destroy(yyscan_t scanner);
parser_t *yyget_extra(yyscan_t scanner);
void yyset_extra(parser_t *, yyscan_t);
void yyset_hold_char(yyscan_t, int);
void parser_l_init(void);
void open_txr_file(val first_try_path, val *txr_lisp_p,
                   val *orig_in_resolved_out, val *stream,
                   val search_dirs, val self);
void prime_parser(parser_t *, val name, enum prime_parser);
void prime_parser_post(parser_t *, enum prime_parser);
#ifdef SPACE
int parser_callgraph_circ_check(struct circ_stack *rs, val obj);
#endif
void prime_scanner(scanner_t *, enum prime_parser);
void parser_resolve_circ(parser_t *);
void parser_circ_def(parser_t *, val num, val expr);
val parser_circ_ref(parser_t *, val num);
void scrub_scanner(scanner_t *, int yy_char, wchar_t *lexeme);
int parse_once(val self, val stream, val name);
int parse(parser_t *parser, val name, enum prime_parser);
val source_loc(val form);
val source_loc_str(val form, val alt);
val expand_meta(val form, val menv);
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
val nread(val source_in, val error_stream, val error_return_val,
          val name_in, val lineno);
val iread(val source_in, val error_stream, val error_return_val,
          val name_in, val lineno);
val get_json(val source_in, val error_stream, val error_return_val,
             val name_in, val lineno);
val read_eval_stream(val self, val stream, val error_stream);
val read_compiled_file(val self, val stream, val error_stream);
val read_objects_from_string(val string, val error_stream,
                             val error_return_val, val name_in);
val read_objects(val source_in, val error_stream, val error_return_val,
                 val name_in, val lineno_in);
val txr_parse(val source, val error_stream,
              val error_return_val, val name_in);
val repl(val bindings, val in_stream, val out_stream, val env);
void parser_common_init(parser_t *);
void parser_cleanup(parser_t *);
val parser(val stream, val name, val lineno);
parser_t *parser_get_impl(val self, val parser);
val ensure_parser(val stream, val name);
val parser_set_lineno(val self, val stream, val lineno);
val parser_errors(val parser);
val parse_errors(val stream);
void parse_init(void);
