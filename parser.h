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

extern cnum lineno;
extern int errors;
extern val yyin_stream;
extern const wchar_t *spec_file;
extern val spec_file_str;
extern val form_to_ln_hash;
int yyparse(void);
val get_spec(void);
void yyerror(const char *s);
void yyerrorf(val s, ...);
void yybadtoken(int tok, val context);
void end_of_regex(void);
void end_of_char(void);
int yylex(void);
int yylex_destroy(void);
void parse_init(void);
void parse_reset(val spec_file);
val source_loc(val form);
val source_loc_str(val form);
val rl(val form, val lineno);
val rlset(val form, val info);
INLINE val rlcp(val to, val from)
{
  return rlset(to, source_loc(from));
}
val regex_parse(val string, val error_stream);
