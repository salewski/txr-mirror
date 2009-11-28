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

#include <limits.h>

typedef union nfa_state nfa_state_t;

typedef struct nfa {
  nfa_state_t *start;
  nfa_state_t *accept;
} nfa_t;

typedef enum nfam_result {
  NFAM_INCOMPLETE, NFAM_FAIL, NFAM_MATCH
} nfam_result_t;

typedef struct nfa_machine nfa_machine_t;

nfa_t nfa_compile_regex(val regex);
void nfa_free(nfa_t);
cnum nfa_run(nfa_t nfa, const wchar_t *str);
void nfa_machine_reset(nfa_machine_t *);
void nfa_machine_init(nfa_machine_t *, nfa_t);
void nfa_machine_cleanup(nfa_machine_t *);
nfam_result_t nfa_machine_feed(nfa_machine_t *, wchar_t ch);
cnum nfa_machine_match_span(nfa_machine_t *);
val regex_compile(val regex_sexp);
val regexp(val);
nfa_t *regex_nfa(val);
val search_regex(val haystack, val needle_regex, val start_num, val from_end);
val match_regex(val str, val regex, val pos);
