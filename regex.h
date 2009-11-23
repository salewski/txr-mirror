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

typedef unsigned int bitcell_t;

#define BITCELL_ALL1 UINT_MAX
#define CHAR_SET_SIZE (256 / (sizeof (bitcell_t) * CHAR_BIT))

typedef enum {
  CHSET_SMALL, CHSET_DISPLACED, CHSET_LARGE, CHSET_XLARGE
} chset_type_t;

typedef bitcell_t cset_L0_t[CHAR_SET_SIZE];
typedef cset_L0_t *cset_L1_t[16];
typedef cset_L1_t *cset_L2_t[16];
typedef cset_L2_t *cset_L3_t[17];

struct any_char_set {
  unsigned type : 3;
  unsigned compl : 1;
};

struct small_char_set {
  unsigned type : 3;
  unsigned compl : 1;
  cset_L0_t bitcell;
};

struct displaced_char_set {
  unsigned type : 3;
  unsigned compl : 1;
  cset_L0_t bitcell;
  wchar_t base;
};


struct large_char_set {
  unsigned type : 3;
  unsigned compl : 1;
  cset_L2_t dir;
};

struct xlarge_char_set {
  unsigned type : 3;
  unsigned compl : 1;
  cset_L3_t dir;
};

typedef union char_set {
  struct any_char_set any;
  struct small_char_set s;
  struct displaced_char_set d;
  struct large_char_set l;
  struct xlarge_char_set xl;
} char_set_t;

char_set_t *char_set_create(chset_type_t, wchar_t);
void char_set_destroy(char_set_t *);

void char_set_compl(char_set_t *);
void char_set_add(char_set_t *, wchar_t);
void char_set_add_range(char_set_t *, wchar_t, wchar_t); /* inclusive */
int char_set_contains(char_set_t *, wchar_t);

typedef enum {
  nfa_accept, nfa_empty, nfa_wild, nfa_single, nfa_set
} nfa_kind_t;

typedef union nfa_state nfa_state_t;

struct nfa_state_accept {
  nfa_kind_t kind;
  unsigned visited;
};

struct nfa_state_empty {
  nfa_kind_t kind;
  unsigned visited;
  nfa_state_t *trans0;
  nfa_state_t *trans1;
};

struct nfa_state_single {
  nfa_kind_t kind;
  unsigned visited;
  nfa_state_t *trans;
  wchar_t ch;
};

struct nfa_state_set {
  nfa_kind_t kind;
  unsigned visited;
  nfa_state_t *trans;
  char_set_t *set;
};

union nfa_state {
  struct nfa_state_accept a;
  struct nfa_state_empty e;
  struct nfa_state_single o;
  struct nfa_state_set s;
};

nfa_state_t *nfa_state_accept(void);
nfa_state_t *nfa_state_empty(nfa_state_t *, nfa_state_t *);
nfa_state_t *nfa_state_single(nfa_state_t *, wchar_t ch);
nfa_state_t *nfa_state_wild(nfa_state_t *);
nfa_state_t *nfa_state_set(nfa_state_t *, char_set_t *);
void nfa_state_free(nfa_state_t *st);
void nfa_state_shallow_free(nfa_state_t *st);
void nfa_state_merge(nfa_state_t *accept, nfa_state_t *);

typedef struct nfa {
  nfa_state_t *start;
  nfa_state_t *accept;
} nfa_t;

typedef enum nfam_result {
  NFAM_INCOMPLETE, NFAM_FAIL, NFAM_MATCH
} nfam_result_t;

typedef struct nfa_machine {
  cnum last_accept_pos;
  unsigned visited;
  nfa_state_t **move, **clos, **stack;
  int nmove, nclos;
  cnum count;
  nfa_t nfa;
} nfa_machine_t;

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
