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
#define BITCELL_LIT(NUMTOKEN) NUMTOKEN ## U

#define CHAR_SET_SIZE ((UCHAR_MAX + 1) / (sizeof (bitcell_t) * CHAR_BIT))

typedef struct char_set {
  bitcell_t bitcell[CHAR_SET_SIZE];
} char_set_t;

void char_set_clear(char_set_t *);
void char_set_compl(char_set_t *);
void char_set_add(char_set_t *, int);
void char_set_add_range(char_set_t *, int, int); /* inclusive */
int char_set_contains(char_set_t *, int);

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
  int ch;
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
nfa_state_t *nfa_state_single(nfa_state_t *, int ch);
nfa_state_t *nfa_state_wild(nfa_state_t *);
nfa_state_t *nfa_state_set(nfa_state_t *);
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
  long last_accept_pos;
  unsigned visited;
  nfa_state_t **move, **clos, **stack;
  int nmove, nclos;
  long count;
  nfa_t nfa;
} nfa_machine_t;

nfa_t nfa_compile_regex(obj_t *regex);
void nfa_free(nfa_t);
long nfa_run(nfa_t nfa, const char *str);
void nfa_machine_reset(nfa_machine_t *);
void nfa_machine_init(nfa_machine_t *, nfa_t);
void nfa_machine_cleanup(nfa_machine_t *);
nfam_result_t nfa_machine_feed(nfa_machine_t *, int ch);
long nfa_machine_match_span(nfa_machine_t *);
obj_t *regex_compile(obj_t *regex_sexp);
obj_t *regexp(obj_t *);
nfa_t *regex_nfa(obj_t *);
obj_t *search_regex(obj_t *haystack, obj_t *needle_regex, obj_t *start_num,
                    obj_t *from_end);
obj_t *match_regex(obj_t *str, obj_t *regex, obj_t *pos);
