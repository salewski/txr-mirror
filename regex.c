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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <wchar.h>
#include <assert.h>
#include <dirent.h>
#include <setjmp.h>
#include <dirent.h>
#include "config.h"
#include "lib.h"
#include "unwind.h"
#include "regex.h"

typedef unsigned int bitcell_t;

#define BITCELL_ALL1 UINT_MAX
#define CHAR_SET_SIZE (256 / (sizeof (bitcell_t) * CHAR_BIT))

#define CHAR_SET_INDEX(CH) ((CH) / (sizeof (bitcell_t) * CHAR_BIT))
#define CHAR_SET_BIT(CH) ((CH) % (sizeof (bitcell_t) * CHAR_BIT))

#define CHAR_SET_L0(CH) ((CH) & 0xFF)
#define CHAR_SET_L1(CH) (((CH) >> 8) & 0xF)
#define CHAR_SET_L2(CH) (((CH) >> 12) & 0xF)
#define CHAR_SET_L3(CH) (((CH) >> 16) & 0x1F)

#define CHAR_SET_L2_LO(CH) ((CH) & (~(wchar_t) 0xFFFF))
#define CHAR_SET_L2_HI(CH) ((CH) | ((wchar_t) 0xFFFF))

#define CHAR_SET_L1_LO(CH) ((CH) & (~(wchar_t) 0xFFF))
#define CHAR_SET_L1_HI(CH) ((CH) | ((wchar_t) 0xFFF))

#define CHAR_SET_L0_LO(CH) ((CH) & (~(wchar_t) 0xFF))
#define CHAR_SET_L0_HI(CH) ((CH) | ((wchar_t) 0xFF))

typedef enum {
  CHSET_SMALL, CHSET_DISPLACED, CHSET_LARGE, CHSET_XLARGE
} chset_type_t;

typedef bitcell_t cset_L0_t[CHAR_SET_SIZE];
typedef cset_L0_t *cset_L1_t[16];
typedef cset_L1_t *cset_L2_t[16];
typedef cset_L2_t *cset_L3_t[17];

struct any_char_set {
  unsigned type : 3;
  unsigned comp : 1;
};

struct small_char_set {
  unsigned type : 3;
  unsigned comp : 1;
  cset_L0_t bitcell;
};

struct displaced_char_set {
  unsigned type : 3;
  unsigned comp : 1;
  cset_L0_t bitcell;
  wchar_t base;
};


struct large_char_set {
  unsigned type : 3;
  unsigned comp : 1;
  cset_L2_t dir;
};

struct xlarge_char_set {
  unsigned type : 3;
  unsigned comp : 1;
  cset_L3_t dir;
};

typedef union char_set {
  struct any_char_set any;
  struct small_char_set s;
  struct displaced_char_set d;
  struct large_char_set l;
  struct xlarge_char_set xl;
} char_set_t;

#define NFA_SET_SIZE 512

typedef enum {
  nfa_accept, nfa_empty, nfa_wild, nfa_single, nfa_set
} nfa_kind_t;

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

struct nfa_machine {
  cnum last_accept_pos;
  unsigned visited;
  nfa_state_t **move, **clos, **stack;
  int nmove, nclos;
  cnum count;
  nfa_t nfa;
};

static int L0_full(cset_L0_t *L0)
{
  int i;

  for (i = 0; i < (int) CHAR_SET_SIZE; i++)
    if ((*L0)[i] != ((bitcell_t) -1))
      return 0;
  return 1;
}

static void L0_fill_range(cset_L0_t *L0, wchar_t ch0, wchar_t ch1)
{
  int i;
  int bt0 = CHAR_SET_BIT(ch0);
  int bc0 = CHAR_SET_INDEX(ch0);
  bitcell_t mask0 = ~(((bitcell_t) 1 << bt0) - 1);
  int bt1 = CHAR_SET_BIT(ch1);
  int bc1 = CHAR_SET_INDEX(ch1);
  bitcell_t mask1 = (((bitcell_t) 1 << (bt1 + 1) % 32) - 1);

  if (bc1 == bc0) {
    (*L0)[bc0] |= (mask0 & mask1);
  } else {
    (*L0)[bc0] |= mask0;
    (*L0)[bc1] |= mask1;
    for (i = bc0 + 1; i < bc1; i++)
      (*L0)[i] = ((bitcell_t) -1);
  }
}

static int L0_contains(cset_L0_t *L0, wchar_t ch)
{
  return ((*L0)[CHAR_SET_INDEX(ch)] & (1 << CHAR_SET_BIT(ch))) != 0;
}

static int L1_full(cset_L1_t *L1)
{
  int i;
  for (i = 0; i < 16; i++)
    if ((*L1)[i] != (cset_L0_t *) -1)
      return 0;
  return 1;
}

static void L1_fill_range(cset_L1_t *L1, wchar_t ch0, wchar_t ch1)
{
  int i1, i10, i11;

  i10 = CHAR_SET_L1(ch0);
  i11 = CHAR_SET_L1(ch1);

  for (i1 = i10; i1 <= i11; i1++) {
    wchar_t c0 = 0, c1 = 0;
    cset_L0_t *L0;

    if (i1 > i10 && i1 < i11) {
      free((*L1)[i1]);
      (*L1)[i1] = (cset_L0_t *) -1;
      continue;
    } else if (i10 == i11) {
      c0 = ch0;
      c1 = ch1;
    } else if (i1 == i10) {
      c0 = ch0;
      c1 = CHAR_SET_L0_HI(ch0);
    } else if (i1 == i11) {
      c0 = CHAR_SET_L0_LO(ch1);
      c1 = ch1;
    }

    if ((L0 = (*L1)[i1]) == (cset_L0_t *) -1)
      continue;

    if (L0 == 0) {
      static cset_L0_t blank;
      L0 = (*L1)[i1] = (cset_L0_t *) chk_malloc(sizeof *L0);
      memcpy(L0, &blank, sizeof *L0);
    }

    L0_fill_range(L0, CHAR_SET_L0(c0), CHAR_SET_L0(c1));

    if (L0_full(L0)) {
      free(L0);
      (*L1)[i1] = (cset_L0_t *) -1;
    }
  }
}

static int L1_contains(cset_L1_t *L1, wchar_t ch)
{
  int i1 = CHAR_SET_L1(ch);
  cset_L0_t *L0 = (*L1)[i1];

  if (L0 == 0)
    return 0;
  else if (L0 == (cset_L0_t *) -1)
    return 1;
  else
    return L0_contains(L0, CHAR_SET_L0(ch));
}


static void L1_free(cset_L1_t *L1)
{
  int i1;

  if (L1 == (cset_L1_t *) -1)
    return;

  for (i1 = 0; i1 < 16; i1++)
    if ((*L1)[i1] != (cset_L0_t *) -1)
      free((*L1)[i1]);
}

static int L2_full(cset_L2_t *L2)
{
  int i;
  for (i = 0; i < 16; i++)
    if ((*L2)[i] != (cset_L1_t *) -1)
      return 0;
  return 1;
}

static void L2_fill_range(cset_L2_t *L2, wchar_t ch0, wchar_t ch1)
{
  int i2, i20, i21;

  i20 = CHAR_SET_L2(ch0);
  i21 = CHAR_SET_L2(ch1);

  for (i2 = i20; i2 <= i21; i2++) {
    wchar_t c0 = 0, c1 = 0;
    cset_L1_t *L1;

    if (i2 > i20 && i2 < i21) {
      free((*L2)[i2]);
      (*L2)[i2] = (cset_L1_t *) -1;
      continue;
    } else if (i20 == i21) {
      c0 = ch0;
      c1 = ch1;
    } else if (i2 == i20) {
      c0 = ch0;
      c1 = CHAR_SET_L1_HI(ch0);
    } else if (i2 == i21) {
      c0 = CHAR_SET_L1_LO(ch1);
      c1 = ch1;
    }

    if ((L1 = (*L2)[i2]) == (cset_L1_t *) -1)
      continue;

    if (L1 == 0) {
      static cset_L1_t blank;
      L1 = (*L2)[i2] = (cset_L1_t *) chk_malloc(sizeof *L1);
      memcpy(L1, &blank, sizeof *L1);
    }

    L1_fill_range(L1, c0, c1);

    if (L1_full(L1)) {
      free(L1);
      (*L2)[i2] = (cset_L1_t *) -1;
    }
  }
}

static int L2_contains(cset_L2_t *L2, wchar_t ch)
{
  int i2 = CHAR_SET_L2(ch);
  cset_L1_t *L1 = (*L2)[i2];

  if (L1 == 0)
    return 0;
  else if (L1 == (cset_L1_t *) -1)
    return 1;
  else
    return L1_contains(L1, ch);
}

static void L2_free(cset_L2_t *L2)
{
  int i2;

  for (i2 = 0; i2 < 16; i2++) {
    cset_L1_t *L1 = (*L2)[i2];
    if (L1 != 0 && L1 != (cset_L1_t *) -1) {
      L1_free((*L2)[i2]);
      free((*L2)[i2]);
    }
  }
}

static void L3_fill_range(cset_L3_t *L3, wchar_t ch0, wchar_t ch1)
{
  int i3, i30, i31;

  i30 = CHAR_SET_L3(ch0);
  i31 = CHAR_SET_L3(ch1);

  for (i3 = i30; i3 <= i31; i3++) {
    wchar_t c0 = 0, c1 = 0;
    cset_L2_t *L2;

    if (i3 > i30 && i3 < i31) {
      free((*L3)[i3]);
      (*L3)[i3] = (cset_L2_t *) -1;
      continue;
    } else if (i30 == i31) {
      c0 = ch0;
      c1 = ch1;
    } else if (i3 == i30) {
      c0 = ch0;
      c1 = CHAR_SET_L2_HI(ch0);
    } else if (i3 == i31) {
      c0 = CHAR_SET_L2_LO(ch1);
      c1 = ch1;
    }

    if ((L2 = (*L3)[i3]) == (cset_L2_t *) -1)
      continue;

    if (L2 == 0) {
      static cset_L2_t blank;
      L2 = (*L3)[i3] = (cset_L2_t *) chk_malloc(sizeof *L2);
      memcpy(L2, &blank, sizeof *L2);
    }

    L2_fill_range(L2, c0, c1);
    if (L2_full(L2)) {
      free(L2);
      (*L3)[i3] = (cset_L2_t *) -1;
    }
  }
}

static int L3_contains(cset_L3_t *L3, wchar_t ch)
{
  int i3 = CHAR_SET_L3(ch);
  cset_L2_t *L2 = (*L3)[i3];

  if (L2 == 0)
    return 0;
  else if (L2 == (cset_L2_t *) -1)
    return 1;
  else
    return L2_contains(L2, ch);
}

static void L3_free(cset_L3_t *L3)
{
  int i3;

  for (i3 = 0; i3 < 17; i3++) {
    cset_L2_t *L2 = (*L3)[i3];
    if (L2 != 0 && L2 != (cset_L2_t *) -1) {
      L2_free((*L3)[i3]);
      free((*L3)[i3]);
    }
  }
}

static char_set_t *char_set_create(chset_type_t type, wchar_t base)
{
  static char_set_t blank;
  char_set_t *cs = (char_set_t *) chk_malloc(sizeof *cs);
  *cs = blank;
  cs->any.type = type;

  if (type == CHSET_DISPLACED)
    cs->d.base = base;

  return cs;
}

static void char_set_destroy(char_set_t *set)
{
  switch (set->any.type) {
  case CHSET_DISPLACED:
  case CHSET_SMALL:
    free(set);
    break;
  case CHSET_LARGE:
    L2_free(&set->l.dir);
    free(set);
    break;
  case CHSET_XLARGE:
    L3_free(&set->xl.dir);
    free(set);
    break;
  }
}

static void char_set_compl(char_set_t *set)
{
  set->any.comp = 1;
}

static void char_set_add(char_set_t *set, wchar_t ch)
{
  switch (set->any.type) {
  case CHSET_DISPLACED:
    assert (ch >= set->d.base && ch < set->d.base + 256);
    ch -= set->d.base;
    /* fallthrough */
  case CHSET_SMALL:
    assert (ch < 256);
    set->s.bitcell[CHAR_SET_INDEX(ch)] |= (1 << CHAR_SET_BIT(ch));
    break;
  case CHSET_LARGE:
    assert (ch < 0x10000);
    L2_fill_range(&set->l.dir, ch, ch);
    break;
  case CHSET_XLARGE:
    assert (ch < 0x110000);
    L3_fill_range(&set->xl.dir, ch, ch);
    break;
  }
}

static void char_set_add_range(char_set_t *set, wchar_t ch0, wchar_t ch1)
{
  if (ch0 >= ch1)
    return;

  switch (set->any.type) {
  case CHSET_DISPLACED:
    assert (ch0 >= set->d.base && ch1 < set->d.base + 256);
    ch0 -= set->d.base;
    ch1 -= set->d.base;
    /* fallthrough */
  case CHSET_SMALL:
    assert (ch1 < 256);
    L0_fill_range(&set->s.bitcell, ch0, ch1);
    break;
  case CHSET_LARGE:
    assert (ch1 < 0x10000);
    L2_fill_range(&set->l.dir, ch0, ch1);
    break;
  case CHSET_XLARGE:
    assert (ch1 < 0x110000);
    L3_fill_range(&set->xl.dir, ch0, ch1);
    break;
  }
}

static int char_set_contains(char_set_t *set, wchar_t ch)
{
  int result = 0;

  switch (set->any.type) {
  case CHSET_DISPLACED:
    if (ch < set->d.base)
      break;
    ch -= set->d.base;
    /* fallthrough */
  case CHSET_SMALL:
    if (ch >= 256)
      break;
    result = L0_contains(&set->s.bitcell, ch);
    break;
  case CHSET_LARGE:
    if (ch >= 0x10000)
      break;
    result = L2_contains(&set->l.dir, ch);
    break;
  case CHSET_XLARGE:
    if (ch >= 0x110000)
      break;
    result = L3_contains(&set->xl.dir, ch);
    break;
  }

  return set->any.comp ? !result : result;
}

static nfa_state_t *nfa_state_accept(void)
{
  nfa_state_t *st = (nfa_state_t *) chk_malloc(sizeof *st);
  st->a.kind = nfa_accept;
  st->a.visited = 0;
  return st;
}

static nfa_state_t *nfa_state_empty(nfa_state_t *t0, nfa_state_t *t1)
{
  nfa_state_t *st = (nfa_state_t *) chk_malloc(sizeof *st);
  st->e.kind = nfa_empty;
  st->e.visited = 0;
  st->e.trans0 = t0;
  st->e.trans1 = t1;
  return st;
}

static nfa_state_t *nfa_state_single(nfa_state_t *t, wchar_t ch)
{
  nfa_state_t *st = (nfa_state_t *) chk_malloc(sizeof *st);
  st->o.kind = nfa_single;
  st->o.visited = 0;
  st->o.trans = t;
  st->o.ch = ch;
  return st;
}

static nfa_state_t *nfa_state_wild(nfa_state_t *t)
{
  nfa_state_t *st = (nfa_state_t *) chk_malloc(sizeof *st);
  st->o.kind = nfa_wild;
  st->o.visited = 0;
  st->o.trans = t;
  st->o.ch = 0;
  return st;
}

static void nfa_state_free(nfa_state_t *st)
{
  if (st->a.kind == nfa_set)
    char_set_destroy(st->s.set);
  free(st);
}

static void nfa_state_shallow_free(nfa_state_t *st)
{
  free(st);
}

static nfa_state_t *nfa_state_set(nfa_state_t *t, char_set_t *cs)
{
  nfa_state_t *st = (nfa_state_t *) chk_malloc(sizeof *st);
  st->s.kind = nfa_set;
  st->s.visited = 0;
  st->s.trans = t;
  st->s.set = cs;
  return st;
}

/*
 * An acceptance state is converted to an empty transition
 * state with specified transitions. It thereby loses
 * its acceptance state status. This is used during
 * compilation to hook new output paths into an inner NFA,
 * either back to itself, or to a new state in the
 * surrounding new NFA.
 */
static void nfa_state_empty_convert(nfa_state_t *acc, nfa_state_t *t0,
                                    nfa_state_t *t1)
{
  assert (acc->a.kind == nfa_accept);
  acc->e.kind = nfa_empty;
  acc->e.trans0 = t0;
  acc->e.trans1 = t1;
}

/*
 * Acceptance state takes on the kind of st, and all associated
 * data. I.e. we merge the identity of accept,
 * with the contents of st, such that the new state has
 * all of the outgoing arrows of st, and
 * all of the incoming arrows of acc.
 * This is easily done with an assignment, provided
 * that st doesn't have any incoming arrows.
 * We ensure that start states don't have any incoming
 * arrows in the compiler, by ensuring that repetition
 * operators terminate their backwards arrows on an
 * existing start state, and allocate a new start
 * state in front of it.
 */
static void nfa_state_merge(nfa_state_t *acc, nfa_state_t *st)
{
  assert (acc->a.kind == nfa_accept);
  *acc = *st;
}

static nfa_t nfa_make(nfa_state_t *s, nfa_state_t *acc)
{
  nfa_t ret;
  ret.start = s;
  ret.accept = acc;
  return ret;
}

/*
 * Combine two NFA's representing regexps that are catenated.
 * The acceptance state of the predecessor is merged with the start state of
 * the successor.
 */
static nfa_t nfa_combine(nfa_t pred, nfa_t succ)
{
  nfa_t ret;
  ret.start = pred.start;
  ret.accept = succ.accept;
  nfa_state_merge(pred.accept, succ.start);
  nfa_state_shallow_free(succ.start); /* No longer needed. */
  return ret;
}

static nfa_t nfa_compile_set(val args, int comp)
{
  val iter;
  wchar_t min = WCHAR_MAX;
  wchar_t max = 0;
  chset_type_t cst;

  for (iter = args; iter; iter = rest(iter)) {
    val item = first(iter);

    if (consp(item)) {
      val from = car(item);
      val to = cdr(item);

      assert (typeof(from) == chr_s && typeof(to) == chr_s);

      if (c_chr(from) < min)
        min = c_chr(from);
      if (c_chr(from) > max)
        max = c_chr(from);

      if (c_chr(to) < min)
        min = c_chr(to);
      if (c_chr(to) > max)
        max = c_chr(to);
    } else if (typeof(item) == chr_s) {
      if (c_chr(item) < min)
        min = c_chr(item);
      if (c_chr(item) > max)
        max = c_chr(item);
    } else {
      assert(0 && "bad regex set");
    }
  }

  if (max < 0x100)
    cst = CHSET_SMALL;
  else if (max - min < 0x100)
    cst = CHSET_DISPLACED;
  else if (max < 0x10000)
    cst = CHSET_LARGE;
  else
    cst = CHSET_XLARGE;

  {
    char_set_t *set = char_set_create(cst, min);
    nfa_state_t *acc = nfa_state_accept();
    nfa_state_t *s = nfa_state_set(acc, set);
    nfa_t ret = nfa_make(s, acc);

    for (iter = args; iter; iter = rest(iter)) {
      val item = first(iter);

      if (consp(item)) {
        val from = car(item);
        val to = cdr(item);

        assert (typeof(from) == chr_s && typeof(to) == chr_s);
        char_set_add_range(set, c_chr(from), c_chr(to));
      } else if (typeof(item) == chr_s) {
        char_set_add(set, c_chr(item));
      } else {
        assert(0 && "bad regex set");
      }
    }

    if (comp)
      char_set_compl(set);
    return ret;
  }
}

/*
 * Input is the items from a regex form,
 * not including the regex symbol.
 * I.e.  (rest '(regex ...)) not '(regex ...).
 */
nfa_t nfa_compile_regex(val items)
{
  if (nullp(items)) {
    nfa_state_t *acc = nfa_state_accept();
    nfa_state_t *s = nfa_state_empty(acc, 0);
    nfa_t nfa = nfa_make(s, acc);
    return nfa;
  } else {
    val item = first(items), others = rest(items);
    nfa_t nfa;

    if (typeof(item) == chr_s) {
      nfa_state_t *acc = nfa_state_accept();
      nfa_state_t *s = nfa_state_single(acc, c_chr(item));
      nfa = nfa_make(s, acc);
    } else if (item == wild_s) {
      nfa_state_t *acc = nfa_state_accept();
      nfa_state_t *s = nfa_state_wild(acc);
      nfa = nfa_make(s, acc);
    } else if (consp(item)) {
      val sym = first(item);
      val args = rest(item);

      if (sym == set_s) {
        nfa = nfa_compile_set(args, 0);
      } else if (sym == cset_s) {
        nfa = nfa_compile_set(args, 1);
      } else if (sym == compound_s) {
        nfa = nfa_compile_regex(args);
      } else if (sym == zeroplus_s) {
        nfa_t nfa_args = nfa_compile_regex(args);
        nfa_state_t *acc = nfa_state_accept();
        /* New start state has empty transitions going through
           the inner NFA, or skipping it right to the new acceptance state. */
        nfa_state_t *s = nfa_state_empty(nfa_args.start, acc);
        /* Convert acceptance state of inner NFA to one which has
           an empty transition back to the start state, and
           an empty transition to the new acceptance state. */
        nfa_state_empty_convert(nfa_args.accept, nfa_args.start, acc);
        nfa = nfa_make(s, acc);
      } else if (sym == oneplus_s) {
        /* One-plus case differs from zero-plus in that the new start state
           does not have an empty transition to the acceptance state.
           So the inner NFA must be traversed once. */
        nfa_t nfa_args = nfa_compile_regex(args);
        nfa_state_t *acc = nfa_state_accept();
        nfa_state_t *s = nfa_state_empty(nfa_args.start, 0); /* <-- diff */
        nfa_state_empty_convert(nfa_args.accept, nfa_args.start, acc);
        nfa = nfa_make(s, acc);
      } else if (sym == optional_s) {
        /* In this case, we can keep the acceptance state of the inner
           NFA as the acceptance state of the new NFA. We simply add
           a new start state which can short-circuit to it via an empty
           transition.  */
        nfa_t nfa_args = nfa_compile_regex(args);
        nfa_state_t *s = nfa_state_empty(nfa_args.start, nfa_args.accept);
        nfa = nfa_make(s, nfa_args.accept);
      } else if (sym == or_s) {
        /* Simple: make a new start and acceptance state, which form
           the ends of a spindle that goes through two branches. */
        nfa_t nfa_first = nfa_compile_regex(first(args));
        nfa_t nfa_second = nfa_compile_regex(second(args));
        nfa_state_t *acc = nfa_state_accept();
        /* New state s has empty transitions into each inner NFA. */
        nfa_state_t *s = nfa_state_empty(nfa_first.start, nfa_second.start);
        /* Acceptance state of each inner NFA converted to empty
           transition to new combined acceptance state. */
        nfa_state_empty_convert(nfa_first.accept, acc, 0);
        nfa_state_empty_convert(nfa_second.accept, acc, 0);
        nfa = nfa_make(s, acc);
      } else {
        assert (0 && "internal error: bad operator in regex");
      }
    } else {
        assert (0 && "internal error: bad regex item");
    }

    /* We made an NFA for the first item, but others follow.
       Compile the others to an NFA recursively, then
       stick it with this NFA. */
    if (others) {
      nfa_t nfa_others = nfa_compile_regex(others);
      nfa = nfa_combine(nfa, nfa_others);
    }

    return nfa;
  }
}

static int nfa_all_states(nfa_state_t **inout, int num, unsigned visited)
{
  int i;

  for (i = 0; i < num; i++)
    inout[i]->a.visited = visited;

  for (i = 0; i < num; i++) {
    nfa_state_t *s = inout[i];

    if (num >= NFA_SET_SIZE)
      internal_error("NFA set size exceeded");

    switch (s->a.kind) {
    case nfa_accept:
      break;
    case nfa_empty:
      {
        nfa_state_t *e0 = s->e.trans0;
        nfa_state_t *e1 = s->e.trans1;

        if (e0 && e0->a.visited != visited) {
          e0->a.visited = visited;
          inout[num++] = e0;
        }
        if (e1 && e1->a.visited != visited) {
          e1->a.visited = visited;
          inout[num++] = e1;
        }
      }
      break;
    case nfa_wild:
    case nfa_single:
    case nfa_set:
      if (s->o.trans->a.visited != visited) {
        s->o.trans->a.visited = visited;
        inout[num++] = s->o.trans;
      }
      break;
    }
  }

  if (num > NFA_SET_SIZE)
    internal_error("NFA set size exceeded");

  return num;
}

void nfa_free(nfa_t nfa)
{
  nfa_state_t **all = (nfa_state_t **) chk_malloc(NFA_SET_SIZE * sizeof *all);
  int nstates, i;

  all[0] = nfa.start;
  all[1] = nfa.accept;

  nstates = nfa_all_states(all, 2, nfa.start->a.visited);

  for (i = 0; i < nstates; i++)
    nfa_state_free(all[i]);

  free(all);
}

/*
 * Compute the epsilon-closure of the NFA states stored in the set in, whose
 * size is given by nin. The results are stored in the set out, the size of
 * which is returned. The stack parameter provides storage used by the
 * algorithm, so it doesn't have to be allocated and freed repeatedly.
 * The visited parameter is a stamp used for marking states which are added
 * to the epsilon-closure set, so that sets are not added twice.
 * If any of the states added to the closure are acceptance states,
 * the accept parameter is used to store the flag 1.
 *
 * An epsilon-closure is the set of all input states, plus all additional
 * states which are reachable from that set with empty (epsilon) transitions.
 * (Transitions that don't do not consume and match an input character).
 */
static int nfa_closure(nfa_state_t **stack, nfa_state_t **in, int nin,
                       nfa_state_t **out, unsigned visited, int *accept)
{
  int i, nout = 0;
  int stackp = 0;

  /* First, add all states in the input state to the closure,
     push them on the stack, and mark them as visited. */
  for (i = 0; i < nin; i++) {
    if (stackp >= NFA_SET_SIZE)
      internal_error("NFA set size exceeded");
    in[i]->a.visited = visited;
    stack[stackp++] = in[i];
    out[nout++] = in[i];
    if (in[i]->a.kind == nfa_accept)
      *accept = 1;
  }

  while (stackp) {
    nfa_state_t *top = stack[--stackp];

    if (nout >= NFA_SET_SIZE)
      internal_error("NFA set size exceeded");

    /* Only states of type nfa_empty are interesting.
       Each such state at most two epsilon transitions. */

    if (top->a.kind == nfa_empty) {
      nfa_state_t *e0 = top->e.trans0;
      nfa_state_t *e1 = top->e.trans1;

      if (e0 && e0->a.visited != visited) {
        e0->a.visited = visited;
        stack[stackp++] = e0;
        out[nout++] = e0;
        if (e0->a.kind == nfa_accept)
          *accept = 1;
      }

      if (e1 && e1->a.visited != visited) {
        e1->a.visited = visited;
        stack[stackp++] = e1;
        out[nout++] = e1;
        if (e1->a.kind == nfa_accept)
          *accept = 1;
      }
    }
  }

  if (nout > NFA_SET_SIZE)
    internal_error("NFA set size exceeded");

  return nout;
}

/*
 * Compute the move set from a given set of NFA states. The move
 * set is the set of states which are reachable from the set of
 * input states on the consumpion of the input character given by ch.
 */
static int nfa_move(nfa_state_t **in, int nin, nfa_state_t **out, wchar_t ch)
{
  int i, nmove;

  for (nmove = 0, i = 0; i < nin; i++) {
    nfa_state_t *s = in[i];

    switch (s->a.kind) {
    case nfa_wild:
      /* Unconditional match; don't have to look at ch. */
      break;
    case nfa_single:
      if (s->o.ch == ch) /* Character match. */
        break;
      continue; /* no match */
    case nfa_set:
      if (char_set_contains(s->s.set, ch)) /* Set match. */
        break;
      continue; /* no match */
    default:
      /* Epsilon-transition and acceptance states have no character moves. */
      continue;
    }

    /* The state matches the character, so add it to the move set.
       C trick: all character-transitioning state types have the
       pointer to the next state in the same position,
       among a common set of leading struct members in the union. */

    if (nmove >= NFA_SET_SIZE)
      internal_error("NFA set size exceeded");
    out[nmove++] = s->o.trans;
  }

  return nmove;
}

/*
 * Match regex against the string in. The match is
 * anchored to the front of the string; to search
 * within the string, a .* must be added to the front
 * of the regex.
 *
 * Returns the length of the prefix of the string
 * which matches the regex.  Or, if you will,
 * the position of the first mismatching
 * character.
 *
 * If the regex does not match at all, zero is
 * returned.
 *
 * Matching stops when a state is reached from which
 * there are no transitions on the next input character,
 * or when the string runs out of characters.
 * The most recently visited acceptance state then
 * determines the match length (defaulting to zero
 * if no acceptance states were encountered).
 */
cnum nfa_run(nfa_t nfa, const wchar_t *str)
{
  const wchar_t *last_accept_pos = 0, *ptr = str;
  unsigned visited = nfa.start->a.visited + 1;
  nfa_state_t **move = (nfa_state_t **) chk_malloc(NFA_SET_SIZE * sizeof *move);
  nfa_state_t **clos = (nfa_state_t **) chk_malloc(NFA_SET_SIZE * sizeof *clos);
  nfa_state_t **stack = (nfa_state_t **) chk_malloc(NFA_SET_SIZE * sizeof *stack);
  int nmove = 1, nclos;
  int accept = 0;

  move[0] = nfa.start;

  nclos = nfa_closure(stack, move, nmove, clos, visited++, &accept);

  if (accept)
    last_accept_pos = ptr;

  for (; *ptr != 0; ptr++) {
    wchar_t ch = *ptr;

    accept = 0;

    nmove = nfa_move(clos, nclos, move, ch);
    nclos = nfa_closure(stack, move, nmove, clos, visited++, &accept);

    if (accept)
      last_accept_pos = ptr + 1;

    if (nclos == 0) /* dead end; no match */
      break;
  }

  nfa.start->a.visited = visited;

  free(stack);
  free(clos);
  free(move);

  return last_accept_pos ? last_accept_pos - str : -1;
}

cnum nfa_machine_match_span(nfa_machine_t *nfam)
{
  return nfam->last_accept_pos;
}

/*
 * NFA machine: represents the logic of the nfa_run function as state machine
 * object which can be fed one character at a time.
 */

void nfa_machine_reset(nfa_machine_t *nfam)
{
  int accept = 0;

  nfam->last_accept_pos = -1;
  nfam->visited = nfam->nfa.start->a.visited + 1;
  nfam->nmove = 1;
  nfam->count = 0;

  nfam->move[0] = nfam->nfa.start;

  nfam->nclos = nfa_closure(nfam->stack, nfam->move, nfam->nmove,
                            nfam->clos, nfam->visited++, &accept);

  if (accept)
    nfam->last_accept_pos = nfam->count;
}

void nfa_machine_init(nfa_machine_t *nfam, nfa_t nfa)
{
  nfam->nfa = nfa;
  nfam->move = (nfa_state_t **) chk_malloc(NFA_SET_SIZE * sizeof *nfam->move);
  nfam->clos = (nfa_state_t **) chk_malloc(NFA_SET_SIZE * sizeof *nfam->clos);
  nfam->stack = (nfa_state_t **) chk_malloc(NFA_SET_SIZE * sizeof *nfam->stack);
  nfa_machine_reset(nfam);
}

void nfa_machine_cleanup(nfa_machine_t *nfam)
{
  free(nfam->stack);
  free(nfam->clos);
  free(nfam->move);
  nfam->stack = 0;
  nfam->clos = 0;
  nfam->move = 0;
  nfam->nfa.start = 0;
  nfam->nfa.accept = 0;
}

nfam_result_t nfa_machine_feed(nfa_machine_t *nfam, wchar_t ch)
{
  int accept = 0;

  if (ch != 0) {
    nfam->count++;

    nfam->nmove = nfa_move(nfam->clos, nfam->nclos, nfam->move, ch);
    nfam->nclos = nfa_closure(nfam->stack, nfam->move, nfam->nmove, nfam->clos,
                              nfam->visited++, &accept);

    if (accept)
      nfam->last_accept_pos = nfam->count;
  }

  nfam->nfa.start->a.visited = nfam->visited;

  if (ch && nfam->nclos != 0) {
    if (accept)
       return NFAM_MATCH;
    return NFAM_INCOMPLETE;
  }

  /* Reached if the null character is
     consumed, or NFA hit a transition dead end. */

  if (nfam->last_accept_pos == nfam->count)
    return NFAM_MATCH;
  if (nfam->last_accept_pos == -1)
    return NFAM_FAIL;
  return NFAM_INCOMPLETE;
}

static void regex_destroy(val regex)
{
  nfa_t *pnfa = (nfa_t *) regex->co.handle;
  nfa_free(*pnfa);
  free(pnfa);
  regex->co.handle = 0;
}

static struct cobj_ops regex_obj_ops = {
  cobj_equal_op,
  cobj_print_op,
  regex_destroy,
  cobj_mark_op,
  cobj_hash_op
};

val regex_compile(val regex_sexp)
{
  nfa_t *pnfa = (nfa_t *) chk_malloc(sizeof *pnfa);
  *pnfa = nfa_compile_regex(regex_sexp);
  return cobj((mem_t *) pnfa, regex_s, &regex_obj_ops);
}

val regexp(val obj)
{
  return (is_ptr(obj) && obj->co.type == COBJ && obj->co.cls == regex_s)
         ? t : nil;
}

nfa_t *regex_nfa(val reg)
{
  assert (reg->co.type == COBJ && reg->co.cls == regex_s);
  return (nfa_t *) reg->co.handle;
}

val search_regex(val haystack, val needle_regex, val start,
                 val from_end)
{
  nfa_t *pnfa = regex_nfa(needle_regex);

  if (length_str_lt(haystack, start)) {
    return nil;
  } else {
    if (from_end) {
      cnum i;
      cnum s = c_num(start);
      const wchar_t *h = c_str(haystack);

      for (i = c_num(length_str(haystack)) - 1; i >= s; i--) {
        cnum span = nfa_run(*pnfa, h + i);
        if (span >= 0)
          return cons(num(i), num(span));
      }
    } else {
      nfa_machine_t nfam;
      val i, pos = start, retval;
      nfam_result_t last_res = NFAM_INCOMPLETE;

      nfa_machine_init(&nfam, *pnfa);

again:
      for (i = pos; length_str_gt(haystack, i); i = plus(i, one)) {
        last_res = nfa_machine_feed(&nfam, c_chr(chr_str(haystack, i)));

        if (last_res == NFAM_FAIL) {
          nfa_machine_reset(&nfam);
          pos = plus(pos, one);
          goto again;
        }
      }

      last_res = nfa_machine_feed(&nfam, 0);

      switch (last_res) {
      case NFAM_INCOMPLETE:
      case NFAM_MATCH:
        retval = cons(pos, num(nfa_machine_match_span(&nfam)));
        nfa_machine_cleanup(&nfam);
        return retval;
      case NFAM_FAIL:
        nfa_machine_cleanup(&nfam);
        return nil;
      }
    }

    return nil;
  }
}

val match_regex(val str, val reg, val pos)
{
  nfa_machine_t nfam;
  val i, retval;
  nfam_result_t last_res = NFAM_INCOMPLETE;
  nfa_t *pnfa = regex_nfa(reg);

  nfa_machine_init(&nfam, *pnfa);

  for (i = pos; length_str_gt(str, i); i = plus(i, one)) {
    last_res = nfa_machine_feed(&nfam, c_chr(chr_str(str, i)));
    if (last_res == NFAM_FAIL)
      break;
  }

  last_res = nfa_machine_feed(&nfam, 0);

  switch (last_res) {
  case NFAM_INCOMPLETE:
  case NFAM_MATCH:
    retval = plus(pos, num(nfa_machine_match_span(&nfam)));
    nfa_machine_cleanup(&nfam);
    return retval;
  case NFAM_FAIL:
    nfa_machine_cleanup(&nfam);
    return nil;
  }

  return nil;
}
