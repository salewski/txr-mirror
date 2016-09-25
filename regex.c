/* Copyright 2009-2016
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <wchar.h>
#include <assert.h>
#include <dirent.h>
#include <limits.h>
#include <signal.h>
#include <stdarg.h>
#include "config.h"
#include ALLOCA_H
#include "lib.h"
#include "parser.h"
#include "signal.h"
#include "unwind.h"
#include "stream.h"
#include "gc.h"
#include "eval.h"
#include "cadr.h"
#include "regex.h"
#include "txr.h"

#if WCHAR_MAX > 65535
#define FULL_UNICODE
#endif

typedef union nfa_state nfa_state_t;

typedef struct nfa {
  nfa_state_t *start;
  nfa_state_t *accept;
} nfa_t;

typedef enum { REGEX_NFA, REGEX_DV } regex_kind_t;

typedef struct regex {
  regex_kind_t kind;
  union {
    struct nfa nfa;
    val dv;
  } r;
  int nstates;
  val source;
} regex_t;

/*
 * Result from regex_machine_feed.
 * These values have two meanings, based on whether
 * the matching is still open (characters are being fed)
 * or finalized.
 *
 * When feeding characters:
 * REGM_INCOMPLETE: no match at this character, but matching can continue.
 * REGM_FAIL: no more state transitions are possible.
 * REGM_MATCH: match (accept state) for this character.
 *
 * When the end of the input is encountered, or a REGM_FAIL,
 * then regex_machine_feed is called one more time with
 * the null character. It then reports:
 * REGM_INCOMPLETE: there was a partial match for the input.
 * REGM_FAIL: none of the input matched.
 * REGM_MATCH: the input was completely matched
 *
 * Note that a REGM_FAIL (no transitions) during the character feeding phase
 * can turn into REGM_INCOMPLETE (partial match) when the match is sealed with
 * the null character signal!
 */
typedef enum regm_result {
  REGM_INCOMPLETE,
  REGM_FAIL,
  REGM_MATCH
} regm_result_t;

typedef union regex_machine regex_machine_t;

typedef unsigned int bitcell_t;

#define CHAR_SET_SIZE (256 / (sizeof (bitcell_t) * CHAR_BIT))

#define BITCELL_BIT (sizeof (bitcell_t) * CHAR_BIT)

#define CHAR_SET_INDEX(CH) ((CH) / BITCELL_BIT)
#define CHAR_SET_BIT(CH) ((CH) % BITCELL_BIT)

#define CHAR_SET_L0(CH) ((CH) & 0xFF)
#define CHAR_SET_L1(CH) (((CH) >> 8) & 0xF)
#define CHAR_SET_L2(CH) (((CH) >> 12) & 0xF)
#ifdef FULL_UNICODE
#define CHAR_SET_L3(CH) (((CH) >> 16) & 0x1F)
#endif

#ifdef FULL_UNICODE
#define CHAR_SET_L2_LO(CH) ((CH) & ~convert(wchar_t, 0xFFFF))
#define CHAR_SET_L2_HI(CH) ((CH) | convert(wchar_t, 0xFFFF))
#endif

#define CHAR_SET_L1_LO(CH) ((CH) & ~convert(wchar_t, 0xFFF))
#define CHAR_SET_L1_HI(CH) ((CH) | convert(wchar_t, 0xFFF))

#define CHAR_SET_L0_LO(CH) ((CH) & ~convert(wchar_t, 0xFF))
#define CHAR_SET_L0_HI(CH) ((CH) | convert(wchar_t, 0xFF))

typedef enum {
  CHSET_SMALL, CHSET_DISPLACED, CHSET_LARGE,
#ifdef FULL_UNICODE
  CHSET_XLARGE
#endif
} chset_type_t;

typedef bitcell_t cset_L0_t[CHAR_SET_SIZE];
typedef cset_L0_t *cset_L1_t[16];
typedef cset_L1_t *cset_L2_t[16];
#ifdef FULL_UNICODE
typedef cset_L2_t *cset_L3_t[17];
#endif

struct any_char_set {
  unsigned type : 3;
  unsigned comp : 1;
  unsigned stat : 1;
};

struct small_char_set {
  unsigned type : 3;
  unsigned comp : 1;
  unsigned stat : 1;
  cset_L0_t bitcell;
};

struct displaced_char_set {
  unsigned type : 3;
  unsigned comp : 1;
  unsigned stat : 1;
  cset_L0_t bitcell;
  wchar_t base;
};


struct large_char_set {
  unsigned type : 3;
  unsigned comp : 1;
  unsigned stat : 1;
  cset_L2_t dir;
};

#ifdef FULL_UNICODE
struct xlarge_char_set {
  unsigned type : 3;
  unsigned comp : 1;
  unsigned stat : 1;
  cset_L3_t dir;
};
#endif

typedef union char_set {
  struct any_char_set any;
  struct small_char_set s;
  struct displaced_char_set d;
  struct large_char_set l;
#ifdef FULL_UNICODE
  struct xlarge_char_set xl;
#endif
} char_set_t;

typedef enum {
  nfa_accept, nfa_reject, nfa_empty, nfa_wild, nfa_single, nfa_set
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
  int is_nfa;           /* common member */
  cnum last_accept_pos; /* common member */
  cnum count;           /* common member */
  unsigned visited;
  nfa_state_t **set, **stack;
  int nclos;
  nfa_t nfa;
  int nstates;
};

struct dv_machine {
  int is_nfa;           /* common member */
  cnum last_accept_pos; /* common member */
  cnum count;           /* common member */
  val deriv;
  val regex;
};

union regex_machine {
  struct nfa_machine n;
  struct dv_machine d;
};

int opt_derivative_regex = 0;

wchar_t spaces[] = {
  0x0009, 0x000a, 0x000b, 0x000c, 0x000d, 0x0020, 0x00a0, 0x1680, 0x180e,
  0x2000, 0x2001, 0x2002, 0x2003, 0x2004, 0x2005, 0x2006, 0x2007, 0x2008,
  0x2009, 0x200a, 0x2028, 0x2029, 0x205f, 0x3000, 0
};

static int L0_full(cset_L0_t *L0)
{
  int i;

  for (i = 0; i < convert(int, CHAR_SET_SIZE); i++)
    if ((*L0)[i] != convert(bitcell_t, -1))
      return 0;
  return 1;
}

static void L0_fill_range(cset_L0_t *L0, wchar_t ch0, wchar_t ch1)
{
  int i;
  int bt0 = CHAR_SET_BIT(ch0);
  int bc0 = CHAR_SET_INDEX(ch0);
  bitcell_t mask0 = ~((convert(bitcell_t, 1) << bt0) - 1);
  int bt1 = CHAR_SET_BIT(ch1);
  int bc1 = CHAR_SET_INDEX(ch1);
  bitcell_t mask1 = (bt1 == (BITCELL_BIT - 1))
                     ? convert(bitcell_t, -1)
                     : (convert(bitcell_t, 1) << (bt1 + 1)) - 1;

  if (bc1 == bc0) {
    (*L0)[bc0] |= (mask0 & mask1);
  } else {
    (*L0)[bc0] |= mask0;
    (*L0)[bc1] |= mask1;
    for (i = bc0 + 1; i < bc1; i++)
      (*L0)[i] = convert(bitcell_t, -1);
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
    if ((*L1)[i] != coerce(cset_L0_t *, -1))
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
      (*L1)[i1] = coerce(cset_L0_t *, -1);
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

    if ((L0 = (*L1)[i1]) == coerce(cset_L0_t *, -1))
      continue;

    if (L0 == 0) {
      static cset_L0_t blank;
      L0 = (*L1)[i1] = coerce(cset_L0_t *, chk_malloc(sizeof *L0));
      memcpy(L0, &blank, sizeof *L0);
    }

    L0_fill_range(L0, CHAR_SET_L0(c0), CHAR_SET_L0(c1));

    if (L0_full(L0)) {
      free(L0);
      (*L1)[i1] = coerce(cset_L0_t *, -1);
    }
  }
}

static int L1_contains(cset_L1_t *L1, wchar_t ch)
{
  int i1 = CHAR_SET_L1(ch);
  cset_L0_t *L0 = (*L1)[i1];

  if (L0 == 0)
    return 0;
  else if (L0 == coerce(cset_L0_t *, -1))
    return 1;
  else
    return L0_contains(L0, CHAR_SET_L0(ch));
}


static void L1_free(cset_L1_t *L1)
{
  int i1;

  if (L1 == coerce(cset_L1_t *, -1))
    return;

  for (i1 = 0; i1 < 16; i1++)
    if ((*L1)[i1] != coerce(cset_L0_t *, -1))
      free((*L1)[i1]);
}

#ifdef FULL_UNICODE
static int L2_full(cset_L2_t *L2)
{
  int i;
  for (i = 0; i < 16; i++)
    if ((*L2)[i] != coerce(cset_L1_t *, -1))
      return 0;
  return 1;
}
#endif

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
      (*L2)[i2] = coerce(cset_L1_t *, -1);
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

    if ((L1 = (*L2)[i2]) == coerce(cset_L1_t *, -1))
      continue;

    if (L1 == 0) {
      static cset_L1_t blank;
      L1 = (*L2)[i2] = coerce(cset_L1_t *, chk_malloc(sizeof *L1));
      memcpy(L1, &blank, sizeof *L1);
    }

    L1_fill_range(L1, c0, c1);

    if (L1_full(L1)) {
      free(L1);
      (*L2)[i2] = coerce(cset_L1_t *, -1);
    }
  }
}

static int L2_contains(cset_L2_t *L2, wchar_t ch)
{
  int i2 = CHAR_SET_L2(ch);
  cset_L1_t *L1 = (*L2)[i2];

  if (L1 == 0)
    return 0;
  else if (L1 == coerce(cset_L1_t *, -1))
    return 1;
  else
    return L1_contains(L1, ch);
}

static void L2_free(cset_L2_t *L2)
{
  int i2;

  for (i2 = 0; i2 < 16; i2++) {
    cset_L1_t *L1 = (*L2)[i2];
    if (L1 != 0 && L1 != coerce(cset_L1_t *, -1)) {
      L1_free((*L2)[i2]);
      free((*L2)[i2]);
    }
  }
}

#ifdef FULL_UNICODE

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
      (*L3)[i3] = coerce(cset_L2_t *, -1);
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

    if ((L2 = (*L3)[i3]) == coerce(cset_L2_t *, -1))
      continue;

    if (L2 == 0) {
      static cset_L2_t blank;
      L2 = (*L3)[i3] = coerce(cset_L2_t *, chk_malloc(sizeof *L2));
      memcpy(L2, &blank, sizeof *L2);
    }

    L2_fill_range(L2, c0, c1);
    if (L2_full(L2)) {
      free(L2);
      (*L3)[i3] = coerce(cset_L2_t *, -1);
    }
  }
}


static int L3_contains(cset_L3_t *L3, wchar_t ch)
{
  int i3 = CHAR_SET_L3(ch);
  cset_L2_t *L2 = (*L3)[i3];

  if (L2 == 0)
    return 0;
  else if (L2 == coerce(cset_L2_t *, -1))
    return 1;
  else
    return L2_contains(L2, ch);
}

static void L3_free(cset_L3_t *L3)
{
  int i3;

  for (i3 = 0; i3 < 17; i3++) {
    cset_L2_t *L2 = (*L3)[i3];
    if (L2 != 0 && L2 != coerce(cset_L2_t *, -1)) {
      L2_free((*L3)[i3]);
      free((*L3)[i3]);
    }
  }
}

#endif

static char_set_t *char_set_create(chset_type_t type, wchar_t base, unsigned st)
{
  static char_set_t blank;
  char_set_t *cs = coerce(char_set_t *, chk_malloc(sizeof *cs));
  *cs = blank;
  cs->any.type = type;
  cs->any.stat = st;

  if (type == CHSET_DISPLACED)
    cs->d.base = base;

  return cs;
}

static void char_set_destroy(char_set_t *set)
{
  if (!set)
    return;

  switch (set->any.type) {
  case CHSET_DISPLACED:
  case CHSET_SMALL:
    free(set);
    break;
  case CHSET_LARGE:
    L2_free(&set->l.dir);
    free(set);
    break;
#ifdef FULL_UNICODE
  case CHSET_XLARGE:
    L3_free(&set->xl.dir);
    free(set);
    break;
#endif
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
#ifdef FULL_UNICODE
  case CHSET_XLARGE:
    assert (ch < 0x110000);
    L3_fill_range(&set->xl.dir, ch, ch);
    break;
#endif
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
#ifdef FULL_UNICODE
  case CHSET_XLARGE:
    assert (ch1 < 0x110000);
    L3_fill_range(&set->xl.dir, ch0, ch1);
    break;
#endif
  }
}

static void char_set_add_str(char_set_t *set, wchar_t *str)
{
  while (*str != 0)
    char_set_add(set, *str++);
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
#ifdef FULL_UNICODE
  case CHSET_XLARGE:
    if (ch >= 0x110000)
      break;
    result = L3_contains(&set->xl.dir, ch);
    break;
#endif
  }

  return set->any.comp ? !result : result;
}

static char_set_t *char_set_compile(val args, val comp)
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
    } else if (item == space_k) {
      if (max < 0x3000)
        max = 0x3000;
      if (min > 0x9)
        min = 0x9;
    } else if (item == digit_k) {
      if (max < '9')
        max = 9;
      if (min > '0')
        min = 0;
    } else if (item == word_char_k) {
      if (min > 'A')
        min = 'A';
      if (max < 'z')
        max = 'z';
    } else if (item == cspace_k || item == cdigit_k || item == cword_char_k) {
      uw_throwf(error_s, lit("bad object in character class syntax: ~s"),
                item, nao);
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
#ifdef FULL_UNICODE
    cst = CHSET_XLARGE;
#else
    cst = CHSET_LARGE;
#endif


  {
    char_set_t *set = char_set_create(cst, min, 0);

    for (iter = args; iter; iter = rest(iter)) {
      val item = first(iter);

      if (consp(item)) {
        val from = car(item);
        val to = cdr(item);

        assert (typeof(from) == chr_s && typeof(to) == chr_s);
        char_set_add_range(set, c_chr(from), c_chr(to));
      } else if (typeof(item) == chr_s) {
        char_set_add(set, c_chr(item));
      } else if (item == space_k) {
        char_set_add_str(set, spaces);
      } else if (item == digit_k) {
        char_set_add_range(set, '0', '9');
      } else if (item == word_char_k) {
        char_set_add_range(set, 'A', 'Z');
        char_set_add_range(set, 'a', 'z');
        char_set_add(set, '_');
      } else {
        assert(0 && "bad regex set");
      }
    }

    if (comp)
      char_set_compl(set);

    return set;
  }
}

static char_set_t *space_cs, *digit_cs, *word_cs;
static char_set_t *cspace_cs, *cdigit_cs, *cword_cs;

static void init_special_char_sets(void)
{
  space_cs = char_set_create(CHSET_LARGE, 0, 1);
  cspace_cs = char_set_create(CHSET_LARGE, 0, 1);
  digit_cs = char_set_create(CHSET_SMALL, 0, 1);
  cdigit_cs = char_set_create(CHSET_SMALL, 0, 1);
  word_cs = char_set_create(CHSET_SMALL, 0, 1);
  cword_cs = char_set_create(CHSET_SMALL, 0, 1);

  char_set_compl(cspace_cs);
  char_set_compl(cdigit_cs);
  char_set_compl(cword_cs);

  char_set_add_str(space_cs, spaces);
  char_set_add_str(cspace_cs, spaces);

  char_set_add_range(digit_cs, '0', '9');
  char_set_add_range(cdigit_cs, '0', '9');

  char_set_add_range(word_cs, 'A', 'Z');
  char_set_add_range(cword_cs, 'A', 'Z');
  char_set_add_range(word_cs, 'a', 'z');
  char_set_add_range(cword_cs, 'a', 'z');
  char_set_add(word_cs, '_');
  char_set_add(cword_cs, '_');
}

static void char_set_cobj_destroy(val chset)
{
  char_set_t *set = coerce(char_set_t *, chset->co.handle);
  if (!set->any.stat)
    char_set_destroy(set);
  chset->co.handle = 0;
}

static struct cobj_ops char_set_obj_ops = cobj_ops_init(eq,
                                                        cobj_print_op,
                                                        char_set_cobj_destroy,
                                                        cobj_mark_op,
                                                        cobj_hash_op);

static nfa_state_t *nfa_state_accept(void)
{
  nfa_state_t *st = coerce(nfa_state_t *, chk_malloc(sizeof *st));
  st->a.kind = nfa_accept;
  st->a.visited = 0;
  return st;
}

static nfa_state_t *nfa_state_reject(void)
{
  nfa_state_t *st = coerce(nfa_state_t *, chk_malloc(sizeof *st));
  st->a.kind = nfa_reject;
  st->a.visited = 0;
  return st;
}

static nfa_state_t *nfa_state_empty(nfa_state_t *t0, nfa_state_t *t1)
{
  nfa_state_t *st = coerce(nfa_state_t *, chk_malloc(sizeof *st));
  st->e.kind = nfa_empty;
  st->e.visited = 0;
  st->e.trans0 = t0;
  st->e.trans1 = t1;
  return st;
}

static nfa_state_t *nfa_state_single(nfa_state_t *t, wchar_t ch)
{
  nfa_state_t *st = coerce(nfa_state_t *, chk_malloc(sizeof *st));
  st->o.kind = nfa_single;
  st->o.visited = 0;
  st->o.trans = t;
  st->o.ch = ch;
  return st;
}

static nfa_state_t *nfa_state_wild(nfa_state_t *t)
{
  nfa_state_t *st = coerce(nfa_state_t *, chk_malloc(sizeof *st));
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
  nfa_state_t *st = coerce(nfa_state_t *, chk_malloc(sizeof *st));
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

static nfa_t nfa_compile_set(val args, val comp)
{
  char_set_t *set = char_set_compile(args, comp);
  nfa_state_t *acc = nfa_state_accept();
  nfa_state_t *s = nfa_state_set(acc, set);
  return nfa_make(s, acc);
}

static nfa_t nfa_compile_given_set(char_set_t *set)
{
  nfa_state_t *acc = nfa_state_accept();
  nfa_state_t *s = nfa_state_set(acc, set);
  return nfa_make(s, acc);
}

static nfa_t nfa_compile_regex(val regex);

/*
 * Helper to nfa_compile_regex for compiling the argument list of
 * a compound regex.
 */

static nfa_t nfa_compile_list(val exp_list)
{
  nfa_t nfa_first = nfa_compile_regex(first(exp_list));

  if (rest(exp_list)) {
    nfa_t nfa_rest = nfa_compile_list(rest(exp_list));
    return nfa_combine(nfa_first, nfa_rest);
  } else {
    return nfa_first;
  }
}

/*
 * Input is the items from a regex form,
 * not including the regex symbol.
 * I.e.  (rest '(regex ...)) not '(regex ...).
 */
static nfa_t nfa_compile_regex(val exp)
{
  if (nilp(exp)) {
    nfa_state_t *acc = nfa_state_accept();
    nfa_state_t *s = nfa_state_empty(acc, 0);
    return nfa_make(s, acc);
  } else if (chrp(exp)) {
    nfa_state_t *acc = nfa_state_accept();
    nfa_state_t *s = nfa_state_single(acc, c_chr(exp));
    return nfa_make(s, acc);
  } else if (stringp(exp)) {
    return nfa_compile_regex(cons(compound_s, list_str(exp)));
  } else if (exp == wild_s) {
    nfa_state_t *acc = nfa_state_accept();
    nfa_state_t *s = nfa_state_wild(acc);
    return nfa_make(s, acc);
  } else if (exp == space_k) {
    return nfa_compile_given_set(space_cs);
  } else if (exp == digit_k) {
    return nfa_compile_given_set(digit_cs);
  } else if (exp == word_char_k) {
    return nfa_compile_given_set(word_cs);
  } else if (exp == cspace_k) {
    return nfa_compile_given_set(cspace_cs);
  } else if (exp == cdigit_k) {
    return nfa_compile_given_set(cdigit_cs);
  } else if (exp == cword_char_k) {
    return nfa_compile_given_set(cword_cs);
  } else if (consp(exp)) {
    val sym = first(exp), args = rest(exp);

    if (sym == set_s) {
      return nfa_compile_set(args, nil);
    } else if (sym == cset_s) {
      return nfa_compile_set(args, t);
    } else if (sym == compound_s) {
      return nfa_compile_list(args);
    } else if (sym == zeroplus_s) {
      nfa_t nfa_arg = nfa_compile_regex(first(args));
      nfa_state_t *acc = nfa_state_accept();
      /* New start state has empty transitions going through
         the inner NFA, or skipping it right to the new acceptance state. */
      nfa_state_t *s = nfa_state_empty(nfa_arg.start, acc);
      /* Convert acceptance state of inner NFA to one which has
         an empty transition back to the start state, and
         an empty transition to the new acceptance state. */
      nfa_state_empty_convert(nfa_arg.accept, nfa_arg.start, acc);
      return nfa_make(s, acc);
    } else if (sym == oneplus_s) {
      /* One-plus case differs from zero-plus in that the new start state
         does not have an empty transition to the acceptance state.
         So the inner NFA must be traversed once. */
      nfa_t nfa_arg = nfa_compile_regex(first(args));
      nfa_state_t *acc = nfa_state_accept();
      nfa_state_t *s = nfa_state_empty(nfa_arg.start, 0); /* <-- diff */
      nfa_state_empty_convert(nfa_arg.accept, nfa_arg.start, acc);
      return nfa_make(s, acc);
    } else if (sym == optional_s) {
      /* In this case, we can keep the acceptance state of the inner
         NFA as the acceptance state of the new NFA. We simply add
         a new start state which can short-circuit to it via an empty
         transition.  */
      nfa_t nfa_arg = nfa_compile_regex(first(args));
      nfa_state_t *s = nfa_state_empty(nfa_arg.start, nfa_arg.accept);
      return nfa_make(s, nfa_arg.accept);
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
      return nfa_make(s, acc);
    } else {
      uw_throwf(error_s, lit("bad operator in regex syntax: ~s"), sym, nao);
    }
  } else if (exp == t) {
    nfa_state_t *acc = nfa_state_accept();
    nfa_state_t *s = nfa_state_reject();
    return nfa_make(s, acc);
  } else {
    uw_throwf(error_s, lit("bad object in regex syntax: ~s"), exp, nao);
  }
}

static void nfa_map_states(nfa_state_t *s,
                           mem_t *ctx, void (*fun)(nfa_state_t *,
                                                   mem_t *ctx),
                           unsigned visited)
{
  if (s != 0 && s->a.visited != visited) {
    s->a.visited = visited;
    fun(s, ctx);

    switch (s->a.kind) {
    case nfa_accept:
    case nfa_reject:
      break;
    case nfa_empty:
      nfa_map_states(s->e.trans0, ctx, fun, visited);
      nfa_map_states(s->e.trans1, ctx, fun, visited);
      break;
    case nfa_wild:
    case nfa_single:
    case nfa_set:
      nfa_map_states(s->o.trans, ctx, fun, visited);
      break;
    }
  }
}

static void nfa_count_one(nfa_state_t *s, mem_t *ctx)
{
  (void) s;
  int *pcount = coerce(int *, ctx);
  (*pcount)++;
}

static int nfa_count_states(nfa_state_t *s)
{
  int count = 0;
  unsigned visited = s->a.visited + 1;
  nfa_map_states(s, coerce(mem_t *, &count), nfa_count_one, visited);
  return count;
}

static void nfa_handle_wraparound(nfa_state_t *s, unsigned *pvisited)
{
  if (*pvisited == UINT_MAX) {
    s->a.visited = UINT_MAX - 1;
    (void) nfa_count_states(s);
    s->a.visited = UINT_MAX;
    (void) nfa_count_states(s);
    *pvisited = 1;
  }
}

static void nfa_collect_one(nfa_state_t *s, mem_t *ctx)
{
  nfa_state_t ***ppel = coerce(nfa_state_t ***, ctx);
  *(*ppel)++ = s;
}

static void nfa_free(nfa_t nfa, int nstates)
{
  nfa_state_t **all = coerce(nfa_state_t **, alloca(nstates * sizeof *all));
  nfa_state_t **pelem = all, *s = nfa.start;
  unsigned visited = s->a.visited + 1;
  int i;

  /* We don't care if visited has reached UINT_MAX here, because the regex is
   * going away, so we don't bother with nfa_handle_wraparound.
   */
  nfa_map_states(s, coerce(mem_t *, &pelem), nfa_collect_one, visited);

  assert (pelem - all == nstates);

  for (i = 0; i < nstates; i++)
    nfa_state_free(all[i]);
}

/*
 * Compute the epsilon-closure of the NFA states stored in the set, whose
 * size is given by nin. The results are stored back in the same array, the
 * size of which is returned. The stack parameter provides storage used by the
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
static int nfa_closure(nfa_state_t **stack, nfa_state_t **set, int nin,
                       int nstates, unsigned visited, int *accept)
{
  int i, nout = 0;
  int stackp = 0;

  /* First, add all states in the input set to the closure,
     push them on the stack, and mark them as visited. */
  for (i = 0; i < nin; i++) {
    bug_unless (stackp < nstates);

    set[i]->a.visited = visited;
    stack[stackp++] = set[i];
    set[nout++] = set[i];
    if (set[i]->a.kind == nfa_accept)
      *accept = 1;
  }

  while (stackp) {
    nfa_state_t *top = stack[--stackp];

    /* Only states of type nfa_empty are interesting.
       Each such state at most two epsilon transitions. */

    if (top->a.kind == nfa_empty) {
      nfa_state_t *e0 = top->e.trans0;
      nfa_state_t *e1 = top->e.trans1;

      if (e0 && e0->a.visited != visited) {
        e0->a.visited = visited;
        stack[stackp++] = e0;
        set[nout++] = e0;
        if (e0->a.kind == nfa_accept)
          *accept = 1;
      }

      if (e1 && e1->a.visited != visited) {
        e1->a.visited = visited;
        stack[stackp++] = e1;
        set[nout++] = e1;
        if (e1->a.kind == nfa_accept)
          *accept = 1;
      }
    }
  }

  bug_unless (nout <= nstates);

  return nout;
}

/*
 * The nfa_move_closure function combines nfa_move and nfa_closure into a
 * single operation, elminating an intermediate array.
 */
static int nfa_move_closure(nfa_state_t **stack, nfa_state_t **set, int nin,
                            int nstates, wchar_t ch,
                            unsigned visited, int *accept)
{
  int i, nout, stackp;

  /*
   * Compute the move set from a given set of NFA states. The move
   * set is the set of states which are reachable from the set of
   * input states on the consumpion of the input character given by ch.
   */
  for (nout = 0, stackp = 0, i = 0; i < nin; i++) {
    nfa_state_t *s = set[i];

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
       among a common set of leading struct members in the union,
       so we can use s->o.trans. */
    {
      nfa_state_t *mov = s->o.trans;

      bug_unless (stackp < nstates);

      mov->a.visited = visited;
      stack[stackp++] = mov;
      set[nout++] = mov;
      if (mov->a.kind == nfa_accept)
        *accept = 1;
    }
  }

  while (stackp) {
    nfa_state_t *top = stack[--stackp];

    /* Only states of type nfa_empty are interesting.
       Each such state at most two epsilon transitions. */

    if (top->a.kind == nfa_empty) {
      nfa_state_t *e0 = top->e.trans0;
      nfa_state_t *e1 = top->e.trans1;

      if (e0 && e0->a.visited != visited) {
        e0->a.visited = visited;
        stack[stackp++] = e0;
        set[nout++] = e0;
        if (e0->a.kind == nfa_accept)
          *accept = 1;
      }

      if (e1 && e1->a.visited != visited) {
        e1->a.visited = visited;
        stack[stackp++] = e1;
        set[nout++] = e1;
        if (e1->a.kind == nfa_accept)
          *accept = 1;
      }
    }
  }

  bug_unless (nout <= nstates);

  return nout;
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
static cnum nfa_run(nfa_t nfa, int nstates, const wchar_t *str)
{
  const wchar_t *last_accept_pos = 0, *ptr = str;
  unsigned visited = nfa.start->a.visited + 1;
  nfa_state_t **set = coerce(nfa_state_t **, alloca(nstates * sizeof *set));
  nfa_state_t **stack = coerce(nfa_state_t **, alloca(nstates * sizeof *stack));
  int nclos;
  int accept = 0;

  nfa_handle_wraparound(nfa.start, &visited);

  set[0] = nfa.start;

  nclos = nfa_closure(stack, set, 1,
                      nstates, visited++, &accept);

  if (accept)
    last_accept_pos = ptr;

  for (; *ptr != 0; ptr++) {
    wchar_t ch = *ptr;

    accept = 0;

    nfa_handle_wraparound(nfa.start, &visited);

    nclos = nfa_move_closure(stack, set, nclos,
                             nstates, ch, visited++, &accept);

    if (accept)
      last_accept_pos = ptr + 1;

    if (nclos == 0) /* dead end; no match */
      break;
  }

  nfa.start->a.visited = visited;
  return last_accept_pos ? last_accept_pos - str : -1;
}

static cnum regex_machine_match_span(regex_machine_t *regm)
{
  return regm->n.last_accept_pos;
}

static void regex_destroy(val obj)
{
  regex_t *regex = coerce(regex_t *, obj->co.handle);
  if (regex->kind == REGEX_NFA)
    nfa_free(regex->r.nfa, regex->nstates);
  free(regex);
  obj->co.handle = 0;
}

static void regex_mark(val obj)
{
  regex_t *regex = coerce(regex_t *, obj->co.handle);
  if (regex->kind == REGEX_DV)
    gc_mark(regex->r.dv);
  gc_mark(regex->source);
}

static void regex_print(val obj, val stream, val pretty);

static struct cobj_ops regex_obj_ops = cobj_ops_init(eq,
                                                     regex_print,
                                                     regex_destroy,
                                                     regex_mark,
                                                     cobj_hash_op);

static val reg_nullable(val);

static val reg_expand_nongreedy(val exp)
{
  if (atom(exp)) {
    return exp;
  } else if (consp(exp)) {
    val sym = first(exp);
    val args = rest(exp);

    if (sym == set_s || sym == cset_s) {
      return exp;
    } else if (sym == compound_s) {
      list_collect_decl (out, iter);
      iter = list_collect(iter, compound_s);
      for (; args; args = cdr(args))
        iter = list_collect(iter, reg_expand_nongreedy(first(args)));
      return out;
    } else if (sym == zeroplus_s || sym == oneplus_s ||
               sym == optional_s || sym == compl_s) {
      return cons(sym, cons(reg_expand_nongreedy(first(args)), nil));
    } else if (sym == or_s || sym == and_s) {
      val xfirst = reg_expand_nongreedy(first(args));
      val xsecond = reg_expand_nongreedy(second(args));
      return cons(sym, cons(xfirst, cons(xsecond, nil)));
    } else if (sym == nongreedy_s) {
      val xfirst = reg_expand_nongreedy(first(args));
      val xsecond = reg_expand_nongreedy(second(args));
      val zplus = cons(zeroplus_s, cons(xfirst, nil));

      if (xsecond == nil) {
        return zplus;
      } else {
        val any = list(zeroplus_s, wild_s, nao);
        val notempty = list(oneplus_s, wild_s, nao);

        return list(compound_s,
                    list(and_s,
                         zplus,
                         list(compl_s,
                              list(compound_s,
                                   any,
                                   if3(reg_nullable(xsecond),
                                       list(and_s, xsecond, notempty, nao),
                                       xsecond),
                                   any, nao),
                              nao),
                         nao),
                    xsecond, nao);
      }
    } else {
      uw_throwf(error_s, lit("bad operator in regex syntax: ~s"), sym, nao);
    }
  } else {
    uw_throwf(error_s, lit("bad object in regex syntax: ~s"), exp, nao);
  }
}
static val reg_compile_csets(val exp)
{
  if (exp == space_k) {
    return cobj(coerce(mem_t *, space_cs), chset_s, &char_set_obj_ops);
  } else if (exp == digit_k) {
    return cobj(coerce(mem_t *, digit_cs), chset_s, &char_set_obj_ops);
  } else if (exp == word_char_k) {
    return cobj(coerce(mem_t *, word_cs), chset_s, &char_set_obj_ops);
  } else if (exp == cspace_k) {
    return cobj(coerce(mem_t *, cspace_cs), chset_s, &char_set_obj_ops);
  } else if (exp == cdigit_k) {
    return cobj(coerce(mem_t *, cdigit_cs), chset_s, &char_set_obj_ops);
  } else if (exp == cword_char_k) {
    return cobj(coerce(mem_t *, cword_cs), chset_s, &char_set_obj_ops);
  } else if (symbolp(exp) || chrp(exp)) {
    return exp;
  } else if (stringp(exp)) {
    return cons(compound_s, list_str(exp));
  } else if (consp(exp)) {
    val sym = first(exp);
    val args = rest(exp);

    if (sym == set_s || sym == cset_s) {
      char_set_t *set = char_set_compile(args, eq(sym, cset_s));
      return cobj(coerce(mem_t *, set), chset_s, &char_set_obj_ops);
    } else if (sym == compound_s) {
      list_collect_decl (out, iter);
      iter = list_collect(iter, compound_s);
      for (; args; args = cdr(args))
        iter = list_collect(iter, reg_compile_csets(first(args)));
      return out;
    } else if (sym == zeroplus_s || sym == oneplus_s ||
               sym == optional_s || sym == compl_s || sym == nongreedy_s) {
      return cons(sym, cons(reg_compile_csets(first(args)), nil));
    } else if (sym == or_s || sym == and_s) {
      val xfirst = reg_compile_csets(first(args));
      val xsecond = reg_compile_csets(second(args));
      return cons(sym, cons(xfirst, cons(xsecond, nil)));
    } else {
      uw_throwf(error_s, lit("bad operator in regex syntax: ~s"), sym, nao);
    }
  } else {
    uw_throwf(error_s, lit("bad object in regex syntax: ~s"), exp, nao);
  }
}

/*
 * Helper to reg_nullable for recursing over
 * contents of a compound expression.
 */
static val reg_nullable_list(val exp_list)
{
  if (rest(exp_list)) {
    return if2(reg_nullable(first(exp_list)) &&
               reg_nullable_list(rest(exp_list)),
               t);
  } else {
    return reg_nullable(first(exp_list));
  }
}

/*
 * Determine whether the given regular expression is nullable: that is
 * to say, can the regular expression match the empty string?
 */
static val reg_nullable(val exp)
{
  if (exp == nil) {
    return t;
  } else if (atom(exp)) {
    return nil;
  } else {
    val sym = first(exp), args = rest(exp);

    if (sym == set_s || sym == cset_s) {
      return nil;
    } else if (sym == compound_s) {
      return reg_nullable_list(args);
    } else if (sym == oneplus_s) {
      return reg_nullable(first(args));
    } else if (sym == zeroplus_s || sym == optional_s) {
      return t;
    } else if (sym == compl_s) {
      return if3(reg_nullable(first(args)), nil, t);
    } else if (sym == or_s) {
      return if2((reg_nullable(first(args)) || reg_nullable(second(args))), t);
    } else if (sym == and_s) {
      return if2((reg_nullable(first(args)) && reg_nullable(second(args))), t);
    } else {
      uw_throwf(error_s, lit("bad operator in regex syntax: ~s"), sym, nao);
    }
  }
}

static val reg_matches_all(val exp)
{
  if (atom(exp)) {
    return nil;
  } else {
    val sym = first(exp), args = rest(exp);

    if (sym == set_s || sym == cset_s) {
      return nil;
    } else if (sym == compound_s) {
      val am = nil;
      for (; args; args = cdr(args)) {
        val arg = car(args);
        if (!reg_nullable(arg))
          return nil;
        if (!am && reg_matches_all(arg))
          am = t;
      }
      return am;
    } else if (sym == oneplus_s) {
      return reg_matches_all(car(args));
    } else if (sym == zeroplus_s) {
      val arg = car(args);
      if (arg == wild_s || reg_matches_all(arg))
        return t;
      return nil;
    } else if (sym == optional_s) {
      return reg_matches_all(car(args));
    } else if (sym == compl_s) {
      val arg = car(args);
      return if2(arg == t, t);
    } else if (sym == or_s) {
      return tnil(reg_matches_all(pop(&args)) || reg_matches_all(pop(&args)));
    } else if (sym == and_s) {
      return tnil(reg_matches_all(pop(&args)) && reg_matches_all(pop(&args)));
    } else {
      uw_throwf(error_s, lit("bad operator in regex syntax: ~s"), sym, nao);
    }
  }
}

static val flatten_or(val or_expr)
{
  if (atom(or_expr) || car(or_expr) != or_s) {
    return cons(or_expr, nil);
  } else {
    val left = second(or_expr);
    val right = third(or_expr);
    return nappend2(flatten_or(left), flatten_or(right));
  }
}

static val unflatten_or(val exlist)
{
  val f = first(exlist);
  val r = rest(exlist);

  if (r) {
    return cons(or_s, cons(f, cons(unflatten_or(r), nil)));
  } else {
    return f;
  }
}

static val unique_first(val exlist)
{
  val f = first(exlist);
  val r = rest(exlist);

  if (!memqual(f, r))
    return cons(first(exlist), nil);
  return nil;
}

static val reduce_or(val or_expr)
{
  val left = second(or_expr);
  val right = third(or_expr);

  /*
   * Do optimization only if this is an or of two or expressions.
   */

  if (consp(left) && first(left) == or_s &&
      consp(right) && first(right) == or_s)
  {
    val exlist = flatten_or(or_expr);
    val repeats_removed = mapcon(func_n1(unique_first), exlist);
    return unflatten_or(repeats_removed);
  } else {
    return or_expr;
  }
}

static val reg_derivative(val, val);

static val reg_derivative_list(val exp_list, val ch)
{
  if (rest(exp_list)) {
    if (reg_nullable(first(exp_list))) {
      val d_first = reg_derivative(first(exp_list), ch);
      val d_rest = reg_derivative_list(rest(exp_list), ch);

      if (d_rest == t && d_first == t)
        return t;

      if (d_rest == t)
        return if3(d_first == nil,
                   cons(compound_s, rest(exp_list)),
                   cons(compound_s, cons(d_first, rest(exp_list))));

      if (d_first == t)
        return d_rest;

      return list(or_s,
                  if3(d_first == nil,
                      cons(compound_s, rest(exp_list)),
                      cons(compound_s, cons(d_first, rest(exp_list)))),
                  d_rest,
                  nao);
    } else {
      val d_first = reg_derivative(first(exp_list), ch);

      if (d_first == t)
        return t;
      else if (d_first == nil)
        return cons(compound_s, rest(exp_list));
      else
        return cons(compound_s,
                    cons(d_first, rest(exp_list)));
    }
  } else {
    return reg_derivative(first(exp_list), ch);
  }
}

/*
 * Determine derivative of regex with respect to character.
 */
static val reg_derivative(val exp, val ch)
{
  if (exp == nil || exp == t) {
    return t;
  } else if (chrp(exp)) {
    return null(eq(exp, ch));
  } else if (typeof(exp) == chset_s) {
    char_set_t *set = coerce(char_set_t *, exp->co.handle);
    return if3(char_set_contains(set, c_chr(ch)), nil, t);
  } else if (exp == wild_s) {
    return nil;
  } else {
    val sym = first(exp);
    val args = rest(exp);

    if (sym == set_s || sym == cset_s) {
      uw_throwf(error_s, lit("uncompiled regex passed to reg_derivative"));
    } else if (sym == compound_s) {
      return reg_derivative_list(args, ch);
    } else if (sym == optional_s) {
      return reg_derivative(first(args), ch);
    } else if (sym == oneplus_s) {
      val arg = first(args);
      val d_arg = reg_derivative(arg, ch);
      if (d_arg == t)
        return t;
      if (d_arg == nil)
        return cons(zeroplus_s, cons(arg, nil));
      return cons(compound_s, cons(d_arg,
                                   cons(cons(zeroplus_s,
                                             cons(arg, nil)), nil)));
    } else if (sym == zeroplus_s) {
      val arg = first(args);
      val d_arg = reg_derivative(arg, ch);
      if (d_arg == t)
        return t;
      if (d_arg == nil)
        return exp;
      return cons(compound_s, cons(d_arg, cons(exp, nil)));
    } else if (sym == compl_s) {
      val d_arg = reg_derivative(first(args), ch);
      if (reg_matches_all(d_arg))
        return t;
      if (d_arg == nil)
        return cons(oneplus_s, cons(wild_s, nil));
      if (d_arg == t)
        return cons(zeroplus_s, cons(wild_s, nil));
      return cons(sym, cons(d_arg, nil));
    } else if (sym == or_s) {
      val d_arg1 = reg_derivative(first(args), ch);
      val d_arg2 = reg_derivative(second(args), ch);

      if (d_arg1 == t)
        return d_arg2;

      if (d_arg2 == t)
        return d_arg1;

      return reduce_or(cons(or_s, cons(d_arg1, cons(d_arg2, nil))));
    } else if (sym == and_s) {
      val d_arg1 = reg_derivative(first(args), ch);
      val d_arg2 = nil;

      if (d_arg1 == t)
        return t;

      d_arg2 = reg_derivative(second(args), ch);

      if (d_arg2 == t)
        return t;

      return cons(and_s, cons(d_arg1, cons(d_arg2, nil)));
    } else {
      uw_throwf(error_s, lit("bad operator in regex syntax: ~s"), sym, nao);
    }
  }
}

static cnum dv_run(val regex, const wchar_t *str)
{
  const wchar_t *last_accept_pos = 0, *ptr = str;

  for (; *ptr != 0; ptr++) {
    wchar_t ch = *ptr;
    val nullable = reg_nullable(regex);
    val deriv = reg_derivative(regex, chr(ch));

    if (nullable)
      last_accept_pos = ptr;

    if (deriv == t)
      return last_accept_pos ? last_accept_pos - str : -1;
  }

  if (reg_nullable(regex))
    return ptr - str;
  return last_accept_pos ? last_accept_pos - str : -1;
}

static val reg_single_char_p(val exp)
{
  if (chrp(exp))
    return t;
  if (consp(exp) && (car(exp) == set_s || car(exp) == cset_s))
    return t;
  if (exp == space_k || exp == word_char_k || exp == digit_k)
    return t;
  if (exp == cspace_k || exp == cword_char_k || exp == cdigit_k)
    return t;
  return nil;
}

static val reg_compl_char_p(val exp)
{
  if (consp(exp) && car(exp) == cset_s)
    return t;
  if (exp == cspace_k || exp == cword_char_k || exp == cdigit_k)
    return t;
  return nil;
}

static val reg_optimize(val exp);

static val invert_single(val exp)
{
  if (chrp(exp))
    return cons(cset_s, cons(exp, nil));
  if (consp(exp)) {
    if (car(exp) == set_s)
      return cons(cset_s, cdr(exp));
    if (car(exp) == cset_s)
      return reg_optimize(cons(set_s, cdr(exp)));
  }
  if (exp == space_k)
    return cspace_k;
  if (exp == cspace_k)
    return space_k;
  if (exp == word_char_k)
    return cword_char_k;
  if (exp == cword_char_k)
    return word_char_k;
  if (exp == digit_k)
    return cdigit_k;
  if (exp == cdigit_k)
    return digit_k;

  abort();
}

static val reg_optimize(val exp)
{
  if (atom(exp)) {
    return exp;
  } else if (reg_matches_all(exp)) {
    return cons(zeroplus_s, cons(wild_s, nil));
  } else {
    val sym = first(exp);
    val args = rest(exp);

    if (sym == set_s) {
      if (!args)
        return t;
      if (!rest(args) && chrp(first(args)))
        return first(args);
      return exp;
    } else if (sym == cset_s) {
      return if3(rest(exp), exp, wild_s);
    } else if (sym == compound_s) {
      val xargs = mapcar(func_n1(reg_optimize), args);
      while (rest(xargs)) {
        if (reg_matches_all(first(xargs)) &&
            reg_matches_all(second(xargs)))
        {
          pop(&xargs);
          continue;
        }
        break;
      }
      if (!xargs)
        return nil;
      if (!cdr(xargs))
        return car(xargs);
      if (memq(t, xargs))
        return t;
      return cons(sym, xargs);
    } else if (sym == zeroplus_s) {
      val arg = reg_optimize(first(args));
      if (consp(arg)) {
        val arg2 = first(arg);
        if (arg2 == zeroplus_s)
          return arg;
        if (arg2 == oneplus_s || arg2 == optional_s)
          return cons(zeroplus_s, cdr(arg));
      }
      if (reg_matches_all(arg))
        return arg;
      return cons(sym, cons(arg, nil));
    } else if (sym == oneplus_s) {
      val arg = reg_optimize(first(args));
      if (reg_matches_all(arg))
        return cons(zeroplus_s, cons(wild_s, nil));
      if (consp(arg)) {
        val arg2 = first(arg);
        if (arg2 == zeroplus_s || arg2 == oneplus_s)
          return arg;
        if (arg2 == optional_s)
          return cons(zeroplus_s, cdr(arg));
      }
      return cons(sym, cons(arg, nil));
    } else if (sym == optional_s) {
      val arg = reg_optimize(first(args));
      if (reg_matches_all(arg))
        return arg;
      if (consp(arg)) {
        val arg2 = first(arg);
        if (arg2 == zeroplus_s || arg2 == optional_s)
          return arg;
        if (arg2 == oneplus_s)
          return cons(zeroplus_s, cdr(arg));
      }
      return cons(sym, cons(arg, nil));
    } else if (sym == compl_s) {
      val arg_unopt = car(args);

      if (consp(arg_unopt) && car(arg_unopt) == compl_s) {
        return reg_optimize(cadr(arg_unopt));
      } else {
        val arg = reg_optimize(arg_unopt);
        if (reg_matches_all(arg))
          return t;
        if (arg == nil)
          return cons(oneplus_s, cons(wild_s, nil));
        if (arg == t)
          return cons(zeroplus_s, cons(wild_s, nil));
        if (reg_single_char_p(arg))
          return list(or_s,
                      list(optional_s, invert_single(arg), nao),
                      list(compound_s, wild_s,
                           list(oneplus_s, wild_s, nao), nao), nao);
        if (consp(arg)) {
          val sym2 = first(arg);
          if (sym2 == compound_s) {
            val args2 = rest(arg);
            if (cddr(args2) && !cdddr(args2)) {
              if (reg_matches_all(first(args2)) &&
                  reg_single_char_p(second(args2)) &&
                  reg_matches_all(third(args2)))
              {
                return cons(zeroplus_s,
                            cons(invert_single(second(args2)), nil));
              }
            }

            if (cdr(args2) && !cddr(args2)) {
              if (reg_matches_all(first(args2)) &&
                  reg_single_char_p(second(args2)))
              {
                return list(optional_s,
                            list(compound_s,
                                 cons(zeroplus_s, cons(wild_s, nil)),
                                 invert_single(second(args2)), nao), nao);
              }

              if (reg_single_char_p(first(args2)) &&
                  reg_matches_all(second(args2))) {
                return list(optional_s,
                            list(compound_s,
                                 invert_single(first(args2)),
                                 cons(zeroplus_s, cons(wild_s, nil)), nao), nao);
              }
            }
          }
        }
        return cons(sym, cons(arg, nil));
      }
    } else if (sym == or_s) {
      val arg1 = reg_optimize(pop(&args));
      val arg2 = reg_optimize(pop(&args));

      if (arg1 == t || reg_matches_all(arg2))
        return arg2;

      if (arg2 == t || reg_matches_all(arg1))
        return arg1;

      if (arg1 == nil)
        return cons(optional_s, cons(arg2, nil));

      if (arg2 == nil)
        return cons(optional_s, cons(arg1, nil));

      if (reg_single_char_p(arg1) && reg_single_char_p(arg2)) {
        if (!reg_compl_char_p(arg1) && !reg_compl_char_p(arg2)) {
          if (atom(arg1) && atom(arg2))
            return list(set_s, arg1, arg2, nao);
          if (consp(arg1) && car(arg1) == set_s && atom(arg2))
            return cons(set_s, cons(arg2, cdr(arg1)));
          if (consp(arg2) && car(arg2) == set_s && atom(arg1))
            return cons(set_s, cons(arg1, cdr(arg2)));
          if (consp(arg1) && car(arg1) == set_s &&
              consp(arg1) && car(arg1) == set_s)
            return cons(set_s, append2(cdr(arg1), cdr(arg2)));
        }
        if (reg_compl_char_p(arg1) && reg_compl_char_p(arg2)) {
          arg1 = invert_single(arg1);
          arg2 = invert_single(arg2);

          if (atom(arg1) && atom(arg2))
            return list(cset_s, arg1, arg2, nao);
          if (consp(arg1) && car(arg1) == set_s && atom(arg2))
            return cons(cset_s, cons(arg2, cdr(arg1)));
          if (consp(arg2) && car(arg2) == set_s && atom(arg1))
            return cons(cset_s, cons(arg1, cdr(arg2)));
          if (consp(arg1) && car(arg1) == set_s &&
              consp(arg1) && car(arg1) == set_s)
            return cons(cset_s, append2(cdr(arg1), cdr(arg2)));
        }
      }

      return cons(sym, cons(arg1, cons(arg2, nil)));
    } else if (sym == and_s) {
      val arg1 = reg_optimize(pop(&args));
      val arg2 = reg_optimize(pop(&args));

      if (arg1 == t || arg2 == t)
        return t;

      if (arg1 == nil)
        return null(reg_nullable(arg2));

      if (arg2 == nil)
        return null(reg_nullable(arg1));

      if (reg_matches_all(arg1))
        return arg2;

      if (reg_matches_all(arg2))
        return arg2;

      return cons(sym, cons(arg1, cons(arg2, nil)));
    } else {
      uw_throwf(error_s, lit("bad operator in regex syntax: ~s"), sym, nao);
    }
  }
}

static val regex_requires_dv(val exp)
{
  if (atom(exp)) {
    return nil;
  } else {
    val sym = first(exp);
    val args = rest(exp);

    if (sym == set_s || sym == cset_s) {
      return nil;
    } else if (sym == compound_s) {
      return some_satisfy(args, func_n1(regex_requires_dv), nil);
    } else if (sym == zeroplus_s || sym == oneplus_s ||
               sym == optional_s) {
      return regex_requires_dv(first(args));
    } else if (sym == compl_s) {
      return t;
    } else if (sym == or_s) {
      return if2(regex_requires_dv(first(args)) ||
                 regex_requires_dv(second(args)), t);
    } else if (sym == and_s || sym == nongreedy_s) {
      return t;
    } else {
      uw_throwf(error_s, lit("bad operator in regex syntax: ~s"), sym, nao);
    }
  }
}

val regex_compile(val regex_sexp, val error_stream)
{
  if (stringp(regex_sexp)) {
    regex_sexp = regex_parse(regex_sexp, default_bool_arg(error_stream));
    return if2(regex_sexp, regex_compile(regex_sexp, error_stream));
  }

  regex_sexp = reg_optimize(reg_expand_nongreedy(regex_sexp));

  if (opt_derivative_regex || regex_requires_dv(regex_sexp)) {
    regex_t *regex = coerce(regex_t *, chk_malloc(sizeof *regex));
    val ret;
    val dv = reg_compile_csets(regex_sexp);
    regex->kind = REGEX_DV;
    regex->nstates = 0;
    regex->source = nil;
    ret = cobj(coerce(mem_t *, regex), regex_s, &regex_obj_ops);
    regex->r.dv = dv;
    regex->source = regex_sexp;
    return ret;
  } else {
    regex_t *regex = coerce(regex_t *, chk_malloc(sizeof *regex));
    val ret;
    regex->kind = REGEX_NFA;
    regex->source = nil;
    ret = cobj(coerce(mem_t *, regex), regex_s, &regex_obj_ops);
    regex->r.nfa = nfa_compile_regex(regex_sexp);
    regex->nstates = nfa_count_states(regex->r.nfa.start);
    regex->source = regex_sexp;
    return ret;
  }
}

val regexp(val obj)
{
  return typeof(obj) == regex_s ? t : nil;
}

static void puts_clear_flag(val str, val stream, int *semi_flag)
{
  *semi_flag = 0;
    put_string(str, stream);
}

static void putc_clear_flag(val ch, val stream, int *semi_flag)
{
  *semi_flag = 0;
  put_char(ch, stream);
}

static void print_class_char(val ch, val first_p, val stream, int *semi_flag)
{
  wchar_t c = c_chr(ch);
  switch (c) {
  case '^':
    if (!first_p)
      break;
    /* fallthrough */
  case '-': case '[': case ']':
    putc_clear_flag(chr('\\'), stream, semi_flag);
    put_char(ch, stream);
    return;
  }
  out_str_char(c_chr(ch), stream, semi_flag);
}

static void print_rec(val exp, val stream, int *semi_flag);

static void paren_print_rec(val exp, val stream, int *semi_flag)
{
  putc_clear_flag(chr('('), stream, semi_flag);
  print_rec(exp, stream, semi_flag);
  putc_clear_flag(chr(')'), stream, semi_flag);
}

static void print_rec(val exp, val stream, int *semi_flag)
{
  if (exp == space_k) {
    puts_clear_flag(lit("\\s"), stream, semi_flag);
  } else if (exp == digit_k) {
    puts_clear_flag(lit("\\d"), stream, semi_flag);
  } else if (exp == word_char_k) {
    puts_clear_flag(lit("\\w"), stream, semi_flag);
  } else if (exp == cspace_k) {
    puts_clear_flag(lit("\\S"), stream, semi_flag);
  } else if (exp == cdigit_k) {
    puts_clear_flag(lit("\\D"), stream, semi_flag);
  } else if (exp == cword_char_k) {
    puts_clear_flag(lit("\\W"), stream, semi_flag);
  } else if (exp == wild_s) {
    putc_clear_flag(chr('.'), stream, semi_flag);
  } else if (chrp(exp)) {
    wchar_t ch = c_chr(exp);
    switch (ch) {
    case '?': case '.': case '*': case '+':
    case '(': case ')': case '|': case '~':
    case '&': case '%': case '/':
    case '[': case ']': case '\\':
      putc_clear_flag(chr('\\'), stream, semi_flag);
      put_char(exp, stream);
      break;
    default:
      out_str_char(ch, stream, semi_flag);
    }
  } else if (stringp(exp)) {
    cnum i;
    cnum l = c_num(length(exp));
    for (i = 0; i < l; i++)
      print_rec(chr_str(exp, num(i)), stream, semi_flag);
  } else if (consp(exp)) {
    val sym = first(exp);
    val args = rest(exp);

    if (sym == set_s || sym == cset_s) {
      putc_clear_flag(chr('['), stream, semi_flag);

      val first_p = t;

      if (sym == cset_s) {
        put_char(chr('^'), stream);
        first_p = nil;
      }

      while (args) {
        val arg = pop(&args);
        if (consp(arg)) {
          print_class_char(car(arg), first_p, stream, semi_flag);
          putc_clear_flag(chr('-'), stream, semi_flag);
          print_class_char(cdr(arg), nil, stream, semi_flag);
        } else if (symbolp(arg)) {
          print_rec(arg, stream, semi_flag);
        } else {
          print_class_char(arg, first_p, stream, semi_flag);
        }
        first_p = nil;
      }
      putc_clear_flag(chr(']'), stream, semi_flag);
    } else if (sym == compound_s) {
      for (; args; args = cdr(args)) {
        val arg = car(args);
        if (consp(arg) && car(arg) != zeroplus_s && car(arg) != oneplus_s &&
            car(arg) != optional_s && car (arg) != compound_s)
          paren_print_rec(arg, stream, semi_flag);
        else
          print_rec(arg, stream, semi_flag);
      }
    } else if (sym == zeroplus_s || sym == oneplus_s || sym == optional_s) {
      val arg = pop(&args);
      if (consp(arg) && car(arg) != set_s && car(arg) != cset_s)
        paren_print_rec(arg, stream, semi_flag);
      else
        print_rec(arg, stream, semi_flag);
      if (sym == zeroplus_s)
        putc_clear_flag(chr('*'), stream, semi_flag);
      else if (sym == oneplus_s)
        putc_clear_flag(chr('+'), stream, semi_flag);
      else
        putc_clear_flag(chr('?'), stream, semi_flag);
    } else if (sym == compl_s) {
      val arg = pop(&args);
      putc_clear_flag(chr('~'), stream, semi_flag);
      if (consp(arg) && (car(arg) == or_s || car(arg) == and_s))
        paren_print_rec(arg, stream, semi_flag);
      else
        print_rec(arg, stream, semi_flag);
    } else if (sym == and_s) {
      val arg1 = pop(&args);
      val arg2 = pop(&args);
      if (consp(arg1) && car(arg1) == or_s)
        paren_print_rec(arg1, stream, semi_flag);
      else
        print_rec(arg1, stream, semi_flag);
      putc_clear_flag(chr('&'), stream, semi_flag);
      if (consp(arg2) && car(arg2) == or_s)
        paren_print_rec(arg2, stream, semi_flag);
      else
        print_rec(arg2, stream, semi_flag);
    } else if (sym == or_s) {
      print_rec(pop(&args), stream, semi_flag);
      putc_clear_flag(chr('|'), stream, semi_flag);
      print_rec(pop(&args), stream, semi_flag);
    } else if (sym == nongreedy_s) {
      val arg1 = pop(&args);
      val arg2 = pop(&args);
      if (consp(arg1) && car(arg1) != set_s && car(arg1) != cset_s)
        paren_print_rec(arg1, stream, semi_flag);
      else
        print_rec(arg1, stream, semi_flag);
      putc_clear_flag(chr('%'), stream, semi_flag);
      if (consp(arg2) && (car(arg2) == and_s && car(arg2) == or_s))
        paren_print_rec(arg2, stream, semi_flag);
      else
        print_rec(arg2, stream, semi_flag);
    } else {
      uw_throwf(error_s, lit("bad operator in regex syntax: ~s"), sym, nao);
    }
  } else if (exp == t) {
    puts_clear_flag(lit("[]"), stream, semi_flag);
  } else if (exp != nil) {
    uw_throwf(error_s, lit("bad object in regex syntax: ~s"), exp, nao);
  }
}

static void regex_print(val obj, val stream, val pretty)
{
  regex_t *regex = coerce(regex_t *, cobj_handle(obj, regex_s));
  int semi_flag = 0;

  (void) pretty;
  put_string(lit("#/"), stream);
  print_rec(regex->source, stream, &semi_flag);
  put_char(chr('/'), stream);
}

static cnum regex_run(val compiled_regex, const wchar_t *str)
{
  regex_t *regex = coerce(regex_t *, cobj_handle(compiled_regex, regex_s));

  return if3(regex->kind == REGEX_DV,
             dv_run(regex->r.dv, str),
             nfa_run(regex->r.nfa, regex->nstates, str));
}

/*
 * Regex machine: represents the logic of the regex_run function as state
 * machine object which can be fed one character at a time.
 */

static void regex_machine_reset(regex_machine_t *regm)
{
  int accept = 0;

  regm->n.last_accept_pos = -1;
  regm->n.count = 0;

  if (regm->n.is_nfa) {
    nfa_state_t *s = regm->n.nfa.start;

    regm->n.visited = regm->n.nfa.start->a.visited + 1;
    nfa_handle_wraparound(s, &regm->n.visited);

    regm->n.set[0] = regm->n.nfa.start;

    regm->n.nclos = nfa_closure(regm->n.stack, regm->n.set, 1,
                                regm->n.nstates,
                                regm->n.visited++, &accept);

    regm->n.nfa.start->a.visited = regm->n.visited;
  } else {
    regm->d.deriv = regm->d.regex;
    accept = (reg_nullable(regm->d.regex) != nil);
  }

  if (accept)
    regm->n.last_accept_pos = regm->n.count;
}

static void regex_machine_init(regex_machine_t *regm, val reg)
{
  regex_t *regex = coerce(regex_t *, cobj_handle(reg, regex_s));

  if (regex->kind == REGEX_DV) {
    regm->n.is_nfa = 0;
    regm->d.regex = regex->r.dv;
  } else {
    regm->n.is_nfa = 1;
    regm->n.nfa = regex->r.nfa;
    regm->n.nstates = regex->nstates;
    regm->n.visited = 0;
    regm->n.set = coerce(nfa_state_t **,
                          chk_malloc(regex->nstates * sizeof *regm->n.set));
    regm->n.stack = coerce(nfa_state_t **,
                           chk_malloc(regex->nstates * sizeof *regm->n.stack));
  }

  regex_machine_reset(regm);
}

static void regex_machine_cleanup(regex_machine_t *regm)
{
  if (regm->n.is_nfa) {
    free(regm->n.stack);
    free(regm->n.set);
    regm->n.stack = 0;
    regm->n.set = 0;
    regm->n.nfa.start = 0;
    regm->n.nfa.accept = 0;
  }
}

static regm_result_t regex_machine_feed(regex_machine_t *regm, wchar_t ch)
{
  int accept = 0;

  if (regm->n.is_nfa) {
    nfa_handle_wraparound(regm->n.nfa.start, &regm->n.visited);

    if (ch != 0) {
      regm->n.count++;

      regm->n.nclos = nfa_move_closure(regm->n.stack,
                                       regm->n.set, regm->n.nclos,
                                       regm->n.nstates, ch, regm->n.visited++,
                                       &accept);

      regm->n.nfa.start->a.visited = regm->n.visited;

      if (accept) {
        regm->n.last_accept_pos = regm->n.count;
        return REGM_MATCH;
      }

      return (regm->n.nclos != 0) ? REGM_INCOMPLETE : REGM_FAIL;
    }
  } else {
    val accept = nil;

    if (ch != 0) {
      regm->d.count++;
      regm->d.deriv = reg_derivative(regm->d.deriv, chr(ch));

      if ((accept = reg_nullable(regm->d.deriv))) {
        regm->d.last_accept_pos = regm->d.count;
        return REGM_MATCH;
      }

      return (regm->d.deriv != t) ? REGM_INCOMPLETE : REGM_FAIL;
    }
  }

  /* Reached if the null character is
     consumed, or NFA/derivation hit a transition dead end. */

  if (regm->n.last_accept_pos == regm->n.count)
    return REGM_MATCH;
  if (regm->n.last_accept_pos == -1)
    return REGM_FAIL;
  return REGM_INCOMPLETE;
}

val search_regex(val haystack, val needle_regex, val start,
                 val from_end)
{
  start = default_arg(start, zero);
  from_end = default_bool_arg(from_end);

  if (length_str_lt(haystack, start)) {
    return nil;
  } else {
    if (from_end) {
      cnum i;
      cnum s = c_num(start);
      const wchar_t *h = c_str(haystack);

      for (i = c_num(length_str(haystack)) - 1; i >= s; i--) {
        cnum span = regex_run(needle_regex, h + i);
        if (span >= 0)
          return cons(num(i), num(span));
      }

      gc_hint(haystack);
    } else {
      regex_machine_t regm;
      val i, pos = start, retval;
      regm_result_t last_res = REGM_INCOMPLETE;

      regex_machine_init(&regm, needle_regex);

again:
      for (i = pos; length_str_gt(haystack, i); i = plus(i, one)) {
        last_res = regex_machine_feed(&regm, c_chr(chr_str(haystack, i)));

        if (last_res == REGM_FAIL) {
          last_res = regex_machine_feed(&regm, 0);
          if (last_res == REGM_FAIL) {
            regex_machine_reset(&regm);
            pos = plus(pos, one);
            goto again;
          }
          break;
        }
      }

      last_res = regex_machine_feed(&regm, 0);

      switch (last_res) {
      case REGM_INCOMPLETE:
      case REGM_MATCH:
        retval = cons(pos, num(regex_machine_match_span(&regm)));
        regex_machine_cleanup(&regm);
        return retval;
      case REGM_FAIL:
        regex_machine_cleanup(&regm);
        return nil;
      }
    }

    return nil;
  }
}

val range_regex(val haystack, val needle_regex, val start,
                val from_end)
{
  val result = search_regex(haystack, needle_regex, start, from_end);

  if (result) {
    cons_bind (pos, len, result);
    return rcons(pos, plus(pos, len));
  }

  return result;
}

val match_regex(val str, val reg, val pos)
{
  regex_machine_t regm;
  val i, retval;
  regm_result_t last_res = REGM_INCOMPLETE;

  if (null_or_missing_p(pos)) {
    pos = zero;
  } else if (lt(pos, zero)) {
    pos = plus(pos, length_str(str));
    if (lt(pos, zero))
      return nil;
  }

  regex_machine_init(&regm, reg);

  for (i = pos; length_str_gt(str, i); i = plus(i, one)) {
    last_res = regex_machine_feed(&regm, c_chr(chr_str(str, i)));
    if (last_res == REGM_FAIL)
      break;
  }

  last_res = regex_machine_feed(&regm, 0);

  switch (last_res) {
  case REGM_INCOMPLETE:
  case REGM_MATCH:
    retval = plus(pos, num(regex_machine_match_span(&regm)));
    regex_machine_cleanup(&regm);
    return retval;
  case REGM_FAIL:
    regex_machine_cleanup(&regm);
    return nil;
  }

  return nil;
}

val match_regex_len(val str, val regex, val pos)
{
  if (null_or_missing_p(pos)) {
    return match_regex(str, regex, pos);
  } else {
    val new_pos = match_regex(str, regex, pos);
    return if2(new_pos, minus(new_pos, pos));
  }
}

static val match_regex_right_old(val str, val regex, val end)
{
  val pos = zero;
  val slen = length(str);

  if (null_or_missing_p(end) || gt(end, slen))
    end = slen;
  else if (lt(end, zero))
    end = plus(end, slen);

  while (le(pos, end)) {
    cons_bind (from, len, search_regex(str, regex, pos, nil));

    if (!from)
      return nil;

    if (eql(plus(from, len), end))
      return len;

    pos = plus(pos, one);
  }

  return nil;
}

val match_regex_right(val str, val regex, val end)
{
  val pos = zero;
  val len = length(str);

  if (null_or_missing_p(end)) {
    end = len;
  } else if (minusp(end)) {
    end = plus(end, len);
    if (lt(end, zero))
      return nil;
  } else if (gt(end, len)) {
    return nil;
  }

  while (lt(pos, end)) {
    regex_machine_t regm;
    val i ;
    regm_result_t last_res = REGM_INCOMPLETE;

    regex_machine_init(&regm, regex);

    for (i = pos; lt(i, end); i = plus(i, one)) {
      last_res = regex_machine_feed(&regm, c_chr(chr_str(str, i)));
      if (last_res == REGM_FAIL)
        break;
    }

    last_res = regex_machine_feed(&regm, 0);

    switch (last_res) {
    case REGM_MATCH:
      regex_machine_cleanup(&regm);
      return minus(end, pos);
    case REGM_INCOMPLETE:
    case REGM_FAIL:
      regex_machine_cleanup(&regm);
      break;
    }

    pos = succ(pos);
  }

  return nil;
}

val regsub(val regex, val repl, val str)
{
  val isfunc = functionp(repl);

  if (functionp(regex)) {
    val range = funcall1(regex, str);

    if (!range)
      return str;

    {
      val rf = from(range);
      val rt = to(range);

      return replace_str(str, if3(isfunc,
                                  funcall1(repl, sub_str(str, rf, rt)),
                                  repl),
                         rf, rt);
    }
  } else {
    list_collect_decl (out, ptail);
    val pos = zero;

    do {
      cons_bind (find, len, search_regex(str, regex, pos, nil));
      if (!find) {
        if (pos == zero)
          return str;
        ptail = list_collect(ptail, sub_str(str, pos, nil));
        break;
      }
      ptail = list_collect(ptail, sub_str(str, pos, find));
      ptail = list_collect(ptail, if3(isfunc,
                                      funcall1(repl, sub_str(str, find,
                                                             plus(find, len))),
                                      repl));
      if (len == zero && eql(find, pos)) {
        if (lt(pos, length_str(str))) {
          ptail = list_collect(ptail, chr_str(str, pos));
          pos = plus(pos, one);
        }
      } else {
        pos = plus(find, len);
      }
    } while (lt(pos, length_str(str)));

    return cat_str(out, nil);
  }
}

val search_regst(val haystack, val needle_regex, val start_num, val from_end)
{
  val range = range_regex(haystack, needle_regex, start_num, from_end);
  return if2(range, sub_str(haystack, from(range), to(range)));
}

val match_regst(val str, val regex, val pos_in)
{
  val pos = default_arg(pos_in, zero);
  val new_pos = match_regex(str, regex, pos);
  return if2(new_pos, sub_str(str, pos, new_pos));
}

static val match_regst_right_old(val str, val regex, val end)
{
  val len = match_regex_right_old(str, regex, end);
  return if2(len, if3(null_or_missing_p(end),
                      sub_str(str, neg(len), t),
                      sub_str(str, minus(end, len), end)));
}

val match_regst_right(val str, val regex, val end)
{
  val len = match_regex_right(str, regex, end);
  return if2(len, if3(null_or_missing_p(end),
                      sub_str(str, neg(len), t),
                      sub_str(str, minus(end, len), end)));
}
static val do_match_full(val regex, val str)
{
   return if2(eql(match_regex(str, regex, zero), length_str(str)), str);
}

static val do_match_full_offs(val env, val str)
{
  cons_bind (regex, pos_in, env);
  val len = length_str(str);
  val pos = if3(minusp(pos_in), plus(pos_in, len), pos_in);
  return if2(eql(match_regex(str, regex, pos), len),
             sub_str(str, pos, t));
}

val regex_match_full_fun(val regex, val pos)
{
  if (null_or_missing_p(pos))
    return func_f1(regex, do_match_full);
  return func_f1(cons(regex, pos), do_match_full_offs);
}

static val do_match_left(val regex, val str)
{
  return match_regst(str, regex, zero);
}

static val do_match_left_offs(val env, val str)
{
  cons_bind (regex, pos, env);
  return match_regst(str, regex, pos);
}

val regex_match_left_fun(val regex, val pos)
{
  if (null_or_missing_p(pos))
    return func_f1(regex, do_match_left);
  return func_f1(cons(regex, pos), do_match_left_offs);
}

static val do_match_right(val regex, val str)
{
  return match_regst_right(str, regex, nil);
}

static val do_match_right_offs(val env, val str)
{
  cons_bind (regex, end, env);
  return match_regst_right(str, regex, end);
}

val regex_match_right_fun(val regex, val end)
{
  if (null_or_missing_p(end))
    return func_f1(regex, do_match_right);
  return func_f1(cons(regex, end), do_match_right_offs);
}

val regex_match_full(val regex, val arg1, val arg2)
{
  if (null_or_missing_p(arg2)) {
    val str = arg1;
    return if2(eql(match_regex(arg1, regex, arg2), length_str(str)), str);
  } else {
    val str = arg2;
    val len = length_str(str);
    val pos = if3(minusp(arg1), plus(len, arg1), arg1);
    return if2(eql(match_regex(str, regex, pos), len), sub_str(str, pos, t));
  }
}

val regex_match_left(val regex, val arg1, val arg2)
{
  if (null_or_missing_p(arg2))
    return match_regst(arg1, regex, arg2);
  return match_regst(arg2, regex, arg1);
}

val regex_match_right(val regex, val arg1, val arg2)
{
  if (null_or_missing_p(arg2))
    return match_regst_right(arg1, regex, arg2);
  return match_regst_right(arg2, regex, arg1);
}

val regex_range_full(val regex, val arg1, val arg2)
{
  if (null_or_missing_p(arg2)) {
    val str = arg1;
    val len = length_str(str);
    return if2(eql(match_regex(str, regex, zero), len), rcons(zero, len));
  } else {
    val str = arg2;
    val len = length_str(str);
    val pos = if3(minusp(arg1), plus(len, arg1), arg1);
    return if2(eql(match_regex(str, regex, pos), len), rcons(pos, len));
  }
}

val regex_range_left(val regex, val arg1, val arg2)
{
  if (null_or_missing_p(arg2)) {
    val len = match_regex(arg1, regex, arg2);
    return if2(len, rcons(zero, len));
  } else {
    val pos = if3(lt(arg1, zero), plus(arg1, length_str(arg2)), arg1);
    val new_pos = match_regex(arg2, regex, pos);
    return if2(new_pos, rcons(pos, new_pos));
  }
}

val regex_range_right(val regex, val arg1, val arg2)
{
  if (null_or_missing_p(arg2)) {
    val len = match_regex_right(arg1, regex, arg2);
    return if2(len, rcons(zero, len));
  } else {
    val end = if3(lt(arg1, zero), plus(arg1, length_str(arg2)), arg1);
    val len = match_regex_right(arg2, regex, end);
    return if2(len, rcons(minus(end, len), end));
  }
}

val read_until_match(val regex, val stream_in, val include_match_in)
{
  regex_machine_t regm;
  val out = nil;
  val stack = nil;
  val match = nil;
  val stream = default_arg(stream_in, std_input);
  val include_match = default_bool_arg(include_match_in);

  regex_machine_init(&regm, regex);

  for (;;) {
    val ch = get_char(stream);

    if (!ch) {
      switch (regex_machine_feed(&regm, 0)) {
      case REGM_FAIL:
      case REGM_INCOMPLETE:
        if (match)
          goto out_match;
        break;
      case REGM_MATCH:
        goto out_match;
      }
      break;
    }

    switch (regex_machine_feed(&regm, c_chr(ch))) {
    case REGM_FAIL:
      unget_char(ch, stream);

      if (match)
        goto out_match;

      while (stack)
        unget_char(rcyc_pop(&stack), stream);

      ch = get_char(stream);

      if (!out)
        out = mkstring(one, ch);
      else
        string_extend(out, ch);

      regex_machine_reset(&regm);
      continue;
    case REGM_MATCH:
      push(ch, &stack);
      match = stack;
      continue;
    case REGM_INCOMPLETE:
      push(ch, &stack);
      continue;
    }

    break;
  }

  if (nil) {
out_match:
    while (stack && stack != match)
      unget_char(rcyc_pop(&stack), stream);
    if (!out)
      out = null_string;
    if (include_match)
      out = cat_str(cons(out, stack = nreverse(stack)), nil);
  }

  regex_machine_cleanup(&regm);

  while (stack)
    rcyc_pop(&stack);

  return out;
}

static char_set_t *create_wide_cs(void)
{
#ifdef FULL_UNICODE
  chset_type_t cst = CHSET_XLARGE;
#else
  chset_type_t cst = CHSET_LARGE;
#endif

  char_set_t *cs = char_set_create(cst, 0, 1);

  char_set_add_range(cs, 0x1100, 0x115F);
  char_set_add_range(cs, 0x2329, 0x232A);
  char_set_add_range(cs, 0x2E80, 0x2E99);
  char_set_add_range(cs, 0x2E9B, 0x2EF3);
  char_set_add_range(cs, 0x2F00, 0x2FD5);
  char_set_add_range(cs, 0x2FF0, 0x2FFB);
  char_set_add_range(cs, 0x3000, 0x303E);
  char_set_add_range(cs, 0x3000, 0x303E);
  char_set_add_range(cs, 0x3041, 0x3096);
  char_set_add_range(cs, 0x3099, 0x30FF);
  char_set_add_range(cs, 0x3105, 0x312D);
  char_set_add_range(cs, 0x3131, 0x318E);
  char_set_add_range(cs, 0x3190, 0x31BA);
  char_set_add_range(cs, 0x31C0, 0x31E3);
  char_set_add_range(cs, 0x31F0, 0x321E);
  char_set_add_range(cs, 0x3220, 0x3247);
  char_set_add_range(cs, 0x3250, 0x32FE);
  char_set_add_range(cs, 0x3300, 0x4DB5);
  char_set_add_range(cs, 0x4E00, 0x9FFF);
  char_set_add_range(cs, 0xA000, 0xA48C);
  char_set_add_range(cs, 0xA490, 0xA4C6);
  char_set_add_range(cs, 0xA960, 0xA97C);
  char_set_add_range(cs, 0xAC00, 0xD7A3);
  char_set_add_range(cs, 0xF900, 0xFAFF);
  char_set_add_range(cs, 0xFE10, 0xFE19);
  char_set_add_range(cs, 0xFE30, 0xFE52);
  char_set_add_range(cs, 0xFE54, 0xFE6B);
  char_set_add_range(cs, 0xFF01, 0xFF60);
  char_set_add_range(cs, 0xFFE0, 0xFFE6);

#ifdef FULL_UNICODE
  char_set_add_range(cs, 0x1B000, 0x1B001);
  char_set_add_range(cs, 0x1F200, 0x1F202);
  char_set_add_range(cs, 0x1F210, 0x1F23A);
  char_set_add_range(cs, 0x1F240, 0x1F248);
  char_set_add_range(cs, 0x1F250, 0x1F251);
  char_set_add_range(cs, 0x20000, 0x2FFFD);
  char_set_add_range(cs, 0x30000, 0x3FFFD);
#endif

  return cs;
}

static char_set_t *wide_cs;

int wide_display_char_p(wchar_t ch)
{
  if (ch < 0x1100)
    return 0;

  if (!wide_cs)
    wide_cs = create_wide_cs();

  return char_set_contains(wide_cs, ch);
}

val space_k, digit_k, word_char_k;
val cspace_k, cdigit_k, cword_char_k;

void regex_init(void)
{
  space_k = intern(lit("space"), keyword_package);
  digit_k = intern(lit("digit"), keyword_package);
  word_char_k = intern(lit("word-char"), keyword_package);
  cspace_k = intern(lit("cspace"), keyword_package);
  cdigit_k = intern(lit("cdigit"), keyword_package);
  cword_char_k = intern(lit("cword-char"), keyword_package);

  reg_fun(intern(lit("regex-compile"), user_package), func_n2o(regex_compile, 1));
  reg_fun(intern(lit("regexp"), user_package), func_n1(regexp));
  reg_fun(intern(lit("search-regex"), user_package), func_n4o(search_regex, 2));
  reg_fun(intern(lit("range-regex"), user_package), func_n4o(range_regex, 2));
  reg_fun(intern(lit("search-regst"), user_package), func_n4o(search_regst, 2));
  reg_fun(intern(lit("match-regex"), user_package),
          func_n3o((opt_compat && opt_compat <= 150) ?
                   match_regex : match_regex_len, 2));
  reg_fun(intern(lit("match-regst"), user_package), func_n3o(match_regst, 2));
  reg_fun(intern(lit("match-regex-right"), user_package),
          func_n3o((opt_compat && opt_compat <= 150) ?
                   match_regex_right_old : match_regex_right, 2));
  reg_fun(intern(lit("match-regst-right"), user_package),
          func_n3o((opt_compat && opt_compat <= 150) ?
                   match_regst_right_old : match_regst_right, 2));
  reg_fun(intern(lit("regsub"), user_package), func_n3(regsub));
  reg_fun(intern(lit("regex-parse"), user_package), func_n2o(regex_parse, 1));

  reg_fun(intern(lit("reg-expand-nongreedy"), system_package),
          func_n1(reg_expand_nongreedy));
  reg_fun(intern(lit("reg-optimize"), system_package), func_n1(reg_optimize));
  reg_fun(intern(lit("read-until-match"), user_package), func_n3o(read_until_match, 1));
  reg_fun(intern(lit("f^$"), user_package), func_n2o(regex_match_full_fun, 1));
  reg_fun(intern(lit("f^"), user_package), func_n2o(regex_match_left_fun, 1));
  reg_fun(intern(lit("f$"), user_package), func_n2o(regex_match_right_fun, 1));
  reg_fun(intern(lit("m^$"), user_package), func_n3o(regex_match_full, 2));
  reg_fun(intern(lit("m^"), user_package), func_n3o(regex_match_left, 2));
  reg_fun(intern(lit("m$"), user_package), func_n3o(regex_match_right, 2));
  reg_fun(intern(lit("r^$"), user_package), func_n3o(regex_range_full, 2));
  reg_fun(intern(lit("r^"), user_package), func_n3o(regex_range_left, 2));
  reg_fun(intern(lit("r$"), user_package), func_n3o(regex_range_right, 2));
  init_special_char_sets();
}

void regex_free_all(void)
{
  char_set_destroy(space_cs);
  char_set_destroy(digit_cs);
  char_set_destroy(word_cs);
  char_set_destroy(cspace_cs);
  char_set_destroy(cdigit_cs);
  char_set_destroy(cword_cs);
  char_set_destroy(wide_cs);
}
