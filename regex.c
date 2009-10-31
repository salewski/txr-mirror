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
#include <assert.h>
#include <dirent.h>
#include <setjmp.h>
#include <dirent.h>
#include "lib.h"
#include "unwind.h"
#include "regex.h"

#define NFA_SET_SIZE 512

#define CHAR_SET_INDEX(CH) ((CH) / (sizeof (bitcell_t) * CHAR_BIT))
#define CHAR_SET_BIT(CH) ((CH) % (sizeof (bitcell_t) * CHAR_BIT))

void char_set_clear(char_set_t *set)
{
  static const char_set_t blank = { { 0 } };
  *set = blank;
}

void char_set_compl(char_set_t *set)
{
  int i;
  for (i = 0; i < CHAR_SET_SIZE; i++)
    set->bitcell[i] ^= BITCELL_ALL1;
}

void char_set_add(char_set_t *set, int ch)
{
  set->bitcell[CHAR_SET_INDEX(ch)] |= (1 << CHAR_SET_BIT(ch));
}

void char_set_add_range(char_set_t *set, int ch0, int ch1)
{
  if (ch0 <= ch1) {
    int i;
    int bt0 = CHAR_SET_BIT(ch0);
    int bc0 = CHAR_SET_INDEX(ch0);
    bitcell_t mask0 = ~((BITCELL_LIT(1) << bt0) - 1);
    int bt1 = CHAR_SET_BIT(ch1);
    int bc1 = CHAR_SET_INDEX(ch1);
    bitcell_t mask1 = ((BITCELL_LIT(1) << (bt1 + 1) % 32) - 1);

    switch (bc1 - bc0) {
    case 0:
      set->bitcell[bc0] |= (mask0 & mask1);
      break;
    default:
      set->bitcell[bc0] |= mask0;
      set->bitcell[bc1] |= mask1;
    case 1:
      for (i = bc0 + 1; i < bc1; i++)
        set->bitcell[i] = BITCELL_ALL1;
      break;
    }
  }
}

int char_set_contains(char_set_t *set, int ch)
{
  return (set->bitcell[CHAR_SET_INDEX(ch)] & (1 << CHAR_SET_BIT(ch))) != 0;
}

nfa_state_t *nfa_state_accept(void)
{
  nfa_state_t *st = (nfa_state_t *) chk_malloc(sizeof *st);
  st->a.kind = nfa_accept;
  st->a.visited = 0;
  return st;
}

nfa_state_t *nfa_state_empty(nfa_state_t *t0, nfa_state_t *t1)
{
  nfa_state_t *st = (nfa_state_t *) chk_malloc(sizeof *st);
  st->e.kind = nfa_empty;
  st->e.visited = 0;
  st->e.trans0 = t0;
  st->e.trans1 = t1;
  return st;
}

nfa_state_t *nfa_state_single(nfa_state_t *t, int ch)
{
  nfa_state_t *st = (nfa_state_t *) chk_malloc(sizeof *st);
  st->o.kind = nfa_single;
  st->o.visited = 0;
  st->o.trans = t;
  st->o.ch = ch;
  return st;
}

nfa_state_t *nfa_state_wild(nfa_state_t *t)
{
  nfa_state_t *st = (nfa_state_t *) chk_malloc(sizeof *st);
  st->o.kind = nfa_wild;
  st->o.visited = 0;
  st->o.trans = t;
  st->o.ch = 0;
  return st;
}

void nfa_state_free(nfa_state_t *st)
{
  if (st->a.kind == nfa_set)
    free(st->s.set);
  free(st);
}

void nfa_state_shallow_free(nfa_state_t *st)
{
  free(st);
}

nfa_state_t *nfa_state_set(nfa_state_t *t)
{
  nfa_state_t *st = (nfa_state_t *) chk_malloc(sizeof *st);
  char_set_t *cs = (char_set_t *) chk_malloc(sizeof *cs);
  char_set_clear(cs);
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
void nfa_state_empty_convert(nfa_state_t *acc, nfa_state_t *t0, nfa_state_t *t1)
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
void nfa_state_merge(nfa_state_t *acc, nfa_state_t *st)
{
  assert (acc->a.kind == nfa_accept);
  *acc = *st;
}

nfa_t nfa_make(nfa_state_t *s, nfa_state_t *acc)
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
nfa_t nfa_combine(nfa_t pred, nfa_t succ)
{
  nfa_t ret;
  ret.start = pred.start;
  ret.accept = succ.accept;
  nfa_state_merge(pred.accept, succ.start);
  nfa_state_shallow_free(succ.start); /* No longer needed. */
  return ret;
}

nfa_t nfa_compile_set(obj_t *args, int compl)
{
  nfa_state_t *acc = nfa_state_accept();
  nfa_state_t *s = nfa_state_set(acc);
  char_set_t *set = s->s.set;
  nfa_t ret = nfa_make(s, acc);

  for (; args; args = rest(args)) {
    obj_t *item = first(args);

    if (consp(item)) {
      obj_t *from = car(item);
      obj_t *to = cdr(item);

      assert (typeof(from) == chr_t && typeof(to) == chr_t);
      char_set_add_range(set, c_chr(from), c_chr(to));
    } else if (typeof(item) == chr_t) {
      char_set_add(set, c_chr(item));
    } else {
      assert(0 && "bad regex set");
    }
  }

  if (compl)
    char_set_compl(set);

  return ret;
}

/*
 * Input is the items from a regex form,
 * not including the regex symbol.
 * I.e.  (rest '(regex ...)) not '(regex ...).
 */
nfa_t nfa_compile_regex(obj_t *items)
{
  if (nullp(items)) {
    nfa_state_t *acc = nfa_state_accept();
    nfa_state_t *s = nfa_state_empty(acc, 0);
    nfa_t nfa = nfa_make(s, acc);
    return nfa;
  } else {
    obj_t *item = first(items), *others = rest(items);
    nfa_t nfa;

    if (typeof(item) == chr_t) {
      nfa_state_t *acc = nfa_state_accept();
      nfa_state_t *s = nfa_state_single(acc, c_chr(item));
      nfa = nfa_make(s, acc);
    } else if (item == wild) {
      nfa_state_t *acc = nfa_state_accept();
      nfa_state_t *s = nfa_state_wild(acc);
      nfa = nfa_make(s, acc);
    } else if (consp(item)) {
      obj_t *sym = first(item);
      obj_t *args = rest(item);

      if (sym == set) {
        nfa = nfa_compile_set(args, 0);
      } else if (sym == cset) {
        nfa = nfa_compile_set(args, 1);
      } else if (sym == compound) {
        nfa = nfa_compile_regex(args);
      } else if (sym == zeroplus) {
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
      } else if (sym == oneplus) {
        /* One-plus case differs from zero-plus in that the new start state
           does not have an empty transition to the acceptance state.
           So the inner NFA must be traversed once. */
        nfa_t nfa_args = nfa_compile_regex(args);
        nfa_state_t *acc = nfa_state_accept();
        nfa_state_t *s = nfa_state_empty(nfa_args.start, 0); /* <-- diff */
        nfa_state_empty_convert(nfa_args.accept, nfa_args.start, acc);
        nfa = nfa_make(s, acc);
      } else if (sym == optional) {
        /* In this case, we can keep the acceptance state of the inner
           NFA as the acceptance state of the new NFA. We simply add
           a new start state which can short-circuit to it via an empty
           transition.  */
        nfa_t nfa_args = nfa_compile_regex(args);
        nfa_state_t *s = nfa_state_empty(nfa_args.start, nfa_args.accept);
        nfa = nfa_make(s, nfa_args.accept);
      } else if (sym == or) {
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

int nfa_all_states(nfa_state_t **inout, int num, int visited)
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
  nfa_state_t **all = chk_malloc(NFA_SET_SIZE * sizeof *all);
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
int nfa_closure(nfa_state_t **stack, nfa_state_t **in, int nin, 
                nfa_state_t **out, int visited, int *accept)
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
int nfa_move(nfa_state_t **in, int nin, nfa_state_t **out, int ch)
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
long nfa_run(nfa_t nfa, const char *str)
{
  const char *last_accept_pos = 0, *ptr = str;
  unsigned visited = nfa.start->a.visited + 1;
  nfa_state_t **move = chk_malloc(NFA_SET_SIZE * sizeof *move);
  nfa_state_t **clos = chk_malloc(NFA_SET_SIZE * sizeof *clos);
  nfa_state_t **stack = chk_malloc(NFA_SET_SIZE * sizeof *stack);
  int nmove = 1, nclos;
  int accept = 0;

  move[0] = nfa.start;

  nclos = nfa_closure(stack, move, nmove, clos, visited++, &accept);

  if (accept)
    last_accept_pos = ptr;

  for (; *ptr != 0; ptr++) {
    int ch = *ptr;

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

static obj_t *regex_equal(obj_t *self, obj_t *other)
{
  return self == other ? t : nil; /* eq equality only */
}

static void regex_destroy(obj_t *regex)
{
  nfa_t *pnfa = (nfa_t *) regex->co.handle;
  nfa_free(*pnfa);
  free(pnfa);
  regex->co.handle = 0;
}

static struct cobj_ops regex_obj_ops = {
  regex_equal, cobj_print_op, regex_destroy, 0,
};

obj_t *regex_compile(obj_t *regex_sexp)
{
  nfa_t *pnfa = chk_malloc(sizeof *pnfa);
  *pnfa = nfa_compile_regex(regex_sexp);
  return cobj(pnfa, regex, &regex_obj_ops);
}

obj_t *regexp(obj_t *obj)
{
  return (obj->co.type == COBJ && obj->co.cls == regex) ? t : nil;
}

nfa_t *regex_nfa(obj_t *reg)
{
  assert (reg->co.type == COBJ && reg->co.cls == regex);
  return (nfa_t *) reg->co.handle;
}

obj_t *search_regex(obj_t *haystack, obj_t *needle_regex, obj_t *start_num,
                    obj_t *from_end)
{
  const char *h = c_str(haystack);
  long len = c_num(length_str(haystack));
  long start = c_num(start_num);
  nfa_t *pnfa = regex_nfa(needle_regex);

  if (start > len) {
    return nil;
  } else {
    long begin = from_end ? len : start;
    long end = from_end ? start - 1 : len + 1;
    int incr = from_end ? -1 : 1;
    long i;

    for (i = begin; i != end; i += incr) {
      long span = nfa_run(*pnfa, h + i);
      if (span >= 0)
        return cons(num(i), num(span));
    }

    return nil;
  }
}

obj_t *match_regex(obj_t *str, obj_t *reg, obj_t *pos)
{
  nfa_t *pnfa = regex_nfa(reg);
  long cpos = c_num(pos);
  long span = nfa_run(*pnfa, c_str(str) + cpos);
  return span >= 0 ? num(span + cpos) : nil;
}
