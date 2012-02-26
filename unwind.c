/* Copyright 2012
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <setjmp.h>
#include <dirent.h>
#include <stdarg.h>
#include "config.h"
#include "lib.h"
#include "gc.h"
#include "stream.h"
#include "txr.h"
#include "unwind.h"

static uw_frame_t *uw_stack;
static uw_frame_t *uw_env_stack;
static uw_frame_t *uw_exit_point;
static uw_frame_t toplevel_env;

static void uw_unwind_to_exit_point(void)
{
  assert (uw_exit_point);

  for (; uw_stack && uw_stack != uw_exit_point; uw_stack = uw_stack->uw.up) {
    switch (uw_stack->uw.type) {
    case UW_CATCH:
      /* If a catch block is not visible, do
         not run its unwind stuff. This
         would cause infinite loops if
         unwind blocks trigger a nonlocal exit. */
      if (!uw_stack->ca.visible)
        continue;
      /* Catches catch everything, so that they
         can do "finally" or "unwind protect" logic.
         If a catch is invoked with a nil exception
         and symbol, it must excecute the
         mandatory clean-up code and then
         continue the unwinding by calling uw_continue,
         passing it the ca.cont value. */
      uw_stack->ca.sym = nil;
      uw_stack->ca.exception = nil;
      uw_stack->ca.cont = uw_exit_point;
      /* 1 means unwind only. */
      longjmp(uw_stack->ca.jb, 1);
      abort();
    case UW_ENV:
      /* Maintain consistency of unwind stack pointer */
      uw_env_stack = uw_env_stack->ev.up_env;
      break;
    default:
      break;
    }
  }

  if (!uw_stack)
    abort();

  uw_exit_point = 0;

  switch (uw_stack->uw.type) {
  case UW_BLOCK:
    longjmp(uw_stack->bl.jb, 1);
    abort();
  case UW_ENV: /* env frame cannot be exit point */
    abort();
  case UW_CATCH:
    /* 2 means actual catch, not just unwind */
    longjmp(uw_stack->ca.jb, 2);
  default:
    abort();
  }
}

void uw_push_block(uw_frame_t *fr, val tag)
{
  memset(fr, 0, sizeof *fr);
  fr->bl.type = UW_BLOCK;
  fr->bl.tag = tag;
  fr->bl.result = nil;
  fr->bl.up = uw_stack;
  uw_stack = fr;
}

static uw_frame_t *uw_find_env(void)
{
  return uw_env_stack ? uw_env_stack : &toplevel_env;
}

void uw_push_env(uw_frame_t *fr)
{
  uw_frame_t *prev_env = uw_find_env();
  memset(fr, 0, sizeof *fr);
  fr->ev.type = UW_ENV;
  fr->ev.up_env = prev_env;
  fr->ev.func_bindings = nil;
  fr->ev.match_context = nil;
  fr->ev.up = uw_stack;
  uw_stack = fr;
  uw_env_stack = fr;
}

val uw_get_func(val sym)
{
  uw_frame_t *env;

  for (env = uw_find_env(); env != 0; env = env->ev.up_env) {
    if (env->ev.func_bindings) {
      val found = assoc(sym, env->ev.func_bindings);
      if (found)
        return cdr(found);
    }
  }

  return nil;
}

val uw_set_func(val sym, val value)
{
  uw_frame_t *env = uw_find_env();
  env->ev.func_bindings = acons_new(sym, value, env->ev.func_bindings);
  return value;
}

val uw_get_match_context(void)
{
  uw_frame_t *env = uw_find_env();
  return env->ev.match_context;
}

val uw_set_match_context(val context)
{
  uw_frame_t *env = uw_find_env();
  env->ev.match_context = context;
  return context;
}

void uw_push_debug(uw_frame_t *fr, val func, val args,
                   val ub_p_a_pairs, val env, val data,
                   val line, val chr)
{
  memset(fr, 0, sizeof *fr);
  fr->db.type = UW_DBG;
  fr->db.func = func;
  fr->db.args = args;
  fr->db.ub_p_a_pairs = ub_p_a_pairs;
  fr->db.env = env;
  fr->db.data = data;
  fr->db.line = line;
  fr->db.chr = chr;
  fr->db.up = uw_stack;
  uw_stack = fr;
}

void uw_pop_frame(uw_frame_t *fr)
{
  assert (fr == uw_stack);
  uw_stack = fr->uw.up;
  if (fr->uw.type == UW_ENV) {
    assert (fr == uw_env_stack);
    uw_env_stack = fr->ev.up_env;
  }
}

uw_frame_t *uw_current_frame(void)
{
  return uw_stack;
}

val uw_block_return(val tag, val result)
{
  uw_frame_t *ex;

  for (ex = uw_stack; ex != 0; ex = ex->uw.up) {
    if (ex->uw.type == UW_BLOCK && ex->bl.tag == tag)
      break;
  }

  if (ex == 0)
    return nil;

  ex->bl.result = result;
  uw_exit_point = ex;
  uw_unwind_to_exit_point();
  abort();
}

void uw_push_catch(uw_frame_t *fr, val matches)
{
  memset(fr, 0, sizeof *fr);
  fr->ca.type = UW_CATCH;
  fr->ca.matches = matches;
  fr->ca.exception = nil;
  fr->ca.cont = 0;
  fr->ca.visible = 1;
  fr->ca.up = uw_stack;
  uw_stack = fr;
}

static val exception_subtypes;

val uw_exception_subtype_p(val sub, val sup)
{
  if (sub == nil || sup == t || sub == sup) {
    return t;
  } else {
    val entry = assoc(sub, exception_subtypes);
    return memq(sup, entry) ? t : nil;
  }
}

val uw_throw(val sym, val exception)
{
  uw_frame_t *ex;

  for (ex = uw_stack; ex != 0; ex = ex->uw.up) {
    if (ex->uw.type == UW_CATCH && ex->ca.visible) {
      /* The some_satisfy would require us to
         cons up a function; we want to
         avoid consing in exception handling, if we can. */
      val matches = ex->ca.matches;
      val match;
      for (match = matches; match; match = cdr(match))
        if (uw_exception_subtype_p(sym, car(match)))
          break;
      if (match)
        break;
    }
  }

  if (ex == 0) {
    if (std_error == 0) {
      fprintf(stderr, "unhandled exception in early initialization\n");
      abort();
    }

    if (opt_loglevel >= 1) {
      val s = stringp(exception);
      format(std_error, lit("~a: unhandled exception of type ~a:\n"),
             prog_string, sym, nao);
      format(std_error, s ? lit("~a: ~a\n") : lit("~a: ~s\n"),
             prog_string, exception, nao);
    }
    if (uw_exception_subtype_p(sym, query_error_s) ||
        uw_exception_subtype_p(sym, file_error_s)) {
      if (!output_produced)
        put_line(lit("false"), std_output);
      exit(EXIT_FAILURE);
    }
    abort();
  }

  ex->ca.sym = sym;
  ex->ca.exception = exception;
  uw_exit_point = ex;
  uw_unwind_to_exit_point();
  abort();
}

val uw_throwf(val sym, val fmt, ...)
{
  va_list vl;
  val stream = make_string_output_stream();

  va_start (vl, fmt);
  (void) vformat(stream, fmt, vl);
  va_end (vl);

  uw_throw(sym, get_string_from_stream(stream));
  abort();
}

val uw_throwfv(val sym, val fmt, val args)
{
  val stream = make_string_output_stream();
  (void) formatv(stream, fmt, args);
  uw_throw(sym, get_string_from_stream(stream));
  abort();
}

val uw_errorf(val fmt, ...)
{
  va_list vl;
  val stream = make_string_output_stream();

  va_start (vl, fmt);
  (void) vformat(stream, fmt, vl);
  va_end (vl);

  uw_throw(error_s, get_string_from_stream(stream));
  abort();
}

val uw_errorfv(val fmt, val args)
{
  val stream = make_string_output_stream();
  (void) formatv(stream, fmt, args);
  uw_throw(error_s, get_string_from_stream(stream));
  abort();
}

val type_mismatch(val fmt, ...)
{
  va_list vl;
  val stream = make_string_output_stream();

  va_start (vl, fmt);
  (void) vformat(stream, fmt, vl);
  va_end (vl);

  uw_throw(type_error_s, get_string_from_stream(stream));
  abort();
}

val uw_register_subtype(val sub, val sup)
{
  val t_entry = assoc(t, exception_subtypes);
  val sub_entry = assoc(sub, exception_subtypes);
  val sup_entry = assoc(sup, exception_subtypes);

  assert (t_entry != 0);

  if (sub == nil)
    return sup;

  if (sub == t) {
    if (sup == t)
      return sup;
    uw_throwf(type_error_s,
              lit("cannot define ~a as an exception subtype of ~a"),
              sub, sup, nao);
  }

  if (sup == nil) {
    uw_throwf(type_error_s,
              lit("cannot define ~a as an exception subtype of ~a"),
              sub, sup, nao);
  }

  if (uw_exception_subtype_p(sub, sup))
    uw_throwf(type_error_s, lit("~a is already an exception subtype of ~a"),
              sub, sup, nao);

  if (uw_exception_subtype_p(sup, sub))
    uw_throwf(type_error_s, lit("~a is already an exception supertype of ~a"),
              sub, sup, nao);

  /* If sup symbol not registered, then we make it
     an immediate subtype of t. */
  if (!sup_entry) {
    sup_entry = cons(sup, t_entry);
    exception_subtypes = cons(sup_entry, exception_subtypes);
  }

  /* Make sub an immediate subtype of sup.
     If sub already registered, we just repoint it. */
  if (sub_entry) {
    *cdr_l(sub_entry) = sup_entry;
  } else {
    sub_entry = cons(sub, sup_entry);
    exception_subtypes = cons(sub_entry, exception_subtypes);
  }
  return sup;
}

void uw_continue(uw_frame_t *current, uw_frame_t *cont)
{
  uw_pop_frame(current);
  uw_exit_point = cont;
  uw_unwind_to_exit_point();
}

void uw_init(void)
{
  protect(&toplevel_env.ev.func_bindings, &exception_subtypes, (val *) 0);
  exception_subtypes = cons(cons(t, nil), exception_subtypes);
  uw_register_subtype(type_error_s, error_s);
  uw_register_subtype(internal_error_s, error_s);
  uw_register_subtype(numeric_error_s, error_s);
  uw_register_subtype(range_error_s, error_s);
  uw_register_subtype(query_error_s, error_s);
  uw_register_subtype(file_error_s, error_s);
  uw_register_subtype(process_error_s, error_s);
}
