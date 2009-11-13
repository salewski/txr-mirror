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
#include <setjmp.h>
#include <dirent.h>
#include <stdarg.h>
#include "lib.h"
#include "gc.h"
#include "stream.h"
#include "txr.h"
#include "unwind.h"

static uw_frame_t *uw_stack;
static uw_frame_t *uw_exit_point;
static uw_frame_t toplevel_env;

static void uw_unwind_to_exit_point()
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
      /* This catch frame is no longer
         visible. If the unwind section
         throws something, it cannot
         be caught in the same frame. */
      uw_stack->ca.visible = 0;
      /* 1 means unwind only. */
      longjmp(uw_stack->ca.jb, 1);
      abort();
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
    /* Catch frame is no longer visible.
       If a catch or unwind throw something,
       it cannot go back to the same catch. */
    uw_stack->ca.visible = 0;
    /* 2 means actual catch, not just unwind */
    longjmp(uw_stack->ca.jb, 2);
  default:
    abort();
  }
}

void uw_push_block(uw_frame_t *fr, obj_t *tag)
{
  fr->bl.type = UW_BLOCK;
  fr->bl.tag = tag;
  fr->bl.result = nil;
  fr->bl.up = uw_stack;
  uw_stack = fr;
}

static uw_frame_t *uw_find_env(void)
{
  uw_frame_t *fr;

  for (fr = uw_stack; fr != 0; fr = fr->uw.up) {
    if (fr->uw.type == UW_ENV)
      break;
  }

  return fr ? fr : &toplevel_env;
}

void uw_push_env(uw_frame_t *fr)
{
  uw_frame_t *prev_env = uw_find_env();
  fr->ev.type = UW_ENV;

  if (prev_env) {
    fr->ev.func_bindings = copy_alist(prev_env->ev.func_bindings);
  } else {
    fr->ev.func_bindings = nil;
  }

  fr->ev.up = uw_stack;
  uw_stack = fr;
}

obj_t *uw_get_func(obj_t *sym)
{
  uw_frame_t *env = uw_find_env();
  return cdr(assoc(env->ev.func_bindings, sym));
}

obj_t *uw_set_func(obj_t *sym, obj_t *value)
{
  uw_frame_t *env = uw_find_env();
  env->ev.func_bindings = acons_new(env->ev.func_bindings, sym, value);
  return value;
}

void uw_pop_frame(uw_frame_t *fr)
{
  assert (fr == uw_stack);
  uw_stack = uw_stack->uw.up;
}

obj_t *uw_block_return(obj_t *tag, obj_t *result)
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

void uw_push_catch(uw_frame_t *fr, obj_t *matches)
{
  fr->ca.type = UW_CATCH;
  fr->ca.matches = matches;
  fr->ca.exception = nil;
  fr->ca.cont = 0;
  fr->ca.visible = 1;
  fr->ca.up = uw_stack;
  uw_stack = fr;
}

static obj_t *exception_subtypes;

obj_t *uw_exception_subtype_p(obj_t *sub, obj_t *sup)
{
  if (sub == nil || sup == t || sub == sup) {
    return t;
  } else {
    obj_t *entry = assoc(exception_subtypes, sub);
    return memq(sup, entry) ? t : nil;
  }
}

obj_t *uw_throw(obj_t *sym, obj_t *exception)
{
  uw_frame_t *ex;

  for (ex = uw_stack; ex != 0; ex = ex->uw.up) {
    if (ex->uw.type == UW_CATCH && ex->ca.visible) {
      /* The some_satisfy would require us to
         cons up a function; we want to
         avoid consing in exception handling, if we can. */
      obj_t *matches = ex->ca.matches;
      obj_t *match;
      for (match = matches; match; match = cdr(match))
        if (uw_exception_subtype_p(sym, car(match)))
          break;
      if (match)
        break;
    }
  }

  if (ex == 0) {
    if (opt_loglevel >= 1) {
      obj_t *s = stringp(exception);
      format(std_error, L"~a: unhandled exception of type ~a:\n",
             prog_string, sym, nao);
      format(std_error, s ? L"~a: ~a\n" : L"~a: ~s\n",
             prog_string, exception, nao);
    }
    if (uw_exception_subtype_p(sym, query_error) ||
        uw_exception_subtype_p(sym, file_error)) {
      if (!output_produced)
        put_cstring(std_output, L"false\n");
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

obj_t *uw_throwf(obj_t *sym, const wchar_t *fmt, ...)
{
  va_list vl;
  obj_t *stream = make_string_output_stream();

  va_start (vl, fmt);
  (void) vformat(stream, fmt, vl);
  va_end (vl);

  uw_throw(sym, get_string_from_stream(stream));
  abort();
}

obj_t *uw_errorf(const wchar_t *fmt, ...)
{
  va_list vl;
  obj_t *stream = make_string_output_stream();

  va_start (vl, fmt);
  (void) vformat(stream, fmt, vl);
  va_end (vl);

  uw_throw(error, get_string_from_stream(stream));
  abort();
}

obj_t *uw_throwcf(obj_t *sym, const wchar_t *fmt, ...)
{
  va_list vl;
  obj_t *stream = make_string_output_stream();

  va_start (vl, fmt);
  (void) vcformat(stream, fmt, vl);
  va_end (vl);

  uw_throw(sym, get_string_from_stream(stream));
  abort();
}

obj_t *uw_errorcf(const wchar_t *fmt, ...)
{
  va_list vl;
  obj_t *stream = make_string_output_stream();

  va_start (vl, fmt);
  (void) vcformat(stream, fmt, vl);
  va_end (vl);

  uw_throw(error, get_string_from_stream(stream));
  abort();
}

obj_t *type_mismatch(const wchar_t *fmt, ...)
{
  va_list vl;
  obj_t *stream = make_string_output_stream();

  va_start (vl, fmt);
  (void) vformat(stream, fmt, vl);
  va_end (vl);

  uw_throw(type_error, get_string_from_stream(stream));
  abort();
}

obj_t *uw_register_subtype(obj_t *sub, obj_t *sup)
{
  obj_t *t_entry = assoc(exception_subtypes, t);
  obj_t *sub_entry = assoc(exception_subtypes, sub);
  obj_t *sup_entry = assoc(exception_subtypes, sup);

  assert (t_entry != 0);

  if (sub == nil)
    return sup;

  if (sub == t) {
    if (sup == t)
      return sup;
    uw_throwf(type_error, L"cannot define ~a as an exception subtype of ~a",
              sub, sup, nao);
  }

  if (sup == nil) {
    uw_throwf(type_error, L"cannot define ~a as an exception subtype of ~a",
              sub, sup, nao);
  }

  if (uw_exception_subtype_p(sub, sup))
    uw_throwf(type_error, L"~a is already an exception subtype of ~a",
              sub, sup, nao);

  if (uw_exception_subtype_p(sup, sub))
    uw_throwf(type_error, L"~a is already an exception supertype of ~a",
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
  protect(&toplevel_env.ev.func_bindings, &exception_subtypes, (obj_t **) 0);
  exception_subtypes = cons(cons(t, nil), exception_subtypes);
  uw_register_subtype(type_error, error);
  uw_register_subtype(internal_err, error);
  uw_register_subtype(numeric_err, error);
  uw_register_subtype(range_err, error);
  uw_register_subtype(query_error, error);
  uw_register_subtype(file_error, error);
  uw_register_subtype(process_error, error);
}
