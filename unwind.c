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
#include "lib.h"
#include "gc.h"
#include "unwind.h"

static uw_frame_t *uw_stack;
static uw_frame_t *uw_exit_point;
static uw_frame_t toplevel_env;

static void uw_unwind_to_exit_point()
{
  while (uw_stack && uw_stack != uw_exit_point)
    uw_stack = uw_stack->uw.up;

  if (!uw_stack)
    abort();

  uw_exit_point = 0;

  switch (uw_stack->uw.type) {
  case UW_BLOCK:
    longjmp(uw_stack->bl.jb, 1);
    break;
  case UW_ENV: /* env frame cannot be exit point */
    abort();
  default:
    abort();
  }
}

void uw_init(void)
{
  protect(&toplevel_env.ev.func_bindings, 0);
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
