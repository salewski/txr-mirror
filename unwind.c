/* Copyright 2009-2016
 * Kaz Kylheku <kaz@kylheku.com>
 * Vancouver, Canada
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <dirent.h>
#include <stdarg.h>
#include <signal.h>
#include "config.h"
#if HAVE_VALGRIND
#include <valgrind/memcheck.h>
#endif
#include "lib.h"
#include "gc.h"
#include "args.h"
#include "stream.h"
#include "txr.h"
#include "signal.h"
#include "eval.h"
#include "struct.h"
#include ALLOCA_H
#include "unwind.h"

static uw_frame_t *uw_stack;
static uw_frame_t *uw_env_stack;
static uw_frame_t *uw_exit_point;
static uw_frame_t toplevel_env;
static uw_frame_t unhandled_ex;

static val unhandled_hook_s, types_s, jump_s, sys_cont_s, sys_cont_poison_s;
static val sys_cont_free_s, sys_capture_cont_s;

static val frame_type, catch_frame_type, handle_frame_type;

/* C99 inline instantiations. */
#if __STDC_VERSION__ >= 199901L
val uw_block_return(val tag, val result);
#endif

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
         and symbol, it must execute the
         mandatory clean-up code and then
         continue the unwinding by calling uw_continue,
         passing it the ca.cont value. */
      uw_stack->ca.sym = nil;
      uw_stack->ca.args = nil;
      uw_stack->ca.cont = uw_exit_point;
      /* 1 means unwind only. */
      extended_longjmp(uw_stack->ca.jb, 1);
      abort();
    case UW_ENV:
      /* Maintain consistency of unwind stack pointer */
      uw_env_stack = uw_env_stack->ev.up_env;
      break;
    case UW_GUARD:
      if (uw_stack->gu.uw_ok)
        break;
      format(std_error, lit("~a: cannot unwind across foreign stack frames\n"),
             prog_string, nao);
      abort();
    default:
      break;
    }
  }

  if (uw_exit_point == &unhandled_ex) {
    val sym = unhandled_ex.ca.sym;
    val args = unhandled_ex.ca.args;

    if (opt_loglevel >= 1) {
      val prefix = format(nil, lit("~a:"), prog_string, nao);

      flush_stream(std_output);
      format(std_error, lit("~a unhandled exception of type ~a:\n"),
             prefix, sym, nao);

      error_trace(sym, args, std_error, prefix);
    }
    if (uw_exception_subtype_p(sym, query_error_s) ||
        uw_exception_subtype_p(sym, file_error_s)) {
      if (opt_print_bindings)
        put_line(lit("false"), std_output);
    }

    exit(EXIT_FAILURE);
  }

  uw_exit_point = 0;

  if (!uw_stack)
    abort();

  switch (uw_stack->uw.type) {
  case UW_BLOCK:
    extended_longjmp(uw_stack->bl.jb, 1);
    abort();
  case UW_ENV: /* env frame cannot be exit point */
    abort();
  case UW_CATCH:
    /* 2 means actual catch, not just unwind */
    extended_longjmp(uw_stack->ca.jb, 2);
  default:
    abort();
  }
}

static void uw_abscond_to_exit_point(void)
{
  assert (uw_exit_point);

  for (; uw_stack && uw_stack != uw_exit_point; uw_stack = uw_stack->uw.up) {
    switch (uw_stack->uw.type) {
    case UW_ENV:
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
    extended_longjmp(uw_stack->bl.jb, 1);
    abort();
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

void uw_push_guard(uw_frame_t *fr, int uw_ok)
{
  memset(fr, 0, sizeof *fr);
  fr->uw.type = UW_GUARD;
  fr->uw.up = uw_stack;
  fr->gu.uw_ok = uw_ok;
  uw_stack = fr;
}

void uw_push_debug(uw_frame_t *fr, val func, struct args *args,
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

void uw_pop_block(uw_frame_t *fr, val *pret)
{
  if (uw_stack->uw.type == UW_CAPTURED_BLOCK) {
    assert (fr->uw.type == UW_BLOCK || fr->uw.type == UW_CAPTURED_BLOCK);
    assert (uw_stack->bl.tag == fr->bl.tag);

    uw_stack = fr->uw.up;
    uw_block_return(uw_stack->bl.tag, *pret);
    abort();
  }

  uw_pop_frame(fr);
}

void uw_pop_until(uw_frame_t *fr)
{
  while (uw_stack != fr)
    uw_pop_frame(uw_stack);
}

uw_frame_t *uw_current_frame(void)
{
  return uw_stack;
}

uw_frame_t *uw_current_exit_point(void)
{
  return uw_exit_point;
}

val uw_get_frames(void)
{
  uw_frame_t *ex;
  list_collect_decl (out, ptail);

  for (ex = uw_stack; ex != 0; ex = ex->uw.up) {
    switch (ex->uw.type) {
    case UW_CATCH:
      if (ex->ca.matches && ex->ca.visible) {
        args_decl(args, ARGS_MIN);
        val cf = make_struct(catch_frame_type, nil, args);
        slotset(cf, types_s, ex->ca.matches);
        slotset(cf, jump_s, cptr(coerce(mem_t *, ex)));
        ptail = list_collect(ptail, cf);
      }
      break;
    case UW_HANDLE:
      if (ex->ha.visible) {
        args_decl(args, ARGS_MIN);
        val hf = make_struct(handle_frame_type, nil, args);
        slotset(hf, types_s, ex->ha.matches);
        slotset(hf, fun_s, ex->ha.fun);
        ptail = list_collect(ptail, hf);
      }
    default:
      break;
    }
  }

  return out;
}

val uw_find_frame(val extype, val frtype)
{
  uw_frame_t *ex;
  uw_frtype_t et;

  extype = default_bool_arg(extype);
  frtype = default_arg_strict(frtype, catch_frame_type);

  if (symbolp(frtype)) {
    frtype = find_struct_type(frtype);
    if (!frtype)
      return nil;
  }

  if (frtype == catch_frame_type)
    et = UW_CATCH;
  else if (frtype == handle_frame_type)
    et = UW_HANDLE;
  else
    return nil;

  if (frtype != catch_frame_type && frtype != handle_frame_type)
    return nil;

  for (ex = uw_stack; ex != 0; ex = ex->uw.up) {
    if (ex->uw.type == et && ex->ca.visible) {
      val match;
      for (match = ex->ca.matches; match; match = cdr(match))
        if (uw_exception_subtype_p(extype, car(match)))
          break;
      if (match) {
        args_decl(args, ARGS_MIN);
        val fr = make_struct(frtype, nil, args);
        slotset(fr, types_s, ex->ca.matches);
        if (et == UW_CATCH)
          slotset(fr, jump_s, cptr(coerce(mem_t *, ex)));
        else
          slotset(fr, fun_s, ex->ha.fun);
        return fr;
      }
    }
  }

  return nil;
}

val uw_invoke_catch(val catch_frame, val sym, struct args *args)
{
  uw_frame_t *ex, *ex_point;

  if (struct_type(catch_frame) != catch_frame_type)
    uw_throwf(type_error_s, lit("invoke-catch: ~s isn't a catch frame"),
              catch_frame, nao);

  ex_point = coerce(uw_frame_t *, cptr_get(slot(catch_frame, jump_s)));

  for (ex = uw_stack; ex != 0; ex = ex->uw.up)
    if (ex == ex_point && ex->uw.type == UW_CATCH)
      break;

  if (!ex)
    uw_throwf(type_error_s, lit("invoke-catch: ~s no longer exists"),
              catch_frame, nao);

  ex->ca.sym = sym;
  ex->ca.args = args_get_list(args);
  uw_exit_point = ex;
  uw_unwind_to_exit_point();
  abort();
}

void uw_push_cont_copy(uw_frame_t *fr, mem_t *ptr,
                       void (*copy)(mem_t *ptr, int parent))
{
  memset(fr, 0, sizeof *fr);
  fr->cp.type = UW_CONT_COPY;
  fr->cp.ptr = ptr;
  fr->cp.copy = copy;
  fr->cp.up = uw_stack;
  uw_stack = fr;
}

val uw_block_return_proto(val tag, val result, val protocol)
{
  uw_frame_t *ex;

  for (ex = uw_stack; ex != 0; ex = ex->uw.up) {
    if (ex->uw.type == UW_BLOCK && ex->bl.tag == tag)
      break;
  }

  if (ex == 0)
    return nil;

  ex->bl.result = result;
  ex->bl.protocol = protocol;
  uw_exit_point = ex;
  uw_unwind_to_exit_point();
  abort();
}

val uw_block_abscond(val tag, val result)
{
  uw_frame_t *ex;

  for (ex = uw_stack; ex != 0; ex = ex->uw.up) {
    if (ex->uw.type == UW_BLOCK && ex->bl.tag == tag)
      break;
    if (ex->uw.type == UW_GUARD)
      uw_throwf(error_s, lit("~a: cannot abscond via foreign stack frames"),
                prog_string, nao);

  }

  if (ex == 0)
    return nil;

  ex->bl.result = result;
  ex->bl.protocol = nil;
  uw_exit_point = ex;
  uw_abscond_to_exit_point();
  abort();
}

void uw_push_catch(uw_frame_t *fr, val matches)
{
  memset(fr, 0, sizeof *fr);
  fr->ca.type = UW_CATCH;
  fr->ca.matches = matches;
  fr->ca.args = nil;
  fr->ca.cont = 0;
  fr->ca.visible = 1;
  fr->ca.up = uw_stack;
  uw_stack = fr;
}

void uw_push_handler(uw_frame_t *fr, val matches, val fun)
{
  memset(fr, 0, sizeof *fr);
  fr->ha.type = UW_HANDLE;
  fr->ha.matches = matches;
  fr->ha.fun = fun;
  fr->ha.visible = 1;
  fr->ha.up = uw_stack;
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

static void invoke_handler(uw_frame_t *fr, struct args *args)
{
  fr->ha.visible = 0;

  uw_simple_catch_begin;

  generic_funcall(fr->ha.fun, args);

  uw_unwind {
    fr->ha.visible = 1;
  }

  uw_catch_end;
}

val uw_throw(val sym, val args)
{
  uw_frame_t *ex;
  static int reentry_count = 0;

  if (++reentry_count > 1) {
    fprintf(stderr, "txr: invalid re-entry of exception handling logic\n");
    abort();
  }

  if (!listp(args))
    args = cons(args, nil);

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
    if (ex->uw.type == UW_HANDLE && ex->ha.visible) {
      val matches = ex->ha.matches;
      val match;
      for (match = matches; match; match = cdr(match))
        if (uw_exception_subtype_p(sym, car(match)))
          break;
      if (match) {
        args_decl_list(gf_args, ARGS_MIN, cons(sym, args));
        --reentry_count;
        invoke_handler(ex, gf_args);
        ++reentry_count;
      }
    }
  }

  if (ex == 0) {
    if (std_error == 0) {
      fprintf(stderr, "txr: unhandled exception in early initialization\n");
      abort();
    }

    {
      loc pfun = lookup_var_l(nil, unhandled_hook_s);
      val fun = deref(pfun);

      set(pfun, nil);

      if (fun) {
        if (functionp(fun))
          funcall3(fun, sym, args, last_form_evaled);
        else
          format(std_error, lit("~a: *unhandled-hook* ~s isn't a function\n"),
                            prog_string, fun, nao);
      }
    }

    ex = &unhandled_ex;
  }

  ex->ca.sym = sym;
  ex->ca.args = args;
  uw_exit_point = ex;
  reentry_count--;
  uw_unwind_to_exit_point();
  abort();
}

val uw_throwv(val sym, struct args *arglist)
{
  uw_throw(sym, args_get_list(arglist));
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

val uw_throwfv(val sym, val fmt, struct args *args)
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

val uw_errorfv(val fmt, struct args *args)
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
              lit("cannot define ~s as an exception subtype of ~s"),
              sub, sup, nao);
  }

  if (sup == nil) {
    uw_throwf(type_error_s,
              lit("cannot define ~s as an exception subtype of ~s"),
              sub, sup, nao);
  }

  if (uw_exception_subtype_p(sub, sup))
    uw_throwf(type_error_s, lit("~s is already an exception subtype of ~s"),
              sub, sup, nao);

  if (uw_exception_subtype_p(sup, sub))
    uw_throwf(type_error_s, lit("~s is already an exception supertype of ~s"),
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
    set(cdr_l(sub_entry), sup_entry);
  } else {
    sub_entry = cons(sub, sup_entry);
    exception_subtypes = cons(sub_entry, exception_subtypes);
  }
  return sup;
}

static val register_exception_subtypes(struct args *args)
{
  val types = args_copy_to_list(args);
  reduce_left(func_n2(uw_register_subtype), types, nil, nil);
  return nil;
}

static val me_defex(val form, val menv)
{
  val types = cdr(form);

  if (!all_satisfy(types, func_n1(symbolp), nil))
    eval_error(form, lit("defex: arguments must all be symbols"), nao);

  return cons(intern(lit("register-exception-subtypes"), user_package),
              mapcar(curry_12_2(list_f, quote_s), types));
}

void uw_continue(uw_frame_t *cont)
{
  uw_exit_point = cont;
  uw_unwind_to_exit_point();
}

struct cont {
  uw_frame_t *orig;
  cnum size;
  val tag;
  mem_t *stack;
};

static void cont_destroy(val obj)
{
  struct cont *cont = coerce(struct cont *, obj->co.handle);
  free(cont->stack);
  free(cont);
}

static void cont_mark(val obj)
{
  struct cont *cont = coerce(struct cont *, obj->co.handle);
  val *mem = coerce(val *, cont->stack);
  if (mem)
    gc_mark_mem(mem, mem + cont->size / sizeof *mem);
  gc_mark(cont->tag);
}

static struct cobj_ops cont_ops = cobj_ops_init(eq,
                                                cobj_print_op,
                                                cont_destroy,
                                                cont_mark,
                                                cobj_hash_op);

static void call_copy_handlers(uw_frame_t *upto, int parent)
{
  uw_frame_t *fr;

  for (fr = uw_stack; fr != 0 && fr != upto; fr = fr->uw.up) {
    if (fr->uw.type == UW_CONT_COPY)
      fr->cp.copy(fr->cp.ptr, parent);
  }
}

static val revive_cont(val dc, val arg)
{
  const int frame_slack = 32 * sizeof (val);
  struct cont *cont = coerce(struct cont *, cobj_handle(dc, sys_cont_s));

  if (arg == sys_cont_free_s) {
    free(cont->stack);
    cont->stack = 0;
    return nil;
  } else if (cont->stack) {
    mem_t *space = coerce(mem_t *, alloca(cont->size + frame_slack)) + frame_slack;
    uint_ptr_t orig_start = coerce(uint_ptr_t, cont->orig);
    uint_ptr_t orig_end = orig_start + cont->size;
    cnum delta = space - coerce(mem_t *, cont->orig);
    mem_t *ptr;
    uw_frame_t *new_uw_stack = coerce(uw_frame_t *, space), *fr;
    int env_set = 0;

    memset(space - frame_slack, 0, frame_slack);
    memcpy(space, cont->stack, cont->size);

    for (ptr = space; ptr < space + cont->size; ptr += sizeof (cnum))
    {
      uint_ptr_t *wordptr = coerce(uint_ptr_t *, ptr);
      uint_ptr_t word;
#if HAVE_VALGRIND
      uint_ptr_t vbits = 0;

      if (opt_vg_debug) {
        VALGRIND_GET_VBITS(wordptr, &vbits, sizeof *wordptr);
        VALGRIND_MAKE_MEM_DEFINED(wordptr, sizeof *wordptr);
      }
#endif
      word = *wordptr;

      if (word >= orig_start - frame_slack &&
          word < orig_end && is_ptr(coerce(val, word)))
        *wordptr = word + delta;

#if HAVE_VALGRIND
      if (opt_vg_debug)
        VALGRIND_SET_VBITS(wordptr, &vbits, sizeof *wordptr);
#endif
    }

    uw_block_begin (cont->tag, result);

    for (fr = new_uw_stack; ; fr = fr->uw.up) {
      if (!env_set && fr->uw.type == UW_ENV) {
        uw_env_stack = fr;
        env_set = 1;
      }
      if (fr->uw.up == 0) {
        bug_unless (fr->uw.type == UW_CAPTURED_BLOCK);
        bug_unless (fr->bl.tag == cont->tag);
        fr->uw.up = uw_stack;
        break;
      }
    }

    uw_stack = new_uw_stack;

    bug_unless (uw_stack->uw.type == UW_BLOCK);

    if (arg != sys_cont_poison_s)
      call_copy_handlers(&uw_blk, 0);

    uw_stack->bl.result = arg;
    uw_exit_point = if3(arg == sys_cont_poison_s, &uw_blk, uw_stack);
    uw_unwind_to_exit_point();
    abort();

    uw_block_end;

    return result;
  } else {
    uw_throwf(error_s, lit("cannot revive freed continuation"), nao);
  }
}

static val capture_cont(val tag, val fun, uw_frame_t *block)
{
  volatile val cont_obj = nil;
  uw_block_begin (nil, result);

  bug_unless (uw_stack < block);

  {
    const int capture_extra = 16 * sizeof (val);
    mem_t *lim = coerce(mem_t *, block + 1) + capture_extra;
    cnum bloff = coerce(mem_t *, block) - coerce(mem_t *, uw_stack);
    cnum size = coerce(mem_t *, lim) - coerce(mem_t *, uw_stack);
    mem_t *stack = chk_malloc(size);
    uw_frame_t *blcopy = coerce(uw_frame_t *, stack + bloff);
    struct cont *cont = coerce(struct cont *, chk_malloc(sizeof *cont));

    cont->orig = uw_stack;
    cont->size = size;
    cont->stack = stack;
    cont->tag = nil;

    memcpy(stack, uw_stack, size);

    blcopy->uw.up = 0;
    blcopy->uw.type = UW_CAPTURED_BLOCK;

    cont_obj = cobj(coerce(mem_t *, cont), sys_cont_s, &cont_ops);

    cont->tag = tag;

    result = nil;
  }

  uw_block_end;

  if (cont_obj) {
    call_copy_handlers(block, 0);
    result = funcall1(fun, func_f1(cont_obj, revive_cont));
  }

  return result;
}

val uw_capture_cont(val tag, val fun, val ctx_form)
{
  uses_or2;
  uw_frame_t *fr;

  for (fr = uw_stack; fr != 0; fr = fr->uw.up) {
    switch (fr->uw.type) {
    case UW_BLOCK:
    case UW_CAPTURED_BLOCK:
      if (fr->bl.tag != tag)
        continue;
      break;
    case UW_GUARD:
      {
        val sym = or2(car(default_bool_arg(ctx_form)), sys_capture_cont_s);
        eval_error(ctx_form, lit("~s: cannot capture continuation "
                                 "spanning external library stack frames"),
                   sym, nao);
      }
    default:
      continue;
    }

    break;
  }

  if (!fr) {
    val sym = or2(car(default_bool_arg(ctx_form)), sys_capture_cont_s);

    if (tag)
      eval_error(ctx_form, lit("~s: no block ~s is visible"), sym, tag, nao);
    else
      eval_error(ctx_form, lit("~s: no anonymous block is visible"), sym, nao);
    abort();
  }

  return capture_cont(tag, fun, fr);
}

void uw_init(void)
{
  protect(&toplevel_env.ev.func_bindings,
          &toplevel_env.ev.match_context,
          &exception_subtypes, convert(val *, 0));
  exception_subtypes = cons(cons(t, nil), exception_subtypes);
  uw_register_subtype(type_error_s, error_s);
  uw_register_subtype(internal_error_s, error_s);
  uw_register_subtype(panic_s, error_s);
  uw_register_subtype(numeric_error_s, error_s);
  uw_register_subtype(range_error_s, error_s);
  uw_register_subtype(query_error_s, error_s);
  uw_register_subtype(file_error_s, error_s);
  uw_register_subtype(process_error_s, error_s);
  uw_register_subtype(system_error_s, error_s);
  uw_register_subtype(timeout_error_s, error_s);
  uw_register_subtype(assert_s, error_s);
  uw_register_subtype(syntax_error_s, error_s);
}

void uw_late_init(void)
{
  protect(&frame_type, &catch_frame_type, &handle_frame_type,
          convert(val *, 0));
  types_s = intern(lit("types"), user_package);
  jump_s = intern(lit("jump"), user_package);
  sys_cont_s = intern(lit("cont"), system_package);
  sys_cont_poison_s = intern(lit("cont-poison"), system_package);
  sys_cont_free_s = intern(lit("cont-free"), system_package);
  frame_type = make_struct_type(intern(lit("frame"), user_package),
                                nil, nil, nil, nil, nil, nil, nil);
  catch_frame_type = make_struct_type(intern(lit("catch-frame"),
                                             user_package),
                                      frame_type, nil,
                                      list(types_s, jump_s, nao),
                                      nil, nil, nil, nil);
  handle_frame_type = make_struct_type(intern(lit("handle-frame"),
                                              user_package),
                                       frame_type, nil,
                                       list(types_s, fun_s, nao),
                                       nil, nil, nil, nil);
  reg_mac(intern(lit("defex"), user_package), func_n2(me_defex));
  reg_var(unhandled_hook_s = intern(lit("*unhandled-hook*"),
          user_package), nil);
  reg_fun(throw_s, func_n1v(uw_throwv));
  reg_fun(intern(lit("throwf"), user_package), func_n2v(uw_throwfv));
  reg_fun(error_s, func_n1v(uw_errorfv));
  reg_fun(intern(lit("register-exception-subtypes"), user_package),
          func_n0v(register_exception_subtypes));
  reg_fun(intern(lit("exception-subtype-p"), user_package),
          func_n2(uw_exception_subtype_p));
  reg_fun(intern(lit("get-frames"), user_package), func_n0(uw_get_frames));
  reg_fun(intern(lit("find-frame"), user_package), func_n2o(uw_find_frame, 0));
  reg_fun(intern(lit("invoke-catch"), user_package),
          func_n2v(uw_invoke_catch));
  reg_fun(sys_capture_cont_s = intern(lit("capture-cont"), system_package),
          func_n3o(uw_capture_cont, 2));
}
