/* Copyright 2011
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
#include <errno.h>
#include <dirent.h>
#include <setjmp.h>
#include <stdarg.h>
#include <wchar.h>
#include "config.h"
#include "lib.h"
#include "gc.h"
#include "unwind.h"
#include "regex.h"
#include "stream.h"
#include "parser.h"
#include "hash.h"
#include "debug.h"
#include "eval.h"

typedef val (*opfun_t)(val, val);

val eval_error_s;

val inc_s, dec_s, push_s, pop_s;
val gethash_s;

val top_vb, top_fb;
val op_table;

val make_env(val vbindings, val fbindings, val up_env)
{
  val env = make_obj();
  env->e.type = ENV;
  env->e.fbindings = fbindings;
  env->e.vbindings = vbindings;
  env->e.up_env = up_env;
  return env;
}

val env_fbind(val env, val sym, val fun)
{
  type_check(env, ENV);
  env->e.fbindings = acons_new(sym, fun, env->e.fbindings);
  return sym;
}

val env_vbind(val env, val sym, val obj)
{
  type_check(env, ENV);
  env->e.vbindings = acons_new(sym, obj, env->e.vbindings);
  return sym;
}

static val eval_error(val form, val fmt, ...)
{
  va_list vl;
  val stream = make_string_output_stream();

  va_start (vl, fmt);
  if (form)
    format(stream, lit("(~a:~a) "), spec_file_str, source_loc(form), nao);
  (void) vformat(stream, fmt, vl);
  va_end (vl);

  uw_throw(eval_error_s, get_string_from_stream(stream));
  abort();
}

val lookup_var(val env, val sym)
{
  if (nullp(env)) {
    return gethash(top_vb, sym);
  } else  {
    type_check(env, ENV);

    {
      val binding = assoc(env->e.vbindings, sym);
      if (binding)
        return binding;
      return lookup_var(env->e.up_env, sym);
    }
  }
}

val lookup_fun(val env, val sym)
{
  if (nullp(env)) {
    return gethash(top_fb, sym);
  } else  {
    type_check(env, ENV);

    {
      val binding = assoc(env->e.fbindings, sym);
      if (binding)
        return binding;
      return lookup_fun(env->e.up_env, sym);
    }
  }
}

static val bind_args(val env, val params, val args, val ctx_form)
{
  val new_bindings = nil;

  for (; args && consp(params); args = cdr(args), params = cdr(params)) {
    val arg = car(args);
    val param = car(params);

    if (!bindable(param))
      eval_error(ctx_form, lit("~a: ~s is not a bindable sybol"),
                 car(ctx_form), param, nao);

    new_bindings = acons(param, arg, new_bindings);
  }

  if (bindable(params)) {
    val param = params;
    if (!bindable(param)) {
      eval_error(ctx_form, lit("~a: ~s is not a bindable sybol"),
                 car(ctx_form), param, nao);
    } else {
      new_bindings = acons(param, args, new_bindings);
    }
  } else if (consp(params)) {
    eval_error(ctx_form, lit("~s: too few arguments"), car(ctx_form), nao);
  } else if (args) {
    eval_error(ctx_form, lit("~s: too many arguments"), car(ctx_form), nao);
  }

  return make_env(new_bindings, 0, env);
}

val apply(val fun, val arglist, val ctx_form)
{
  val arg[32], *p = arg;
  int variadic, minparam, nargs;
 
  type_check (fun, FUN);

  type_assert (listp(arglist),
               (lit("apply arglist ~s is not a list"), arglist, nao));

  variadic = fun->f.variadic;
  minparam = fun->f.minparam;

  if (!variadic) {
    for (; arglist; arglist = cdr(arglist))
      *p++ = car(arglist);

    nargs = p - arg;

    if (nargs != minparam)
      eval_error(ctx_form, lit("apply: wrong number of arguments"), nao);

    switch (fun->f.functype) {
    case F0:
      return fun->f.f.f0(fun->f.env);
    case F1:
      return fun->f.f.f1(fun->f.env, arg[0]);
    case F2:
      return fun->f.f.f2(fun->f.env, arg[0], arg[1]);
    case F3:
      return fun->f.f.f3(fun->f.env, arg[0], arg[1], arg[2]);
    case F4:
      return fun->f.f.f4(fun->f.env, arg[0], arg[1], arg[2], arg[3]);
    case N0:
      return fun->f.f.n0();
    case N1:
      return fun->f.f.n1(arg[0]);
    case N2:
      return fun->f.f.n2(arg[0], arg[1]);
    case N3:
      return fun->f.f.n3(arg[0], arg[1], arg[2]);
    case N4:
      return fun->f.f.n4(arg[0], arg[1], arg[2], arg[3]);
    case FINTERP:
      internal_error("unsupported function type");
    }
  } else {
    for (; arglist && p - arg < minparam; arglist = cdr(arglist))
      *p++ = car(arglist);

    nargs = p - arg;

    if (nargs < minparam)
      eval_error(ctx_form, lit("apply: too few arguments"), nao);

    switch (fun->f.functype) {
    case FINTERP:
      return interp_fun(fun->f.env, fun->f.f.interp_fun, arglist);
    case F0:
      return fun->f.f.f0v(fun->f.env, arglist);
    case F1:
      return fun->f.f.f1v(fun->f.env, arg[0], arglist);
    case F2:
      return fun->f.f.f2v(fun->f.env, arg[0], arg[1], arglist);
    case F3:
      return fun->f.f.f3v(fun->f.env, arg[0], arg[1], arg[2], arglist);
    case F4:
      return fun->f.f.f4v(fun->f.env, arg[0], arg[1], arg[2], arg[3], arglist);
    case N0:
      return fun->f.f.n0v(arglist);
    case N1:
      return fun->f.f.n1v(arg[0], arglist);
    case N2:
      return fun->f.f.n2v(arg[0], arg[1], arglist);
    case N3:
      return fun->f.f.n3v(arg[0], arg[1], arg[2], arglist);
    case N4:
      return fun->f.f.n4v(arg[0], arg[1], arg[2], arg[3], arglist);
    }
  }

  internal_error("corrupt function type field");
}

static val eval_args(val form, val env, val ctx_form)
{
  list_collect_decl (values, ptail);
  for (; form; form = cdr(form))
    list_collect(ptail, eval(car(form), env, ctx_form));
  return values;
}

val interp_fun(val env, val fun, val args)
{
  val def = cdr(fun);
  val params = car(def);
  val body = cdr(def);
  val ev_args = eval_args(args, env, args);
  val fun_env = bind_args(env, params, ev_args, fun);
  return eval_progn(body, fun_env, body);
}

val eval(val form, val env, val ctx_form)
{
  type_check(env, ENV);
  debug_check(consp(form) ? form : ctx_form, env->e.vbindings, nil, nil, nil);

  if (nullp(form)) {
    return nil;
  } else if (symbolp(form)) {
    if (!bindable(form)) {
      return form;
    } else {
      val binding = lookup_var(env, form);
      if (binding)
        return cdr(binding);
      eval_error(ctx_form, lit("unbound variable ~s"), form, nao);
      abort();
    }
  } else if (consp(form)) {
    val oper = car(form);

    if (regexp(oper))
      return oper;

    {
      val fbinding = lookup_fun(env, oper);

      if (fbinding) {
        return apply(cdr(fbinding), 
                     eval_args(rest(form), env, form),
                     form);
      } else {
        val entry = gethash(op_table, oper);

        if (!entry) {
          eval_error(form, lit("no such function or operator: ~s"), oper, nao);
          abort();
        } else { 
          opfun_t fp = (opfun_t) cptr_get(entry);
          return fp(form, env);
        }
      }
    }
  } else {
    return form;
  }
}

val bindable(val obj)
{
  return (obj && symbolp(obj) && obj != t && !keywordp(obj)) ? t : nil;
}

val eval_progn(val forms, val env, val ctx_form)
{
  val retval = nil;

  for (; forms; forms = cdr(forms))
    retval = eval(car(forms), env, ctx_form);

  return retval;
}

static val op_let(val form, val env)
{
  val args = rest(form);
  val vars = first(args);
  val body = rest(args);
  val iter;
  list_collect_decl (new_bindings, ptail);

  for (iter = vars; iter; iter = cdr(iter)) {
    val item = car(iter);
    val var, val = nil;

    if (consp(item)) {
      if (!consp(cdr(item))) 
        eval_error(form, lit("let: invalid syntax: ~s"), item, nao);
      var = first(item);
      val = second(item);
    }

    if (symbolp(var)) {
      if (!bindable(var))
        eval_error(form, lit("let: ~s is not a bindable sybol"), var, nao);
    }

    list_collect (ptail, cons(var, val));
  }

  return eval_progn(body, make_env(new_bindings, 0, env), form);
}

static val op_lambda(val form, val env)
{
  return func_interp(env, form);
}

static val op_call(val form, val env)
{
  val args = rest(form);
  val func_form = first(args);
  val func = eval(func_form, env, form);

  if (functionp(func)) {
    return apply(func, eval_args(rest(args), env, form), form);
  } else if (symbolp(func)) {
    val binding = gethash(top_vb, func);
    if (binding)
      return apply(cdr(binding), eval_args(rest(args), env, form), form);
    eval_error(form, lit("call: no such function ~s"), form, nao);
  } else {
    eval_error(form, lit("call: ~s is not a funcallable object"), form, nao);
  }
  abort();
}

static val op_cond(val form, val env)
{
  val iter = rest(form);

  for (; iter; iter = cdr(iter)) {
    val pair = car(iter);
    if (eval(first(pair), env, form))
      return eval_progn(rest(pair), env, pair);
  }

  return nil;
}

static val op_if(val form, val env)
{
  val args = rest(form);

  return if3(eval(first(args), env, form),
             eval(second(args), env, form),
             eval(third(args), env, form));
}

static val op_and(val form, val env)
{
  val args = rest(form);
  val result = t;

  for (; args; args = cdr(args))
    if (!(result = eval(first(args), env, form)))
      return nil;

  return result;
}

static val op_or(val form, val env)
{
  val args = rest(form);

  for (; args; args = cdr(args)) {
    val result;
    if ((result = eval(first(args), env, form)))
      return result;
  }

  return nil;
}

static val op_defvar(val form, val env)
{
  val args = rest(form);
  val sym = first(args);

  if (!bindable(sym))
    eval_error(form, lit("let: ~s is not a bindable sybol"), sym, nao);

  {
    val value = eval(second(args), env, form);
    val existing = gethash(top_vb, sym);

    if (existing)
      *cdr_l(existing) = value;
    else 
      sethash(top_vb, sym, cons(sym, value));
  }

  return sym;
}

static val op_defun(val form, val env)
{
  val args = rest(form);
  val name = first(args);
  val params = second(args);

  if (!bindable(name))
    eval_error(form, lit("defun: ~s is not a bindable sybol"), name, nao);

  if (!all_satisfy(params, func_n1(bindable), nil))
    eval_error(form, lit("defun: arguments must be bindable symbols"), nao);

  /* defun captures lexical environment, so env is passed */
  sethash(top_fb, name, cons(name, func_interp(env, args)));
  return name;
}

static val op_modplace(val form, val env)
{
  val op = first(form);
  val place = second(form);
  val inc = or2(eval(third(form), env, form), num(1));
  val *loc = 0;
  val binding = nil;

  if (symbolp(place)) {
    if (!bindable(place))
      eval_error(form, lit("~a: ~s is not a bindable sybol"), op, place, nao);
    binding = lookup_var(env, place);
    if (!binding)
      eval_error(form, lit("unbound variable ~s"), place, nao);
    loc = cdr_l(binding);
  } else if (consp(place)) {
    if (first(place) == gethash_s) {
      val hash = eval(second(place), env, form);
      val key = eval(third(place), env, form);
      val new_p;
      loc = gethash_l(hash, key, &new_p);
      if (new_p)
        *loc = eval(fourth(place), env, form);
    } else {
      eval_error(form, lit("~a: ~s is not a recognized place form"),
                 op, place, nao);
    }
  } else {
    eval_error(form, lit("~a: ~s is not a place"), op, place, nao);
  }

  if (!loc)
    eval_error(form, lit("~a: place ~s doesn't exist"), op, place, nao);

  if (op == set_s) {
    return *loc = inc;
  } else if (op == inc_s) {
    return *loc = plus(*loc, inc);
  } else if (op == dec_s) {
    return *loc = plus(*loc, inc);
  } else if (op == push_s) {
    return push(inc, loc);
  } else if (op == pop_s) {
    return pop(loc);
  }

  internal_error("unrecognized operator");
}

static void reg_fun(val sym, val fun)
{
  sethash(top_fb, sym, cons(sym, fun));
}

void eval_init(void)
{
  protect(&top_vb, &top_fb, &op_table, (val *) 0);
  top_fb = make_hash(t, nil, nil);
  top_vb = make_hash(t, nil, nil);
  op_table = make_hash(nil, nil, nil);

  inc_s = intern(lit("inc"), user_package);
  dec_s = intern(lit("dec"), user_package);
  push_s = intern(lit("push"), user_package);
  pop_s = intern(lit("pop"), user_package);
  gethash_s = intern(lit("gethash"), user_package);

  sethash(op_table, intern(lit("let"), user_package), cptr((mem_t *) op_let));
  sethash(op_table, intern(lit("lambda"), user_package), cptr((mem_t *) op_lambda));
  sethash(op_table, intern(lit("call"), user_package), cptr((mem_t *) op_call));
  sethash(op_table, intern(lit("cond"), user_package), cptr((mem_t *) op_cond));
  sethash(op_table, intern(lit("if"), user_package), cptr((mem_t *) op_if));
  sethash(op_table, intern(lit("and"), user_package), cptr((mem_t *) op_and));
  sethash(op_table, intern(lit("or"), user_package), cptr((mem_t *) op_or));
  sethash(op_table, intern(lit("defvar"), user_package), cptr((mem_t *) op_defvar));
  sethash(op_table, intern(lit("defun"), user_package), cptr((mem_t *) op_defun));

  sethash(op_table, inc_s, cptr((mem_t *) op_modplace));
  sethash(op_table, dec_s, cptr((mem_t *) op_modplace));
  sethash(op_table, set_s, cptr((mem_t *) op_modplace));
  sethash(op_table, push_s, cptr((mem_t *) op_modplace));
  sethash(op_table, pop_s, cptr((mem_t *) op_modplace));

  reg_fun(cons_s, func_n2(cons));
  reg_fun(intern(lit("car"), user_package), func_n1(car));
  reg_fun(intern(lit("cdr"), user_package), func_n1(car));
  reg_fun(intern(lit("first"), user_package), func_n1(car));
  reg_fun(intern(lit("rest"), user_package), func_n1(cdr));

  reg_fun(intern(lit("atom"), user_package), func_n1(atom));
  reg_fun(intern(lit("null"), user_package), func_n1(nullp));
  reg_fun(intern(lit("consp"), user_package), func_n1(consp));
  reg_fun(intern(lit("listp"), user_package), func_n1(listp));
  reg_fun(intern(lit("proper-listp"), user_package), func_n1(proper_listp));
  reg_fun(intern(lit("length"), user_package), func_n1(length));

  reg_fun(intern(lit("+"), user_package), func_n0v(plusv));
  reg_fun(intern(lit("-"), user_package), func_n1v(minusv));
  reg_fun(intern(lit("*"), user_package), func_n0v(mulv));
  reg_fun(intern(lit("trunc"), user_package), func_n2(trunc));
  reg_fun(intern(lit("mod"), user_package), func_n2(mod));
  reg_fun(intern(lit("numberp"), user_package), func_n1(nump));

  reg_fun(intern(lit(">"), user_package), func_n1v(gtv));
  reg_fun(intern(lit("<"), user_package), func_n1v(ltv));
  reg_fun(intern(lit(">="), user_package), func_n1v(gev));
  reg_fun(intern(lit("<="), user_package), func_n1v(lev));
  reg_fun(intern(lit("max"), user_package), func_n1v(maxv));
  reg_fun(intern(lit("min"), user_package), func_n1v(minv));
  reg_fun(intern(lit("int-str"), user_package), func_n2(int_str));

  reg_fun(intern(lit("search-regex"), user_package), func_n4(search_regex));
  reg_fun(intern(lit("match-regex"), user_package), func_n3(match_regex));

  reg_fun(intern(lit("make-hash"), user_package), func_n3(make_hash));
  reg_fun(intern(lit("gethash"), user_package), func_n3(gethash_n));
  reg_fun(intern(lit("sethash"), user_package), func_n3(sethash));
  reg_fun(intern(lit("pushhash"), user_package), func_n3(pushhash));
  reg_fun(intern(lit("remhash"), user_package), func_n2(remhash));
  reg_fun(intern(lit("hash-count"), user_package), func_n1(hash_count));
  reg_fun(intern(lit("get-hash-userdata"), user_package),
          func_n1(get_hash_userdata));
  reg_fun(intern(lit("set-hash-userdata"), user_package),
          func_n2(set_hash_userdata));

  eval_error_s = intern(lit("eval-error"), user_package);
  uw_register_subtype(eval_error_s, error_s);
}
