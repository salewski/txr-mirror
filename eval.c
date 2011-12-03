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
#include "match.h"
#include "eval.h"

typedef val (*opfun_t)(val, val);

val top_vb, top_fb;
val op_table;

val eval_error_s;
val let_s, let_star_s, lambda_s, call_s;
val cond_s, if_s, and_s, or_s, defvar_s, defun_s;
val inc_s, dec_s, push_s, pop_s, gethash_s, car_s, cdr_s;
val for_s, for_star_s, dohash_s, uw_protect_s, return_s, return_from_s;
val list_s, append_s, apply_s;

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

static void env_replace_vbind(val env, val bindings)
{
  type_check(env, ENV);
  env->e.vbindings = bindings;
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

  return make_env(new_bindings, nil, env);
}

val apply(val fun, val arglist, val ctx_form)
{
  val arg[32], *p = arg;
  int variadic, minparam, nargs;

  if (symbolp(fun)) {
    val binding = gethash(top_fb, fun);
    if (!binding)
      eval_error(ctx_form, lit("~s: no such function ~s"), car(ctx_form), fun, nao);
    fun = cdr(binding);
  }

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
      eval_error(ctx_form, lit("~s: wrong number of arguments"),
                 car(ctx_form), nao);

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

static val apply_intrinsic(val fun, val args)
{
  return apply(fun, args, cons(apply_s, nil));
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
  val fun_env = bind_args(env, params, args, fun);
  return eval_progn(body, fun_env, body);
}

static val eval_intrinsic(val form, val env)
{
  uses_or2;
  expand(form);
  return eval(form, or2(env, make_env(nil, nil, env)), form);
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

static val op_quote(val form, val env)
{
  return second(form);
}

static val bindings_helper(val vars, val env, val sequential, val ctx_form)
{
  val iter;
  list_collect_decl (new_bindings, ptail);
  val nenv = if3(sequential, make_env(nil, nil, env), env);

  for (iter = vars; iter; iter = cdr(iter)) {
    val item = car(iter);
    val var, val = nil;

    if (consp(item)) {
      if (!consp(cdr(item))) 
        eval_error(ctx_form, lit("let: invalid syntax: ~s"),
                   car(ctx_form), item, nao);
      var = first(item);
      val = eval(second(item), nenv, ctx_form);
    } else {
      var = item;
    }

    if (symbolp(var)) {
      if (!bindable(var))
        eval_error(ctx_form, lit("let: ~s is not a bindable sybol"),
                   car(ctx_form), var, nao);
    }

    list_collect (ptail, cons(var, val));

    if (sequential)
      env_replace_vbind(nenv, new_bindings);
  }
  return new_bindings;
}

static val op_let(val form, val env)
{
  val let = first(form);
  val args = rest(form);
  val vars = first(args);
  val body = rest(args);
  val new_bindings = bindings_helper(vars, env, eq(let, let_star_s), form);
  return eval_progn(body, make_env(new_bindings, nil, env), form);
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
  return apply(func, eval_args(rest(args), env, form), form);
}

static val op_fun(val form, val env)
{
  val name = second(form);
  val fbinding = lookup_fun(env, name);

  if (!fbinding)
    eval_error(form, lit("no function exists named ~s"), name, nao);

  return cdr(fbinding);
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
  uses_or2;
  val op = first(form);
  val place = second(form);
  val third_arg_p = rest(rest(form));
  val newval = if3(car(third_arg_p), eval(third(form), env, form), nil);
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
    val sym = car(place);
    if (sym == gethash_s) {
      val hash = eval(second(place), env, form);
      val key = eval(third(place), env, form);
      val new_p;
      loc = gethash_l(hash, key, &new_p);
      if (new_p)
        *loc = eval(fourth(place), env, form);
    } else if (sym == car_s) {
      val cons = eval(second(place), env, form);
      loc = car_l(cons);
    } else if (sym == car_s) {
      val cons = eval(second(place), env, form);
      loc = cdr_l(cons);
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
    if (!third_arg_p)
      eval_error(form, lit("~a: missing argument"), op, place, nao);
    return *loc = newval;
  } else if (op == inc_s) {
    val inc = or2(newval, num(1));
    return *loc = plus(*loc, inc);
  } else if (op == dec_s) {
    val inc = or2(newval, num(1));
    return *loc = plus(*loc, inc);
  } else if (op == push_s) {
    if (!third_arg_p)
      eval_error(form, lit("~a: missing argument"), op, place, nao);
    return push(newval, loc);
  } else if (op == pop_s) {
    if (third_arg_p)
      eval_error(form, lit("~a: superfluous argument"), op, place, nao);
    return pop(loc);
  }

  internal_error("unrecognized operator");
}

static val op_for(val form, val env)
{
  val forsym = first(form);
  val vars = second(form);
  val cond = third(form);
  val incs = fourth(form);
  val forms = rest(rest(rest(rest(form))));
  val new_bindings = bindings_helper(vars, env, eq(forsym, for_star_s), form);
  val new_env = make_env(new_bindings, nil, env);

  for (; eval(car(cond), new_env, form); eval_progn(incs, new_env, form))
    eval_progn(forms, new_env, form);

  return eval_progn(rest(cond), new_env, form);
}

static val op_dohash(val form, val env)
{
  val spec = second(form);
  val keysym = first(spec);
  val valsym = second(spec);
  val hashform = third(spec);
  val resform = fourth(spec);
  val body = rest(rest(form));
  val iter = hash_begin(eval(hashform, env, hashform));
  val keyvar = cons(keysym, nil);
  val valvar = cons(valsym, nil);
  val new_env = make_env(cons(keyvar, cons(valvar, nil)), nil, env);
  val cell;

  while ((cell = hash_next(&iter)) != nil) {
    *cdr_l(keyvar) = car(cell);
    *cdr_l(valvar) = cdr(cell);
    eval_progn(body, new_env, form);
  }

  return eval(resform, new_env, form);
}

static val op_unwind_protect(val form, val env)
{
  val prot_form = second(form);
  val cleanup_forms = rest(rest(form));
  val result;

  uw_catch_begin(nil, exsym, exvals);

  result = eval(prot_form, env, prot_form);

  uw_do_unwind;

  uw_catch (exsym, exvals);

  uw_unwind
    eval_progn(cleanup_forms, env, cleanup_forms);

  uw_catch_end;

  return result;
}

static val op_block(val form, val env)
{
  val sym = second(form);
  val body = rest(rest(form));

  uw_block_begin (sym, result);
  result = eval_progn(body, env, form);
  uw_block_end;

  return result;
}

static val op_return(val form, val env)
{
  val retval = eval(second(form), env, form);
  uw_block_return(nil, retval);
  abort();
}

static val op_return_from(val form, val env)
{
  val name = second(form);
  val retval = eval(third(form), env, form);
  uw_block_return(name, retval);
  abort();
}

static val expand_forms(val form)
{
  if (atom(form)) {
    return form;
  } else {
    val f = car(form);
    val r = cdr(form);
    val ex_f = expand(f);
    val ex_r = expand_forms(r);

    if (ex_f == f && ex_r == r)
      return form;
    return rlcp(cons(ex_f, ex_r), form);
  }
}

static val expand_cond_pairs(val form)
{
  if (atom(form)) {
    return form;
  } else {
    val pair = first(form);
    val others = rest(form);
    val pair_ex = expand_forms(pair);
    val others_ex = expand_cond_pairs(others);

    if (pair_ex == pair && others_ex == others)
      return form;
    return rlcp(cons(pair_ex, others_ex), form);
  }
}

static val expand_place(val place)
{
  if (atom(place)) {
    return place;
  } else {
    val sym = first(place);
    if (sym == gethash_s) {
      val hash = second(place);
      val key = third(place);
      val dfl_val = fourth(place);
      val hash_ex = expand(hash);
      val key_ex = expand(key);
      val dfl_val_ex = expand(dfl_val);

      if (hash == hash_ex && key == key_ex && dfl_val == dfl_val_ex)
        return place;

      return rlcp(cons(sym, cons(hash_ex, cons(key_ex, 
                                               cons(dfl_val_ex, nil)))), 
                  place);
    } else if (sym == car_s || sym == cdr_s) {
      val cell = second(place);
      val cell_ex = expand(cell);
      if (cell == cell_ex)
        return place;
      return cons(sym, cons(cell_ex, nil));
    } else {
      eval_error(place, lit("unrecognized place: ~s"), place, nao);
    }
    abort();
  }
}

static val expand_qquote(val qquoted_form)
{
  if (nullp(qquoted_form)) {
    return nil;
  } if (atom(qquoted_form)) {
    return rlcp(cons(quote_s, cons(qquoted_form, nil)), qquoted_form);
  } else {
    val sym = car(qquoted_form);

    if (sym == splice_s) {
      eval_error(qquoted_form, lit("',*~s syntax is invalid"),
                 second(qquoted_form), nao);
    } else if (sym == unquote_s) {
      return expand(second(qquoted_form));
    } else {
      val f = car(qquoted_form);
      val r = cdr(qquoted_form);
      val f_ex;
      val r_ex = expand_qquote(r);

      if (consp(f)) {
        val qsym = car(f);
        if (qsym == splice_s) {
          f_ex = expand(second(f));
        } else if (qsym == unquote_s) {
          f_ex = cons(list_s, cons(expand(second(f)), nil));
        } else if (qsym == qquote_s) {
          f_ex = cons(list_s, cons(expand_qquote(expand_qquote(second(f))), nil));
        } else {
          f_ex = cons(list_s, cons(expand_qquote(f), nil));
        }
      } else {
        f_ex = cons(list_s, cons(expand_qquote(f), nil));
      }

      if (atom(r_ex)) {
        return rlcp(cons(append_s, cons(f_ex, r_ex)), qquoted_form);
      } else {
        if (car(r_ex) == append_s)
          r_ex = cdr(r_ex);
        return rlcp(cons(append_s, cons(f_ex, r_ex)), qquoted_form);
      }
    }
  }
  abort();
}

static val expand_vars(val vars)
{
  if (atom(vars)) {
    return vars;
  } else if (symbolp(car(vars))) {
    val rest_vars = rest(vars);
    val rest_vars_ex = expand_vars(rest_vars);
    if (rest_vars == rest_vars_ex)
      return vars;
    return cons(car(vars), rest_vars_ex);
  } else {
    cons_bind (var, init, car(vars));
    val rest_vars = rest(vars);
    val init_ex = expand(init);
    val rest_vars_ex = expand_vars(rest_vars);

    if (init == init_ex && rest_vars == rest_vars_ex)
      return vars;

    return cons(cons(var, init_ex), rest_vars_ex);
  }
}

val expand(val form)
{
  if (atom(form)) {
    return form;
  } else {
    val sym = car(form);

    if (sym == let_s || sym == let_star_s || sym == lambda_s) {
      val body = rest(rest(form));
      val vars = second(form);
      val body_ex = expand_forms(body);
      val vars_ex = expand_vars(vars);
      if (body == body_ex && vars == vars_ex)
        return form;
      return rlcp(cons(sym, cons(vars_ex, body_ex)), form);
    } else if (sym == block_s || sym == return_from_s) {
      val name = second(form);
      val body = rest(rest(form));
      val body_ex = expand_forms(body);
      if (body == body_ex)
        return form;
      return rlcp(cons(sym, cons(name, body_ex)), form);
    } else if (sym == cond_s) {
      val pairs = rest(form);
      val pairs_ex = expand_cond_pairs(pairs);

      if (pairs == pairs_ex)
        return form;
      return rlcp(cons(cond_s, pairs_ex), form);
    } else if (sym == defvar_s) {
      val name = second(form);
      val init = third(form);
      val init_ex = expand(init);

      if (init == init_ex)
        return form;
      return rlcp(cons(sym, cons(name, cons(init_ex, nil))), form);
    } else if (sym == defun_s) {
      val name = second(form);
      val args = third(form);
      val body = rest(rest(rest(form)));
      val body_ex = expand_forms(body);

      if (body == body_ex)
        return form;
      return rlcp(cons(sym, cons(name, cons(args, body_ex))), form);
    } else if (sym == inc_s || sym == dec_s || sym == push_s || sym == pop_s) {
      val place = second(form);
      val inc = third(form);
      val place_ex = expand_place(place);
      val inc_x = expand(inc);

      if (place == place_ex && inc == inc_x)
        return form;
      return rlcp(cons(sym, cons(place, cons(inc_x, nil))), form);
    } else if (sym == quote_s || sym == fun_s) {
      return form;
    } else if (sym == qquote_s) {
      return expand_qquote(second(form));
    } else if (sym == for_s || sym == for_star_s) {
      val vars = second(form);
      val cond = third(form);
      val incs = fourth(form);
      val forms = rest(rest(rest(rest(form))));
      val vars_ex = expand_vars(vars);
      val cond_ex = expand_forms(cond);
      val incs_ex = expand_forms(incs);
      val forms_ex = expand_forms(forms);

      if (vars == vars_ex && cond == cond_ex && 
          incs == incs_ex && forms == forms_ex)
        return form;
      return rlcp(cons(sym, 
                       cons(vars_ex, 
                            cons(cond_ex, cons(incs_ex, forms_ex)))), form);
    } else if (sym == dohash_s) {
      val spec = second(form);
      val keysym = first(spec);
      val valsym = second(spec);
      val hashform = third(spec);
      val resform = fourth(spec);
      val body = rest(rest(form));
      val hashform_ex = expand(hashform);
      val resform_ex = expand(resform);
      val body_ex = expand_forms(body);
     
      if (hashform == hashform_ex && resform == resform_ex && body == body_ex)
        return form;
      return cons(sym, cons(cons(keysym, 
                                 cons(valsym, cons(hashform_ex, nil))),
                            body_ex));
    } else if (sym == do_s) {
      val forms = rest(form);
      val forms_ex = expand_forms(forms);

      if (forms == forms_ex)
        return form;
      return rlcp(cons(sym, forms_ex), form);
    } else {
      /* funtion call */
      /* also handles: call, if, and, or, unwind-protect, return */
      val args = rest(form);
      val args_ex = expand_forms(args);

      if (args == args_ex)
        return form;
      return rlcp(cons(sym, args_ex), form);
    }
    abort();
  }
}

static val mapcarv(val fun, val list_of_lists)
{
  if (!cdr(list_of_lists)) {
    return mapcar(fun, car(list_of_lists));
  } else {
    val lofl = copy_list(list_of_lists);
    list_collect_decl (out, otail);

    for (;;) {
      val iter;
      list_collect_decl (args, atail);

      for (iter = lofl; iter; iter = cdr(iter)) {
        val list = car(iter);
        if (!list)
          return out;
        list_collect(atail, car(list));
        *car_l(iter) = cdr(list);
      }

      list_collect(otail, apply(fun, args, nil));
    }
  }
}

static val mappendv(val fun, val list_of_lists)
{
  if (!cdr(list_of_lists)) {
    return mappend(fun, car(list_of_lists));
  } else {
    val lofl = copy_list(list_of_lists);
    list_collect_decl (out, otail);

    for (;;) {
      val iter;
      list_collect_decl (args, atail);

      for (iter = lofl; iter; iter = cdr(iter)) {
        val list = car(iter);
        if (!list)
          return out;
        list_collect(atail, car(list));
        *car_l(iter) = cdr(list);
      }

      list_collect_append (otail, apply(fun, args, nil));
    }
  }
}

static void reg_fun(val sym, val fun)
{
  sethash(top_fb, sym, cons(sym, fun));
}

static void reg_var(val sym, val obj)
{
  sethash(top_vb, sym, cons(sym, obj));
}

void eval_init(void)
{
  protect(&top_vb, &top_fb, &op_table, (val *) 0);
  top_fb = make_hash(t, nil, nil);
  top_vb = make_hash(t, nil, nil);
  op_table = make_hash(nil, nil, nil);

  let_s = intern(lit("let"), user_package);
  let_star_s = intern(lit("let*"), user_package);
  lambda_s = intern(lit("lambda"), user_package);
  call_s = intern(lit("call"), user_package);
  cond_s = intern(lit("cond"), user_package);
  if_s = intern(lit("if"), user_package);
  and_s = intern(lit("and"), user_package);
  or_s = intern(lit("or"), user_package);
  defvar_s = intern(lit("defvar"), user_package);
  defun_s = intern(lit("defun"), user_package);
  inc_s = intern(lit("inc"), user_package);
  dec_s = intern(lit("dec"), user_package);
  push_s = intern(lit("push"), user_package);
  pop_s = intern(lit("pop"), user_package);
  for_s = intern(lit("for"), user_package);
  for_star_s = intern(lit("for*"), user_package);
  dohash_s = intern(lit("dohash"), user_package);
  uw_protect_s = intern(lit("unwind-protect"), user_package);
  return_s = intern(lit("return"), user_package);
  return_from_s = intern(lit("return-from"), user_package);
  gethash_s = intern(lit("gethash"), user_package);
  car_s = intern(lit("car"), user_package);
  cdr_s = intern(lit("cdr"), user_package);
  car_s = intern(lit("car"), user_package);
  cdr_s = intern(lit("cdr"), user_package);
  list_s = intern(lit("list"), user_package);
  append_s = intern(lit("append"), user_package);
  apply_s = intern(lit("apply"), user_package);

  sethash(op_table, quote_s, cptr((mem_t *) op_quote));
  sethash(op_table, let_s, cptr((mem_t *) op_let));
  sethash(op_table, let_star_s, cptr((mem_t *) op_let));
  sethash(op_table, lambda_s, cptr((mem_t *) op_lambda));
  sethash(op_table, call_s, cptr((mem_t *) op_call));
  sethash(op_table, fun_s, cptr((mem_t *) op_fun));
  sethash(op_table, cond_s, cptr((mem_t *) op_cond));
  sethash(op_table, if_s, cptr((mem_t *) op_if));
  sethash(op_table, and_s, cptr((mem_t *) op_and));
  sethash(op_table, or_s, cptr((mem_t *) op_or));
  sethash(op_table, defvar_s, cptr((mem_t *) op_defvar));
  sethash(op_table, defun_s, cptr((mem_t *) op_defun));
  sethash(op_table, inc_s, cptr((mem_t *) op_modplace));
  sethash(op_table, dec_s, cptr((mem_t *) op_modplace));
  sethash(op_table, set_s, cptr((mem_t *) op_modplace));
  sethash(op_table, push_s, cptr((mem_t *) op_modplace));
  sethash(op_table, pop_s, cptr((mem_t *) op_modplace));
  sethash(op_table, for_s, cptr((mem_t *) op_for));
  sethash(op_table, for_star_s, cptr((mem_t *) op_for));
  sethash(op_table, dohash_s, cptr((mem_t *) op_dohash));
  sethash(op_table, uw_protect_s, cptr((mem_t *) op_unwind_protect));
  sethash(op_table, block_s, cptr((mem_t *) op_block));
  sethash(op_table, return_s, cptr((mem_t *) op_return));
  sethash(op_table, return_from_s, cptr((mem_t *) op_return_from));

  reg_fun(cons_s, func_n2(cons));
  reg_fun(car_s, func_n1(car));
  reg_fun(cdr_s, func_n1(car));
  reg_fun(intern(lit("first"), user_package), func_n1(car));
  reg_fun(intern(lit("rest"), user_package), func_n1(cdr));
  reg_fun(append_s, func_n0v(appendv));
  reg_fun(list_s, func_n0v(identity));

  reg_fun(intern(lit("atom"), user_package), func_n1(atom));
  reg_fun(intern(lit("null"), user_package), func_n1(nullp));
  reg_fun(intern(lit("consp"), user_package), func_n1(consp));
  reg_fun(intern(lit("listp"), user_package), func_n1(listp));
  reg_fun(intern(lit("proper-listp"), user_package), func_n1(proper_listp));
  reg_fun(intern(lit("length"), user_package), func_n1(length));

  reg_fun(intern(lit("mapcar"), user_package), func_n1v(mapcarv));
  reg_fun(intern(lit("mappend"), user_package), func_n1v(mappendv));
  reg_fun(apply_s, func_n2(apply_intrinsic));

  reg_fun(intern(lit("second"), user_package), func_n1(second));
  reg_fun(intern(lit("third"), user_package), func_n1(third));
  reg_fun(intern(lit("fourth"), user_package), func_n1(fourth));
  reg_fun(intern(lit("fifth"), user_package), func_n1(fifth));
  reg_fun(intern(lit("sixth"), user_package), func_n1(sixth));
  reg_fun(intern(lit("copy-list"), user_package), func_n1(copy_list));
  reg_fun(intern(lit("nreverse"), user_package), func_n1(nreverse));
  reg_fun(intern(lit("reverse"), user_package), func_n1(reverse));
  reg_fun(intern(lit("ldiff"), user_package), func_n2(ldiff));
  reg_fun(intern(lit("flatten"), user_package), func_n1(flatten));
  reg_fun(intern(lit("memq"), user_package), func_n2(memq));
  reg_fun(intern(lit("memqual"), user_package), func_n2(memqual));
  reg_fun(intern(lit("tree-find"), user_package), func_n3(tree_find));
  reg_fun(intern(lit("some"), user_package), func_n3(some_satisfy));
  reg_fun(intern(lit("all"), user_package), func_n3(all_satisfy));
  reg_fun(intern(lit("none"), user_package), func_n3(none_satisfy));
  reg_fun(intern(lit("eq"), user_package), func_n2(eq));
  reg_fun(intern(lit("eql"), user_package), func_n2(eql));
  reg_fun(intern(lit("equal"), user_package), func_n2(equal));

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

  reg_fun(intern(lit("search-regex"), user_package), func_n4(search_regex));
  reg_fun(intern(lit("match-regex"), user_package), func_n3(match_regex));

  reg_fun(intern(lit("make-hash"), user_package), func_n3(make_hash));
  reg_fun(gethash_s, func_n3(gethash_n));
  reg_fun(intern(lit("sethash"), user_package), func_n3(sethash));
  reg_fun(intern(lit("pushhash"), user_package), func_n3(pushhash));
  reg_fun(intern(lit("remhash"), user_package), func_n2(remhash));
  reg_fun(intern(lit("hash-count"), user_package), func_n1(hash_count));
  reg_fun(intern(lit("get-hash-userdata"), user_package),
          func_n1(get_hash_userdata));
  reg_fun(intern(lit("set-hash-userdata"), user_package),
          func_n2(set_hash_userdata));
  reg_fun(intern(lit("hashp"), user_package), func_n1(hashp));
  reg_fun(intern(lit("maphash"), user_package), func_n2(maphash));

  reg_fun(intern(lit("eval"), user_package), func_n2(eval_intrinsic));

  reg_var(intern(lit("*stdout*"), user_package), std_output);
  reg_var(intern(lit("*stdin*"), user_package), std_input);
  reg_var(intern(lit("*stderr*"), user_package), std_error);
  reg_fun(intern(lit("format"), user_package), func_n2v(formatv));
  reg_fun(intern(lit("print"), user_package), func_n2(obj_print));
  reg_fun(intern(lit("pprint"), user_package), func_n2(obj_pprint));
  reg_fun(intern(lit("make-string-input-stream"), user_package), func_n1(make_string_input_stream));
  reg_fun(intern(lit("make-string-byte-input-stream"), user_package), func_n1(make_string_byte_input_stream));
  reg_fun(intern(lit("make-string-output-stream"), user_package), func_n0(make_string_output_stream));
  reg_fun(intern(lit("get-string-from-stream"), user_package), func_n1(get_string_from_stream));
  reg_fun(intern(lit("make-strlist-output-stream"), user_package), func_n0(make_strlist_output_stream));
  reg_fun(intern(lit("get-list-from-stream"), user_package), func_n1(get_list_from_stream));
  reg_fun(intern(lit("close-stream"), user_package), func_n2(close_stream));
  reg_fun(intern(lit("get-line"), user_package), func_n1(get_line));
  reg_fun(intern(lit("get-char"), user_package), func_n1(get_char));
  reg_fun(intern(lit("get-byte"), user_package), func_n1(get_byte));
  reg_fun(intern(lit("put-string"), user_package), func_n2(put_string));
  reg_fun(intern(lit("put-line"), user_package), func_n2(put_line));
  reg_fun(intern(lit("put-char"), user_package), func_n2(put_char));
  reg_fun(intern(lit("flush-stream"), user_package), func_n1(flush_stream));
  reg_fun(intern(lit("open-directory"), user_package), func_n1(open_directory));
  reg_fun(intern(lit("open-file"), user_package), func_n2(open_file));
  reg_fun(intern(lit("open-pipe"), user_package), func_n2(open_pipe));

  reg_var(intern(lit("*user-package*"), user_package), user_package);
  reg_var(intern(lit("*keyword-package*"), user_package), keyword_package);
  reg_var(intern(lit("*system-package*"), user_package), system_package);
  reg_fun(intern(lit("make-sym"), user_package), func_n1(make_sym));
  reg_fun(intern(lit("make-package"), user_package), func_n1(make_package));
  reg_fun(intern(lit("find-package"), user_package), func_n1(find_package));
  reg_fun(intern(lit("intern"), user_package), func_n2(intern));
  reg_fun(intern(lit("symbolp"), user_package), func_n1(symbolp));
  reg_fun(intern(lit("symbol-name"), user_package), func_n1(symbol_name));
  reg_fun(intern(lit("symbol-package"), user_package), func_n1(symbol_package));
  reg_fun(intern(lit("keywordp"), user_package), func_n1(keywordp));

  reg_fun(intern(lit("mkstring"), user_package), func_n2(mkstring));
  reg_fun(intern(lit("copy-str"), user_package), func_n1(copy_str));
  reg_fun(intern(lit("upcase-str"), user_package), func_n1(upcase_str));
  reg_fun(intern(lit("downcase-str"), user_package), func_n1(downcase_str));
  reg_fun(intern(lit("string-extend"), user_package), func_n2(string_extend));
  reg_fun(intern(lit("stringp"), user_package), func_n1(stringp));
  reg_fun(intern(lit("lazy-stringp"), user_package), func_n1(lazy_stringp));
  reg_fun(intern(lit("length-str"), user_package), func_n1(length_str));
  reg_fun(intern(lit("search-str"), user_package), func_n4(search_str));
  reg_fun(intern(lit("search-str-tree"), user_package), func_n4(search_str_tree));
  reg_fun(intern(lit("sub-str"), user_package), func_n3(sub_str));
  reg_fun(intern(lit("cat-str"), user_package), func_n2(cat_str));
  reg_fun(intern(lit("split-str"), user_package), func_n2(split_str));
  reg_fun(intern(lit("split-str-set"), user_package), func_n2(split_str_set));
  reg_fun(intern(lit("trim-str"), user_package), func_n1(trim_str));
  reg_fun(intern(lit("string-lt"), user_package), func_n2(string_lt));
  reg_fun(intern(lit("int-str"), user_package), func_n2(int_str));
  reg_fun(intern(lit("chrp"), user_package), func_n1(chrp));
  reg_fun(intern(lit("chr-str"), user_package), func_n2(chr_str));
  reg_fun(intern(lit("chr-str-set"), user_package), func_n3(chr_str_set));
  reg_fun(intern(lit("span-str"), user_package), func_n2(span_str));
  reg_fun(intern(lit("compl-span-str"), user_package), func_n2(compl_span_str));
  reg_fun(intern(lit("break-str"), user_package), func_n2(break_str));

  eval_error_s = intern(lit("eval-error"), user_package);
  uw_register_subtype(eval_error_s, error_s);
}
