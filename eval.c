/* Copyright 2010-2014
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
#include <signal.h>
#include <time.h>
#include "config.h"
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_WINDOWS_H
#include <windows.h>
#endif
#ifdef HAVE_SYSLOG
#include <syslog.h>
#endif
#include "lib.h"
#include "gc.h"
#include "arith.h"
#include "signal.h"
#include "unwind.h"
#include "regex.h"
#include "stream.h"
#include "parser.h"
#include "hash.h"
#include "debug.h"
#include "match.h"
#include "rand.h"
#include "filter.h"
#include "txr.h"
#ifdef HAVE_SYSLOG
#include "syslog.h"
#endif
#include "combi.h"
#include "eval.h"

typedef val (*opfun_t)(val, val);

struct c_var {
  val *loc;
  val bind;
};

val top_vb, top_fb, top_mb;
val op_table;

val eval_error_s;
val dwim_s, progn_s, prog1_s, let_s, let_star_s, lambda_s, call_s;
val cond_s, if_s, defvar_s, defun_s, defmacro_s, tree_case_s, tree_bind_s;
val inc_s, dec_s, push_s, pop_s, flip_s, gethash_s, car_s, cdr_s;
val del_s, vecref_s;
val for_s, for_star_s, each_s, each_star_s, collect_each_s, collect_each_star_s;
val append_each_s, append_each_star_s;
val dohash_s;
val uw_protect_s, return_s, return_from_s;
val list_s, append_s, apply_s, gen_s, generate_s, rest_s;
val delay_s, promise_s, op_s;
val hash_lit_s, hash_construct_s;
val vector_lit_s, vector_list_s;
val macro_time_s;

val whole_k, env_k;

val last_form_evaled;

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
  set(env->e.fbindings, acons_new(sym, fun, env->e.fbindings));
  return sym;
}

val env_vbind(val env, val sym, val obj)
{
  type_check(env, ENV);
  set(env->e.vbindings, acons_new(sym, obj, env->e.vbindings));
  return sym;
}

static void env_replace_vbind(val env, val bindings)
{
  type_check(env, ENV);
  set(env->e.vbindings, bindings);
}

noreturn static val eval_error(val form, val fmt, ...)
{
  va_list vl;
  val stream = make_string_output_stream();

  va_start (vl, fmt);
  if (form)
    format(stream, lit("(~a) "), source_loc_str(form), nao);
  (void) vformat(stream, fmt, vl);
  va_end (vl);

  uw_throw(eval_error_s, get_string_from_stream(stream));
  abort();
}

val lookup_var(val env, val sym)
{
  if (nullp(env)) {
    val bind = gethash(top_vb, sym);
    if (cobjp(bind)) {
      struct c_var *cv = (struct c_var *) cptr_get(bind);
      set(cv->bind->c.cdr, *cv->loc);
      return cv->bind;
    }
    return bind;
  } else  {
    type_check(env, ENV);

    {
      val binding = assoc(sym, env->e.vbindings);
      if (binding)
        return binding;
      return lookup_var(env->e.up_env, sym);
    }
  }
}

val *lookup_var_l(val env, val sym)
{
  if (nullp(env)) {
    val bind = gethash(top_vb, sym);
    if (cobjp(bind)) {
      struct c_var *cv = (struct c_var *) cptr_get(bind);
      return cv->loc;
    }
    if (bind)
      return cdr_l(bind);
    return 0;
  } else  {
    type_check(env, ENV);

    {
      val binding = assoc(sym, env->e.vbindings);
      if (binding)
        return cdr_l(binding);
      return lookup_var_l(env->e.up_env, sym);
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
      val binding = assoc(sym, env->e.fbindings);
      if (binding)
        return binding;
      return lookup_fun(env->e.up_env, sym);
    }
  }
}

static val lookup_sym_lisp1(val env, val sym)
{
  uses_or2;

  if (nullp(env)) {
    val bind = gethash(top_vb, sym);
    if (cobjp(bind)) {
      struct c_var *cv = (struct c_var *) cptr_get(bind);
      set(cv->bind->c.cdr, *cv->loc);
      return cv->bind;
    }
    return or2(bind, gethash(top_fb, sym));
  } else  {
    type_check(env, ENV);

    {
      val binding = or2(assoc(sym, env->e.vbindings),
                        assoc(sym, env->e.fbindings));
      if (binding)
        return binding;
      return lookup_sym_lisp1(env->e.up_env, sym);
    }
  }
}

static val bind_args(val env, val params, val args, val ctx_form)
{
  val new_env = make_env(nil, nil, env);
  val optargs = nil;

  for (; args && consp(params); args = cdr(args), params = cdr(params)) {
    val param = car(params);
    val initform = nil;
    val presentsym = nil;

    if (param == colon_k) {
      if (optargs)
        goto twocol;
      optargs = t;
      params = cdr(params);
      if (!consp(params))
        break;
      param = car(params);
    }

    if (optargs && consp(param)) {
      val sym = pop(&param);
      initform = pop(&param);
      presentsym = pop(&param);
      param = sym;
    }

    if (!bindable(param))
      eval_error(ctx_form, lit("~s: ~s is not a bindable symbol"),
                 car(ctx_form), param, nao);

    if (presentsym && !bindable(presentsym)) 
      eval_error(ctx_form, lit("~s: ~s is not a bindable symbol"),
                 car(ctx_form), presentsym, nao);

    if (optargs) {
      val arg = car(args);
      val initval = nil;
      val present = nil;

      if (arg == colon_k) {
        if (initform) {
          initval = eval(initform, new_env, ctx_form);
          new_env = make_env(nil, nil, new_env);
        } 
      } else {
        initval = arg;
        present = t;
      }
      env_vbind(new_env, param, initval);
      if (presentsym)
        env_vbind(new_env, presentsym, present);
    } else {
      env_vbind(new_env, param, car(args));
    }
  }

  if (bindable(params)) {
    env_vbind(new_env, params, args);
  } else if (consp(params)) {
    if (car(params) == colon_k) {
      if (optargs)
        goto twocol;
      optargs = t;
      params = cdr(params);
    }
    if (!optargs)
      eval_error(ctx_form, lit("~s: too few arguments"), car(ctx_form), nao);
    while (consp(params)) {
      val param = car(params);
      if (param == colon_k)
        goto twocol;
      if (consp(param)) {
        val sym = pop(&param);
        val initform = pop(&param);
        val presentsym = pop(&param);
        val initval = eval(initform, new_env, ctx_form);
        if (!bindable(sym))
          eval_error(ctx_form, lit("~s: ~s is not a bindable symbol"),
                     car(ctx_form), sym, nao);

        new_env = make_env(nil, nil, new_env);
        env_vbind(new_env, sym, initval);
        if (presentsym) {
          if (!bindable(presentsym)) 
            eval_error(ctx_form, lit("~s: ~s is not a bindable symbol"),
                       car(ctx_form), presentsym, nao);
          env_vbind(new_env, presentsym, nil);
        }
      } else {
        env_vbind(new_env, param, nil);
      }
      params = cdr(params);
    }
    if (bindable(params))
      env_vbind(new_env, params, nil);
  } else if (params) {
    eval_error(ctx_form, lit("~s: ~s is not a bindable symbol"),
               car(ctx_form), params, nao);
  } else if (args) {
    eval_error(ctx_form, lit("~s: too many arguments"), car(ctx_form), nao);
  }


  return new_env;
twocol:
  eval_error(ctx_form, lit("~s: multiple colons in parameter list"),
             car(ctx_form), nao);
}

static val expand_opt_params(val params)
{
  if (atom(params)) {
    return params;
  } else {
    val form = car(params);
    if (atom(form) || !consp(cdr(form))) { /* sym, or no init form */
      val params_ex = expand_opt_params(cdr(params));
      if (params_ex == cdr(params))
        return params;
      return rlcp(cons(form, params_ex), cdr(params));
    } else { /* has initform */
      val initform = car(cdr(form));
      val initform_ex = rlcp(expand(initform), initform);
      val form_ex = rlcp(cons(car(form), cons(initform_ex, cdr(cdr(form)))),
                         form);
      return rlcp(cons(form_ex, expand_opt_params(rest(params))), cdr(params));
    } 
  }
}

static val expand_params(val params)
{
  if (atom(params)) {
    return params;
  } else if (car(params) == colon_k) {
    val params_ex = expand_opt_params(cdr(params));
    if (params_ex == cdr(params))
      return params;
    return rlcp(cons(colon_k, params_ex), cdr(params));
  } else if (consp(car(params))) {
    val car_ex = expand_params(car(params));
    val params_ex = expand_params(cdr(params));
    if (car_ex == car(params) && params_ex == cdr(params))
      return params;
    return rlcp(cons(car_ex, params_ex), params);
  } else {
    val params_ex = expand_params(cdr(params));
    if (params_ex == cdr(params))
      return params;
    return rlcp(cons(car(params), params_ex), cdr(params));
  }
}

val apply(val fun, val arglist, val ctx_form)
{
  val arg[32], *p = arg;
  int variadic, fixparam, reqargs, nargs;

  if (fun && symbolp(fun)) {
    val binding = gethash(top_fb, fun);
    if (!binding)
      eval_error(ctx_form, lit("~s: no such function ~s"), car(ctx_form), fun, nao);
    fun = cdr(binding);
  }

  if (!functionp(fun)) {
    for (nargs = 0;
         (p < arg + 32) && consp(arglist);
         nargs++, p++, arglist = cdr(arglist))
    {
      *p = car(arglist);
    }
    return generic_funcall(fun, arg, nargs);
  }

  type_check (fun, FUN);

  if (!listp(arglist)) {
    val arglist_conv = tolist(arglist);
    type_assert (listp(arglist_conv),
                 (lit("~s: arglist ~s is not a list"), car(ctx_form),
                  arglist, nao));
    arglist = arglist_conv;
  }

  variadic = fun->f.variadic;
  fixparam = fun->f.fixparam;
  reqargs = fixparam - fun->f.optargs;

  if (!variadic) {
    for (; arglist; arglist = cdr(arglist))
      *p++ = car(arglist);

    nargs = p - arg;

    if (nargs < reqargs)
      eval_error(ctx_form, lit("~s: missing required arguments"),
                 car(ctx_form), nao);
    
    if (nargs > fixparam)
      eval_error(ctx_form, lit("~s: too many arguments"),
                 car(ctx_form), nao);

    for (; nargs < fixparam; nargs++)
      *p++ = colon_k;

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
    case N5:
      return fun->f.f.n5(arg[0], arg[1], arg[2], arg[3], arg[4]);
    case N6:
      return fun->f.f.n6(arg[0], arg[1], arg[2], arg[3], arg[4], arg[5]);
    case N7:
      return fun->f.f.n7(arg[0], arg[1], arg[2], arg[3], arg[4], arg[5], arg[6]);
    case FINTERP:
      internal_error("unsupported function type");
    }
  } else {
    for (; arglist && p - arg < fixparam; arglist = cdr(arglist))
      *p++ = car(arglist);

    nargs = p - arg;

    if (nargs < reqargs)
      eval_error(ctx_form, lit("~s: missing required arguments"),
                 car(ctx_form), nao);

    for (; nargs < fixparam; nargs++)
      *p++ = colon_k;

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
    case N5:
      return fun->f.f.n5v(arg[0], arg[1], arg[2], arg[3], arg[4], arglist);
    case N6:
      return fun->f.f.n6v(arg[0], arg[1], arg[2], arg[3], arg[4], arg[5], arglist);
    case N7:
      return fun->f.f.n7v(arg[0], arg[1], arg[2], arg[3], arg[4], arg[5], arg[6], arglist);
    }
  }

  internal_error("corrupt function type field");
}

static val apply_frob_args(val args)
{
  val *plast = lastcons(args);
  if (plast) {
    *plast = car(*plast);
    return args;
  } else {
    return car(args);
  }
}

static val apply_intrinsic(val fun, val args)
{
  return apply(fun, apply_frob_args(args), cons(apply_s, nil));
}

static val list_star_intrinsic(val args)
{
  return apply_frob_args(args);
}

static val bind_macro_params(val env, val mac_env, val params, val form,
                             val loose_p, val ctx_form)
{
  val new_env = make_env(nil, nil, env);
  val err_sym = nil;
  val whole = form;
  val optargs = nil;

  while (consp(params)) {
    val param = car(params);

    if (param == whole_k || param == env_k) {
      val nparam;
      val next = cdr(params);
      if (!next)
        eval_error(ctx_form, lit("~s: dangling ~s in param list"),
                   car(ctx_form), param, nao);
      nparam = car(next);
      if (!bindable(nparam)) {
        err_sym = nparam;
        goto nbind;
      }
      env_vbind(new_env, nparam, if3(param == whole_k, whole, mac_env));
      params = cdr(next);
      continue;
    } 
    
    if (param == colon_k) {
      if (optargs)
        goto twocol;
      optargs = t;
      params = cdr(params);
      continue;
    } 

    if (consp(form)) {
      if (car(form) == colon_k) {
        form = cdr(form);
        goto noarg;
      }

      if (bindable(param)) {
        env_vbind(new_env, param, car(form));
      } else if (consp(param)) {
        if (optargs) {
          val nparam = pop(&param);
          val initform = pop(&param);
          val presentsym = pop(&param);

          (void) initform;

          if (presentsym && !bindable(presentsym)) {
            err_sym = presentsym;
            goto nbind;
          }

          new_env = bind_macro_params(new_env, mac_env,
                                      nparam, car(form), t, ctx_form);

          if (presentsym)
            env_vbind(new_env, presentsym, t);
        } else {
          new_env = bind_macro_params(new_env, mac_env,
                                      param, car(form),
                                      loose_p, ctx_form);
          if (!new_env)
            return nil;
        }
      } else {
        err_sym = param;
        goto nbind;
      }
      params = cdr(params);
      form = cdr(form);
      continue;
    }

    if (form) {
      if (loose_p == colon_k)
        return nil;
      eval_error(ctx_form, lit("~s: atom ~s not matched by parameter list"),
                 car(ctx_form), form, nao);
    }

    if (!optargs) {
      if (!loose_p)
        eval_error(ctx_form, lit("~s: insufficient number of arguments"),
                   car(ctx_form), nao);
      if (loose_p == colon_k)
        return nil;
    }

noarg:
    if (bindable(param)) {
      env_vbind(new_env, param, nil);
    } else if (consp(param)) {
      val nparam = pop(&param);
      val initform = pop(&param);
      val presentsym = pop(&param);

      if (presentsym && !bindable(presentsym)) {
        err_sym = presentsym;
        goto nbind;
      }

      if (initform) {
        val initval = eval(initform, new_env, ctx_form);
        new_env = bind_macro_params(new_env, mac_env,
                                    nparam, initval, t, ctx_form);
      } else {
        new_env = bind_macro_params(new_env, mac_env,
                                    nparam, nil, t, ctx_form);
      }

      if (presentsym)
        env_vbind(new_env, presentsym, nil);
    } else {
      err_sym = param;
      goto nbind;
    }

    params = cdr(params);
  }

  if (params) {
    if (!bindable(params)) {
      err_sym = params;
      goto nbind;
    }
    env_vbind(new_env, params, form);
    return new_env;
  }

  if (form) {
    if (loose_p == colon_k)
      return nil;
    eval_error(ctx_form,
               lit("~s: extra form part ~s not matched by parameter list"),
               car(ctx_form), form, nao);
  }

  return new_env;

nbind:
  eval_error(ctx_form, lit("~s: ~s is not a bindable symbol"),
             car(ctx_form), err_sym, nao);
twocol:
  eval_error(ctx_form, lit("~s: multiple colons in parameter list"),
             car(ctx_form), nao);
}

static val do_eval(val form, val env, val ctx_form,
                   val (*lookup)(val env, val sym));

static val do_eval_args(val form, val env, val ctx_form,
                        val (*lookup)(val env, val sym))
{
  list_collect_decl (values, ptail);
  for (; consp(form); form = cdr(form))
    ptail = list_collect(ptail, do_eval(car(form), env, ctx_form, lookup));
  if (form) {
    val dotpos = do_eval(form, env, ctx_form, lookup);
    ptail = list_collect_append(ptail, if3(listp(dotpos),
                                           dotpos, tolist(dotpos)));
  }
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

val eval_intrinsic(val form, val env)
{
  form = expand(form);
  return eval(form, default_arg(env, make_env(nil, nil, env)), form);
}

static val do_eval(val form, val env, val ctx_form,
                   val (*lookup)(val env, val sym))
{
  debug_enter;

  type_check(env, ENV);
  debug_check(consp(form) ? form : ctx_form, env, nil, nil, nil, nil);

  if (nullp(form)) {
    debug_return (nil);
  } else if (symbolp(form)) {
    if (!bindable(form)) {
      debug_return (form);
    } else {
      val binding = lookup(env, form);
      if (binding)
        debug_return (cdr(binding));
      eval_error(ctx_form, lit("unbound variable ~s"), form, nao);
      abort();
    }
  } else if (consp(form)) {
    val oper = car(form);

    last_form_evaled = form;

    if (regexp(oper))
      debug_return (oper);

    {
      val entry = gethash(op_table, oper);

      if (entry) {
        opfun_t fp = (opfun_t) cptr_get(entry);
        debug_return (fp(form, env));
      } else {
        val fbinding = lookup_fun(env, oper);
        if (!fbinding) {
          eval_error(form, lit("no such function or operator: ~s"), oper, nao);
          abort();
        } else {
          val args = do_eval_args(rest(form), env, form, lookup);
          debug_frame(oper, args, nil, env, nil, nil, nil);
          debug_return (apply(cdr(fbinding), args, form));
          debug_end;
        }
      }
    }
  } else {
    debug_return (form);
  }

  debug_leave;
}

val eval(val form, val env, val ctx_form)
{
  return do_eval(form, env, ctx_form, &lookup_var);
}

static val eval_lisp1(val form, val env, val ctx_form)
{
  return do_eval(form, env, ctx_form, &lookup_sym_lisp1);
}

static val eval_args(val form, val env, val ctx_form)
{
  return do_eval_args(form, env, ctx_form, &lookup_var);
}

static val eval_args_lisp1(val form, val env, val ctx_form)
{
  return do_eval_args(form, env, ctx_form, &lookup_sym_lisp1);
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

static val eval_prog1(val forms, val env, val ctx_form)
{
  val retval = nil;

  if (forms) {
    retval = eval(car(forms), env, ctx_form);
    forms = cdr(forms);
  }

  for (; forms; forms = cdr(forms))
    eval(car(forms), env, ctx_form);

  return retval;
}

static val op_quote(val form, val env)
{
  return second(form);
}

static val op_qquote_error(val form, val env)
{
  eval_error(form, lit("unexpanded quasiquote encountered"), nao);
  return second(form);
}

static val op_unquote_error(val form, val env)
{
  eval_error(form, lit("unquote/splice without matching quote"), nao);
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
        eval_error(ctx_form, lit("~s: invalid syntax: ~s"),
                   car(ctx_form), item, nao);
      var = first(item);
      val = eval(second(item), nenv, ctx_form);
    } else {
      var = item;
    }

    if (symbolp(var)) {
      if (!bindable(var))
        eval_error(ctx_form, lit("~s: ~s is not a bindable symbol"),
                   car(ctx_form), var, nao);
    }

    ptail = list_collect (ptail, cons(var, val));

    if (sequential)
      env_replace_vbind(nenv, new_bindings);
  }
  return new_bindings;
}

static val op_progn(val form, val env)
{
  return eval_progn(rest(form), env, form);
}

static val op_prog1(val form, val env)
{
  return eval_prog1(rest(form), env, form);
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

static val op_each(val form, val env)
{
  uses_or2;
  val each = first(form);
  val args = rest(form);
  val vars = first(args);
  val body = rest(args);
  val star = or3(eq(each, each_star_s),
                 eq(each, collect_each_star_s),
                 eq(each, append_each_star_s));
  val collect = or2(eq(each, collect_each_s), eq(each, collect_each_star_s));
  val append = or2(eq(each, append_each_s), eq(each, append_each_star_s));
  val new_bindings = bindings_helper(vars, env, star, form);
  val lists = mapcar(cdr_f, new_bindings);
  list_collect_decl (collection, ptail);

  uw_block_begin (nil, result);

  for (;;) {
    val biter, liter;
    
    for (biter = new_bindings, liter = lists; biter; 
         biter = cdr(biter), liter = cdr(liter))
    {
      val binding = car(biter);
      val list = car(liter);
      if (!list)
        goto out;
      rplacd(binding, car(list));
      rplaca(liter, cdr(list));
    }
  
    {
      val res = eval_progn(body, make_env(new_bindings, nil, env), form);
      if (collect)
        ptail = list_collect(ptail, res);
      else if (append)
        ptail = list_collect_nconc(ptail, res);
    }
  }

out:
  result = collection;

  uw_block_end;

  return result;
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
    val group = car(iter);
    val restgroup = rest(group);
    val firstval = eval(first(group), env, group);
    if (firstval)
      return if3(restgroup, eval_progn(rest(group), env, group), firstval);
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
    eval_error(form, lit("let: ~s is not a bindable symbol"), sym, nao);

  {
    if (!gethash(top_vb, sym)) {
      val value = eval(second(args), env, form);
      sethash(top_vb, sym, cons(sym, value));
    }
  }

  return sym;
}

static val op_defun(val form, val env)
{
  val args = rest(form);
  val name = first(args);
  val params = second(args);
  val body = rest(rest(args));
  val block = cons(block_s, cons(name, body));
  val fun = cons(name, cons(params, cons(block, nil)));
  val iter;
  val colon = nil;

  if (!bindable(name))
    eval_error(form, lit("defun: ~s is not a bindable symbol"), name, nao);

  for (iter = params; consp(iter); iter = cdr(iter)) {
    val param = car(iter);
    if (param == colon_k) {
      if (colon)
        eval_error(form, lit("defun: multiple colons in parameter list"), nao);
      else
          colon = t;
      continue;
    }
    if (colon && consp(param))
      continue;
    if (!bindable(param))
      eval_error(form, lit("defun: parameter ~s is not a bindable symbol"), param, nao);
  }

  if (iter && !bindable(iter))
    eval_error(form, lit("defun: dot parameter ~s is not a bindable symbol"), iter, nao);
  
  /* defun captures lexical environment, so env is passed */
  sethash(top_fb, name, cons(name, func_interp(env, fun)));
  return name;
}

static val op_defmacro(val form, val env)
{
  val args = rest(form);
  val name = first(args);
  val params = second(args);
  val body = rest(rest(args));
  val block = cons(block_s, cons(name, body));

  if (!bindable(name))
    eval_error(form, lit("defmacro: ~s is not a bindable symbol"), name, nao);

  /* defmacro captures lexical environment, so env is passed */
  sethash(top_mb, name, cons(name, cons(env, cons(params, cons(block, nil)))));
  return name;
}

static val expand_macro(val form, val expander, val mac_env)
{
  debug_enter;
  val name = car(form);
  val args = rest(form);
  val env = car(cdr(expander));
  val params = car(cdr(cdr(expander)));
  val body = cdr(cdr(cdr(expander)));
  val exp_env = bind_macro_params(env, mac_env, params, args, nil, form);
  debug_frame(name, args, nil, env, nil, nil, nil);
  debug_return(eval_progn(body, exp_env, body));
  debug_end;
  debug_leave;
}

static val op_tree_case(val form, val env)
{
  val cases = form;
  val expr = (pop(&cases), pop(&cases));

  val expr_val = eval(expr, env, form);

  for (; consp(cases); cases = cdr(cases)) {
    val onecase = car(cases);
    cons_bind (params, forms, onecase);
    val new_env = bind_macro_params(env, nil, params, expr_val,
                                      colon_k, onecase);
    if (new_env) {
      val ret = eval_progn(forms, new_env, forms);
      if (ret != colon_k)
        return ret;
    }
  }

  return nil;
}

static val expand_tree_cases(val cases)
{
  if (atom(cases)) {
    return cases;
  } else {
    val onecase = car(cases);

    if (atom(onecase)) {
      val rest_ex = expand_tree_cases(cdr(cases));
      if (rest_ex == cdr(cases))
        return cases;
      return rlcp(cons(onecase, rest_ex), cases);
    } else {
      val dstr_args = car(onecase);
      val forms = cdr(onecase);
      val dstr_args_ex = expand_params(dstr_args);
      val forms_ex = expand_forms(forms);
      val rest_ex = expand_tree_cases(cdr(cases));

      if (dstr_args_ex == dstr_args && forms_ex == forms &&
          rest_ex == cdr(cases))
        return cases;

      return rlcp(cons(cons(dstr_args_ex, forms_ex), rest_ex), cases);
    }
  }
}

static val expand_tree_case(val form)
{
  val sym = first(form);
  val expr = second(form);
  val tree_cases = rest(rest(form));
  val expr_ex = expand(expr);
  val tree_cases_ex = expand_tree_cases(tree_cases);

  if (expr_ex == expr && tree_cases_ex == tree_cases)
    return form;

  return rlcp(cons(sym, cons(expr_ex, tree_cases_ex)), form);
}

static val op_tree_bind(val form, val env)
{
  val params = second(form);
  val expr = third(form);
  val body = rest(rest(rest(form)));
  val expr_val = eval(expr, env, expr);
  val new_env = bind_macro_params(env, nil, params, expr_val, nil, form);
  return eval_progn(body, new_env, body);
}

static val op_modplace(val form, val env);

static val *dwim_loc(val form, val env, val op, val newform, val *retval)
{
  val obj = eval_lisp1(second(form), env, form);
  val args = eval_args_lisp1(rest(rest(form)), env, form);

  switch (type(obj)) {
  case LIT:
  case STR:
  case LSTR:
    if (rest(args))
      eval_error(form, lit("[~s ...]: string indexing needs one arg"),
                 obj, nao);
    {
      val index = first(args);

      if (consp(index)) {
        if (op == set_s) {
          val newval = eval(newform, env, form);
          replace_str(obj, newval, car(index), cdr(index));
          *retval = newval;
        } else if (op == del_s) {
          *retval = sub_str(obj, car(index), cdr(index));
          replace_str(obj, nil, car(index), cdr(index));
        } else {
          eval_error(form, lit("[~s ~s]: ranges allow only set and del operators"),
                     obj, index, nao);
        }

        return 0;
      } else {
        uses_or2;

        if (op == set_s) {
          val newval = eval(newform, env, form);
          chr_str_set(obj, index, eval(newform, env, form));
          *retval = newval;
        } else if (op == inc_s) {
          val newval = plus(chr_str(obj, index),
                            or2(eval(newform, env, form), one));
          chr_str_set(obj, index, newval);
          *retval = newval;
        } else if (op == dec_s) {
          val newval = minus(chr_str(obj, index),
                             or2(eval(newform, env, form), one));
          chr_str_set(obj, index, newval);
          *retval = newval;
        } else if (op == del_s) {
          *retval = chr_str(obj, index);
          replace_str(obj, nil, index, plus(index, one));
        } else {
          eval_error(form, lit("[~s ~s]: only set, inc, dec and del can be "
                               "used for string indices"), obj, index, nao);
        }
        return 0;
      }
    }
  case SYM:
  case FUN:
    eval_error(form, lit("[~s ...]: assigning through function not implemented!"),
               obj, nao);
  case VEC:
    if (rest(args))
      eval_error(form, lit("[~s ...]: vector indexing needs one arg"),
                 obj, nao);
    {
      val index = first(args);

      if (consp(index)) {

        if (op == set_s) {
          val newval = eval(newform, env, form);
          replace_vec(obj, newval, car(index), cdr(index));
          *retval = newval;
        } else if (op == del_s) {
          *retval = sub_vec(obj, car(index), cdr(index));
          replace_vec(obj, nil, car(index), cdr(index));
        } else {
          eval_error(form, lit("[~s ~s]: ranges allow only set and del operators"),
                     obj, index, nao);
        }
        return 0;
      } else {
        if (op == del_s) {
          *retval = vecref(obj, index);
          replace_vec(obj, nil, index, plus(index, one));
          return 0;
        }
        return vecref_l(obj, index);
      }
    }
  case NIL:
  case CONS:
  case LCONS:
    if (rest(args))
      eval_error(form, lit("[~s ...]: list indexing needs one arg"),
                 obj, nao);
    {
      val index = first(args);
      val cell = obj;
      if (bignump(index) || fixnump(index)) {
        if (op == del_s) {
          *retval = vecref(obj, index);
          replace_list(obj, nil, index, plus(index, one));
          return 0;
        }
        return listref_l(obj, index);
      } else if (consp(index)) {
        val newlist;
        val tempform;

        if (op == set_s) {
          val newval = eval(newform, env, form);
          newlist = replace_list(obj, newval, car(index), cdr(index));
          tempform = list(op, second(form), 
                          cons(quote_s, cons(newlist, nil)), nao);
          op_modplace(tempform, env);
          *retval = newval;
        } else if (op == del_s) {
          *retval = sub_list(obj, car(index), cdr(index));
          newlist = replace_list(obj, nil, car(index), cdr(index));
          tempform = list(op, second(form), 
                          cons(quote_s, cons(newlist, nil)), nao);
          op_modplace(tempform, env);
        } else {
          eval_error(form, lit("[~s ~s]: ranges allow only set and del operators"),
                     obj, index, nao);
        }
        return 0;
      } else {
        eval_error(form, lit("[~s ~s]: index must be integer, or pair"),
                   cell, index, nao);
      }
    }
  case COBJ:
    {
      if (hashp(obj)) {
        val new_p, *loc;
        if (lt(length(args), one))
          eval_error(form, lit("[~s ...]: hash indexing needs at least one arg"),
                     obj, nao);

        if (op == del_s) {
          *retval = gethash(obj, first(args));
          remhash(obj, first(args));
          return 0;
        }

        loc = gethash_l(obj, first(args), &new_p);
        if (new_p)
          set(*loc, second(args));
        return loc;
      }
    }
  default:
    eval_error(form, lit("object ~s not supported by [] notation"), obj, nao);
  }

  return 0;
}

static val op_modplace(val form, val env)
{
  uses_or2;
  val op = first(form);
  val place = second(form);
  val third_arg_p = rest(rest(form));
  val newform = if3(car(third_arg_p), third(form), nil);
  val newval = nil;
  val *loc = 0;

  if (op == push_s) {
    val tmp = place;
    if (!third_arg_p)
      eval_error(form, lit("~s: missing argument"), op, place, nao);
    place = third(form);
    newform = tmp;
    newval = eval(newform, env, form);
  }

  if (symbolp(place)) {
    if (!bindable(place))
      eval_error(form, lit("~s: ~s is not a bindable symbol"), op, place, nao);
    loc = lookup_var_l(env, place);
    if (!loc)
      eval_error(form, lit("unbound variable ~s"), place, nao);
  } else if (consp(place)) {
    /* TODO: dispatch these with hash table. */
    val sym = car(place);
    if (sym == dwim_s) {
      val ret = nil;
      loc = dwim_loc(place, env, op, newform, &ret);
      if (loc == 0)
        return ret;
    } else if (sym == gethash_s) {
      val hash = eval(second(place), env, form);
      val key = eval(third(place), env, form);
      val new_p;
      if (op == del_s) {
        val ret = gethash(hash, key);
        remhash(hash, key);
        return ret;
      }
      loc = gethash_l(hash, key, &new_p);
      if (new_p)
        set(*loc, eval(fourth(place), env, form));
    } else if (sym == car_s) {
      val cons = eval(second(place), env, form);
      loc = car_l(cons);
    } else if (sym == cdr_s) {
      val cons = eval(second(place), env, form);
      loc = cdr_l(cons);
    } else if (sym == vecref_s) {
      val vec = eval(second(place), env, form);
      val ind = eval(third(place), env, form);
      loc = vecref_l(vec, ind);
    } else {
      eval_error(form, lit("~s: ~s is not a recognized place form"),
                 op, place, nao);
    }
  } else {
    eval_error(form, lit("~s: ~s is not a place"), op, place, nao);
  }

  if (!loc)
    eval_error(form, lit("~s: place ~s doesn't exist"), op, place, nao);

  if (op == set_s) {
    if (!third_arg_p)
      eval_error(form, lit("~s: missing argument"), op, nao);
    return set(*loc, eval(newform, env, form));
  } else if (op == inc_s) {
    val inc = or2(eval(newform, env, form), one);
    return set(*loc, plus(*loc, inc));
  } else if (op == dec_s) {
    val inc = or2(eval(newform, env, form), one);
    return set(*loc,  minus(*loc, inc));
  } else if (op == push_s) {
    return mpush(newval, *loc);
  } else if (op == pop_s) {
    if (third_arg_p)
      eval_error(form, lit("~s: superfluous argument"), op, nao);
    return pop(loc);
  } else if (op == flip_s) {
    return *loc = nullp(*loc);
  } else if (op == del_s) {
    eval_error(form, lit("~s: cannot delete ~a"), op, place, nao);
  }

  internal_error("unhandled place modifier");
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

  uw_block_begin (nil, result);

  for (; cond == nil || eval(car(cond), new_env, form);
       eval_progn(incs, new_env, form))
    eval_progn(forms, new_env, form);

  result = eval_progn(rest(cond), new_env, form);

  uw_block_end;

  return result;
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

  uw_block_begin (nil, result);

  while ((cell = hash_next(iter)) != nil) {
    /* These assignments are gc-safe, because keyvar and valvar
       are newer objects than existing entries in the hash,
       unless the body mutates hash by inserting newer objects,
       and also deleting them such that these variables end up
       with the only reference. But in that case, those objects
       will be noted in the GC's check list. */
    *cdr_l(keyvar) = car(cell);
    *cdr_l(valvar) = cdr(cell);
    eval_progn(body, new_env, form);
  }

  result = eval(resform, new_env, form);

  uw_block_end;

  return result;
}

static val op_unwind_protect(val form, val env)
{
  val prot_form = second(form);
  val cleanup_forms = rest(rest(form));
  val result = nil;

  uw_simple_catch_begin;

  result = eval(prot_form, env, prot_form);

  uw_unwind {
    eval_progn(cleanup_forms, env, cleanup_forms);
  }

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

static val op_dwim(val form, val env)
{
  val obj = eval_lisp1(second(form), env, form);
  val args = eval_args_lisp1(rest(rest(form)), env, form);
  return apply(obj, args, form);
}

static val op_catch(val form, val env)
{
  val catch_syms = second(form);
  val try_form = third(form);
  val result = nil;

  uw_catch_begin (catch_syms, exsym, exvals);

  result = eval(try_form, env, try_form);

  uw_catch(exsym, exvals) {
    val catches = rest(rest(rest(form)));
    val iter;

    for (iter = catches; iter; iter = cdr(iter)) {
      val clause = car(iter);
      val type = first(clause);

      if (uw_exception_subtype_p(exsym, type)) {
        val params = second(clause);
        val clause_env = bind_args(env, params, if3(listp(exvals),
                                                    exvals, cons(exvals, nil)),
                                   clause);
        result = eval_progn(rest(rest(clause)), clause_env, clause);
        break;
      }
    }
  }

  uw_unwind;

  uw_catch_end;

  return result;
}

static val subst_vars(val forms, val env)
{
  list_collect_decl(out, iter);

  while (forms) {
    val form = first(forms);

    if (consp(form)) {
      val sym = first(form);

      if (sym == var_s) {
        val expr = second(form);
        val pat = third(form);
        val modifiers = fourth(form);
        val str = eval(expr, env, form);

        /* If the object is a list, we let format_field deal with the
           conversion to text, because the modifiers influence how
           it is done. */
        if (!stringp(str) && !listp(str))
          str = tostringp(str);

        if (pat) {
          forms = cons(str, cons(pat, rest(forms)));
        } else if (modifiers) {
          forms = cons(format_field(str, modifiers, nil, 
                                    curry_123_1(func_n3(eval), env, form)),
                       rest(forms));
        } else {
          if (listp(str))
            str = cat_str(mapcar(func_n1(tostringp), str), lit(" "));
          forms = cons(str, rest(forms));
        }

        continue;
      } else if (sym == quasi_s) {
        val nested = subst_vars(rest(form), env);
        iter = list_collect_append(iter, nested);
        forms = cdr(forms);
        continue;
      } else if (sym == expr_s) {
        val str = eval(rest(form), env, form);
        if (listp(str))
          str = cat_str(mapcar(func_n1(tostringp), str), lit(" "));
        else if (!stringp(str))
          str = tostringp(str);
        forms = cons(str, rest(forms));
        continue;
      } else {
        val nested = subst_vars(form, env);
        iter = list_collect_append(iter, nested);
        forms = cdr(forms);
        continue;
      }
    } else if (bindable(form)) {
      val pair = lookup_var(env, form);

      if (pair) {
        val str = cdr(pair);

        if (!stringp(str) && !listp(str))
          str = format(nil, lit("~a"), str, nao);

        forms = cons(str, rest(forms));
        continue;
      }
      uw_throwf(query_error_s, lit("unbound variable ~a"),
                               form, nao);
    }

    iter = list_collect(iter, form);
    forms = cdr(forms);
  }

  return out;
}

static val op_quasi_lit(val form, val env)
{
  return cat_str(subst_vars(rest(form), env), nil);
}

val expand_forms(val form)
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
    if (sym == dwim_s) {
      val args = rest(place);
      val args_ex = expand_forms(args);

      if (args == args_ex)
        return place;

      return rlcp(cons(sym, args_ex), place);
    } if (sym == gethash_s) {
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
    } else if (sym == vecref_s) {
      val vec = second(place);
      val vec_ex = expand(vec);
      val ind = third(place);
      val ind_ex = expand(ind);

      if (vec == vec_ex && ind == ind_ex)
        return place;
      return rlcp(cons(sym, cons(vec_ex, cons(ind_ex, nil))), place);
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
    } else if (sym == qquote_s) {
      return rlcp(expand_qquote(expand_qquote(second(qquoted_form))), 
                  qquoted_form);
    } else if (sym == hash_lit_s) {
      val args = expand_qquote(second(qquoted_form));
      val pairs = expand_qquote(rest(rest(qquoted_form)));
      return rlcp(list(hash_construct_s, args, pairs, nao), qquoted_form);
    } else if (sym == vector_lit_s) {
      val args = expand_qquote(second(qquoted_form));
      return rlcp(list(vector_list_s, args, nao), qquoted_form);
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
    return rlcp(cons(car(vars), rest_vars_ex), vars);
  } else {
    cons_bind (var, init, car(vars));
    val rest_vars = rest(vars);
    val init_ex = rlcp(expand_forms(init), init);
    val rest_vars_ex = rlcp(expand_vars(rest_vars), rest_vars);

    if (init == init_ex && rest_vars == rest_vars_ex)
      return vars;

    return rlcp(cons(cons(var, init_ex), rest_vars_ex), vars);
  }
}

static val expand_quasi(val quasi_forms)
{
  if (nullp(quasi_forms)) {
    return nil;
  } else {
    val form = first(quasi_forms);
    val form_ex = form;

    if (atom(form)) {
      form_ex = form;
    } else {
      val sym = car(form);
      if (sym == expr_s) {
        val expr_ex = expand(rest(form));

        if (expr_ex != rest(form))
          form_ex = rlcp(cons(sym, expr_ex), form);
      } 
    }

    if (form != form_ex)
      return rlcp(cons(form_ex, expand_quasi(rest(quasi_forms))), quasi_forms);
    return quasi_forms;
  }
}

static val expand_gen(val args)
{
  return list(generate_s,
              list(lambda_s, nil, first(args), nao),
              list(lambda_s, nil, second(args), nao), nao);
}

static val expand_delay(val args)
{
  return list(cons_s,
              cons(quote_s, cons(promise_s, nil)),
              cons(lambda_s, cons(nil, args)), nao);
}

static val format_op_arg(val num)
{
  return format(nil, lit("arg-~,02s-"), num, nao);
}

static val meta_meta_p(val form)
{
  uses_or2;
  if (atom(form))
    return nil;
  if (cdr(form))
    return if2(car(form) == var_s && meta_meta_p(cdr(form)), t);
  return or2(integerp(car(form)), eq(car(form), rest_s));
}

static val meta_meta_strip(val args)
{
  uses_or2;
  val strip = cdr(args);
  return if3(or2(integerp(car(strip)), eq(car(strip), rest_s)), 
             cons(var_s, strip),
             cons(expr_s, strip));
}

static val transform_op(val forms, val syms, val rg)
{
  if (atom(forms)) {
    return cons(syms, forms);
  } else {
    val fi = first(forms);
    val re = rest(forms);

    if (fi == expr_s && meta_meta_p(re))
      return cons(syms, rlcp(meta_meta_strip(re), forms));

    /* This handles improper list forms like (a b c . @42)
       when the recursion hits the @42 part. */
    if (fi == var_s && integerp(car(re))) {
      cons_bind (outsyms, outforms, transform_op(cons(forms, nil), syms, rg)); 
      return cons(outsyms, rlcp(car(outforms), outforms));
    }

    if (consp(fi) && car(fi) == var_s && consp(cdr(fi))) {
      val vararg = car(cdr(fi));

      if (integerp(vararg)) {
        val newsyms = syms;
        val new_p;
        val cell = acons_new_c(vararg, &new_p, &newsyms);
        val sym = if3(new_p, rplacd(cell, gensym(format_op_arg(vararg))),
                             cdr(cell));
        cons_bind (outsyms, outforms, transform_op(re, newsyms, rg)); 
        return cons(outsyms, rlcp(cons(sym, outforms), outforms));
      } else if (eq(vararg, rest_s)) {
        cons_bind (outsyms, outforms, transform_op(re, syms, rg)); 
        return cons(outsyms, rlcp(cons(rg, outforms), outforms));
      }
    }

    {
      cons_bind (fisyms, fiform, transform_op(fi, syms, rg)); 
      cons_bind (resyms, reforms, transform_op(re, fisyms, rg)); 
      return cons(resyms, rlcp(cons(fiform, reforms), fiform));
    }
  }
}

static val cons_find(val obj, val structure, val test)
{
  uses_or2;

  if (funcall2(test, obj, structure))
    return structure;
  if (atom(structure))
    return nil;
  return or2(cons_find(obj, car(structure), test),
             cons_find(obj, cdr(structure), test));
}

static val supplement_op_syms(val ssyms, val max)
{
  list_collect_decl (outsyms, tl);
  val si, ni;

  for (si = ssyms, ni = one;
       ssyms;
       ni = plus(ni, one), ssyms = cdr(ssyms))
  {
    val entry = car(si);
    val num = car(entry);

    for (; lt(ni, num); ni = plus(ni, one))
      tl = list_collect(tl, cons(ni, gensym(format_op_arg(ni))));
    tl = list_collect(tl, entry);
  }

  return outsyms;
}

static val expand_op(val sym, val body)
{
  val body_ex = if3(sym == op_s, expand_forms(body), expand(body));
  val rest_gensym = gensym(lit("rest-"));
  cons_bind (syms, body_trans, transform_op(body_ex, nil, rest_gensym));
  val ssyms = sort(syms, func_n2(lt), car_f);
  val nums = mapcar(car_f, ssyms);
  val max = if3(nums, maxv(car(nums), cdr(nums)), zero);
  val min = if3(nums, minv(car(nums), cdr(nums)), zero);
  val has_rest = cons_find(rest_gensym, body_trans, eq_f);
  val is_op = and3(sym == do_s, consp(body_trans),
                   gethash(op_table, car(body_trans)));

  if (!eql(max, length(nums)) && !zerop(min))
    ssyms = supplement_op_syms(ssyms, max);

  rlcp(body_trans, body);

  {
    uses_or2;
    val dwim_body = rlcp(cons(dwim_s, 
                              if3(or4(is_op, has_rest, ssyms, 
                                      nullp(proper_listp(body_trans))),
                                  body_trans,
                                  append2(body_trans, rest_gensym))),
                         body_trans);

    if (sym == do_s)
      dwim_body = rlcp(cdr(dwim_body), dwim_body);

    return cons(lambda_s,
                cons(append2(mapcar(cdr_f, ssyms), rest_gensym),
                     cons(dwim_body, nil)));
  }
}

static val expand_catch_clause(val form)
{
  val sym = first(form);
  val vars = second(form);
  val body = rest(rest(form));
  val vars_ex = expand_vars(vars);
  val body_ex = expand_forms(body);
  if (body == body_ex && vars == vars_ex)
    return form;
  return rlcp(cons(sym, cons(vars_ex, body_ex)), form);
}

static val expand_catch(val body)
{
  val try_form = first(body);
  val catch_clauses = rest(body);
  val catch_syms = mapcar(car_f, catch_clauses);
  val try_form_ex = expand(try_form);
  val catch_clauses_ex = rlcp(mapcar(func_n1(expand_catch_clause), 
                                     catch_clauses),
                              catch_clauses);

  val expanded = cons(catch_s, 
                      cons(catch_syms, 
                           cons(try_form_ex, catch_clauses_ex)));
  return rlcp(expanded, body);
}

val expand(val form)
{
  val macro = nil;

tail:
  if (atom(form)) {
    return form;
  } else {
    val sym = car(form);

    if (sym == let_s || sym == let_star_s ||
        sym == each_s || sym == each_star_s || sym == collect_each_s ||
        sym == collect_each_star_s || sym == append_each_s ||
        sym == append_each_star_s)
    {
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
    } else if (sym == lambda_s) {
      val params = second(form);
      val body = rest(rest(form));
      val params_ex = expand_params(params);
      val body_ex = expand_forms(body);

      if (body == body_ex && params == params_ex)
        return form;
      return rlcp(cons(sym, cons(params_ex, body_ex)), form);
    } else if (sym == defun_s || sym == defmacro_s) {
      val name = second(form);
      val params = third(form);
      val params_ex = expand_params(params);
      val body = rest(rest(rest(form)));
      val body_ex = expand_forms(body);
      val form_ex = form;

      if (body != body_ex || params != params_ex)
        form_ex = rlcp(cons(sym, cons(name, cons(params_ex, body_ex))), form);

      if (sym == defmacro_s) {
        val result = eval(form_ex, make_env(nil, nil, nil), form);
        return cons(quote_s, cons(result, nil));
      }
      return form_ex;
    } else if (sym == tree_case_s) {
      return expand_tree_case(form);
    } else if (sym == tree_bind_s) {
      val params = second(form);
      val expr = third(form);
      val body = rest(rest(rest(form)));
      val params_ex = expand_params(params);
      val expr_ex = expand(expr);
      val body_ex = expand_forms(body);

      if (params_ex == params && expr_ex == expr && body_ex == body)
        return form;
      return rlcp(cons(sym, cons(params_ex, cons(expr_ex, body_ex))), form);
    } else if (sym == set_s || sym == inc_s || sym == dec_s) {
      val place = second(form);
      val inc = third(form);
      val place_ex = expand_place(place);
      val inc_ex = expand(inc);

      if (place == place_ex && inc == inc_ex)
        return form;
      if (inc == nil)
        return rlcp(cons(sym, cons(place_ex, nil)), form);
      return rlcp(cons(sym, cons(place_ex, cons(inc_ex, nil))), form);
    } else if (sym == push_s) {
      val inc = second(form);
      val inc_ex = expand(inc);
      val place = third(form);
      val place_ex = expand_place(place);

      if (place == place_ex && inc == inc_ex)
        return form;
      return rlcp(cons(sym, cons(inc_ex, cons(place_ex, nil))), form);
    } else if (sym == pop_s || sym == flip_s) {
      val place = second(form);
      val place_ex = expand_place(place);
      if (place == place_ex)
        return form;
      return rlcp(cons(sym, cons(place_ex, nil)), form);
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
                                 cons(valsym, 
                                      cons(hashform_ex, 
                                           cons(resform_ex, nil)))),
                            body_ex));
    } else if (sym == quasi_s) {
      val quasi = rest(form);
      val quasi_ex = expand_quasi(quasi);
      if (quasi == quasi_ex)
        return form;
      return rlcp(cons(sym, quasi_ex), form);
    } else if (sym == gen_s) {
      form = expand_gen(rest(form));
      goto tail;
    } else if (sym == delay_s) {
      form = expand_delay(rest(form));
      goto tail;
    } else if (sym == op_s || sym == do_s) {
      return expand_op(sym, rest(form));
    } else if (sym == catch_s) {
      return expand_catch(rest(form));
    } else if (sym == regex_s || regexp(sym)) {
      return form;
    } else if (sym == macro_time_s) {
      val args = rest(form);
      val args_ex = expand_forms(args);
      val result = eval_progn(args_ex, make_env(nil, nil, nil), args);
      return cons(quote_s, cons(result, nil));
    } else if ((macro = gethash(top_mb, sym))) {
      val mac_expand = expand_macro(form, macro, make_env(nil, nil, nil));
      if (mac_expand == form)
        return form;
      rlcp_tree(mac_expand, form);
      form = mac_expand;
      goto tail;
    } else {
      /* funtion call
         also handles: progn, prog1, call, if, and, or, 
         unwind-protect, return, dwim  */
      val args = rest(form);
      val args_ex = expand_forms(args);

      if (args == args_ex)
        return form;
      return rlcp(cons(sym, args_ex), form);
    }
    abort();
  }
}

static val macro_form_p(val form)
{
  if (!consp(form))
    return nil;
  if (!gethash(top_mb, car(form)))
    return nil;
  return t;
}

static val macroexpand_1(val form, val mac_env)
{
  val macro;

  mac_env = default_arg(mac_env, make_env(nil, nil, nil));

  if (atom(form)) {
    return form;
  } else if ((macro = gethash(top_mb, car(form)))) {
    val mac_expand = expand_macro(form, macro, mac_env);
    if (mac_expand == form)
      return form;
    rlcp_tree(mac_expand, form);
    return mac_expand;
  }
  return form;
}

static val macroexpand(val form, val mac_env)
{
  for (;;) {
    val mac_expand = macroexpand_1(form, mac_env);
    if (mac_expand == form)
      return form;
    form = mac_expand;
  }
}

val mapcarv(val fun, val list_of_lists)
{
  if (!cdr(list_of_lists)) {
    return mapcar(fun, car(list_of_lists));
  } else {
    val lofl = copy_list(list_of_lists);
    val list_orig = car(list_of_lists);
    list_collect_decl (out, otail);

    for (;;) {
      val iter;
      list_collect_decl (args, atail);

      for (iter = lofl; iter; iter = cdr(iter)) {
        val list = car(iter);
        if (!list)
          return make_like(out, list_orig);
        atail = list_collect(atail, car(list));
        *car_l(iter) = cdr(list);
      }

      otail = list_collect(otail, apply(fun, args, nil));
    }
  }
}

static val mappendv(val fun, val list_of_lists)
{
  if (!cdr(list_of_lists)) {
    return mappend(fun, car(list_of_lists));
  } else {
    val lofl = copy_list(list_of_lists);
    val list_orig = car(list_of_lists);
    list_collect_decl (out, otail);

    for (;;) {
      val iter;
      list_collect_decl (args, atail);

      for (iter = lofl; iter; iter = cdr(iter)) {
        val list = car(iter);
        if (!list)
          return make_like(out, list_orig);
        atail = list_collect(atail, car(list));
        *car_l(iter) = cdr(list);
      }

      otail = list_collect_append(otail, apply(fun, args, nil));
    }
  }
}

static val lazy_mapcar_func(val env, val lcons)
{
  cons_bind (fun, list, env);

  rplaca(lcons, funcall1(fun, car(list)));
  rplacd(env, cdr(list));

  if (cdr(list))
    rplacd(lcons, make_lazy_cons(lcons_fun(lcons)));
  else
    rplacd(lcons, nil);
  return nil;
}

static val lazy_mapcar(val fun, val list)
{
  if (!list)
    return nil;
  return make_lazy_cons(func_f1(cons(fun, list), lazy_mapcar_func));
}

static val lazy_mapcarv_func(val env, val lcons)
{
  cons_bind (fun, lofl, env);
  val args = mapcar(car_f, lofl);
  val next = mapcar(cdr_f, lofl);

  rplaca(lcons, apply(fun, args, nil));
  rplacd(env, next);

  if (all_satisfy(next, identity_f, identity_f))
    rplacd(lcons, make_lazy_cons(lcons_fun(lcons)));
  else
    rplacd(lcons, nil);
  return nil;
}

static val lazy_mapcarv(val fun, val list_of_lists)
{
  if (!cdr(list_of_lists)) {
    return lazy_mapcar(fun, car(list_of_lists));
  } else if (some_satisfy(list_of_lists, null_f, identity_f)) {
    return nil;
  } else {
    val lofl = copy_list(list_of_lists);
    return make_lazy_cons(func_f1(cons(fun, lofl), lazy_mapcarv_func));
  }
}

static val lazy_mappendv(val fun, val list_of_lists)
{
  return lazy_appendv(lazy_mapcarv(fun, list_of_lists));
}

static val symbol_value(val sym)
{
  return cdr(lookup_var(nil, sym));
}

static val symbol_function(val sym)
{
  return cdr(lookup_fun(nil, sym));
}

static val boundp(val sym)
{
  return if3(lookup_var(nil, sym), t, nil);
}

static val fboundp(val sym)
{
  return if3(lookup_fun(nil, sym), t,
             if3(gethash(op_table, sym), t, nil));
}

static val rangev_func(val env, val lcons)
{
  cons_bind (from, to_step, env);
  cons_bind (to, step, to_step);
  val next = if3(functionp(step),
                 funcall1(step, from),
                 plus(from, step));

  rplaca(lcons, from);

  if (eql(from, to) ||
      (to && 
       ((lt(from, to) && gt(next, to)) ||
        (gt(from, to) && lt(next, to)))))
  {
    rplacd(lcons, nil);
    return nil;
  }

  rplacd(lcons, make_lazy_cons(lcons_fun(lcons)));
  rplaca(env, next);
  return nil;
}

static val rangev(val args)
{
  val from = default_arg(first(args), zero);
  val to = default_bool_arg(second(args));
  val step = default_arg(third(args), if3(to && gt(from, to), negone, one));
  val env = cons(from, cons(to, step));

  return make_lazy_cons(func_f1(env, rangev_func));
}

static val range_star_v_func(val env, val lcons)
{
  cons_bind (from, to_step, env);
  cons_bind (to, step, to_step);
  val next = if3(functionp(step),
                 funcall1(step, from),
                 plus(from, step));

  rplaca(lcons, from);

  if (eql(next, to) ||
      (to &&
       ((lt(from, to) && gt(next, to)) ||
        (gt(from, to) && lt(next, to)))))
  {
    rplacd(lcons, nil);
    return nil;
  }

  rplacd(lcons, make_lazy_cons(lcons_fun(lcons)));
  rplaca(env, next);
  return nil;
}

static val range_star_v(val args)
{
  uses_or2;
  val from = default_arg(first(args), zero);
  val to = default_bool_arg(second(args));

  if (eql(from, to)) {
    return nil;
  } else {
    val step = or2(third(args), if3(to && gt(from, to), negone, one));
    val env = cons(from, cons(to, step));

    return make_lazy_cons(func_f1(env, range_star_v_func));
  }
}

static val generate_func(val env, val lcons)
{
  cons_bind (while_pred, gen_fun, env);

  if (!funcall(while_pred)) {
    rplacd(lcons, nil);
  } else {
    val next_item = funcall(gen_fun);
    val lcons_next = make_lazy_cons(lcons_fun(lcons));
    rplacd(lcons, lcons_next);
    rplaca(lcons_next, next_item);
  }
  return nil;
}

val generate(val while_pred, val gen_fun)
{
  if (!funcall(while_pred)) {
    return nil;
  } else {
    val first_item = funcall(gen_fun);
    val lc = make_lazy_cons(func_f1(cons(while_pred, gen_fun), generate_func));
    rplaca(lc, first_item);
    return lc;
  }
}

static val repeat_infinite_func(val env, val lcons)
{
  if (!car(env))
    rplaca(env, cdr(env));
  rplaca(lcons, pop(car_l(env)));
  rplacd(lcons, make_lazy_cons(lcons_fun(lcons)));
  return nil;
}

static val repeat_times_func(val env, val lcons)
{
  cons_bind (stack, list_count, env);
  cons_bind (list, count, list_count);

  if (!stack) {
    rplaca(env, list);
    rplacd(list_count, count = minus(count, one));
  }

  rplaca(lcons, pop(car_l(env)));

  if (!car(env) && count == one) {
    rplacd(lcons, nil);
    return nil;
  }

  rplacd(lcons, make_lazy_cons(lcons_fun(lcons)));
  return nil;
}

static val repeatv(val list, val rest)
{
  if (!list)
    return nil;
  if (rest) {
    val count = car(rest);
    if (count == zero)
      return nil;
    return make_lazy_cons(func_f1(cons(list, cons(list, count)), 
                                  repeat_times_func));
  }
  return make_lazy_cons(func_f1(cons(list, list), repeat_infinite_func));
}

static val force(val promise)
{
  if (car(promise) != promise_s)
    return cdr(promise);

  rplaca(promise, nil);
  return rplacd(promise, funcall(cdr(promise)));
}

static val errno_wrap(val newval)
{
  val oldval = num(errno);
  if (default_bool_arg(newval))
    errno = c_num(newval);
  return oldval;
}

#if HAVE_DAEMON
static val daemon_wrap(val nochdir, val noclose)
{
  int result = daemon(nochdir ? 1 : 0, noclose ? 1 : 0);
  return result == 0 ? t : nil;
}
#endif

static val exit_wrap(val status)
{
  int stat;

  if (status == nil)
    stat = EXIT_FAILURE;
  else if (status == t)
    stat = EXIT_SUCCESS;
  else
    stat = c_num(status);

  exit(stat);
  /* notreached */
  return nil;
}

static val usleep_wrap(val usec)
{
  val retval;
  cnum u = c_num(usec);

  sig_save_enable;

#if HAVE_POSIX_NANOSLEEP
  struct timespec ts;
  ts.tv_sec = u / 1000000;
  ts.tv_nsec = (u % 1000000) * 1000;
  retval = if3(nanosleep(&ts, 0) == 0, t, nil);
#elif HAVE_POSIX_SLEEP && HAVE_POSIX_USLEEP
  retval = if2(sleep(u / 1000000) == 0 &&
               usleep(u % 1000000) == 0, t);
#elif HAVE_WINDOWS_H
  Sleep(u / 1000);
  retval = t;
#else
#error port me!
#endif

  sig_restore_enable;
  return retval;
}

static val env_hash(void)
{
  val env_strings = env();
  val hash = make_hash(nil, nil, t);

  for (; env_strings; env_strings = cdr(env_strings)) {
    cons_bind (key, val_cons, split_str(car(env_strings), lit("=")));
    sethash(hash, key, car(val_cons));
  }

  return hash;
}

static void reg_fun(val sym, val fun)
{
  sethash(top_fb, sym, cons(sym, fun));
}

static void c_var_mark(val obj)
{
  struct c_var *cv = (struct c_var *) obj->co.handle;
  cv->bind->c.cdr = *cv->loc; /* synchronize shadow binding with variable */
  gc_mark(cv->bind);
  /* we don't mark *loc since it should be a gc-protected C global! */
}

static struct cobj_ops c_var_ops = {
  cobj_equal_op,
  cobj_print_op,
  cobj_destroy_free_op,
  c_var_mark,
  cobj_hash_op
};

static void reg_var(val sym, val *loc)
{
  struct c_var *cv = (struct c_var *) chk_malloc(sizeof *cv);
  cv->loc = loc;
  cv->bind = cons(sym, *loc);
  sethash(top_vb, sym, cobj((mem_t *) cv, cptr_s, &c_var_ops));
}

static val if_fun(val cond, val then, val alt)
{
  return if3(cond, then, default_bool_arg(alt));
}

static val or_fun(val vals)
{
  for (; vals != nil; vals = cdr(vals)) {
    val item = car(vals);
    if (item)
      return item;
  }
  return nil;
}

static val and_fun(val vals)
{
  val item = t;

  for (; vals != nil; vals = cdr(vals)) {
    item = car(vals);
    if (!item)
      return nil;
  }

  return item;
}

void eval_init(void)
{
  protect(&top_vb, &top_fb, &top_mb, &op_table, &last_form_evaled, (val *) 0);
  top_fb = make_hash(t, nil, nil);
  top_vb = make_hash(t, nil, nil);
  top_mb = make_hash(t, nil, nil);
  op_table = make_hash(nil, nil, nil);

  dwim_s = intern(lit("dwim"), user_package);
  progn_s = intern(lit("progn"), user_package);
  prog1_s = intern(lit("prog1"), user_package);
  let_s = intern(lit("let"), user_package);
  let_star_s = intern(lit("let*"), user_package);
  lambda_s = intern(lit("lambda"), user_package);
  call_s = intern(lit("call"), user_package);
  cond_s = intern(lit("cond"), user_package);
  if_s = intern(lit("if"), user_package);
  defvar_s = intern(lit("defvar"), user_package);
  defun_s = intern(lit("defun"), user_package);
  defmacro_s = intern(lit("defmacro"), user_package);
  tree_case_s = intern(lit("tree-case"), user_package);
  tree_bind_s = intern(lit("tree-bind"), user_package);
  inc_s = intern(lit("inc"), user_package);
  dec_s = intern(lit("dec"), user_package);
  push_s = intern(lit("push"), user_package);
  pop_s = intern(lit("pop"), user_package);
  flip_s = intern(lit("flip"), user_package);
  del_s = intern(lit("del"), user_package);
  for_s = intern(lit("for"), user_package);
  for_star_s = intern(lit("for*"), user_package);
  each_s = intern(lit("each"), user_package);
  each_star_s = intern(lit("each*"), user_package);
  collect_each_s = intern(lit("collect-each"), user_package);
  collect_each_star_s = intern(lit("collect-each*"), user_package);
  append_each_s = intern(lit("append-each"), user_package);
  append_each_star_s = intern(lit("append-each*"), user_package);
  dohash_s = intern(lit("dohash"), user_package);
  uw_protect_s = intern(lit("unwind-protect"), user_package);
  return_s = intern(lit("return"), user_package);
  return_from_s = intern(lit("return-from"), user_package);
  gethash_s = intern(lit("gethash"), user_package);
  car_s = intern(lit("car"), user_package);
  cdr_s = intern(lit("cdr"), user_package);
  vecref_s = intern(lit("vecref"), user_package);
  list_s = intern(lit("list"), user_package);
  append_s = intern(lit("append"), user_package);
  apply_s = intern(lit("apply"), user_package);
  gen_s = intern(lit("gen"), user_package);
  generate_s = intern(lit("generate"), user_package);
  delay_s = intern(lit("delay"), user_package);
  promise_s = intern(lit("promise"), system_package);
  op_s = intern(lit("op"), user_package);
  rest_s = intern(lit("rest"), user_package);
  hash_lit_s = intern(lit("hash-construct"), system_package);
  hash_construct_s = intern(lit("hash-construct"), user_package);
  vector_lit_s = intern(lit("vector-lit"), system_package);
  vector_list_s = intern(lit("vector-list"), user_package);
  macro_time_s = intern(lit("macro-time"), user_package);
  whole_k = intern(lit("whole"), keyword_package);

  sethash(op_table, quote_s, cptr((mem_t *) op_quote));
  sethash(op_table, qquote_s, cptr((mem_t *) op_qquote_error));
  sethash(op_table, unquote_s, cptr((mem_t *) op_unquote_error));
  sethash(op_table, splice_s, cptr((mem_t *) op_unquote_error));
  sethash(op_table, progn_s, cptr((mem_t *) op_progn));
  sethash(op_table, prog1_s, cptr((mem_t *) op_prog1));
  sethash(op_table, let_s, cptr((mem_t *) op_let));
  sethash(op_table, each_s, cptr((mem_t *) op_each));
  sethash(op_table, each_star_s, cptr((mem_t *) op_each));
  sethash(op_table, collect_each_s, cptr((mem_t *) op_each));
  sethash(op_table, collect_each_star_s, cptr((mem_t *) op_each));
  sethash(op_table, append_each_s, cptr((mem_t *) op_each));
  sethash(op_table, append_each_star_s, cptr((mem_t *) op_each));
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
  sethash(op_table, defmacro_s, cptr((mem_t *) op_defmacro));
  sethash(op_table, tree_case_s, cptr((mem_t *) op_tree_case));
  sethash(op_table, tree_bind_s, cptr((mem_t *) op_tree_bind));
  sethash(op_table, inc_s, cptr((mem_t *) op_modplace));
  sethash(op_table, dec_s, cptr((mem_t *) op_modplace));
  sethash(op_table, set_s, cptr((mem_t *) op_modplace));
  sethash(op_table, push_s, cptr((mem_t *) op_modplace));
  sethash(op_table, pop_s, cptr((mem_t *) op_modplace));
  sethash(op_table, flip_s, cptr((mem_t *) op_modplace));
  sethash(op_table, del_s, cptr((mem_t *) op_modplace));
  sethash(op_table, for_s, cptr((mem_t *) op_for));
  sethash(op_table, for_star_s, cptr((mem_t *) op_for));
  sethash(op_table, dohash_s, cptr((mem_t *) op_dohash));
  sethash(op_table, uw_protect_s, cptr((mem_t *) op_unwind_protect));
  sethash(op_table, block_s, cptr((mem_t *) op_block));
  sethash(op_table, return_s, cptr((mem_t *) op_return));
  sethash(op_table, return_from_s, cptr((mem_t *) op_return_from));
  sethash(op_table, dwim_s, cptr((mem_t *) op_dwim));
  sethash(op_table, quasi_s, cptr((mem_t *) op_quasi_lit));
  sethash(op_table, catch_s, cptr((mem_t *) op_catch));

  reg_fun(cons_s, func_n2(cons));
  reg_fun(intern(lit("make-lazy-cons"), user_package), func_n1(make_lazy_cons));
  reg_fun(intern(lit("lcons-fun"), user_package), func_n1(lcons_fun));
  reg_fun(car_s, car_f);
  reg_fun(cdr_s, cdr_f);
  reg_fun(intern(lit("rplaca"), user_package), func_n2(rplaca));
  reg_fun(intern(lit("rplacd"), user_package), func_n2(rplacd));
  reg_fun(intern(lit("first"), user_package), func_n1(car));
  reg_fun(rest_s, func_n1(cdr));
  reg_fun(intern(lit("sub-list"), user_package), func_n3o(sub_list, 1));
  reg_fun(intern(lit("replace-list"), user_package), func_n4o(replace_list, 2));
  reg_fun(append_s, func_n0v(appendv));
  reg_fun(intern(lit("append*"), user_package), func_n0v(lazy_appendv));
  reg_fun(list_s, func_n0v(identity));
  reg_fun(intern(lit("list*"), user_package), func_n0v(list_star_intrinsic));
  reg_fun(intern(lit("identity"), user_package), identity_f);
  reg_fun(intern(lit("typeof"), user_package), func_n1(typeof));

  reg_fun(intern(lit("atom"), user_package), func_n1(atom));
  reg_fun(intern(lit("null"), user_package), func_n1(nullp));
  reg_fun(intern(lit("not"), user_package), func_n1(nullp));
  reg_fun(intern(lit("consp"), user_package), func_n1(consp));
  reg_fun(intern(lit("listp"), user_package), func_n1(listp));
  reg_fun(intern(lit("proper-listp"), user_package), func_n1(proper_listp));
  reg_fun(intern(lit("length-list"), user_package), func_n1(length_list));

  reg_fun(intern(lit("mapcar"), user_package), func_n1v(mapcarv));
  reg_fun(intern(lit("mapcar*"), user_package), func_n1v(lazy_mapcarv));
  reg_fun(intern(lit("mappend"), user_package), func_n1v(mappendv));
  reg_fun(intern(lit("mappend*"), user_package), func_n1v(lazy_mappendv));
  reg_fun(apply_s, func_n1v(apply_intrinsic));
  reg_fun(intern(lit("reduce-left"), user_package), func_n4o(reduce_left, 2));
  reg_fun(intern(lit("reduce-right"), user_package), func_n4o(reduce_right, 2));

  reg_fun(intern(lit("second"), user_package), func_n1(second));
  reg_fun(intern(lit("third"), user_package), func_n1(third));
  reg_fun(intern(lit("fourth"), user_package), func_n1(fourth));
  reg_fun(intern(lit("fifth"), user_package), func_n1(fifth));
  reg_fun(intern(lit("sixth"), user_package), func_n1(sixth));
  reg_fun(intern(lit("conses"), user_package), func_n1(conses));
  reg_fun(intern(lit("conses*"), user_package), func_n1(lazy_conses));
  reg_fun(intern(lit("copy-list"), user_package), func_n1(copy_list));
  reg_fun(intern(lit("nreverse"), user_package), func_n1(nreverse));
  reg_fun(intern(lit("reverse"), user_package), func_n1(reverse));
  reg_fun(intern(lit("ldiff"), user_package), func_n2(ldiff));
  reg_fun(intern(lit("flatten"), user_package), func_n1(flatten));
  reg_fun(intern(lit("flatten*"), user_package), func_n1(lazy_flatten));
  reg_fun(intern(lit("memq"), user_package), func_n2(memq));
  reg_fun(intern(lit("memql"), user_package), func_n2(memql));
  reg_fun(intern(lit("memqual"), user_package), func_n2(memqual));
  reg_fun(intern(lit("remq"), user_package), func_n2(remq));
  reg_fun(intern(lit("remql"), user_package), func_n2(remql));
  reg_fun(intern(lit("remqual"), user_package), func_n2(remqual));
  reg_fun(intern(lit("remove-if"), user_package), func_n3o(remove_if, 2));
  reg_fun(intern(lit("keep-if"), user_package), func_n3o(keep_if, 2));
  reg_fun(intern(lit("remq*"), user_package), func_n2(remq_lazy));
  reg_fun(intern(lit("remql*"), user_package), func_n2(remql_lazy));
  reg_fun(intern(lit("remqual*"), user_package), func_n2(remqual_lazy));
  reg_fun(intern(lit("remove-if*"), user_package), func_n3o(remove_if_lazy, 2));
  reg_fun(intern(lit("keep-if*"), user_package), func_n3o(keep_if_lazy, 2));
  reg_fun(intern(lit("tree-find"), user_package), func_n3o(tree_find, 2));
  reg_fun(intern(lit("countqual"), user_package), func_n2(countqual));
  reg_fun(intern(lit("countql"), user_package), func_n2(countql));
  reg_fun(intern(lit("countq"), user_package), func_n2(countq));
  reg_fun(intern(lit("count-if"), user_package), func_n3o(count_if, 2));
  reg_fun(intern(lit("posqual"), user_package), func_n2(posqual));
  reg_fun(intern(lit("posql"), user_package), func_n2(posql));
  reg_fun(intern(lit("posq"), user_package), func_n2(posq));
  reg_fun(intern(lit("pos"), user_package), func_n4o(pos, 2));
  reg_fun(intern(lit("pos-if"), user_package), func_n3o(pos_if, 2));
  reg_fun(intern(lit("some"), user_package), func_n3o(some_satisfy, 1));
  reg_fun(intern(lit("all"), user_package), func_n3o(all_satisfy, 1));
  reg_fun(intern(lit("none"), user_package), func_n3o(none_satisfy, 1));
  reg_fun(intern(lit("eq"), user_package), eq_f);
  reg_fun(intern(lit("eql"), user_package), eql_f);
  reg_fun(intern(lit("equal"), user_package), equal_f);

  reg_fun(intern(lit("+"), user_package), func_n0v(plusv));
  reg_fun(intern(lit("-"), user_package), func_n1v(minusv));
  reg_fun(intern(lit("*"), user_package), func_n0v(mulv));
  reg_fun(intern(lit("abs"), user_package), func_n1(abso));
  reg_fun(intern(lit("trunc"), user_package), func_n2(trunc));
  reg_fun(intern(lit("mod"), user_package), func_n2(mod));
  reg_fun(intern(lit("/"), user_package), func_n2(divi));
  reg_fun(intern(lit("expt"), user_package), func_n0v(exptv));
  reg_fun(intern(lit("exptmod"), user_package), func_n3(exptmod));
  reg_fun(intern(lit("isqrt"), user_package), func_n1(isqrt));
  reg_fun(intern(lit("gcd"), user_package), func_n2(gcd));
  reg_fun(intern(lit("floor"), user_package), func_n1(floorf));
  reg_fun(intern(lit("ceil"), user_package), func_n1(ceili));
  reg_fun(intern(lit("sin"), user_package), func_n1(sine));
  reg_fun(intern(lit("cos"), user_package), func_n1(cosi));
  reg_fun(intern(lit("tan"), user_package), func_n1(tang));
  reg_fun(intern(lit("asin"), user_package), func_n1(asine));
  reg_fun(intern(lit("acos"), user_package), func_n1(acosi));
  reg_fun(intern(lit("atan"), user_package), func_n1(atang));
  reg_fun(intern(lit("atan2"), user_package), func_n2(atang2));
  reg_fun(intern(lit("log"), user_package), func_n1(loga));
  reg_fun(intern(lit("exp"), user_package), func_n1(expo));
  reg_fun(intern(lit("sqrt"), user_package), func_n1(sqroot));
  reg_fun(intern(lit("cum-norm-dist"), user_package), func_n1(cum_norm_dist));
  reg_fun(intern(lit("n-choose-k"), user_package), func_n2(n_choose_k));
  reg_fun(intern(lit("n-perm-k"), user_package), func_n2(n_perm_k));
  reg_fun(intern(lit("fixnump"), user_package), func_n1(fixnump));
  reg_fun(intern(lit("bignump"), user_package), func_n1(bignump));
  reg_fun(intern(lit("floatp"), user_package), func_n1(floatp));
  reg_fun(intern(lit("integerp"), user_package), func_n1(integerp));
  reg_fun(intern(lit("numberp"), user_package), func_n1(numberp));

  reg_fun(intern(lit("zerop"), user_package), func_n1(zerop));
  reg_fun(intern(lit("evenp"), user_package), func_n1(evenp));
  reg_fun(intern(lit("oddp"), user_package), func_n1(oddp));
  reg_fun(intern(lit(">"), user_package), func_n1v(gtv));
  reg_fun(intern(lit("<"), user_package), func_n1v(ltv));
  reg_fun(intern(lit(">="), user_package), func_n1v(gev));
  reg_fun(intern(lit("<="), user_package), func_n1v(lev));
  reg_fun(intern(lit("="), user_package), func_n1v(numeqv));
  reg_fun(intern(lit("/="), user_package), func_n0v(numneqv));
  reg_fun(intern(lit("max"), user_package), func_n1v(maxv));
  reg_fun(intern(lit("min"), user_package), func_n1v(minv));
  reg_fun(intern(lit("logand"), user_package), func_n0v(logandv));
  reg_fun(intern(lit("logior"), user_package), func_n0v(logiorv));
  reg_fun(intern(lit("logxor"), user_package), func_n2(logxor));
  reg_fun(intern(lit("logtest"), user_package), func_n2(logtest));
  reg_fun(intern(lit("lognot"), user_package), func_n2o(lognot, 1));
  reg_fun(intern(lit("logtrunc"), user_package), func_n2(logtrunc));
  reg_fun(intern(lit("ash"), user_package), func_n2(ash));
  reg_fun(intern(lit("mask"), user_package), func_n0v(maskv));

  reg_fun(intern(lit("regex-compile"), user_package), func_n2o(regex_compile, 1));
  reg_fun(intern(lit("regexp"), user_package), func_n1(regexp));
  reg_fun(intern(lit("search-regex"), user_package), func_n4o(search_regex, 2));
  reg_fun(intern(lit("match-regex"), user_package), func_n3o(match_regex, 2));
  reg_fun(intern(lit("match-regex-right"), user_package),
          func_n3o(match_regex_right, 2));
  reg_fun(intern(lit("regsub"), user_package), func_n3(regsub));
  reg_fun(intern(lit("regex-parse"), user_package), func_n2o(regex_parse, 1));

  reg_fun(intern(lit("make-hash"), user_package), func_n3(make_hash));
  reg_fun(intern(lit("make-similar-hash"), user_package), func_n1(make_similar_hash));
  reg_fun(intern(lit("copy-hash"), user_package), func_n1(copy_hash));
  reg_fun(intern(lit("hash"), user_package), func_n0v(hashv));
  reg_fun(intern(lit("hash-construct"), user_package), func_n2(hash_construct));
  reg_fun(gethash_s, func_n3o(gethash_n, 2));
  reg_fun(intern(lit("inhash"), user_package), func_n3o(inhash, 2));
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
  reg_fun(intern(lit("hash-eql"), user_package), func_n1(hash_eql));
  reg_fun(intern(lit("hash-equal"), user_package), func_n1(hash_equal));
  reg_fun(intern(lit("hash-keys"), user_package), func_n1(hash_keys));
  reg_fun(intern(lit("hash-values"), user_package), func_n1(hash_values));
  reg_fun(intern(lit("hash-pairs"), user_package), func_n1(hash_pairs));
  reg_fun(intern(lit("hash-alist"), user_package), func_n1(hash_alist));
  reg_fun(intern(lit("hash-uni"), user_package), func_n3o(hash_uni, 2));
  reg_fun(intern(lit("hash-diff"), user_package), func_n2(hash_diff));
  reg_fun(intern(lit("hash-isec"), user_package), func_n3o(hash_isec, 2));
  reg_fun(intern(lit("group-by"), user_package), func_n2v(group_by));
  reg_fun(intern(lit("hash-update"), user_package), func_n2(hash_update));
  reg_fun(intern(lit("hash-update-1"), user_package),
          func_n4o(hash_update_1, 3));

  reg_fun(intern(lit("eval"), user_package), func_n2o(eval_intrinsic, 1));
  reg_fun(intern(lit("lisp-parse"), user_package), func_n2o(lisp_parse, 0));
  reg_fun(intern(lit("read"), user_package), func_n2o(lisp_parse, 0));
  reg_fun(intern(lit("expand"), system_package), func_n1(expand));
  reg_fun(intern(lit("macro-form-p"), user_package), func_n1(macro_form_p));
  reg_fun(intern(lit("macroexpand-1"), user_package),
          func_n2o(macroexpand_1, 1));
  reg_fun(intern(lit("macroexpand"), user_package),
          func_n2o(macroexpand, 1));
  reg_fun(intern(lit("chain"), user_package), func_n0v(chainv));
  reg_fun(intern(lit("andf"), user_package), func_n0v(andv));
  reg_fun(intern(lit("orf"), user_package), func_n0v(orv));
  reg_fun(intern(lit("iff"), user_package), func_n3o(iff, 2));
  reg_fun(intern(lit("iffi"), user_package), func_n3o(iffi, 2));
  reg_fun(intern(lit("if"), user_package), func_n3o(if_fun, 2));
  reg_fun(intern(lit("or"), user_package), func_n0v(or_fun));
  reg_fun(intern(lit("and"), user_package), func_n0v(and_fun));

  reg_var(intern(lit("*stdout*"), user_package), &std_output);
  reg_var(intern(lit("*stddebug*"), user_package), &std_debug);
  reg_var(intern(lit("*stdin*"), user_package), &std_input);
  reg_var(intern(lit("*stderr*"), user_package), &std_error);
  reg_var(intern(lit("*stdnull*"), user_package), &std_null);
#ifdef HAVE_SYSLOG
  reg_var(intern(lit("*stdlog*"), user_package), &std_log);
#endif
  reg_fun(intern(lit("format"), user_package), func_n2v(formatv));
  reg_fun(intern(lit("print"), user_package), func_n2o(obj_print, 1));
  reg_fun(intern(lit("pprint"), user_package), func_n2o(obj_pprint, 1));
  reg_fun(intern(lit("tostring"), user_package), func_n1(tostring));
  reg_fun(intern(lit("tostringp"), user_package), func_n1(tostringp));
  reg_fun(intern(lit("make-string-input-stream"), user_package), func_n1(make_string_input_stream));
  reg_fun(intern(lit("make-string-byte-input-stream"), user_package), func_n1(make_string_byte_input_stream));
  reg_fun(intern(lit("make-string-output-stream"), user_package), func_n0(make_string_output_stream));
  reg_fun(intern(lit("get-string-from-stream"), user_package), func_n1(get_string_from_stream));
  reg_fun(intern(lit("make-strlist-output-stream"), user_package), func_n0(make_strlist_output_stream));
  reg_fun(intern(lit("get-list-from-stream"), user_package), func_n1(get_list_from_stream));
  reg_fun(intern(lit("close-stream"), user_package), func_n2o(close_stream, 1));
  reg_fun(intern(lit("get-line"), user_package), func_n1o(get_line, 0));
  reg_fun(intern(lit("get-char"), user_package), func_n1o(get_char, 0));
  reg_fun(intern(lit("get-byte"), user_package), func_n1o(get_byte, 0));
  reg_fun(intern(lit("put-string"), user_package), func_n2o(put_string, 1));
  reg_fun(intern(lit("put-line"), user_package), func_n2o(put_line, 1));
  reg_fun(intern(lit("put-char"), user_package), func_n2o(put_char, 1));
  reg_fun(intern(lit("put-byte"), user_package), func_n2o(put_byte, 1));
  reg_fun(intern(lit("unget-char"), user_package), func_n2o(unget_char, 1));
  reg_fun(intern(lit("unget-byte"), user_package), func_n2o(unget_byte, 1));
  reg_fun(intern(lit("flush-stream"), user_package), func_n1(flush_stream));
  reg_fun(intern(lit("seek-stream"), user_package), func_n3(seek_stream));
  reg_fun(intern(lit("stat"), user_package), func_n1(statf));
  reg_fun(intern(lit("streamp"), user_package), func_n1(streamp));
  reg_fun(intern(lit("real-time-stream-p"), user_package), func_n1(real_time_stream_p));
  reg_fun(intern(lit("stream-set-prop"), user_package), func_n3(stream_set_prop));
  reg_fun(intern(lit("stream-get-prop"), user_package), func_n2(stream_get_prop));
  reg_fun(intern(lit("make-catenated-stream"), user_package), func_n0v(make_catenated_stream));
  reg_var(intern(lit("s-ifmt"), user_package), &s_ifmt);
  reg_var(intern(lit("s-iflnk"), user_package), &s_iflnk);
  reg_var(intern(lit("s-ifreg"), user_package), &s_ifreg);
  reg_var(intern(lit("s-ifblk"), user_package), &s_ifblk);
  reg_var(intern(lit("s-ifdir"), user_package), &s_ifdir);
  reg_var(intern(lit("s-ifchr"), user_package), &s_ifchr);
  reg_var(intern(lit("s-ififo"), user_package), &s_ififo);
  reg_var(intern(lit("s-isuid"), user_package), &s_isuid);
  reg_var(intern(lit("s-isgid"), user_package), &s_isgid);
  reg_var(intern(lit("s-isvtx"), user_package), &s_isvtx);
  reg_var(intern(lit("s-irwxu"), user_package), &s_irwxu);
  reg_var(intern(lit("s-irusr"), user_package), &s_irusr);
  reg_var(intern(lit("s-iwusr"), user_package), &s_iwusr);
  reg_var(intern(lit("s-ixusr"), user_package), &s_ixusr);
  reg_var(intern(lit("s-irwxg"), user_package), &s_irwxg);
  reg_var(intern(lit("s-irgrp"), user_package), &s_irgrp);
  reg_var(intern(lit("s-iwgrp"), user_package), &s_iwgrp);
  reg_var(intern(lit("s-ixgrp"), user_package), &s_ixgrp);
  reg_var(intern(lit("s-irwxo"), user_package), &s_irwxo);
  reg_var(intern(lit("s-iroth"), user_package), &s_iroth);
  reg_var(intern(lit("s-iwoth"), user_package), &s_iwoth);
  reg_var(intern(lit("s-ixoth"), user_package), &s_ixoth);

  reg_fun(intern(lit("open-directory"), user_package), func_n1(open_directory));
  reg_fun(intern(lit("open-file"), user_package), func_n2o(open_file, 1));
  reg_fun(intern(lit("open-tail"), user_package), func_n3o(open_tail, 1));
  reg_fun(intern(lit("open-command"), user_package), func_n2o(open_command, 1));
  reg_fun(intern(lit("open-pipe"), user_package), func_n2(open_command));
  reg_fun(intern(lit("open-process"), user_package), func_n3o(open_process, 2));
  reg_fun(intern(lit("remove-path"), user_package), func_n1(remove_path));
  reg_fun(intern(lit("rename-path"), user_package), func_n2(rename_path));

  reg_var(intern(lit("*user-package*"), user_package), &user_package);
  reg_var(intern(lit("*keyword-package*"), user_package), &keyword_package);
  reg_var(intern(lit("*system-package*"), user_package), &system_package);
  reg_fun(intern(lit("make-sym"), user_package), func_n1(make_sym));
  reg_fun(intern(lit("gensym"), user_package), func_n1o(gensym, 0));
  reg_var(intern(lit("*gensym-counter*"), user_package), &gensym_counter);
  reg_fun(intern(lit("make-package"), user_package), func_n1(make_package));
  reg_fun(intern(lit("find-package"), user_package), func_n1(find_package));
  reg_fun(intern(lit("delete-package"), user_package), func_n1(delete_package));
  reg_fun(intern(lit("intern"), user_package), func_n2o(intern, 1));
  reg_fun(intern(lit("rehome-sym"), user_package), func_n2o(rehome_sym, 1));
  reg_fun(intern(lit("symbolp"), user_package), func_n1(symbolp));
  reg_fun(intern(lit("symbol-name"), user_package), func_n1(symbol_name));
  reg_fun(intern(lit("symbol-package"), user_package), func_n1(symbol_package));
  reg_fun(intern(lit("packagep"), user_package), func_n1(packagep));
  reg_fun(intern(lit("keywordp"), user_package), func_n1(keywordp));

  reg_fun(intern(lit("mkstring"), user_package), func_n2(mkstring));
  reg_fun(intern(lit("copy-str"), user_package), func_n1(copy_str));
  reg_fun(intern(lit("upcase-str"), user_package), func_n1(upcase_str));
  reg_fun(intern(lit("downcase-str"), user_package), func_n1(downcase_str));
  reg_fun(intern(lit("string-extend"), user_package), func_n2(string_extend));
  reg_fun(intern(lit("stringp"), user_package), func_n1(stringp));
  reg_fun(intern(lit("lazy-stringp"), user_package), func_n1(lazy_stringp));
  reg_fun(intern(lit("length-str"), user_package), func_n1(length_str));
  reg_fun(intern(lit("search-str"), user_package), func_n4o(search_str, 2));
  reg_fun(intern(lit("search-str-tree"), user_package), func_n4o(search_str_tree, 2));
  reg_fun(intern(lit("match-str"), user_package), func_n3o(match_str, 2));
  reg_fun(intern(lit("match-str-tree"), user_package), func_n3o(match_str_tree, 2));
  reg_fun(intern(lit("sub-str"), user_package), func_n3o(sub_str, 1));
  reg_fun(intern(lit("replace-str"), user_package), func_n4o(replace_str, 2));
  reg_fun(intern(lit("cat-str"), user_package), func_n2o(cat_str, 1));
  reg_fun(intern(lit("split-str"), user_package), func_n2(split_str));
  reg_fun(intern(lit("split-str-set"), user_package), func_n2(split_str_set));
  reg_fun(intern(lit("tok-str"), user_package), func_n3o(tok_str, 1));
  reg_fun(intern(lit("list-str"), user_package), func_n1(list_str));
  reg_fun(intern(lit("trim-str"), user_package), func_n1(trim_str));
  reg_fun(intern(lit("string-cmp"), user_package), func_n2(string_cmp));
  reg_fun(intern(lit("string-lt"), user_package), func_n2(string_lt));
  reg_fun(intern(lit("int-str"), user_package), func_n2o(int_str, 1));
  reg_fun(intern(lit("flo-str"), user_package), func_n1(flo_str));
  reg_fun(intern(lit("num-str"), user_package), func_n1(num_str));
  reg_fun(intern(lit("int-flo"), user_package), func_n1(int_flo));
  reg_fun(intern(lit("flo-int"), user_package), func_n1(flo_int));
  reg_fun(intern(lit("chrp"), user_package), func_n1(chrp));
  reg_fun(intern(lit("chr-isalnum"), user_package), func_n1(chr_isalnum));
  reg_fun(intern(lit("chr-isalpha"), user_package), func_n1(chr_isalpha));
  reg_fun(intern(lit("chr-isascii"), user_package), func_n1(chr_isascii));
  reg_fun(intern(lit("chr-iscntrl"), user_package), func_n1(chr_iscntrl));
  reg_fun(intern(lit("chr-isdigit"), user_package), func_n1(chr_isdigit));
  reg_fun(intern(lit("chr-isgraph"), user_package), func_n1(chr_isgraph));
  reg_fun(intern(lit("chr-islower"), user_package), func_n1(chr_islower));
  reg_fun(intern(lit("chr-isprint"), user_package), func_n1(chr_isprint));
  reg_fun(intern(lit("chr-ispunct"), user_package), func_n1(chr_ispunct));
  reg_fun(intern(lit("chr-isspace"), user_package), func_n1(chr_isspace));
  reg_fun(intern(lit("chr-isupper"), user_package), func_n1(chr_isupper));
  reg_fun(intern(lit("chr-isxdigit"), user_package), func_n1(chr_isxdigit));
  reg_fun(intern(lit("chr-toupper"), user_package), func_n1(chr_toupper));
  reg_fun(intern(lit("chr-tolower"), user_package), func_n1(chr_tolower));
  reg_fun(intern(lit("num-chr"), user_package), func_n1(num_chr));
  reg_fun(intern(lit("chr-num"), user_package), func_n1(chr_num));
  reg_fun(intern(lit("chr-str"), user_package), func_n2(chr_str));
  reg_fun(intern(lit("chr-str-set"), user_package), func_n3(chr_str_set));
  reg_fun(intern(lit("span-str"), user_package), func_n2(span_str));
  reg_fun(intern(lit("compl-span-str"), user_package), func_n2(compl_span_str));
  reg_fun(intern(lit("break-str"), user_package), func_n2(break_str));

  reg_fun(intern(lit("lazy-stream-cons"), user_package), func_n1(lazy_stream_cons));
  reg_fun(intern(lit("lazy-str"), user_package), func_n3o(lazy_str, 1));
  reg_fun(intern(lit("lazy-stringp"), user_package), func_n1(lazy_stringp));
  reg_fun(intern(lit("lazy-str-force-upto"), user_package), func_n2(lazy_str_force_upto));
  reg_fun(intern(lit("lazy-str-force"), user_package), func_n1(lazy_str_force));
  reg_fun(intern(lit("lazy-str-get-trailing-list"), user_package), func_n2(lazy_str_get_trailing_list));
  reg_fun(intern(lit("length-str->"), user_package), func_n2(length_str_gt));
  reg_fun(intern(lit("length-str->="), user_package), func_n2(length_str_ge));
  reg_fun(intern(lit("length-str-<"), user_package), func_n2(length_str_lt));
  reg_fun(intern(lit("length-str-<="), user_package), func_n2(length_str_le));

  reg_fun(intern(lit("vector"), user_package), func_n2o(vector, 1));
  reg_fun(intern(lit("vec"), user_package), func_n0v(vector_list));
  reg_fun(intern(lit("vectorp"), user_package), func_n1(vectorp));
  reg_fun(intern(lit("vec-set-length"), user_package), func_n2(vec_set_length));
  reg_fun(vecref_s, func_n2(vecref));
  reg_fun(intern(lit("vec-push"), user_package), func_n2(vec_push));
  reg_fun(intern(lit("length-vec"), user_package), func_n1(length_vec));
  reg_fun(intern(lit("size-vec"), user_package), func_n1(size_vec));
  reg_fun(vector_list_s, func_n1(vector_list));
  reg_fun(intern(lit("list-vector"), user_package), func_n1(list_vector));
  reg_fun(intern(lit("copy-vec"), user_package), func_n1(copy_vec));
  reg_fun(intern(lit("sub-vec"), user_package), func_n3o(sub_vec, 1));
  reg_fun(intern(lit("replace-vec"), user_package), func_n4o(replace_vec, 2));
  reg_fun(intern(lit("cat-vec"), user_package), func_n1(cat_vec));

  reg_fun(intern(lit("assoc"), user_package), func_n2(assoc));
  reg_fun(intern(lit("assql"), user_package), func_n2(assql));
  reg_fun(intern(lit("acons"), user_package), func_n3(acons));
  reg_fun(intern(lit("acons-new"), user_package), func_n3(acons_new));
  reg_fun(intern(lit("aconsql-new"), user_package), func_n3(aconsql_new));
  reg_fun(intern(lit("alist-remove"), user_package), func_n1v(alist_remove));
  reg_fun(intern(lit("alist-nremove"), user_package), func_n1v(alist_nremove));
  reg_fun(intern(lit("copy-cons"), user_package), func_n1(copy_cons));
  reg_fun(intern(lit("copy-alist"), user_package), func_n1(copy_alist));
  reg_fun(intern(lit("prop"), user_package), func_n2(getplist));
  reg_fun(intern(lit("merge"), user_package), func_n4o(merge, 3));
  reg_fun(intern(lit("sort"), user_package), func_n3o(sort, 2));
  reg_fun(intern(lit("find"), user_package), func_n4o(find, 2));
  reg_fun(intern(lit("multi-sort"), user_package), func_n3o(multi_sort, 2));
  reg_fun(intern(lit("find-if"), user_package), func_n3o(find_if, 2));
  reg_fun(intern(lit("set-diff"), user_package), func_n4o(set_diff, 2));

  reg_fun(intern(lit("length"), user_package), func_n1(length));

  reg_fun(intern(lit("sub"), user_package), func_n3o(sub, 1));
  reg_fun(intern(lit("ref"), user_package), func_n2(ref));
  reg_fun(intern(lit("refset"), user_package), func_n3(refset));
  reg_fun(intern(lit("replace"), user_package), func_n4o(replace, 2));
  reg_fun(intern(lit("update"), user_package), func_n2(update));

  reg_fun(intern(lit("symbol-value"), user_package), func_n1(symbol_value));
  reg_fun(intern(lit("symbol-function"), user_package), func_n1(symbol_function));
  reg_fun(intern(lit("boundp"), user_package), func_n1(boundp));
  reg_fun(intern(lit("fboundp"), user_package), func_n1(fboundp));
  reg_fun(intern(lit("func-get-form"), user_package), func_n1(func_get_form));
  reg_fun(intern(lit("func-get-env"), user_package), func_n1(func_get_env));
  reg_fun(intern(lit("func-set-env"), user_package), func_n2(func_set_env));
  reg_fun(intern(lit("functionp"), user_package), func_n1(functionp));
  reg_fun(intern(lit("interp-fun-p"), user_package), func_n1(interp_fun_p));

  reg_var(intern(lit("*random-state*"), user_package), &random_state);
  reg_fun(intern(lit("make-random-state"), user_package), func_n1(make_random_state));
  reg_fun(intern(lit("random-state-p"), user_package), func_n1(random_state_p));
  reg_fun(intern(lit("random-fixnum"), user_package), func_n1o(random_fixnum, 1));
  reg_fun(intern(lit("random"), user_package), func_n2(random));
  reg_fun(intern(lit("rand"), user_package), func_n2o(rnd, 1));

  reg_fun(intern(lit("range"), user_package), func_n0v(rangev));
  reg_fun(intern(lit("range*"), user_package), func_n0v(range_star_v));
  reg_fun(generate_s, func_n2(generate));
  reg_fun(intern(lit("repeat"), user_package), func_n1v(repeatv));
  reg_fun(intern(lit("force"), user_package), func_n1(force));
  reg_fun(intern(lit("rperm"), user_package), func_n2(rperm));
  reg_fun(intern(lit("perm"), user_package), func_n2o(perm, 1));
  reg_fun(intern(lit("comb"), user_package), func_n2(comb));
  reg_fun(intern(lit("rcomb"), user_package), func_n2(rcomb));

  reg_fun(throw_s, func_n1v(uw_throw));
  reg_fun(intern(lit("throwf"), user_package), func_n2v(uw_throwfv));
  reg_fun(error_s, func_n1v(uw_errorfv));

  reg_fun(intern(lit("match-fun"), user_package), func_n4(match_fun));

  reg_fun(intern(lit("url-encode"), user_package), func_n2o(url_encode, 1));
  reg_fun(intern(lit("url-decode"), user_package), func_n2o(url_decode, 1));

  reg_fun(intern(lit("time"), user_package), func_n0(time_sec));
  reg_fun(intern(lit("time-usec"), user_package), func_n0(time_sec_usec));
  reg_fun(intern(lit("time-string-local"), user_package), func_n2(time_string_local));
  reg_fun(intern(lit("time-string-utc"), user_package), func_n2(time_string_utc));
  reg_fun(intern(lit("time-fields-local"), user_package), func_n1(time_fields_local));
  reg_fun(intern(lit("time-fields-utc"), user_package), func_n1(time_fields_utc));
  reg_fun(intern(lit("make-time"), user_package), func_n7(make_time));
  reg_fun(intern(lit("make-time-utc"), user_package), func_n7(make_time_utc));

  reg_fun(intern(lit("errno"), user_package), func_n1o(errno_wrap, 0));
  reg_fun(intern(lit("exit"), user_package), func_n1(exit_wrap));
  reg_fun(intern(lit("usleep"), user_package), func_n1(usleep_wrap));

  reg_fun(intern(lit("env"), user_package), func_n0(env));
  reg_fun(intern(lit("env-hash"), user_package), func_n0(env_hash));
  reg_var(intern(lit("*args*"), user_package), &prog_args);
  reg_var(intern(lit("*full-args*"), user_package), &prog_args_full);

#if HAVE_DAEMON
  reg_fun(intern(lit("daemon"), user_package), func_n2(daemon_wrap));
#endif

#if HAVE_SYSLOG
  reg_var(intern(lit("log-pid"), user_package), &log_pid_v);
  reg_var(intern(lit("log-cons"), user_package), &log_cons_v);
  reg_var(intern(lit("log-ndelay"), user_package), &log_ndelay_v);
  reg_var(intern(lit("log-odelay"), user_package), &log_odelay_v);
  reg_var(intern(lit("log-nowait"), user_package), &log_nowait_v);
#ifdef LOG_PERROR
  reg_var(intern(lit("log-perror"), user_package), &log_perror_v);
#endif
  reg_var(intern(lit("log-user"), user_package), &log_user_v);
  reg_var(intern(lit("log-daemon"), user_package), &log_daemon_v);
  reg_var(intern(lit("log-auth"), user_package), &log_auth_v);
#ifdef LOG_AUTHPRIV
  reg_var(intern(lit("log-authpriv"), user_package), &log_authpriv_v);
#endif
  reg_var(intern(lit("log-emerg"), user_package), &log_emerg_v);
  reg_var(intern(lit("log-alert"), user_package), &log_alert_v);
  reg_var(intern(lit("log-crit"), user_package), &log_crit_v);
  reg_var(intern(lit("log-err"), user_package), &log_err_v);
  reg_var(intern(lit("log-warning"), user_package), &log_warning_v);
  reg_var(intern(lit("log-notice"), user_package), &log_notice_v);
  reg_var(intern(lit("log-info"), user_package), &log_info_v);
  reg_var(intern(lit("log-debug"), user_package), &log_debug_v);
  reg_fun(intern(lit("openlog"), user_package), func_n3o(openlog_wrap, 1));
  reg_fun(intern(lit("closelog"), user_package), func_n0(closelog_wrap));
  reg_fun(intern(lit("setlogmask"), user_package), func_n1(setlogmask_wrap));
  reg_fun(intern(lit("syslog"), user_package), func_n2v(syslog_wrap));
#endif

#if HAVE_POSIX_SIGS
  reg_fun(intern(lit("set-sig-handler"), user_package), func_n2(set_sig_handler));
  reg_fun(intern(lit("get-sig-handler"), user_package), func_n1(get_sig_handler));
  reg_fun(intern(lit("sig-check"), user_package), func_n0(sig_check));
  reg_var(intern(lit("sig-hup"), user_package), &sig_hup);
  reg_var(intern(lit("sig-int"), user_package), &sig_int);
  reg_var(intern(lit("sig-quit"), user_package), &sig_quit);
  reg_var(intern(lit("sig-ill"), user_package), &sig_ill);
  reg_var(intern(lit("sig-trap"), user_package), &sig_trap);
  reg_var(intern(lit("sig-abrt"), user_package), &sig_abrt);
  reg_var(intern(lit("sig-bus"), user_package), &sig_bus);
  reg_var(intern(lit("sig-fpe"), user_package), &sig_fpe);
  reg_var(intern(lit("sig-kill"), user_package), &sig_kill);
  reg_var(intern(lit("sig-usr1"), user_package), &sig_usr1);
  reg_var(intern(lit("sig-segv"), user_package), &sig_segv);
  reg_var(intern(lit("sig-usr2"), user_package), &sig_usr2);
  reg_var(intern(lit("sig-pipe"), user_package), &sig_pipe);
  reg_var(intern(lit("sig-alrm"), user_package), &sig_alrm);
  reg_var(intern(lit("sig-term"), user_package), &sig_term);
  reg_var(intern(lit("sig-chld"), user_package), &sig_chld);
  reg_var(intern(lit("sig-cont"), user_package), &sig_cont);
  reg_var(intern(lit("sig-stop"), user_package), &sig_stop);
  reg_var(intern(lit("sig-tstp"), user_package), &sig_tstp);
  reg_var(intern(lit("sig-ttin"), user_package), &sig_ttin);
  reg_var(intern(lit("sig-ttou"), user_package), &sig_ttou);
  reg_var(intern(lit("sig-urg"), user_package), &sig_urg);
  reg_var(intern(lit("sig-xcpu"), user_package), &sig_xcpu);
  reg_var(intern(lit("sig-xfsz"), user_package), &sig_xfsz);
  reg_var(intern(lit("sig-vtalrm"), user_package), &sigtalrm);
  reg_var(intern(lit("sig-prof"), user_package), &sig_prof);
  reg_var(intern(lit("sig-poll"), user_package), &sig_poll);
  reg_var(intern(lit("sig-sys"), user_package), &sig_sys);
#ifdef SIGWINCH
  reg_var(intern(lit("sig-winch"), user_package), &sig_winch);
#endif
#ifdef SIGIOT
  reg_var(intern(lit("sig-iot"), user_package), &sig_iot);
#endif
#ifdef SIGSTKFLT
  reg_var(intern(lit("sig-stkflt"), user_package), &sig_stkflt);
#endif
#ifdef SIGIO
  reg_var(intern(lit("sig-io"), user_package), &sig_io);
#endif
#ifdef SIGLOST
  reg_var(intern(lit("sig-lost"), user_package), &sig_lost);
#endif
#ifdef SIGPWR
  reg_var(intern(lit("sig-pwr"), user_package), &sig_pwr);
#endif
#endif

  reg_fun(intern(lit("source-loc"), user_package), func_n1(source_loc));
  reg_fun(intern(lit("source-loc-str"), user_package), func_n1(source_loc_str));
  reg_fun(intern(lit("rlcp"), user_package), func_n2(rlcp));

  reg_var(intern(lit("*self-path*"), user_package), &self_path);

  eval_error_s = intern(lit("eval-error"), user_package);
  uw_register_subtype(eval_error_s, error_s);
}
