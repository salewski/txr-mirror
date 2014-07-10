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
#include "txr.h"
#include "combi.h"
#include "eval.h"

typedef val (*opfun_t)(val, val);
typedef val (*mefun_t)(val, val);

struct c_var {
  val *loc;
  val bind;
};

val top_vb, top_fb, top_mb, top_smb, special;
val op_table;
val dyn_env;

val eval_error_s;
val dwim_s, progn_s, prog1_s, let_s, let_star_s, lambda_s, call_s;
val cond_s, if_s, defvar_s, defun_s, defmacro_s, tree_case_s, tree_bind_s;
val inc_s, dec_s, push_s, pop_s, flip_s, gethash_s, car_s, cdr_s, not_s;
val del_s, vecref_s;
val for_s, for_star_s, each_s, each_star_s, collect_each_s, collect_each_star_s;
val append_each_s, append_each_star_s;
val dohash_s;
val uw_protect_s, return_s, return_from_s;
val list_s, append_s, apply_s, iapply_s;
val gen_s, gun_s, generate_s, rest_s, plus_s;
val promise_s, op_s, identity_s, apf_s, ipf_s;
val hash_lit_s, hash_construct_s;
val vector_lit_s, vector_list_s;
val macro_time_s, with_saved_vars_s, macrolet_s;
val defsymacro_s, symacrolet_s, prof_s;

val special_s, whole_k;

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

/*
 * Wrapper for performance reasons: don't make make_env
 * process default arguments.
 */
static val make_env_intrinsic(val vbindings, val fbindings, val up_env)
{
  vbindings = default_bool_arg(vbindings);
  fbindings = default_bool_arg(fbindings);
  up_env = default_bool_arg(up_env);
  return make_env(vbindings, fbindings, up_env);
}

val env_fbind(val env, val sym, val fun)
{
  val cell;
  type_check(env, ENV);
  cell = acons_new_c(sym, nulloc, mkloc(env->e.fbindings, env));
  rplacd(cell, fun);
  return cell;
}

val env_vbind(val env, val sym, val obj)
{
  val cell;
  type_check(env, ENV);
  cell = acons_new_c(sym, nulloc, mkloc(env->e.vbindings, env));
  rplacd(cell, obj);
  return cell;
}

noreturn static val eval_error(val form, val fmt, ...)
{
  va_list vl;
  val stream = make_string_output_stream();

  va_start (vl, fmt);

  if (!form)
    form = last_form_evaled;

  if (form)
    format(stream, lit("(~a) "), source_loc_str(form), nao);
  (void) vformat(stream, fmt, vl);
  va_end (vl);

  uw_throw(eval_error_s, get_string_from_stream(stream));
  abort();
}

val lookup_var(val env, val sym)
{
  if (env) {
    type_check(env, ENV);

    for (; env; env = env->e.up_env) {
      val binding = assoc(sym, env->e.vbindings);
      if (binding)
        return binding;
    }
  }

  for (env = dyn_env; env; env = env->e.up_env) {
    val binding = assoc(sym, env->e.vbindings);
    if (binding)
      return binding;
  } 
    
  return(gethash(top_vb, sym));
}

static val lookup_sym_lisp1(val env, val sym)
{
  uses_or2;

  if (env) {
    type_check(env, ENV);

    for (; env; env = env->e.up_env) {
      val binding = or2(assoc(sym, env->e.vbindings),
                        assoc(sym, env->e.fbindings));
      if (binding)
        return binding;
    }
  }

  for (env = dyn_env; env; env = env->e.up_env) {
      val binding = or2(assoc(sym, env->e.vbindings),
                        assoc(sym, env->e.fbindings));
    if (binding)
      return binding;
  } 

  return or2(gethash(top_vb, sym), gethash(top_fb, sym));
}

loc lookup_var_l(val env, val sym)
{
  if (env) {
    type_check(env, ENV);

    for (; env; env = env->e.up_env) {
      val binding = assoc(sym, env->e.vbindings);
      if (binding)
        return cdr_l(binding);
    }
  }

  for (env = dyn_env; env; env = env->e.up_env) {
    val binding = assoc(sym, env->e.vbindings);
    if (binding)
      return cdr_l(binding);
  }

  {
    val binding = gethash(top_vb, sym);
    return (binding) ? cdr_l(binding) : nulloc;
  }
}

val lookup_fun(val env, val sym)
{
  if (nilp(env)) {
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

static val lookup_mac(val menv, val sym)
{
  if (nilp(menv)) {
    return gethash(top_mb, sym);
  } else  {
    type_check(menv, ENV);

    {
      val binding = assoc(sym, menv->e.fbindings);
      if (binding)
        return binding;
      return lookup_mac(menv->e.up_env, sym);
    }
  }
}

static val lookup_symac(val menv, val sym)
{
  if (nilp(menv)) {
    return gethash(top_smb, sym);
  } else  {
    type_check(menv, ENV);

    {
      val binding = assoc(sym, menv->e.vbindings);
      if (binding) /* special_s: see make_var_shadowing_env */
        return (cdr(binding) == special_s) ? nil : binding;
      return lookup_symac(menv->e.up_env, sym);
    }
  }
}

static void mark_special(val sym)
{
  sethash(special, sym, t);
}

static val special_p(val sym)
{
  return gethash(special, sym);
}

static val env_vbind_special(val env, val sym, val obj,
                             val special_list, val ctx_form)
{
  if (special_list && memq(sym, special_list)) {
    if (dyn_env)
      return env_vbind(dyn_env, sym, obj);
    internal_error("cannot rebind special var: there is no dynamic env");
  } else {
    return env_vbind(env, sym, obj);
  }
}

static val bind_args(val env, val params, val args, val ctx_form)
{
  val new_env = make_env(nil, nil, env);
  val optargs = nil;
  val special_list = nil;

  for (; args && consp(params); params = cdr(params)) {
    val param = car(params);
    val initform = nil;
    val presentsym = nil;

    if (param == colon_k) {
      if (optargs)
        goto twocol;
      optargs = t;
      continue;
    }

    if (consp(param)) {
      val sym = pop(&param);
      if (optargs) {
        initform = pop(&param);
        presentsym = pop(&param);
        param = sym;
      } else if (sym == special_s) {
        special_list = param;
        continue;
      } else {
        eval_error(ctx_form, lit("~s: bad object ~s in param list"),
                   car(ctx_form), sym, nao);
      }
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
      env_vbind_special(new_env, param, initval, special_list, ctx_form);
      if (presentsym)
        env_vbind_special(new_env, presentsym, present, special_list, ctx_form);
    } else {
      env_vbind_special(new_env, param, car(args), special_list, ctx_form);
    }

    args = cdr(args);
  }

  if (bindable(params)) {
    env_vbind_special(new_env, params, args, special_list, ctx_form);
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
        env_vbind_special(new_env, sym, initval, special_list, ctx_form);
        if (presentsym) {
          if (!bindable(presentsym)) 
            eval_error(ctx_form, lit("~s: ~s is not a bindable symbol"),
                       car(ctx_form), presentsym, nao);
          env_vbind_special(new_env, presentsym, nil, special_list, ctx_form);
        }
      } else {
        env_vbind_special(new_env, param, nil, special_list, ctx_form);
      }
      params = cdr(params);
    }
    if (bindable(params))
      env_vbind_special(new_env, params, nil, special_list, ctx_form);
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

static val expand_opt_params_rec(val params, val menv, val *pspecials)
{
  if (atom(params)) {
    if (special_p(params))
      push(params, pspecials);
    return params;
  } else {
    val form = car(params);
    if (atom(form) || !consp(cdr(form))) { /* sym, or no init form */
      val params_ex = expand_opt_params_rec(cdr(params), menv, pspecials);
      if (special_p(form))
        push(form, pspecials);
      if (params_ex == cdr(params))
        return params;
      return rlcp(cons(form, params_ex), cdr(params));
    } else { /* has initform */
      val sym = car(form);
      val initform = car(cdr(form));
      val initform_ex = rlcp(expand(initform, menv), initform);
      val form_ex = rlcp(cons(car(form), cons(initform_ex, cdr(cdr(form)))),
                         form);
      if (special_p(sym))
        push(sym, pspecials);
      return rlcp(cons(form_ex, expand_opt_params_rec(rest(params),
                                                      menv, pspecials)),
                  cdr(params));
    } 
  }
}

static val expand_params_rec(val params, val menv, val *pspecials)
{
  if (atom(params)) {
    if (special_p(params))
      push(params, pspecials);
    return params;
  } else if (car(params) == colon_k) {
    val params_ex = expand_opt_params_rec(cdr(params), menv, pspecials);
    if (params_ex == cdr(params))
      return params;
    return rlcp(cons(colon_k, params_ex), cdr(params));
  } else if (consp(car(params))) {
    val car_ex = expand_params_rec(car(params), menv, pspecials);
    val params_ex = expand_params_rec(cdr(params), menv, pspecials);
    if (car_ex == car(params) && params_ex == cdr(params))
      return params;
    return rlcp(cons(car_ex, params_ex), params);
  } else {
    val params_ex = expand_params_rec(cdr(params), menv, pspecials);
    if (params_ex == cdr(params))
      return params;
    return rlcp(cons(car(params), params_ex), cdr(params));
  }
}

static val expand_params(val params, val menv)
{
  val specials = nil;
  val params_ex = expand_params_rec(params, menv, &specials);
  return if3(specials,
             rlcp(cons(cons(special_s, specials), params_ex), params_ex),
             params_ex);
}


static val get_opt_param_syms(val params)
{
  if (bindable(params)) {
    return cons(params, nil);
  } else if (atom(params)) {
    return nil;
  } else {
    val form = car(params);

    if (atom(form) || !consp(cdr(form))) { /* sym, or no init form */
      val rest_syms = get_opt_param_syms(cdr(params));
      if (bindable(form))
        return cons(form, rest_syms);
      if (bindable(car(form)))
        return cons(car(form), rest_syms);
      return rest_syms;
    } else { /* has initform */
      val sym = car(form);
      return cons(sym, get_opt_param_syms(cdr(params)));
    } 
  }
}

static val get_param_syms(val params)
{
  if (bindable(params)) {
    return cons(params, nil);
  } else if (atom(params)) {
    return nil;
  } else if (car(params) == colon_k) {
    return get_opt_param_syms(cdr(params));
  } else if (consp(car(params))) {
    return nappend2(get_param_syms(car(params)),
                    get_param_syms(cdr(params)));
  } else if (bindable(car(params))) {
    return cons(car(params), get_param_syms(cdr(params)));
  } else {
    return get_param_syms(cdr(params));
  }
}

val apply(val fun, val arglist, val ctx_form)
{
  val arg[32], *p = arg;
  int variadic, fixparam, reqargs, nargs;
  val ctx = if3(ctx_form, car(ctx_form), apply_s);

  if (fun && symbolp(fun)) {
    val binding = gethash(top_fb, fun);
    if (!binding)
      eval_error(ctx_form, lit("~s: no such function ~s"), ctx, fun, nao);
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
                 (lit("~s: arglist ~s is not a list"), ctx,
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
                 ctx, nao);

    if (nargs > fixparam)
      eval_error(ctx_form, lit("~s: too many arguments"),
                 ctx, nao);

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
      eval_error(ctx_form, lit("~s: missing required arguments"), ctx, nao);

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
  if (!cdr(args)) {
    return car(args);
  } else {
    list_collect_decl (out, ptail);

    for (; cdr(args); args = cdr(args))
      ptail = list_collect(ptail, car(args));

    list_collect_nconc(ptail, car(args));
    return out;
  }
}

val apply_intrinsic(val fun, val args)
{
  return apply(fun, apply_frob_args(args), nil);
}

static val iapply(val fun, val args)
{
  list_collect_decl (mod_args, ptail);
  loc saved_ptail;

  for (; cdr(args); args = cdr(args))
    ptail = list_collect(ptail, car(args));

  saved_ptail = ptail;

  ptail = list_collect_nconc(ptail, car(args));

  {
    loc pterm = term(ptail);
    val tatom = deref(pterm);

    if (tatom) {
      deref(ptail) = nil;
      ptail = list_collect_nconc(saved_ptail, copy_list(car(args)));
      set(term(ptail), cons(tatom, nil));
    }
  }

  return apply(fun, mod_args, nil);
}

static val call(val fun, val args)
{
  return apply(fun, args, cons(apply_s, nil));
}

static val list_star_intrinsic(val args)
{
  return apply_frob_args(args);
}

static val bind_macro_params(val env, val menv, val params, val form,
                             val loose_p, val ctx_form)
{
  val new_env = make_env(nil, nil, env);
  val err_sym = nil;
  val whole = form;
  val optargs = nil;
  val specials = nil;

  if (consp(params)) {
    val head = car(params);
    if (consp(head)) {
      val sym = car(head);
      if (sym == special_s) {
        specials = cdr(head);
        params = cdr(params);
      }
    }
  }

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
      env_vbind_special(new_env, nparam, if3(param == whole_k, whole, menv),
                        specials, ctx_form);
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
        env_vbind_special(new_env, param, car(form), specials, ctx_form);
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

          new_env = bind_macro_params(new_env, menv,
                                      nparam, car(form), t, ctx_form);

          if (presentsym)
            env_vbind_special(new_env, presentsym, t, specials, ctx_form);
        } else {
          new_env = bind_macro_params(new_env, menv,
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
      env_vbind_special(new_env, param, nil, specials, ctx_form);
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
        new_env = bind_macro_params(new_env, menv,
                                    nparam, initval, t, ctx_form);
      } else {
        new_env = bind_macro_params(new_env, menv,
                                    nparam, nil, t, ctx_form);
      }

      if (presentsym)
        env_vbind_special(new_env, presentsym, nil, specials, ctx_form);
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
    env_vbind_special(new_env, params, form, specials, ctx_form);
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

static val set_dyn_env(val de)
{
  val old = dyn_env;
  dyn_env = de;
  return old;
}

val interp_fun(val env, val fun, val args)
{
  val def = cdr(fun);
  val params = car(def);
  val body = cdr(def);
  val saved_de = set_dyn_env(make_env(nil, nil, dyn_env));
  val fun_env = bind_args(env, params, args, fun);
  val ret = eval_progn(body, fun_env, body);
  set_dyn_env(saved_de);
  return ret;
}

val eval_intrinsic(val form, val env)
{
  form = expand(form, nil);
  return eval(form, default_bool_arg(env), form);
}

static val do_eval(val form, val env, val ctx_form,
                   val (*lookup)(val env, val sym))
{
  debug_enter;

  debug_check(consp(form) ? form : ctx_form, env, nil, nil, nil, nil);

  if (nilp(form)) {
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

    if (regexp(oper))
      debug_return (oper);

    {
      val entry = gethash(op_table, oper);

      if (entry) {
        opfun_t fp = (opfun_t) cptr_get(entry);
        last_form_evaled = form;
        debug_return (fp(form, env));
      } else {
        val fbinding = lookup_fun(env, oper);
        if (!fbinding) {
          last_form_evaled = form;
          eval_error(form, lit("no such function or operator: ~s"), oper, nao);
          abort();
        } else {
          val args = do_eval_args(rest(form), env, form, &lookup_var);
          debug_frame(oper, args, nil, env, nil, nil, nil);
          last_form_evaled = form;
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


static val bindings_helper(val vars, val env, val sequential,
                           val *env_out, val ret_new_bindings,
                           val ctx_form)
{
  val iter;
  val de = if3(sequential, dyn_env, make_env(nil, nil, dyn_env));
  val ne = if3(sequential, env, make_env(nil, nil, env));
  list_collect_decl (new_bindings, ptail);

  for (iter = vars; iter; iter = cdr(iter)) {
    val item = car(iter);
    val var, value = nil;

    if (consp(item)) {
      var = pop(&item);
      value = eval(pop(&item), if3(sequential, ne, env), ctx_form);
    } else {
      var = item;
    }

    if (var == special_s) {
      val special = car(item);
      val binding = env_vbind(de, special, value);
      if (ret_new_bindings)
        ptail = list_collect (ptail, binding);
    } else if (bindable(var)) {
      val le = if3(sequential, make_env(nil, nil, ne), ne);
      val binding = env_vbind(le, var, value);
      if (ret_new_bindings)
        ptail = list_collect (ptail, binding);
      ne = le;
    } else {
      eval_error(ctx_form, lit("~s: ~s is not a bindable symbol"),
                 car(ctx_form), var, nao);
    }
  }

  if (de != dyn_env)
    dyn_env = de;
  if (env_out)
    *env_out = ne;
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
  val new_env;
  (void) bindings_helper(vars, env, eq(let, let_star_s), &new_env, nil, form);
  return eval_progn(body, new_env, form);
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
  val new_env;
  val new_bindings = bindings_helper(vars, env, star, &new_env, t, form);
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
      val res = eval_progn(body, new_env, form);
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
      remhash(top_smb, sym);
      sethash(top_vb, sym, cons(sym, value));
    }
    mark_special(sym);
  }

  return sym;
}

static val op_defsymacro(val form, val env)
{
  val args = rest(form);
  val sym = first(args);

  (void) env;

  if (!bindable(sym))
    eval_error(form, lit("let: ~s is not a bindable symbol"), sym, nao);

  remhash(top_vb, sym);
  sethash(top_smb, sym, cons(sym, second(args)));
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

  if (gethash(op_table, name))
    eval_error(form, lit("defun: ~s is a special operator"), name, nao);

  remhash(top_mb, name);

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
    if (!bindable(param)) {
      if (consp(param) && car(param) == special_s)
        continue; /* special vars list */
      eval_error(form, lit("defun: parameter ~s is not a bindable symbol"), param, nao);
    }
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

  if (gethash(op_table, name))
    eval_error(form, lit("defmacro: ~s is a special operator"), name, nao);

  remhash(top_fb, name);
  /* defmacro captures lexical environment, so env is passed */
  sethash(top_mb, name, cons(name, cons(env, cons(params, cons(block, nil)))));
  return name;
}

static val expand_macro(val form, val expander, val menv)
{
  if (cobjp(expander)) {
    mefun_t fp = (mefun_t) cptr_get(expander);
    return fp(form, menv);
  } else {
    debug_enter;
    val name = car(form);
    val args = rest(form);
    val env = car(cdr(expander));
    val params = car(cdr(cdr(expander)));
    val body = cdr(cdr(cdr(expander)));
    val saved_de = set_dyn_env(make_env(nil, nil, dyn_env));
    val exp_env = bind_macro_params(env, menv, params, args, nil, form);
    debug_frame(name, args, nil, env, nil, nil, nil);
    debug_return(eval_progn(body, exp_env, body));
    debug_end;
    set_dyn_env(saved_de); /* not reached but shuts up compiler */
    debug_leave;
  }
}

static val maybe_progn(val forms)
{
  return if3(cdr(forms), cons(progn_s, forms), car(forms));
}

static val self_evaluating_p(val form)
{
  if (nilp(form) || form == t)
    return t;

  if (symbolp(form))
    return if2(keywordp(form), t);

  if (atom(form))
    return t;

  return nil;
}

static val maybe_quote(val form)
{
  if (self_evaluating_p(form))
    return form;
  return cons(quote_s, cons(form, nil));
}

static val expand_macrolet(val form, val menv)
{
  val body = cdr(form);
  val macs = pop(&body);
  val new_env = make_env(nil, nil, menv);

  for (; macs; macs = cdr(macs)) {
    val macro = car(macs);
    val name = pop(&macro);
    val params = expand_params(pop(&macro), menv);
    val macro_ex = expand_forms(macro, menv);
    val block = cons(block_s, cons(name, macro_ex));

    /* We store the macrolet in the same form as a top level defmacro,
     * so they can be treated uniformly. The nil after the name is
     * the ordinary lexical environment: a macrolet doesn't capture that.
     */
    env_fbind(new_env, name, cons(nil, cons(params, cons(block, nil))));
  }

  return maybe_progn(expand_forms(body, new_env));
}

static val expand_symacrolet(val form, val menv)
{
  val body = cdr(form);
  val symacs = pop(&body);
  val new_env = make_env(nil, nil, menv);

  for (; symacs; symacs = cdr(symacs)) {
    val macro = car(symacs);
    val name = pop(&macro);
    val repl = pop(&macro);
    val repl_ex = expand(repl, menv);
    env_vbind(new_env, name, repl_ex);
  }

  return maybe_progn(expand_forms(body, new_env));
}

/*
 * Generate a symbol macro environment in which every
 * variable in the binding list vars is listed
 * as a binding, with the value sys:special.
 * This is a shadow entry, which allows ordinary
 * bindings to shadow symbol macro bindings.
 */
static val make_var_shadowing_env(val menv, val vars)
{
  if (nilp(vars)) {
    return menv;
  } else {
    list_collect_decl (shadows, ptail);

    for (; vars; vars = cdr(vars)) {
      val var = car(vars);

      ptail = list_collect(ptail, cons(if3(consp(var),
                                           car(var),
                                           var), special_s));
    }

    return make_env(shadows, nil, menv);
  }
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

static val expand_tree_cases(val cases, val menv)
{
  if (atom(cases)) {
    return cases;
  } else {
    val onecase = car(cases);

    if (atom(onecase)) {
      val rest_ex = expand_tree_cases(cdr(cases), menv);
      if (rest_ex == cdr(cases))
        return cases;
      return rlcp(cons(onecase, rest_ex), cases);
    } else {
      val dstr_args = car(onecase);
      val forms = cdr(onecase);
      val new_menv = make_var_shadowing_env(menv, get_param_syms(dstr_args));
      val dstr_args_ex = expand_params(dstr_args, menv);
      val forms_ex = expand_forms(forms, new_menv);
      val rest_ex = expand_tree_cases(cdr(cases), menv);

      if (dstr_args_ex == dstr_args && forms_ex == forms &&
          rest_ex == cdr(cases))
        return cases;

      return rlcp(cons(cons(dstr_args_ex, forms_ex), rest_ex), cases);
    }
  }
}

static val expand_tree_case(val form, val menv)
{
  val sym = first(form);
  val expr = second(form);
  val tree_cases = rest(rest(form));
  val expr_ex = expand(expr, menv);
  val tree_cases_ex = expand_tree_cases(tree_cases, menv);

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

static loc dwim_loc(val form, val env, val op, val newform, val *retval)
{
  val evargs = eval_args_lisp1(rest(form), env, form);
  val obj = first(evargs);
  val args = rest(evargs);

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
        cons_bind (from, to, index);

        if (listp(to)) {
          from = index;
          to = colon_k;
        }

        if (op == set_s) {
          val newval = eval(newform, env, form);
          replace_str(obj, newval, from, to);
          *retval = newval;
        } else if (op == del_s) {
          *retval = sub_str(obj, from, to);
          replace_str(obj, nil, from, to);
        } else {
          eval_error(form, lit("[~s ~s]: ranges and index lists allow only set and del operators"),
                     obj, index, nao);
        }

        return nulloc;
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
        return nulloc;
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
        cons_bind (from, to, index);

        if (listp(to)) {
          from = index;
          to = colon_k;
        }

        if (op == set_s) {
          val newval = eval(newform, env, form);
          replace_vec(obj, newval, from, to);
          *retval = newval;
        } else if (op == del_s) {
          *retval = sub_vec(obj, from, to);
          replace_vec(obj, nil, from, to);
        } else {
          eval_error(form, lit("[~s ~s]: ranges allow only set and del operators"),
                     obj, index, nao);
        }
        return nulloc;
      } else {
        if (op == del_s) {
          *retval = vecref(obj, index);
          replace_vec(obj, nil, index, plus(index, one));
          return nulloc;
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
          return nulloc;
        }
        return listref_l(obj, index);
      } else if (consp(index)) {
        val newlist;
        val tempform;
        cons_bind (from, to, index);

        if (listp(to)) {
          from = index;
          to = colon_k;
        }

        if (op == set_s) {
          val newval = eval(newform, env, form);
          newlist = replace_list(obj, newval, from, to);
          tempform = list(op, second(form), 
                          cons(quote_s, cons(newlist, nil)), nao);
          op_modplace(tempform, env);
          *retval = newval;
        } else if (op == del_s) {
          *retval = sub_list(obj, from, to);
          newlist = replace_list(obj, nil, from, to);
          tempform = list(op, second(form), 
                          cons(quote_s, cons(newlist, nil)), nao);
          op_modplace(tempform, env);
        } else {
          eval_error(form, lit("[~s ~s]: ranges allow only set and del operators"),
                     obj, index, nao);
        }
        return nulloc;
      } else {
        eval_error(form, lit("[~s ~s]: index must be integer, or pair"),
                   cell, index, nao);
      }
    }
  case COBJ:
    {
      if (hashp(obj)) {
        val new_p;
        loc place;
        if (lt(length(args), one))
          eval_error(form, lit("[~s ...]: hash indexing needs at least one arg"),
                     obj, nao);

        if (op == del_s) {
          *retval = gethash(obj, first(args));
          remhash(obj, first(args));
          return nulloc;
        }

        place = gethash_l(obj, first(args), mkcloc(new_p));
        if (new_p)
          set(place, second(args));
        return place;
      }
    }
  default:
    eval_error(form, lit("object ~s not supported by [] notation"), obj, nao);
  }

  return nulloc;
}

static val op_modplace(val form, val env)
{
  uses_or2;
  val op = first(form);
  val place = second(form);
  val third_arg_p = rest(rest(form));
  val newform = if3(car(third_arg_p), third(form), nil);
  val newval = nil;
  loc ptr = nulloc;

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
    ptr = lookup_var_l(env, place);
    if (nullocp(ptr))
      eval_error(form, lit("unbound variable ~s"), place, nao);
  } else if (consp(place)) {
    /* TODO: dispatch these with hash table. */
    val sym = car(place);
    if (sym == dwim_s) {
      val ret = nil;
      ptr = dwim_loc(place, env, op, newform, &ret);
      if (nullocp(ptr))
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
      ptr = gethash_l(hash, key, mkcloc(new_p));
      if (new_p)
        set(ptr, eval(fourth(place), env, form));
    } else if (sym == car_s) {
      val cons = eval(second(place), env, form);
      ptr = car_l(cons);
    } else if (sym == cdr_s) {
      val cons = eval(second(place), env, form);
      ptr = cdr_l(cons);
    } else if (sym == vecref_s) {
      val vec = eval(second(place), env, form);
      val ind = eval(third(place), env, form);
      ptr = vecref_l(vec, ind);
    } else {
      eval_error(form, lit("~s: ~s is not a recognized place form"),
                 op, place, nao);
    }
  } else {
    eval_error(form, lit("~s: ~s is not a place"), op, place, nao);
  }

  if (nullocp(ptr))
    eval_error(form, lit("~s: place ~s doesn't exist"), op, place, nao);

  if (op == set_s) {
    if (!third_arg_p)
      eval_error(form, lit("~s: missing argument"), op, nao);
    return set(ptr, eval(newform, env, form));
  } else if (op == inc_s) {
    val inc = or2(eval(newform, env, form), one);
    return set(ptr, plus(deref(ptr), inc));
  } else if (op == dec_s) {
    val inc = or2(eval(newform, env, form), one);
    return set(ptr, minus(deref(ptr), inc));
  } else if (op == push_s) {
    return mpush(newval, ptr);
  } else if (op == pop_s) {
    if (third_arg_p)
      eval_error(form, lit("~s: superfluous argument"), op, nao);
    return pop(valptr(ptr));
  } else if (op == flip_s) {
    return deref(ptr) = null(deref(ptr));
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
  val new_env;
  val new_bindings = bindings_helper(vars, env, eq(forsym, for_star_s),
                                     &new_env, t, form);
  uw_block_begin (nil, result);

  (void) new_bindings;

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
    deref(cdr_l(keyvar)) = car(cell);
    deref(cdr_l(valvar)) = cdr(cell);
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
  val args = eval_args_lisp1(cdr(form), env, form);
  return apply(car(args), cdr(args), form);
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
        val saved_de = set_dyn_env(make_env(nil, nil, dyn_env));
        val clause_env = bind_args(env, params, if3(listp(exvals),
                                                    exvals, cons(exvals, nil)),
                                   clause);
        result = eval_progn(rest(rest(clause)), clause_env, clause);
        set_dyn_env(saved_de);
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
      eval_error(forms, lit("unbound variable ~s"), form, nao);
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

static val op_with_saved_vars(val form, val env)
{
  val prot_form = second(form);
  val saved_de = set_dyn_env(make_env(nil, nil, dyn_env));
  val result = eval(prot_form, env, prot_form);
  set_dyn_env(saved_de);
  return result;
}

static val op_prof(val form, val env)
{
  clock_t start_time = clock();
  alloc_bytes_t start_mlbytes = malloc_bytes;
  alloc_bytes_t start_gcbytes = gc_bytes;
  val result = eval_progn(rest(form), env, form);
  return list(result,
              num(malloc_bytes - start_mlbytes),
              num(gc_bytes - start_gcbytes),
              trunc(mul(num(clock() - start_time), num_fast(1000)), num_fast(CLOCKS_PER_SEC)),
              nao);
}

static val me_gen(val form, val menv)
{
  (void) menv;
  return list(generate_s,
              list(lambda_s, nil, second(form), nao),
              list(lambda_s, nil, third(form), nao), nao);
}

static val me_gun(val form, val menv)
{
  val var = gensym(nil);
  val expr = second(form);
  (void) menv;
  return list(let_s, cons(var, nil),
              list(gen_s, list(set_s, var, expr, nao), var, nao), nao);
}

static val me_delay(val form, val menv)
{
  (void) menv;
  return list(cons_s,
              cons(quote_s, cons(promise_s, nil)),
              cons(lambda_s, cons(nil, rest(form))), nao);
}

static val me_pprof(val form, val menv)
{
  val retval = gensym(lit("retval-"));
  val mbytes = gensym(lit("mal-bytes-"));
  val gcbytes = gensym(lit("gc-bytes-"));
  val msecs = gensym(lit("usecs-"));

  (void) menv;
  return list(tree_bind_s,
              list(retval, mbytes, gcbytes, msecs, nao),
              cons(prof_s, rest(form)),
              list(format_s, t, lit("malloc bytes:  ~12a\n"
                                    "gc heap bytes: ~12a\n"
                                    "total:         ~12a\n"
                                    "milliseconds:  ~12a\n"),
                   mbytes, gcbytes,
                   list(plus_s, mbytes, gcbytes, nao),
                   msecs, nao),
              retval,
              nao);
}

static val me_when(val form, val menv)
{
  (void) menv;
  return cons(cond_s, cons(rest(form), nil));
}

static val me_unless(val form, val menv)
{
  (void) menv;
  return list(if_s, second(form), nil, cons(progn_s, rest(rest(form))), nao);
}

static val me_while(val form, val menv)
{
  (void) menv;
  return apply_frob_args(list(for_s, nil, cons(second(form), nil), nil,
                              rest(rest(form)), nao));
}

static val me_until(val form, val menv)
{
  val inv = cons(not_s, cons(second(form), nil));
  (void) menv;
  return apply_frob_args(list(for_s, nil, cons(inv, nil), nil,
                              rest(rest(form)), nao));
}

static val me_quasilist(val form, val menv)
{
  return cons(list_s, cdr(form));
}

val expand_forms(val form, val menv)
{
  if (atom(form)) {
    return form;
  } else {
    val f = car(form);
    val r = cdr(form);
    val ex_f = expand(f, menv);
    val ex_r = expand_forms(r, menv);

    if (ex_f == f && ex_r == r)
      return form;
    return rlcp(cons(ex_f, ex_r), form);
  }
}

static val expand_cond_pairs(val form, val menv)
{
  if (atom(form)) {
    return form;
  } else {
    val pair = first(form);
    val others = rest(form);
    val pair_ex = expand_forms(pair, menv);
    val others_ex = expand_cond_pairs(others, menv);

    if (pair_ex == pair && others_ex == others)
      return form;
    return rlcp(cons(pair_ex, others_ex), form);
  }
}

static val expand_qquote(val qquoted_form, val menv,
                         val qq, val unq, val spl)
{
  if (nilp(qquoted_form)) {
    return nil;
  } if (atom(qquoted_form)) {
    return rlcp(cons(quote_s, cons(qquoted_form, nil)), qquoted_form);
  } else {
    val sym = car(qquoted_form);

    if (sym == spl) {
      val error_msg = if3(spl == sys_splice_s,
                          lit("the splice ,*~s cannot occur as an atom "
                              "or in the dotted position of a list"),
                          lit("(splice ~s) cannot occur as an atom "
                              "or in the dotted position of a list"));
        eval_error(qquoted_form, error_msg,
                   second(qquoted_form), nao);
    } else if (sym == unq) {
      return expand(second(qquoted_form), menv);
    } else if (sym == qq) {
      return rlcp(expand_qquote(expand_qquote(second(qquoted_form),
                                              menv, qq, unq, spl),
                                menv, qq, unq, spl), 
                  qquoted_form);
    } else if (sym == hash_lit_s) {
      val args = expand_qquote(second(qquoted_form), menv, qq, unq, spl);
      val pairs = expand_qquote(rest(rest(qquoted_form)), menv, qq, unq, spl);
      return rlcp(list(hash_construct_s, args, pairs, nao), qquoted_form);
    } else if (sym == vector_lit_s) {
      val args = expand_qquote(second(qquoted_form), menv, qq, unq, spl);
      return rlcp(list(vector_list_s, args, nao), qquoted_form);
    } else {
      val f = sym;
      val r = cdr(qquoted_form);
      val f_ex;
      val r_ex = expand_qquote(r, menv, qq, unq, spl);

      if (consp(f)) {
        val qsym = car(f);
        if (qsym == spl) {
          f_ex = expand(second(f), menv);
        } else if (qsym == unq) {
          f_ex = cons(list_s, cons(expand(second(f), menv), nil));
        } else if (qsym == qq) {
          f_ex = cons(list_s, cons(expand_qquote(expand_qquote(second(f),
                                                               menv, qq,
                                                               unq, spl),
                                                 menv, qq, unq, spl), nil));
        } else {
          f_ex = cons(list_s, cons(expand_qquote(f, menv, qq, unq, spl), nil));
        }
      } else {
        f_ex = cons(list_s, cons(expand_qquote(f, menv, qq, unq, spl), nil));
      }

      if (nilp(r_ex)) {
        return rlcp(cons(append_s, cons(f_ex, nil)), qquoted_form);
      } else if (atom(r_ex)) {
        return rlcp(cons(append_s, cons(f_ex, cons(r_ex, nil))), qquoted_form);
      } else {
        if (consp(r) && car(r) == unq)
          r_ex = cons(r_ex, nil);
        else if (car(r_ex) == append_s)
          r_ex = cdr(r_ex);
        else if (car(r_ex) == quote_s)
          r_ex = cons(r_ex, nil);
        return rlcp(cons(append_s, cons(f_ex, r_ex)), qquoted_form);
      }
    }
  }
  abort();
}

static val me_qquote(val form, val menv)
{
  if (first(form) == sys_qquote_s)
    return expand_qquote(second(form), menv,
                         sys_qquote_s, sys_unquote_s, sys_splice_s);
  return expand_qquote(second(form), menv,
                       qquote_s, unquote_s, splice_s);
                       
}

static val expand_vars(val vars, val menv, val form,
                       val *spec_p, int seq_p)
{
  val sym;

  if (nilp(vars)) {
    return nil;
  } else if (atom(vars)) {
    eval_error(form, lit("~a is an invalid variable binding syntax"),
               vars, nao);
    return vars;
  } else if (special_p(sym = car(vars))) {
    val rest_vars = rest(vars);
    val rest_vars_ex = rlcp(expand_vars(rest_vars, menv, form, spec_p, seq_p),
                            rest_vars);
    val var_ex = cons(special_s, cons(nil, cons(sym, nil)));
    return rlcp(cons(var_ex, rest_vars_ex), vars);
  } else if (symbolp(sym)) {
    val rest_vars = rest(vars);
    val rest_vars_ex = expand_vars(rest_vars, menv, form, spec_p, seq_p);
    if (rest_vars == rest_vars_ex)
      return vars;
    return rlcp(cons(sym, rest_vars_ex), vars);
  } else {
    cons_bind (var, init, sym);
    val rest_vars = rest(vars);
    /* This var's init form sees a previous symbol macro whose name is
       the same as the variable, so menv is used. */
    val init_ex = rlcp(expand_forms(init, menv), init);
    /* The initforms of subsequent vars in a sequential binding
       do not see a previous symbol macro; they see the var. */
    val menv_new = seq_p ? make_var_shadowing_env(menv, cons(var, nil)) : menv;
    val rest_vars_ex = rlcp(expand_vars(rest_vars, menv_new, form,
                                        spec_p, seq_p),
                            rest_vars);

    if (special_p(var)) {
      val var_ex = cons(special_s, cons(car(init_ex), cons(var, nil)));
      *spec_p = t;
      return rlcp(cons(var_ex, rest_vars_ex), vars);
    } else {
      if (init == init_ex && rest_vars == rest_vars_ex)
        return vars;
      return rlcp(cons(cons(var, init_ex), rest_vars_ex), vars);
    } 
  }
}

static val expand_quasi(val quasi_forms, val menv)
{
  if (nilp(quasi_forms)) {
    return nil;
  } else {
    val form = first(quasi_forms);
    val form_ex = form;

    if (consp(form)) {
      val sym = car(form);
      if (sym == expr_s) {
        val expr_ex = expand(rest(form), menv);

        if (expr_ex != rest(form))
          form_ex = rlcp(cons(sym, expr_ex), form);
      } else if (sym == var_s) {
        val param = second(form);
        val next = third(form);
        val mods = fourth(form);
        val param_ex = expand(param, menv);
        val mods_ex = expand_forms(mods, menv);

        /* next should be nil because this structure should have
           been passed through o_elemes_transform in the parser
           which unravels the nesting. */
        assert (next == nil);

        if (param_ex != param || mods_ex != mods)
          form_ex = rlcp(list(sym, param_ex, nil, mods_ex, nao), form);
      }
    }

    {
      val rest_forms = rest(quasi_forms);
      val rest_ex = expand_quasi(rest_forms, menv);

      if (form == form_ex && rest_ex == rest_forms)
        return quasi_forms;

      return rlcp(cons(form_ex, rest_ex), quasi_forms);
    }
  }
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
        val cell = acons_new_c(vararg, mkcloc(new_p), mkcloc(newsyms));
        val sym = cdr(if3(new_p,
                          rplacd(cell, gensym(format_op_arg(vararg))),
                          cell));
        cons_bind (outsyms, outforms, transform_op(re, newsyms, rg)); 
        return cons(outsyms, rlcp(cons(sym, outforms), outforms));
      } else if (vararg == rest_s) {
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

static val me_op(val form, val menv)
{
  cons_bind (sym, body, form);
  val body_ex = if3(sym == op_s, expand_forms(body, menv), expand(body, menv));
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
    val dwim_body = rlcp_tree(cons(dwim_s, 
                                   if3(or4(is_op, has_rest, ssyms, 
                                           null(proper_listp(body_trans))),
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

static val me_ap(val form, val menv)
{
  return list(apf_s, cons(op_s, rest(form)), nao);
}

static val me_ip(val form, val menv)
{
  return list(ipf_s, cons(op_s, rest(form)), nao);
}

static val me_ado(val form, val menv)
{
  return list(apf_s, cons(do_s, rest(form)), nao);
}

static val me_ido(val form, val menv)
{
  return list(ipf_s, cons(do_s, rest(form)), nao);
}

static val me_ret(val form, val menv)
{
  return cons(op_s, cons(identity_s, rest(form)));
}

static val expand_catch_clause(val form, val menv)
{
  val sym = first(form);
  val params = second(form);
  val body = rest(rest(form));
  val new_menv = make_var_shadowing_env(menv, get_param_syms(params));
  val params_ex = expand_params(params, menv);
  val body_ex = expand_forms(body, new_menv);
  if (body == body_ex && params == params_ex)
    return form;
  return rlcp(cons(sym, cons(params_ex, body_ex)), form);
}

static val expand_catch(val body, val menv)
{
  val try_form = first(body);
  val catch_clauses = rest(body);
  val catch_syms = mapcar(car_f, catch_clauses);
  val try_form_ex = expand(try_form, menv);
  val catch_clauses_ex = rlcp(mapcar(curry_12_1(func_n2(expand_catch_clause),
                                                menv), 
                                     catch_clauses),
                              catch_clauses);

  val expanded = cons(catch_s, 
                      cons(catch_syms, 
                           cons(try_form_ex, catch_clauses_ex)));
  return rlcp(expanded, body);
}

static val expand_save_specials(val form, val specials)
{
  if (!specials)
    return form;
  return rlcp(cons(with_saved_vars_s, cons(form, nil)), form);
}

val expand(val form, val menv)
{
  val macro = nil;

  menv = default_bool_arg(menv);

tail:
  if (nilp(form)) {
    return nil;
  } else if (bindable(form)) {
    val symac_bind = lookup_symac(menv, form);

    if (symac_bind) {
      val symac = cdr(symac_bind);
      if (symac == form)
        return form;
      form = rlcp_tree(symac, form);
      goto tail;
    }
    return form;
  } else if (atom(form)) {
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
      int seq_p = sym == let_star_s || sym == each_star_s ||
                  sym == collect_each_star_s || sym == append_each_star_s;
      val new_menv = make_var_shadowing_env(menv, vars);
      val body_ex = expand_forms(body, new_menv);
      val specials_p = nil;
      val vars_ex = expand_vars(vars, menv, form, &specials_p, seq_p);
      if (body == body_ex && vars == vars_ex && !specials_p) {
        return form;
      } else {
        val basic_form = rlcp(cons(sym, cons(vars_ex, body_ex)), form);
        return expand_save_specials(basic_form, specials_p);
      }
    } else if (sym == block_s || sym == return_from_s) {
      val name = second(form);
      val body = rest(rest(form));
      val body_ex = expand_forms(body, menv);
      if (body == body_ex)
        return form;
      return rlcp(cons(sym, cons(name, body_ex)), form);
    } else if (sym == cond_s) {
      val pairs = rest(form);
      val pairs_ex = expand_cond_pairs(pairs, menv);

      if (pairs == pairs_ex)
        return form;
      return rlcp(cons(cond_s, pairs_ex), form);
    } else if (sym == defvar_s || sym == defsymacro_s) {
      val name = second(form);
      val init = third(form);
      val init_ex = expand(init, menv);
      val form_ex = form;

      if (init != init_ex)
        form_ex = rlcp(cons(sym, cons(name, cons(init_ex, nil))), form);

      if (sym == defsymacro_s) {
        val result = eval(form_ex, make_env(nil, nil, nil), form);
        return cons(quote_s, cons(result, nil));
      } else {
        mark_special(name);
      }

      return form_ex;
    } else if (sym == lambda_s) {
      val params = second(form);
      val body = rest(rest(form));
      val new_menv = make_var_shadowing_env(menv, get_param_syms(params));
      val params_ex = expand_params(params, menv);
      val body_ex = expand_forms(body, new_menv);

      if (body == body_ex && params == params_ex)
        return form;
      return rlcp(cons(sym, cons(params_ex, body_ex)), form);
    } else if (sym == defun_s || sym == defmacro_s) {
      val name = second(form);
      val params = third(form);
      val new_menv = make_var_shadowing_env(menv, get_param_syms(params));
      val params_ex = expand_params(params, menv);
      val body = rest(rest(rest(form)));
      val body_ex = expand_forms(body, new_menv);
      val form_ex = form;

      if (body != body_ex || params != params_ex)
        form_ex = rlcp(cons(sym, cons(name, cons(params_ex, body_ex))), form);

      if (sym == defmacro_s) {
        val result = eval(form_ex, make_env(nil, nil, nil), form);
        return cons(quote_s, cons(result, nil));
      }
      return form_ex;
    } else if (sym == tree_case_s) {
      return expand_tree_case(form, menv);
    } else if (sym == tree_bind_s) {
      val params = second(form);
      val expr = third(form);
      val body = rest(rest(rest(form)));
      val new_menv = make_var_shadowing_env(menv, get_param_syms(params));
      val params_ex = expand_params(params, menv);
      val expr_ex = expand(expr, new_menv);
      val body_ex = expand_forms(body, new_menv);

      if (params_ex == params && expr_ex == expr && body_ex == body)
        return form;
      return rlcp(cons(sym, cons(params_ex, cons(expr_ex, body_ex))), form);
    } else if (sym == quote_s || sym == fun_s) {
      return form;
    } else if (sym == for_s || sym == for_star_s) {
      val vars = second(form);
      val cond = third(form);
      val incs = fourth(form);
      val forms = rest(rest(rest(rest(form))));
      val specials_p = nil;
      val vars_ex = expand_vars(vars, menv, form, &specials_p,
                                sym == for_star_s);
      val new_menv = make_var_shadowing_env(menv, vars);
      val cond_ex = expand_forms(cond, new_menv);
      val incs_ex = expand_forms(incs, new_menv);
      val forms_ex = expand_forms(forms, new_menv);

      if (vars == vars_ex && cond == cond_ex && 
          incs == incs_ex && forms == forms_ex && !specials_p) {
        return form;
      } else {
        val basic_form = rlcp(cons(sym, 
                                   cons(vars_ex, 
                                        cons(cond_ex,
                                             cons(incs_ex, forms_ex)))), form);
        return expand_save_specials(basic_form, specials_p);
      }
    } else if (sym == dohash_s) {
      val spec = second(form);
      val keysym = first(spec);
      val valsym = second(spec);
      val hashform = third(spec);
      val resform = fourth(spec);
      val body = rest(rest(form));
      val hashform_ex = expand(hashform, menv);
      val resform_ex = expand(resform, menv);
      val body_ex = expand_forms(body, menv);
     
      if (hashform == hashform_ex && resform == resform_ex && body == body_ex)
        return form;
      return cons(sym, cons(cons(keysym, 
                                 cons(valsym, 
                                      cons(hashform_ex, 
                                           cons(resform_ex, nil)))),
                            body_ex));
    } else if (sym == quasi_s) {
      val quasi = rest(form);
      val quasi_ex = expand_quasi(quasi, menv);
      if (quasi == quasi_ex)
        return form;
      return rlcp(cons(sym, quasi_ex), form);
    } else if (sym == catch_s) {
      return expand_catch(rest(form), menv);
    } else if (sym == regex_s || regexp(sym)) {
      return form; /* regex syntax isn't Lisp code; don't expand! */
    } else if (sym == macro_time_s) {
      val args = rest(form);
      val args_ex = expand_forms(args, menv);
      val result = eval_progn(args_ex, make_env(nil, nil, nil), args);
      return maybe_quote(result);
    } else if (sym == macrolet_s) {
      return expand_macrolet(form, menv);
    } else if (sym == symacrolet_s) {
      return expand_symacrolet(form, menv);
    } else if ((macro = lookup_mac(menv, sym))) {
      val mac_expand = expand_macro(form, macro, menv);
      if (mac_expand == form)
        return form;
      form = rlcp_tree(mac_expand, form);
      goto tail;
    } else {
      /* funtion call
         also handles: progn, prog1, call, if, and, or, 
         unwind-protect, return, dwim, set, inc, dec,
         push, pop, flip, and with-saved-vars. */
      val args = rest(form);
      val args_ex = expand_forms(args, menv);

      if (args == args_ex)
        return form;
      return rlcp(cons(sym, args_ex), form);
    }
    abort();
  }
}

static val macro_form_p(val form, val menv)
{
  menv = default_bool_arg(menv);

  if (bindable(form) && lookup_symac(menv, form)) 
    return t;
  if (consp(form) && lookup_mac(menv, car(form)))
    return t;
  return nil;
}

static val macroexpand_1(val form, val menv)
{
  val macro;

  menv = default_bool_arg(menv);

  if (consp(form) && (macro = lookup_mac(menv, car(form)))) {
    val mac_expand = expand_macro(form, macro, menv);
    if (mac_expand == form)
      return form;
    return rlcp_tree(mac_expand, form);
  }

  if (bindable(form) && (macro = lookup_symac(menv, form))) {
    val mac_expand = cdr(macro);
    if (mac_expand == form)
      return form;
    return rlcp_tree(mac_expand, form);
  }

  return form;
}

static val macroexpand(val form, val menv)
{
  for (;;) {
    val mac_expand = macroexpand_1(form, menv);
    if (mac_expand == form)
      return form;
    form = mac_expand;
  }
}

val mapcarv(val fun, val list_of_lists)
{
  if (!cdr(list_of_lists)) {
    return mapcar(fun, nullify(car(list_of_lists)));
  } else {
    val lofl = mapcar_listout(func_n1(nullify), list_of_lists);
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
        deref(car_l(iter)) = cdr(list);
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
    val lofl = mapcar(func_n1(nullify), list_of_lists);
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
        deref(car_l(iter)) = cdr(list);
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

val lazy_mapcar(val fun, val list)
{
  list = nullify(list);
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
    val lofl = mapcar(func_n1(nullify), list_of_lists);
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
  uses_or2;
  return or2(cdr(or2(lookup_fun(nil, sym),
                     lookup_mac(nil, sym))),
             gethash(op_table, sym));
}

static val boundp(val sym)
{
  return if3(lookup_var(nil, sym), t, nil);
}

static val fboundp(val sym)
{
  return if2(lookup_fun(nil, sym) || lookup_mac(nil, sym) ||
             gethash(op_table, sym), t);
}

static val rangev_func(val env, val lcons)
{
  cons_bind (from, to_step, env);
  cons_bind (to, step, to_step);
  val next = if3(functionp(step),
                 funcall1(step, from),
                 plus(from, step));

  rplaca(lcons, from);

  if (to &&
      (numeq(from, to) ||
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

  if (to &&
      (numeq(next, to) ||
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
  rplaca(lcons, pop(valptr(car_l(env))));
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

  rplaca(lcons, pop(valptr(car_l(env))));

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
  return cdr(rplacd(promise, funcall(cdr(promise))));
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

static void reg_op(val sym, opfun_t fun)
{
  assert (sym != 0);
  sethash(op_table, sym, cptr((mem_t *) fun));
}

void reg_fun(val sym, val fun)
{
  assert (sym != 0);
  sethash(top_fb, sym, cons(sym, fun));
}

static void reg_mac(val sym, mefun_t fun)
{
  assert (sym != 0);
  sethash(top_mb, sym, cptr((mem_t *) fun));
}

void reg_var(val sym, val val)
{
  assert (sym != nil);
  sethash(top_vb, sym, cons(sym, val));
  mark_special(sym);
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

static val not_null(val obj)
{
  return if3(nilp(obj), nil, t);
}

static val tf(val args)
{
  (void) args;
  return t;
}

static val nilf(val args)
{
  (void) args;
  return nil;
}

static val do_retf(val ret, val args)
{
  (void) args;
  return ret;
}

static val retf(val ret)
{
  return func_f0v(ret, do_retf);
}

static val do_apf(val fun, val args)
{
  return apply_intrinsic(fun, args);
}

static val apf(val fun)
{
  return func_f0v(fun, do_apf);
}

static val do_ipf(val fun, val args)
{
  return iapply(fun, args);
}

static val ipf(val fun)
{
  return func_f0v(fun, do_ipf);
}

static val prinl(val obj, val stream)
{
  val ret = obj_print(obj, stream);
  put_char(chr('\n'), stream);
  return ret;
}

static val pprinl(val obj, val stream)
{
  val ret = obj_pprint(obj, stream);
  put_char(chr('\n'), stream);
  return ret;
}

void eval_init(void)
{
  protect(&top_vb, &top_fb, &top_mb, &top_smb, &special, &dyn_env,
          &op_table, &last_form_evaled, (val *) 0);
  top_fb = make_hash(t, nil, nil);
  top_vb = make_hash(t, nil, nil);
  top_mb = make_hash(t, nil, nil);
  top_smb = make_hash(t, nil, nil);
  special = make_hash(t, nil, nil);
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
  defsymacro_s = intern(lit("defsymacro"), user_package);
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
  not_s = intern(lit("not"), user_package);
  vecref_s = intern(lit("vecref"), user_package);
  list_s = intern(lit("list"), user_package);
  append_s = intern(lit("append"), user_package);
  apply_s = intern(lit("apply"), user_package);
  iapply_s = intern(lit("iapply"), user_package);
  gen_s = intern(lit("gen"), user_package);
  gun_s = intern(lit("gun"), user_package);
  generate_s = intern(lit("generate"), user_package);
  promise_s = intern(lit("promise"), system_package);
  op_s = intern(lit("op"), user_package);
  do_s = intern(lit("do"), user_package);
  apf_s = intern(lit("apf"), user_package);
  ipf_s = intern(lit("ipf"), user_package);
  identity_s = intern(lit("identity"), user_package);
  rest_s = intern(lit("rest"), user_package);
  hash_lit_s = intern(lit("hash-construct"), system_package);
  hash_construct_s = intern(lit("hash-construct"), user_package);
  vector_lit_s = intern(lit("vector-lit"), system_package);
  vector_list_s = intern(lit("vector-list"), user_package);
  macro_time_s = intern(lit("macro-time"), user_package);
  macrolet_s = intern(lit("macrolet"), user_package);
  symacrolet_s = intern(lit("symacrolet"), user_package);
  with_saved_vars_s = intern(lit("with-saved-vars"), system_package);
  whole_k = intern(lit("whole"), keyword_package);
  special_s = intern(lit("special"), system_package);
  prof_s = intern(lit("prof"), user_package);

  reg_op(quote_s, op_quote);
  reg_op(qquote_s, op_qquote_error);
  reg_op(sys_qquote_s, op_qquote_error);
  reg_op(unquote_s, op_unquote_error);
  reg_op(sys_unquote_s, op_unquote_error);
  reg_op(splice_s, op_unquote_error);
  reg_op(sys_splice_s, op_unquote_error);
  reg_op(progn_s, op_progn);
  reg_op(prog1_s, op_prog1);
  reg_op(let_s, op_let);
  reg_op(each_s, op_each);
  reg_op(each_star_s, op_each);
  reg_op(collect_each_s, op_each);
  reg_op(collect_each_star_s, op_each);
  reg_op(append_each_s, op_each);
  reg_op(append_each_star_s, op_each);
  reg_op(let_star_s, op_let);
  reg_op(lambda_s, op_lambda);
  reg_op(fun_s, op_fun);
  reg_op(cond_s, op_cond);
  reg_op(if_s, op_if);
  reg_op(and_s, op_and);
  reg_op(or_s, op_or);
  reg_op(defvar_s, op_defvar);
  reg_op(defun_s, op_defun);
  reg_op(defmacro_s, op_defmacro);
  reg_op(defsymacro_s, op_defsymacro);
  reg_op(tree_case_s, op_tree_case);
  reg_op(tree_bind_s, op_tree_bind);
  reg_op(inc_s, op_modplace);
  reg_op(dec_s, op_modplace);
  reg_op(set_s, op_modplace);
  reg_op(push_s, op_modplace);
  reg_op(pop_s, op_modplace);
  reg_op(flip_s, op_modplace);
  reg_op(del_s, op_modplace);
  reg_op(for_s, op_for);
  reg_op(for_star_s, op_for);
  reg_op(dohash_s, op_dohash);
  reg_op(uw_protect_s, op_unwind_protect);
  reg_op(block_s, op_block);
  reg_op(return_s, op_return);
  reg_op(return_from_s, op_return_from);
  reg_op(dwim_s, op_dwim);
  reg_op(quasi_s, op_quasi_lit);
  reg_op(catch_s, op_catch);
  reg_op(with_saved_vars_s, op_with_saved_vars);
  reg_op(prof_s, op_prof);

  reg_mac(gen_s, me_gen);
  reg_mac(gun_s, me_gun);
  reg_mac(intern(lit("delay"), user_package), me_delay);
  reg_mac(op_s, me_op);
  reg_mac(do_s, me_op);
  reg_mac(intern(lit("ap"), user_package), me_ap);
  reg_mac(intern(lit("ip"), user_package), me_ip);
  reg_mac(intern(lit("ado"), user_package), me_ado);
  reg_mac(intern(lit("ido"), user_package), me_ido);
  reg_mac(intern(lit("ret"), user_package), me_ret);
  reg_mac(qquote_s, me_qquote);
  reg_mac(sys_qquote_s, me_qquote);
  reg_mac(intern(lit("pprof"), user_package), me_pprof);
  reg_mac(intern(lit("when"), user_package), me_when);
  reg_mac(intern(lit("unless"), user_package), me_unless);
  reg_mac(intern(lit("while"), user_package), me_while);
  reg_mac(intern(lit("until"), user_package), me_until);
  reg_mac(quasilist_s, me_quasilist);

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
  reg_fun(list_s, list_f);
  reg_fun(intern(lit("list*"), user_package), func_n0v(list_star_intrinsic));
  reg_fun(identity_s, identity_f);
  reg_fun(intern(lit("typeof"), user_package), func_n1(typeof));

  reg_fun(intern(lit("atom"), user_package), func_n1(atom));
  reg_fun(intern(lit("null"), user_package), null_f);
  reg_fun(intern(lit("false"), user_package), null_f);
  reg_fun(intern(lit("true"), user_package), func_n1(not_null));
  reg_fun(not_s, null_f);
  reg_fun(intern(lit("consp"), user_package), func_n1(consp));
  reg_fun(intern(lit("listp"), user_package), func_n1(listp));
  reg_fun(intern(lit("proper-listp"), user_package), func_n1(proper_listp));
  reg_fun(intern(lit("length-list"), user_package), func_n1(length_list));

  reg_fun(intern(lit("mapcar"), user_package), func_n1v(mapcarv));
  reg_fun(intern(lit("mapcar*"), user_package), func_n1v(lazy_mapcarv));
  reg_fun(intern(lit("mappend"), user_package), func_n1v(mappendv));
  reg_fun(intern(lit("mappend*"), user_package), func_n1v(lazy_mappendv));
  reg_fun(apply_s, func_n1v(apply_intrinsic));
  reg_fun(iapply_s, func_n1v(iapply));
  reg_fun(call_s, func_n1v(call));
  reg_fun(intern(lit("reduce-left"), user_package), func_n4o(reduce_left, 2));
  reg_fun(intern(lit("reduce-right"), user_package), func_n4o(reduce_right, 2));
  reg_fun(intern(lit("transpose"), user_package), func_n1(transpose));
  reg_fun(intern(lit("zip"), user_package), func_n0v(transpose));

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
  reg_fun(intern(lit("last"), user_package), func_n1(last));
  reg_fun(intern(lit("flatten"), user_package), func_n1(flatten));
  reg_fun(intern(lit("flatten*"), user_package), func_n1(lazy_flatten));
  reg_fun(intern(lit("tuples"), user_package), func_n3o(tuples, 2));
  reg_fun(intern(lit("memq"), user_package), func_n2(memq));
  reg_fun(intern(lit("memql"), user_package), func_n2(memql));
  reg_fun(intern(lit("memqual"), user_package), func_n2(memqual));
  reg_fun(intern(lit("member"), user_package), func_n4o(member, 2));
  reg_fun(intern(lit("member-if"), user_package), func_n3o(member_if, 2));
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
  reg_fun(intern(lit("multi"), user_package), func_n1v(multi));
  reg_fun(intern(lit("eq"), user_package), eq_f);
  reg_fun(intern(lit("eql"), user_package), eql_f);
  reg_fun(intern(lit("equal"), user_package), equal_f);

  reg_fun(plus_s = intern(lit("+"), user_package), func_n0v(plusv));
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
  reg_fun(intern(lit("log10"), user_package), func_n1(logten));
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
  reg_fun(intern(lit("pos-max"), user_package), func_n3o(pos_max, 1));
  reg_fun(intern(lit("pos-min"), user_package), func_n3o(pos_min, 1));
  reg_fun(intern(lit("logand"), user_package), func_n0v(logandv));
  reg_fun(intern(lit("logior"), user_package), func_n0v(logiorv));
  reg_fun(intern(lit("logxor"), user_package), func_n2(logxor));
  reg_fun(intern(lit("logtest"), user_package), func_n2(logtest));
  reg_fun(intern(lit("lognot"), user_package), func_n2o(lognot, 1));
  reg_fun(intern(lit("logtrunc"), user_package), func_n2(logtrunc));
  reg_fun(intern(lit("ash"), user_package), func_n2(ash));
  reg_fun(intern(lit("bit"), user_package), func_n2(bit));
  reg_fun(intern(lit("mask"), user_package), func_n0v(maskv));

  reg_fun(intern(lit("regex-compile"), user_package), func_n2o(regex_compile, 1));
  reg_fun(intern(lit("regexp"), user_package), func_n1(regexp));
  reg_fun(intern(lit("search-regex"), user_package), func_n4o(search_regex, 2));
  reg_fun(intern(lit("range-regex"), user_package), func_n4o(range_regex, 2));
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
  reg_fun(intern(lit("expand"), system_package), func_n2o(expand, 1));
  reg_fun(intern(lit("macro-form-p"), user_package), func_n2o(macro_form_p, 1));
  reg_fun(intern(lit("macroexpand-1"), user_package),
          func_n2o(macroexpand_1, 1));
  reg_fun(intern(lit("macroexpand"), user_package),
          func_n2o(macroexpand, 1));
  reg_fun(intern(lit("make-env"), user_package), func_n3o(make_env_intrinsic, 0));
  reg_fun(intern(lit("env-fbind"), user_package), func_n3(env_fbind));
  reg_fun(intern(lit("env-vbind"), user_package), func_n3(env_vbind));
  reg_fun(intern(lit("chain"), user_package), func_n0v(chainv));
  reg_fun(intern(lit("andf"), user_package), func_n0v(andv));
  reg_fun(intern(lit("orf"), user_package), func_n0v(orv));
  reg_fun(intern(lit("iff"), user_package), func_n3o(iff, 2));
  reg_fun(intern(lit("iffi"), user_package), func_n3o(iffi, 2));
  reg_fun(intern(lit("if"), user_package), func_n3o(if_fun, 2));
  reg_fun(intern(lit("or"), user_package), func_n0v(or_fun));
  reg_fun(intern(lit("and"), user_package), func_n0v(and_fun));
  reg_fun(intern(lit("retf"), user_package), func_n1(retf));
  reg_fun(apf_s, func_n1(apf));
  reg_fun(ipf_s, func_n1(ipf));
  reg_fun(intern(lit("tf"), user_package), func_n0v(tf));
  reg_fun(intern(lit("nilf"), user_package), func_n0v(nilf));

  reg_fun(intern(lit("print"), user_package), func_n2o(obj_print, 1));
  reg_fun(intern(lit("pprint"), user_package), func_n2o(obj_pprint, 1));
  reg_fun(intern(lit("tostring"), user_package), func_n1(tostring));
  reg_fun(intern(lit("tostringp"), user_package), func_n1(tostringp));
  reg_fun(intern(lit("prinl"), user_package), func_n2o(prinl, 1));
  reg_fun(intern(lit("pprinl"), user_package), func_n2o(pprinl, 1));

  reg_var(user_package_s = intern(lit("*user-package*"), user_package_var),
          user_package_var);
  reg_var(system_package_s = intern(lit("*system-package*"), user_package_var),
          system_package_var);
  reg_var(keyword_package_s = intern(lit("*keyword-package*"), user_package_var),
          keyword_package_var);

  reg_fun(intern(lit("make-sym"), user_package), func_n1(make_sym));
  reg_fun(intern(lit("gensym"), user_package), func_n1o(gensym, 0));
  reg_var(gensym_counter_s = intern(lit("*gensym-counter*"), user_package), zero);
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
  reg_fun(intern(lit("tok-where"), user_package), func_n2(tok_where));
  reg_fun(intern(lit("list-str"), user_package), func_n1(list_str));
  reg_fun(intern(lit("trim-str"), user_package), func_n1(trim_str));
  reg_fun(intern(lit("cmp-str"), user_package), func_n2(cmp_str));
  reg_fun(intern(lit("string-lt"), user_package), func_n2(str_lt));
  reg_fun(intern(lit("str="), user_package), func_n2(str_eq));
  reg_fun(intern(lit("str<"), user_package), func_n2(str_lt));
  reg_fun(intern(lit("str>"), user_package), func_n2(str_gt));
  reg_fun(intern(lit("str<="), user_package), func_n2(str_le));
  reg_fun(intern(lit("str>="), user_package), func_n2(str_ge));
  reg_fun(intern(lit("int-str"), user_package), func_n2o(int_str, 1));
  reg_fun(intern(lit("flo-str"), user_package), func_n1(flo_str));
  reg_fun(intern(lit("num-str"), user_package), func_n1(num_str));
  reg_fun(intern(lit("int-flo"), user_package), func_n1(int_flo));
  reg_fun(intern(lit("flo-int"), user_package), func_n1(flo_int));
  reg_fun(intern(lit("tofloat"), user_package), func_n1(tofloat));
  reg_fun(intern(lit("toint"), user_package), func_n2o(toint, 1));
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
  reg_fun(intern(lit("find-max"), user_package), func_n3o(find_max, 1));
  reg_fun(intern(lit("find-min"), user_package), func_n3o(find_min, 1));
  reg_fun(intern(lit("set-diff"), user_package), func_n4o(set_diff, 2));

  reg_fun(intern(lit("seqp"), user_package), func_n1(seqp));
  reg_fun(intern(lit("length"), user_package), func_n1(length));
  reg_fun(intern(lit("empty"), user_package), func_n1(empty));
  reg_fun(intern(lit("copy"), user_package), func_n1(copy));
  reg_fun(intern(lit("sub"), user_package), func_n3o(sub, 1));
  reg_fun(intern(lit("ref"), user_package), func_n2(ref));
  reg_fun(intern(lit("refset"), user_package), func_n3(refset));
  reg_fun(intern(lit("replace"), user_package), func_n4o(replace, 2));
  reg_fun(intern(lit("update"), user_package), func_n2(update));
  reg_fun(intern(lit("search"), user_package), func_n4o(search, 2));
  reg_fun(intern(lit("where"), user_package), func_n2(where));
  reg_fun(intern(lit("select"), user_package), func_n2(sel));

  reg_fun(intern(lit("make-like"), user_package), func_n2(make_like));
  reg_fun(intern(lit("nullify"), user_package), func_n1(nullify));

  reg_fun(intern(lit("symbol-value"), user_package), func_n1(symbol_value));
  reg_fun(intern(lit("symbol-function"), user_package), func_n1(symbol_function));
  reg_fun(intern(lit("boundp"), user_package), func_n1(boundp));
  reg_fun(intern(lit("fboundp"), user_package), func_n1(fboundp));
  reg_fun(intern(lit("func-get-form"), user_package), func_n1(func_get_form));
  reg_fun(intern(lit("func-get-env"), user_package), func_n1(func_get_env));
  reg_fun(intern(lit("func-set-env"), user_package), func_n2(func_set_env));
  reg_fun(intern(lit("functionp"), user_package), func_n1(functionp));
  reg_fun(intern(lit("interp-fun-p"), user_package), func_n1(interp_fun_p));

  reg_fun(intern(lit("make-random-state"), user_package), func_n1o(make_random_state, 0));
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

#if HAVE_DAEMON
  reg_fun(intern(lit("daemon"), user_package), func_n2(daemon_wrap));
#endif

#if HAVE_MKDIR || HAVE_WINDOWS_H
  reg_fun(intern(lit("mkdir"), user_package), func_n2o(mkdir_wrap, 1));
#endif

  reg_fun(intern(lit("chdir"), user_package), func_n1(chdir_wrap));
  reg_fun(intern(lit("pwd"), user_package), func_n0(getcwd_wrap));

#if HAVE_MAKEDEV
  reg_fun(intern(lit("makedev"), user_package), func_n2(makedev_wrap));
  reg_fun(intern(lit("minor"), user_package), func_n1(minor_wrap));
  reg_fun(intern(lit("major"), user_package), func_n1(major_wrap));
#endif

#if HAVE_MKNOD
  reg_fun(intern(lit("mknod"), user_package), func_n3(mknod_wrap));
#endif

#if HAVE_SYMLINK
  reg_fun(intern(lit("symlink"), user_package), func_n2(symlink_wrap));
  reg_fun(intern(lit("link"), user_package), func_n2(link_wrap));
  reg_fun(intern(lit("readlink"), user_package), func_n1(readlink_wrap));
#endif

  reg_fun(intern(lit("source-loc"), user_package), func_n1(source_loc));
  reg_fun(intern(lit("source-loc-str"), user_package), func_n1(source_loc_str));
  reg_fun(intern(lit("rlcp"), user_package), func_n2(rlcp));

  eval_error_s = intern(lit("eval-error"), user_package);
  uw_register_subtype(eval_error_s, error_s);
}
