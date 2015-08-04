/* Copyright 2010-2015
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
#include <dirent.h>
#include <setjmp.h>
#include <stdarg.h>
#include <wchar.h>
#include <signal.h>
#include <time.h>
#include "config.h"
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
#include "lisplib.h"
#include "eval.h"

#define APPLY_ARGS 32

typedef val (*opfun_t)(val, val);
typedef val (*mefun_t)(val, val);

struct c_var {
  val *loc;
  val bind;
};

val top_vb, top_fb, top_mb, top_smb, special, builtin;
val op_table;
val dyn_env;
val eval_initing;

val eval_error_s;
val dwim_s, progn_s, prog1_s, let_s, let_star_s, lambda_s, call_s;
val cond_s, if_s, iflet_s, when_s;
val defvar_s, defvarl_s, defparm_s, defparml_s, defun_s, defmacro_s;
val tree_case_s, tree_bind_s;
val sys_mark_special_s;
val caseq_s, caseql_s, casequal_s;
val memq_s, memql_s, memqual_s;
val eq_s, eql_s, equal_s;
val gethash_s, car_s, cdr_s, not_s, vecref_s;
val setq_s, inc_s, zap_s;
val for_s, for_star_s, each_s, each_star_s, collect_each_s, collect_each_star_s;
val append_each_s, append_each_star_s, while_s, while_star_s, until_star_s;
val dohash_s;
val uw_protect_s, return_s, return_from_s;
val list_s, append_s, apply_s, iapply_s;
val gen_s, gun_s, generate_s, rest_s, plus_s;
val promise_s, promise_forced_s, promise_inprogress_s, force_s;
val op_s, ap_s, identity_s, apf_s, ipf_s;
val ret_s, aret_s;
val hash_lit_s, hash_construct_s;
val vector_lit_s, vector_list_s;
val macro_time_s, with_saved_vars_s, macrolet_s;
val defsymacro_s, symacrolet_s, prof_s;
val fbind_s, lbind_s, flet_s, labels_s;
val opip_s, oand_s, chain_s, chand_s;
val sys_load_s, sys_lisp1_value_s;

val special_s, whole_k, symacro_k, fun_k;

val last_form_evaled, last_form_expanded;

val call_f;

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

static void env_vb_to_fb(val env)
{
  if (env) {
    type_check(env, ENV);
    env->e.fbindings = env->e.vbindings;
    env->e.vbindings = nil;
  }
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

val lookup_global_var(val sym)
{
  uses_or2;
  return or2(gethash(top_vb, sym),
             if2(lisplib_try_load(sym), gethash(top_vb, sym)));
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

  return lookup_global_var(sym);
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

  return or3(gethash(top_vb, sym),
             if2(lisplib_try_load(sym),
                 gethash(top_vb, sym)),
             gethash(top_fb, sym));
}

loc lookup_var_l(val env, val sym)
{
  val binding = lookup_var(env, sym);
  return if3(binding, cdr_l(binding), nulloc);
}

loc lookup_global_var_l(val sym)
{
  val binding = lookup_global_var(sym);
  return if3(binding, cdr_l(binding), nulloc);
}

val lookup_fun(val env, val sym)
{
  uses_or2;

  if (nilp(env)) {
    return or2(gethash(top_fb, sym),
               if2(lisplib_try_load(sym), gethash(top_fb, sym)));
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
  uses_or2;

  if (nilp(menv)) {
    return or2(gethash(top_mb, sym),
               if2(lisplib_try_load(sym), gethash(top_mb, sym)));
  } else  {
    type_check(menv, ENV);

    {
      val binding = assoc(sym, menv->e.fbindings);
      if (binding) /* special_s: see make_fun_shadowing_env */
        return (cdr(binding) == special_s) ? nil : binding;
      return lookup_mac(menv->e.up_env, sym);
    }
  }
}

static val lookup_symac(val menv, val sym)
{
  uses_or2;

  if (nilp(menv)) {
    return or2(gethash(top_smb, sym),
               if2(lisplib_try_load(sym), gethash(top_smb, sym)));
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

static val lookup_symac_lisp1(val menv, val sym)
{
  uses_or2;

  if (nilp(menv)) {
    return or2(gethash(top_smb, sym),
               if2(lisplib_try_load(sym), gethash(top_smb, sym)));
  } else  {
    type_check(menv, ENV);

    /* Of course, we are not looking for symbol macros in the operator macro
     * name space.  Rather, the object of the lookup rule implemented by this
     * function is to allow lexical function bindings to shadow symbol macros.
     */
    {
      val vbinding = assoc(sym, menv->e.vbindings);
      if (vbinding) {
        return (cdr(vbinding) == special_s) ? nil : vbinding;
      } else {
        val fbinding = assoc(sym, menv->e.fbindings);
        if (fbinding && cdr(fbinding) == special_s)
          return nil;
      }

      return lookup_symac(menv->e.up_env, sym);
    }
  }
}

static val lexical_var_p(val menv, val sym)
{
  if (nilp(menv)) {
    return nil;
  } else {
    type_check(menv, ENV);

    {
      val binding = assoc(sym, menv->e.vbindings);

      if (binding) /* special_s: see make_var_shadowing_env */
        return c_true(cdr(binding) == special_s);
      return lexical_var_p(menv->e.up_env, sym);
    }
  }
}

static val lexical_fun_p(val menv, val sym)
{
  if (nilp(menv)) {
    return nil;
  } else {
    type_check(menv, ENV);

    {
      val binding = assoc(sym, menv->e.fbindings);

      if (binding) /* special_s: see make_var_shadowing_env */
        return c_true(cdr(binding) == special_s);
      return lexical_fun_p(menv->e.up_env, sym);
    }
  }
}

static val lexical_lisp1_binding(val menv, val sym)
{
  if (nilp(menv)) {
    return nil;
  } else {
    type_check(menv, ENV);

    {
      val binding = assoc(sym, menv->e.vbindings);

      if (binding) /* special_s: see make_var_shadowing_env */
        return if3(cdr(binding) == special_s,
                   var_k, symacro_k);
    }

    {
      val binding = assoc(sym, menv->e.fbindings);

      if (binding && cdr(binding) == special_s)
        return fun_k;
      return lexical_lisp1_binding(menv->e.up_env, sym);
    }
  }
}

static val mark_special(val sym)
{
  assert (sym != nil);
  return sethash(special, sym, t);
}

static val special_var_p(val sym)
{
  uses_or2;
  return or2(gethash(special, sym),
             if2(lisplib_try_load(sym), gethash(special, sym)));
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
    if (special_var_p(params))
      push(params, pspecials);
    return params;
  } else {
    val form = car(params);
    if (atom(form) || !consp(cdr(form))) { /* sym, or no init form */
      val params_ex = expand_opt_params_rec(cdr(params), menv, pspecials);
      if (special_var_p(form))
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
      if (special_var_p(sym))
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
    if (special_var_p(params))
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

static val get_param_syms(val params);

static val get_opt_param_syms(val params)
{
  if (bindable(params)) {
    return cons(params, nil);
  } else if (atom(params)) {
    return nil;
  } else {
    val spec = car(params);

    if (atom(spec)) {
      val rest_syms = get_opt_param_syms(cdr(params));
      if (bindable(spec))
        return cons(spec, rest_syms);
      return rest_syms;
    } else {
      val pat = car(spec);
      return nappend2(get_param_syms(pat), get_opt_param_syms(cdr(params)));
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
  val arg[APPLY_ARGS], *p = arg;
  int fixparam, reqargs, nargs;
  val ctx = if3(ctx_form, car(ctx_form), apply_s);

  if (fun && symbolp(fun)) {
    val binding = gethash(top_fb, fun);
    if (!binding)
      eval_error(ctx_form, lit("~s: no such function ~s"), ctx, fun, nao);
    fun = cdr(binding);
  }

  if (!functionp(fun)) {
    for (nargs = 0;
         (p < arg + APPLY_ARGS) && consp(arglist);
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

  fixparam = fun->f.fixparam;
  reqargs = fixparam - fun->f.optargs;

  if (!fun->f.variadic) {
    for (; arglist && p < arg + APPLY_ARGS; arglist = cdr(arglist))
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
      return fun->f.f.f1(fun->f.env, z(arg[0]));
    case F2:
      return fun->f.f.f2(fun->f.env, z(arg[0]), z(arg[1]));
    case F3:
      return fun->f.f.f3(fun->f.env, z(arg[0]), z(arg[1]), z(arg[2]));
    case F4:
      return fun->f.f.f4(fun->f.env, z(arg[0]), z(arg[1]), z(arg[2]), z(arg[3]));
    case N0:
      return fun->f.f.n0();
    case N1:
      return fun->f.f.n1(z(arg[0]));
    case N2:
      return fun->f.f.n2(z(arg[0]), z(arg[1]));
    case N3:
      return fun->f.f.n3(z(arg[0]), z(arg[1]), z(arg[2]));
    case N4:
      return fun->f.f.n4(z(arg[0]), z(arg[1]), z(arg[2]), z(arg[3]));
    case N5:
      return fun->f.f.n5(z(arg[0]), z(arg[1]), z(arg[2]), z(arg[3]), z(arg[4]));
    case N6:
      return fun->f.f.n6(z(arg[0]), z(arg[1]), z(arg[2]), z(arg[3]), z(arg[4]), z(arg[5]));
    case N7:
      return fun->f.f.n7(z(arg[0]), z(arg[1]), z(arg[2]), z(arg[3]), z(arg[4]), z(arg[5]), z(arg[6]));
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
      return interp_fun(fun->f.env, fun->f.f.interp_fun, z(arglist));
    case F0:
      return fun->f.f.f0v(fun->f.env, z(arglist));
    case F1:
      return fun->f.f.f1v(fun->f.env, z(arg[0]), z(arglist));
    case F2:
      return fun->f.f.f2v(fun->f.env, z(arg[0]), z(arg[1]), z(arglist));
    case F3:
      return fun->f.f.f3v(fun->f.env, z(arg[0]), z(arg[1]), z(arg[2]), z(arglist));
    case F4:
      return fun->f.f.f4v(fun->f.env, z(arg[0]), z(arg[1]), z(arg[2]), z(arg[3]), z(arglist));
    case N0:
      return fun->f.f.n0v(z(arglist));
    case N1:
      return fun->f.f.n1v(z(arg[0]), z(arglist));
    case N2:
      return fun->f.f.n2v(z(arg[0]), z(arg[1]), z(arglist));
    case N3:
      return fun->f.f.n3v(z(arg[0]), z(arg[1]), z(arg[2]), z(arglist));
    case N4:
      return fun->f.f.n4v(z(arg[0]), z(arg[1]), z(arg[2]), z(arg[3]), z(arglist));
    case N5:
      return fun->f.f.n5v(z(arg[0]), z(arg[1]), z(arg[2]), z(arg[3]), z(arg[4]), z(arglist));
    case N6:
      return fun->f.f.n6v(z(arg[0]), z(arg[1]), z(arg[2]), z(arg[3]), z(arg[4]), z(arg[5]), z(arglist));
    case N7:
      return fun->f.f.n7v(z(arg[0]), z(arg[1]), z(arg[2]), z(arg[3]), z(arg[4]), z(arg[5]), z(arg[6]), z(arglist));
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
  return apply(fun, apply_frob_args(z(args)), nil);
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

  return apply(fun, z(mod_args), nil);
}

static val call(val fun, val args)
{
  return apply(fun, z(args), cons(apply_s, nil));
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
      if (optargs && car(form) == colon_k) {
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
  val firstparam = if2(consp(params), car(params));

  if (!consp(firstparam) || car(firstparam) != special_s)
  {
    val fun_env = bind_args(env, params, args, fun);
    return eval_progn(body, fun_env, body);
  } else {
    val saved_de = set_dyn_env(make_env(nil, nil, dyn_env));
    val fun_env = bind_args(env, params, args, fun);
    val ret = eval_progn(body, fun_env, body);
    set_dyn_env(saved_de);
    return ret;
  }
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
    val entry = gethash(op_table, oper);

    if (entry) {
      opfun_t fp = coerce(opfun_t, cptr_get(entry));
      val ret, lfe_save = last_form_evaled;
      ret = fp(form, env);
      last_form_evaled = lfe_save;
      debug_return (ret);
    } else {
      val fbinding = lookup_fun(env, oper);
      if (!fbinding) {
        last_form_evaled = form;
        eval_error(form, lit("no such function or operator: ~s"), oper, nao);
        abort();
      } else {
        val args = do_eval_args(rest(form), env, form, &lookup_var);
        val ret, lfe_save = last_form_evaled;
        debug_frame(oper, args, nil, env, nil, nil, nil);
        last_form_evaled = form;
        ret = apply(cdr(fbinding), z(args), form);
        last_form_evaled = lfe_save;
        debug_end;
        debug_return (ret);
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

static val op_error(val form, val env)
{
  eval_error(form, lit("unexpanded ~s encountered"), car(form), nao);
  abort();
}

static val op_quote(val form, val env)
{
  val d = cdr(form);

  if (!consp(d) || cdr(d))
    eval_error(form, lit("bad quote syntax: ~s"), form, nao);
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
  val de = if3(sequential, dyn_env, nil);
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
      val binding = env_vbind(de = (de ? de : make_env(nil, nil, dyn_env)),
                              special, value);
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

  if (de && de != dyn_env)
    dyn_env = de;
  if (env_out)
    *env_out = ne;
  return new_bindings;
}

static val fbindings_helper(val vars, val env, val lbind, val ctx_form)
{
  val iter;
  val nenv = make_env(nil, nil, env);
  val lenv = if3(lbind, nenv, env);

  for (iter = vars; iter; iter = cdr(iter)) {
    val item = car(iter);
    val var = pop(&item);
    val value = eval(pop(&item), lenv, ctx_form);

    if (bindable(var)) {
      (void) env_fbind(nenv, var, value);
    } else {
      eval_error(ctx_form, lit("~s: ~s is not a bindable symbol"),
                 car(ctx_form), var, nao);
    }
  }

  return nenv;
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

static val op_fbind(val form, val env)
{
  val oper = first(form);
  val args = rest(form);
  val vars = first(args);
  val body = rest(args);
  val new_env = fbindings_helper(vars, env, eq(oper, lbind_s), form);
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

static void check_lambda_list(val form, val sym, val params)
{
  val iter;
  val colon = nil;

  for (iter = params; consp(iter); iter = cdr(iter)) {
    val param = car(iter);
    if (param == colon_k) {
      if (colon)
        eval_error(form, lit("~s: multiple colons in parameter list"),
                   sym, nao);
      else
        colon = t;
      continue;
    }
    if (colon && consp(param))
      continue;
    if (!bindable(param)) {
      if (consp(param) && car(param) == special_s)
        continue; /* special vars list */
      eval_error(form, lit("~s: parameter ~s is not a bindable symbol"),
                 sym, param, nao);
    }
  }

  if (iter && !bindable(iter))
    eval_error(form, lit("~s: dot parameter ~s is not a bindable symbol"),
               sym, iter, nao);
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

static val op_defvarl(val form, val env)
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

  remhash(top_mb, name);

  /* defun captures lexical environment, so env is passed */
  sethash(top_fb, name, cons(name, func_interp(env, fun)));
  if (eval_initing)
    sethash(builtin, name, defun_s);
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
  if (eval_initing)
    sethash(builtin, name, defmacro_s);
  return name;
}

static val expand_macro(val form, val expander, val menv)
{
  if (cobjp(expander)) {
    mefun_t fp = coerce(mefun_t, cptr_get(expander));
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
    val result;
    debug_frame(name, args, nil, env, nil, nil, nil);
    result = eval_progn(body, exp_env, body);
    debug_end;
    set_dyn_env(saved_de);
    debug_return(result);
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

static void builtin_reject_test(val op, val sym, val form)
{
    val builtin_kind = gethash(builtin, sym);
    val is_operator = gethash(op_table, sym);

    if (!bindable(sym)) {
      eval_error(form, lit("~s: cannot bind ~s, which is not a bindable symbol"),
                 is_operator, sym, nao);
    } else if (builtin_kind) {
      eval_error(form, lit("~s: cannot bind ~s, which is a built-in ~s"),
                 op, sym, builtin_kind, nao);
    } else if (is_operator) {
      eval_error(form, lit("~s: cannot bind ~s, which is a built-in operator"),
                 op, sym, nao);
    }
}

static val expand_macrolet(val form, val menv)
{
  val op = car(form);
  val body = cdr(form);
  val macs = pop(&body);
  val new_env = make_env(nil, nil, menv);

  for (; macs; macs = cdr(macs)) {
    val macro = car(macs);
    val name = pop(&macro);
    val params = expand_params(pop(&macro), menv);
    val macro_ex = expand_forms(macro, menv);
    val block = cons(block_s, cons(name, macro_ex));

    builtin_reject_test(op, name, form);

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

static val make_fun_shadowing_env(val menv, val funcs)
{
  val env = make_var_shadowing_env(menv, funcs);
  env_vb_to_fb(env);
  return env;
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

static val op_mac_param_bind(val form, val env)
{
  val body = cdr(form);
  val ctx_form = pop(&body);
  val params = pop(&body);
  val expr = pop(&body);
  val ctx_val = eval(ctx_form, env, ctx_form);
  val expr_val = eval(expr, env, expr);
  val new_env = bind_macro_params(env, nil, params, expr_val, nil, ctx_val);
  return eval_progn(body, new_env, body);
}

static val op_setq(val form, val env)
{
  val args = rest(form);
  val var = pop(&args);
  val newval = pop(&args);

  if (!bindable(var)) {
    eval_error(form, lit("sys:setq: ~s is not a bindable symbol"), var, nao);
  } else {
    val binding = lookup_var(env, var);
    if (nilp(binding))
      eval_error(form, lit("unbound variable ~s"), var, nao);
    return sys_rplacd(binding, eval(newval, env, form));
  }
}

static val op_lisp1_setq(val form, val env)
{
  val args = rest(form);
  val var = pop(&args);
  val newval = pop(&args);

  if (!bindable(var)) {
    eval_error(form, lit("sys:lisp1-setq: ~s is not a bindable symbol"), var, nao);
  } else {
    val binding = lookup_sym_lisp1(env, var);
    if (nilp(binding))
      eval_error(form, lit("unbound variable ~s"), var, nao);
    return sys_rplacd(binding, eval(newval, env, form));
  }
}

static val expand_lisp1_value(val form, val menv)
{
  if (length(form) != two)
    eval_error(form, lit("~s: invalid syntax"), first(form), nao);

  {
    val sym = second(form);
    val binding_type = lexical_lisp1_binding(menv, sym);

    if (nilp(binding_type))
      return form;

    if (binding_type == var_k)
      return sym;

    if (binding_type == fun_k)
      return rlcp(cons(fun_s, cons(sym, nil)), form);

    eval_error(form, lit("~s: misapplied to symbol macro ~s"),
               first(form), sym, nao);
  }
}

static val op_lisp1_value(val form, val env)
{
  val args = rest(form);
  val arg = car(args);

  if (!bindable(arg)) {
    return eval(arg, env, form);
  } else {
    val binding = lookup_sym_lisp1(env, arg);
    if (nilp(binding))
      eval_error(form, lit("unbound variable ~s"), arg, nao);
    return cdr(binding);
  }
}

static val op_setqf(val form, val env)
{
  val args = rest(form);
  val var = pop(&args);
  val newval = pop(&args);

  if (!bindable(var)) {
    eval_error(form, lit("sys:setqf: ~s is not a bindable symbol"), var, nao);
  } else {
    val binding = lookup_fun(env, var);
    if (nilp(binding))
      eval_error(form, lit("unbound function ~s"), var, nao);
    return sys_rplacd(binding, eval(newval, env, form));
  }
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
  val fi = car(args);
  val re = cdr(z(args));
  return apply(z(fi), z(re), form);
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
        val clause_env = bind_args(env, params, exvals, clause);
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
        val modifiers = third(form);
        val str = eval(expr, env, form);

        /* If the object is a list, we let format_field deal with the
           conversion to text, because the modifiers influence how
           it is done. */
        if (!stringp(str) && !listp(str))
          str = tostringp(str);

        if (modifiers) {
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
      forms = cons(cons(var_s, cons(form, nil)), cdr(forms));
      continue;
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

static val me_def_variable(val form, val menv)
{
  val args = rest(form);
  val op = first(form);
  val sym = first(args);
  val initform = second(args);
  val setval = if2(op == defparm_s || op == defparml_s,
                   cons(list(set_s, sym, initform, nao), nil));

  (void) menv;

  if (op != defvar_s && length(args) != two)
    eval_error(form, lit("~s: two arguments expected"), op, nao);

  return apply_frob_args(list(prog1_s,
                              cons(defvarl_s,
                                   cons(sym, if2(op == defvar_s,
                                                 cons(initform, nil)))),
                              if3(op == defparm_s || op == defvar_s,
                                  cons(list(sys_mark_special_s,
                                            list(quote_s, sym, nao),
                                            nao),
                                       setval),
                                  setval),
                              nao));
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
  rlcp_tree(rest(form), second(form));
  return list(cons_s,
              cons(quote_s, cons(promise_s, nil)),
              list(cons_s, cons(lambda_s, cons(nil, rest(form))),
                   cons(quote_s, cons(form, nil)), nao),
              nao);
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

static val me_while_star(val form, val menv)
{
  val once = gensym(lit("once-"));
  (void) menv;
  return apply_frob_args(list(for_s, cons(list(once, t, nao), nil),
                              list(or_s, once, second(form), nao),
                              cons(list(zap_s, once, nao), nil),
                              rest(rest(form)), nao));
}

static val me_until(val form, val menv)
{
  val inv = cons(not_s, cons(second(form), nil));
  (void) menv;
  return apply_frob_args(list(for_s, nil, cons(inv, nil), nil,
                              rest(rest(form)), nao));
}

static val me_until_star(val form, val menv)
{
  val once = gensym(lit("once-"));
  val inv = cons(not_s, cons(second(form), nil));
  (void) menv;
  return apply_frob_args(list(for_s, cons(list(once, t, nao), nil),
                              list(or_s, once, inv, nao),
                              cons(list(zap_s, once, nao), nil),
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

static val constantp(val form, val env_in);

static val expand_progn(val form, val menv)
{
  if (atom(form)) {
    return form;
  } else {
    val f = car(form);
    val r = cdr(form);
    val ex_f = expand(f, menv);
    val ex_r = expand_progn(r, menv);

    if (consp(ex_f) && car(ex_f) == progn_s) {
      if (ex_r)
        return expand_progn(rlcp_tree(append2(cdr(ex_f), ex_r), form), menv);
      return rlcp(cdr(ex_f), form);
    }

    if ((symbolp(ex_f) || constantp(ex_f, nil)) && ex_r)
      return rlcp(ex_r, form);

    if (ex_f == f && ex_r == r)
      return form;

    return rlcp(cons(ex_f, ex_r), form);
  }
}

static val expand_lisp1(val form, val menv)
{
tail:
  if (bindable(form)) {
    val symac_bind = lookup_symac_lisp1(menv, form);

    if (symac_bind) {
      val symac = cdr(symac_bind);
      if (symac == form)
        return form;
      form = rlcp_tree(symac, form);
      goto tail;
    }
    return form;
  }

  return expand(form, menv);
}

static val expand_forms_lisp1(val form, val menv)
{
  if (atom(form)) {
    return form;
  } else {
    val f = car(form);
    val r = cdr(form);
    val ex_f = expand_lisp1(f, menv);
    val ex_r = expand_forms_lisp1(r, menv);

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
                          lit("the splice ,*~s cannot occur outside of a list "
                              "or in the dotted position of a list"),
                          lit("(splice ~s) cannot occur outside of a list "
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
  } else if (special_var_p(sym = car(vars))) {
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

    if (special_var_p(var)) {
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

static val expand_fbind_vars(val vars, val menv, val form)
{
  val sym;

  if (nilp(vars)) {
    return nil;
  } else if (atom(vars)) {
    eval_error(form, lit("~a is an invalid function binding syntax"),
               vars, nao);
    return vars;
  } else if (symbolp(sym = car(vars))) {
    eval_error(form, lit("symbols in this construct require initforms"), nao);
  } else {
    cons_bind (var, init, sym);
    val rest_vars = rest(vars);
    /* This var's init form sees a previous macro whose name is
       the same as the symbol, so menv is used. */
    val init_ex = rlcp(expand_forms(init, menv), init);
    /* The initforms of subsequent vars in a sequential binding
       do not see a previous symbol macro; they see the var. */
    val rest_vars_ex = rlcp(expand_fbind_vars(rest_vars, menv, form),
                            rest_vars);

    builtin_reject_test(car(form), var, form);

    if (init == init_ex && rest_vars == rest_vars_ex)
      return vars;
    return rlcp(cons(cons(var, init_ex), rest_vars_ex), vars);
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
        val mods = third(form);
        val param_ex = expand(param, menv);
        val mods_ex = expand_forms(mods, menv);

        if (param_ex != param || mods_ex != mods)
          form_ex = rlcp(list(sym, param_ex, mods_ex, nao), form);
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
        val sym_form = if3(cdr(cdr(fi)),
                           cons(var_s, cons(sym, cdr(cdr(fi)))), sym);
        cons_bind (outsyms, outforms, transform_op(re, newsyms, rg));
        return cons(outsyms, rlcp(cons(sym_form, outforms), outforms));
      } else if (vararg == rest_s) {
        val sym_form = if3(cdr(cdr(fi)),
                           cons(var_s, cons(rg, cdr(cdr(fi)))), rg);
        cons_bind (outsyms, outforms, transform_op(re, syms, rg));
        return cons(outsyms, rlcp(cons(sym_form, outforms), outforms));
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

  for (si = ssyms, ni = one; si; ni = plus(ni, one), si = cdr(si))
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

static val me_ret_aret(val form, val menv)
{
  val sym = if3(eq(car(form), ret_s), op_s, ap_s);
  return list(sym, identity_s, cons(progn_s,
                                    cons(list(var_s, rest_s, nao),
                                         rest(form))), nao);
}

static val me_flet_labels(val form, val menv)
{
  val body = form;
  val sym = pop(&body);
  val funcs = pop(&body);
  list_collect_decl (lambdas, ptail);

  for (; funcs; funcs = cdr(funcs)) {
    val func = car(funcs);
    val name = pop(&func);
    val params = pop(&func);
    val lambda = cons(lambda_s, cons(params, func));

    check_lambda_list(form, sym, params);

    ptail = list_collect (ptail, cons(name, cons(lambda, nil)));
  }

  return cons(if3(eq(sym, flet_s), fbind_s, lbind_s),
              cons(lambdas, body));
}

static val me_case(val form, val menv)
{
  val form_orig = form;
  val casesym = pop(&form);
  val testform = pop(&form);
  val tformsym = gensym(lit("test-"));
  val memfuncsym, eqfuncsym;
  list_collect_decl (condpairs, ptail);

  if (casesym == caseq_s) {
    memfuncsym = memq_s;
    eqfuncsym = eq_s;
  } else if (casesym == caseql_s) {
    memfuncsym = memql_s;
    eqfuncsym = eql_s;
  } else {
    memfuncsym = memqual_s;
    eqfuncsym = equal_s;
  }

  for (; consp(form); form = cdr(form)) {
    cons_bind (clause, rest, form);
    cons_bind (keys, forms, clause);

    if (!rest && keys == t) {
      ptail = list_collect(ptail, clause);
      break;
    }

    if (keys == t)
      eval_error(form_orig, lit("~s: symbol t used as key"), casesym, nao);

    ptail = list_collect(ptail,
                         cons(list(if3(atom(keys), eqfuncsym, memfuncsym),
                                   tformsym,
                                   if3(atom(keys), keys, list(quote_s, keys, nao)),
                                   nao),
                              forms));
  }

  if (form && atom(form))
    eval_error(form_orig, lit("~s: improper form terminated by ~s"), casesym, form, nao);

  return list(let_s, cons(list(tformsym, testform, nao), nil),
              cons(cond_s, condpairs), nao);
}

static val me_tb(val form, val menv)
{
  val opsym = pop(&form);
  val pat = pop(&form);
  val body = form;
  val args = gensym(lit("args-"));

  (void) opsym;
  (void) menv;

  return list(lambda_s, args,
              cons(tree_bind_s, cons(pat, cons(args, body))), nao);
}

static val me_tc(val form, val menv)
{
  val opsym = pop(&form);
  val cases = form;
  val args = gensym(lit("args-"));

  (void) opsym;
  (void) menv;

  return list(lambda_s, args,
              cons(tree_case_s, cons(args, cases)), nao);
}

static val me_opip(val form, val menv)
{
  val opsym = pop(&form);
  val clauses = form;
  val chain_chand = if3(opsym == opip_s, chain_s, chand_s);
  list_collect_decl (transformed_forms, ptail);

  for (; clauses; clauses = cdr(clauses)) {
    val clause = car(clauses);

    if (consp(clause)) {
      uses_or2;
      val sym = car(clause);

      if (sym == dwim_s) {
        list_collect(ptail, clause);
      } else {
        val opdo = if3(or2(macro_form_p(clause, menv),
                           gethash(op_table, sym)),
                       do_s, op_s);
        list_collect(ptail, cons(opdo, clause));
      }
    } else {
      list_collect(ptail, clause);
    }
  }

  return cons(dwim_s, cons(chain_chand, transformed_forms));
}

static val me_ignerr(val form, val menv)
{
  return list(catch_s, cons(progn_s, rest(form)),
              list(error_s, error_s, nao), nao);
}

static val me_whilet(val form, val env)
{
  val body = form;
  val sym = pop(&body);
  val lets = pop(&body);
  val lastlet = last(lets);
  val not_done = gensym(lit("not-done"));

  if (nilp(lastlet))
    eval_error(form, lit("~s: empty binding list"), sym, nao);

  return list(let_s, cons(list(not_done, t, nao), nil),
              list(while_s, not_done,
                   list(let_star_s, lets,
                        list(if_s, car(car(lastlet)),
                             cons(progn_s, body),
                             list(set_s, not_done, nil, nao), nao), nao), nao), nao);
}

static val me_iflet_whenlet(val form, val env)
{
  val args = form;
  val sym = pop(&args);
  val lets = pop(&args);
  val lastlet = last(lets);

  if (nilp(lastlet))
    eval_error(form, lit("~s: empty binding list"), sym, nao);

  return list(let_star_s, lets,
              cons(if3(sym == iflet_s, if_s, when_s),
                   cons(car(car(lastlet)), args)), nao);
}

static val me_dotimes(val form, val env)
{
  val count = gensym(lit("count-"));
  val args = rest(form);
  val spec = pop(&args);
  val counter = pop(&spec);
  val count_form = pop(&spec);
  val result = pop(&spec);
  val body = args;
  val lt = intern(lit("<"), user_package);
  val raw = list(for_s, list(list(counter, zero, nao),
                             list(count, count_form, nao),
                             nao),
                 list(list(lt, counter, count, nao), result, nao),
                 list(list(inc_s, counter, nao), nao),
                 body, nao);

  return apply_frob_args(raw);
}

static val me_lcons(val form, val menv)
{
  val car_form = second(form);
  val cdr_form = third(form);
  val lc_sym = gensym(lit("lcons-"));
  val make_lazy_cons = intern(lit("make-lazy-cons"), user_package);
  val rplaca = intern(lit("rplaca"), user_package);
  val rplacd = intern(lit("rplacd"), user_package);
  (void) menv;

  return list(make_lazy_cons,
              list(lambda_s, cons(lc_sym, nil),
                   list(rplaca, lc_sym, car_form, nao),
                   list(rplacd, lc_sym, cdr_form, nao), nao), nao);
}

static val me_mlet(val form, val menv)
{
  uses_or2;
  val body = cdr(form);
  val bindings = pop(&body);
  val symacrolet = intern(lit("symacrolet"), user_package);
  val delay = intern(lit("delay"), user_package);

  list_collect_decl (ordinary_syms, ptail_osyms);
  list_collect_decl (syms, ptail_syms);
  list_collect_decl (inits, ptail_inits);
  list_collect_decl (gensyms, ptail_gensyms);
  list_collect_decl (smacs, ptail_smacs);
  list_collect_decl (sets, ptail_sets);

  for (; consp(bindings); bindings = cdr(bindings)) {
    val binding = car(bindings);

    if (atom(binding)) {
      if (!bindable(binding))
        uw_throwf(error_s, lit("mlet: ~s isn't a bindable symbol"),
                  binding, nao);
      ptail_osyms = list_collect(ptail_osyms, binding);
    } else {
      val sym = car(binding);

      if (!bindable(sym))
        uw_throwf(error_s, lit("mlet: ~s isn't a bindable symbol"),
                  sym, nao);

      if (cdr(binding)) {
        val init = car(cdr(binding));
        val gen = gensym(nil);
        ptail_syms = list_collect(ptail_syms, sym);
        ptail_inits = list_collect(ptail_inits, init);
        ptail_gensyms = list_collect(ptail_gensyms, gen);
        ptail_smacs = list_collect(ptail_smacs,
                                   list(sym, list(force_s, gen, nao), nao));
        ptail_sets = list_collect(ptail_sets,
                                  list(set_s, gen,
                                       list(delay, init, nao), nao));
      } else {
        ptail_osyms = list_collect(ptail_osyms, sym);
      }
    }
  }

  if (bindings)
    uw_throwf(error_s, lit("mlet: misplaced atom ~s in binding syntax"),
              bindings, nao);

  return list(let_s, append2(ordinary_syms, gensyms),
              apply_frob_args(list(symacrolet, smacs,
                                   append2(sets, or2(body, cons(nil, nil))),
                                   nao)), nao);
}

static val sys_load(val target, val sloc)
{
  uses_or2;
  val parent = or2(cdr(sloc), null_string);
  val path = if3(abs_path_p(target),
                 target,
                 cat_str(nappend2(sub_list(split_str(parent, lit("/")),
                                           zero, negone),
                                  cons(target, nil)), lit("/")));
  val name, stream;
  val txr_lisp_p = t;

  open_txr_file(path, &txr_lisp_p, &name, &stream);

  if (!txr_lisp_p) {
    rlset(sloc, sloc);
    eval_error(sloc, lit("load doesn't process .txr files"), nao);
  }

  if (!read_eval_stream(stream, std_error, nil)) {
    rlset(sloc, sloc);
    eval_error(sloc, lit("load: ~s contains errors"), path, nao);
  }

  return nil;
}

static val me_load(val form, val menv)
{
  val args = cdr(form);
  val name = pop(&args);

  (void) menv;

  if (args)
    uw_throwf(error_s, lit("load: too many arguments"), nao);

  return list(sys_load_s, name, list(quote_s, source_loc(form), nao), nao);
}

val load(val target)
{
  return sys_load(target, nil);
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

static val do_expand(val form, val menv)
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
      val body_ex = expand_progn(body, new_menv);
      val specials_p = nil;
      val vars_ex = expand_vars(vars, menv, form, &specials_p, seq_p);
      if (body == body_ex && vars == vars_ex && !specials_p) {
        return form;
      } else {
        val basic_form = rlcp(cons(sym, cons(vars_ex, body_ex)), form);
        return expand_save_specials(basic_form, specials_p);
      }
    } else if (sym == fbind_s || sym == lbind_s) {
      val body = rest(rest(form));
      val funcs = second(form);
      val new_menv = make_fun_shadowing_env(menv, funcs);
      val body_ex = expand_progn(body, new_menv);
      val funcs_ex = expand_fbind_vars(funcs,
                                       sym == lbind_s ? new_menv : menv, form);
      if (body == body_ex && funcs == funcs_ex) {
        return form;
      } else {
        return rlcp(cons(sym, cons(funcs_ex, body_ex)), form);
      }
    } else if (sym == block_s || sym == return_from_s) {
      val name = second(form);
      val body = rest(rest(form));
      val body_ex = expand_progn(body, menv);
      if (body == body_ex)
        return form;
      return rlcp(cons(sym, cons(name, body_ex)), form);
    } else if (sym == cond_s) {
      val pairs = rest(form);
      val pairs_ex = expand_cond_pairs(pairs, menv);

      if (pairs == pairs_ex)
        return form;
      return rlcp(cons(cond_s, pairs_ex), form);
    } else if (sym == defvarl_s || sym == defsymacro_s) {
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
      if (!cdr(form))
        eval_error(form, lit("~s: missing argument list"), sym, nao);

      if (atom(cdr(form)))
        eval_error(form, lit("~s: bad syntax"), sym, nao);

      check_lambda_list(form, sym, second(form));

      {
        val params = second(form);
        val body = rest(rest(form));
        val new_menv = make_var_shadowing_env(menv, get_param_syms(params));
        val params_ex = expand_params(params, menv);
        val body_ex = expand_progn(body, new_menv);

        if (body == body_ex && params == params_ex)
          return form;
        return rlcp(cons(sym, cons(params_ex, body_ex)), form);
      }
    } else if (sym == defun_s || sym == defmacro_s) {
      val name = second(form);
      val params = third(form);

      builtin_reject_test(sym, name, form);

      if (sym == defun_s)
        check_lambda_list(form, sym, params);

      {
        val new_menv = make_var_shadowing_env(menv, get_param_syms(params));
        val params_ex = expand_params(params, menv);
        val body = rest(rest(rest(form)));
        val body_ex = expand_progn(body, new_menv);
        val form_ex = form;


        if (body != body_ex || params != params_ex)
          form_ex = rlcp(cons(sym, cons(name, cons(params_ex, body_ex))), form);

        if (sym == defmacro_s) {
          val result = eval(form_ex, make_env(nil, nil, nil), form);
          return cons(quote_s, cons(result, nil));
        }
        return form_ex;
      }
    } else if (sym == tree_case_s) {
      return expand_tree_case(form, menv);
    } else if (sym == tree_bind_s) {
      val params = second(form);
      val expr = third(form);
      val body = rest(rest(rest(form)));
      val new_menv = make_var_shadowing_env(menv, get_param_syms(params));
      val params_ex = expand_params(params, menv);
      val expr_ex = expand(expr, new_menv);
      val body_ex = expand_progn(body, new_menv);

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
      val forms_ex = expand_progn(forms, new_menv);

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
      val body_ex = expand_progn(body, menv);

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
    } else if (sym == macro_time_s) {
      val args = rest(form);
      val args_ex = expand_forms(args, menv);
      val result = eval_progn(args_ex, make_env(nil, nil, nil), args);
      return maybe_quote(result);
    } else if (sym == macrolet_s) {
      return expand_macrolet(form, menv);
    } else if (sym == symacrolet_s) {
      return expand_symacrolet(form, menv);
    } else if (sym == dwim_s) {
      val args = rest(form);
      val args_ex = expand_forms_lisp1(args, menv);

      if (args == args_ex)
        return form;
      return rlcp(cons(sym, args_ex), form);
    } else if ((macro = lookup_mac(menv, sym))) {
      val mac_expand = expand_macro(form, macro, menv);
      if (mac_expand == form)
        return form;
      form = rlcp_tree(mac_expand, form);
      goto tail;
    } else if (sym == progn_s) {
      val args = rest(form);
      val args_ex = expand_progn(args, menv);

      if (args == args_ex)
        return form;
      return rlcp(cons(sym, args_ex), form);
    } else if (sym == sys_lisp1_value_s) {
      return expand_lisp1_value(form, menv);
    } else {
      /* funtion call
         also handles: prog1, call, if, and, or,
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

val expand(val form, val menv)
{
  val ret = nil;
  val lfe_save = last_form_expanded;

  last_form_expanded = form;
  ret = do_expand(form, menv);
  last_form_expanded = lfe_save;

  return ret;
}

val macro_form_p(val form, val menv)
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

static val constantp_noex(val form)
{
  if (consp(form)) {
    return eq(car(form), quote_s);
  } else {
    if (bindable(form))
      return nil;
    return t;
  }
}

static val constantp(val form, val env_in)
{
  val env = default_bool_arg(env_in);

  if (consp(form)) {
    if (car(form) == quote_s) {
      return t;
    } else if (macro_form_p(form, env)) {
      return constantp_noex(macroexpand(form, env));
    } else {
      return nil;
    }
  } else if (symbolp(form)) {
    if (!bindable(form)) {
      return t;
    } else if (macro_form_p(form, env)) {
      return constantp_noex(macroexpand(form, env));
    } else {
      return nil;
    }
  } else {
    return t;
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

      otail = list_collect(otail, apply(fun, z(args), nil));
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

      otail = list_collect_append(otail, apply(fun, z(args), nil));
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

  rplaca(lcons, apply(fun, z(args), nil));
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

static val mapdov(val fun, val list_of_lists)
{
  if (!cdr(list_of_lists)) {
    return mapdo(fun, car(list_of_lists));
  } else {
    val lofl = mapcar_listout(func_n1(nullify), list_of_lists);

    for (;;) {
      val iter;
      list_collect_decl (args, atail);

      for (iter = lofl; iter; iter = cdr(iter)) {
        val list = car(iter);
        if (!list)
          return nil;
        atail = list_collect(atail, car(list));
        deref(car_l(iter)) = cdr(list);
      }

      apply(fun, z(args), nil);
    }
  }
}

static val symbol_value(val sym)
{
  uses_or2;

  return cdr(or2(lookup_var(nil, sym),
                 lookup_symac(nil, sym)));
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
  return if2(lookup_var(nil, sym) || lookup_symac(nil, sym), t);
}

val fboundp(val sym)
{
  return if2(lookup_fun(nil, sym) || lookup_mac(nil, sym) ||
             gethash(op_table, sym), t);
}

val special_operator_p(val sym)
{
  return if2(gethash(op_table, sym), t);
}

static val makunbound(val sym)
{
  lisplib_try_load(sym),
  remhash(top_vb, sym);
  remhash(top_smb, sym);
  remhash(special, sym);
  return sym;
}

static val fmakunbound(val sym)
{
  lisplib_try_load(sym),
  remhash(top_fb, sym);
  remhash(top_mb, sym);
  return sym;
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

static val giterate_func(val env, val lcons)
{
  cons_bind (while_pred, gen_fun, env);
  val next_item = funcall1(gen_fun, lcons->lc.car);

  if (funcall1(while_pred, next_item)) {
    val lcons_next = make_lazy_cons(lcons_fun(lcons));
    rplacd(lcons, lcons_next);
    rplaca(lcons_next, next_item);
  }
  return nil;
}

static val giterate(val while_pred, val gen_fun, val init_val)
{
  init_val = default_bool_arg(init_val);

  if (!funcall1(while_pred, init_val)) {
    return nil;
  } else {
    val lc = make_lazy_cons(func_f1(cons(while_pred, gen_fun), giterate_func));
    rplaca(lc, init_val);
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

static val repeat(val list, val count)
{
  if (!list)
    return nil;

  if (!missingp(count)) {
    if (le(count, zero))
      return nil;
    return make_lazy_cons(func_f1(cons(list, cons(list, count)),
                                  repeat_times_func));
  }
  return make_lazy_cons(func_f1(cons(list, list), repeat_infinite_func));
}

static val pad_func(val env, val lcons)
{
  cons_bind (list, item_count, env);
  val next = cdr(list);

  rplaca(lcons, car(list));

  if (next && seqp(next)) {
    rplaca(env, next);
    rplacd(lcons, make_lazy_cons(lcons_fun(lcons)));
    return nil;
  } else if (!next) {
    val count = cdr(item_count);
    rplacd(item_count, nil);
    rplacd(lcons, repeat(item_count, count));
  } else {
    uw_throwf(error_s, lit("pad: cannot pad improper list terminated by ~s"),
              next, nao);
  }

  return nil;
}

static val pad(val list, val item_in, val count)
{
  val item = default_bool_arg(item_in);

  switch (type(list)) {
  case NIL:
    return repeat(cons(item, nil), count);
  case CONS:
    return append2(list, repeat(cons(item, nil), count));
  case LCONS:
  case VEC:
  case LIT:
  case STR:
  case LSTR:
    return make_lazy_cons(func_f1(cons(list, cons(item, count)), pad_func));
  default:
    uw_throwf(error_s, lit("pad: cannot pad ~s, only sequences"), list, nao);
  }
}

static val weave_while(val env)
{
  cons_bind (uniq, tuples, env);
  val tuple;

  if (!tuples)
    return nil;

  tuple = remq(uniq, car(tuples));

  if (!tuple)
    return nil;

  rplaca(tuples, tuple);
  return t;
}

static val weave_gen(val env)
{
  val tuples = cdr(env);
  val ret = car(tuples);
  rplacd(env, cdr(tuples));
  return ret;
}

static val weavev(val lists)
{
  val uniq = cons(nil, nil);
  val padded_lists = mapcar(curry_123_1(func_n3(pad), uniq, colon_k), lists);
  val tuples = lazy_mapcarv(list_f, padded_lists);
  val env = cons(uniq, tuples);
  val whil = func_f0(env, weave_while);
  val gen = func_f0(env, weave_gen);
  return lazy_appendv(generate(whil, gen));
}

static val force(val promise)
{
  loc pstate = car_l(promise);
  val cd = cdr(promise);
  loc pval = car_l(cd);

  if (deref(pstate) == promise_forced_s) {
    return deref(pval);
  } else if (deref(pstate) == promise_s) {
    val ret;
    /* Safe: promise symbols are older generation */
    deref(pstate) = promise_inprogress_s;
    ret = funcall(deref(pval));
    deref(pstate) = promise_forced_s;
    deref(pval) = ret;
    return ret;
  } else if (deref(pstate) == promise_inprogress_s) {
    val form = second(cdr(cd));
    val sloc = source_loc_str(form);
    eval_error(nil, lit("force: recursion forcing delayed form ~s (~a)"),
               form, sloc, nao);
  } else {
    uw_throwf(error_s, lit("force: ~s is not a promise"), promise, nao);
  }
}

static void reg_op(val sym, opfun_t fun)
{
  assert (sym != 0);
  sethash(op_table, sym, cptr(coerce(mem_t *, fun)));
}

void reg_fun(val sym, val fun)
{
  assert (sym != 0);
  sethash(top_fb, sym, cons(sym, fun));
  sethash(builtin, sym, defun_s);
}

static void reg_mac(val sym, mefun_t fun)
{
  assert (sym != 0);
  sethash(top_mb, sym, cptr(coerce(mem_t *, fun)));
  sethash(builtin, sym, defmacro_s);
}

void reg_varl(val sym, val val)
{
  assert (sym != nil);
  sethash(top_vb, sym, cons(sym, val));
}

void reg_var(val sym, val val)
{
  reg_varl(sym, val);
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
  return apply_intrinsic(fun, z(args));
}

static val apf(val fun)
{
  return func_f0v(fun, do_apf);
}

static val do_ipf(val fun, val args)
{
  return iapply(fun, z(args));
}

static val ipf(val fun)
{
  return func_f0v(fun, do_ipf);
}

static val callf(val func, val funlist)
{
  val juxt_fun = juxtv(funlist);
  val apf_fun = apf(func);
  return chain(juxt_fun, apf_fun, nao);
}

static val do_mapf(val env, val args)
{
  cons_bind (fun, funlist, env);
  val mapped_args = mapcarv(call_f, cons(funlist, cons(z(args), nil)));
  return apply(fun, z(mapped_args), nil);
}

static val mapf(val fun, val funlist)
{
  return func_f0v(cons(fun, funlist), do_mapf);
}

val prinl(val obj, val stream)
{
  val ret = obj_print(obj, stream);
  put_char(chr('\n'), stream);
  return ret;
}

val pprinl(val obj, val stream)
{
  val ret = obj_pprint(obj, stream);
  put_char(chr('\n'), stream);
  return ret;
}

val tprint(val obj, val out)
{
  switch (type(obj)) {
  case NIL:
    break;
  case CONS:
  case LCONS:
  case VEC:
    mapdo(curry_12_1(func_n2(tprint), out), obj);
    break;
  case LIT:
  case STR:
  case LSTR:
    put_line(obj, out);
    break;
  default:
    pprinl(obj, out);
    break;
  }

  return nil;
}

static val merge_wrap(val seq1, val seq2, val lessfun, val keyfun)
{
  if (!nullify(seq1)) {
    if (type(seq1) == type(seq2))
      return seq2;
    return make_like(tolist(seq2), seq1);
  } else if (!nullify(seq2)) {
    if (type(seq1) == type(seq2))
      return seq1;
    return make_like(tolist(seq1), seq1);
  } else {
    val list1 = tolist(seq1);
    val list2 = tolist(seq2);

    keyfun = default_arg(keyfun, identity_f);
    lessfun = default_arg(lessfun, less_f);

    return make_like(merge(list1, list2, lessfun, keyfun), seq1);
  }
}

void eval_init(void)
{
  val not_null_f = func_n1(not_null);

  protect(&top_vb, &top_fb, &top_mb, &top_smb, &special, &builtin, &dyn_env,
          &op_table, &last_form_evaled, &last_form_expanded,
          &call_f, convert(val *, 0));
  top_fb = make_hash(t, nil, nil);
  top_vb = make_hash(t, nil, nil);
  top_mb = make_hash(t, nil, nil);
  top_smb = make_hash(t, nil, nil);
  special = make_hash(t, nil, nil);
  builtin = make_hash(t, nil, nil);
  op_table = make_hash(nil, nil, nil);

  eval_initing = t;

  call_f = func_n1v(call);

  dwim_s = intern(lit("dwim"), user_package);
  progn_s = intern(lit("progn"), user_package);
  prog1_s = intern(lit("prog1"), user_package);
  let_s = intern(lit("let"), user_package);
  let_star_s = intern(lit("let*"), user_package);
  lambda_s = intern(lit("lambda"), user_package);
  fbind_s = intern(lit("fbind"), system_package);
  lbind_s = intern(lit("lbind"), system_package);
  flet_s = intern(lit("flet"), user_package);
  labels_s = intern(lit("labels"), user_package);
  call_s = intern(lit("call"), user_package);
  cond_s = intern(lit("cond"), user_package);
  caseq_s = intern(lit("caseq"), user_package);
  caseql_s = intern(lit("caseql"), user_package);
  casequal_s = intern(lit("casequal"), user_package);
  memq_s = intern(lit("memq"), user_package);
  memql_s = intern(lit("memql"), user_package);
  memqual_s = intern(lit("memqual"), user_package);
  eq_s = intern(lit("eq"), user_package);
  eql_s = intern(lit("eql"), user_package);
  equal_s = intern(lit("equal"), user_package);
  if_s = intern(lit("if"), user_package);
  when_s = intern(lit("when"), user_package);
  iflet_s = intern(lit("iflet"), user_package);
  defvar_s = intern(lit("defvar"), user_package);
  defvarl_s = intern(lit("defvarl"), user_package);
  defparm_s = intern(lit("defparm"), user_package);
  defparml_s = intern(lit("defparml"), user_package);
  sys_mark_special_s = intern(lit("mark-special"), system_package);
  defun_s = intern(lit("defun"), user_package);
  defmacro_s = intern(lit("defmacro"), user_package);
  defsymacro_s = intern(lit("defsymacro"), user_package);
  tree_case_s = intern(lit("tree-case"), user_package);
  tree_bind_s = intern(lit("tree-bind"), user_package);
  setq_s = intern(lit("setq"), system_package);
  inc_s = intern(lit("inc"), user_package);
  zap_s = intern(lit("zap"), user_package);
  for_s = intern(lit("for"), user_package);
  for_star_s = intern(lit("for*"), user_package);
  each_s = intern(lit("each"), user_package);
  each_star_s = intern(lit("each*"), user_package);
  collect_each_s = intern(lit("collect-each"), user_package);
  collect_each_star_s = intern(lit("collect-each*"), user_package);
  append_each_s = intern(lit("append-each"), user_package);
  append_each_star_s = intern(lit("append-each*"), user_package);
  dohash_s = intern(lit("dohash"), user_package);
  while_s = intern(lit("while"), user_package);
  while_star_s = intern(lit("while*"), user_package);
  until_star_s = intern(lit("until*"), user_package);
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
  promise_forced_s = intern(lit("promise-forced"), system_package);
  promise_inprogress_s = intern(lit("promise-inprogress"), system_package);
  force_s = intern(lit("force"), user_package);
  op_s = intern(lit("op"), user_package);
  ap_s = intern(lit("ap"), user_package);
  do_s = intern(lit("do"), user_package);
  apf_s = intern(lit("apf"), user_package);
  ipf_s = intern(lit("ipf"), user_package);
  ret_s = intern(lit("ret"), user_package);
  aret_s = intern(lit("aret"), user_package);
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
  symacro_k = intern(lit("symacro"), keyword_package);
  fun_k = intern(lit("fun"), keyword_package);
  prof_s = intern(lit("prof"), user_package);
  opip_s = intern(lit("opip"), user_package);
  oand_s = intern(lit("oand"), user_package);
  chain_s = intern(lit("chain"), user_package);
  chand_s = intern(lit("chand"), user_package);
  sys_load_s = intern(lit("load"), system_package);
  sys_lisp1_value_s = intern(lit("lisp1-value"), system_package);

  with_saved_vars_s = intern(lit("with-saved-vars"), system_package);
  reg_op(macrolet_s, op_error);
  reg_op(symacrolet_s, op_error);
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
  reg_op(fbind_s, op_fbind);
  reg_op(lbind_s, op_fbind);
  reg_op(lambda_s, op_lambda);
  reg_op(fun_s, op_fun);
  reg_op(cond_s, op_cond);
  reg_op(if_s, op_if);
  reg_op(and_s, op_and);
  reg_op(or_s, op_or);
  reg_op(defvarl_s, op_defvarl);
  reg_op(defun_s, op_defun);
  reg_op(defmacro_s, op_defmacro);
  reg_op(defsymacro_s, op_defsymacro);
  reg_op(tree_case_s, op_tree_case);
  reg_op(tree_bind_s, op_tree_bind);
  reg_op(intern(lit("mac-param-bind"), user_package), op_mac_param_bind);
  reg_op(setq_s, op_setq);
  reg_op(intern(lit("lisp1-setq"), system_package), op_lisp1_setq);
  reg_op(sys_lisp1_value_s, op_lisp1_value);
  reg_op(intern(lit("setqf"), system_package), op_setqf);
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

  reg_mac(defvar_s, me_def_variable);
  reg_mac(defparm_s, me_def_variable);
  reg_mac(defparml_s, me_def_variable);
  reg_mac(gen_s, me_gen);
  reg_mac(gun_s, me_gun);
  reg_mac(intern(lit("delay"), user_package), me_delay);
  reg_mac(op_s, me_op);
  reg_mac(do_s, me_op);
  reg_mac(ap_s, me_ap);
  reg_mac(intern(lit("ip"), user_package), me_ip);
  reg_mac(intern(lit("ado"), user_package), me_ado);
  reg_mac(intern(lit("ido"), user_package), me_ido);
  reg_mac(ret_s, me_ret_aret);
  reg_mac(aret_s, me_ret_aret);
  reg_mac(qquote_s, me_qquote);
  reg_mac(sys_qquote_s, me_qquote);
  reg_mac(intern(lit("pprof"), user_package), me_pprof);
  reg_mac(when_s, me_when);
  reg_mac(intern(lit("unless"), user_package), me_unless);
  reg_mac(while_s, me_while);
  reg_mac(while_star_s, me_while_star);
  reg_mac(until_s, me_until);
  reg_mac(until_star_s, me_until_star);
  reg_mac(quasilist_s, me_quasilist);
  reg_mac(flet_s, me_flet_labels);
  reg_mac(labels_s, me_flet_labels);
  reg_mac(caseq_s, me_case);
  reg_mac(caseql_s, me_case);
  reg_mac(casequal_s, me_case);
  reg_mac(intern(lit("tb"), user_package), me_tb);
  reg_mac(intern(lit("tc"), user_package), me_tc);
  reg_mac(opip_s, me_opip);
  reg_mac(oand_s, me_opip);
  reg_mac(intern(lit("ignerr"), user_package), me_ignerr);
  reg_mac(intern(lit("whilet"), user_package), me_whilet);
  reg_mac(iflet_s, me_iflet_whenlet);
  reg_mac(intern(lit("whenlet"), user_package), me_iflet_whenlet);
  reg_mac(intern(lit("dotimes"), user_package), me_dotimes);
  reg_mac(intern(lit("lcons"), user_package), me_lcons);
  reg_mac(intern(lit("mlet"), user_package), me_mlet);
  reg_mac(intern(lit("load"), user_package), me_load);

  reg_fun(cons_s, func_n2(cons));
  reg_fun(intern(lit("make-lazy-cons"), user_package), func_n1(make_lazy_cons));
  reg_fun(intern(lit("lcons-fun"), user_package), func_n1(lcons_fun));
  reg_fun(car_s, car_f);
  reg_fun(cdr_s, cdr_f);
  reg_fun(intern(lit("rplaca"), user_package), func_n2(rplaca));
  reg_fun(intern(lit("rplacd"), user_package), func_n2(rplacd));
  reg_fun(intern(lit("rplaca"), system_package), func_n2(sys_rplaca));
  reg_fun(intern(lit("rplacd"), system_package), func_n2(sys_rplacd));
  reg_fun(intern(lit("first"), user_package), func_n1(car));
  reg_fun(rest_s, func_n1(cdr));
  reg_fun(intern(lit("sub-list"), user_package), func_n3o(sub_list, 1));
  reg_fun(intern(lit("replace-list"), user_package), func_n4o(replace_list, 2));
  reg_fun(append_s, func_n0v(appendv));
  reg_fun(intern(lit("append*"), user_package), func_n0v(lazy_appendv));
  reg_fun(intern(lit("nconc"), user_package), func_n0v(nconcv));
  reg_fun(list_s, list_f);
  reg_fun(intern(lit("list*"), user_package), func_n0v(list_star_intrinsic));
  reg_fun(identity_s, identity_f);
  reg_fun(intern(lit("typeof"), user_package), func_n1(typeof));

  reg_fun(intern(lit("atom"), user_package), func_n1(atom));
  reg_fun(intern(lit("null"), user_package), null_f);
  reg_fun(intern(lit("false"), user_package), null_f);
  reg_fun(intern(lit("true"), user_package), not_null_f);
  reg_fun(intern(lit("have"), user_package), not_null_f);
  reg_fun(not_s, null_f);
  reg_fun(intern(lit("consp"), user_package), func_n1(consp));
  reg_fun(intern(lit("lconsp"), user_package), func_n1(lconsp));
  reg_fun(intern(lit("listp"), user_package), func_n1(listp));
  reg_fun(intern(lit("proper-listp"), user_package), func_n1(proper_listp));
  reg_fun(intern(lit("length-list"), user_package), func_n1(length_list));

  reg_fun(intern(lit("mapcar"), user_package), func_n1v(mapcarv));
  reg_fun(intern(lit("mapcar*"), user_package), func_n1v(lazy_mapcarv));
  reg_fun(intern(lit("mappend"), user_package), func_n1v(mappendv));
  reg_fun(intern(lit("mappend*"), user_package), func_n1v(lazy_mappendv));
  reg_fun(intern(lit("mapdo"), user_package), func_n1v(mapdov));
  reg_fun(apply_s, func_n1v(apply_intrinsic));
  reg_fun(iapply_s, func_n1v(iapply));
  reg_fun(call_s, call_f);
  reg_fun(intern(lit("reduce-left"), user_package), func_n4o(reduce_left, 2));
  reg_fun(intern(lit("reduce-right"), user_package), func_n4o(reduce_right, 2));
  reg_fun(intern(lit("transpose"), user_package), func_n1(transpose));
  reg_fun(intern(lit("zip"), user_package), func_n0v(transpose));
  reg_fun(intern(lit("interpose"), user_package), func_n2(interpose));

  reg_fun(intern(lit("second"), user_package), func_n1(second));
  reg_fun(intern(lit("third"), user_package), func_n1(third));
  reg_fun(intern(lit("fourth"), user_package), func_n1(fourth));
  reg_fun(intern(lit("fifth"), user_package), func_n1(fifth));
  reg_fun(intern(lit("sixth"), user_package), func_n1(sixth));
  reg_fun(intern(lit("seventh"), user_package), func_n1(seventh));
  reg_fun(intern(lit("eighth"), user_package), func_n1(eighth));
  reg_fun(intern(lit("ninth"), user_package), func_n1(ninth));
  reg_fun(intern(lit("tenth"), user_package), func_n1(tenth));
  reg_fun(intern(lit("conses"), user_package), func_n1(conses));
  reg_fun(intern(lit("conses*"), user_package), func_n1(lazy_conses));
  reg_fun(intern(lit("copy-list"), user_package), func_n1(copy_list));
  reg_fun(intern(lit("nreverse"), user_package), func_n1(nreverse));
  reg_fun(intern(lit("reverse"), user_package), func_n1(reverse));
  reg_fun(intern(lit("ldiff"), user_package), func_n2(ldiff));
  reg_fun(intern(lit("last"), user_package), func_n1(last));
  reg_fun(intern(lit("nthcdr"), user_package), func_n2(nthcdr));
  reg_fun(intern(lit("flatten"), user_package), func_n1(flatten));
  reg_fun(intern(lit("flatten*"), user_package), func_n1(lazy_flatten));
  reg_fun(intern(lit("tuples"), user_package), func_n3o(tuples, 2));
  reg_fun(intern(lit("partition-by"), user_package), func_n2(partition_by));
  reg_fun(intern(lit("partition"), user_package), func_n2(partition));
  reg_fun(intern(lit("split"), user_package), func_n2(split));
  reg_fun(intern(lit("partition*"), user_package), func_n2(partition_star));
  reg_fun(memq_s, func_n2(memq));
  reg_fun(memql_s, func_n2(memql));
  reg_fun(memqual_s, func_n2(memqual));
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
  reg_fun(eq_s, eq_f);
  reg_fun(eql_s, eql_f);
  reg_fun(equal_s, equal_f);

  reg_fun(plus_s = intern(lit("+"), user_package), func_n0v(plusv));
  reg_fun(intern(lit("-"), user_package), func_n1v(minusv));
  reg_fun(intern(lit("*"), user_package), func_n0v(mulv));
  reg_fun(intern(lit("abs"), user_package), func_n1(abso));
  reg_fun(intern(lit("trunc"), user_package), func_n2(trunc));
  reg_fun(intern(lit("mod"), user_package), func_n2(mod));
  reg_fun(intern(lit("trunc-rem"), user_package), func_n2(trunc_rem));
  reg_fun(intern(lit("wrap"), user_package), func_n3(wrap));
  reg_fun(intern(lit("wrap*"), user_package), func_n3(wrap_star));
  reg_fun(intern(lit("/"), user_package), func_n2o(divi, 1));
  reg_fun(intern(lit("expt"), user_package), func_n0v(exptv));
  reg_fun(intern(lit("exptmod"), user_package), func_n3(exptmod));
  reg_fun(intern(lit("isqrt"), user_package), func_n1(isqrt));
  reg_fun(intern(lit("gcd"), user_package), func_n0v(gcdv));
  reg_fun(intern(lit("lcm"), user_package), func_n0v(lcmv));
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
  reg_fun(intern(lit("log2"), user_package), func_n1(logtwo));
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
  reg_fun(intern(lit("plusp"), user_package), func_n1(plusp));
  reg_fun(intern(lit("minusp"), user_package), func_n1(minusp));
  reg_fun(intern(lit("evenp"), user_package), func_n1(evenp));
  reg_fun(intern(lit("oddp"), user_package), func_n1(oddp));
  reg_fun(intern(lit("succ"), user_package), func_n1(succ));
  reg_fun(intern(lit("ssucc"), user_package), func_n1(ssucc));
  reg_fun(intern(lit("sssucc"), user_package), func_n1(sssucc));
  reg_fun(intern(lit("pred"), user_package), func_n1(pred));
  reg_fun(intern(lit("ppred"), user_package), func_n1(ppred));
  reg_fun(intern(lit("pppred"), user_package), func_n1(pppred));
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
  reg_fun(intern(lit("in"), user_package), func_n4o(in, 2));
  reg_fun(intern(lit("logand"), user_package), func_n0v(logandv));
  reg_fun(intern(lit("logior"), user_package), func_n0v(logiorv));
  reg_fun(intern(lit("logxor"), user_package), func_n2(logxor));
  reg_fun(intern(lit("logtest"), user_package), func_n2(logtest));
  reg_fun(intern(lit("lognot"), user_package), func_n2o(lognot, 1));
  reg_fun(intern(lit("logtrunc"), user_package), func_n2(logtrunc));
  reg_fun(intern(lit("sign-extend"), user_package), func_n2(sign_extend));
  reg_fun(intern(lit("ash"), user_package), func_n2(ash));
  reg_fun(intern(lit("bit"), user_package), func_n2(bit));
  reg_fun(intern(lit("mask"), user_package), func_n0v(maskv));
  reg_fun(intern(lit("width"), user_package), func_n1(width));

  reg_fun(intern(lit("regex-compile"), user_package), func_n2o(regex_compile, 1));
  reg_fun(intern(lit("regexp"), user_package), func_n1(regexp));
  reg_fun(intern(lit("search-regex"), user_package), func_n4o(search_regex, 2));
  reg_fun(intern(lit("range-regex"), user_package), func_n4o(range_regex, 2));
  reg_fun(intern(lit("search-regst"), user_package), func_n4o(search_regst, 2));
  reg_fun(intern(lit("match-regex"), user_package), func_n3o(match_regex, 2));
  reg_fun(intern(lit("match-regst"), user_package), func_n3o(match_regst, 2));
  reg_fun(intern(lit("match-regex-right"), user_package),
          func_n3o(match_regex_right, 2));
  reg_fun(intern(lit("match-regst-right"), user_package),
          func_n3o(match_regst_right, 2));
  reg_fun(intern(lit("regsub"), user_package), func_n3(regsub));
  reg_fun(intern(lit("regex-parse"), user_package), func_n2o(regex_parse, 1));

  reg_fun(intern(lit("make-hash"), user_package), func_n3(make_hash));
  reg_fun(intern(lit("make-similar-hash"), user_package), func_n1(make_similar_hash));
  reg_fun(intern(lit("copy-hash"), user_package), func_n1(copy_hash));
  reg_fun(intern(lit("hash"), user_package), func_n0v(hashv));
  reg_fun(intern(lit("hash-construct"), user_package), func_n2(hash_construct));
  reg_fun(intern(lit("hash-from-pairs"), user_package), func_n1v(hash_from_pairs));
  reg_fun(intern(lit("hash-list"), user_package), func_n1v(hash_list));
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
  reg_fun(intern(lit("hash-subset"), user_package), func_n2(hash_subset));
  reg_fun(intern(lit("hash-proper-subset"), user_package), func_n2(hash_proper_subset));
  reg_fun(intern(lit("group-by"), user_package), func_n2v(group_by));
  reg_fun(intern(lit("sort-group"), user_package), func_n3o(sort_group, 1));
  reg_fun(intern(lit("unique"), user_package), func_n2ov(unique, 1));
  reg_fun(intern(lit("uniq"), user_package), func_n1(uniq));
  reg_fun(intern(lit("hash-update"), user_package), func_n2(hash_update));
  reg_fun(intern(lit("hash-update-1"), user_package),
          func_n4o(hash_update_1, 3));
  reg_fun(intern(lit("hash-revget"), user_package), func_n4o(hash_revget, 2));

  reg_fun(intern(lit("eval"), user_package), func_n2o(eval_intrinsic, 1));
  reg_fun(intern(lit("lisp-parse"), user_package), func_n4o(lisp_parse, 0));
  reg_fun(intern(lit("read"), user_package), func_n4o(lisp_parse, 0));
  reg_fun(intern(lit("load"), system_package), func_n2(sys_load));
  reg_fun(intern(lit("expand"), system_package), func_n2o(expand, 1));
  reg_fun(intern(lit("macro-form-p"), user_package), func_n2o(macro_form_p, 1));
  reg_fun(intern(lit("macroexpand-1"), user_package),
          func_n2o(macroexpand_1, 1));
  reg_fun(intern(lit("macroexpand"), user_package),
          func_n2o(macroexpand, 1));
  reg_fun(intern(lit("constantp"), user_package), func_n2o(constantp, 1));
  reg_fun(intern(lit("make-env"), user_package), func_n3o(make_env_intrinsic, 0));
  reg_fun(intern(lit("env-fbind"), user_package), func_n3(env_fbind));
  reg_fun(intern(lit("env-vbind"), user_package), func_n3(env_vbind));
  reg_fun(intern(lit("lexical-var-p"), user_package), func_n2(lexical_var_p));
  reg_fun(intern(lit("lexical-fun-p"), user_package), func_n2(lexical_fun_p));
  reg_fun(intern(lit("lexical-lisp1-binding"), user_package),
          func_n2(lexical_lisp1_binding));
  reg_fun(chain_s, func_n0v(chainv));
  reg_fun(chand_s, func_n0v(chandv));
  reg_fun(intern(lit("juxt"), user_package), func_n0v(juxtv));
  reg_fun(intern(lit("andf"), user_package), func_n0v(andv));
  reg_fun(intern(lit("orf"), user_package), func_n0v(orv));
  reg_fun(intern(lit("notf"), user_package), func_n1(notf));
  reg_fun(intern(lit("iff"), user_package), func_n3o(iff, 1));
  reg_fun(intern(lit("iffi"), user_package), func_n3o(iffi, 2));
  reg_fun(intern(lit("dup"), user_package), func_n1(dupl));
  reg_fun(intern(lit("flipargs"), user_package), func_n1(swap_12_21));
  reg_fun(intern(lit("if"), user_package), func_n3o(if_fun, 2));
  reg_fun(intern(lit("or"), user_package), func_n0v(or_fun));
  reg_fun(intern(lit("and"), user_package), func_n0v(and_fun));
  reg_fun(intern(lit("retf"), user_package), func_n1(retf));
  reg_fun(apf_s, func_n1(apf));
  reg_fun(ipf_s, func_n1(ipf));
  reg_fun(intern(lit("callf"), user_package), func_n1v(callf));
  reg_fun(intern(lit("mapf"), user_package), func_n1v(mapf));
  reg_fun(intern(lit("tf"), user_package), func_n0v(tf));
  reg_fun(intern(lit("nilf"), user_package), func_n0v(nilf));

  reg_fun(intern(lit("print"), user_package), func_n2o(obj_print, 1));
  reg_fun(intern(lit("pprint"), user_package), func_n2o(obj_pprint, 1));
  reg_fun(intern(lit("tostring"), user_package), func_n1(tostring));
  reg_fun(intern(lit("tostringp"), user_package), func_n1(tostringp));
  reg_fun(intern(lit("prinl"), user_package), func_n2o(prinl, 1));
  reg_fun(intern(lit("pprinl"), user_package), func_n2o(pprinl, 1));
  reg_fun(intern(lit("tprint"), user_package), func_n2o(tprint, 1));

  reg_varl(user_package_s = intern(lit("user-package"), user_package_var),
           user_package_var);
  reg_varl(system_package_s = intern(lit("system-package"), user_package_var),
           system_package_var);
  reg_varl(keyword_package_s = intern(lit("keyword-package"), user_package_var),
           keyword_package_var);

  reg_varl(intern(lit("*user-package*"), user_package), user_package_var);
  reg_varl(intern(lit("*system-package*"), user_package), system_package_var);
  reg_varl(intern(lit("*keyword-package*"), user_package), keyword_package_var);

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
  reg_fun(intern(lit("bindable"), user_package), func_n1(bindable));
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
  reg_fun(intern(lit("less"), user_package), func_n1v(lessv));
  reg_fun(intern(lit("greater"), user_package), func_n1v(greaterv));
  reg_fun(intern(lit("lequal"), user_package), func_n1v(lequalv));
  reg_fun(intern(lit("gequal"), user_package), func_n1v(gequalv));
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
  reg_fun(intern(lit("chr-isblank"), user_package), func_n1(chr_isblank));
  reg_fun(intern(lit("chr-isunisp"), user_package), func_n1(chr_isunisp));
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
  reg_fun(intern(lit("get-lines"), user_package), func_n1o(lazy_stream_cons, 0));
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
  reg_fun(intern(lit("merge"), user_package), func_n4o(merge_wrap, 2));
  reg_fun(intern(lit("sort"), user_package), func_n3o(sort, 1));
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
  reg_fun(intern(lit("dwim-set"), system_package), func_n3(dwim_set));
  reg_fun(intern(lit("dwim-del"), system_package), func_n2(dwim_del));
  reg_fun(intern(lit("update"), user_package), func_n2(update));
  reg_fun(intern(lit("search"), user_package), func_n4o(search, 2));
  reg_fun(intern(lit("where"), user_package), func_n2(where));
  reg_fun(intern(lit("select"), user_package), func_n2(sel));

  reg_fun(intern(lit("make-like"), user_package), func_n2(make_like));
  reg_fun(intern(lit("nullify"), user_package), func_n1(nullify));

  reg_varl(intern(lit("top-vb"), system_package), top_vb);
  reg_varl(intern(lit("top-fb"), system_package), top_fb);
  reg_fun(intern(lit("symbol-value"), user_package), func_n1(symbol_value));
  reg_fun(intern(lit("symbol-function"), user_package), func_n1(symbol_function));
  reg_fun(intern(lit("boundp"), user_package), func_n1(boundp));
  reg_fun(intern(lit("fboundp"), user_package), func_n1(fboundp));
  reg_fun(intern(lit("makunbound"), user_package), func_n1(makunbound));
  reg_fun(intern(lit("fmakunbound"), user_package), func_n1(fmakunbound));
  reg_fun(intern(lit("special-operator-p"), user_package), func_n1(special_operator_p));
  reg_fun(intern(lit("special-var-p"), user_package), func_n1(special_var_p));
  reg_fun(sys_mark_special_s, func_n1(mark_special));
  reg_fun(intern(lit("func-get-form"), user_package), func_n1(func_get_form));
  reg_fun(intern(lit("func-get-env"), user_package), func_n1(func_get_env));
  reg_fun(intern(lit("func-set-env"), user_package), func_n2(func_set_env));
  reg_fun(intern(lit("functionp"), user_package), func_n1(functionp));
  reg_fun(intern(lit("interp-fun-p"), user_package), func_n1(interp_fun_p));

  reg_fun(intern(lit("make-random-state"), user_package), func_n1o(make_random_state, 0));
  reg_fun(intern(lit("random-state-p"), user_package), func_n1(random_state_p));
  reg_fun(intern(lit("random-fixnum"), user_package), func_n1o(random_fixnum, 0));
  reg_fun(intern(lit("random"), user_package), func_n2(random));
  reg_fun(intern(lit("rand"), user_package), func_n2o(rnd, 1));

  reg_fun(intern(lit("range"), user_package), func_n0v(rangev));
  reg_fun(intern(lit("range*"), user_package), func_n0v(range_star_v));
  reg_fun(generate_s, func_n2(generate));
  reg_fun(intern(lit("giterate"), user_package), func_n3o(giterate, 2));
  reg_fun(intern(lit("repeat"), user_package), func_n2o(repeat, 1));
  reg_fun(intern(lit("pad"), user_package), func_n3o(pad, 1));
  reg_fun(intern(lit("weave"), user_package), func_n0v(weavev));
  reg_fun(force_s, func_n1(force));
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

  reg_fun(intern(lit("source-loc"), user_package), func_n1(source_loc));
  reg_fun(intern(lit("source-loc-str"), user_package), func_n1(source_loc_str));
  reg_fun(intern(lit("rlcp"), user_package), func_n2(rlcp));

  eval_error_s = intern(lit("eval-error"), user_package);
  uw_register_subtype(eval_error_s, error_s);

  lisplib_init();

  eval_initing = nil;
}

void eval_compat_fixup(int compat_ver)
{
  if (compat_ver <= 107)
    reg_fun(intern(lit("flip"), user_package), func_n1(swap_12_21));
}
