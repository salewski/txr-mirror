/* Copyright 2010-2017
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

extern val dwim_s, lambda_s, vector_lit_s, vec_list_s, list_s;
extern val hash_lit_s, hash_construct_s, struct_lit_s, qref_s, uref_s;
extern val eval_error_s, if_s, call_s;
extern val eq_s, eql_s, equal_s;
extern val car_s, cdr_s;
extern val last_form_evaled, last_form_expanded;
extern val load_path_s, load_recursive_s;

#define load_path (deref(lookup_var_l(nil, load_path_s)))

noreturn val eval_error(val ctx, val fmt, ...);
val ctx_form(val obj);
val ctx_name(val obj);
val lookup_origin(val form);
val set_last_form_evaled(val form);
void error_trace(val exsym, val exvals, val out_stream, val prefix);
val make_env(val fbindings, val vbindings, val up_env);
val copy_env(val oenv);
val env_fbind(val env, val sym, val fun);
val env_vbind(val env, val sym, val obj);
val lookup_var(val env, val sym);
val lookup_global_var(val sym);
loc lookup_var_l(val env, val sym);
loc lookup_global_var_l(val sym);
val lookup_fun(val env, val sym);
val set_dyn_env(val de);
val funcall_interp(val interp_fun, struct args *);
val boundp(val sym);
val fboundp(val sym);
val mboundp(val sym);
val special_operator_p(val sym);
val macro_form_p(val form, val menv);
val func_get_name(val fun, val env);
void reg_varl(val sym, val val);
void reg_var(val sym, val val);
void reg_fun(val sym, val fun);
void reg_mac(val sym, val fun);
val set_get_symacro(val sym, val form);
val apply(val fun, val arglist);
val apply_intrinsic(val fun, val args);
val eval_progn(val forms, val env, val ctx_form);
val eval(val form, val env, val ctx_form);
val eval_intrinsic(val form, val env);
val format_field(val string_or_list, val modifier, val filter, val eval_fun);
val subst_vars(val forms, val env, val filter);
val expand_quasi(val quasi_forms, val menv);
val load(val target);
val expand(val form, val menv);
val expand_forms(val forms, val menv);
val bindable(val obj);
val mapcarv(val fun, struct args *lists);
val mapcarl(val fun, val list_of_lists);
val lazy_mapcar(val fun, val list);
val generate(val while_pred, val gen_fun);
val prinl(val obj, val stream);
val pprinl(val obj, val stream);
val tprint(val obj, val out);

void eval_init(void);
void eval_compat_fixup(int compat_ver);
