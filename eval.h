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

extern val dwim_s, lambda_s, vector_lit_s, vec_list_s, list_s;
extern val hash_lit_s, hash_construct_s, struct_lit_s, qref_s;
extern val eval_error_s;
extern val last_form_evaled, last_form_expanded;

noreturn val eval_error(val form, val fmt, ...);
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
val interp_fun(val env, val fun, struct args *);
val boundp(val sym);
val fboundp(val sym);
val special_operator_p(val sym);
val macro_form_p(val form, val menv);
val func_get_name(val fun, val env);
void reg_varl(val sym, val val);
void reg_var(val sym, val val);
void reg_fun(val sym, val fun);
val apply(val fun, val arglist, val ctx_form);
val apply_intrinsic(val fun, val args);
val eval_progn(val forms, val env, val ctx_form);
val eval(val form, val env, val ctx_form);
val eval_intrinsic(val form, val env);
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
