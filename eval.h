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

val make_env(val fbindings, val vbindings, val up_env);
val env_fbind(val env, val sym, val fun);
val env_vbind(val env, val sym, val obj);
val lookup_var(val env, val sym);
val *lookup_var_l(val env, val sym);
val lookup_fun(val env, val sym);
val interp_fun(val env, val fun, val args);
val apply(val fun, val arglist, val ctx_form);
val eval_progn(val forms, val env, val ctx_form);
val eval(val form, val env, val ctx_form);
val expand(val form);
val bindable(val obj);

void eval_init(void);
