/* Copyright 2009-2021
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

extern val tree_s, tree_fun_whitelist_s;

#define tree_fun_whitelist (deref(lookup_var_l(nil, tree_fun_whitelist_s)))

val tnode(val key, val left, val right);
val tnodep(val obj);
val left(val node);
val right(val node);
val key(val node);
val set_left(val node, val nleft);
val set_right(val node, val nright);
val set_key(val node, val nkey);
val copy_tnode(val node);
val tree(val keys, val key_fn, val less_fn, val equal_fn);
val copy_search_tree(val tree);
val treep(val obj);
val tree_insert_node(val tree, val node);
val tree_begin(val tree);
val tree_begin_at(val tree, val lowkey);
val tree_reset(val iter, val tree);
val tree_reset_at(val iter, val tree, val lowkey);
val tree_next(val iter);
val tree_clear(val tree);
void tree_init(void);
