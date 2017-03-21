/* Copyright 2015-2017
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

extern val struct_type_s, meth_s, make_struct_lit_s;
val make_struct_type(val name, val super,
                     val static_slots, val slots,
                     val static_initfun, val initfun, val boactor,
                     val postinitfun);
val struct_type_p(val obj);
val super(val type);
val make_struct(val type, val plist, struct args *);
val struct_from_plist(val type, struct args *plist);
val struct_from_args(val type, struct args *boa);
val make_lazy_struct(val type, val argfun);
val make_struct_lit(val type, val plist);
val copy_struct(val strct);
val clear_struct(val strct, val value);
val replace_struct(val target, val source);
val reset_struct(val strct);
val find_struct_type(val sym);
val slot(val strct, val sym);
val maybe_slot(val strct, val sym);
val slotset(val strct, val sym, val newval);
val static_slot(val stype, val sym);
val static_slot_set(val stype, val sym, val newval);
val static_slot_ensure(val stype, val sym, val newval, val no_error_p);
val static_slot_home(val stype, val sym);
val test_dirty(val strct);
val test_clear_dirty(val strct);
val clear_dirty(val strct);
val slotp(val type, val sym);
val static_slot_p(val type, val sym);
val slots(val stype);
val structp(val obj);
val struct_type(val strct);
val method(val strct, val slotsym);
val method_args(val strct, val slotsym, struct args *);
val super_method(val strct, val slotsym);
val uslot(val slot);
val umethod(val slot, struct args *);
val method_name(val fun);
val get_slot_syms(val package, val is_current, val method_only);
void struct_init(void);
