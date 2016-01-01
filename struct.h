/* Copyright 2015-2016
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

extern val struct_type_s, meth_s;
val make_struct_type(val name, val super,
                     val static_slots, val slots,
                     val static_initfun, val initfun, val boactor,
                     val postinitfun);
val struct_type_p(val obj);
val super(val type);
val make_struct(val type, val plist, struct args *);
val make_lazy_struct(val type, val argfun);
val copy_struct(val strct);
val clear_struct(val strct, val value);
val replace_struct(val target, val source);
val reset_struct(val strct);
val find_struct_type(val sym);
val slot(val strct, val sym);
val slotset(val strct, val sym, val newval);
val static_slot(val stype, val sym);
val static_slot_set(val stype, val sym, val newval);
val static_slot_ensure(val stype, val sym, val newval, val no_error_p);
val slotp(val type, val sym);
val static_slot_p(val type, val sym);
val structp(val obj);
val struct_type(val strct);
val method(val strct, val slotsym);
val super_method(val strct, val slotsym);
val uslot(val slot);
val umethod(val slot);
val method_name(val fun);
void struct_init(void);
