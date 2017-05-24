/* Copyright 2017
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

extern val uint8_s, int8_s;
extern val uint16_s, int16_s;
extern val uint32_s, int32_s;
extern val uint64_s, int64_s;

extern val char_s, uchar_s, bchar_s, wchar_s;
extern val short_s, ushort_s;
extern val int_s, uint_s;
extern val long_s, ulong_s;
extern val void_s;
extern val double_s;

extern val val_s;

extern val array_s, zarray_s, carray_s;

extern val struct_s;

extern val str_d_s, wstr_s, wstr_d_s, bstr_s, bstr_d_s;

extern val buf_d_s;

extern val ptr_in_s, ptr_out_s, ptr_in_d_s, ptr_out_d_s, ptr_out_s_s, ptr_s;

extern val closure_s;

extern val ffi_type_s, ffi_call_desc_s, ffi_closure_s;

val ffi_type_compile(val syntax);
val ffi_make_call_desc(val ntotal, val nfixed, val rettype, val argtypes);
val ffi_make_closure(val fun, val call_desc, val safe_p_in, val abort_ret_in);
mem_t *ffi_closure_get_fptr(val closure);
val ffi_call_wrap(val fptr, val ffi_call_desc, struct args *args);
val ffi_typedef(val name, val type);
val ffi_size(val type);
val ffi_put_into(val dstbuf, val obj, val type);
val ffi_put(val obj, val type);
val ffi_in(val srcbuf, val obj, val type, val copy_p);
val ffi_get(val srcbuf, val type);
val ffi_out(val dstbuf, val obj, val type, val copy_p);
val make_carray(val type, mem_t *data, cnum nelem, val ref);
val carray_set_length(val carray, val nelem);
val carray_dup(val carray);
val carray_own(val carray);
val carray_free(val carray);
val carray_type(val carray);
val length_carray(val carray);
mem_t *carray_get(val carray, val type, val self);
val carray_vec(val vec, val type, val null_term_p);
val carray_blank(val nelem, val type);
val carray_buf(val buf, val type);
val carray_buf_sync(val carray);
val carray_cptr(val cptr, val type, val len);
val vec_carray(val carray, val null_term_p);
val list_carray(val carray, val null_term_p);
val carray_ref(val carray, val idx);
val carray_refset(val carray, val idx, val newval);
void ffi_init(void);
