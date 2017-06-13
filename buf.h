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

val make_buf(val len, val init_val, val alloc_size);
val bufp(val object);
val make_borrowed_buf(val len, mem_t *data);
val make_duplicate_buf(val len, mem_t *data);
val buf_trim(val buf);
val buf_set_length(val obj, val len, val init_val);
val length_buf(val buf);
mem_t *buf_get(val buf, val self);
void buf_fill(val buf, mem_t *src, val self);

#if HAVE_I8
val buf_put_i8(val buf, val pos, val num);
val buf_put_u8(val buf, val pos, val num);
#endif

#if HAVE_I16
val buf_put_i16(val buf, val pos, val num);
val buf_put_u16(val buf, val pos, val num);
#endif

#if HAVE_I32
val buf_put_i32(val buf, val pos, val num);
val buf_put_u32(val buf, val pos, val num);
#endif

#if HAVE_I64
val buf_put_i64(val buf, val pos, val num);
val buf_put_u64(val buf, val pos, val num);
#endif

val buf_put_char(val buf, val pos, val num);
val buf_put_uchar(val buf, val pos, val num);
val buf_put_short(val buf, val pos, val num);
val buf_put_ushort(val buf, val pos, val num);
val buf_put_int(val buf, val pos, val num);
val buf_put_uint(val buf, val pos, val num);
val buf_put_long(val buf, val pos, val num);
val buf_put_ulong(val buf, val pos, val num);
val buf_put_float(val buf, val pos, val num);
val buf_put_double(val buf, val pos, val num);
val buf_put_cptr(val buf, val pos, val cptr);

#if HAVE_I8
val buf_get_i8(val buf, val pos);
val buf_get_u8(val buf, val pos);
#endif

#if HAVE_I16
val buf_get_i16(val buf, val pos);
val buf_get_u16(val buf, val pos);
#endif

#if HAVE_I32
val buf_get_i32(val buf, val pos);
val buf_get_u32(val buf, val pos);
#endif

#if HAVE_I64
val buf_get_i64(val buf, val pos);
val buf_get_u64(val buf, val pos);
#endif

val buf_get_char(val buf, val pos);
val buf_get_uchar(val buf, val pos);
val buf_get_short(val buf, val pos);
val buf_get_ushort(val buf, val pos);
val buf_get_int(val buf, val pos);
val buf_get_uint(val buf, val pos);
val buf_get_long(val buf, val pos);
val buf_get_ulong(val buf, val pos);
val buf_get_float(val buf, val pos);
val buf_get_double(val buf, val pos);
val buf_get_cptr(val buf, val pos);

val buf_print(val buf, val stream);
val buf_pprint(val buf, val stream);

void buf_init(void);
