/* Copyright 2012-2016
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

val make_bignum(void);
val bignum(cnum cn);
val bignum_from_long(long l);
val bignum_from_uintptr(uint_ptr_t u);
val num_from_buffer(mem_t *buf, int bytes);
int num_to_buffer(val num, mem_t *buf, int bytes);
int highest_bit(int_ptr_t n);
val normalize(val bignum);
val in_int_ptr_range(val bignum);
uint_ptr_t c_uint_ptr_num(val num);
val cum_norm_dist(val x);
val n_choose_k(val n, val k);
val n_perm_k(val n, val k);
val tofloat(val obj);
val toint(val obj, val base);
val tofloatz(val obj);
val tointz(val obj, val base);
val width(val num);
void arith_init(void);
void arith_free_all(void);
