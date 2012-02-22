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

extern val weak_keys_k, weak_vals_k, equal_based_k;

val make_hash(val weak_keys, val weak_vals, val equal_based);
val *gethash_l(val hash, val key, val *new_p);
val gethash(val hash, val key);
val gethash_n(val hash, val key, val notfound_val);
val gethash_f(val hash, val key, val *found);
val sethash(val hash, val key, val value);
val pushhash(val hash, val key, val value);
val remhash(val hash, val key);
val hash_count(val hash);
val get_hash_userdata(val hash);
val set_hash_userdata(val hash, val data);
val hashp(val obj);
val maphash(val func, val hash);
val hash_begin(val hash);
val hash_next(val *iter);
val hash_eql(val obj);
val hash_equal(val obj);
val hashv(val args);

void hash_process_weak(void);

void hash_init(void);
