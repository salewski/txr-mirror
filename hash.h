/* Copyright 2011
 * Kaz Kylheku <kkylheku@gmail.com>
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

val hash_obj(val);
val make_hash(val weak_keys, val weak_vals);
val *gethash_l(val hash, val key, val *new_p);
val gethash(val hash, val key);
val gethash_f(val hash, val key, val *found);
val sethash(val hash, val key, val value);
val remhash(val hash, val key);
val hash_count(val hash);
val get_hash_userdata(val hash);
val set_hash_userdata(val hash, val data);
val hashp(val obj);
val hash_begin(val hash);
val hash_next(val *iter);
void hash_process_weak(void);

void hash_init(void);
