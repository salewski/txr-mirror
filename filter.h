/* Copyright 2012
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

extern val filters;
extern val filter_k, lfilt_k, rfilt_k, to_html_k, from_html_k;
extern val upcase_k, downcase_k, fun_k;
extern val topercent_k, frompercent_k, tourl_k, fromurl_k;
extern val tonumber_k, tointeger_k, tofloat_k, hextoint_k;

val trie_lookup_begin(val trie);
val trie_value_at(val node);
val trie_lookup_feed_char(val node, val ch);
val get_filter(val sym);
val filter_string(val trie, val str);
val filter_equal(val lfilt, val rfilt, val left, val right);
val register_filter(val sym, val table);

val url_encode(val str, val space_plus);
val url_decode(val str, val space_plus);

void filter_init(void);

