/* Copyright 2009-2015
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

extern val filters;
extern val filter_k, lfilt_k, rfilt_k, to_html_k, from_html_k;
extern val upcase_k, downcase_k, fun_k;
extern val topercent_k, frompercent_k, tourl_k, fromurl_k;
extern val tonumber_k, tointeger_k, tofloat_k, hextoint_k;

val trie_lookup_begin(val trie);
val trie_value_at(val node);
val trie_lookup_feed_char(val node, val ch);
val get_filter(val sym);
val filter_string_tree(val trie, val str_tree);
val filter_equal(val lfilt, val rfilt, val left, val right);
val register_filter(val sym, val table);

val url_encode(val str, val space_plus);
val url_decode(val str, val space_plus);

void filter_init(void);

