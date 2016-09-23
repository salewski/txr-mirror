/* Copyright 2009-2016
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

extern val space_k, digit_k, word_char_k;
extern val cspace_k, cdigit_k, cword_char_k;

extern wchar_t spaces[];

val regex_compile(val regex, val error_stream);
val regexp(val);
val search_regex(val haystack, val needle_regex, val start_num, val from_end);
val range_regex(val haystack, val needle_regex, val start_num, val from_end);
val match_regex(val str, val regex, val pos);
val match_regex_len(val str, val regex, val pos);
val match_regex_right(val str, val regex, val end);
val search_regst(val haystack, val needle_regex, val start_num, val from_end);
val match_regst(val str, val regex, val pos);
val match_regst_right(val str, val regex, val end);
val regsub(val regex, val repl, val str);
val read_until_match(val regex, val stream, val keep_match);
val regex_match_full(val regex, val arg1, val arg2);
val regex_match_full_fun(val regex, val pos);
val regex_match_left_fun(val regex, val pos);
val regex_match_right_fun(val regex, val end);
val regex_match_left(val regex, val arg1, val arg2);
val regex_match_right(val regex, val arg1, val arg2);
val regex_range_full(val regex, val arg1, val arg2);
val regex_range_left(val regex, val arg1, val arg2);
val regex_range_right(val regex, val arg1, val arg2);
int wide_display_char_p(wchar_t ch);
void regex_init(void);
void regex_free_all(void);
