/* Copyright 2009-2016
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

extern val space_k, digit_k, word_char_k;
extern val cspace_k, cdigit_k, cword_char_k;

extern wchar_t spaces[];

val regex_compile(val regex, val error_stream);
val regexp(val);
val regex_source(val regex);
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
val regex_range_search(val regex, val arg1, val arg2, val arg3);
int wide_display_char_p(wchar_t ch);
void regex_init(void);
void regex_free_all(void);
