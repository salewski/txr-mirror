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

extern val std_input, std_output, std_error;

val make_stdio_stream(FILE *, val descr, val input, val output);
val make_pipe_stream(FILE *, val descr, val input, val output);
val make_string_input_stream(val);
val make_string_byte_input_stream(val);
val make_string_output_stream(void);
val get_string_from_stream(val);
val make_strlist_output_stream(void);
val get_list_from_stream(val);
val make_dir_stream(DIR *);
val close_stream(val stream, val throw_on_error);
val get_line(val);
val get_char(val);
val get_byte(val);
val vformat(val stream, val string, va_list);
val vformat_to_string(val string, va_list);
val format(val stream, val string, ...);
val put_string(val stream, val string);
val put_line(val stream, val string);
val put_char(val stream, val ch);

void stream_init(void);
