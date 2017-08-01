/* Copyright 2009
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

extern obj_t *std_input, *std_output, *std_error;

obj_t *make_stdio_stream(FILE *, obj_t *input, obj_t *output);
obj_t *make_pipe_stream(FILE *, obj_t *input, obj_t *output);
obj_t *make_string_input_stream(obj_t *);
obj_t *make_string_output_stream(void);
obj_t *get_string_from_stream(obj_t *);
obj_t *make_dir_stream(DIR *);
obj_t *close_stream(obj_t *);
obj_t *get_line(obj_t *);
obj_t *get_char(obj_t *);
obj_t *vformat(obj_t *stream, const char *string, va_list); /* nao-terminated */
obj_t *vcformat(obj_t *stream, const char *string, va_list); /* printf-style */
obj_t *format(obj_t *stream, const char *string, ...);
obj_t *cformat(obj_t *stream, const char *string, ...);
obj_t *put_string(obj_t *stream, obj_t *string);
obj_t *put_line(obj_t *stream, obj_t *string);
obj_t *put_cstring(obj_t *stream, const char *);
obj_t *put_char(obj_t *stream, obj_t *ch);
obj_t *put_cchar(obj_t *stream, int ch);

void stream_init(void);
