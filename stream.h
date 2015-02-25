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

enum strm_whence {
  strm_start = SEEK_SET,
  strm_cur = SEEK_CUR,
  strm_end = SEEK_SET
};

struct strm_ops {
  struct cobj_ops cobj_ops;
  val (*put_string)(val, val);
  val (*put_char)(val, val);
  val (*put_byte)(val, int);
  val (*get_line)(val);
  val (*get_char)(val);
  val (*get_byte)(val);
  val (*unget_char)(val, val);
  val (*unget_byte)(val, int);
  val (*close)(val, val);
  val (*flush)(val);
  val (*seek)(val, cnum, enum strm_whence);
  val (*get_prop)(val, val ind);
  val (*set_prop)(val, val ind, val);
  val (*get_error)(val);
  val (*get_error_str)(val);
  void (*clear_error)(val);
};

#define strm_ops_init(cobj_init_macro, put_string, put_char, put_byte,      \
                      get_line, get_char, get_byte, unget_char, unget_byte, \
                      close, flush, seek, get_prop, set_prop,               \
                      get_error, get_error_str, clear_error)                \
{                                                                           \
    cobj_init_macro, put_string, put_char, put_byte, get_line,              \
    get_char, get_byte, unget_char, unget_byte,                             \
    close, flush, seek, get_prop, set_prop,                                 \
    get_error, get_error_str, clear_error                                   \
}

#define std_input (deref(lookup_var_l(nil, stdin_s)))
#define std_output (deref(lookup_var_l(nil, stdout_s)))
#define std_debug (deref(lookup_var_l(nil, stddebug_s)))
#define std_error (deref(lookup_var_l(nil, stderr_s)))
#define std_null (deref(lookup_var_l(nil, stdnull_s)))
loc lookup_var_l(val env, val sym);

extern val dev_k, ino_k, mode_k, nlink_k, uid_k;
extern val gid_k, rdev_k, size_k, blksize_k, blocks_k;
extern val atime_k, mtime_k, ctime_k;
extern val from_start_k, from_current_k, from_end_k;
extern val real_time_k, name_k;
extern val format_s;

extern val stdin_s, stdout_s, stddebug_s, stderr_s, stdnull_s;

val make_null_stream(void);
val make_stdio_stream(FILE *, val descr);
val make_tail_stream(FILE *, val descr);
val make_pipe_stream(FILE *, val descr);
val make_string_input_stream(val);
val make_string_byte_input_stream(val);
val make_string_output_stream(void);
val get_string_from_stream(val);
val make_strlist_output_stream(void);
val get_list_from_stream(val);
val make_dir_stream(DIR *);
val streamp(val obj);
val real_time_stream_p(val obj);
val stream_set_prop(val stream, val ind, val prop);
val stream_get_prop(val stream, val ind);
val close_stream(val stream, val throw_on_error);
val get_error(val stream);
val get_error_str(val stream);
val clear_error(val stream);
val get_line(val);
val get_char(val);
val get_byte(val);
val unget_char(val ch, val stream);
val unget_byte(val byte, val stream);
val vformat(val stream, val string, va_list);
val vformat_to_string(val string, va_list);
val format(val stream, val string, ...);
val formatv(val stream, val string, val args);
val put_string(val string, val stream);
val put_line(val string, val stream);
val put_char(val ch, val stream);
val put_byte(val byte, val stream);
val put_strings(val strings, val stream);
val put_lines(val lines, val stream);
val flush_stream(val stream);
val seek_stream(val stream, val offset, val whence);
val get_string(val stream, val nchars, val close_after_p);
val statf(val path);
val open_directory(val path);
val open_file(val path, val mode_str);
val open_tail(val path, val mode_str, val seek_end_p);
val open_command(val path, val mode_str);
val open_process(val path, val mode_str, val args);
val make_catenated_stream(val stream_list);
val remove_path(val path);
val rename_path(val from, val to);
val abs_path_p(val path);

void stream_init(void);
