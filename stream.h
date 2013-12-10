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
  val (*close)(val, val);
  val (*flush)(val);
  val (*seek)(val, cnum, enum strm_whence);
  val (*get_prop)(val, val ind);
  val (*set_prop)(val, val ind, val);
};

extern val std_input, std_output, std_debug, std_error, std_null;
extern val output_produced;

extern val dev_k, ino_k, mode_k, nlink_k, uid_k;
extern val gid_k, rdev_k, size_k, blksize_k, blocks_k;
extern val atime_k, mtime_k, ctime_k;
extern val from_start_k, from_current_k, from_end_k;
extern val real_time_k;

extern val s_ifmt, s_iflnk, s_ifreg, s_ifblk, s_ifdir;
extern val s_ifchr, s_ififo, s_isuid, s_isgid, s_isvtx, s_irwxu;
extern val s_irusr, s_iwusr, s_ixusr, s_irwxg, s_irgrp, s_iwgrp;
extern val s_ixgrp, s_irwxo, s_iroth, s_iwoth, s_ixoth;

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
val get_line(val);
val get_char(val);
val get_byte(val);
val vformat(val stream, val string, va_list);
val vformat_to_string(val string, va_list);
val format(val stream, val string, ...);
val formatv(val stream, val string, val args);
val put_string(val string, val stream);
val put_line(val string, val stream);
val put_char(val ch, val stream);
val put_byte(val byte, val stream);
val flush_stream(val stream);
val seek_stream(val stream, val offset, val whence);
val statf(val path);
val open_directory(val path);
val open_file(val path, val mode_str);
val open_tail(val path, val mode_str, val seek_end_p);
val open_command(val path, val mode_str);
val open_process(val path, val mode_str, val args);

void stream_init(void);
