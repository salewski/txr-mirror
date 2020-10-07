/* Copyright 2015-2020
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

extern val time_s, time_local_s, time_utc_s, time_string_s, time_parse_s;
extern val year_s, month_s, day_s, hour_s, min_s, sec_s;
extern val dst_s, gmtoff_s, zone_s;

val time_sec(void);
val time_sec_usec(void);
val time_string_local(val time, val format);
val time_string_utc(val time, val format);
val time_fields_local(val time);
val time_fields_utc(val time);
val time_struct_local(val time);
val time_struct_utc(val time);
val make_time(val year, val month, val day,
              val hour, val minute, val second,
              val isdst);
val make_time_utc(val year, val month, val day,
                  val hour, val minute, val second,
                  val isdst);
#if HAVE_STRPTIME
val time_parse(val format, val string);
val time_parse_local(val format, val string);
val time_parse_utc(val format, val string);
void time_init(void);
#endif
