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

extern int opt_debugger;

val debug(val form, val bindings, val data, val line, val chr);

INLINE val debug_check(val form, val bindings, val data, val line, val chr)
{
  return (opt_debugger) ? debug(form, bindings, data, line, chr) : nil;
}

void debug_init(void);

#define debug_begin(FUNC, ARGS, UBP,    \
                    BINDINGS, DATA,     \
                    LINE, CHR)          \
  do {                                  \
    uw_frame_t db_env;                  \
    if (opt_debugger)                   \
      uw_push_debug(&db_env, FUNC, ARGS,\
                    UBP, BINDINGS, DATA,\
                    LINE, CHR);         \
    (void) 0

#define debug_end                       \
    if (opt_debugger)                   \
      uw_pop_frame(&db_env);            \
  } while (0)
