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

extern int opt_debugger;
extern int debug_depth;
extern val debug_block_s;

val debug(val form, val bindings, val data, val line, val pos, val base);

#if CONFIG_DEBUG_SUPPORT

#define debug_enter                                     \
  {                                                     \
    int debug_depth_save = debug_depth++;               \
    uw_block_begin(debug_block_s, debug_result);        \
    uw_simple_catch_begin {

#define debug_leave                                     \
    }                                                   \
    uw_unwind {                                         \
      debug_depth = debug_depth_save;                   \
    }                                                   \
    uw_catch_end;                                       \
    uw_block_end;                                       \
    return debug_result;                                \
  }

#define debug_return(VAL)                               \
  uw_block_return(debug_block_s, VAL)

INLINE val debug_check(val form, val bindings, val data, val line,
                       val pos, val base)
{
  return (opt_debugger) ? debug(form, bindings, data, line, pos, base) : nil;
}

void debug_init(void);

#define debug_frame(FUNC, ARGS, UBP,    \
                    BINDINGS, DATA,     \
                    LINE, CHR)          \
  do {                                  \
    uw_frame_t db_env;                  \
    if (opt_debugger) {                 \
      uw_push_debug(&db_env, FUNC, ARGS,\
                    UBP, BINDINGS, DATA,\
                    LINE, CHR);         \
    }                                   \
    (void) 0

#define debug_end                       \
    if (opt_debugger) {                 \
      uw_pop_frame(&db_env);            \
    }                                   \
  } while (0)

#else

#define debug_enter {

#define debug_leave }

#define debug_return(VAL) return VAL

INLINE val debug_check(val form, val bindings, val data, val line, val chr)
{
  return nil;
}

#define debug_frame(FUNC, ARGS, UBP,    \
                    BINDINGS, DATA,     \
                    LINE, CHR)          \
  do {                                  \
    (void) 0

#define debug_end                       \
  } while (0)

INLINE void debug_init(void)
{
}

#endif
