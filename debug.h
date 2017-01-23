/* Copyright 2012-2017
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

extern int opt_debugger;
extern int debug_depth;

val debug(val form, val bindings, val data, val line, val pos, val base);

#if CONFIG_DEBUG_SUPPORT

typedef struct {
  int next_depth;
  int step_mode;
} debug_state_t;

#define debug_enter                             \
  {                                             \
    int debug_depth_save = debug_depth++;       \
    val debug_result = nil;                     \
    (void) 0

#define debug_leave                             \
  debug_return_out:                             \
    debug_depth = debug_depth_save;             \
    return debug_result;                        \
  }

#define debug_return(VAL)               \
  do {                                  \
    debug_result = VAL;                 \
    goto debug_return_out;              \
  } while (0)

INLINE val debug_check(val ctx, val bindings, val data, val line,
                       val pos, val base)
{
  return (opt_debugger) ? debug(ctx, bindings, data, line, pos, base) : nil;
}

debug_state_t debug_set_state(int depth, int step);
void debug_restore_state(debug_state_t);
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

typedef int debug_state_t;

#define debug_enter {

#define debug_leave }

#define debug_return(VAL) return VAL

INLINE val debug_check(val form, val bindings, val data, val line,
                       val pos, val base)
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

#define debug_set_state(D, S) (0)
#define debug_restore_state(S) ((void) 0)

INLINE void debug_init(void)
{
}

#endif
