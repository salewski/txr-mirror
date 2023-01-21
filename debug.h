/* Copyright 2012-2023
 * Kaz Kylheku <kaz@kylheku.com>
 * Vancouver, Canada
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */


#define DBG_ENABLE      1
#define DBG_STEP        2
#define DBG_BACKTRACE   4
#define DBG_ALL         7

#if CONFIG_DEBUG_SUPPORT

extern unsigned debug_state;

void debug_init(void);

INLINE unsigned debug_clear(unsigned mask)
{
  int ret = debug_state;
  debug_state &= ~mask;
  return ret;
}

INLINE unsigned debug_set(unsigned mask)
{
  int ret = debug_state;
  debug_state |= mask;
  return ret;
}

INLINE void debug_restore(unsigned state)
{
  debug_state = state;
}

void debug_dump_backtrace(val stream, val prefix);

#define dbg_backtrace (debug_state & DBG_BACKTRACE)

#define dbg_fcall_begin(fun, args)      \
  {                                     \
    uw_frame_t uw_fc;                   \
    args_decl(args_cp, args->argc);     \
    args_copy(args_cp, args);           \
    uw_push_fcall(&uw_fc, fun, args_cp)

#define dbg_fcall_end                   \
    uw_pop_frame(&uw_fc);               \
  }

#else

#define debug_init() ((void) 0)
#define debug_clear(mask) 0
#define debug_set(mask) 0
#define debug_restore(state) ((void) 0)
#define debug_dump_backtrace(stream, prefix) ((void) 0)

#define dbg_backtrace 0

#define dbg_fcall_begin(fun, args)      \
  {

#define dbg_fcall_end                   \
  }

#endif
