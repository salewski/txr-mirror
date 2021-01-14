/* Copyright 2013-2021
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

#if HAVE_POSIX_SIGS

typedef struct {
  unsigned int set;
} small_sigset_t;

extern small_sigset_t sig_blocked_cache;

#define sig_save_enable                         \
  do {                                          \
    int sig_save = async_sig_enabled;           \
    if (!sig_save)                              \
      sig_check();                              \
    async_sig_enabled = 1;                      \
    {                                           \
      do { } while (0)

#define sig_restore_enable                      \
    }                                           \
    async_sig_enabled = sig_save;               \
  } while(0)

#define sig_save_disable                        \
  do {                                          \
    int sig_save = async_sig_enabled;           \
    async_sig_enabled = 0;                      \
    {                                           \
      do { } while (0)

#define sig_restore_disable                     \
    }                                           \
    async_sig_enabled = sig_save;               \
    if (sig_save)                               \
      sig_check();                              \
  } while(0)

val sig_check(void);

INLINE val sig_check_fast(void)
{
  extern volatile unsigned long sig_deferred;
  return if2(sig_deferred, sig_check());
}

extern volatile sig_atomic_t async_sig_enabled;

#else

#define sig_save_enable do { do ; while (0)
#define sig_save_disable do { do ; while (0)

#define sig_restore_enable } while (0)
#define sig_restore_disable } while (0)

#define sig_check_fast() ((void) 0)

extern int async_sig_enabled;

#endif

void sig_init(void);
val set_sig_handler(val signo, val lambda);
val get_sig_handler(val signo);
#if HAVE_POSIX_SIGS
int sig_mask(int how, const small_sigset_t *set, small_sigset_t *oldset);
#endif

#if HAVE_ITIMER
val getitimer_wrap(val which);
val setitimer_wrap(val which, val interval, val currval);
#endif
