/* Copyright 2013-2015
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


#if CONFIG_DEBUG_SUPPORT
extern int debug_depth;
#define EJ_DBG_MEMB volatile int dbg_depth;
#define EJ_DBG_SAVE(EJB) (EJB).dbg_depth = debug_depth,
#define EJ_DBG_REST(EJB) debug_depth = (EJB).dbg_depth,
#else
#define EJ_DBG_MEMB
#define EJ_DBG_SAVE(EJB)
#define EJ_DBG_REST(EJB)
#endif

#define EJ_OPT_MEMB EJ_DBG_MEMB
#define EJ_OPT_SAVE(EJB) EJ_DBG_SAVE(EJB)
#define EJ_OPT_REST(EJB) EJ_DBG_REST(EJB)

#if HAVE_POSIX_SIGS

INLINE void copy_sigset(volatile sigset_t *dest, const sigset_t *src)
{
#ifdef __cplusplus
  *strip_qual(sigset_t *, dest) = *src;
#else
  *dest = *src;
#endif
}

#define sig_save_enable                         \
  do {                                          \
    int sig_save = async_sig_enabled;           \
    if (!sig_save)                              \
      sig_check();                              \
    async_sig_enabled = 1;                      \
    {                                           \
      do ; while (0)

#define sig_restore_enable                      \
    }                                           \
    async_sig_enabled = sig_save;               \
  } while(0)

#define sig_save_disable                        \
  do {                                          \
    int sig_save = async_sig_enabled;           \
    async_sig_enabled = 0;                      \
    {                                           \
      do ; while (0)

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

typedef struct {
  jmp_buf jb;
  volatile sig_atomic_t se;
  volatile sigset_t blocked;
  volatile val de;
  volatile int gc;
  val **volatile gc_pt;
  EJ_OPT_MEMB
  volatile int rv;
} extended_jmp_buf;

#define extended_setjmp(EJB)                    \
  (setjmp((EJB).jb)                             \
   ? (async_sig_enabled = (EJB).se,             \
      dyn_env = (EJB).de,                       \
      gc_enabled = (EJB).gc,                    \
      gc_prot_top = (EJB).gc_pt,                \
      sig_mask(SIG_SETMASK,                     \
               strip_qual(sigset_t *,           \
                          &(EJB).blocked), 0),  \
      EJ_OPT_REST(EJB)                          \
      (EJB).rv)                                 \
   : ((EJB).se = async_sig_enabled,             \
      (EJB).de = dyn_env,                       \
      (EJB).gc = gc_enabled,                    \
      (EJB).gc_pt = gc_prot_top,                \
      copy_sigset(&(EJB).blocked,               \
                  &sig_blocked_cache),          \
      EJ_OPT_SAVE(EJB)                          \
      0))

#define extended_longjmp(EJB, ARG)              \
  ((EJB).rv = (ARG), longjmp((EJB).jb, 1))

extern sigset_t sig_blocked_cache;
extern volatile sig_atomic_t async_sig_enabled;

#else

#define sig_save_enable do { do ; while (0)
#define sig_save_disable do { do ; while (0)

#define sig_restore_enable } while (0)
#define sig_restore_disable } while (0)

#define sig_check_fast() ((void) 0)

typedef struct {
  jmp_buf jb;
  volatile val de;
  volatile int gc;
  val **volatile gc_pt;
  EJ_OPT_MEMB
  volatile int rv;
} extended_jmp_buf;

#define extended_setjmp(EJB)                    \
  (setjmp((EJB).jb)                             \
   ? (dyn_env = (EJB).de,                       \
      gc_enabled = ((EJB).gc),                  \
      gc_prot_top = (EJB).gc_pt,                \
      EJ_OPT_REST(EJB)                          \
      (EJB).rv)                                 \
   : ((EJB).de = dyn_env,                       \
      (EJB).gc = gc_enabled,                    \
      (EJB).gc_pt = gc_prot_top,                \
      EJ_OPT_SAVE(EJB)                          \
      0))

#define extended_longjmp(EJB, ARG)              \
  ((EJB).rv = (ARG), longjmp((EJB).jb, 1))

extern int async_sig_enabled;

#endif

extern val dyn_env; /* eval.c */

void sig_init(void);
val set_sig_handler(val signo, val lambda);
val get_sig_handler(val signo);
#if HAVE_POSIX_SIGS
int sig_mask(int how, const sigset_t *set, sigset_t *oldset);
#endif

#if HAVE_ITIMER
val getitimer_wrap(val which);
val setitimer_wrap(val which, val interval, val currval);
#endif
