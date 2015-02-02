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


#if HAVE_POSIX_SIGS

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
    async_sig_enabled = 1;                      \
    {                                           \
      do ; while (0)

#define sig_restore_disable                     \
    }                                           \
    async_sig_enabled = sig_save;               \
    if (sig_save)                               \
      sig_check();                              \
  } while(0)

typedef struct {
  jmp_buf jb;
  sig_atomic_t se;
  sigset_t blocked;
  val de;
  int gc;
  val **gc_pt;
  int rv;
} extended_jmp_buf;

#define extended_setjmp(EJB)                    \
  (setjmp((EJB).jb)                             \
   ? (async_sig_enabled = (EJB).se,             \
      dyn_env = (EJB).de,                       \
      gc_enabled = (EJB).gc,                    \
      gc_prot_top = (EJB).gc_pt,                \
      sig_mask(SIG_SETMASK, &(EJB).blocked, 0), \
      (EJB).rv)                                 \
   : ((EJB).se = async_sig_enabled,             \
      (EJB).de = dyn_env,                       \
      (EJB).gc = gc_enabled,                    \
      (EJB).gc_pt = gc_prot_top,                \
      (EJB).blocked = sig_blocked_cache,        \
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

typedef struct {
  jmp_buf jb;
  val de;
  int gc;
  val **gc_pt;
  int rv;
} extended_jmp_buf;

#define extended_setjmp(EJB)                    \
  (setjmp((EJB).jb)                             \
   ? (dyn_env = (EJB).de,                       \
      gc_enabled = ((EJB).gc),                  \
      gc_prot_top = (EJB).gc_pt,                \
      (EJB).rv)                                 \
   : ((EJB).de = dyn_env,                       \
      (EJB).gc = gc_enabled,                    \
      (EJB).gc_pt = gc_prot_top,                \
      0))

#define extended_longjmp(EJB, ARG)              \
  ((EJB).rv = (ARG), longjmp((EJB).jb, 1))

extern int async_sig_enabled;

#endif

extern val dyn_env; /* eval.c */

void sig_init(void);
val set_sig_handler(val signo, val lambda);
val get_sig_handler(val signo);
val sig_check(void);
#if HAVE_POSIX_SIGS
int sig_mask(int how, const sigset_t *set, sigset_t *oldset);
#endif

#if HAVE_ITIMER
val getitimer_wrap(val which);
val setitimer_wrap(val which, val interval, val currval);
#endif
