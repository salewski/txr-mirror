/* Copyright 2013-2016
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

#if __i386__

struct jmp {
   unsigned eip;
   unsigned esp;
   unsigned ebp;
   unsigned ebx;
   unsigned esi;
   unsigned edi;
};

#elif __x86_64__

struct jmp {
   unsigned long rip;
   unsigned long rsp;
   unsigned long rbp;
   unsigned long rbx;
   unsigned long r12;
   unsigned long r13;
   unsigned long r14;
   unsigned long r15;
#if __CYGWIN__
   unsigned long rsi;
   unsigned long rdi;
#endif
};

#elif __arm__ && !__thumb__

struct jmp {
  unsigned long r4;
  unsigned long r5;
  unsigned long r6;
  unsigned long r7;
  unsigned long r8;
  unsigned long r9;
  unsigned long r10;
  unsigned long fp;
  unsigned long sp;
  unsigned long lr;
};

#elif __arm__ && __thumb__

struct jmp {
  unsigned long lr;
  unsigned long r4;
  unsigned long r5;
  unsigned long r6;
  unsigned long r7;
  unsigned long r8;
  unsigned long r9;
  unsigned long r10;
  unsigned long fp;
  unsigned long sp;
};

#elif __PPC64__

struct jmp {
  unsigned long r1;
  unsigned long r2;
  unsigned long r11;
  unsigned long r12;
  unsigned long r13;
  unsigned long r14;
  unsigned long r15;
  unsigned long r16;
  unsigned long r17;
  unsigned long r18;
  unsigned long r19;
  unsigned long r20;
  unsigned long r21;
  unsigned long r22;
  unsigned long r23;
  unsigned long r24;
  unsigned long r25;
  unsigned long r26;
  unsigned long r27;
  unsigned long r28;
  unsigned long r29;
  unsigned long r30;
  unsigned long r31;
};

#else
#error port me!
#endif

#ifdef __cplusplus
extern "C" {
#endif

int jmp_save(struct jmp *);
void jmp_restore(struct jmp *, int);

#ifdef __cplusplus
}
#endif

#if HAVE_POSIX_SIGS

typedef struct {
  unsigned int set;
} small_sigset_t;

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
  struct jmp jb;
  volatile sig_atomic_t se;
  volatile small_sigset_t blocked;
  volatile val de;
  volatile int gc;
  val **volatile gc_pt;
  EJ_OPT_MEMB
  volatile int rv;
} extended_jmp_buf;

#define extended_setjmp(EJB)                    \
  (jmp_save(&(EJB).jb)                          \
   ? (async_sig_enabled = (EJB).se,             \
      dyn_env = (EJB).de,                       \
      gc_enabled = (EJB).gc,                    \
      gc_prot_top = (EJB).gc_pt,                \
      sig_mask(SIG_SETMASK,                     \
               strip_qual(small_sigset_t *,     \
                          &(EJB).blocked), 0),  \
      EJ_OPT_REST(EJB)                          \
      (EJB).rv)                                 \
   : ((EJB).se = async_sig_enabled,             \
      (EJB).de = dyn_env,                       \
      (EJB).gc = gc_enabled,                    \
      (EJB).gc_pt = gc_prot_top,                \
      (EJB).blocked.set = sig_blocked_cache.set,\
      EJ_OPT_SAVE(EJB)                          \
      0))

#define extended_longjmp(EJB, ARG)              \
  ((EJB).rv = (ARG), jmp_restore(&(EJB).jb, 1))

extern small_sigset_t sig_blocked_cache;
extern volatile sig_atomic_t async_sig_enabled;

#else

#define sig_save_enable do { do ; while (0)
#define sig_save_disable do { do ; while (0)

#define sig_restore_enable } while (0)
#define sig_restore_disable } while (0)

#define sig_check_fast() ((void) 0)

typedef struct {
  struct jmp jb;
  volatile val de;
  volatile int gc;
  val **volatile gc_pt;
  EJ_OPT_MEMB
  volatile int rv;
} extended_jmp_buf;

#define extended_setjmp(EJB)                    \
  (jmp_save(&(EJB).jb)                          \
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
  ((EJB).rv = (ARG), jmp_restore(&(EJB).jb, 1))

extern int async_sig_enabled;

#endif

extern val dyn_env; /* eval.c */

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
