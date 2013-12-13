/* Copyright 2013-2014
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


#if HAVE_POSIX_SIGS

#define sig_save_enable                         \
  do {                                          \
    int sig_save = async_sig_enabled;           \
    async_sig_enabled = 1;                      \
    if (!sig_save)                              \
      sig_check();                              \
    {                                           \
      typedef void v_o_i_d

#define sig_restore_enable                      \
      do { } while (0);                         \
    }                                           \
    async_sig_enabled = sig_save;               \
  } while(0)

#define sig_save_disable                        \
  do {                                          \
    int sig_save = async_sig_enabled;           \
    async_sig_enabled = 1;                      \
    {                                           \
      typedef void v_o_i_d

#define sig_restore_disable                     \
      do { } while (0);                         \
    }                                           \
    async_sig_enabled = sig_save;               \
    if (sig_save)                               \
      sig_check();                              \
  } while(0)
    
typedef struct {
  jmp_buf jb;
  sig_atomic_t se;
  int rv;
} extended_jmp_buf;

#define extended_setjmp(EJB)                    \
  (sigsetjmp((EJB).jb, 1)                       \
   ? (async_sig_enabled = (EJB).se, (EJB).rv)   \
   : ((EJB).se = async_sig_enabled, 0))

#define extended_longjmp(EJB, ARG)              \
  ((EJB).rv = (ARG), longjmp((EJB).jb, 1))

#else

#define sig_save_enable do {
#define sig_save_disable do {

#define sig_restore_enable do { } while (0); } while (0)
#define sig_restore_disable do { } while (0); } while (0)

tyedef jmp_buf extended_jmp_buf;
#define extended_setjmp(EJB) setjmp(EJB)
#define extended_longjmp(EJB, ARG) longjmp(EJB, ARG)

#endif

extern volatile sig_atomic_t async_sig_enabled;

extern val sig_hup, sig_int, sig_quit, sig_ill, sig_trap, sig_abrt, sig_bus;
extern val sig_fpe, sig_kill, sig_usr1, sig_segv, sig_usr2, sig_pipe, sig_alrm;
extern val sig_term, sig_chld, sig_cont, sig_stop, sig_tstp, sig_ttin;
extern val sig_ttou, sig_urg, sig_xcpu, sig_xfsz, sigtalrm, sig_prof;
extern val sig_poll, sig_sys, sig_winch, sig_iot, sig_stkflt;
extern val sig_io, sig_lost, sig_pwr;

void sig_init(void);
val set_sig_handler(val signo, val lambda);
val get_sig_handler(val signo);
val sig_check(void);
