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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <setjmp.h>
#include <errno.h>
#include <wchar.h>
#include <dirent.h>
#include <signal.h>
#include "config.h"
#include "lib.h"
#include "gc.h"
#include "signal.h"
#include "unwind.h"
#include "eval.h"

#define MAX_SIG 32

volatile sig_atomic_t async_sig_enabled = 0;
sigset_t sig_blocked_cache;

static val sig_lambda[MAX_SIG];
volatile unsigned long sig_deferred;

val sig_hup, sig_int, sig_quit, sig_ill, sig_trap, sig_abrt, sig_bus;
val sig_fpe, sig_kill, sig_usr1, sig_segv, sig_usr2, sig_pipe, sig_alrm;
val sig_term, sig_chld, sig_cont, sig_stop, sig_tstp, sig_ttin;
val sig_ttou, sig_urg, sig_xcpu, sig_xfsz, sigtalrm, sig_prof;
val sig_poll, sig_sys, sig_winch, sig_iot, sig_stkflt;
val sig_io, sig_lost, sig_pwr;

static int is_cpu_exception(int sig)
{
  switch (sig) {
  case SIGFPE: case SIGILL:
  case SIGSEGV: case SIGBUS:
  case SIGTRAP:
    return 1;
  default:
    return 0;
  }
}

static void sig_handler(int sig)
{
  val lambda = sig_lambda[sig];
  int gc = 0;
  int as = 0;
  int exc = is_cpu_exception(sig);

  if (exc) {
    gc = gc_state(0);
    as = async_sig_enabled;
    async_sig_enabled = 1;
  }

  if (lambda) {
    if (async_sig_enabled) {
      async_sig_enabled = 0;
      if (funcall2(lambda, num_fast(sig), t))
        sig_deferred |= (1UL << sig);
      async_sig_enabled = 1;
    } else {
      sig_deferred |= (1UL << sig);
    }
  }

  if (exc) {
    async_sig_enabled = as;
    gc_state(gc);
  }
}

void sig_init(void)
{
  int i;

  for (i = 0; i < MAX_SIG; i++) {
    /* t means SIG_DFL, which is what signals
     * are before we manipulate them. */
    sig_lambda[i] = t; 
    prot1(&sig_lambda[i]);
  }

  reg_var(intern(lit("sig-hup"), user_package), num_fast(SIGHUP));
  reg_var(intern(lit("sig-int"), user_package), num_fast(SIGINT));
  reg_var(intern(lit("sig-quit"), user_package), num_fast(SIGQUIT));
  reg_var(intern(lit("sig-ill"), user_package), num_fast(SIGILL));
  reg_var(intern(lit("sig-trap"), user_package), num_fast(SIGTRAP));
  reg_var(intern(lit("sig-abrt"), user_package), num_fast(SIGABRT));
  reg_var(intern(lit("sig-bus"), user_package), num_fast(SIGBUS));
  reg_var(intern(lit("sig-fpe"), user_package), num_fast(SIGFPE));
  reg_var(intern(lit("sig-kill"), user_package), num_fast(SIGKILL));
  reg_var(intern(lit("sig-usr1"), user_package), num_fast(SIGUSR1));
  reg_var(intern(lit("sig-segv"), user_package), num_fast(SIGSEGV));
  reg_var(intern(lit("sig-usr2"), user_package), num_fast(SIGUSR2));
  reg_var(intern(lit("sig-pipe"), user_package), num_fast(SIGPIPE));
  reg_var(intern(lit("sig-alrm"), user_package), num_fast(SIGALRM));
  reg_var(intern(lit("sig-term"), user_package), num_fast(SIGTERM));
  reg_var(intern(lit("sig-chld"), user_package), num_fast(SIGCHLD));
  reg_var(intern(lit("sig-cont"), user_package), num_fast(SIGCONT));
  reg_var(intern(lit("sig-stop"), user_package), num_fast(SIGSTOP));
  reg_var(intern(lit("sig-tstp"), user_package), num_fast(SIGTSTP));
  reg_var(intern(lit("sig-ttin"), user_package), num_fast(SIGTTIN));
  reg_var(intern(lit("sig-ttou"), user_package), num_fast(SIGTTOU));
  reg_var(intern(lit("sig-urg"), user_package), num_fast(SIGURG));
  reg_var(intern(lit("sig-xcpu"), user_package), num_fast(SIGXCPU));
  reg_var(intern(lit("sig-xfsz"), user_package), num_fast(SIGXFSZ));
  reg_var(intern(lit("sig-vtalrm"), user_package), num_fast(SIGVTALRM));
  reg_var(intern(lit("sig-prof"), user_package), num_fast(SIGPROF));
#ifdef SIGPOLL
  reg_var(intern(lit("sig-poll"), user_package), num_fast(SIGPOLL));
#endif
  reg_var(intern(lit("sig-sys"), user_package), num_fast(SIGSYS));
#ifdef SIGWINCH
  reg_var(intern(lit("sig-winch"), user_package), num_fast(SIGWINCH));
#endif
#ifdef SIGIOT
  reg_var(intern(lit("sig-iot"), user_package), num_fast(SIGIOT));
#endif
#ifdef SIGSTKFLT
  reg_var(intern(lit("sig-stkflt"), user_package), num_fast(SIGSTKFLT));
#endif
#ifdef SIGIO
  reg_var(intern(lit("sig-io"), user_package), num_fast(SIGIO));
#endif
#ifdef SIGLOST
  reg_var(intern(lit("sig-lost"), user_package), num_fast(SIGLOST));
#endif
#ifdef SIGPWR
  reg_var(intern(lit("sig-pwr"), user_package), num_fast(SIGPWR));
#endif

  reg_fun(intern(lit("set-sig-handler"), user_package), func_n2(set_sig_handler));
  reg_fun(intern(lit("get-sig-handler"), user_package), func_n1(get_sig_handler));
  reg_fun(intern(lit("sig-check"), user_package), func_n0(sig_check));
}

#if HAVE_SIGALTSTACK

static void *stack;

static void setup_alt_stack(void)
{
  stack_t ss;

  if (!stack)
    stack = chk_malloc(SIGSTKSZ);

  ss.ss_sp = stack;
  ss.ss_size = SIGSTKSZ;
  ss.ss_flags = 0;

  if (sigaltstack(&ss, NULL) == -1) {
    free(stack);
    stack = 0;
  }
}

static void teardown_alt_stack(void)
{
  stack_t ss;

  if (!stack)
    return;

  ss.ss_sp = stack;
  ss.ss_size = SIGSTKSZ;
  ss.ss_flags = SS_DISABLE;

  if (sigaltstack(&ss, NULL) == -1)
    return;

  free(stack);
  stack = 0;
}

#endif

val set_sig_handler(val signo, val lambda)
{
  static struct sigaction blank;
  cnum sig = c_num(signo);
  val old_lambda;
  sigset_t block, saved;

  sigfillset(&block);
  sig_mask(SIG_BLOCK, &block, &saved);

  if (sig < 0 || sig >= MAX_SIG)
    uw_throwf(error_s, lit("set-sig-handler: signal ~s out of range\n"), sig, nao);

  old_lambda = sig_lambda[sig];

  if (lambda != old_lambda) {
    unsigned long mask = 1UL << sig;

    if (lambda == nil) {
      signal(sig, SIG_IGN);
      sig_deferred &= ~mask;
    } else if (lambda == t) {
      signal(sig, SIG_DFL);
      sig_deferred &= ~mask;
    } else {
      struct sigaction sa = blank;

      type_check(lambda, FUN);

      sa.sa_flags = SA_RESTART;
      sa.sa_handler = sig_handler;
      sigfillset(&sa.sa_mask);
#if HAVE_SIGALTSTACK
      if (sig == SIGSEGV)
        setup_alt_stack();
      sa.sa_flags |= SA_ONSTACK;
#endif
      sigaction(sig, &sa, 0);
    }

#if HAVE_SIGALTSTACK
    if (sig == SIGSEGV && (lambda == nil || lambda == t))
        teardown_alt_stack();
#endif

    sig_lambda[sig] = lambda;
  }

  sig_mask(SIG_SETMASK, &saved, 0);

  return old_lambda;
}

val get_sig_handler(val signo)
{
  cnum sig = c_num(signo);

  if (sig < 0 || sig >= MAX_SIG)
    uw_throwf(error_s, lit("get-sig-handler: signal ~s out of range\n"), sig, nao);

  return sig_lambda[sig];
}

val sig_check(void)
{
  unsigned long sd;
  int i;

  if ((sd = sig_deferred) == 0)
    return nil;

  for (i = 0; i < MAX_SIG; i++) {
    unsigned long mask = 1UL << i;
    if ((sd & mask) != 0) {
      sd &= ~mask;
      sig_deferred = sd;
      funcall2(sig_lambda[i], num_fast(i), nil);
    }
  }

  return t;
}

static void mem_set_bits(mem_t *target, const mem_t *bits, size_t size)
{
  while (size--)
    *target++ |= *bits++;
}

static void mem_clr_bits(mem_t *target, const mem_t *bits, size_t size)
{
  while (size--)
    *target++ &= ~*bits++;
}

int sig_mask(int how, const sigset_t *set, sigset_t *oldset)
{
  sigset_t newset;
  const sigset_t *pnew;

  switch (how) {
  case SIG_SETMASK:
    pnew = set;
    break;
  case SIG_BLOCK:
    pnew = &newset;
    newset = sig_blocked_cache;
    mem_set_bits((mem_t *) &newset, (mem_t *) set, sizeof newset);
    break;
  case SIG_UNBLOCK:
    pnew = &newset;
    newset = sig_blocked_cache;
    mem_clr_bits((mem_t *) &newset, (mem_t *) set, sizeof newset);
    break;
  default:
    errno = EINVAL;
    return -1;
  }

  if (memcmp(&sig_blocked_cache, pnew, sizeof *pnew) != 0) {
    sig_blocked_cache = *pnew;
    return sig_mask(SIG_BLOCK, pnew, oldset);
  }

  if (oldset != 0)
    *oldset = sig_blocked_cache;
  return 0;
}
