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
#include <stdarg.h>
#include <setjmp.h>
#include <wchar.h>
#include <dirent.h>
#include <signal.h>
#include "config.h"
#include "lib.h"
#include "stream.h"
#include "hash.h"
#include "gc.h"
#include "signal.h"
#include "unwind.h"

#define MAX_SIG 32

volatile sig_atomic_t async_sig_enabled = 0;

static val sig_lambda[MAX_SIG];
volatile unsigned long sig_deferred;

static void sig_handler(int sig)
{
  val lambda = sig_lambda[sig];
  if (lambda) {
    if (async_sig_enabled) {
      async_sig_enabled = 0;
      funcall1(lambda, num_fast(sig));
      async_sig_enabled = 1;
    } else {
      sig_deferred |= (1UL << sig);
    }
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
}

val set_sig_handler(val signo, val lambda)
{
  cnum sig = c_num(signo);
  val old_lambda;

  if (sig < 0 || sig >= MAX_SIG)
    uw_throwf(error_s, lit("set-sig-handler: signal ~s out of range\n"), sig, nao);

  old_lambda = sig_lambda[sig];

  if (lambda != old_lambda) {
    if (lambda == nil) {
      signal(sig, SIG_IGN);
    } else if (lambda == t) {
      signal(sig, SIG_DFL);
    } else {
      struct sigaction sa = { 0 };

      type_check(lambda, FUN);

      sa.sa_flags = SA_RESTART;
      sa.sa_handler = sig_handler;
      sigfillset(&sa.sa_mask);
      sigaction(sig, &sa, 0);
    }

    sig_lambda[sig] = lambda;
  }

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
      funcall1(sig_lambda[i], num_fast(i));
    }
  }

  return t;
}
