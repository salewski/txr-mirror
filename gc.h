/* Copyright 2009-2017
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

void gc_init(val *stack_bottom);
void gc_late_init(void);
val prot1(val *loc);
void protect(val *, ...);
val make_obj(void);
void gc(void);
int gc_state(int);
int gc_inprogress(void);
void gc_mark(val);
void gc_conservative_mark(val);
void gc_mark_mem(val *low, val *high);
int gc_is_reachable(val);
val gc_finalize(val obj, val fun, val rev_order_p);
val gc_call_finalizers(val obj);

#if CONFIG_GEN_GC
val gc_set(loc, val);
val gc_push(val, loc);
val gc_mutated(val);
extern int full_gc;
#endif

void unmark(void);
void gc_cancel(void);
void gc_hint_func(val *);
void gc_report_copies(val *pvar);
void gc_free_all(void);

extern int gc_enabled;
extern val **gc_prot_top;

#if CONFIG_EXTRA_DEBUGGING
extern val break_obj;
#endif

#define gc_hint(var) gc_hint_func(&var)
#define REACHABLE 0x100
#define FREE      0x200

INLINE val zap(volatile val *loc) { val ret = *loc; *loc = nil; return ret; }
#define z(lvalue) zap(&lvalue)
