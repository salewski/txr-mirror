/* Copyright 2009-2015
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

void gc_init(val *stack_bottom);
void gc_late_init(void);
val prot1(val *loc);
void protect(val *, ...);
val make_obj(void);
void gc(void);
int gc_state(int);
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
void gc_hint_func(val *);
void gc_report_copies(val *pvar);

extern int gc_enabled;
extern val **gc_prot_top;

#if EXTRA_DEBUGGING
extern val break_obj;
#endif

#define gc_hint(var) gc_hint_func(&var)
#define REACHABLE 0x100
#define FREE      0x200

INLINE val zap(volatile val *loc) { val ret = *loc; *loc = nil; return ret; }
#define z(lvalue) zap(&lvalue)
