/* Copyright 2009-2014
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

void gc_init(val *stack_bottom);
val prot1(val *loc);
void rel1(val *loc);
void protect(val *, ...);
val make_obj(void);
void gc(void);
int gc_state(int);
void gc_mark(val);
int gc_is_reachable(val);

#if CONFIG_GEN_GC
val gc_set(val *, val);
val gc_push(val, val *);
val gc_mutated(val);
#endif

void unmark(void);
void gc_hint_func(val *);

#if EXTRA_DEBUGGING
val break_obj;
#endif

#define gc_hint(var) gc_hint_func(&var)
#define REACHABLE 0x100
#define FREE      0x200
