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

struct args {
  cnum argc;
  cnum fill;
  val list;
  val arg[1];
};

typedef int arg_index;

#define ARGS_MAX 1024

#define args_alloc(N)                                                   \
  (coerce(struct args *,                                                \
          alloca(offsetof(struct args, arg) + (N)*sizeof (val))))

cnum args_limit(val name, cnum in);

INLINE void args_init(struct args *args, cnum argc)
{
  args->argc = argc;
  args->fill = 0;
  args->list = nil;
}

INLINE void args_init_list(struct args *args, cnum argc, val list)
{
  args->argc = argc;
  args->fill = 0;
  args->list = list;
}

INLINE val args_add(struct args *args, val arg)
{
  return args->arg[args->fill++] = arg;
}

void args_add_list(struct args *args, val list);

val args_add_checked(val name, struct args *args, val arg);

INLINE int args_more(struct args *args, cnum index)
{
  return index < args->fill;
}

INLINE val args_get_list(struct args *args)
{
  extern val args_cons_list(struct args *args);
  return (args->fill == 0 || args->list) ? args->list : args_cons_list(args);
}

INLINE val args_get(struct args *args, cnum *arg_index)
{
  return args->arg[(*arg_index)++];
}

val args_get_checked(val name, struct args *args, cnum *arg_index);
