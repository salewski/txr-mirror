/* Copyright 2009-2016
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

#define ARGS_MAX 32
#define ARGS_MIN 4

#define args_alloc(N)                                                   \
  (coerce(struct args *,                                                \
          alloca(offsetof(struct args, arg) + (N)*sizeof (val))))

INLINE struct args *args_init_list(struct args *args, cnum argc, val list)
{
  args->argc = argc;
  args->fill = 0;
  args->list = list;
  return args;
}

INLINE struct args *args_init(struct args *args, cnum argc)
{
  return args_init_list(args, argc, nil);
}

INLINE void args_set_fill(struct args *args, cnum fill)
{
  args->fill = fill;
}

#define args_decl_list(NAME, N, L)                                      \
  mem_t *NAME ## _mem =                                                 \
    coerce(mem_t *,                                                     \
           alloca(offsetof(struct args, arg) + (N)*sizeof (val)));      \
  struct args *NAME = args_init_list(coerce(struct args *,              \
                                            NAME ## _mem), N, L)

#define args_decl(NAME, N) args_decl_list(NAME, N, nil)


INLINE val args_add(struct args *args, val arg)
{
  return args->arg[args->fill++] = arg;
}

INLINE void args_add2(struct args *args, val arg1, val arg2)
{
  val *arg = args->arg + args->fill;
  args->fill += 2;
  *arg++ = arg1;
  *arg++ = arg2;
}

INLINE void args_add3(struct args *args, val arg1, val arg2, val arg3)
{
  val *arg = args->arg + args->fill;
  args->fill += 3;
  *arg++ = arg1;
  *arg++ = arg2;
  *arg++ = arg3;
}

INLINE void args_add4(struct args *args, val arg1, val arg2, val arg3, val arg4)
{
  val *arg = args->arg + args->fill;
  args->fill += 4;
  *arg++ = arg1;
  *arg++ = arg2;
  *arg++ = arg3;
  *arg++ = arg4;
}

val args_add_checked(val name, struct args *args, val arg);

INLINE void args_add_list(struct args *args, val list)
{
  args->list = list;
}

INLINE int args_more(struct args *args, cnum index)
{
  return index < args->fill || args->list;
}

INLINE int args_two_more(struct args *args, cnum index)
{
  return
    index + 1 < args->fill ||
    (index + 1 == args->fill && args->list) ||
    cdr(args->list);
}

void args_normalize(struct args *args, cnum fill);
void args_normalize_fill(struct args *args, cnum minfill, cnum maxfill);

INLINE val args_get_list(struct args *args)
{
  if (args->fill == 0)
    return z(args->list);
  args_normalize(args, 0);
  return z(args->list);
}

INLINE val args_get_rest(struct args *args, cnum index)
{
  if (args->fill == index)
    return z(args->list);
  args_normalize(args, index);
  return z(args->list);
}

INLINE val args_at(struct args *args, cnum arg_index)
{
  if (arg_index < args->fill)
    return args->arg[arg_index];
  return car(args->list);
}

INLINE val args_atz(struct args *args, cnum arg_index)
{
  if (arg_index < args->fill) {
    return z(args->arg[arg_index]);
  } else {
    loc l = car_l(args->list);
    return zap(valptr(l));
  }
}

INLINE val args_get(struct args *args, cnum *arg_index)
{
  if (*arg_index < args->fill)
    return z(args->arg[(*arg_index)++]);
  return pop(&args->list);
}

INLINE void args_clear(struct args *args)
{
  args->fill = 0;
}

val args_get_checked(val name, struct args *args, cnum *arg_index);
struct args *args_copy(struct args *to, struct args *from);
struct args *args_copy_zap(struct args *to, struct args *from);
struct args *args_cat_zap(struct args *to, struct args *from);
val args_copy_to_list(struct args *args);
