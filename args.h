/* Copyright 2009-2020
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

struct args {
  cnum argc;
  cnum fill;
  val list;
  val arg[1];
};

typedef int arg_index;

#define ARGS_MAX 32
#define ARGS_MIN 4

struct args_bool_key {
  val key;
  val arg_p;
  val *store;
};

INLINE struct args *args_init_list(struct args *args, cnum argc, val list)
{
  args->argc = argc;
  args->fill = 0;
  args->list = list;
  return args;
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

INLINE void args_add5(struct args *args, val arg1, val arg2, val arg3,
                      val arg4, val arg5)
{
  val *arg = args->arg + args->fill;
  args->fill += 5;
  *arg++ = arg1;
  *arg++ = arg2;
  *arg++ = arg3;
  *arg++ = arg4;
  *arg++ = arg5;
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

INLINE int args_more_nozap(struct args *args, cnum index, val list)
{
  return list || index < args->fill;
}

void args_normalize_exact(struct args *args, cnum fill);
void args_normalize_least(struct args *args, cnum fill);
void args_normalize_fill(struct args *args, cnum minfill, cnum maxfill);

INLINE val args_get_list(struct args *args)
{
  if (args->fill == 0)
    return z(args->list);
  args_normalize_exact(args, 0);
  return z(args->list);
}

INLINE val args_get_rest(struct args *args, cnum index)
{
  if (args->fill == index)
    return z(args->list);
  args_normalize_exact(args, index);
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
    return car(z(args->list));
  }
}

INLINE val args_get(struct args *args, cnum *arg_index)
{
  if (*arg_index < args->fill)
    return z(args->arg[(*arg_index)++]);
  return pop(&args->list);
}

INLINE val args_get_nozap(struct args *args, cnum *arg_index, val *list)
{
  if (*arg_index < args->fill)
    return args->arg[(*arg_index)++];
  return pop(list);
}

INLINE cnum args_count(struct args *args, val self)
{
  return args->fill + c_num(length_list(args->list), self);
}

val args_get_checked(val name, struct args *args, cnum *arg_index);
struct args *args_copy(struct args *to, struct args *from);
struct args *args_copy_zap(struct args *to, struct args *from);
struct args *args_cat(struct args *to, struct args *from);
struct args *args_cat_zap(struct args *to, struct args *from);
struct args *args_cat_zap_from(struct args *to, struct args *from, cnum index);
struct args *args_copy_reverse(struct args *to, struct args *from, cnum nargs);
val args_copy_to_list(struct args *args);
void args_for_each(struct args *args,
                   int (*fn)(val arg, int ix, mem_t *ctx),
                   mem_t *ctx);
void args_keys_extract(struct args *args, struct args_bool_key *, int n);
val dyn_args(struct args *args, val car, val cdr);
