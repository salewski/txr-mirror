/* Copyright 2011
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

#ifdef __GNUC__
#define noreturn __attribute__((noreturn))
#else
#define noreturn
#endif

typedef union uw_frame uw_frame_t;
typedef enum uw_frtype { UW_BLOCK, UW_ENV, UW_CATCH } uw_frtype_t;

struct uw_common {
  uw_frame_t *up;
  uw_frtype_t type;
};

struct uw_block {
  uw_frame_t *up;
  uw_frtype_t type;
  val tag;
  val result;
  jmp_buf jb;
};

struct uw_dynamic_env {
  uw_frame_t *up;
  uw_frtype_t type;
  uw_frame_t *up_env;
  val func_bindings;
  val match_context;
};

struct uw_catch {
  uw_frame_t *up;
  uw_frtype_t type;
  val matches;
  val sym;
  val exception;
  uw_frame_t *cont;
  int visible;
  jmp_buf jb;
};

union uw_frame {
  struct uw_common uw;
  struct uw_block bl;
  struct uw_dynamic_env ev;
  struct uw_catch ca;
};

void uw_push_block(uw_frame_t *, val tag);
void uw_push_env(uw_frame_t *);
val uw_get_func(val sym);
val uw_set_func(val sym, val value);
val uw_get_match_context(void);
val uw_set_match_context(val context);
val uw_block_return(val tag, val result);
void uw_push_catch(uw_frame_t *, val matches);
noreturn val uw_throw(val sym, val exception);
noreturn val uw_throwf(val sym, val fmt, ...);
noreturn val uw_errorf(val fmt, ...);
val uw_register_subtype(val sub, val super);
val uw_exception_subtype_p(val sub, val sup);
void uw_continue(uw_frame_t *curr, uw_frame_t *target);
void uw_pop_frame(uw_frame_t *);
void uw_init(void);

noreturn val type_mismatch(val, ...);

#define uw_block_begin(TAG, RESULTVAR)  \
  obj_t *RESULTVAR = nil;               \
  do                                    \
  {                                     \
    uw_frame_t uw_blk;                  \
    uw_push_block(&uw_blk, TAG);        \
    if (setjmp(uw_blk.bl.jb)) {         \
      RESULTVAR = uw_blk.bl.result;     \
    } else {                            \
      typedef int uw_d_u_m_m_y

#define uw_block_end                    \
    }                                   \
    uw_pop_frame(&uw_blk);              \
  } while (0)

#define uw_env_begin                    \
  do {                                  \
    uw_frame_t uw_env;                  \
    uw_push_env(&uw_env)

#define uw_env_end                      \
    uw_pop_frame(&uw_env);              \
  } while (0)

#define uw_catch_begin(MATCHES, SYMVAR, \
                       EXCVAR)          \
  obj_t *SYMVAR = nil;                  \
  obj_t *EXCVAR = nil;                  \
  do {                                  \
    uw_frame_t uw_catch;                \
    uw_push_catch(&uw_catch, MATCHES);  \
    switch (setjmp(uw_catch.ca.jb)) {   \
    case 0:

#define uw_do_unwind                    \
      goto uw_unwind_label

#define uw_catch(SYMVAR, EXCVAR)        \
      break;                            \
    case 2:                             \
      EXCVAR = uw_catch.ca.exception;   \
      SYMVAR = uw_catch.ca.sym;

#define uw_unwind                       \
      break;                            \
    uw_unwind_label:                    \
    case 1:

#define uw_catch_end                    \
    default:                            \
      break;                            \
    }                                   \
    if (uw_catch.ca.cont)               \
      uw_continue(&uw_catch,            \
                  uw_catch.ca.cont);    \
    uw_pop_frame(&uw_catch);            \
  } while(0)

#define internal_error(STR)             \
  do {                                  \
    extern obj_t *num(cnum);            \
    uw_throwf(internal_error_s,         \
              lit("~a:~a ~a"),          \
              lit(__FILE__),            \
              num(__LINE__), lit(STR),  \
              nao);                     \
  } while (0)

#define type_assert(EXPR, ARGS)         \
  if (!(EXPR)) type_mismatch ARGS

#define bug_unless(EXPR)                \
  if (!(EXPR))                          \
    internal_error("assertion "         \
                            #EXPR       \
                            " failed")

#define numeric_assert(EXPR)            \
  if (!(EXPR))                          \
   uw_throwf(numeric_error_s,           \
             lit("assertion " #EXPR     \
                 "failed"), nao)

#define range_bug_unless(EXPR)          \
  if (!(EXPR))                          \
   uw_throwf(range_error_s,             \
             lit("assertion " #EXPR     \
                 "failed"), nao)

