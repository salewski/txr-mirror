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

#ifdef __GNUC__
#define noreturn __attribute__((noreturn))
#else
#define noreturn
#endif

typedef union uw_frame uw_frame_t;
typedef enum uw_frtype { UW_BLOCK, UW_ENV, UW_CATCH, UW_DBG } uw_frtype_t;

struct uw_common {
  uw_frame_t *up;
  uw_frtype_t type;
};

struct uw_block {
  uw_frame_t *up;
  uw_frtype_t type;
  val tag;
  val result;
  val protocol;
  extended_jmp_buf jb;
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
  extended_jmp_buf jb;
};

struct uw_debug {
  uw_frame_t *up;
  uw_frtype_t type;
  val func;
  val args;
  val ub_p_a_pairs;
  val env;
  val data;
  val line;
  val chr;
};

union uw_frame {
  struct uw_common uw;
  struct uw_block bl;
  struct uw_dynamic_env ev;
  struct uw_catch ca;
  struct uw_debug db;
};

void uw_push_block(uw_frame_t *, val tag);
void uw_push_env(uw_frame_t *);
val uw_get_func(val sym);
val uw_set_func(val sym, val value);
val uw_get_match_context(void);
val uw_set_match_context(val context);
val uw_block_return_proto(val tag, val result, val protocol);
INLINE val uw_block_return(val tag, val result)
{
   return uw_block_return_proto(tag, result, nil);
}
void uw_push_catch(uw_frame_t *, val matches);
noreturn val uw_throw(val sym, val exception);
noreturn val uw_throwf(val sym, val fmt, ...);
noreturn val uw_throwfv(val sym, val fmt, val args);
noreturn val uw_errorf(val fmt, ...);
noreturn val uw_errorfv(val fmt, val args);
val uw_register_subtype(val sub, val super);
val uw_exception_subtype_p(val sub, val sup);
void uw_continue(uw_frame_t *curr, uw_frame_t *target);
void uw_push_debug(uw_frame_t *, val func, val args,
                   val ub_p_a_pairs, val env, val data,
                   val line, val chr);
void uw_pop_frame(uw_frame_t *);
void uw_pop_until(uw_frame_t *);
uw_frame_t *uw_current_frame(void);
uw_frame_t *uw_current_exit_point(void);
void uw_init(void);
void uw_late_init(void);

noreturn val type_mismatch(val, ...);

#define uw_mark_frame                           \
  uw_frame_t *uw_top = uw_current_frame()

#define uw_fast_return(VAL)                     \
  do {                                          \
    uw_pop_until(uw_top);                       \
    return VAL;                                 \
  } while (0)

#define uw_block_begin(TAG, RESULTVAR)          \
  obj_t *RESULTVAR = nil;                       \
  do {                                          \
    uw_frame_t uw_blk;                          \
    uw_push_block(&uw_blk, TAG);                \
    if (extended_setjmp(uw_blk.bl.jb)) {        \
      RESULTVAR = uw_blk.bl.result;             \
    } else {                                    \
      do { } while (0)

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

#define uw_simple_catch_begin                   \
  do {                                          \
    uw_frame_t uw_catch;                        \
    uw_push_catch(&uw_catch, nil);              \
    switch (extended_setjmp(uw_catch.ca.jb)) {  \
    case 0:

#define uw_catch_begin(MATCHES, SYMVAR,         \
                       EXCVAR)                  \
  do {                                          \
    obj_t *SYMVAR = nil;                        \
    obj_t *EXCVAR = nil;                        \
    uw_frame_t uw_catch;                        \
    uw_push_catch(&uw_catch, MATCHES);          \
    switch (extended_setjmp(uw_catch.ca.jb)) {  \
    case 0:

#define uw_catch(SYMVAR, EXCVAR)        \
    goto uw_unwind_label;               \
      break;                            \
    case 2:                             \
      EXCVAR = uw_catch.ca.exception;   \
      SYMVAR = uw_catch.ca.sym;         \
      (void) SYMVAR;                    \
      /* prevent looping */             \
      uw_catch.ca.matches = nil;

#define uw_unwind                       \
    /* suppress unused label warning */ \
    goto uw_unwind_label;               \
    uw_unwind_label:                    \
    case 1:                             \
      /* prevent looping */             \
      uw_catch.ca.visible = 0;

#define uw_catch_end                    \
      break;                            \
    default:                            \
      abort();                          \
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
