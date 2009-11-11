/* Copyright 2009
 * Kaz Kylheku <kkylheku@gmail.com>
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
typedef enum uw_frtype uw_frtype_t;

enum uw_frtype { UW_BLOCK, UW_ENV, UW_CATCH };

struct uw_common {
  uw_frame_t *up;
  uw_frtype_t type;
};

struct uw_block {
  uw_frame_t *up;
  uw_frtype_t type;
  obj_t *tag;
  obj_t *result;
  jmp_buf jb;
};

struct uw_dynamic_env {
  uw_frame_t *up;
  uw_frtype_t type;
  obj_t *func_bindings;
};

struct uw_catch {
  uw_frame_t *up;
  uw_frtype_t type;
  obj_t *matches;
  obj_t *sym;
  obj_t *exception;
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

void uw_push_block(uw_frame_t *, obj_t *tag);
void uw_push_env(uw_frame_t *);
obj_t *uw_get_func(obj_t *sym);
obj_t *uw_set_func(obj_t *sym, obj_t *value);
obj_t *uw_block_return(obj_t *tag, obj_t *result);
void uw_push_catch(uw_frame_t *, obj_t *matches);
noreturn obj_t *uw_throw(obj_t *sym, obj_t *exception);
noreturn obj_t *uw_throwf(obj_t *sym, const wchar_t *fmt, ...);
noreturn obj_t *uw_errorf(const wchar_t *fmt, ...);
noreturn obj_t *uw_throwcf(obj_t *sym, const char *fmt, ...);
noreturn obj_t *uw_errorcf(const char *fmt, ...);
obj_t *uw_register_subtype(obj_t *sub, obj_t *super);
obj_t *uw_exception_subtype_p(obj_t *sub, obj_t *sup);
void uw_continue(uw_frame_t *curr, uw_frame_t *target);
void uw_pop_frame(uw_frame_t *);
void uw_init(void);

noreturn obj_t *type_mismatch(const wchar_t *, ...);

#define uw_block_begin(TAG, RESULTVAR)  \
  obj_t *RESULTVAR = nil;               \
  {                                     \
    uw_frame_t uw_blk;                  \
    uw_push_block(&uw_blk, TAG);        \
    if (setjmp(uw_blk.bl.jb)) {         \
      RESULTVAR = uw_blk.bl.result;     \
    } else  {

#define uw_block_end                    \
    }                                   \
    uw_pop_frame(&uw_blk);              \
  }

#define uw_env_begin                    \
  {                                     \
    uw_frame_t uw_env;                  \
    uw_push_env(&uw_env)

#define uw_env_end                      \
    uw_pop_frame(&uw_env);              \
  }

#define uw_catch_begin(MATCHES, SYMVAR, \
                       EXCVAR)          \
  obj_t *SYMVAR = nil;                  \
  obj_t *EXCVAR = nil;                  \
  {                                     \
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
      SYMVAR = uw_catch.ca.sym;         \

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
  }

#define internal_error(STR)             \
  uw_throwcf(internal_err,              \
             "%s:%d %s", __FILE__,      \
             __LINE__, STR)

#define type_assert(EXPR, ARGS)         \
  if (!(EXPR)) type_mismatch ARGS

#define bug_unless(EXPR)                \
  if (!(EXPR))                          \
    internal_error("assertion "         \
                            #EXPR       \
                            " failed")

#define numeric_assert(EXPR)            \
  if (!(EXPR))                          \
   uw_throwcf(numeric_err, "%s",        \
              "assertion " #EXPR        \
              " failed")

#define range_bug_unless(EXPR)          \
  if (!(EXPR))                          \
   uw_throwcf(range_err, "%s",          \
              "assertion" #EXPR         \
              " failed")
