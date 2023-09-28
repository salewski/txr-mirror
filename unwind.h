/* Copyright 2009-2023
 * Kaz Kylheku <kaz@kylheku.com>
 * Vancouver, Canada
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

#if __i386__

struct jmp {
   unsigned eip;
   unsigned esp;
   unsigned ebp;
   unsigned ebx;
   unsigned esi;
   unsigned edi;
};

#elif __x86_64__

struct jmp {
   unsigned long rip;
   unsigned long rsp;
   unsigned long rbp;
   unsigned long rbx;
   unsigned long r12;
   unsigned long r13;
   unsigned long r14;
   unsigned long r15;
#if __CYGWIN__
   unsigned long rsi;
   unsigned long rdi;
#endif
};

#elif __arm__ && !__thumb__

struct jmp {
  unsigned long r4;
  unsigned long r5;
  unsigned long r6;
  unsigned long r7;
  unsigned long r8;
  unsigned long r9;
  unsigned long r10;
  unsigned long fp;
  unsigned long sp;
  unsigned long lr;
};

#elif __arm__ && __thumb__

struct jmp {
  unsigned long lr;
  unsigned long r4;
  unsigned long r5;
  unsigned long r6;
  unsigned long r7;
  unsigned long r8;
  unsigned long r9;
  unsigned long r10;
  unsigned long fp;
  unsigned long sp;
};

#elif __PPC64__

struct jmp {
  unsigned long r1;
  unsigned long r2;
  unsigned long r11;
  unsigned long r12;
  unsigned long r13;
  unsigned long r14;
  unsigned long r15;
  unsigned long r16;
  unsigned long r17;
  unsigned long r18;
  unsigned long r19;
  unsigned long r20;
  unsigned long r21;
  unsigned long r22;
  unsigned long r23;
  unsigned long r24;
  unsigned long r25;
  unsigned long r26;
  unsigned long r27;
  unsigned long r28;
  unsigned long r29;
  unsigned long r30;
  unsigned long r31;
};

#elif __aarch64__

struct jmp {
  unsigned long x19;
  unsigned long x20;
  unsigned long x21;
  unsigned long x22;
  unsigned long x23;
  unsigned long x24;
  unsigned long x25;
  unsigned long x26;
  unsigned long x27;
  unsigned long x28;
  unsigned long x29;
  unsigned long x30;
  unsigned long d8;
  unsigned long d9;
  unsigned long d10;
  unsigned long d11;
  unsigned long d12;
  unsigned long d13;
  unsigned long d14;
  unsigned long d15;
  unsigned long x16;
};

/* Jump buffer contains:
   x19-x28, x29(fp), x30(lr), (x31)sp, d8-d15.  Other registers are not
   saved.  */

#elif __mips__

struct jmp {
  unsigned long s0; /* $16 */
  unsigned long s1; /* ... */
  unsigned long s2;
  unsigned long s3;
  unsigned long s4;
  unsigned long s5;
  unsigned long s6; /* ... */
  unsigned long s8; /* $23 */
  unsigned long gp; /* $28 */
  unsigned long sp; /* $29 */
  unsigned long fp; /* $30 */
  unsigned long ra; /* $31 */
};

#elif __riscv

struct jmp {
  unsigned long ra;   /*  x1 */
  unsigned long sp;   /*  x2 */
  unsigned long fp;   /*  x8 */
  unsigned long s1;   /*  x9 */
  unsigned long s2;   /* x18 */
  unsigned long s3;   /* x19 */
  unsigned long s4;   /* x20 */
  unsigned long s5;   /* x21 */
  unsigned long s6;   /* x22 */
  unsigned long s7;   /* x23 */
  unsigned long s8;   /* x24 */
  unsigned long s9;   /* x25 */
  unsigned long s10;  /* x26 */
  unsigned long s11;  /* x27 */
#if 0 && !__riscv_float_abi_soft
  double fs0;         /*  f8 */
  double fs1;         /*  f9 */
  double fs2;         /* f18 */
  double fs3;         /* f19 */
  double fs4;         /* f20 */
  double fs5;         /* f21 */
  double fs6;         /* f22 */
  double fs7;         /* f23 */
  double fs8;         /* f24 */
  double fs9;         /* f25 */
  double fs10;        /* f26 */
  double fs11;        /* f27 */
#endif
};

#elif __loongarch64

struct jmp {
  unsigned long ra;
  unsigned long sp;
  unsigned long fp;
  unsigned long s0;
  unsigned long s1;
  unsigned long s2;
  unsigned long s3;
  unsigned long s4;
  unsigned long s5;
  unsigned long s6;
  unsigned long s7;
  unsigned long s8;
#if 0 && !__loongarch64_soft_float
  double fs0;
  double fs1;
  double fs2;
  double fs3;
  double fs4;
  double fs5;
  double fs6;
  double fs7;
#endif
};

#else
#error port me!
#endif

#ifdef __cplusplus
extern "C" {
#endif

int jmp_save(struct jmp *);
NORETURN void jmp_restore(struct jmp *, int);

#ifdef __cplusplus
}
#endif

#if CONFIG_DEBUG_SUPPORT
#define EJ_DBG_MEMB int ds;
#else
#define EJ_DBG_MEMB
#endif

#define EJ_OPT_MEMB EJ_DBG_MEMB

#if HAVE_POSIX_SIGS

typedef struct {
  struct jmp jb;
  volatile sig_atomic_t se;
  volatile small_sigset_t blocked;
  volatile val de;
  volatile int gc;
  val **volatile gc_pt;
  EJ_OPT_MEMB
  volatile int rv;
} extended_jmp_buf;

#else

typedef struct {
  struct jmp jb;
  volatile val de;
  volatile int gc;
  val **volatile gc_pt;
  EJ_OPT_MEMB
  volatile int rv;
} extended_jmp_buf;

#endif

#define extended_setjmp(EJB)                    \
  (jmp_save(&(EJB).jb)                          \
   ? ((EJB).rv)                                 \
   : (extjmp_save(&(EJB)), 0))

#define extended_longjmp(EJB, ARG)              \
  ((EJB).rv = (ARG), extjmp_restore(&(EJB)), jmp_restore(&(EJB).jb, 1))

void extjmp_save(extended_jmp_buf *ejb);
void extjmp_restore(extended_jmp_buf *);

typedef union uw_frame uw_frame_t;
typedef enum uw_frtype {
  UW_BLOCK, UW_CAPTURED_BLOCK, UW_MENV, UW_CATCH, UW_HANDLE,
  UW_CONT_COPY, UW_GUARD,
#if CONFIG_DEBUG_SUPPORT
  UW_FCALL, UW_EVAL, UW_EXPAND
#endif
} uw_frtype_t;

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
  mem_t *cont_bottom;
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
  int visible;
  val sym;
  val args;
  val desc;
  uw_frame_t *cont;
  extended_jmp_buf jb;
};

struct uw_handler {
  uw_frame_t *up;
  uw_frtype_t type;
  val matches; /* Same position as in uw_catch! */
  int visible; /* Likewise. */
  val fun;
  val package, package_alist;
};

struct uw_cont_copy {
  uw_frame_t *up;
  uw_frtype_t type;
  mem_t *ptr;
  void (*copy)(mem_t *ptr);
};

struct uw_guard {
  uw_frame_t *up;
  uw_frtype_t type;
  int uw_ok;
};

#if CONFIG_DEBUG_SUPPORT

struct uw_fcall {
  uw_frame_t *up;
  uw_frtype_t type;
  val fun;
  struct args *args;
};

struct uw_eval {
  uw_frame_t *up;
  uw_frtype_t type;
  val form;
  val env;
};

#endif

#if __aarch64__ || (__clang__ && __amd64__)
#define UW_FRAME_ALIGN __attribute__ ((aligned (16)))
#else
#define UW_FRAME_ALIGN
#endif

union uw_frame {
  struct uw_common uw;
  struct uw_block bl;
  struct uw_dynamic_env ev;
  struct uw_catch ca;
  struct uw_handler ha;
  struct uw_cont_copy cp;
  struct uw_guard gu;
#if CONFIG_DEBUG_SUPPORT
  struct uw_fcall fc;
  struct uw_eval el;
#endif
} UW_FRAME_ALIGN;

typedef struct {
  val de;
  uw_frame_t *stack;
  uw_frame_t *menv_stack;
} uw_snapshot_t;

extern val catch_frame_s;

uw_snapshot_t uw_snapshot(void);
void uw_restore(const uw_snapshot_t *);
void uw_push_block(uw_frame_t *, val tag);
void uw_push_match_env(uw_frame_t *);
val uw_get_func(val sym);
val uw_set_func(val sym, val value);
val uw_get_match_context(void);
val uw_set_match_context(val context);
val uw_block_return_proto(val tag, val result, val protocol);
INLINE val uw_block_return(val tag, val result)
{
   return uw_block_return_proto(tag, result, nil);
}
val uw_block_abscond(val tag, val result);
void uw_push_catch(uw_frame_t *, val matches);
void uw_push_handler(uw_frame_t *, val matches, val fun);
#if CONFIG_DEBUG_SUPPORT
void uw_push_fcall(uw_frame_t *, val fun, struct args *args);
void uw_push_eval(uw_frame_t *, val form, val env);
void uw_push_expand(uw_frame_t *, val form, val env);
#endif
val uw_rthrow(val sym, val exception);
val uw_rthrowv(val sym, struct args *);
val uw_rthrowfv(val sym, val fmt, struct args *);
NORETURN val uw_throw(val sym, val exception);
NORETURN val uw_throwf(val sym, val fmt, ...);
NORETURN val uw_ethrowf(val sym, val fmt, ...);
NORETURN val uw_errorfv(val fmt, struct args *args);
val uw_warningf(val fmt, ...);
val uw_defer_warning(val args);
val uw_warning_exists(val tag);
val uw_dump_deferred_warnings(val stream);
val uw_release_deferred_warnings(void);
val uw_purge_deferred_warning(val tag);
val uw_register_tentative_def(val tag);
val uw_tentative_def_exists(val tag);
val uw_register_subtype(val sub, val super);
val uw_exception_subtype_p(val sub, val sup);
void uw_continue(uw_frame_t *target);
void uw_push_guard(uw_frame_t *, int uw_ok);
void uw_pop_frame(uw_frame_t *);
void uw_pop_block(uw_frame_t *, val *pret);
void uw_pop_until(uw_frame_t *);
uw_frame_t *uw_current_frame(void);
uw_frame_t *uw_current_exit_point(void);
val uw_get_frames(void);
val uw_find_frame(val extype, val frtype);
val uw_find_frames(val extype, val frtype);
#if CONFIG_DEBUG_SUPPORT
val uw_find_frames_by_mask(val mask);
val uw_last_form_expanded(void);
#else
#define uw_last_form_expanded() nil
#endif
val uw_invoke_catch(val catch_frame, val sym, struct args *);
val uw_muffle_warning(val exc, struct args *);
val uw_trace_error(val ctx, val exc, struct args *);
val uw_capture_cont(val tag, val fun, val ctx_form);
void uw_push_cont_copy(uw_frame_t *, mem_t *ptr,
                       void (*copy)(mem_t *ptr));
void uw_init(void);
void uw_late_init(void);

NORETURN val type_mismatch(val, ...);
NORETURN void invalid_ops(val self, val obj1, val obj2);
NORETURN void invalid_op(val self, val obj);

#define uw_mark_frame                           \
  uw_frame_t *uw_top = uw_current_frame()

#define uw_fast_return(VAL)                     \
  do {                                          \
    uw_pop_until(uw_top);                       \
    return VAL;                                 \
  } while (0)

#define uw_block_beg(TAG, RESULTVAR)            \
  do {                                          \
    uw_frame_t uw_blk;                          \
    obj_t **uw_rslt = &RESULTVAR;               \
    uw_push_block(&uw_blk, TAG);                \
    if (extended_setjmp(uw_blk.bl.jb)) {        \
      RESULTVAR = uw_blk.bl.result;             \
    } else {                                    \
      enum { dummy ## __LINE__ }

#define uw_block_begin(TAG, RESULTVAR)          \
  obj_t *RESULTVAR = nil;                       \
  do {                                          \
    uw_frame_t uw_blk;                          \
    obj_t **uw_rslt = &RESULTVAR;               \
    uw_push_block(&uw_blk, TAG);                \
    if (extended_setjmp(uw_blk.bl.jb)) {        \
      RESULTVAR = uw_blk.bl.result;             \
    } else {                                    \
      enum { dummy ## __LINE__ }

#define uw_block_end                            \
    }                                           \
    uw_pop_block(&uw_blk, uw_rslt);             \
  } while (0)

#define uw_match_env_begin              \
  do {                                  \
    uw_frame_t uw_env;                  \
    uw_push_match_env(&uw_env);         \
    {                                   \
      enum { dummy ## __LINE__ }

#define uw_match_env_end                \
    }                                   \
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

#define uw_catch_begin_w_desc(MATCHES, SYMVAR,  \
                              EXCVAR, DESC)     \
  do {                                          \
    obj_t *SYMVAR = nil;                        \
    obj_t *EXCVAR = nil;                        \
    uw_frame_t uw_catch;                        \
    uw_push_catch(&uw_catch, MATCHES);          \
    uw_catch.ca.desc = (DESC);                  \
    switch (extended_setjmp(uw_catch.ca.jb)) {  \
    case 0:
#define uw_catch(SYMVAR, EXCVAR)        \
    goto uw_unwind_label;               \
      break;                            \
    case 2:                             \
      EXCVAR = uw_catch.ca.args;        \
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

#define uw_curr_exit_point              \
  (uw_catch.ca.cont)

#define uw_catch_end                    \
      break;                            \
    default:                            \
      abort();                          \
    }                                   \
    uw_pop_frame(&uw_catch);            \
    if (uw_catch.ca.cont)               \
      uw_continue(uw_catch.ca.cont);    \
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

#define panic(STR)                      \
  do {                                  \
    extern obj_t *num(cnum);            \
    uw_throwf(panic_s,                  \
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

#define ignerr_func_body(type, init, expr,              \
                         stream, prefix)                \
   val (_s_y_m_s) = cons(error_s, nil);                 \
   type (_r_e_t) = (init);                              \
   uw_frame_t _h_n_d;                                   \
   uw_catch_begin (_s_y_m_s, _e_x, _e_x_a);             \
   uw_push_handler(&_h_n_d, _s_y_m_s,                   \
                   func_f1v(cons(stream, prefix),       \
                            uw_trace_error));           \
   _r_e_t = expr;                                       \
   uw_pop_frame(&_h_n_d);                               \
   uw_catch(_e_x, _e_x_a);                              \
   (void) _e_x_a;                                       \
   uw_unwind { }                                        \
   uw_catch_end;                                        \
   return _r_e_t;

#define ignerr_begin                                            \
   uw_catch_begin (cons(error_s, nil), _x_s_y_m, _x_a_r_g_s)

#define ignerr_end                                              \
   uw_catch(_x_s_y_m, _x_a_r_g_s)                               \
   (void) _x_s_y_m; (void) _x_a_r_g_s;                          \
   uw_unwind { }                                                \
   uw_catch_end
