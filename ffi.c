/* Copyright 2017-2022
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

#include <limits.h>
#include <float.h>
#include <math.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <signal.h>
#include <wchar.h>
#include <time.h>
#include "config.h"
#if HAVE_LIBFFI
#include <ffi.h>
#endif
#if HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#if HAVE_SOCKETS
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#endif
#if HAVE_MMAP
#include <sys/mman.h>
#include <unistd.h>
#include <errno.h>
#endif
#include "alloca.h"
#include "lib.h"
#include "stream.h"
#include "gc.h"
#include "signal.h"
#include "unwind.h"
#include "eval.h"
#include "struct.h"
#include "cadr.h"
#include "buf.h"
#include "itypes.h"
#include "arith.h"
#include "args.h"
#include "utf8.h"
#include "hash.h"
#if HAVE_MMAP
#include "sysif.h"
#endif
#include "ffi.h"
#include "txr.h"

#define zalloca(size) memset(alloca(size), 0, size)

#define alignof(type) offsetof(struct {char x; type y;}, y)

#define pad_retval(size) (!(size) || convert(size_t, size) > sizeof (ffi_arg) \
                          ? convert(size_t, size) \
                          : sizeof (ffi_arg))

#define min(a, b) ((a) < (b) ? (a) : (b))

#if HAVE_LITTLE_ENDIAN
#define ifbe(expr) (0)
#define ifbe2(expr1, expr2) (expr2)
#else
#define ifbe(expr) (expr)
#define ifbe2(expr1, expr2) (expr1)
#endif

#if !HAVE_LIBFFI
typedef struct ffi_type {
  char type, size;
} ffi_type;
#define FFI_TYPE_STRUCT 0
typedef unsigned long ffi_arg;
#endif

typedef enum {
  FFI_KIND_VOID,
  FFI_KIND_NUM,
  FFI_KIND_ENUM,
  FFI_KIND_PTR,
  FFI_KIND_STRUCT,
  FFI_KIND_UNION,
  FFI_KIND_ARRAY
} ffi_kind_t;

val uint8_s, int8_s;
val uint16_s, int16_s;
val uint32_s, int32_s;
val uint64_s, int64_s;

val char_s, zchar_s, uchar_s, bchar_s, wchar_s;
val short_s, ushort_s;
val int_s, uint_s;
val long_s, ulong_s;
val double_s;
val void_s;

val val_s;

val be_uint16_s, be_int16_s;
val be_uint32_s, be_int32_s;
val be_uint64_s, be_int64_s;
val be_float_s, be_double_s;

val le_uint16_s, le_int16_s;
val le_uint32_s, le_int32_s;
val le_uint64_s, le_int64_s;
val le_float_s, le_double_s;

val array_s, zarray_s, carray_s;

val union_s;

val str_d_s, wstr_s, wstr_d_s, bstr_s, bstr_d_s;

val buf_d_s;


val ptr_in_s, ptr_out_s, ptr_in_d_s, ptr_out_d_s, ptr_out_s_s, ptr_s;

val closure_s;

val sbit_s, ubit_s; /* bit_s is in arith.c */

val enum_s, enumed_s, elemtype_s;

val align_s;

val bool_s;

val ffi_type_s, ffi_call_desc_s, ffi_closure_s;

static val ffi_typedef_hash, ffi_struct_tag_hash;

#if HAVE_LIBFFI
static uw_frame_t *s_exit_point;
#endif

#if !HAVE_LIBFFI
static ffi_type ffi_type_void, ffi_type_pointer, ffi_type_sint;
static ffi_type ffi_type_schar, ffi_type_uchar;
static ffi_type ffi_type_sshort, ffi_type_ushort;
static ffi_type ffi_type_sint, ffi_type_uint;
static ffi_type ffi_type_slong, ffi_type_ulong;
static ffi_type ffi_type_sint8, ffi_type_uint8;
static ffi_type ffi_type_sint16, ffi_type_uint16;
static ffi_type ffi_type_sint32, ffi_type_uint32;
static ffi_type ffi_type_sint64, ffi_type_uint64;
static ffi_type ffi_type_float, ffi_type_double;
#endif

static struct cobj_class *ffi_type_cls, *ffi_call_desc_cls;
static struct cobj_class *ffi_closure_cls, *union_cls;
struct cobj_class *carray_cls;

struct smemb {
  val mname;
  val mtype;
  struct txr_ffi_type *mtft;
  cnum offs;
};

enum char_conv {
  conv_none,
  conv_char,
  conv_zchar,
  conv_wchar,
  conv_bchar
};

struct txr_ffi_type {
  val self;
  ffi_kind_t kind;
  ffi_type *ft;
  ffi_type **elements;
  val lt;
  val syntax;
  val eltype;
  cnum size, align;
  unsigned shift, mask;
  cnum nelem;
  struct smemb *memb;
  val tag;
  val sym_num, num_sym;
  enum char_conv ch_conv : 3;
  unsigned null_term : 1;
  unsigned by_value_in : 1;
  unsigned incomplete : 1;
  unsigned flexible : 1;
  unsigned bitfield : 1;
  struct txr_ffi_type *(*clone)(struct txr_ffi_type *);
#if HAVE_LIBFFI
  void (*calcft)(struct txr_ffi_type *);
#endif
  void (*put)(struct txr_ffi_type *, val obj, mem_t *dst, val self);
  val (*get)(struct txr_ffi_type *, mem_t *src, val self);
  val (*in)(struct txr_ffi_type *, int copy, mem_t *src, val obj, val self);
  void (*out)(struct txr_ffi_type *, int copy, val obj, mem_t *dest, val self);
  void (*release)(struct txr_ffi_type *, val obj, mem_t *dst, val self);
  cnum (*dynsize)(struct txr_ffi_type *, val obj, val self);
  mem_t *(*alloc)(struct txr_ffi_type *, val obj, val self);
  void (*free)(void *);
#if !HAVE_LITTLE_ENDIAN
  void (*rput)(struct txr_ffi_type *, val obj, mem_t *dst, val self);
  val (*rget)(struct txr_ffi_type *, mem_t *src, val self);
#endif
};

static struct txr_ffi_type *ffi_type_struct(val obj)
{
  return coerce(struct txr_ffi_type *, obj->co.handle);
}

static struct txr_ffi_type *ffi_type_struct_checked(val self, val obj)
{
  return coerce(struct txr_ffi_type *, cobj_handle(self, obj, ffi_type_cls));
}

#if HAVE_LIBFFI
static ffi_type *ffi_get_type(val self, val obj)
{
  struct txr_ffi_type *tffi = ffi_type_struct_checked(self, obj);
  if (tffi->calcft != 0) {
    tffi->calcft(tffi);
    tffi->calcft = 0;
  }
  return tffi->ft;
}
#endif

static val ffi_get_lisp_type(val self, val obj)
{
  struct txr_ffi_type *tffi = ffi_type_struct_checked(self, obj);
  return tffi->lt;
}

static void ffi_type_print_op(val obj, val out, val pretty, struct strm_ctx *ctx)
{
  struct txr_ffi_type *tft = ffi_type_struct(obj);
  put_string(lit("#<"), out);
  obj_print_impl(obj->co.cls->cls_sym, out, pretty, ctx);
  format(out, lit(" ~!~s>"), tft->syntax, nao);
}

static void ffi_type_struct_destroy_op(val obj)
{
  struct txr_ffi_type *tft = ffi_type_struct(obj);

#if HAVE_LIBFFI
  free(tft->elements);
  tft->elements = 0;
  free(tft->ft);
  tft->ft = 0;
#endif

  free(tft->memb);
  tft->memb = 0;
  free(tft);
}

static void ffi_type_common_mark(struct txr_ffi_type *tft)
{
  gc_mark(tft->lt);
  gc_mark(tft->syntax);
  gc_mark(tft->tag);
}

static void ffi_type_mark(val obj)
{
  struct txr_ffi_type *tft = ffi_type_struct(obj);
  ffi_type_common_mark(tft);
}

static void ffi_struct_type_mark(val obj)
{
  struct txr_ffi_type *tft = ffi_type_struct(obj);
  cnum i;
  ffi_type_common_mark(tft);

  if (tft->eltype)
    gc_mark(tft->eltype);

  if (tft->memb != 0) {
    for (i = 0; i < tft->nelem; i++) {
      gc_mark(tft->memb[i].mname);
      gc_mark(tft->memb[i].mtype);
    }
  }
}

static void ffi_ptr_type_mark(val obj)
{
  struct txr_ffi_type *tft = ffi_type_struct(obj);
  ffi_type_common_mark(tft);
  gc_mark(tft->eltype);
}

static void ffi_enum_type_mark(val obj)
{
  struct txr_ffi_type *tft = ffi_type_struct(obj);
  ffi_type_common_mark(tft);
  gc_mark(tft->sym_num);
  gc_mark(tft->num_sym);
}

static struct cobj_ops ffi_type_builtin_ops =
  cobj_ops_init(eq,
                ffi_type_print_op,
                cobj_destroy_free_op,
                ffi_type_mark,
                cobj_eq_hash_op);

static struct cobj_ops ffi_type_struct_ops =
  cobj_ops_init(eq,
                ffi_type_print_op,
                ffi_type_struct_destroy_op,
                ffi_struct_type_mark,
                cobj_eq_hash_op);

static struct cobj_ops ffi_type_ptr_ops =
  cobj_ops_init(eq,
                ffi_type_print_op,
                cobj_destroy_free_op,
                ffi_ptr_type_mark,
                cobj_eq_hash_op);

static struct cobj_ops ffi_type_enum_ops =
  cobj_ops_init(eq,
                ffi_type_print_op,
                cobj_destroy_free_op,
                ffi_enum_type_mark,
                cobj_eq_hash_op);

#if HAVE_LIBFFI

struct txr_ffi_closure {
  ffi_closure *clo;
  mem_t *fptr;
  cnum nparam;
  val fun;
  val call_desc;
  val abort_retval;
  struct txr_ffi_call_desc *tfcd;
};

static struct txr_ffi_closure *ffi_closure_struct(val obj)
{
  return coerce(struct txr_ffi_closure *, obj->co.handle);
}

static struct txr_ffi_closure *ffi_closure_struct_checked(val self, val obj)
{
  return coerce(struct txr_ffi_closure *, cobj_handle(self, obj,
                                                      ffi_closure_cls));
}

static void ffi_closure_print_op(val obj, val out,
                                 val pretty, struct strm_ctx *ctx)
{
  struct txr_ffi_closure *tfcl = ffi_closure_struct(obj);
  put_string(lit("#<"), out);
  obj_print_impl(obj->co.cls->cls_sym, out, pretty, ctx);
  format(out, lit(" ~s ~s>"), tfcl->fun, tfcl->call_desc, nao);
}

static void ffi_closure_destroy_op(val obj)
{
  struct txr_ffi_closure *tfcl = ffi_closure_struct(obj);
  if (tfcl->clo != 0) {
    ffi_closure_free(tfcl->clo);
    tfcl->clo = 0;
    tfcl->fptr = 0;
  }
  free(tfcl);
}

static void ffi_closure_mark_op(val obj)
{
  struct txr_ffi_closure *tfcl = ffi_closure_struct(obj);
  gc_mark(tfcl->fun);
  gc_mark(tfcl->call_desc);
  gc_mark(tfcl->abort_retval);
}

static struct cobj_ops ffi_closure_ops =
  cobj_ops_init(eq,
                ffi_closure_print_op,
                ffi_closure_destroy_op,
                ffi_closure_mark_op,
                cobj_eq_hash_op);

#endif

static void ffi_void_put(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  (void) tft;
  (void) n;
  (void) dst;
  (void) self;
}

static cnum ffi_fixed_dynsize(struct txr_ffi_type *tft, val obj, val self)
{
  (void) tft;
  (void) obj;
  (void) self;
  return tft->size;
}

static mem_t *ffi_fixed_alloc(struct txr_ffi_type *tft, val obj, val self)
{
  (void) tft;
  (void) obj;
  (void) self;
  return chk_calloc(1, tft->size);
}

static cnum ffi_varray_dynsize(struct txr_ffi_type *tft, val obj, val self)
{
  switch (tft->ch_conv) {
  case conv_char:
  case conv_zchar:
    return utf8_to_buf(0, c_str(obj, self), tft->null_term);
  case conv_wchar:
  case conv_bchar:
  case conv_none:
  default:
    {
      cnum len = c_num(length(obj), self) + tft->null_term;
      val eltype = tft->eltype;
      struct txr_ffi_type *etft = ffi_type_struct(eltype);
      if (etft->incomplete)
        uw_throwf(error_s, lit("~a: incomplete type array element"), self, nao);
      if (INT_PTR_MAX / etft->size < len)
        uw_throwf(error_s, lit("~a: array too large"), self,  nao);
      return len * etft->size;
    }
  }
}

static mem_t *ffi_varray_alloc(struct txr_ffi_type *tft, val obj, val self)
{
  cnum dynsize = ffi_varray_dynsize(tft, obj, self);
  size_t size = dynsize;
  if (convert(cnum, size) != dynsize)
    uw_throwf(error_s, lit("~a: array too large"), self,  nao);
  return chk_calloc(size, 1);
}

static cnum ffi_flex_dynsize(struct txr_ffi_type *tft, val obj, val self)
{
  struct smemb *lastm = &tft->memb[tft->nelem - 1];
  struct txr_ffi_type *ltft = lastm->mtft;
  val lobj = slot(obj, lastm->mname);
  cnum lmds = ltft->dynsize(ltft, lobj, self);

  if (lastm->offs > INT_PTR_MAX - lmds)
    uw_throwf(error_s, lit("~a: flexible struct size overflow"), self, nao);

  return lastm->offs + lmds;
}

static mem_t *ffi_flex_alloc(struct txr_ffi_type *tft, val obj, val self)
{
  return chk_calloc(1, ffi_flex_dynsize(tft, obj, self));
}

static void ffi_noop_free(void *ptr)
{
  (void) ptr;
}

static val ffi_void_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  (void) tft;
  (void) src;
  (void) self;
  return nil;
}

static void ffi_simple_release(struct txr_ffi_type *tft, val obj,
                               mem_t *dst, val self)
{
  mem_t **loc = coerce(mem_t **, dst);
  (void) tft;
  (void) obj;
  (void) self;
  free(*loc);
  *loc = 0;
}

#if __i386__ || __x86_64__ || __PPC64__ || __ARM_FEATURE_UNALIGNED

#define align_sw_get(type, src) enum { dummy ## __LINE__ }
#define align_sw_end
#define align_sw_put_end
#define align_sw_put(type, dst, expr) (expr)

#else

#define align_sw_get(type, src)  {                                      \
  const int al = ((alignof (type) - 1) & coerce(uint_ptr_t, src)) == 0; \
  const size_t sz = sizeof (type);                                      \
  mem_t *src_prev = src;                                                \
  mem_t *buf = al ? src : convert(mem_t *, alloca(sz));                 \
  mem_t *src = al ? buf : (memcpy(buf, src_prev, sz), buf)

#define align_sw_end                                                    \
}

#define align_sw_put_end                                                \
  if (al)                                                               \
    memcpy(src_prev, buf, sz);                                          \
}

#define align_sw_put(type, dst, expr) {                                 \
  if (((alignof (type) - 1) & coerce(uint_ptr_t, dst)) == 0) {          \
    expr;                                                               \
  } else {                                                              \
    mem_t *prev_dst = dst;                                              \
    mem_t *dst = convert(mem_t *, alloca(sizeof (type)));               \
    expr;                                                               \
    memcpy(prev_dst, dst, sizeof (type));                               \
  }                                                                     \
}

#endif

#if HAVE_I8
static void ffi_i8_put(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  i8_t v = c_i8(n, self);
  (void) tft;
  *coerce(i8_t *, dst) = v;
}

static val ffi_i8_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  (void) tft;
  (void) self;
  return num_fast(*src);
}

static void ffi_u8_put(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  u8_t v = c_u8(n, self);
  (void) tft;
  *coerce(u8_t *, dst) = v;
}

static val ffi_u8_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  (void) tft;
  (void) self;
  return num_fast(*coerce(u8_t *, src));
}

#endif

#if HAVE_I16
static void ffi_i16_put(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  i16_t v = c_i16(n, self);
  (void) tft;
  align_sw_put(i16_t, dst, *coerce(i16_t *, dst) = v);
}

static val ffi_i16_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  align_sw_get(i16_t, src);
  i16_t n = *coerce(i16_t *, src);
  (void) tft;
  (void) self;
  return num_fast(n);
  align_sw_end;
}

static void ffi_u16_put(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  u16_t v = c_u16(n, self);
  (void) tft;
  align_sw_put(u16_t, dst, *coerce(u16_t *, dst) = v);
}

static val ffi_u16_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  align_sw_get(u16_t, src);
  u16_t n = *coerce(u16_t *, src);
  (void) tft;
  (void) self;
  return num_fast(n);
  align_sw_end;
}
#endif

#if HAVE_I32
static void ffi_i32_put(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  i32_t v = c_i32(n, self);
  (void) tft;
  align_sw_put(i32_t, dst, *coerce(i32_t *, dst) = v);
}

static val ffi_i32_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  align_sw_get(i32_t, src);
  i32_t n = *coerce(i32_t *, src);
  (void) tft;
  (void) self;
  return num(n);
  align_sw_end;
}

static void ffi_u32_put(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  u32_t v = c_u32(n, self);
  (void) tft;
  align_sw_put(u32_t, dst, *coerce(u32_t *, dst) = v);
}

static val ffi_u32_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  align_sw_get(u32_t, src);
  u32_t n = *coerce(u32_t *, src);
  (void) tft;
  (void) self;
  return unum(n);
  align_sw_end;
}
#endif

#if HAVE_I64
static void ffi_i64_put(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  i64_t v = c_i64(n, self);
  (void) tft;
  align_sw_put(i64_t, dst, *coerce(i64_t *, dst) = v);
}

static val ffi_i64_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  align_sw_get(i64_t, src);
  i64_t n = *coerce(i64_t *, src);

  (void) tft;
  (void) self;

  if (sizeof (i64_t) <= sizeof (cnum)) {
    return num(n);
  } else {
    val high = num(n >> 32);
    val low = unum(n & 0xFFFFFFFF);
    return logior(ash(high, num_fast(32)), low);
  }
  align_sw_end;
}

static void ffi_u64_put(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  u64_t v = c_u64(n, self);
  (void) tft;
  align_sw_put(u64_t, dst, *coerce(u64_t *, dst) = v);
}

static val ffi_u64_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  align_sw_get(u64_t, src);
  u64_t n = *coerce(u64_t *, src);

  (void) tft;
  (void) self;

  if (sizeof (u64_t) <= sizeof (uint_ptr_t)) {
    return unum(n);
  } else {
    val high = unum(n >> 32);
    val low = unum(n & 0xFFFFFFFF);
    return logior(ash(high, num_fast(32)), low);
  }
  align_sw_end;
}

#endif

static void ffi_char_put(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  char v = c_char(n, self);
  (void) tft;
  *coerce(char *, dst) = v;
}

static val ffi_char_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  (void) tft;
  (void) self;
  return chr(*coerce(char *, src));
}

static void ffi_uchar_put(struct txr_ffi_type *tft, val n, mem_t *dst,
                          val self)
{
  unsigned char v = c_uchar(n, self);
  (void) tft;
  *coerce(unsigned char *, dst) = v;
}

static val ffi_uchar_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  (void) tft;
  (void) self;
  return num_fast(*src);
}

static val ffi_bchar_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  (void) tft;
  (void) self;
  return chr(*src);
}

static void ffi_short_put(struct txr_ffi_type *tft, val n, mem_t *dst,
                          val self)
{
  short v = c_short(n, self);
  (void) tft;
  align_sw_put(short, dst, *coerce(short *, dst) = v);
}

static val ffi_short_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  align_sw_get(short, src);
  short n = *coerce(short *, src);
  (void) tft;
  (void) self;
  return num_fast(n);
  align_sw_end;
}

static void ffi_ushort_put(struct txr_ffi_type *tft, val n, mem_t *dst,
                           val self)
{
  unsigned short v = c_ushort(n, self);
  (void) tft;
  align_sw_put(unsigned, dst, *coerce(unsigned short *, dst) = v);
}

static val ffi_ushort_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  align_sw_get(unsigned short, src);
  unsigned short n = *coerce(unsigned short *, src);
  (void) tft;
  (void) self;
  return num_fast(n);
  align_sw_end;
}

static void ffi_int_put(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  int v = c_int(n, self);
  (void) tft;
  align_sw_put(int, dst, *coerce(int *, dst) = v);
}

static val ffi_int_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  align_sw_get(int, src);
  int n = *coerce(int *, src);
  (void) tft;
  (void) self;
  return num(n);
  align_sw_end;
}

static void ffi_uint_put(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  unsigned v = c_uint(n, self);
  (void) tft;
  align_sw_put(unsigned, dst, *coerce(unsigned *, dst) = v);
}

static val ffi_uint_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  align_sw_get(unsigned, src);
  unsigned n = *coerce(unsigned *, src);
  (void) tft;
  (void) self;
  return unum(n);
  align_sw_end;
}

static void ffi_long_put(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  long v = c_long(n, self);
  (void) tft;
  align_sw_put(long, dst, *coerce(long *, dst) = v);
}

static val ffi_long_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  align_sw_get(long, src);
  long n = *coerce(long *, src);
  (void) tft;
  (void) self;
  return num(n);
  align_sw_end;
}

static void ffi_ulong_put(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  unsigned long v = c_ulong(n, self);
  (void) tft;
  align_sw_put(unsigned long, dst, *coerce(unsigned long *, dst) = v);
}

static val ffi_ulong_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  align_sw_get(unsigned long, src);
  unsigned long n = *coerce(unsigned long *, src);
  (void) tft;
  (void) self;
  return unum(n);
  align_sw_end;
}

static void ffi_float_put(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  double v;

  (void) tft;

  switch (type(n)) {
  case NUM:
  case CHR:
    v = c_num(n, self);
    break;
  case BGNUM:
    n = int_flo(n);
    /* fallthrough */
  default:
    v = c_flo(n, self);
    break;
  }

  {
    double pv = fabs(v);
    if (pv > FLT_MAX || (pv != 0.0 && pv < FLT_MIN))
    uw_throwf(error_s, lit("~a: ~s is out of float range"), self, n, nao);
  }

  align_sw_put(double, dst, *coerce(float *, dst) = v);
}

static val ffi_float_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  align_sw_get(float, src);
  float n = *coerce(float *, src);
  (void) tft;
  (void) self;
  return flo(n);
  align_sw_end;
}

static void ffi_double_put(struct txr_ffi_type *tft, val n, mem_t *dst,
                           val self)
{
  double v;

  (void) tft;

  switch (type(n)) {
  case NUM:
  case CHR:
    v = c_num(n, self);
    break;
  case BGNUM:
    n = int_flo(n);
    /* fallthrough */
  default:
    v = c_flo(n, self);
    break;
  }

  align_sw_put(double, dst, *coerce(double *, dst) = v);
}

static val ffi_double_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  align_sw_get(double, src);
  double n = *coerce(double *, src);
  (void) tft;
  (void) self;
  return flo(n);
  align_sw_end;
}

static void ffi_val_put(struct txr_ffi_type *tft, val v, mem_t *dst, val self)
{
  (void) tft;
  (void) self;
  align_sw_put(val *, dst, *coerce(val *, dst) = v);
}

static val ffi_val_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  align_sw_get(val, src);
  val v = *coerce(val *, src);
  (void) tft;
  if (!valid_object_p(v))
    uw_throwf(error_s, lit("~a: bit pattern ~0,0*x isn't a valid Lisp object"),
              self, num_fast(sizeof (v) * 2), bits(v), nao);
  return v;
  align_sw_end;
}

static u16_t ffi_swap_u16(u16_t n)
{
  return convert(u16_t, n << 8 | n >> 8);
}

static u32_t ffi_swap_u32(u32_t n)
{
  n = (n & 0xFF00FF00U) >> 8 | (n & 0x00FF00FF) << 8;
  return n << 16 | n >> 16;
}

static u64_t ffi_swap_u64(u64_t n)
{
  n = (n & 0xFF00FF00FF00FF00U) >>  8 | (n & 0x00FF00FF00FF00FF) <<  8;
  n = (n & 0xFFFF0000FFFF0000U) >> 16 | (n & 0x0000FFFF0000FFFF) << 16;
  return n << 32 | n >> 32;
}

#if HAVE_I16
static i16_t ffi_swap_i16(i16_t n)
{
  return convert(i16_t, ffi_swap_u16(convert(u16_t, n)));
}
#endif

#if HAVE_I32
static i32_t ffi_swap_i32(i32_t n)
{
  return convert(i32_t, ffi_swap_u32(convert(u32_t, n)));
}
#endif

#if HAVE_I64
static i64_t ffi_swap_i64(i64_t n)
{
  return convert(i64_t, ffi_swap_u64(convert(u64_t, n)));
}
#endif

#if HAVE_I16
static void ffi_swap_i16_put(struct txr_ffi_type *tft, val n,
                             mem_t *dst, val self)
{
  i16_t v = ffi_swap_i16(c_i16(n, self));
  (void) tft;
  align_sw_put(i16_t, dst, *coerce(i16_t *, dst) = v);
}


static val ffi_swap_i16_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  align_sw_get(i16_t, src);
  i16_t n = ffi_swap_i16(*coerce(i16_t *, src));
  (void) tft;
  (void) self;
  return num_fast(n);
  align_sw_end;
}

static void ffi_swap_u16_put(struct txr_ffi_type *tft, val n,
                             mem_t *dst, val self)
{
  u16_t v = ffi_swap_u16(c_u16(n, self));
  (void) tft;
  align_sw_put(u16_t, dst, *coerce(u16_t *, dst) = v);
}

static val ffi_swap_u16_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  align_sw_get(u16_t, src);
  u16_t n = ffi_swap_u16(*coerce(u16_t *, src));
  (void) tft;
  (void) self;
  return num_fast(n);
  align_sw_end;
}
#endif

#if HAVE_I32
static void ffi_swap_i32_put(struct txr_ffi_type *tft, val n,
                             mem_t *dst, val self)
{
  i32_t v = ffi_swap_i32(c_i32(n, self));
  (void) tft;
  align_sw_put(i32_t, dst, *coerce(i32_t *, dst) = v);
}

static val ffi_swap_i32_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  align_sw_get(i32_t, src);
  i32_t n = ffi_swap_i32(*coerce(i32_t *, src));
  (void) tft;
  (void) self;
  return num(n);
  align_sw_end;
}

static void ffi_swap_u32_put(struct txr_ffi_type *tft, val n,
                             mem_t *dst, val self)
{
  u32_t v = ffi_swap_u32(c_u32(n, self));
  (void) tft;
  align_sw_put(u32_t, dst, *coerce(u32_t *, dst) = v);
}

static val ffi_swap_u32_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  align_sw_get(u32_t, src);
  u32_t n = ffi_swap_u32(*coerce(u32_t *, src));
  (void) tft;
  (void) self;
  return unum(n);
  align_sw_end;
}
#endif

#if HAVE_I64
static void ffi_swap_i64_put(struct txr_ffi_type *tft, val n,
                             mem_t *dst, val self)
{
  i64_t v = ffi_swap_i64(c_i64(n, self));
  (void) tft;
  align_sw_put(i64_t, dst, *coerce(i64_t *, dst) = v);
}

static val ffi_swap_i64_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  align_sw_get(i64_t, src);
  i64_t n = ffi_swap_i64(*coerce(i64_t *, src));

  (void) tft;
  (void) self;

  if (sizeof (i64_t) <= sizeof (cnum)) {
    return num(n);
  } else {
    val high = num(n >> 32);
    val low = unum(n & 0xFFFFFFFF);
    return logior(ash(high, num_fast(32)), low);
  }
  align_sw_end;
}

static void ffi_swap_u64_put(struct txr_ffi_type *tft, val n,
                             mem_t *dst, val self)
{
  u64_t v = ffi_swap_u64(c_u64(n, self));
  (void) tft;
  align_sw_put(u64_t, dst, *coerce(u64_t *, dst) = v);
}

static val ffi_swap_u64_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  align_sw_get(u64_t, src);
  u64_t n = ffi_swap_u64(*coerce(u64_t *, src));

  (void) tft;
  (void) self;

  if (sizeof (u64_t) <= sizeof (uint_ptr_t)) {
    return unum(n);
  } else {
    val high = unum(n >> 32);
    val low = unum(n & 0xFFFFFFFF);
    return logior(ash(high, num_fast(32)), low);
  }
  align_sw_end;
}
#endif

#if HAVE_LITTLE_ENDIAN

#define ffi_be_i16_put ffi_swap_i16_put
#define ffi_be_i16_get ffi_swap_i16_get
#define ffi_be_u16_put ffi_swap_u16_put
#define ffi_be_u16_get ffi_swap_u16_get

#define ffi_be_i32_put ffi_swap_i32_put
#define ffi_be_i32_get ffi_swap_i32_get
#define ffi_be_u32_put ffi_swap_u32_put
#define ffi_be_u32_get ffi_swap_u32_get

#define ffi_be_i64_put ffi_swap_i64_put
#define ffi_be_i64_get ffi_swap_i64_get
#define ffi_be_u64_put ffi_swap_u64_put
#define ffi_be_u64_get ffi_swap_u64_get

#define ffi_le_i16_put ffi_i16_put
#define ffi_le_i16_get ffi_i16_get
#define ffi_le_u16_put ffi_u16_put
#define ffi_le_u16_get ffi_u16_get

#define ffi_le_i32_put ffi_i32_put
#define ffi_le_i32_get ffi_i32_get
#define ffi_le_u32_put ffi_u32_put
#define ffi_le_u32_get ffi_u32_get

#define ffi_le_i64_put ffi_i64_put
#define ffi_le_i64_get ffi_i64_get
#define ffi_le_u64_put ffi_u64_put
#define ffi_le_u64_get ffi_u64_get

#else

#define ffi_be_i16_put ffi_i16_put
#define ffi_be_i16_get ffi_i16_get
#define ffi_be_u16_put ffi_u16_put
#define ffi_be_u16_get ffi_u16_get

#define ffi_be_i32_put ffi_i32_put
#define ffi_be_i32_get ffi_i32_get
#define ffi_be_u32_put ffi_u32_put
#define ffi_be_u32_get ffi_u32_get

#define ffi_be_i64_put ffi_i64_put
#define ffi_be_i64_get ffi_i64_get
#define ffi_be_u64_put ffi_u64_put
#define ffi_be_u64_get ffi_u64_get

#define ffi_le_i16_put ffi_swap_i16_put
#define ffi_le_i16_get ffi_swap_i16_get
#define ffi_le_u16_put ffi_swap_u16_put
#define ffi_le_u16_get ffi_swap_u16_get

#define ffi_le_i32_put ffi_swap_i32_put
#define ffi_le_i32_get ffi_swap_i32_get
#define ffi_le_u32_put ffi_swap_u32_put
#define ffi_le_u32_get ffi_swap_u32_get

#define ffi_le_i64_put ffi_swap_i64_put
#define ffi_le_i64_get ffi_swap_i64_get
#define ffi_le_u64_put ffi_swap_u64_put
#define ffi_le_u64_get ffi_swap_u64_get

#endif

static void ffi_be_float_put(struct txr_ffi_type *tft, val n,
                             mem_t *dst, val self)
{
#if HAVE_LITTLE_ENDIAN
  mem_t *c = convert(mem_t *, alloca(4));
  ffi_float_put(tft, n, c, self);

  dst[0] = c[3];
  dst[1] = c[2];
  dst[2] = c[1];
  dst[3] = c[0];
#else
  ffi_float_put(tft, n, dst, self);
#endif
}

static val ffi_be_float_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
#if HAVE_LITTLE_ENDIAN
  mem_t *c = convert(mem_t *, alloca(4));

  c[0] = src[3];
  c[1] = src[2];
  c[2] = src[1];
  c[3] = src[0];

  return ffi_float_get(tft, c, self);
#else
  return ffi_float_get(tft, src, self);
#endif
}

static void ffi_le_float_put(struct txr_ffi_type *tft, val n,
                             mem_t *dst, val self)
{
#if !HAVE_LITTLE_ENDIAN
  mem_t *c = convert(mem_t *, alloca(4));

  ffi_float_put(tft, n, c, self);

  dst[0] = c[3];
  dst[1] = c[2];
  dst[2] = c[1];
  dst[3] = c[0];
#else
  ffi_float_put(tft, n, dst, self);
#endif
}

static val ffi_le_float_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
#if !HAVE_LITTLE_ENDIAN
  mem_t *c = convert(mem_t *, alloca(4));

  c[0] = src[3];
  c[1] = src[2];
  c[2] = src[1];
  c[3] = src[0];

  return ffi_float_get(tft, c, self);
#else
  return ffi_float_get(tft, src, self);
#endif
}

static void ffi_be_double_put(struct txr_ffi_type *tft, val n,
                             mem_t *dst, val self)
{
#if HAVE_LITTLE_ENDIAN
  mem_t *c = convert(mem_t *, alloca(8));

  ffi_double_put(tft, n, c, self);

  dst[0] = c[7];
  dst[1] = c[6];
  dst[2] = c[5];
  dst[3] = c[4];
  dst[4] = c[3];
  dst[5] = c[2];
  dst[6] = c[1];
  dst[7] = c[0];
#else
  ffi_double_put(tft, n, dst, self);
#endif
}

static val ffi_be_double_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
#if HAVE_LITTLE_ENDIAN
  mem_t *c = convert(mem_t *, alloca(8));

  c[0] = src[7];
  c[1] = src[6];
  c[2] = src[5];
  c[3] = src[4];
  c[4] = src[3];
  c[5] = src[2];
  c[6] = src[1];
  c[7] = src[0];

  return ffi_double_get(tft, c, self);
#else
  return ffi_double_get(tft, src, self);
#endif
}

static void ffi_le_double_put(struct txr_ffi_type *tft, val n,
                              mem_t *dst, val self)
{
#if !HAVE_LITTLE_ENDIAN
  mem_t *c = convert(mem_t *, alloca(8));

  ffi_double_put(tft, n, c, self);

  dst[0] = c[7];
  dst[1] = c[6];
  dst[2] = c[5];
  dst[3] = c[4];
  dst[4] = c[3];
  dst[5] = c[2];
  dst[6] = c[1];
  dst[7] = c[0];
#else
  ffi_double_put(tft, n, dst, self);
#endif
}

static val ffi_le_double_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
#if !HAVE_LITTLE_ENDIAN
  mem_t *c = convert(mem_t *, alloca(8));

  c[0] = src[7];
  c[1] = src[6];
  c[2] = src[5];
  c[3] = src[4];
  c[4] = src[3];
  c[5] = src[2];
  c[6] = src[1];
  c[7] = src[0];

  return ffi_double_get(tft, c, self);
#else
  return ffi_double_get(tft, src, self);
#endif
}

#if SIZEOF_WCHAR_T == SIZEOF_SHORT
#define ffi_type_wchar ffi_type_ushort
#elif SIZEOF_WCHAR_T == SIZEOF_INT
#define ffi_type_wchar ffi_type_uint
#elif SIZEOF_WCHAR_T == SIZEOF_LONG
#define ffi_type_wchar ffi_type_long
#else
#error portme
#endif

static void ffi_wchar_put(struct txr_ffi_type *tft, val ch, mem_t *dst,
                          val self)
{
  wchar_t c = c_chr(ch);
  (void) tft;
  (void) self;
  align_sw_put(wchar_t, dst, *coerce(wchar_t *, dst) = c);
}

static val ffi_wchar_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  align_sw_get(wchar_t, src);
  wchar_t c = *coerce(wchar_t *, src);
  (void) tft;
  (void) self;
  return chr(c);
  align_sw_end;
}

static void ffi_sbit_put(struct txr_ffi_type *tft, val n,
                         mem_t *dst, val self)
{
  unsigned mask = tft->mask;
  unsigned sbmask = mask ^ (mask >> 1);
  int shift = tft->shift;
  cnum cn = c_num(n, self);
  int in = cn;
  unsigned uput = (convert(unsigned, in) << shift) & mask;

  if (in != cn)
    goto range;

  if (uput & sbmask) {
    int icheck = -convert(int, ((uput ^ mask) >> shift) + 1);
    if (icheck != cn)
      goto range;
  } else if (convert(cnum, uput >> shift) != cn) {
    goto range;
  }

  {
    align_sw_get(unsigned, dst);
    unsigned field = *coerce(unsigned *, dst);
    field &= ~mask;
    field |= uput;
    *coerce(unsigned *, dst) = field;
    align_sw_put_end;
  }

  return;
range:
  uw_throwf(error_s, lit("~a: value ~s is out of range of "
                         "signed ~s bit-field"),
            self, n, num_fast(tft->nelem), nao);
}

static val ffi_sbit_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  align_sw_get(unsigned int, src);
  unsigned mask = tft->mask;
  unsigned sbmask = mask ^ (mask >> 1);
  int shift = tft->shift;
  unsigned uget = *coerce(unsigned *, src) & mask;

  (void) self;

  if (uget & sbmask)
    return num(-convert(int, ((uget ^ mask) >> shift) + 1));
  return unum(uget >> shift);
  align_sw_end;
}

static void ffi_ubit_put(struct txr_ffi_type *tft, val n,
                         mem_t *dst, val self)
{
  unsigned mask = tft->mask;
  int shift = tft->shift;
  ucnum cn = c_unum(n, self);
  unsigned un = cn;
  unsigned uput = (un << shift) & mask;

  if (un != cn)
    goto range;

  if (uput >> shift != cn)
    goto range;

  {
    align_sw_get(unsigned, dst);
    unsigned field = *coerce(unsigned *, dst);
    field &= ~mask;
    field |= uput;
    *coerce(unsigned *, dst) = field;
    align_sw_put_end;
  }

  return;

range:
  uw_throwf(error_s, lit("~a: value ~s is out of range of "
                         "unsigned ~s bit-field"),
            self, n, num_fast(tft->nelem), nao);
}

static val ffi_ubit_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  align_sw_get(unsigned, src);
  unsigned mask = tft->mask;
  int shift = tft->shift;
  unsigned uget = *coerce(unsigned *, src) & mask;
  (void) self;
  return unum(uget >> shift);
  align_sw_end;
}

static void ffi_generic_sbit_put(struct txr_ffi_type *tft, val n,
                                 mem_t *dst, val self)
{
  mem_t *tmp = coerce(mem_t *, zalloca(sizeof (int)));
  memcpy(tmp, dst, tft->size);
  ffi_sbit_put(tft, n, tmp, self);
  memcpy(dst, tmp, tft->size);
}

static val ffi_generic_sbit_get(struct txr_ffi_type *tft,
                                mem_t *src, val self)
{
  mem_t *tmp = coerce(mem_t *, zalloca(sizeof (int)));
  memcpy(tmp, src, tft->size);
  return ffi_sbit_get(tft, tmp, self);
}

static void ffi_generic_ubit_put(struct txr_ffi_type *tft, val n,
                                 mem_t *dst, val self)
{
  mem_t *tmp = coerce(mem_t *, zalloca(sizeof (int)));
  memcpy(tmp, dst, tft->size);
  ffi_ubit_put(tft, n, tmp, self);
  memcpy(dst, tmp, tft->size);
}

static val ffi_generic_ubit_get(struct txr_ffi_type *tft,
                                mem_t *src, val self)
{
  mem_t *tmp = coerce(mem_t *, zalloca(sizeof (int)));
  memcpy(tmp, src, tft->size);
  return ffi_ubit_get(tft, tmp, self);
}

static void ffi_bool_put(struct txr_ffi_type *tft, val truth,
                         mem_t *dst, val self)
{
  val n = truth ? one : zero;
  struct txr_ffi_type *tgtft = ffi_type_struct(tft->eltype);
  tgtft->put(tft, n, dst, self); /* tft deliberate */
}

static val ffi_bool_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  struct txr_ffi_type *tgtft = ffi_type_struct(tft->eltype);
  val n = tgtft->get(tft, src, self); /* tft deliberate */
  return null(zerop(n));
}

#if !HAVE_LITTLE_ENDIAN

static void ffi_i8_rput(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  i8_t v = c_i8(n, self);
  (void) tft;
  *coerce(i8_t *, dst) = v;
}

static val ffi_i8_rget(struct txr_ffi_type *tft, mem_t *src, val self)
{
  (void) tft;
  (void) self;
  return num_fast(*src);
}

static void ffi_u8_rput(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  u8_t v = c_u8(n, self);
  (void) tft;
  (void) self;
  *coerce(u8_t *, dst) = v;
}

static val ffi_u8_rget(struct txr_ffi_type *tft, mem_t *src, val self)
{
  (void) tft;
  (void) self;
  return num_fast(*coerce(u8_t *, src));
}

#if HAVE_I16
static void ffi_i16_rput(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  i16_t v = c_i16(n, self);
  (void) tft;
  (void) self;
  *coerce(ffi_arg *, dst) = v;
}

static val ffi_i16_rget(struct txr_ffi_type *tft, mem_t *src, val self)
{
  i16_t n = *coerce(ffi_arg *, src);
  (void) tft;
  (void) self;
  return num_fast(n);
}

static void ffi_u16_rput(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  u16_t v = c_u16(n, self);
  (void) tft;
  (void) self;
  *coerce(ffi_arg *, dst) = v;
}

static val ffi_u16_rget(struct txr_ffi_type *tft, mem_t *src, val self)
{
  u16_t n = *coerce(ffi_arg *, src);
  (void) tft;
  (void) self;
  return num_fast(n);
}
#endif

#if HAVE_I32
static void ffi_i32_rput(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  i32_t v = c_i32(n, self);
  (void) tft;
  (void) self;
  *coerce(ffi_arg *, dst) = v;
}

static val ffi_i32_rget(struct txr_ffi_type *tft, mem_t *src, val self)
{
  i32_t n = *coerce(ffi_arg *, src);
  (void) tft;
  (void) self;
  return num(n);
}
#endif

static void ffi_u32_rput(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  u32_t v = c_u32(n, self);
  (void) tft;
  (void) self;
  *coerce(ffi_arg *, dst) = v;
}

static val ffi_u32_rget(struct txr_ffi_type *tft, mem_t *src, val self)
{
  u32_t n = *coerce(ffi_arg *, src);
  (void) tft;
  (void) self;
  return unum(n);
}

static void ffi_char_rput(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  char v = c_char(n, self);
  (void) tft;
  *coerce(ffi_arg *, dst) = v;
}

static val ffi_char_rget(struct txr_ffi_type *tft, mem_t *src, val self)
{
  (void) tft;
  (void) self;
  return chr((char) *coerce(ffi_arg *, src));
}

static void ffi_uchar_rput(struct txr_ffi_type *tft, val n, mem_t *dst,
                          val self)
{
  unsigned char v = c_uchar(n, self);
  (void) tft;
  *coerce(ffi_arg *, dst) = v;
}

static val ffi_uchar_rget(struct txr_ffi_type *tft, mem_t *src, val self)
{
  (void) tft;
  (void) self;
  return num_fast((unsigned char) *coerce(ffi_arg *, src));
}

static val ffi_bchar_rget(struct txr_ffi_type *tft, mem_t *src, val self)
{
  (void) tft;
  (void) self;
  return chr((unsigned char) *coerce(ffi_arg *, src));
}

static void ffi_short_rput(struct txr_ffi_type *tft, val n, mem_t *dst,
                          val self)
{
  short v = c_short(n, self);
  (void) tft;
  *coerce(ffi_arg *, dst) = v;
}

static val ffi_short_rget(struct txr_ffi_type *tft, mem_t *src, val self)
{
  short n = *coerce(ffi_arg *, src);
  (void) tft;
  (void) self;
  return num_fast(n);
}

static void ffi_ushort_rput(struct txr_ffi_type *tft, val n, mem_t *dst,
                           val self)
{
  unsigned short v = c_ushort(n, self);
  (void) tft;
  *coerce(ffi_arg *, dst) = v;
}

static val ffi_ushort_rget(struct txr_ffi_type *tft, mem_t *src, val self)
{
  unsigned short n = *coerce(ffi_arg *, src);
  (void) tft;
  (void) self;
  return num_fast(n);
}

static void ffi_int_rput(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  int v = c_int(n, self);
  (void) tft;
  *coerce(ffi_arg *, dst) = v;
}

static val ffi_int_rget(struct txr_ffi_type *tft, mem_t *src, val self)
{
  int n = *coerce(ffi_arg *, src);
  (void) tft;
  (void) self;
  return num(n);
}

static void ffi_uint_rput(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  unsigned v = c_uint(n, self);
  (void) tft;
  *coerce(ffi_arg *, dst) = v;
}

static val ffi_uint_rget(struct txr_ffi_type *tft, mem_t *src, val self)
{
  unsigned n = *coerce(ffi_arg *, src);
  (void) tft;
  (void) self;
  return unum(n);
}

static void ffi_long_rput(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  long v = c_long(n, self);
  (void) tft;
  (void) self;
  *coerce(ffi_arg *, dst) = v;
}

static val ffi_long_rget(struct txr_ffi_type *tft, mem_t *src, val self)
{
  long n = *coerce(ffi_arg *, src);
  (void) tft;
  (void) self;
  return num(n);
}

static void ffi_ulong_rput(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  unsigned long v = c_ulong(n, self);
  (void) tft;
  *coerce(ffi_arg *, dst) = v;
}

static val ffi_ulong_rget(struct txr_ffi_type *tft, mem_t *src, val self)
{
  unsigned long n = *coerce(ffi_arg *, src);
  (void) tft;
  (void) self;
  return unum(n);
}

static void ffi_wchar_rput(struct txr_ffi_type *tft, val ch, mem_t *dst,
                           val self)
{
  wchar_t c = c_chr(ch);
  (void) tft;
  (void) self;
  *coerce(ffi_arg *, dst) = c;
}

static val ffi_wchar_rget(struct txr_ffi_type *tft, mem_t *src, val self)
{
  wchar_t c = *coerce(ffi_arg *, src);
  (void) tft;
  (void) self;
  return chr(c);
}

#if HAVE_I16

static void ffi_be_i16_rput(struct txr_ffi_type *tft, val n, mem_t *dst,
                            val self)
{
  (void) tft;
  ffi_be_i16_put(tft, n, dst + 6, self);
}

static val ffi_be_i16_rget(struct txr_ffi_type *tft, mem_t *src, val self)
{
  return ffi_be_i16_get(tft, src + 6, self);
}

static void ffi_be_u16_rput(struct txr_ffi_type *tft, val n, mem_t *dst,
                            val self)
{
  memset(dst, 0, 6);
  ffi_be_u16_put(tft, n, dst + 6, self);
}

static val ffi_be_u16_rget(struct txr_ffi_type *tft, mem_t *src, val self)
{
  return ffi_be_u16_get(tft, src + 6, self);
}

#endif

#if HAVE_I32

static void ffi_be_i32_rput(struct txr_ffi_type *tft, val n, mem_t *dst,
                            val self)
{
  memset(dst, 0, 4);
  ffi_be_i32_put(tft, n, dst + 4, self);
}

static val ffi_be_i32_rget(struct txr_ffi_type *tft, mem_t *src, val self)
{
  return ffi_be_i32_get(tft, src + 4, self);
}

static void ffi_be_u32_rput(struct txr_ffi_type *tft, val n, mem_t *dst,
                            val self)
{
  memset(dst, 0, 4);
  ffi_be_u32_put(tft, n, dst + 4, self);
}

static val ffi_be_u32_rget(struct txr_ffi_type *tft, mem_t *src, val self)
{
  return ffi_be_u32_get(tft, src + 4, self);
}

#endif

#if HAVE_I16

static void ffi_le_i16_rput(struct txr_ffi_type *tft, val n, mem_t *dst,
                            val self)
{
  memset(dst, 0, 6);
  ffi_le_i16_put(tft, n, dst + 6, self);
}

static val ffi_le_i16_rget(struct txr_ffi_type *tft, mem_t *src, val self)
{
  return ffi_le_i16_get(tft, src + 6, self);
}

static void ffi_le_u16_rput(struct txr_ffi_type *tft, val n, mem_t *dst,
                            val self)
{
  memset(dst, 0, 6);
  ffi_le_u16_put(tft, n, dst + 6, self);
}

static val ffi_le_u16_rget(struct txr_ffi_type *tft, mem_t *src, val self)
{
  return ffi_le_u16_get(tft, src + 6, self);
}

#endif

#if HAVE_I32

static void ffi_le_i32_rput(struct txr_ffi_type *tft, val n, mem_t *dst,
                            val self)
{
  memset(dst, 0, 4);
  ffi_le_i32_put(tft, n, dst + 4, self);
}

static val ffi_le_i32_rget(struct txr_ffi_type *tft, mem_t *src, val self)
{
  return ffi_le_i32_get(tft, src + 4, self);
}

static void ffi_le_u32_rput(struct txr_ffi_type *tft, val n, mem_t *dst,
                            val self)
{
  memset(dst, 0, 4);
  ffi_le_u32_put(tft, n, dst + 4, self);
}

static val ffi_le_u32_rget(struct txr_ffi_type *tft, mem_t *src, val self)
{
  return ffi_le_u32_get(tft, src + 4, self);
}

#endif

static void ffi_bool_rput(struct txr_ffi_type *tft, val truth,
                          mem_t *dst, val self)
{
  val n = truth ? one : zero;
  struct txr_ffi_type *tgtft = ffi_type_struct(tft->eltype);
  tgtft->rput(tft, n, dst, self); /* tft deliberate */
}

static val ffi_bool_rget(struct txr_ffi_type *tft, mem_t *src, val self)
{
  struct txr_ffi_type *tgtft = ffi_type_struct(tft->eltype);
  val n = tgtft->rget(tft, src, self); /* tft deliberate */
  return null(zerop(n));
}

#endif

static void ffi_cptr_put(struct txr_ffi_type *tft, val ptr, mem_t *dst,
                         val self)
{
  mem_t *p = 0;

  if (type(ptr) == CPTR)
    p = cptr_handle(ptr, tft->tag, self);
  else
    p = carray_ptr(ptr, tft->eltype, self);

  *coerce(mem_t **, dst) = p;
}

static val ffi_cptr_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  mem_t *p = *coerce(mem_t **, src);
  (void) self;
  return cptr_typed(p, tft->tag, 0);
}

static val ffi_cptr_in(struct txr_ffi_type *tft, int copy, mem_t *src,
                       val ptr, val self)
{
  if (ptr) {
    if (copy) {
      mem_t *newp = *coerce(mem_t **, src);

      if (type(ptr) == CPTR) {
        mem_t **addr = cptr_addr_of(ptr, tft->tag, self);
        *addr = newp;
      } else {
        carray_set_ptr(ptr, tft->eltype, newp, self);
      }
    }
  } else {
    ptr = ffi_cptr_get(tft, src, self);
  }

  return ptr;
}

static mem_t *ffi_cptr_alloc(struct txr_ffi_type *tft, val ptr, val self)
{
  return coerce(mem_t *, cptr_addr_of(ptr, tft->tag, self));
}

static val ffi_str_in(struct txr_ffi_type *tft, int copy,
                      mem_t *src, val obj, val self)
{
  char **loc = coerce(char **, src);
  (void) tft;
  (void) self;
  if (copy)
    obj = if2(*loc, string_utf8(*loc));
  free(*loc);
  *loc = 0;
  return obj;
}

static void ffi_str_put(struct txr_ffi_type *tft, val s, mem_t *dst,
                        val self)
{
  (void) tft;
  (void) self;
  if (s == nil) {
    *coerce(const char **, dst) = 0;
  } else {
    const wchar_t *ws = c_str(s, self);
    char *u8s = utf8_dup_to(ws);
    *coerce(const char **, dst) = u8s;
  }
}

static val ffi_str_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  const char *p = *coerce(const char **, src);
  (void) tft;
  (void) self;
  return p ? string_utf8(p) : nil;
}

static val ffi_str_d_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  char **loc = coerce(char **, src);
  val ret = *loc ? string_utf8(*loc) : nil;
  (void) tft;
  (void) self;
  free(*loc);
  *loc = 0;
  return ret;
}

static val ffi_wstr_in(struct txr_ffi_type *tft, int copy,
                       mem_t *src, val obj, val self)
{
  wchar_t **loc = coerce(wchar_t **, src);
  (void) tft;
  (void) self;
  if (copy)
    obj = if2(*loc, string(*loc));
  free(*loc);
  *loc = 0;
  return obj;
}

static val ffi_wstr_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  const wchar_t *p = *coerce(wchar_t **, src);
  (void) tft;
  (void) self;
  return p ? string(p) : 0;
}

static void ffi_wstr_put(struct txr_ffi_type *tft, val s, mem_t *dst,
                         val self)
{
  (void) tft;
  (void) self;
  if (s == nil) {
    *coerce(const wchar_t **, dst) = 0;
  } else {
    const wchar_t *ws = c_str(s, self);
    *coerce(const wchar_t **, dst) = chk_strdup(ws);
  }
}

static val ffi_wstr_d_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  wchar_t **loc = coerce(wchar_t **, src);
  val ret = *loc ? string_own(*loc) : nil;
  (void) tft;
  (void) self;
  *loc = 0;
  return ret;
}

static val ffi_bstr_in(struct txr_ffi_type *tft, int copy,
                       mem_t *src, val obj, val self)
{
  unsigned char **loc = coerce(unsigned char **, src);
  (void) tft;
  (void) self;
  if (copy)
    obj = if2(*loc, string_8bit(*loc));
  free(*loc);
  *loc = 0;
  return obj;
}

static void ffi_bstr_put(struct txr_ffi_type *tft, val s, mem_t *dst,
                         val self)
{
  (void) tft;
  (void) self;
  if (s == nil) {
    *coerce(unsigned char **, dst) = 0;
  } else {
    const wchar_t *ws = c_str(s, self);
    unsigned char *u8s = chk_strdup_8bit(ws);
    *coerce(unsigned char **, dst) = u8s;
  }
}

static val ffi_bstr_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  unsigned char *p = *coerce(unsigned char **, src);
  (void) tft;
  (void) self;
  return p ? string_8bit(p) : nil;
}

static val ffi_bstr_d_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  unsigned char **loc = coerce(unsigned char **, src);
  val ret = *loc ? string_8bit(*loc) : nil;
  (void) tft;
  (void) self;
  free(*loc);
  *loc = 0;
  return ret;
}

static val ffi_buf_in(struct txr_ffi_type *tft, int copy, mem_t *src,
                      val obj, val self)
{
  mem_t **loc = coerce(mem_t **, src);
  mem_t *origptr = if3(obj, buf_get(obj, self), 0);

  (void) tft;

  if (copy && *loc != origptr)
    obj = if2(*loc, make_duplicate_buf(length_buf(obj), *loc));

  return obj;
}

static void ffi_buf_put(struct txr_ffi_type *tft, val buf, mem_t *dst,
                        val self)
{
  (void) tft;

  if (buf == nil) {
    *coerce(const mem_t **, dst) = 0;
  } else {
    mem_t *b = buf_get(buf, self);
    *coerce(const mem_t **, dst) = b;
  }
}

static val ffi_buf_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  mem_t *p = *coerce(mem_t **, src);
  (void) self;
  return p ? make_duplicate_buf(num(tft->nelem), p) : nil;
}

static val ffi_buf_d_in(struct txr_ffi_type *tft, int copy, mem_t *src,
                        val obj, val self)
{
  mem_t **loc = coerce(mem_t **, src);

  (void) self;

  if (copy) {
    obj = if2(*loc, make_borrowed_buf(num(tft->nelem), *loc));
    *loc = 0;
  }

  return obj;
}

static void ffi_buf_d_put(struct txr_ffi_type *tft, val buf, mem_t *dst,
                          val self)
{
  (void) tft;
  (void) self;

  if (buf == nil) {
    *coerce(const mem_t **, dst) = 0;
  } else {
    mem_t *b = buf_get(buf, self);
    *coerce(const mem_t **, dst) = chk_copy_obj(b, c_num(length(buf), self));
  }
}

static val ffi_buf_d_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  mem_t **loc = coerce(mem_t **, src);
  val ret = *loc ? make_borrowed_buf(num(tft->nelem), *loc) : nil;
  (void) self;
  *loc = 0;
  return ret;
}

#if HAVE_LIBFFI
static void ffi_closure_put(struct txr_ffi_type *tft, val ptr, mem_t *dst,
                            val self)
{
  val type = typeof(ptr);
  mem_t *p = 0;

  (void) tft;

  if (type == cptr_s) {
    p = ptr->co.handle;
  } else if (type == ffi_closure_s) {
    struct txr_ffi_closure *tfcl = ffi_closure_struct(ptr);
    p = tfcl->fptr;
  } else if (ptr != nil) {
    uw_throwf(error_s, lit("~a: ~s cannot be used as function pointer"),
              self, ptr, nao);
  }

  memcpy(dst, &p, sizeof p);
}
#endif

static val ffi_ptr_in_in(struct txr_ffi_type *tft, int copy, mem_t *src,
                         val obj, val self)
{
  val tgttype = tft->eltype;
  struct txr_ffi_type *tgtft = ffi_type_struct(tgttype);
  mem_t **loc = coerce(mem_t **, src);
  (void) copy;
  if (!*loc)
    return nil;
  if (tgtft->in != 0 && tgtft->by_value_in)
    tgtft->in(tgtft, 0, *loc, obj, self);
  tgtft->free(*loc);
  *loc = 0;
  return obj;
}

static val ffi_ptr_in_d_in(struct txr_ffi_type *tft, int copy, mem_t *src,
                           val obj, val self)
{
  val tgttype = tft->eltype;
  struct txr_ffi_type *tgtft = ffi_type_struct(tgttype);
  mem_t **loc = coerce(mem_t **, src);
  (void) copy;
  if (!*loc)
    return nil;
  if (tgtft->in != 0 && tgtft->by_value_in)
    tgtft->in(tgtft, 0, *loc, obj, self);
  return obj;
}

static void ffi_ptr_in_out(struct txr_ffi_type *tft, int copy, val s,
                           mem_t *dst, val self)
{
  val tgttype = tft->eltype;
  struct txr_ffi_type *tgtft = ffi_type_struct(tgttype);
  (void) copy;
  if (tgtft->out != 0) {
    mem_t *buf = *coerce(mem_t **, dst);
    tgtft->out(tgtft, 0, s, buf, self);
  }
}

static val ffi_ptr_out_in(struct txr_ffi_type *tft, int copy, mem_t *src,
                          val obj, val self)
{
  val tgttype = tft->eltype;
  struct txr_ffi_type *tgtft = ffi_type_struct(tgttype);
  mem_t **loc = coerce(mem_t **, src);
  (void) copy;
  if (!*loc)
    return nil;
  if (tgtft->in != 0)
    obj = tgtft->in(tgtft, 1, *loc, obj, self);
  else
    obj = tgtft->get(tgtft, *loc, self);
  tgtft->free(*loc);
  *loc = 0;
  return obj;
}

static void ffi_ptr_out_put(struct txr_ffi_type *tft, val s, mem_t *dst,
                            val self)
{
  val tgttype = tft->eltype;
  struct txr_ffi_type *tgtft = ffi_type_struct(tgttype);
  if (s == nil) {
    *coerce(mem_t **, dst) =  0;
  } else {
    mem_t *buf = tgtft->alloc(tgtft, s, self);
    *coerce(mem_t **, dst) = buf;
  }
}

static void ffi_ptr_out_out(struct txr_ffi_type *tft, int copy, val s,
                            mem_t *dst, val self)
{
  val tgttype = tft->eltype;
  struct txr_ffi_type *tgtft = ffi_type_struct(tgttype);
  mem_t *buf = *coerce(mem_t **, dst);
  (void) copy;
  if (tgtft->out != 0)
    tgtft->out(tgtft, 1, s, buf, self);
  else
    tgtft->put(tgtft, s, buf, self);
}

static val ffi_ptr_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  val tgttype = tft->eltype;
  struct txr_ffi_type *tgtft = ffi_type_struct(tgttype);
  mem_t *ptr = *coerce(mem_t **, src);
  return ptr ? tgtft->get(tgtft, ptr, self) : nil;
}

static val ffi_ptr_d_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  val tgttype = tft->eltype;
  struct txr_ffi_type *tgtft = ffi_type_struct(tgttype);
  mem_t **loc = coerce(mem_t **, src);
  val ret = *loc ? tgtft->get(tgtft, *loc, self) : nil;
  free(*loc);
  *loc = 0;
  return ret;
}

static void ffi_ptr_in_put(struct txr_ffi_type *tft, val s, mem_t *dst,
                           val self)
{
  val tgttype = tft->eltype;
  struct txr_ffi_type *tgtft = ffi_type_struct(tgttype);
  if (s == nil) {
    *coerce(mem_t **, dst) = 0;
  } else {
    mem_t *buf = tgtft->alloc(tgtft, s, self);
    tgtft->put(tgtft, s, buf, self);
    *coerce(mem_t **, dst) = buf;
  }
}

static void ffi_ptr_out_null_put(struct txr_ffi_type *tft, val s, mem_t *dst,
                                 val self)
{
  (void) tft;
  (void) s;
  (void) self;
  *coerce(mem_t **, dst) =  0;
}

static val ffi_ptr_out_s_in(struct txr_ffi_type *tft, int copy,
                            mem_t *src, val obj, val self)
{
  val tgttype = tft->eltype;
  struct txr_ffi_type *tgtft = ffi_type_struct(tgttype);
  mem_t **loc = coerce(mem_t **, src);
  (void) copy;
  if (!*loc)
    return nil;
  if (tgtft->in != 0)
    obj = tgtft->in(tgtft, 1, *loc, obj, self);
  else
    obj = tgtft->get(tgtft, *loc, self);
  return obj;
}

static void ffi_ptr_in_release(struct txr_ffi_type *tft, val obj,
                               mem_t *dst, val self)
{
  struct txr_ffi_type *tgtft = ffi_type_struct(tft->eltype);
  mem_t **loc = coerce(mem_t **, dst);

  if (tgtft->release != 0 && *loc != 0)
    tgtft->release(tgtft, obj, *loc, self);
  free(*loc);
  *loc = 0;
}

static val ffi_flex_struct_in(struct txr_ffi_type *tft, val strct, val self)
{
  struct smemb *lastm = &tft->memb[tft->nelem - 1];
  val length_meth = get_special_slot(strct, length_m);

  (void) self;

  if (length_meth) {
    val len = funcall1(length_meth, strct);
    val memb = slot(strct, lastm->mname);
    if (vectorp(memb))
      return vec_set_length(memb, len);
    else
      return slotset(strct, lastm->mname, vector(len, nil));
  }

  return slot(strct, lastm->mname);
}

static val ffi_struct_in(struct txr_ffi_type *tft, int copy, mem_t *src,
                         val strct, val self)
{
  cnum i, nmemb = tft->nelem;
  struct smemb *memb = tft->memb;
  int flexp = tft->flexible;

  if (!copy && (!tft->by_value_in || strct == nil))
    return strct;

  if (strct == nil) {
    args_decl(args, ARGS_ABS_MIN);
    strct = make_struct(tft->lt, nil, args);
  }

  for (i = 0; i < nmemb; i++) {
    val slsym = memb[i].mname;
    struct txr_ffi_type *mtft = memb[i].mtft;
    ucnum offs = memb[i].offs;
    if (slsym) {
      if (flexp && copy && i == nmemb - 1)
        ffi_flex_struct_in(tft, strct, self);
      if (mtft->in != 0) {
        val slval = slot(strct, slsym);
        slotset(strct, slsym, mtft->in(mtft, copy, src + offs, slval, self));
      } else if (copy) {
        val slval = mtft->get(mtft, src + offs, self);
        slotset(strct, slsym, slval);
      }
    }
  }

  return strct;
}

static void ffi_struct_put(struct txr_ffi_type *tft, val strct, mem_t *dst,
                           val self)
{
  cnum i, nmemb = tft->nelem;
  struct smemb *memb = tft->memb;

  for (i = 0; i < nmemb; i++) {
    val slsym = memb[i].mname;
    struct txr_ffi_type *mtft = memb[i].mtft;
    ucnum offs = memb[i].offs;
    if (slsym) {
      val slval = slot(strct, slsym);
      mtft->put(mtft, slval, dst + offs, self);
    }
  }
}

static void ffi_struct_out(struct txr_ffi_type *tft, int copy, val strct,
                           mem_t *dst, val self)
{
  cnum i, nmemb = tft->nelem;
  struct smemb *memb = tft->memb;

  for (i = 0; i < nmemb; i++) {
    val slsym = memb[i].mname;
    struct txr_ffi_type *mtft = memb[i].mtft;
    ucnum offs = memb[i].offs;
    if (slsym) {
      if (mtft->out != 0) {
        val slval = slot(strct, slsym);
        mtft->out(mtft, copy, slval, dst + offs, self);
      } else if (copy) {
        val slval = slot(strct, slsym);
        mtft->put(mtft, slval, dst + offs, self);
      }
    }
  }
}

static val ffi_struct_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  cnum i, nmemb = tft->nelem;
  struct smemb *memb = tft->memb;
  args_decl(args, ARGS_ABS_MIN);
  val strct = make_struct(tft->lt, nil, args);
  int flexp = tft->flexible;

  for (i = 0; i < nmemb; i++) {
    val slsym = memb[i].mname;
    struct txr_ffi_type *mtft = memb[i].mtft;
    ucnum offs = memb[i].offs;
    if (slsym) {
      if (flexp && i == nmemb - 1) {
        val slval = ffi_flex_struct_in(tft, strct, self);
        if (mtft->in != 0)
          slotset(strct, slsym, mtft->in(mtft, 1, src + offs, slval, self));
      } else {
        val slval = mtft->get(mtft, src + offs, self);
        slotset(strct, slsym, slval);
      }
    }
  }

  return strct;
}

static void ffi_struct_release(struct txr_ffi_type *tft, val strct,
                               mem_t *dst, val self)
{
  cnum i, nmemb = tft->nelem;
  struct smemb *memb = tft->memb;

  if (strct == nil)
    return;

  for (i = 0; i < nmemb; i++) {
    val slsym = memb[i].mname;
    struct txr_ffi_type *mtft = memb[i].mtft;
    ucnum offs = memb[i].offs;
    if (slsym) {
      if (mtft->release != 0) {
        val slval = slot(strct, slsym);
        mtft->release(mtft, slval, dst + offs, self);
      }
    }
  }
}

static val ffi_char_array_get(struct txr_ffi_type *tft, mem_t *src,
                              cnum nelem)
{
  if (nelem == 0) {
    return null_string;
  } else {
    const char *chptr = coerce(const char *, src);
    if (tft->null_term) {
      return string_utf8(chptr);
    } else {
      wchar_t *wch = utf8_dup_from_buf(chptr, nelem);
      return string_own(wch);
    }
  }
}

static void ffi_char_array_put(struct txr_ffi_type *tft, val str, mem_t *dst,
                               cnum nelem, val self)
{
  int nt = tft->null_term;
  const wchar_t *wstr = c_str(str, self);
  cnum needed = utf8_to_buf(0, wstr, nt);

  if (needed <= nelem) {
    utf8_to_buf(dst, wstr, nt);
    memset(dst + needed, 0, nelem - needed);
  } else {
    char *u8str = utf8_dup_to(wstr);
    memcpy(dst, u8str, nelem);
    free(u8str);
  }

  if (nt)
    dst[nelem - 1] = 0;
}

static val ffi_zchar_array_get(struct txr_ffi_type *tft, mem_t *src,
                               cnum nelem)
{
  if (nelem == 0) {
    return null_string;
  } else {
    const char *chptr = coerce(const char *, src);
    if (tft->null_term) {
      return string_utf8(chptr);
    } else if (memchr(chptr, 0, nelem)) {
      return string_utf8(chptr);
    } else {
      wchar_t *wch = utf8_dup_from_buf(chptr, nelem);
      return string_own(wch);
    }
  }
}


static val ffi_wchar_array_get(struct txr_ffi_type *tft, mem_t *src,
                               cnum nelem, val self)
{
  if (nelem == 0) {
    return null_string;
  } else {
    const wchar_t *wchptr = coerce(const wchar_t *, src);

    if (tft->null_term) {
      return string(wchptr);
    } else {
      val ustr = mkustring(num_fast(nelem));
      return init_str(ustr, wchptr, self);
    }
  }
}

static void ffi_wchar_array_put(struct txr_ffi_type *tft, val str, mem_t *dst,
                                cnum nelem, val self)
{
  const wchar_t *wstr = c_str(str, self);
  wcsncpy(coerce(wchar_t *, dst), wstr, nelem);
  if (tft->null_term)
    dst[nelem - 1] = 0;
}

static val ffi_bchar_array_get(struct txr_ffi_type *tft, mem_t *src,
                               cnum nelem)
{
  if (nelem == 0) {
    return null_string;
  } else {
    const unsigned char *chptr = coerce(const unsigned char *, src);
    if (tft->null_term)
      return string_8bit(chptr);
    else
      return string_8bit_size(chptr, nelem);
  }
}

static void ffi_bchar_array_put(struct txr_ffi_type *tft, val str, mem_t *dst,
                                cnum nelem, val self)
{
  const wchar_t *wstr = c_str(str, self);
  cnum i;

  for (i = 0; i < nelem && wstr[i]; i++) {
    wchar_t wch = wstr[i];
    if (wch < 0 || wch > 255)
      uw_throwf(error_s, lit("~a: character ~s out of unsigned 8 bit range"),
                self, chr(wch), nao);
    dst[i] = wch;
  }

  if (i < nelem) {
    for (; i < nelem; i++)
      dst[i] = 0;
  } else if (tft->null_term) {
    dst[nelem - 1] = 0;
  }
}

static val ffi_array_in_common(struct txr_ffi_type *tft, int copy,
                               mem_t *src, val vec, val self, cnum nelem)
{
  val eltype = tft->eltype;
  ucnum offs = 0;
  struct txr_ffi_type *etft = ffi_type_struct(eltype);
  cnum elsize = etft->size, i;
  cnum znelem = if3(tft->null_term && nelem > 0 &&
                    vec && length(vec) < num_fast(nelem), nelem - 1, nelem);

  if (!copy && (!tft->by_value_in || vec == nil))
    return vec;

  if (vec == nil)
    vec = vector(num_fast(znelem), nil);

  for (i = 0; i < znelem; i++) {
    if (etft->in != 0) {
      val elval = ref(vec, num_fast(i));
      refset(vec, num_fast(i), etft->in(etft, copy, src + offs, elval, self));
    } else if (copy) {
      val elval = etft->get(etft, src + offs, self);
      refset(vec, num_fast(i), elval);
    }
    offs += elsize;
  }

  return vec;
}

static val ffi_array_in(struct txr_ffi_type *tft, int copy, mem_t *src,
                        val vec, val self)
{
  if (copy) {
    switch (tft->ch_conv) {
    case conv_char:
      {
        val str = ffi_char_array_get(tft, src, tft->nelem);
        return if3(vec, replace(vec, str, zero, t), str);
      }
    case conv_zchar:
      {
        val str = ffi_zchar_array_get(tft, src, tft->nelem);
        return if3(vec, replace(vec, str, zero, t), str);
      }
    case conv_wchar:
      {
        val str = ffi_wchar_array_get(tft, src, tft->nelem, self);
        return if3(vec, replace(vec, str, zero, t), str);
      }
    case conv_bchar:
      {
        val str = ffi_bchar_array_get(tft, src, tft->nelem);
        return if3(vec, replace(vec, str, zero, t), str);
      }
    case conv_none:
      break;
    }
  }
  return ffi_array_in_common(tft, copy, src, vec, self, tft->nelem);
}

static void ffi_array_put_common(struct txr_ffi_type *tft, val vec, mem_t *dst,
                                 val self, cnum nelem)
{
  val eltype = tft->eltype;
  struct txr_ffi_type *etft = ffi_type_struct(eltype);
  cnum elsize = etft->size;
  int nt = tft->null_term;
  cnum i = 0;
  ucnum offs = 0;
  seq_info_t si = seq_info(vec);

  switch (si.kind) {
  case SEQ_NIL:
  case SEQ_LISTLIKE:
    {
      val iter = si.obj;

      for (; i < nelem - nt && !endp(iter); i++, iter = cdr(iter)) {
        val elval = car(iter);
        etft->put(etft, elval, dst + offs, self);
        offs += elsize;
      }
    }
    break;
  case SEQ_VECLIKE:
    {
      val v = si.obj;
      cnum lim = min(nelem - nt, c_num(length(si.obj), self));

      for (; i < lim; i++) {
        val elval = ref(v, num_fast(i));
        etft->put(etft, elval, dst + offs, self);
        offs += elsize;
      }
    }
    break;
  default:
    uw_throwf(error_s, lit("~a: ~s isn't convertible to a C array"), self,
              vec, nao);
  }

  if (i < nelem)
    memset(dst + offs, 0, elsize * (nelem - i));
}

static void ffi_array_put(struct txr_ffi_type *tft, val vec, mem_t *dst,
                          val self)
{
  if (tft->ch_conv != conv_none && stringp(vec)) {
    switch (tft->ch_conv) {
    case conv_char:
    case conv_zchar:
      ffi_char_array_put(tft, vec, dst, tft->nelem, self);
      break;
    case conv_wchar:
      ffi_wchar_array_put(tft, vec, dst, tft->nelem, self);
      break;
    case conv_bchar:
      ffi_bchar_array_put(tft, vec, dst, tft->nelem, self);
      break;
    case conv_none:
      /* notreached */
      break;
    }
  } else {
    ffi_array_put_common(tft, vec, dst, self, tft->nelem);
  }
}

static void ffi_array_out_common(struct txr_ffi_type *tft, int copy, val vec,
                                 mem_t *dst, val self, cnum nelem)
{
  val eltype = tft->eltype;
  struct txr_ffi_type *etft = ffi_type_struct(eltype);
  cnum elsize = etft->size;
  int nt = tft->null_term;
  cnum i;
  ucnum offs = 0;

  for (i = 0; i < nelem; i++) {
    if (nt && i == nelem - 1) {
      memset(dst + offs, 0, elsize);
      break;
    }
    if (etft->out != 0) {
      val elval = ref(vec, num_fast(i));
      etft->out(etft, copy, elval, dst + offs, self);
    } else if (copy) {
      val elval = ref(vec, num_fast(i));
      etft->put(etft, elval, dst + offs, self);
    }
    offs += elsize;
  }
}

static void ffi_array_out(struct txr_ffi_type *tft, int copy, val vec,
                          mem_t *dst, val self)
{
  if (tft->ch_conv != conv_none && stringp(vec)) {
    switch (tft->ch_conv) {
    case conv_char:
    case conv_zchar:
      ffi_char_array_put(tft, vec, dst, tft->nelem, self);
      break;
    case conv_wchar:
      ffi_wchar_array_put(tft, vec, dst, tft->nelem, self);
      break;
    case conv_bchar:
      ffi_bchar_array_put(tft, vec, dst, tft->nelem, self);
      break;
    case conv_none:
      /* notreached */
      break;
    }
  } else {
    ffi_array_out_common(tft, copy, vec, dst, self, tft->nelem);
  }
}

static val ffi_array_get_common(struct txr_ffi_type *tft, mem_t *src, val self,
                                cnum nelem)
{
  val eltype = tft->eltype;

  switch (tft->ch_conv) {
  case conv_char:
    return ffi_char_array_get(tft, src, nelem);
  case conv_zchar:
    return ffi_zchar_array_get(tft, src, nelem);
  case conv_wchar:
    return ffi_wchar_array_get(tft, src, nelem, self);
  case conv_bchar:
    return ffi_bchar_array_get(tft, src, nelem);
  case conv_none:
    {
      cnum znelem = if3(tft->null_term && nelem > 0, nelem - 1, nelem);
      val vec = vector(num_fast(znelem), nil);
      struct txr_ffi_type *etft = ffi_type_struct(eltype);
      cnum elsize = etft->size;
      cnum offs, i;

      for (i = 0, offs = 0; i < znelem; i++) {
        val elval = etft->get(etft, src + offs, self);
        refset(vec, num_fast(i), elval);
        offs += elsize;
      }

      return vec;
    }
  }

  /* notreached */
  return nil;
}

static val ffi_array_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  cnum nelem = tft->nelem;
  return ffi_array_get_common(tft, src, self, nelem);
}

static void ffi_array_release_common(struct txr_ffi_type *tft, val vec,
                                     mem_t *dst, cnum nelem, val self)
{
  val eltype = tft->eltype;
  ucnum offs = 0;
  struct txr_ffi_type *etft = ffi_type_struct(eltype);
  cnum elsize = etft->size, i;
  cnum znelem = if3(tft->null_term && nelem > 0 &&
                    vec && length(vec) < num_fast(nelem), nelem - 1, nelem);

  if (vec == nil)
    return;

  if (tft->ch_conv != conv_none)
    return;

  for (i = 0; i < znelem; i++) {
    if (etft->release != 0) {
      val elval = ref(vec, num_fast(i));
      etft->release(etft, elval, dst + offs, self);
    }
    offs += elsize;
  }
}

static void ffi_array_release(struct txr_ffi_type *tft, val vec,
                              mem_t *dst, val self)
{
  ffi_array_release_common(tft, vec, dst, tft->nelem, self);
}

static void ffi_varray_put(struct txr_ffi_type *tft, val vec, mem_t *dst,
                           val self)
{
  struct txr_ffi_type *etft = ffi_type_struct(tft->eltype);
  cnum nelem = ffi_varray_dynsize(tft, vec, self) / etft->size;

  if (tft->ch_conv != conv_none && stringp(vec)) {
    switch (tft->ch_conv) {
    case conv_char:
    case conv_zchar:
      ffi_char_array_put(tft, vec, dst, nelem, self);
      break;
    case conv_wchar:
      ffi_wchar_array_put(tft, vec, dst, nelem, self);
      break;
    case conv_bchar:
      ffi_bchar_array_put(tft, vec, dst, nelem, self);
      break;
    case conv_none:
      /* notreached */
      break;
    }
  } else {
    ffi_array_put_common(tft, vec, dst, self, nelem);
  }
}

static val ffi_varray_in(struct txr_ffi_type *tft, int copy, mem_t *src,
                         val vec, val self)
{
  if (copy && vec) {
    struct txr_ffi_type *etft = ffi_type_struct(tft->eltype);
    cnum nelem = ffi_varray_dynsize(tft, vec, self) / etft->size;

    switch (tft->ch_conv) {
    case conv_char:
      {
        val str = ffi_char_array_get(tft, src, nelem);
        return if3(vec, replace(vec, str, zero, t), str);
      }
    case conv_zchar:
      {
        val str = ffi_zchar_array_get(tft, src, nelem);
        return if3(vec, replace(vec, str, zero, t), str);
      }
    case conv_wchar:
      {
        val str = ffi_wchar_array_get(tft, src, nelem, self);
        return if3(vec, replace(vec, str, zero, t), str);
      }
    case conv_bchar:
      {
        val str = ffi_bchar_array_get(tft, src, nelem);
        return if3(vec, replace(vec, str, zero, t), str);
      }
    case conv_none:
      break;
    }
    return ffi_array_in_common(tft, copy, src, vec, self, nelem);
  }

  return vec;
}

static val ffi_varray_null_term_in(struct txr_ffi_type *tft, int copy, mem_t *src,
                                   val vec_in, val self)
{
  if (tft->ch_conv != conv_none) {
    return ffi_varray_in(tft, copy, src, vec_in, self);
  } else {
    val vec = vector(zero, nil);
    val eltype = tft->eltype;
    struct txr_ffi_type *etft = ffi_type_struct(eltype);
    cnum elsize = etft->size;
    cnum offs, i;
    cnum nelem_orig = c_num(length(vec_in), self);

    for (i = 0, offs = 0; ; i++) {
      mem_t *el = src + offs, *p;

      for (p = el; p < el + elsize; p++)
        if (*p)
          break;

      if (p == el + elsize)
        break;

      if (etft->in != 0 && i < nelem_orig) {
        val elval = ref(vec_in, num_fast(i));
        vec_push(vec, etft->in(etft, copy, src + offs, elval, self));
      } else if (copy) {
        val elval = etft->get(etft, src + offs, self);
        vec_push(vec, elval);
      }

      offs += elsize;
    }

    return if3(vec_in, replace(vec_in, vec, zero, t), vec);
  }
}

static val ffi_varray_null_term_get(struct txr_ffi_type *tft, mem_t *src,
                                    val self)
{
  val eltype = tft->eltype;

  if (tft->ch_conv != conv_none) {
    return ffi_array_get_common(tft, src, self, INT_PTR_MAX);
  } else {
    val vec = vector(zero, nil);
    struct txr_ffi_type *etft = ffi_type_struct(eltype);
    cnum elsize = etft->size;
    cnum offs, i;

    for (i = 0, offs = 0; ; i++) {
      mem_t *el = src + offs, *p;

      for (p = el; p < el + elsize; p++)
        if (*p)
          break;

      if (p == el + elsize)
        break;

      {
        val elval = etft->get(etft, src + offs, self);
        vec_push(vec, elval);
        offs += elsize;
      }
    }

    return vec;
  }
}

static void ffi_varray_release(struct txr_ffi_type *tft, val vec,
                               mem_t *dst, val self)
{
  cnum nelem = c_num(length(vec), self) + tft->null_term;
  ffi_array_release_common(tft, vec, dst, nelem, self);
}

static val ffi_carray_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  mem_t *p = *coerce(mem_t **, src);
  (void) self;
  return make_carray(tft->eltype, p, -1, nil, 0);
}

static val ffi_carray_in(struct txr_ffi_type *tft, int copy, mem_t *src,
                         val carray, val self)
{
  if (carray) {
    if (copy) {
      mem_t *p = *coerce(mem_t **, src);
      carray_set_ptr(carray, tft->eltype, p, self);
    }
  } else {
    carray = ffi_carray_get(tft, src, self);
  }

  return carray;
}

static void ffi_carray_put(struct txr_ffi_type *tft, val carray, mem_t *dst,
                           val self)
{
  mem_t *p = carray_ptr(carray, tft->eltype, self);
  *coerce(mem_t **, dst) = p;
}

static void ffi_enum_put(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  struct txr_ffi_type *etft = ffi_type_struct(tft->eltype);

  if (symbolp(n)) {
    val n_num = gethash(tft->num_sym, n);
    if (!n_num)
      uw_throwf(error_s, lit("~a: ~s has no member ~s"), self,
                tft->syntax, n, nao);
    n = n_num;
  }
  etft->put(tft, n, dst, self); /* tft deliberate */
}

static val ffi_enum_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  struct txr_ffi_type *etft = ffi_type_struct(tft->eltype);
  val n = etft->get(tft, src, self); /* tft deliberate */
  val sym = gethash(tft->sym_num, n);
  return if3(sym, sym, n);
}

#if !HAVE_LITTLE_ENDIAN

static void ffi_enum_rput(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  struct txr_ffi_type *etft = ffi_type_struct(tft->eltype);

  if (symbolp(n)) {
    val n_num = gethash(tft->num_sym, n);
    if (!n_num)
      uw_throwf(error_s, lit("~a: ~s has no member ~s"), self,
                tft->syntax, n, nao);
    n = n_num;
  }
  etft->rput(tft, n, dst, self); /* tft deliberate */
}

static val ffi_enum_rget(struct txr_ffi_type *tft, mem_t *src, val self)
{
  struct txr_ffi_type *etft = ffi_type_struct(tft->eltype);
  val n = etft->rget(tft, src, self); /* tft deliberate */
  val sym = gethash(tft->sym_num, n);
  return if3(sym, sym, n);
}

#endif

static struct txr_ffi_type *ffi_find_memb(struct txr_ffi_type *tft, val name)
{
  cnum i;
  for (i = 0; i < tft->nelem; i++) {
    if (tft->memb[i].mname == name)
      return tft->memb[i].mtft;
  }

  return 0;
}

static void ffi_memb_not_found(val type, val name, val self)
{
  uw_throwf(error_s, lit("~a: ~s doesn't name a member of ~s"),
            type, name, self, nao);
}

static val make_union_tft(mem_t *buf, struct txr_ffi_type *tft);

static val ffi_union_in(struct txr_ffi_type *tft, int copy, mem_t *src,
                        val uni, val self)
{
  if (copy) {
    if (uni == nil) {
      uni = make_union_tft(src, tft);
    } else {
      mem_t *ptr = union_get_ptr(self, uni);
      memcpy(ptr, src, tft->size);
    }
  }

  return uni;
}

static void ffi_union_put(struct txr_ffi_type *tft, val uni,
                          mem_t *dst, val self)
{
  mem_t *ptr = union_get_ptr(self, uni);
  memcpy(dst, ptr, tft->size);
}

static val ffi_union_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  (void) self;
  return make_union_tft(src, tft);
}

static val ffi_type_copy(val orig)
{
  struct txr_ffi_type *otft = ffi_type_struct(orig);
  struct txr_ffi_type *ctft = otft->clone(otft);
  return cobj(coerce(mem_t *, ctft), orig->co.cls, orig->co.ops);
}

static val ffi_type_copy_new_ops(val orig, struct cobj_ops *ops)
{
  struct txr_ffi_type *otft = ffi_type_struct(orig);
  struct txr_ffi_type *ctft = otft->clone(otft);
  return cobj(coerce(mem_t *, ctft), orig->co.cls, ops);
}

static struct txr_ffi_type *ffi_simple_clone(struct txr_ffi_type *orig)
{
  return coerce(struct txr_ffi_type *, chk_copy_obj(coerce(mem_t *, orig),
                                                    sizeof *orig));
}

static val make_ffi_type_builtin(val syntax, val lisp_type, ffi_kind_t kind,
                                 cnum size, cnum align, ffi_type *ft,
                                 void (*put)(struct txr_ffi_type *,
                                             val obj, mem_t *dst, val self),
                                 val (*get)(struct txr_ffi_type *,
                                            mem_t *src, val self),
                                 void (*rput)(struct txr_ffi_type *,
                                             val obj, mem_t *dst, val self),
                                 val (*rget)(struct txr_ffi_type *,
                                             mem_t *src, val self))
{
  struct txr_ffi_type *tft = coerce(struct txr_ffi_type *,
                                    chk_calloc(1, sizeof *tft));

  val obj = cobj(coerce(mem_t *, tft), ffi_type_cls, &ffi_type_builtin_ops);

  tft->self = obj;
  tft->kind = kind;
  tft->ft = ft;
  tft->syntax = syntax;
  tft->lt = lisp_type;
  tft->size = size;
  tft->align = align;
  tft->clone = ffi_simple_clone;
  tft->put = put;
  tft->get = get;
  tft->alloc = ffi_fixed_alloc;
  tft->dynsize = ffi_fixed_dynsize;
  tft->free = free;
#if !HAVE_LITTLE_ENDIAN
  tft->rput = (rput ? rput : put);
  tft->rget = (rget ? rget : get);
#else
  (void) rput;
  (void) rget;
#endif

  return obj;
}

static val make_ffi_type_pointer(val syntax, val lisp_type,
                                 void (*put)(struct txr_ffi_type *, val obj,
                                             mem_t *dst, val self),
                                 val (*get)(struct txr_ffi_type *,
                                            mem_t *src, val self),
                                 val (*in)(struct txr_ffi_type *, int copy,
                                           mem_t *src, val obj, val self),
                                 void (*out)(struct txr_ffi_type *, int copy,
                                             val obj, mem_t *dst, val self),
                                 void (*release)(struct txr_ffi_type *,
                                                 val obj, mem_t *dst, val self),
                                 val tgtype)
{
  val self = lit("ffi-type-compile");
  struct txr_ffi_type *tgtft = ffi_type_struct(tgtype);

  if (tgtft->bitfield) {
    uw_throwf(error_s, lit("~a: type combination ~s not allowed"),
              self, syntax, nao);
  } else {
    struct txr_ffi_type *tft = coerce(struct txr_ffi_type *,
                                      chk_calloc(1, sizeof *tft));

    val obj = cobj(coerce(mem_t *, tft), ffi_type_cls, &ffi_type_ptr_ops);

    tft->self = obj;
    tft->kind = FFI_KIND_PTR;
    tft->ft = &ffi_type_pointer;
    tft->syntax = syntax;
    tft->lt = lisp_type;
    tft->size = sizeof (mem_t *);
    tft->align = alignof (mem_t *);
    tft->clone = ffi_simple_clone;
    tft->put = put;
    tft->get = get;
#if !HAVE_LITTLE_ENDIAN
    tft->rput = put;
    tft->rget = get;
#endif
    tft->eltype = tgtype;
    tft->in = in;
    tft->out = out;
    tft->release = release;
    tft->alloc = ffi_fixed_alloc;
    tft->dynsize = ffi_fixed_dynsize;
    tft->free = free;
    tft->by_value_in = (in != 0);

    return obj;
  }
}

static struct txr_ffi_type *ffi_struct_clone(struct txr_ffi_type *orig)
{
  cnum nmemb = orig->nelem;
  struct txr_ffi_type *copy = ffi_simple_clone(orig);
  size_t memb_size = sizeof *orig->memb * nmemb;

  copy->memb = coerce(struct smemb *, chk_copy_obj(coerce(mem_t *,
                                                          orig->memb),
                                                   memb_size));

  return copy;
}

static val ffi_memb_compile(val syntax, int last, int *pflexp, val self)
{
  val type = cadr(syntax);
  val comp_type = ffi_type_compile(type);
  struct txr_ffi_type *ctft = ffi_type_struct(comp_type);
  if (consp(cddr(syntax)) && cdddr(syntax))
    uw_throwf(error_s, lit("~a: excess elements in type-member pair ~s"),
              self, syntax, nao);
  if (ctft->flexible || (ctft->incomplete && ctft->kind == FFI_KIND_ARRAY)) {
    if (!last)
      uw_throwf(error_s,
                lit("~a: flexible type ~s can only be last member"),
                self, type, nao);
    *pflexp = 1;
  } else if (ctft->incomplete) {
    uw_throwf(error_s,
              lit("~a: incomplete type ~s can't be struct/union member"),
              self, type, nao);
  }

  return comp_type;
}

#if HAVE_LIBFFI

static void ffi_struct_calcft(struct txr_ffi_type *tft)
{
  cnum nmemb = tft->nelem;
  ffi_type *ft = coerce(ffi_type *, chk_calloc(1, sizeof *ft));
  ffi_type **elem = coerce(ffi_type **, chk_calloc(nmemb + 1, sizeof *elem));
  cnum i, e, po;

  tft->ft = ft;
  tft->elements = elem;

  ft->type = FFI_TYPE_STRUCT;
  ft->size = tft->size;
  ft->alignment = tft->align;
  ft->elements = tft->elements;

  for (i = e = po = 0; i < nmemb; i++)
  {
    struct smemb *memb = &tft->memb[i];
    struct txr_ffi_type *mtft = memb->mtft;

    if (memb->offs != po) {
      po = memb->offs;
      if (mtft->calcft)
        mtft->calcft(mtft);
      elem[e++] = mtft->ft;
    }
  }
}

static void ffi_union_calcft(struct txr_ffi_type *tft)
{
  cnum nmemb = tft->nelem;
  ffi_type *ft = coerce(ffi_type *, chk_calloc(1, sizeof *ft));
  cnum i, e, po;
  struct txr_ffi_type *most_aligned = 0;

  for (i = e = po = 0; i < nmemb; i++)
  {
    struct smemb *memb = &tft->memb[i];
    struct txr_ffi_type *mtft = memb->mtft;

    if (most_aligned == 0 || mtft->align > most_aligned->align)
      most_aligned = mtft;
  }

  {
    ucnum units = tft->size / most_aligned->size, u;
    ffi_type **elem = coerce(ffi_type **, chk_calloc(units + 1, sizeof *elem));
    for (u = 0; u < units; u++)
      elem[i] = most_aligned->ft;
    tft->elements = elem;
  }

  tft->ft = ft;
  ft->type = FFI_TYPE_STRUCT;
  ft->size = tft->size;
  ft->alignment = tft->align;
  ft->elements = tft->elements;
}

static void ffi_array_calcft(struct txr_ffi_type *tft)
{
  cnum nmemb = tft->nelem;
  ffi_type *ft = coerce(ffi_type *, chk_calloc(1, sizeof *ft));
  ffi_type **elem = coerce(ffi_type **, chk_calloc(nmemb + 1, sizeof *elem));
  struct txr_ffi_type *etft = ffi_type_struct(tft->eltype);
  cnum i;

  tft->ft = ft;
  tft->elements = elem;

  ft->type = FFI_TYPE_STRUCT;
  ft->size = tft->size;
  ft->alignment = tft->align;
  ft->elements = tft->elements;

  for (i = 0; i < nmemb; i++)
    elem[i] = etft->ft;
}

#endif

static val make_ffi_type_struct(val syntax, val lisp_type,
                                val use_existing, val self)
{
  val slot_exprs = cddr(syntax);
  cnum nmemb = c_num(length(slot_exprs), self), i;
  struct txr_ffi_type *tft = if3(use_existing,
                                 ffi_type_struct(use_existing),
                                 coerce(struct txr_ffi_type *,
                                        chk_calloc(1, sizeof *tft)));
  int flexp = 0;
  struct smemb *memb = coerce(struct smemb *,
                              chk_calloc(nmemb, sizeof *memb));
  val obj = if3(use_existing,
                tft->self,
                cobj(coerce(mem_t *, tft), ffi_type_cls, &ffi_type_struct_ops));
  ucnum offs = 0;
  ucnum most_align = 0;
  int need_out_handler = 0;
  int bit_offs = 0;
  const unsigned bits_int = 8 * sizeof(int);

  if (use_existing) {
    if (tft->nelem != 0) {
      free(memb);
      return make_ffi_type_struct(syntax, lisp_type, nil, self);
    }
#if HAVE_LIBFFI
    free(tft->ft);
    free(tft->elements);
#endif
    free(tft->memb);
    memset(tft, 0, sizeof *tft);
  }

  tft->self = obj;
  tft->kind = FFI_KIND_STRUCT;
  tft->syntax = syntax;
  tft->lt = lisp_type;
  tft->clone = ffi_struct_clone;
#if HAVE_LIBFFI
  tft->calcft = ffi_struct_calcft;
#endif
  tft->put = ffi_struct_put;
  tft->get = ffi_struct_get;
#if !HAVE_LITTLE_ENDIAN
  tft->rput = ffi_struct_put;
  tft->rget = ffi_struct_get;
#endif
  tft->in = ffi_struct_in;
  tft->release = ffi_struct_release;
  tft->alloc = ffi_fixed_alloc;
  tft->dynsize = ffi_fixed_dynsize;
  tft->free = free;

  tft->incomplete = 1;

  setcheck(obj, syntax);
  setcheck(obj, lisp_type);

  sethash(ffi_struct_tag_hash, cadr(syntax), obj);

  for (i = 0; i < nmemb; i++) {
    val slot_syntax = pop(&slot_exprs);
    val slot = car(slot_syntax);
    val type = ffi_memb_compile(slot_syntax, i == nmemb - 1, &flexp, self);
    struct txr_ffi_type *mtft = ffi_type_struct(type);
    cnum size = mtft->size;

    tft->nelem = i + 1;

    memb[i].mtype = type;
    memb[i].mname = slot;
    memb[i].mtft = mtft;

    setcheck(obj, slot);
    setcheck(obj, type);

    if (mtft->bitfield) {
      ucnum size = mtft->size;
      ucnum bits_type = 8 * size;
      ucnum bits = mtft->nelem;
      ucnum offs_mask = size - 1;
      ucnum align_mask = ~offs_mask;
      ucnum unit_offs = offs & align_mask;
      ucnum bits_alloc = 8 * (offs - unit_offs) + bit_offs;
      ucnum room = bits_type - bits_alloc;
      ucnum align = if3(slot, mtft->align, 1);

      if (bits == 0) {
        if (offs != unit_offs || bit_offs > 0)
          offs = unit_offs + size;
        bit_offs = 0;
        nmemb--, i--;
        continue;
      }

      if (bits > room) {
        offs = unit_offs + size;
        bit_offs = bits_alloc = 0;
      }

      memb[i].offs = offs;

#if HAVE_LITTLE_ENDIAN
      mtft->shift = bit_offs;
#else
      mtft->shift = bits_int - bit_offs - bits;
#endif
      if (bits == bits_int)
        mtft->mask = UINT_MAX;
      else
        mtft->mask = ((1U << bits) - 1) << mtft->shift;
      bit_offs += bits;
      offs += bit_offs / 8;
      bit_offs %= 8;

      if (most_align < align)
        most_align = align;
    } else {
      ucnum align = mtft->align;
      ucnum almask = align - 1;

      if (bit_offs > 0) {
        bug_unless (bit_offs < 8);
        offs++;
        bit_offs = 0;
      }

      offs = (offs + almask) & ~almask;
      memb[i].offs = offs;
      offs += size;

      if (align > most_align)
        most_align = align;
    }

    need_out_handler = need_out_handler || mtft->out != 0;

    if (mtft->by_value_in)
      tft->by_value_in = 1;
  }

  tft->memb = memb;

  if (bit_offs > 0) {
    bug_unless (bit_offs < 8);
    offs++;
  }

  tft->incomplete = (flexp || nmemb == 0);
  tft->flexible = flexp;

  if (need_out_handler)
    tft->out = ffi_struct_out;

  if (flexp) {
    tft->size = offs;
    tft->alloc = ffi_flex_alloc;
    tft->dynsize = ffi_flex_dynsize;
  } else {
    tft->size = (offs + most_align - 1) & ~(most_align - 1);
  }

  tft->align = most_align;

  return obj;
}

static val make_ffi_type_union(val syntax, val use_existing, val self)
{
  struct txr_ffi_type *tft = if3(use_existing,
                                 ffi_type_struct(use_existing),
                                 coerce(struct txr_ffi_type *,
                                        chk_calloc(1, sizeof *tft)));
  int flexp = 0;
  val slot_exprs = cddr(syntax);
  cnum nmemb = c_num(length(slot_exprs), self), i;
  struct smemb *memb = coerce(struct smemb *,
                              chk_calloc(nmemb, sizeof *memb));
  val obj = if3(use_existing,
                tft->self,
                cobj(coerce(mem_t *, tft), ffi_type_cls, &ffi_type_struct_ops));
  ucnum most_align = 0;
  ucnum biggest_size = 0;
  const unsigned bits_int = 8 * sizeof(int);

  if (use_existing) {
    if (tft->nelem != 0) {
      free(memb);
      return make_ffi_type_union(syntax, nil, self);
    }
#if HAVE_LIBFFI
    free(tft->ft);
    free(tft->elements);
#endif
    free(tft->memb);
    memset(tft, 0, sizeof *tft);
  }

  tft->self = obj;
  tft->kind = FFI_KIND_UNION;
  tft->syntax = syntax;
  tft->lt = union_s;
  tft->nelem = nmemb;
  tft->clone = ffi_struct_clone;
#if HAVE_LIBFFI
  tft->calcft = ffi_union_calcft;
#endif
  tft->put = ffi_union_put;
  tft->get = ffi_union_get;
#if !HAVE_LITTLE_ENDIAN
  tft->rput = ffi_union_put;
  tft->rget = ffi_union_get;
#endif
  tft->in = ffi_union_in;
  tft->alloc = ffi_fixed_alloc;
  tft->dynsize = ffi_fixed_dynsize;
  tft->free = free;

  tft->incomplete = 1;

  setcheck(obj, syntax);

  sethash(ffi_struct_tag_hash, cadr(syntax), obj);

  for (i = 0; i < nmemb; i++) {
    val slot_syntax = pop(&slot_exprs);
    val slot = car(slot_syntax);
    val type = ffi_memb_compile(slot_syntax, i == nmemb - 1, &flexp, self);
    struct txr_ffi_type *mtft = ffi_type_struct(type);

    memb[i].mtype = type;
    memb[i].mname = slot;
    memb[i].mtft = mtft;
    memb[i].offs = 0;

    setcheck(obj, slot);
    setcheck(obj, type);

    if (mtft->bitfield) {
      ucnum bits = mtft->nelem;
      ucnum size = (bits + 7) / 8;
      ucnum align = if3(slot, mtft->align, 1);

      if (bits == 0) {
        nmemb--, i--;
        continue;
      }

#if HAVE_LITTLE_ENDIAN
      mtft->shift = 0;
#else
      mtft->shift = bits_int - bits;
#endif
      if (bits == bits_int)
        mtft->mask = UINT_MAX;
      else
        mtft->mask = ((1U << bits) - 1) << mtft->shift;

      if (most_align < align)
        most_align = align;
      if (biggest_size < size)
        biggest_size = size;
    } else {
      if (most_align < convert(ucnum, mtft->align))
        most_align = mtft->align;
      if (biggest_size < convert(ucnum, mtft->size))
        biggest_size = mtft->size;
    }
  }

  tft->memb = memb;

  if (flexp)
    uw_throwf(error_s,
              lit("~a: unions cannot contain incomplete member"),
              self, nao);

  tft->nelem = i;

  tft->size = (biggest_size + most_align - 1) & ~(most_align - 1);
  tft->align = most_align;

  return obj;
}

static val make_ffi_type_array(val syntax, val lisp_type,
                               val dim, val eltype, val self)
{
  struct txr_ffi_type *tft = coerce(struct txr_ffi_type *,
                                    chk_calloc(1, sizeof *tft));
  cnum nelem = c_num(dim, self);
  val obj = cobj(coerce(mem_t *, tft), ffi_type_cls, &ffi_type_struct_ops);

  struct txr_ffi_type *etft = ffi_type_struct(eltype);

  (void) self;

  tft->self = obj;
  tft->kind = FFI_KIND_ARRAY;
  tft->syntax = syntax;
  tft->lt = lisp_type;
  tft->eltype = eltype;
  tft->clone = ffi_simple_clone;
#if HAVE_LIBFFI
  tft->calcft = ffi_array_calcft;
#endif
  tft->put = ffi_array_put;
  tft->get = ffi_array_get;
#if !HAVE_LITTLE_ENDIAN
  tft->rput = ffi_array_put;
  tft->rget = ffi_array_get;
#endif
  tft->in = ffi_array_in;
  tft->release = ffi_array_release;
  tft->alloc = ffi_fixed_alloc;
  tft->dynsize = ffi_fixed_dynsize;
  tft->free = free;
  tft->by_value_in = etft->by_value_in;
  tft->size = etft->size * nelem;
  tft->align = etft->align;
  if (etft->out != 0)
    tft->out = ffi_array_out;
  tft->nelem = nelem;

  return obj;
}

static val ffi_eval_expr(val expr, val menv, val env)
{
  val expr_ex = expand(expr, menv);
  return eval(expr_ex, env, expr_ex);
}

static val make_ffi_type_enum(val syntax, val enums,
                              val base_type, val self)
{
  val type_copy = ffi_type_copy_new_ops(base_type, &ffi_type_enum_ops);
  struct txr_ffi_type *tft = ffi_type_struct(type_copy);
  struct txr_ffi_type *btft = ffi_type_struct(base_type);
  val sym_num = make_hash(hash_weak_none, t);
  val num_sym = make_hash(hash_weak_none, nil);
  val cur;
  val iter;
  val enum_env = make_env(nil, nil, nil);
  val shadow_menv = make_env(nil, nil, nil);

  if (btft->kind != FFI_KIND_NUM)
    uw_throwf(error_s, lit("~a: type ~s can't be basis for enum"),
              self, btft->syntax, nao);

  tft->kind = FFI_KIND_ENUM;
  tft->syntax = syntax;
  tft->lt = sym_s;
  tft->put = ffi_enum_put;
  tft->get = ffi_enum_get;
#if !HAVE_LITTLE_ENDIAN
  tft->rput = ffi_enum_rput;
  tft->rget = ffi_enum_rget;
#endif
  tft->eltype = base_type;

  tft->num_sym = num_sym;
  tft->sym_num = sym_num;

  for (cur = negone, iter = enums; !endp(iter); iter = cdr(iter)) {
    int_ptr_t conv_buf[2];
    val en = car(iter);
    val sym;

    if (symbolp(en)) {
      sym = en;
      if (!bindable(sym))
        uw_throwf(error_s, lit("~a: ~s member ~s isn't a bindable symbol"),
                  self, syntax, sym, nao);

      if (gethash(num_sym, sym))
        uw_throwf(error_s, lit("~a: ~s duplicate member ~s"),
                  self, syntax, sym, nao);

      cur = plus(cur, one);
    } else {
      val expr = cadr(en);

      sym = car(en);

      if (!bindable(sym))
        uw_throwf(error_s, lit("~a: ~s member ~s isn't a bindable symbol"),
                  self, syntax, sym, nao);
      if (gethash(num_sym, sym))
        uw_throwf(error_s, lit("~a: ~s duplicate member ~s"),
                  self, syntax, sym, nao);

      cur = ffi_eval_expr(expr, shadow_menv, enum_env);

      if (!integerp(cur)) {
        uw_throwf(error_s, lit("~a: ~s member ~s value ~s not integer"),
                  self, syntax, sym, cur, nao);
      }
    }

    btft->put(btft, cur, coerce(mem_t *, conv_buf), self);

    sethash(num_sym, sym, cur);
    sethash(sym_num, cur, sym);
    env_vbind(enum_env, sym, cur);
    env_vbind(shadow_menv, sym, special_s);
  }

  return type_copy;
}

static val ffi_type_lookup(val sym)
{
  return gethash(ffi_typedef_hash, sym);
}

static val ffi_type_lookup_checked(val self, val sym)
{
  val type = gethash(ffi_typedef_hash, sym);
  if (!type)
    uw_throwf(error_s, lit("~a: unrecognized type specifier: ~s"),
              self, sym, nao);
  return type;
}

static val ffi_struct_init(val slot_init, val strct)
{
  val stype = struct_type(strct);
  for (; slot_init; slot_init = us_cdr(slot_init)) {
    us_cons_bind(slot, initval, us_car(slot_init));
    if (!static_slot_p(stype, slot))
      slotset(strct, slot, initval);
  }

  return nil;
}

val ffi_type_compile(val syntax)
{
  val self = lit("ffi-type-compile");

  if (consp(syntax)) {
    val sym = car(syntax);

    if (!cdr(syntax))
      goto toofew;

    if (sym == struct_s) {
      val name = cadr(syntax);
      val membs = cddr(syntax);
      val sname = if3(name, name, gensym(lit("ffi-struct-")));
      val existing_type = if2(name, gethash(ffi_struct_tag_hash, sname));

      if (!membs) {
        if (!existing_type) {
          val xsyntax = cons(struct_s, cons(sname, nil));
          return make_ffi_type_struct(xsyntax, nil, nil, self);
        } else {
          return existing_type;
        }
      } else {
        uses_or2;
        val iter;
        list_collect_decl (slots, ptslots);
        list_collect_decl (slot_inits, ptinits);

        for (iter = membs; iter; iter = cdr(iter)) {
          val spec = car(iter);
          val slot = car(spec);
          val init = caddr(spec);
          if (!slot && init)
            uw_warningf(lit("~a: padding slot struct ~s specifies init-form"),
                        self, name, nao);
          if (slot)
            ptslots = list_collect(ptslots, slot);
          if (slot && init)
            ptinits = list_collect(ptinits, cons(slot, ffi_eval_expr(init, nil, nil)));
        }

        {
          val stype = or2(if2(name, find_struct_type(sname)),
                          make_struct_type(sname, nil, nil,
                                           slots, nil,
                                           if2(slot_inits, func_f1(slot_inits, ffi_struct_init)),
                                           nil, nil));
          val xsyntax = cons(struct_s,
                             cons(sname, membs));
          return make_ffi_type_struct(xsyntax, stype, existing_type, self);
        }
      }
    } else if (sym == union_s) {
      val name = cadr(syntax);
      val membs = cddr(syntax);
      val sname = if3(name, name, gensym(lit("ffi-union-")));
      val existing_type = if2(name, gethash(ffi_struct_tag_hash, sname));
      val xsyntax = cons(union_s,
                         cons(sname, membs));
      return make_ffi_type_union(xsyntax, existing_type, self);
    } else if (sym == array_s || sym == zarray_s) {
      if (length(syntax) == two) {
        val eltype_syntax = cadr(syntax);
        val eltype = ffi_type_compile(eltype_syntax);
        val type = make_ffi_type_pointer(syntax, vec_s,
                                         ffi_varray_put, ffi_void_get,
                                         ffi_varray_in, 0, ffi_varray_release,
                                         eltype);
        struct txr_ffi_type *tft = ffi_type_struct(type);
        struct txr_ffi_type *etft = ffi_type_struct(eltype);

        tft->kind = FFI_KIND_ARRAY;

        if (etft->incomplete || etft->bitfield)
          uw_throwf(error_s,
                    lit("~a: ~a ~s cannot be array element"),
                    self,
                    if3(etft->bitfield,
                        lit("bitfield"), lit("incomplete type")),
                    eltype_syntax, nao);
        if (sym == zarray_s) {
          tft->null_term = 1;
          tft->get = ffi_varray_null_term_get;
          tft->in = ffi_varray_null_term_in;
        }
        if (etft->syntax == char_s)
          tft->ch_conv = conv_char;
        else if (etft->syntax == zchar_s)
          tft->ch_conv = conv_zchar;
        else if (etft->syntax == wchar_s)
          tft->ch_conv = conv_wchar;
        else if (etft->syntax == bchar_s)
          tft->ch_conv = conv_bchar;
        tft->alloc = ffi_varray_alloc;
        tft->dynsize = ffi_varray_dynsize;
        tft->free = free;
        tft->size = 0;
        tft->incomplete = 1;
        return type;
      } else if (length(syntax) == three) {
        val dim = ffi_eval_expr(cadr(syntax), nil, nil);
        val eltype_syntax = caddr(syntax);
        val eltype = ffi_type_compile(eltype_syntax);
        val xsyntax = list(sym, dim, eltype_syntax, nao);
        struct txr_ffi_type *etft = ffi_type_struct(eltype);

        if (etft->incomplete || etft->bitfield)
          uw_throwf(error_s,
                    lit("~a: ~a ~s cannot be array element"),
                    self,
                    if3(etft->bitfield,
                        lit("bitfield"), lit("incomplete type")),
                    eltype_syntax, nao);

        if (minusp(dim))
          uw_throwf(error_s, lit("~a: negative dimension in ~s"),
                    self, syntax, nao);

        {
          val type = make_ffi_type_array(xsyntax, vec_s, dim, eltype, self);
          struct txr_ffi_type *tft = ffi_type_struct(type);

          if (sym == zarray_s) {
            tft->null_term = 1;
            if (zerop(dim))
              uw_throwf(error_s, lit("~a: zero dimension in ~s"),
                        self, syntax, nao);
          }

          if (etft->syntax == char_s)
            tft->ch_conv = conv_char;
          else if (etft->syntax == zchar_s)
            tft->ch_conv = conv_zchar;
          else if (etft->syntax == wchar_s)
            tft->ch_conv = conv_wchar;
          else if (etft->syntax == bchar_s)
            tft->ch_conv = conv_bchar;
          return type;
        }
      } else {
        goto excess;
      }
    } else if (sym == ptr_in_s) {
      val target_type = ffi_type_compile(cadr(syntax));
      if (cddr(syntax))
        goto excess;
      return make_ffi_type_pointer(syntax, ffi_get_lisp_type(self, target_type),
                                   ffi_ptr_in_put, ffi_ptr_get,
                                   ffi_ptr_in_in, ffi_ptr_in_out,
                                   ffi_ptr_in_release, target_type);
    } else if (sym == ptr_in_d_s) {
      val target_type = ffi_type_compile(cadr(syntax));
      if (cddr(syntax))
        goto excess;
      return make_ffi_type_pointer(syntax, ffi_get_lisp_type(self, target_type),
                                   ffi_ptr_in_put, ffi_ptr_d_get,
                                   ffi_ptr_in_d_in, ffi_ptr_in_out,
                                   ffi_ptr_in_release, target_type);
    } else if (sym == ptr_out_s) {
      val target_type = ffi_type_compile(cadr(syntax));
      if (cddr(syntax))
        goto excess;
      return make_ffi_type_pointer(syntax, ffi_get_lisp_type(self, target_type),
                                   ffi_ptr_out_put, ffi_ptr_get,
                                   ffi_ptr_out_in, ffi_ptr_out_out,
                                   ffi_simple_release, target_type);
    } else if (sym == ptr_out_d_s) {
      val target_type = ffi_type_compile(cadr(syntax));
      if (cddr(syntax))
        goto excess;
      return make_ffi_type_pointer(syntax, ffi_get_lisp_type(self, target_type),
                                   ffi_ptr_out_null_put, ffi_ptr_d_get,
                                   ffi_ptr_out_in, ffi_ptr_out_out,
                                   0, target_type);
    } else if (sym == ptr_s) {
      val target_type = ffi_type_compile(cadr(syntax));
      if (cddr(syntax))
        goto excess;
      return make_ffi_type_pointer(syntax, ffi_get_lisp_type(self, target_type),
                                   ffi_ptr_in_put, ffi_ptr_get,
                                   ffi_ptr_out_in, ffi_ptr_out_out,
                                   ffi_ptr_in_release, target_type);
    } else if (sym == ptr_out_s_s) {
      val target_type = ffi_type_compile(cadr(syntax));
      if (cddr(syntax))
        goto excess;
      return make_ffi_type_pointer(syntax, ffi_get_lisp_type(self, target_type),
                                   ffi_ptr_out_null_put, ffi_ptr_get,
                                   ffi_ptr_out_s_in, ffi_ptr_out_out,
                                   0, target_type);
    } else if (sym == buf_s || sym == buf_d_s) {
      val size = ffi_eval_expr(cadr(syntax), nil, nil);
      val xsyntax = list(sym, size, nao);
      cnum nelem = c_num(size, self);
      val type = make_ffi_type_builtin(xsyntax, buf_s, FFI_KIND_PTR,
                                       sizeof (mem_t *),
                                       alignof (mem_t *),
                                       &ffi_type_pointer,
                                       if3(sym == buf_s,
                                           ffi_buf_put, ffi_buf_d_put),
                                       if3(sym == buf_s,
                                           ffi_buf_get, ffi_buf_d_get),
                                       0, 0);
      struct txr_ffi_type *tft = ffi_type_struct(type);

      if (cddr(syntax))
        goto excess;

      if (nelem < 0)
        uw_throwf(error_s, lit("~a: negative size in ~s"),
                  self, syntax, nao);

      if (sym == buf_s) {
        tft->in = ffi_buf_in;
      } else {
        tft->in = ffi_buf_d_in;
        tft->release = ffi_simple_release;
      }

      tft->nelem = nelem;
      return type;
    } else if (sym == cptr_s) {
      val tag = cadr(syntax);
      val type = make_ffi_type_builtin(cptr_s, cptr_s, FFI_KIND_PTR,
                                       sizeof (mem_t *), alignof (mem_t *),
                                       &ffi_type_pointer,
                                       ffi_cptr_put, ffi_cptr_get, 0, 0);
      struct txr_ffi_type *tft = ffi_type_struct(type);
      tft->in = ffi_cptr_in;
      tft->alloc = ffi_cptr_alloc;
      tft->free = ffi_noop_free;
      tft->tag = tag;
      tft->eltype = gethash(ffi_typedef_hash, tag);
      if (cddr(syntax))
        goto excess;
      return type;
    } else if (sym == carray_s) {
      if (cddr(syntax)) {
        goto excess;
      } else {
        val eltype = ffi_type_compile(cadr(syntax));
        val type = make_ffi_type_pointer(syntax, carray_s,
                                         ffi_carray_put, ffi_carray_get,
                                         0, 0, 0, eltype);
        struct txr_ffi_type *tft = ffi_type_struct(type);
        tft->in = ffi_carray_in;
        return type;
      }
    } else if (sym == sbit_s || sym == ubit_s) {
      val nbits = ffi_eval_expr(cadr(syntax), nil, nil);
      cnum nb = c_num(nbits, self);
      val xsyntax = list(sym, nbits, nao);
      val type = make_ffi_type_builtin(xsyntax, integer_s,
                                       FFI_KIND_NUM,
                                       sizeof (int), alignof (int),
                                       &ffi_type_void,
                                       if3(sym == sbit_s,
                                           ffi_sbit_put, ffi_ubit_put),
                                       if3(sym == sbit_s,
                                           ffi_sbit_get, ffi_ubit_get),
                                       0, 0);
      struct txr_ffi_type *tft = ffi_type_struct(type);
      const int bits_int = 8 * sizeof(int);
      if (cddr(syntax))
        goto excess;
      if (nb < 0 || nb > bits_int)
        uw_throwf(error_s, lit("~a: invalid bitfield size ~s; "
                               "must be 0 to ~s"),
                  self, nbits, num_fast(bits_int), nao);
      tft->nelem = c_num(nbits, self);
      tft->bitfield = 1;
      if (nb == bits_int)
        tft->mask = UINT_MAX;
      else
        tft->mask = ((1U << nb) - 1);
      return type;
    } else if (sym == bit_s && !consp(cddr(syntax))) {
      goto toofew;
    } else if (sym == bit_s) {
      val nbits = ffi_eval_expr(cadr(syntax), nil, nil);
      cnum nb = c_num(nbits, self);
      val type_syntax = caddr(syntax);
      val xsyntax = list(sym, nbits, type_syntax, nao);
      val type = ffi_type_compile(type_syntax);
      struct txr_ffi_type *tft = ffi_type_struct(type);
      const int bits_int = 8 * sizeof(int);
      val type_copy = ffi_type_copy(type);
      struct txr_ffi_type *tft_cp = ffi_type_struct(type_copy);
      val syn = tft->syntax;
      int unsgnd = 0;

      if (cdddr(syntax))
        goto excess;

      if (syn == uint8_s || syn == uint16_s || syn == uint32_s ||
          syn == uchar_s || syn == ushort_s || syn == uint_s)
      {
        unsgnd = 1;
      } else if (syn != int8_s && syn != int16_s && syn != int32_s &&
                 syn != char_s && syn != short_s && syn != int_s)
      {
        uw_throwf(error_s, lit("~a: ~s not supported as bitfield type"),
                  self, type, nao);
      }

      if (nb < 0 || nb > bits_int)
        uw_throwf(error_s, lit("~a: invalid bitfield size ~s; "
                               "must be 0 to ~s"),
                  self, nbits, num_fast(bits_int), nao);
      tft_cp->syntax = xsyntax;
      tft_cp->nelem = nb;
      tft_cp->put = if3(unsgnd, ffi_generic_ubit_put, ffi_generic_sbit_put);
      tft_cp->get = if3(unsgnd, ffi_generic_ubit_get, ffi_generic_sbit_get);
      tft_cp->bitfield = 1;
      /* mask needed at type compilation time by (enumed (bit ...)) */
      if (nb == bits_int)
        tft_cp->mask = UINT_MAX;
      else
        tft_cp->mask = ((1U << nb) - 1);
      return type_copy;
    } else if (sym == enum_s) {
      val name = cadr(syntax);
      val enums = cddr(syntax);
      val xsyntax = cons(enum_s, cons(name, nil));
      if (name && !bindable(name))
        uw_throwf(error_s,
                  lit("~a: enum name ~s must be bindable symbol or nil"),
                  self, name, nao);
      return make_ffi_type_enum(xsyntax, enums, ffi_type_lookup(int_s), self);
    } else if (sym == enumed_s && !consp(cddr(syntax))) {
      goto toofew;
    } else if (sym == enumed_s) {
      val base_type_syntax = cadr(syntax);
      val name = caddr(syntax);
      val enums = cdddr(syntax);
      val xsyntax = list(enumed_s, base_type_syntax, name, nao);
      val base_type = ffi_type_compile(base_type_syntax);
      if (name && !bindable(name))
        uw_throwf(error_s,
                  lit("~a: enum name ~s must be bindable symbol or nil"),
                  self, name, nao);
      return make_ffi_type_enum(xsyntax, enums, base_type, self);
    } else if (sym == align_s && !consp(cddr(syntax))) {
      goto toofew;
    } else if (sym == align_s) {
      val align = ffi_eval_expr(cadr(syntax), nil, nil);
      ucnum al = c_num(align, self);
      if (cdddr(syntax))
        goto excess;
      if (al <= 0) {
        uw_throwf(error_s, lit("~a: alignment must be positive"),
                  self, nao);
      } else if (al != 0 && (al & (al - 1)) != 0) {
        uw_throwf(error_s, lit("~a: alignment must be a power of two"),
                  self, nao);
      } else {
        val alsyntax = caddr(syntax);
        val altype = ffi_type_compile(alsyntax);
        val altype_copy = ffi_type_copy(altype);
        struct txr_ffi_type *atft = ffi_type_struct(altype_copy);
        atft->align = al;
        return altype_copy;
      }
    } else if (sym == bool_s) {
      val type_syntax = cadr(syntax);
      val type = ffi_type_compile(type_syntax);
      val type_copy = ffi_type_copy(type);
      struct txr_ffi_type *tft = ffi_type_struct(type_copy);
      if (cddr(syntax))
        goto excess;
      if (tft->eltype || tft->memb != 0)
        uw_throwf(error_s, lit("~a: type ~s can't be basis for bool"),
                  self, tft->syntax, nao);
      tft->syntax = syntax;
      tft->eltype = type;
      tft->get = ffi_bool_get;
      tft->put = ffi_bool_put;
#if !HAVE_LITTLE_ENDIAN
      tft->rget = ffi_bool_rget;
      tft->rput = ffi_bool_rput;
#endif
      return type_copy;
    } else if (sym == qref_s) {
      val args = cdr(syntax);
      val type = nil;
      struct txr_ffi_type *tft = 0;

      for (; consp(args); args = cdr(args)) {
        val next = car(args);
        if (!tft) {
          type = ffi_type_compile(next);
          tft = ffi_type_struct(type);
          if (tft->clone != ffi_struct_clone)
            uw_throwf(error_s, lit("~a: ~s in ~s isn't a struct/union type"),
                      self, next, syntax, nao);
        } else {
          tft = ffi_find_memb(tft, next);
          if (!tft)
            uw_throwf(error_s, lit("~a: ~s in ~s is a nonexistent member"),
                      self, next, syntax, nao);
          type = tft->self;
        }
      }

      if (type == nil || args)
        uw_throwf(error_s, lit("~a: invalid ~s syntax"), self, sym, nao);

      return type;
    } else if (sym == elemtype_s) {
      val args = cdr(syntax);
      if (!consp(args) || cdr(args)) {
        uw_throwf(error_s, lit("~a: one argument required"), self, qref_s, nao);
      } else {
        val expr = car(args);
        val type = ffi_type_compile(expr);
        struct txr_ffi_type *tft = ffi_type_struct_checked(self, type);

        if (!tft->eltype) {
          uw_throwf(error_s, lit("~a: ~s isn't an array, pointer or enum"),
                    self, type, nao);
        }

        return tft->eltype;
      }
    }

    uw_throwf(error_s, lit("~a: unrecognized type operator: ~s"),
              self, sym, nao);
  } else {
    val sub = gethash(ffi_typedef_hash, syntax);

    if (sub != nil)
      return sub;

    uw_throwf(error_s, lit("~a: unrecognized type specifier: ~!~s"),
              self, syntax, nao);
  }

toofew:
  uw_throwf(error_s, lit("~a: missing arguments in ~s"),
            self, syntax, nao);

excess:
  uw_throwf(error_s, lit("~a: excess elements in ~s"),
            self, syntax, nao);
}

val ffi_type_operator_p(val sym)
{
  return tnil(sym == struct_s || sym == union_s || sym == array_s ||
              sym == zarray_s || sym == ptr_in_s || sym == ptr_in_d_s ||
              sym == ptr_out_s || sym == ptr_out_d_s || sym == ptr_s ||
              sym == ptr_out_s_s || sym == buf_s || sym == buf_d_s ||
              sym == cptr_s || sym == carray_s || sym == sbit_s ||
              sym == ubit_s || sym == bit_s || sym == enum_s ||
              sym == enumed_s || sym == align_s || sym == bool_s);
}

val ffi_type_p(val sym)
{
  return tnil(gethash(ffi_typedef_hash, sym));
}

static void ffi_init_types(void)
{
#if UCHAR_MAX == CHAR_MAX
  ffi_type *ffi_char = &ffi_type_uchar;
#else
  ffi_type *ffi_char = &ffi_type_schar;
#endif

#if HAVE_I8
  ffi_typedef(uint8_s, make_ffi_type_builtin(uint8_s, integer_s,
                                             FFI_KIND_NUM,
                                             sizeof (i8_t), alignof (i8_t),
                                             &ffi_type_uint8,
                                             ffi_u8_put, ffi_u8_get,
                                             ifbe(ffi_u8_rput),
                                             ifbe(ffi_u8_rget)));
  ffi_typedef(int8_s, make_ffi_type_builtin(int8_s, integer_s,
                                            FFI_KIND_NUM,
                                            sizeof (i8_t), alignof (i8_t),
                                            &ffi_type_sint8,
                                            ffi_i8_put, ffi_i8_get,
                                            ifbe(ffi_i8_rput),
                                            ifbe(ffi_i8_rget)));
#endif
#if HAVE_I16
  ffi_typedef(uint16_s, make_ffi_type_builtin(uint16_s, integer_s,
                                              FFI_KIND_NUM,
                                              sizeof (i16_t), alignof (i16_t),
                                              &ffi_type_uint16,
                                              ffi_u16_put, ffi_u16_get,
                                              ifbe(ffi_u16_rput),
                                              ifbe(ffi_u16_rget)));
  ffi_typedef(int16_s, make_ffi_type_builtin(int16_s, integer_s,
                                             FFI_KIND_NUM,
                                             sizeof (i16_t), alignof (i16_t),
                                             &ffi_type_sint16,
                                             ffi_i16_put, ffi_i16_get,
                                             ifbe(ffi_i16_rput),
                                             ifbe(ffi_i16_rget)));
#endif
#if HAVE_I32
  ffi_typedef(uint32_s, make_ffi_type_builtin(uint32_s, integer_s,
                                              FFI_KIND_NUM,
                                              sizeof (i32_t), alignof (i32_t),
                                              &ffi_type_uint32,
                                              ffi_u32_put, ffi_u32_get,
                                              ifbe(ffi_u32_rput),
                                              ifbe(ffi_u32_rget)));
  ffi_typedef(int32_s, make_ffi_type_builtin(int32_s, integer_s,
                                             FFI_KIND_NUM,
                                             sizeof (i32_t), alignof (i32_t),
                                             &ffi_type_sint32,
                                             ffi_i32_put, ffi_i32_get,
                                             ifbe(ffi_i32_rput),
                                             ifbe(ffi_i32_rget)));
#endif
#if HAVE_I64
  ffi_typedef(uint64_s, make_ffi_type_builtin(uint64_s, integer_s,
                                              FFI_KIND_NUM,
                                              sizeof (i64_t), alignof (i64_t),
                                              &ffi_type_uint64,
                                              ffi_u64_put, ffi_u64_get, 0, 0));
  ffi_typedef(int64_s, make_ffi_type_builtin(int64_s, integer_s,
                                             FFI_KIND_NUM,
                                             sizeof (i64_t), alignof (i64_t),
                                             &ffi_type_sint64,
                                             ffi_i64_put, ffi_i64_get, 0, 0));
#endif
  ffi_typedef(uchar_s, make_ffi_type_builtin(uchar_s, integer_s,
                                             FFI_KIND_NUM,
                                             1, 1,
                                             &ffi_type_uchar,
                                             ffi_uchar_put, ffi_uchar_get,
                                             ifbe(ffi_uchar_rput),
                                             ifbe(ffi_uchar_rget)));
  ffi_typedef(char_s, make_ffi_type_builtin(char_s, integer_s,
                                            FFI_KIND_NUM,
                                            1, 1,
                                            ffi_char, ffi_char_put,
                                            ffi_char_get,
                                            ifbe(ffi_char_rput),
                                            ifbe(ffi_char_rget)));
  ffi_typedef(zchar_s, make_ffi_type_builtin(zchar_s, integer_s,
                                             FFI_KIND_NUM,
                                             1, 1,
                                             ffi_char, ffi_char_put,
                                             ffi_char_get,
                                             ifbe(ffi_char_rput),
                                             ifbe(ffi_char_rget)));
  ffi_typedef(bchar_s, make_ffi_type_builtin(bchar_s, char_s,
                                             FFI_KIND_NUM,
                                             1, 1,
                                             &ffi_type_uchar,
                                             ffi_uchar_put, ffi_bchar_get,
                                             ifbe(ffi_uchar_rput),
                                             ifbe(ffi_bchar_rget)));
  ffi_typedef(wchar_s, make_ffi_type_builtin(wchar_s, char_s,
                                             FFI_KIND_NUM,
                                             sizeof (wchar_t),
                                             alignof (wchar_t),
                                             &ffi_type_wchar,
                                             ffi_wchar_put, ffi_wchar_get,
                                             ifbe(ffi_wchar_rput),
                                             ifbe(ffi_wchar_rget)));
  ffi_typedef(ushort_s, make_ffi_type_builtin(ushort_s, integer_s,
                                              FFI_KIND_NUM,
                                              sizeof (short), alignof (short),
                                              &ffi_type_ushort,
                                              ffi_ushort_put, ffi_ushort_get,
                                              ifbe(ffi_ushort_rput),
                                              ifbe(ffi_ushort_rget)));
  ffi_typedef(short_s, make_ffi_type_builtin(short_s, integer_s,
                                             FFI_KIND_NUM,
                                             sizeof (short), alignof (short),
                                             &ffi_type_sshort,
                                             ffi_short_put, ffi_short_get,
                                             ifbe(ffi_short_rput),
                                             ifbe(ffi_short_rget)));
  ffi_typedef(int_s, make_ffi_type_builtin(int_s, integer_s,
                                           FFI_KIND_NUM,
                                           sizeof (int), alignof (int),
                                           &ffi_type_sint,
                                           ffi_int_put, ffi_int_get,
                                           ifbe(ffi_int_rput),
                                           ifbe(ffi_int_rget)));
  ffi_typedef(uint_s, make_ffi_type_builtin(uint_s, integer_s,
                                            FFI_KIND_NUM,
                                            sizeof (int), alignof (int),
                                            &ffi_type_uint,
                                            ffi_uint_put, ffi_uint_get,
                                            ifbe(ffi_uint_rput),
                                            ifbe(ffi_uint_rget)));
  ffi_typedef(ulong_s, make_ffi_type_builtin(ulong_s, integer_s,
                                             FFI_KIND_NUM,
                                             sizeof (long), alignof (long),
                                             &ffi_type_ulong,
                                             ffi_ulong_put, ffi_ulong_get,
                                             ifbe(ffi_ulong_rput),
                                             ifbe(ffi_ulong_rget)));
  ffi_typedef(long_s, make_ffi_type_builtin(long_s, integer_s,
                                            FFI_KIND_NUM,
                                            sizeof (long), alignof (long),
                                            &ffi_type_slong,
                                            ffi_long_put, ffi_long_get,
                                            ifbe(ffi_long_rput),
                                            ifbe(ffi_long_rget)));
  ffi_typedef(float_s, make_ffi_type_builtin(float_s, float_s,
                                             FFI_KIND_NUM,
                                             sizeof (float), alignof (float),
                                             &ffi_type_float,
                                             ffi_float_put, ffi_float_get,
                                             0, 0));
  ffi_typedef(double_s, make_ffi_type_builtin(double_s, float_s,
                                              FFI_KIND_NUM,
                                              sizeof (double),
                                              alignof (double),
                                              &ffi_type_double,
                                              ffi_double_put, ffi_double_get,
                                              0, 0));
  ffi_typedef(val_s, make_ffi_type_builtin(val_s, t,
                                           FFI_KIND_PTR,
                                           sizeof (val),
                                           alignof (val),
                                           &ffi_type_pointer,
                                           ffi_val_put, ffi_val_get,
                                           0, 0));

#if HAVE_I16
  ffi_typedef(be_uint16_s, make_ffi_type_builtin(be_uint16_s, integer_s,
                                                 FFI_KIND_NUM,
                                                 sizeof (u16_t),
                                                 alignof (u16_t),
                                                 &ffi_type_uint16,
                                                 ffi_be_u16_put,
                                                 ffi_be_u16_get,
                                                 ifbe(ffi_be_u16_rput),
                                                 ifbe(ffi_be_u16_rget)));
  ffi_typedef(be_int16_s, make_ffi_type_builtin(be_int16_s, integer_s,
                                                FFI_KIND_NUM,
                                                sizeof (i16_t),
                                                alignof (i16_t),
                                                &ffi_type_sint16,
                                                ffi_be_i16_put,
                                                ffi_be_i16_get,
                                                ifbe(ffi_be_i16_rput),
                                                ifbe(ffi_be_i16_rget)));
#endif

#if HAVE_I32
  ffi_typedef(be_uint32_s, make_ffi_type_builtin(be_uint32_s, integer_s,
                                                 FFI_KIND_NUM,
                                                 sizeof (u32_t),
                                                 alignof (u32_t),
                                                 &ffi_type_uint32,
                                                 ffi_be_u32_put,
                                                 ffi_be_u32_get,
                                                 ifbe(ffi_be_u32_rput),
                                                 ifbe(ffi_be_u32_rget)));
  ffi_typedef(be_int32_s, make_ffi_type_builtin(be_int32_s, integer_s,
                                                FFI_KIND_NUM,
                                                sizeof (i32_t),
                                                alignof (i32_t),
                                                &ffi_type_sint32,
                                                ffi_be_i32_put,
                                                ffi_be_i32_get,
                                                ifbe(ffi_be_i32_rput),
                                                ifbe(ffi_be_i32_rget)));
#endif

#if HAVE_I64
  ffi_typedef(be_uint64_s, make_ffi_type_builtin(be_uint64_s, integer_s,
                                                 FFI_KIND_NUM,
                                                 sizeof (u64_t),
                                                 alignof (u64_t),
                                                 &ffi_type_uint64,
                                                 ffi_be_u64_put,
                                                 ffi_be_u64_get, 0, 0));
  ffi_typedef(be_int64_s, make_ffi_type_builtin(be_int64_s, integer_s,
                                                FFI_KIND_NUM,
                                                sizeof (i64_t),
                                                alignof (i64_t),
                                                &ffi_type_sint64,
                                                ffi_be_i64_put,
                                                ffi_be_i64_get, 0, 0));
#endif

  ffi_typedef(be_float_s, make_ffi_type_builtin(be_float_s, integer_s,
                                                FFI_KIND_NUM,
                                                sizeof (float),
                                                alignof (float),
                                                &ffi_type_float,
                                                ffi_be_float_put,
                                                ffi_be_float_get, 0, 0));
  ffi_typedef(be_double_s, make_ffi_type_builtin(be_double_s, integer_s,
                                                 FFI_KIND_NUM,
                                                 sizeof (double),
                                                 alignof (double),
                                                 &ffi_type_double,
                                                 ffi_be_double_put,
                                                 ffi_be_double_get, 0, 0));

#if HAVE_I16
  ffi_typedef(le_uint16_s, make_ffi_type_builtin(le_uint16_s, integer_s,
                                                 FFI_KIND_NUM,
                                                 sizeof (u16_t),
                                                 alignof (u16_t),
                                                 &ffi_type_uint16,
                                                 ffi_le_u16_put,
                                                 ffi_le_u16_get,
                                                 ifbe(ffi_le_u16_rput),
                                                 ifbe(ffi_le_u16_rget)));
  ffi_typedef(le_int16_s, make_ffi_type_builtin(le_int16_s, integer_s,
                                                FFI_KIND_NUM,
                                                sizeof (i16_t),
                                                alignof (i16_t),
                                                &ffi_type_sint16,
                                                ffi_le_i16_put,
                                                ffi_le_i16_get,
                                                ifbe(ffi_le_i16_rput),
                                                ifbe(ffi_le_i16_rget)));
#endif

#if HAVE_I32
  ffi_typedef(le_uint32_s, make_ffi_type_builtin(le_uint32_s, integer_s,
                                                 FFI_KIND_NUM,
                                                 sizeof (u32_t),
                                                 alignof (u32_t),
                                                 &ffi_type_uint32,
                                                 ffi_le_u32_put,
                                                 ffi_le_u32_get,
                                                 ifbe(ffi_le_u32_rput),
                                                 ifbe(ffi_le_u32_rget)));
  ffi_typedef(le_int32_s, make_ffi_type_builtin(le_int32_s, integer_s,
                                                FFI_KIND_NUM,
                                                sizeof (i32_t),
                                                alignof (i32_t),
                                                &ffi_type_sint32,
                                                ffi_le_i32_put,
                                                ffi_le_i32_get,
                                                ifbe(ffi_le_i32_rput),
                                                ifbe(ffi_le_i32_rget)));
#endif

#if HAVE_I64
  ffi_typedef(le_uint64_s, make_ffi_type_builtin(le_uint64_s, integer_s,
                                                 FFI_KIND_NUM,
                                                 sizeof (u64_t),
                                                 alignof (u64_t),
                                                 &ffi_type_uint64,
                                                 ffi_le_u64_put,
                                                 ffi_le_u64_get, 0, 0));
  ffi_typedef(le_int64_s, make_ffi_type_builtin(le_int64_s, integer_s,
                                                FFI_KIND_NUM,
                                                sizeof (i64_t),
                                                alignof (i64_t),
                                                &ffi_type_sint64,
                                                ffi_le_i64_put,
                                                ffi_le_i64_get, 0, 0));
#endif

  ffi_typedef(le_float_s, make_ffi_type_builtin(le_float_s, integer_s,
                                                FFI_KIND_NUM,
                                                sizeof (float),
                                                alignof (float),
                                                &ffi_type_float,
                                                ffi_le_float_put,
                                                ffi_le_float_get, 0, 0));
  ffi_typedef(le_double_s, make_ffi_type_builtin(le_double_s, integer_s,
                                                 FFI_KIND_NUM,
                                                 sizeof (double),
                                                 alignof (double),
                                                 &ffi_type_double,
                                                 ffi_le_double_put,
                                                 ffi_le_double_get, 0, 0));
  {
    val type = make_ffi_type_builtin(cptr_s, cptr_s, FFI_KIND_PTR,
                                     sizeof (mem_t *), alignof (mem_t *),
                                     &ffi_type_pointer,
                                     ffi_cptr_put, ffi_cptr_get, 0, 0);
    struct txr_ffi_type *tft = ffi_type_struct(type);
    tft->in = ffi_cptr_in;
    tft->alloc = ffi_cptr_alloc;
    tft->free = ffi_noop_free;
    tft->tag = nil;
    ffi_typedef(cptr_s, type);
  }

  {
    val type = make_ffi_type_builtin(str_s, str_s, FFI_KIND_PTR,
                                     sizeof (mem_t *), alignof (mem_t *),
                                     &ffi_type_pointer,
                                     ffi_str_put, ffi_str_get, 0, 0);
    struct txr_ffi_type *tft = ffi_type_struct(type);
    tft->in = ffi_str_in;
    tft->release = ffi_simple_release;
    tft->by_value_in = 1;
    ffi_typedef(str_s, type);
  }

  {
    val type = make_ffi_type_builtin(bstr_s, str_s, FFI_KIND_PTR,
                                     sizeof (mem_t *), alignof (mem_t *),
                                     &ffi_type_pointer,
                                     ffi_bstr_put, ffi_bstr_get, 0, 0);
    struct txr_ffi_type *tft = ffi_type_struct(type);
    tft->in = ffi_bstr_in;
    tft->release = ffi_simple_release;
    tft->by_value_in = 1;
    ffi_typedef(bstr_s, type);
  }

  ffi_typedef(str_d_s, make_ffi_type_builtin(str_d_s, str_s, FFI_KIND_PTR,
                                             sizeof (mem_t *), alignof (mem_t *),
                                             &ffi_type_pointer,
                                             ffi_str_put, ffi_str_d_get, 0, 0));
  {
    val type = ffi_typedef(wstr_s, make_ffi_type_builtin(wstr_s, str_s,
                                                         FFI_KIND_PTR,
                                                         sizeof (mem_t *),
                                                         alignof (mem_t *),
                                                         &ffi_type_pointer,
                                                         ffi_wstr_put,
                                                         ffi_wstr_get, 0, 0));
    struct txr_ffi_type *tft = ffi_type_struct(type);
    tft->in = ffi_wstr_in;
    tft->release = ffi_simple_release;
    tft->by_value_in = 1;
    ffi_typedef(wstr_s, type);
  }

  ffi_typedef(wstr_d_s, make_ffi_type_builtin(wstr_d_s, str_s, FFI_KIND_PTR,
                                              sizeof (mem_t *),
                                              alignof (mem_t *),
                                              &ffi_type_pointer,
                                              ffi_wstr_put, ffi_wstr_d_get,
                                              0, 0));
  ffi_typedef(bstr_d_s, make_ffi_type_builtin(bstr_d_s, str_s, FFI_KIND_PTR,
                                              sizeof (mem_t *),
                                              alignof (mem_t *),
                                              &ffi_type_pointer,
                                              ffi_bstr_put, ffi_bstr_d_get,
                                              0, 0));

  {
    val iter;

    for (iter = list(buf_s, buf_d_s, nao); iter; iter = cdr(iter)) {
      val sym = car(iter);
      ffi_typedef(sym, make_ffi_type_builtin(sym, buf_s, FFI_KIND_PTR,
                                             sizeof (mem_t *),
                                             alignof (mem_t *),
                                             &ffi_type_pointer,
                                             if3(sym == buf_s,
                                                 ffi_buf_put, ffi_buf_d_put),
                                             ffi_void_get, 0, 0));
    }
  }

#if HAVE_LIBFFI
  ffi_typedef(closure_s, make_ffi_type_builtin(closure_s, fun_s, FFI_KIND_PTR,
                                               sizeof (mem_t *),
                                               alignof (mem_t *),
                                               &ffi_type_pointer,
                                               ffi_closure_put, ffi_cptr_get,
                                               0, 0));
#endif

  {
    val type = ffi_typedef(void_s, make_ffi_type_builtin(void_s, null_s,
                                                         FFI_KIND_VOID,
                                                         0, 0,
                                                         &ffi_type_void,
                                                         ffi_void_put,
                                                         ffi_void_get,
                                                         0, 0));
    struct txr_ffi_type *tft = ffi_type_struct(type);
    tft->incomplete = 1;
  }

  ffi_typedef(bool_s, ffi_type_compile(cons(bool_s, cons(uchar_s, nil))));
}

static void ffi_init_extra_types(void)
{
  val type_by_size[2][18] = { { 0 }, { 0 } };

#if HAVE_I64
  type_by_size[0][sizeof (i64_t)] = ffi_type_lookup(int64_s);
  type_by_size[1][sizeof (i64_t)] = ffi_type_lookup(uint64_s);
#endif
#if HAVE_I32
  type_by_size[0][sizeof (i32_t)] = ffi_type_lookup(int32_s);
  type_by_size[1][sizeof (i32_t)] = ffi_type_lookup(uint32_s);
#endif
#if HAVE_I16
  type_by_size[0][sizeof (i16_t)] = ffi_type_lookup(int16_s);
  type_by_size[1][sizeof (i16_t)] = ffi_type_lookup(uint16_s);
#endif
#if HAVE_I8
  type_by_size[0][sizeof (i8_t)] = ffi_type_lookup(int8_s);
  type_by_size[1][sizeof (i8_t)] = ffi_type_lookup(uint8_s);
#endif
  type_by_size[0][sizeof (long)] = ffi_type_lookup(long_s);
  type_by_size[1][sizeof (long)] = ffi_type_lookup(ulong_s);
  type_by_size[0][sizeof (int)] = ffi_type_lookup(int_s);
  type_by_size[1][sizeof (int)] = ffi_type_lookup(uint_s);
  type_by_size[0][sizeof (short)] = ffi_type_lookup(short_s);
  type_by_size[1][sizeof (short)] = ffi_type_lookup(ushort_s);

  ffi_typedef(intern(lit("size-t"), user_package),
              type_by_size[1][sizeof (size_t)]);
  ffi_typedef(intern(lit("time-t"), user_package),
              type_by_size[convert(time_t, -1) > 0][sizeof (time_t)]);
  ffi_typedef(intern(lit("clock-t"), user_package),
              if3(convert(clock_t, 0.5) == 0,
                  type_by_size[convert(clock_t, -1) > 0][sizeof (clock_t)],
                  if3(sizeof (clock_t) == sizeof (float),
                      ffi_type_lookup(float_s),
                      if2(sizeof (clock_t) == sizeof (double),
                          ffi_type_lookup(double_s)))));
  ffi_typedef(intern(lit("int-ptr-t"), user_package),
              type_by_size[convert(int_ptr_t, -1) > 0][sizeof (int_ptr_t)]);
  ffi_typedef(intern(lit("uint-ptr-t"), user_package),
              type_by_size[convert(uint_ptr_t, -1) > 0][sizeof (uint_ptr_t)]);
  ffi_typedef(intern(lit("sig-atomic-t"), user_package),
              type_by_size[convert(sig_atomic_t, -1) > 0][sizeof (sig_atomic_t)]);
  ffi_typedef(intern(lit("ptrdiff-t"), user_package),
              type_by_size[convert(ptrdiff_t, -1) > 0][sizeof (ptrdiff_t)]);
  ffi_typedef(intern(lit("wint-t"), user_package),
              type_by_size[convert(wint_t, -1) > 0][sizeof (wint_t)]);

#if HAVE_SYS_TYPES_H
  ffi_typedef(intern(lit("blkcnt-t"), user_package),
              type_by_size[convert(blkcnt_t, -1) > 0][sizeof (blkcnt_t)]);
  ffi_typedef(intern(lit("blksize-t"), user_package),
              type_by_size[convert(blksize_t, -1) > 0][sizeof (blksize_t)]);
#if HAVE_CLOCKID_T
  ffi_typedef(intern(lit("clockid-t"), user_package),
              type_by_size[convert(clockid_t, -1) > 0][sizeof (clockid_t)]);
#endif
  ffi_typedef(intern(lit("dev-t"), user_package),
              type_by_size[convert(dev_t, -1) > 0][sizeof (dev_t)]);
  ffi_typedef(intern(lit("fsblkcnt-t"), user_package),
              type_by_size[convert(fsblkcnt_t, -1) > 0][sizeof (fsblkcnt_t)]);
  ffi_typedef(intern(lit("fsfilcnt-t"), user_package),
              type_by_size[convert(fsfilcnt_t, -1) > 0][sizeof (fsfilcnt_t)]);
  ffi_typedef(intern(lit("gid-t"), user_package),
              type_by_size[convert(gid_t, -1) > 0][sizeof (gid_t)]);
  ffi_typedef(intern(lit("id-t"), user_package),
              type_by_size[convert(id_t, -1) > 0][sizeof (id_t)]);
  ffi_typedef(intern(lit("ino-t"), user_package),
              type_by_size[convert(ino_t, -1) > 0][sizeof (ino_t)]);
  ffi_typedef(intern(lit("key-t"), user_package),
              type_by_size[convert(key_t, -1) > 0][sizeof (key_t)]);
#if HAVE_LOFF_T
  ffi_typedef(intern(lit("loff-t"), user_package),
              type_by_size[convert(loff_t, -1) > 0][sizeof (loff_t)]);
#endif
  ffi_typedef(intern(lit("mode-t"), user_package),
              type_by_size[convert(mode_t, -1) > 0][sizeof (mode_t)]);
  ffi_typedef(intern(lit("nlink-t"), user_package),
              type_by_size[convert(nlink_t, -1) > 0][sizeof (nlink_t)]);
  ffi_typedef(intern(lit("off-t"), user_package),
              type_by_size[convert(off_t, -1) > 0][sizeof (off_t)]);
  ffi_typedef(intern(lit("pid-t"), user_package),
              type_by_size[convert(pid_t, -1) > 0][sizeof (pid_t)]);
  ffi_typedef(intern(lit("ssize-t"), user_package),
              type_by_size[convert(ssize_t, -1) > 0][sizeof (ssize_t)]);
  ffi_typedef(intern(lit("uid-t"), user_package),
              type_by_size[convert(uid_t, -1) > 0][sizeof (uid_t)]);
#endif

#if HAVE_SOCKETS
  ffi_typedef(intern(lit("socklen-t"), user_package),
              type_by_size[convert(socklen_t, -1) > 0][sizeof (socklen_t)]);
#endif

  ffi_typedef(intern(lit("longlong"), user_package),
              type_by_size[0][sizeof (long long)]);
  ffi_typedef(intern(lit("ulonglong"), user_package),
              type_by_size[1][sizeof (long long)]);
}

#if HAVE_LIBFFI

struct txr_ffi_call_desc {
  ffi_cif cif;
  ffi_type **args;
  int variadic;
  cnum nfixed, ntotal;
  val argtypes;
  val rettype;
  val name;
};

static struct txr_ffi_call_desc *ffi_call_desc(val obj)
{
  return coerce(struct txr_ffi_call_desc *, obj->co.handle);
}

static struct txr_ffi_call_desc *ffi_call_desc_checked(val self, val obj)
{
  return coerce(struct txr_ffi_call_desc *, cobj_handle(self, obj,
                                                        ffi_call_desc_cls));
}

static void ffi_call_desc_print_op(val obj, val out,
                                   val pretty, struct strm_ctx *ctx)
{
  struct txr_ffi_call_desc *tfcd = ffi_call_desc(obj);
  put_string(lit("#<"), out);
  obj_print_impl(obj->co.cls->cls_sym, out, pretty, ctx);
  format(out, lit(" ~s ~s ~!~s>"), tfcd->name, tfcd->rettype,
         tfcd->argtypes, nao);
}

static void ffi_call_desc_destroy_op(val obj)
{
  struct txr_ffi_call_desc *tfcd = ffi_call_desc(obj);
  free(tfcd->args);
  tfcd->args = 0;
  free(tfcd);
}

static void ffi_call_desc_mark_op(val obj)
{
  struct txr_ffi_call_desc *tfcd = ffi_call_desc(obj);
  gc_mark(tfcd->argtypes);
  gc_mark(tfcd->rettype);
  gc_mark(tfcd->name);
}

static struct cobj_ops ffi_call_desc_ops =
  cobj_ops_init(eq,
                ffi_call_desc_print_op,
                ffi_call_desc_destroy_op,
                ffi_call_desc_mark_op,
                cobj_eq_hash_op);

val ffi_make_call_desc(val ntotal, val nfixed, val rettype, val argtypes,
                       val name_in)
{
  val name = default_null_arg(name_in);
  val self = if3(name, name, lit("ffi-make-call-desc"));
  cnum nt = c_num(ntotal, self), i;
  cnum nf = c_num(default_arg(nfixed, ntotal), self);
  struct txr_ffi_call_desc *tfcd = coerce(struct txr_ffi_call_desc *,
                                          chk_calloc(1, sizeof *tfcd));
  ffi_type **args = coerce(ffi_type **, chk_xalloc(nt, sizeof *args, self));
  val obj = cobj(coerce(mem_t *, tfcd), ffi_call_desc_cls, &ffi_call_desc_ops);
  ffi_status ffis = FFI_OK;

  tfcd->variadic = (nt != nf);
  tfcd->nfixed = nf;
  tfcd->ntotal = nt;
  tfcd->argtypes = argtypes;
  tfcd->rettype = rettype;
  tfcd->args = args;
  tfcd->name = name;

  for (i = 0; i < nt; i++) {
    val type = pop(&argtypes);
    struct txr_ffi_type *tft = ffi_type_struct_checked(self, type);
    if (tft->incomplete)
      uw_throwf(error_s, lit("~a: can't pass incomplete type ~s by value"),
                self, type, nao);
    if (tft->bitfield)
      uw_throwf(error_s, lit("~a: can't pass bitfield as argument"),
                self, nao);
    if (tft->calcft != 0) {
      tft->calcft(tft);
      tft->calcft = 0;
    }
    args[i] = tft->ft;
  }

  {
    struct txr_ffi_type *tft = ffi_type_struct_checked(self, rettype);
    if (tft->incomplete && tft->ft != &ffi_type_void)
      uw_throwf(error_s, lit("~a: can't return incomplete type ~s by value"),
                self, rettype, nao);
    if (tft->bitfield)
      uw_throwf(error_s, lit("~a: can't return bitfield from function"),
                self, nao);
  }

  if (tfcd->variadic)
    ffis = ffi_prep_cif_var(&tfcd->cif, FFI_DEFAULT_ABI, nf, nt,
                            ffi_get_type(self, rettype), args);
  else
    ffis = ffi_prep_cif(&tfcd->cif, FFI_DEFAULT_ABI, nt,
                        ffi_get_type(self, rettype), args);

  if (ffis != FFI_OK)
    uw_throwf(error_s, lit("~a: ffi_prep_cif failed: ~s"),
              self, num(ffis), nao);

  return obj;
}

val ffi_call_wrap(val fptr, val ffi_call_desc, struct args *args)
{
  val real_self = lit("ffi-call");
  struct txr_ffi_call_desc *tfcd = ffi_call_desc_checked(real_self, ffi_call_desc);
  val self = if3(tfcd->name, tfcd->name, real_self);
  mem_t *fp = cptr_get(fptr);
  cnum n = tfcd->ntotal;
  void **values = convert(void **, alloca(sizeof *values * tfcd->ntotal));
  val types = tfcd->argtypes;
  val rtype = tfcd->rettype;
  struct txr_ffi_type *rtft = ffi_type_struct(rtype);
  void *rc = alloca(pad_retval(rtft->size));
  int in_pass_needed = 0;
  volatile int cleanup_needed = 1;
  volatile cnum i;
  val ret;
  struct txr_ffi_type **type = convert(struct txr_ffi_type **,
                                       alloca(n * sizeof *type));

  if (args->argc < n) {
    args_decl(args_copy, n);
    args_copy_zap(args_copy, args);
    args = args_copy;
  }

  args_normalize_least(args, n);

  if (args->fill < n || args->list)
    uw_throwf(error_s, lit("~a: ~s requires ~s arguments"),
              self, ffi_call_desc, num(n), nao);

  for (i = 0; i < n; i++) {
    struct txr_ffi_type *mtft = type[i] = ffi_type_struct(pop(&types));
    values[i] = zalloca(mtft->size);
    in_pass_needed = in_pass_needed || mtft->by_value_in;
  }

  uw_simple_catch_begin;

  for (i = 0; i < n; i++) {
    struct txr_ffi_type *mtft = type[i];
    mtft->put(mtft, args->arg[i], convert(mem_t *, values[i]), self);
  }

  cleanup_needed = 0;

  uw_unwind {
    if (cleanup_needed && in_pass_needed) {
      cnum nreached = i;
      for (i = 0; i < nreached; i++) {
        struct txr_ffi_type *mtft = type[i];
        if (mtft->release != 0)
          mtft->release(mtft, args->arg[i], convert(mem_t *, values[i]), self);
      }
    }
  }

  uw_catch_end;

  ffi_call(&tfcd->cif, coerce(void (*)(void), fp), rc, values);

  ret = ifbe2(rtft->rget, rtft->get)(rtft, convert(mem_t *, rc), self);

  if (in_pass_needed) {
    for (i = 0; i < n; i++) {
      struct txr_ffi_type *mtft = type[i];
      if (mtft->by_value_in)
        mtft->in(mtft, 0, convert(mem_t *, values[i]), args->arg[i], self);
    }
  }

  if (s_exit_point) {
    uw_frame_t *ep = s_exit_point;
    s_exit_point = 0;
    uw_continue(ep);
  }

  return ret;
}

static void ffi_closure_dispatch(ffi_cif *cif, void *cret,
                                 void *cargs[], void *clo)
{
  val closure = coerce(val, clo);
  struct txr_ffi_closure *tfcl = ffi_closure_struct(closure);
  cnum i, nargs = tfcl->nparam;
  struct txr_ffi_call_desc *tfcd = tfcl->tfcd;
  val self = if3(tfcd->name, tfcd->name, lit("ffi-closure-dispatch"));
  val types = tfcd->argtypes;
  val rtype = tfcd->rettype;
  struct txr_ffi_type *rtft = ffi_type_struct(rtype);
  val retval = nil;
  int out_pass_needed = 0;
  args_decl(args, nargs);
  args_decl(args_cp, nargs);

  (void) cif;

  for (i = 0; i < nargs; i++) {
    val type = pop(&types);
    struct txr_ffi_type *mtft = ffi_type_struct(type);
    val arg = mtft->get(mtft, convert(mem_t *, cargs[i]), self);
    args_add(args, arg);
    if (mtft->out != 0)
      out_pass_needed = 1;
  }

  args_copy(args_cp, args);

  retval = generic_funcall(tfcl->fun, args);

  if (out_pass_needed) {
    for (types = tfcd->argtypes, i = 0; i < nargs; i++) {
      val type = pop(&types);
      val arg = args_at(args_cp, i);
      struct txr_ffi_type *mtft = ffi_type_struct(type);
      if (mtft->out != 0)
        mtft->out(mtft, 0, arg, convert(mem_t *, cargs[i]), self);
    }
  }

  ifbe2(rtft->rput, rtft->put)(rtft, retval, convert(mem_t *, cret), self);
}

static void ffi_closure_dispatch_safe(ffi_cif *cif, void *cret,
                                      void *cargs[], void *clo)
{
  val closure = coerce(val, clo);
  struct txr_ffi_closure *tfcl = ffi_closure_struct(closure);
  cnum i, nargs = tfcl->nparam;
  struct txr_ffi_call_desc *tfcd = tfcl->tfcd;
  val self = if3(tfcd->name, tfcd->name, lit("ffi-closure-dispatch-safe"));
  val types = tfcd->argtypes;
  val rtype = tfcd->rettype;
  struct txr_ffi_type *rtft = ffi_type_struct(rtype);
  volatile val retval = nao;
  int out_pass_needed = 0;
  size_t rsize = pad_retval(rtft->size);
  uw_frame_t cont_guard;

  (void) cif;

  if (rtft->release != 0)
    memset(cret, 0, rsize);

  s_exit_point = 0;

  uw_push_guard(&cont_guard, 0);

  uw_simple_catch_begin;

  {
    args_decl(args, nargs);
    args_decl(args_cp, nargs);

    for (i = 0; i < nargs; i++) {
      val type = pop(&types);
      struct txr_ffi_type *mtft = ffi_type_struct(type);
      val arg = mtft->get(mtft, convert(mem_t *, cargs[i]), self);
      args_add(args, arg);
      if (mtft->out != 0)
        out_pass_needed = 1;
    }

    args_copy(args_cp, args);

    retval = generic_funcall(tfcl->fun, args);

    if (out_pass_needed) {
      for (types = tfcd->argtypes, i = 0; i < nargs; i++) {
        val type = pop(&types);
        val arg = args_at(args_cp, i);
        struct txr_ffi_type *mtft = ffi_type_struct(type);
        if (mtft->out != 0)
          mtft->out(mtft, 0, arg, convert(mem_t *, cargs[i]), self);
      }
    }

    ifbe2(rtft->rput, rtft->put)(rtft, retval, convert(mem_t *, cret), self);
  }

  uw_unwind {
    s_exit_point = uw_curr_exit_point;
    if (s_exit_point) {
      if (rtft->release != 0 && retval != nao)
        rtft->release(rtft, retval, convert(mem_t *, cret), self);
      if (!tfcl->abort_retval)
        memset(cret, 0, rsize);
      else
        ifbe2(rtft->rput, rtft->put)(rtft, tfcl->abort_retval,
                                     convert(mem_t *, cret), self);
    }
    uw_curr_exit_point = 0; /* stops unwinding */
  }

  uw_catch_end;

  uw_pop_frame(&cont_guard);
}


val ffi_make_closure(val fun, val call_desc, val safe_p_in, val abort_ret_in)
{
  val real_self = lit("ffi-make-closure");
  struct txr_ffi_closure *tfcl = coerce(struct txr_ffi_closure *,
                                        chk_calloc(1, sizeof *tfcl));
  struct txr_ffi_call_desc *tfcd = ffi_call_desc_checked(real_self, call_desc);
  val self = if3(tfcd->name, tfcd->name, real_self);
  val obj = cobj(coerce(mem_t *, tfcl), ffi_closure_cls, &ffi_closure_ops);
  val safe_p = default_arg_strict(safe_p_in, t);
  ffi_status ffis = FFI_OK;

  tfcl->clo = convert(ffi_closure *,
                      ffi_closure_alloc(sizeof *tfcl->clo,
                                        coerce(void **, &tfcl->fptr)));

  if (!tfcl->clo)
    uw_throwf(error_s, lit("~a: failed to allocate special closure memory"),
              self, nao);

  if ((ffis = ffi_prep_closure_loc(tfcl->clo, &tfcd->cif,
                                   if3(safe_p,
                                       ffi_closure_dispatch_safe,
                                       ffi_closure_dispatch),
                                   obj,
                                   coerce(void *, tfcl->fptr))) != FFI_OK)
    uw_throwf(error_s, lit("~a: ffi_prep_closure_loc failed: ~s"),
              self, num(ffis), nao);

  tfcl->nparam = tfcd->ntotal;
  tfcl->fun = fun;
  tfcl->call_desc = call_desc;
  tfcl->tfcd = tfcd;
  tfcl->abort_retval = default_null_arg(abort_ret_in);

  return obj;
}

mem_t *ffi_closure_get_fptr(val self, val closure)
{
  struct txr_ffi_closure *tfcl = ffi_closure_struct_checked(self, closure);
  return tfcl->fptr;
}

#endif

val ffi_typedef(val name, val type)
{
  val self = lit("ffi-typedef");
  struct txr_ffi_type *tft = ffi_type_struct_checked(self, type);
  if (tft->bitfield)
    uw_throwf(error_s, lit("~a: cannot create a typedef for bitfield type"),
              self, nao);
  return sethash(ffi_typedef_hash, name, type);
}

val ffi_size(val type)
{
  val self = lit("ffi-size");
  struct txr_ffi_type *tft = ffi_type_struct_checked(self, type);
  if (tft->bitfield)
    uw_throwf(error_s, lit("~a: bitfield type ~s has no size"),
              self, type, nao);
  return num(tft->size);
}

val ffi_alignof(val type)
{
  val self = lit("ffi-alignof");
  struct txr_ffi_type *tft = ffi_type_struct_checked(self, type);
  if (tft->bitfield)
    uw_throwf(error_s, lit("~a: bitfield type ~s has no alignment"),
              self, type, nao);
  return num(tft->align);
}

val ffi_offsetof(val type, val memb)
{
  val self = lit("ffi-offsetof");
  struct txr_ffi_type *tft = ffi_type_struct_checked(self, type);
  cnum i;

  if (!tft->memb)
    uw_throwf(error_s, lit("~a: ~s isn't a struct type"), self, type, nao);

  for (i = 0; i < tft->nelem; i++) {
    struct smemb *pmemb = tft->memb + i;

    if (pmemb->mname == memb) {
      if (pmemb->mtft->mask != 0)
        uw_throwf(error_s, lit("~a: ~s is a bitfield in ~s"), self,
                  memb, type, nao);
      return num(tft->memb[i].offs);
    }
  }

  uw_throwf(error_s, lit("~a: ~s has no member ~s"), self, type, memb, nao);
}

val ffi_arraysize(val type)
{
  val self = lit("ffi-put-into");
  struct txr_ffi_type *tft = ffi_type_struct_checked(self, type);
  if (!tft->eltype)
    uw_throwf(error_s, lit("~a: ~s isn't an array"), self, type, nao);
  return num(tft->nelem);
}

val ffi_elemsize(val type)
{
  val self = lit("ffi-elemsize");
  struct txr_ffi_type *tft = ffi_type_struct_checked(self, type);
  if (!tft->eltype) {
    uw_throwf(error_s, lit("~a: ~s isn't an array, pointer or enum"),
              self, type, nao);
  } else {
    struct txr_ffi_type *etft = ffi_type_struct(tft->eltype);
    return num(etft->size);
  }
}

val ffi_elemtype(val type)
{
  val self = lit("ffi-elemtype");
  struct txr_ffi_type *tft = ffi_type_struct_checked(self, type);
  val eltype = tft->eltype;

  if (!eltype) {
    uw_throwf(error_s, lit("~a: ~s isn't an array, pointer or enum"),
              self, type, nao);
  }

  return eltype;
}

val ffi_put_into(val dstbuf, val obj, val type, val offset_in)
{
  val self = lit("ffi-put-into");
  struct txr_ffi_type *tft = ffi_type_struct_checked(self, type);
  mem_t *dst = buf_get(dstbuf, self);
  val offset = default_arg(offset_in, zero);
  cnum offsn = c_num(offset, self);
  cnum room = c_num(minus(length_buf(dstbuf), offset), self);
  cnum size = tft->dynsize(tft, obj, self);
  if (offsn < 0)
    uw_throwf(error_s, lit("~a: negative offset ~s specified"),
              self, offset, nao);
  if (room < size)
    uw_throwf(error_s, lit("~a: buffer ~s is too small for type ~s at offset ~s"),
              self, dstbuf, type, offset, nao);
  tft->put(tft, obj, dst + offsn, self);
  return dstbuf;
}

val ffi_put(val obj, val type)
{
  val self = lit("ffi-put");
  struct txr_ffi_type *tft = ffi_type_struct_checked(self, type);
  val buf = make_buf(num(tft->dynsize(tft, obj, self)), zero, nil);
  mem_t *dst = buf_get(buf, self);
  tft->put(tft, obj, dst, self);
  return buf;
}

val ffi_in(val srcbuf, val obj, val type, val copy_p, val offset_in)
{
  val self = lit("ffi-in");
  struct txr_ffi_type *tft = ffi_type_struct_checked(self, type);
  mem_t *src = buf_get(srcbuf, self);
  val offset = default_arg(offset_in, zero);
  cnum offsn = c_num(offset, self);
  cnum room = c_num(minus(length_buf(srcbuf), offset), self);
  cnum size = tft->dynsize(tft, obj, self);
  if (offsn < 0)
    uw_throwf(error_s, lit("~a: negative offset ~s specified"),
              self, offset, nao);
  if (room < size)
    uw_throwf(error_s, lit("~a: buffer ~s is too small for type ~s at offset ~s"),
              self, srcbuf, type, offset, nao);
  if (tft->in != 0)
    return tft->in(tft, copy_p != nil, src + offsn, obj, self);
  else if (copy_p)
    return tft->get(tft, src + offsn, self);
  return obj;
}

val ffi_get(val srcbuf, val type, val offset_in)
{
  val self = lit("ffi-get");
  struct txr_ffi_type *tft = ffi_type_struct_checked(self, type);
  mem_t *src = buf_get(srcbuf, self);
  val offset = default_arg(offset_in, zero);
  cnum offsn = c_num(offset, self);
  cnum room = c_num(minus(length_buf(srcbuf), offset), self);
  if (offsn < 0)
    uw_throwf(error_s, lit("~a: negative offset ~s specified"),
              self, offset, nao);
  if (room < tft->size)
    uw_throwf(error_s, lit("~a: buffer ~s is too small for type ~s at offset ~s"),
              self, srcbuf, type, offset, nao);
  return tft->get(tft, src + offsn, self);
}

val ffi_out(val dstbuf, val obj, val type, val copy_p, val offset_in)
{
  val self = lit("ffi-out");
  struct txr_ffi_type *tft = ffi_type_struct_checked(self, type);
  mem_t *dst = buf_get(dstbuf, self);
  val offset = default_arg(offset_in, zero);
  cnum offsn = c_num(offset, self);
  cnum room = c_num(minus(length_buf(dstbuf), offset), self);
  cnum size = tft->dynsize(tft, obj, self);
  if (offsn < 0)
    uw_throwf(error_s, lit("~a: negative offset ~s specified"),
              self, offset, nao);
  if (room < size)
    uw_throwf(error_s, lit("~a: buffer ~s is too small for type ~s at offset ~s"),
              self, dstbuf, type, offset, nao);
  if (tft->out != 0)
    tft->out(tft, copy_p != nil, obj, dst + offsn, self);
  else
    tft->put(tft, obj, dst + offsn, self);
  return dstbuf;
}

struct carray {
  val eltype;
  struct txr_ffi_type *eltft;
  mem_t *data;
  cnum nelem;
  val ref;
  cnum offs;
  val artype[2];
#if HAVE_MMAP
  size_t mm_len;
#endif
};

static struct carray *carray_struct(val carray)
{
  return coerce(struct carray*, carray->co.handle);
}

static struct carray *carray_struct_checked(val self, val carray)
{
  return coerce(struct carray*, cobj_handle(self, carray, carray_cls));
}

static void carray_print_op(val obj, val out, val pretty, struct strm_ctx *ctx)
{
  struct carray *scry = carray_struct(obj);
  put_string(lit("#<"), out);
  obj_print_impl(obj->co.cls->cls_sym, out, pretty, ctx);
  format(out, lit(" ~a"), if3(scry->nelem < 0,
                              lit("unknown-len"), num(scry->nelem)), nao);
  format(out, lit(" ~s>"), scry->eltype, nao);
}

static void carray_mark_op(val obj)
{
  struct carray *scry = carray_struct(obj);
  gc_mark(scry->eltype);
  gc_mark(scry->ref);
  gc_mark(scry->artype[0]);
  gc_mark(scry->artype[1]);
}

static void carray_destroy_op(val obj)
{
  struct carray *scry = carray_struct(obj);
  free(scry->data);
  scry->data = 0;
  free(scry);
}

static struct cobj_ops carray_borrowed_ops =
  cobj_ops_init(eq,
                carray_print_op,
                cobj_destroy_free_op,
                carray_mark_op,
                cobj_eq_hash_op);

static struct cobj_ops carray_owned_ops =
  cobj_ops_init(eq,
                carray_print_op,
                carray_destroy_op,
                carray_mark_op,
                cobj_eq_hash_op);

val make_carray(val type, mem_t *data, cnum nelem, val ref, cnum offs)
{
  val self = lit("make-carray");
  struct carray *scry = coerce(struct carray *, chk_malloc(sizeof *scry));
  val obj;
  scry->eltype = nil;
  scry->eltft = ffi_type_struct_checked(self, type);
  scry->data = data;
  scry->nelem = nelem;
  scry->ref = nil;
  scry->artype[0] = scry->artype[1] = nil;
  obj = cobj(coerce(mem_t *, scry), carray_cls, &carray_borrowed_ops);
  scry->eltype = type;
  scry->ref = ref;
  scry->offs = offs;
#if HAVE_MMAP
  scry->mm_len = 0;
#endif
  return obj;
}

val carrayp(val obj)
{
  return cobjclassp(obj, carray_cls);
}

val carray_set_length(val carray, val nelem)
{
  val self = lit("carray-set-length");
  struct carray *scry = carray_struct_checked(self, carray);
  cnum nel = c_num(nelem, self);

  if (carray->co.ops == &carray_owned_ops)
    uw_throwf(error_s,
              lit("~a: can't set length of owned carray ~s"), self,
              carray, nao);

  if (nel < 0)
    uw_throwf(error_s,
              lit("~a: can't set length of ~s to negative value"), self,
              carray, nao);

  scry->nelem = nel;
  scry->artype[0] = scry->artype[1] = nil;
  return nil;
}

val carray_dup(val carray)
{
  val self = lit("carray-dup");
  struct carray *scry = carray_struct_checked(self, carray);

  if (carray->co.ops == &carray_owned_ops) {
    return nil;
  } else if (scry->nelem < 0) {
    uw_throwf(error_s, lit("~a: size of ~s carray unknown"), self, carray, nao);
  } else if (scry->data == 0) {
    uw_throwf(error_s, lit("~a: ~s: carray data pointer is null"),
              self, carray, nao);
  } else {
    cnum elsize = scry->eltft->size;
    cnum size = convert(ucnum, scry->nelem) * convert(ucnum, elsize);
    mem_t *dup = chk_copy_obj(scry->data, scry->nelem * scry->eltft->size);

    if (size < 0 || (elsize > 0 && size / elsize != scry->nelem))
      uw_throwf(error_s, lit("~a: carray size overflow"), self, nao);

    carray->co.ops = &carray_owned_ops;
    scry->data = dup;
    scry->ref = nil;
    return t;
  }
}

val carray_own(val carray)
{
  val self = lit("carray-own");
  struct carray *scry = carray_struct_checked(self, carray);
  if (scry->ref)
    uw_throwf(error_s, lit("~a: cannot own buffer belonging to ~s"),
              self, scry->ref, nao);
  carray->co.ops = &carray_owned_ops;
  return nil;
}

val carray_free(val carray)
{
  val self = lit("carray-free");
  struct carray *scry = carray_struct_checked(self, carray);

  if (carray->co.ops == &carray_owned_ops) {
    free(scry->data);
    scry->data = 0;
    scry->nelem = 0;
  } else {
    uw_throwf(error_s, lit("~a: cannot free unowned carray ~s"),
              self, carray, nao);
  }

  return nil;
}

val carray_type(val carray)
{
  val self = lit("carray-type");
  struct carray *scry = carray_struct_checked(self, carray);
  return scry->eltype;
}

val length_carray(val carray)
{
  val self = lit("length-carray");
  struct carray *scry = carray_struct_checked(self, carray);
  return if3(scry->nelem < 0, nil, num(scry->nelem));
}

val copy_carray(val carray)
{
  val self = lit("copy-carray");
  struct carray *scry = carray_struct_checked(self, carray);
  if (scry->nelem >= 0) {
    val copy = make_carray(scry->eltype, scry->data, scry->nelem, nil, 0);
    carray_dup(copy);
    return copy;
  }
  uw_throwf(error_s, lit("~a: size of ~s carray unknown"), self, carray, nao);
}

mem_t *carray_ptr(val carray, val type, val self)
{
  struct carray *scry = carray_struct_checked(self, carray);
  if (scry->eltype != type)
    uw_throwf(error_s, lit("~a: ~s is not of element type ~!~s"),
              self, carray, type, nao);
  return scry->data;
}

void carray_set_ptr(val carray, val type, mem_t *ptr, val self)
{
  struct carray *scry = carray_struct_checked(self, carray);
  if (scry->eltype != type)
    uw_throwf(error_s, lit("~a: ~s is not of element type ~!~s"),
              self, carray, type, nao);
  if (carray->co.ops == &carray_borrowed_ops) {
    /* nothing to do */
  } else if (carray->co.ops == &carray_owned_ops) {
    free(scry->data);
    scry->nelem = 0;
    carray->co.ops = &carray_borrowed_ops;
  } else {
    uw_throwf(error_s, lit("~a: cannot change address of mmapped ~!~s"),
              self, carray, type, nao);
  }

  scry->data = ptr;
}

val carray_vec(val vec, val type, val null_term_p)
{
  val self = lit("carray-vec");
  val len = length(vec);
  val nt_p = default_null_arg(null_term_p);
  cnum i, l = c_num(if3(nt_p, succ(len), len), self);
  val carray = carray_blank(len, type);

  for (i = 0; i < l; i++) {
    val ni = num_fast(i);
    val el = ref(vec, ni);
    carray_refset(carray, ni, el);
  }

  return carray;
}

val carray_list(val list, val type, val null_term_p)
{
  val self = lit("carray-vec");
  val nt_p = default_null_arg(null_term_p);
  val len = if3(nt_p, succ(length(list)), length(list));
  val carray = carray_blank(len, type);
  cnum i;

  (void) c_num(len, self);

  for (i = 0; !endp(list); list = cdr(list), i++) {
    val el = car(list);
    carray_refset(carray, num_fast(i), el);
  }

  return carray;
}

val carray_blank(val nelem, val type)
{
  val self = lit("carray-blank");
  cnum nel = c_num(nelem, self);
  struct txr_ffi_type *tft = ffi_type_struct_checked(self, type);

  if (nel < 0) {
    uw_throwf(error_s, lit("~a: negative carray size"), self, nao);
  } else {
    mem_t *data = chk_calloc(nel, tft->size);
    val carray = make_carray(type, data, nel, nil, 0);
    carray->co.ops = &carray_owned_ops;
    return carray;
  }
}

static void carray_elem_check(struct txr_ffi_type *tft, val self)
{
  if (tft->incomplete || tft->bitfield)
    uw_throwf(error_s,
              lit("~a: ~s ~s cannot be carray element"),
              self, if3(tft->bitfield,
                        lit("bitfield"), lit("incomplete type")),
              tft->syntax, nao);
}

val carray_buf(val buf, val type, val offs_in)
{
  val self = lit("carray-buf");
  mem_t *data = buf_get(buf, self);
  val offs = default_arg_strict(offs_in, zero);
  cnum offsn = c_num(offs, self);
  cnum blen = c_num(minus(length_buf(buf), offs), self);
  struct txr_ffi_type *tft = ffi_type_struct_checked(self, type);
  cnum nelem = if3(tft->size, blen / tft->size, 0);
  if (offsn < 0)
    uw_throwf(error_s,
              lit("~a: negative offset ~s not permitted"),
              self, offs, nao);
  if (blen < 0)
    uw_throwf(error_s,
              lit("~a: offset ~s past end of buffer ~s"),
              self, offs, buf, nao);
  carray_elem_check(tft, self);
  return make_carray(type, data + offsn, nelem, buf, offsn);
}

val carray_buf_sync(val carray)
{
  val self = lit("carray-buf-sync");
  struct carray *scry = carray_struct_checked(self, carray);
  val buf = scry->ref;
  mem_t *data = buf_get(buf, self);
  cnum blen = c_num(minus(length_buf(buf), num(scry->offs)), self);
  struct txr_ffi_type *tft = ffi_type_struct(scry->eltype);
  if (blen < 0)
    uw_throwf(error_s,
              lit("~a: offset ~s past end of buffer ~s"),
              self, num(scry->offs), buf, nao);
  scry->nelem = blen / tft->size;
  scry->data = data + scry->offs;
  return buf;
}

val buf_carray(val carray)
{
  val self = lit("buf-carray");
  struct carray *scry = carray_struct_checked(self, carray);
  struct txr_ffi_type *etft = scry->eltft;
  if (scry->nelem >= 0) {
    cnum bytes = scry->nelem * etft->size;
    return make_duplicate_buf(num(bytes), scry->data);
  }
  uw_throwf(error_s, lit("~a: size of ~s carray unknown"), self, carray, nao);
}

val carray_cptr(val cptr, val type, val len)
{
  val self = lit("carray-cptr");
  mem_t *data = cptr_get(cptr);
  cnum nelem = c_num(default_arg(len, negone), self);
  (void) ffi_type_struct_checked(self, type);
  return make_carray(type, data, nelem, nil, 0);
}

val cptr_carray(val carray, val type_sym_in)
{
  val self = lit("cptr-carray");
  struct carray *scry = carray_struct_checked(self, carray);
  val type_sym = default_null_arg(type_sym_in);
  return cptr_typed(scry->data, type_sym, 0);
}

val vec_carray(val carray, val null_term_p)
{
  val self = lit("vec-carray");
  val nt_p = default_null_arg(null_term_p);
  struct carray *scry = carray_struct_checked(self, carray);
  cnum i, l = if3(nt_p, scry->nelem - 1, scry->nelem);

  if (l >= 0) {
    val vec = vector(num(l), nil);
    for (i = 0; i < l; i++) {
      val ni = num_fast(i);
      val el = carray_ref(carray, ni);
      set(vecref_l(vec, ni), el);
    }
    return vec;
  } else if (scry->nelem >= 0) {
    return vector(zero, nil);
  } else {
    uw_throwf(error_s,
              lit("~a: cannot convert unknown length carray to vector"),
              self, nao);
  }
}

val list_carray(val carray, val null_term_p)
{
  val self = lit("list-carray");
  val nt_p = default_null_arg(null_term_p);
  struct carray *scry = carray_struct_checked(self, carray);
  cnum i, l = if3(nt_p, scry->nelem - 1, scry->nelem);

  if (l >= 0) {
    list_collect_decl (list, ptail);
    for (i = 0; i < l; i++) {
      val ni = num_fast(i);
      val el = carray_ref(carray, ni);
      ptail = list_collect(ptail, el);
    }
    return list;
  } else if (scry->nelem >= 0) {
    return nil;
  } else {
    uw_throwf(error_s,
              lit("~a: cannot convert unknown length carray to list"),
              self, nao);
  }
}

val carray_ref(val carray, val idx)
{
  val self = lit("carray-ref");
  struct carray *scry = carray_struct_checked(self, carray);
  cnum ix = c_num(idx, self);

  if (ix < 0 && scry->nelem >= 0)
    ix += scry->nelem;

  if (ix < 0 || (scry->nelem >= 0 && ix >= scry->nelem)) {
    uw_throwf(error_s, lit("~a: ~s: index ~s out of bounds"),
              self, carray, idx, nao);
  } else {
    struct txr_ffi_type *eltft = scry->eltft;
    if (scry->data == 0)
      uw_throwf(error_s, lit("~a: ~s: carray storage was freed"),
                self, carray, nao);
    return eltft->get(eltft, scry->data + eltft->size * ix, self);
  }
}

val carray_refset(val carray, val idx, val newval)
{
  val self = lit("carray-refset");
  struct carray *scry = carray_struct_checked(self, carray);
  cnum ix = c_num(idx, self);

  if (ix < 0 && scry->nelem >= 0)
    ix += scry->nelem;

  if (ix < 0 || (scry->nelem >= 0 && ix >= scry->nelem)) {
    uw_throwf(error_s, lit("~a: ~s: index ~s out of bounds"),
              self, carray, idx, nao);
  } else {
    struct txr_ffi_type *eltft = scry->eltft;
    if (scry->data == 0)
      uw_throwf(error_s, lit("~a: ~s: carray storage was freed"),
                self, carray, nao);
    eltft->put(eltft, newval, scry->data + eltft->size * ix, self);
    return newval;
  }
}

val carray_sub(val carray, val from, val to)
{
  val self = lit("carray-sub");
  struct carray *scry = carray_struct_checked(self, carray);
  cnum ln = scry->nelem;
  val len = num(ln);

  if (null_or_missing_p(from))
    from = zero;
  else if (from == t)
    from = len;
  else if (minusp(from)) {
    if (ln < 0)
      goto nolen;
    from = plus(from, len);
    if (to == zero)
      to = len;
  }

  if (null_or_missing_p(to) || to == t) {
    if (ln < 0)
      goto nolen;
    to = len;
  } else if (minusp(to)) {
    if (ln < 0)
      goto nolen;
    to = plus(to, len);
  }

  {
    cnum fn = c_num(from, self);
    cnum tn = c_num(to, self);
    cnum elsize = scry->eltft->size;

    if (fn < 0)
      fn = 0;

    if (tn < 0)
      tn = 0;

    if (tn > ln)
      tn = ln;

    if (fn > ln)
      fn = ln;

    if (tn < fn)
      tn = fn;

    return make_carray(scry->eltype, scry->data + fn * elsize, tn - fn, carray, 0);
  }
nolen:
  uw_throwf(error_s, lit("~a: operation requires size of ~s to be known"), self, carray, nao);
}

val carray_replace(val carray, val values, val from, val to)
{
  val self = lit("carray-replace");
  struct carray *scry = carray_struct_checked(self, carray);
  cnum ln = scry->nelem;
  val len = num(ln);

  if (null_or_missing_p(from)) {
    from = zero;
  } else if (from == t) {
    from = len;
  } else if (!integerp(from)) {
    seq_iter_t wh_iter, item_iter;
    val wh, item;
    seq_iter_init(self, &wh_iter, from);
    seq_iter_init(self, &item_iter, values);

    if (!missingp(to))
      uw_throwf(error_s,
                lit("~a: to-arg not applicable when from-arg is a list"),
                self, nao);

    while (seq_get(&wh_iter, &wh) && seq_get(&item_iter, &item)) {
      if (ln < 0)
        goto nolen;
      if (ge(wh, len))
        break;
      carray_refset(carray, wh, item);
    }

    return carray;
  } else if (minusp(from)) {
    if (ln < 0)
      goto nolen;
    from = plus(from, len);
    if (to == zero)
      to = len;
  }

  if (null_or_missing_p(to) || to == t) {
    if (ln < 0)
      goto nolen;
    to = len;
  } else if (minusp(to)) {
    if (ln < 0)
      goto nolen;
    to = plus(to, len);
  }

  {
    val vlen = length(values);
    cnum fn = c_num(from, self);
    cnum tn = c_num(to, self);
    struct txr_ffi_type *eltft = scry->eltft;
    cnum elsize = eltft->size;
    cnum size = convert(ucnum, ln) * convert(ucnum, elsize);
    cnum vn = c_num(vlen, self);
    cnum sn;
    mem_t *ptr;
    seq_iter_t item_iter;
    seq_iter_init(self, &item_iter, values);

    if (fn < 0)
      fn = 0;

    if (tn < 0)
      tn = 0;

    if (tn > ln)
      tn = ln;

    if (fn > ln)
      fn = ln;

    if (tn < fn)
      tn = fn;

    sn = fn + vn;

    if (sn > ln)
      sn = ln;

    if (size < 0 || (ln != 0 && size / elsize != ln) || (sn < fn))
      uw_throwf(error_s, lit("~a: carray size overflow"), self, nao);

    ptr = scry->data + fn * elsize;

    {
      cnum oldrange = (tn - fn) * elsize;
      cnum newrange = (sn - fn) * elsize;
      cnum tail = (ln - tn) * elsize;
      cnum whole = ln * elsize;

      if (newrange > oldrange) {
        cnum delta = newrange - oldrange;
        memmove(ptr + newrange, ptr + oldrange, tail - delta);
      } else if (newrange < oldrange) {
        cnum delta = oldrange - newrange;
        memmove(ptr + newrange, ptr + oldrange, tail);
        memset(scry->data + whole - delta, 0, delta);
      }
    }

    for (; fn < sn; fn++, ptr += elsize) {
      val item = seq_geti(&item_iter);
      eltft->put(eltft, item, ptr, self);
    }

    return carray;
  }
nolen:
  uw_throwf(error_s, lit("~a: operation requires size of ~s to be known"), self, carray, nao);
}

static void carray_ensure_artype(val carray, struct carray *scry, int null_term, val self)
{
  if (!scry->artype[null_term]) {
    val dim = num(scry->nelem);
    val syntax = if3(scry->nelem < 0,
                     list(carray_s, scry->eltft->syntax, nao),
                     list(carray_s, dim, scry->eltft->syntax, nao));
    struct txr_ffi_type *etft = scry->eltft;
    set(mkloc(scry->artype[null_term], carray),
        make_ffi_type_array(syntax, vec_s,
                            dim, scry->eltype,
                            self));

    {
      struct txr_ffi_type *atft = ffi_type_struct(scry->artype[null_term]);

      if (etft->syntax == char_s)
        atft->ch_conv = conv_char;
      else if (etft->syntax == wchar_s)
        atft->ch_conv = conv_wchar;
      else if (etft->syntax == bchar_s)
        atft->ch_conv = conv_bchar;

      atft->null_term = null_term;
    }
  }
}

static val carray_get_common(val carray, val self, unsigned null_term)
{
  struct carray *scry = carray_struct_checked(self, carray);

  carray_ensure_artype(carray, scry, null_term, self);

  {
    struct txr_ffi_type *atft = ffi_type_struct(scry->artype[null_term]);
    return atft->get(atft, scry->data, self);
  }
}

static void carray_put_common(val carray, val seq, val self, unsigned null_term)
{
  struct carray *scry = carray_struct_checked(self, carray);

  carray_ensure_artype(carray, scry, null_term, self);

  {
    struct txr_ffi_type *atft = ffi_type_struct(scry->artype[null_term]);
    return atft->put(atft, seq, scry->data, self);
  }
}

val carray_get(val carray)
{
  val self = lit("carray-get");
  return carray_get_common(carray, self, 0);
}

val carray_getz(val carray)
{
  val self = lit("carray-getz");
  return carray_get_common(carray, self, 1);
}

val carray_put(val carray, val seq)
{
  val self = lit("carray-put");
  carray_put_common(carray, seq, self, 0);
  return carray;
}

val carray_putz(val carray, val seq)
{
  val self = lit("carray-putz");
  carray_put_common(carray, seq, self, 1);
  return carray;
}

val carray_pun(val carray, val type, val offset_in, val lim_in)
{
  val self = lit("carray-pun");
  struct carray *scry = carray_struct_checked(self, carray);
  struct txr_ffi_type *tft = ffi_type_struct_checked(self, type);
  ucnum len = scry->nelem;
  ucnum elsize = scry->eltft->size;
  ucnum size = len * elsize;
  ucnum off = if3(missingp(offset_in), 0, c_unum(offset_in, self));
  ucnum lim = if3(missingp(lim_in), size - off, c_unum(lim_in, self));

  carray_elem_check(tft, self);

  if (len != 0 && size / elsize != len)
    uw_throwf(error_s, lit("~a: carray size overflow"), self, nao);

  if (off > size)
    uw_throwf(error_s, lit("~a: ~s: offset ~a is out of bounds"),
              self, carray, unum(off), nao);

  if (off + lim < off)
    uw_throwf(error_s, lit("~a: ~s: limit ~a from offset ~a wraps around"),
              self, carray, unum(lim), unum(off), nao);

  if (off + lim > size)
    uw_throwf(error_s, lit("~a: ~s: limit ~a from offset ~a extends out of bounds"),
              self, carray, unum(lim), unum(off), nao);

  return make_carray(type, scry->data + off, lim / tft->size, carray, 0);
}

val carray_uint(val num, val eltype_in)
{
  val self = lit("carray-uint");
  val eltype = default_arg(eltype_in, ffi_type_compile(uchar_s));
  struct txr_ffi_type *tft = ffi_type_struct_checked(self, eltype);

  carray_elem_check(tft, self);

  switch (type(num)) {
  case NUM: case CHR:
    num = bignum(c_num(num, self));
    /* fallthrough */
  case BGNUM:
    if (minusp(num))
      uw_throwf(error_s,
                lit("~a: negative number ~s passed; non-negative required"),
                self, num, nao);
    {
      mp_int *m = mp(num);
      ucnum size = mp_unsigned_bin_size(m);
      ucnum nelem = (size + tft->size - 1) / tft->size;
      mem_t *data = chk_xalloc(nelem, tft->size, self);
      ucnum delta = nelem * tft->size - size;
      val ca = make_carray(eltype, data, nelem, nil, 0);
      memset(data, 0, delta);
      mp_to_unsigned_bin(m, data + delta);
      gc_hint(num);
      return ca;
    }
  default:
    uw_throwf(type_error_s, lit("~a: ~s isn't an integer or character"),
              self, num, nao);
  }
}

val carray_int(val num, val eltype_in)
{
  val self = lit("carray-int");
  val eltype = default_arg(eltype_in, ffi_type_compile(uchar_s));
  struct txr_ffi_type *tft = ffi_type_struct_checked(self, eltype);

  carray_elem_check(tft, self);

  switch (type(num)) {
  case NUM: case CHR:
    num = bignum(c_num(num, self));
    /* fallthrough */
  case BGNUM:
    {
      val wi = width(num);
      val bits = succ(wi);
      val bytes = ash(plus(bits, num_fast(7)), num_fast(-3));
      val bitsround = ash(bytes, num_fast(3));
      val un = logtrunc(num, bitsround);
      val ube = if3(bignump(un), un, bignum(c_num(un, self)));
      mp_int *m = mp(ube);
      ucnum size = mp_unsigned_bin_size(m);
      ucnum nelem = (c_unum(bytes, self) + tft->size - 1) / tft->size;
      mem_t *data = chk_xalloc(nelem, tft->size, self);
      ucnum delta = nelem * tft->size - size;
      val ca = make_carray(eltype, data, nelem, nil, 0);
      mp_to_unsigned_bin(m, data + delta);
      memset(data, if3(bit(ube, wi), 0xff, 0), delta);
      gc_hint(num);
      gc_hint(ube);
      return ca;
    }
  default:
    uw_throwf(type_error_s, lit("~a: ~s isn't an integer or character"),
              self, num, nao);
  }
}

val uint_carray(val carray)
{
  val self = lit("uint-carray");
  struct carray *scry = carray_struct_checked(self, carray);
  struct txr_ffi_type *etft = scry->eltft;
  ucnum size = convert(ucnum, etft->size) * convert(ucnum, scry->nelem);
  val ubn = make_bignum();
  mp_err mpe = mp_read_unsigned_bin(mp(ubn), scry->data, size);
  if (mpe != MP_OKAY)
    do_mp_error(self, mpe);
  return normalize(ubn);
}

val int_carray(val carray)
{
  val self = lit("int-carray");
  struct carray *scry = carray_struct_checked(self, carray);
  struct txr_ffi_type *etft = scry->eltft;
  ucnum size = convert(ucnum, etft->size) * convert(ucnum, scry->nelem);
  ucnum bits = size * 8;
  val ubn = make_bignum();
  mp_err mpe = mp_read_unsigned_bin(mp(ubn), scry->data, size);
  if (mpe != MP_OKAY)
    do_mp_error(self, mpe);
  return sign_extend(normalize(ubn), unum(bits));
}

val put_carray(val carray, val offs, val stream)
{
  val self = lit("put-carray");
  struct carray *scry = carray_struct_checked(self, carray);
  struct txr_ffi_type *etft = scry->eltft;
  ucnum size = convert(ucnum, etft->size) * convert(ucnum, scry->nelem);
  val buf = make_borrowed_buf(unum(size), scry->data);
  val pos = default_arg(offs, zero);
  val ret = put_buf(buf, pos, stream);
  gc_hint(carray);
  return ret;
}

val fill_carray(val carray, val offs, val stream)
{
  val self = lit("fill-carray");
  struct carray *scry = carray_struct_checked(self, carray);
  struct txr_ffi_type *etft = scry->eltft;
  ucnum size = convert(ucnum, etft->size) * convert(ucnum, scry->nelem);
  val buf = make_borrowed_buf(unum(size), scry->data);
  val pos = default_arg(offs, zero);
  val ret = fill_buf(buf, pos, stream);
  gc_hint(carray);
  return ret;
}

#if HAVE_MMAP

#ifndef MAP_GROWSDOWN
#define MAP_GROWSDOWN 0
#endif
#ifndef MAP_LOCKED
#define MAP_LOCKED 0
#endif
#ifndef MAP_NORESERVE
#define MAP_NORESERVE 0
#endif
#ifndef MAP_POPULATE
#define MAP_POPULATE 0
#endif
#ifndef MAP_NONBLOCK
#define MAP_NONBLOCK 0
#endif
#ifndef MAP_STACK
#define MAP_STACK 0
#endif
#ifndef MAP_HUGETLB
#define MAP_HUGETLB 0
#endif
#ifndef MAP_SHARED
#define MAP_SHARED 0
#endif
#ifndef MAP_PRIVATE
#define MAP_PRIVATE 0
#endif
#ifndef MAP_FIXED
#define MAP_FIXED 0
#endif
#if !defined MAP_ANON && defined MAP_ANONYMOUS
#define MAP_ANON MAP_ANONYMOUS
#elif !defined MAP_ANON
#define MAP_ANON 0
#endif
#ifndef MAP_HUGE_SHIFT
#define MAP_HUGE_SHIFT 0
#endif
#ifndef MAP_HUGE_MASK
#define MAP_HUGE_MASK 0
#endif

#ifndef PROT_READ
#define PROT_READ 0
#endif
#ifndef PROT_WRITE
#define PROT_WRITE 0
#endif
#ifndef PROT_EXEC
#define PROT_EXEC 0
#endif
#ifndef PROT_NONE
#define PROT_NONE 0
#endif
#ifndef PROT_GROWSDOWN
#define PROT_GROWSDOWN 0
#endif
#ifndef PROT_GROWSUP
#define PROT_GROWSUP 0
#endif

#ifndef MADV_NORMAL
#define MADV_NORMAL 0
#endif
#ifndef MADV_RANDOM
#define MADV_RANDOM 0
#endif
#ifndef MADV_SEQUENTIAL
#define MADV_SEQUENTIAL 0
#endif
#ifndef MADV_WILLNEED
#define MADV_WILLNEED 0
#endif
#ifndef MADV_DONTNEED
#define MADV_DONTNEED 0
#endif
#ifndef MADV_FREE
#define MADV_FREE 0
#endif
#ifndef MADV_REMOVE
#define MADV_REMOVE 0
#endif
#ifndef MADV_DONTFORK
#define MADV_DONTFORK 0
#endif
#ifndef MADV_DOFORK
#define MADV_DOFORK 0
#endif
#ifndef MADV_MERGEABLE
#define MADV_MERGEABLE 0
#endif
#ifndef MADV_UNMERGEABLE
#define MADV_UNMERGEABLE 0
#endif
#ifndef MADV_HUGEPAGE
#define MADV_HUGEPAGE 0
#endif
#ifndef MADV_NOHUGEPAGE
#define MADV_NOHUGEPAGE 0
#endif
#ifndef MADV_DONTDUMP
#define MADV_DONTDUMP 0
#endif
#ifndef MADV_DODUMP
#define MADV_DODUMP 0
#endif
#ifndef MADV_WIPEONFORK
#define MADV_WIPEONFORK 0
#endif
#ifndef MADV_KEEPONFORK
#define MADV_KEEPONFORK 0
#endif
#ifndef MADV_HWPOISON
#define MADV_HWPOISON 0
#endif

#ifndef MS_ASYNC
#define MS_ASYNC 0
#endif
#ifndef MS_SYNC
#define MS_SYNC 0
#endif
#ifndef MS_INVALIDATE
#define MS_INVALIDATE 0
#endif

static void carray_munmap_op(val obj)
{
  struct carray *scry = carray_struct(obj);
  munmap(scry->data, scry->mm_len);
  scry->data = 0;
  free(scry);
}

static struct cobj_ops carray_mmap_ops =
  cobj_ops_init(eq,
                carray_print_op,
                carray_munmap_op,
                carray_mark_op,
                cobj_eq_hash_op);

val mmap_wrap(val type, val len, val prot, val flags,
              val source_opt, val offset_opt, val addr_opt)
{
  val self = lit("mmap");
  val source = default_null_arg(source_opt);
  val offset = default_arg_strict(offset_opt, zero);
  val addr = default_null_arg(addr_opt);
  void *ad_req = if3(addr, coerce(void *, c_unum(addr, self)), 0);
  mem_t *ad_out;
  int fd = -1;
  ucnum ln = c_unum(len, self);
  struct txr_ffi_type *tft = ffi_type_struct_checked(self, type);
  cnum nelem = if3(tft->size, ln / tft->size, 0);
  int pro = c_int(prot, self);
  int flg = c_int(flags, self);

  if (ln != 0 && nelem == 0)
    uw_throwf(error_s, lit("~a: zero-sized element type ~s specified"),
              self, type, nao);

  if (streamp(source)) {
    val fileno = stream_fd(source);
    if (!fileno)
      uw_throwf(type_error_s, lit("~a: stream ~s has no file descriptor"),
                self, source, nao);
    fd = c_int(fileno, self);
  } else if (integerp(source)) {
    fd = c_int(source, self);
  } else if (stringp(source)) {
    val mode = if3(pro & PROT_WRITE, lit("r+"), lit("r"));
    val stream = open_file(source, mode);
    val map = nil;
    uw_simple_catch_begin;
    map = mmap_wrap(type, len, prot, flags, stream, offset_opt, addr_opt);
    uw_unwind {
      close_stream(stream, nil);
    }
    uw_catch_end;
    return map;
  } else if (source) {
    uw_throwf(type_error_s, lit("~a: unsupported map source object ~s"),
              self, source, nao);
  }

  ad_out = coerce(mem_t *,
                  mmap(ad_req, ln, pro, flg, fd, c_u64(offset, self)));

  if (ad_out == MAP_FAILED) {
    int eno = errno;
    uw_ethrowf(system_error_s, lit("~a: mmap failed: ~d/~s"),
               self, num(eno), errno_to_str(eno), nao);
  } else {
    val ca = make_carray(type, ad_out, nelem, nil, 0);
    struct carray *scry = carray_struct(ca);
    scry->mm_len = ln;
    ca->co.ops = &carray_mmap_ops;
    return ca;
  }
}

val munmap_wrap(val carray)
{
  val self = lit("munmap");
  struct carray *scry = carray_struct_checked(self, carray);

  if (carray->co.ops != &carray_mmap_ops)
    uw_throwf(type_error_s, lit("~a: ~s isn't a mmapped carray"),
              self, carray, nao);
  if (scry->data != 0) {
    munmap(scry->data, scry->mm_len);
    scry->data = 0;
    return t;
  }

  return nil;
}

static val mmap_op(val carray, val offset_in, val size_in,
                   val arg, int (*op_fn)(void *, size_t, int),
                   val self)
{
  struct carray *scry = carray_struct_checked(self, carray);
  size_t off = 0, sz;

  if (carray->co.ops != &carray_mmap_ops)
    uw_throwf(type_error_s, lit("~a: ~s isn't a mmapped carray"),
              self, carray, nao);

  if (missingp(offset_in) && missingp(size_in)) {
    sz = scry->mm_len;
  } else if (missingp(offset_in)) {
    sz = c_unum(size_in, self);
  } else if (missingp(size_in)) {
    off = c_unum(offset_in, self);
    sz = scry->mm_len - off;
  } else {
    off = c_unum(offset_in, self);
    sz = c_unum(size_in, self);
  }

  if (off > scry->mm_len)
    uw_throwf(error_s, lit("~a: ~s: offset ~s lies beyond ~s byte mapping"),
              self, carray, unum(off), unum(scry->mm_len), nao);

  if (off + sz < off)
    uw_throwf(error_s,
              lit("~a: ~s: size ~s from offset ~s wraps around"),
              self, carray, unum(sz), unum(off), nao);

  if (off + sz > scry->mm_len)
    uw_throwf(error_s,
              lit("~a: ~s: size ~s from offset ~s extends beyond ~s byte mapping"),
              self, carray, unum(sz), unum(off), unum(scry->mm_len), nao);

  if (op_fn(scry->data + off, sz, c_int(arg, self)) < 0) {
    int eno = errno;
    uw_ethrowf(system_error_s, lit("~a: ~s: ~a failed: ~d/~s"),
               self, carray, self, num(eno), errno_to_str(eno), nao);
  }

  return t;
}

val mprotect_wrap(val carray, val prot, val offset, val size)
{
  return mmap_op(carray, offset, size, prot, mprotect, lit("mprotect"));
}

val madvise_wrap(val carray, val advice, val offset, val size)
{
  return mmap_op(carray, offset, size, advice, madvise, lit("madvise"));
}

val msync_wrap(val carray, val flags, val offset, val size)
{
  return mmap_op(carray, offset, size, flags, msync, lit("msync"));
}

#endif

static val cptr_getobj(val cptr, val type_in)
{
  val self = lit("cptr-get");
  mem_t *data = cptr_get(cptr);
  val type = default_arg(type_in, ffi_type_lookup_checked(self, cptr->cp.cls));
  struct txr_ffi_type *tft = ffi_type_struct_checked(self, type);
  if (data != 0)
    return tft->get(tft, data, self);
  uw_throwf(type_error_s, lit("~a: ~s is a null pointer"), self, cptr, nao);
}

static val cptr_out(val cptr, val obj, val type_in)
{
  val self = lit("cptr-out");
  mem_t *data = cptr_get(cptr);
  val type = default_arg(type_in, ffi_type_lookup_checked(self, cptr->cp.cls));
  struct txr_ffi_type *tft = ffi_type_struct_checked(self, type);
  if (data != 0) {
    if (tft->out != 0)
      tft->out(tft, 0, obj, data, self);
    else
      tft->put(tft, obj, data, self);
    return obj;
  }
  uw_throwf(type_error_s, lit("~a: ~s is a null pointer"), self, cptr, nao);
}

struct uni {
  struct txr_ffi_type *tft;
  mem_t *data;
};

static struct uni *uni_struct(val obj)
{
  return coerce(struct uni *, obj->co.handle);
}

static struct uni *uni_struct_checked(val self, val obj)
{
  return coerce(struct uni *, cobj_handle(self, obj, union_cls));
}

static void union_destroy_op(val obj)
{
  struct uni *us = uni_struct(obj);
  free(us->data);
  us->data = 0;
  free(us);
}

static void union_mark_op(val obj)
{
  struct uni *us = uni_struct(obj);
  gc_mark(us->tft->self);
}

static struct cobj_ops union_ops =
  cobj_ops_init(eq,
                cobj_print_op,
                union_destroy_op,
                union_mark_op,
                cobj_eq_hash_op);

static val make_union_common(mem_t *data, struct txr_ffi_type *tft)
{
  struct uni *us = coerce(struct uni *, chk_calloc(1, sizeof *us));
  val obj = cobj(coerce(mem_t *, us), union_cls, &union_ops);
  us->tft = tft;
  us->data = data;
  return obj;
}

static val make_union_tft(mem_t *data_in, struct txr_ffi_type *tft)
{
  mem_t *data = chk_copy_obj(data_in, tft->size);
  return make_union_common(data, tft);
}

mem_t *union_get_ptr(val self, val uni)
{
  struct uni *us = uni_struct_checked(self, uni);
  return us->data;
}

val make_union(val type, val init, val memb)
{
  val self = lit("make-union");
  struct txr_ffi_type *tft = ffi_type_struct_checked(self, type);
  mem_t *data = chk_calloc(1, tft->size);
  val uni = make_union_common(data, tft);
  if (!missingp(init)) {
    if (tft->nelem == 0)
      uw_throwf(error_s, lit("~a: ~s cannot be initialized: no members"),
                self, type, nao);
    memb = default_arg(memb, tft->memb[0].mname);
    union_put(uni, memb, init);
  }
  return uni;
}

val union_members(val uni)
{
  val self = lit("union-members");
  struct uni *us = uni_struct_checked(self, uni);
  struct txr_ffi_type *tft = us->tft;
  cnum i;
  list_collect_decl (out, ptail);

  for (i = 0; i < tft->nelem; i++)
    ptail = list_collect(ptail, tft->memb[i].mname);

  return out;
}

val union_get(val uni, val memb)
{
  val self = lit("union-get");
  struct uni *us = uni_struct_checked(self, uni);
  struct txr_ffi_type *tft = us->tft;
  struct txr_ffi_type *mtft = ffi_find_memb(tft, memb);
  if (mtft == 0)
    ffi_memb_not_found(tft->self, memb, self);
  return mtft->get(mtft, us->data, self);
}

val union_put(val uni, val memb, val newval)
{
  val self = lit("union-put");
  struct uni *us = uni_struct_checked(self, uni);
  struct txr_ffi_type *tft = us->tft;
  struct txr_ffi_type *mtft = ffi_find_memb(tft, memb);
  if (mtft == 0)
    ffi_memb_not_found(tft->self, memb, self);
  mtft->put(mtft, newval, us->data, self);
  return newval;
}

val union_in(val uni, val memb, val memb_obj)
{
  val self = lit("union-in");
  struct uni *us = uni_struct_checked(self, uni);
  struct txr_ffi_type *tft = us->tft;
  struct txr_ffi_type *mtft = ffi_find_memb(tft, memb);
  if (mtft == 0)
    ffi_memb_not_found(tft->self, memb, self);
  return mtft->in(mtft, 0, us->data, memb_obj, self);
}

val union_out(val uni, val memb, val memb_obj)
{
  val self = lit("union-out");
  struct uni *us = uni_struct_checked(self, uni);
  struct txr_ffi_type *tft = us->tft;
  struct txr_ffi_type *mtft = ffi_find_memb(tft, memb);
  if (mtft == 0)
    ffi_memb_not_found(tft->self, memb, self);
  mtft->out(mtft, 0, memb_obj, us->data, self);
  return memb_obj;
}

val make_zstruct(val type, struct args *args)
{
  val self = lit("make-zstruct");
  struct txr_ffi_type *tft = ffi_type_struct_checked(self, type);
  val pairs = args_get_list(args);
  args_decl(ms_args, ARGS_ABS_MIN);
  val strct = make_struct(tft->lt, nil, ms_args);
  mem_t *zbuf;
  char *inited = coerce(char *, zalloca(tft->nelem));
  cnum i, largest;

  if (!tft->memb)
    uw_throwf(error_s, lit("~a: ~s isn't a struct type"), self, type, nao);

  for (i = largest = 0; i < tft->nelem; i++) {
    cnum size = tft->memb[i].mtft->size;
    if (size > largest)
      largest = size;
  }

  zbuf = coerce(mem_t *, zalloca(largest));

  while (pairs) {
    val sym = pop(&pairs);
    val initval = pop(&pairs);

    slotset(strct, sym, initval);

    for (i = 0; i < tft->nelem; i++)
      if (tft->memb[i].mname == sym)
        inited[i] = 1;
  }

  for (i = 0; i < tft->nelem; i++) {
    if (!inited[i]) {
      struct smemb *m = &tft->memb[i];
      val slsym = m->mname;
      if (slsym) {
        val initval = m->mtft->get(m->mtft, zbuf, self);
        slotset(strct, slsym, initval);
      }
    }
  }

  return strct;
}

val zero_fill(val type, val obj)
{
  val self = lit("zero-fill");
  struct txr_ffi_type *tft = ffi_type_struct_checked(self, type);
  cnum size = tft->size;
  int need_free = (size >= 1024);
  mem_t *buf = if3(need_free, chk_calloc(1, size), coerce(mem_t *, zalloca(size)));
  val ret = nil;

  if (need_free) {
    uw_simple_catch_begin;

    if (tft->in != 0)
      ret = tft->in(tft, 1, buf, obj, self);
    else
      ret = tft->get(tft, buf, self);

    uw_unwind {
      free(buf);
    }

    uw_catch_end;
  } else {
    if (tft->in != 0)
      ret = tft->in(tft, 1, buf, obj, self);
    else
      ret = tft->get(tft, buf, self);
  }

  return ret;
}

val put_obj(val obj, val type, val stream)
{
  val self = lit("put-obj");
  struct txr_ffi_type *tft = ffi_type_struct_checked(self, type);
  cnum size = tft->size;
  val len = num(size);
  mem_t *data = coerce(mem_t *, zalloca(size));
  obj_t buf_obj;
  val buf = init_borrowed_buf(&buf_obj, len, data);
  tft->put(tft, obj, data, self);
  return eql(put_buf(buf, zero, stream), len);
}


val get_obj(val type, val stream)
{
  val self = lit("get-obj");
  struct txr_ffi_type *tft = ffi_type_struct_checked(self, type);
  cnum size = tft->size;
  val len = num(size);
  mem_t *data = coerce(mem_t *, zalloca(size));
  obj_t buf_obj;
  val buf = init_borrowed_buf(&buf_obj, len, data);
  if (neql(fill_buf(buf, zero, stream), len))
    return nil;
  return tft->get(tft, data, self);
}

val fill_obj(val obj, val type, val stream)
{
  val self = lit("fill-obj");
  struct txr_ffi_type *tft = ffi_type_struct_checked(self, type);
  cnum size = tft->size;
  val len = num(size);
  mem_t *data = coerce(mem_t *, zalloca(size));
  obj_t buf_obj;
  val buf = init_borrowed_buf(&buf_obj, len, data);
  if (neql(fill_buf(buf, zero, stream), len))
    return nil;
  return tft->in(tft, 1, data, obj, self);
}

static val dyn_size(val type, val obj)
{
  val self = lit("sizeof");
  struct txr_ffi_type *tft = ffi_type_struct_checked(self, type);
  return num(tft->dynsize(tft, obj, self));
}

#if HAVE_SOCKETS

static val sock_opt(val sock, val level, val option, val type_opt)
{
  val self = lit("sock-opt");
  val sfd = stream_fd(sock);
  int lvl = c_int(level, self);
  int opt = c_int(option, self);
  val type = default_arg(type_opt, ffi_type_lookup(int_s));
  struct txr_ffi_type *tft = ffi_type_struct_checked(self, type);

  if (!sfd) {
    uw_throwf(socket_error_s, lit("~a: cannot get option on ~s"),
              self, sock, nao);
  } else {
    socklen_t size = convert(socklen_t, tft->size);
    mem_t *data = coerce(mem_t *, zalloca(size));
    if (getsockopt(c_num(sfd, self), lvl, opt, data, &size) != 0)
      uw_ethrowf(socket_error_s, lit("~a failed on ~s: ~d/~s"),
                 self, sock, num(errno), errno_to_str(errno), nao);
    /* TODO: Add a separate function to handle options with
     * variable-size values, for example the platform-specific
     * SO_BINDTODEVICE.
     * (Or perhaps add an optional argument following type_opt
     * specifying the requested length of the value, presumably of type
     * carray.) */
    if (size != convert(socklen_t, tft->size))
      uw_throwf(socket_error_s, lit("~a: variable-size option on ~s"),
                self, sock, nao);
    return tft->get(tft, data, self);
  }
}

static val sock_set_opt(val sock, val level, val option, val value,
                        val type_opt)
{
  val self = lit("sock-set-opt");
  val sfd = stream_fd(sock);
  int lvl = c_int(level, self);
  int opt = c_int(option, self);
  val type = default_arg(type_opt, ffi_type_lookup(int_s));
  struct txr_ffi_type *tft = ffi_type_struct_checked(self, type);

  if (!sfd) {
    uw_throwf(socket_error_s, lit("~a: cannot set option on ~s"),
              self, sock, nao);
  } else {
    socklen_t size = convert(socklen_t, tft->size);
    mem_t *data = coerce(mem_t *, zalloca(size));
    tft->put(tft, value, data, self);
    if (setsockopt(c_num(sfd, self), lvl, opt, data, size) != 0)
      uw_ethrowf(socket_error_s, lit("~a failed on ~s: ~d/~s"),
                 self, sock, num(errno), errno_to_str(errno), nao);
    return value;
  }
}

#endif

void ffi_init(void)
{
  prot1(&ffi_typedef_hash);
  prot1(&ffi_struct_tag_hash);
  uint8_s = intern(lit("uint8"), user_package);
  int8_s = intern(lit("int8"), user_package);
  uint16_s = intern(lit("uint16"), user_package);
  int16_s = intern(lit("int16"), user_package);
  uint32_s = intern(lit("uint32"), user_package);
  int32_s = intern(lit("int32"), user_package);
  uint64_s = intern(lit("uint64"), user_package);
  int64_s = intern(lit("int64"), user_package);
  char_s = intern(lit("char"), user_package);
  zchar_s = intern(lit("zchar"), user_package);
  uchar_s = intern(lit("uchar"), user_package);
  bchar_s = intern(lit("bchar"), user_package);
  wchar_s = intern(lit("wchar"), user_package);
  short_s = intern(lit("short"), user_package);
  ushort_s = intern(lit("ushort"), user_package);
  int_s = intern(lit("int"), user_package);
  uint_s = intern(lit("uint"), user_package);
  long_s = intern(lit("long"), user_package);
  ulong_s = intern(lit("ulong"), user_package);
  double_s = intern(lit("double"), user_package);
  val_s = intern(lit("val"), user_package);
  be_uint16_s = intern(lit("be-uint16"), user_package);
  be_int16_s = intern(lit("be-int16"), user_package);
  be_uint32_s = intern(lit("be-uint32"), user_package);
  be_int32_s = intern(lit("be-int32"), user_package);
  be_uint64_s = intern(lit("be-uint64"), user_package);
  be_int64_s = intern(lit("be-int64"), user_package);
  be_float_s = intern(lit("be-float"), user_package);
  be_double_s = intern(lit("be-double"), user_package);
  le_uint16_s = intern(lit("le-uint16"), user_package);
  le_int16_s = intern(lit("le-int16"), user_package);
  le_uint32_s = intern(lit("le-uint32"), user_package);
  le_int32_s = intern(lit("le-int32"), user_package);
  le_uint64_s = intern(lit("le-uint64"), user_package);
  le_int64_s = intern(lit("le-int64"), user_package);
  le_float_s = intern(lit("le-float"), user_package);
  le_double_s = intern(lit("le-double"), user_package);
  void_s = intern(lit("void"), user_package);
  array_s = intern(lit("array"), user_package);
  zarray_s = intern(lit("zarray"), user_package);
  carray_s = intern(lit("carray"), user_package);
  union_s = intern(lit("union"), user_package);
  str_d_s = intern(lit("str-d"), user_package);
  wstr_s = intern(lit("wstr"), user_package);
  wstr_d_s = intern(lit("wstr-d"), user_package);
  bstr_s = intern(lit("bstr"), user_package);
  bstr_d_s = intern(lit("bstr-d"), user_package);
  buf_d_s = intern(lit("buf-d"), user_package);
  ptr_in_s = intern(lit("ptr-in"), user_package);
  ptr_out_s = intern(lit("ptr-out"), user_package);
  ptr_in_d_s = intern(lit("ptr-in-d"), user_package);
  ptr_out_d_s = intern(lit("ptr-out-d"), user_package);
  ptr_out_s_s = intern(lit("ptr-out-s"), user_package);
  ptr_s = intern(lit("ptr"), user_package);
  closure_s = intern(lit("closure"), user_package);
  sbit_s = intern(lit("sbit"), user_package);
  ubit_s = intern(lit("ubit"), user_package);
  bit_s = intern(lit("bit"), user_package);
  enum_s = intern(lit("enum"), user_package);
  enumed_s = intern(lit("enumed"), user_package);
  elemtype_s = intern(lit("elemtype"), user_package);
  align_s = intern(lit("align"), user_package);
  bool_s = intern(lit("bool"), user_package);
  ffi_type_s = intern(lit("ffi-type"), user_package);
  ffi_call_desc_s = intern(lit("ffi-call-desc"), user_package);
  ffi_closure_s = intern(lit("ffi-closure"), user_package);
  ffi_type_cls = cobj_register(ffi_type_s);
  ffi_call_desc_cls = cobj_register(ffi_call_desc_s);
  ffi_closure_cls = cobj_register(ffi_closure_s);
  carray_cls = cobj_register(carray_s);
  union_cls = cobj_register(union_s);
  reg_fun(intern(lit("ffi-type-compile"), user_package), func_n1(ffi_type_compile));
  reg_fun(intern(lit("ffi-type-operator-p"), user_package), func_n1(ffi_type_operator_p));
  reg_fun(intern(lit("ffi-type-p"), user_package), func_n1(ffi_type_p));
#if HAVE_LIBFFI
  reg_fun(intern(lit("ffi-make-call-desc"), user_package), func_n5o(ffi_make_call_desc, 4));
  reg_fun(intern(lit("ffi-call"), user_package), func_n2v(ffi_call_wrap));
  reg_fun(intern(lit("ffi-make-closure"), user_package), func_n4o(ffi_make_closure, 2));
#endif
  reg_fun(intern(lit("ffi-typedef"), user_package), func_n2(ffi_typedef));
  reg_fun(intern(lit("ffi-size"), user_package), func_n1(ffi_size));
  reg_fun(intern(lit("ffi-alignof"), user_package), func_n1(ffi_alignof));
  reg_fun(intern(lit("ffi-offsetof"), user_package), func_n2(ffi_offsetof));
  reg_fun(intern(lit("ffi-arraysize"), user_package), func_n1(ffi_arraysize));
  reg_fun(intern(lit("ffi-elemsize"), user_package), func_n1(ffi_elemsize));
  reg_fun(intern(lit("ffi-elemtype"), user_package), func_n1(ffi_elemtype));
  reg_fun(intern(lit("ffi-put-into"), user_package), func_n4o(ffi_put_into, 3));
  reg_fun(intern(lit("ffi-put"), user_package), func_n2(ffi_put));
  reg_fun(intern(lit("ffi-in"), user_package), func_n5o(ffi_in, 4));
  reg_fun(intern(lit("ffi-get"), user_package), func_n3o(ffi_get, 2));
  reg_fun(intern(lit("ffi-out"), user_package), func_n5o(ffi_out, 4));
  reg_fun(intern(lit("carrayp"), user_package), func_n1(carrayp));
  reg_fun(intern(lit("carray-set-length"), user_package), func_n2(carray_set_length));
  reg_fun(intern(lit("carray-dup"), user_package), func_n1(carray_dup));
  reg_fun(intern(lit("carray-own"), user_package), func_n1(carray_own));
  reg_fun(intern(lit("carray-free"), user_package), func_n1(carray_free));
  reg_fun(intern(lit("carray-type"), user_package), func_n1(carray_type));
  reg_fun(intern(lit("length-carray"), user_package), func_n1(length_carray));
  reg_fun(intern(lit("copy-carray"), user_package), func_n1(copy_carray));
  reg_fun(intern(lit("carray-vec"), user_package), func_n3o(carray_vec, 2));
  reg_fun(intern(lit("carray-list"), user_package), func_n3o(carray_list, 2));
  reg_fun(intern(lit("carray-blank"), user_package), func_n2(carray_blank));
  reg_fun(intern(lit("carray-buf"), user_package), func_n3o(carray_buf, 2));
  reg_fun(intern(lit("carray-buf-sync"), user_package), func_n1(carray_buf_sync));
  reg_fun(intern(lit("buf-carray"), user_package), func_n1(buf_carray));
  reg_fun(intern(lit("carray-cptr"), user_package), func_n3o(carray_cptr, 2));
  reg_fun(intern(lit("cptr-carray"), user_package), func_n2o(cptr_carray, 1));
  reg_fun(intern(lit("vec-carray"), user_package), func_n2o(vec_carray, 1));
  reg_fun(intern(lit("list-carray"), user_package), func_n2o(list_carray, 1));
  reg_fun(intern(lit("carray-ref"), user_package), func_n2(carray_ref));
  reg_fun(intern(lit("carray-refset"), user_package), func_n3(carray_refset));
  reg_fun(intern(lit("carray-sub"), user_package), func_n3o(carray_sub, 1));
  reg_fun(intern(lit("carray-replace"), user_package), func_n4o(carray_replace, 2));
  reg_fun(intern(lit("carray-get"), user_package), func_n1(carray_get));
  reg_fun(intern(lit("carray-getz"), user_package), func_n1(carray_getz));
  reg_fun(intern(lit("carray-put"), user_package), func_n2(carray_put));
  reg_fun(intern(lit("carray-putz"), user_package), func_n2(carray_putz));
  reg_fun(intern(lit("carray-pun"), user_package), func_n4o(carray_pun, 2));
  {
    val ca_uint = func_n2o(carray_uint, 1);
    val ca_int = func_n2o(carray_int, 1);
    val uint_ca = func_n1(uint_carray);
    val int_ca = func_n1(int_carray);

    reg_fun(intern(lit("carray-uint"), user_package), ca_uint);
    reg_fun(intern(lit("carray-int"), user_package), ca_int);
    reg_fun(intern(lit("uint-carray"), user_package), uint_ca);
    reg_fun(intern(lit("int-carray"), user_package), int_ca);
  }
  reg_fun(intern(lit("put-carray"), user_package), func_n3o(put_carray, 1));
  reg_fun(intern(lit("fill-carray"), user_package), func_n3o(fill_carray, 1));
  reg_fun(intern(lit("cptr-get"), user_package), func_n2o(cptr_getobj, 1));
  reg_fun(intern(lit("cptr-out"), user_package), func_n3o(cptr_out, 2));
#if HAVE_MMAP
  reg_fun(intern(lit("mmap"), user_package), func_n7o(mmap_wrap, 4));
  reg_fun(intern(lit("munmap"), user_package), func_n1(munmap_wrap));
  reg_fun(intern(lit("mprotect"), user_package), func_n4o(mprotect_wrap, 2));
  reg_fun(intern(lit("madvise"), user_package), func_n4o(madvise_wrap, 2));
  reg_fun(intern(lit("msync"), user_package), func_n4o(msync_wrap, 2));
  reg_varl(intern(lit("map-growsdown"), user_package), num_fast(MAP_GROWSDOWN));
  reg_varl(intern(lit("map-locked"), user_package), num_fast(MAP_LOCKED));
  reg_varl(intern(lit("map-noreserve"), user_package), num_fast(MAP_NORESERVE));
  reg_varl(intern(lit("map-populate"), user_package), num_fast(MAP_POPULATE));
  reg_varl(intern(lit("map-nonblock"), user_package), num_fast(MAP_NONBLOCK));
  reg_varl(intern(lit("map-stack"), user_package), num_fast(MAP_STACK));
  reg_varl(intern(lit("map-hugetlb"), user_package), num_fast(MAP_HUGETLB));
  reg_varl(intern(lit("map-shared"), user_package), num_fast(MAP_SHARED));
  reg_varl(intern(lit("map-private"), user_package), num_fast(MAP_PRIVATE));
  reg_varl(intern(lit("map-fixed"), user_package), num_fast(MAP_FIXED));
  reg_varl(intern(lit("map-anon"), user_package), num_fast(MAP_ANON));
  reg_varl(intern(lit("map-huge-shift"), user_package), num_fast(MAP_HUGE_SHIFT));
  reg_varl(intern(lit("map-huge-mask"), user_package), num_fast(MAP_HUGE_MASK));
  reg_varl(intern(lit("prot-read"), user_package), num_fast(PROT_READ));
  reg_varl(intern(lit("prot-write"), user_package), num_fast(PROT_WRITE));
  reg_varl(intern(lit("prot-exec"), user_package), num_fast(PROT_EXEC));
  reg_varl(intern(lit("prot-none"), user_package), num_fast(PROT_NONE));
  reg_varl(intern(lit("prot-growsdown"), user_package), num_fast(PROT_GROWSDOWN));
  reg_varl(intern(lit("prot-growsup"), user_package), num_fast(PROT_GROWSUP));
  reg_varl(intern(lit("madv-normal"), user_package), num_fast(MADV_NORMAL));
  reg_varl(intern(lit("madv-random"), user_package), num_fast(MADV_RANDOM));
  reg_varl(intern(lit("madv-sequential"), user_package), num_fast(MADV_SEQUENTIAL));
  reg_varl(intern(lit("madv-willneed"), user_package), num_fast(MADV_WILLNEED));
  reg_varl(intern(lit("madv-dontneed"), user_package), num_fast(MADV_DONTNEED));
  reg_varl(intern(lit("madv-free"), user_package), num_fast(MADV_FREE));
  reg_varl(intern(lit("madv-remove"), user_package), num_fast(MADV_REMOVE));
  reg_varl(intern(lit("madv-dontfork"), user_package), num_fast(MADV_DONTFORK));
  reg_varl(intern(lit("madv-dofork"), user_package), num_fast(MADV_DOFORK));
  reg_varl(intern(lit("madv-mergeable"), user_package), num_fast(MADV_MERGEABLE));
  reg_varl(intern(lit("madv-unmergeable"), user_package), num_fast(MADV_UNMERGEABLE));
  reg_varl(intern(lit("madv-hugepage"), user_package), num_fast(MADV_HUGEPAGE));
  reg_varl(intern(lit("madv-nohugepage"), user_package), num_fast(MADV_NOHUGEPAGE));
  reg_varl(intern(lit("madv-dontdump"), user_package), num_fast(MADV_DONTDUMP));
  reg_varl(intern(lit("madv-dodump"), user_package), num_fast(MADV_DODUMP));
  reg_varl(intern(lit("madv-wipeonfork"), user_package), num_fast(MADV_WIPEONFORK));
  reg_varl(intern(lit("madv-keeponfork"), user_package), num_fast(MADV_KEEPONFORK));
  reg_varl(intern(lit("madv-hwpoison"), user_package), num_fast(MADV_HWPOISON));
  reg_varl(intern(lit("ms-async"), user_package), num_fast(MS_ASYNC));
  reg_varl(intern(lit("ms-sync"), user_package), num_fast(MS_SYNC));
  reg_varl(intern(lit("ms-invalidate"), user_package), num_fast(MS_INVALIDATE));
  reg_varl(intern(lit("page-size"), user_package), num_fast(sysconf(_SC_PAGESIZE)));
#endif
  reg_fun(intern(lit("make-union"), user_package), func_n3o(make_union, 1));
  reg_fun(intern(lit("union-members"), user_package), func_n1(union_members));
  reg_fun(intern(lit("union-get"), user_package), func_n2(union_get));
  reg_fun(intern(lit("union-put"), user_package), func_n3(union_put));
  reg_fun(intern(lit("union-in"), user_package), func_n3(union_in));
  reg_fun(intern(lit("union-out"), user_package), func_n3(union_out));
  reg_fun(intern(lit("make-zstruct"), user_package), func_n1v(make_zstruct));
  reg_fun(intern(lit("zero-fill"), user_package), func_n2(zero_fill));
  reg_fun(intern(lit("put-obj"), user_package), func_n3o(put_obj, 2));
  reg_fun(intern(lit("get-obj"), user_package), func_n2o(get_obj, 1));
  reg_fun(intern(lit("fill-obj"), user_package), func_n3o(fill_obj, 2));
  reg_fun(intern(lit("dyn-size"), system_package), func_n2(dyn_size));
#if HAVE_SOCKETS
  reg_fun(intern(lit("sock-opt"), user_package), func_n4o(sock_opt, 3));
  reg_fun(intern(lit("sock-set-opt"), user_package), func_n5o(sock_set_opt, 4));
  reg_varl(intern(lit("sol-socket"), user_package), num_fast(SOL_SOCKET));
  reg_varl(intern(lit("ipproto-ip"), user_package), num_fast(IPPROTO_IP));
  reg_varl(intern(lit("ipproto-ipv6"), user_package), num_fast(IPPROTO_IPV6));
  reg_varl(intern(lit("ipproto-tcp"), user_package), num_fast(IPPROTO_TCP));
  reg_varl(intern(lit("ipproto-udp"), user_package), num_fast(IPPROTO_UDP));
  reg_varl(intern(lit("so-acceptconn"), user_package), num_fast(SO_ACCEPTCONN));
  reg_varl(intern(lit("so-broadcast"), user_package), num_fast(SO_BROADCAST));
  reg_varl(intern(lit("so-debug"), user_package), num_fast(SO_DEBUG));
  reg_varl(intern(lit("so-dontroute"), user_package), num_fast(SO_DONTROUTE));
  reg_varl(intern(lit("so-error"), user_package), num_fast(SO_ERROR));
  reg_varl(intern(lit("so-keepalive"), user_package), num_fast(SO_KEEPALIVE));
  reg_varl(intern(lit("so-linger"), user_package), num_fast(SO_LINGER));
  reg_varl(intern(lit("so-oobinline"), user_package), num_fast(SO_OOBINLINE));
  reg_varl(intern(lit("so-rcvbuf"), user_package), num_fast(SO_RCVBUF));
  reg_varl(intern(lit("so-rcvlowat"), user_package), num_fast(SO_RCVLOWAT));
  reg_varl(intern(lit("so-rcvtimeo"), user_package), num_fast(SO_RCVTIMEO));
  reg_varl(intern(lit("so-reuseaddr"), user_package), num_fast(SO_REUSEADDR));
  reg_varl(intern(lit("so-sndbuf"), user_package), num_fast(SO_SNDBUF));
  reg_varl(intern(lit("so-sndlowat"), user_package), num_fast(SO_SNDLOWAT));
  reg_varl(intern(lit("so-sndtimeo"), user_package), num_fast(SO_SNDTIMEO));
  reg_varl(intern(lit("so-type"), user_package), num_fast(SO_TYPE));
  reg_varl(intern(lit("ipv6-join-group"), user_package), num_fast(IPV6_JOIN_GROUP));
  reg_varl(intern(lit("ipv6-leave-group"), user_package), num_fast(IPV6_LEAVE_GROUP));
  reg_varl(intern(lit("ipv6-multicast-hops"), user_package), num_fast(IPV6_MULTICAST_HOPS));
  reg_varl(intern(lit("ipv6-multicast-if"), user_package), num_fast(IPV6_MULTICAST_IF));
  reg_varl(intern(lit("ipv6-multicast-loop"), user_package), num_fast(IPV6_MULTICAST_LOOP));
  reg_varl(intern(lit("ipv6-unicast-hops"), user_package), num_fast(IPV6_UNICAST_HOPS));
  reg_varl(intern(lit("ipv6-v6only"), user_package), num_fast(IPV6_V6ONLY));
  reg_varl(intern(lit("tcp-nodelay"), user_package), num_fast(TCP_NODELAY));
#endif
  ffi_typedef_hash = make_hash(hash_weak_none, nil);
  ffi_struct_tag_hash = make_hash(hash_weak_none, nil);
  ffi_init_types();
  ffi_init_extra_types();
}

void ffi_compat_fixup(int compat_ver)
{
  if (compat_ver <= 227) {
    val ca_uint = func_n2o(carray_uint, 1);
    val ca_int = func_n2o(carray_int, 1);
    val uint_ca = func_n1(uint_carray);
    val int_ca = func_n1(int_carray);

    reg_fun(intern(lit("carray-unum"), user_package), ca_uint);
    reg_fun(intern(lit("carray-num"), user_package), ca_int);
    reg_fun(intern(lit("unum-carray"), user_package), uint_ca);
    reg_fun(intern(lit("num-carray"), user_package), int_ca);
  }
}
