/* Copyright 2017
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
#include <dirent.h>
#include <time.h>
#include "config.h"
#if HAVE_LIBFFI
#include <ffi.h>
#endif
#if HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include ALLOCA_H
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
#include "ffi.h"

#define zalloca(size) memset(alloca(size), 0, size)

#define alignof(type) offsetof(struct {char x; type y;}, y)

#define pad_retval(size) (convert(size_t, size) > sizeof (ffi_arg) \
                          ? (size) \
                          : sizeof (ffi_arg))

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
#endif

val uint8_s, int8_s;
val uint16_s, int16_s;
val uint32_s, int32_s;
val uint64_s, int64_s;

val char_s, uchar_s, bchar_s, wchar_s;
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

val struct_s, union_s;

val str_d_s, wstr_s, wstr_d_s, bstr_s, bstr_d_s;

val buf_d_s;


val ptr_in_s, ptr_out_s, ptr_in_d_s, ptr_out_d_s, ptr_out_s_s, ptr_s;

val closure_s;

val sbit_s, ubit_s, bit_s;

val enum_s;

val align_s;

val bool_s;

val ffi_type_s, ffi_call_desc_s, ffi_closure_s;

static val ffi_typedef_hash;

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

struct smemb {
  val mname;
  val mtype;
  struct txr_ffi_type *mtft;
  cnum offs;
};

struct txr_ffi_type {
  val self;
  ffi_type *ft;
  ffi_type *elements[1];
  val lt;
  val syntax;
  val eltype;
  cnum size, align;
  unsigned shift, mask;
  cnum nelem;
  struct smemb *memb;
  val tag;
  val sym_num, num_sym;
  unsigned null_term : 1;
  unsigned by_value_in : 1;
  unsigned char_conv : 1;
  unsigned wchar_conv : 1;
  unsigned bchar_conv : 1;
  struct txr_ffi_type *(*clone)(struct txr_ffi_type *);
  void (*put)(struct txr_ffi_type *, val obj, mem_t *dst, val self);
  val (*get)(struct txr_ffi_type *, mem_t *src, val self);
  val (*in)(struct txr_ffi_type *, int copy, mem_t *src, val obj, val self);
  void (*out)(struct txr_ffi_type *, int copy, val obj, mem_t *dest, val self);
  void (*release)(struct txr_ffi_type *, val obj, mem_t *dst);
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

static struct txr_ffi_type *ffi_type_struct_checked(val obj)
{
  return coerce(struct txr_ffi_type *, cobj_handle(obj, ffi_type_s));
}

#if HAVE_LIBFFI
static ffi_type *ffi_get_type(val obj)
{
  struct txr_ffi_type *tffi = ffi_type_struct_checked(obj);
  return tffi->ft;
}
#endif

static val ffi_get_lisp_type(val obj)
{
  struct txr_ffi_type *tffi = ffi_type_struct_checked(obj);
  return tffi->lt;
}

static void ffi_type_print_op(val obj, val out, val pretty, struct strm_ctx *ctx)
{
  struct txr_ffi_type *tft = ffi_type_struct(obj);
  put_string(lit("#<"), out);
  obj_print_impl(obj->co.cls, out, pretty, ctx);
  format(out, lit(" ~!~s>"), tft->syntax, nao);
}

static void ffi_type_struct_destroy_op(val obj)
{
  struct txr_ffi_type *tft = ffi_type_struct(obj);
#if HAVE_LIBFFI
  ffi_type *ft = tft->ft;
#endif

#if HAVE_LIBFFI
  ft->elements = 0;
  free(ft);
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

static struct txr_ffi_closure *ffi_closure_struct_checked(val obj)
{
  return coerce(struct txr_ffi_closure *, cobj_handle(obj, ffi_closure_s));
}

static void ffi_closure_print_op(val obj, val out,
                                 val pretty, struct strm_ctx *ctx)
{
  struct txr_ffi_closure *tfcl = ffi_closure_struct(obj);
  put_string(lit("#<"), out);
  obj_print_impl(obj->co.cls, out, pretty, ctx);
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
}

static mem_t *ffi_fixed_alloc(struct txr_ffi_type *tft, val obj, val self)
{
  return chk_calloc(1, tft->size);
}

static mem_t *ffi_varray_alloc(struct txr_ffi_type *tft, val obj, val self)
{
  cnum len = c_num(length(obj));
  val eltype = tft->eltype;
  struct txr_ffi_type *etft = ffi_type_struct(eltype);
  return chk_calloc(len, etft->size);
}

static void ffi_noop_free(void *ptr)
{
}

static val ffi_void_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  return nil;
}

static void ffi_simple_release(struct txr_ffi_type *tft, val obj, mem_t *dst)
{
  mem_t **loc = coerce(mem_t **, dst);
  free(*loc);
  *loc = 0;
}

#define align_sw_get(type, src)  {                                      \
  const int al = ((alignof (type) - 1) & coerce(uint_ptr_t, src)) == 0; \
  const size_t sz = sizeof (type);                                      \
  mem_t *src_prev = src;                                                \
  mem_t *buf = al ? src : convert(mem_t *, alloca(sz));                 \
  mem_t *src = al ? buf : (memcpy(buf, src_prev, sz), buf);

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

#if HAVE_I8
static void ffi_i8_put(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  i8_t v = c_i8(n, self);
  *coerce(i8_t *, dst) = v;
}

static val ffi_i8_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  return num_fast(*src);
}

static void ffi_u8_put(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  u8_t v = c_u8(n, self);
  *coerce(u8_t *, dst) = v;
}

static val ffi_u8_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  return num_fast(*coerce(u8_t *, src));
}

#endif

#if HAVE_I16
static void ffi_i16_put(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  i16_t v = c_i16(n, self);
  align_sw_put(i16_t, dst, *coerce(i16_t *, dst) = v);
}

static val ffi_i16_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  align_sw_get(i16_t, src);
  i16_t n = *coerce(i16_t *, src);
  return num_fast(n);
  align_sw_end;
}

static void ffi_u16_put(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  u16_t v = c_u16(n, self);
  align_sw_put(u16_t, dst, *coerce(u16_t *, dst) = v);
}

static val ffi_u16_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  align_sw_get(u16_t, src);
  u16_t n = *coerce(u16_t *, src);
  return num_fast(n);
  align_sw_end;
}
#endif

#if HAVE_I32
static void ffi_i32_put(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  i32_t v = c_i32(n, self);
  align_sw_put(i32_t, dst, *coerce(i32_t *, dst) = v);
}

static val ffi_i32_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  align_sw_get(i32_t, src);
  i32_t n = *coerce(i32_t *, src);
  return num(n);
  align_sw_end;
}

static void ffi_u32_put(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  u32_t v = c_u32(n, self);
  align_sw_put(u32_t, dst, *coerce(u32_t *, dst) = v);
}

static val ffi_u32_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  align_sw_get(u32_t, src);
  u32_t n = *coerce(u32_t *, src);
  return unum(n);
  align_sw_end;
}
#endif

#if HAVE_I64
static void ffi_i64_put(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  i64_t v = c_i64(n, self);
  align_sw_put(i64_t, dst, *coerce(i64_t *, dst) = v);
}

static val ffi_i64_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  align_sw_get(i64_t, src);
  i64_t n = *coerce(i64_t *, src);

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
  align_sw_put(u64_t, dst, *coerce(u64_t *, dst) = v);
}

static val ffi_u64_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  align_sw_get(u64_t, src);
  u64_t n = *coerce(u64_t *, src);

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
  *coerce(char *, dst) = v;
}

static val ffi_char_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  return chr(*coerce(char *, src));
}

static void ffi_uchar_put(struct txr_ffi_type *tft, val n, mem_t *dst,
                          val self)
{
  unsigned char v = c_uchar(n, self);
  *coerce(unsigned char *, dst) = v;
}

static val ffi_uchar_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  return num_fast(*src);
}

static val ffi_bchar_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  return chr(*src);
}

static void ffi_short_put(struct txr_ffi_type *tft, val n, mem_t *dst,
                          val self)
{
  short v = c_short(n, self);
  align_sw_put(short, dst, *coerce(short *, dst) = v);
}

static val ffi_short_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  align_sw_get(short, src);
  short n = *coerce(short *, src);
  return num_fast(n);
  align_sw_end;
}

static void ffi_ushort_put(struct txr_ffi_type *tft, val n, mem_t *dst,
                           val self)
{
  unsigned short v = c_ushort(n, self);
  align_sw_put(unsigned, dst, *coerce(unsigned short *, dst) = v);
}

static val ffi_ushort_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  unsigned short n = *coerce(unsigned short *, src);
  return num_fast(n);
}

static void ffi_int_put(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  int v = c_int(n, self);
  align_sw_put(int, dst, *coerce(int *, dst) = v);
}

static val ffi_int_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  align_sw_get(int, src);
  int n = *coerce(int *, src);
  return num(n);
  align_sw_end;
}

static void ffi_uint_put(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  unsigned v = c_uint(n, self);
  align_sw_put(unsigned, dst, *coerce(unsigned *, dst) = v);
}

static val ffi_uint_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  align_sw_get(unsigned, src);
  unsigned n = *coerce(unsigned *, src);
  return unum(n);
  align_sw_end;
}

static void ffi_long_put(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  long v = c_long(n, self);
  align_sw_put(long, dst, *coerce(long *, dst) = v);
}

static val ffi_long_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  align_sw_get(long, src);
  long n = *coerce(long *, src);
  return num(n);
  align_sw_end;
}

static void ffi_ulong_put(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  unsigned long v = c_ulong(n, self);
  align_sw_put(unsigned long, dst, *coerce(unsigned long *, dst) = v);
}

static val ffi_ulong_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  align_sw_get(unsigned long, src);
  unsigned long n = *coerce(unsigned long *, src);
  return unum(n);
  align_sw_end;
}

static void ffi_float_put(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  double v;

  switch (type(n)) {
  case NUM:
  case CHR:
    v = c_num(n);
    break;
  case BGNUM:
    n = int_flo(n);
    /* fallthrough */
  default:
    v = c_flo(n);
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
  return flo(n);
  align_sw_end;
}

static void ffi_double_put(struct txr_ffi_type *tft, val n, mem_t *dst,
                           val self)
{
  double v;

  switch (type(n)) {
  case NUM:
  case CHR:
    v = c_num(n);
    break;
  case BGNUM:
    n = int_flo(n);
    /* fallthrough */
  default:
    v = c_flo(n);
    break;
  }

  align_sw_put(double, dst, *coerce(double *, dst) = v);
}

static val ffi_double_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  align_sw_get(double, src);
  double n = *coerce(double *, src);
  return flo(n);
  align_sw_end;
}

static void ffi_val_put(struct txr_ffi_type *tft, val v, mem_t *dst, val self)
{
  align_sw_put(val *, dst, *coerce(val *, dst) = v);
}

static val ffi_val_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  align_sw_get(val, src);
  val v = *coerce(val *, src);
  if (!valid_object_p(v))
    uw_throwf(error_s, lit("~a: bit pattern ~0,0*x isn't a valid Lisp object"),
              self, num_fast(sizeof (v) * 2), bits(v), nao);
  return v;
  align_sw_end;
}

static void ffi_be_i16_put(struct txr_ffi_type *tft, val n,
                           mem_t *dst, val self)
{
  cnum v = c_num(n);

  if (v < -32768 || v > 32767)
    uw_throwf(error_s, lit("~a: value ~s is out of signed 16 bit range"),
              self, n, nao);

  dst[0] = (v >> 8) & 0xff;
  dst[1] = v & 0xff;
}

static val ffi_be_i16_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  cnum n = (src[0] << 8) | src[1];
  if ((n & 0x8000) != 0)
    n = -((n ^ 0xFFFF) + 1);
  return num(n);
}

static void ffi_be_u16_put(struct txr_ffi_type *tft, val n,
                           mem_t *dst, val self)
{
  cnum v = c_num(n);

  if (v < -32768 || v > 32767)
    uw_throwf(error_s, lit("~a: value ~s is out of signed 16 bit range"),
              self, n, nao);

  dst[0] = (v >> 8) & 0xff;
  dst[1] = v & 0xff;
}

static val ffi_be_u16_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  cnum n = (src[0] << 8) | src[1];
  return num(n);
}

static void ffi_le_i16_put(struct txr_ffi_type *tft, val n,
                           mem_t *dst, val self)
{
  cnum v = c_num(n);

  if (v < -32768 || v > 32767)
    uw_throwf(error_s, lit("~a: value ~s is out of signed 16 bit range"),
              self, n, nao);

  dst[1] = (v >> 8) & 0xff;
  dst[0] = v & 0xff;
}

static val ffi_le_i16_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  cnum n = (src[1] << 8) | src[0];
  if ((n & 0x8000) != 0)
    n = -((n ^ 0xFFFF) + 1);
  return num(n);
}

static void ffi_le_u16_put(struct txr_ffi_type *tft, val n,
                           mem_t *dst, val self)
{
  cnum v = c_num(n);

  if (v < 0|| v > 65535)
    uw_throwf(error_s, lit("~a: value ~s is out of unsigned 16 bit range"),
              self, n, nao);

  dst[1] = (v >> 8) & 0xff;
  dst[0] = v & 0xff;
}

static val ffi_le_u16_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  cnum n = (src[1] << 8) | src[0];
  return num(n);
}

static void ffi_be_i32_put(struct txr_ffi_type *tft, val n,
                           mem_t *dst, val self)
{
  cnum v = c_num(n);

  if (v < -convert(cnum, 0x7FFFFFFF) - 1 || v > 0x7FFFFFFF)
    uw_throwf(error_s, lit("~a: value ~s is out of signed 32 bit range"),
              self, n, nao);

  dst[0] = (v >> 24) & 0xff;
  dst[1] = (v >> 16) & 0xff;
  dst[2] = (v >> 8) & 0xff;
  dst[3] = v & 0xff;
}

static val ffi_be_i32_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  cnum n = (convert(cnum, src[0]) << 24 | convert(cnum, src[1]) << 16 |
            convert(cnum, src[2]) << 8 | src[3]);
  if ((n & 0x80000000) != 0)
    n = -((n ^ 0xFFFFFFFF) + 1);
  return num(n);
}

static void ffi_be_u32_put(struct txr_ffi_type *tft, val n,
                           mem_t *dst, val self)
{
  ucnum v = c_unum(n);

  if (v > 0xFFFFFFFF)
    uw_throwf(error_s, lit("~a: value ~s is out of unsigned 32 bit range"),
              self, n, nao);

  dst[0] = (v >> 24) & 0xff;
  dst[1] = (v >> 16) & 0xff;
  dst[2] = (v >> 8) & 0xff;
  dst[3] = v & 0xff;
}

static val ffi_be_u32_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  ucnum n = (convert(ucnum, src[0]) << 24 | convert(ucnum, src[1]) << 16 |
             convert(ucnum, src[2]) << 8 | src[3]);
  return unum(n);
}

static void ffi_le_i32_put(struct txr_ffi_type *tft, val n,
                           mem_t *dst, val self)
{
  cnum v = c_num(n);

  if (v < - convert(cnum, 0x7fffffff) - 1 || v > 0x7FFFFFFF)
    uw_throwf(error_s, lit("~a: value ~s is out of signed 32 bit range"),
              self, n, nao);

  dst[3] = (v >> 24) & 0xff;
  dst[2] = (v >> 16) & 0xff;
  dst[1] = (v >> 8) & 0xff;
  dst[0] = v & 0xff;
}

static val ffi_le_i32_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  cnum n = (convert(cnum, src[3]) << 24 | convert(cnum, src[2]) << 16 |
            convert(cnum, src[1]) << 8 | src[0]);
  if ((n & 0x80000000) != 0)
    n = -((n ^ 0xFFFFFFFF) + 1);
  return num(n);
}

static void ffi_le_u32_put(struct txr_ffi_type *tft, val n,
                           mem_t *dst, val self)
{
  ucnum v = c_unum(n);

  if (v > 0xFFFFFFFF)
    uw_throwf(error_s, lit("~a: value ~s is out of unsigned 32 bit range"),
              self, n, nao);

  dst[3] = (v >> 24) & 0xff;
  dst[2] = (v >> 16) & 0xff;
  dst[1] = (v >> 8) & 0xff;
  dst[0] = v & 0xff;
}

static val ffi_le_u32_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  ucnum n = (convert(ucnum, src[3]) << 24 | convert(ucnum, src[2]) << 16 |
             convert(ucnum, src[1]) << 8 | src[0]);
  return unum(n);
}

static void ffi_be_i64_put(struct txr_ffi_type *tft, val n,
                           mem_t *dst, val self)
{
#if SIZEOF_PTR >= 8
  cnum v = c_num(n);

  if (v < -convert(cnum, 0x7FFFFFFFFFFFFFFF - 1) || v > 0x7FFFFFFFFFFFFFFF)
    goto range;

  dst[0] = (v >> 56) & 0xff;
  dst[1] = (v >> 48) & 0xff;
  dst[2] = (v >> 40) & 0xff;
  dst[3] = (v >> 32) & 0xff;
  dst[4] = (v >> 24) & 0xff;
  dst[5] = (v >> 16) & 0xff;
  dst[6] = (v >> 8) & 0xff;
  dst[7] = v & 0xff;
#else
  cnum hi32 = c_num(ash(n, num_fast(-32)));
  ucnum lo32 = c_unum(logtrunc(n, num_fast(32)));

  if (hi32 < -convert(cnum, 0x7FFFFFFF - 1) || hi32 > 0x7FFFFFFF)
    goto range;

  dst[0] = (hi32 >> 24) & 0xff;
  dst[1] = (hi32 >> 16) & 0xff;
  dst[2] = (hi32 >> 8) & 0xff;
  dst[3] = hi32 & 0xff;
  dst[4] = (lo32 >> 24) & 0xff;
  dst[5] = (lo32 >> 16) & 0xff;
  dst[6] = (lo32 >> 8) & 0xff;
  dst[7] = lo32 & 0xff;
#endif
  return;
range:
    uw_throwf(error_s, lit("~a: value ~s is out of signed 64 bit range"),
              self, n, nao);
}

static val ffi_be_i64_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
#if SIZEOF_PTR >= 8
  cnum n = (convert(cnum, src[0]) << 56 | convert(cnum, src[1]) << 48 |
            convert(cnum, src[2]) << 40 | convert(cnum, src[3]) << 32 |
            convert(cnum, src[4]) << 24 | convert(cnum, src[5]) << 16 |
            convert(cnum, src[6]) << 8 | src[7]);
  if ((n & 0x8000000000000000) != 0)
    n = -((n ^ 0xFFFFFFFFFFFFFFFF) + 1);
  return num(n);
#else
  cnum hi32 = (convert(cnum, src[0]) << 24 | convert(cnum, src[1]) << 16 |
               convert(cnum, src[2]) << 8 | src[3]);
  cnum lo32 = (convert(cnum, src[4]) << 24 | convert(cnum, src[5]) << 16 |
               convert(cnum, src[6]) << 8 | src[7]);
  return logior(ash(num(hi32), num_fast(32)), unum(lo32));
#endif
}

static void ffi_be_u64_put(struct txr_ffi_type *tft, val n,
                           mem_t *dst, val self)
{
#if SIZEOF_PTR >= 8
  ucnum v = c_unum(n);

  if (v > 0xFFFFFFFFFFFFFFFF)
    goto range;

  dst[0] = (v >> 56) & 0xff;
  dst[1] = (v >> 48) & 0xff;
  dst[2] = (v >> 40) & 0xff;
  dst[3] = (v >> 32) & 0xff;
  dst[4] = (v >> 24) & 0xff;
  dst[5] = (v >> 16) & 0xff;
  dst[6] = (v >> 8) & 0xff;
  dst[7] = v & 0xff;
#else
  ucnum hi32 = c_unum(ash(n, num_fast(-32)));
  ucnum lo32 = c_unum(logtrunc(n, num_fast(32)));

  if (hi32 > 0xFFFFFFFF)
    goto range;

  dst[0] = (hi32 >> 24) & 0xff;
  dst[1] = (hi32 >> 16) & 0xff;
  dst[2] = (hi32 >> 8) & 0xff;
  dst[3] = hi32 & 0xff;
  dst[4] = (lo32 >> 24) & 0xff;
  dst[5] = (lo32 >> 16) & 0xff;
  dst[6] = (lo32 >> 8) & 0xff;
  dst[7] = lo32 & 0xff;
#endif
  return;
range:
    uw_throwf(error_s, lit("~a: value ~s is out of unsigned 64 bit range"),
              self, n, nao);
}

static val ffi_be_u64_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
#if SIZEOF_PTR >= 8
  ucnum n = (convert(ucnum, src[0]) << 56 | convert(ucnum, src[1]) << 48 |
             convert(ucnum, src[2]) << 40 | convert(ucnum, src[3]) << 32 |
             convert(ucnum, src[4]) << 24 | convert(ucnum, src[5]) << 16 |
             convert(ucnum, src[6]) << 8 | src[7]);
  return unum(n);
#else
  ucnum hi32 = (convert(ucnum, src[0]) << 24 | convert(ucnum, src[1]) << 16 |
                convert(ucnum, src[2]) << 8 | src[3]);
  ucnum lo32 = (convert(ucnum, src[4]) << 24 | convert(ucnum, src[5]) << 16 |
                convert(ucnum, src[6]) << 8 | src[7]);
  return logior(ash(unum(hi32), num_fast(32)), unum(lo32));
#endif
}

static void ffi_le_i64_put(struct txr_ffi_type *tft, val n,
                           mem_t *dst, val self)
{
#if SIZEOF_PTR >= 8
  cnum v = c_num(n);

  if (v < -convert(cnum, 0x7FFFFFFFFFFFFFFF - 1) || v > 0x7FFFFFFFFFFFFFFF)
    goto range;

  dst[7] = (v >> 56) & 0xff;
  dst[6] = (v >> 48) & 0xff;
  dst[5] = (v >> 40) & 0xff;
  dst[4] = (v >> 32) & 0xff;
  dst[3] = (v >> 24) & 0xff;
  dst[2] = (v >> 16) & 0xff;
  dst[1] = (v >> 8) & 0xff;
  dst[0] = v & 0xff;
#else
  cnum hi32 = c_num(ash(n, num_fast(-32)));
  ucnum lo32 = c_unum(logtrunc(n, num_fast(32)));

  if (hi32 < -convert(cnum, 0x7FFFFFFF - 1) || hi32 > 0x7FFFFFFF)
    goto range;

  dst[7] = (hi32 >> 24) & 0xff;
  dst[6] = (hi32 >> 16) & 0xff;
  dst[5] = (hi32 >> 8) & 0xff;
  dst[4] = hi32 & 0xff;
  dst[3] = (lo32 >> 24) & 0xff;
  dst[2] = (lo32 >> 16) & 0xff;
  dst[1] = (lo32 >> 8) & 0xff;
  dst[0] = lo32 & 0xff;
#endif
  return;
range:
    uw_throwf(error_s, lit("~a: value ~s is out of signed 64 bit range"),
              self, n, nao);
}

static val ffi_le_i64_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
#if SIZEOF_PTR >= 8
  cnum n = (convert(cnum, src[7]) << 56 | convert(cnum, src[6]) << 48 |
            convert(cnum, src[5]) << 40 | convert(cnum, src[4]) << 32 |
            convert(cnum, src[3]) << 24 | convert(cnum, src[2]) << 16 |
            convert(cnum, src[1]) << 8 | src[0]);
  if ((n & 0x8000000000000000) != 0)
    n = -((n ^ 0xFFFFFFFFFFFFFFFF) + 1);
  return num(n);
#else
  cnum hi32 = (convert(cnum, src[3]) << 24 | convert(cnum, src[2]) << 16 |
               convert(cnum, src[1]) << 8 | src[0]);
  cnum lo32 = (convert(cnum, src[3]) << 24 | convert(cnum, src[2]) << 16 |
               convert(cnum, src[1]) << 8 | src[0]);
  return logior(ash(num(hi32), num_fast(32)), unum(lo32));
#endif
}

static void ffi_le_u64_put(struct txr_ffi_type *tft, val n,
                           mem_t *dst, val self)
{
#if SIZEOF_PTR >= 8
  ucnum v = c_unum(n);

  if (v > 0xFFFFFFFFFFFFFFFF)
    goto range;

  dst[7] = (v >> 56) & 0xff;
  dst[6] = (v >> 48) & 0xff;
  dst[5] = (v >> 40) & 0xff;
  dst[4] = (v >> 32) & 0xff;
  dst[3] = (v >> 24) & 0xff;
  dst[2] = (v >> 16) & 0xff;
  dst[1] = (v >> 8) & 0xff;
  dst[0] = v & 0xff;
#else
  ucnum hi32 = c_unum(ash(n, num_fast(-32)));
  ucnum lo32 = c_unum(logtrunc(n, num_fast(32)));

  if (hi32 > 0xFFFFFFFF)
    goto range;

  dst[7] = (hi32 >> 24) & 0xff;
  dst[6] = (hi32 >> 16) & 0xff;
  dst[5] = (hi32 >> 8) & 0xff;
  dst[4] = hi32 & 0xff;
  dst[3] = (lo32 >> 24) & 0xff;
  dst[2] = (lo32 >> 16) & 0xff;
  dst[1] = (lo32 >> 8) & 0xff;
  dst[0] = lo32 & 0xff;
#endif
  return;
range:
    uw_throwf(error_s, lit("~a: value ~s is out of unsigned 64 bit range"),
              self, n, nao);
}

static val ffi_le_u64_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
#if SIZEOF_PTR >= 8
  ucnum n = (convert(ucnum, src[7]) << 56 | convert(ucnum, src[6]) << 48 |
             convert(ucnum, src[5]) << 40 | convert(ucnum, src[4]) << 32 |
             convert(ucnum, src[3]) << 24 | convert(ucnum, src[2]) << 16 |
             convert(ucnum, src[1]) << 8 | src[0]);
  return unum(n);
#else
  ucnum hi32 = (convert(ucnum, src[3]) << 24 | convert(ucnum, src[2]) << 16 |
                convert(ucnum, src[1]) << 8 | src[0]);
  ucnum lo32 = (convert(ucnum, src[3]) << 24 | convert(ucnum, src[2]) << 16 |
                convert(ucnum, src[1]) << 8 | src[0]);
  return logior(ash(unum(hi32), num_fast(32)), unum(lo32));
#endif
}

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
  align_sw_put(wchar_t, dst, *coerce(wchar_t *, dst) = c);
}

static val ffi_wchar_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  align_sw_get(wchar_t, src);
  wchar_t c = *coerce(wchar_t *, src);
  return chr(c);
  align_sw_end;
}

static void ffi_sbit_put(struct txr_ffi_type *tft, val n,
                         mem_t *dst, val self)
{
  unsigned mask = tft->mask;
  unsigned sbmask = mask ^ (mask >> 1);
  int shift = tft->shift;
  cnum cn = c_num(n);
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
  ucnum cn = c_unum(n);
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
  *coerce(i8_t *, dst) = v;
}

static val ffi_i8_rget(struct txr_ffi_type *tft, mem_t *src, val self)
{
  return num_fast(*src);
}

static void ffi_u8_rput(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  u8_t v = c_u8(n, self);
  *coerce(u8_t *, dst) = v;
}

static val ffi_u8_rget(struct txr_ffi_type *tft, mem_t *src, val self)
{
  return num_fast(*coerce(u8_t *, src));
}

#if HAVE_I16
static void ffi_i16_rput(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  i16_t v = c_i16(n, self);
  *coerce(ffi_arg *, dst) = v;
}

static val ffi_i16_rget(struct txr_ffi_type *tft, mem_t *src, val self)
{
  i16_t n = *coerce(ffi_arg *, src);
  return num_fast(n);
}

static void ffi_u16_rput(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  u16_t v = c_u16(n, self);
  *coerce(ffi_arg *, dst) = v;
}

static val ffi_u16_rget(struct txr_ffi_type *tft, mem_t *src, val self)
{
  u16_t n = *coerce(ffi_arg *, src);
  return num_fast(n);
}
#endif

#if HAVE_I32
static void ffi_i32_rput(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  i32_t v = c_i32(n, self);
  *coerce(ffi_arg *, dst) = v;
}

static val ffi_i32_rget(struct txr_ffi_type *tft, mem_t *src, val self)
{
  i32_t n = *coerce(ffi_arg *, src);
  return num(n);
}
#endif

static void ffi_u32_rput(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  u32_t v = c_u32(n, self);
  *coerce(ffi_arg *, dst) = v;
}

static val ffi_u32_rget(struct txr_ffi_type *tft, mem_t *src, val self)
{
  u32_t n = *coerce(ffi_arg *, src);
  return unum(n);
}

static void ffi_char_rput(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  char v = c_char(n, self);
  *coerce(ffi_arg *, dst) = v;
}

static val ffi_char_rget(struct txr_ffi_type *tft, mem_t *src, val self)
{
  return chr((char) *coerce(ffi_arg *, src));
}

static void ffi_uchar_rput(struct txr_ffi_type *tft, val n, mem_t *dst,
                          val self)
{
  unsigned char v = c_uchar(n, self);
  *coerce(ffi_arg *, dst) = v;
}

static val ffi_uchar_rget(struct txr_ffi_type *tft, mem_t *src, val self)
{
  return num_fast((unsigned char) *coerce(ffi_arg *, src));
}

static val ffi_bchar_rget(struct txr_ffi_type *tft, mem_t *src, val self)
{
  return chr((unsigned char) *coerce(ffi_arg *, src));
}

static void ffi_short_rput(struct txr_ffi_type *tft, val n, mem_t *dst,
                          val self)
{
  short v = c_short(n, self);
  *coerce(ffi_arg *, dst) = v;
}

static val ffi_short_rget(struct txr_ffi_type *tft, mem_t *src, val self)
{
  short n = *coerce(ffi_arg *, src);
  return num_fast(n);
}

static void ffi_ushort_rput(struct txr_ffi_type *tft, val n, mem_t *dst,
                           val self)
{
  unsigned short v = c_ushort(n, self);
  *coerce(ffi_arg *, dst) = v;
}

static val ffi_ushort_rget(struct txr_ffi_type *tft, mem_t *src, val self)
{
  unsigned short n = *coerce(ffi_arg *, src);
  return num_fast(n);
}

static void ffi_int_rput(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  int v = c_int(n, self);
  *coerce(ffi_arg *, dst) = v;
}

static val ffi_int_rget(struct txr_ffi_type *tft, mem_t *src, val self)
{
  int n = *coerce(ffi_arg *, src);
  return num(n);
}

static void ffi_uint_rput(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  unsigned v = c_uint(n, self);
  *coerce(ffi_arg *, dst) = v;
}

static val ffi_uint_rget(struct txr_ffi_type *tft, mem_t *src, val self)
{
  unsigned n = *coerce(ffi_arg *, src);
  return unum(n);
}

static void ffi_long_rput(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  long v = c_long(n, self);
  *coerce(ffi_arg *, dst) = v;
}

static val ffi_long_rget(struct txr_ffi_type *tft, mem_t *src, val self)
{
  long n = *coerce(ffi_arg *, src);
  return num(n);
}

static void ffi_ulong_rput(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  unsigned long v = c_ulong(n, self);
  *coerce(ffi_arg *, dst) = v;
}

static val ffi_ulong_rget(struct txr_ffi_type *tft, mem_t *src, val self)
{
  unsigned long n = *coerce(ffi_arg *, src);
  return unum(n);
}

static void ffi_wchar_rput(struct txr_ffi_type *tft, val ch, mem_t *dst,
                           val self)
{
  wchar_t c = c_chr(ch);
  *coerce(ffi_arg *, dst) = c;
}

static val ffi_wchar_rget(struct txr_ffi_type *tft, mem_t *src, val self)
{
  wchar_t c = *coerce(ffi_arg *, src);
  return chr(c);
}

static void ffi_be_i16_rput(struct txr_ffi_type *tft, val n, mem_t *dst,
                            val self)
{
  memset(dst, 0, 6);
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

static void ffi_cptr_put(struct txr_ffi_type *tft, val n, mem_t *dst,
                         val self)
{
  mem_t *p = cptr_handle(n, tft->tag, self);
  *coerce(mem_t **, dst) = p;
}

static val ffi_cptr_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  mem_t *p = *coerce(mem_t **, src);
  return cptr_typed(p, tft->tag, 0);
}

static mem_t *ffi_cptr_alloc(struct txr_ffi_type *tft, val ptr, val self)
{
  return coerce(mem_t *, cptr_addr_of(ptr, tft->tag, self));
}

static val ffi_str_in(struct txr_ffi_type *tft, int copy,
                      mem_t *src, val obj, val self)
{
  char **loc = coerce(char **, src);
  if (copy)
    obj = if2(*loc, string_utf8(*loc));
  free(*loc);
  *loc = 0;
  return obj;
}

static void ffi_str_put(struct txr_ffi_type *tft, val s, mem_t *dst,
                        val self)
{
  if (s == nil) {
    *coerce(const char **, dst) = 0;
  } else {
    const wchar_t *ws = c_str(s);
    char *u8s = utf8_dup_to(ws);
    *coerce(const char **, dst) = u8s;
  }
}

static val ffi_str_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  const char *p = *coerce(const char **, src);
  return p ? string_utf8(p) : nil;
}

static val ffi_str_d_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  char **loc = coerce(char **, src);
  val ret = *loc ? string_utf8(*loc) : nil;
  free(*loc);
  *loc = 0;
  return ret;
}

static val ffi_wstr_in(struct txr_ffi_type *tft, int copy,
                       mem_t *src, val obj, val self)
{
  wchar_t **loc = coerce(wchar_t **, src);
  if (copy)
    obj = if2(*loc, string(*loc));
  free(*loc);
  *loc = 0;
  return obj;
}

static val ffi_wstr_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  const wchar_t *p = *coerce(wchar_t **, src);
  return p ? string(p) : 0;
}

static void ffi_wstr_put(struct txr_ffi_type *tft, val s, mem_t *dst,
                           val self)
{
  if (s == nil) {
    *coerce(const wchar_t **, dst) = 0;
  } else {
    const wchar_t *ws = c_str(s);
    *coerce(const wchar_t **, dst) = chk_strdup(ws);
  }
}

static val ffi_wstr_d_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  wchar_t **loc = coerce(wchar_t **, src);
  val ret = *loc ? string_own(*loc) : nil;
  *loc = 0;
  return ret;
}

static val ffi_bstr_in(struct txr_ffi_type *tft, int copy,
                       mem_t *src, val obj, val self)
{
  unsigned char **loc = coerce(unsigned char **, src);
  if (copy)
    obj = if2(*loc, string_8bit(*loc));
  free(*loc);
  *loc = 0;
  return obj;
}

static void ffi_bstr_put(struct txr_ffi_type *tft, val s, mem_t *dst,
                         val self)
{
  if (s == nil) {
    *coerce(unsigned char **, dst) = 0;
  } else {
    const wchar_t *ws = c_str(s);
    unsigned char *u8s = chk_strdup_8bit(ws);
    *coerce(unsigned char **, dst) = u8s;
  }
}

static val ffi_bstr_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  unsigned char *p = *coerce(unsigned char **, src);
  return p ? string_8bit(p) : nil;
}

static val ffi_bstr_d_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  unsigned char **loc = coerce(unsigned char **, src);
  val ret = *loc ? string_8bit(*loc) : nil;
  free(*loc);
  *loc = 0;
  return ret;
}

static val ffi_buf_in(struct txr_ffi_type *tft, int copy, mem_t *src,
                      val obj, val self)
{
  mem_t **loc = coerce(mem_t **, src);
  mem_t *origptr = if3(obj, buf_get(obj, self), 0);

  if (copy && *loc != origptr)
    obj = if2(*loc, make_duplicate_buf(length_buf(obj), *loc));

  return obj;
}

static void ffi_buf_put(struct txr_ffi_type *tft, val buf, mem_t *dst,
                        val self)
{
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
  return p ? make_duplicate_buf(num(tft->nelem), p) : nil;
}

static val ffi_buf_d_in(struct txr_ffi_type *tft, int copy, mem_t *src,
                        val obj, val self)
{
  mem_t **loc = coerce(mem_t **, src);

  if (copy) {
    obj = if2(*loc, make_borrowed_buf(num(tft->nelem), *loc));
    *loc = 0;
  }

  return obj;
}

static void ffi_buf_d_put(struct txr_ffi_type *tft, val buf, mem_t *dst,
                          val self)
{
  if (buf == nil) {
    *coerce(const mem_t **, dst) = 0;
  } else {
    mem_t *b = buf_get(buf, self);
    *coerce(const mem_t **, dst) = chk_copy_obj(b, c_num(length(buf)));
  }
}

static val ffi_buf_d_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  mem_t **loc = coerce(mem_t **, src);
  val ret = *loc ? make_borrowed_buf(num(tft->nelem), *loc) : nil;
  *loc = 0;
  return ret;
}

#if HAVE_LIBFFI
static void ffi_closure_put(struct txr_ffi_type *tft, val ptr, mem_t *dst,
                            val self)
{
  val type = typeof(ptr);
  mem_t *p = 0;

  if (type == cptr_s) {
    p = ptr->co.handle;
  } else if (type == ffi_closure_s) {
    struct txr_ffi_closure *tfcl = ffi_closure_struct(ptr);
    p = tfcl->fptr;
  } else {
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
  if (tgtft->in != 0 && tgtft->by_value_in)
    tgtft->in(tgtft, 0, *loc, obj, self);
  return obj;
}

static void ffi_ptr_in_out(struct txr_ffi_type *tft, int copy, val s,
                           mem_t *dst, val self)
{
  val tgttype = tft->eltype;
  struct txr_ffi_type *tgtft = ffi_type_struct(tgttype);
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
  *coerce(mem_t **, dst) =  0;
}

static val ffi_ptr_out_s_in(struct txr_ffi_type *tft, int copy,
                            mem_t *src, val obj, val self)
{
  val tgttype = tft->eltype;
  struct txr_ffi_type *tgtft = ffi_type_struct(tgttype);
  mem_t **loc = coerce(mem_t **, src);
  if (tgtft->in != 0)
    obj = tgtft->in(tgtft, 1, *loc, obj, self);
  else
    obj = tgtft->get(tgtft, *loc, self);
  return obj;
}

static void ffi_ptr_in_release(struct txr_ffi_type *tft, val obj, mem_t *dst)
{
  struct txr_ffi_type *tgtft = ffi_type_struct(tft->eltype);
  mem_t **loc = coerce(mem_t **, dst);

  if (tgtft->release != 0 && *loc != 0)
    tgtft->release(tgtft, obj, *loc);
  free(*loc);
  *loc = 0;
}

static val ffi_struct_in(struct txr_ffi_type *tft, int copy, mem_t *src,
                         val strct, val self)
{
  cnum i, nmemb = tft->nelem;
  struct smemb *memb = tft->memb;

  if (!copy && (!tft->by_value_in || strct == nil))
    return strct;

  if (strct == nil) {
    args_decl(args, 0);
    strct = make_struct(tft->lt, nil, args);
  }

  for (i = 0; i < nmemb; i++) {
    val slsym = memb[i].mname;
    struct txr_ffi_type *mtft = memb[i].mtft;
    ucnum offs = memb[i].offs;
    if (slsym) {
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
  args_decl(args, 0);
  val strct = make_struct(tft->lt, nil, args);

  for (i = 0; i < nmemb; i++) {
    val slsym = memb[i].mname;
    struct txr_ffi_type *mtft = memb[i].mtft;
    ucnum offs = memb[i].offs;
    if (slsym) {
      val slval = mtft->get(mtft, src + offs, self);
      slotset(strct, slsym, slval);
    }
  }

  return strct;
}

static void ffi_struct_release(struct txr_ffi_type *tft, val strct, mem_t *dst)
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
        mtft->release(mtft, slval, dst + offs);
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
                               cnum nelem)
{
  int nt = tft->null_term;
  const wchar_t *wstr = c_str(str);
  cnum needed = utf8_to_buf(0, wstr, nt);

  if (needed <= nelem) {
    utf8_to_buf(dst, wstr, nt);
    memset(dst + needed, 0, nelem - needed);
  } else {
    char *u8str = utf8_dup_to(wstr);
    memcpy(dst, u8str, nelem);
    dst[nelem - 1] = 0;
    free(u8str);
  }
}

static val ffi_wchar_array_get(struct txr_ffi_type *tft, mem_t *src,
                               cnum nelem)
{
  if (nelem == 0) {
    return null_string;
  } else {
    const wchar_t *wchptr = coerce(const wchar_t *, src);

    if (tft->null_term) {
      return string(wchptr);
    } else {
      val ustr = mkustring(num_fast(nelem));
      return init_str(ustr, wchptr);
    }
  }
}

static void ffi_wchar_array_put(struct txr_ffi_type *tft, val str, mem_t *dst,
                                cnum nelem)
{
  const wchar_t *wstr = c_str(str);
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
  const wchar_t *wstr = c_str(str);
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
    if (tft->char_conv) {
      val str = ffi_char_array_get(tft, src, tft->nelem);
      return if3(vec, replace(vec, str, zero, t), str);
    } else if (tft->wchar_conv) {
      val str = ffi_wchar_array_get(tft, src, tft->nelem);
      return if3(vec, replace(vec, str, zero, t), str);
    } else if (tft->bchar_conv) {
      val str = ffi_bchar_array_get(tft, src, tft->nelem);
      return if3(vec, replace(vec, str, zero, t), str);
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
  cnum i;
  ucnum offs = 0;

  for (i = 0; i < nelem; i++) {
    if (nt && i == nelem - 1) {
      memset(dst + offs, 0, elsize);
      break;
    } else {
      val elval = ref(vec, num_fast(i));
      etft->put(etft, elval, dst + offs, self);
      offs += elsize;
    }
  }
}

static void ffi_array_put(struct txr_ffi_type *tft, val vec, mem_t *dst,
                          val self)
{
  if (tft->char_conv && stringp(vec))
    ffi_char_array_put(tft, vec, dst, tft->nelem);
  else if (tft->wchar_conv && stringp(vec))
    ffi_wchar_array_put(tft, vec, dst, tft->nelem);
  else if (tft->bchar_conv && stringp(vec))
    ffi_bchar_array_put(tft, vec, dst, tft->nelem, self);
  else
    ffi_array_put_common(tft, vec, dst, self, tft->nelem);
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
  if (tft->char_conv && stringp(vec))
    ffi_char_array_put(tft, vec, dst, tft->nelem);
  else if (tft->wchar_conv && stringp(vec))
    ffi_wchar_array_put(tft, vec, dst, tft->nelem);
  else if (tft->bchar_conv && stringp(vec))
    ffi_bchar_array_put(tft, vec, dst, tft->nelem, self);
  else
    ffi_array_out_common(tft, copy, vec, dst, self, tft->nelem);
}

static val ffi_array_get_common(struct txr_ffi_type *tft, mem_t *src, val self,
                                cnum nelem)
{
  val eltype = tft->eltype;

  if (tft->char_conv) {
    return ffi_char_array_get(tft, src, nelem);
  } else if (tft->wchar_conv) {
    return ffi_wchar_array_get(tft, src, nelem);
  } else if (tft->bchar_conv) {
    return ffi_bchar_array_get(tft, src, nelem);
  } else {
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

static val ffi_array_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  cnum nelem = tft->nelem;
  return ffi_array_get_common(tft, src, self, nelem);
}

static void ffi_array_release_common(struct txr_ffi_type *tft, val vec,
                                     mem_t *dst, cnum nelem)
{
  val eltype = tft->eltype;
  ucnum offs = 0;
  struct txr_ffi_type *etft = ffi_type_struct(eltype);
  cnum elsize = etft->size, i;
  cnum znelem = if3(tft->null_term && nelem > 0 &&
                    vec && length(vec) < num_fast(nelem), nelem - 1, nelem);

  if (vec == nil)
    return;

  if (tft->char_conv || tft->bchar_conv || tft->wchar_conv)
    return;

  for (i = 0; i < znelem; i++) {
    if (etft->release != 0) {
      val elval = ref(vec, num_fast(i));
      etft->release(etft, elval, dst + offs);
    }
    offs += elsize;
  }
}

static void ffi_array_release(struct txr_ffi_type *tft, val vec, mem_t *dst)
{
  ffi_array_release_common(tft, vec, dst, tft->nelem);
}

static void ffi_varray_put(struct txr_ffi_type *tft, val vec, mem_t *dst,
                           val self)
{
  cnum nelem = c_num(length(vec)) + tft->null_term;
  ffi_array_put_common(tft, vec, dst, self, nelem);
}

static val ffi_varray_in(struct txr_ffi_type *tft, int copy, mem_t *src,
                         val vec, val self)
{
  if (copy && vec) {
    cnum nelem = c_num(length(vec)) + tft->null_term;
    return ffi_array_in_common(tft, copy, src, vec, self, nelem);
  }
  return vec;
}

static val ffi_varray_null_term_in(struct txr_ffi_type *tft, int copy, mem_t *src,
                                   val vec_in, val self)
{
  val vec = vector(zero, nil);
  val eltype = tft->eltype;
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

    if (etft->in != 0) {
      val elval = ref(vec, num_fast(i));
      vec_push(vec, elval);
    } else if (copy) {
      val elval = etft->get(etft, src + offs, self);
      vec_push(vec, elval);
    }

    offs += elsize;
  }

  return replace(vec_in, vec, 0, length_vec(vec));
}

static val ffi_varray_null_term_get(struct txr_ffi_type *tft, mem_t *src,
                                    val self)
{
  val eltype = tft->eltype;

  if (tft->char_conv || tft->wchar_conv || tft->bchar_conv) {
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

static void ffi_varray_release(struct txr_ffi_type *tft, val vec, mem_t *dst)
{
  cnum nelem = c_num(length(vec)) + tft->null_term;
  ffi_array_release_common(tft, vec, dst, nelem);
}

static val ffi_carray_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  mem_t *p = *coerce(mem_t **, src);
  return make_carray(tft->eltype, p, -1, nil);
}

static void ffi_carray_put(struct txr_ffi_type *tft, val carray, mem_t *dst,
                           val self)
{
  mem_t *p = carray_ptr(carray, tft->eltype, self);
  *coerce(mem_t **, dst) = p;
}

static void ffi_enum_put(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  if (symbolp(n)) {
    n = gethash(tft->num_sym, n);
    if (!n)
      uw_throwf(error_s, lit("~s: ~s has no member ~s"), self,
                tft->syntax, n, nao);
  }
  ffi_int_put(tft, n, dst, self);
}

static val ffi_enum_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  val n = ffi_int_get(tft, src, self);
  val sym = gethash(tft->sym_num, n);
  return if3(sym, sym, n);
}

#if !HAVE_LITTLE_ENDIAN

static void ffi_enum_rput(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  if (symbolp(n)) {
    n = gethash(tft->num_sym, n);
    if (!n)
      uw_throwf(error_s, lit("~s: ~s has no member ~s"), self,
                tft->syntax, n, nao);
  }
  ffi_int_rput(tft, n, dst, self);
}

static val ffi_enum_rget(struct txr_ffi_type *tft, mem_t *src, val self)
{
  val n = ffi_int_rget(tft, src, self);
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
      mem_t *ptr = union_get_ptr(uni);
      memcpy(ptr, src, tft->size);
    }
  }

  return uni;
}

static void ffi_union_put(struct txr_ffi_type *tft, val uni,
                          mem_t *dst, val self)
{
  mem_t *ptr = union_get_ptr(uni);
  memcpy(dst, ptr, tft->size);
}

static val ffi_union_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  return make_union_tft(src, tft);
}

static val bitfield_syntax_p(val syntax)
{
  if (!consp(syntax)) {
    return nil;
  } else {
    val sym = car(syntax);
    return tnil(sym == sbit_s || sym == ubit_s || sym == bit_s);
  }
}

static struct txr_ffi_type *ffi_simple_clone(struct txr_ffi_type *orig)
{
  return coerce(struct txr_ffi_type *, chk_copy_obj(coerce(mem_t *, orig),
                                                    sizeof *orig));
}

static val make_ffi_type_builtin(val syntax, val lisp_type,
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

  val obj = cobj(coerce(mem_t *, tft), ffi_type_s, &ffi_type_builtin_ops);

  tft->self = obj;
  tft->ft = ft;
  tft->syntax = syntax;
  tft->lt = lisp_type;
  tft->size = size;
  tft->align = align;
  tft->clone = ffi_simple_clone;
  tft->put = put;
  tft->get = get;
  tft->alloc = ffi_fixed_alloc;
  tft->free = free;
#if !HAVE_LITTLE_ENDIAN
  tft->rput = (rput ? rput : put);
  tft->rget = (rget ? rget : get);
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
                                                 val obj, mem_t *dst),
                                 val tgtype)
{
  val self = lit("ffi-type-compile");
  struct txr_ffi_type *tgtft = ffi_type_struct(tgtype);

  if (bitfield_syntax_p(tgtft->syntax)) {
    uw_throwf(error_s, lit("~a: type combination ~s not allowed"),
              self, syntax, nao);
  } else {
    struct txr_ffi_type *tft = coerce(struct txr_ffi_type *,
                                      chk_calloc(1, sizeof *tft));

    val obj = cobj(coerce(mem_t *, tft), ffi_type_s, &ffi_type_ptr_ops);

    tft->self = obj;
    tft->ft = &ffi_type_pointer;
    tft->syntax = syntax;
    tft->lt = lisp_type;
    tft->size = sizeof (mem_t *);
    tft->align = alignof (mem_t *);
    tft->clone = ffi_simple_clone;
    tft->put = put;
    tft->get = get;
    tft->eltype = tgtype;
    tft->in = in;
    tft->out = out;
    tft->release = release;
    tft->alloc = ffi_fixed_alloc;
    tft->free = free;
    tft->by_value_in = 1;

    return obj;
  }
}

static struct txr_ffi_type *ffi_struct_clone(struct txr_ffi_type *orig)
{
  cnum nmemb = orig->nelem;
  struct txr_ffi_type *copy = ffi_simple_clone(orig);
  size_t memb_size = sizeof *orig->memb * nmemb;
  ffi_type *ft = coerce(ffi_type *, chk_copy_obj(coerce(mem_t *, orig->ft),
                                                 sizeof *orig->ft));

  copy->ft = ft;
#if HAVE_LIBFFI
  ft->elements = copy->elements;
#endif
  copy->memb = coerce(struct smemb *, chk_copy_obj(coerce(mem_t *,
                                                          orig->memb),
                                                   memb_size));

  return copy;
}

static val make_ffi_type_struct(val syntax, val lisp_type,
                                val slots, val types)
{
  struct txr_ffi_type *tft = coerce(struct txr_ffi_type *,
                                    chk_calloc(1, sizeof *tft));
  ffi_type *ft = coerce(ffi_type *, chk_calloc(1, sizeof *ft));

  cnum nmemb = c_num(length(types)), i;
  struct smemb *memb = coerce(struct smemb *,
                              chk_calloc(nmemb, sizeof *memb));
  val obj = cobj(coerce(mem_t *, tft), ffi_type_s, &ffi_type_struct_ops);
  ucnum offs = 0;
  ucnum most_align = 0;
  int need_out_handler = 0;
  int bit_offs = 0;
  const unsigned bits_int = 8 * sizeof(int);

  tft->self = obj;
  tft->ft = ft;
  tft->syntax = syntax;
  tft->lt = lisp_type;
  tft->nelem = nmemb;
  tft->clone = ffi_struct_clone;
  tft->put = ffi_struct_put;
  tft->get = ffi_struct_get;
  tft->in = ffi_struct_in;
  tft->release = ffi_struct_release;
  tft->alloc = ffi_fixed_alloc;
  tft->free = free;
  tft->memb = memb;

  for (i = 0; i < nmemb; i++) {
    val type = pop(&types);
    val slot = pop(&slots);
    struct txr_ffi_type *mtft = ffi_type_struct(type);
    cnum size = mtft->size;

    memb[i].mtype = type;
    memb[i].mname = slot;
    memb[i].mtft = mtft;

    if (bitfield_syntax_p(mtft->syntax)) {
      ucnum size = mtft->size;
      ucnum bits_type = 8 * size;
      ucnum bits = mtft->nelem;
      ucnum offs_mask = size - 1;
      ucnum align_mask = ~offs_mask;
      ucnum unit_offs = offs & align_mask;
      ucnum bits_alloc = 8 * (offs - unit_offs) + bit_offs;
      ucnum room = bits_type - bits_alloc;

      if (bits == 0) {
        if (offs != unit_offs)
          offs = unit_offs + size;
        bit_offs = 0;
        nmemb--, i--;
        continue;
      }

      if (bits > room) {
        offs = unit_offs + size;
        bit_offs = bits_alloc = 0;
      }

      if (bits_alloc == 0) {
        if (most_align < (ucnum) mtft->align)
          most_align = mtft->align;
      }

      memb[i].offs = offs;

#if HAVE_LITTLE_ENDIAN
      mtft->shift = bits_alloc;
#else
      mtft->shift = bits_int - bits_alloc - bits;
#endif
      if (bits == bits_int)
        mtft->mask = UINT_MAX;
      else
        mtft->mask = ((1U << bits) - 1) << mtft->shift;
      bit_offs += bits;
      offs += bit_offs / 8;
      bit_offs %= 8;
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

  if (bit_offs > 0) {
    bug_unless (bit_offs < 8);
    offs++;
  }

  tft->nelem = i;

  if (need_out_handler)
    tft->out = ffi_struct_out;

  tft->size = (offs + most_align - 1) & ~(most_align - 1);
  tft->align = most_align;

#if HAVE_LIBFFI
  ft->type = FFI_TYPE_STRUCT;
  ft->size = tft->size;
  ft->alignment = tft->align;
  ft->elements = tft->elements;
#endif

  return obj;
}

static val make_ffi_type_union(val syntax, val lisp_type,
                               val slots, val types)
{
  struct txr_ffi_type *tft = coerce(struct txr_ffi_type *,
                                    chk_calloc(1, sizeof *tft));
  ffi_type *ft = coerce(ffi_type *, chk_calloc(1, sizeof *ft));

  cnum nmemb = c_num(length(types)), i;
  struct smemb *memb = coerce(struct smemb *,
                              chk_calloc(nmemb, sizeof *memb));
  val obj = cobj(coerce(mem_t *, tft), ffi_type_s, &ffi_type_enum_ops);
  ucnum most_align = 0;
  ucnum biggest_size = 0;
  const unsigned bits_int = 8 * sizeof(int);

  tft->self = obj;
  tft->ft = ft;
  tft->syntax = syntax;
  tft->lt = lisp_type;
  tft->nelem = nmemb;
  tft->clone = ffi_struct_clone;
  tft->put = ffi_union_put;
  tft->get = ffi_union_get;
  tft->in = ffi_union_in;
  tft->alloc = ffi_fixed_alloc;
  tft->free = free;
  tft->memb = memb;

  for (i = 0; i < nmemb; i++) {
    val type = pop(&types);
    val slot = pop(&slots);
    struct txr_ffi_type *mtft = ffi_type_struct(type);

    memb[i].mtype = type;
    memb[i].mname = slot;
    memb[i].mtft = mtft;
    memb[i].offs = 0;

    if (most_align < (ucnum) mtft->align)
      most_align = mtft->align;

    if (biggest_size < (ucnum) mtft->size)
      biggest_size = mtft->size;

    if (bitfield_syntax_p(mtft->syntax)) {
      ucnum bits = mtft->nelem;

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
    }
  }

  tft->nelem = i;

  tft->size = biggest_size;
  tft->align = most_align;

#if HAVE_LIBFFI
  ft->type = FFI_TYPE_STRUCT;
  ft->size = tft->size;
  ft->alignment = tft->align;
  ft->elements = tft->elements;
#endif

  return obj;
}


static struct txr_ffi_type *ffi_array_clone(struct txr_ffi_type *orig)
{
  struct txr_ffi_type *copy = ffi_simple_clone(orig);
  ffi_type *ft = coerce(ffi_type *, chk_copy_obj(coerce(mem_t *, orig->ft),
                                                 sizeof *orig->ft));

  copy->ft = ft;
#if HAVE_LIBFFI
  ft->elements = copy->elements;
#endif
  return copy;
}

static val make_ffi_type_array(val syntax, val lisp_type,
                               val dim, val eltype, val self)
{
  struct txr_ffi_type *tft = coerce(struct txr_ffi_type *,
                                    chk_calloc(1, sizeof *tft));
  ffi_type *ft = coerce(ffi_type *, chk_calloc(1, sizeof *ft));
  cnum nelem = c_num(dim), i;
  val obj = cobj(coerce(mem_t *, tft), ffi_type_s, &ffi_type_struct_ops);

  struct txr_ffi_type *etft = ffi_type_struct(eltype);

  tft->self = obj;
  tft->ft = ft;
  tft->syntax = syntax;
  tft->lt = lisp_type;
  tft->eltype = eltype;
  tft->clone = ffi_array_clone;
  tft->put = ffi_array_put;
  tft->get = ffi_array_get;
  tft->in = ffi_array_in;
  tft->release = ffi_array_release;
  tft->alloc = ffi_fixed_alloc;
  tft->free = free;
  tft->by_value_in = etft->by_value_in;

  for (i = 0; i < nelem; i++) {
    if (i == 0) {
      tft->size = etft->size * nelem;
      tft->align = etft->align;
      if (etft->out != 0)
        tft->out = ffi_array_out;
    }
  }

  tft->nelem = nelem;

#if HAVE_LIBFFI
  ft->type = FFI_TYPE_STRUCT;
  ft->size = etft->size * nelem;
  ft->alignment = etft->align;
  ft->elements = tft->elements;
#endif

  return obj;
}

static val ffi_eval_expr(val expr, val menv, val env)
{
  val expr_ex = expand(expr, menv);
  return eval(expr_ex, env, expr_ex);
}

static val make_ffi_type_enum(val syntax, val enums, val self)
{
  struct txr_ffi_type *tft = coerce(struct txr_ffi_type *,
                                    chk_calloc(1, sizeof *tft));

  val obj = cobj(coerce(mem_t *, tft), ffi_type_s, &ffi_type_enum_ops);
  cnum lowest = INT_PTR_MAX;
  cnum highest = INT_PTR_MIN;
  cnum cur = -1;
  ucnum count = 0;
  val iter;
  val sym_num = make_hash(nil, nil, t);
  val num_sym = make_hash(nil, nil, nil);
  val enum_env = make_env(nil, nil, nil);
  val shadow_menv = make_env(nil, nil, nil);

  tft->self = obj;
  tft->ft = &ffi_type_sint;
  tft->syntax = syntax;
  tft->lt = sym_s;
  tft->size = sizeof (int);
  tft->align = alignof (int);
  tft->clone = ffi_simple_clone;
  tft->put = ffi_enum_put;
  tft->get = ffi_enum_get;
  tft->alloc = ffi_fixed_alloc;
  tft->free = free;
#if !HAVE_LITTLE_ENDIAN
  tft->rput = ffi_enum_rput;
  tft->rget = ffi_enum_rget;
#endif

  for (iter = enums; !endp(iter); iter = cdr(iter), count++) {
    val en = car(iter);
    val nn;
    if (symbolp(en)) {
      val sym = en;
      if (!bindable(sym))
        uw_throwf(error_s, lit("~s: ~s member ~s isn't a bindable symbol"),
                  self, syntax, sym, nao);
      if (cur == INT_MAX)
        uw_throwf(error_s, lit("~s: ~s overflow at member ~s"),
                  self, syntax, sym, nao);
      if (gethash(num_sym, sym))
        uw_throwf(error_s, lit("~s: ~s duplicate member ~s"),
                  self, syntax, sym, nao);
      sethash(num_sym, sym, nn = num(++cur));
      sethash(sym_num, nn, sym);
      env_vbind(enum_env, sym, nn);
      env_vbind(shadow_menv, sym, special_s);
      if (cur > highest)
        highest = cur;
    } else {
      val expr = cadr(en);
      val sym = car(en);
      val n;
      if (!bindable(sym))
        uw_throwf(error_s, lit("~s: ~s member ~s isn't a bindable symbol"),
                  self, syntax, sym, nao);
      if (gethash(num_sym, sym))
        uw_throwf(error_s, lit("~s: ~s duplicate member ~s"),
                  self, syntax, sym, nao);

      n = ffi_eval_expr(expr, shadow_menv, enum_env);

      if (!integerp(n)) {
        uw_throwf(error_s, lit("~s: ~s member ~s value ~s not integer"),
                  self, syntax, n, nao);
      }

      cur = c_num(n);
      if (cur > INT_MAX)
        uw_throwf(error_s, lit("~s: ~s member ~s value ~s too large"),
                  self, syntax, n, nao);
      sethash(num_sym, sym, nn = num(cur));
      sethash(sym_num, nn, sym);
      env_vbind(enum_env, sym, nn);
      env_vbind(shadow_menv, sym, special_s);
      if (cur < lowest)
        lowest = cur;
    }
  }

  tft->num_sym = num_sym;
  tft->sym_num = sym_num;

  return obj;
}

static val ffi_type_copy(val orig)
{
  struct txr_ffi_type *otft = ffi_type_struct(orig);
  struct txr_ffi_type *ctft = otft->clone(otft);
  return cobj(coerce(mem_t *, ctft), orig->co.cls, orig->co.ops);
}

static val ffi_struct_compile(val membs, val *ptypes, val self)
{
  list_collect_decl (slots, pstail);
  list_collect_decl (types, pttail);

  for (; !endp(membs); membs = cdr(membs)) {
    val mp = car(membs);
    val name = car(mp);
    val type = cadr(mp);
    val comp_type = ffi_type_compile(type);
    struct txr_ffi_type *ctft = ffi_type_struct(comp_type);
    if (cddr(mp))
      uw_throwf(error_s, lit("~a: excess elements in type-member pair ~s"),
                self, mp, nao);
    if (ctft->size == 0)
      uw_throwf(error_s,
                lit("~a: incomplete type ~s cannot be struct/union member"),
                self, type, nao);
    pttail = list_collect(pttail, comp_type);
    pstail = list_collect(pstail, name);
  }

  *ptypes = types;
  return slots;
}

val ffi_type_compile(val syntax)
{
  val self = lit("ffi-type-compile");

  if (consp(syntax)) {
    val sym = car(syntax);

    if (sym == struct_s) {
      uses_or2;
      val name = cadr(syntax);
      val membs = cddr(syntax);
      val types;
      val sname = if3(name, name, gensym(lit("ffi-struct-")));
      val slots = ffi_struct_compile(membs, &types, self);
      val stype = or2(if2(name, find_struct_type(sname)),
                      make_struct_type(sname, nil, nil,
                                       remq(nil, slots, nil),
                                       nil, nil, nil, nil));
      val xsyntax = cons(struct_s,
                         cons(sname, membs));
      return make_ffi_type_struct(xsyntax, stype, slots, types);
    } else if (sym == union_s) {
      val name = cadr(syntax);
      val membs = cddr(syntax);
      val sname = if3(name, name, gensym(lit("ffi-union-")));
      val types;
      val slots = ffi_struct_compile(membs, &types, self);
      val xsyntax = cons(struct_s,
                         cons(sname, membs));
      return make_ffi_type_union(xsyntax, union_s, slots, types);
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
        if (etft->size == 0)
          uw_throwf(error_s,
                    lit("~a: incomplete type ~s cannot be array element"),
                    self, eltype_syntax, nao);
        if (sym == zarray_s) {
          tft->null_term = 1;
          tft->get = ffi_varray_null_term_get;
          tft->in = ffi_varray_null_term_in;
        }
        if (etft->syntax == char_s)
          tft->char_conv = 1;
        else if (etft->syntax == wchar_s)
          tft->wchar_conv = 1;
        else if (etft->syntax == bchar_s)
          tft->bchar_conv = 1;
        tft->alloc = ffi_varray_alloc;
        tft->free = free;
        tft->size = 0;
        return type;
      } else if (length(syntax) == three) {
        val dim = ffi_eval_expr(cadr(syntax), nil, nil);
        val eltype_syntax = caddr(syntax);
        val eltype = ffi_type_compile(eltype_syntax);
        val xsyntax = list(sym, dim, eltype_syntax, nao);
        struct txr_ffi_type *etft = ffi_type_struct(eltype);

        if (etft->size == 0)
          uw_throwf(error_s,
                    lit("~a: incomplete type ~s cannot be array element"),
                    self, eltype_syntax, nao);

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
            tft->char_conv = 1;
          else if (etft->syntax == wchar_s)
            tft->wchar_conv = 1;
          else if (etft->syntax == bchar_s)
            tft->bchar_conv = 1;
          return type;
        }
      } else {
        uw_throwf(error_s, lit("~a: excess elements in ~s"),
                  self, syntax, nao);
      }
    } else if (sym == ptr_in_s) {
      val target_type = ffi_type_compile(cadr(syntax));
      return make_ffi_type_pointer(syntax, ffi_get_lisp_type(target_type),
                                   ffi_ptr_in_put, ffi_ptr_get,
                                   ffi_ptr_in_in, ffi_ptr_in_out,
                                   ffi_ptr_in_release, target_type);
    } else if (sym == ptr_in_d_s) {
      val target_type = ffi_type_compile(cadr(syntax));
      return make_ffi_type_pointer(syntax, ffi_get_lisp_type(target_type),
                                   ffi_ptr_in_put, ffi_ptr_d_get,
                                   ffi_ptr_in_d_in, ffi_ptr_in_out,
                                   ffi_ptr_in_release, target_type);
    } else if (sym == ptr_out_s) {
      val target_type = ffi_type_compile(cadr(syntax));
      return make_ffi_type_pointer(syntax, ffi_get_lisp_type(target_type),
                                   ffi_ptr_out_put, ffi_ptr_get,
                                   ffi_ptr_out_in, ffi_ptr_out_out,
                                   ffi_simple_release, target_type);
    } else if (sym == ptr_out_d_s) {
      val target_type = ffi_type_compile(cadr(syntax));
      return make_ffi_type_pointer(syntax, ffi_get_lisp_type(target_type),
                                   ffi_ptr_out_null_put, ffi_ptr_d_get,
                                   ffi_ptr_out_in, ffi_ptr_out_out,
                                   0, target_type);
    } else if (sym == ptr_s) {
      val target_type = ffi_type_compile(cadr(syntax));
      return make_ffi_type_pointer(syntax, ffi_get_lisp_type(target_type),
                                   ffi_ptr_in_put, ffi_ptr_get,
                                   ffi_ptr_out_in, ffi_ptr_out_out,
                                   ffi_ptr_in_release, target_type);
    } else if (sym == ptr_out_s_s) {
      val target_type = ffi_type_compile(cadr(syntax));
      return make_ffi_type_pointer(syntax, ffi_get_lisp_type(target_type),
                                   ffi_ptr_out_null_put, ffi_ptr_get,
                                   ffi_ptr_out_s_in, ffi_ptr_out_out,
                                   0, target_type);
    } else if (sym == buf_s || sym == buf_d_s) {
      val size = ffi_eval_expr(cadr(syntax), nil, nil);
      val xsyntax = list(sym, size, nao);
      cnum nelem = c_num(size);
      val type = make_ffi_type_builtin(xsyntax, buf_s,
                                       sizeof (mem_t *),
                                       alignof (mem_t *),
                                       &ffi_type_pointer,
                                       if3(sym == buf_s,
                                           ffi_buf_put, ffi_buf_d_put),
                                       if3(sym == buf_s,
                                           ffi_buf_get, ffi_buf_d_get),
                                       0, 0);
      struct txr_ffi_type *tft = ffi_type_struct(type);

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
      val type = make_ffi_type_builtin(cptr_s, cptr_s, sizeof (mem_t *),
                                       alignof (mem_t *),
                                       &ffi_type_pointer,
                                       ffi_cptr_put, ffi_cptr_get, 0, 0);
      struct txr_ffi_type *tft = ffi_type_struct(type);
      tft->alloc = ffi_cptr_alloc;
      tft->free = ffi_noop_free;
      tft->tag = tag;
      return type;
    } else if (sym == carray_s) {
      val eltype = ffi_type_compile(cadr(syntax));
      return make_ffi_type_pointer(syntax, carray_s,
                                   ffi_carray_put, ffi_carray_get,
                                   0, 0, 0, eltype);
    } else if (sym == sbit_s || sym == ubit_s) {
      val nbits = ffi_eval_expr(cadr(syntax), nil, nil);
      cnum nb = c_num(nbits);
      val xsyntax = list(sym, nbits, nao);
      val type = make_ffi_type_builtin(xsyntax, integer_s,
                                       sizeof (int), alignof (int),
                                       &ffi_type_void,
                                       if3(sym == sbit_s,
                                           ffi_sbit_put, ffi_ubit_put),
                                       if3(sym == sbit_s,
                                           ffi_sbit_get, ffi_ubit_get),
                                       0, 0);
      struct txr_ffi_type *tft = ffi_type_struct(type);
      const int bits_int = 8 * sizeof(int);
      if (nb < 0 || nb > bits_int)
        uw_throwf(error_s, lit("~a: invalid bitfield size ~s; "
                               "must be 0 to ~s"),
                  self, nbits, num_fast(bits_int), nao);
      tft->nelem = c_num(nbits);
      return type;
    } else if (sym == bit_s) {
      val nbits = ffi_eval_expr(cadr(syntax), nil, nil);
      cnum nb = c_num(nbits);
      val type_syntax = caddr(syntax);
      val xsyntax = list(sym, nbits, type_syntax, nao);
      val type = ffi_type_compile(type_syntax);
      struct txr_ffi_type *tft = ffi_type_struct(type);
      const cnum max_bits = 8 * tft->size;
      val type_copy = ffi_type_copy(type);
      struct txr_ffi_type *tft_cp = ffi_type_struct(type_copy);
      val syn = tft->syntax;
      int unsgnd = 0;

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

      if (nb < 0 || nb > max_bits)
        uw_throwf(error_s, lit("~a: invalid bitfield size ~s; "
                               "must be 0 to ~s"),
                  self, nbits, num_fast(max_bits), nao);
      tft_cp->syntax = xsyntax;
      tft_cp->nelem = nb;
      tft_cp->put = if3(unsgnd, ffi_generic_ubit_put, ffi_generic_sbit_put);
      tft_cp->get = if3(unsgnd, ffi_generic_ubit_get, ffi_generic_sbit_get);
      return type_copy;
    } else if (sym == enum_s) {
      val name = cadr(syntax);
      val enums = cddr(syntax);
      val xsyntax = cons(enum_s, cons(name, nil));
      if (name && !bindable(name))
        uw_throwf(error_s,
                  lit("~a: enum name ~s must be bindable symbol or nil"),
                  self, name, nao);
      return make_ffi_type_enum(xsyntax, enums, self);
    } else if (sym == align_s) {
      val align = ffi_eval_expr(cadr(syntax), nil, nil);
      ucnum al = c_num(align);
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
      if (tft->eltype || tft->memb != 0)
        uw_throwf(error_s, lit("~a: type ~s can't be basis for bool"),
                  self, tft->syntax, nao);
      tft->syntax = type_syntax;
      tft->eltype = type;
      tft->get = ffi_bool_get;
      tft->put = ffi_bool_put;
      return type_copy;
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
}

static void ffi_init_types(void)
{
#if UCHAR_MAX == CHAR_MAX
  ffi_type *ffi_char = &ffi_type_uchar;
#else
  ffi_type *ffi_char = &ffi_type_schar;
#endif

#if HAVE_I8
  ffi_typedef(uint8_s, make_ffi_type_builtin(uint8_s, integer_s, sizeof (i8_t),
                                             alignof (i8_t), &ffi_type_uint8,
                                             ffi_u8_put, ffi_u8_get,
                                             ifbe(ffi_u8_rput),
                                             ifbe(ffi_u8_rget)));
  ffi_typedef(int8_s, make_ffi_type_builtin(int8_s, integer_s, sizeof (i8_t),
                                            alignof (i8_t), &ffi_type_sint8,
                                            ffi_i8_put, ffi_i8_get,
                                            ifbe(ffi_i8_rput),
                                            ifbe(ffi_i8_rget)));
#endif
#if HAVE_I16
  ffi_typedef(uint16_s, make_ffi_type_builtin(uint16_s, integer_s,
                                              sizeof (i16_t), alignof (i16_t),
                                              &ffi_type_uint16,
                                              ffi_u16_put, ffi_u16_get,
                                              ifbe(ffi_u16_rput),
                                              ifbe(ffi_u16_rget)));
  ffi_typedef(int16_s, make_ffi_type_builtin(int16_s, integer_s,
                                             sizeof (i16_t), alignof (i16_t),
                                             &ffi_type_sint16,
                                             ffi_i16_put, ffi_i16_get,
                                             ifbe(ffi_i16_rput),
                                             ifbe(ffi_i16_rget)));
#endif
#if HAVE_I32
  ffi_typedef(uint32_s, make_ffi_type_builtin(uint32_s, integer_s,
                                              sizeof (i32_t), alignof (i32_t),
                                              &ffi_type_uint32,
                                              ffi_u32_put, ffi_u32_get,
                                              ifbe(ffi_u32_rput),
                                              ifbe(ffi_u32_rget)));
  ffi_typedef(int32_s, make_ffi_type_builtin(int32_s, integer_s,
                                             sizeof (i32_t), alignof (i32_t),
                                             &ffi_type_sint32,
                                             ffi_i32_put, ffi_i32_get,
                                             ifbe(ffi_i32_rput),
                                             ifbe(ffi_i32_rget)));
#endif
#if HAVE_I64
  ffi_typedef(uint64_s, make_ffi_type_builtin(uint64_s, integer_s,
                                              sizeof (i64_t), alignof (i64_t),
                                              &ffi_type_uint64,
                                              ffi_u64_put, ffi_u64_get, 0, 0));
  ffi_typedef(int64_s, make_ffi_type_builtin(int64_s, integer_s,
                                             sizeof (i64_t), alignof (i64_t),
                                             &ffi_type_sint64,
                                             ffi_i64_put, ffi_i64_get, 0, 0));
#endif
  ffi_typedef(uchar_s, make_ffi_type_builtin(uchar_s, integer_s, 1, 1,
                                             &ffi_type_uchar,
                                             ffi_uchar_put, ffi_uchar_get,
                                             ifbe(ffi_uchar_rput),
                                             ifbe(ffi_uchar_rget)));
  ffi_typedef(char_s, make_ffi_type_builtin(char_s, integer_s, 1, 1,
                                            ffi_char, ffi_char_put,
                                            ffi_char_get,
                                            ifbe(ffi_char_rput),
                                            ifbe(ffi_char_rget)));
  ffi_typedef(bchar_s, make_ffi_type_builtin(bchar_s, char_s, 1, 1,
                                             &ffi_type_uchar,
                                             ffi_uchar_put, ffi_bchar_get,
                                             ifbe(ffi_uchar_rput),
                                             ifbe(ffi_bchar_rget)));
  ffi_typedef(wchar_s, make_ffi_type_builtin(wchar_s, char_s,
                                             sizeof (wchar_t),
                                             alignof (wchar_t),
                                             &ffi_type_wchar,
                                             ffi_wchar_put, ffi_wchar_get,
                                             ifbe(ffi_wchar_rput),
                                             ifbe(ffi_wchar_rget)));
  ffi_typedef(ushort_s, make_ffi_type_builtin(ushort_s, integer_s,
                                              sizeof (short), alignof (short),
                                              &ffi_type_ushort,
                                              ffi_ushort_put, ffi_ushort_get,
                                              ifbe(ffi_ushort_rput),
                                              ifbe(ffi_ushort_rget)));
  ffi_typedef(short_s, make_ffi_type_builtin(short_s, integer_s,
                                             sizeof (short), alignof (short),
                                             &ffi_type_sshort,
                                             ffi_short_put, ffi_short_get,
                                             ifbe(ffi_short_rput),
                                             ifbe(ffi_short_rget)));
  ffi_typedef(int_s, make_ffi_type_builtin(int_s, integer_s,
                                           sizeof (int), alignof (int),
                                           &ffi_type_sint,
                                           ffi_int_put, ffi_int_get,
                                           ifbe(ffi_int_rput),
                                           ifbe(ffi_int_rget)));
  ffi_typedef(uint_s, make_ffi_type_builtin(uint_s, integer_s,
                                            sizeof (int), alignof (int),
                                            &ffi_type_uint,
                                            ffi_uint_put, ffi_uint_get,
                                            ifbe(ffi_uint_rput),
                                            ifbe(ffi_uint_rget)));
  ffi_typedef(ulong_s, make_ffi_type_builtin(ulong_s, integer_s,
                                             sizeof (long), alignof (long),
                                             &ffi_type_ulong,
                                             ffi_ulong_put, ffi_ulong_get,
                                             ifbe(ffi_ulong_rput),
                                             ifbe(ffi_ulong_rget)));
  ffi_typedef(long_s, make_ffi_type_builtin(long_s, integer_s,
                                            sizeof (long), alignof (long),
                                            &ffi_type_slong,
                                            ffi_long_put, ffi_long_get,
                                            ifbe(ffi_long_rput),
                                            ifbe(ffi_long_rget)));
  ffi_typedef(float_s, make_ffi_type_builtin(float_s, float_s,
                                             sizeof (float), alignof (float),
                                             &ffi_type_float,
                                             ffi_float_put, ffi_float_get,
                                             0, 0));
  ffi_typedef(double_s, make_ffi_type_builtin(double_s, float_s,
                                              sizeof (double),
                                              alignof (double),
                                              &ffi_type_double,
                                              ffi_double_put, ffi_double_get,
                                              0, 0));
  ffi_typedef(val_s, make_ffi_type_builtin(val_s, t,
                                           sizeof (val),
                                           alignof (val),
                                           &ffi_type_pointer,
                                           ffi_val_put, ffi_val_get,
                                           0, 0));
  ffi_typedef(be_uint16_s, make_ffi_type_builtin(be_uint16_s, integer_s,
                                                 sizeof (u16_t),
                                                 alignof (u16_t),
                                                 &ffi_type_uint16,
                                                 ffi_be_u16_put,
                                                 ffi_be_u16_get,
                                                 ifbe(ffi_be_u16_rput),
                                                 ifbe(ffi_be_u16_rget)));
  ffi_typedef(be_int16_s, make_ffi_type_builtin(be_int16_s, integer_s,
                                                sizeof (i16_t),
                                                alignof (i16_t),
                                                &ffi_type_sint16,
                                                ffi_be_i16_put,
                                                ffi_be_i16_get,
                                                ifbe(ffi_be_i16_rput),
                                                ifbe(ffi_be_i16_rget)));
  ffi_typedef(be_uint32_s, make_ffi_type_builtin(be_uint32_s, integer_s,
                                                 sizeof (u32_t),
                                                 alignof (u32_t),
                                                 &ffi_type_uint32,
                                                 ffi_be_u32_put,
                                                 ffi_be_u32_get,
                                                 ifbe(ffi_be_u32_rput),
                                                 ifbe(ffi_be_u32_rget)));
  ffi_typedef(be_int32_s, make_ffi_type_builtin(be_int32_s, integer_s,
                                                sizeof (i32_t),
                                                alignof (i32_t),
                                                &ffi_type_sint32,
                                                ffi_be_i32_put,
                                                ffi_be_i32_get,
                                                ifbe(ffi_be_i32_rput),
                                                ifbe(ffi_be_i32_rget)));
  ffi_typedef(be_uint64_s, make_ffi_type_builtin(be_uint64_s, integer_s,
                                                 sizeof (u64_t),
                                                 alignof (u64_t),
                                                 &ffi_type_uint64,
                                                 ffi_be_u64_put,
                                                 ffi_be_u64_get, 0, 0));
  ffi_typedef(be_int64_s, make_ffi_type_builtin(be_int64_s, integer_s,
                                                sizeof (i64_t),
                                                alignof (i64_t),
                                                &ffi_type_sint64,
                                                ffi_be_i64_put,
                                                ffi_be_i64_get, 0, 0));
  ffi_typedef(be_float_s, make_ffi_type_builtin(be_float_s, integer_s,
                                                sizeof (float),
                                                alignof (float),
                                                &ffi_type_float,
                                                ffi_be_float_put,
                                                ffi_be_float_get, 0, 0));
  ffi_typedef(be_double_s, make_ffi_type_builtin(be_double_s, integer_s,
                                                 sizeof (double),
                                                 alignof (double),
                                                 &ffi_type_double,
                                                 ffi_be_double_put,
                                                 ffi_be_double_get, 0, 0));
  ffi_typedef(le_uint16_s, make_ffi_type_builtin(le_uint16_s, integer_s,
                                                 sizeof (u16_t),
                                                 alignof (u16_t),
                                                 &ffi_type_uint16,
                                                 ffi_le_u16_put,
                                                 ffi_le_u16_get,
                                                 ifbe(ffi_le_u16_rput),
                                                 ifbe(ffi_le_u16_rget)));
  ffi_typedef(le_int16_s, make_ffi_type_builtin(le_int16_s, integer_s,
                                                sizeof (i16_t),
                                                alignof (i16_t),
                                                &ffi_type_sint16,
                                                ffi_le_i16_put,
                                                ffi_le_i16_get,
                                                ifbe(ffi_le_i16_rput),
                                                ifbe(ffi_le_i16_rget)));
  ffi_typedef(le_uint32_s, make_ffi_type_builtin(le_uint32_s, integer_s,
                                                 sizeof (u32_t),
                                                 alignof (u32_t),
                                                 &ffi_type_uint32,
                                                 ffi_le_u32_put,
                                                 ffi_le_u32_get,
                                                 ifbe(ffi_le_u32_rput),
                                                 ifbe(ffi_le_u32_rget)));
  ffi_typedef(le_int32_s, make_ffi_type_builtin(le_int32_s, integer_s,
                                                sizeof (i32_t),
                                                alignof (i32_t),
                                                &ffi_type_sint32,
                                                ffi_le_i32_put,
                                                ffi_le_i32_get,
                                                ifbe(ffi_le_i32_rput),
                                                ifbe(ffi_le_i32_rget)));
  ffi_typedef(le_uint64_s, make_ffi_type_builtin(le_uint64_s, integer_s,
                                                 sizeof (u64_t),
                                                 alignof (u64_t),
                                                 &ffi_type_uint64,
                                                 ffi_le_u64_put,
                                                 ffi_le_u64_get, 0, 0));
  ffi_typedef(le_int64_s, make_ffi_type_builtin(le_int64_s, integer_s,
                                                sizeof (i64_t),
                                                alignof (i64_t),
                                                &ffi_type_sint64,
                                                ffi_le_i64_put,
                                                ffi_le_i64_get, 0, 0));
  ffi_typedef(le_float_s, make_ffi_type_builtin(le_float_s, integer_s,
                                                sizeof (float),
                                                alignof (float),
                                                &ffi_type_float,
                                                ffi_le_float_put,
                                                ffi_le_float_get, 0, 0));
  ffi_typedef(le_double_s, make_ffi_type_builtin(le_double_s, integer_s,
                                                 sizeof (double),
                                                 alignof (double),
                                                 &ffi_type_double,
                                                 ffi_le_double_put,
                                                 ffi_le_double_get, 0, 0));
  {
    val type = make_ffi_type_builtin(cptr_s, cptr_s, sizeof (mem_t *),
                                     alignof (mem_t *),
                                     &ffi_type_pointer,
                                     ffi_cptr_put, ffi_cptr_get, 0, 0);
    struct txr_ffi_type *tft = ffi_type_struct(type);
    tft->alloc = ffi_cptr_alloc;
    tft->free = ffi_noop_free;
    tft->tag = nil;
    ffi_typedef(cptr_s, type);
  }

  {
    val type = make_ffi_type_builtin(str_s, str_s, sizeof (mem_t *),
                                     alignof (mem_t *),
                                     &ffi_type_pointer,
                                     ffi_str_put, ffi_str_get, 0, 0);
    struct txr_ffi_type *tft = ffi_type_struct(type);
    tft->in = ffi_str_in;
    tft->release = ffi_simple_release;
    tft->by_value_in = 1;
    ffi_typedef(str_s, type);
  }

  {
    val type = make_ffi_type_builtin(bstr_s, str_s, sizeof (mem_t *),
                                     alignof (mem_t *),
                                     &ffi_type_pointer,
                                     ffi_bstr_put, ffi_bstr_get, 0, 0);
    struct txr_ffi_type *tft = ffi_type_struct(type);
    tft->in = ffi_bstr_in;
    tft->release = ffi_simple_release;
    tft->by_value_in = 1;
    ffi_typedef(bstr_s, type);
  }

  ffi_typedef(str_d_s, make_ffi_type_builtin(str_d_s, str_s, sizeof (mem_t *),
                                             alignof (mem_t *),
                                             &ffi_type_pointer,
                                             ffi_str_put, ffi_str_d_get, 0, 0));
  {
    val type = ffi_typedef(wstr_s, make_ffi_type_builtin(wstr_s, str_s,
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

  ffi_typedef(wstr_d_s, make_ffi_type_builtin(wstr_d_s, str_s, sizeof (mem_t *),
                                              alignof (mem_t *),
                                              &ffi_type_pointer,
                                              ffi_wstr_put, ffi_wstr_d_get,
                                              0, 0));
  ffi_typedef(bstr_d_s, make_ffi_type_builtin(bstr_d_s, str_s, sizeof (mem_t *),
                                              alignof (mem_t *),
                                              &ffi_type_pointer,
                                              ffi_bstr_put, ffi_bstr_d_get,
                                              0, 0));

  {
    val iter;

    for (iter = list(buf_s, buf_d_s, nao); iter; iter = cdr(iter)) {
      val sym = car(iter);
      ffi_typedef(sym, make_ffi_type_builtin(sym, buf_s, sizeof (mem_t *),
                                             alignof (mem_t *),
                                             &ffi_type_pointer,
                                             if3(sym == buf_s,
                                                 ffi_buf_put, ffi_buf_d_put),
                                             ffi_void_get, 0, 0));
    }
  }

#if HAVE_LIBFFI
  ffi_typedef(closure_s, make_ffi_type_builtin(closure_s, fun_s,
                                               sizeof (mem_t *),
                                               alignof (mem_t *),
                                               &ffi_type_pointer,
                                               ffi_closure_put, ffi_cptr_get,
                                               0, 0));
#endif

  ffi_typedef(void_s, make_ffi_type_builtin(void_s, null_s, 0, 0,
                                            &ffi_type_void,
                                            ffi_void_put, ffi_void_get,
                                            0, 0));

  ffi_typedef(bool_s, ffi_type_compile(cons(bool_s, cons(uchar_s, nil))));
}

static val ffi_type_lookup(val sym)
{
  return gethash(ffi_typedef_hash, sym);
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
};

static struct txr_ffi_call_desc *ffi_call_desc(val obj)
{
  return coerce(struct txr_ffi_call_desc *, obj->co.handle);
}

static struct txr_ffi_call_desc *ffi_call_desc_checked(val obj)
{
  return coerce(struct txr_ffi_call_desc *, cobj_handle(obj, ffi_call_desc_s));
}

static void ffi_call_desc_print_op(val obj, val out,
                                   val pretty, struct strm_ctx *ctx)
{
  struct txr_ffi_call_desc *tfcd = ffi_call_desc(obj);
  put_string(lit("#<"), out);
  obj_print_impl(obj->co.cls, out, pretty, ctx);
  format(out, lit(" ~s ~!~s>"), tfcd->rettype, tfcd->argtypes, nao);
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
}

static struct cobj_ops ffi_call_desc_ops =
  cobj_ops_init(eq,
                ffi_call_desc_print_op,
                ffi_call_desc_destroy_op,
                ffi_call_desc_mark_op,
                cobj_eq_hash_op);

val ffi_make_call_desc(val ntotal, val nfixed, val rettype, val argtypes)
{
  val self = lit("ffi-make-call-desc");
  cnum nf = c_num(default_arg(nfixed, zero));
  cnum nt = c_num(ntotal), i;
  struct txr_ffi_call_desc *tfcd = coerce(struct txr_ffi_call_desc *,
                                          chk_calloc(1, sizeof *tfcd));
  ffi_type **args = coerce(ffi_type **, chk_xalloc(nt, sizeof *args, self));
  val obj = cobj(coerce(mem_t *, tfcd), ffi_call_desc_s, &ffi_call_desc_ops);
  ffi_status ffis = FFI_OK;

  tfcd->variadic = (nfixed != nil);
  tfcd->nfixed = nf;
  tfcd->ntotal = nt;
  tfcd->argtypes = argtypes;
  tfcd->rettype = rettype;
  tfcd->args = args;

  for (i = 0; i < nt; i++) {
    val type = pop(&argtypes);
    struct txr_ffi_type *tft = ffi_type_struct_checked(type);
    if (tft->size == 0)
      uw_throwf(error_s, lit("~a: can't pass type ~s by value"),
                self, type, nao);
    args[i] = tft->ft;
  }

  {
    struct txr_ffi_type *tft = ffi_type_struct_checked(rettype);
    if (tft->size == 0 && tft->ft != &ffi_type_void)
      uw_throwf(error_s, lit("~a: can't return type ~s by value"),
                self, rettype, nao);
  }

  if (tfcd->variadic)
    ffis = ffi_prep_cif_var(&tfcd->cif, FFI_DEFAULT_ABI, nf, nt,
                            ffi_get_type(rettype), args);
  else
    ffis = ffi_prep_cif(&tfcd->cif, FFI_DEFAULT_ABI, nt,
                        ffi_get_type(rettype), args);

  if (ffis != FFI_OK)
    uw_throwf(error_s, lit("~a: ffi_prep_cif failed: ~s"),
              self, num(ffis), nao);

  return obj;
}

val ffi_call_wrap(val fptr, val ffi_call_desc, struct args *args)
{
  val self = lit("ffi-call");
  struct txr_ffi_call_desc *tfcd = ffi_call_desc_checked(ffi_call_desc);
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

  args_normalize(args, n);

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
          mtft->release(mtft, args->arg[i], convert(mem_t *, values[i]));
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
  val self = lit("ffi-closure-dispatch");
  val closure = coerce(val, clo);
  struct txr_ffi_closure *tfcl = ffi_closure_struct(closure);
  cnum i, nargs = tfcl->nparam;
  struct txr_ffi_call_desc *tfcd = tfcl->tfcd;
  val types = tfcd->argtypes;
  val rtype = tfcd->rettype;
  struct txr_ffi_type *rtft = ffi_type_struct(rtype);
  val retval = nil;
  int out_pass_needed = 0;
  args_decl(args, tfcl->nparam);
  args_decl(args_cp, tfcl->nparam);

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
  val self = lit("ffi-closure-dispatch-safe");
  val closure = coerce(val, clo);
  struct txr_ffi_closure *tfcl = ffi_closure_struct(closure);
  cnum i, nargs = tfcl->nparam;
  struct txr_ffi_call_desc *tfcd = tfcl->tfcd;
  val types = tfcd->argtypes;
  val rtype = tfcd->rettype;
  struct txr_ffi_type *rtft = ffi_type_struct(rtype);
  volatile val retval = nao;
  int out_pass_needed = 0;
  uw_frame_t cont_guard;

  if (rtft->release != 0)
    memset(cret, 0, rtft->size);

  uw_push_guard(&cont_guard, 0);

  uw_simple_catch_begin;

  {
    args_decl(args, tfcl->nparam);
    args_decl(args_cp, tfcl->nparam);

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
        rtft->release(rtft, retval, convert(mem_t *, cret));
      if (!tfcl->abort_retval)
        memset(cret, 0, pad_retval(rtft->size));
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
  val self = lit("ffi-make-closure");
  struct txr_ffi_closure *tfcl = coerce(struct txr_ffi_closure *,
                                        chk_calloc(1, sizeof *tfcl));
  struct txr_ffi_call_desc *tfcd = ffi_call_desc_checked(call_desc);
  val obj = cobj(coerce(mem_t *, tfcl), ffi_closure_s, &ffi_closure_ops);
  val safe_p = default_arg(safe_p_in, t);
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

mem_t *ffi_closure_get_fptr(val closure)
{
  struct txr_ffi_closure *tfcl = ffi_closure_struct_checked(closure);
  return tfcl->fptr;
}

#endif

val ffi_typedef(val name, val type)
{
  val self = lit("ffi-typedef");
  struct txr_ffi_type *tft = ffi_type_struct_checked(type);
  if (bitfield_syntax_p(tft->syntax))
    uw_throwf(error_s, lit("~a: cannot create a typedef for bitfield type"),
              self, nao);
  return sethash(ffi_typedef_hash, name, type);
}

val ffi_size(val type)
{
  val self = lit("ffi-size");
  struct txr_ffi_type *tft = ffi_type_struct_checked(type);
  if (bitfield_syntax_p(tft->syntax))
    uw_throwf(error_s, lit("~a: bitfield type ~s has no size"),
              self, type, nao);
  return num(tft->size);
}

val ffi_alignof(val type)
{
  val self = lit("ffi-alignof");
  struct txr_ffi_type *tft = ffi_type_struct_checked(type);
  if (bitfield_syntax_p(tft->syntax))
    uw_throwf(error_s, lit("~a: bitfield type ~s has no alignment"),
              self, type, nao);
  return num(tft->align);
}

val ffi_offsetof(val type, val memb)
{
  val self = lit("ffi-offsetof");
  struct txr_ffi_type *tft = ffi_type_struct_checked(type);
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
  struct txr_ffi_type *tft = ffi_type_struct_checked(type);
  if (!tft->eltype)
    uw_throwf(error_s, lit("~a: ~s isn't an array"), self, type, nao);
  return num(tft->nelem);
}

val ffi_elemsize(val type)
{
  val self = lit("ffi-elemsize");
  struct txr_ffi_type *tft = ffi_type_struct_checked(type);
  if (!tft->eltype) {
    uw_throwf(error_s, lit("~a: ~s isn't an array or pointer"),
              self, type, nao);
  } else {
    struct txr_ffi_type *etft = ffi_type_struct(tft->eltype);
    return num(etft->size);
  }
}

val ffi_elemtype(val type)
{
  val self = lit("ffi-elemtype");
  struct txr_ffi_type *tft = ffi_type_struct_checked(type);
  val eltype = tft->eltype;

  if (!eltype) {
    uw_throwf(error_s, lit("~a: ~s isn't an array or pointer"),
              self, type, nao);
  }

  return eltype;
}

val ffi_put_into(val dstbuf, val obj, val type)
{
  val self = lit("ffi-put-into");
  struct txr_ffi_type *tft = ffi_type_struct_checked(type);
  mem_t *dst = buf_get(dstbuf, self);
  if (lt(length_buf(dstbuf), num_fast(tft->size)))
    uw_throwf(error_s, lit("~a: buffer ~s is too small for type ~s"),
              self, dstbuf, type, nao);
  tft->put(tft, obj, dst, self);
  return dstbuf;
}

val ffi_put(val obj, val type)
{
  val self = lit("ffi-put");
  struct txr_ffi_type *tft = ffi_type_struct_checked(type);
  val buf = make_buf(num_fast(tft->size), zero, nil);
  mem_t *dst = buf_get(buf, self);
  tft->put(tft, obj, dst, self);
  return buf;
}

val ffi_in(val srcbuf, val obj, val type, val copy_p)
{
  val self = lit("ffi-in");
  struct txr_ffi_type *tft = ffi_type_struct_checked(type);
  mem_t *src = buf_get(srcbuf, self);
  if (lt(length_buf(srcbuf), num_fast(tft->size)))
    uw_throwf(error_s, lit("~a: buffer ~s is too small for type ~s"),
              self, srcbuf, type, nao);
  if (tft->in != 0)
    return tft->in(tft, copy_p != nil, src, obj, self);
  else if (copy_p)
    return tft->get(tft, src, self);
  return obj;
}

val ffi_get(val srcbuf, val type)
{
  val self = lit("ffi-get");
  struct txr_ffi_type *tft = ffi_type_struct_checked(type);
  mem_t *src = buf_get(srcbuf, self);
  if (lt(length_buf(srcbuf), num_fast(tft->size)))
    uw_throwf(error_s, lit("~a: buffer ~s is too small for type ~s"),
              self, srcbuf, type, nao);
  return tft->get(tft, src, self);
}

val ffi_out(val dstbuf, val obj, val type, val copy_p)
{
  val self = lit("ffi-out");
  struct txr_ffi_type *tft = ffi_type_struct_checked(type);
  mem_t *dst = buf_get(dstbuf, self);
  if (lt(length_buf(dstbuf), num_fast(tft->size)))
    uw_throwf(error_s, lit("~a: buffer ~s is too small for type ~s"),
              self, dstbuf, type, nao);
  if (tft->out != 0)
    tft->out(tft, copy_p != nil, obj, dst, self);
  else
    tft->put(tft, obj, dst, self);
  return dstbuf;
}

struct carray {
  val eltype;
  struct txr_ffi_type *eltft;
  mem_t *data;
  cnum nelem;
  val ref;
  val artype;
};

static struct carray *carray_struct(val carray)
{
  return coerce(struct carray*, carray->co.handle);
}

static struct carray *carray_struct_checked(val carray)
{
  return coerce(struct carray*, cobj_handle(carray, carray_s));
}

static void carray_print_op(val obj, val out, val pretty, struct strm_ctx *ctx)
{
  struct carray *scry = carray_struct(obj);
  put_string(lit("#<"), out);
  obj_print_impl(obj->co.cls, out, pretty, ctx);
  format(out, lit(" ~a"), if3(scry->nelem < 0,
                              lit("unknown-len"), num(scry->nelem)), nao);
  format(out, lit(" ~s>"), scry->eltype, nao);
}

static void carray_mark_op(val obj)
{
  struct carray *scry = carray_struct(obj);
  gc_mark(scry->eltype);
  gc_mark(scry->ref);
  gc_mark(scry->artype);
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

val make_carray(val type, mem_t *data, cnum nelem, val ref)
{
  struct carray *scry = coerce(struct carray *, chk_malloc(sizeof *scry));
  val obj;
  scry->eltype = nil;
  scry->eltft = ffi_type_struct_checked(type);
  scry->data = data;
  scry->nelem = nelem;
  scry->ref = nil;
  scry->artype = nil;
  obj = cobj(coerce(mem_t *, scry), carray_s, &carray_borrowed_ops);
  scry->eltype = type;
  scry->ref = ref;
  return obj;
}

val carrayp(val obj)
{
  return tnil(typeof(obj) == carray_s);
}

val carray_set_length(val carray, val nelem)
{
  struct carray *scry = carray_struct_checked(carray);
  val self = lit("carray-set-length");
  cnum nel = c_num(nelem);

  if (carray->co.ops == &carray_owned_ops)
    uw_throwf(error_s,
              lit("~a: can't set length of owned carray ~s"), self,
              carray, nao);

  if (nel < 0)
    uw_throwf(error_s,
              lit("~a: can't set length of ~s to negative value"), self,
              carray, nao);

  scry->nelem = nel;
  return nil;
}

val carray_dup(val carray)
{
  val self = lit("carray-dup");
  struct carray *scry = carray_struct_checked(carray);

  if (carray->co.ops == &carray_owned_ops) {
    return nil;
  } else if (scry->nelem < 0) {
    uw_throwf(error_s, lit("~a: size of ~s array unknown"), self, carray, nao);
  } else if (scry->data == 0) {
    uw_throwf(error_s, lit("~a: ~s: array data pointer is null"),
              self, carray, nao);
  } else {
    cnum elsize = scry->eltft->size;
    cnum size = (ucnum) scry->nelem * (ucnum) elsize;
    mem_t *dup = chk_copy_obj(scry->data, scry->nelem * scry->eltft->size);

    if (size < 0 || (elsize > 0 && size / elsize != scry->nelem))
      uw_throwf(error_s, lit("~a: array size overflow"), self, nao);

    carray->co.ops = &carray_owned_ops;
    scry->data = dup;
    scry->ref = nil;
    return t;
  }
}

val carray_own(val carray)
{
  val self = lit("carray-own");
  struct carray *scry = carray_struct_checked(carray);
  if (scry->ref)
    uw_throwf(error_s, lit("~a: cannot own buffer belonging to ~s"),
              self, scry->ref, nao);
  carray->co.ops = &carray_owned_ops;
  return nil;
}

val carray_free(val carray)
{
  val self = lit("carray-free");
  struct carray *scry = carray_struct_checked(carray);

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
  struct carray *scry = carray_struct_checked(carray);
  return scry->eltype;
}

val length_carray(val carray)
{
  struct carray *scry = carray_struct_checked(carray);
  return if3(scry->nelem < 0, nil, num(scry->nelem));
}

val copy_carray(val carray)
{
  struct carray *scry = carray_struct_checked(carray);
  val copy = make_carray(scry->eltype, scry->data, scry->nelem, nil);
  carray_dup(copy);
  return copy;
}

mem_t *carray_ptr(val carray, val type, val self)
{
  struct carray *scry = carray_struct_checked(carray);
  if (scry->eltype != type)
    uw_throwf(error_s, lit("~a: ~s is not of element type ~!~s"),
              self, carray, type, nao);
  return scry->data;
}

val carray_vec(val vec, val type, val null_term_p)
{
  val len = length(vec);
  val nt_p = default_null_arg(null_term_p);
  cnum i, l = c_num(if3(nt_p, succ(len), len));
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
  val nt_p = default_null_arg(null_term_p);
  val len = if3(nt_p, succ(length(list)), length(list));
  val carray = carray_blank(len, type);
  cnum i;

  (void) c_num(len);

  for (i = 0; !endp(list); list = cdr(list), i++) {
    val el = car(list);
    carray_refset(carray, num_fast(i), el);
  }

  return carray;
}

val carray_blank(val nelem, val type)
{
  val self = lit("carray-blank");
  cnum nel = c_num(nelem);
  struct txr_ffi_type *tft = ffi_type_struct(type);

  if (nel < 0) {
    uw_throwf(error_s, lit("~a: negative array size"), self, nao);
  } else {
    mem_t *data = chk_calloc(nel, tft->size);
    val carray = make_carray(type, data, nel, nil);
    carray->co.ops = &carray_owned_ops;
    return carray;
  }
}

val carray_buf(val buf, val type)
{
  val self = lit("carray-buf");
  mem_t *data = buf_get(buf, self);
  cnum blen = c_num(length_buf(buf));
  struct txr_ffi_type *tft = ffi_type_struct(type);
  cnum nelem = if3(tft->size, blen / tft->size, 0);
  if (tft->size == 0)
    uw_throwf(error_s,
              lit("~a: incomplete type ~s cannot be carray element"),
              self, tft->syntax, nao);
  return make_carray(type, data, nelem, buf);
}

val carray_buf_sync(val carray)
{
  val self = lit("carray-buf-sync");
  struct carray *scry = carray_struct_checked(carray);
  val buf = scry->ref;
  mem_t *data = buf_get(buf, self);
  cnum blen = c_num(length_buf(buf));
  struct txr_ffi_type *tft = ffi_type_struct(scry->eltype);
  scry->nelem = blen / tft->size;
  scry->data = data;
  return buf;
}

val carray_cptr(val cptr, val type, val len)
{
  mem_t *data = cptr_get(cptr);
  cnum nelem = c_num(default_arg(len, negone));
  (void) ffi_type_struct(type);
  return make_carray(type, data, nelem, 0);
}

val vec_carray(val carray, val null_term_p)
{
  val nt_p = default_null_arg(null_term_p);
  struct carray *scry = carray_struct_checked(carray);
  cnum i, l = if3(nt_p, scry->nelem - 1, scry->nelem);
  val vec = vector(num(l), nil);
  for (i = 0; i < l; i++) {
    val ni = num_fast(i);
    val el = carray_ref(carray, ni);
    set(vecref_l(vec, ni), el);
  }
  return vec;
}

val list_carray(val carray, val null_term_p)
{
  val nt_p = default_null_arg(null_term_p);
  struct carray *scry = carray_struct_checked(carray);
  cnum i, l = if3(nt_p, scry->nelem - 1, scry->nelem);
  list_collect_decl (list, ptail);
  for (i = 0; i < l; i++) {
    val ni = num_fast(i);
    val el = carray_ref(carray, ni);
    ptail = list_collect(ptail, el);
  }
  return list;
}

val carray_ref(val carray, val idx)
{
  val self = lit("carray-ref");
  struct carray *scry = carray_struct_checked(carray);
  cnum ix = c_num(idx);

  if (ix < 0 || (scry->nelem >= 0 && ix >= scry->nelem)) {
    uw_throwf(error_s, lit("~a: ~s: index ~s out of bounds"),
              self, carray, idx, nao);
  } else {
    struct txr_ffi_type *eltft = scry->eltft;
    if (scry->data == 0)
      uw_throwf(error_s, lit("~a: ~s: array was freed"),
                self, carray, nao);
    return eltft->get(eltft, scry->data + eltft->size * ix, self);
  }
}

val carray_refset(val carray, val idx, val newval)
{
  val self = lit("carray-refset");
  struct carray *scry = carray_struct_checked(carray);
  cnum ix = c_num(idx);

  if (ix < 0 || (scry->nelem >= 0 && ix >= scry->nelem)) {
    uw_throwf(error_s, lit("~a: ~s: index ~s out of bounds"),
              self, carray, idx, nao);
  } else {
    struct txr_ffi_type *eltft = scry->eltft;
    if (scry->data == 0)
      uw_throwf(error_s, lit("~a: ~s: array was freed"),
                self, carray, nao);
    eltft->put(eltft, newval, scry->data + eltft->size * ix, self);
    return newval;
  }
}

val carray_sub(val carray, val from, val to)
{
  struct carray *scry = carray_struct_checked(carray);
  cnum ln = scry->nelem;
  val len = num(ln);

  if (null_or_missing_p(from))
    from = zero;

  if (null_or_missing_p(to))
    to = len;

  if (minusp(to))
    to = plus(to, len);

  if (minusp(from))
    from = plus(from, len);

  {
    cnum fn = c_num(from);
    cnum tn = c_num(to);
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

    return make_carray(scry->eltype, scry->data + fn * elsize, tn - fn, carray);
  }
}

val carray_replace(val carray, val values, val from, val to)
{
  val self = lit("carray-replace");
  struct carray *scry = carray_struct_checked(carray);
  cnum ln = scry->nelem;
  val len = num(ln);
  val vlen = length(values);

  if (null_or_missing_p(from))
    from = zero;

  if (null_or_missing_p(to))
    to = len;

  if (minusp(to))
    to = plus(to, len);

  if (minusp(from))
    from = plus(from, len);

  {
    cnum fn = c_num(from);
    cnum tn = c_num(to);
    struct txr_ffi_type *eltft = scry->eltft;
    cnum elsize = eltft->size;
    cnum size = (ucnum) ln * (ucnum) elsize;
    cnum vn = c_num(vlen);
    cnum sn;
    mem_t *ptr;

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
      uw_throwf(error_s, lit("~a: array size overflow"), self, nao);

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
        memset(ptr + whole - delta, 0, delta);
      }
    }

    if (consp(values)) {
      val iter;

      for (iter = values; fn < sn; iter = cdr(iter), fn++, ptr += elsize)
      {
        val newval = car(iter);
        eltft->put(eltft, newval, ptr, self);
      }
    } else if (values) {
      cnum i;

      for (i = 0; fn < sn; i++, fn++, ptr += elsize) {
        val newval = ref(values, num_fast(i));
        eltft->put(eltft, newval, ptr, self);
      }
    }

    return carray;
  }
}

static void carray_ensure_artype(val carray, struct carray *scry, val self)
{
  if (!scry->artype) {
    val dim = num(scry->nelem);
    val syntax = list(carray_s, dim, scry->eltft->syntax, nao);
    struct txr_ffi_type *etft = scry->eltft;
    set(mkloc(scry->artype, carray), make_ffi_type_array(syntax, vec_s,
                                                         dim, scry->eltype,
                                                         self));

    {
      struct txr_ffi_type *atft = ffi_type_struct(scry->artype);
      if (etft->syntax == char_s)
        atft->char_conv = 1;
      else if (etft->syntax == wchar_s)
        atft->wchar_conv = 1;
      else if (etft->syntax == bchar_s)
        atft->bchar_conv = 1;
    }
  }
}

static val carray_get_common(val carray, val self, unsigned null_term)
{
  struct carray *scry = carray_struct_checked(carray);

  carray_ensure_artype(carray, scry, self);

  {
    struct txr_ffi_type *atft = ffi_type_struct(scry->artype);
    atft->null_term = null_term;
    return atft->get(atft, scry->data, self);
  }
}

static void carray_put_common(val carray, val seq, val self, unsigned null_term)
{
  struct carray *scry = carray_struct_checked(carray);

  carray_ensure_artype(carray, scry, self);

  {
    struct txr_ffi_type *atft = ffi_type_struct(scry->artype);
    atft->null_term = null_term;
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

val carray_pun(val carray, val type)
{
  val self = lit("carray-pun");
  struct carray *scry = carray_struct_checked(carray);
  struct txr_ffi_type *tft = ffi_type_struct(type);
  cnum len = scry->nelem;
  cnum elsize = scry->eltft->size;
  cnum size = (ucnum) len * (ucnum) elsize;

  if (tft->size == 0)
    uw_throwf(error_s,
              lit("~a: incomplete type ~s cannot be carray element"),
              self, tft->syntax, nao);

  if (len != 0 && size / elsize != len)
    uw_throwf(error_s, lit("~a: array size overflow"), self, nao);

  return make_carray(type, scry->data, size / tft->size, carray);
}

val carray_unum(val num, val eltype_in)
{
  val self = lit("carray-unum");
  val eltype = default_arg(eltype_in, ffi_type_compile(uchar_s));
  struct txr_ffi_type *tft = ffi_type_struct(eltype);

  if (tft->size == 0)
    uw_throwf(error_s,
              lit("~a: incomplete type ~s cannot be carray element"),
              self, tft->syntax, nao);

  switch (type(num)) {
  case NUM: case CHR:
    num = bignum(c_num(num));
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
      val ca = make_carray(eltype, data, nelem, nil);
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

val carray_num(val num, val eltype_in)
{
  val self = lit("carray-unum");
  val eltype = default_arg(eltype_in, ffi_type_compile(uchar_s));
  struct txr_ffi_type *tft = ffi_type_struct(eltype);

  if (tft->size == 0)
    uw_throwf(error_s,
              lit("~a: incomplete type ~s cannot be carray element"),
              self, tft->syntax, nao);

  switch (type(num)) {
  case NUM: case CHR:
    num = bignum(c_num(num));
    /* fallthrough */
  case BGNUM:
    {
      val wi = width(num);
      val bits = succ(wi);
      val bytes = ash(plus(bits, num_fast(7)), num_fast(-3));
      val bitsround = ash(bytes, num_fast(3));
      val un = logtrunc(num, bitsround);
      val ube = if3(bignump(un), un, bignum(c_num(un)));
      mp_int *m = mp(ube);
      ucnum size = mp_unsigned_bin_size(m);
      ucnum nelem = (c_unum(bytes) + tft->size - 1) / tft->size;
      mem_t *data = chk_xalloc(nelem, tft->size, self);
      ucnum delta = nelem * tft->size - size;
      val ca = make_carray(eltype, data, nelem, nil);
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

val unum_carray(val carray)
{
  val self = lit("unum-carray");
  struct carray *scry = carray_struct_checked(carray);
  struct txr_ffi_type *etft = scry->eltft;
  ucnum size = (ucnum) etft->size * (ucnum) scry->nelem;
  val ubn = make_bignum();
  mp_err mpe = mp_read_unsigned_bin(mp(ubn), scry->data, size);
  if (mpe != MP_OKAY)
    do_mp_error(self, mpe);
  return normalize(ubn);
}

val num_carray(val carray)
{
  val self = lit("num-carray");
  struct carray *scry = carray_struct_checked(carray);
  struct txr_ffi_type *etft = scry->eltft;
  ucnum size = (ucnum) etft->size * (ucnum) scry->nelem;
  ucnum bits = size * 8;
  val ubn = make_bignum();
  mp_err mpe = mp_read_unsigned_bin(mp(ubn), scry->data, size);
  if (mpe != MP_OKAY)
    do_mp_error(self, mpe);
  return sign_extend(normalize(ubn), unum(bits));
}

val put_carray(val carray, val offs, val stream)
{
  struct carray *scry = carray_struct_checked(carray);
  struct txr_ffi_type *etft = scry->eltft;
  ucnum size = (ucnum) etft->size * (ucnum) scry->nelem;
  val buf = make_borrowed_buf(unum(size), scry->data);
  val pos = default_arg(offs, zero);
  val ret = put_buf(buf, pos, stream);
  gc_hint(carray);
  return ret;
}

val fill_carray(val carray, val offs, val stream)
{
  struct carray *scry = carray_struct_checked(carray);
  struct txr_ffi_type *etft = scry->eltft;
  ucnum size = (ucnum) etft->size * (ucnum) scry->nelem;
  val buf = make_borrowed_buf(unum(size), scry->data);
  val pos = default_arg(offs, zero);
  val ret = fill_buf(buf, pos, stream);
  gc_hint(carray);
  return ret;
}

struct uni {
  struct txr_ffi_type *tft;
  mem_t *data;
};

static struct uni *uni_struct(val obj)
{
  return coerce(struct uni *, obj->co.handle);
}

static struct uni *uni_struct_checked(val obj)
{
  return coerce(struct uni *, cobj_handle(obj, union_s));
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
  val obj = cobj(coerce(mem_t *, us), union_s, &union_ops);
  us->tft = tft;
  us->data = data;
  return obj;
}

static val make_union_tft(mem_t *data_in, struct txr_ffi_type *tft)
{
  mem_t *data = chk_copy_obj(data_in, tft->size);
  return make_union_common(data, tft);
}

mem_t *union_get_ptr(val uni)
{
  struct uni *us = uni_struct_checked(uni);
  return us->data;
}

val make_union(val type)
{
  struct txr_ffi_type *tft = ffi_type_struct_checked(type);
  mem_t *data = chk_calloc(1, tft->size);
  return make_union_common(data, tft);
}

val union_members(val uni)
{
  struct uni *us = uni_struct_checked(uni);
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
  struct uni *us = uni_struct_checked(uni);
  struct txr_ffi_type *tft = us->tft;
  struct txr_ffi_type *mtft = ffi_find_memb(tft, memb);
  if (mtft == 0)
    ffi_memb_not_found(tft->self, memb, self);
  return mtft->get(mtft, us->data, self);
}

val union_put(val uni, val memb, val newval)
{
  val self = lit("union-put");
  struct uni *us = uni_struct_checked(uni);
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
  struct uni *us = uni_struct_checked(uni);
  struct txr_ffi_type *tft = us->tft;
  struct txr_ffi_type *mtft = ffi_find_memb(tft, memb);
  if (mtft == 0)
    ffi_memb_not_found(tft->self, memb, self);
  return mtft->in(mtft, 0, us->data, memb_obj, self);
}

val union_out(val uni, val memb, val memb_obj)
{
  val self = lit("union-out");
  struct uni *us = uni_struct_checked(uni);
  struct txr_ffi_type *tft = us->tft;
  struct txr_ffi_type *mtft = ffi_find_memb(tft, memb);
  if (mtft == 0)
    ffi_memb_not_found(tft->self, memb, self);
  mtft->out(mtft, 0, memb_obj, us->data, self);
  return memb_obj;
}


void ffi_init(void)
{
  prot1(&ffi_typedef_hash);
  uint8_s = intern(lit("uint8"), user_package);
  int8_s = intern(lit("int8"), user_package);
  int8_s = intern(lit("int8"), user_package);
  uint16_s = intern(lit("uint16"), user_package);
  int16_s = intern(lit("int16"), user_package);
  uint32_s = intern(lit("uint32"), user_package);
  int32_s = intern(lit("int32"), user_package);
  uint64_s = intern(lit("uint64"), user_package);
  int64_s = intern(lit("int64"), user_package);
  char_s = intern(lit("char"), user_package);
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
  struct_s = intern(lit("struct"), user_package);
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
  align_s = intern(lit("align"), user_package);
  bool_s = intern(lit("bool"), user_package);
  ffi_type_s = intern(lit("ffi-type"), user_package);
  ffi_call_desc_s = intern(lit("ffi-call-desc"), user_package);
  ffi_closure_s = intern(lit("ffi-closure"), user_package);
  reg_fun(intern(lit("ffi-type-compile"), user_package), func_n1(ffi_type_compile));
#if HAVE_LIBFFI
  reg_fun(intern(lit("ffi-make-call-desc"), user_package), func_n4(ffi_make_call_desc));
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
  reg_fun(intern(lit("ffi-put-into"), user_package), func_n3(ffi_put_into));
  reg_fun(intern(lit("ffi-put"), user_package), func_n2(ffi_put));
  reg_fun(intern(lit("ffi-in"), user_package), func_n4(ffi_in));
  reg_fun(intern(lit("ffi-get"), user_package), func_n2(ffi_get));
  reg_fun(intern(lit("ffi-out"), user_package), func_n4(ffi_out));
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
  reg_fun(intern(lit("carray-buf"), user_package), func_n2(carray_buf));
  reg_fun(intern(lit("carray-buf-sync"), user_package), func_n1(carray_buf_sync));
  reg_fun(intern(lit("carray-cptr"), user_package), func_n3o(carray_cptr, 2));
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
  reg_fun(intern(lit("carray-pun"), user_package), func_n2(carray_pun));
  reg_fun(intern(lit("carray-unum"), user_package), func_n2o(carray_unum, 1));
  reg_fun(intern(lit("carray-num"), user_package), func_n2o(carray_num, 1));
  reg_fun(intern(lit("unum-carray"), user_package), func_n1(unum_carray));
  reg_fun(intern(lit("num-carray"), user_package), func_n1(num_carray));
  reg_fun(intern(lit("put-carray"), user_package), func_n3o(put_carray, 1));
  reg_fun(intern(lit("fill-carray"), user_package), func_n3o(fill_carray, 1));
  reg_fun(intern(lit("make-union"), user_package), func_n1(make_union));
  reg_fun(intern(lit("union-members"), user_package), func_n1(union_members));
  reg_fun(intern(lit("union-get"), user_package), func_n2(union_get));
  reg_fun(intern(lit("union-put"), user_package), func_n3(union_put));
  reg_fun(intern(lit("union-in"), user_package), func_n3(union_in));
  reg_fun(intern(lit("union-out"), user_package), func_n3(union_out));
  ffi_typedef_hash = make_hash(nil, nil, nil);
  ffi_init_types();
  ffi_init_extra_types();
}
