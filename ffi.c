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
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <wchar.h>
#include <dirent.h>
#include <time.h>
#include <ffi.h>
#include "config.h"
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

val array_s, zarray_s;

val struct_s;

val str_d_s, wstr_s, wstr_d_s, bstr_s, bstr_d_s;

val buf_d_s;

val ptr_in_s, ptr_out_s, ptr_in_d_s, ptr_out_d_s, ptr_out_s_s, ptr_s;

val closure_s;

val ffi_type_s, ffi_call_desc_s, ffi_closure_s;

static val ffi_typedef_hash;

struct txr_ffi_type {
  ffi_type *ft;
  val lt;
  val syntax;
  val mnames;
  val mtypes;
  cnum size, align;
  cnum nelem;
  unsigned null_term : 1;
  unsigned char_conv : 1;
  unsigned wchar_conv : 1;
  void (*put)(struct txr_ffi_type *, val obj, mem_t *dst, val self);
  val (*get)(struct txr_ffi_type *, mem_t *src, val self);
  val (*in)(struct txr_ffi_type *, mem_t *src, val obj, val self);
  void (*out)(struct txr_ffi_type *, int copy, val obj, mem_t *dest, val self);
  mem_t *(*alloc)(struct txr_ffi_type *, val obj, val self);
  void (*free)(void *);
};

static struct txr_ffi_type *ffi_type_struct(val obj)
{
  return coerce(struct txr_ffi_type *, obj->co.handle);
}

static struct txr_ffi_type *ffi_type_struct_checked(val obj)
{
  return coerce(struct txr_ffi_type *, cobj_handle(obj, ffi_type_s));
}

static ffi_type *ffi_get_type(val obj)
{
  struct txr_ffi_type *tffi = ffi_type_struct_checked(obj);
  return tffi->ft;
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
  ffi_type *ft = tft->ft;
  free(ft->elements);
  ft->elements = 0;
  free(ft);
  tft->ft = 0;
  free(tft);
}

static void ffi_struct_type_mark(val obj)
{
  struct txr_ffi_type *tft = ffi_type_struct(obj);
  gc_mark(tft->lt);
  gc_mark(tft->syntax);
  gc_mark(tft->mnames);
  gc_mark(tft->mtypes);
}

static void ffi_ptr_type_mark(val obj)
{
  struct txr_ffi_type *tft = ffi_type_struct(obj);
  gc_mark(tft->lt);
  gc_mark(tft->syntax);
  gc_mark(tft->mtypes);
}

static struct cobj_ops ffi_type_builtin_ops =
  cobj_ops_init(eq,
                ffi_type_print_op,
                cobj_destroy_free_op,
                cobj_mark_op,
                cobj_hash_op);

static struct cobj_ops ffi_type_struct_ops =
  cobj_ops_init(eq,
                ffi_type_print_op,
                ffi_type_struct_destroy_op,
                ffi_struct_type_mark,
                cobj_hash_op);

static struct cobj_ops ffi_type_ptr_ops =
  cobj_ops_init(eq,
                ffi_type_print_op,
                cobj_destroy_free_op,
                ffi_ptr_type_mark,
                cobj_hash_op);

struct txr_ffi_closure {
  ffi_closure *clo;
  mem_t *fptr;
  cnum nparam;
  val fun;
  val call_desc;
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
  put_string(lit("#<"), out);
  obj_print_impl(obj->co.cls, out, pretty, ctx);
  put_string(lit("#>"), out);
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
}

static struct cobj_ops ffi_closure_ops =
  cobj_ops_init(eq,
                ffi_closure_print_op,
                ffi_closure_destroy_op,
                ffi_closure_mark_op,
                cobj_hash_op);

static void ffi_void_put(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
}

static mem_t *ffi_fixed_alloc(struct txr_ffi_type *tft, val obj, val self)
{
  return chk_malloc(tft->size);
}

static void ffi_noop_free(void *ptr)
{
}

static val ffi_void_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  return nil;
}

#if HAVE_I8
static void ffi_i8_put(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  i8_t v = c_i8(n, self);
  memcpy(dst, &v, sizeof v);
}

static val ffi_i8_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  return num_fast(*src);
}

static void ffi_u8_put(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  u8_t v = c_u8(n, self);
  memcpy(dst, &v, sizeof v);
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
  memcpy(dst, &v, sizeof v);
}

static val ffi_i16_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  i16_t n;
  memcpy(&n, src, sizeof n);
  return num_fast(n);
}

static void ffi_u16_put(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  u16_t v = c_u16(n, self);
  memcpy(dst, &v, sizeof v);
}

static val ffi_u16_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  u16_t n;
  memcpy(&n, src, sizeof n);
  return num_fast(n);
}
#endif

#if HAVE_I32
static void ffi_i32_put(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  i32_t v = c_i32(n, self);
  memcpy(dst, &v, sizeof v);
}

static val ffi_i32_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  i32_t n;
  memcpy(&n, src, sizeof n);
  return num(n);
}

static void ffi_u32_put(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  u32_t v = c_u32(n, self);
  memcpy(dst, &v, sizeof v);
}

static val ffi_u32_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  u32_t n;
  memcpy(&n, src, sizeof n);
  return unum(n);
}
#endif

#if HAVE_I64
static void ffi_i64_put(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  i64_t v = c_i64(n, self);
  memcpy(dst, &v, sizeof v);
}

static val ffi_i64_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  i64_t n;
  memcpy(&n, src, sizeof n);

  if (sizeof (i64_t) <= sizeof (cnum)) {
    return num(n);
  } else {
    val high = num(n >> 32);
    val low = unum(n & 0xFFFFFFFF);
    return logior(ash(high, num_fast(32)), low);
  }
}

static void ffi_u64_put(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  u64_t v = c_u64(n, self);
  memcpy(dst, &v, sizeof v);
}

static val ffi_u64_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  u64_t n;
  memcpy(&n, src, sizeof n);

  if (sizeof (u64_t) <= sizeof (uint_ptr_t)) {
    return unum(n);
  } else {
    val high = unum(n >> 32);
    val low = unum(n & 0xFFFFFFFF);
    return logior(ash(high, num_fast(32)), low);
  }
}

#endif

static void ffi_char_put(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  char v = c_char(n, self);
  memcpy(dst, &v, sizeof v);
}

static val ffi_char_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  return chr(*coerce(char *, src));
}

static void ffi_uchar_put(struct txr_ffi_type *tft, val n, mem_t *dst,
                          val self)
{
  unsigned char v = c_uchar(n, self);
  memcpy(dst, &v, sizeof v);
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
  memcpy(dst, &v, sizeof v);
}

static val ffi_short_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  short n;
  memcpy(&n, src, sizeof n);
  return num_fast(n);
}

static void ffi_ushort_put(struct txr_ffi_type *tft, val n, mem_t *dst,
                           val self)
{
  unsigned short v = c_ushort(n, self);
  memcpy(dst, &v, sizeof v);
}

static val ffi_ushort_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  unsigned short n;
  memcpy(&n, src, sizeof n);
  return num_fast(n);
}

static void ffi_int_put(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  int v = c_int(n, self);
  memcpy(dst, &v, sizeof v);
}

static val ffi_int_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  int n;
  memcpy(&n, src, sizeof n);
  return num(n);
}

static void ffi_uint_put(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  unsigned v = c_uint(n, self);
  memcpy(dst, &v, sizeof v);
}

static val ffi_uint_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  unsigned n;
  memcpy(&n, src, sizeof n);
  return unum(n);
}

static void ffi_long_put(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  long v = c_long(n, self);
  memcpy(dst, &v, sizeof v);
}

static val ffi_long_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  long n;
  memcpy(&n, src, sizeof n);
  return num(n);
}

static void ffi_ulong_put(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  unsigned long v = c_ulong(n, self);
  memcpy(dst, &v, sizeof v);
}

static val ffi_ulong_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  unsigned long n;
  memcpy(&n, src, sizeof n);
  return unum(n);
}

static void ffi_float_put(struct txr_ffi_type *tft, val n, mem_t *dst, val self)
{
  double f = c_flo(n);
  double v;
  if (f > FLT_MAX || f < FLT_MIN)
    uw_throwf(error_s, lit("~a: ~s is out of float range"), self, num, nao);
  v = f;
  memcpy(dst, &v, sizeof v);
}

static val ffi_float_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  float n;
  memcpy(&n, src, sizeof n);
  return flo(n);
}

static void ffi_double_put(struct txr_ffi_type *tft, val n, mem_t *dst,
                           val self)
{
  double v = c_flo(n);
  memcpy(dst, &v, sizeof v);
}

static val ffi_double_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  double n;
  memcpy(&n, src, sizeof n);
  return flo(n);
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
  memcpy(dst, &c, sizeof c);
}

static val ffi_wchar_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  wchar_t c;
  memcpy(&c, src, sizeof c);
  return chr(c);
}

static void ffi_cptr_put(struct txr_ffi_type *tft, val n, mem_t *dst,
                         val self)
{
  mem_t *p = cptr_get(n);
  memcpy(dst, &p, sizeof p);
}

static val ffi_cptr_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  mem_t *p;
  memcpy(&p, src, sizeof p);
  return cptr(p);
}

static mem_t *ffi_cptr_alloc(struct txr_ffi_type *tft, val ptr, val self)
{
  return coerce(mem_t *, cptr_addr_of(ptr));
}

static val ffi_freeing_in(struct txr_ffi_type *tft, mem_t *src, val obj,
                          val self)
{
  mem_t **loc = coerce(mem_t **, src);
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
  char *p = *coerce(char **, src);
  val ret = p ? string_utf8(p) : nil;
  free(p);
  return ret;
}

static void ffi_wstr_put(struct txr_ffi_type *tft, val s, mem_t *dst,
                         val self)
{
  if (s == nil) {
    *coerce(const wchar_t **, dst) = 0;
  } else {
    const wchar_t *ws = c_str(s);
    *coerce(const wchar_t **, dst) = ws;
  }
}

static val ffi_wstr_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  const wchar_t *p = *coerce(wchar_t **, src);
  return p ? string(p) : 0;
}

static void ffi_wstr_d_put(struct txr_ffi_type *tft, val s, mem_t *dst,
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
  wchar_t *p = *coerce(wchar_t **, src);
  return p ? string_own(p) : nil;
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
  unsigned char *p = *coerce(unsigned char **, src);
  val ret = p ? string_8bit(p) : nil;
  free(p);
  return ret;
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
  mem_t *p = *coerce(mem_t **, src);
  return p ? make_borrowed_buf(num(tft->nelem), p) : nil;
}

static mem_t *ffi_buf_alloc(struct txr_ffi_type *tft, val buf, val self)
{
  return coerce(mem_t *, buf_addr_of(buf, self));
}

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

static val ffi_ptr_in_in(struct txr_ffi_type *tft, mem_t *src, val obj,
                         val self)
{
  val tgttype = tft->mtypes;
  struct txr_ffi_type *tgtft = ffi_type_struct(tgttype);
  mem_t **loc = coerce(mem_t **, src);
  tgtft->free(*loc);
  *loc = 0;
  return obj;
}

static void ffi_ptr_in_out(struct txr_ffi_type *tft, int copy, val s,
                           mem_t *dst, val self)
{
  val tgttype = tft->mtypes;
  struct txr_ffi_type *tgtft = ffi_type_struct(tgttype);
  if (tgtft->out != 0) {
    mem_t *buf = *coerce(mem_t **, dst);
    tgtft->out(tgtft, 0, s, buf, self);
  }
}

static val ffi_ptr_out_in(struct txr_ffi_type *tft, mem_t *src, val obj,
                          val self)
{
  val tgttype = tft->mtypes;
  struct txr_ffi_type *tgtft = ffi_type_struct(tgttype);
  mem_t **loc = coerce(mem_t **, src);
  if (tgtft->in != 0)
    obj = tgtft->in(tgtft, *loc, obj, self);
  tgtft->free(*loc);
  *loc = 0;
  return obj;
}

static void ffi_ptr_out_put(struct txr_ffi_type *tft, val s, mem_t *dst,
                            val self)
{
  val tgttype = tft->mtypes;
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
  val tgttype = tft->mtypes;
  struct txr_ffi_type *tgtft = ffi_type_struct(tgttype);
  mem_t *buf = *coerce(mem_t **, dst);
  if (tgtft->out != 0)
    tgtft->out(tgtft, 1, s, buf, self);
  else
    tgtft->put(tgtft, s, buf, self);
}

static val ffi_ptr_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  val tgttype = tft->mtypes;
  struct txr_ffi_type *tgtft = ffi_type_struct(tgttype);
  mem_t *ptr = *coerce(mem_t **, src);
  return ptr ? tgtft->get(tgtft, ptr, self) : nil;
}

static val ffi_ptr_d_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  val tgttype = tft->mtypes;
  struct txr_ffi_type *tgtft = ffi_type_struct(tgttype);
  mem_t *ptr = *coerce(mem_t **, src);
  val ret = ptr ? tgtft->get(tgtft, ptr, self) : nil;
  free(ptr);
  return ret;
}

static void ffi_ptr_in_put(struct txr_ffi_type *tft, val s, mem_t *dst,
                           val self)
{
  val tgttype = tft->mtypes;
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

static val ffi_ptr_out_s_in(struct txr_ffi_type *tft, mem_t *src, val obj,
                            val self)
{
  val tgttype = tft->mtypes;
  struct txr_ffi_type *tgtft = ffi_type_struct(tgttype);
  mem_t **loc = coerce(mem_t **, src);
  if (tgtft->in != 0)
    obj = tgtft->in(tgtft, *loc, obj, self);
  return obj;
}


static val ffi_struct_in(struct txr_ffi_type *tft, mem_t *src, val strct,
                         val self)
{
  val slots = tft->mnames;
  val types = tft->mtypes;
  ucnum offs = 0;

  if (strct == nil) {
    args_decl(args, 0);
    strct = make_struct(tft->lt, nil, args);
  }

  while (slots) {
    val slsym = pop(&slots);
    val type = pop(&types);
    struct txr_ffi_type *mtft = ffi_type_struct(type);
    ucnum almask = mtft->align - 1;
    offs = (offs + almask) & ~almask;
    if (slsym) {
      if (mtft->in != 0) {
        val slval = slot(strct, slsym);
        slotset(strct, slsym, mtft->in(mtft, src + offs, slval, self));
      } else {
        val slval = mtft->get(mtft, src + offs, self);
        slotset(strct, slsym, slval);
      }
    }
    offs += mtft->size;
  }

  return strct;
}

static void ffi_struct_put(struct txr_ffi_type *tft, val strct, mem_t *dst,
                           val self)
{
  val slots = tft->mnames;
  val types = tft->mtypes;
  ucnum offs = 0;

  while (slots) {
    val slsym = pop(&slots);
    val type = pop(&types);
    struct txr_ffi_type *mtft = ffi_type_struct(type);
    ucnum almask = mtft->align - 1;
    offs = (offs + almask) & ~almask;
    if (slsym) {
      val slval = slot(strct, slsym);
      mtft->put(mtft, slval, dst + offs, self);
    } else {
      memset(dst + offs, 0, mtft->size);
    }
    offs += mtft->size;
  }
}

static void ffi_struct_out(struct txr_ffi_type *tft, int copy, val strct,
                           mem_t *dst, val self)
{
  val slots = tft->mnames;
  val types = tft->mtypes;
  ucnum offs = 0;

  while (types) {
    val slsym = pop(&slots);
    val type = pop(&types);
    struct txr_ffi_type *mtft = ffi_type_struct(type);
    ucnum almask = mtft->align - 1;
    offs = (offs + almask) & ~almask;
    if (mtft->out != 0) {
      if (slsym) {
        val slval = slot(strct, slsym);
        mtft->out(mtft, copy, slval, dst + offs, self);
      } else {
        memset(dst + offs, 0, mtft->size);
      }
    }
    offs += mtft->size;
  }
}

static val ffi_struct_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  val slots = tft->mnames;
  val types = tft->mtypes;
  ucnum offs = 0;
  args_decl(args, 0);
  val strct = make_struct(tft->lt, nil, args);

  while (slots) {
    val slsym = pop(&slots);
    val type = pop(&types);
    struct txr_ffi_type *mtft = ffi_type_struct(type);
    ucnum almask = mtft->align - 1;
    offs = (offs + almask) & ~almask;
    if (slsym) {
      val slval = mtft->get(mtft, src + offs, self);
      slotset(strct, slsym, slval);
    }
    offs += mtft->size;
  }

  return strct;
}

static val ffi_array_in(struct txr_ffi_type *tft, mem_t *src, val vec,
                        val self)
{
  val eltype = tft->mtypes;
  cnum nelem = tft->nelem;

  if (tft->char_conv) {
    val str;

    if (nelem == 0) {
      str = null_string;
    } else {
      const char *chptr = coerce(const char *, src);
      if (chptr[tft->size - 1] == 0) {
        str = string_utf8(chptr);
      } else {
        wchar_t *wch = utf8_dup_from_buf(chptr, tft->size);
        str = string_own(wch);
      }
    }
    vec = if3(vec, replace(vec, str, zero, t), str);
  } else if (tft->wchar_conv) {
    val str;

    if (nelem == 0) {
      str = null_string;
    } else {
      cnum nchar = tft->size / sizeof (wchar_t);
      const wchar_t *wchptr = coerce(const wchar_t *, src);

      if (wchptr[nchar - 1] == 0) {
        str = string(wchptr);
      } else {
        val ustr = mkustring(num_fast(nchar));
        str = init_str(ustr, wchptr);
      }
    }
    vec = if3(vec, replace(vec, str, zero, t), str);
  } else {
    ucnum offs = 0;
    struct txr_ffi_type *etft = ffi_type_struct(eltype);
    cnum elsize = etft->size, i;

    if (vec == nil)
      vec = vector(num_fast(nelem), nil);

    for (i = 0; i < nelem; i++) {
      if (etft->in != 0) {
        val elval = ref(vec, num_fast(i));
        refset(vec, num_fast(i), etft->in(etft, src + offs, elval, self));
      } else {
        val elval = etft->get(etft, src + offs, self);
        refset(vec, num_fast(i), elval);
      }
      offs += elsize;
    }
  }

  return vec;
}

static void ffi_array_put(struct txr_ffi_type *tft, val vec, mem_t *dst,
                          val self)
{
  val eltype = tft->mtypes;
  struct txr_ffi_type *etft = ffi_type_struct(eltype);
  cnum elsize = etft->size;
  cnum nelem = tft->nelem, i;
  int nt = tft->null_term;
  ucnum offs = 0;

  for (i = 0; i < nelem; i++) {
    val elval = ref(vec, num_fast(i));
    if (nt && i == nelem - 1) {
      memset(dst + offs, 0, elsize);
      break;
    }
    etft->put(etft, elval, dst + offs, self);
    offs += elsize;
  }
}

static void ffi_array_out(struct txr_ffi_type *tft, int copy, val vec,
                          mem_t *dst, val self)
{
  val eltype = tft->mtypes;
  struct txr_ffi_type *etft = ffi_type_struct(eltype);
  cnum elsize = etft->size;
  cnum nelem = tft->nelem, i;
  int nt = tft->null_term;
  ucnum offs = 0;

  for (i = 0; i < nelem; i++) {
    if (nt && i == nelem - 1) {
      memset(dst + offs, 0, elsize);
      break;
    }
    if (etft->out != 0) {
      val elval = ref(vec, num_fast(i));
      etft->out(etft, copy, elval, dst + offs, self);
    }
    offs += elsize;
  }
}

static val ffi_array_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  val eltype = tft->mtypes;
  cnum nelem = tft->nelem;

  if (tft->char_conv) {
    if (nelem == 0) {
      return null_string;
    } else {
      const char *chptr = coerce(const char *, src);
      if (chptr[tft->size - 1] == 0) {
        return string_utf8(chptr);
      } else {
        wchar_t *wch = utf8_dup_from_buf(chptr, tft->size);
        return string_own(wch);
      }
    }
  } else if (tft->wchar_conv) {
    if (nelem == 0) {
      return null_string;
    } else {
      cnum nchar = tft->size / sizeof (wchar_t);
      const wchar_t *wchptr = coerce(const wchar_t *, src);

      if (wchptr[nchar - 1] == 0) {
        return string(wchptr);
      } else {
        val ustr = mkustring(num_fast(nchar));
        return init_str(ustr, wchptr);
      }
    }
  } else {
    val vec = vector(num_fast(nelem), nil);
    struct txr_ffi_type *etft = ffi_type_struct(eltype);
    cnum elsize = etft->size;
    cnum offs, i;

    for (i = 0, offs = 0; i < nelem; i++) {
      val elval = etft->get(etft, src + offs, self);
      refset(vec, num_fast(i), elval);
      offs += elsize;
    }

    return vec;
  }
}

static val make_ffi_type_builtin(val syntax, val lisp_type,
                                 cnum size, ffi_type *ft,
                                 void (*put)(struct txr_ffi_type *,
                                             val obj, mem_t *dst, val self),
                                 val (*get)(struct txr_ffi_type *,
                                            mem_t *src, val self))
{
  struct txr_ffi_type *tft = coerce(struct txr_ffi_type *,
                                    chk_calloc(1, sizeof *tft));

  val obj = cobj(coerce(mem_t *, tft), ffi_type_s, &ffi_type_builtin_ops);

  tft->ft = ft;
  tft->syntax = syntax;
  tft->lt = lisp_type;
  tft->mnames = tft->mtypes = nil;
  tft->size = tft->align = size;
  tft->put = put;
  tft->get = get;
  tft->alloc = ffi_fixed_alloc;
  tft->free = free;

  return obj;
}

static val make_ffi_type_pointer(val syntax, val lisp_type,
                                 cnum size, ffi_type *ft,
                                 void (*put)(struct txr_ffi_type *, val obj,
                                             mem_t *dst, val self),
                                 val (*get)(struct txr_ffi_type *,
                                            mem_t *src, val self),
                                 val (*in)(struct txr_ffi_type *, mem_t *src,
                                           val obj, val self),
                                 void (*out)(struct txr_ffi_type *, int copy,
                                             val obj, mem_t *dst, val self),
                                 val tgtype)
{
  struct txr_ffi_type *tft = coerce(struct txr_ffi_type *,
                                    chk_calloc(1, sizeof *tft));

  val obj = cobj(coerce(mem_t *, tft), ffi_type_s, &ffi_type_ptr_ops);

  tft->ft = ft;
  tft->syntax = syntax;
  tft->lt = lisp_type;
  tft->mnames = tft->mtypes = nil;
  tft->size = tft->align = size;
  tft->put = put;
  tft->get = get;
  tft->mtypes = tgtype;
  tft->in = in;
  tft->out = out;
  tft->alloc = ffi_fixed_alloc;
  tft->free = free;

  return obj;
}

static val make_ffi_type_struct(val syntax, val lisp_type,
                                val slots, val types)
{
  struct txr_ffi_type *tft = coerce(struct txr_ffi_type *,
                                    chk_calloc(1, sizeof *tft));
  ffi_type *ft = coerce(ffi_type *, chk_calloc(1, sizeof *ft));

  cnum nmemb = c_num(length(types)), i;
  ffi_type **elements = coerce(ffi_type **, chk_malloc(sizeof *elements *
                                                       (nmemb + 1)));
  val obj = cobj(coerce(mem_t *, tft), ffi_type_s, &ffi_type_struct_ops);
  cnum total_size = 0;
  cnum most_align = 0;
  int need_out_handler = 0;

  ft->type = FFI_TYPE_STRUCT;
  ft->size = 0;

  tft->ft = ft;
  tft->syntax = syntax;
  tft->lt = lisp_type;
  tft->mnames = slots;
  tft->mtypes = types;
  tft->put = ffi_struct_put;
  tft->get = ffi_struct_get;
  tft->in = ffi_struct_in;
  tft->alloc = ffi_fixed_alloc;
  tft->free = free;

  for (i = 0; i < nmemb; i++) {
    val type = pop(&types);
    struct txr_ffi_type *mtft = ffi_type_struct(type);
    cnum align = mtft->align;
    cnum size = mtft->size;

    elements[i] = mtft->ft;

    if (align > most_align)
      most_align = align;

    total_size = (total_size + align - 1) / align * align + size;
    need_out_handler = need_out_handler || mtft->out != 0;
  }

  elements[i] = 0;

  if (need_out_handler)
    tft->out = ffi_struct_out;

  ft->elements = elements;

  total_size = (total_size + most_align - 1) / most_align * most_align;

  tft->size = total_size;
  tft->align = most_align;

  return obj;
}

static val make_ffi_type_array(val syntax, val lisp_type,
                               val dim, val eltype)
{
  struct txr_ffi_type *tft = coerce(struct txr_ffi_type *,
                                    chk_calloc(1, sizeof *tft));
  ffi_type *ft = coerce(ffi_type *, chk_calloc(1, sizeof *ft));

  cnum nelem = c_num(dim), i;
  ffi_type **elements = coerce(ffi_type **, chk_malloc(sizeof *elements *
                                                       (nelem + 1)));
  val obj = cobj(coerce(mem_t *, tft), ffi_type_s, &ffi_type_struct_ops);

  struct txr_ffi_type *etft = ffi_type_struct(eltype);

  ft->type = FFI_TYPE_STRUCT;
  ft->size = 0;

  tft->ft = ft;
  tft->syntax = syntax;
  tft->lt = lisp_type;
  tft->mnames = nil;
  tft->mtypes = eltype;
  tft->put = ffi_array_put;
  tft->get = ffi_array_get;
  tft->in = ffi_array_in;
  tft->alloc = ffi_fixed_alloc;
  tft->free = free;

  for (i = 0; i < nelem; i++) {
    elements[i] = etft->ft;
    if (i == 0) {
      tft->size = etft->size * nelem;
      tft->align = etft->align;
      if (etft->out != 0)
        tft->out = ffi_array_out;
    }
  }

  elements[i] = 0;

  ft->elements = elements;

  tft->nelem = nelem;

  return obj;
}

static val ffi_struct_compile(val membs, val *ptypes, val self)
{
  list_collect_decl (slots, pstail);
  list_collect_decl (types, pttail);

  for (; !endp(membs); membs = cdr(membs)) {
    val mp = car(membs);
    val name = car(mp);
    val type = cadr(mp);
    if (cddr(mp))
      uw_throwf(error_s, lit("~a: excess elements in type-member pair ~s"),
                self, mp, nao);
    pttail = list_collect(pttail, ffi_type_compile(type));
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
                      make_struct_type(sname, nil, nil, slots,
                                       nil, nil, nil, nil));
      val xsyntax = cons(struct_s,
                         cons(sname, membs));
      return make_ffi_type_struct(xsyntax, stype, slots, types);
    } else if (sym == array_s || sym == zarray_s) {
      val dim = cadr(syntax);
      val eltype_syntax = caddr(syntax);
      val eltype = ffi_type_compile(eltype_syntax);

      {
        val type = make_ffi_type_array(syntax, vec_s, dim, eltype);
        struct txr_ffi_type *tft = ffi_type_struct(type);
        if (sym == zarray_s)
          tft->null_term = 1;
        if (eltype_syntax == char_s)
          tft->char_conv = 1;
        else if (eltype_syntax == wchar_s)
          tft->wchar_conv = 1;
        return type;
      }
    } else if (sym == ptr_in_s) {
      val target_type = ffi_type_compile(cadr(syntax));
      return make_ffi_type_pointer(syntax, cptr_s, sizeof (mem_t *),
                                   &ffi_type_pointer,
                                   ffi_ptr_in_put, ffi_ptr_get,
                                   ffi_ptr_in_in, ffi_ptr_in_out,
                                   target_type);
    } else if (sym == ptr_in_d_s) {
      val target_type = ffi_type_compile(cadr(syntax));
      return make_ffi_type_pointer(syntax, cptr_s, sizeof (mem_t *),
                                   &ffi_type_pointer,
                                   ffi_ptr_in_put, ffi_ptr_d_get,
                                   0, ffi_ptr_in_out,
                                   target_type);
    } else if (sym == ptr_out_s) {
      val target_type = ffi_type_compile(cadr(syntax));
      return make_ffi_type_pointer(syntax, cptr_s, sizeof (mem_t *),
                                   &ffi_type_pointer,
                                   ffi_ptr_out_put, ffi_ptr_get,
                                   ffi_ptr_out_in, ffi_ptr_out_out,
                                   target_type);
    } else if (sym == ptr_out_d_s) {
      val target_type = ffi_type_compile(cadr(syntax));
      return make_ffi_type_pointer(syntax, cptr_s, sizeof (mem_t *),
                                   &ffi_type_pointer,
                                   ffi_ptr_out_null_put, ffi_ptr_d_get,
                                   ffi_ptr_out_in, ffi_ptr_out_out,
                                   target_type);
    } else if (sym == ptr_s) {
      val target_type = ffi_type_compile(cadr(syntax));
      return make_ffi_type_pointer(syntax, cptr_s, sizeof (mem_t *),
                                   &ffi_type_pointer,
                                   ffi_ptr_in_put, ffi_ptr_get,
                                   ffi_ptr_out_in, ffi_ptr_out_out,
                                   target_type);
    } else if (sym == ptr_out_s_s) {
      val target_type = ffi_type_compile(cadr(syntax));
      return make_ffi_type_pointer(syntax, cptr_s, sizeof (mem_t *),
                                   &ffi_type_pointer,
                                   ffi_ptr_out_null_put, ffi_ptr_get,
                                   ffi_ptr_out_s_in, ffi_ptr_out_out,
                                   target_type);
    } else if (sym == buf_s || sym == buf_d_s) {
      cnum nelem = c_num(cadr(syntax));
      val type = make_ffi_type_builtin(syntax, cptr_s, sizeof (mem_t *),
                                       &ffi_type_pointer,
                                       if3(sym == buf_s,
                                           ffi_buf_put, ffi_buf_d_put),
                                       if3(sym == buf_s,
                                           ffi_buf_get, ffi_buf_d_get));
      struct txr_ffi_type *tft = ffi_type_struct(type);
      tft->alloc = ffi_buf_alloc;
      tft->free = ffi_noop_free;
      tft->nelem = nelem;
      return type;
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
                                             &ffi_type_uint8,
                                             ffi_u8_put, ffi_u8_get));
  ffi_typedef(int8_s, make_ffi_type_builtin(int8_s, integer_s, sizeof (i8_t),
                                            &ffi_type_sint8,
                                            ffi_i8_put, ffi_i8_get));
#endif
#if HAVE_I16
  ffi_typedef(uint16_s, make_ffi_type_builtin(uint16_s, integer_s,
                                              sizeof (i16_t),
                                              &ffi_type_uint16,
                                              ffi_u16_put, ffi_u16_get));
  ffi_typedef(int16_s, make_ffi_type_builtin(int16_s, integer_s,
                                             sizeof (i16_t),
                                             &ffi_type_sint16,
                                             ffi_i16_put, ffi_i16_get));
#endif
#if HAVE_I32
  ffi_typedef(uint32_s, make_ffi_type_builtin(uint32_s, integer_s,
                                              sizeof (i32_t), &ffi_type_uint32,
                                              ffi_u32_put, ffi_u32_get));
  ffi_typedef(int32_s, make_ffi_type_builtin(int32_s, integer_s,
                                             sizeof (i32_t), &ffi_type_sint32,
                                             ffi_i32_put, ffi_i32_get));
#endif
#if HAVE_I64
  ffi_typedef(uint64_s, make_ffi_type_builtin(uint64_s, integer_s,
                                              sizeof (i64_t), &ffi_type_uint64,
                                              ffi_u64_put, ffi_u64_get));
  ffi_typedef(int64_s, make_ffi_type_builtin(int64_s, integer_s,
                                              sizeof (i64_t), &ffi_type_sint64,
                                              ffi_i64_put, ffi_i64_get));
#endif
  ffi_typedef(uchar_s, make_ffi_type_builtin(uchar_s, integer_s, 1, &ffi_type_uchar,
                                             ffi_uchar_put, ffi_uchar_get));
  ffi_typedef(char_s, make_ffi_type_builtin(char_s, integer_s, 1,
                                            ffi_char, ffi_char_put, ffi_char_get));
  ffi_typedef(bchar_s, make_ffi_type_builtin(bchar_s, char_s,
                                             sizeof (char), &ffi_type_uchar,
                                             ffi_uchar_put, ffi_bchar_get));
  ffi_typedef(wchar_s, make_ffi_type_builtin(wchar_s, char_s,
                                             sizeof (wchar_t), &ffi_type_wchar,
                                             ffi_wchar_put, ffi_wchar_get));
  ffi_typedef(ushort_s, make_ffi_type_builtin(ushort_s, integer_s,
                                              sizeof (short), &ffi_type_ushort,
                                              ffi_ushort_put, ffi_ushort_get));
  ffi_typedef(short_s, make_ffi_type_builtin(short_s, integer_s,
                                             sizeof (short), &ffi_type_sshort,
                                             ffi_short_put, ffi_short_get));
  ffi_typedef(int_s, make_ffi_type_builtin(int_s, integer_s,
                                           sizeof (int), &ffi_type_sint,
                                           ffi_int_put, ffi_int_get));
  ffi_typedef(uint_s, make_ffi_type_builtin(uint_s, integer_s,
                                            sizeof (int), &ffi_type_uint,
                                            ffi_uint_put, ffi_uint_get));
  ffi_typedef(ulong_s, make_ffi_type_builtin(ulong_s, integer_s,
                                             sizeof (long), &ffi_type_ulong,
                                             ffi_ulong_put, ffi_ulong_get));
  ffi_typedef(long_s, make_ffi_type_builtin(long_s, integer_s,
                                            sizeof (long), &ffi_type_slong,
                                            ffi_long_put, ffi_long_get));
  ffi_typedef(float_s, make_ffi_type_builtin(float_s, float_s,
                                             sizeof (float), &ffi_type_float,
                                             ffi_float_put, ffi_float_get));
  ffi_typedef(double_s, make_ffi_type_builtin(double_s, float_s,
                                              sizeof (double), &ffi_type_double,
                                              ffi_double_put, ffi_double_get));

  {
    val type = make_ffi_type_builtin(cptr_s, cptr_s, sizeof (mem_t *),
                                     &ffi_type_pointer,
                                     ffi_cptr_put, ffi_cptr_get);
    struct txr_ffi_type *tft = ffi_type_struct(type);
    tft->alloc = ffi_cptr_alloc;
    tft->free = ffi_noop_free;
    ffi_typedef(cptr_s, type);
  }

  {
    val type = make_ffi_type_builtin(str_s, str_s, sizeof (mem_t *),
                                     &ffi_type_pointer,
                                     ffi_str_put, ffi_str_get);
    struct txr_ffi_type *tft = ffi_type_struct(type);
    tft->in = ffi_freeing_in;
    ffi_typedef(str_s, type);
  }

  ffi_typedef(str_d_s, make_ffi_type_builtin(str_d_s, str_s,
                                             sizeof (mem_t *), &ffi_type_pointer,
                                             ffi_str_put, ffi_str_d_get));
  ffi_typedef(wstr_s, make_ffi_type_builtin(wstr_s, str_s,
                                            sizeof (mem_t *), &ffi_type_pointer,
                                            ffi_wstr_put, ffi_wstr_get));
  ffi_typedef(wstr_d_s, make_ffi_type_builtin(wstr_d_s, str_s,
                                              sizeof (mem_t *), &ffi_type_pointer,
                                              ffi_wstr_d_put, ffi_wstr_d_get));
  ffi_typedef(bstr_s, make_ffi_type_builtin(bstr_s, str_s,
                                            sizeof (mem_t *), &ffi_type_pointer,
                                            ffi_bstr_put, ffi_bstr_get));
  ffi_typedef(bstr_d_s, make_ffi_type_builtin(bstr_d_s, str_s,
                                              sizeof (mem_t *), &ffi_type_pointer,
                                              ffi_bstr_put, ffi_bstr_d_get));

  {
    val iter;

    for (iter = list(buf_s, buf_d_s, nao); iter; iter = cdr(iter)) {
      val sym = car(iter);
      val type = make_ffi_type_builtin(sym, buf_s, sizeof (mem_t *),
                                       &ffi_type_pointer,
                                       if3(sym == buf_s,
                                           ffi_buf_put, ffi_buf_d_put),
                                       ffi_void_get);
      struct txr_ffi_type *tft = ffi_type_struct(type);
      tft->alloc = ffi_buf_alloc;
      tft->free = ffi_noop_free;
      ffi_typedef(sym, type);
    }
  }

  ffi_typedef(closure_s, make_ffi_type_builtin(closure_s, fun_s,
                                               sizeof (mem_t *),
                                               &ffi_type_pointer,
                                               ffi_closure_put, ffi_cptr_get));
  ffi_typedef(void_s, make_ffi_type_builtin(void_s, null_s, 0, &ffi_type_void,
                                            ffi_void_put, ffi_void_get));
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
              type_by_size[(time_t) -1 > 0][sizeof (time_t)]);
  ffi_typedef(intern(lit("clock-t"), user_package),
              if3((clock_t) 0.5 == 0,
                  type_by_size[(clock_t) -1 > 0][sizeof (clock_t)],
                  if3(sizeof (clock_t) == sizeof (float),
                      ffi_type_lookup(float_s),
                      if2(sizeof (clock_t) == sizeof (double),
                          ffi_type_lookup(double_s)))));
  ffi_typedef(intern(lit("int-ptr-t"), user_package),
              type_by_size[(int_ptr_t) -1 > 0][sizeof (int_ptr_t)]);
  ffi_typedef(intern(lit("uint-ptr-t"), user_package),
              type_by_size[(uint_ptr_t) -1 > 0][sizeof (uint_ptr_t)]);
  ffi_typedef(intern(lit("sig-atomic-t"), user_package),
              type_by_size[(sig_atomic_t) -1 > 0][sizeof (sig_atomic_t)]);
  ffi_typedef(intern(lit("ptrdiff-t"), user_package),
              type_by_size[(ptrdiff_t) -1 > 0][sizeof (ptrdiff_t)]);
  ffi_typedef(intern(lit("wint-t"), user_package),
              type_by_size[(wint_t) -1 > 0][sizeof (wint_t)]);

#if HAVE_SYS_TYPES_H
  ffi_typedef(intern(lit("blkcnt-t"), user_package),
              type_by_size[(blkcnt_t) -1 > 0][sizeof (blkcnt_t)]);
  ffi_typedef(intern(lit("blksize-t"), user_package),
              type_by_size[(blksize_t) -1 > 0][sizeof (blksize_t)]);
  ffi_typedef(intern(lit("clockid-t"), user_package),
              type_by_size[(clockid_t) -1 > 0][sizeof (clockid_t)]);
  ffi_typedef(intern(lit("dev-t"), user_package),
              type_by_size[(dev_t) -1 > 0][sizeof (dev_t)]);
  ffi_typedef(intern(lit("fsblkcnt-t"), user_package),
              type_by_size[(fsblkcnt_t) -1 > 0][sizeof (fsblkcnt_t)]);
  ffi_typedef(intern(lit("fsfilcnt-t"), user_package),
              type_by_size[(fsfilcnt_t) -1 > 0][sizeof (fsfilcnt_t)]);
  ffi_typedef(intern(lit("gid-t"), user_package),
              type_by_size[(gid_t) -1 > 0][sizeof (gid_t)]);
  ffi_typedef(intern(lit("id-t"), user_package),
              type_by_size[(id_t) -1 > 0][sizeof (id_t)]);
  ffi_typedef(intern(lit("ino-t"), user_package),
              type_by_size[(ino_t) -1 > 0][sizeof (ino_t)]);
  ffi_typedef(intern(lit("key-t"), user_package),
              type_by_size[(key_t) -1 > 0][sizeof (key_t)]);
  ffi_typedef(intern(lit("loff-t"), user_package),
              type_by_size[(loff_t) -1 > 0][sizeof (loff_t)]);
  ffi_typedef(intern(lit("mode-t"), user_package),
              type_by_size[(mode_t) -1 > 0][sizeof (mode_t)]);
  ffi_typedef(intern(lit("nlink-t"), user_package),
              type_by_size[(nlink_t) -1 > 0][sizeof (nlink_t)]);
  ffi_typedef(intern(lit("off-t"), user_package),
              type_by_size[(off_t) -1 > 0][sizeof (off_t)]);
  ffi_typedef(intern(lit("pid-t"), user_package),
              type_by_size[(pid_t) -1 > 0][sizeof (pid_t)]);
  ffi_typedef(intern(lit("ssize-t"), user_package),
              type_by_size[(ssize_t) -1 > 0][sizeof (ssize_t)]);
  ffi_typedef(intern(lit("uid-t"), user_package),
              type_by_size[(uid_t) -1 > 0][sizeof (uid_t)]);
#endif
}

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
                cobj_hash_op);

val ffi_make_call_desc(val ntotal, val nfixed, val rettype, val argtypes)
{
  val self = lit("ffi-make-call-desc");
  cnum nf = c_num(default_arg(nfixed, zero));
  cnum nt = c_num(ntotal), i;
  struct txr_ffi_call_desc *tfcd = coerce(struct txr_ffi_call_desc *,
                                          chk_calloc(1, sizeof *tfcd));
  ffi_type **args = coerce(ffi_type **, chk_malloc(sizeof *args * nt));
  val obj = cobj(coerce(mem_t *, tfcd), ffi_call_desc_s, &ffi_call_desc_ops);
  ffi_status ffis = FFI_OK;

  tfcd->variadic = (nfixed != nil);
  tfcd->nfixed = nf;
  tfcd->ntotal = nt;
  tfcd->argtypes = argtypes;
  tfcd->rettype = rettype;
  tfcd->args = args;

  for (i = 0; i < nt; i++)
    args[i] = ffi_get_type(pop(&argtypes));

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

val ffi_call_wrap(val ffi_call_desc, val fptr, val args_in)
{
  val self = lit("ffi-call");
  struct txr_ffi_call_desc *tfcd = ffi_call_desc_checked(ffi_call_desc);
  mem_t *fp = cptr_get(fptr);
  cnum n = tfcd->ntotal, i;
  void **values = convert(void **, alloca(sizeof *values * tfcd->ntotal));
  val args = args_in;
  val types = tfcd->argtypes;
  val rtype = tfcd->rettype;
  struct txr_ffi_type *rtft = ffi_type_struct(rtype);
  void *rc = alloca(rtft->size);
  int in_pass_needed = 0;

  for (i = 0; i < n; i++) {
    val type = pop(&types);
    val arg = pop(&args);
    struct txr_ffi_type *mtft = ffi_type_struct(type);
    values[i] = alloca(mtft->size);
    mtft->put(mtft, arg, convert(mem_t *, values[i]), self);
    in_pass_needed = in_pass_needed || mtft->in != 0;
  }

  ffi_call(&tfcd->cif, coerce(void (*)(void), fp), rc, values);

  if (in_pass_needed) {
    types = tfcd->argtypes;
    args = args_in;
    for (i = 0; i < n; i++) {
      val type = pop(&types);
      val arg = pop(&args);
      struct txr_ffi_type *mtft = ffi_type_struct(type);
      if (mtft->in != 0)
        mtft->in(mtft, convert(mem_t *, values[i]), arg, self);
    }
  }

  return rtft->get(rtft, convert(mem_t *, rc), self);
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
      mtft->out(mtft, 0, arg, convert(mem_t *, cargs[i]), self);
    }
  }

  rtft->put(rtft, retval, convert(mem_t *, cret), self);
}

val ffi_make_closure(val fun, val call_desc)
{
  val self = lit("ffi-make-closure");
  struct txr_ffi_closure *tfcl = coerce(struct txr_ffi_closure *,
                                        chk_calloc(1, sizeof *tfcl));
  struct txr_ffi_call_desc *tfcd = ffi_call_desc_checked(call_desc);
  val obj = cobj(coerce(mem_t *, tfcl), ffi_closure_s, &ffi_closure_ops);
  ffi_status ffis = FFI_OK;

  tfcl->clo = convert(ffi_closure *,
                      ffi_closure_alloc(sizeof *tfcl->clo,
                                        coerce(void **, &tfcl->fptr)));

  if (!tfcl->clo)
    uw_throwf(error_s, lit("~a: failed to allocate special closure memory"),
              self, nao);

  if ((ffis = ffi_prep_closure_loc(tfcl->clo, &tfcd->cif, ffi_closure_dispatch, obj,
                                   coerce(void *, tfcl->fptr))) != FFI_OK)
    uw_throwf(error_s, lit("~a: ffi_prep_closure_loc failed: ~s"),
              self, num(ffis), nao);

  tfcl->nparam = tfcd->ntotal;
  tfcl->fun = fun;
  tfcl->call_desc = call_desc;
  tfcl->tfcd = tfcd;

  return obj;
}

mem_t *ffi_closure_get_fptr(val closure)
{
  struct txr_ffi_closure *tfcl = ffi_closure_struct_checked(closure);
  return tfcl->fptr;
}

static val cptr_make(val n)
{
  return if3(missingp(n), cptr(0), cptr(coerce(mem_t *, c_num(n))));
}

val ffi_typedef(val name, val type)
{
  return sethash(ffi_typedef_hash, name, type);
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
  void_s = intern(lit("void"), user_package);
  array_s = intern(lit("array"), user_package);
  zarray_s = intern(lit("zarray"), user_package);
  struct_s = intern(lit("struct"), user_package);
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
  ffi_type_s = intern(lit("ffi-type"), user_package);
  ffi_call_desc_s = intern(lit("ffi-call-desc"), user_package);
  ffi_closure_s = intern(lit("ffi-closure"), user_package);
  reg_fun(intern(lit("ffi-type-compile"), user_package), func_n1(ffi_type_compile));
  reg_fun(intern(lit("ffi-make-call-desc"), user_package), func_n4(ffi_make_call_desc));
  reg_fun(intern(lit("ffi-call"), user_package), func_n3(ffi_call_wrap));
  reg_fun(intern(lit("ffi-make-closure"), user_package), func_n2(ffi_make_closure));
  reg_fun(intern(lit("cptr"), user_package), func_n1o(cptr_make, 0));
  reg_fun(intern(lit("ffi-typedef"), user_package), func_n2(ffi_typedef));
  reg_varl(intern(lit("cptr-null"), user_package), cptr(0));
  ffi_typedef_hash = make_hash(nil, nil, nil);
  ffi_init_types();
  ffi_init_extra_types();
}
