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
#include <ffi.h>
#include "config.h"
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
#include "ffi.h"

val uint8_s, int8_s;
val uint16_s, int16_s;
val uint32_s, int32_s;
val uint64_s, int64_s;

val char_s, uchar_s;
val short_s, ushort_s;
val int_s, uint_s;
val long_s, ulong_s;

val double_s;

val array_s;

val void_s;

val struct_s;

val wstr_s;

val ptr_in_s, ptr_out_s, ptr_in_out_s;

val ffi_type_s, ffi_call_desc_s;

struct txr_ffi_type {
  ffi_type *ft;
  val lt;
  val syntax;
  val mnames;
  val mtypes;
  cnum size, align;
  cnum nelem;
  void (*put)(struct txr_ffi_type *, val obj, mem_t *dst, val self);
  val (*get)(struct txr_ffi_type *, mem_t *src, val self);
  void (*in)(struct txr_ffi_type *, val obj, val self);
  mem_t *(*alloc)(struct txr_ffi_type *, val obj, val self);
  void (*free)(void *);
  void (*fill)(struct txr_ffi_type *, mem_t *src, val obj, val self);
  mem_t *buf;
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

static void ffi_builtin_type_struct_destroy_op(val obj)
{
  struct txr_ffi_type *tft = ffi_type_struct(obj);

  if (tft->in)
    (void) tft->in(tft, nil, nil);

  free(obj->co.handle);
}

static void ffi_type_struct_destroy_op(val obj)
{
  struct txr_ffi_type *tft = ffi_type_struct(obj);
  ffi_type *ft = tft->ft;

  if (ft != 0) {
    int i;
    for (i = 0; ; i++) {
      ffi_type *el = ft->elements[i];
      if (!el)
        break;
      free(el);
    }
    ft->elements = 0;
  }

  free(ft);
  tft->ft = 0;

  if (tft->in)
    (void) tft->in(tft, nil, nil);

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
                ffi_builtin_type_struct_destroy_op,
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
                ffi_builtin_type_struct_destroy_op,
                ffi_ptr_type_mark,
                cobj_hash_op);

static void ffi_void_put(struct txr_ffi_type *tft,
                         val n, mem_t *dst, val self)
{
  (void) tft;
  (void) n;
  (void) dst;
  (void) self;
}

static mem_t *ffi_fixed_alloc(struct txr_ffi_type *tft, val obj, val self)
{
  (void) obj;
  (void) self;
  return chk_malloc(tft->size);
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

#if HAVE_I8
static void ffi_i8_put(struct txr_ffi_type *tft,
                       val n, mem_t *dst, val self)
{
  (void) tft;
  i8_t v = c_i8(n, self);
  memcpy(dst, &v, sizeof v);
}

static val ffi_i8_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  (void) tft;
  (void) self;
  return num_fast(*src);
}

static void ffi_u8_put(struct txr_ffi_type *tft,
                       val n, mem_t *dst, val self)
{
  (void) tft;
  u8_t v = c_u8(n, self);
  memcpy(dst, &v, sizeof v);
}

static val ffi_u8_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  (void) tft;
  (void) self;
  return num_fast(*coerce(u8_t *, src));
}

#endif

#if HAVE_I16
static void ffi_i16_put(struct txr_ffi_type *tft,
                        val n, mem_t *dst, val self)
{
  (void) tft;
  i16_t v = c_i16(n, self);
  memcpy(dst, &v, sizeof v);
}

static val ffi_i16_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  (void) tft;
  (void) self;
  i16_t n;
  memcpy(&n, src, sizeof n);
  return num_fast(n);
}

static void ffi_u16_put(struct txr_ffi_type *tft,
                        val n, mem_t *dst, val self)
{
  (void) tft;
  u16_t v = c_u16(n, self);
  memcpy(dst, &v, sizeof v);
}

static val ffi_u16_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  (void) tft;
  (void) self;
  u16_t n;
  memcpy(&n, src, sizeof n);
  return num_fast(n);
}
#endif

#if HAVE_I32
static void ffi_i32_put(struct txr_ffi_type *tft,
                        val n, mem_t *dst, val self)
{
  (void) tft;
  i32_t v = c_i32(n, self);
  memcpy(dst, &v, sizeof v);
}

static val ffi_i32_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  (void) tft;
  (void) self;
  i32_t n;
  memcpy(&n, src, sizeof n);
  return num(n);
}

static void ffi_u32_put(struct txr_ffi_type *tft,
                        val n, mem_t *dst, val self)
{
  (void) tft;
  u32_t v = c_u32(n, self);
  memcpy(dst, &v, sizeof v);
}

static val ffi_u32_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  (void) tft;
  (void) self;
  u32_t n;
  memcpy(&n, src, sizeof n);
  return unum(n);
}
#endif

#if HAVE_I64
static void ffi_i64_put(struct txr_ffi_type *tft,
                        val n, mem_t *dst, val self)
{
  i64_t v = c_i64(n, self);
  (void) tft;
  memcpy(dst, &v, sizeof v);
}

static val ffi_i64_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  (void) tft;
  (void) self;
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

static void ffi_u64_put(struct txr_ffi_type *tft,
                        val n, mem_t *dst, val self)
{
  u64_t v = c_u64(n, self);
  memcpy(dst, &v, sizeof v);
}

static val ffi_u64_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  (void) tft;
  (void) self;
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

static void ffi_char_put(struct txr_ffi_type *tft,
                         val n, mem_t *dst, val self)
{
  char v = c_char(n, self);
  (void) tft;
  memcpy(dst, &v, sizeof v);
}

static val ffi_char_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  (void) tft;
  (void) self;
  return num_fast(*coerce(char *, src));
}

static void ffi_uchar_put(struct txr_ffi_type *tft,
                          val n, mem_t *dst, val self)
{
  unsigned char v = c_uchar(n, self);
  (void) tft;
  memcpy(dst, &v, sizeof v);
}

static val ffi_uchar_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  (void) tft;
  (void) self;
  return num_fast(*src);
}

static void ffi_short_put(struct txr_ffi_type *tft,
                          val n, mem_t *dst, val self)
{
  short v = c_short(n, self);
  (void) tft;
  memcpy(dst, &v, sizeof v);
}

static val ffi_short_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  (void) tft;
  (void) self;
  short n;
  memcpy(&n, src, sizeof n);
  return num_fast(n);
}

static void ffi_ushort_put(struct txr_ffi_type *tft,
                           val n, mem_t *dst, val self)
{
  unsigned short v = c_ushort(n, self);
  (void) tft;
  memcpy(dst, &v, sizeof v);
}

static val ffi_ushort_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  (void) tft;
  (void) self;
  unsigned short n;
  memcpy(&n, src, sizeof n);
  return num_fast(n);
}

static void ffi_int_put(struct txr_ffi_type *tft,
                        val n, mem_t *dst, val self)
{
  int v = c_int(n, self);
  (void) tft;
  memcpy(dst, &v, sizeof v);
}

static val ffi_int_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  (void) tft;
  (void) self;
  int n;
  memcpy(&n, src, sizeof n);
  return num(n);
}

static void ffi_uint_put(struct txr_ffi_type *tft,
                         val n, mem_t *dst, val self)
{
  unsigned v = c_uint(n, self);
  (void) tft;
  memcpy(dst, &v, sizeof v);
}

static val ffi_uint_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  (void) tft;
  (void) self;
  unsigned n;
  memcpy(&n, src, sizeof n);
  return unum(n);
}

static void ffi_long_put(struct txr_ffi_type *tft,
                         val n, mem_t *dst, val self)
{
  long v = c_long(n, self);
  (void) tft;
  memcpy(dst, &v, sizeof v);
}

static val ffi_long_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  (void) tft;
  (void) self;
  long n;
  memcpy(&n, src, sizeof n);
  return num(n);
}

static void ffi_ulong_put(struct txr_ffi_type *tft,
                          val n, mem_t *dst, val self)
{
  unsigned long v = c_ulong(n, self);
  (void) tft;
  memcpy(dst, &v, sizeof v);
}

static val ffi_ulong_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  (void) tft;
  (void) self;
  unsigned long n;
  memcpy(&n, src, sizeof n);
  return unum(n);
}

static void ffi_float_put(struct txr_ffi_type *tft,
                          val n, mem_t *dst, val self)
{
  double f = c_flo(n);
  double v;
  (void) tft;
  if (f > FLT_MAX || f < FLT_MIN)
    uw_throwf(error_s, lit("~a: ~s is out of float range"), self, num, nao);
  v = f;
  memcpy(dst, &v, sizeof v);
}

static val ffi_float_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  (void) tft;
  (void) self;
  float n;
  memcpy(&n, src, sizeof n);
  return flo(n);
}

static void ffi_double_put(struct txr_ffi_type *tft,
                           val n, mem_t *dst, val self)
{
  double v = c_flo(n);
  (void) tft;
  memcpy(dst, &v, sizeof v);
}

static val ffi_double_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  (void) tft;
  (void) self;
  double n;
  memcpy(&n, src, sizeof n);
  return flo(n);
}

static void ffi_ptr_put(struct txr_ffi_type *tft,
                        val n, mem_t *dst, val self)
{
  mem_t *p = cptr_get(n);
  (void) tft;
  memcpy(dst, &p, sizeof p);
}

static val ffi_ptr_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  (void) tft;
  (void) self;
  mem_t *p;
  memcpy(&p, src, sizeof p);
  return cptr(p);
}

static void ffi_freeing_in(struct txr_ffi_type *tft, val obj, val self)
{
  (void) obj;
  (void) self;
  free(tft->buf);
  tft->buf = 0;
}

static void ffi_str_put(struct txr_ffi_type *tft,
                        val s, mem_t *dst, val self)
{
  const wchar_t *ws = c_str(s);
  char *u8s = utf8_dup_to(ws);
  free(tft->buf);
  tft->buf = coerce(mem_t *, u8s);
  tft->in = ffi_freeing_in;
  *coerce(const char **, dst) = u8s;
}

static val ffi_str_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  (void) tft;
  (void) self;
  const char *p;
  p = *coerce(const char **, src);
  return string_utf8(p);
}

static void ffi_wstr_put(struct txr_ffi_type *tft,
                        val s, mem_t *dst, val self)
{
  const wchar_t *ws = c_str(s);
  *coerce(const wchar_t **, dst) = ws;
}

static val ffi_wstr_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  (void) tft;
  (void) self;
  const wchar_t *p = *coerce(wchar_t **, src);
  return string(p);
}

static void ffi_buf_put(struct txr_ffi_type *tft,
                        val buf, mem_t *dst, val self)
{
  mem_t *b = buf_get(buf, self);
  *coerce(const mem_t **, dst) = b;
}

static val ffi_buf_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  (void) tft;
  (void) self;
  mem_t *p = *coerce(mem_t **, src);
  return make_duplicate_buf(num(tft->nelem), p);
}

static mem_t *ffi_buf_alloc(struct txr_ffi_type *tft, val buf, val self)
{
  (void) tft;
  return buf_get(buf, self);
}

static void ffi_buf_fill(struct txr_ffi_type *tft, mem_t *src,
                         val buf, val self)
{
  (void) tft;
  buf_fill(buf, src, self);
}

static void ffi_ptr_in_in(struct txr_ffi_type *tft, val obj, val self)
{
  val tgttype = tft->mtypes;
  struct txr_ffi_type *tgtft = ffi_type_struct(tgttype);
  tgtft->free(tft->buf);
  tft->buf = 0;
}

static void ffi_ptr_in_put(struct txr_ffi_type *tft,
                           val s, mem_t *dst, val self)
{
  val tgttype = tft->mtypes;
  struct txr_ffi_type *tgtft = ffi_type_struct(tgttype);
  mem_t *buf = tgtft->alloc(tgtft, s, self);
  tgtft->put(tgtft, s, buf, self);
  tft->buf = buf;
  tft->in = ffi_ptr_in_in;
}

static void ffi_ptr_out_in(struct txr_ffi_type *tft, val obj, val self)
{
  val tgttype = tft->mtypes;
  struct txr_ffi_type *tgtft = ffi_type_struct(tgttype);
  if (tgtft->fill != 0)
    tgtft->fill(tgtft, tft->buf, obj, self);
  tgtft->free(tft->buf);
  tft->buf = 0;
}

static void ffi_ptr_out_put(struct txr_ffi_type *tft,
                            val s, mem_t *dst, val self)
{
  val tgttype = tft->mtypes;
  struct txr_ffi_type *tgtft = ffi_type_struct(tgttype);
  mem_t *buf = tgtft->alloc(tgtft, s, self);
  tft->buf = buf;
  tft->in = ffi_ptr_out_in;
  *coerce(mem_t **, dst) = buf;
}

static val ffi_ptr_out_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  val tgttype = tft->mtypes;
  struct txr_ffi_type *tgtft = ffi_type_struct(tgttype);
  mem_t *ptr = *coerce(mem_t **, src);
  return tgtft->get(tgtft, ptr, self);
}

static void ffi_ptr_in_out_put(struct txr_ffi_type *tft,
                               val s, mem_t *dst, val self)
{
  val tgttype = tft->mtypes;
  struct txr_ffi_type *tgtft = ffi_type_struct(tgttype);
  mem_t *buf = tgtft->alloc(tgtft, s, self);
  tgtft->put(tgtft, s, buf, self);
  tft->buf = buf;
  tft->in = ffi_ptr_out_in;
  *coerce(mem_t **, dst) = buf;
}

static void ffi_struct_in(struct txr_ffi_type *tft, val obj, val self)
{
  val types = tft->mtypes;

  while (types) {
    val type = pop(&types);
    struct txr_ffi_type *mtft = ffi_type_struct(type);
    if (mtft->in != 0)
      mtft->in(mtft, obj, self);
  }
}

static void ffi_struct_put(struct txr_ffi_type *tft,
                           val strct, mem_t *dst, val self)
{
  val slots = tft->mnames;
  val types = tft->mtypes;
  ucnum offs = 0;
  int in_pass_needed = 0;

  while (slots) {
    val slsym = pop(&slots);
    val type = pop(&types);
    val slval = slot(strct, slsym);
    struct txr_ffi_type *mtft = ffi_type_struct(type);
    ucnum almask = mtft->align - 1;
    offs = (offs + almask) & ~almask;
    mtft->put(mtft, slval, dst + offs, self);
    offs += mtft->size;
    in_pass_needed = in_pass_needed || mtft->in != 0;
  }

  tft->in = (in_pass_needed ? ffi_struct_in : 0);
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
    val slval;
    offs = (offs + almask) & ~almask;
    slval = mtft->get(mtft, src + offs, self);
    slotset(strct, slsym, slval);
    offs += mtft->size;
  }

  return strct;
}

static void ffi_struct_fill(struct txr_ffi_type *tft, mem_t *src,
                            val strct, val self)
{
  val slots = tft->mnames;
  val types = tft->mtypes;
  ucnum offs = 0;

  while (slots) {
    val slsym = pop(&slots);
    val type = pop(&types);
    struct txr_ffi_type *mtft = ffi_type_struct(type);
    ucnum almask = mtft->align - 1;
    val slval;
    offs = (offs + almask) & ~almask;
    slval = mtft->get(mtft, src + offs, self);
    slotset(strct, slsym, slval);
    offs += mtft->size;
  }
}

static void ffi_array_in(struct txr_ffi_type *tft, val obj, val self)
{
  val eltypes = tft->mtypes;
  cnum nelem = tft->nelem, i;

  for (i = 0; i < nelem; i++) {
    val eltype = pop(&eltypes);
    struct txr_ffi_type *etft = ffi_type_struct(eltype);
    if (etft->in != 0)
      etft->in(etft, obj, self);
  }
}

static void ffi_array_put(struct txr_ffi_type *tft,
                          val vec, mem_t *dst, val self)
{
  val eltypes = tft->mtypes;
  cnum nelem = tft->nelem, i;
  ucnum offs = 0;
  int in_pass_needed = 0;

  for (i = 0; i < nelem; i++) {
    val eltype = pop(&eltypes);
    struct txr_ffi_type *etft = ffi_type_struct(eltype);
    cnum elsize = etft->size;
    val elval = ref(vec, num_fast(i));
    etft->put(etft, elval, dst + offs, self);
    offs += elsize;
    in_pass_needed = in_pass_needed || etft->in != 0;
  }

  tft->in = (in_pass_needed ? ffi_array_in : 0);
}

static val ffi_array_get(struct txr_ffi_type *tft, mem_t *src, val self)
{
  val eltypes = tft->mtypes;
  cnum nelem = tft->nelem, i;
  val vec = vector(num_fast(nelem), nil);
  ucnum offs = 0;

  for (i = 0; i < nelem; i++) {
    val eltype = pop(&eltypes);
    struct txr_ffi_type *etft = ffi_type_struct(eltype);
    cnum elsize = etft->size;
    val elval = etft->get(etft, src + offs, self);
    refset(vec, num_fast(i), elval);
    offs += elsize;
  }

  return vec;
}

static void ffi_array_fill(struct txr_ffi_type *tft, mem_t *src,
                           val vec, val self)
{
  val eltypes = tft->mtypes;
  cnum nelem = tft->nelem, i;
  ucnum offs = 0;

  for (i = 0; i < nelem; i++) {
    val eltype = pop(&eltypes);
    struct txr_ffi_type *etft = ffi_type_struct(eltype);
    cnum elsize = etft->size;
    val elval = etft->get(etft, src + offs, self);
    refset(vec, num_fast(i), elval);
    offs += elsize;
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
                                 void (*put)(struct txr_ffi_type *,
                                             val obj, mem_t *dst, val self),
                                 val (*get)(struct txr_ffi_type *,
                                            mem_t *src, val self),
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
  tft->alloc = ffi_fixed_alloc;
  tft->free = free;

  return obj;
}


static val make_ffi_type_struct(val syntax, val lisp_type,
                                val slots, val types)
{
  struct txr_ffi_type *tft = coerce(struct txr_ffi_type *,
                                    chk_malloc(sizeof *tft));
  ffi_type *ft = coerce(ffi_type *, chk_calloc(1, sizeof *ft));

  cnum nmemb = c_num(length(types)), i;
  ffi_type **elements = coerce(ffi_type **, chk_malloc(sizeof *elements *
                                                       (nmemb + 1)));
  val obj = cobj(coerce(mem_t *, tft), ffi_type_s, &ffi_type_struct_ops);
  cnum total_size = 0;
  cnum most_align = 0;

  ft->type = FFI_TYPE_STRUCT;
  ft->size = 0;

  tft->ft = ft;
  tft->syntax = syntax;
  tft->lt = lisp_type;
  tft->mnames = slots;
  tft->mtypes = types;
  tft->put = ffi_struct_put;
  tft->get = ffi_struct_get;
  tft->alloc = ffi_fixed_alloc;
  tft->free = free;
  tft->fill = ffi_struct_fill;

  for (i = 0; i < nmemb; i++) {
    val type = pop(&types);
    struct txr_ffi_type *mtft = ffi_type_struct(type);
    cnum align = mtft->align;
    cnum size = mtft->size;

    elements[i] = mtft->ft;

    if (align > most_align)
      most_align = align;

    total_size = (total_size + align - 1) / align * align + size;
  }

  elements[i] = 0;

  ft->elements = elements;

  total_size = (total_size + most_align - 1) / most_align * most_align;

  tft->size = total_size;
  tft->align = most_align;

  return obj;
}

static val make_ffi_type_array(val syntax, val lisp_type,
                               val dim, val eltypes)
{
  struct txr_ffi_type *tft = coerce(struct txr_ffi_type *,
                                    chk_malloc(sizeof *tft));
  ffi_type *ft = coerce(ffi_type *, chk_calloc(1, sizeof *ft));

  cnum nelem = c_num(dim), i;
  ffi_type **elements = coerce(ffi_type **, chk_malloc(sizeof *elements *
                                                       (nelem + 1)));
  val obj = cobj(coerce(mem_t *, tft), ffi_type_s, &ffi_type_struct_ops);

  ft->type = FFI_TYPE_STRUCT;
  ft->size = 0;

  tft->ft = ft;
  tft->syntax = syntax;
  tft->lt = lisp_type;
  tft->mnames = nil;
  tft->mtypes = eltypes;
  tft->put = ffi_array_put;
  tft->get = ffi_array_get;
  tft->alloc = ffi_fixed_alloc;
  tft->free = free;
  tft->fill = ffi_array_fill;
  tft->size = 0;
  tft->align = 0;

  for (i = 0; i < nelem; i++) {
    val eltype = pop(&eltypes);
    struct txr_ffi_type *etft = ffi_type_struct(eltype);
    elements[i] = etft->ft;
    if (i == 0) {
      tft->size = etft->size * nelem;
      tft->align = etft->align;
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
    } else if (sym == array_s) {
      val dim = cadr(syntax);
      val eltype_syntax = caddr(syntax);
      list_collect_decl (eltypes, ptail);
      cnum dimn = c_num(dim), i;
      for (i = 0; i < dimn; i++)
        ptail = list_collect(ptail, ffi_type_compile(eltype_syntax));
      return make_ffi_type_array(syntax, eltypes, dim, eltypes);
    } else if (sym == ptr_in_s) {
      val target_type = ffi_type_compile(cadr(syntax));
      return make_ffi_type_pointer(syntax, cptr_s, sizeof (mem_t *),
                                   &ffi_type_pointer,
                                   ffi_ptr_in_put, ffi_void_get,
                                   target_type);
    } else if (sym == ptr_out_s) {
      val target_type = ffi_type_compile(cadr(syntax));
      struct txr_ffi_type *tft = ffi_type_struct(target_type);
      if (tft->fill == 0)
        uw_throwf(error_s, lit("~a: ~s cannot be ptr-out target"),
                  self, cadr(syntax), nao);
      return make_ffi_type_pointer(syntax, cptr_s, sizeof (mem_t *),
                                   &ffi_type_pointer,
                                   ffi_ptr_out_put, ffi_ptr_out_get,
                                   target_type);
    } else if (sym == ptr_in_out_s) {
      val target_type = ffi_type_compile(cadr(syntax));
      struct txr_ffi_type *tft = ffi_type_struct(target_type);
      if (tft->fill == 0)
        uw_throwf(error_s, lit("~a: ~s cannot be ptr-in-out target"),
                  self, cadr(syntax), nao);
      return make_ffi_type_pointer(syntax, cptr_s, sizeof (mem_t *),
                                   &ffi_type_pointer,
                                   ffi_ptr_in_out_put, ffi_ptr_out_get,
                                   target_type);
    } else if (sym == buf_s) {
      cnum nelem = c_num(cadr(syntax));
      val type = make_ffi_type_builtin(syntax, cptr_s, sizeof (mem_t *),
                                       &ffi_type_pointer,
                                       ffi_buf_put, ffi_buf_get);
      struct txr_ffi_type *tft = ffi_type_struct(type);
      tft->alloc = ffi_buf_alloc;
      tft->free = ffi_noop_free;
      tft->fill = ffi_buf_fill;
      tft->nelem = nelem;
      return type;
    }

    uw_throwf(error_s, lit("~a: unimplemented case"), self, nao);
#if HAVE_I8
  } else if (syntax == uint8_s) {
    return make_ffi_type_builtin(syntax, integer_s, sizeof (i8_t),
                                 &ffi_type_uint8,
                                 ffi_u8_put, ffi_u8_get);

  } else if (syntax == int8_s) {
    return make_ffi_type_builtin(syntax, integer_s, sizeof (i8_t),
                                 &ffi_type_sint8,
                                 ffi_i8_put, ffi_i8_get);
#endif
#if HAVE_I16
  } else if (syntax == uint16_s) {
    return make_ffi_type_builtin(syntax, integer_s, sizeof (i16_t),
                                 &ffi_type_uint16,
                                 ffi_u16_put, ffi_u16_get);
  } else if (syntax == int16_s) {
    return make_ffi_type_builtin(syntax, integer_s, sizeof (i16_t),
                                 &ffi_type_sint16,
                                 ffi_i16_put, ffi_i16_get);
#endif
#if HAVE_I32
  } else if (syntax == uint32_s) {
    return make_ffi_type_builtin(syntax, integer_s, sizeof (i32_t),
                                 &ffi_type_uint32,
                                 ffi_u32_put, ffi_u32_get);
  } else if (syntax == int32_s) {
    return make_ffi_type_builtin(syntax, integer_s, sizeof (i32_t),
                                 &ffi_type_sint32,
                                 ffi_i32_put, ffi_i32_get);
#endif
#if HAVE_I64
  } else if (syntax == uint64_s) {
    return make_ffi_type_builtin(syntax, integer_s, sizeof (i64_t),
                                 &ffi_type_uint64,
                                 ffi_u64_put, ffi_u64_get);
  } else if (syntax == int64_s) {
    return make_ffi_type_builtin(syntax, integer_s, sizeof (i64_t),
                                 &ffi_type_sint64,
                                 ffi_i64_put, ffi_i64_get);
#endif
  } else if (syntax == uchar_s) {
    return make_ffi_type_builtin(syntax, integer_s, 1, &ffi_type_uchar,
                                 ffi_uchar_put, ffi_uchar_get);
  } else if (syntax == char_s) {
#if UCHAR_MAX == CHAR_MAX
    ffi_type *ffi_char = &ffi_type_uchar;
#else
    ffi_type *ffi_char = &ffi_type_schar;
#endif
    return make_ffi_type_builtin(syntax, integer_s, 1, ffi_char,
                                 ffi_char_put, ffi_char_get);
  } else if (syntax == ushort_s) {
    return make_ffi_type_builtin(syntax, integer_s, sizeof (short),
                                 &ffi_type_ushort,
                                 ffi_ushort_put, ffi_ushort_get);
  } else if (syntax == short_s) {
    return make_ffi_type_builtin(syntax, integer_s, sizeof (short),
                                 &ffi_type_sshort,
                                 ffi_short_put, ffi_short_get);
  } else if (syntax == int_s) {
    return make_ffi_type_builtin(syntax, integer_s, sizeof (int),
                                 &ffi_type_sint,
                                 ffi_int_put, ffi_int_get);
  } else if (syntax == uint_s) {
    return make_ffi_type_builtin(syntax, integer_s, sizeof (int),
                                 &ffi_type_uint,
                                 ffi_uint_put, ffi_uint_get);
  } else if (syntax == ulong_s) {
    return make_ffi_type_builtin(syntax, integer_s, sizeof (long),
                                 &ffi_type_ulong,
                                 ffi_ulong_put, ffi_ulong_get);
  } else if (syntax == long_s) {
    return make_ffi_type_builtin(syntax, integer_s, sizeof (long),
                                 &ffi_type_slong,
                                 ffi_long_put, ffi_long_get);
  } else if (syntax == float_s) {
    return make_ffi_type_builtin(syntax, float_s, sizeof (float),
                                 &ffi_type_float,
                                 ffi_float_put, ffi_float_get);
  } else if (syntax == double_s) {
    return make_ffi_type_builtin(syntax, float_s, sizeof (double),
                                 &ffi_type_double,
                                 ffi_double_put, ffi_double_get);
  } else if (syntax == cptr_s) {
    return make_ffi_type_builtin(syntax, cptr_s, sizeof (mem_t *),
                                 &ffi_type_pointer,
                                 ffi_ptr_put, ffi_ptr_get);
  } else if (syntax == str_s) {
    return make_ffi_type_builtin(syntax, cptr_s, sizeof (mem_t *),
                                 &ffi_type_pointer,
                                 ffi_str_put, ffi_str_get);
  } else if (syntax == wstr_s) {
    return make_ffi_type_builtin(syntax, cptr_s, sizeof (mem_t *),
                                 &ffi_type_pointer,
                                 ffi_wstr_put, ffi_wstr_get);
  } else if (syntax == buf_s) {
    val type = make_ffi_type_builtin(syntax, cptr_s, sizeof (mem_t *),
                                     &ffi_type_pointer,
                                     ffi_buf_put, ffi_void_get);
    struct txr_ffi_type *tft = ffi_type_struct(type);
    tft->alloc = ffi_buf_alloc;
    tft->free = ffi_noop_free;
    tft->fill = ffi_buf_fill;
    return type;
  } else if (syntax == void_s) {
    return make_ffi_type_builtin(syntax, nil, 0, &ffi_type_void,
                                 ffi_void_put, ffi_void_get);
  } else {
    uw_throwf(error_s, lit("~a: bad type syntax: ~!~s"),
              self, syntax, nao);
  }
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
        mtft->in(mtft, arg, self);
    }
  }

  return rtft->get(rtft, convert(mem_t *, rc), self);
}

void ffi_init(void)
{
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
  short_s = intern(lit("short"), user_package);
  ushort_s = intern(lit("ushort"), user_package);
  int_s = intern(lit("int"), user_package);
  uint_s = intern(lit("uint"), user_package);
  long_s = intern(lit("long"), user_package);
  ulong_s = intern(lit("ulong"), user_package);
  double_s = intern(lit("double"), user_package);
  array_s = intern(lit("array"), user_package);
  void_s = intern(lit("void"), user_package);
  struct_s = intern(lit("struct"), user_package);
  wstr_s = intern(lit("wstr"), user_package);
  ptr_in_s = intern(lit("ptr-in"), user_package);
  ptr_out_s = intern(lit("ptr-out"), user_package);
  ptr_in_out_s = intern(lit("ptr-in-out"), user_package);
  ffi_type_s = intern(lit("ffi-type"), user_package);
  ffi_call_desc_s = intern(lit("ffi-call-desc"), user_package);
  reg_fun(intern(lit("ffi-type-compile"), user_package), func_n1(ffi_type_compile));
  reg_fun(intern(lit("ffi-make-call-desc"), user_package), func_n4(ffi_make_call_desc));
  reg_fun(intern(lit("ffi-call"), user_package), func_n3(ffi_call_wrap));
}
