/* Copyright 2018-2019
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

#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <stdlib.h>
#include <limits.h>
#include <signal.h>
#include <assert.h>
#include "config.h"
#include "alloca.h"
#include "lib.h"
#include "eval.h"
#include "signal.h"
#include "unwind.h"
#include "gc.h"
#include "args.h"
#include "itypes.h"
#include "buf.h"
#include "arith.h"
#include "vmop.h"
#include "vm.h"

typedef u32_t vm_word_t;

#define max(a, b) ((a) > (b) ? (a) : (b))

#define zalloca(size) memset(alloca(size), 0, size)

struct vm_desc_links {
  struct vm_desc *next, *prev;
};

struct vm_desc {
  struct vm_desc_links lnk;
  val self;
  int nlvl;
  int nreg;
  int frsz;
  cnum stsz;
  val bytecode;
  val datavec;
  val symvec;
  vm_word_t *code;
  val *data;
  struct vm_stent *stab;
};

struct vm_stent {
  val bind;
  loc bindloc;
};

struct vm_env {
  val *mem;
  val vec;
};

struct vm {
  struct vm_desc *vd;
  int nlvl;
  int lev;
  unsigned ip;
  vm_word_t *code;
  struct vm_env *dspl;
};

struct vm_closure {
  struct vm_desc *vd;
  int frsz;
  int nlvl;
  unsigned ip;
  struct vm_env dspl[1];
};

val vm_desc_s, vm_closure_s;

static_forward(struct cobj_ops vm_desc_ops);

static_forward(struct cobj_ops vm_closure_ops);

static struct vm_desc_links vmd_list = {
  coerce(struct vm_desc *, &vmd_list), coerce(struct vm_desc *, &vmd_list)
};

static struct vm_desc *vm_desc_struct(val self, val obj)
{
  return coerce(struct vm_desc *, cobj_handle(self, obj, vm_desc_s));
}

val vm_make_desc(val nlevels, val nregs, val bytecode,
                 val datavec, val symvec)
{
  val self = lit("sys:vm-make-desc");
  int nlvl = c_int(nlevels, self), nreg = c_int(nregs, self);

  if (nlvl < 2 || nlvl > 256)
    uw_throwf(error_s, lit("~a: nlevels must be 2 to 256; ~s given"),
              self, nlevels, nao);

  if (nreg < 1 || nreg > 256)
    uw_throwf(error_s, lit("~a: nregs must be 1 to 256; ~s given"),
              self, nregs, nao);

  {
    mem_t *code = buf_get(bytecode, self);
    val dvl = length_vec(datavec);
    cnum stsz = c_num(length_vec(symvec));
    loc data_loc = if3(dvl != zero, vecref_l(datavec, zero), nulloc);
    struct vm_desc *vd = coerce(struct vm_desc *, chk_malloc(sizeof *vd));
    struct vm_desc *vtail = vmd_list.prev, *vnull = vtail->lnk.next;
    struct vm_stent *stab = if3(stsz != 0,
                                coerce(struct vm_stent *,
                                       chk_calloc(stsz, sizeof *stab)), 0);
    val desc;

    vd->nlvl = nlvl;
    vd->nreg = nreg;
    vd->code = coerce(vm_word_t *, code);
    vd->data = valptr(data_loc);
    vd->stab = stab;
    vd->stsz = stsz;

    vd->bytecode = nil;
    vd->datavec = nil;
    vd->symvec = nil;

    vd->frsz = nlvl * 2 + nreg;

    vd->self = nil;

    vd->lnk.prev = vtail;
    vd->lnk.next = vnull;
    vnull->lnk.prev = vd;
    vtail->lnk.next = vd;

    desc = cobj(coerce(mem_t *, vd), vm_desc_s, &vm_desc_ops);

    vd->bytecode = bytecode;
    vd->datavec = datavec;
    vd->symvec = symvec;
    vd->self = desc;

    return desc;
  }
}

static val vm_desc_nlevels(val desc)
{
  val self = lit("vm_desc_nlevels");
  struct vm_desc *vd = vm_desc_struct(self, desc);
  return num(vd->nlvl);
}

static val vm_desc_nregs(val desc)
{
  val self = lit("vm-desc-nregs");
  struct vm_desc *vd = vm_desc_struct(self, desc);
  return num(vd->nreg);
}

static val vm_desc_bytecode(val desc)
{
  val self = lit("vm-desc-bytecode");
  struct vm_desc *vd = vm_desc_struct(self, desc);
  return vd->bytecode;
}

static val vm_desc_datavec(val desc)
{
  val self = lit("vm-desc-datavec");
  struct vm_desc *vd = vm_desc_struct(self, desc);
  return vd->datavec;
}

static val vm_desc_symvec(val desc)
{
  val self = lit("vm-desc-symvec");
  struct vm_desc *vd = vm_desc_struct(self, desc);
  return vd->symvec;
}

static void vm_desc_destroy(val obj)
{
  struct vm_desc *vd = coerce(struct vm_desc *, obj->co.handle);
  struct vm_desc *vn = vd->lnk.next, *vp = vd->lnk.prev;
  vp->lnk.next = vn;
  vn->lnk.prev = vp;
  vd->lnk.prev = vd->lnk.next = 0;
  free(vd->stab);
  free(vd);
}

static void vm_desc_mark(val obj)
{
  struct vm_desc *vd = coerce(struct vm_desc *, obj->co.handle);
  cnum i, stsz = vd->stsz;

  gc_mark(vd->bytecode);
  gc_mark(vd->datavec);
  gc_mark(vd->symvec);

  for (i = 0; i < stsz; i++)
    gc_mark(vd->stab[i].bind);
}

static struct vm_closure *vm_closure_struct(val self, val obj)
{
  return coerce(struct vm_closure *, cobj_handle(self, obj, vm_closure_s));
}

static val vm_make_closure(struct vm *vm, int frsz)
{
  size_t dspl_sz = vm->nlvl * sizeof (struct vm_env);
  struct vm_closure *vc = coerce(struct vm_closure *,
                                 chk_malloc(offsetof (struct vm_closure, dspl)
                                            + dspl_sz));
  val closure;
  int i;

  vc->frsz = frsz;
  vc->ip = vm->ip;
  vc->nlvl = vm->lev + 1;
  vc->vd = vm->vd;

  memset(vc->dspl, 0, dspl_sz);

  assert (vc->nlvl <= vm->nlvl);

  closure = cobj(coerce(mem_t *, vc), vm_closure_s, &vm_closure_ops);

  for (i = 2; i < vc->nlvl; i++) {
    struct vm_env *sdi = &vm->dspl[i];
    struct vm_env *cdi = &vc->dspl[i];
    val vec = sdi->vec;
    val *mem = sdi->mem;

    switch (type(vec)) {
    case NIL:
      cdi->vec = nil;
      cdi->mem = 0;
      break;
    case NUM:
      {
        val heap_vec = vector(vec, nil);
        size_t size = sizeof *cdi->mem * c_num(vec);
        cdi->vec = heap_vec;
        cdi->mem = heap_vec->v.vec;
        memcpy(cdi->mem, mem, size);
        memset(mem, 0, size);
        setcheck(closure, heap_vec);
        *sdi = *cdi;
        break;
      }
    case VEC:
      cdi->vec = vec;
      cdi->mem = mem;
      break;
    default:
      internal_error("bad vector in vm display");
    }
  }

  return closure;
}

val vm_copy_closure(val oclosure)
{
  struct vm_closure *ovc = coerce(struct vm_closure *, oclosure->co.handle);
  const size_t hdr_sz = offsetof (struct vm_closure, dspl);
  size_t dspl_sz = ovc->nlvl * sizeof (struct vm_env);
  struct vm_closure *nvc = coerce(struct vm_closure *,
                                  chk_malloc(hdr_sz + dspl_sz));
  val nclosure;
  int i;

  memcpy(nvc, ovc, hdr_sz + dspl_sz);

  nclosure = cobj(coerce(mem_t *, nvc), vm_closure_s, &vm_closure_ops);

  for (i = 2; i < nvc->nlvl; i++) {
    struct vm_env *ndi = &nvc->dspl[i];

    if (ndi->vec != nil) {
      ndi->vec = copy_vec(ndi->vec);
      ndi->mem = ndi->vec->v.vec;
    }
  }

  return nclosure;
}

static void vm_closure_mark(val obj)
{
  struct vm_closure *vc = coerce(struct vm_closure *, obj->co.handle);
  int i;

  gc_mark(vc->vd->self);

  for (i = 2; i < vc->nlvl; i++)
    gc_mark(vc->dspl[i].vec);
}

static void vm_reset(struct vm *vm, struct vm_desc *vd,
                     struct vm_env *dspl,
                     int start_lev, unsigned start_ip)
{
  vm->vd = vd;
  vm->nlvl = vd->nlvl;
  vm->lev = start_lev;
  vm->ip = start_ip;
  vm->code = vd->code;
  vm->dspl = dspl;
}

#define vm_insn_opcode(insn) convert(vm_op_t, ((insn) >> 26))
#define vm_insn_operand(insn) ((insn) & 0xFFFFU)
#define vm_insn_extra(insn) (((insn) >> 16) & 0x3FF)
#define vm_insn_bigop(insn) (((insn) & 0x3FFFFFFU))
#define vm_arg_operand_lo(arg) ((arg) & 0xFFFFU)
#define vm_arg_operand_hi(arg) ((arg) >> 16)
#define VM_LEV_BITS 10
#define VM_LEV_MASK 0x3FF
#define VM_SM_LEV_BITS 6
#define VM_SM_LEV_MASK 0x3F
#define vm_lev(arg) ((arg) >> VM_LEV_BITS)
#define vm_idx(arg) ((arg) & VM_LEV_MASK)
#define vm_sm_lev(arg) ((arg) >> VM_SM_LEV_BITS)
#define vm_sm_idx(arg) ((arg) & VM_SM_LEV_MASK)

static val vm_execute(struct vm *vm);

INLINE val vm_get(struct vm_env *dspl, unsigned ref)
{
  return dspl[vm_lev(ref)].mem[vm_idx(ref)];
}

INLINE val vm_getz(struct vm_env *dspl, unsigned ref)
{
  unsigned lev = vm_lev(ref);
  val *addr = &dspl[lev].mem[vm_idx(ref)];
  return (lev == 0) ? z(*addr) : *addr;
}

INLINE val vm_sm_get(struct vm_env *dspl, unsigned ref)
{
  return dspl[vm_sm_lev(ref)].mem[vm_sm_idx(ref)];
}

NOINLINE static void vm_set_nil_error(void)
{
    uw_throwf(error_s, lit("modification of t00/nil"), nao);
}

INLINE void vm_set(struct vm_env *dspl, unsigned ref, val newval)
{
  unsigned d = vm_lev(ref);
  unsigned i = vm_idx(ref);
  struct vm_env *env = &dspl[d];

  if (ref == 0)
    vm_set_nil_error();

  env->mem[i] = newval;

  if (is_ptr(env->vec))
    setcheck(env->vec, newval);
}

INLINE void vm_sm_set(struct vm_env *dspl, unsigned ref, val newval)
{
  unsigned d = vm_sm_lev(ref);
  unsigned i = vm_sm_idx(ref);
  struct vm_env *env = &dspl[d];

  if (ref == 0)
    vm_set_nil_error();

  env->mem[i] = newval;

  if (is_ptr(env->vec))
    setcheck(env->vec, newval);
}


static void vm_do_frame(struct vm *vm, vm_word_t insn, int capturable)
{
  int lev = vm_insn_extra(insn);
  int size = vm_insn_operand(insn);

  if (lev != vm->lev + 1)
    uw_throwf(error_s, lit("frame level mismatch"), nao);

  vm->lev = lev;
  vm->dspl[lev].mem = coerce(val *, zalloca(size * sizeof (val *)));
  vm->dspl[lev].vec = (capturable ? num_fast(size) : 0);
  vm_execute(vm);
  vm->lev = lev - 1;
}

static val vm_prof_callback(mem_t *ctx)
{
  struct vm *vm = coerce(struct vm *, ctx);
  return vm_execute(vm);
}

NOINLINE static void vm_prof(struct vm *vm, vm_word_t insn)
{
  unsigned dest = vm_insn_operand(insn);
  val result = prof_call(vm_prof_callback, coerce(mem_t *, vm));
  vm_set(vm->dspl, dest, result);
}

NOINLINE static void vm_frame(struct vm *vm, vm_word_t insn)
{
  vm_do_frame(vm, insn, 1);
}

NOINLINE static void vm_sframe(struct vm *vm, vm_word_t insn)
{
  vm_do_frame(vm, insn, 0);
}

NOINLINE static void vm_dframe(struct vm *vm, vm_word_t insn)
{
  val saved_dyn_env = dyn_env;
  dyn_env = make_env(nil, nil, dyn_env);
  vm_frame(vm, insn);
  dyn_env = saved_dyn_env;
}

NOINLINE static val vm_end(struct vm *vm, vm_word_t insn)
{
  return vm_get(vm->dspl, vm_insn_operand(insn));
}

NOINLINE static val vm_fin(struct vm *vm, vm_word_t insn)
{
  vm->ip--;
  return vm_get(vm->dspl, vm_insn_operand(insn));
}

NOINLINE static void vm_call(struct vm *vm, vm_word_t insn)
{
  unsigned nargs = vm_insn_extra(insn);
  unsigned dest = vm_insn_operand(insn);
  vm_word_t argw = vm->code[vm->ip++];
  unsigned fun = vm_arg_operand_lo(argw);
  val result;
  args_decl (args, max(nargs, ARGS_MIN));

  if (nargs--) {
    args_add(args, vm_get(vm->dspl, vm_arg_operand_hi(argw)));

    while (nargs >= 2) {
      nargs -= 2;
      argw = vm->code[vm->ip++];
      args_add(args, vm_getz(vm->dspl, vm_arg_operand_lo(argw)));
      args_add(args, vm_getz(vm->dspl, vm_arg_operand_hi(argw)));
    }

    if (nargs) {
      argw = vm->code[vm->ip++];
      args_add(args, vm_getz(vm->dspl, vm_arg_operand_lo(argw)));
    }
  }

  result = generic_funcall(vm_getz(vm->dspl, fun), args);
  vm_set(vm->dspl, dest, result);
}

NOINLINE static void vm_apply(struct vm *vm, vm_word_t insn)
{
  unsigned nargs = vm_insn_extra(insn);
  unsigned dest = vm_insn_operand(insn);
  vm_word_t argw = vm->code[vm->ip++];
  unsigned fun = vm_arg_operand_lo(argw);
  val result;
  args_decl (args, max(nargs, ARGS_MIN));

  if (nargs--) {
    args_add(args, vm_getz(vm->dspl, vm_arg_operand_hi(argw)));

    while (nargs >= 2) {
      nargs -= 2;
      argw = vm->code[vm->ip++];
      args_add(args, vm_getz(vm->dspl, vm_arg_operand_lo(argw)));
      args_add(args, vm_getz(vm->dspl, vm_arg_operand_hi(argw)));
    }

    if (nargs) {
      argw = vm->code[vm->ip++];
      args_add(args, vm_getz(vm->dspl, vm_arg_operand_lo(argw)));
    }
  }

  result = applyv(vm_getz(vm->dspl, fun), args);
  vm_set(vm->dspl, dest, result);
}


NOINLINE static loc vm_stab_slowpath(struct vm *vm, unsigned fun,
                                     val (*lookup_fn)(val env, val sym),
                                     val kind_str)
{
  struct vm_desc *vd = vm->vd;
  struct vm_stent *fe = &vd->stab[fun];

  if (nilp(fe->bind = lookup_fn(nil, vecref(vd->symvec, num_fast(fun)))))
    eval_error(vd->bytecode,
               lit("~a ~s is not defined"), kind_str,
               vecref(vd->symvec, num(fun)), nao);
  setcheck(vd->self, fe->bind);

  return (fe->bindloc = cdr_l(fe->bind));
}

INLINE loc vm_stab(struct vm *vm, unsigned fun,
                   val (*lookup_fn)(val env, val sym), val kind_str)
{
  struct vm_desc *vd = vm->vd;
  struct vm_stent *fe = &vd->stab[fun];
  loc bindloc = fe->bindloc;

  if (!nullocp(bindloc))
    return bindloc;

  return vm_stab_slowpath(vm, fun, lookup_fn, kind_str);
}

NOINLINE static void vm_gcall(struct vm *vm, vm_word_t insn)
{
  unsigned nargs = vm_insn_extra(insn);
  unsigned dest = vm_insn_operand(insn);
  vm_word_t argw = vm->code[vm->ip++];
  unsigned funidx = vm_arg_operand_lo(argw);
  val fun = deref(vm_stab(vm, funidx, lookup_fun, lit("function")));
  val result;

  switch (nargs) {
  case 0:
    result = funcall(fun);
    break;
  case 1:
    {
      val arg1 = vm_getz(vm->dspl, vm_arg_operand_hi(argw));
      result = funcall1(fun, z(arg1));
    }
    break;
  case 2:
    {
      vm_word_t argx = vm->code[vm->ip++];
      val arg1 = vm_getz(vm->dspl, vm_arg_operand_hi(argw));
      val arg2 = vm_getz(vm->dspl, vm_arg_operand_lo(argx));
      result = funcall2(fun, z(arg1), z(arg2));
    }
    break;
  case 3:
    {
      vm_word_t argx = vm->code[vm->ip++];
      val arg1 = vm_getz(vm->dspl, vm_arg_operand_hi(argw));
      val arg2 = vm_getz(vm->dspl, vm_arg_operand_lo(argx));
      val arg3 = vm_getz(vm->dspl, vm_arg_operand_hi(argx));
      result = funcall3(fun, z(arg1), z(arg2), z(arg3));
    }
    break;
  case 4:
    {
      vm_word_t argx = vm->code[vm->ip++];
      vm_word_t argy = vm->code[vm->ip++];
      val arg1 = vm_getz(vm->dspl, vm_arg_operand_hi(argw));
      val arg2 = vm_getz(vm->dspl, vm_arg_operand_lo(argx));
      val arg3 = vm_getz(vm->dspl, vm_arg_operand_hi(argx));
      val arg4 = vm_getz(vm->dspl, vm_arg_operand_lo(argy));
      result = funcall4(fun, z(arg1), z(arg2), z(arg3), z(arg4));
    }
    break;
  default:
    {
      args_decl (args, max(nargs, ARGS_MIN));
      args_add(args, vm_getz(vm->dspl, vm_arg_operand_hi(argw)));
      nargs--;

      while (nargs >= 2) {
        nargs -= 2;
        argw = vm->code[vm->ip++];
        args_add(args, vm_getz(vm->dspl, vm_arg_operand_lo(argw)));
        args_add(args, vm_getz(vm->dspl, vm_arg_operand_hi(argw)));
      }

      if (nargs) {
        argw = vm->code[vm->ip++];
        args_add(args, vm_getz(vm->dspl, vm_arg_operand_lo(argw)));
      }

      result = generic_funcall(fun, args);
    }
    break;
  }

  vm_set(vm->dspl, dest, result);
}

NOINLINE static void vm_gapply(struct vm *vm, vm_word_t insn)
{
  unsigned nargs = vm_insn_extra(insn);
  unsigned dest = vm_insn_operand(insn);
  vm_word_t argw = vm->code[vm->ip++];
  unsigned fun = vm_arg_operand_lo(argw);
  val result;
  args_decl (args, max(nargs, ARGS_MIN));

  if (nargs--) {
    args_add(args, vm_getz(vm->dspl, vm_arg_operand_hi(argw)));

    while (nargs >= 2) {
      nargs -= 2;
      argw = vm->code[vm->ip++];
      args_add(args, vm_getz(vm->dspl, vm_arg_operand_lo(argw)));
      args_add(args, vm_getz(vm->dspl, vm_arg_operand_hi(argw)));
    }

    if (nargs) {
      argw = vm->code[vm->ip++];
      args_add(args, vm_getz(vm->dspl, vm_arg_operand_lo(argw)));
    }
  }

  result = applyv(deref(vm_stab(vm, fun, lookup_fun,
                                lit("function"))), args);
  vm_set(vm->dspl, dest, result);
}

NOINLINE static void vm_movrs(struct vm *vm, vm_word_t insn)
{
  val datum = vm_sm_get(vm->dspl, vm_insn_extra(insn));
  vm_set(vm->dspl, vm_insn_operand(insn), datum);
}

NOINLINE static void vm_movsr(struct vm *vm, vm_word_t insn)
{
  val datum = vm_get(vm->dspl, vm_insn_operand(insn));
  vm_sm_set(vm->dspl, vm_insn_extra(insn), datum);
}

NOINLINE static void vm_movrr(struct vm *vm, vm_word_t insn)
{
  vm_word_t arg = vm->code[vm->ip++];
  val datum = vm_get(vm->dspl, vm_arg_operand_lo(arg));
  vm_set(vm->dspl, vm_insn_operand(insn), datum);
}

NOINLINE static void vm_movrsi(struct vm *vm, vm_word_t insn)
{
  unsigned dst = vm_insn_operand(insn);
  ucnum negmask = ~convert(ucnum, 0x3FF);
  ucnum imm = vm_insn_extra(insn);

  if ((imm & TAG_MASK) == NUM && (imm & 0x200))
    imm |= negmask;

  vm_set(vm->dspl, dst, coerce(val, imm));
}

NOINLINE static void vm_movsmi(struct vm *vm, vm_word_t insn)
{
  unsigned dst = vm_insn_extra(insn);
  ucnum negmask = ~convert(ucnum, 0xFFFF);
  ucnum imm = vm_insn_operand(insn);

  if ((imm & TAG_MASK) == NUM && (imm & 0x8000))
    imm |= negmask;

  vm_sm_set(vm->dspl, dst, coerce(val, imm));
}

NOINLINE static void vm_movrbi(struct vm *vm, vm_word_t insn)
{
  unsigned dst = vm_insn_operand(insn);
  ucnum negmask = ~convert(ucnum, 0xFFFFFFFF);
  ucnum imm = vm->code[vm->ip++];

  if ((imm & TAG_MASK) == NUM && (imm & 0x80000000))
    imm |= negmask;

  vm_set(vm->dspl, dst, coerce(val, imm));
}

static void vm_jmp(struct vm *vm, vm_word_t insn)
{
  vm->ip = vm_insn_bigop(insn);
}

NOINLINE static void vm_if(struct vm *vm, vm_word_t insn)
{
  unsigned ip = vm_insn_bigop(insn);
  vm_word_t arg = vm->code[vm->ip++];
  val test = vm_get(vm->dspl, vm_arg_operand_lo(arg));

  if (!test)
    vm->ip = vm_insn_bigop(ip);
}

NOINLINE static void vm_ifq(struct vm *vm, vm_word_t insn)
{
  unsigned ip = vm_insn_bigop(insn);
  vm_word_t arg = vm->code[vm->ip++];
  val a = vm_get(vm->dspl, vm_arg_operand_lo(arg));
  val b = vm_get(vm->dspl, vm_arg_operand_hi(arg));

  if (a != b)
    vm->ip = vm_insn_bigop(ip);
}

NOINLINE static void vm_ifql(struct vm *vm, vm_word_t insn)
{
  unsigned ip = vm_insn_bigop(insn);
  vm_word_t arg = vm->code[vm->ip++];
  val a = vm_get(vm->dspl, vm_arg_operand_lo(arg));
  val b = vm_get(vm->dspl, vm_arg_operand_hi(arg));

  if (!eql(a, b))
    vm->ip = vm_insn_bigop(ip);
}

NOINLINE static void vm_swtch(struct vm *vm, vm_word_t insn)
{
  unsigned tblsz = vm_insn_extra(insn);
  ucnum idx = c_unum(vm_get(vm->dspl, vm_insn_operand(insn)));

  if (idx < tblsz) {
    vm_word_t tgt = vm->code[vm->ip + idx / 2];
    unsigned shift = (idx % 2) * 16;
    vm->ip = (tgt >> shift) & 0xFFFFU;
  } else {
    struct vm_desc *vd = vm->vd;
    eval_error(vd->bytecode,
               lit("switch index ~s is out of range"),
               num(idx), nao);
  }
}

NOINLINE static void vm_uwprot(struct vm *vm, vm_word_t insn)
{
  int saved_lev = vm->lev;
  unsigned cleanup_ip = vm_insn_bigop(insn);

  uw_simple_catch_begin;

  vm_execute(vm);

  uw_unwind {
    vm->lev = saved_lev;
    vm->ip = cleanup_ip;
    vm_execute(vm);
  }

  uw_catch_end;
}

NOINLINE static void vm_block(struct vm *vm, vm_word_t insn)
{
  unsigned exitpt = vm_insn_bigop(insn);
  vm_word_t arg = vm->code[vm->ip++];
  unsigned outreg = vm_arg_operand_hi(arg);
  unsigned blname = vm_arg_operand_lo(arg);
  int saved_lev = vm->lev;

  uw_block_begin (vm_get(vm->dspl, blname), result);
  uw_blk.bl.cont_bottom = coerce(mem_t *, vm + 1);
  result = vm_execute(vm);
  uw_block_end;

  vm_set(vm->dspl, outreg, result);
  vm->ip = exitpt;
  vm->lev = saved_lev;
}

NOINLINE static void vm_no_block_err(struct vm *vm, val name)
{
  if (name)
    eval_error(vm->vd->bytecode,
               lit("return-from: no block named ~s is visible"),
               name, nao);
  else
    eval_error(vm->vd->bytecode,
               lit("return: no anonymous block is visible"),
               name, nao);
}

NOINLINE static void vm_retsr(struct vm *vm, vm_word_t insn)
{
  val res = vm_get(vm->dspl, vm_insn_operand(insn));
  val tag = vm_sm_get(vm->dspl, vm_insn_extra(insn));

  uw_block_return(tag, res);
  vm_no_block_err(vm, tag);
}

NOINLINE static void vm_retrs(struct vm *vm, vm_word_t insn)
{
  val res = vm_sm_get(vm->dspl, vm_insn_extra(insn));
  val tag = vm_get(vm->dspl, vm_insn_operand(insn));

  uw_block_return(tag, res);
  vm_no_block_err(vm, tag);
}

NOINLINE static void vm_retrr(struct vm *vm, vm_word_t insn)
{
  vm_word_t arg = vm->code[vm->ip++];
  val res = vm_get(vm->dspl, vm_insn_operand(insn));
  val tag = vm_get(vm->dspl, vm_arg_operand_lo(arg));

  uw_block_return(tag, res);
  vm_no_block_err(vm, tag);
}

NOINLINE static void vm_abscsr(struct vm *vm, vm_word_t insn)
{
  val res = vm_get(vm->dspl, vm_insn_operand(insn));
  val tag = vm_sm_get(vm->dspl, vm_insn_extra(insn));

  uw_block_abscond(tag, res);
  vm_no_block_err(vm, tag);
}

NOINLINE static void vm_catch(struct vm *vm, vm_word_t insn)
{
  unsigned catch_ip = vm_insn_bigop(insn);
  vm_word_t arg1 = vm->code[vm->ip++];
  vm_word_t arg2 = vm->code[vm->ip++];
  unsigned sym_reg = vm_arg_operand_hi(arg1);
  unsigned args_reg = vm_arg_operand_lo(arg1);
  val catch_syms = vm_get(vm->dspl, vm_arg_operand_lo(arg2));
  val desc = vm_get(vm->dspl, vm_arg_operand_hi(arg2));
  int saved_lev = vm->lev;

  uw_catch_begin_w_desc (catch_syms, exsym, exvals, desc);

  vm_execute(vm);

  uw_catch(exsym, exvals) {
    vm_set(vm->dspl, sym_reg, exsym);
    vm_set(vm->dspl, args_reg, exvals);

    vm->ip = catch_ip;
    vm->lev = saved_lev;

    vm_execute(vm);
  }

  uw_unwind;

  uw_catch_end;
}

NOINLINE static void vm_handle(struct vm *vm, vm_word_t insn)
{
  val fun = vm_get(vm->dspl, vm_insn_operand(insn));
  vm_word_t arg1 = vm->code[vm->ip++];
  val handle_syms = vm_get(vm->dspl, vm_arg_operand_lo(arg1));
  uw_frame_t uw_handler;

  uw_push_handler(&uw_handler, handle_syms, fun);

  vm_execute(vm);

  uw_pop_frame(&uw_handler);
}

static val vm_get_binding(struct vm *vm, vm_word_t insn,
                          val (*lookup_fn)(val env, val sym),
                          val kind_str)
{
  val sym = vm_sm_get(vm->dspl, vm_insn_extra(insn));
  val binding = lookup_fn(nil, sym);

  if (nilp(binding))
    eval_error(vm->vd->bytecode, lit("unbound ~a ~s"), kind_str, sym, nao);

  return binding;
}

NOINLINE static void vm_getsym(struct vm *vm, vm_word_t insn,
                               val (*lookup_fn)(val env, val sym),
                               val kind_str)
{
  val binding = vm_get_binding(vm, insn, lookup_fn, kind_str);
  unsigned dst = vm_insn_operand(insn);
  vm_set(vm->dspl, dst, cdr(binding));
}

NOINLINE static void vm_getbind(struct vm *vm, vm_word_t insn,
                                val (*lookup_fn)(val env, val sym),
                                val kind_str)
{
  val binding = vm_get_binding(vm, insn, lookup_fn, kind_str);
  unsigned dst = vm_insn_operand(insn);
  vm_set(vm->dspl, dst, binding);
}

NOINLINE static void vm_setsym(struct vm *vm, vm_word_t insn,
                               val (*lookup_fn)(val env, val sym),
                               val kind_str)
{
  val binding = vm_get_binding(vm, insn, lookup_fn, kind_str);
  unsigned src = vm_insn_operand(insn);
  rplacd(binding, vm_get(vm->dspl, src));
}

NOINLINE static void vm_bindv(struct vm *vm, vm_word_t insn)
{
  val sym = vm_sm_get(vm->dspl, vm_insn_extra(insn));
  int src = vm_insn_operand(insn);

  if (nilp(dyn_env))
    eval_error(vm->vd->bytecode,
               lit("no environment for dynamic binding"), nao);

  env_vbind(dyn_env, sym, vm_get(vm->dspl, src));
}

NOINLINE static void vm_gettab(struct vm *vm, vm_word_t insn,
                               val (*lookup_fn)(val env, val sym),
                               val kind_str)
{
  unsigned idx = vm_insn_operand(insn);
  unsigned dst = vm_insn_extra(insn);
  vm_sm_set(vm->dspl, dst, deref(vm_stab(vm, idx, lookup_fn, kind_str)));
}

NOINLINE static void vm_settab(struct vm *vm, vm_word_t insn,
                               val (*lookup_fn)(val env, val sym),
                               val kind_str)
{
  unsigned idx = vm_insn_operand(insn);
  unsigned src = vm_insn_extra(insn);
  set(vm_stab(vm, idx, lookup_fn, kind_str), vm_sm_get(vm->dspl, src));
}

NOINLINE static void vm_close(struct vm *vm, vm_word_t insn)
{
  unsigned dst = vm_insn_bigop(insn);
  vm_word_t arg1 = vm->code[vm->ip++];
  vm_word_t arg2 = vm->code[vm->ip++];
  unsigned vari_fr = vm_arg_operand_hi(arg1);
  int variadic = vari_fr & (1 << VM_LEV_BITS);
  int frsz = vari_fr & VM_LEV_MASK;
  unsigned reg = vm_arg_operand_lo(arg1);
  int reqargs = vm_arg_operand_hi(arg2);
  int fixparam = vm_arg_operand_lo(arg2);
  val closure = vm_make_closure(vm, frsz);
  val vf = func_vm(closure, vm->vd->self, fixparam, reqargs, variadic);

  vm_set(vm->dspl, reg, vf);
  vm->ip = dst;
}

NOINLINE static val vm_execute(struct vm *vm)
{
  for (;;) {
    vm_word_t insn = vm->code[vm->ip++];
    vm_op_t opcode = vm_insn_opcode(insn);

    switch (opcode) {
    case NOOP:
      break;
    case FRAME:
      vm_frame(vm, insn);
      break;
    case SFRAME:
      vm_sframe(vm, insn);
      break;
    case DFRAME:
      vm_dframe(vm, insn);
      break;
    case END:
      return vm_end(vm, insn);
    case FIN:
      return vm_fin(vm, insn);
    case PROF:
      vm_prof(vm, insn);
      break;
    case CALL:
      vm_call(vm, insn);
      break;
    case APPLY:
      vm_apply(vm, insn);
      break;
    case GCALL:
      vm_gcall(vm, insn);
      break;
    case GAPPLY:
      vm_gapply(vm, insn);
      break;
    case MOVRS:
      vm_movrs(vm, insn);
      break;
    case MOVSR:
      vm_movsr(vm, insn);
      break;
    case MOVRR:
      vm_movrr(vm, insn);
      break;
    case MOVRSI:
      vm_movrsi(vm, insn);
      break;
    case MOVSMI:
      vm_movsmi(vm, insn);
      break;
    case MOVRBI:
      vm_movrbi(vm, insn);
      break;
    case JMP:
      vm_jmp(vm, insn);
      break;
    case IF:
      vm_if(vm, insn);
      break;
    case IFQ:
      vm_ifq(vm, insn);
      break;
    case IFQL:
      vm_ifql(vm, insn);
      break;
    case SWTCH:
      vm_swtch(vm, insn);
      break;
    case UWPROT:
      vm_uwprot(vm, insn);
      break;
    case BLOCK:
      vm_block(vm, insn);
      break;
    case RETSR:
      vm_retsr(vm, insn);
      break;
    case RETRS:
      vm_retrs(vm, insn);
      break;
    case RETRR:
      vm_retrr(vm, insn);
      break;
    case ABSCSR:
      vm_abscsr(vm, insn);
      break;
    case CATCH:
      vm_catch(vm, insn);
      break;
    case HANDLE:
      vm_handle(vm, insn);
      break;
    case GETV:
      vm_getsym(vm, insn, lookup_var, lit("variable"));
      break;
    case OLDGETF:
      vm_getsym(vm, insn, lookup_fun, lit("function"));
      break;
    case GETL1:
      vm_getsym(vm, insn, lookup_sym_lisp1, lit("variable/function"));
      break;
    case GETVB:
      vm_getbind(vm, insn, lookup_var, lit("variable"));
      break;
    case GETFB:
      vm_getbind(vm, insn, lookup_fun, lit("function"));
      break;
    case GETL1B:
      vm_getbind(vm, insn, lookup_sym_lisp1, lit("variable/function"));
      break;
    case SETV:
      vm_setsym(vm, insn, lookup_var, lit("variable"));
      break;
    case SETL1:
      vm_setsym(vm, insn, lookup_sym_lisp1, lit("variable/function"));
      break;
    case BINDV:
      vm_bindv(vm, insn);
      break;
    case CLOSE:
      vm_close(vm, insn);
      break;
    case GETLX:
      vm_gettab(vm, insn, lookup_var, lit("variable"));
      break;
    case SETLX:
      vm_settab(vm, insn, lookup_var, lit("variable"));
      break;
    case GETF:
      vm_gettab(vm, insn, lookup_fun, lit("function"));
      break;
    default:
      uw_throwf(error_s, lit("invalid opcode ~s"), num_fast(opcode), nao);
    }
  }
}

val vm_execute_toplevel(val desc)
{
  val self = lit("vm-execute-toplevel");
  struct vm_desc *vd = vm_desc_struct(self, desc);
  struct vm vm;
  val *frame = coerce(val *, alloca(sizeof *frame * vd->frsz));
  struct vm_env *dspl = coerce(struct vm_env *, frame + vd->nreg);

  vm_reset(&vm, vd, dspl, 1, 0);

  vm.dspl = coerce(struct vm_env *, frame + vd->nreg);

  frame[0] = nil;

  vm.dspl[0].mem = frame;
  vm.dspl[0].vec = nil;

  vm.dspl[1].mem = vd->data;
  vm.dspl[1].vec = vd->datavec;

  return vm_execute(&vm);
}

val vm_execute_closure(val fun, struct args *args)
{
  val self = lit("vm-execute-closure");
  val closure = fun->f.env;
  val desc = fun->f.f.vm_desc;
  int fixparam = fun->f.fixparam;
  int variadic = fun->f.variadic;
  struct vm_desc *vd = vm_desc_struct(self, desc);
  struct vm_closure *vc = coerce(struct vm_closure *, closure->co.handle);
  struct vm vm;
  val *frame = coerce(val *, alloca(sizeof *frame * vd->frsz));
  struct vm_env *dspl = coerce(struct vm_env *, frame + vd->nreg);
  val vargs = if3(variadic, args_get_rest(args, fixparam), nil);
  cnum ix = 0;
  vm_word_t argw = 0;

  vm_reset(&vm, vd, dspl, vc->nlvl - 1, vc->ip);

  vm.dspl = coerce(struct vm_env *, frame + vd->nreg);

  frame[0] = nil;

  vm.dspl[0].mem = frame;
  vm.dspl[0].vec = nil;

  vm.dspl[1].mem = vd->data;
  vm.dspl[1].vec = vd->datavec;

  memcpy(vm.dspl + 2, vc->dspl + 2, (vc->nlvl - 2) * sizeof *vm.dspl);

  if (vc->frsz != 0) {
    vm.lev++;
    vm.dspl[vm.lev].mem = coerce(val *, zalloca(vc->frsz * sizeof (val *)));
    vm.dspl[vm.lev].vec = num_fast(vc->frsz);
  }

  while (fixparam >= 2) {
    fixparam -= 2;
    argw = vm.code[vm.ip++];
    unsigned xreg = vm_arg_operand_lo(argw);
    unsigned yreg = vm_arg_operand_hi(argw);
    vm_set(dspl, xreg, args_get(args, &ix));
    vm_set(dspl, yreg, args_get(args, &ix));
  }

  if (fixparam) {
    argw = vm.code[vm.ip++];
    unsigned xreg = vm_arg_operand_lo(argw);
    vm_set(dspl, xreg, args_get(args, &ix));
  }

  if (variadic) {
    unsigned vreg;
    if (!fixparam) {
      argw = vm.code[vm.ip++];
      vreg = vm_arg_operand_lo(argw);
    } else {
      vreg = vm_arg_operand_hi(argw);
    }

    vm_set(dspl, vreg, z(vargs));
  }

  return vm_execute(&vm);
}

#define vm_funcall_common \
  val closure = fun->f.env;                                                  \
  val desc = fun->f.f.vm_desc;                                               \
  struct vm_desc *vd = vm_desc_struct(self, desc);                           \
  struct vm_closure *vc = coerce(struct vm_closure *, closure->co.handle);   \
  struct vm vm;                                                              \
  val *frame = coerce(val *, alloca(sizeof *frame * vd->frsz));              \
  struct vm_env *dspl = coerce(struct vm_env *, frame + vd->nreg);           \
  vm_reset(&vm, vd, dspl, vc->nlvl - 1, vc->ip);                             \
  vm.dspl = coerce(struct vm_env *, frame + vd->nreg);                       \
  frame[0] = nil;                                                            \
  vm.dspl[0].mem = frame;                                                    \
  vm.dspl[0].vec = nil;                                                      \
  vm.dspl[1].mem = vd->data;                                                 \
  vm.dspl[1].vec = vd->datavec;                                              \
  memcpy(vm.dspl + 2, vc->dspl + 2, (vc->nlvl - 2) * sizeof *vm.dspl);       \
  if (vc->frsz != 0) {                                                       \
    vm.lev++;                                                                \
    vm.dspl[vm.lev].mem = coerce(val *, zalloca(vc->frsz * sizeof (val *))); \
    vm.dspl[vm.lev].vec = num_fast(vc->frsz);                                \
  }

val vm_funcall(val fun)
{
  val self = lit("vm-funcall");
  vm_funcall_common;

  return vm_execute(&vm);
}

val vm_funcall1(val fun, val arg)
{
  val self = lit("vm-funcall1");
  vm_funcall_common;

  {
    vm_word_t argw = vm.code[vm.ip++];
    unsigned areg = vm_arg_operand_lo(argw);
    vm_set(dspl, areg, arg);
  }

  return vm_execute(&vm);
}

val vm_funcall2(val fun, val arg1, val arg2)
{
  val self = lit("vm-funcall2");
  vm_funcall_common;

  {
    vm_word_t arg12w = vm.code[vm.ip++];
    unsigned a1reg = vm_arg_operand_lo(arg12w);
    unsigned a2reg = vm_arg_operand_hi(arg12w);
    vm_set(dspl, a1reg, arg1);
    vm_set(dspl, a2reg, arg2);
  }

  return vm_execute(&vm);
}

val vm_funcall3(val fun, val arg1, val arg2, val arg3)
{
  val self = lit("vm-funcall3");
  vm_funcall_common;

  {
    vm_word_t arg12w = vm.code[vm.ip++];
    vm_word_t arg34w = vm.code[vm.ip++];
    unsigned a1reg = vm_arg_operand_lo(arg12w);
    unsigned a2reg = vm_arg_operand_hi(arg12w);
    unsigned a3reg = vm_arg_operand_lo(arg34w);
    vm_set(dspl, a1reg, arg1);
    vm_set(dspl, a2reg, arg2);
    vm_set(dspl, a3reg, arg3);
  }

  return vm_execute(&vm);
}

val vm_funcall4(val fun, val arg1, val arg2, val arg3, val arg4)
{
  val self = lit("vm-funcall4");
  vm_funcall_common;

  {
    vm_word_t arg12w = vm.code[vm.ip++];
    vm_word_t arg34w = vm.code[vm.ip++];
    unsigned a1reg = vm_arg_operand_lo(arg12w);
    unsigned a2reg = vm_arg_operand_hi(arg12w);
    unsigned a3reg = vm_arg_operand_lo(arg34w);
    unsigned a4reg = vm_arg_operand_hi(arg34w);
    vm_set(dspl, a1reg, arg1);
    vm_set(dspl, a2reg, arg2);
    vm_set(dspl, a3reg, arg3);
    vm_set(dspl, a4reg, arg4);
  }

  return vm_execute(&vm);
}

static val vm_closure_desc(val closure)
{
  val self = lit("vm-closure-desc");
  struct vm_closure *vc = vm_closure_struct(self, closure);
  return vc->vd->self;
}

static val vm_closure_entry(val closure)
{
  val self = lit("vm-closure-entry");
  struct vm_closure *vc = vm_closure_struct(self, closure);
  return unum(vc->ip);
}

void vm_invalidate_binding(val sym)
{
  struct vm_desc *vd = vmd_list.next, *vnull = vd->lnk.prev;

  for (; vd != vnull; vd = vd->lnk.next) {
    cnum i;
    for (i = 0; i < vd->stsz; i++) {
      if (vecref(vd->symvec, num_fast(i)) == sym) {
        struct vm_stent *sti = &vd->stab[i];
        sti->bind = nil;
        sti->bindloc = nulloc;
      }
    }
  }
}

static_def(struct cobj_ops vm_desc_ops =
  cobj_ops_init(eq,
                cobj_print_op,
                vm_desc_destroy,
                vm_desc_mark,
                cobj_eq_hash_op));

static_def(struct cobj_ops vm_closure_ops =
  cobj_ops_init(eq,
                cobj_print_op,
                cobj_destroy_free_op,
                vm_closure_mark,
                cobj_eq_hash_op));

void vm_init(void)
{
  vm_desc_s = intern(lit("vm-desc"), system_package);
  vm_closure_s = intern(lit("vm-closure"), system_package);
  reg_fun(intern(lit("vm-make-desc"), system_package), func_n5(vm_make_desc));
  reg_fun(intern(lit("vm-desc-nlevels"), system_package), func_n1(vm_desc_nlevels));
  reg_fun(intern(lit("vm-desc-nregs"), system_package), func_n1(vm_desc_nregs));
  reg_fun(intern(lit("vm-desc-bytecode"), system_package), func_n1(vm_desc_bytecode));
  reg_fun(intern(lit("vm-desc-datavec"), system_package), func_n1(vm_desc_datavec));
  reg_fun(intern(lit("vm-desc-symvec"), system_package), func_n1(vm_desc_symvec));
  reg_fun(intern(lit("vm-execute-toplevel"), system_package), func_n1(vm_execute_toplevel));
  reg_fun(intern(lit("vm-closure-desc"), system_package), func_n1(vm_closure_desc));
  reg_fun(intern(lit("vm-closure-entry"), system_package), func_n1(vm_closure_entry));
}
