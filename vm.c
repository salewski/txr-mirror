/* Copyright 2018
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
#include <dirent.h>
#include <stdarg.h>
#include <stdlib.h>
#include <limits.h>
#include <signal.h>
#include <assert.h>
#include "config.h"
#include ALLOCA_H
#include "lib.h"
#include "hash.h"
#include "eval.h"
#include "signal.h"
#include "unwind.h"
#include "gc.h"
#include "args.h"
#include "itypes.h"
#include "buf.h"
#include "vmop.h"
#include "vm.h"

typedef u32_t vm_word_t;

#if HAVE_LITTLE_ENDIAN
#define vm_opcode(vw) ((vw) >> 26)
#define vm_oparg(vw) (((vw) >> 16) & 0x3FF)
#else
#define vm_opcode(vw) ((vw) >> 26)
#define vm_oparg(vw) (((vw) >> 16) & 0x3FF)
#endif

#define zalloca(size) memset(alloca(size), 0, size)

struct vm_desc {
  val self;
  int nlvl;
  int nreg;
  int frsz;
  val bytecode;
  val datavec;
  vm_word_t *code;
  val *data;
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
  struct vm_env *dspl;
};

val vm_desc_s, vm_closure_s;

static_forward(struct cobj_ops vm_desc_ops);

static_forward(struct cobj_ops vm_closure_ops);

static struct vm_desc *vm_desc_struct(val obj)
{
  return coerce(struct vm_desc *, cobj_handle(obj, vm_desc_s));
}

val vm_make_desc(val nlevels, val nregs, val bytecode, val datavec)
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
    loc data_loc = vecref_l(datavec, zero);
    struct vm_desc *vd = coerce(struct vm_desc *, chk_malloc(sizeof *vd));
    val desc;

    vd->nlvl = nlvl;
    vd->nreg = nreg;
    vd->code = coerce(vm_word_t *, code);
    vd->data = valptr(data_loc);

    vd->bytecode = nil;
    vd->datavec = nil;

    vd->frsz = nlvl * 2 + nreg;

    vd->self = nil;

    desc = cobj(coerce(mem_t *, vd), vm_desc_s, &vm_desc_ops);

    vd->bytecode = bytecode;
    vd->datavec = datavec;
    vd->self = desc;

    return desc;
  }
}

static val vm_desc_bytecode(val desc)
{
  struct vm_desc *vd = vm_desc_struct(desc);
  return vd->bytecode;
}

static void vm_desc_mark(val obj)
{
  struct vm_desc *vd = coerce(struct vm_desc *, obj->co.handle);
  gc_mark(vd->bytecode);
  gc_mark(vd->datavec);
}

static val vm_make_closure(struct vm *vm, int frsz)
{
  struct vm_env *dspl = coerce(struct vm_env *,
                               chk_calloc(vm->nlvl, sizeof *dspl));
  struct vm_closure *vc = coerce(struct vm_closure *, chk_malloc(sizeof *vc));
  val closure;
  int i, j;

  vc->frsz = frsz;
  vc->ip = vm->ip;
  vc->nlvl = vm->lev + 1;
  vc->vd = vm->vd;
  vc->dspl = dspl;

  closure = cobj(coerce(mem_t *, vc), vm_closure_s, &vm_closure_ops);

  for (i = 2; i < vc->nlvl; i++) {
    struct vm_env *sdi = &vm->dspl[i];
    struct vm_env *cdi = &dspl[i];
    val vec = sdi->vec;
    val *mem = sdi->mem;

    switch (type(vec)) {
    case NIL:
      cdi->vec = nil;
      cdi->mem = 0;
      break;
    case NUM:
      {
        int n = c_num(vec);
        val heap_vec = vector(vec, nil);
        cdi->vec = heap_vec;
        cdi->mem = heap_vec->v.vec;
        for (j = 0; j < n; j++)
          heap_vec->v.vec[j] = mem[j];
        mut(closure);
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

#define vm_insn_opcode(insn) coerce(vm_op_t, ((insn) >> 26))
#define vm_insn_operand(insn) ((insn) & 0xFFFFU)
#define vm_insn_extra(insn) (((insn) >> 16) & 0x3FF)
#define vm_insn_bigop(insn) (((insn) & 0x3FFFFFFU))
#define vm_arg_operand_lo(arg) ((arg) & 0xFFFFU)
#define vm_arg_operand_hi(arg) ((arg) >> 16)

static val vm_execute(struct vm *vm);

INLINE val vm_get(struct vm_env *dspl, unsigned ref)
{
  return dspl[ref >> 8].mem[ref & 0xFF];
}

INLINE void vm_set(struct vm_env *dspl, unsigned ref, val newval)
{
  unsigned d = ref >> 8;
  unsigned i = ref & 0xFF;
  struct vm_env *env = &dspl[d];

  if (d == 1)
    uw_throwf(error_s, lit("modification of VM static data"), nao);

  if (ref == 0)
    uw_throwf(error_s, lit("modification of t00/nil"), nao);

  env->mem[i] = newval;

  if (is_ptr(env->vec))
    mut(env->vec);
}

static void vm_frame(struct vm *vm, vm_word_t insn)
{
  int lev = vm_insn_extra(insn);
  int size = vm_insn_operand(insn);

  if (lev != vm->lev + 1)
    uw_throwf(error_s, lit("frame level mismatch"), nao);

  vm->lev = lev;
  vm->dspl[lev].mem = coerce(val *, zalloca(size * sizeof (val *)));
  vm->dspl[lev].vec = num_fast(size);
  vm_execute(vm);
  vm->lev = lev - 1;
}

static void vm_dframe(struct vm *vm, vm_word_t insn)
{
  val saved_dyn_env = dyn_env;
  dyn_env = make_env(nil, nil, dyn_env);
  vm_frame(vm, insn);
  dyn_env = saved_dyn_env;
}

static val vm_end(struct vm *vm, vm_word_t insn)
{
  return vm_get(vm->dspl, vm_insn_operand(insn));
}

static val vm_fin(struct vm *vm, vm_word_t insn)
{
  vm->ip--;
  return vm_get(vm->dspl, vm_insn_operand(insn));
}

static void vm_call(struct vm *vm, vm_word_t insn)
{
  unsigned nargs = vm_insn_extra(insn);
  unsigned dest = vm_insn_operand(insn);
  vm_word_t argw = vm->code[vm->ip++];
  unsigned fun = vm_arg_operand_lo(argw);
  val result;
  args_decl (args, nargs < ARGS_MIN ? ARGS_MIN : nargs);

  if (nargs--) {
    args_add(args, vm_get(vm->dspl, vm_arg_operand_hi(argw)));

    while (nargs >= 2) {
      nargs -= 2;
      argw = vm->code[vm->ip++];
      args_add(args, vm_get(vm->dspl, vm_arg_operand_lo(argw)));
      args_add(args, vm_get(vm->dspl, vm_arg_operand_hi(argw)));
    }

    if (nargs) {
      argw = vm->code[vm->ip++];
      args_add(args, vm_get(vm->dspl, vm_arg_operand_lo(argw)));
    }
  }

  result = generic_funcall(vm_get(vm->dspl, fun), args);
  vm_set(vm->dspl, dest, result);
}

static void vm_apply(struct vm *vm, vm_word_t insn)
{
  unsigned nargs = vm_insn_extra(insn);
  unsigned dest = vm_insn_operand(insn);
  vm_word_t argw = vm->code[vm->ip++];
  unsigned fun = vm_arg_operand_lo(argw);
  val result;
  args_decl (args, nargs < ARGS_MIN ? ARGS_MIN : nargs);

  if (nargs--) {
    args_add(args, vm_get(vm->dspl, vm_arg_operand_hi(argw)));

    while (nargs >= 2) {
      nargs -= 2;
      argw = vm->code[vm->ip++];
      args_add(args, vm_get(vm->dspl, vm_arg_operand_lo(argw)));
      args_add(args, vm_get(vm->dspl, vm_arg_operand_hi(argw)));
    }

    if (nargs) {
      argw = vm->code[vm->ip++];
      args_add(args, vm_get(vm->dspl, vm_arg_operand_lo(argw)));
    }
  }

  result = apply_intrinsic(vm_get(vm->dspl, fun), args_get_list(args));
  vm_set(vm->dspl, dest, result);
}

static void vm_movrs(struct vm *vm, vm_word_t insn)
{
  val datum = vm_get(vm->dspl, vm_insn_extra(insn));
  vm_set(vm->dspl, vm_insn_operand(insn), datum);
}

static void vm_movsr(struct vm *vm, vm_word_t insn)
{
  val datum = vm_get(vm->dspl, vm_insn_operand(insn));
  vm_set(vm->dspl, vm_insn_extra(insn), datum);
}

static void vm_movrr(struct vm *vm, vm_word_t insn)
{
  vm_word_t arg = vm->code[vm->ip++];
  val datum = vm_get(vm->dspl, vm_arg_operand_lo(arg));
  vm_set(vm->dspl, vm_insn_operand(insn), datum);
}

static void vm_movrsi(struct vm *vm, vm_word_t insn)
{
  unsigned dst = vm_insn_operand(insn);
  ucnum negmask = ~convert(ucnum, 0x3FF);
  ucnum imm = vm_insn_extra(insn);

  if ((imm & TAG_MASK) == NUM && (imm & 0x200))
    imm |= negmask;

  vm_set(vm->dspl, dst, coerce(val, imm));
}

static void vm_movsmi(struct vm *vm, vm_word_t insn)
{
  unsigned dst = vm_insn_extra(insn);
  ucnum negmask = ~convert(ucnum, 0xFFFF);
  ucnum imm = vm_insn_operand(insn);

  if ((imm & TAG_MASK) == NUM && (imm & 0x8000))
    imm |= negmask;

  vm_set(vm->dspl, dst, coerce(val, imm));
}

static void vm_movrbi(struct vm *vm, vm_word_t insn)
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

static void vm_if(struct vm *vm, vm_word_t insn)
{
  unsigned ip = vm_insn_bigop(insn);
  vm_word_t arg = vm->code[vm->ip++];
  val test = vm_get(vm->dspl, vm_arg_operand_lo(arg));

  if (!test)
    vm->ip = vm_insn_bigop(ip);
}

static void vm_uwprot(struct vm *vm, vm_word_t insn)
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

static void vm_block(struct vm *vm, vm_word_t insn)
{
  unsigned exitpt = vm_insn_bigop(insn);
  vm_word_t arg = vm->code[vm->ip++];
  unsigned outreg = vm_arg_operand_hi(arg);
  unsigned blname = vm_arg_operand_lo(arg);
  int saved_lev = vm->lev;

  uw_block_begin (vm_get(vm->dspl, blname), result);
  result = vm_execute(vm);
  uw_block_end;

  vm_set(vm->dspl, outreg, result);
  vm->ip = exitpt;
  vm->lev = saved_lev;
}


static void vm_retsr(struct vm *vm, vm_word_t insn)
{
  val res = vm_get(vm->dspl, vm_insn_operand(insn));
  val tag = vm_get(vm->dspl, vm_insn_extra(insn));

  uw_block_return(tag, res);
}

static void vm_retrs(struct vm *vm, vm_word_t insn)
{
  val res = vm_get(vm->dspl, vm_insn_extra(insn));
  val tag = vm_get(vm->dspl, vm_insn_operand(insn));

  uw_block_return(tag, res);
}

static void vm_retrr(struct vm *vm, vm_word_t insn)
{
  vm_word_t arg = vm->code[vm->ip++];
  val res = vm_get(vm->dspl, vm_insn_operand(insn));
  val tag = vm_get(vm->dspl, vm_arg_operand_lo(arg));

  uw_block_return(tag, res);
}

static void vm_catch(struct vm *vm, vm_word_t insn)
{
  unsigned catch_ip = vm_insn_bigop(insn);
  vm_word_t arg1 = vm->code[vm->ip++];
  vm_word_t arg2 = vm->code[vm->ip++];
  unsigned sym_reg = vm_arg_operand_hi(arg1);
  unsigned args_reg = vm_arg_operand_lo(arg1);
  val catch_syms = vm_get(vm->dspl, vm_arg_operand_lo(arg2));
  int saved_lev = vm->lev;

  uw_catch_begin (catch_syms, exsym, exvals);

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

static void vm_handle(struct vm *vm, vm_word_t insn)
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
  val sym = vm_get(vm->dspl, vm_insn_operand(insn));
  val binding = lookup_fn(nil, sym);

  if (nilp(binding))
    eval_error(vm->vd->bytecode, lit("unbound ~a ~s"), kind_str, sym, nao);

  return binding;
}

static void vm_getsym(struct vm *vm, vm_word_t insn,
                      val (*lookup_fn)(val env, val sym),
                      val kind_str)
{
  val binding = vm_get_binding(vm, insn, lookup_fn, kind_str);
  int dst = vm_insn_extra(insn);
  vm_set(vm->dspl, dst, cdr(binding));
}

static void vm_getbind(struct vm *vm, vm_word_t insn,
                       val (*lookup_fn)(val env, val sym),
                       val kind_str)
{
  val binding = vm_get_binding(vm, insn, lookup_fn, kind_str);
  int dst = vm_insn_extra(insn);
  vm_set(vm->dspl, dst, binding);
}

static void vm_setsym(struct vm *vm, vm_word_t insn,
                      val (*lookup_fn)(val env, val sym),
                      val kind_str)
{
  val binding = vm_get_binding(vm, insn, lookup_fn, kind_str);
  int src = vm_insn_extra(insn);
  rplacd(binding, vm_get(vm->dspl, src));
}

static void vm_bindv(struct vm *vm, vm_word_t insn)
{
  val sym = vm_get(vm->dspl, vm_insn_operand(insn));
  int src = vm_insn_extra(insn);

  if (nilp(dyn_env))
    eval_error(vm->vd->bytecode,
               lit("no environment for dynamic binding"), nao);

  env_vbind(dyn_env, sym, vm_get(vm->dspl, src));
}

static void vm_close(struct vm *vm, vm_word_t insn)
{
  unsigned dst = vm_insn_bigop(insn);
  vm_word_t arg1 = vm->code[vm->ip++];
  vm_word_t arg2 = vm->code[vm->ip++];
  unsigned vari_fr = vm_arg_operand_hi(arg1);
  int variadic = vari_fr & 0x100;
  int frsz = vari_fr & 0xFF;
  unsigned reg = vm_arg_operand_lo(arg1);
  int reqargs = vm_arg_operand_hi(arg2);
  int fixparam = vm_arg_operand_lo(arg2);
  val closure = vm_make_closure(vm, frsz);
  val vf = func_vm(closure, vm->vd->self, fixparam, reqargs, variadic);

  vm_set(vm->dspl, reg, vf);
  vm->ip = dst;
}

static val vm_execute(struct vm *vm)
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
    case DFRAME:
      vm_dframe(vm, insn);
      break;
    case END:
      return vm_end(vm, insn);
    case FIN:
      return vm_fin(vm, insn);
    case CALL:
      vm_call(vm, insn);
      break;
    case APPLY:
      vm_apply(vm, insn);
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
    case CATCH:
      vm_catch(vm, insn);
      break;
    case HANDLE:
      vm_handle(vm, insn);
      break;
    case GETV:
      vm_getsym(vm, insn, lookup_var, lit("variable"));
      break;
    case GETF:
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
    default:
      uw_throwf(error_s, lit("invalid opcode ~s"), num_fast(opcode), nao);
    }
  }
}

val vm_execute_toplevel(val desc)
{
  struct vm_desc *vd = vm_desc_struct(desc);
  struct vm vm;
  val *frame = coerce(val *, alloca(sizeof *frame * vd->frsz));
  struct vm_env *dspl = coerce(struct vm_env *, frame + vd->nreg);

  vm_reset(&vm, vd, dspl, 1, 0);

  vm.dspl = coerce(struct vm_env *, frame + vd->nreg);

  frame[0] = nil;

  vm.dspl[0].mem = frame;
  vm.dspl[0].vec = nil;

  vm.dspl[1].mem = vd->data;
  vm.dspl[0].vec = vd->datavec;

  return vm_execute(&vm);
}

val vm_execute_closure(val fun, struct args *args)
{
  val closure = fun->f.env;
  val desc = fun->f.f.vm_desc;
  int fixparam = fun->f.fixparam;
  int variadic = fun->f.variadic;
  int nargs = fixparam + variadic;
  struct vm_desc *vd = vm_desc_struct(desc);
  struct vm_closure *vc = coerce(struct vm_closure *, closure->co.handle);
  struct vm vm;
  val *frame = coerce(val *, alloca(sizeof *frame * vd->frsz));
  struct vm_env *dspl = coerce(struct vm_env *, frame + vd->nreg);
  val vargs = if3(variadic, args_get_rest(args, nargs), nil);
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

  while (nargs >= 2) {
    nargs -= 2;
    argw = vm.code[vm.ip++];
    unsigned xreg = vm_arg_operand_lo(argw);
    unsigned yreg = vm_arg_operand_hi(argw);
    vm_set(dspl, xreg, args_get(args, &ix));
    vm_set(dspl, yreg, args_get(args, &ix));
  }

  if (nargs) {
    argw = vm.code[vm.ip++];
    unsigned xreg = vm_arg_operand_lo(argw);
    vm_set(dspl, xreg, args_get(args, &ix));
  }

  if (variadic) {
    unsigned vreg;
    if (!nargs) {
      argw = vm.code[vm.ip++];
      vreg = vm_arg_operand_lo(argw);
    } else {
      vreg = vm_arg_operand_hi(argw);
    }

    vm_set(dspl, vreg, vargs);
  }

  return vm_execute(&vm);
}

static_def(struct cobj_ops vm_desc_ops =
  cobj_ops_init(eq,
                cobj_print_op,
                cobj_destroy_free_op,
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
  reg_fun(intern(lit("vm-make-desc"), system_package), func_n4(vm_make_desc));
  reg_fun(intern(lit("vm-desc-bytecode"), system_package), func_n1(vm_desc_bytecode));
  reg_fun(intern(lit("vm-interpret-toplevel"), system_package), func_n1(vm_execute_toplevel));
}
