/* Copyright 2009-2021
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

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <wctype.h>
#include <limits.h>
#include <stdarg.h>
#include <errno.h>
#include <wchar.h>
#include <math.h>
#include <signal.h>
#include <assert.h>
#include <locale.h>
#include "config.h"
#include "alloca.h"
#if HAVE_GETENVIRONMENTSTRINGS
#define NOMINMAX
#include <windows.h>
#endif
#if HAVE_MALLOC_H
#include <malloc.h>
#endif
#include "lib.h"
#include "gc.h"
#include "arith.h"
#include "rand.h"
#include "hash.h"
#include "tree.h"
#include "signal.h"
#include "unwind.h"
#include "args.h"
#include "stream.h"
#include "strudel.h"
#include "utf8.h"
#include "filter.h"
#include "eval.h"
#include "vm.h"
#include "sysif.h"
#include "time.h"
#include "regex.h"
#include "parser.h"
#include "syslog.h"
#include "glob.h"
#include "ftw.h"
#include "termios.h"
#include "cadr.h"
#include "struct.h"
#include "itypes.h"
#include "buf.h"
#include "ffi.h"
#include "chksum.h"
#include "txr.h"
#include "debug.h"

#define max(a, b) ((a) > (b) ? (a) : (b))
#define min(a, b) ((a) < (b) ? (a) : (b))
#define nelem(array) (sizeof (array) / sizeof (array)[0])

#if !HAVE_POSIX_SIGS
int async_sig_enabled = 0;
#endif

val packages;

val system_package, keyword_package, user_package;
val public_package, package_alist_s;
val package_s, system_package_s, keyword_package_s, user_package_s;

val null_s, t, cons_s, str_s, chr_s, fixnum_s, sym_s, pkg_s, fun_s, vec_s;
val lit_s, stream_s, hash_s, hash_iter_s, lcons_s, lstr_s, cobj_s, cptr_s;
val atom_s, integer_s, number_s, sequence_s, string_s;
val env_s, bignum_s, float_s, range_s, rcons_s, buf_s, tnode_s, args_s;
val var_s, expr_s, regex_s, chset_s, set_s, cset_s, wild_s, oneplus_s;
val nongreedy_s;
val quote_s, qquote_s, unquote_s, splice_s;
val sys_qquote_s, sys_unquote_s, sys_splice_s;
val zeroplus_s, optional_s, compl_s, compound_s;
val or_s, and_s, quasi_s, quasilist_s;
val skip_s, trailer_s, block_s, next_s, freeform_s, fail_s, accept_s;
val all_s, some_s, none_s, maybe_s, cases_s, collect_s, until_s, coll_s;
val define_s, output_s, single_s, first_s, last_s, empty_s;
val repeat_s, rep_s, flatten_s, forget_s;
val local_s, merge_s, bind_s, rebind_s, cat_s;
val try_s, catch_s, finally_s, throw_s, defex_s, deffilter_s;
val eof_s, eol_s, assert_s, name_s;
val error_s, type_error_s, internal_error_s, panic_s;
val numeric_error_s, range_error_s;
val query_error_s, file_error_s, process_error_s, syntax_error_s;
val timeout_error_s, system_error_s, alloc_error_s, stack_overflow_s;
val path_not_found_s, path_exists_s, path_permission_s;
val warning_s, defr_warning_s, restart_s, continue_s;
val gensym_counter_s, length_s;
val rplaca_s, rplacd_s, seq_iter_s;

val nothrow_k, args_k, colon_k, auto_k, fun_k;
val wrap_k, reflect_k;

val null_string;
val nil_string;
val null_list;

val identity_f, identity_star_f;
val equal_f, eql_f, eq_f, car_f, cdr_f, null_f;
val list_f, less_f, greater_f;

val prog_string;

#if CONFIG_LOCALE_TOLERANCE
char dec_point = '.';
#endif

static struct cobj_class cobj_class[64], *cobj_ptr = cobj_class;

static val cobj_hash;

struct cobj_class *seq_iter_cls;

static val recycled_conses;

const seq_kind_t seq_kind_tab[MAXTYPE+1] = {
  SEQ_NIL,      /* NIL */
  SEQ_NOTSEQ,   /* NUM */
  SEQ_NOTSEQ,   /* CHR */
  SEQ_VECLIKE,  /* LIT */
  SEQ_LISTLIKE, /* CONS */
  SEQ_VECLIKE,  /* STR */
  SEQ_NOTSEQ,   /* SYM */
  SEQ_NOTSEQ,   /* PKG */
  SEQ_NOTSEQ,   /* FUN */
  SEQ_VECLIKE,  /* VEC */
  SEQ_LISTLIKE, /* LCONS */
  SEQ_VECLIKE,  /* LSTR */
  SEQ_NOTSEQ,   /* COBJ */
  SEQ_NOTSEQ,   /* CPTR */
  SEQ_NOTSEQ,   /* ENV */
  SEQ_NOTSEQ,   /* BGNUM */
  SEQ_NOTSEQ,   /* FLNUM */
  SEQ_NOTSEQ,   /* RNG */
  SEQ_VECLIKE,  /* BUF */
  SEQ_NOTSEQ,   /* TNOD */
  SEQ_NOTSEQ,   /* DARG */
};

val identity(val obj)
{
  return obj;
}

static val identity_star(varg args)
{
  cnum index = 0;
  if (args_more(args, index))
    return args_get(args, &index);
  return nil;
}

static val code2type(int code)
{
  switch (convert(type_t, code)) {
  case NIL: return null_s;
  case CONS: return cons_s;
  case STR: return str_s;
  case LIT: return lit_s;
  case CHR: return chr_s;
  case NUM: return fixnum_s;
  case SYM: return sym_s;
  case PKG: return pkg_s;
  case FUN: return fun_s;
  case VEC: return vec_s;
  case LCONS: return lcons_s;
  case LSTR: return lstr_s;
  case COBJ: return cobj_s;
  case CPTR: return cptr_s;
  case ENV: return env_s;
  case BGNUM: return bignum_s;
  case FLNUM: return float_s;
  case RNG: return range_s;
  case BUF: return buf_s;
  case TNOD: return tnode_s;
  case DARG: return args_s;
  }
  return nil;
}

val built_in_type_p(val sym)
{
  int i;

  for (i = NIL; i <= MAXTYPE; i++) {
    val type = code2type(i);
    if (subtypep(type, sym))
      return t;
  }

  if (gethash(cobj_hash, sym))
    return t;

  return nil;
}

val typeof(val obj)
{
  switch (tag(obj)) {
  case TAG_NUM:
    return fixnum_s;
  case TAG_CHR:
    return chr_s;
  case TAG_LIT:
    return lit_s;
  case TAG_PTR:
    {
      int typecode = type(obj);

      if (typecode == COBJ) {
        if (obj->co.cls == struct_cls)
          return struct_type_name(obj);
        else
          return obj->co.cls->cls_sym;
      } else {
        val typesym = code2type(typecode);
        if (!typesym)
          internal_error("corrupt type field");
        return typesym;
      }
    }
  default:
    internal_error("invalid type tag");
  }
}

static struct cobj_class *class_from_sym(val cls_sym)
{
  val idx = gethash(cobj_hash, cls_sym);
  return idx ? cobj_class + c_n(idx) : 0;
}

val subtypep(val sub, val sup)
{
  if (sub == sup) {
    return t;
  } else if (sub == nil || sup == t) {
    return t;
  } else if (sup == atom_s) {
    return tnil(sub != cons_s && sub != lcons_s);
  } else if (sup == integer_s) {
    return tnil(sub == fixnum_s || sub == bignum_s);
  } else if (sup == number_s) {
    return tnil(sub == fixnum_s || sub == bignum_s ||
                sub == integer_s || sub == float_s);
  } else if (sup == cons_s) {
    return tnil(sub == lcons_s);
  } else if (sup == sym_s) {
    return tnil(sub == null_s);
  } else if (sup == list_s) {
    return tnil(sub == null_s || sub == cons_s || sub == lcons_s);
  } else if (sup == sequence_s) {
    val sub_struct = find_struct_type(sub);
    if (sub_struct) {
      if (get_special_slot_by_type(sub_struct, length_m) ||
          get_special_slot_by_type(sub_struct, car_m))
        return t;
      return nil;
    }
    return tnil(sub == str_s || sub == lit_s || sub == lstr_s ||
                sub == vec_s || sub == null_s || sub == cons_s ||
                sub == lcons_s || sub == list_s || sub == string_s);
  } else if (sup == string_s) {
    return tnil(sub == str_s || sub == lit_s || sub == lstr_s);
  } else if (sup == struct_s) {
    return tnil(find_struct_type(sub));
  } else {
    {
      val sub_struct = find_struct_type(sub);
      val sup_struct = find_struct_type(sup);

      if (sub_struct && sup_struct)
        return struct_subtype_p(sub_struct, sup_struct);
    }

    {
      struct cobj_class *sub_cls = class_from_sym(sub);
      struct cobj_class *sup_cls = class_from_sym(sup);

      if (sub_cls && sup_cls) {
        struct cobj_class *pcls = sub_cls;
        do {
          if (pcls == sup_cls)
            return t;
          pcls = pcls->super;
        } while (pcls);
      }
    }

    return nil;
  }
}

val typep(val obj, val type)
{
  return subtypep(typeof(obj), type);
}

seq_info_t seq_info(val obj)
{
  seq_info_t ret;
  type_t to = type(obj);

  ret.obj = obj;
  ret.type = to;
  ret.kind = SEQ_NOTSEQ;

  if (to != COBJ) {
    ret.kind = seq_kind_tab[to];
    return ret;
  } else {
    val cls = obj->co.cls->cls_sym;

    if (cls == hash_s) {
      ret.kind = SEQ_HASHLIKE;
    } else if (cls == carray_s) {
      ret.kind = SEQ_VECLIKE;
    } else if (obj_struct_p(obj)) {
      val sub = obj;
      val nullify_meth = get_special_slot(obj, nullify_m);

      if (nullify_meth) {
        sub = funcall1(nullify_meth, obj);

        if (sub != obj) {
          if (!sub)
            ret.obj = nil;
          return seq_info(sub);
        }
      }

      if (get_special_slot(obj, length_m))
        ret.kind = SEQ_VECLIKE;
      else if (get_special_slot(obj, car_m))
        ret.kind = SEQ_LISTLIKE;
    } else if (cls == tree_s) {
      ret.kind = SEQ_TREELIKE;
    }
  }

  return ret;
}

static val seq_iterable(seq_info_t si)
{
  if (si.kind != SEQ_NOTSEQ)
    return t;

  switch (si.type) {
  case RNG:
    {
      val rf = from(si.obj);

      switch (type(rf)) {
      case NUM:
      case CHR:
      case BGNUM:
        return t;
      default:
        break;
      }
    }
    break;
  case CHR:
  case NUM:
  case BGNUM:
  case FLNUM:
    return t;
  case COBJ:
    if (obj_struct_p(si.obj) && get_special_slot(si.obj, iter_begin_m))
      return t;
    break;
  default:
    break;
  }

  return nil;
}

static void NORETURN unsup_obj(val self, val obj)
{
  uw_throwf(error_s, lit("~a: unsupported object ~s"), self, obj, nao);
  abort();
}

static int seq_iter_get_nil(seq_iter_t *it, val *pval)
{
  (void) it;
  (void) pval;
  return 0;
}

static int seq_iter_peek_nil(seq_iter_t *it, val *pval)
{
  (void) it;
  (void) pval;
  return 0;
}

static int seq_iter_get_list(seq_iter_t *it, val *pval)
{
  if (!it->ui.iter)
    return 0;
  *pval = car(it->ui.iter);
  it->ui.iter = cdr(it->ui.iter);
  return 1;
}

static int seq_iter_peek_list(seq_iter_t *it, val *pval)
{
  if (!it->ui.iter)
    return 0;
  *pval = car(it->ui.iter);
  return 1;
}

static int seq_iter_get_vec(seq_iter_t *it, val *pval)
{
  if (it->ui.index < it->ul.len) {
    *pval = ref(it->inf.obj, num(it->ui.index++));
    return 1;
  }
  return 0;
}

static int seq_iter_peek_vec(seq_iter_t *it, val *pval)
{
  if (it->ui.index < it->ul.len) {
    *pval = ref(it->inf.obj, num(it->ui.index));
    return 1;
  }
  return 0;
}

static int seq_iter_get_hash(seq_iter_t *it, val *pval)
{
  *pval = hash_next(it->ui.iter);
  return *pval != nil;
}

static int seq_iter_peek_hash(seq_iter_t *it, val *pval)
{
  *pval = hash_peek(it->ui.iter);
  return *pval != nil;
}

static int seq_iter_get_tree(seq_iter_t *it, val *pval)
{
  val node = tree_next(it->ui.iter);
  *pval = if2(node, key(node));
  return node != nil;
}

static int seq_iter_peek_tree(seq_iter_t *it, val *pval)
{
  val node = tree_peek(it->ui.iter);
  *pval = if2(node, key(node));
  return node != nil;
}

static int seq_iter_get_range_cnum(seq_iter_t *it, val *pval)
{
  if (it->ui.cn < it->ul.cbound) {
    *pval = num(it->ui.index++);
    return 1;
  }
  return 0;
}

static int seq_iter_peek_range_cnum(seq_iter_t *it, val *pval)
{
  if (it->ui.cn < it->ul.cbound) {
    *pval = num(it->ui.index);
    return 1;
  }
  return 0;
}

static int seq_iter_get_range_chr(seq_iter_t *it, val *pval)
{
  if (it->ui.cn < it->ul.cbound) {
    *pval = chr(it->ui.index++);
    return 1;
  }
  return 0;
}

static int seq_iter_peek_range_chr(seq_iter_t *it, val *pval)
{
  if (it->ui.cn < it->ul.cbound) {
    *pval = chr(it->ui.index);
    return 1;
  }
  return 0;
}

static int seq_iter_get_range_bignum(seq_iter_t *it, val *pval)
{
  if (lt(it->ui.vn, it->ul.vbound)) {
    *pval = it->ui.iter;
    it->ui.iter = succ(it->ui.iter);
    return 1;
  }
  return 0;
}

static int seq_iter_peek_range_bignum(seq_iter_t *it, val *pval)
{
  if (lt(it->ui.vn, it->ul.vbound)) {
    *pval = it->ui.iter;
    return 1;
  }
  return 0;
}

static int seq_iter_get_rev_range_cnum(seq_iter_t *it, val *pval)
{
  if (it->ui.cn > it->ul.cbound) {
    *pval = num(--it->ui.index);
    return 1;
  }
  return 0;
}

static int seq_iter_peek_rev_range_cnum(seq_iter_t *it, val *pval)
{
  if (it->ui.cn > it->ul.cbound) {
    *pval = num(it->ui.index - 1);
    return 1;
  }
  return 0;
}

static int seq_iter_get_rev_range_chr(seq_iter_t *it, val *pval)
{
  if (it->ui.cn > it->ul.cbound) {
    *pval = chr(--it->ui.index);
    return 1;
  }
  return 0;
}

static int seq_iter_peek_rev_range_chr(seq_iter_t *it, val *pval)
{
  if (it->ui.cn > it->ul.cbound) {
    *pval = chr(it->ui.index - 1);
    return 1;
  }
  return 0;
}

static int seq_iter_get_rev_range_bignum(seq_iter_t *it, val *pval)
{
  if (gt(it->ui.vn, it->ul.vbound)) {
    *pval = it->ui.iter = pred(it->ui.iter);
    return 1;
  }
  return 0;
}

static int seq_iter_peek_rev_range_bignum(seq_iter_t *it, val *pval)
{
  if (gt(it->ui.vn, it->ul.vbound)) {
    *pval = pred(it->ui.iter);
    return 1;
  }
  return 0;
}


static int seq_iter_get_chr(seq_iter_t *it, val *pval)
{
  if (it->ui.index <= 0x10FFFF) {
    *pval = chr(it->ui.index++);
    return 1;
  }
  return 0;
}

static int seq_iter_peek_chr(seq_iter_t *it, val *pval)
{
  if (it->ui.index <= 0x10FFFF) {
    *pval = chr(it->ui.index);
    return 1;
  }
  return 0;
}

static int seq_iter_get_num(seq_iter_t *it, val *pval)
{
  *pval = it->ui.iter;
  it->ui.iter = succ(it->ui.iter);
  return 1;
}

static int seq_iter_peek_num(seq_iter_t *it, val *pval)
{
  *pval = it->ui.iter;
  return 1;
}

static int seq_iter_get_oop(seq_iter_t *it, val *pval)
{
  val iter = it->ui.iter;

  /* The assignments to ui.iter and ui.next are wrong if the it structure is
   * embedded inside a heap-allocated iterator object. The object could be a
   * gen 1 object, whereas the value being assigned could be a gen 0.
   *
   * The only way this can happen is if the obsolescent seq-begin function is
   * used on an object that supports the iter-begin method. The seq-begin
   * constructor is the only function which creates a heap-allocated iterator
   * which is initialized via seq_iter_init_with_info, which binds this
   * seq_iter_get_oop function and its sisters.
   *
   * The other heap-allocated iterator type is iter-begin. iter-begin applies
   * its own handling for OOP iterators; it doesn't set up seq_iter_t for
   * objects of that type, and so these functions are not used.
   */
  if (it->ul.next != nao) {
    val iter_step_meth = get_special_required_slot(iter, iter_step_m);
    *pval = it->ul.next;
    it->ui.iter = funcall1(iter_step_meth, iter);
    it->ul.next = nao;
    return 1;
  } else {
    val iter_more_meth = get_special_required_slot(iter, iter_more_m);

    if (!funcall1(iter_more_meth, iter)) {
      return 0;
    } else {
      val iter_item_meth = get_special_required_slot(iter, iter_item_m);
      val iter_step_meth = get_special_required_slot(iter, iter_step_m);
      *pval = funcall1(iter_item_meth, iter);
      it->ui.iter = funcall1(iter_step_meth, iter);
      it->ul.next = nao;
      return 1;
    }
  }
}

static int seq_iter_peek_oop(seq_iter_t *it, val *pval)
{
  val iter = it->ui.iter;

  /* See comment in seq_iter_get_oop */

  if (it->ul.next != nao) {
    *pval = it->ul.next;
    return 1;
  } else {
    val iter_more_meth = get_special_required_slot(iter, iter_more_m);

    if (funcall1(iter_more_meth, iter)) {
      val iter_item_meth = get_special_required_slot(iter, iter_item_m);
      it->ul.next = *pval = funcall1(iter_item_meth, iter);
    }

    return 1;
  }

  return 0;
}

static int seq_iter_get_fast_oop(seq_iter_t *it, val *pval)
{
  val iter = it->ui.iter;

  if (iter) {
    val item = it->ul.next;
    val iter_step_meth = get_special_required_slot(iter, iter_step_m);

    if (item == nao) {
      val iter_item_meth = get_special_required_slot(iter, iter_item_m);
      *pval = funcall1(iter_item_meth, iter);
    } else {
      *pval = item;
    }

    /* See comment in seq_iter_get_oop */

    it->ui.iter = funcall1(iter_step_meth, iter);
    it->ul.next = nao;
    return 1;
  }

  return 0;
}

static int seq_iter_peek_fast_oop(seq_iter_t *it, val *pval)
{
  val iter = it->ui.iter;

  if (it->ul.next != nao) {
    *pval = it->ul.next;
    return 1;
  }

  /* See comment in seq_iter_get_oop */

  if (iter) {
    val iter_item_meth = get_special_required_slot(iter, iter_item_m);
    it->ul.next = *pval = funcall1(iter_item_meth, iter);
    return 1;
  }

  return 0;
}

val seq_geti(seq_iter_t *it)
{
  val v = nil;
  (void) it->get(it, &v);
  return v;
}

static void seq_iter_rewind(seq_iter_t *it, val self)
{
  switch (it->inf.type) {
  case RNG:
    {
      val rf = from(it->inf.obj);

      switch (type(rf)) {
      case NUM:
        it->ui.cn = c_num(rf, self);
        break;
      case CHR:
        it->ui.cn = c_chr(rf);
        break;
      case BGNUM:
        it->ui.vn = rf;
        break;
      default:
        break;
      }
    }
    break;
  case CHR:
    it->ui.cn = c_chr(it->inf.obj);
    break;
  case NUM:
  case BGNUM:
  case FLNUM:
    it->ui.vn = it->inf.obj;
    break;
  default:
    switch (it->inf.kind) {
    case SEQ_NIL:
      it->ui.iter = nil;
      break;
    case SEQ_LISTLIKE:
      it->ui.iter = it->inf.obj;
      break;
    case SEQ_VECLIKE:
      it->ui.index = 0;
      break;
    case SEQ_HASHLIKE:
      it->ui.iter = hash_reset(it->ui.iter, it->inf.obj);
      break;
    case SEQ_TREELIKE:
      it->ui.iter = tree_reset(it->ui.iter, it->inf.obj, colon_k, colon_k);
      break;
    default:
      break;
    }
  }
}

void seq_iter_init_with_info(val self, seq_iter_t *it,
                             seq_info_t si, int support_rewind)
{
  it->inf = si;

  switch (it->inf.type) {
  case RNG:
    {
      val rf = from(it->inf.obj);
      val rt = to(it->inf.obj);

      if (rt == colon_k || rt == t) {
        seq_iter_init_with_info(self, it, seq_info(rf), support_rewind);
        break;
      }

      if (lt(rf, rt)) switch (type(rf)) {
      case NUM:
        it->ui.cn = c_num(rf, self);
        it->ul.cbound = c_num(rt, self);
        it->get = seq_iter_get_range_cnum;
        it->peek = seq_iter_peek_range_cnum;
        break;
      case CHR:
        it->ui.cn = c_chr(rf);
        it->ul.cbound = c_chr(rt);
        it->get = seq_iter_get_range_chr;
        it->peek = seq_iter_peek_range_chr;
        break;
      case BGNUM:
        it->ui.vn = rf;
        it->ul.vbound = rt;
        it->get = seq_iter_get_range_bignum;
        it->peek = seq_iter_peek_range_bignum;
        break;
      default:
        unsup_obj(self, it->inf.obj);
      } else if (gt(rf, rt)) switch (type(rf)) {
      case NUM:
        it->ui.cn = c_num(rf, self);
        it->ul.cbound = c_num(rt, self);
        it->get = seq_iter_get_rev_range_cnum;
        it->peek = seq_iter_peek_rev_range_cnum;
        break;
      case CHR:
        it->ui.cn = c_chr(rf);
        it->ul.cbound = c_chr(rt);
        it->get = seq_iter_get_rev_range_chr;
        it->peek = seq_iter_peek_rev_range_chr;
        break;
      case BGNUM:
        it->ui.vn = rf;
        it->ul.vbound = rt;
        it->get = seq_iter_get_rev_range_bignum;
        it->peek = seq_iter_peek_rev_range_bignum;
        break;
      default:
        unsup_obj(self, it->inf.obj);
      } else {
        seq_iter_init_with_info(self, it, seq_info(nil), support_rewind);
        break;
      }
    }
    break;
  case CHR:
    it->ui.cn = c_chr(it->inf.obj);
    it->ul.cbound = 0;
    it->get = seq_iter_get_chr;
    it->peek = seq_iter_peek_chr;
    break;
  case NUM:
  case BGNUM:
  case FLNUM:
    it->ui.vn = it->inf.obj;
    it->ul.vbound = nil;
    it->get = seq_iter_get_num;
    it->peek = seq_iter_peek_num;
    break;
  case COBJ:
    if (obj_struct_p(it->inf.obj)) {
      val iter_begin_meth = get_special_slot(it->inf.obj, iter_begin_m);
      if (iter_begin_meth) {
        val iter = funcall1(iter_begin_meth, it->inf.obj);
        if (iter == nil) {
          it->ui.iter = nil;
          it->ul.len = 0;
          it->get = seq_iter_get_nil;
          it->peek = seq_iter_peek_nil;
          break;
        } else {
          val iter_more_meth = get_special_slot(iter, iter_more_m);
          if (iter_more_meth) {
            it->ui.iter = iter;
            it->ul.next = nao;
            it->get = seq_iter_get_oop;
            it->peek = seq_iter_peek_oop;
          } else {
            it->ui.iter = iter;
            it->ul.next = nao;
            it->get = seq_iter_get_fast_oop;
            it->peek = seq_iter_peek_fast_oop;
          }
          break;
        }
      }
    }
    if (it->inf.obj->co.cls == tree_iter_cls)
    {
      it->ui.iter = if3(support_rewind,
                        copy_tree_iter(it->inf.obj),
                        it->inf.obj);
      it->ul.len = 0;
      it->get = seq_iter_get_tree;
      it->peek = seq_iter_peek_tree;
      break;
    }
    /* fallthrough */
  default:
    switch (it->inf.kind) {
    case SEQ_NIL:
      it->ui.iter = nil;
      it->ul.len = 0;
      it->get = seq_iter_get_nil;
      it->peek = seq_iter_peek_nil;
      break;
    case SEQ_LISTLIKE:
      it->ui.iter = it->inf.obj;
      it->ul.len = 0;
      it->get = seq_iter_get_list;
      it->peek = seq_iter_peek_list;
      if (!support_rewind)
        it->inf.obj = nil;
      break;
    case SEQ_VECLIKE:
      it->ui.index = 0;
      it->ul.len = c_num(length(it->inf.obj), self);
      it->get = seq_iter_get_vec;
      it->peek = seq_iter_peek_vec;
      break;
    case SEQ_HASHLIKE:
      it->ui.iter = hash_begin(it->inf.obj);
      it->ul.len = 0;
      it->get = seq_iter_get_hash;
      it->peek = seq_iter_peek_hash;
      break;
    case SEQ_TREELIKE:
      it->ui.iter = tree_begin(it->inf.obj, colon_k, colon_k);
      it->ul.len = 0;
      it->get = seq_iter_get_tree;
      it->peek = seq_iter_peek_tree;
      break;
    default:
      unsup_obj(self, it->inf.obj);
    }
  }
}

void seq_iter_init(val self, seq_iter_t *it, val obj)
{
  seq_iter_init_with_info(self, it, seq_info(obj), 0);
}

static void seq_iter_init_with_rewind(val self, seq_iter_t *it, val obj)
{
  seq_iter_init_with_info(self, it, seq_info(obj), 1);
}

val seq_getpos(val self, seq_iter_t *it)
{
  switch (it->inf.kind) {
  case SEQ_NIL:
  case SEQ_LISTLIKE:
    return it->ui.iter;
  case SEQ_VECLIKE:
    return num(it->ui.index);
  default:
    unsup_obj(self, it->inf.obj);
  }
}

void seq_setpos(val self, seq_iter_t *it, val pos)
{
  switch (it->inf.kind) {
  case SEQ_NIL:
  case SEQ_LISTLIKE:
    it->ui.iter = pos;
    break;
  case SEQ_VECLIKE:
    it->ui.index = c_num(pos, self);
    break;
  default:
    unsup_obj(self, it->inf.obj);
  }
}

static void seq_iter_mark(val seq_iter)
{
  struct seq_iter *si = coerce(struct seq_iter *, seq_iter->co.handle);

  gc_mark(si->inf.obj);

  switch (si->inf.kind) {
  case SEQ_LISTLIKE:
  case SEQ_HASHLIKE:
  case SEQ_TREELIKE:
    gc_mark(si->ui.iter);
    break;
  case SEQ_NOTSEQ:
    if (cobjp(si->inf.obj) && obj_struct_p(si->inf.obj))
      gc_mark(si->ui.iter);
    break;
  case SEQ_NIL:
  case SEQ_VECLIKE:
    break;
  }
}

static struct cobj_ops seq_iter_ops = cobj_ops_init(eq,
                                                    cobj_print_op,
                                                    cobj_destroy_free_op,
                                                    seq_iter_mark,
                                                    cobj_eq_hash_op);

val seq_begin(val obj)
{
  val self = lit("seq-begin");
  val si_obj;
  struct seq_iter *si = coerce(struct seq_iter *, chk_calloc(1, sizeof *si));
  si_obj = cobj(coerce(mem_t *, si), seq_iter_cls, &seq_iter_ops);
  seq_iter_init(self, si, obj);
  return si_obj;
}

val seq_next(val iter, val end_val)
{
  val self = lit("seq-next");
  struct seq_iter *si = coerce(struct seq_iter *,
                               cobj_handle(self, iter, seq_iter_cls));
  val item = nil;
  return if3(seq_get(si, &item), item, end_val);
}

val seq_reset(val iter, val obj)
{
  val self = lit("seq-reset");
  struct seq_iter *si = coerce(struct seq_iter *,
                               cobj_handle(self, iter, seq_iter_cls));
  seq_iter_init(self, si, obj);
  return iter;
}

val iter_begin(val obj)
{
  val self = lit("iter-begin");
  seq_info_t sinf = seq_info(obj);

  switch (sinf.type) {
  case CHR:
  case NUM:
  case BGNUM:
    return obj;
  case COBJ:
    if (obj_struct_p(obj)) {
      val iter_begin_meth = get_special_slot(obj, iter_begin_m);
      if (iter_begin_meth)
        return funcall1(iter_begin_meth, obj);
    }
    /* fallthrough */
  default:
    switch (sinf.kind) {
    case SEQ_NIL:
    case SEQ_LISTLIKE:
      return sinf.obj;
    default:
      {
        val si_obj;
        struct seq_iter *si = coerce(struct seq_iter *,
                                     chk_calloc(1, sizeof *si));
        si_obj = cobj(coerce(mem_t *, si), seq_iter_cls, &seq_iter_ops);
        seq_iter_init_with_info(self, si, sinf, 0);
        return si_obj;
      }
    }
  }
}

val iter_more(val iter)
{
  switch (type(iter)) {
  case NIL:
    return nil;
  case CHR:
    return if2(c_chr(iter) <= 0x10FFFF, t);
  case NUM:
  case BGNUM:
    return t;
  case COBJ:
    if (iter->co.cls == seq_iter_cls)
    {
      struct seq_iter *si = coerce(struct seq_iter *, iter->co.handle);
      val item = nil;
      return if2(seq_peek(si, &item), t);
    }
    if (obj_struct_p(iter)) {
      val iter_more_meth = get_special_slot(iter, iter_more_m);
      if (iter_more_meth)
        return funcall1(iter_more_meth, iter);
    }
    /* fallthrough */
  default:
    return t;
  }
}

val iter_item(val iter)
{
  switch (type(iter)) {
  case NIL:
    return nil;
  case CHR:
  case NUM:
  case BGNUM:
    return iter;
  case COBJ:
    if (iter->co.cls == seq_iter_cls)
    {
      struct seq_iter *si = coerce(struct seq_iter *, iter->co.handle);
      val item = nil;
      return if2(seq_peek(si, &item), item);
    }
    if (obj_struct_p(iter)) {
      val iter_item_meth = get_special_slot(iter, iter_item_m);
      if (iter_item_meth)
        return funcall1(iter_item_meth, iter);
    }
    /* fallthrough */
  default:
    return car(iter);
  }
}

val iter_step(val iter)
{
  val self = lit("iter-step");

  switch (type(iter)) {
  case NIL:
    return nil;
  case CHR:
  case NUM:
  case BGNUM:
    return plus(iter, one);
  case CONS:
  case LCONS:
    {
      val next = cdr(iter);
      if (next && !consp(next))
        uw_throwf(error_s, lit("~a: ~s is not a cons"), self, next, nao);
      return next;
    }
  case COBJ:
    if (iter->co.cls == seq_iter_cls)
    {
      struct seq_iter *si = coerce(struct seq_iter *, iter->co.handle);
      val item = nil;
      (void) seq_get(si, &item);
      return iter;
    }
    if (obj_struct_p(iter)) {
      val iter_step_meth = get_special_slot(iter, iter_step_m);
      if (iter_step_meth)
        return funcall1(iter_step_meth, iter);
    }
    /* fallthrough */
  default:
    {
      val next = cdr(iter);
      if (next) {
        seq_info_t sinf = seq_info(next);
        if (sinf.kind != SEQ_LISTLIKE)
          uw_throwf(error_s, lit("~a: ~s is improperly terminated"),
                    self, iter, nao);
      }
      return next;
    }
  }
}

val iter_reset(val iter, val obj)
{
  val self = lit("iter-reset");
  seq_info_t sinf = seq_info(obj);

  switch (type(iter)) {
  case CHR:
  case NUM:
  case BGNUM:
    return obj;
  case COBJ:
    if (iter->co.cls == seq_iter_cls)
    {
      struct seq_iter *si = coerce(struct seq_iter *, iter->co.handle);
      seq_iter_init_with_info(self, si, sinf, 0);
      return iter;
    }
    /* fallthrough */
  default:
    if (cobjp(obj) && obj_struct_p(obj)) {
      val iter_reset_meth = get_special_slot(obj, iter_reset_m);
      if (iter_reset_meth)
        return funcall2(iter_reset_meth, obj, iter);
    }
    switch (sinf.kind) {
    case SEQ_NIL:
    case SEQ_LISTLIKE:
      return sinf.obj;
    default:
      return iter_begin(obj);
    }
  }
}

val throw_mismatch(val self, val obj, type_t t)
{
  type_mismatch(lit("~a: ~s is not of type ~s"), self, obj, code2type(t), nao);
}

val class_check(val self, val cobj, struct cobj_class *cls)
{
  type_assert (cobjclassp(cobj, cls),
               (lit("~a: ~s is not of type ~s"), self, cobj, cls->cls_sym, nao));
  return t;
}

val car(val cons)
{
  switch (type(cons)) {
  case NIL:
    return nil;
  case CONS:
    return cons->c.car;
  case LCONS:
    if (cons->lc.func == nil) {
      return cons->lc.car;
    } else {
      sig_check_fast();
      funcall1(cons->lc.func, cons);
      cons->lc.func = nil;
      return cons->lc.car;
    }
  case VEC:
    if (zerop(cons->v.vec[vec_length]))
      return nil;
    return cons->v.vec[0];
  case STR:
  case LIT:
  case LSTR:
    if (zerop(length_str(cons)))
      return nil;
    return chr_str(cons, zero);
  case BUF:
    if (zerop(length_buf(cons)))
      return nil;
    return buf_get_uchar(cons, zero);
  case COBJ:
    if (obj_struct_p(cons)) {
      {
        val car_meth = get_special_slot(cons, car_m);
        if (car_meth)
          return funcall1(car_meth, cons);
      }
      {
        val lambda_meth = get_special_slot(cons, lambda_m);
        if (lambda_meth)
          return funcall2(lambda_meth, cons, zero);
      }
    }
    /* fallthrough */
  default:
    type_mismatch(lit("car: ~s is not a cons"), cons, nao);
  }
}

val cdr(val cons)
{
  switch (type(cons)) {
  case NIL:
    return nil;
  case CONS:
    return cons->c.cdr;
  case LCONS:
    if (cons->lc.func == nil) {
      return cons->lc.cdr;
    } else {
      sig_check_fast();
      funcall1(cons->lc.func, cons);
      cons->lc.func = nil;
      return cons->lc.cdr;
    }
  case VEC:
  case STR:
  case LIT:
  case LSTR:
    if (le(length(cons), one))
      return nil;
    return sub(cons, one, t);
  case BUF:
    if (le(length_buf(cons), one))
      return nil;
    return sub_buf(cons, one, nil);
  case COBJ:
    if (obj_struct_p(cons)) {
      {
        val cdr_meth = get_special_slot(cons, cdr_m);
        if (cdr_meth)
          return funcall1(cdr_meth, cons);
      }
      {
        val lambda_meth = get_special_slot(cons, lambda_m);
        if (lambda_meth)
          return funcall2(lambda_meth, cons, rcons(one, t));
      }
    }
    /* fallthrough */
  default:
    type_mismatch(lit("cdr: ~s is not a cons"), cons, nao);
  }
}

val rplaca(val cons, val new_car)
{
  switch (type(cons)) {
  case CONS:
    set(mkloc(cons->c.car, cons), new_car);
    return cons;
  case LCONS:
    set(mkloc(cons->lc.car, cons), new_car);
    return cons;
  case VEC:
  case STR:
  case LSTR:
    refset(cons, zero, new_car);
    return cons;
  case BUF:
    buf_put_uchar(cons, zero, new_car);
    return cons;
  case COBJ:
    if (obj_struct_p(cons)) {
      {
        val rplaca_meth = get_special_slot(cons, rplaca_m);
        if (rplaca_meth) {
          (void) funcall2(rplaca_meth, cons, new_car);
          return cons;
        }
      }
      {
        val lambda_set_meth = get_special_slot(cons, lambda_set_m);
        if (lambda_set_meth) {
          (void) funcall3(lambda_set_meth, cons, zero, new_car);
          return cons;
        }
      }
      type_mismatch(lit("rplaca: ~s lacks ~s or ~s method"),
                    cons, rplaca_s, lambda_set_s, nao);
    }
    /* fallthrough */
  default:
    type_mismatch(lit("rplaca: cannot modify ~s"), cons, nao);
  }
}

val rplacd(val cons, val new_cdr)
{
  switch (type(cons)) {
  case CONS:
    set(mkloc(cons->c.cdr, cons), new_cdr);
    return cons;
  case LCONS:
    set(mkloc(cons->lc.cdr, cons), new_cdr);
    return cons;
  case VEC:
  case STR:
  case LSTR:
  case BUF:
    replace(cons, new_cdr, one, t);
    return cons;
  case COBJ:
    if (obj_struct_p(cons)) {
      {
        val rplacd_meth = get_special_slot(cons, rplacd_m);
        if (rplacd_meth) {
          (void) funcall2(rplacd_meth, cons, new_cdr);
          return cons;
        }
      }
      replace(cons, new_cdr, one, t);
      return cons;
    }
    /* fallthrough */
  default:
    type_mismatch(lit("rplacd: cannot modify ~s"), cons, nao);
  }
}

val us_rplaca(val cons, val new_car)
{
  set(mkloc(cons->c.car, cons), new_car);
  return cons;
}

val us_rplacd(val cons, val new_cdr)
{
  set(mkloc(cons->c.cdr, cons), new_cdr);
  return cons;
}

val sys_rplaca(val cons, val new_car)
{
  (void) rplaca(cons, new_car);
  return new_car;
}

val sys_rplacd(val cons, val new_cdr)
{
  (void) rplacd(cons, new_cdr);
  return new_cdr;
}

loc car_l(val cons)
{
  switch (type(cons)) {
  case CONS:
    return mkloc(cons->c.car, cons);
  case LCONS:
    if (cons->lc.func) {
      sig_check_fast();
      funcall1(cons->lc.func, cons);
      cons->lc.func = nil;
    }
    return mkloc(cons->lc.car, cons);
  default:
    type_mismatch(lit("~s is not a cons"), cons, nao);
  }
}

loc cdr_l(val cons)
{
  switch (type(cons)) {
  case CONS:
    return mkloc(cons->c.cdr, cons);
  case LCONS:
    if (cons->lc.func) {
      sig_check_fast();
      funcall1(cons->lc.func, cons);
      cons->lc.func = nil;
    }
    return mkloc(cons->lc.cdr, cons);
  default:
    type_mismatch(lit("~s is not a cons"), cons, nao);
  }
}

val first(val cons)
{
  return car(cons);
}

val rest(val cons)
{
  return cdr(cons);
}

val second(val obj)
{
  return ref(obj, one);
}

val third(val obj)
{
  return ref(obj, two);
}

val fourth(val obj)
{
  return ref(obj, three);
}

val fifth(val obj)
{
  return ref(obj, four);
}

val sixth(val obj)
{
  return ref(obj, num_fast(5));
}

val seventh(val obj)
{
  return ref(obj, num_fast(6));
}

val eighth(val obj)
{
  return ref(obj, num_fast(7));
}

val ninth(val obj)
{
  return ref(obj, num_fast(8));
}

val tenth(val obj)
{
  return ref(obj, num_fast(9));
}

val cxr(val addr, val obj)
{
  val self = lit("cxr");

  switch (type(addr)) {
  case NUM:
    {
      cnum a = c_num(addr, self);
      if (a > 0) {
        for (; a != 1; a >>= 1)
          obj = if3((a & 1) != 0, car(obj), cdr(obj));
        return obj;
      }
    }
    break;
  case BGNUM:
    {
      mp_int *a = mp(addr);
      if (!mp_isneg(a)) {
        mp_size i, n = mp_count_bits(a);
        for (i = 0; i < n - 1; i++)
          obj = if3(mp_bit(a, i) == MP_YES, car(obj), cdr(obj));
        return obj;
      }
    }
    break;
  default:
    break;
  }

  uw_throwf(error_s, lit("~a: ~s is an invalid address"), self, addr, nao);
}

val cyr(val addr, val obj)
{
  val self = lit("cyr");

  switch (type(addr)) {
  case NUM:
    {
      cnum a = c_num(addr, self);
      if (a > 0) {
        int h = highest_bit(a);
	cnum m;
        if (h > 1) {
          for (m = convert(cnum, 1) << (h - 2); m != 0; m >>= 1)
            obj = if3((a & m) != 0, car(obj), cdr(obj));
        }
        return obj;
      }
    }
    break;
  case BGNUM:
    {
      mp_int *a = mp(addr);
      if (!mp_isneg(a)) {
        mp_size i, n = mp_count_bits(a);
        for (i = n - 2; i != (mp_size) -1; i--)
          obj = if3(mp_bit(a, i) == MP_YES, car(obj), cdr(obj));
        return obj;
      }
    }
    break;
  default:
    break;
  }

  uw_throwf(error_s, lit("~a: ~s is an invalid address"), self, addr, nao);
}

val conses(val list)
{
  list_collect_decl (out, ptail);

  if (listp(list))
    for (; consp(list); list = cdr(list))
      ptail = list_collect(ptail, list);
  else
    for (; list; list = cdr(list))
      ptail = list_collect(ptail, list);

  return out;
}

static val lazy_conses_func(val env, val lcons)
{
  val fun = us_lcons_fun(lcons);
  us_rplaca(lcons, env);
  func_set_env(fun, env = cdr(env));

  if (env)
    rplacd(lcons, make_lazy_cons(fun));
  else
    rplacd(lcons, nil);
  return nil;
}

val lazy_conses(val list)
{
  if (!list)
    return nil;
  return make_lazy_cons(func_f1(list, lazy_conses_func));
}

val listref(val list, val ind)
{
  gc_hint(list);
  if (minusp(ind)) {
    ind = plus(ind, length_list(list));
    if (minusp(ind))
      return nil;
  }
  for (; gt(ind, zero); ind = minus(ind, one))
    list = cdr(list);
  return car(list);
}

loc tail(val cons)
{
  val d;
  for (d = cdr(cons); consp(d); d = cdr(d))
    cons = d;
  return cdr_l(cons);
}

val lastcons(val list)
{
  val ret;
  gc_hint(list);

  for (ret = nil; consp(list); list = cdr(list))
    ret = list;

  return ret;
}

val last(val seq, val n)
{
  seq_info_t si = seq_info(seq);

  switch (si.kind) {
  case SEQ_NIL:
    return nil;
  case SEQ_LISTLIKE:
    return if3(null_or_missing_p(n),
               lastcons(seq),
               nthlast(n, seq));
  case SEQ_VECLIKE:
    return if3(null_or_missing_p(n),
               sub(seq, negone, t),
               if3(plusp(n),
                   sub(seq, neg(n), t),
                   sub(seq, t, t)));
  default:
    uw_throwf(error_s, lit("last: ~s isn't a sequence"), seq, nao);
  }
}

val nthcdr(val pos, val list)
{
  val self = lit("nthcdr");
  cnum n = c_num(pos, self);

  if (n < 0)
    uw_throwf(error_s, lit("~a: negative index ~s given"), self, pos, nao);

  gc_hint(list);

  while (list && n-- > 0)
    list = cdr(list);

  return list;
}

val nth(val pos, val list)
{
  return car(nthcdr(pos, list));
}

val nthlast(val pos, val list)
{
  val iter = list;

  while (plusp(pos) && consp(list)) {
    list = cdr(list);
    pos = pred(pos);
  }

  if (plusp(pos))
    return iter;

  if (list == iter) {
    while (consp(iter))
      iter = cdr(iter);
  } else {
    while (consp(list)) {
      iter = cdr(iter);
      list = cdr(list);
    }
  }

  return iter;
}

val butlastn(val n, val list)
{
  val tail = nthlast(n, list);
  return ldiff(list, tail);
}

val pop(val *plist)
{
  val ret = car(*plist);
  *plist = cdr(*plist);
  return ret;
}

val rcyc_pop(val *plist)
{
  val rcyc = *plist;
  val ret = rcyc->c.car;
  *plist = rcyc->c.cdr;
  rcyc_cons(rcyc);
  return ret;
}

val upop(val *plist, val *pundo)
{
  *pundo = *plist;
  return pop(plist);
}

val push(val value, val *plist)
{
  /* Unsafe for mutating object fields: use mpush macro. */
  return *plist = cons(value, *plist);
}

val copy_list(val list)
{
  list_collect_decl (out, ptail);

  list = nullify(list);

  while (consp(list)) {
    ptail = list_collect(ptail, car(list));
    list = cdr(list);
  }

  ptail = list_collect_nconc(ptail, list);

  return out;
}

val make_like(val list, val thatobj)
{
  if (list != thatobj) {
    switch (type(thatobj)) {
    case VEC:
      return vec_list(list);
    case STR:
    case LIT:
    case LSTR:
      if (!opt_compat || opt_compat > 101) {
        if (!list)
          return null_string;
      }
      if (is_chr(car(list)))
        return cat_str(list, nil);
      break;
    case BUF:
      if (!list)
        return make_buf(zero, zero, zero);
      if (integerp(car(list)))
        return buf_list(list);
      break;
    case COBJ:
      if (obj_struct_p(thatobj)) {
        val from_list_meth = get_special_slot(thatobj, from_list_m);
        if (from_list_meth)
          return funcall1(from_list_meth, list);
      }
      if (thatobj->co.cls == carray_cls)
        return carray_list(list, carray_type(thatobj), nil);
      break;
    case NIL:
    case CONS:
    case LCONS:
    default:
      break;
    }
  }

  return list;
}

val tolist(val seq)
{
  switch (type(seq)) {
  case VEC:
    return list_vec(seq);
  case STR:
  case LIT:
  case LSTR:
    return list_str(seq);
  case COBJ:
    return mapcar_listout(identity_f, seq);
  case NIL:
  case CONS:
  case LCONS:
  default:
    return seq;
  }
}

val nullify(val obj)
{
  val self = lit("nullify");
  seq_info_t si = seq_info(obj);

  if (seq_iterable(si))
  {
    seq_iter_t iter;
    val elem;
    seq_iter_init_with_info(self, &iter, si, 0);
    return if2(seq_peek(&iter, &elem), obj);
  }

  return si.obj;
}

val empty(val seq)
{
  val self = lit("empty");
  val elem;
  seq_iter_t iter;
  seq_iter_init(self, &iter, seq);

  return tnil(!seq_peek(&iter, &elem));
}

val seqp(val obj)
{
  seq_info_t si = seq_info(obj);
  return tnil(si.kind != SEQ_NOTSEQ);
}

val iterable(val obj)
{
  seq_info_t si = seq_info(obj);
  return seq_iterable(si);
}

static val list_seq_func(val lcons)
{
  val iter = us_car(lcons);
  val item = iter_item(iter);
  val new_iter = iter_step(iter);

  us_rplaca(lcons, item);

  if (iter_more(new_iter))
    us_rplacd(lcons, make_lazy_cons_car(us_lcons_fun(lcons), new_iter));

  return nil;
}

val list_seq(val seq)
{
  val iter = iter_begin(seq);

  if (iter_more(iter))
    return make_lazy_cons_car(func_n1(list_seq_func), iter);

  return nil;
}

val vec_seq(val seq)
{
  val self = lit("vec-seq");
  seq_iter_t iter;
  val elem;
  val vec = vector(zero, nil);
  seq_iter_init(self, &iter, seq);

  while (seq_get(&iter, &elem))
    vec_push(vec, elem);

  return vec;
}

val str_seq(val seq)
{
  val self = lit("str-seq");
  seq_iter_t iter;
  val elem;
  val str = mkustring(zero);
  seq_iter_init(self, &iter, seq);

  while (seq_get(&iter, &elem)) {
    if (chrp(elem) || stringp(elem))
      string_extend(str, elem);
    else
      unsup_obj(self, elem);
  }

  return str;
}

loc list_collect(loc ptail, val obj)
{
  val items = cons(obj, nil);
  val tailobj = deref(ptail);

again:
  switch (type(tailobj)) {
  case NIL:
    set(ptail, items);
    return ptail;
  case CONS:
  case LCONS:
    ptail = tail(tailobj);
    tailobj = deref(ptail);
    goto again;
  case VEC:
    replace_vec(tailobj, items, t, t);
    return ptail;
  case STR:
  case LIT:
  case LSTR:
    replace_str(tailobj, items, t, t);
    return ptail;
  case BUF:
    replace_buf(tailobj, items, t, t);
    return ptail;
  case COBJ:
    if (tailobj->co.cls == carray_cls) {
      carray_replace(tailobj, items, t, t);
      return ptail;
    }
    if (obj_struct_p(tailobj)) {
      replace_obj(tailobj, items, t, t);
      return ptail;
    }
    /* fallthrough */
  default:
    uw_throwf(error_s, lit("cannot append ~s"), deref(ptail), nao);
  }
}

loc list_collect_nconc(loc ptail, val obj)
{
  val tailobj = deref(ptail);

again:
  switch (type(tailobj)) {
  case NIL:
    set(ptail, nullify(obj));
    return ptail;
  case CONS:
  case LCONS:
    ptail = tail(tailobj);
    tailobj = deref(ptail);
    goto again;
  case VEC:
    replace_vec(tailobj, obj, t, t);
    return ptail;
  case STR:
  case LIT:
  case LSTR:
    replace_str(tailobj, obj, t, t);
    return ptail;
  case BUF:
    replace_buf(tailobj, obj, t, t);
    return ptail;
  case COBJ:
    set(ptail, tolist(tailobj));
    ptail = tail(deref(ptail));
    set(ptail, tolist(obj));
    return ptail;
  default:
    uw_throwf(error_s, lit("cannot nconc to ~s"), tailobj, nao);
  }
}

loc list_collect_append(loc ptail, val obj)
{
  val tailobj = deref(ptail);
  obj = nullify(obj);

again:
  switch (type(tailobj)) {
  case NIL:
    set(ptail, obj);
    return ptail;
  case CONS:
  case LCONS:
    set(ptail, copy_list(tailobj));
    ptail = tail(deref(ptail));
    tailobj = deref(ptail);
    goto again;
  case VEC:
    set(ptail, copy_vec(tailobj));
    replace_vec(deref(ptail), obj, t, t);
    return ptail;
  case STR:
  case LIT:
  case LSTR:
    set(ptail, copy_str(tailobj));
    replace_str(deref(ptail), obj, t, t);
    return ptail;
  case BUF:
    set(ptail, copy_buf(tailobj));
    replace_buf(deref(ptail), obj, t, t);
    return ptail;
  case COBJ:
    set(ptail, tolist(tailobj));
    ptail = tail(deref(ptail));
    set(ptail, tolist(obj));
    return ptail;
  default:
    uw_throwf(error_s, lit("cannot append to ~s"), tailobj, nao);
  }
}

loc list_collect_nreconc(loc ptail, val obj)
{
  val rev = nreverse(nullify(obj));
  val tailobj = deref(ptail);

again:
  switch (type(tailobj)) {
  case CONS:
  case LCONS:
    ptail = tail(tailobj);
    tailobj = deref(ptail);
    goto again;
  case NIL:
    set(ptail, rev);
    switch (type(obj)) {
    case CONS:
    case LCONS:
      return cdr_l(obj);
    default:
      return ptail;
    }
  case VEC:
    replace_vec(tailobj, rev, t, t);
    return ptail;
  case STR:
  case LIT:
  case LSTR:
    replace_str(tailobj, rev, t, t);
    return ptail;
  case BUF:
    replace_buf(tailobj, obj, t, t);
    return ptail;
  default:
    uw_throwf(error_s, lit("cannot nconc ~s to ~s"), obj, tailobj, nao);
  }
}

static val revlist(val in, val *tail)
{
  val rev = nil;

  *tail = nil;

  if (in) {
    *tail = rev = cons(car(in), rev);
    in = cdr(in);
  }

  while (in) {
    rev = cons(car(in), rev);
    in = cdr(in);
  }

  return rev;
}

loc list_collect_revappend(loc ptail, val obj)
{
  val last;
  obj = nullify(obj);
  val tailobj = deref(ptail);

again:
  switch (type(tailobj)) {
  case CONS:
  case LCONS:
    set(ptail, copy_list(tailobj));
    ptail = tail(deref(ptail));
    tailobj = deref(ptail);
    goto again;
  case NIL:
    switch (type(obj)) {
    case CONS:
    case LCONS:
      set(ptail, revlist(obj, &last));
      return cdr_l(last);
    case NIL:
      return ptail;
    default:
      set(ptail, reverse(obj));
      return ptail;
    }
    set(ptail, obj);
    return ptail;
  case VEC:
    set(ptail, copy_vec(tailobj));
    replace_vec(deref(ptail), reverse(obj), t, t);
    return ptail;
  case STR:
  case LIT:
  case LSTR:
    set(ptail, copy_str(tailobj));
    replace_str(deref(ptail), reverse(obj), t, t);
    return ptail;
  case BUF:
    set(ptail, copy_buf(tailobj));
    replace_buf(deref(ptail), reverse(obj), t, t);
    return ptail;
  default:
    uw_throwf(error_s, lit("cannot append to ~s"), tailobj, nao);
  }
}

val nreverse(val in)
{
  val self = lit("nreverse");
  seq_info_t si = seq_info(in);

  switch (si.kind) {
  case SEQ_NIL:
    return nil;
  case SEQ_LISTLIKE:
    {
      val rev = nil;
      val in = si.obj;

      while (in) {
        val temp = cdr(in);
        rplacd(in, rev);
        rev = in;
        in = temp;
      }

      return rev;
    }
  case SEQ_VECLIKE:
    {
      val in = si.obj;
      cnum len = c_fixnum(length(in), self);
      cnum i;

      for (i = 0; i < len / 2; i++) {
        cnum j = len - i - 1;
        val tmp = ref(in, num_fast(i));
        refset(in, num_fast(i), ref(in, num_fast(j)));
        refset(in, num_fast(j), tmp);
      }

      return in;
    }
  default:
    uw_throwf(error_s, lit("~a: cannot reverse ~s"), self, in, nao);
  }
}

val reverse(val seq_in)
{
  val self = lit("reverse");
  seq_info_t si = seq_info(seq_in);

  switch (si.kind) {
  case SEQ_NIL:
    return nil;
  case SEQ_LISTLIKE:
    {
      val rev = nil;
      val in = si.obj;

      while (in) {
        rev = cons(car(in), rev);
        in = cdr(in);
      }

      return make_like(rev, seq_in);
    }
  case SEQ_VECLIKE:
    if (si.type == LSTR)
      si.obj = lazy_str_force(si.obj);

    {
      val obj = copy(si.obj);
      cnum len = c_fixnum(length(si.obj), self);
      cnum i;

      for (i = 0; i < len / 2; i++) {
        cnum j = len - i - 1;
        val tmp = ref(obj, num_fast(i));
        refset(obj, num_fast(i), ref(obj, num_fast(j)));
        refset(obj, num_fast(j), tmp);
      }

      return obj;
    }
  default:
    uw_throwf(error_s, lit("~a: cannot reverse ~s"), self, seq_in, nao);
  }
}

val us_nreverse(val in)
{
  val rev = nil;

  while (in) {
    val temp = us_cdr(in);
    us_rplacd(in, rev);
    rev = in;
    in = temp;
  }

  return rev;
}

val append2(val list1, val list2)
{
  list_collect_decl (out, ptail);

  ptail = list_collect_append (ptail, list1);
  ptail = list_collect_append (ptail, list2);

  return out;
}

val appendv(struct args *lists)
{
  cnum index = 0;
  list_collect_decl (out, ptail);

  while (args_more(lists, index)) {
    val item = args_get(lists, &index);
    ptail = list_collect_append(ptail, item);
  }

  return out;
}

static val appendl(val lists)
{
  args_decl_list(args, ARGS_MIN, lists);
  return appendv(args);
}

val nappend2(val list1, val list2)
{
  list_collect_decl (out, ptail);

  ptail = list_collect_nconc (ptail, list1);
  ptail = list_collect_nconc (ptail, list2);

  return out;
}

val revappend(val list1, val list2)
{
  list_collect_decl (out, ptail);

  ptail = list_collect_revappend(ptail, list1);
  ptail = list_collect_nconc(ptail, list2);

  return out;
}

val nreconc(val list1, val list2)
{
  list_collect_decl (out, ptail);

  ptail = list_collect_nreconc(ptail, list1);
  ptail = list_collect_nconc(ptail, list2);

  return out;
}

val nconcv(struct args *lists)
{
  cnum index = 0;
  list_collect_decl (out, ptail);

  while (args_more(lists, index))
    ptail = list_collect_nconc(ptail, args_get(lists, &index));

  return out;
}

val sub_list(val list, val from, val to)
{
  val len = nil;

  if (!list)
    return nil;

  if (null_or_missing_p(from))
    from = zero;
  else if (from == t)
    from = nil;
  else if (minusp(from)) {
    from = plus(from, len = length(list));
    if (to == zero)
      to = nil;
  }

  if (to == t || null_or_missing_p(to))
    to = nil;
  else if (!null_or_missing_p(to) && minusp(to))
    to = plus(to, if3(len, len, len = length(list)));

  gc_hint(list);

  if (to && from && gt(from, to)) {
    return nil;
  } else if (!to || (len && ge(to, len)))  {
    val i;

    for (i = zero; list; list = cdr(list), i = plus(i, one)) {
      if (from && ge(i, from))
        break;
    }
    return list;
  } else {
    val i;
    list_collect_decl (out, ptail);

    for (i = zero; list; list = cdr(list), i = plus(i, one)) {
      if (ge(i, to))
        break;
      if (from && ge(i, from))
        ptail = list_collect(ptail, car(list));
    }

    return out;
  }
}

val replace_list(val list, val items, val from, val to)
{
  val self = lit("replace-list");
  val len = nil;

  if (!list)
    return tolist(items);

  if (missingp(from)) {
    from = zero;
  } else if (from == t) {
    from = nil;
  } else if (!integerp(from)) {
    seq_iter_t wh_iter;
    val iter = list, idx = zero, item, wh;
    seq_iter_t item_iter;
    seq_iter_init(self, &item_iter, items);
    seq_iter_init(self, &wh_iter, from);

    if (!missingp(to))
      uw_throwf(error_s,
                lit("~a: to-arg not applicable when from-arg is a list"),
                self, nao);

    while (iter && seq_peek(&item_iter, &item) && seq_peek(&wh_iter, &wh)) {
      if (minusp(wh))
        wh = plus(wh, len ? len : (len = length(list)));
      if (lt(wh, idx)) {
        seq_geti(&wh_iter);
        seq_geti(&item_iter);
        continue;
      } else if (eql(wh, idx)) {
        rplaca(iter, item);
        seq_geti(&wh_iter);
        seq_geti(&item_iter);
      }

      iter = cdr(iter);
      idx = plus(idx, one);
    }

    return list;
  } else if (minusp(from)) {
    from = plus(from, len ? len : (len = length(list)));
    if (to == zero)
      to = len;
  }

  if (to == t || null_or_missing_p(to))
    to = nil;
  if (to && minusp(to))
    to = plus(to, len ? len : (len = length(list)));

  gc_hint(list);

  if (!to || (len && ge(to, len)))  {
    if (from && zerop(from)) {
      return tolist(items);
    } else {
      val i;
      list_collect_decl (out, ptail);

      for (i = zero; list; list = cdr(list), i = plus(i, one)) {
        if (from && ge(i, from))
          break;
        ptail = list_collect(ptail, car(list));
      }

      list_collect_nconc(ptail, tolist(items));

      return out;
    }
  } else {
    val i;
    list_collect_decl (out, ptail);

    for (i = zero; list; list = cdr(list), i = plus(i, one)) {
      if (ge(i, to))
        break;
      if (from && lt(i, from))
        ptail = list_collect(ptail, car(list));
    }

    if (listp(items)) {
      ptail = list_collect_nconc(ptail, items);
      list_collect_append(ptail, list);
    } else {
      ptail = list_collect_nconc(ptail, tolist(items));
      list_collect_nconc(ptail, list);
    }

    return out;
  }
}

static val lazy_appendv_func(val rl, val lcons)
{
  val fl = us_car(lcons);
  cons_bind (fe, re, fl);

  us_rplaca(lcons, fe);

  if (re) {
    us_rplacd(lcons, make_lazy_cons_car(us_lcons_fun(lcons), re));
  } else if (rl) {
    do {
      fl = car(rl);
      rl = cdr(rl);
    } while (!fl && rl);

    if (fl) {
      if (rl) {
        val fun = us_lcons_fun(lcons);
        us_func_set_env(fun, rl);
        us_rplacd(lcons, make_lazy_cons_car(fun, fl));
      } else {
        us_rplacd(lcons, fl);
      }
    }
  }

  return nil;
}

val lazy_appendv(struct args *args)
{
  val nonempty = nil;
  cnum index = 0;

  while (args_more(args, index)) {
    nonempty = args_get(args, &index);
    if (nonempty)
      break;
  }

  if (!args_more(args, index))
    return nonempty;

  if (atom(nonempty))
    uw_throwf(error_s, lit("append*: cannot append to atom ~s"),
              nonempty, nao);

  {
    return make_lazy_cons_car(func_f1(args_get_rest(args, index),
                                      lazy_appendv_func), nonempty);
  }
}

val lazy_appendl(val lists)
{
  args_decl_list(args, ARGS_MIN, lists);
  return lazy_appendv(args);
}

val ldiff(val seq1_in, val seq2)
{
  val seq1 = seq1_in;
  list_collect_decl (out, ptail);

loop:
  if (seq1 == seq2)
    return out;

  {
    seq_info_t si1 = seq_info(seq1);
    seq_info_t si2 = seq_info(seq2);

    switch (SEQ_KIND_PAIR(si1.kind, si2.kind)) {
    case SEQ_KIND_PAIR(SEQ_NIL, SEQ_NIL):
    case SEQ_KIND_PAIR(SEQ_NIL, SEQ_LISTLIKE):
    case SEQ_KIND_PAIR(SEQ_NIL, SEQ_VECLIKE):
    case SEQ_KIND_PAIR(SEQ_NIL, SEQ_HASHLIKE):
    case SEQ_KIND_PAIR(SEQ_NIL, SEQ_TREELIKE):
    case SEQ_KIND_PAIR(SEQ_NIL, SEQ_NOTSEQ):
      break;
    case SEQ_KIND_PAIR(SEQ_LISTLIKE, SEQ_NIL):
    case SEQ_KIND_PAIR(SEQ_VECLIKE, SEQ_NIL):
    case SEQ_KIND_PAIR(SEQ_HASHLIKE, SEQ_NIL):
    case SEQ_KIND_PAIR(SEQ_TREELIKE, SEQ_NIL):
    case SEQ_KIND_PAIR(SEQ_NOTSEQ, SEQ_NIL):
      return seq1;
    case SEQ_KIND_PAIR(SEQ_LISTLIKE, SEQ_LISTLIKE):
      ptail = list_collect(ptail, car(si1.obj));
      seq1 = cdr(si1.obj);
      goto loop;
    case SEQ_KIND_PAIR(SEQ_LISTLIKE, SEQ_VECLIKE):
    case SEQ_KIND_PAIR(SEQ_LISTLIKE, SEQ_HASHLIKE):
    case SEQ_KIND_PAIR(SEQ_LISTLIKE, SEQ_TREELIKE):
    case SEQ_KIND_PAIR(SEQ_LISTLIKE, SEQ_NOTSEQ):
      ptail = list_collect(ptail, car(si1.obj));
      seq1 = cdr(si1.obj);
      goto loop;
    case SEQ_KIND_PAIR(SEQ_VECLIKE, SEQ_VECLIKE):
      if (equal(seq1, seq2) || zerop(length(seq1)))
        break;
      ptail = list_collect(ptail, ref(seq1, zero));
      seq1 = sub(seq1, one, t);
      goto loop;
    case SEQ_KIND_PAIR(SEQ_HASHLIKE, SEQ_HASHLIKE):
    case SEQ_KIND_PAIR(SEQ_HASHLIKE, SEQ_TREELIKE):
    case SEQ_KIND_PAIR(SEQ_NOTSEQ, SEQ_NOTSEQ):
      if (!equal(seq1, seq2))
        ptail = list_collect_append(ptail, seq1);
      break;
    default:
      ptail = list_collect_append(ptail, seq1);
      break;
    }
  }

  return make_like(out, seq1_in);
}

val ldiff_old(val list1, val list2)
{
  val list_orig = list1;
  list_collect_decl (out, ptail);

  list1 = nullify(list1);
  list2 = nullify(list2);

  switch (type(list2)) {
  case NIL:
  case CONS:
  case LCONS:
    while (list1 && list1 != list2) {
      ptail = list_collect(ptail, car(list1));
      list1 = cdr(list1);
    }
    break;
  default:
    while (list1 && !equal(list1, list2)) {
      ptail = list_collect(ptail, car(list1));
      list1 = cdr(list1);
    }
    break;
  }

  return make_like(out, list_orig);
}

val tailp(val obj, val list)
{
  while (obj != list) {
    if (atom(list))
      return nil;
    list = cdr(list);
  }
  return t;
}

val memq(val obj, val list)
{
  val list_orig = list;
  list = nullify(list);
  gc_hint(list);
  while (list && car(list) != obj)
    list = cdr(list);
  return make_like(list, list_orig);
}

val rmemq(val obj, val list)
{
  val list_orig = list;
  val found = nil;
  list = nullify(list);
  gc_hint(list);
  while (list && (found = (car(list) != obj ? list : found)))
    list = cdr(list);
  return make_like(found, list_orig);
}

val memql(val obj, val list)
{
  val list_orig = list;
  list = nullify(list);
  gc_hint(list);
  while (list && !eql(car(list), obj))
    list = cdr(list);
  return make_like(list, list_orig);
}

val rmemql(val obj, val list)
{
  val list_orig = list;
  val found = nil;
  list = nullify(list);
  gc_hint(list);
  while (list && (found = (eql(car(list), obj) ? list : found)))
    list = cdr(list);
  return make_like(found, list_orig);
}

val memqual(val obj, val list)
{
  val list_orig = list;
  list = nullify(list);
  gc_hint(list);
  while (list && !equal(car(list), obj))
    list = cdr(list);
  return make_like(list, list_orig);
}

val rmemqual(val obj, val list)
{
  val list_orig = list;
  val found = nil;
  list = nullify(list);
  gc_hint(list);
  while (list && (found = (equal(car(list), obj) ? list : found)))
    list = cdr(list);
  return make_like(found, list_orig);
}

val member(val item, val list, val testfun, val keyfun)
{
  testfun = default_arg(testfun, equal_f);
  keyfun = default_arg(keyfun, identity_f);

  list = nullify(list);

  gc_hint(list);

  for (; list; list = cdr(list)) {
    val elem = car(list);
    val key = funcall1(keyfun, elem);

    if (funcall2(testfun, item, key))
      return list;
  }

  return nil;
}

val rmember(val item, val list, val testfun, val keyfun)
{
  val found = nil;
  testfun = default_arg(testfun, equal_f);
  keyfun = default_arg(keyfun, identity_f);

  list = nullify(list);

  gc_hint(list);

  for (; list; list = cdr(list)) {
    val elem = car(list);
    val key = funcall1(keyfun, elem);

    if (funcall2(testfun, item, key))
      found = list;
  }

  return found;
}

val member_if(val pred, val list, val key)
{
  key = default_arg(key, identity_f);
  list = nullify(list);

  gc_hint(list);

  for (; list; list = cdr(list)) {
    val item = car(list);
    val subj = funcall1(key, item);

    if (funcall1(pred, subj))
      return list;
  }

  return nil;
}

val rmember_if(val pred, val list, val key)
{
  val found = nil;
  key = default_arg(key, identity_f);
  list = nullify(list);

  gc_hint(list);

  for (; list; list = cdr(list)) {
    val item = car(list);
    val subj = funcall1(key, item);

    if (funcall1(pred, subj))
      found = list;
  }

  return found;
}

static val rem_impl(val (*eqfun)(val, val), val name,
                    val obj, val seq_in, val keyfun_in)
{
  val keyfun = default_null_arg(keyfun_in);

  switch (type(seq_in)) {
  case NIL:
    return nil;
  case CONS:
  case LCONS:
  case COBJ:
    {
      list_collect_decl (out, ptail);
      val list = seq_in;
      val lastmatch = cons(nil, list);

      gc_hint(list);

      for (; list; list = cdr(list)) {
        val elem = car(list);
        val key = keyfun ? funcall1(keyfun, elem) : elem;

        if (eqfun(key, obj)) {
          ptail = list_collect_nconc(ptail, ldiff(cdr(lastmatch), list));
          lastmatch = list;
        }
      }
      ptail = list_collect_nconc(ptail, cdr(lastmatch));
      return out;
    }
  case LIT:
  case STR:
  case LSTR:
    {
      val out = mkustring(zero);
      val str = seq_in;
      cnum len = c_fixnum(length_str(str), name), i;

      for (i = 0; i < len; i++) {
        val elem = chr_str(str, num_fast(i));
        val key = keyfun ? funcall1(keyfun, elem) : elem;

        if (!eqfun(key, obj))
          string_extend(out, elem);
      }

      return out;
    }
  case VEC:
    {
      val out = vector(zero, nil);
      val vec = seq_in;
      cnum len = c_fixnum(length_vec(vec), name), i;

      for (i = 0; i < len; i++) {
        val elem = vecref(vec, num_fast(i));
        val key = keyfun ? funcall1(keyfun, elem) : elem;

        if (!eqfun(key, obj))
          vec_push(out, elem);
      }

      return out;
    }
  default:
    uw_throwf(error_s, lit("~a: ~s isn't a sequence"), name, seq_in, nao);
  }
}

val remove_if(val pred, val seq_in, val keyfun_in)
{
  val self = lit("remove-if");
  val keyfun = default_null_arg(keyfun_in);

  switch (type(seq_in)) {
  case NIL:
    return nil;
  case CONS:
  case LCONS:
  case COBJ:
    {
      list_collect_decl (out, ptail);
      val list = seq_in;
      val lastmatch = cons(nil, list);

      gc_hint(list);

      for (; list; list = cdr(list)) {
        val elem = car(list);
        val key = keyfun ? funcall1(keyfun, elem) : elem;

        if (funcall1(pred, key)) {
          ptail = list_collect_nconc(ptail, ldiff(cdr(lastmatch), list));
          lastmatch = list;
        }
      }
      ptail = list_collect_nconc(ptail, cdr(lastmatch));
      return out;
    }
  case LIT:
  case STR:
  case LSTR:
    {
      val out = mkustring(zero);
      val str = seq_in;
      cnum len = c_fixnum(length_str(str), self), i;

      for (i = 0; i < len; i++) {
        val elem = chr_str(str, num_fast(i));
        val key = keyfun ? funcall1(keyfun, elem) : elem;

        if (!funcall1(pred, key))
          string_extend(out, elem);
      }

      return out;
    }
  case VEC:
    {
      val out = vector(zero, nil);
      val vec = seq_in;
      cnum len = c_fixnum(length_vec(vec), self), i;

      for (i = 0; i < len; i++) {
        val elem = vecref(vec, num_fast(i));
        val key = keyfun ? funcall1(keyfun, elem) : elem;

        if (!funcall1(pred, key))
          vec_push(out, elem);
      }

      return out;
    }
  default:
    uw_throwf(error_s, lit("~a: ~s isn't a sequence"), self, seq_in, nao);
  }
}


val remq(val obj, val seq, val keyfun)
{
  return rem_impl(eq, lit("remq"), obj, seq, keyfun);
}

val remql(val obj, val seq, val keyfun)
{
  return rem_impl(eql, lit("remq"), obj, seq, keyfun);
}

val remqual(val obj, val seq, val keyfun)
{
  return rem_impl(equal, lit("remqual"), obj, seq, keyfun);
}

val keepq(val obj, val seq, val keyfun)
{
  return rem_impl(neq, lit("keepq"), obj, seq, keyfun);
}

val keepql(val obj, val seq, val keyfun)
{
  return rem_impl(neql, lit("keepql"), obj, seq, keyfun);
}

val keepqual(val obj, val seq, val keyfun)
{
  return rem_impl(nequal, lit("keepqual"), obj, seq, keyfun);
}

val keep_if(val pred, val seq, val keyfun)
{
  return remove_if(notf(pred), seq, keyfun);
}

val separate(val pred, val seq_in, val keyfun_in)
{
  val self = lit("separate");
  val keyfun = default_null_arg(keyfun_in);

  switch (type(seq_in)) {
  case NIL:
    return nil;
  case CONS:
  case LCONS:
  case COBJ:
    {
      list_collect_decl (yea, yptail);
      list_collect_decl (nay, nptail);
      val list = seq_in;
      val lastdiff = list;
      val was_yea = nil; /* Initialize to nil to silence compiler warning. */

      gc_hint(list);

      for (; list; list = cdr(list)) {
        val elem = car(list);
        val key = keyfun ? funcall1(keyfun, elem) : elem;
        val is_yea = if3(funcall1(pred, key), t, nil);

        if (list != seq_in && neq(is_yea, was_yea)) {
          if (was_yea)
            yptail = list_collect_nconc(yptail, ldiff(lastdiff, list));
          else
            nptail = list_collect_nconc(nptail, ldiff(lastdiff, list));

          lastdiff = list;
        }

        was_yea = is_yea;
      }

      if (was_yea)
        yptail = list_collect_nconc(yptail, lastdiff);
      else
        nptail = list_collect_nconc(nptail, lastdiff);

      return cons(yea, cons(nay, nil));
    }
  case LIT:
  case STR:
  case LSTR:
    {
      val yea = mkustring(zero);
      val nay = mkustring(zero);
      val str = seq_in;
      cnum len = c_fixnum(length_str(str), self), i;

      for (i = 0; i < len; i++) {
        val elem = chr_str(str, num_fast(i));
        val key = keyfun ? funcall1(keyfun, elem) : elem;

        string_extend(funcall1(pred, key) ? yea : nay, elem);
      }

      return cons(yea, cons(nay, nil));
    }
  case VEC:
    {
      val yea = vector(zero, nil);
      val nay = vector(zero, nil);
      val vec = seq_in;
      cnum len = c_fixnum(length_vec(vec), self), i;

      for (i = 0; i < len; i++) {
        val elem = vecref(vec, num_fast(i));
        val key = keyfun ? funcall1(keyfun, elem) : elem;

        vec_push(funcall1(pred, key) ? yea : nay, elem);
      }

      return cons(yea, cons(nay, nil));
    }
  default:
    uw_throwf(error_s, lit("~a: ~s isn't a sequence"), self, seq_in, nao);
  }
}

static val rem_lazy_rec(val obj, val list, val env, val func);

static val rem_lazy_func(val env, val lcons)
{
  cons_bind (pred, list, env);
  val rest = rem_lazy_rec(pred, list, env, us_lcons_fun(lcons));
  us_rplacd(lcons, rem_lazy_rec(pred, list, env, us_lcons_fun(lcons)));
  return rest;
}

static val rem_lazy_rec(val pred, val list, val env, val func)
{
  while (list && funcall1(pred, car(list)))
    list = cdr(list);
  if (!list)
    return nil;
  if (!env)
    func = func_f1(cons(pred, cdr(list)), rem_lazy_func);
  else
    rplacd(env, cdr(list));
  return make_lazy_cons_car(func, car(list));
}

val remq_lazy(val obj, val list)
{
  return rem_lazy_rec(pa_12_1(eq_f, obj), nullify(list), nil, nil);
}

val remql_lazy(val obj, val list)
{
  return rem_lazy_rec(pa_12_1(eql_f, obj), nullify(list), nil, nil);
}

val remqual_lazy(val obj, val list)
{
  return rem_lazy_rec(pa_12_1(equal_f, obj), nullify(list), nil, nil);
}

val remove_if_lazy(val pred, val list, val key)
{
  val pred_key = chain(default_arg(key, identity_f), pred, nao);
  return rem_lazy_rec(pred_key, nullify(list), nil, nil);
}

val keep_if_lazy(val pred, val list, val key)
{
  val pred_key = chain(default_arg(key, identity_f), pred, null_f, nao);
  return rem_lazy_rec(pred_key, nullify(list), nil, nil);
}

val tree_find(val obj, val tree, val testfun)
{
  if (funcall2(default_arg(testfun, equal_f), obj, tree))
    return t;
  else if (consp(tree))
    return some_satisfy(tree, pa_123_2(func_n3(tree_find),
                                       obj, testfun), nil);
  return nil;
}

val countqual(val obj, val seq)
{
  val self = lit("countqual");
  seq_iter_t iter;
  ucnum count = 0;
  val ocount = zero;
  val elem;

  seq_iter_init(self, &iter, z(seq));

  while (seq_get(&iter, &elem))
    if (equal(elem, obj))
      if (++count == 0)
        ocount = plus(ocount, one);

  return if3(ocount == zero,
             unum(count),
             plus(unum(count), ash(ocount, num_fast(CHAR_BIT * sizeof count))));
}

val countql(val obj, val seq)
{
  val self = lit("countql");
  seq_iter_t iter;
  ucnum count = 0;
  val ocount = zero;
  val elem;

  seq_iter_init(self, &iter, z(seq));

  while (seq_get(&iter, &elem))
    if (eql(elem, obj))
      if (++count == 0)
        ocount = plus(ocount, one);

  return if3(ocount == zero,
             unum(count),
             plus(unum(count), ash(ocount, num_fast(CHAR_BIT * sizeof count))));
}

val countq(val obj, val seq)
{
  val self = lit("countq");
  seq_iter_t iter;
  ucnum count = 0;
  val ocount = zero;
  val elem;

  seq_iter_init(self, &iter, z(seq));

  while (seq_get(&iter, &elem))
    if (elem == obj)
      if (++count == 0)
        ocount = plus(ocount, one);

  return if3(ocount == zero,
             unum(count),
             plus(unum(count), ash(ocount, num_fast(CHAR_BIT * sizeof count))));
}

val count_if(val pred, val seq, val key_in)
{
  val self = lit("count-if");
  seq_iter_t iter;
  ucnum count = 0;
  val ocount = zero;
  val key = default_arg(key_in, identity_f);
  val elem;

  seq_iter_init(self, &iter, z(seq));

  while (seq_get(&iter, &elem)) {
    val subj = funcall1(key, elem);
    if (funcall1(pred, subj))
      if (++count == 0)
        ocount = plus(ocount, one);
  }

  return if3(ocount == zero,
             unum(count),
             plus(unum(count), ash(ocount, num_fast(CHAR_BIT * sizeof count))));
}

val some_satisfy(val seq, val pred_in, val key_in)
{
  val pred = default_arg(pred_in, identity_f);
  val key = default_arg(key_in, identity_f);
  val self = lit("some");
  seq_iter_t iter;
  val elem;

  seq_iter_init(self, &iter, z(seq));

  while (seq_get(&iter, &elem)) {
    val item = funcall1(pred, funcall1(key, elem));
    if (item != nil)
      return item;
  }

  return nil;
}

val all_satisfy(val seq, val pred_in, val key_in)
{
  val pred = default_arg(pred_in, identity_f);
  val key = default_arg(key_in, identity_f);
  val self = lit("some");
  seq_iter_t iter;
  val elem;
  val ret = t;

  seq_iter_init(self, &iter, z(seq));

  while (seq_get(&iter, &elem)) {
    if ((ret = funcall1(pred, funcall1(key, elem))) == nil)
      return nil;
  }

  return ret;
}

val none_satisfy(val seq, val pred_in, val key_in)
{
  val pred = default_arg(pred_in, identity_f);
  val key = default_arg(key_in, identity_f);
  val self = lit("none");
  seq_iter_t iter;
  val elem;

  seq_iter_init(self, &iter, z(seq));

  while (seq_get(&iter, &elem)) {
    if (funcall1(pred, funcall1(key, elem)))
      return nil;
  }

  return t;
}

val multi(val func, struct args *lists)
{
  val transposed = mapcarv(list_f, lists);
  val processed = funcall1(func, transposed);
  return mapcarl(list_f, processed);
}

val flatten(val list)
{
  if (list == nil)
    return nil;

  if (atom(list))
    return cons(list, nil);

  return mappend(func_n1(flatten), list);
}

/*
 * Return the embedded list whose car is the non-nil atom in the given nested
 * list, updating the escape stack in the process. If no such atom is found in
 * the list, try to retreate up the stack to find it in the surrounding
 * structure, finally returning nil if nothing is found.
 */
static val lazy_flatten_scan(val list, val *escape)
{
  for (;;) {
    if (list) {
      val a = car(list);
      if (nilp(a)) {
        list = cdr(list);
      } else if (atom(a)) {
        return list;
      } else do {
        push(cdr(list), escape); /* safe mutation: *escape is a local var */
        list = a;
        a = car(list);
      } while (consp(a));
      return list;
    } else if (*escape) {
      list = pop(escape);
    } else {
      return nil;
    }
  }
}

static val lazy_flatten_func(val lcons)
{
  us_cons_bind (list, escape, lcons);
  val atom = car(list);
  val next = lazy_flatten_scan(cdr(list), &escape);

  us_rplaca(lcons, atom);

  if (next)
    us_rplacd(lcons, make_lazy_cons_car_cdr(us_lcons_fun(lcons), next, escape));
  else
    us_rplacd(lcons, nil);

  return nil;
}

val lazy_flatten(val list)
{
  if (atom(list)) {
    return cons(list, nil);
  } else {
    val escape = nil;
    val next = lazy_flatten_scan(list, &escape);

    if (!next)
      return nil;

    return make_lazy_cons_car_cdr(func_n1(lazy_flatten_func),
                                  next, escape);
  }
}

val flatcar(val tree)
{
  if (atom(tree))
    return cons(tree, nil);
  if (cdr(tree))
    return nappend2(flatcar(car(tree)), flatcar(cdr(tree)));
  return flatcar(car(tree));
}

static val lazy_flatcar_scan(val tree, val *cont)
{
  for (;;) {
    val a = car(tree);
    val d = cdr(tree);

    if (d != nil)
      *cont = cons(d, *cont);

    if (atom(a))
      return a;

    tree = a;
  }
}

static val lazy_flatcar_func(val tree, val lcons)
{
  val cont = nil;
  val atom = lazy_flatcar_scan(tree, &cont);
  val fun = us_lcons_fun(lcons);

  rplaca(lcons, atom);
  us_func_set_env(fun, cont);

  if (cont)
    us_rplacd(lcons, make_lazy_cons(fun));

  return nil;
}

val lazy_flatcar(val tree)
{
  if (atom(tree)) {
    return cons(tree, nil);
  } else {
    val cont = nil;
    val nextatom = lazy_flatcar_scan(tree, &cont);

    if (!cont)
      return cons(nextatom, nil);

    return make_lazy_cons(func_f1(tree, lazy_flatcar_func));
  }
}


static val tuples_func(val n, val lcons)
{
  list_collect_decl (out, ptail);
  us_cons_bind (seq_in, fill, lcons);
  val seq = seq_in;
  val count;

  for (count = n; count != zero && seq; count = minus(count, one))
    ptail = list_collect(ptail, pop(&seq));

  if (!missingp(fill))
    for (; gt(count, zero); count = minus(count, one))
      ptail = list_collect(ptail, fill);

  if (seq)
    us_rplacd(lcons, make_lazy_cons_car_cdr(us_lcons_fun(lcons), seq, fill));
  else
    us_rplacd(lcons, nil);
  us_rplaca(lcons, make_like(out, seq_in));

  return nil;
}

val tuples(val n, val seq, val fill)
{
  seq = nullify(seq);

  if (!seq)
    return nil;

  return make_lazy_cons_car_cdr(func_f1(n, tuples_func), seq, fill);
}

static val partition_by_func(val func, val lcons)
{
  list_collect_decl (out, ptail);
  us_cons_bind (flast, seq_in, lcons);
  val seq = seq_in;
  val fnext = nil;

  ptail = list_collect(ptail, pop(&seq));

  while (seq) {
    val next = car(seq);
    fnext = funcall1(func, next);

    if (!equal(flast, fnext))
      break;

    ptail = list_collect(ptail, next);

    seq = cdr(seq);
    flast = fnext;
  }

  us_rplacd(lcons, if2(seq,
                       make_lazy_cons_car_cdr(us_lcons_fun(lcons),
                                              fnext, seq)));
  us_rplaca(lcons, make_like(out, seq_in));
  return nil;
}

val partition_by(val func, val seq)
{
  seq = nullify(seq);

  if (!seq)
    return nil;

  return make_lazy_cons_car_cdr(func_f1(func, partition_by_func),
                                funcall1(func, car(seq)), seq);
}

static val partition_func(val base, val lcons)
{
  us_cons_bind (seq, indices, lcons);
  val len = nil;

  for (;;) {
    if (indices) {
      val raw_index = pop(&indices);
      val index = if3((!opt_compat || opt_compat > 170) && minusp(raw_index),
                      plus(raw_index, if3(len, len, len = length(seq))),
                      raw_index);
      val index_rebased = minus(index, base);

      if (le(index_rebased, zero)) {
        continue;
      } else {
        val first = sub(seq, zero, index_rebased);
        val rest = nullify(sub(seq, index_rebased, t));

        if (rest) {
          val fun = us_lcons_fun(lcons);
          us_func_set_env(fun, index);
          us_rplacd(lcons, make_lazy_cons_car_cdr(fun, rest, indices));
        } else {
          us_rplacd(lcons, nil);
        }

        us_rplaca(lcons, first);
      }
    } else {
      us_rplaca(lcons, seq);
      us_rplacd(lcons, nil);
    }
    break;
  }

  return nil;
}

static val split_func(val base, val lcons)
{
  us_cons_bind (seq, indices, lcons);
  val len = nil;

  for (;;) {
    if (indices) {
      val raw_index = pop(&indices);
      val index = if3((!opt_compat || opt_compat > 170) && minusp(raw_index),
                      plus(raw_index, if3(len, len, len = length(seq))),
                      raw_index);
      val index_rebased = minus(index, base);

      if (minusp(index_rebased)) {
        continue;
      } else {
        val first = sub(seq, zero, index_rebased);
        val rsub = sub(seq, index_rebased, t);
        val rest = nullify(rsub);

        if (rest) {
          val fun = us_lcons_fun(lcons);
          us_func_set_env(fun, index);
          us_rplacd(lcons, make_lazy_cons_car_cdr(fun, rest, indices));
        } else {
          us_rplacd(lcons, cons(rsub, nil));
        }

        us_rplaca(lcons, first);
      }
    } else {
      us_rplaca(lcons, seq);
      us_rplacd(lcons, nil);
    }
    break;
  }

  return nil;
}

static val split_star_func(val base, val lcons)
{
  us_cons_bind (seq, indices, lcons);
  val len = nil;

  for (;;) {
    if (indices) {
      val raw_index = pop(&indices);
      val index = if3((!opt_compat || opt_compat > 170) && minusp(raw_index),
                      plus(raw_index, if3(len, len, len = length(seq))),
                      raw_index);
      val index_rebased = minus(index, base);

      if (minusp(index_rebased)) {
        continue;
      } else {
        val first = sub(seq, zero, index_rebased);
        val rsub = sub(seq, succ(index_rebased), t);
        val rest = nullify(rsub);

        if (rest) {
          val fun = us_lcons_fun(lcons);
          us_func_set_env(fun, succ(index));
          us_rplacd(lcons, make_lazy_cons_car_cdr(fun, rest, indices));
        } else {
          us_rplacd(lcons, cons(rsub, nil));
        }

        us_rplaca(lcons, first);
      }
    } else {
      us_rplaca(lcons, seq);
      us_rplacd(lcons, nil);
    }
    break;
  }

  return nil;
}

static val partition_split_common(val seq, val indices,
                                  val (*split_fptr)(val env, val lcons))
{
  seq = nullify(seq);

  if (!seq)
    return nil;

  if (functionp(indices))
    indices = funcall1(indices, seq);

  indices = nullify(indices);

  if (!indices)
    return cons(seq, nil);

  if (!seqp(indices))
    indices = cons(indices, nil);

  return make_lazy_cons_car_cdr(func_f1(zero, split_fptr), seq, indices);
}

val partition(val seq, val indices)
{
  return partition_split_common(seq, indices, partition_func);
}

val split(val seq, val indices)
{
  return partition_split_common(seq, indices, split_func);
}

val split_star(val seq, val indices)
{
  return partition_split_common(seq, indices, split_star_func);
}

static val partition_star_func(val base, val lcons)
{
  us_cons_bind (seq, indices, lcons);
  val fun = us_lcons_fun(lcons);
  val len = nil;

  for (;;) {
    if (indices) {
      val raw_index = pop(&indices);
      val index = if3((!opt_compat || opt_compat > 170) && minusp(raw_index),
                      plus(raw_index, if3(len, len, len = length(seq))),
                      raw_index);
      val index_rebased = minus(index, base);
      val first = nullify(sub(seq, zero, index_rebased));

      seq = nullify(sub(seq, plus(index_rebased, one), t));

      base = plus(index, one);

      while (seq && eql(car(indices), base)) {
        seq = nullify(cdr(seq));
        base = plus(base, one);
        pop(&indices);
      }

      if (!first)
        continue;
      us_rplaca(lcons, first);
      us_rplacd(lcons, if2(seq, make_lazy_cons_car_cdr(fun, seq, indices)));
    } else {
      us_rplaca(lcons, seq);
      us_rplacd(lcons, nil);
    }

    us_func_set_env(fun, base);
    break;
  }

  return nil;
}

val partition_star(val seq, val indices)
{
  val base = zero;
  seq = nullify(seq);
  indices = nullify(indices);

  if (!seq)
    return nil;

  if (functionp(indices))
    indices = funcall1(indices, seq);

  if (!indices)
    return cons(seq, nil);

  if (indices == zero)
    return cons(nullify(rest(seq)), nil);

  if (!seqp(indices))
    indices = cons(indices, nil);

  {
    while (indices && eql(car(indices), base)) {
      seq = nullify(cdr(seq));
      if (!seq)
        return nil;
      base = plus(base, one);
      pop(&indices);
    }

    if (!indices)
      return cons(seq, nil);

    return make_lazy_cons_car_cdr(func_f1(base, partition_star_func),
                                  seq, indices);
  }
}

cnum c_num(val num, val self);

val eql(val left, val right)
{
  /* eql is the same as eq except that numbers
     are compared by value, and ranges are
     specially treated also. This means that bignum and
     floating point objects which are distinct are
     treated through the equal function.
     Two ranges are eql if they are the same object,
     or if their corresponding parts are eql. */
  if (left == right)
    return t;

  switch (type(left)) {
  case BGNUM:
  case FLNUM:
    return equal(left, right);
  case RNG:
    if (type(right) == RNG &&
        eql(from(left), from(right)) &&
        eql(to(left), to(right)))
      return t;
    /* fallthrough */
  default:
    return nil;
  }
}

val equal(val left, val right)
{
  val self = lit("equal");

  if (left == right)
    return t;

  switch (type(left)) {
  case NIL:
  case CHR:
  case NUM:
    break;
  case CONS:
    switch (type(right)) {
    case CONS:
      if (equal(left->c.car, right->c.car) && equal(left->c.cdr, right->c.cdr))
        return t;
      return nil;
    case LCONS:
      if (equal(left->c.car, car(right)) && equal(left->c.cdr, right->c.cdr))
        return t;
      return nil;
    default:
      break;
    }
    return nil;
  case LCONS:
    switch (type(right)) {
    case CONS:
      if (equal(car(left), right->c.car) && equal(left->c.cdr, right->c.cdr))
        return t;
      return nil;
    case LCONS:
      if (equal(car(left), car(right)) && equal(left->c.cdr, right->c.cdr))
        return t;
      return nil;
    default:
      break;
    }
    break;
  case LIT:
    switch (type(right)) {
    case LIT:
      return wcscmp(litptr(left), litptr(right)) == 0 ? t : nil;
    case STR:
      return wcscmp(litptr(left), right->st.str) == 0 ? t : nil;
    case LSTR:
      lazy_str_force(right);
      return equal(left, right->ls.prefix);
    case COBJ:
      break;
    default:
      return nil;
    }
    break;
  case STR:
    switch (type(right)) {
    case LIT:
      return wcscmp(left->st.str, litptr(right)) == 0 ? t : nil;
    case STR:
      return wcscmp(left->st.str, right->st.str) == 0 ? t : nil;
    case LSTR:
      lazy_str_force(right);
      return equal(left, right->ls.prefix);
    case COBJ:
      break;
    default:
      return nil;
    }
    break;
  case SYM:
  case PKG:
  case ENV:
    break;
  case FUN:
    if (type(right) == FUN &&
        left->f.functype == right->f.functype &&
        equal(left->f.env, right->f.env))
    {
      switch (left->f.functype) {
      case FINTERP: return (equal(left->f.f.interp_fun, right->f.f.interp_fun));
      case FVM: return t;
      case F0: return (left->f.f.f0 == right->f.f.f0) ? t : nil;
      case F1: return (left->f.f.f1 == right->f.f.f1) ? t : nil;
      case F2: return (left->f.f.f2 == right->f.f.f2) ? t : nil;
      case F3: return (left->f.f.f3 == right->f.f.f3) ? t : nil;
      case F4: return (left->f.f.f4 == right->f.f.f4) ? t : nil;
      case N0: return (left->f.f.n0 == right->f.f.n0) ? t : nil;
      case N1: return (left->f.f.n1 == right->f.f.n1) ? t : nil;
      case N2: return (left->f.f.n2 == right->f.f.n2) ? t : nil;
      case N3: return (left->f.f.n3 == right->f.f.n3) ? t : nil;
      case N4: return (left->f.f.n4 == right->f.f.n4) ? t : nil;
      case N5: return (left->f.f.n5 == right->f.f.n5) ? t : nil;
      case N6: return (left->f.f.n6 == right->f.f.n6) ? t : nil;
      case N7: return (left->f.f.n7 == right->f.f.n7) ? t : nil;
      case N8: return (left->f.f.n8 == right->f.f.n8) ? t : nil;
      }
      return nil;
    }
    break;
  case VEC:
    if (type(right) == VEC) {
      cnum i, length;
      if (!equal(left->v.vec[vec_length], right->v.vec[vec_length]))
        return nil;
      length = c_num(left->v.vec[vec_length], self);
      for (i = 0; i < length; i++) {
        if (!equal(left->v.vec[i], right->v.vec[i]))
          return nil;
      }
      return t;
    }
    break;
  case LSTR:
    switch (type(right)) {
    case LIT:
    case STR:
    case LSTR:
      lazy_str_force(left);
      return equal(left->ls.prefix, right);
    case COBJ:
      break;
    default:
      return nil;
    }
    return nil;
  case BGNUM:
    if (type(right) == BGNUM) {
      if (mp_cmp(mp(left), mp(right)) == MP_EQ)
        return t;
      return nil;
    }
    break;
  case FLNUM:
    if (type(right) == FLNUM) {
      if (left->fl.n == right->fl.n)
        return t;
      return nil;
    }
    break;
  case RNG:
    if (type(right) == RNG) {
      if (equal(from(left), from(right)) &&
          equal(to(left), to(right)))
        return t;
      return nil;
    }
    break;
  case BUF:
    if (type(right) == BUF) {
      cnum ll = c_num(left->b.len, self);
      cnum rl = c_num(right->b.len, self);
      if (ll == rl && memcmp(left->b.data, right->b.data, ll) == 0)
        return t;
    }
    break;
  case TNOD:
    if (type(right) == TNOD) {
      if (equal(left->tn.key, right->tn.key) &&
          equal(left->tn.left, right->tn.left) &&
          equal(left->tn.right, right->tn.right))
        return t;
      return nil;
    }
    break;
  case COBJ:
    if (left->co.ops->equalsub) {
      val lsub = left->co.ops->equalsub(left);
      if (lsub)
        return equal(lsub, right);
    }

    if (type(right) == COBJ && left->co.ops == right->co.ops)
      return left->co.ops->equal(left, right);

    return nil;
  case CPTR:
    if (type(right) == CPTR && left->co.ops == right->co.ops)
      return left->co.ops->equal(left, right);
  case DARG:
    break;
  }

  if (type(right) != COBJ)
    return nil;

  if (right->co.ops->equalsub) {
    val rsub = right->co.ops->equalsub(right);
    if (rsub)
      return equal(left, rsub);
  }

  return nil;
}

val meq(val item, varg args)
{
  cnum index = 0;
  while (args_more(args, index))
    if (eq(item, args_get(args, &index)))
      return t;
  return nil;
}

val meql(val item, varg args)
{
  cnum index = 0;
  while (args_more(args, index))
    if (eql(item, args_get(args, &index)))
      return t;
  return nil;
}

val mequal(val item, varg args)
{
  cnum index = 0;
  while (args_more(args, index))
    if (equal(item, args_get(args, &index)))
      return t;
  return nil;
}

alloc_bytes_t malloc_bytes;

static void oom(void)
{
  uw_throwf(alloc_error_s, lit("out of memory"), nao);
}

mem_t *chk_malloc(size_t size)
{
  mem_t *ptr = convert(mem_t *, malloc(size));

  assert (!async_sig_enabled);

  if (size && ptr == 0)
    oom();
  malloc_bytes += size;
  return ptr;
}

#if HAVE_POSIX_MEMALIGN

static void *memalign(size_t align, size_t size)
{
  void *ptr;
  if (posix_memalign(&ptr, align, size) == 0)
    return ptr;
  return 0;
}

#elif !HAVE_MEMALIGN

static void *memalign(size_t align, size_t size)
{
  (void) align;
  return malloc(size);
}

#endif

mem_t *chk_malloc_gc_more(size_t size)
{
  mem_t *ptr = convert(mem_t *, memalign(sizeof (obj_t), size));
  assert (!async_sig_enabled);
  if (size && ptr == 0)
    oom();
  return ptr;
}

mem_t *chk_calloc(size_t n, size_t size)
{
  mem_t *ptr = convert(mem_t *, calloc(n, size));
  alloc_bytes_t total = convert(alloc_bytes_t, size) * n;

  assert (!async_sig_enabled);

  if (size && ptr == 0)
    oom();
  malloc_bytes += total;
  return ptr;
}

mem_t *chk_realloc(mem_t *old, size_t size)
{
  mem_t *newptr = convert(mem_t *, realloc(old, size));

  assert (!async_sig_enabled);

  if (size != 0 && newptr == 0)
    oom();
  malloc_bytes += size;
  return newptr;
}

mem_t *chk_grow_vec(mem_t *old, size_t oldelems, size_t newelems,
                    size_t elsize)
{
  static const size_t no_oflow = convert(size_t, -1) >> (CHAR_BIT * sizeof(size_t) / 2);
  size_t bytes = newelems * elsize;

  if (elsize == 0)
    internal_error("elsize == 0");

  if (newelems <= oldelems ||
      ((newelems > no_oflow || elsize > no_oflow) && bytes / elsize != newelems))
    uw_throw(error_s, lit("array size overflow"));

  return chk_realloc(old, bytes);
}

static size_t bounding_pow_two(size_t s)
{
  s -= 1;

  s |= (s >> 1);
  s |= (s >> 2);
  s |= (s >> 4);
  s |= (s >> 8);
  s |= (s >> 16);

  if (sizeof (size_t) * CHAR_BIT > 32) {
    size_t t = s >> 16; /* suppress warning about s >> 32 */
    s |= (t >> 16);
  }

  return s + 1;
}

mem_t *chk_manage_vec(mem_t *old, size_t oldfilled, size_t newfilled,
                      size_t elsize, mem_t *fillval)
{
  if (newfilled == 0) {
    free(old);
    return 0;
  } else {
    static const size_t no_oflow = convert(size_t, -1) >> (CHAR_BIT * sizeof(size_t) / 2);
    size_t oldbytes = oldfilled * elsize;
    size_t newbytes = newfilled * elsize;
    size_t oldsize = bounding_pow_two(oldbytes);
    size_t newsize = bounding_pow_two(newbytes);

    if (((newfilled > no_oflow || elsize > no_oflow) &&
         newbytes / elsize != newfilled) ||
        (newsize < newbytes))
      uw_throw(error_s, lit("array size overflow"));

    if (oldsize != newsize)
      old = chk_realloc(old, newsize);

    if (fillval)
      while (oldbytes < newbytes) {
        memcpy(old + oldbytes, fillval, elsize);
        oldbytes += elsize;
      }

    return old;
  }
}

wchar_t *chk_wmalloc(size_t nwchar)
{
  return coerce(wchar_t *, chk_xalloc(nwchar, sizeof (wchar_t),
                                      lit("string operation")));
}

wchar_t *chk_wrealloc(wchar_t *old, size_t nwchar)
{
  size_t size = nwchar * sizeof (wchar_t);
  if (size < nwchar)
    uw_throw(error_s, lit("string size overflow"));
  return coerce(wchar_t *, chk_realloc(coerce(mem_t *, old),
                                       sizeof (wchar_t) * nwchar));
}

wchar_t *chk_strdup(const wchar_t *str)
{
  size_t nchar = wcslen(str) + 1;
  wchar_t *copy = chk_wmalloc(nchar);
  wmemcpy(copy, str, nchar);
  return copy;
}

wchar_t *chk_substrdup(const wchar_t *str, size_t off, size_t len)
{
  size_t size = wcslen(str) + 1, nchar;
  wchar_t *copy;
  if (off >= size - 1)
    return chk_strdup(L"");
  if (off + len < off)
    uw_throw(error_s, lit("string size overflow"));
  nchar = min(size - off, len + 1);
  copy = chk_wmalloc(nchar);
  wmemcpy(copy, str, nchar - 1);
  copy[nchar - 1] = 0;
  return copy;
}

char *chk_strdup_utf8(const char *str)
{
  size_t nchar = strlen(str) + 1;
  char *copy = coerce(char *, chk_malloc(nchar));
  memcpy(copy, str, nchar);
  return copy;
}

unsigned char *chk_strdup_8bit(const wchar_t *str)
{
  size_t nchar = wcslen(str) + 1, i;
  unsigned char *copy = coerce(unsigned char *, chk_malloc(nchar));
  for (i = 0; i < nchar; i++) {
    if (str[i] < 0 || str[i] > 255) {
      free(copy);
      uw_throwf(error_s, lit("cannot coerce ~s to 8 bit"),
                string(str), nao);
    }
    copy[i] = str[i];
  }
  return copy;
}

mem_t *chk_copy_obj(mem_t *orig, size_t size)
{
  mem_t *copy = chk_malloc(size);
  memcpy(copy, orig, size);
  return copy;
}

mem_t *chk_xalloc(ucnum m, ucnum n, val self)
{
  ucnum mn = m * n;
  size_t size = mn;

  if ((m > 0 && mn / m != n) || (ucnum) size != mn)
    uw_throwf(error_s, lit("~a: memory allocation size overflow"),
              self, nao);

  return chk_malloc(size);
}

val cons(val car, val cdr)
{
  val obj;

  if (recycled_conses) {
    obj = recycled_conses;
    recycled_conses = recycled_conses->c.cdr;
    setcheck(obj, car);
    setcheck(obj, cdr);
  } else {
    obj = make_obj();
    obj->c.type = CONS;
  }

  obj->c.car = car;
  obj->c.cdr = cdr;
  return obj;
}

val make_lazy_cons(val func)
{
  val obj = make_obj();
  obj->lc.type = LCONS;
  obj->lc.car = obj->lc.cdr = nil;
  obj->lc.func = func;
  return obj;
}

val make_lazy_cons_car(val func, val car)
{
  val obj = make_obj();
  obj->lc.type = LCONS;
  obj->lc.car = car;
  obj->lc.cdr = nil;
  obj->lc.func = func;
  return obj;
}

val make_lazy_cons_car_cdr(val func, val car, val cdr)
{
  val obj = make_obj();
  obj->lc.type = LCONS;
  obj->lc.car = car;
  obj->lc.cdr = cdr;
  obj->lc.func = func;
  return obj;
}

val make_lazy_cons_pub(val func, val car, val cdr)
{
  return make_lazy_cons_car_cdr(func, default_null_arg(car), default_null_arg(cdr));
}

val lcons_car(val lcons)
{
  type_check(lit("lcons-car"), lcons, LCONS);
  return lcons->lc.car;
}

val lcons_cdr(val lcons)
{
  type_check(lit("lcons-cdr"), lcons, LCONS);
  return lcons->lc.cdr;
}

void rcyc_cons(val cons)
{
  cons->c.cdr = recycled_conses;
  cons->c.car = nil;
  recycled_conses = cons;
}

void rcyc_list(val list)
{
  if (list) {
    val rl_orig = recycled_conses;
    recycled_conses = list;

    while (list->c.cdr)
      list = list->c.cdr;

    list->c.cdr = rl_orig;
  }
}

void rcyc_empty(void)
{
  recycled_conses = nil;
}

val lcons_fun(val lcons)
{
  type_check(lit("lcons-fun"), lcons, LCONS);
  return lcons->lc.func;
}

val list(val first, ...)
{
  va_list vl;
  val list = nil;
  val array[32], *ptr = array;

  if (first != nao) {
    val next = first;

    va_start (vl, first);

    do {
      *ptr++ = next;
      if (ptr == array + 32)
        internal_error("runaway arguments in list function");
      next = va_arg(vl, val);
    } while (next != nao);

    va_end (vl);

    while (ptr > array)
      list = cons(*--ptr, list);
  }

  return list;
}

val listv(struct args *args)
{
  return args_get_list(args);
}

val consp(val obj)
{
  type_t ty = type(obj);
  return (ty == CONS || ty == LCONS) ? t : nil;
}

val lconsp(val obj)
{
  return type(obj) == LCONS ? t : nil;
}

val atom(val obj)
{
  return if3(consp(obj), nil, t);
}

val listp(val obj)
{
  return if2(obj == nil || consp(obj), t);
}

val endp(val obj)
{
  if (obj == nil)
    return t;
  if (consp(obj))
    return nil;
  uw_throwf(error_s, lit("endp: list improperly terminated by ~s"),
            obj, nao);
}

val proper_list_p(val obj)
{
  while (consp(obj))
    obj = cdr(obj);

  return (obj == nil) ? t : nil;
}

val length_list(val list)
{
  cnum len = 0;
  val bn_len;

  gc_hint(list);

  while (consp(list) && len < INT_PTR_MAX) {
    len++;
    list = cdr(list);
  }

  if (len < INT_PTR_MAX)
    return num(len);

  list = cdr(list);
  bn_len = num(INT_PTR_MAX);

  while (consp(list)) {
    bn_len = succ(bn_len);
    list = cdr(list);
  }

  return bn_len;
}

static val length_proper_list(val list)
{
  cnum len = 0;
  val bn_len;

  gc_hint(list);

  while (list && len < INT_PTR_MAX) {
    len++;
    list = cdr(list);
  }

  if (len < INT_PTR_MAX)
    return num(len);

  list = cdr(list);
  bn_len = num(INT_PTR_MAX);

  while (list) {
    bn_len = succ(bn_len);
    list = cdr(list);
  }

  return bn_len;
}

val getplist(val list, val key)
{
  gc_hint(list);

  for (; list; list = cdr(cdr(list))) {
    val ind = first(list);
    if (ind == key)
      return second(list);
  }

  return nil;
}

val getplist_f(val list, val key, loc found)
{
  gc_hint(list);

  for (; list; list = cdr(cdr(list))) {
    val ind = first(list);
    if (ind == key) {
      deref(found) = t;
      return second(list);
    }
  }

  deref(found) = nil;
  return nil;
}

val memp(val key, val plist)
{
  for (; plist; plist = cddr(plist)) {
    if (car(plist) == key)
      return plist;
  }
  return nil;
}

val plist_to_alist(val list)
{
  list_collect_decl (out, ptail);

  for (; list; list = cdr(cdr(list))) {
    val ind = first(list);
    val prop = second(list);
    ptail = list_collect(ptail, cons(ind, prop));
  }

  return out;
}

val improper_plist_to_alist(val list, val boolean_keys)
{
  list_collect_decl (out, ptail);

  for (; list; list = cdr(list)) {
    val ind = first(list);

    if (memqual(ind, boolean_keys)) {
      ptail = list_collect(ptail, cons(ind, t));
    } else {
      val prop = second(list);
      ptail = list_collect(ptail, cons(ind, prop));
      list = cdr(list);
    }
  }

  return out;
}

val max2(val a, val b)
{
  return if3(less(a, b), b, a);
}

val min2(val a, val b)
{
  return if3(less(a, b), a, b);
}

val maxv(val first, struct args *rest)
{
  return nary_simple_op(max2, rest, first);
}

val minv(val first, struct args *rest)
{
  return nary_simple_op(min2, rest, first);
}

val maxl(val first, val rest)
{
  args_decl_list(args, ARGS_MIN, rest);
  return maxv(first, args);
}

val minl(val first, val rest)
{
  args_decl_list(args, ARGS_MIN, rest);
  return minv(first, args);
}

val clamp(val low, val high, val num)
{
  return max2(low, min2(high, num));
}

val bracket(val larg, struct args *args)
{
  cnum index = 0, i = 0;

  for (; args_more(args, index); i++) {
    val rarg = args_get(args, &index);
    if (less(larg, rarg))
      return num(i);
  }

  return num(i);
}

val string_own(wchar_t *str)
{
  val obj = make_obj();
  obj->st.type = STR;
  obj->st.str = str;
  obj->st.len = nil;
  obj->st.alloc = nil;
  return obj;
}

val string(const wchar_t *str)
{
  val obj = make_obj();
  obj->st.type = STR;
  obj->st.str = chk_strdup(str);
  obj->st.len = nil;
  obj->st.alloc = nil;
  return obj;
}

val string_utf8(const char *str)
{
  val obj = make_obj();
  obj->st.type = STR;
  obj->st.str = utf8_dup_from(str);
  obj->st.len = nil;
  obj->st.alloc = nil;
  return obj;
}

val string_8bit(const unsigned char *str)
{
  size_t l = strlen(coerce(const char *, str)), i;
  wchar_t *wstr = chk_wmalloc(l + 1);
  for (i = 0; i <= l; i++)
    wstr[i] = str[i];
  return string_own(wstr);
}

val string_8bit_size(const unsigned char *str, size_t sz)
{
  size_t i;
  wchar_t *wstr = chk_wmalloc(sz + 1);
  for (i = 0; i < sz; i++)
    wstr[i] = str[i];
  wstr[i] = 0;
  return string_own(wstr);
}

val mkstring(val len, val ch_in)
{
  val self = lit("mkstring");
  size_t l = if3(minusp(len),
                 (uw_throwf(error_s, lit("~a: negative size ~s specified"),
                            self, len, nao), 0),
                 c_num(len, self));
  wchar_t *str = chk_wmalloc(l + 1);
  val s = string_own(str);
  val ch = default_arg_strict(ch_in, chr(' '));
  wmemset(str, c_chr(ch), l);
  str[l] = 0;
  s->st.len = len;
  s->st.alloc = plus(len, one);
  return s;
}

val mkustring(val len)
{
  val self = lit("mkustring");
  cnum l = if3(minusp(len),
               (uw_throwf(error_s, lit("~a: negative size ~s specified"),
                          len, nao), 0),
               c_num(len, self));
  wchar_t *str = chk_wmalloc(l + 1);
  val s = string_own(str);
  str[l] = 0;
  s->st.len = len;
  s->st.alloc = plus(len, one);
  return s;
}

val init_str(val str, const wchar_t *data, val self)
{
  wmemcpy(str->st.str, data, c_num(str->st.len, self));
  return str;
}

static val copy_lazy_str(val lstr);

val copy_str(val str)
{
  val self = lit("copy-str");
  return if3(lazy_stringp(str),
             copy_lazy_str(str),
             string(c_str(str, self)));
}

val upcase_str(val str)
{
  val self = lit("upcase-str");
  val len = length_str(str);
  wchar_t *dst = chk_wmalloc(c_unum(len, self) + 1);
  const wchar_t *src = c_str(str, self);
  val out = string_own(dst);

  while ((*dst++ = towupper(*src++)))
    ;

  return out;
}

val downcase_str(val str)
{
  val self = lit("downcase-str");
  val len = length_str(str);
  wchar_t *dst = chk_wmalloc(c_unum(len, self) + 1);
  const wchar_t *src = c_str(str, self);
  val out = string_own(dst);

  while ((*dst++ = towlower(*src++)))
    ;

  return out;
}

val string_extend(val str, val tail)
{
  val self = lit("string-extend");

  type_check(self, str, STR);
  {
    cnum len = c_fixnum(length_str(str), self);
    cnum oalloc = c_fixnum(str->st.alloc, self), alloc = oalloc;
    cnum delta, needed;

    if (stringp(tail))
      delta = c_fixnum(length_str(tail), self);
    else if (chrp(tail))
      delta = 1;
    else if (integerp(tail))
      delta = c_fixnum(tail, self);
    else
      uw_throwf(error_s, lit("~a: tail ~s bad type"), self, str, nao);

    if (NUM_MAX - delta - 1 < len)
      uw_throwf(error_s, lit("~a: overflow"), self, nao);

    needed = len + delta + 1;

    if (needed > alloc) {
      if (alloc >= (NUM_MAX - NUM_MAX / 5))
        alloc = NUM_MAX;
      else
        alloc = max(alloc + alloc / 4, needed);

      if (alloc != oalloc) {
        str->st.str = coerce(wchar_t *,
                             chk_grow_vec(coerce(mem_t *, str->st.str),
                                          oalloc, alloc,
                                          sizeof *str->st.str));
        set(mkloc(str->st.alloc, str), num_fast(alloc));
      }
    }

    set(mkloc(str->st.len, str), num_fast(len + delta));

    if (stringp(tail)) {
      wmemcpy(str->st.str + len, c_str(tail, self), delta + 1);
    } else if (chrp(tail)) {
      str->st.str[len] = c_chr(tail);
      str->st.str[len + 1] = 0;
    }
  }

  return str;
}

val stringp(val str)
{
  switch (type(str)) {
  case LIT:
  case STR:
  case LSTR:
    return t;
  default:
    return nil;
  }
}

val lazy_stringp(val str)
{
  return type(str) == LSTR ? t : nil;
}

val length_str(val str)
{
  val self = lit("length-str");
  switch (type(str)) {
  case LIT:
    return num(wcslen(c_str(str, self)));
  case LSTR:
    lazy_str_force(str);
    return length_str(str->ls.prefix);
  case STR:
    if (!str->st.len) {
      set(mkloc(str->st.len, str), num(wcslen(str->st.str)));
      set(mkloc(str->st.alloc, str), plus(str->st.len, one));
    }
    return str->st.len;
  default:
    type_mismatch(lit("length-str: ~s is not a string"), str, nao);
  }
}

val coded_length(val str)
{
  val self = lit("coded-length");
  return unum(utf8_to_buf(0, c_str(str, self), 0));
}

const wchar_t *c_str(val obj, val self)
{
  switch (type(obj)) {
  case LIT:
    return litptr(obj);
  case STR:
    return obj->st.str;
  case LSTR:
    lazy_str_force(obj);
    return c_str(obj->ls.prefix, self);
  case SYM:
    if (opt_compat && opt_compat <= 231)
      return c_str(symbol_name(obj), self);
    /* fallthrough */
  default:
    self = default_arg(self, lit("internal error"));
    type_mismatch(lit("~a: ~s is not a string"), self, obj, nao);
  }
}

val search_str(val haystack, val needle, val start_num, val from_end)
{
  val self = lit("search-str");
  from_end = default_null_arg(from_end);
  start_num = default_arg(start_num, zero);

  if (length_str_lt(haystack, start_num)) {
    return nil;
  } else {
    val h_is_lazy = lazy_stringp(haystack);
    cnum start = c_num(start_num, self);
    cnum good = -1, pos = -1;
    const wchar_t *n = c_str(needle, self), *h;

    if (!h_is_lazy) {
      h = c_str(haystack, self);

      if (start < 0)
        start += wcslen(h);

    nonlazy:
      do {
        const wchar_t *f = wcsstr(h + start, n);

        if (f)
          pos = f - h;
        else
          pos = -1;
      } while (pos != -1 && (good = pos) != -1 && from_end && h[start++]);
    } else {
      size_t ln = c_num(length_str(needle), self);

      if (start < 0) {
        lazy_str_force(haystack);
        h = c_str(haystack->ls.prefix, self);
        start += wcslen(h);
        goto nonlazy;
      }

      do {
        lazy_str_force_upto(haystack, plus(num(start + 1), length_str(needle)));
        h = c_str(haystack->ls.prefix, self);

        if (!wcsncmp(h + start, n, ln))
          good = start;
      } while (h[start++] && (from_end || good == -1));
    }
    return (good == -1) ? nil : num(good);
  }
}

val search_str_tree(val haystack, val tree, val start_num, val from_end)
{
  if (stringp(tree)) {
    val result = search_str(haystack, tree, start_num, from_end);
    if (result)
      return cons(result, length_str(tree));
  } else if (consp(tree)) {
    val it = nil, minpos = nil, maxlen = nil;

    for (; tree; tree = cdr(tree)) {
      val result = search_str_tree(haystack, car(tree), start_num, from_end);
      if (result) {
        cons_bind (pos, len, result);
        if (!it || lt(pos, minpos) || (pos == minpos && gt(len, maxlen))) {
          minpos = pos;
          maxlen = len;
          it = result;
        }
      }
    }

    return it;
  }

  return nil;
}

static val do_match_str(val bigstr, val str, cnum pos, val self)
{
  switch (TYPE_PAIR(type(bigstr), type(str))) {
  case TYPE_PAIR(LIT, LIT):
  case TYPE_PAIR(LIT, STR):
  case TYPE_PAIR(STR, LIT):
  case TYPE_PAIR(STR, STR):
    {
      cnum bl = c_num(length_str(bigstr), self);
      cnum sl = c_num(length_str(str), self);

      if (sl == 0 && pos <= bl)
        return num(pos);

      if (pos > bl || sl > bl)
        return nil;

      if (pos > INT_PTR_MAX - sl)
        return nil;

      if (pos + sl > bl)
        return nil;

      {
        const wchar_t *bs = c_str(bigstr, self);
        const wchar_t *ss = c_str(str, self);

        return if3(wmemcmp(bs + pos, ss, sl) == 0, num(pos + sl), nil);
      }
    }
  case TYPE_PAIR(LSTR, LIT):
  case TYPE_PAIR(LSTR, STR):
    {
      lazy_str_force_upto(bigstr, num(pos + c_num(length_str(str), self)));
      return do_match_str(bigstr->ls.prefix, str, pos, self);
    }
  case TYPE_PAIR(LIT, LSTR):
  case TYPE_PAIR(STR, LSTR):
    {
      if (length_str_gt(str, length_str(bigstr)))
        return nil;

      lazy_str_force(str);
      return do_match_str(bigstr, str->ls.prefix, pos, self);
    }
  case TYPE_PAIR(LSTR, LSTR):
    {
      cnum i, p;

      for (i = 0;
           length_str_gt(bigstr, num((p = pos + i))) &&
           length_str_gt(str, num(i));
           i++)
      {
        if (chr_str(bigstr, num(p)) != chr_str(str, num(i)))
          return nil;
      }

      return length_str_le(str, num(i)) ? num(p) : nil;
    }
  default:
    invalid_ops(self, bigstr, str);
  }
}

static val do_rmatch_str(val bigstr, val str, cnum pos, val self)
{
  if (type(bigstr) == LSTR) {
    lazy_str_force(bigstr);
    return do_rmatch_str(bigstr->ls.prefix, str, pos, self);
  }

  if (type(str) == LSTR) {
    lazy_str_force(str);
    return do_rmatch_str(bigstr, str->ls.prefix, pos, self);
  }

  {
    cnum bl = c_num(length_str(bigstr), self);
    cnum sl = c_num(length_str(str), self);

    pos += bl;

    if (pos < 0)
      return nil;

    if (sl == 0)
      return num(pos + 1);

    if (sl > pos + 1)
      return nil;

    if (pos > INT_PTR_MAX - sl)
      return nil;

    {
      const wchar_t *bs = c_str(bigstr, self);
      const wchar_t *ss = c_str(str, self);
      cnum start = pos + 1 - sl;
      return if3(wmemcmp(bs + start, ss, sl) == 0, num(start), nil);
    }
  }
}

val match_str(val bigstr, val str, val pos)
{
  val self = lit("match-str");
  cnum p = c_num(default_arg(pos, zero), self);

  return if3(p >= 0,
             do_match_str(bigstr, str, p, self),
             do_rmatch_str(bigstr, str, p, self));
}

val match_str_tree(val bigstr, val tree, val pos)
{
  pos = default_arg(pos, zero);

  if (stringp(tree)) {
    if (match_str(bigstr, tree, pos))
      return length_str(tree);
  } else if (consp(tree)) {
    val maxlen = nil;

    for (; tree; tree = cdr(tree)) {
      val result = match_str_tree(bigstr, car(tree), pos);
      if (result && (!maxlen || gt(result, maxlen)))
        maxlen = result;
    }

    return maxlen;
  }

  return nil;
}

static val lazy_sub_str(val lstr, val from, val to)
{
  val len = nil;
  val len_pfx = length_str(lstr->ls.prefix);

  if (null_or_missing_p(from)) {
    from = zero;
  } else if (from == t) {
    return null_string;
  } else {
    if (minusp(from)) {
      from = plus(from, len = length_str(lstr));
      from = max2(zero, from);

      if (to == zero)
        to = t;
    }

    if (ge(from, len_pfx)) {
      if (!lazy_str_force_upto(lstr, from))
        return null_string;
    }
  }

  if (null_or_missing_p(to) || to == t) {
    to = t;
  } else {
    if (minusp(to)) {
      to = plus(to, len = length_str(lstr));
      to = max(zero, to);
    }

    if (ge(to, len_pfx))
      if (!lazy_str_force_upto(lstr, minus(to, one)))
        to = t;
  }

  {
    val pfxsub = sub_str(lstr->ls.prefix, from, to);

    if (to != t) {
      return pfxsub;
    } else {
      val pfxcopy = copy_str(pfxsub);
      val lsub = make_obj();
      lsub->ls.type = LSTR;
      lsub->ls.prefix = pfxcopy;
      lsub->ls.list = lstr->ls.list;
      lsub->ls.props = coerce(struct lazy_string_props *,
                              chk_copy_obj(coerce(mem_t *, lstr->ls.props),
                                           sizeof *lstr->ls.props));
      return lsub;
    }
  }
}

val sub_str(val str_in, val from, val to)
{
  val self = lit("sub-str");
  val len = nil;

  if (lazy_stringp(str_in))
    return lazy_sub_str(str_in, from, to);

  len = length_str(str_in);

  if (null_or_missing_p(from))
    from = zero;
  else if (from == t)
    return null_string;
  else if (minusp(from)) {
    from = plus(from, len);
    if (to == zero)
      to = len;
  }

  if (null_or_missing_p(to) || to == t)
    to = len;
  else if (minusp(to))
    to = plus(to, len);

  from = max2(zero, min2(from, len));
  to = max2(zero, min2(to, len));

  if (ge(from, to)) {
    return null_string;
  } else if (from == zero && eql(to, len) &&
             (opt_compat == 0 || opt_compat > 215))
  {
    return str_in;
  } else {
    size_t nchar = c_num(to, self) - c_num(from, self) + 1;
    wchar_t *sub = chk_wmalloc(nchar);
    const wchar_t *str = c_str(str_in, self);
    wcsncpy(sub, str + c_num(from, self), nchar);
    sub[nchar-1] = 0;
    return string_own(sub);
  }
}

val replace_str(val str_in, val items, val from, val to)
{
  val self = lit("replace-str");
  val len = length_str(str_in);

  if (type(str_in) != STR) {
    uw_throwf(error_s, lit("~a: ~s of type ~s is not "
                           "a modifiable string"),
              self, str_in, typeof(str_in), nao);
  }

  if (missingp(from)) {
    from = zero;
  } else if (from == t) {
    from = len;
  } else if (!integerp(from)) {
    val wh, item;
    seq_iter_t wh_iter, item_iter;
    seq_iter_init(self, &item_iter, items);
    seq_iter_init(self, &wh_iter, from);

    if (!missingp(to))
      uw_throwf(error_s,
                lit("~a: to-arg not applicable when from-arg is a list"),
                self, nao);

    while (seq_get(&wh_iter, &wh) && seq_get(&item_iter, &item)) {
      if (ge(wh, len))
        break;
      chr_str_set(str_in, wh, item);
    }

    return str_in;
  } else if (minusp(from)) {
    from = plus(from, len);
    if (to == zero)
      to = len;
  }

  if (null_or_missing_p(to) || to == t)
    to = len;
  else if (minusp(to))
    to = plus(to, len);

  from = max2(zero, min2(from, len));
  to = max2(zero, min2(to, len));


  {
    val len_rep = minus(to, from);
    val len_it = length(items);

    if (gt(len_rep, len_it)) {
      val len_diff = minus(len_rep, len_it);
      cnum t = c_num(to, self);
      cnum l = c_num(len, self);

      wmemmove(str_in->st.str + t - c_num(len_diff, self),
               str_in->st.str + t, (l - t) + 1);
      set(mkloc(str_in->st.len, str_in), minus(len, len_diff));
      to = plus(from, len_it);
    } else if (lt(len_rep, len_it)) {
      val len_diff = minus(len_it, len_rep);
      cnum t = c_num(to, self);
      cnum l = c_num(len, self);

      string_extend(str_in, len_diff);
      wmemmove(str_in->st.str + t + c_num(len_diff, self),
               str_in->st.str + t, (l - t) + 1);
      to = plus(from, len_it);
    }

    if (zerop(len_it))
      return str_in;
    if (stringp(items)) {
      wmemmove(str_in->st.str + c_num(from, self),
               c_str(items, self), c_num(len_it, self));
    } else {
      seq_iter_t item_iter;
      seq_iter_init(self, &item_iter, items);
      cnum f = c_num(from, self);
      cnum t = c_num(to, self);

      for (; f != t; f++)
        str_in->st.str[f] = c_chr(seq_geti(&item_iter));
    }
  }

  return str_in;
}

struct cat_str {
  val sep;
  cnum len_sep;
  size_t total;
  wchar_t *str, *ptr;
};

static void cat_str_init(struct cat_str *cs, val sep, wchar_t *onech, val self)
{
  cs->sep = sep;
  cs->total = 1;
  cs->str = cs->ptr = 0;

  if (null_or_missing_p(sep)) {
    cs->len_sep = 0;
  } else if (chrp(sep)) {
    onech[0] = c_chr(sep);
    cs->len_sep = 1;
    cs->sep = auto_str(coerce(const wchli_t *, wref(onech)));
  } else {
    cs->len_sep = c_num(length_str(cs->sep), self);
  }
}

static void cat_str_measure(struct cat_str *cs, val item, int more_p, val self)
{
  if (!item)
    return;

  if (stringp(item)) {
    size_t ntotal = cs->total + c_num(length_str(item), self);

    if (cs->len_sep && more_p)
      ntotal += cs->len_sep;

    if (ntotal < cs->total)
      goto oflow;

    cs->total = ntotal;
    return;
  }

  if (chrp(item)) {
    size_t ntotal = cs->total + 1;

    if (cs->len_sep && more_p)
      ntotal += cs->len_sep;

    if (ntotal < cs->total)
      goto oflow;

    cs->total = ntotal;
    return;
  }

  uw_throwf(error_s, lit("~a: ~s is not a character or string"), self,
            item, nao);
oflow:
  uw_throwf(error_s, lit("~a: string length overflow"), self, nao);
}

static void cat_str_alloc(struct cat_str *cs)
{
  cs->ptr = cs->str = chk_wmalloc(cs->total);
}

static void cat_str_append(struct cat_str *cs, val item, int more_p, val self)
{
  if (!item)
    return;
  if (stringp(item)) {
    cnum len = c_num(length_str(item), self);
    wmemcpy(cs->ptr, c_str(item, self), len);
    cs->ptr += len;
  } else {
    *cs->ptr++ = c_chr(item);
  }

  if (cs->len_sep && more_p) {
    wmemcpy(cs->ptr, c_str(cs->sep, self), cs->len_sep);
    cs->ptr += cs->len_sep;
  }
}

static val cat_str_get(struct cat_str *cs)
{
  *cs->ptr = 0;
  return string_own(cs->str);
}

val cat_str(val items, val sep)
{
  val self = lit("cat-str");
  seq_iter_t item_iter;
  val item, peek = nil;
  int more = 0;
  struct cat_str cs;
  wchar_t onech[] = wini(" ");


  cat_str_init(&cs, sep, onech, self);

  seq_iter_init(self, &item_iter, items);
  more = seq_get(&item_iter, &item);
  while (more)
  {
    cat_str_measure(&cs, item, more = seq_get(&item_iter, &peek), self);
    item = peek;
  }

  cat_str_alloc(&cs);

  seq_iter_init(self, &item_iter, items);
  more = seq_get(&item_iter, &item);
  while (more)
  {
    cat_str_append(&cs, item, more = seq_get(&item_iter, &peek), self);
    item = peek;
  }

  return cat_str_get(&cs);
}

static val vscat(val sep, va_list vl1, va_list vl2, val self)
{
  val item, next;
  struct cat_str cs;
  wchar_t onech[] = wini(" ");

  cat_str_init(&cs, sep, onech, self);

  for (item = va_arg(vl1, val); item != nao; item = next)
  {
    next = va_arg(vl1, val);
    cat_str_measure(&cs, item, next != nao, self);
  }

  cat_str_alloc(&cs);

  for (item = va_arg(vl2, val); item != nao; item = next)
  {
    next = va_arg(vl2, val);
    cat_str_append(&cs, item, next != nao, self);
  }

  return cat_str_get(&cs);
}

val scat(val sep, ...)
{
  val self = lit("scat");
  va_list vl1, vl2;
  val ret;
  va_start (vl1, sep);
  va_start (vl2, sep);
  ret = vscat(sep, vl1, vl2, self);
  va_end (vl1);
  va_end (vl2);
  return ret;
}

val scat2(val s1, val s2)
{
  val self = lit("scat2");
  struct cat_str cs;

  cat_str_init(&cs, nil, NULL, self);

  cat_str_measure(&cs, s1, 1, self);
  cat_str_measure(&cs, s2, 0, self);

  cat_str_alloc(&cs);

  cat_str_append(&cs, s1, 1, self);
  cat_str_append(&cs, s2, 0, self);

  return cat_str_get(&cs);
}

val scat3(val s1, val sep, val s2)
{
  val self = lit("scat3");
  struct cat_str cs;
  wchar_t onech[] = wini(" ");

  cat_str_init(&cs, sep, onech, self);

  cat_str_measure(&cs, s1, 1, self);
  cat_str_measure(&cs, s2, 0, self);

  cat_str_alloc(&cs);

  cat_str_append(&cs, s1, 1, self);
  cat_str_append(&cs, s2, 0, self);

  return cat_str_get(&cs);
}

val join_with(val sep, struct args *args)
{
  val self = lit("join-with");
  cnum index;
  val iter;
  int more;
  struct cat_str cs;
  wchar_t onech[] = wini(" ");

  cat_str_init(&cs, sep, onech, self);

  for (index = 0, iter = args->list, more = args_more_nozap(args, index, iter);
       more;)
  {
    val item = args_get_nozap(args, &index, &iter);
    cat_str_measure(&cs, item, more = args_more_nozap(args, index, iter), self);
  }

  cat_str_alloc(&cs);

  for (index = 0, iter = args->list, more = args_more_nozap(args, index, iter);
       more;)
  {
    val item = args_get_nozap(args, &index, &iter);
    cat_str_append(&cs, item, more = args_more_nozap(args, index, iter), self);
  }

  return cat_str_get(&cs);
}

val fmt_join(struct args *args)
{
  return join_with(nil, args);
}

val split_str_keep(val str, val sep, val keep_sep)
{
  val self = lit("split-str");
  keep_sep = default_null_arg(keep_sep);

  if (regexp(sep)) {
    list_collect_decl (out, iter);
    val pos = zero;
    val slen = length(str);

    for (;;) {
      cons_bind (new_pos, len, search_regex(str, sep, pos, nil));

      if (len == zero && new_pos != slen)
        new_pos = plus(new_pos, one);

      iter = list_collect(iter, sub_str(str, pos, new_pos));
      pos = new_pos;

      if (len && pos != slen) {
        pos = plus(pos, len);
        if (keep_sep)
          iter = list_collect(iter, sub_str(str, new_pos, pos));
        continue;
      }
      break;
    }

    return out;
  } else {
    size_t len_sep;
    wchar_t onech[] = wini(" ");

    if (chrp(sep)) {
      wref(onech)[0] = c_chr(sep);
      len_sep = 1;
      sep = auto_str(coerce(const wchli_t *, wref(onech)));
    } else {
      len_sep = c_num(length_str(sep), self);
    }

    if (len_sep == 0) {
      if (opt_compat && opt_compat <= 100) {
        return list_str(str);
      } else {
        const wchar_t *cstr = c_str(str, self);

        if (*cstr) {
          list_collect_decl (out, iter);

          for (; *cstr; cstr++) {
            val piece = mkustring(one);
            init_str(piece, cstr, self);
            iter = list_collect(iter, piece);
            if (keep_sep && *(cstr+1))
              iter = list_collect(iter, null_string);
          }

          gc_hint(str);

          return out;
        } else {
          return cons(str, nil);
        }
      }
    } else {
      const wchar_t *cstr = c_str(str, self);
      const wchar_t *csep = c_str(sep, self);

      list_collect_decl (out, iter);

      for (;;) {
        const wchar_t *psep = wcsstr(cstr, csep);
        size_t span = (psep != 0) ? (size_t) (psep - cstr) : wcslen(cstr);
        val piece = mkustring(num(span));
        init_str(piece, cstr, self);
        iter = list_collect(iter, piece);
        cstr += span;
        if (psep != 0) {
          cstr += len_sep;
          if (keep_sep)
            iter = list_collect(iter, sep);
          continue;
        }
        break;
      }

      gc_hint(sep);
      gc_hint(str);

      return out;
    }
  }
}

val spl(val sep, val arg1, val arg2)
{
  return if3(missingp(arg2),
             split_str_keep(arg1, sep, arg2),
             split_str_keep(arg2, sep, arg1));
}

val split_str(val str, val sep)
{
  return split_str_keep(str, sep, nil);
}

val split_str_set(val str, val set)
{
  val self = lit("split-str-set");
  const wchar_t *cstr = c_str(str, self);
  const wchar_t *cset = c_str(set, self);
  list_collect_decl (out, iter);

  for (;;) {
    size_t span = wcscspn(cstr, cset);
    val piece = mkustring(num(span));
    init_str(piece, cstr, self);
    iter = list_collect(iter, piece);
    cstr += span;
    if (*cstr) {
      cstr++;
      continue;
    }
    break;
  }

  gc_hint(set);
  gc_hint(str);

  return out;
}

val sspl(val set, val str)
{
  return split_str_set(str, set);
}

val tok_str(val str, val tok_regex, val keep_sep)
{
  list_collect_decl (out, iter);
  val pos = zero;
  val last_end = zero;
  val slen = length(str);
  int prev_empty = 1;

  keep_sep = default_null_arg(keep_sep);

  if (opt_compat && opt_compat <= 155) for (;;) {
    cons_bind (new_pos, len, search_regex(str, tok_regex, pos, nil));

    if (len == zero && new_pos != slen)
      new_pos = plus(new_pos, one);

    if (new_pos == slen || !len) {
      if (keep_sep)
        iter = list_collect(iter, sub_str(str, pos, t));
      break;
    }

    if (keep_sep)
      iter = list_collect(iter, sub_str(str, pos, new_pos));

    pos = plus(new_pos, len);
    iter = list_collect(iter, sub_str(str, new_pos, pos));
  } else for (;;) {
    cons_bind (new_pos, len, search_regex(str, tok_regex, pos, nil));

    if (!len || (new_pos == slen && !prev_empty)) {
      if (keep_sep)
        iter = list_collect(iter, sub_str(str, last_end, t));
      break;
    }

    if (len != zero || prev_empty) {
      if (keep_sep)
        iter = list_collect(iter, sub_str(str, last_end, new_pos));
      last_end = plus(new_pos, len);
      iter = list_collect(iter, sub_str(str, new_pos, last_end));
      prev_empty = (len == zero);
    } else {
      prev_empty = 1;
    }

    pos = plus(new_pos, len);

    if (len == zero)
      pos = succ(pos);
  }

  return out;
}

val tok(val tok_regex, val arg1, val arg2)
{
  return if3(missingp(arg2),
             tok_str(arg1, tok_regex, arg2),
             tok_str(arg2, tok_regex, arg1));
}

val tok_where(val str, val tok_regex)
{
  list_collect_decl (out, iter);
  val pos = zero;
  int prev_empty = 1;

  if (opt_compat && opt_compat <= 155) for (;;) {
    val range = range_regex(str, tok_regex, pos, nil);

    if (range) {
      range_bind (match_start, match_end, range);

      iter = list_collect(iter, range);

      pos = match_end;

      if (match_end == match_start)
        pos = plus(pos, one);
      continue;
    }

    break;
  } else for (;;) {
    val range = range_regex(str, tok_regex, pos, nil);

    if (range) {
      range_bind (match_start, match_end, range);

      if (match_end != match_start || prev_empty) {
        iter = list_collect(iter, range);
        prev_empty = (match_end == match_start);
      } else {
        prev_empty = 1;
      }

      pos = match_end;

      if (match_end == match_start)
        pos = plus(pos, one);
      continue;
    }

    break;
  }

  return out;
}

val list_str(val str)
{
  val self = lit("list-str");
  const wchar_t *cstr = c_str(str, self);
  list_collect_decl (out, iter);

  while (*cstr)
    iter = list_collect(iter, chr(*cstr++));

  gc_hint(str);

  return out;
}

val trim_str(val str)
{
  val self = lit("trim-str");
  const wchar_t *start = c_str(str, self);
  const wchar_t *end = start + c_num(length_str(str), self);

  if (opt_compat && opt_compat <= 148) {
    while (start[0] && iswspace(start[0]))
      start++;

    while (end > start && iswspace(end[-1]))
      end--;
  } else {
    while (start[0] && wcschr(L" \t\n", start[0]))
      start++;

    while (end > start && wcschr(L" \t\n", end[-1]))
      end--;
  }

  if (end == start) {
    return null_string;
  } else {
    size_t len = end - start;
    wchar_t *buf = chk_wmalloc(len + 1);
    wmemcpy(buf, start, len);
    buf[len] = 0;
    return string_own(buf);
  }
}

val cmp_str(val astr, val bstr)
{
  val self = lit("bstr");

  switch (TYPE_PAIR(type(astr), type(bstr))) {
  case TYPE_PAIR(LIT, LIT):
  case TYPE_PAIR(STR, STR):
  case TYPE_PAIR(LIT, STR):
  case TYPE_PAIR(STR, LIT):
    {
      int cmp = wcscmp(c_str(astr, self), c_str(bstr, self));
      return if3(cmp < 0, negone, if3(cmp > 0, one, zero));
    }
  case TYPE_PAIR(LSTR, LIT):
  case TYPE_PAIR(LSTR, STR):
  case TYPE_PAIR(LIT, LSTR):
  case TYPE_PAIR(STR, LSTR):
  case TYPE_PAIR(LSTR, LSTR):
    {
      val i;
      for (i = zero;
           length_str_lt(astr, i) && length_str_lt(bstr, i);
           i = plus(i, one))
      {
        val ach = chr_str(astr, i);
        val bch = chr_str(bstr, i);

        if (ach < bch)
          return one;
        else if (ach < bch)
          return one;
      }
      if (length_str_lt(bstr, i))
        return negone;
      if (length_str_lt(astr, i))
        return negone;
      return zero;
    }
  default:
    uw_throwf(error_s, lit("cmp-str: invalid operands ~s ~s"),
              astr, bstr, nao);
  }
}

val str_eq(val astr, val bstr)
{
  return if2(cmp_str(astr, bstr) == zero, t);
}

val str_lt(val astr, val bstr)
{
  return if2(cmp_str(astr, bstr) == negone, t);
}

val str_gt(val astr, val bstr)
{
  return if2(cmp_str(astr, bstr) == one, t);
}

val str_le(val astr, val bstr)
{
  val cmp = cmp_str(astr, bstr);
  return if2(cmp == zero || cmp == negone, t);
}

val str_ge(val astr, val bstr)
{
  val cmp = cmp_str(astr, bstr);
  return if2(cmp == zero || cmp == one, t);
}

val int_str(val str, val base)
{
  val self = lit("int-str");
  const wchar_t *wcs = c_str(str, self);
  wchar_t *ptr;
  long value;
  cnum b = c_num(default_arg(base, num_fast(10)), self);
  int zerox = 0, octzero = 0, minus = 0, flip = 0;

  switch (wcs[0]) {
  case '-':
    minus = 1;
    /* fallthrough */
  case '+':
    switch (wcs[1]) {
    case '0':
      switch (wcs[2]) {
      case 'x': case 'X':
        zerox = 1;
        wcs += 3;
        flip = minus;
        break;
      default:
        octzero = 1;
        break;
      }
    }
    break;
  case '0':
    switch (wcs[1]) {
    case 'x': case 'X':
      zerox = 1;
      wcs += 2;
      break;
    default:
      octzero = 1;
      break;
    }
    break;
  }

  if (base == chr('c')) {
    b = (zerox ? 16 : (octzero ? 8 : 10));
  } else if (b == 16) {
    /* If base is 16, strtoul and its siblings
       still recognize the 0x prefix. We don't want that;
       except if base is the character #\c. Otherwise,
       it is a zero with trailing junk. */
    if (zerox)
      return zero;
  } else if (b < 2 || b > 36) {
     uw_throwf(error_s, lit("~a: invalid base ~s"), self, base, nao);
  }

  /* TODO: detect if we have wcstoll */
  value = wcstol(wcs, &ptr, b);

  if (value == 0 && ptr == wcs)
    return nil;

  if ((value == LONG_MAX || value == LONG_MIN) && errno == ERANGE) {
    val bignum = make_bignum();
    mp_err err = mp_read_radix(mp(bignum), wcs, b);

    gc_hint(str);

    if (err != MP_OKAY)
      return nil;

    if (flip)
      mp_neg(mp(bignum), mp(bignum));
    /* If wcstol overflowed, but the range of long is smaller than
       that of fixnums, that means that the value might not
       actually be a bignum, and so we must normalize.
       We do not need this on our usual target platforms, where NUM_MAX is
       never larger than LONG_MAX. */
    return (LONG_MAX < NUM_MAX) ? normalize(bignum) : bignum;
  }

  if (flip)
    value = -value;

  if (value >= NUM_MIN && value <= NUM_MAX)
    return num(value);

  return bignum_from_long(value);
}

val flo_str_utf8(const char *str)
{
  char *ptr;
  double value;

#if CONFIG_LOCALE_TOLERANCE
  if (dec_point != '.') {
    size_t size = strlen(str) + 1;
    char *scopy = alloca(size), *dot = scopy;
    wmemcpy(scopy, str, size);
    str = scopy;
    while ((dot = strchr(dot, '.')) != 0)
      *dot++ = dec_point;
  }
#endif

  value = strtod(str, &ptr);

  if (value == 0 && ptr == str)
    return nil;
  if ((value == HUGE_VAL || value == -HUGE_VAL) && errno == ERANGE)
    return nil;
  return flo(value);
}

val flo_str(val str)
{
  val self = lit("flo-str");
  const wchar_t *wcs = c_str(str, self);
  wchar_t *ptr;
  double value;

#if CONFIG_LOCALE_TOLERANCE
  if (dec_point != '.') {
    size_t size = c_unum(length_str(str), self) + 1;
    wchar_t *wcopy = alloca(sizeof *wcopy * size), *dot = wcopy;
    wmemcpy(wcopy, wcs, size);
    wcs = wcopy;
    while ((dot = wcschr(dot, '.')) != 0)
      *dot++ = dec_point;
  }
#endif

  value = wcstod(wcs, &ptr);

  if (value == 0 && ptr == wcs)
    return nil;
  if ((value == HUGE_VAL || value == -HUGE_VAL) && errno == ERANGE)
    return nil;
  return flo(value);
}

val num_str(val str)
{
  val self = lit("num-str");
  const wchar_t *wcs = c_str(str, self);
  const wchar_t *nws = wcs + wcsspn(wcs, L"\f\n\r\t\v");
  const wchar_t *dig = nws + wcsspn(wcs, L"+-");

  if (wcsspn(dig, L"0123456789") == wcsspn(dig, L"0123456789eE."))
    return int_str(str, nil);
  return flo_str(str);
}

enum less_handling {
  less_false,
  less_true,
  less_compare,
  less_cannot,
};

static enum less_handling less_tab[MAXTYPE+1][MAXTYPE+1];

static void less_tab_init(void)
{
  int l, r;
  static int type_prec[MAXTYPE+1];

  type_prec[NIL] = 4;
  type_prec[NUM] = 1;
  type_prec[CHR] = 1;
  type_prec[LIT] = 3;
  type_prec[CONS] = 5;
  type_prec[STR] = 3;
  type_prec[SYM] = 4;
  type_prec[LCONS] = 5;
  type_prec[LSTR] = 3;
  type_prec[BGNUM] = 1;
  type_prec[FLNUM] = 1;
  type_prec[RNG] = 2;
  type_prec[BUF] = 6;

  for (l = 0; l <= MAXTYPE; l++)
    for (r = 0; r <= MAXTYPE; r++) {
      int l_prec = type_prec[l];
      int r_prec = type_prec[r];

      if (l_prec == 0 || r_prec == 0)
        less_tab[l][r] = less_cannot;
      else if (l_prec == r_prec)
        less_tab[l][r] = less_compare;
      else if (l_prec < r_prec)
        less_tab[l][r] = less_true;
    }
}

val less(val left, val right)
{
  val self = lit("less");
  type_t l_type, r_type;

  if (left == right)
    return nil;

tail:
  l_type = type(left);
  r_type = type(right);

  if (l_type == COBJ && left->co.ops->equalsub) {
    val lsub = left->co.ops->equalsub(left);
    if (lsub) {
      left = lsub;
      goto tail;
    }
  }

  if (r_type == COBJ && right->co.ops->equalsub) {
    val rsub = right->co.ops->equalsub(right);
    if (rsub) {
      right = rsub;
      goto tail;
    }
  }

  switch (less_tab[l_type][r_type]) {
  case less_false:
    return nil;
  case less_true:
    return t;
  case less_compare:
    break;
  case less_cannot:
    uw_throwf(type_error_s, lit("~a: cannot compare ~s and ~s"),
              self, left, right, nao);
  }

  switch (l_type) {
  case NUM:
  case CHR:
  case BGNUM:
  case FLNUM:
    return lt(left, right);
  case LIT:
  case STR:
  case LSTR:
    return str_lt(left, right);
  case NIL:
    return str_lt(nil_string, symbol_name(right));
  case SYM:
    {
      val cmp = cmp_str(left->s.name, symbol_name(right));
      if (cmp == negone) {
        return t;
      } else if (cmp == one) {
        return nil;
      } else {
        val lpkg = left->s.package;
        val rpkg = right->s.package;

        if (lpkg == nil && rpkg == nil)
          return tnil(left < right);
        if (lpkg == nil)
          return t;
        if (rpkg == nil)
          return nil;

        return str_lt(lpkg->pk.name, rpkg->pk.name);
      }
    }
  case CONS:
  case LCONS:
    for (;;) {
      val carl = car(left);
      val carr = car(right);

      if (less(carl, carr))
        return t;

      if (equal(carl, carr)) {
        val cdrl = cdr(left);
        val cdrr = cdr(right);

        if (consp(cdrl) && consp(cdrr)) {
          left = cdrl;
          right = cdrr;
          continue;
        }

        return less(cdrl, cdrr);
      }
      break;
    }
    return nil;
  case VEC:
    {
      cnum i;
      cnum lenl = c_fixnum(length_vec(left), self);
      cnum lenr = c_fixnum(length_vec(right), self);
      cnum len = min(lenl, lenr);

      for (i = 0; i < len; i++) {
        val litem = vecref(left, num_fast(i));
        val ritem = vecref(right, num_fast(i));

        if (less(litem, ritem))
          return t;

        if (!equal(litem, ritem))
          return nil;
      }

      return tnil(lenl < lenr);
    }
  case RNG:
    {
      val fl = from(left);
      val fr = from(right);

      if (less(fl, fr))
        return t;

      if (equal(fl, fr))
        return less(to(left), to(right));

      return nil;
    }
  case BUF:
    {
      cnum ll = c_num(left->b.len, self);
      cnum rl = c_num(right->b.len, self);
      cnum len = min(ll, rl);
      int cmp = memcmp(left->b.data, right->b.data, len);

      if (cmp < 0 || (cmp == 0 && ll < rl))
        return t;

      return nil;
    }
    break;
  default:
    internal_error("unhandled case in less function");
  }
}

val greater(val left, val right)
{
  return less(right, left);
}

val lequal(val left, val right)
{
  uses_or2;
  return or2(equal(left, right), less(left, right));
}

val gequal(val left, val right)
{
  uses_or2;
  return or2(equal(left, right), less(right, left));
}

val lessv(val first, struct args *rest)
{
  cnum index = 0;

  while (args_more(rest, index)) {
    val elem = args_get(rest, &index);
    if (!less(first, elem))
      return nil;
    first = elem;
  }

  return t;
}

val greaterv(val first, struct args *rest)
{
  cnum index = 0;

  while (args_more(rest, index)) {
    val elem = args_get(rest, &index);
    if (!less(elem, first))
      return nil;
    first = elem;
  }

  return t;
}

val lequalv(val first, struct args *rest)
{
  cnum index = 0;

  while (args_more(rest, index)) {
    val elem = args_get(rest, &index);
    if (!equal(first, elem) && !less(first, elem))
      return nil;
    first = elem;
  }

  return t;
}

val gequalv(val first, struct args *rest)
{
  cnum index = 0;

  while (args_more(rest, index)) {
    val elem = args_get(rest, &index);
    if (!equal(first, elem) && !less(elem, first))
      return nil;
    first = elem;
  }

  return t;
}


val chrp(val chr)
{
  return (is_chr(chr)) ? t : nil;
}

wchar_t c_chr(val chr)
{
  if (!is_chr(chr))
    type_mismatch(lit("~s is not a character"), chr, nao);
  return convert(wchar_t, coerce(cnum, chr) >> TAG_SHIFT);
}

val chr_isalnum(val ch)
{
  return tnil(iswalnum(c_chr(ch)));
}

val chr_isalpha(val ch)
{
  return tnil(iswalpha(c_chr(ch)));
}

val chr_isascii(val ch)
{
  return tnil(c_chr(ch) >= 0 && c_chr(ch) < 128);
}

val chr_iscntrl(val ch)
{
  wchar_t c = c_chr(ch);
  switch ((c >> 5)) {
  case 0: case 4:
    return t;
  default:
    return tnil(c == 0x7F);
  }
}

val chr_isdigit(val ch)
{
  return if2(iswdigit(c_chr(ch)), t);
}

val chr_digit(val ch)
{
  return if2(iswdigit(c_chr(ch)), minus(ch, chr('0')));
}

val chr_isgraph(val ch)
{
  return tnil(iswgraph(c_chr(ch)));
}

val chr_islower(val ch)
{
  return tnil(iswlower(c_chr(ch)));
}

val chr_isprint(val ch)
{
  return tnil(iswprint(c_chr(ch)));
}

val chr_ispunct(val ch)
{
  return tnil(iswpunct(c_chr(ch)));
}

val chr_isspace(val ch)
{
  return tnil(iswspace(c_chr(ch)));
}

val chr_isblank(val ch)
{
  return tnil(ch == chr(' ') || ch == chr('\t'));
}

val chr_isunisp(val ch)
{
  return tnil(wcschr(spaces, c_chr(ch)));
}

val chr_isupper(val ch)
{
  return tnil(iswupper(c_chr(ch)));
}

val chr_isxdigit(val ch)
{
  return tnil(iswxdigit(c_chr(ch)));
}

val chr_xdigit(val ch)
{
  wchar_t cc = c_chr(ch);

  if ('0' <= cc && cc <= '9')
    return num_fast(cc - '0');

  if ('A' <= cc && cc <= 'F')
    return num_fast(cc - 'A' + 10);

  if ('a' <= cc && cc <= 'a')
    return num_fast(cc - 'a' + 10);

  return nil;
}

val chr_toupper(val ch)
{
  return chr(towupper(c_chr(ch)));
}

val chr_tolower(val ch)
{
  return chr(towlower(c_chr(ch)));
}

val int_chr(val ch)
{
  return num_fast(c_chr(ch));
}

val chr_int(val num)
{
  val self = lit("chr-int");
  cnum n = c_num(num, self);
  if (n < 0 || n > 0x10FFFF)
    uw_throwf(numeric_error_s,
              lit("chr-num: ~s is out of character range"), num, nao);
  return chr(n);
}

val chr_str(val str, val ind)
{
  val self = lit("chr-str");
  cnum index = c_num(ind, self);

  if (index < 0) {
    ind = plus(length_str(str), ind);
    index = c_num(ind, self);
  }

  if (index < 0 || !length_str_gt(str, ind))
    uw_throwf(error_s, lit("~a: ~s is out of range for string ~s"),
              self, ind, str, nao);

  if (lazy_stringp(str)) {
    lazy_str_force_upto(str, ind);
    return chr(c_str(str->ls.prefix, self)[index]);
  } else {
    return chr(c_str(str, self)[index]);
  }
}

val chr_str_set(val str, val ind, val chr)
{
  val self = lit("chr-str-set");
  cnum index = c_num(ind, self);

  if (is_lit(str)) {
    uw_throwf(error_s, lit("~a: cannot modify literal string ~s"),
              self, str, nao);
  }

  if (index < 0) {
    ind = plus(length_str(str), ind);
    index = c_num(ind, self);
  }

  if (index < 0 || !length_str_gt(str, ind))
    uw_throwf(error_s, lit("~a: ~s is out of range for string ~s"),
              self, ind, str, nao);


  if (lazy_stringp(str)) {
    lazy_str_force_upto(str, ind);
    str->ls.prefix->st.str[index] = c_chr(chr);
  } else {
    str->st.str[index] = c_chr(chr);
  }

  return chr;
}

val span_str(val str, val set)
{
  val self = lit("span-str");
  const wchar_t *cstr = c_str(str, self);
  const wchar_t *cset = c_str(set, self);
  size_t span = wcsspn(cstr, cset);
  return num(span);
}

val compl_span_str(val str, val set)
{
  val self = lit("compl-span-str");
  const wchar_t *cstr = c_str(str, self);
  const wchar_t *cset = c_str(set, self);
  size_t span = wcscspn(cstr, cset);
  return num(span);
}

val break_str(val str, val set)
{
  val self = lit("break-str");
  const wchar_t *cstr = c_str(str, self);
  const wchar_t *cset = c_str(set, self);
  const wchar_t *brk = wcspbrk(cstr, cset);
  if (!brk)
    return nil;
  return num(brk - cstr);
}

val symbol_name(val sym)
{
  if (sym)
    type_check(lit("symbol-name"), sym, SYM);
  return sym ? sym->s.name : nil_string;
}

static void symbol_setname(val sym, val name)
{
  type_check(lit("internal error"), sym, SYM);
  sym->s.name = name;
}

val symbol_package(val sym)
{
  if (sym == nil)
    return user_package;
  type_check(lit("symbol-package"), sym, SYM);
  return sym->s.package;
}

val make_sym(val name)
{
  if (!stringp(name)) {
    uw_throwf(error_s, lit("make-sym: name ~s isn't a string"), name, nao);
  } else {
    val obj = make_obj();
    obj->s.type = SYM;
    obj->s.name = name;
    obj->s.package = nil;
    obj->s.slot_cache = 0;
    return obj;
  }
}

val gensym(val prefix)
{
  prefix = default_arg(prefix, lit("g"));
  loc gs_loc = lookup_var_l(nil, gensym_counter_s);
  val name = format(nil, lit("~a~,04d"), prefix,
                    set(gs_loc, plus(deref(gs_loc), one)), nao);
  return make_sym(name);
}

static val make_package_common(val name, val weak)
{
  val weak_vals = default_null_arg(weak);
  val sh = make_hash(nil, weak_vals, t);
  val hh = make_hash(nil, weak_vals, t);
  val obj = make_obj();
  obj->pk.type = PKG;
  obj->pk.name = name;
  obj->pk.symhash = sh;
  obj->pk.hidhash = hh;
  return obj;
}

val make_package(val name, val weak)
{
  if (find_package(name)) {
    uw_throwf(error_s, lit("make-package: ~s exists already"), name, nao);
  } else if (!stringp(name)) {
    uw_throwf(error_s, lit("make-package: name ~s isn't a string"), name, nao);
  } else if (length(name) == zero) {
    uw_throwf(error_s, lit("make-package: package name can't be empty string"),
              nao);
  } else {
    val obj = make_package_common(name, weak);
    mpush(cons(name, obj), cur_package_alist_loc);
    return obj;
  }
}

val make_anon_package(val weak)
{
  return make_package_common(lit("#<anon-package>"), weak);
}

val packagep(val obj)
{
  return type(obj) == PKG ? t : nil;
}

static val lookup_package(val name)
{
  return cdr(assoc(name, deref(cur_package_alist_loc)));
}

val find_package(val package)
{
  if (stringp(package) || (symbolp(package) &&
                           (package = symbol_name(package))))
    return lookup_package(package);
  return nil;
}

static val get_package(val fname, val package, val missing_ok)
{
  if (missing_ok && null_or_missing_p(package))
    return cur_package;
  if (stringp(package) || (symbolp(package) &&
                           (package = symbol_name(package))))
  {
    val p = lookup_package(package);
    if (!p)
      uw_throwf(error_s, lit("~a: no such package: ~s"), fname, package, nao);
    return p;
  }
  type_check(fname, package, PKG);
  return package;
}

val delete_package(val package_in)
{
  val package = get_package(lit("delete-package"), package_in, nil);
  val iter;
  loc cpll = cur_package_alist_loc;
  set(cpll, remqual(package->pk.name, deref(cpll), car_f));
  for (iter = deref(cpll); iter; iter = cdr(iter))
    unuse_package(package, cdar(iter));
  return nil;
}

val merge_delete_package(val to_in, val victim_in)
{
  val self = lit("merge-delete-package");
  val to = get_package(self, to_in, nil);
  val victim = get_package(self, victim_in, t);
  struct hash_iter hi;
  val cell;

  us_hash_iter_init(&hi, victim->pk.symhash);

  while ((cell = hash_iter_next(&hi))) {
    val sym = us_cdr(cell);
    if (symbol_package(sym) == victim)
      rehome_sym(sym, to);
  }

  return delete_package(victim);
}

val package_alist(void)
{
  return deref(cur_package_alist_loc);
}

val package_name(val package)
{
  type_check(lit("package-name"), package, PKG);
  return package->pk.name;
}

val package_symbols(val package_in)
{
  val package = get_package(lit("package-symbols"), package_in, t);
  return hash_values(package->pk.symhash);
}

val package_local_symbols(val package_in)
{
  val package = get_package(lit("package-local-symbols"), package_in, t);
  list_collect_decl (out, ptail);
  struct hash_iter hi;
  val cell;

  us_hash_iter_init(&hi, package->pk.symhash);

  while ((cell = hash_iter_next(&hi))) {
    val sym = us_cdr(cell);
    if (symbol_package(sym) == package)
      ptail = list_collect(ptail, sym);
  }

  return out;
}

val package_foreign_symbols(val package_in)
{
  val package = get_package(lit("package-foreign-symbols"), package_in, t);
  list_collect_decl (out, ptail);
  struct hash_iter hi;
  val cell;

  us_hash_iter_init(&hi, package->pk.symhash);

  while ((cell = hash_iter_next(&hi))) {
    val sym = us_cdr(cell);
    if (symbol_package(sym) != package)
      ptail = list_collect(ptail, sym);
  }

  return out;
}

static void prot_sym_check(val func, val symname, val package)
{
  extern val *protected_sym[];
  val **iter = protected_sym;

  if (package == user_package ||
      package == system_package ||
      package == keyword_package)
  {
    val sym = gethash(package->pk.symhash, symname);

    for (; sym && *iter; iter++) {
      if (**iter == sym)
        uw_throwf(error_s,
                  lit("~a: cannot remove built-in symbol ~s "
                      "from ~a package"),
                  func, sym, package_name(package), nao);
    }
  }
}

val use_sym(val symbol, val package_in)
{
  val self = lit("use-sym");
  val package = get_package(self, package_in, t);

  if (symbol_package(symbol) != package) {
    val name = symbol_name(symbol);
    val found = gethash_e(self, package->pk.symhash, name);
    val existing = cdr(found);

    if (found && symbol_package(existing) == package) {
      if (existing == nil)
        uw_throwf(error_s, lit("~a: cannot hide ~s"), self, existing, nao);
      prot_sym_check(self, name, package);
      sethash(package->pk.hidhash, name, existing);
      existing->s.package = nil;
    }

    sethash(package->pk.symhash, name, symbol);
  }

  return symbol;
}

val unuse_sym(val symbol, val package_in)
{
  val self = lit("unuse-sym");
  val package = get_package(self, package_in, t);
  val name = symbol_name(symbol);
  val found_visible = gethash_e(self, package->pk.symhash, name);
  val found_hidden = gethash_e(self, package->pk.hidhash, name);
  val visible = cdr(found_visible);
  val hidden = cdr(found_hidden);

  if (!found_visible || visible != symbol)
    return nil;

  if (symbol_package(symbol) == package)
    uw_throwf(error_s, lit("~a: cannot unuse ~s from its home package ~s"),
              self, symbol, package, nao);

  if (found_hidden) {
    remhash(package->pk.hidhash, name);
    sethash(package->pk.symhash, name, hidden);
    set(mkloc(hidden->s.package, hidden), package);
    return hidden;
  }

  remhash(package->pk.symhash, name);
  return symbol;
}

static val resolve_package_designators(val fname, val designator_list)
{
  if (consp(designator_list)) {
    list_collect_decl (out, ptail);

    for (; designator_list; designator_list = cdr(designator_list))
      ptail = list_collect(ptail, get_package(fname, car(designator_list), nil));

    return out;
  }
  return cons(get_package(fname, designator_list, nil), nil);
}

val use_package(val use_list, val package_in)
{
  val self = lit("use-package");
  val package = get_package(self, package_in, t);
  val use_package_list = resolve_package_designators(self, use_list);
  val iter;

  if (package == keyword_package)
    uw_throwf(error_s, lit("~a: keyword package cannot use packages"),
              self, nao);

  if (memq(keyword_package, use_package_list))
    uw_throwf(error_s, lit("~a: keyword package cannot be target of use"),
              self, nao);

  if (memq(package, use_package_list))
    uw_throwf(error_s, lit("~a: invalid request for package ~s to use itself"),
              self, package, nao);

  for (iter = use_package_list; iter; iter = cdr(iter)) {
    val use_syms = package_local_symbols(car(iter));
    val use_iter;
    for (use_iter = use_syms; use_iter; use_iter = cdr(use_iter))
      use_sym(car(use_iter), package);
  }

  return use_package_list;
}

val unuse_package(val unuse_list, val package_in)
{
  val self = lit("unuse-package");
  val package = get_package(self, package_in, t);
  val unuse_package_list = resolve_package_designators(self, unuse_list);
  val iter;

  if (memq(package, unuse_package_list))
    uw_throwf(error_s, lit("~a: invalid request for package ~s to unuse itself"),
              self, package, nao);

  for (iter = unuse_package_list; iter; iter = cdr(iter)) {
    val unuse_syms = package_local_symbols(car(iter));
    val unuse_iter;
    for (unuse_iter = unuse_syms; unuse_iter; unuse_iter = cdr(unuse_iter))
      unuse_sym(car(unuse_iter), package);
  }

  return unuse_package_list;
}

/* symbol_visible assumes the perspective that package
 * is the current package!
 */
val symbol_visible(val package, val sym)
{
  val self = lit("internal error");
  val name = symbol_name(sym);
  type_check(self, package, PKG);

  if (sym->s.package == package)
    return t;

  {
    val cell = gethash_e(self, package->pk.symhash, name);

    if (cell)
      return eq(cdr(cell), sym);
  }

  {
    val fallback = get_hash_userdata(package->pk.symhash);

    for (; fallback; fallback = cdr(fallback)) {
      val fb_pkg = car(fallback);
      val cell = gethash_e(self, fb_pkg->pk.symhash, name);

      if (cell)
        return eq(cdr(cell), sym);
    }
  }

  return nil;
}

/* symbol_needs_prefix assumes the perspective that package
 * is the current package!
 */
val symbol_needs_prefix(val self, val package, val sym)
{
  val name = symbol_name(sym);
  val sym_pkg = sym->s.package;
  type_check (self, package, PKG);

  if (!sym_pkg)
    return lit("#");

  if (sym_pkg == keyword_package)
    return null_string;

  if (length_str(name) == zero)
    return sym_pkg->pk.name;

  if (sym_pkg == package) {
    if (us_hash_count(package->pk.hidhash) != zero) {
      val here_cell = gethash_e(self, package->pk.symhash, name);

      if (here_cell) {
        int found_here = (eq(cdr(here_cell), sym) != nil);
        if (found_here)
          return nil;
      }

      return lit("#");
    }

    return nil;
  }

  {
    val fallback = get_hash_userdata(package->pk.symhash);

    for (; fallback; fallback = cdr(fallback)) {
      val fb_pkg = car(fallback);

      if (sym_pkg == fb_pkg) {
        if (us_hash_count(fb_pkg->pk.hidhash) != zero) {
          val cell = gethash_e(self, fb_pkg->pk.symhash, name);
          if (cell) {
            int found_in_fallback = (eq(cdr(cell), sym) != nil);
            if (found_in_fallback)
              return nil;
            break;
          }
        }
        return nil;
      } else {
        if (gethash_e(self, fb_pkg->pk.symhash, name))
          break;
      }
    }
  }

  if (us_hash_count(sym_pkg->pk.hidhash) != zero) {
    val home_cell = gethash_e(self, sym_pkg->pk.symhash, name);

    if (home_cell) {
      int found_in_home = (eq(cdr(home_cell), sym) != nil);
      if (found_in_home)
        return sym_pkg->pk.name;
    }

    return lit("#");
  }

  return sym_pkg->pk.name;
}

val find_symbol(val name, val package_in, val notfound_val_in)
{
  val self = lit("find-symbol");
  val package = get_package(self, package_in, t);

  if (!stringp(name))
    uw_throwf(error_s, lit("~a: name ~s isn't a string"), self, name, nao);

  {
    val cell = gethash_e(self, package->pk.symhash, name);
    if (cell)
      return cdr(cell);
  }

  return default_null_arg(notfound_val_in);
}

val find_symbol_fb(val name, val package_in, val notfound_val_in)
{
  val self = lit("find-symbol-fb");
  val package = get_package(self, package_in, t);

  if (!stringp(name))
    uw_throwf(error_s, lit("~a: name ~s isn't a string"), self, name, nao);

  {
    val cell = gethash_e(self, package->pk.symhash, name);
    if (cell)
      return cdr(cell);
  }

  {
    val fallback = get_hash_userdata(package->pk.symhash);

    for (; fallback; fallback = cdr(fallback)) {
      val fb_pkg = car(fallback);
      val cell = gethash_e(self, fb_pkg->pk.symhash, name);
      if (cell)
        return cdr(cell);
    }
  }

  return default_null_arg(notfound_val_in);
}

val intern(val str, val package)
{
  val self = lit("intern");
  val new_p;
  loc place = gethash_l(self, package->pk.symhash, str, mkcloc(new_p));

  if (!new_p) {
    return deref(place);
  } else {
    val newsym = make_sym(str);
    newsym->s.package = package;
    return set(place, newsym);
  }
}

val intern_intrinsic(val str, val package_in)
{
  val self = lit("intern");
  val package = get_package(self, package_in, t);

  if (!stringp(str))
    uw_throwf(error_s, lit("~a: name ~s isn't a string"), self, str, nao);

  return intern(str, package);
}

val unintern(val symbol, val package_in)
{
  val self = lit("unintern");
  val package = get_package(self, package_in, t);
  val name = symbol_name(symbol);
  val found_visible = gethash_e(self, package->pk.symhash, name);
  val found_hidden = gethash_e(self, package->pk.hidhash, name);
  val visible = cdr(found_visible);
  val hidden = cdr(found_hidden);


  prot_sym_check(self, name, package);

  if (!found_visible || visible != symbol) {
    if (found_hidden && hidden == symbol) {
      remhash(package->pk.hidhash, name);
      return hidden;
    }
    return nil;
  }

  if (found_hidden) {
    remhash(package->pk.hidhash, name);
    sethash(package->pk.symhash, name, hidden);
    set(mkloc(hidden->s.package, hidden), package);
    return hidden;
  }

  if (symbol_package(symbol) == package) {
    if (symbol == nil)
      uw_throwf(error_s, lit("~a: cannot unintern ~s from ~s"),
                self, symbol, package, nao);
    symbol->s.package = nil;
  }

  remhash(package->pk.symhash, name);

  return symbol;
}

val rehome_sym(val sym, val package_in)
{
  val self = lit("rehome-sym");
  val package = get_package(self, package_in, t);
  val name = symbol_name(sym);

  if (!sym)
    uw_throwf(error_s, lit("~a: cannot rehome ~s"), self, sym, nao);

  prot_sym_check(self, name, sym->s.package);
  prot_sym_check(self, name, package);

  if (sym->s.package) {
    val name = symbol_name(sym);
    if (sym->s.package == package)
      return sym;
    remhash(sym->s.package->pk.symhash, name);
  }
  set(mkloc(sym->s.package, sym), package);
  sethash(package->pk.symhash, name, sym);
  remhash(package->pk.hidhash, name);
  return sym;
}

val package_fallback_list(val package_in)
{
  val package = get_package(lit("package-fallback-list"), package_in, t);
  return get_hash_userdata(package->pk.symhash);
}

val set_package_fallback_list(val package_in, val list_in)
{
  val self = lit("set-package-fallback-list");
  val package = get_package(self, package_in, t);
  val list = resolve_package_designators(self, list_in);
  return set_hash_userdata(package->pk.symhash, list);
}

val intern_fallback(val str, val package)
{
  val self = lit("intern-fb");
  val fblist = get_hash_userdata(package->pk.symhash);

  if (fblist) {
    val found = gethash_e(self, package->pk.symhash, str);

    if (found)
      return cdr(found);

    for (; fblist; fblist = cdr(fblist)) {
      val otherpkg = car(fblist);
      val found = gethash_e(self, otherpkg->pk.symhash, str);
      if (found)
        return cdr(found);
    }
  }

  {
    val new_p;
    loc place;

    place = gethash_l(self, package->pk.symhash, str, mkcloc(new_p));

    if (!new_p) {
      return deref(place);
    } else {
      val newsym = make_sym(str);
      newsym->s.package = package;
      return set(place, newsym);
    }
  }
}

val intern_fallback_intrinsic(val str, val package_in)
{
  val self = lit("intern-fb");
  val package = get_package(self, package_in, t);

  if (!stringp(str))
    uw_throwf(error_s, lit("~a: name ~s isn't a string"), self, str, nao);

  return intern_fallback(str, package);
}

val symbolp(val sym)
{
  switch (type(sym)) {
  case NIL:
  case SYM:
    return t;
  default:
    return nil;
  }
}

val keywordp(val sym)
{
  return tnil(sym && symbolp(sym) && sym->s.package == keyword_package);
}

val get_current_package(void)
{
  val pkg_binding = lookup_var(nil, package_s);
  val pkg = cdr(pkg_binding);
  if (type(pkg) != PKG) {
    rplacd(pkg_binding, user_package);
    uw_throwf(error_s, lit("variable *package* non-package "
                           "value ~s (reset to sane value)"),
              pkg, nao);
  }
  return pkg;
}

loc get_current_package_alist_loc(void)
{
  if (package_alist_s) {
    loc var_loc = lookup_var_l(nil, package_alist_s);
    if (!nullocp(var_loc))
      return var_loc;
  }
  return mkcloc(packages);
}

val func_f0(val env, val (*fun)(val))
{
  val obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = F0;
  obj->f.env = env;
  obj->f.f.f0 = fun;
  obj->f.variadic = 0;
  obj->f.fixparam = 0;
  obj->f.optargs = 0;
  return obj;
}

val func_f1(val env, val (*fun)(val, val))
{
  val obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = F1;
  obj->f.env = env;
  obj->f.f.f1 = fun;
  obj->f.variadic = 0;
  obj->f.fixparam = 1;
  obj->f.optargs = 0;
  return obj;
}

val func_f2(val env, val (*fun)(val, val, val))
{
  val obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = F2;
  obj->f.env = env;
  obj->f.f.f2 = fun;
  obj->f.variadic = 0;
  obj->f.fixparam = 2;
  obj->f.optargs = 0;
  return obj;
}

val func_f3(val env, val (*fun)(val, val, val, val))
{
  val obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = F3;
  obj->f.env = env;
  obj->f.f.f3 = fun;
  obj->f.variadic = 0;
  obj->f.fixparam = 3;
  obj->f.optargs = 0;
  return obj;
}

val func_f4(val env, val (*fun)(val, val, val, val, val))
{
  val obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = F4;
  obj->f.env = env;
  obj->f.f.f4 = fun;
  obj->f.variadic = 0;
  obj->f.fixparam = 4;
  obj->f.optargs = 0;
  return obj;
}

val func_n0(val (*fun)(void))
{
  val obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = N0;
  obj->f.env = nil;
  obj->f.f.n0 = fun;
  obj->f.variadic = 0;
  obj->f.fixparam = 0;
  obj->f.optargs = 0;
  return obj;
}

val func_n1(val (*fun)(val))
{
  val obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = N1;
  obj->f.env = nil;
  obj->f.f.n1 = fun;
  obj->f.variadic = 0;
  obj->f.fixparam = 1;
  obj->f.optargs = 0;
  return obj;
}

val func_n2(val (*fun)(val, val))
{
  val obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = N2;
  obj->f.env = nil;
  obj->f.f.n2 = fun;
  obj->f.variadic = 0;
  obj->f.fixparam = 2;
  obj->f.optargs = 0;
  return obj;
}

val func_n3(val (*fun)(val, val, val))
{
  val obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = N3;
  obj->f.env = nil;
  obj->f.f.n3 = fun;
  obj->f.variadic = 0;
  obj->f.fixparam = 3;
  obj->f.optargs = 0;
  return obj;
}

val func_n4(val (*fun)(val, val, val, val))
{
  val obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = N4;
  obj->f.env = nil;
  obj->f.f.n4 = fun;
  obj->f.variadic = 0;
  obj->f.fixparam = 4;
  obj->f.optargs = 0;
  return obj;
}

val func_n5(val (*fun)(val, val, val, val, val))
{
  val obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = N5;
  obj->f.env = nil;
  obj->f.f.n5 = fun;
  obj->f.variadic = 0;
  obj->f.fixparam = 5;
  obj->f.optargs = 0;
  return obj;
}

val func_n6(val (*fun)(val, val, val, val, val, val))
{
  val obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = N6;
  obj->f.env = nil;
  obj->f.f.n6 = fun;
  obj->f.variadic = 0;
  obj->f.fixparam = 6;
  obj->f.optargs = 0;
  return obj;
}

val func_n7(val (*fun)(val, val, val, val, val, val, val))
{
  val obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = N7;
  obj->f.env = nil;
  obj->f.f.n7 = fun;
  obj->f.variadic = 0;
  obj->f.fixparam = 7;
  obj->f.optargs = 0;
  return obj;
}

val func_n8(val (*fun)(val, val, val, val, val, val, val, val))
{
  val obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = N8;
  obj->f.env = nil;
  obj->f.f.n8 = fun;
  obj->f.variadic = 0;
  obj->f.fixparam = 8;
  obj->f.optargs = 0;
  return obj;
}


val func_f0v(val env, val (*fun)(val, varg))
{
  val obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = F0;
  obj->f.env = env;
  obj->f.f.f0v = fun;
  obj->f.variadic = 1;
  obj->f.fixparam = 0;
  obj->f.optargs = 0;
  return obj;
}

val func_f1v(val env, val (*fun)(val env, val, varg))
{
  val obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = F1;
  obj->f.env = env;
  obj->f.f.f1v = fun;
  obj->f.variadic = 1;
  obj->f.fixparam = 1;
  obj->f.optargs = 0;
  return obj;
}

val func_f2v(val env, val (*fun)(val env, val, val, varg))
{
  val obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = F2;
  obj->f.env = env;
  obj->f.f.f2v = fun;
  obj->f.variadic = 1;
  obj->f.fixparam = 2;
  obj->f.optargs = 0;
  return obj;
}

val func_f3v(val env, val (*fun)(val env, val, val, val, varg))
{
  val obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = F3;
  obj->f.env = env;
  obj->f.f.f3v = fun;
  obj->f.variadic = 1;
  obj->f.fixparam = 3;
  obj->f.optargs = 0;
  return obj;
}

val func_f4v(val env, val (*fun)(val env, val, val, val, val, varg))
{
  val obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = F4;
  obj->f.env = env;
  obj->f.f.f4v = fun;
  obj->f.variadic = 1;
  obj->f.fixparam = 4;
  obj->f.optargs = 0;
  return obj;
}

val func_n0v(val (*fun)(varg))
{
  val obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = N0;
  obj->f.env = nil;
  obj->f.f.n0v = fun;
  obj->f.variadic = 1;
  obj->f.fixparam = 0;
  obj->f.optargs = 0;
  return obj;
}

val func_n1v(val (*fun)(val, varg))
{
  val obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = N1;
  obj->f.env = nil;
  obj->f.f.n1v = fun;
  obj->f.variadic = 1;
  obj->f.fixparam = 1;
  obj->f.optargs = 0;
  return obj;
}

val func_n2v(val (*fun)(val, val, varg))
{
  val obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = N2;
  obj->f.env = nil;
  obj->f.f.n2v = fun;
  obj->f.variadic = 1;
  obj->f.fixparam = 2;
  obj->f.optargs = 0;
  return obj;
}

val func_n3v(val (*fun)(val, val, val, varg))
{
  val obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = N3;
  obj->f.env = nil;
  obj->f.f.n3v = fun;
  obj->f.variadic = 1;
  obj->f.fixparam = 3;
  obj->f.optargs = 0;
  return obj;
}

val func_n4v(val (*fun)(val, val, val, val, varg))
{
  val obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = N4;
  obj->f.env = nil;
  obj->f.f.n4v = fun;
  obj->f.variadic = 1;
  obj->f.fixparam = 4;
  obj->f.optargs = 0;
  return obj;
}

val func_n5v(val (*fun)(val, val, val, val, val, varg))
{
  val obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = N5;
  obj->f.env = nil;
  obj->f.f.n5v = fun;
  obj->f.variadic = 1;
  obj->f.fixparam = 5;
  obj->f.optargs = 0;
  return obj;
}

val func_n6v(val (*fun)(val, val, val, val, val, val, varg))
{
  val obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = N6;
  obj->f.env = nil;
  obj->f.f.n6v = fun;
  obj->f.variadic = 1;
  obj->f.fixparam = 6;
  obj->f.optargs = 0;
  return obj;
}

val func_n7v(val (*fun)(val, val, val, val, val, val, val, varg))
{
  val obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = N7;
  obj->f.env = nil;
  obj->f.f.n7v = fun;
  obj->f.variadic = 1;
  obj->f.fixparam = 7;
  obj->f.optargs = 0;
  return obj;
}

val func_n8v(val (*fun)(val, val, val, val, val, val, val, val, varg))
{
  val obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = N8;
  obj->f.env = nil;
  obj->f.f.n8v = fun;
  obj->f.variadic = 1;
  obj->f.fixparam = 8;
  obj->f.optargs = 0;
  return obj;
}

val func_n1o(val (*fun)(val), int reqargs)
{
  val obj = func_n1(fun);
  obj->f.optargs = 1 - reqargs;
  return obj;
}

val func_n2o(val (*fun)(val, val), int reqargs)
{
  val obj = func_n2(fun);
  obj->f.optargs = 2 - reqargs;
  return obj;
}

val func_n3o(val (*fun)(val, val, val), int reqargs)
{
  val obj = func_n3(fun);
  obj->f.optargs = 3 - reqargs;
  return obj;
}

val func_n4o(val (*fun)(val, val, val, val), int reqargs)
{
  val obj = func_n4(fun);
  obj->f.optargs = 4 - reqargs;
  return obj;
}

val func_n5o(val (*fun)(val, val, val, val, val), int reqargs)
{
  val obj = func_n5(fun);
  obj->f.optargs = 5 - reqargs;
  return obj;
}

val func_n6o(val (*fun)(val, val, val, val, val, val), int reqargs)
{
  val obj = func_n6(fun);
  obj->f.optargs = 6 - reqargs;
  return obj;
}

val func_n7o(val (*fun)(val, val, val, val, val, val, val), int reqargs)
{
  val obj = func_n7(fun);
  obj->f.optargs = 7 - reqargs;
  return obj;
}

val func_n8o(val (*fun)(val, val, val, val, val, val, val, val), int reqargs)
{
  val obj = func_n8(fun);
  obj->f.optargs = 8 - reqargs;
  return obj;
}

val func_n1ov(val (*fun)(val, varg), int reqargs)
{
  val obj = func_n1v(fun);
  obj->f.optargs = 1 - reqargs;
  return obj;
}

val func_n2ov(val (*fun)(val, val, varg), int reqargs)
{
  val obj = func_n2v(fun);
  obj->f.optargs = 2 - reqargs;
  return obj;
}

val func_n3ov(val (*fun)(val, val, val, varg), int reqargs)
{
  val obj = func_n3v(fun);
  obj->f.optargs = 3 - reqargs;
  return obj;
}

val func_n4ov(val (*fun)(val, val, val, val, varg), int reqargs)
{
  val obj = func_n4v(fun);
  obj->f.optargs = 4 - reqargs;
  return obj;
}

val func_interp(val env, val form)
{
  val obj = make_obj();
  obj->f.type = FUN;
  obj->f.functype = FINTERP;
  obj->f.env = env;
  obj->f.f.interp_fun = form;
  obj->f.variadic = 1;
  obj->f.fixparam = 0;
  obj->f.optargs = 0;
  return obj;
}

val func_vm(val closure, val desc, int fixparam, int reqargs, int variadic)
{
  if (fixparam > FIXPARAM_MAX) {
    uw_throwf(error_s, lit("closure in ~s with more than ~s fixed parameters"),
              desc, num(FIXPARAM_MAX), nao);
  } else if (fixparam < 0 || reqargs < 0 || reqargs > fixparam) {
    uw_throwf(error_s, lit("closure in ~s with bogus parameters"),
              desc, nao);
  } else {
    val obj = make_obj();
    obj->f.type = FUN;
    obj->f.functype = FVM;
    obj->f.env = closure;
    obj->f.f.vm_desc = desc;
    obj->f.fixparam = fixparam;
    obj->f.optargs = fixparam - reqargs;
    obj->f.variadic = (variadic != 0);
    return obj;
  }
}

val copy_fun(val ofun)
{
  val self = lit("copy-fun");
  type_check(self, ofun, FUN);
  {
    val nfun = make_obj();
    nfun->f = ofun->f;

    if (nfun->f.env)
      nfun->f.env = if3(nfun->f.functype == FVM,
                        vm_copy_closure, deep_copy_env)(nfun->f.env);
    return nfun;
  }
}

val func_get_form(val fun)
{
  val self = lit("func-get-form");
  type_check(self, fun, FUN);
  if (fun->f.functype != FINTERP)
    uw_throwf(error_s, lit("~a: ~a is not an interpreted function"),
              self, fun, nao);
  return fun->f.f.interp_fun;
}

val func_get_env(val fun)
{
  type_check(lit("func-get-env"), fun, FUN);
  return fun->f.env;
}

val func_set_env(val fun, val env)
{
  type_check(lit("func-set-env"), fun, FUN);
  set(mkloc(fun->f.env, fun), env);
  return env;
}

val us_func_set_env(val fun, val env)
{
  set(mkloc(fun->f.env, fun), env);
  return env;
}

val functionp(val obj)
{
  return type(obj) == FUN ? t : nil;
}

val interp_fun_p(val obj)
{
  return (functionp(obj) && obj->f.functype == FINTERP) ? t : nil;
}

val vm_fun_p(val obj)
{
  return (functionp(obj) && obj->f.functype == FVM) ? t : nil;
}

static val get_param_counts(val params, cnum *fixparam, cnum *optparam)
{
  cnum fx = 0, oa = -1;

  for (; consp(params); params = us_cdr(params)) {
    val param = us_car(params);
    if (param == colon_k && oa < 0) {
      oa = 0;
    } else {
      fx++;
      oa += (oa >= 0);
    }
  }

  *fixparam = fx;
  *optparam = (oa > 0) ? oa : 0;
  return params;
}

val fun_fixparam_count(val fun)
{
  type_check(lit("func-fixparam-count"), fun, FUN);

  if (fun->f.functype != FINTERP) {
    return num(fun->f.fixparam);
  } else {
    val form = fun->f.f.interp_fun;
    cnum fixparam, optparam;
    val params = cadr(form);
    (void) get_param_counts(params, &fixparam, &optparam);
    return num(fixparam);
  }
}

val fun_optparam_count(val fun)
{
  type_check(lit("func-optparam-count"), fun, FUN);

  if (fun->f.functype != FINTERP) {
    return num(fun->f.optargs);
  } else {
    val form = fun->f.f.interp_fun;
    cnum fixparam, optparam;
    val params = cadr(form);
    (void) get_param_counts(params, &fixparam, &optparam);
    return num(optparam);
  }
}

val fun_variadic(val fun)
{
  type_check(lit("func-variadic"), fun, FUN);

  if (fun->f.functype != FINTERP) {
    return tnil(fun->f.variadic);
  } else {
    val form = fun->f.f.interp_fun;
    cnum fixparam, optparam;
    val params = cadr(form);
    return tnil(get_param_counts(params, &fixparam, &optparam));
  }
}

static NORETURN void callerror(val fun, val msg)
{
  uses_or2;

  if (functionp(fun))
    fun = format(nil, lit("~s"), or2(func_get_name(fun, nil), fun), nao);
  else
    fun = format(nil, lit("object ~s called as function"), fun, nao);

  uw_throwf(error_s, lit("~a: ~a"), fun, msg, nao);
  abort();
}

INLINE val do_generic_funcall(val fun, struct args *args_in)
{
  int variadic, fixparam, reqargs;
  struct args *args = args_in;

  switch (type(fun)) {
  case FUN:
    break;
  case NIL:
  case CONS:
  case LCONS:
  case VEC:
  case STR:
  case LIT:
  case LSTR:
  case BUF:
  carray:
    bug_unless (args->argc >= ARGS_MIN);
    args_normalize_least(args, 3);

    switch (args->fill) {
    case 0:
      callerror(fun, lit("missing required arguments"));
    case 1:
      switch (type(args->arg[0])) {
      case NIL:
      case CONS:
      case LCONS:
      case VEC:
        return sel(fun, args->arg[0]);
      case RNG:
        return sub(fun, args->arg[0]->rn.from, args->arg[0]->rn.to);
      default:
        return ref(fun, args->arg[0]);
      }
    case 2:
      return sub(fun, args->arg[0], args->arg[1]);
    default:
      callerror(fun, lit("too many arguments"));
    }
  case SYM:
    {
      val binding = lookup_fun(nil, fun);
      if (!binding)
        callerror(fun, lit("has no function binding"));
      fun = cdr(binding);
    }
    break;
  case COBJ:
    if (fun->co.cls == hash_cls) {
      bug_unless (args->argc >= ARGS_MIN);
      args_normalize_least(args, 3);

      switch (args->fill) {
      case 0:
        callerror(fun, lit("missing required arguments"));
      case 1:
        return gethash(fun, args->arg[0]);
      case 2:
        return gethash_n(fun, args->arg[0], args->arg[1]);
      default:
        callerror(fun, lit("too many arguments"));
      }
    } else if (fun->co.cls == regex_cls) {
      bug_unless (args->argc >= ARGS_MIN);
      args_normalize_least(args, 3);

      switch (args->fill) {
      case 0:
        callerror(fun, lit("missing required arguments"));
      case 1:
        return search_regst(z(args->arg[0]), fun, nil, nil);
      case 2:
        return search_regst(z(args->arg[1]), fun, z(args->arg[0]), nil);
      case 3:
        return search_regst(z(args->arg[2]), fun, z(args->arg[0]), z(args->arg[1]));
      default:
        callerror(fun, lit("too many arguments"));
      }
    } else if (fun->co.cls == vm_desc_cls) {
      if (args->fill || args->list)
        callerror(fun, lit("too many arguments"));
      return vm_execute_toplevel(fun);
    } else if (fun->co.cls == carray_cls) {
      goto carray;
    } else if (fun->co.cls == tree_cls) {
      switch (args->fill) {
      case 0:
        callerror(fun, lit("missing required arguments"));
      case 1:
        switch (type(args->arg[0])) {
        case RNG:
          return sub(fun, args->arg[0]->rn.from, args->arg[0]->rn.to);
        default:
          return ref(fun, args->arg[0]);
        }
      case 2:
        return sub(fun, args->arg[0], args->arg[1]);
      default:
        callerror(fun, lit("too many arguments"));
      }
    } else if (obj_struct_p(fun)) {
      fun = method(fun, lambda_s);
      break;
    }
    /* fallthrough */
  default:
    callerror(fun, lit("object is not callable"));
  }

  variadic = fun->f.variadic;
  fixparam = fun->f.fixparam;
  reqargs = fixparam - fun->f.optargs;

  if (!variadic) {
    val *arg = 0;

    if (args->argc < fixparam) {
      args_decl(args_copy, max(fixparam, ARGS_MIN));
      args_copy_zap(args_copy, args_in);
      args = args_copy;
    }

    arg = args->arg;

    args_normalize_fill(args, reqargs, fixparam);

    if (args->fill < reqargs)
      callerror(fun, lit("missing required arguments"));

    if (args->fill > fixparam || args->list)
      callerror(fun, lit("too many arguments"));

    switch (fun->f.functype) {
    case F0:
      return fun->f.f.f0(fun->f.env);
    case F1:
      return fun->f.f.f1(fun->f.env, z(arg[0]));
    case F2:
      return fun->f.f.f2(fun->f.env, z(arg[0]), z(arg[1]));
    case F3:
      return fun->f.f.f3(fun->f.env, z(arg[0]), z(arg[1]), z(arg[2]));
    case F4:
      return fun->f.f.f4(fun->f.env, z(arg[0]), z(arg[1]), z(arg[2]), z(arg[3]));
    case N0:
      return fun->f.f.n0();
    case N1:
      return fun->f.f.n1(z(arg[0]));
    case N2:
      return fun->f.f.n2(z(arg[0]), z(arg[1]));
    case N3:
      return fun->f.f.n3(z(arg[0]), z(arg[1]), z(arg[2]));
    case N4:
      return fun->f.f.n4(z(arg[0]), z(arg[1]), z(arg[2]), z(arg[3]));
    case N5:
      return fun->f.f.n5(z(arg[0]), z(arg[1]), z(arg[2]), z(arg[3]), z(arg[4]));
    case N6:
      return fun->f.f.n6(z(arg[0]), z(arg[1]), z(arg[2]), z(arg[3]), z(arg[4]), z(arg[5]));
    case N7:
      return fun->f.f.n7(z(arg[0]), z(arg[1]), z(arg[2]), z(arg[3]), z(arg[4]), z(arg[5]), z(arg[6]));
    case N8:
      return fun->f.f.n8(z(arg[0]), z(arg[1]), z(arg[2]), z(arg[3]), z(arg[4]), z(arg[5]), z(arg[6]), z(arg[7]));
    case FVM:
      return vm_execute_closure(fun, args);
    case FINTERP:
      internal_error("unsupported function type");
    }
  } else {
    val *arg = 0;

    if (args->argc < fixparam) {
      args_decl(args_copy, max(fixparam, ARGS_MIN));
      args = args_copy_zap(args_copy, args_in);
    }

    arg = args->arg;

    args_normalize_fill(args, reqargs, fixparam);

    if (args->fill < reqargs)
      callerror(fun, lit("missing required arguments"));

    switch (fun->f.functype) {
    case FINTERP:
      return funcall_interp(fun, args);
    case FVM:
      return vm_execute_closure(fun, args);
    default:
      {
        args_decl(args_copy, max(args->fill - fixparam, ARGS_MIN));
        args = args_cat_zap_from(args_copy, args, fixparam);
      }

      switch (fun->f.functype) {
      case F0:
        return fun->f.f.f0v(fun->f.env, args);
      case F1:
        return fun->f.f.f1v(fun->f.env, z(arg[0]), args);
      case F2:
        return fun->f.f.f2v(fun->f.env, z(arg[0]), z(arg[1]), args);
      case F3:
        return fun->f.f.f3v(fun->f.env, z(arg[0]), z(arg[1]), z(arg[2]), args);
      case F4:
        return fun->f.f.f4v(fun->f.env, z(arg[0]), z(arg[1]), z(arg[2]), z(arg[3]), args);
      case N0:
        return fun->f.f.n0v(args);
      case N1:
        return fun->f.f.n1v(z(arg[0]), args);
      case N2:
        return fun->f.f.n2v(z(arg[0]), z(arg[1]), args);
      case N3:
        return fun->f.f.n3v(z(arg[0]), z(arg[1]), z(arg[2]), args);
      case N4:
        return fun->f.f.n4v(z(arg[0]), z(arg[1]), z(arg[2]), z(arg[3]), args);
      case N5:
        return fun->f.f.n5v(z(arg[0]), z(arg[1]), z(arg[2]), z(arg[3]), z(arg[4]), args);
      case N6:
        return fun->f.f.n6v(z(arg[0]), z(arg[1]), z(arg[2]), z(arg[3]), z(arg[4]), z(arg[5]), args);
      case N7:
        return fun->f.f.n7v(z(arg[0]), z(arg[1]), z(arg[2]), z(arg[3]), z(arg[4]), z(arg[5]), z(arg[6]), args);
      case N8:
        return fun->f.f.n8v(z(arg[0]), z(arg[1]), z(arg[2]), z(arg[3]), z(arg[4]), z(arg[5]), z(arg[6]), z(arg[7]), args);
      }
    }
  }

  internal_error("corrupt function type field");
}

val generic_funcall(val fun, struct args *args)
{
  if (dbg_backtrace) {
    val ret;
    dbg_fcall_begin(fun, args);
    ret = do_generic_funcall(fun, args);
    dbg_fcall_end;
    return ret;
  }
  return do_generic_funcall(fun, args);
}

static NORETURN void wrongargs(val fun)
{
  callerror(fun, lit("wrong number of arguments"));
}

val funcall(val fun)
{
  if (type(fun) != FUN || fun->f.optargs || dbg_backtrace) {
    args_decl(args, ARGS_MIN);
    return generic_funcall(fun, args);
  }

  if (fun->f.variadic) {
    args_decl(args, ARGS_MIN);

    switch (fun->f.functype) {
    case FINTERP:
      return funcall_interp(fun, args);
    case FVM:
      return vm_execute_closure(fun, args);
    case F0:
      return fun->f.f.f0v(fun->f.env, args);
    case N0:
      return fun->f.f.n0v(args);
    default:
      break;
    }
  } else {
    switch (fun->f.functype) {
    case FVM:
      if (fun->f.fixparam != 0)
        break;
      return vm_funcall(fun);
    case F0:
      return fun->f.f.f0(fun->f.env);
    case N0:
      return fun->f.f.n0();
    default:
      break;
    }
  }
  wrongargs(fun);
}

val funcall1(val fun, val arg)
{
  if (type(fun) != FUN || fun->f.optargs || dbg_backtrace) {
    args_decl(args, ARGS_MIN);
    args_add(args, z(arg));
    return generic_funcall(fun, args);
  }

  if (fun->f.variadic) {
    args_decl(args, ARGS_MIN);

    switch (fun->f.functype) {
    case FINTERP:
      args_add(args, arg);
      return funcall_interp(fun, args);
    case FVM:
      if (fun->f.fixparam > 1)
        break;
      args_add(args, arg);
      return vm_execute_closure(fun, args);
    case F0:
      args_add(args, arg);
      return fun->f.f.f0v(fun->f.env, args);
    case N0:
      args_add(args, arg);
      return fun->f.f.n0v(args);
    case F1:
      return fun->f.f.f1v(fun->f.env, z(arg), args);
    case N1:
      return fun->f.f.n1v(z(arg), args);
    default:
      break;
    }
  } else {
    switch (fun->f.functype) {
    case FVM:
      if (fun->f.fixparam != 1)
        break;
      return vm_funcall1(fun, z(arg));
    case F1:
      return fun->f.f.f1(fun->f.env, z(arg));
    case N1:
      return fun->f.f.n1(z(arg));
    default:
      break;
    }
  }
  wrongargs(fun);
}

val funcall2(val fun, val arg1, val arg2)
{
  if (type(fun) != FUN || fun->f.optargs || dbg_backtrace) {
    args_decl(args, ARGS_MIN);
    args_add2(args, z(arg1), z(arg2));
    return generic_funcall(fun, args);
  }

  if (fun->f.variadic) {
    args_decl(args, ARGS_MIN);

    switch (fun->f.functype) {
    case FINTERP:
      args_add2(args, arg1, arg2);
      return funcall_interp(fun, args);
    case FVM:
      if (fun->f.fixparam > 2)
        break;
      args_add2(args, arg1, arg2);
      return vm_execute_closure(fun, args);
    case F0:
      args_add2(args, arg1, arg2);
      return fun->f.f.f0v(fun->f.env, args);
    case N0:
      args_add2(args, arg1, arg2);
      return fun->f.f.n0v(args);
    case F1:
      args_add(args, arg2);
      return fun->f.f.f1v(fun->f.env, z(arg1), args);
    case N1:
      args_add(args, arg2);
      return fun->f.f.n1v(z(arg1), args);
    case F2:
      return fun->f.f.f2v(fun->f.env, z(arg1), z(arg2), args);
    case N2:
      return fun->f.f.n2v(z(arg1), z(arg2), args);
    default:
      break;
    }
  } else {
    switch (fun->f.functype) {
    case FVM:
      if (fun->f.fixparam != 2)
        break;
      return vm_funcall2(fun, z(arg1), z(arg2));
    case F2:
      return fun->f.f.f2(fun->f.env, z(arg1), z(arg2));
    case N2:
      return fun->f.f.n2(z(arg1), z(arg2));
    default:
      break;
    }
  }
  wrongargs(fun);
}

val funcall3(val fun, val arg1, val arg2, val arg3)
{
  if (type(fun) != FUN || fun->f.optargs || dbg_backtrace) {
    args_decl(args, ARGS_MIN);
    args_add3(args, z(arg1), z(arg2), z(arg3));
    return generic_funcall(fun, args);
  }

  if (fun->f.variadic) {
    args_decl(args, ARGS_MIN);

    switch (fun->f.functype) {
    case FINTERP:
      args_add3(args, arg1, arg2, arg3);
      return funcall_interp(fun, args);
    case FVM:
      if (fun->f.fixparam > 3)
        break;
      args_add3(args, arg1, arg2, arg3);
      return vm_execute_closure(fun, args);
    case F0:
      args_add3(args, arg1, arg2, arg3);
      return fun->f.f.f0v(fun->f.env, args);
    case N0:
      args_add3(args, arg1, arg2, arg3);
      return fun->f.f.n0v(args);
    case F1:
      args_add2(args, arg2, arg3);
      return fun->f.f.f1v(fun->f.env, z(arg1), args);
    case N1:
      args_add2(args, arg2, arg3);
      return fun->f.f.n1v(z(arg1), args);
    case F2:
      args_add(args, arg3);
      return fun->f.f.f2v(fun->f.env, z(arg1), z(arg2), args);
    case N2:
      args_add(args, arg3);
      return fun->f.f.n2v(z(arg1), z(arg2), args);
    case F3:
      return fun->f.f.f3v(fun->f.env, z(arg1), z(arg2), z(arg3), args);
    case N3:
      return fun->f.f.n3v(z(arg1), z(arg2), z(arg3), args);
    default:
      break;
    }
  } else {
    switch (fun->f.functype) {
    case FVM:
      if (fun->f.fixparam != 3)
        break;
      return vm_funcall3(fun, z(arg1), z(arg2), z(arg3));
    case F3:
      return fun->f.f.f3(fun->f.env, z(arg1), z(arg2), z(arg3));
    case N3:
      return fun->f.f.n3(z(arg1), z(arg2), z(arg3));
    default:
      break;
    }
  }
  wrongargs(fun);
}

val funcall4(val fun, val arg1, val arg2, val arg3, val arg4)
{
  if (type(fun) != FUN || fun->f.optargs || dbg_backtrace) {
    args_decl(args, ARGS_MIN);
    args_add4(args, z(arg1), z(arg2), z(arg3), z(arg4));
    return generic_funcall(fun, args);
  }

  if (fun->f.variadic) {
    args_decl(args, ARGS_MIN);

    switch (fun->f.functype) {
    case FINTERP:
      args_add4(args, arg1, arg2, arg3, arg4);
      return funcall_interp(fun, args);
    case FVM:
      if (fun->f.fixparam > 4)
        break;
      args_add4(args, arg1, arg2, arg3, arg4);
      return vm_execute_closure(fun, args);
    case F0:
      args_add4(args, arg1, arg2, arg3, arg4);
      return fun->f.f.f0v(fun->f.env, args);
    case N0:
      args_add4(args, arg1, arg2, arg3, arg4);
      return fun->f.f.n0v(args);
    case F1:
      args_add3(args, arg2, arg3, arg4);
      return fun->f.f.f1v(fun->f.env, z(arg1), args);
    case N1:
      args_add3(args, arg2, arg3, arg4);
      return fun->f.f.n1v(z(arg1), args);
    case F2:
      args_add2(args, arg3, arg4);
      return fun->f.f.f2v(fun->f.env, z(arg1), z(arg2), args);
    case N2:
      args_add2(args, arg3, arg4);
      return fun->f.f.n2v(z(arg1), z(arg2), args);
    case F3:
      args_add(args, arg4);
      return fun->f.f.f3v(fun->f.env, z(arg1), z(arg2), z(arg3), args);
    case N3:
      args_add(args, arg4);
      return fun->f.f.n3v(z(arg1), z(arg2), z(arg3), args);
    case F4:
      return fun->f.f.f4v(fun->f.env, z(arg1), z(arg2), z(arg3), z(arg4), args);
    case N4:
      return fun->f.f.n4v(z(arg1), z(arg2), z(arg3), z(arg4), args);
    default:
      break;
    }
  } else {
    switch (fun->f.functype) {
    case FVM:
      if (fun->f.fixparam != 4)
        break;
      return vm_funcall4(fun, z(arg1), z(arg2), z(arg3), z(arg4));
    case F4:
      return fun->f.f.f4(fun->f.env, z(arg1), z(arg2), z(arg3), z(arg4));
    case N4:
      return fun->f.f.n4(z(arg1), z(arg2), z(arg3), z(arg4));
    default:
      break;
    }
  }
  wrongargs(fun);
}

val reduce_left(val fun, val seq, val init, val key)
{
  val self = lit("reduce-left");
  seq_iter_t item_iter;
  val item;

  if (null_or_missing_p(key))
    key = identity_f;

  seq_iter_init(self, &item_iter, seq);

  if (missingp(init)) {
    if (seq_get(&item_iter, &item))
      init = funcall1(key, item);
    else
      return funcall(fun);
  }

  while (seq_get(&item_iter, &item))
    init = funcall2(fun, init, funcall1(key, item));

  return init;
}

val reduce_right(val fun, val list, val init, val key)
{
  if (null_or_missing_p(key))
    key = identity_f;

  list = nullify(list);

  if (list) {
    if (missingp(init)) {
      if (!rest(list))
        return funcall1(key, first(list));
      if (!rest(rest(list)))
        return funcall2(fun, funcall1(key, first(list)),
                        funcall1(key, second(list)));
      /* fall through: no init, three or more items in list */
    } else {
      if (!rest(list))
        return funcall2(fun, funcall1(key, first(list)), init);
      /* fall through: init, and two or more items in list */
    }
  } else if (!missingp(init)) {
    return init;
  } else {
    return funcall(fun);
  }

  return funcall2(fun, funcall1(key, car(list)),
                       if3(cdr(list), reduce_right(fun, cdr(list), init, key),
                                      init));
}

static val do_pa_12_2(val fcons, val arg2)
{
  return funcall2(car(fcons), cdr(fcons), arg2);
}

val pa_12_2(val fun2, val arg)
{
  return func_f1(cons(fun2, arg), do_pa_12_2);
}

static val do_pa_12_1(val fcons, val arg1)
{
  return funcall2(car(fcons), arg1, cdr(fcons));
}

val pa_12_1(val fun2, val arg2)
{
  return func_f1(cons(fun2, arg2), do_pa_12_1);
}

static val do_pa_12_1_v(val fcons, struct args *args)
{
  return funcall2(car(fcons), args_get_list(args), cdr(fcons));
}

static val pa_12_1_v(val fun2, val arg2)
{
  return func_f0v(cons(fun2, arg2), do_pa_12_1_v);
}

static val do_pa_123_3(val fcons, val arg3)
{
  return funcall3(car(fcons), car(cdr(fcons)), cdr(cdr(fcons)), arg3);
}

val pa_123_3(val fun3, val arg1, val arg2)
{
  return func_f1(cons(fun3, cons(arg1, arg2)), do_pa_123_3);
}

static val do_pa_123_2(val fcons, val arg2)
{
  return funcall3(car(fcons), car(cdr(fcons)), arg2, cdr(cdr(fcons)));
}

val pa_123_2(val fun3, val arg1, val arg3)
{
  return func_f1(cons(fun3, cons(arg1, arg3)), do_pa_123_2);
}

static val do_pa_123_1(val fcons, val arg1)
{
  return funcall3(car(fcons), arg1, car(cdr(fcons)), cdr(cdr(fcons)));
}

val pa_123_1(val fun3, val arg2, val arg3)
{
  return func_f1(cons(fun3, cons(arg2, arg3)), do_pa_123_1);
}

static val do_pa_123_23(val fcons, val arg2, val arg3)
{
  return funcall3(car(fcons), cdr(fcons), arg2, arg3);
}

val pa_123_23(val fun3, val arg1)
{
  return func_f2(cons(fun3, arg1), do_pa_123_23);
}

static val do_pa_1234_1(val fcons, val arg1)
{
  cons_bind (fun, dr, fcons);
  cons_bind (arg2, ddr, dr);
  cons_bind (arg3, dddr, ddr);
  val arg4 = car(dddr);

  return funcall4(fun, arg1, arg2, arg3, arg4);
}

val pa_1234_1(val fun4, val arg2, val arg3, val arg4)
{
  return func_f1(list(fun4, arg2, arg3, arg4, nao), do_pa_1234_1);
}

static val do_pa_1234_34(val fcons, val arg3, val arg4)
{
  return funcall4(car(fcons), car(cdr(fcons)), cdr(cdr(fcons)), arg3, arg4);
}

val pa_1234_34(val fun4, val arg1, val arg2)
{
  return func_f2(cons(fun4, cons(arg1, arg2)), do_pa_1234_34);
}

val transposev(struct args *list)
{
  val func = list_f;

  if (!args_more(list, 0))
    return nil;

  switch (type(args_at(list, 0))) {
  case STR:
  case LSTR:
  case LIT:
    func = pa_12_1_v(func_n2(cat_str), nil);
    break;
  case VEC:
    func = func_n0v(vectorv);
    break;
  default:
    break;
  }

  return mapcarv(func, list);
}

val transpose(val seq)
{
  args_decl_list(args, ARGS_MIN, tolist(seq));
  return make_like(transposev(args), seq);
}

static val do_chain(val fun1_list, struct args *args)
{
  val arg = nil;

  fun1_list = nullify(fun1_list);

  if (fun1_list) {
    arg = generic_funcall(car(fun1_list), args);
    fun1_list = cdr(fun1_list);
  }

  for (; fun1_list; fun1_list = cdr(fun1_list))
    arg = funcall1(car(fun1_list), arg);

  return arg;
}

val chain(val first_fun, ...)
{
  va_list vl;
  list_collect_decl (out, iter);

  if (first_fun != nao) {
    val next_fun;
    va_start (vl, first_fun);
    iter = list_collect(iter, first_fun);

    while ((next_fun = va_arg(vl, val)) != nao)
      iter = list_collect(iter, next_fun);

    va_end (vl);
  }

  return func_f0v(out, do_chain);
}

val chainv(struct args *funlist)
{
  return func_f0v(args_get_list(funlist), do_chain);
}

static val do_chand(val fun1_list, struct args *args)
{
  val arg = nil;

  fun1_list = nullify(fun1_list);

  if (fun1_list) {
    arg = generic_funcall(car(fun1_list), args);
    fun1_list = cdr(fun1_list);
  }

  for (; arg && fun1_list; fun1_list = cdr(fun1_list))
    arg = funcall1(car(fun1_list), arg);

  return arg;
}


val chandv(struct args *funlist)
{
  return func_f0v(args_get_list(funlist), do_chand);
}

static val do_juxt(val funcs, struct args *args)
{
  return mapcar(pa_12_1(func_n2(apply), args_get_list(args)), funcs);
}

val juxtv(struct args *funlist)
{
  return func_f0v(args_get_list(funlist), do_juxt);
}

static val do_and(val fun1_list, struct args *args_in)
{
  cnum argc = args_in->argc;
  args_decl(args, argc);
  val ret = t;

  fun1_list = nullify(fun1_list);

  gc_hint(fun1_list);

  for (; fun1_list; fun1_list = cdr(fun1_list)) {
    args_copy(args, args_in);
    if (nilp((ret = generic_funcall(car(fun1_list), args))))
      break;
  }

  return ret;
}

val andf(val first_fun, ...)
{
  va_list vl;
  list_collect_decl (out, iter);

  if (first_fun != nao) {
    val next_fun;
    va_start (vl, first_fun);
    iter = list_collect(iter, first_fun);

    while ((next_fun = va_arg(vl, val)) != nao)
      iter = list_collect(iter, next_fun);

    va_end (vl);
  }

  return func_f0v(out, do_and);
}

val andv(struct args *funlist)
{
  return func_f0v(args_get_list(funlist), do_and);
}

static val do_swap_12_21(val fun, val left, val right)
{
  return funcall2(fun, right, left);
}

val swap_12_21(val fun)
{
  return func_f2(fun, do_swap_12_21);
}

static val do_or(val fun1_list, struct args *args_in)
{
  cnum argc = args_in->argc;
  args_decl(args, argc);
  val ret = nil;

  fun1_list = nullify(fun1_list);

  gc_hint(fun1_list);

  for (; fun1_list; fun1_list = cdr(fun1_list)) {
    args_copy(args, args_in);
    if ((ret = generic_funcall(car(fun1_list), args)))
      break;
  }

  return ret;
}

val orf(val first_fun, ...)
{
  va_list vl;
  list_collect_decl (out, iter);

  if (first_fun != nao) {
    val next_fun;
    va_start (vl, first_fun);
    iter = list_collect(iter, first_fun);

    while ((next_fun = va_arg(vl, val)) != nao)
      iter = list_collect(iter, next_fun);

    va_end (vl);
  }

  return func_f0v(out, do_or);
}

val orv(struct args *funlist)
{
  return func_f0v(args_get_list(funlist), do_or);
}

static val do_not(val fun, struct args *args)
{
  return null(apply(fun, args_get_list(args)));
}

val notf(val fun)
{
  return func_f0v(fun, do_not);
}

static val do_iff(val env, struct args *args_in)
{
  cons_bind (condfun, choices, env);
  cons_bind (thenfun, elsefun, choices);
  args_decl(args, args_in->argc);

  args_copy(args, args_in);

  return if3(generic_funcall(condfun, args_in),
             generic_funcall(thenfun, args),
             if2(elsefun, generic_funcall(elsefun, args)));
}

val iff(val condfun, val thenfun, val elsefun)
{
  thenfun = default_arg(thenfun, identity_f);
  elsefun = default_null_arg(elsefun);
  return func_f0v(cons(condfun, cons(thenfun, elsefun)), do_iff);
}

val iffi(val condfun, val thenfun, val elsefun)
{
  elsefun = default_arg(elsefun, identity_f);
  return func_f0v(cons(condfun, cons(thenfun, elsefun)), do_iff);
}

static val do_dup(val fun, val arg)
{
  val arg1 = z(arg);
  val arg2 = arg1;
  return funcall2(fun, z(arg1), z(arg2));
}

val dupl(val fun)
{
  return func_f1(fun, do_dup);
}

val vector(val length, val initval)
{
  val self = lit("vector");
  unsigned i;
  ucnum len = c_unum(length, self);
  ucnum alloc_plus = len + 2;
  ucnum size = if3(alloc_plus > len, alloc_plus, (ucnum) -1);
  val *v = coerce(val *, chk_xalloc(size, sizeof *v, self));
  val vec = make_obj();
  vec->v.type = VEC;
  initval = default_null_arg(initval);
#if HAVE_VALGRIND
  vec->v.vec_true_start = v;
#endif
  v += 2;
  vec->v.vec = v;
  v[vec_alloc] = length;
  v[vec_length] = length;
  for (i = 0; i < alloc_plus - 2; i++)
    vec->v.vec[i] = initval;
  return vec;
}

val vectorp(val vec)
{
  return type(vec) == VEC ? t : nil;
}

val vec_set_length(val vec, val length)
{
  val self = lit("vec-set-length");
  type_check(self, vec, VEC);

  {
    cnum new_length = c_num(length, self);
    cnum old_length = c_num(vec->v.vec[vec_length], self);
    cnum old_alloc = c_num(vec->v.vec[vec_alloc], self);

    if (new_length < 0)
      uw_throwf(error_s, lit("~a: negative length ~s specified"),
                self, length, nao);

    if (new_length > INT_PTR_MAX - 2)
    {
      uw_throwf(error_s, lit("~a: cannot extend to length ~s"),
                self, length, nao);
    }

    if (new_length > old_alloc) {
      cnum new_alloc;
      val *newvec;

      if (old_alloc > (INT_PTR_MAX - 2) - (INT_PTR_MAX - 2) / 5)
        new_alloc = INT_PTR_MAX - 2;
      else
        new_alloc = max(new_length, old_alloc + old_alloc / 4);

      newvec = coerce(val *, chk_realloc(coerce(mem_t *, vec->v.vec - 2),
                                         (new_alloc + 2) * sizeof *newvec));
      vec->v.vec = newvec + 2;
      set(mkloc(vec->v.vec[vec_alloc], vec), num(new_alloc));
#if HAVE_VALGRIND
      vec->v.vec_true_start = newvec;
#endif
    }

    if (new_length > old_length) {
      cnum i;
      for (i = old_length; i < new_length; i++)
        vec->v.vec[i] = nil;
    }

    set(mkloc(vec->v.vec[vec_length], vec), length);
  }

  return vec;
}

val vecref(val vec, val ind)
{
  val self = lit("vecref");
  cnum index = c_num(ind, self);
  cnum len = c_num(length_vec(vec), self);
  if (index < 0)
    index = len + index;
  if (index < 0 || index >= len)
    uw_throwf(error_s, lit("~a: ~s is out of range for vector ~s"),
              self, ind, vec, nao);
  return vec->v.vec[index];
}

loc vecref_l(val vec, val ind)
{
  val self = lit("vecref");
  cnum index = c_num(ind, self);
  cnum len = c_num(length_vec(vec), self);
  if (index < 0)
    index = len + index;
  if (index < 0 || index >= len)
    uw_throwf(error_s, lit("~a: ~s is out of range for vector ~s"),
              self, ind, vec, nao);
  return mkloc(vec->v.vec[index], vec);
}

val vec_push(val vec, val item)
{
  val length = length_vec(vec);
  vec_set_length(vec, plus(length, one));
  set(vecref_l(vec, length), item);
  return length;
}

val length_vec(val vec)
{
  type_check(lit("length-vec"), vec, VEC);
  return vec->v.vec[vec_length];
}

val size_vec(val vec)
{
  type_check(lit("size-vec"), vec, VEC);
  return vec->v.vec[vec_alloc];
}

val vectorv(struct args *args)
{
  cnum index = 0;
  val vec = vector(zero, nil);

  while (args_more(args, index))
    vec_push(vec, args_get(args, &index));

  return vec;
}

val vec(val first, ...)
{
  va_list vl;
  val vec = vector(zero, nil);
  val next;
  int count;

  va_start (vl, first);

  for (next = first, count = 0;
       next != nao && count < 32;
       next = va_arg(vl, val), count++)
  {
    vec_push(vec, next);
  }

  if (count == 32 && next != nao)
    internal_error("runaway arguments in vec function");

  return vec;
}

val vec_list(val list)
{
  val vec = vector(zero, nil);

  for (; list; list = cdr(list))
    vec_push(vec, car(list));

  return vec;
}

val list_vec(val vec)
{
  val self = lit("list-vec");
  list_collect_decl (list, ptail);
  int i, len;

  type_check(self, vec, VEC);

  len = c_num(vec->v.vec[vec_length], self);

  for (i = 0; i < len; i++)
    ptail = list_collect(ptail, vec->v.vec[i]);

  return list;
}

val copy_vec(val vec_in)
{
  val self = lit("copy-vec");
  val length = length_vec(vec_in);
  ucnum alloc_plus = c_unum(length, self) + 2;
  val *v = coerce(val *, chk_xalloc(alloc_plus, sizeof *v, self));
  val vec = make_obj();
  vec->v.type = VEC;
#if HAVE_VALGRIND
  vec->v.vec_true_start = v;
#endif
  v += 2;
  vec->v.vec = v;
  v[vec_alloc] = length;
  v[vec_length] = length;
  memcpy(vec->v.vec, vec_in->v.vec, (alloc_plus - 2) * sizeof *vec->v.vec);
  return vec;
}

val sub_vec(val vec_in, val from, val to)
{
  val self = lit("sub-vec");
  val len = length_vec(vec_in);

  if (null_or_missing_p(from))
    from = zero;
  else if (from == t)
    from = len;
  else if (minusp(from)) {
    from = plus(from, len);
    if (to == zero)
      to = len;
  }

  if (null_or_missing_p(to) || to == t)
    to = len;
  else if (minusp(to))
    to = plus(to, len);

  from = max2(zero, min2(from, len));
  to = max2(zero, min2(to, len));

  if (ge(from, to)) {
    return vector(zero, nil);
  } else if (from == zero && eql(to, len)) {
    return vec_in;
  } else {
    cnum cfrom = c_num(from, self);
    size_t nelem = c_num(to, self) - cfrom;
    val *v = coerce(val *, chk_xalloc((nelem + 2), sizeof *v, self));
    val vec = make_obj();
    vec->v.type = VEC;
#if HAVE_VALGRIND
    vec->v.vec_true_start = v;
#endif
    v += 2;
    vec->v.vec = v;
    v[vec_length] = v[vec_alloc] = unum(nelem);
    memcpy(vec->v.vec, vec_in->v.vec + cfrom, nelem * sizeof *vec->v.vec);
    return vec;
  }
}

val replace_vec(val vec_in, val items, val from, val to)
{
  val self = lit("replace-vec");
  val len = length_vec(vec_in);

  if (missingp(from)) {
    from = zero;
  } else if (from == t) {
    from = len;
  } else if (!integerp(from)) {
    seq_iter_t wh_iter, item_iter;
    val wh, item;
    seq_iter_init(self, &wh_iter, from);
    seq_iter_init(self, &item_iter, items);

    if (!missingp(to))
      uw_throwf(error_s,
                lit("~a: to-arg not applicable when from-arg is a list"),
                self, nao);

    while (seq_get(&wh_iter, &wh) && seq_get(&item_iter, &item)) {
      if (ge(wh, len))
        break;
      set(vecref_l(vec_in, wh), item);
    }

    return vec_in;
  } else if (minusp(from)) {
    from = plus(from, len);
    if (to == zero)
      to = len;
  }

  if (null_or_missing_p(to) || to == t)
    to = len;
  else if (minusp(to))
    to = plus(to, len);

  from = max2(zero, min2(from, len));
  to = max2(zero, min2(to, len));

  {
    val len_rep = minus(to, from);
    val len_it = length(items);

    if (gt(len_rep, len_it)) {
      val len_diff = minus(len_rep, len_it);
      cnum t = c_num(to, self);
      cnum l = c_num(len, self);

      memmove(vec_in->v.vec + t - c_num(len_diff, self),
              vec_in->v.vec + t,
              (l - t) * sizeof vec_in->v.vec);

      vec_in->v.vec[vec_length] = minus(len, len_diff);
      to = plus(from, len_it);
    } else if (lt(len_rep, len_it)) {
      val len_diff = minus(len_it, len_rep);
      cnum t = c_num(to, self);
      cnum l = c_num(len, self);

      vec_set_length(vec_in, plus(len, len_diff));

      memmove(vec_in->v.vec + t + c_num(len_diff, self),
              vec_in->v.vec + t,
              (l - t) * sizeof vec_in->v.vec);
      to = plus(from, len_it);
    }

    if (zerop(len_it))
      return vec_in;
    if (vectorp(items)) {
      memmove(vec_in->v.vec + c_num(from, self), items->v.vec,
              sizeof *vec_in->v.vec * c_num(len_it, self));
      mut(vec_in);
    } else {
      seq_iter_t item_iter;
      seq_iter_init(self, &item_iter, items);
      int mut_needed = 0;
      cnum f = c_num(from, self);
      cnum t = c_num(to, self);

      for (; f != t; f++) {
        val item = seq_geti(&item_iter);
        if (is_ptr(item))
          mut_needed = 1;
        vec_in->v.vec[f] = item;
      }

      if (mut_needed)
        mut(vec_in);
    }
  }

  return vec_in;
}

val replace_obj(val obj, val items, val from, val to)
{
  val self = lit("replace");
  val lambda_set_meth = get_special_slot(obj, lambda_set_m);

  if (!lambda_set_meth)
    uw_throwf(error_s, lit("~a: object ~s lacks ~s method"),
              self, obj, lambda_set_s, nao);

  if (listp(from)) {
    if (!missingp(to))
      uw_throwf(error_s,
                lit("~a: to-arg not applicable when from-arg is a list"),
                self, nao);

    (void) funcall3(slot(obj, lambda_set_s), obj, from, items);
  } else {
    (void) funcall3(slot(obj, lambda_set_s), obj, rcons(from, to), items);
  }

  return obj;
}

val fill_vec(val vec, val item, val from_in, val to_in)
{
  val self = lit("fill-vec");
  val len = length_vec(vec);
  cnum from = c_num(default_arg(from_in, zero), self);
  cnum to = c_num(default_arg(to_in, len), self);
  cnum l = c_num(len, self);
  cnum i;

  if (from < 0)
    from += l;

  if (to < 0)
    to += l;

  if (from < 0 || from > l)
    uw_throwf(error_s, lit("~a: from index ~s is out of range for vector ~s"),
              self, num(from), vec, nao);

  if (to < 0 || to > l)
    uw_throwf(error_s, lit("~a: to index ~s is out of range for vector ~s"),
              self, num(to), vec, nao);

  if (from >= to)
    return vec;

  for (i = from; i < to - 1; i++)
    vec->v.vec[i] = item;

  set(mkloc(vec->v.vec[i], vec), item);

  return vec;
}

val cat_vec(val list)
{
  val self = lit("cat-vec");
  ucnum total = 0;
  val iter;
  val vec, *v;

  list = nullify(list);

  for (iter = list; iter != nil; iter = cdr(iter)) {
    ucnum newtot = total + c_unum(length_vec(car(iter)), self);
    if (newtot < total)
      goto toobig;
    total = newtot;
  }

  if (total + 2 < total)
    goto toobig;

  v = coerce(val *, chk_xalloc(total + 2, sizeof *v, self));
  vec = make_obj();
  vec->v.type = VEC;

#if HAVE_VALGRIND
  vec->v.vec_true_start = v;
#endif
  v += 2;
  vec->v.vec = v;
  v[vec_length] = v[vec_alloc] = num(total);

  for (iter = list; iter != nil; iter = cdr(iter)) {
    val item = car(iter);
    cnum len = c_num(item->v.vec[vec_length], self);
    memcpy(v, item->v.vec, len * sizeof *v);
    v += len;
  }

  return vec;
toobig:
  uw_throwf(error_s, lit("~a: resulting vector too large"), self, nao);
}

static val simple_lazy_stream_func(val stream, val lcons)
{
  if (set(mkloc(lcons->lc.car, lcons), get_line(stream)) != nil) {
    set(mkloc(lcons->lc.cdr, lcons), make_lazy_cons(us_lcons_fun(lcons)));
  } else {
    close_stream(stream, t);
    lcons->lc.cdr = nil;
  }

  return nil;
}

static val lazy_stream_cont(val stream, val func, val env)
{
  val next = get_line(stream);

  if (!next) {
    close_stream(stream, t);
    return nil;
  }

  rplacd(env, next);
  return make_lazy_cons(func);
}

static val lazy_stream_func(val env, val lcons)
{
  val stream = car(env);
  val prefetched_line = cdr(env);

  set(mkloc(lcons->lc.car, lcons), prefetched_line);
  set(mkloc(lcons->lc.cdr, lcons), lazy_stream_cont(stream,
                                                    us_lcons_fun(lcons), env));

  return prefetched_line;
}

val lazy_stream_cons(val stream)
{
  stream = default_arg_strict(stream, std_input);

  if (real_time_stream_p(stream)) {
    return make_lazy_cons(func_f1(stream, simple_lazy_stream_func));
  } else {
    val first = get_line(stream);

    if (!first) {
      close_stream(stream, t);
      return nil;
    }

    return make_lazy_cons(func_f1(cons(stream, first),
                                  lazy_stream_func));
  }
}

val lazy_str(val lst, val term, val limit)
{
  val obj = make_obj();
  obj->ls.type = LSTR;

  /* Must init before calling something that can gc! */
  obj->ls.list = obj->ls.prefix = nil;
  obj->ls.props = coerce(struct lazy_string_props *,
                         chk_calloc(1, sizeof *obj->ls.props));

  term = default_arg(term, lit("\n"));
  limit = default_null_arg(limit);

  if (nilp(lst)) {
    obj->ls.prefix = null_string;
    obj->ls.list = nil;
  } else {
    val prefix = scat(nil, first(lst), term, nao);
    set(mkloc(obj->ls.prefix, obj), prefix);
    set(mkloc(obj->ls.list, obj), rest(lst));
    limit = if2(limit, minus(limit, one));
  }

  set(mkloc(obj->ls.props->term, obj), term);
  set(mkloc(obj->ls.props->limit, obj), limit);

  return obj;
}

static val copy_lazy_str(val lstr)
{
  val obj = make_obj();
  obj->ls.type = LSTR;
  obj->ls.prefix = nil;
  obj->ls.list = lstr->ls.list;
  obj->ls.props = coerce(struct lazy_string_props *,
                         chk_copy_obj(coerce(mem_t *, lstr->ls.props),
                                      sizeof *lstr->ls.props));
  obj->ls.prefix = copy_str(lstr->ls.prefix);
  return obj;
}

val lazy_str_force(val lstr)
{
  val lim, term, pfx;
  type_check(lit("lazy-str-force"), lstr, LSTR);
  lim = lstr->ls.props->limit;
  term = lstr->ls.props->term;
  pfx = lstr->ls.prefix;

  while ((!lim || gt(lim, zero)) && lstr->ls.list) {
    val next = pop(&lstr->ls.list);
    if (!next)
      break;
    string_extend(pfx, next);
    string_extend(pfx, term);
    if (lim)
      lim = minus(lim, one);
  }

  if (lim)
    set(mkloc(lstr->ls.props->limit, lstr), lim);

  return lstr->ls.prefix;
}

INLINE cnum max_str_chars(cnum max_len)
{
  return max_len < INT_PTR_MAX / 8 ? 8 * max(3, max_len) : INT_PTR_MAX;
}

val lazy_str_put(val lstr, val stream, struct strm_base *s)
{
  val self = lit("lazy-str-put");
  val lim = lstr->ls.props->limit;
  val term = lstr->ls.props->term;
  val iter;
  cnum max_len = s->max_length;
  cnum max_chr = max_len ? max_str_chars(max_len) : 0;

  put_string(lstr->ls.prefix, stream);

  for (iter = lstr->ls.list; (!lim || gt(lim, zero)) && iter;
       iter = cdr(iter))
  {
    val str = car(iter);
    if (!str)
      break;
    if (max_len) {
      if (length_str_gt(str, num(max_chr))) {
        put_string(sub_str(str, zero, num(max_chr)), stream);
        goto max_reached;
      }
      if (--max_chr == 0)
        goto max_reached;
      max_chr -= c_num(length_str(str), self);
    }
    if (lim)
      lim = pred(lim);
    put_string(str, stream);
    put_string(term, stream);
  }

  return t;

max_reached:
  put_string(lit("..."), stream);
  put_string(term, stream);
  return t;
}

val lazy_str_force_upto(val lstr, val index)
{
  uses_or2;
  val lim, term, ltrm, pfx, len;
  type_check(lit("lazy-str-force-upto"), lstr, LSTR);
  lim = lstr->ls.props->limit;
  term = lstr->ls.props->term;
  ltrm = length_str(term);
  pfx = lstr->ls.prefix;
  len = length_str(pfx);

  while (ge(index, len) && lstr->ls.list &&
         or2(null(lim),gt(lim,zero)))
  {
    val next = pop(&lstr->ls.list);
    if (!next)
      break;
    string_extend(pfx, next);
    string_extend(pfx, term);
    if (lim)
      lim = minus(lim, one);
    len = plus(len, length_str(next));
    len = plus(len, ltrm);
  }

  if (lim)
    set(mkloc(lstr->ls.props->limit, lstr), lim);

  return lt(index, len);
}

val length_str_gt(val str, val len)
{
  val self = lit("length-str-gt");
  switch (type(str)) {
  case LIT:
    {
      const wchar_t *cstr = c_str(str, self);
      size_t clen = c_num(len, self);
      const wchar_t *nult = wmemchr(cstr, 0, clen + 1);
      return nult == 0 ? t : nil;
    }
  case STR:
    return gt(length_str(str), len);
  case LSTR:
    lazy_str_force_upto(str, len);
    return gt(length_str(str->ls.prefix), len);
  default:
    type_mismatch(lit("~a: ~s is not a string"), self, str, nao);
  }
}

val length_str_ge(val str, val len)
{
  val self = lit("length-str-ge");
  switch (type(str)) {
  case LIT:
    {
      const wchar_t *cstr = c_str(str, self);
      size_t clen = c_num(len, self);
      const wchar_t *nult = wmemchr(cstr, 0, clen);
      return nult == 0 ? t : nil;
    }
  case STR:
    return ge(length_str(str), len);
  case LSTR:
    lazy_str_force_upto(str, len);
    return ge(length_str(str->ls.prefix), len);
  default:
    type_mismatch(lit("~a: ~s is not a string"), self, str, nao);
  }
}

val length_str_lt(val str, val len)
{
  val self = lit("length-str-lt");
  switch (type(str)) {
  case LIT:
    {
      const wchar_t *cstr = c_str(str, self);
      size_t clen = c_num(len, self);
      const wchar_t *nult = wmemchr(cstr, 0, clen);
      return nult != 0 ? t : nil;
    }
  case STR:
    return lt(length_str(str), len);
  case LSTR:
    lazy_str_force_upto(str, len);
    return lt(length_str(str->ls.prefix), len);
  default:
    type_mismatch(lit("~a: ~s is not a string"), self, str, nao);
  }
}

val length_str_le(val str, val len)
{
  val self = lit("length-str-le");
  switch (type(str)) {
  case LIT:
    {
      const wchar_t *cstr = c_str(str, self);
      size_t clen = c_num(len, self);
      const wchar_t *nult = wmemchr(cstr, 0, clen + 1);
      return nult != 0 ? t : nil;
    }
  case STR:
    return le(length_str(str), len);
  case LSTR:
    lazy_str_force_upto(str, len);
    return le(length_str(str->ls.prefix), len);
  default:
    type_mismatch(lit("~a: ~s is not a string"), self, str, nao);
  }
}

val lazy_str_get_trailing_list(val lstr, val index)
{
  type_check(lit("lazy-str-get-trailing-list"), lstr, LSTR);

  /* Force lazy string up through the index position */
  if (ge(index, length_str(lstr->ls.prefix)))
    lazy_str_force_upto(lstr, index);

  {
    uses_or2;
    val split_suffix = split_str(sub_str(lstr->ls.prefix, index, nil),
                                 or2(lstr->ls.props->term, lit("\n")));

    if (!cdr(split_suffix) && equal(car(split_suffix), null_string))
      return lstr->ls.list;

    return nappend2(split_suffix, lstr->ls.list);
  }
}

struct cobj_class *cobj_register(val cls_sym)
{
  if ((size_t) (cobj_ptr - cobj_class) >= nelem(cobj_class))
    internal_error("cobj array too small");
  cobj_ptr->cls_sym = cls_sym;
  return cobj_ptr++;
}

struct cobj_class *cobj_register_super(val cls_sym, struct cobj_class *super)
{
  struct cobj_class *cls = cobj_register(cls_sym);
  cls->super = super;
  return cls;
}

static void cobj_populate_hash(void)
{
  struct cobj_class *ptr;
  for (ptr = cobj_class; ptr < cobj_ptr; ptr++)
    sethash(cobj_hash, ptr->cls_sym, num_fast(ptr - cobj_class));
}

val cobj(mem_t *handle, struct cobj_class *cls, struct cobj_ops *ops)
{
  if (cls != 0) {
    val obj = make_obj();
    obj->co.type = COBJ;
    obj->co.handle = handle;
    obj->co.ops = ops;
    obj->co.cls = cls;
    return obj;
  }
  internal_error("cobj creation with null class pointer");
}

val cobjp(val obj)
{
  return type(obj) == COBJ ? t : nil;
}

val cobjclassp(val obj, struct cobj_class *cls)
{
  if (is_ptr(obj) && obj->t.type == COBJ) {
    struct cobj_class *pcls;
    for (pcls = obj->co.cls; pcls != 0; pcls = pcls->super)
      if (pcls == cls)
        return t;
  }
  return nil;
}

mem_t *cobj_handle(val self, val cobj, struct cobj_class *cls)
{
  class_check(self, cobj, cls);
  return cobj->co.handle;
}

struct cobj_ops *cobj_ops(val self, val cobj, struct cobj_class *cls)
{
  class_check(self, cobj, cls);
  return cobj->co.ops;
}

void cobj_print_op(val obj, val out, val pretty, struct strm_ctx *ctx)
{
  put_string(lit("#<"), out);
  obj_print_impl(obj->co.cls->cls_sym, out, pretty, ctx);
  format(out, lit(": ~p>"), coerce(val, obj->co.handle), nao);
}

void cptr_print_op(val obj, val out, val pretty, struct strm_ctx *ctx)
{
  put_string(lit("#<cptr"), out);
  if (obj->co.cls) {
    put_char(chr(' '), out);
    obj_print_impl(obj->cp.cls, out, pretty, ctx);
  }
  format(out, lit(": ~p>"), coerce(val, obj->co.handle), nao);
}


val cobj_equal_handle_op(val left, val right)
{
  return (left->co.handle == right->co.handle) ? t : nil;
}

ucnum cobj_handle_hash_op(val obj, int *count, ucnum seed)
{
  mem_t *handle = obj->co.handle;
  return cobj_eq_hash_op(coerce(val, handle), count, seed);
}

static struct cobj_ops cptr_ops = cobj_ops_init(cobj_equal_handle_op,
                                                cptr_print_op,
                                                cobj_destroy_stub_op,
                                                cobj_mark_op,
                                                cobj_handle_hash_op);

val cptr_typed(mem_t *handle, val type_sym, struct cobj_ops *ops)
{
  val obj = make_obj();
  obj->cp.type = CPTR;
  obj->cp.handle = handle;
  obj->cp.ops = (ops != 0 ? ops : &cptr_ops);
  obj->cp.cls = type_sym;
  return obj;
}

val cptr(mem_t *ptr)
{
  return cptr_typed(ptr, nil, &cptr_ops);
}

val cptrp(val obj)
{
  return type(obj) == CPTR ? t : nil;
}

val cptr_type(val cptr)
{
  (void) cptr_handle(cptr, nil, lit("cptr-type"));
  return cptr->cp.cls;
}

val cptr_size_hint(val cptr, val size)
{
  val self = lit("cptr-size-hint");
  (void) cptr;
  malloc_bytes += c_unum(size, self);
  return nil;
}

val cptr_int(val n, val type_sym_in)
{
  val self = lit("cptr-int");
  val type_sym = default_null_arg(type_sym_in);
  return cptr_typed(coerce(mem_t *, c_num(n, self)), type_sym, 0);
}

val cptr_obj(val obj, val type_sym_in)
{
  val type_sym = default_null_arg(type_sym_in);
  return cptr_typed(coerce(mem_t *, obj), type_sym, 0);
}

val cptr_buf(val buf, val type_sym_in)
{
  val type_sym = default_null_arg(type_sym_in);
  mem_t *ptr = buf_get(buf, lit("cptr-buf"));
  return cptr_typed(ptr, type_sym, 0);
}

val cptr_zap(val cptr)
{
  (void) cptr_handle(cptr, nil, lit("cptr-zap"));
  cptr->co.handle = 0;
  return cptr;
}

val cptr_free(val cptr)
{
  (void) cptr_handle(cptr, nil, lit("cptr-free"));
  free(cptr->co.handle);
  cptr->co.handle = 0;
  return cptr;
}

val cptr_cast(val to_type, val cptr)
{
  mem_t *ptr = cptr_handle(cptr, nil, lit("cptr-cast"));
  return cptr_typed(ptr, to_type, 0);
}

val int_cptr(val cptr)
{
  return num(coerce(cnum, cptr_handle(cptr, nil, lit("int-cptr"))));
}

mem_t *cptr_handle(val cptr, val type_sym, val self)
{
  if (type(cptr) != CPTR) {
    uw_throwf(error_s, lit("~a: ~s isn't a cptr"), self, cptr, nao);
  } else {
    mem_t *ptr = cptr->cp.handle;

    if (type_sym && cptr->cp.cls != type_sym && (ptr != 0 || cptr->cp.cls))
      uw_throwf(error_s, lit("~a: cptr ~s isn't of type ~s"), self, cptr,
                type_sym, nao);

    return ptr;
  }
}

mem_t *cptr_get(val cptr)
{
  return cptr_handle(cptr, nil, lit("cptr-get"));
}

mem_t **cptr_addr_of(val cptr, val type_sym, val self)
{
  (void) cptr_handle(cptr, type_sym, self);
  return &cptr->cp.handle;
}

val assoc(val key, val list)
{
  list = nullify(list);

  while (list) {
    val elem = car(list);
    if (equal(car(elem), key))
      return elem;
    list = cdr(list);
  }

  return nil;
}

val assql(val key, val list)
{
  list = nullify(list);

  while (list) {
    val elem = car(list);
    if (eql(car(elem), key))
      return elem;
    list = cdr(list);
  }

  return nil;
}

val assq(val key, val list)
{
  list = nullify(list);

  while (list) {
    val elem = car(list);
    if (eq(car(elem), key))
      return elem;
    list = cdr(list);
  }

  return nil;
}

val rassoc(val key, val list)
{
  list = nullify(list);

  while (list) {
    val elem = car(list);
    if (equal(cdr(elem), key))
      return elem;
    list = cdr(list);
  }

  return nil;
}

val rassql(val key, val list)
{
  list = nullify(list);

  while (list) {
    val elem = car(list);
    if (eql(cdr(elem), key))
      return elem;
    list = cdr(list);
  }

  return nil;
}

val rassq(val key, val list)
{
  list = nullify(list);

  while (list) {
    val elem = car(list);
    if (eq(cdr(elem), key))
      return elem;
    list = cdr(list);
  }

  return nil;
}

val acons(val car, val cdr, val list)
{
  return cons(cons(car, cdr), list);
}

val acons_new(val key, val value, val list)
{
  val existing = assoc(key, list);

  if (existing) {
    rplacd(existing, value);
    return list;
  } else {
    return cons(cons(key, value), list);
  }
}

val acons_new_c(val key, loc new_p, loc list)
{
  val existing = assoc(key, deref(list));

  if (existing) {
    if (!nullocp(new_p))
      deref(new_p) = nil;
    return existing;
  } else {
    val nc = cons(key, nil);
    set(list, cons(nc, deref(list)));
    if (!nullocp(new_p))
      deref(new_p) = t;
    return nc;
  }
}

val aconsql_new(val key, val value, val list)
{
  val existing = assql(key, list);

  if (existing) {
    rplacd(existing, value);
    return list;
  } else {
    return cons(cons(key, value), list);
  }
}

val aconsql_new_c(val key, loc new_p, loc list)
{
  val existing = assql(key, deref(list));

  if (existing) {
    if (!nullocp(new_p))
      deref(new_p) = nil;
    return existing;
  } else {
    val nc = cons(key, nil);
    set(list, cons(nc, deref(list)));
    if (!nullocp(new_p))
      deref(new_p) = t;
    return nc;
  }
}


static val alist_remove_test(val item, val key)
{
  return equal(car(item), key);
}

val alist_remove(val list, val keys)
{
  return set_diff(list, keys, func_n2(alist_remove_test), nil);
}

val alist_removev(val list, struct args *keys)
{
  return alist_remove(list, args_get_list(keys));
}

val alist_nremove(val list, val keys)
{
  loc plist = mkcloc(list);

  while (deref(plist)) {
    if (memqual(car(car(deref(plist))), keys))
      set(plist, cdr(deref(plist)));
    else
      plist = cdr_l(deref(plist));
  }

  return list;
}

val alist_nremovev(val list, struct args *keys)
{
  return alist_nremove(list, args_get_list(keys));
}

val alist_nremove1(val list, val key)
{
  loc plist = mkcloc(list);

  while (deref(plist)) {
    if (equal(car(car(deref(plist))), key))
      set(plist, cdr(deref(plist)));
    else
      plist = cdr_l(deref(plist));
  }

  return list;
}

val copy_cons(val cell)
{
  switch (type(cell)) {
  case CONS:
  case LCONS:
    {
      val obj = make_obj();
      *obj = *cell;
      return obj;
    }
  default:
    type_mismatch(lit("copy-cons: ~s is not a cons"), cell, nao);
  }
}

val copy_tree(val tree)
{
  if (atom(tree)) {
    return tree;
  } else {
    val car = copy_tree(tree->c.car);
    val cdr = copy_tree(tree->c.cdr);
    val copy = make_obj();
    *copy = *tree;
    copy->c.car = car;
    copy->c.cdr = cdr;
    return copy;
  }
}

val copy_alist(val list)
{
  list_collect_decl (out, ptail);
  for (; !endp(list); list = cdr(list))
    ptail = list_collect(ptail, copy_cons(car(list)));
  return out;
}

val mapcar_listout(val fun, val seq)
{
  val self = lit("mapcar");
  seq_iter_t iter;
  seq_iter_init(self, &iter, seq);
  val elem;
  list_collect_decl (out, ptail);

  while (seq_get(&iter, &elem))
    ptail = list_collect(ptail, funcall1(fun, elem));

  return out;
}

val mapcar(val fun, val seq)
{
  return make_like(mapcar_listout(fun, seq), seq);
}

val mapcon(val fun, val list)
{
  list_collect_decl (out, iter);
  val list_orig = list;

  list = nullify(list);

  for (; list; list = cdr(list))
    iter = list_collect_nconc(iter, funcall1(fun, list));

  return make_like(out, list_orig);
}

val mappend(val fun, val seq)
{
  val self = lit("mappend");
  seq_iter_t iter;
  seq_iter_init(self, &iter, seq);
  val elem;
  list_collect_decl (out, ptail);

  while (seq_get(&iter, &elem))
    ptail = list_collect_append(ptail, funcall1(fun, elem));

  return make_like(out, seq);
}

val mapdo(val fun, val seq)
{
  val self = lit("mapdo");
  seq_iter_t iter;
  seq_iter_init(self, &iter, seq);
  val elem;

  while (seq_get(&iter, &elem))
    funcall1(fun, elem);

  return nil;
}

static cnum calc_win_size(cnum ra)
{
  cnum ws = 2 * ra + 1;
  if (ra < 1)
    uw_throwf(error_s, lit("window-map: range must be nonnegative"), nao);
  if (ws > 1025)
    uw_throwf(error_s, lit("window-map: window size exceeds 1025"), nao);
  return ws;
}

enum wmap_op {
  WMAP_MAP, WMAP_MAPPEND, WMAP_MAPDO
};

static val window_map_list(val range, val boundary, val fun, val list,
                           enum wmap_op op)
{
  val self = lit("window-map");
  cnum i, j, ra = c_fixnum(range, self), ws = calc_win_size(ra);
  val iter;
  args_decl (args, ws);
  list_collect_decl (out, ptail);

  args_set_fill(args, ws);

  if (boundary == wrap_k || boundary == reflect_k) {
    val lw = sub(list, num_fast(-ra), t), lwing = lw;
    val rw = sub(list, zero, range), rwing = rw;
    cnum i, len = c_fixnum(length(list), self);

    if (boundary == reflect_k) {
      lwing = reverse(rw);
      rwing = reverse(lw);
      lw = lwing;
      rw = rwing;
    }

    for (i = len; i < ra; i += len) {
      lwing = append2(lwing, lw);
      rwing = append2(rwing, rw);
    }

    if (len < ra)
      boundary = append2(sub(lwing, num_fast(-ra), t), sub(rwing, zero, range));
    else
      boundary = append2(lwing, rwing);
  }

  for (i = 0; i < ra; i++)
    args->arg[i] = ref(boundary, num_fast(i));

  for (iter = list; iter && i < ws; iter = cdr(iter), i++)
    args->arg[i] = car(iter);

  for (j = ra; i < ws; i++)
    args->arg[i] = ref(boundary, num(j++));

  for (;;) {
    args_decl (args_cp, ws);
    args_copy(args_cp, args);
    val item = generic_funcall(fun, args_cp);

    switch (op) {
    case WMAP_MAP: ptail = list_collect(ptail, item); break;
    case WMAP_MAPPEND: ptail = list_collect_append(ptail, item); break;
    case WMAP_MAPDO: (void) item; break;
    }

    if (nilp(list = cdr(list)))
      break;

    for (i = 0; i < ws - 1; i++)
      args->arg[i] = args->arg[i + 1];

    if (iter) {
      args->arg[i] = car(iter);
      iter = cdr(iter);
    } else {
      args->arg[i] = ref(boundary, num(j++));
    }
  }

  return out;
}

static val window_map_vec(val range, val boundary, val fun, val seq,
                          enum wmap_op op)
{
  val list = tolist(seq);
  val out = window_map_list(range, boundary, fun, list, op);
  return make_like(out, seq);
}

val window_map(val range, val boundary, val fun, val seq)
{
  switch (type(seq)) {
  case NIL:
    return nil;
  case CONS:
  case LCONS:
    return window_map_list(range, boundary, fun, seq, WMAP_MAP);
  case VEC:
  case LIT:
  case STR:
  case LSTR:
    return window_map_vec(range, boundary, fun, seq, WMAP_MAP);
  default:
    type_mismatch(lit("window-map: ~s is not a sequence"), seq, nao);
  }
}

val window_mappend(val range, val boundary, val fun, val seq)
{
  switch (type(seq)) {
  case NIL:
    return nil;
  case CONS:
  case LCONS:
    return window_map_list(range, boundary, fun, seq, WMAP_MAPPEND);
  case VEC:
  case LIT:
  case STR:
  case LSTR:
    return window_map_vec(range, boundary, fun, seq, WMAP_MAPPEND);
  default:
    type_mismatch(lit("window-mappend: ~s is not a sequence"), seq, nao);
  }
}

val window_mapdo(val range, val boundary, val fun, val seq)
{
  switch (type(seq)) {
  case NIL:
    return nil;
  case CONS:
  case LCONS:
    (void) window_map_list(range, boundary, fun, seq, WMAP_MAPDO);
    return nil;
  case VEC:
  case LIT:
  case STR:
  case LSTR:
    (void) window_map_vec(range, boundary, fun, seq, WMAP_MAPDO);
    return nil;
  default:
    type_mismatch(lit("window-mapdo: ~s is not a sequence"), seq, nao);
  }
}

static val lazy_interpose_func(val sep, val lcons)
{
  val list = us_car(lcons);
  val next = cdr(list);
  val fun = us_lcons_fun(lcons);

  us_rplaca(lcons, car(list));

  if (next)
    us_rplacd(lcons, cons(sep, make_lazy_cons_car(fun, next)));

  return nil;
}

static val lazy_interpose(val sep, val list)
{
  return make_lazy_cons_car(func_f1(sep, lazy_interpose_func), list);
}

val interpose(val sep, val seq)
{
  switch (type(seq)) {
  case NIL:
    return nil;
  case CONS:
    {
      val next;
      list_collect_decl (out, ptail);
      for (next = cdr(seq); next; seq = next, next = cdr(seq)) {
        ptail = list_collect(ptail, car(seq));
        ptail = list_collect(ptail, sep);
        if (lconsp(next)) {
          list_collect_nconc(ptail, lazy_interpose(sep, next));
          return out;
        }
      }
      list_collect(ptail, car(seq));
      return out;
    }
  case LCONS:
    return lazy_interpose(sep, seq);
  case LIT:
  case STR:
  case LSTR:
    return cat_str(interpose(sep, tolist(seq)), nil);
  case VEC:
    return vec_list(interpose(sep, tolist(seq)));
  default:
    type_mismatch(lit("interpose: ~s is not a sequence"), seq, nao);
  }
}

val merge(val list1, val list2, val lessfun, val keyfun)
{
  list_collect_decl (out, ptail);

  while (list1 && list2) {
    val el1 = funcall1(keyfun, first(list1));
    val el2 = funcall1(keyfun, first(list2));

    if (funcall2(lessfun, el2, el1)) {
      loc pnext = cdr_l(list2);
      val next = deref(pnext);
      deref(pnext) = nil;
      ptail = list_collect_nconc(ptail, list2);
      list2 = next;
    } else {
      loc pnext = cdr_l(list1);
      val next = deref(pnext);
      deref(pnext) = nil;
      ptail = list_collect_nconc(ptail, list1);
      list1 = next;
    }
  }

  if (list1)
    ptail = list_collect_nconc(ptail, list1);
  else
    ptail = list_collect_nconc(ptail, list2);

  return out;
}

static val sort_list(val list, val lessfun, val keyfun)
{
  if (list == nil)
    return nil;
  if (!cdr(list))
    return list;
  if (!cdr(cdr(list))) {
    if (funcall2(lessfun, funcall1(keyfun, second(list)),
                          funcall1(keyfun, first(list))))
    {
      val cons2 = cdr(list);
      rplacd(cons2, list);
      rplacd(list, nil);
      return cons2;
    } else {
      return list;
    }
  }

  {
    val bisect, iter;
    val list2;

    for (iter = cdr(cdr(list)), bisect = list; iter;
         bisect = cdr(bisect), iter = cdr(cdr(iter)))
      ; /* empty */

    list2 = cdr(bisect);
    rplacd(bisect, nil);

    return merge(sort_list(list, lessfun, keyfun),
                 sort_list(list2, lessfun, keyfun),
                 lessfun, keyfun);
  }
}

static void swap(val vec, val i, val j)
{
  if (i != j) {
    val temp = ref(vec, i);
    refset(vec, i, ref(vec, j));
    refset(vec, j, temp);
  }
}

static cnum med_of_three(val vec, val lessfun, val keyfun, cnum from, cnum to,
                         val *pkval)
{
  cnum mid = from + (to - from) / 2;
  val fval = ref(vec, num_fast(from));
  val mval = ref(vec, num_fast(mid));
  val tval = ref(vec, num_fast(to - 1));
  val fkval = funcall1(keyfun, fval);
  val mkval = funcall1(keyfun, mval);
  val tkval = funcall1(keyfun, tval);

  if (funcall2(lessfun, fkval, mkval)) {
    if (funcall2(lessfun, mkval, tval)) {
      *pkval = mkval;
      return mid;
    } else if (funcall2(lessfun, fkval, tkval)) {
      *pkval = tkval;
      return to - 1;
    } else {
      *pkval = fkval;
      return from;
    }
  } else {
    if (funcall2(lessfun, fkval, tval)) {
      *pkval = fkval;
      return from;
    } else if (funcall2(lessfun, mkval, tkval)) {
      *pkval = tkval;
      return to - 1;
    } else {
      *pkval = mkval;
      return mid;
    }
  }
}

static cnum middle_pivot(val vec, val keyfun, cnum from, cnum to,
                         val *pkval)
{
  cnum pivot = from + (to - from) / 2;
  val pval = ref(vec, num_fast(pivot));
  *pkval = funcall1(keyfun, pval);
  return pivot;
}

static void quicksort(val vec, val lessfun, val keyfun, cnum from, cnum to)
{
  while (to - from >= 2) {
    val pkval;
    cnum i, j;
    cnum pivot = if3(to - from > 15,
                     med_of_three(vec, lessfun, keyfun, from, to, &pkval),
                     middle_pivot(vec, keyfun, from, to, &pkval));

    swap(vec, num_fast(pivot), num_fast(to - 1));

    for (j = from, i = from; i < to - 1; i++)
      if (funcall2(lessfun, funcall1(keyfun, ref(vec, num_fast(i))), pkval))
        swap(vec, num_fast(i), num_fast(j++));

    swap(vec, num_fast(j), num_fast(to - 1));

    if (j - from > to - j) {
      quicksort(vec, lessfun, keyfun, j + 1, to);
      to = j;
    } else {
      quicksort(vec, lessfun, keyfun, from, j);
      from = j + 1;
    }
  }
}

static void sort_vec(val vec, val lessfun, val keyfun)
{
  val self = lit("sort");
  cnum len = c_fixnum(length(vec), self);
  quicksort(vec, lessfun, keyfun, 0, len);
}

val nsort(val seq, val lessfun, val keyfun)
{
  val self = lit("nsort");
  seq_info_t si = seq_info(seq);

  keyfun = default_arg(keyfun, identity_f);
  lessfun = default_arg(lessfun, less_f);

  switch (si.kind) {
  case SEQ_NIL:
    return nil;
  case SEQ_VECLIKE:
  case SEQ_HASHLIKE:
    sort_vec(seq, lessfun, keyfun);
    return seq;
  case SEQ_LISTLIKE:
    return sort_list(seq, lessfun, keyfun);
  case SEQ_TREELIKE:
  case SEQ_NOTSEQ:
    unsup_obj(self, seq);
  }

  abort();
}

val sort(val seq, val lessfun, val keyfun)
{
  val self = lit("sort");
  seq_info_t si = seq_info(seq);

  keyfun = default_arg(keyfun, identity_f);
  lessfun = default_arg(lessfun, less_f);

  switch (si.kind) {
  case SEQ_NIL:
    return nil;
  case SEQ_VECLIKE:
  case SEQ_HASHLIKE:
    seq = copy(seq);
    sort_vec(seq, lessfun, keyfun);
    return seq;
  case SEQ_LISTLIKE:
    return sort_list(copy_list(seq), lessfun, keyfun);
  case SEQ_TREELIKE:
  case SEQ_NOTSEQ:
    unsup_obj(self, seq);
  }

  abort();
}

val nshuffle(val seq, val randstate)
{
  seq_info_t si = seq_info(seq);

  switch (si.kind) {
  case SEQ_NIL:
    return nil;
  case SEQ_LISTLIKE:
    if (cdr(seq))
    {
      val v = nshuffle(vec_list(seq), randstate);
      val i, l;

      for (l = seq, i = zero; l; i = succ(i), l = cdr(l))
        rplaca(l, ref(v, i));
    }
    return seq;
  case SEQ_VECLIKE:
  case SEQ_HASHLIKE:
    {
      val rs = default_arg(randstate, random_state);
      val n = length(seq);
      val i;

      if (n == zero || n == one)
        return seq;

      for (i = pred(n); ge(i, one); i = pred(i)) {
        val j = random(rs, succ(i));
        val t = ref(seq, i);
        refset(seq, i, ref(seq, j));
        refset(seq, j, t);
      }

      return seq;
    }
  case SEQ_NOTSEQ:
  case SEQ_TREELIKE:
    unsup_obj(lit("nshuffle"), seq);
  }

  abort();
}

val shuffle(val seq, val randstate)
{
  if (seqp(seq))
    return nshuffle(copy(seq), randstate);
  type_mismatch(lit("nshuffle: ~s is not a sequence"), seq, nao);
}

static val multi_sort_less(val funcs_cons, val llist, val rlist)
{
  cons_bind (funcs, key_funcs, funcs_cons);
  val less = t;

  while (funcs) {
    val func = pop(&funcs);
    val test = pop(&key_funcs);
    val left = if3(test, funcall1(test, pop(&llist)), pop(&llist));
    val right = if3(test, funcall1(test, pop(&rlist)), pop(&rlist));

    if (funcall2(func, left, right))
      break;

    if (funcall2(func, right, left)) {
      less = nil;
      break;
    }
  }

  return less;
}

val multi_sort(val lists, val funcs, val key_funcs)
{
  val tuples = mapcarl(list_f, nullify(lists));

  if (tuples) {
    key_funcs = default_null_arg(key_funcs);

    if (functionp(funcs))
      funcs = cons(funcs, nil);

    tuples = sort_list(tuples, func_f2(cons(funcs, key_funcs),
                                       multi_sort_less), identity_f);

    return mapcarl(list_f, tuples);
  } else {
    list_collect_decl (out, ptail);
    for (; !endp(lists); lists = us_cdr(lists))
      ptail = list_collect(ptail, nil);
    return out;
  }
}

val sort_group(val seq, val keyfun, val lessfun)
{
  val kf = default_arg(keyfun, identity_f);
  val lf = default_arg(lessfun, less_f);
  val sorted = sort(seq, lf, kf);
  return partition_by(kf, sorted);
}

val unique(val seq, val keyfun, struct args *hashv_args)
{
  val self = lit("unique");
  val hash = hashv(hashv_args);
  val kf = default_arg(keyfun, identity_f);

  list_collect_decl (out, ptail);

  if (vectorp(seq) || stringp(seq)) {
    cnum i, len;

    for (i = 0, len = c_fixnum(length(seq), self); i < len; i++) {
      val new_p;
      val v = ref(seq, num_fast(i));

      (void) gethash_c(self, hash, funcall1(kf, v), mkcloc(new_p));

      if (new_p)
        ptail = list_collect(ptail, v);
    }
  } else {
    for (; seq; seq = cdr(seq)) {
      val new_p;
      val v = car(seq);

      (void) gethash_c(self, hash, funcall1(kf, v), mkcloc(new_p));

      if (new_p)
        ptail = list_collect(ptail, v);
    }
  }

  return make_like(out, seq);
}

val uniq(val seq)
{
  args_decl(hashv_args, ARGS_MIN);
  args_add(hashv_args, equal_based_k);
  return unique(seq, identity_f, hashv_args);
}

val grade(val seq, val lessfun, val keyfun_in)
{
  val self = lit("grade");
  seq_info_t si = seq_info(seq);

  if (si.kind != SEQ_NIL) {
    val keyfun = if3(missingp(keyfun_in),
                     car_f,
                     chain(car_f, keyfun_in, nao));
    cnum i, len = c_fixnum(length(seq), self);
    val iter, v = vector(num_fast(len), nil);

    switch (si.kind) {
    case SEQ_LISTLIKE:
      for (iter = si.obj, i = 0; i < len; i++, iter = cdr(iter)) {
        set(mkloc(v->v.vec[i], v), cons(car(iter), num_fast(i)));
      }
      break;
    case SEQ_VECLIKE:
      for (i = 0; i < len; i++) {
        val ix = num_fast(i);
        set(mkloc(v->v.vec[i], v), cons(ref(seq, ix), ix));
      }
      break;
    default:
      unsup_obj(self, seq);
    }

    {
      list_collect_decl (out, ptail);

      nsort(v, lessfun, keyfun);

      for (i = 0; i < len; i++)
        ptail = list_collect(ptail, cdr(v->v.vec[i]));

      return out;
    }
  }

  return nil;
}

val find(val item, val seq, val testfun, val keyfun)
{
  val self = lit("find");
  testfun = default_arg(testfun, equal_f);
  keyfun = default_arg(keyfun, identity_f);
  seq_info_t si = seq_info(seq);

  switch (si.kind) {
  case SEQ_NIL:
    return nil;
  case SEQ_LISTLIKE:
    {
      gc_hint(seq);

      for (seq = z(si.obj); seq; seq = cdr(seq)) {
        val elem = car(seq);
        val key = funcall1(keyfun, elem);

        if (funcall2(testfun, item, key))
          return elem;
      }
    }
    return nil;
  case SEQ_VECLIKE:
    switch (si.type) {
    case STR:
    case LIT:
      if (keyfun == identity_f &&
          (testfun == equal_f || testfun == eql_f || testfun == eq_f))
      {
        const wchar_t ch = c_chr(item);
        const wchar_t *cstr = c_str(seq, self);
        if (wcschr(cstr, ch))
          return item;
        return nil;
      }
      /* fallthrough */
    default:
      {
        val vec = si.obj;
        cnum len = c_fixnum(length(vec), self);
        cnum i;

        for (i = 0; i < len; i++) {
          val elem = ref(vec, num_fast(i));
          val key = funcall1(keyfun, elem);
          if (funcall2(testfun, item, key))
            return elem;
        }
      }
      break;
    }
    return nil;
  default:
    unsup_obj(self, seq);
  }
}

val rfind(val item, val seq, val testfun, val keyfun)
{
  val self = lit("rfind");
  testfun = default_arg(testfun, equal_f);
  keyfun = default_arg(keyfun, identity_f);
  seq_info_t si = seq_info(seq);

  switch (si.kind) {
  case SEQ_NIL:
    return nil;
  case SEQ_LISTLIKE:
    {
      val found = nil;
      gc_hint(seq);

      for (seq = z(si.obj); seq; seq = cdr(seq)) {
        val elem = car(seq);
        val key = funcall1(keyfun, elem);

        if (funcall2(testfun, item, key))
          found = elem;
      }
      return found;
    }
  case SEQ_VECLIKE:
    switch (si.type) {
    case STR:
    case LIT:
      if (keyfun == identity_f &&
          (testfun == equal_f || testfun == eql_f || testfun == eq_f))
      {
        const wchar_t ch = c_chr(item);
        const wchar_t *cstr = c_str(seq, self);
        if (wcschr(cstr, ch))
          return item;
        return nil;
      }
      /* fallthrough */
    default:
      {
        val vec = si.obj;
        cnum len = c_fixnum(length(vec), self);
        cnum i;

        for (i = len - 1; i >= 0; i--) {
          val elem = ref(vec, num_fast(i));
          val key = funcall1(keyfun, elem);
          if (funcall2(testfun, item, key))
            return elem;
        }
      }
      break;
    }
    return nil;
  default:
    unsup_obj(self, seq);
  }
}

val find_max(val seq, val testfun, val keyfun)
{
  val self = lit("find-max");
  seq_info_t si = seq_info(seq);
  testfun = default_arg(testfun, greater_f);
  keyfun = default_arg(keyfun, identity_f);

  switch (si.kind) {
  case SEQ_NIL:
    return nil;
  case SEQ_HASHLIKE:
    {
      struct hash_iter hi;
      val cell = (hash_iter_init(&hi, si.obj, self), hash_iter_next(&hi));
      val maxelt = cell;
      val maxkey = if2(cell, funcall1(keyfun, cell));

      while (cell && (cell = hash_iter_next(&hi))) {
        val key = funcall1(keyfun, cell);
        if (funcall2(testfun, key, maxkey)) {
          maxkey = key;
          maxelt = cell;
        }
      }

      return maxelt;
    }
  case SEQ_LISTLIKE:
    {
      val maxelt = car(z(si.obj));
      val maxkey = funcall1(keyfun, maxelt);

      gc_hint(seq);

      for (seq = cdr(seq); seq; seq = cdr(seq)) {
        val elt = car(seq);
        val key = funcall1(keyfun, elt);
        if (funcall2(testfun, key, maxkey)) {
          maxkey = key;
          maxelt = elt;
        }
      }

      return maxelt;
    }
  case SEQ_VECLIKE:
    {
      val vec = si.obj;
      cnum len = c_fixnum(length(vec), self);

      if (len > 0) {
        val maxelt = ref(vec, zero);
        val maxkey = funcall1(keyfun, maxelt);
        cnum i;

        for (i = 1; i < len; i++) {
          val elt = ref(vec, num_fast(i));
          val key = funcall1(keyfun, elt);
          if (funcall2(testfun, key, maxkey)) {
            maxkey = key;
            maxelt = elt;
          }
        }

        return maxelt;
      }

      return nil;
    }
  case SEQ_NOTSEQ:
  default:
    unsup_obj(self, seq);
  }
}

val find_min(val seq, val testfun, val keyfun)
{
  return find_max(seq, default_arg(testfun, less_f), keyfun);
}

val find_true(val pred, val seq, val key)
{
  val self = lit("find-true");
  val keyfun = default_arg(key, identity_f);
  seq_info_t si = seq_info(seq);

  switch (si.kind) {
  case SEQ_NIL:
    break;
  case SEQ_HASHLIKE:
    {
      struct hash_iter hi;
      val cell;

      hash_iter_init(&hi, si.obj, self);

      while ((cell = hash_iter_next(&hi))) {
        val key = funcall1(keyfun, cell);
        val res = funcall1(pred, key);
        if (res)
          return res;
      }

      break;
    }
  case SEQ_LISTLIKE:
    {
      gc_hint(seq);

      for (seq = z(si.obj); seq; seq = cdr(seq)) {
        val elt = car(seq);
        val key = funcall1(keyfun, elt);
        val res = funcall1(pred, key);
        if (res)
          return res;
      }

      break;
    }
  case SEQ_VECLIKE:
    {
      val vec = si.obj;
      cnum len = c_fixnum(length(vec), self);
      cnum i;

      for (i = 0; i < len; i++) {
        val elt = ref(vec, num_fast(i));
        val key = funcall1(keyfun, elt);
        val res = funcall1(pred, key);
        if (res)
          return res;
      }

      break;
    }
  case SEQ_NOTSEQ:
  default:
    unsup_obj(self, seq);
  }

  return nil;
}

val find_if(val pred, val seq, val key)
{
  val self = lit("find-if");
  val keyfun = default_arg(key, identity_f);
  seq_info_t si = seq_info(seq);

  switch (si.kind) {
  case SEQ_NIL:
    break;
  case SEQ_HASHLIKE:
    {
      struct hash_iter hi;
      val cell;

      hash_iter_init(&hi, si.obj, self);

      while ((cell = hash_iter_next(&hi))) {
        val key = funcall1(keyfun, cell);
        if (funcall1(pred, key))
          return cell;
      }

      break;
    }
  case SEQ_LISTLIKE:
    {
      gc_hint(seq);

      for (seq = z(si.obj); seq; seq = cdr(seq)) {
        val elt = car(seq);
        val key = funcall1(keyfun, elt);
        if (funcall1(pred, key))
          return elt;
      }

      break;
    }
  case SEQ_VECLIKE:
    {
      val vec = si.obj;
      cnum len = c_fixnum(length(vec), self);
      cnum i;

      for (i = 0; i < len; i++) {
        val elt = ref(vec, num_fast(i));
        val key = funcall1(keyfun, elt);
        if (funcall1(pred, key))
          return elt;
      }

      break;
    }
  case SEQ_NOTSEQ:
  default:
    unsup_obj(self, seq);
  }

  return nil;
}

val rfind_if(val predi, val seq, val key)
{
  val self = lit("rfind-if");
  val keyfun = default_arg(key, identity_f);
  seq_info_t si = seq_info(seq);
  val found = nil;

  switch (si.kind) {
  case SEQ_NIL:
    break;
  case SEQ_HASHLIKE:
    {
      struct hash_iter hi;
      val cell;

      hash_iter_init(&hi, si.obj, self);

      while ((cell = hash_iter_next(&hi))) {
        val key = funcall1(keyfun, cell);
        if (funcall1(predi, key))
          found = cell;
      }

      break;
    }
  case SEQ_LISTLIKE:
    {
      gc_hint(seq);

      for (seq = z(si.obj); seq; seq = cdr(seq)) {
        val elt = car(seq);
        val key = funcall1(keyfun, elt);
        if (funcall1(predi, key))
          found = elt;
      }

      break;
    }
  case SEQ_VECLIKE:
    {
      val vec = si.obj;
      cnum i = c_fixnum(length(vec), self) - 1;

      for (; i >= 0; i--) {
        val elt = ref(vec, num_fast(i));
        val key = funcall1(keyfun, elt);
        if (funcall1(predi, key))
          return elt;
      }

      break;
    }
  case SEQ_NOTSEQ:
  default:
    unsup_obj(self, seq);
  }

  return found;
}

val pos(val item, val seq, val testfun, val keyfun)
{
  val self = lit("pos");
  testfun = default_arg(testfun, equal_f);
  keyfun = default_arg(keyfun, identity_f);
  seq_info_t si = seq_info(seq);

  switch (si.kind) {
  case SEQ_NIL:
    return nil;
  case SEQ_LISTLIKE:
    {
      val pos = zero;
      gc_hint(seq);

      for (seq = z(si.obj); seq; seq = cdr(seq), pos = succ(pos)) {
        val elem = car(seq);
        val key = funcall1(keyfun, elem);

        if (funcall2(testfun, item, key))
          return pos;
      }
    }
    return nil;
  case SEQ_VECLIKE:
    switch (si.type) {
    case STR:
    case LIT:
      if (keyfun == identity_f &&
          (testfun == equal_f || testfun == eql_f || testfun == eq_f))
      {
        const wchar_t ch = c_chr(item);
        const wchar_t *cstr = c_str(seq, self);
        const wchar_t *cpos = wcschr(cstr, ch);
        if (cpos != 0)
          return num(cpos - cstr);
        return nil;
      }
      /* fallthrough */
    default:
      {
        val vec = si.obj;
        cnum len = c_fixnum(length(vec), self);
        cnum i;

        for (i = 0; i < len; i++) {
          val ni = num_fast(i);
          val elem = ref(vec, ni);
          val key = funcall1(keyfun, elem);
          if (funcall2(testfun, item, key))
            return ni;
        }
      }
      break;
    }
    return nil;
  default:
    unsup_obj(self, seq);
  }
}

val rpos(val item, val seq, val testfun, val keyfun)
{
  val self = lit("rpos");
  testfun = default_arg(testfun, equal_f);
  keyfun = default_arg(keyfun, identity_f);
  seq_info_t si = seq_info(seq);

  switch (si.kind) {
  case SEQ_NIL:
    return nil;
  case SEQ_LISTLIKE:
    {
      val pos = zero, fpos = nil;
      gc_hint(seq);

      for (seq = z(si.obj); seq; seq = cdr(seq), pos = succ(pos)) {
        val elem = car(seq);
        val key = funcall1(keyfun, elem);

        if (funcall2(testfun, item, key))
          fpos = pos;
      }
      return fpos;
    }
  case SEQ_VECLIKE:
    switch (si.type) {
    case STR:
    case LIT:
      if (keyfun == identity_f &&
          (testfun == equal_f || testfun == eql_f || testfun == eq_f))
      {
        const wchar_t ch = c_chr(item);
        const wchar_t *cstr = c_str(seq, self);
        const wchar_t *cpos = wcsrchr(cstr, ch);
        if (cpos != 0)
          return num(cpos - cstr);
        return nil;
      }
      /* fallthrough */
    default:
      {
        val vec = si.obj;
        cnum len = c_fixnum(length(vec), self);
        cnum i;

        for (i = len - 1; i >= 0; i--) {
          val ni = num_fast(i);
          val elem = ref(vec, ni);
          val key = funcall1(keyfun, elem);
          if (funcall2(testfun, item, key))
            return ni;
        }
      }
      break;
    }
    return nil;
  default:
    unsup_obj(self, seq);
  }
}

val posqual(val obj, val list)
{
  return pos(obj, list, equal_f, identity_f);
}

val rposqual(val obj, val list)
{
  return rpos(obj, list, equal_f, identity_f);
}

val posql(val obj, val list)
{
  return pos(obj, list, eql_f, identity_f);
}

val rposql(val obj, val list)
{
  return rpos(obj, list, eql_f, identity_f);
}

val posq(val obj, val list)
{
  return pos(obj, list, eq_f, identity_f);
}

val rposq(val obj, val list)
{
  return rpos(obj, list, eq_f, identity_f);
}

val pos_if(val pred, val seq, val key)
{
  val self = lit("pos-if");
  val keyfun = default_arg(key, identity_f);
  seq_info_t si = seq_info(seq);

  switch (si.kind) {
  case SEQ_NIL:
    return nil;
  case SEQ_LISTLIKE:
    {
      val pos = zero;
      gc_hint(seq);

      for (seq = z(si.obj); seq; seq = cdr(seq), pos = succ(pos)) {
        val elem = car(seq);
        val key = funcall1(keyfun, elem);

        if (funcall1(pred, key))
          return pos;
      }
    }
    return nil;
  case SEQ_VECLIKE:
    {
      val vec = si.obj;
      cnum len = c_fixnum(length(vec), self);
      cnum i;

      for (i = 0; i < len; i++) {
        val ni = num_fast(i);
        val elem = ref(vec, ni);
        val key = funcall1(keyfun, elem);
        if (funcall1(pred, key))
          return ni;
      }
    }
    return nil;
  default:
    unsup_obj(self, seq);
  }
}

val rpos_if(val pred, val seq, val key)
{
  val self = lit("rpos-if");
  val keyfun = default_arg(key, identity_f);
  seq_info_t si = seq_info(seq);

  switch (si.kind) {
  case SEQ_NIL:
    return nil;
  case SEQ_LISTLIKE:
    {
      val pos = zero;
      val fpos = nil;
      gc_hint(seq);

      for (seq = z(si.obj); seq; seq = cdr(seq), pos = succ(pos)) {
        val elem = car(seq);
        val key = funcall1(keyfun, elem);

        if (funcall1(pred, key))
          fpos = pos;
      }

      return fpos;
    }
  case SEQ_VECLIKE:
    {
      val vec = si.obj;
      cnum len = c_fixnum(length(vec), self);
      cnum i;

      for (i = len - 1; i >= 0; i--) {
        val ni = num_fast(i);
        val elem = ref(vec, ni);
        val key = funcall1(keyfun, elem);
        if (funcall1(pred, key))
          return ni;
      }
      return nil;
    }
  default:
    unsup_obj(self, seq);
  }
}

val pos_max(val seq, val testfun, val keyfun)
{
  val self = lit("pos-max");
  seq_info_t si = seq_info(seq);
  testfun = default_arg(testfun, greater_f);
  keyfun = default_arg(keyfun, identity_f);

  switch (si.kind) {
  case SEQ_NIL:
    return nil;
  case SEQ_LISTLIKE:
    {
      val maxkey = funcall1(keyfun, car(si.obj));
      val maxpos = zero;
      val pos;

      gc_hint(seq);

      for (pos = one, seq = cdr(z(si.obj));
           seq;
           seq = cdr(seq), pos = succ(pos))
      {
        val elt = car(seq);
        val key = funcall1(keyfun, elt);
        if (funcall2(testfun, key, maxkey)) {
          maxkey = key;
          maxpos = pos;
        }
      }

      return maxpos;
    }
  case SEQ_VECLIKE:
    {
      val vec = si.obj;
      cnum len = c_fixnum(length(vec), self);

      if (len > 0) {
        val maxpos = zero;
        val maxkey = funcall1(keyfun, ref(vec, zero));
        cnum i;

        for (i = 1; i < len; i++) {
          val ni = num_fast(i);
          val elt = ref(vec, ni);
          val key = funcall1(keyfun, elt);
          if (funcall2(testfun, key, maxkey)) {
            maxkey = key;
            maxpos = ni;
          }
        }
        return maxpos;
      }

      return nil;
    }
  case SEQ_NOTSEQ:
  default:
    unsup_obj(self, seq);
  }
}

val pos_min(val seq, val testfun, val keyfun)
{
  return pos_max(seq, default_arg(testfun, less_f), keyfun);
}

val mismatch(val left, val right, val testfun_in, val keyfun_in)
{
  val testfun = default_arg(testfun_in, equal_f);
  val keyfun = default_arg(keyfun_in, identity_f);

  switch (type(left)) {
  case NIL:
    switch (type(right)) {
    case NIL:
      return nil;
    case CONS:
    case LCONS:
      return zero;
    case VEC:
    case LIT:
    case STR:
      return if3(length(right) == zero, nil, zero);
    case LSTR:
      return if3(length_str_lt(right, one), nil, zero);
    default:
      break;
    }
    break;
  case CONS:
  case LCONS:
  default:
    switch (type(right)) {
    case NIL:
      return zero;
    case CONS:
    case LCONS:
      {
        val pos = zero;

        gc_hint(left);
        gc_hint(right);

        for (; !endp(left) && !endp(right);
             left = cdr(left), right = cdr(right), pos = succ(pos))
        {
          val lelt = funcall1(keyfun, car(left));
          val relt = funcall1(keyfun, car(right));
          if (!funcall2(testfun, lelt, relt))
            break;
        }

        return if3(left || right, pos, nil);
      }
    case VEC:
    case LIT:
    case STR:
    case LSTR:
      {
        val pos = zero;
        val rlen = length(right);

        gc_hint(left);

        for (; !endp(left) && lt(pos, rlen);
             left = cdr(left), pos = succ(pos))
        {
          val lelt = funcall1(keyfun, car(left));
          val relt = funcall1(keyfun, ref(right, pos));
          if (!funcall2(testfun, lelt, relt))
            break;
        }

        return if3(left || lt(pos, rlen), pos, nil);
      }
    default:
      break;
    }
    break;
  case STR:
  case LSTR:
  case LIT:
  case VEC:
    switch (type(right)) {
    case NIL:
      return if3(length(left) == zero, nil, zero);
    case CONS:
    case LCONS:
      return mismatch(right, left, testfun, keyfun);
    case STR:
    case LIT:
      switch (type(left)) {
      case STR:
      case LIT:
        if (keyfun == identity_f && (testfun == equal_f ||
                                     testfun == eql_f ||
                                     testfun == eq_f))
        {
          const wchar_t *lft = c_str(left, nil), *le = lft;
          const wchar_t *rgt = c_str(right, nil), *ri = rgt;

          while (*le && *ri && *le == *ri)
            le++, ri++;

          return if3(*le || *ri, num(le - lft), nil);
        }
      default:
          break;
      }
      /* fallthrough */
    case LSTR:
    case VEC:
      {
        val llen = length(left);
        val rlen = length(right);
        val pos = zero;

        for (; lt(pos, llen) && lt(pos, rlen); pos = succ(pos))
        {
          val lelt = funcall1(keyfun, ref(left, pos));
          val relt = funcall1(keyfun, ref(right, pos));
          if (!funcall2(testfun, lelt, relt))
            break;
        }

        return if3(lt(pos, llen) || lt(pos, rlen), pos, nil);
      }
    default:
      break;
    }
    break;
  }

  uw_throwf(error_s, lit("mismatch: invalid arguments ~!~s and ~s"),
            left, right, nao);
}

val rmismatch(val left, val right, val testfun_in, val keyfun_in)
{
  val self = lit("rmismatch");
  val testfun = default_arg(testfun_in, equal_f);
  val keyfun = default_arg(keyfun_in, identity_f);

  switch (type(left)) {
  case NIL:
    switch (type(right)) {
    case NIL:
      return nil;
    case CONS:
    case LCONS:
      return negone;
    case VEC:
    case LIT:
    case STR:
      return if3(length(right) == zero, nil, negone);
    case LSTR:
      return if3(length_str_lt(right, one), nil, negone);
    default:
      break;
    }
    break;
  case CONS:
  case LCONS:
  default:
    switch (type(right)) {
    case NIL:
      return negone;
    case CONS:
    case LCONS:
      {
        val mm = mismatch(reverse(left), reverse(right), testfun, keyfun);
        return if2(mm, minus(negone, mm));
      }
    case VEC:
    case LIT:
    case STR:
    case LSTR:
      {
        val rleft = reverse(left);
        val rlen = length(right);
        val rpos = pred(rlen);

        for (; !endp(rleft) && !minusp(rpos);
             rleft = cdr(rleft), rpos = pred(rpos))
        {
          val lelt = funcall1(keyfun, car(rleft));
          val relt = funcall1(keyfun, ref(right, rpos));
          if (!funcall2(testfun, lelt, relt))
            break;
        }

        return if2(rleft || !minusp(rpos), minus(rpos, rlen));
      }
    default:
      break;
    }
    break;
  case STR:
  case LSTR:
  case LIT:
  case VEC:
    switch (type(right)) {
    case NIL:
      return if3(length(left) == zero, nil, negone);
    case CONS:
    case LCONS:
      return rmismatch(right, left, testfun, keyfun);
    case LIT:
    case STR:
      switch (type(left)) {
      case STR:
      case LIT:
        if (keyfun == identity_f && (testfun == equal_f ||
                                     testfun == eql_f ||
                                     testfun == eq_f))
        {
          cnum ll = c_num(length(left), self), li = ll - 1;
          cnum rl = c_num(length(right), self), ri = rl - 1;
          const wchar_t *lft = c_str(left, self);
          const wchar_t *rgt = c_str(right, self);

          for (; li >= 0 && ri >= 0; li--, ri--) {
            if (lft[li] != rgt[ri])
              break;
          }

          return if2(li >= 0 || ri >= 0, num(li - ll));
        }
      default:
        break;
      }
      /* fallthrough */
    case VEC:
    case LSTR:
      {
        val llen = length(left);
        val rlen = length(right);
        val lpos = pred(llen);
        val rpos = pred(rlen);

        for (; !minusp(lpos) && !minusp(rpos);
             lpos = pred(lpos), rpos = pred(rpos))
        {
          val lelt = funcall1(keyfun, ref(left, lpos));
          val relt = funcall1(keyfun, ref(right, rpos));
          if (!funcall2(testfun, lelt, relt))
            break;
        }

        return if2(!minusp(lpos) || !minusp(rpos), minus(lpos, llen));
      }
    default:
      break;
    }
    break;
  }

  uw_throwf(error_s, lit("rmismatch: invalid arguments ~!~s and ~s"),
            left, right, nao);
}

val starts_with(val little, val big, val testfun, val keyfun)
{
  val mm = mismatch(little, big, testfun, keyfun);
  return tnil(!mm || eql(mm, length(little)));
}

val ends_with(val little, val big, val testfun, val keyfun)
{
  val mm = rmismatch(little, big, testfun, keyfun);
  return tnil(!mm || eql(pred(neg(mm)), length(little)));
}

static val lazy_take_list_fun(val list, val lcons)
{
  val count = us_car(lcons);

  us_rplaca(lcons, pop(&list));

  if (list == nil || le((count = pred(count)), zero)) {
    us_rplacd(lcons, nil);
  } else {
    val fun = us_lcons_fun(lcons);
    us_rplacd(lcons, make_lazy_cons_car(fun, count));
    us_func_set_env(fun, list);
  }

  return nil;
}

val take(val count, val seq)
{
  seq_info_t si = seq_info(seq);
  val self = lit("take");

  switch (si.kind) {
  case SEQ_NIL:
    return nil;
  case SEQ_LISTLIKE:
    if (le(count, zero))
      return nil;
    return make_lazy_cons_car(func_f1(si.obj, lazy_take_list_fun), count);
  case SEQ_VECLIKE:
    return sub(seq, zero, count);
  case SEQ_HASHLIKE:
    type_mismatch(lit("~a: hashes not supported"), self, nao);
  case SEQ_TREELIKE:
    type_mismatch(lit("~a: trees not supported"), self, nao);
  default:
    type_mismatch(lit("~a: ~s is not a sequence"), self, seq, nao);
  }
}

static val lazy_take_while_list_fun(val pred, val lcons)
{
  us_cons_bind (keyfun, list, lcons);

  us_rplaca(lcons, pop(&list));

  if (!list || !funcall1(pred, funcall1(keyfun, car(list))))
    us_rplacd(lcons, nil);
  else
    us_rplacd(lcons, make_lazy_cons_car_cdr(us_lcons_fun(lcons), keyfun, list));

  return nil;
}

val take_while(val pred, val seq, val keyfun)
{
  seq_info_t si = seq_info(seq);
  val self = lit("take-while");

  switch (si.kind) {
  case SEQ_NIL:
    return nil;
  case SEQ_LISTLIKE:
    keyfun = default_arg(keyfun, identity_f);
    if (!funcall1(pred, funcall1(keyfun, (car(seq)))))
      return nil;
    return make_lazy_cons_car_cdr(func_f1(pred, lazy_take_while_list_fun),
                                  keyfun, si.obj);
  case SEQ_VECLIKE:
    {
      val pos = pos_if(notf(pred), seq, keyfun);
      if (!pos)
        return seq;
      return sub(seq, zero, pos);
    }
  case SEQ_HASHLIKE:
    type_mismatch(lit("~a: hashes not supported"), self, nao);
  case SEQ_TREELIKE:
    type_mismatch(lit("~a: trees not supported"), self, nao);
  default:
    type_mismatch(lit("~a: ~s is not a sequence"), self, seq, nao);
  }
}

static val lazy_take_until_list_fun(val pred, val lcons)
{
  us_cons_bind (keyfun, list, lcons);
  val item = pop(&list);
  us_rplaca(lcons, item);

  if (!list || funcall1(pred, funcall1(keyfun, item)))
    us_rplacd(lcons, nil);
  else
    us_rplacd(lcons, make_lazy_cons_car_cdr(us_lcons_fun(lcons), keyfun, list));

  return nil;
}

val take_until(val pred, val seq, val keyfun)
{
  seq_info_t si = seq_info(seq);
  val self = lit("take-until");

  switch (si.kind) {
  case SEQ_NIL:
    return nil;
  case SEQ_LISTLIKE:
    keyfun = default_arg(keyfun, identity_f);
    return make_lazy_cons_car_cdr(func_f1(pred, lazy_take_until_list_fun),
                                  keyfun, si.obj);
  case SEQ_VECLIKE:
    {
      val pos = pos_if(pred, seq, keyfun);
      if (!pos)
        return seq;
      return sub(seq, zero, succ(pos));
    }
  case SEQ_HASHLIKE:
    type_mismatch(lit("~a: hashes not supported"), self, nao);
  case SEQ_TREELIKE:
    type_mismatch(lit("~a: trees not supported"), self, nao);
  default:
    type_mismatch(lit("~a: ~s is not a sequence"), self, seq, nao);
  }
}

val drop(val count, val seq)
{
  if (le(count, zero))
    return seq;
  return sub(seq, count, t);
}

val drop_while(val pred, val seq, val keyfun)
{
  seq_info_t si = seq_info(seq);
  val self = lit("drop-while");

  switch (si.kind) {
  case SEQ_NIL:
    return nil;
  case SEQ_LISTLIKE:
    keyfun = default_arg(keyfun, identity_f);
    while (seq && funcall1(pred, funcall1(keyfun, car(seq))))
      pop(&seq);
    return seq;
  case SEQ_VECLIKE:
    {
      val pos = pos_if(notf(pred), seq, keyfun);
      if (!pos)
        return make_like(nil, seq);
      return sub(seq, pos, t);
    }
  case SEQ_HASHLIKE:
    type_mismatch(lit("~a: hashes not supported"), self, nao);
  case SEQ_TREELIKE:
    type_mismatch(lit("~a: trees not supported"), self, nao);
  default:
    type_mismatch(lit("~a: ~s is not a sequence"), self, seq, nao);
  }
}

val drop_until(val pred, val seq, val keyfun)
{
  seq_info_t si = seq_info(seq);
  val self = lit("drop-until");

  switch (si.kind) {
  case SEQ_NIL:
    return nil;
  case SEQ_LISTLIKE:
    {
      val key = default_arg(keyfun, identity_f);
      val item;

      do {
        item = pop(&seq);
      } while (!funcall1(pred, funcall1(key, item)));

      return seq;
    }
  case SEQ_VECLIKE:
    {
      val pos = pos_if(pred, seq, keyfun);
      if (!pos)
        return seq;
      return sub(seq, succ(pos), t);
    }
  case SEQ_HASHLIKE:
    type_mismatch(lit("~a: hashes not supported"), self, nao);
  case SEQ_TREELIKE:
    type_mismatch(lit("~a: trees not supported"), self, nao);
  default:
    type_mismatch(lit("~a: ~s is not a sequence"), self, seq, nao);
  }
}

val in(val seq, val item, val testfun, val keyfun)
{
  val self = lit("in");
  seq_info_t si = seq_info(seq);

  switch (si.type) {
  case NIL:
    return nil;
  case LIT:
  case STR:
  case LSTR:
    {
      testfun = default_arg(testfun, equal_f);
      keyfun = default_arg(keyfun, identity_f);
      val len = length_str(seq);
      val ind;

      for (ind = zero; lt(ind, len); ind = plus(ind, one)) {
        val elem = chr_str(seq, ind);
        val key = funcall1(keyfun, elem);

        if (funcall2(testfun, item, key))
          return t;
      }

      return nil;
    }
  case VEC:
    {
      testfun = default_arg(testfun, equal_f);
      keyfun = default_arg(keyfun, identity_f);
      val len = length_vec(seq);
      val ind;

      for (ind = zero; lt(ind, len); ind = plus(ind, one)) {
        val elem = vecref(seq, ind);
        val key = funcall1(keyfun, elem);

        if (funcall2(testfun, item, key))
          return t;
      }

      return nil;
    }
  default:
    switch (si.kind) {
    case SEQ_HASHLIKE:
      if (null_or_missing_p(testfun) && null_or_missing_p(keyfun))
        return tnil(gethash_e(self, si.obj, item));
      /* fallthrough */
    case SEQ_LISTLIKE:
    case SEQ_VECLIKE:
      {
        seq_iter_t iter;
        val elem;

        seq_iter_init(self, &iter, seq);

        testfun = default_arg(testfun, equal_f);
        keyfun = default_arg(keyfun, identity_f);

        while (seq_get(&iter, &elem)) {
          val key = funcall1(keyfun, elem);
          if (funcall2(testfun, item, key))
            return t;
        }

        return nil;
      }
    default:
      type_mismatch(lit("~a: ~s is not a sequence"), self, seq, nao);
    }
  }
}

val diff(val seq1, val seq2, val testfun, val keyfun)
{
  val self = lit("diff");
  list_collect_decl (out, ptail);
  seq_iter_t si1, si2;
  val el1;

  testfun = default_arg(testfun, equal_f);
  keyfun = default_arg(keyfun, identity_f);

  seq_iter_init(self, &si1, seq1);
  seq_iter_init_with_rewind(self, &si2, seq2);

  while (seq_get(&si1, &el1)) {
    val el1_key = funcall1(keyfun, el1);
    val el2;
    int found = 0;

    seq_iter_rewind(&si2, self);

    while (seq_get(&si2, &el2)) {
      val el2_key = funcall1(keyfun, el2);

      if (funcall2(testfun, el1_key, el2_key)) {
        found = 1;
        break;
      }
    }

    if (!found)
      ptail = list_collect(ptail, el1);
  }

  return make_like(out, seq1);
}

val set_diff(val list1, val list2, val testfun, val keyfun)
{
  list_collect_decl (out, ptail);
  val list_orig = list1;

  list1 = nullify(list1);
  list2 = nullify(list2);

  testfun = default_arg(testfun, equal_f);
  keyfun = default_arg(keyfun, identity_f);

  for (; list1; list1 = cdr(list1)) {
    /* optimization: list2 is a tail of list1, and so we
       are done, unless the application has a weird test function. */
    if (list1 == list2) {
      break;
    } else {
      val item = car(list1);
      val list1_key = funcall1(keyfun, item);

      if (!member(list1_key, list2, testfun, keyfun))
        ptail = list_collect(ptail, item);
    }
  }

  return make_like(out, list_orig);
}

val symdiff(val seq1, val seq2, val testfun, val keyfun)
{
  val self = lit("symdiff");
  list_collect_decl (out, ptail);
  seq_iter_t si1, si2;
  val el1, el2;

  testfun = default_arg(testfun, equal_f);
  keyfun = default_arg(keyfun, identity_f);

  seq_iter_init(self, &si1, seq1);
  seq_iter_init(self, &si2, seq2);

  while (seq_get(&si1, &el1))
    ptail = list_collect(ptail, el1);

  while (seq_get(&si2, &el2)) {
    val el2_key = funcall1(keyfun, el2);
    val *iter;
    int found = 0;

    for (iter = &out; *iter; ) {
      val elo = us_car(*iter);
      val elo_key = funcall1(keyfun, elo);
      if (funcall2(testfun, elo_key, el2_key)) {
        val del = us_cdr(*iter);
        if (deref(ptail) == del)
          ptail = mkcloc(*iter);
        *iter = del;
        found = 1;
        break;
      } else {
        iter = us_cdr_p(*iter);
      }
    }

    if (!found)
      ptail = list_collect(ptail, el2);
  }

  return make_like(out, seq1);
}

val isec(val seq1, val seq2, val testfun, val keyfun)
{
  val self = lit("isec");
  list_collect_decl (out, ptail);
  seq_iter_t si1, si2;
  val el1;

  testfun = default_arg(testfun, equal_f);
  keyfun = default_arg(keyfun, identity_f);

  seq_iter_init(self, &si1, seq1);
  seq_iter_init_with_rewind(self, &si2, seq2);

  while (seq_get(&si1, &el1)) {
    val el1_key = funcall1(keyfun, el1);
    val el2;

    seq_iter_rewind(&si2, self);

    while (seq_get(&si2, &el2)) {
      val el2_key = funcall1(keyfun, el2);

      if (funcall2(testfun, el1_key, el2_key)) {
        ptail = list_collect(ptail, el1);
        break;
      }
    }
  }

  return make_like(out, seq1);
}

val uni(val seq1, val seq2, val testfun, val keyfun)
{
  val self = lit("uni");
  list_collect_decl (out, ptail);
  seq_iter_t si1, si2;
  val el1, el2;

  testfun = default_arg(testfun, equal_f);
  keyfun = default_arg(keyfun, identity_f);

  seq_iter_init(self, &si1, seq1);
  seq_iter_init(self, &si2, seq2);

  while (seq_get(&si1, &el1))
    ptail = list_collect(ptail, el1);

  while (seq_get(&si2, &el2)) {
    val el2_key = funcall1(keyfun, el2);

    if (!member(el2_key, out, testfun, keyfun))
      ptail = list_collect(ptail, el2);
  }

  return make_like(out, seq1);
}

val copy(val seq)
{
  switch (type(seq)) {
  case NIL:
    return nil;
  case CONS:
  case LCONS:
    return copy_list(seq);
  case LIT:
  case STR:
  case LSTR:
    return copy_str(seq);
  case VEC:
    return copy_vec(seq);
  case BUF:
    return copy_buf(seq);
  case FUN:
    return copy_fun(seq);
  case TNOD:
    return copy_tnode(seq);
  case COBJ:
    if (seq->co.cls == hash_cls)
      return copy_hash(seq);
    if (seq->co.cls == random_state_cls)
      return make_random_state(seq, nil);
    if (seq->co.cls == carray_cls)
      return copy_carray(seq);
    if (seq->co.cls == tree_cls)
      return copy_search_tree(seq);
    if (seq->co.cls == tree_iter_cls)
      return copy_tree_iter(seq);
    if (obj_struct_p(seq))
      return copy_struct(seq);
    /* fallthrough */
  default:
    type_mismatch(lit("copy: cannot copy object of type ~s"),
                  typeof(seq), nao);
  }
}

val length(val seq)
{
  switch (type(seq)) {
  case NIL:
    return num(0);
  case CONS:
  case LCONS:
    return length_list(seq);
  case LIT:
  case STR:
  case LSTR:
    return length_str(seq);
  case VEC:
    return length_vec(seq);
  case RNG:
    return minus(to(seq), from(seq));
  case BUF:
    return length_buf(seq);
  case COBJ:
    if (seq->co.cls == hash_cls)
      return hash_count(seq);
    if (seq->co.cls == carray_cls)
      return length_carray(seq);
    if (obj_struct_p(seq)) {
      val length_meth = get_special_slot(seq, length_m);

      if (length_meth)
        return funcall1(length_meth, seq);

      if (get_special_slot(seq, car_m))
        return length_proper_list(nullify(seq));

      type_mismatch(lit("length: ~s has no length or car method"), seq, nao);
    }
    /* fallthrough */
  default:
    type_mismatch(lit("length: ~s is not a sequence"), seq, nao);
  }
}

val sub(val seq, val from, val to)
{
  switch (type(seq)) {
  case NIL:
    return nil;
  case CONS:
  case LCONS:
    return sub_list(seq, from, to);
  case LIT:
  case STR:
  case LSTR:
    return sub_str(seq, from, to);
  case VEC:
    return sub_vec(seq, from, to);
  case BUF:
    return sub_buf(seq, from, to);
  case COBJ:
    if (seq->co.cls == carray_cls)
      return carray_sub(seq, from, to);
    if (seq->co.cls == tree_cls)
      return sub_tree(seq, from, to);
    if (obj_struct_p(seq)) {
      val lambda_meth = get_special_slot(seq, lambda_m);
      if (lambda_meth)
        return funcall2(lambda_meth, seq, rcons(from, to));
      seq = nullify(seq);
      return sub_list(seq, from, to);
    }
    /* fallthrough */
  default:
    type_mismatch(lit("sub: ~s is not a sequence"), seq, nao);
  }
}

val ref(val seq, val ind)
{
  switch (type(seq)) {
  case NIL:
    return nil;
  case COBJ:
    if (seq->co.cls == hash_cls)
      return gethash(seq, ind);
    if (seq->co.cls == carray_cls)
      return carray_ref(seq, ind);
    if (seq->co.cls == tree_cls)
      return tree_lookup(seq, ind);
    if (obj_struct_p(seq)) {
      val lambda_meth = get_special_slot(seq, lambda_m);
      if (lambda_meth)
        return funcall2(lambda_meth, seq, ind);
    }
    /* fallthrough */
  case CONS:
  case LCONS:
    return listref(seq, ind);
  case LIT:
  case STR:
  case LSTR:
    return chr_str(seq, ind);
  case VEC:
    return vecref(seq, ind);
  case BUF:
    return buf_get_uchar(seq, ind);
  default:
    type_mismatch(lit("ref: ~s is not a sequence"), seq, nao);
  }
}

val refset(val seq, val ind, val newval)
{
  switch (type(seq)) {
  case NIL:
  case CONS:
  case LCONS:
  list:
    {
      val nthcons = nthcdr(ind, seq);
      if (nilp(nthcons))
        uw_throwf(error_s, lit("refset: ~s has no assignable location at ~s"),
                  seq, ind, nao);
      (void) rplaca(nthcons, newval);
      return newval;
    }
  case LIT:
  case STR:
  case LSTR:
    return chr_str_set(seq, ind, newval);
  case VEC:
    return set(vecref_l(seq, ind), newval);
  case BUF:
    return buf_put_uchar(seq, ind, newval);
  case COBJ:
    if (seq->co.cls == hash_cls)
      return sethash(seq, ind, newval);
    if (seq->co.cls == carray_cls)
      return carray_refset(seq, ind, newval);
    if (obj_struct_p(seq)) {
      {
        val lambda_set_meth = get_special_slot(seq, lambda_set_m);
        if (lambda_set_meth) {
          (void) funcall3(lambda_set_meth, seq, ind, newval);
          return newval;
        }
      }
      {
        val car_meth = get_special_slot(seq, car_m);
        if (car_meth)
          goto list;
      }
      type_mismatch(lit("refset: object ~s lacks ~s or ~s method"), seq,
                    lambda_set_s, car_s, nao);
    }
    /* fallthrough */
  default:
    type_mismatch(lit("refset: ~s is not a sequence"), seq, nao);
  }
  return newval;
}

val replace(val seq, val items, val from, val to)
{
  val self = lit("replace");

  switch (type(seq)) {
  case NIL:
  case CONS:
  case LCONS:
    return replace_list(seq, items, from, to);
  case LIT:
  case STR:
  case LSTR:
    return replace_str(seq, items, from, to);
  case VEC:
    return replace_vec(seq, items, from, to);
  case BUF:
    return replace_buf(seq, items, from, to);
  case COBJ:
    if (seq->co.cls == carray_cls)
      return carray_replace(seq, items, from, to);
    if (obj_struct_p(seq))
      return replace_obj(seq, items, from, to);
    /* fallthrough */
  default:
    type_mismatch(lit("~a: ~s is not a sequence"), self, seq, nao);
  }
}

val dwim_set(val place_p, val seq, varg vargs)
{
  val self = lit("index/range assignment");

  switch (type(seq)) {
  case COBJ:
    if (type(seq) == COBJ) {
      if (seq->co.cls == hash_cls) {
        args_normalize_least(vargs, 3);

        switch (vargs->fill) {
        case 2:
          (void) sethash(seq, vargs->arg[0], vargs->arg[1]);
          break;
        case 3:
          if (vargs->list)
            goto excargs;
          (void) sethash(seq, vargs->arg[0], vargs->arg[2]);
          break;
        default:
          goto fewargs;
        }

        return seq;
      }
      if (obj_struct_p(seq)) {
        {
          val lambda_set_meth = get_special_slot(seq, lambda_set_m);
          if (lambda_set_meth) {
            (void) funcall(method_args(seq, lambda_set_s, vargs));
            return seq;
          }
        }
        if (get_special_slot(seq, car_m))
          goto list;
        type_mismatch(lit("~a: object ~s lacks "
                          "~s or ~s method"),
                      self, seq, lambda_set_s, car_s, nao);
      }
    }
    /* fallthrough */
  default:
  list:
    {
      cnum index = 0;
      val ind_range, newval;
      if (!args_two_more(vargs, 0))
        goto fewargs;
      ind_range = args_get(vargs, &index);
      newval = args_get(vargs, &index);

      switch (type(ind_range)) {
      case NIL:
      case CONS:
      case LCONS:
      case VEC:
        if (!place_p && listp(seq))
          goto notplace;
        return replace(seq, newval, ind_range, colon_k);
      case RNG:
        {
          range_bind (x, y, ind_range);
          if (!place_p && listp(seq))
            goto notplace;
          return replace(seq, newval, x, y);
        }
      default:
        (void) refset(seq, ind_range, newval);
        return seq;
      }
    }
  }
notplace:
  uw_throwf(error_s, lit("~a: list form must be place"), self, nao);
fewargs:
  uw_throwf(error_s, lit("~a: missing required arguments"), self, nao);
excargs:
  uw_throwf(error_s, lit("~a: too many arguments"), self, nao);
}

val dwim_del(val place_p, val seq, val ind_range)
{
  switch (type(seq)) {
  case NIL:
  case CONS:
  case LCONS:
    if (!place_p)
      uw_throwf(error_s, lit("index/range delete: list form must be place"),
                nao);
    break;
  case COBJ:
    if (seq->co.cls == hash_cls) {
      (void) remhash(seq, ind_range);
      return seq;
    }
    if (obj_struct_p(seq))
      uw_throwf(error_s, lit("index/range delete: not supported for structs"),
                nao);
  default:
    break;
  }

  if (rangep(ind_range)) {
    return replace(seq, nil, from(ind_range), to(ind_range));
  } else {
    return replace(seq, nil, ind_range, succ(ind_range));
  }
}

val butlast(val seq, val idx)
{
  if (listp(seq)) {
    return butlastn(default_arg(idx, one), seq);
  } else {
    val nidx = if3(null_or_missing_p(idx), negone, neg(idx));
    return sub(seq, zero, if3(plusp(nidx), zero, nidx));
  }
}

val update(val seq, val fun)
{
  val self = lit("update");
  seq_info_t si = seq_info(seq);

  switch (si.kind) {
  case SEQ_NIL:
    return nil;
  case SEQ_LISTLIKE:
    {
      val iter = seq;

      while (consp(iter)) {
        rplaca(iter, funcall1(fun, car(iter)));
        iter = cdr(iter);
      }
    }
    break;
  case SEQ_VECLIKE:
    {
      val len = length(seq);
      val i;
      for (i = zero; lt(i, len); i = plus(i, one))
        refset(seq, i, funcall1(fun, ref(seq, i)));
    }
    break;
  case SEQ_HASHLIKE:
    return hash_update(seq, fun);
  case SEQ_TREELIKE:
    type_mismatch(lit("~a: trees not supported"), self, nao);
  default:
    type_mismatch(lit("~a: ~s is not a sequence"), self, seq, nao);
  }

  return seq;
}

static val search_common(val self, int from_right,
                         val seq, val key, val testfun, val keyfun)
{
  testfun = default_arg(testfun, equal_f);
  keyfun = default_arg(keyfun, identity_f);
  seq_iter_t si, ki;

  seq_iter_init(self, &si, seq);
  seq_iter_init(self, &ki, key);

  if (si.inf.kind == SEQ_NIL) {
    return if3(length(key) == zero, zero, nil);
  } else if (ki.inf.kind == SEQ_HASHLIKE || si.inf.kind == SEQ_HASHLIKE) {
    type_mismatch(lit("~a: hashes not supported"), self, nao);
  } else if (ki.inf.kind == SEQ_NIL) {
    return if3(from_right, length(seq), zero);
  } else {
    val selem, kelem;
    val pos = zero, found = nil;

    if (!seq_peek(&ki, &kelem))
      return if3(from_right, length(seq), zero);

    for (;;) {
      val did_save = nil;
      val saved_seq_pos = nil;
      val saved_key_pos = nil;
      int more_seq, more_key;

      while ((more_seq = seq_peek(&si, &selem),
              more_key = seq_peek(&ki, &kelem),
              more_key && more_seq))
      {
        if (!did_save)
          saved_key_pos = seq_getpos(self, &ki);

        seq_geti(&si);
        seq_geti(&ki);

        if (!did_save) {
          saved_seq_pos = seq_getpos(self, &si);
          did_save = t;
        }

        if (!funcall2(testfun,
                      funcall1(keyfun, selem),
                      funcall1(keyfun, kelem)))
        {
          break;
        }
      }

      if (!more_key) {
        if (from_right)
          found = pos;
        else
          return pos;
      }

      if (!more_seq)
        return found;

      pos = plus(pos, one);
      seq_setpos(self, &si, saved_seq_pos);
      seq_setpos(self, &ki, saved_key_pos);
    }
  }
}

val search(val seq, val key, val testfun, val keyfun)
{
  return search_common(lit("search"), 0, seq, key, testfun, keyfun);
}

val rsearch(val seq, val key, val testfun, val keyfun)
{
  return search_common(lit("rsearch"), 1, seq, key, testfun, keyfun);
}

val contains(val key, val seq, val testfun, val keyfun)
{
  return search_common(lit("contains"), 0, seq, key, testfun, keyfun);
}

static val lazy_where_func(val iter, val lcons)
{
  val iter_orig = iter;
  us_cons_bind (index, func, lcons);

  for (;;) {
    if (!iter_more(iter)) {
      us_rplacd(lcons, nil);
      return nil;
    }
    index = succ(index);
    if (funcall1(func, iter_item(iter)))
      break;
    iter = iter_step(iter);
  }
  iter = iter_step(iter);

  {
    val fun = us_lcons_fun(lcons);
    if (iter != iter_orig)
      us_func_set_env(fun, iter);
    us_rplacd(lcons, make_lazy_cons_car_cdr(fun, index, func));
    return nil;
  }
}

static val lazy_where_hash_func(val hash_iter, val lcons)
{
  val func = us_cdr(lcons);
  val key;

  for (;;) {
    val hcell = hash_next(hash_iter);
    if (!hcell) {
      us_rplacd(lcons, nil);
      return nil;
    }
    if (funcall1(func, us_cdr(hcell))) {
      key = us_car(hcell);
      break;
    }
  }

  {
    us_rplacd(lcons, make_lazy_cons_car_cdr(us_lcons_fun(lcons), key, func));
    return nil;
  }
}

static val lazy_where_tree_func(val tree_iter, val lcons)
{
  val func = us_cdr(lcons);

  for (;;) {
    val node = tree_next(tree_iter);
    if (!node) {
      us_rplacd(lcons, nil);
      return nil;
    } else {
      val ky = key(node);
      if (funcall1(func, ky)) {
        val fun = us_lcons_fun(lcons);
        us_rplacd(lcons, make_lazy_cons_car_cdr(fun, ky, func));
        return nil;
      }
    }
  }
}

val where(val func, val seq)
{
  if (hashp(seq)) {
    val hash_iter = hash_begin(seq);
    val key;

    for (;;) {
      val hcell = hash_next(hash_iter);
      if (!hcell)
        return nil;
      if (funcall1(func, us_cdr(hcell))) {
        key = us_car(hcell);
        break;
      }
    }

    return make_lazy_cons_car_cdr(func_f1(hash_iter, lazy_where_hash_func),
                                  key, func);
  } else if (treep(seq)) {
    val tree_iter = tree_begin(seq, colon_k, colon_k);

    for (;;) {
      val node = tree_next(tree_iter);
      if (!node) {
        return nil;
      } else {
        val ky = key(node);
        if (funcall1(func, ky))
          return make_lazy_cons_car_cdr(func_f1(tree_iter, lazy_where_tree_func),
                                        ky, func);
      }
    }
  } else {
    val iter = iter_begin(seq);
    val index = zero;

    for (;;) {
      if (!iter_more(iter))
        return nil;
      if (funcall1(func, iter_item(iter)))
        break;
      iter = iter_step(iter);
      index = succ(index);
    }

    iter = iter_step(iter);
    return make_lazy_cons_car_cdr(func_f1(iter, lazy_where_func),
                                  index, func);
  }
}

val sel(val seq, val where_in)
{
  val self = lit("select");
  list_collect_decl (out, ptail);
  val where = if3(functionp(where_in),
                  funcall1(where_in, seq),
                  where_in);
  seq_info_t si = seq_info(seq);
  seq_iter_t wh_iter;
  val wh;

  seq_iter_init(self, &wh_iter, where);

  switch (si.kind) {
  case SEQ_NIL:
    return nil;
  case SEQ_HASHLIKE:
    {
      val newhash = make_similar_hash(si.obj);

      while (seq_get(&wh_iter, &wh)) {
        val found = gethash_e(self, seq, wh);
        if (found)
          sethash(newhash, wh, cdr(found));
      }

      return newhash;
    }
  case SEQ_TREELIKE:
    {
      val newtree = make_similar_tree(si.obj);

      while (seq_get(&wh_iter, &wh)) {
        val node = tree_lookup_node(seq, wh);
        if (node)
          tree_insert(newtree, key(node));
      }

      return newtree;
    }
  case SEQ_LISTLIKE:
    {
      val idx = zero;
      val iter = si.obj;
      val len = nil;

      while (iter && seq_peek(&wh_iter, &wh)) {
        if (minusp(wh))
          wh = plus(wh, len ? len : (len = length(seq)));
        if (lt(wh, idx)) {
          seq_geti(&wh_iter);
          continue;
        } else if (eql(wh, idx)) {
          ptail = list_collect(ptail, car(iter));
          seq_geti(&wh_iter);
        }

        iter = cdr(iter);
        idx = plus(idx, one);
      }
    }
    break;
  case SEQ_VECLIKE:
    {
      val len = length(seq);

      while (seq_get(&wh_iter, &wh)) {
        if (ge(wh, len))
          break;
        ptail = list_collect(ptail, ref(si.obj, wh));
      }
    }
    break;
  case SEQ_NOTSEQ:
    type_mismatch(lit("~a: ~s is not a sequence"), self, seq, nao);
  }

  return make_like(out, seq);
}

val reject(val seq, val where_in)
{
  val self = lit("reject");
  seq_info_t si = seq_info(seq);
  val (*appendfn)(val) = lazy_appendl;

  switch (si.kind) {
  case SEQ_NIL:
    return nil;
  case SEQ_HASHLIKE:
  case SEQ_TREELIKE:
    {
      seq_iter_t wh_iter;
      val wh;
      val newobj = copy(si.obj);
      val where = if3(functionp(where_in),
                      funcall1(where_in, seq),
                      where_in);
      seq_iter_init(self, &wh_iter, where);

      if (si.kind == SEQ_HASHLIKE)
        while (seq_get(&wh_iter, &wh))
          remhash(newobj, wh);
      else
        while (seq_get(&wh_iter, &wh))
          tree_delete(newobj, wh);

      return newobj;
    }
  case SEQ_VECLIKE:
    appendfn = appendl;
    /* fallthrough */
  case SEQ_LISTLIKE:
    {
      val list = appendfn(split_star(seq, where_in));
      return make_like(list, seq);
    }
    break;
  case SEQ_NOTSEQ:
    type_mismatch(lit("~a: ~s is not a sequence"), self, seq, nao);
  }

  abort();
}

static val do_relate(val env, val arg)
{
  cons_bind (dom, rng, env);
  val pos = posqual(arg, dom);
  return if3(pos, ref(rng, pos), arg);
}

static val do_relate_dfl(val env, val arg)
{
  val dom = env->v.vec[0];
  val rng = env->v.vec[1];
  val dfl = env->v.vec[2];
  val pos = posqual(arg, dom);
  return if3(pos, ref(rng, pos), dfl);
}

static val do_relate_hash(val hash, val arg)
{
  val cell = gethash_e(lit("relate"), hash, arg);
  return if3(cell, cdr(cell), arg);
}

static val do_relate_hash_dfl(val env, val arg)
{
  cons_bind (hash, dfl, env);
  val cell = gethash_e(lit("relate"), hash, arg);
  return if3(cell, cdr(cell), dfl);
}

val relate(val domain_seq, val range_seq, val dfl_val)
{
  val lds = length(domain_seq);
  val use_hash = and2(gt(lds, num_fast(10)),
                      le(lds, length(range_seq)));
  args_decl(args, ARGS_MIN);
  val hash = if2(use_hash, hash_zip(domain_seq, range_seq, args));

  return if3(missingp(dfl_val),
             if3(use_hash,
                 func_f1(hash, do_relate_hash),
                 func_f1(cons(domain_seq, range_seq), do_relate)),
             if3(use_hash,
                 if3(null(dfl_val),
                     hash,
                     func_f1(cons(hash, dfl_val), do_relate_hash_dfl)),
                 func_f1(vec(domain_seq, range_seq, dfl_val, nao),
                         do_relate_dfl)));
}

val rcons(val from, val to)
{
  val obj = make_obj();
  obj->rn.type = RNG;
  obj->rn.from = from;
  obj->rn.to = to;
  return obj;
}

val rangep(val obj)
{
  return type(obj) == RNG ? t : nil;
}

val from(val range)
{
  type_check(lit("from"), range, RNG);
  return range->rn.from;
}

val to(val range)
{
  type_check(lit("to"), range, RNG);
  return range->rn.to;
}

val set_from(val range, val from)
{
  type_check(lit("set-from"), range, RNG);
  set(mkloc(range->rn.from, range), from);
  return range;
}

val set_to(val range, val to)
{
  type_check(lit("set-to"), range, RNG);
  set(mkloc(range->rn.to, range), to);
  return range;
}

val in_range(val range, val num)
{
  type_check(lit("in-range"), range, RNG);
  {
    val from = range->rn.from;
    val to = range->rn.to;
    return and2(lequal(from, num), lequal(num, to));
  }
}

val in_range_star(val range, val num)
{
  type_check(lit("in-range*"), range, RNG);
  {
    val from = range->rn.from;
    val to = range->rn.to;
    return and2(lequal(from, num), less(num, to));
  }
}

#if CONFIG_LOCALE_TOLERANCE

static void locale_init(void)
{
  struct lconv *lc = localeconv();
  dec_point = *lc->decimal_point;
}

#endif

static void obj_init(void)
{
  /*
   * No need to GC-protect the convenience variables which hold the interned
   * symbols, because the interned_syms list holds a reference to all the
   * symbols.
   */

  protect(&packages, &system_package, &keyword_package,
          &user_package, &public_package,
          &null_list, &equal_f, &eq_f, &eql_f,
          &car_f, &cdr_f, &null_f, &list_f,
          &identity_f, &identity_star_f, &less_f, &greater_f,
          &prog_string, &cobj_hash,
          convert(val *, 0));

  nil_string = lit("nil");
  null_string = lit("");
  null_list = cons(nil, nil);

  hash_s = make_sym(lit("hash"));
  system_package = make_package(lit("sys"), nil);
  keyword_package = make_package(lit("keyword"), nil);
  user_package = make_package(lit("usr"), nil);
  public_package = make_package(lit("pub"), nil);

  rehome_sym(hash_s, user_package);

  /* nil can't be interned because it's not a SYM object;
     it works as a symbol because the nil case is handled by
     symbol-manipulating function. */
  sethash(user_package->pk.symhash, nil_string, nil);

  /* Replace fake t (value 1 set in init) with real symbol. */
  t = intern(lit("t"), user_package);

  set_package_fallback_list(system_package, cons(user_package, nil));
  set_package_fallback_list(public_package, cons(user_package, nil));

  null_s = intern(lit("null"), user_package);
  cons_s = intern(lit("cons"), user_package);
  str_s = intern(lit("str"), user_package);
  lit_s = intern(lit("lit"), user_package);
  chr_s = intern(lit("chr"), user_package);
  fixnum_s = intern(lit("fixnum"), user_package);
  sym_s = intern(lit("sym"), user_package);
  pkg_s = intern(lit("pkg"), user_package);
  fun_s = intern(lit("fun"), user_package);
  vec_s = intern(lit("vec"), user_package);
  stream_s = intern(lit("stream"), user_package);
  hash_s = intern(lit("hash"), user_package);
  hash_iter_s = intern(lit("hash-iter"), user_package);
  lcons_s = intern(lit("lcons"), user_package);
  lstr_s = intern(lit("lstr"), user_package);
  cobj_s = intern(lit("cobj"), user_package);
  cptr_s = intern(lit("cptr"), user_package);
  atom_s = intern(lit("atom"), user_package);
  integer_s = intern(lit("integer"), user_package);
  number_s = intern(lit("number"), user_package);
  sequence_s = intern(lit("sequence"), user_package);
  string_s = intern(lit("string"), user_package);
  env_s = intern(lit("env"), user_package);
  bignum_s = intern(lit("bignum"), user_package);
  float_s = intern(lit("float"), user_package);
  range_s = intern(lit("range"), user_package);
  rcons_s = intern(lit("rcons"), user_package);
  buf_s = intern(lit("buf"), user_package);
  tnode_s = intern(lit("tnode"), user_package);
  args_s = intern(lit("args"), user_package);
  var_s = intern(lit("var"), system_package);
  expr_s = intern(lit("expr"), system_package);
  regex_s = intern(lit("regex"), user_package);
  nongreedy_s = intern(lit("ng0+"), user_package);
  quote_s = intern(lit("quote"), user_package);
  qquote_s = intern(lit("qquote"), user_package);
  unquote_s = intern(lit("unquote"), user_package);
  splice_s = intern(lit("splice"), user_package);
  sys_qquote_s = intern(lit("qquote"), system_package);
  sys_unquote_s = intern(lit("unquote"), system_package);
  sys_splice_s = intern(lit("splice"), system_package);
  chset_s = intern(lit("chset"), system_package);
  set_s = intern(lit("set"), user_package);
  cset_s = intern(lit("cset"), user_package);
  wild_s = intern(lit("wild"), user_package);
  oneplus_s = intern(lit("1+"), user_package);
  zeroplus_s = intern(lit("0+"), user_package);
  optional_s = intern(lit("?"), user_package);
  compl_s = intern(lit("~"), user_package);
  compound_s = intern(lit("compound"), user_package);
  or_s = intern(lit("or"), user_package);
  and_s = intern(lit("and"), user_package);
  quasi_s = intern(lit("quasi"), system_package);
  quasilist_s = intern(lit("quasilist"), system_package);
  skip_s = intern(lit("skip"), user_package);
  trailer_s = intern(lit("trailer"), user_package);
  block_s = intern(lit("block"), user_package);
  next_s = intern(lit("next"), user_package);
  freeform_s = intern(lit("freeform"), user_package);
  fail_s = intern(lit("fail"), user_package);
  accept_s = intern(lit("accept"), user_package);
  all_s = intern(lit("all"), user_package);
  some_s = intern(lit("some"), user_package);
  none_s = intern(lit("none"), user_package);
  maybe_s = intern(lit("maybe"), user_package);
  cases_s = intern(lit("cases"), user_package);
  collect_s = intern(lit("collect"), user_package);
  until_s = intern(lit("until"), user_package);
  coll_s = intern(lit("coll"), user_package);
  define_s = intern(lit("define"), user_package);
  output_s = intern(lit("output"), user_package);
  single_s = intern(lit("single"), user_package);
  first_s = intern(lit("first"), user_package);
  last_s = intern(lit("last"), user_package);
  empty_s = intern(lit("empty"), user_package);
  repeat_s = intern(lit("repeat"), user_package);
  rep_s = intern(lit("rep"), user_package);
  flatten_s = intern(lit("flatten"), user_package);
  forget_s = intern(lit("forget"), user_package);
  local_s = intern(lit("local"), user_package);
  merge_s = intern(lit("merge"), user_package);
  bind_s = intern(lit("bind"), user_package);
  rebind_s = intern(lit("rebind"), user_package);
  cat_s = intern(lit("cat"), user_package);
  try_s = intern(lit("try"), user_package);
  catch_s = intern(lit("catch"), user_package);
  finally_s = intern(lit("finally"), user_package);
  throw_s = intern(lit("throw"), user_package);
  defex_s = intern(lit("defex"), user_package);
  deffilter_s = intern(lit("deffilter"), user_package);
  eof_s = intern(lit("eof"), user_package);
  eol_s = intern(lit("eol"), user_package);
  error_s = intern(lit("error"), user_package);
  type_error_s = intern(lit("type-error"), user_package);
  internal_error_s = intern(lit("internal-error"), user_package);
  panic_s = intern(lit("panic"), user_package);
  numeric_error_s = intern(lit("numeric-error"), user_package);
  range_error_s = intern(lit("range-error"), user_package);
  query_error_s = intern(lit("query-error"), user_package);
  file_error_s = intern(lit("file-error"), user_package);
  process_error_s = intern(lit("process-error"), user_package);
  syntax_error_s = intern(lit("syntax-error"), user_package);
  system_error_s = intern(lit("system-error"), user_package);
  timeout_error_s = intern(lit("timeout-error"), user_package);
  alloc_error_s = intern(lit("alloc-error"), user_package);
  stack_overflow_s = intern(lit("stack-overflow"), user_package);
  path_not_found_s = intern(lit("path-not-found"), user_package);
  path_exists_s = intern(lit("path-exists"), user_package);
  path_permission_s = intern(lit("path-permission"), user_package);
  assert_s = intern(lit("assert"), user_package);
  warning_s = intern(lit("warning"), user_package);
  defr_warning_s = intern(lit("defr-warning"), user_package);
  restart_s = intern(lit("restart"), user_package);
  continue_s = intern(lit("continue"), user_package);
  name_s = intern(lit("name"), user_package);
  length_s = intern(lit("length"), user_package);
  rplaca_s = intern(lit("rplaca"), user_package);
  rplacd_s = intern(lit("rplacd"), user_package);
  seq_iter_s = intern(lit("seq-iter"), user_package);

  args_k = intern(lit("args"), keyword_package);
  nothrow_k = intern(lit("nothrow"), keyword_package);
  colon_k = intern(lit(""), keyword_package);
  auto_k = intern(lit("auto"), keyword_package);
  fun_k = intern(lit("fun"), keyword_package);
  wrap_k = intern(lit("wrap"), keyword_package);
  reflect_k = intern(lit("reflect"), keyword_package);

  seq_iter_cls = cobj_register(seq_iter_s);

  equal_f = func_n2(equal);
  eq_f = func_n2(eq);
  eql_f = func_n2(eql);
  identity_f = func_n1(identity);
  identity_star_f = func_n0v(identity_star);
  car_f = func_n1(car);
  cdr_f = func_n1(cdr);
  null_f = func_n1(null);
  list_f = func_n0v(listv);
  less_f = func_n2(less);
  greater_f = func_n2(greater);
  prog_string = string(progname);

  cobj_hash = make_hash(nil, nil, nil);
}

static val simple_qref_args_p(val args, val pos)
{
  if (nilp(args)) {
    return if2(ge(pos, two), t);
  } else if (!consp(args)) {
    return nil;
  } else {
    val arg = car(args);

    if (symbolp(arg)) {
      val name = symbol_name(arg);
      if (length(name) == zero)
        return nil;
      if (!zerop(pos) && chr_isdigit(chr_str(name, zero)))
      {
        return nil;
      }
      return simple_qref_args_p(cdr(args), succ(pos));
    }
    if (consp(arg) && car(arg) != qref_s && car(arg) != uref_s) {
      return simple_qref_args_p(cdr(args), succ(pos));
    }
    return nil;
  }
}

void out_str_char(wchar_t ch, val out, int *semi_flag, int regex)
{
  if (*semi_flag && (iswxdigit(ch) || ch == ';'))
    put_char(chr(';'), out);

  *semi_flag = 0;

  switch (ch) {
  case '\a': put_string(lit("\\a"), out); break;
  case '\b': put_string(lit("\\b"), out); break;
  case '\t': put_string(lit("\\t"), out); break;
  case '\n': put_string(lit("\\n"), out); break;
  case '\v': put_string(lit("\\v"), out); break;
  case '\f': put_string(lit("\\f"), out); break;
  case '\r': put_string(lit("\\r"), out); break;
  case '\\': put_string(lit("\\\\"), out); break;
  case 27: put_string(lit("\\e"), out); break;
  case '"':
    if (regex)
      put_char(chr(ch), out);
    else
      put_string(lit("\\\""), out);
    break;
  default:
    {
      val fmt = nil;

      if ((ch < 0x20) || (ch >= 0x7F && ch < 0xA0))
        fmt = lit("\\x~,02X");
      else if ((ch >= 0xD800 && ch < 0xE000) || ch == 0xFFFE || ch == 0xFFFF)
        fmt = lit("\\x~,04X");
      else if (ch >= 0xFFFF)
        fmt = lit("\\x~,06X");
      else
        put_char(chr(ch), out);

      if (fmt) {
        format(out, fmt, num(ch), nao);
        *semi_flag = 1;
      }
    }
  }
}

static void out_str_readable(const wchar_t *ptr, val out, int *semi_flag)
{
  for (; *ptr; ptr++)
    out_str_char(*ptr, out, semi_flag, 0);
}

static void out_lazy_str(val lstr, val out, struct strm_base *strm)
{
  val self = lit("print");
  int semi_flag = 0;
  val lim = lstr->ls.props->limit;
  val term = lstr->ls.props->term;
  val iter;
  const wchar_t *wcterm;
  cnum max_len = strm->max_length;
  cnum max_chr = max_len ? max_str_chars(max_len) : 0;

  wcterm = c_str(term, self);

  put_char(chr('"'), out);

  out_str_readable(c_str(lstr->ls.prefix, self), out, &semi_flag);

  for (iter = lstr->ls.list; (!lim || gt(lim, zero)) && iter;
       iter = cdr(iter))
  {
    val str = car(iter);
    if (!str)
      break;
    if (max_len) {
      if (length_str_gt(str, num(max_chr))) {
        out_str_readable(c_str(sub_str(str, zero, num(max_chr)), self),
                         out, &semi_flag);
        goto max_reached;
      }
      if (--max_chr == 0)
        goto max_reached;
      max_chr -= c_num(length_str(str), self);
    }
    out_str_readable(c_str(str, self), out, &semi_flag);
    out_str_readable(wcterm, out, &semi_flag);
    if (lim)
      lim = pred(lim);
  }

  if (0) {
max_reached:
    put_string(lit("\\..."), out);
  }
  put_char(chr('"'), out);
}

static void out_quasi_str_sym(val name, val mods, val rem_args,
                              val out, struct strm_ctx *ctx)
{
  val next_elem = car(rem_args);
  val next_elem_char = and3(next_elem && !mods, stringp(next_elem),
                            chr_str(next_elem, zero));
  val osstrm = make_string_output_stream();
  val namestr = (obj_print_impl(name, osstrm, nil, ctx),
                 get_string_from_stream(osstrm));
  int need_brace = mods ||
    (next_elem_char &&
     (chr_isalpha(next_elem_char) ||
      chr_isdigit(next_elem_char) ||
      next_elem_char == chr('_'))) ||
    (span_str(namestr, lit("0123456789_"
                          "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                          "abcdefghijklmnopqrstuvwxyz"))
     != length(namestr));
  put_char(chr('@'), out);
  if (need_brace)
    put_char(chr('{'), out);
  put_string(namestr, out);
  while (mods) {
    put_char(chr(' '), out);
    obj_print_impl(car(mods), out, nil, ctx);
    mods = cdr(mods);
  }
  if (need_brace)
    put_char(chr('}'), out);
}

static void out_quasi_str(val args, val out, struct strm_ctx *ctx)
{
  val self = lit("print");
  val iter, next;
  cnum max_len = ctx->strm->max_length, max_count = max_len;
  cnum max_chr = max_len ? max_str_chars(max_len) : 0;

  for (iter = cdr(args); iter; iter = next) {
    val elem = car(iter);
    next = cdr(iter);

    if (stringp(elem)) {
      int semi_flag = 0;
      if (max_len && length_str_gt(elem, num(max_chr))) {
        out_str_readable(c_str(sub_str(elem, zero, num(max_chr)), self),
                         out, &semi_flag);
        goto max_exceeded;
      } else {
        out_str_readable(c_str(elem, self), out, &semi_flag);
        if (max_len) {
          max_chr -= c_num(length(elem), self);
          if (max_chr == 0) {
            goto max_reached;
          }
        }
      }
    } else if (consp(elem)) {
      val sym = car(elem);
      if (sym == var_s)
      {
        out_quasi_str_sym(second(elem), third(elem), next,
                          out, ctx);
      } else {
        put_char(chr('@'), out);
        obj_print_impl(elem, out, nil, ctx);
      }
    } else if (symbolp(elem)) {
      out_quasi_str_sym(elem, nil, next, out, ctx);
    } else {
      obj_print_impl(elem, out, nil, ctx);
    }

    if (--max_count == 0)
      goto max_reached;
  }

  return;

max_reached:
  if (next)
max_exceeded:
    put_string(lit("\\..."), out);
}

static void out_json_str(val str, val out)
{
  val self = lit("print");
  const wchar_t *cstr = c_str(str, self);
  wchar_t ch;

  put_char(chr('"'), out);

  while ((ch = *cstr++)) {
    switch (ch) {
    case '\\':
    case '"':
      put_char(chr('\\'), out);
      put_char(chr(ch), out);
      break;
    case '\b':
      put_string(lit("\\b"), out);
      break;
    case '\f':
      put_string(lit("\\f"), out);
      break;
    case '\n':
      put_string(lit("\\n"), out);
      break;
    case '\r':
      put_string(lit("\\r"), out);
      break;
    case '\t':
      put_string(lit("\\t"), out);
      break;
    case '<':
      put_char(chr(ch), out);
      if (wcsncmp(cstr, L"/script", 7) == 0) {
        put_char(chr('\\'), out);
      } else if (wcsncmp(cstr, L"!--", 3) == 0) {
        put_string(lit("\\u0021"), out);
        cstr++;
      }
      break;
    case '-':
      put_char(chr(ch), out);
      if (wcsncmp(cstr, L"->", 2) == 0) {
        put_string(lit("\\u002D"), out);
        cstr++;
      }
      break;
    case 0xDC00:
      put_string(lit("\\u0000"), out);
      break;
    default:
      {

        if ((ch < 0x20) || (ch >= 0x7F && ch < 0xA0) ||
            (ch >= 0xD800 && ch < 0xDC00) ||
            (ch >= 0xDD00 && ch < 0xE000) ||
            ch == 0xFFFE || ch == 0xFFFF)
        {
          format(out, lit("\\u~,04X"), chr(ch), nao);
        } else if (ch >= 0xFFFF) {
          wchar_t c20 = ch - 0x10000;
          wchar_t sg0 = 0xD800 + ((c20 >> 10) & 0x3FF);
          wchar_t sg1 = 0xDC00 + (c20 & 0x3FF);
          format(out, lit("\\u~,04X\\u~,04X"), chr(sg0), chr(sg1), nao);
        } else {
          put_char(chr(ch), out);
        }
      }
      break;
    }
  }

  put_char(chr('"'), out);
}

INLINE int circle_print_eligible(val obj)
{
  return is_ptr(obj) && (!symbolp(obj) || !symbol_package(obj));
}

static int check_emit_circle(val obj, val out, struct strm_ctx *ctx, val self)
{
  if (ctx->obj_hash && circle_print_eligible(obj)) {
    loc pcdr = gethash_l(self, ctx->obj_hash, obj, nulloc);
    val label = deref(pcdr);

    if (label == t) {
      val counter = succ(ctx->counter);
      ctx->counter = counter;
      set(pcdr, counter);
      format(out, lit("#~s="), counter, nao);
    } else if (integerp(label)) {
      format(out, lit("#~s#"), label, nao);
      return 1;
    } else if (!label) {
      set(pcdr, colon_k);
    }
  }

  return 0;
}

static void out_json_rec(val obj, val out, struct strm_ctx *ctx)
{
  val self = lit("print");

  if (ctx && check_emit_circle(obj, out, ctx, self))
    return;

  switch (type(obj)) {
  case NIL:
    put_string(lit("false"), out);
    return;
  case SYM:
    if (obj == t) {
      put_string(lit("true"), out);
      return;
    }
    if (obj == null_s) {
      put_string(lit("null"), out);
      return;
    }
    break;
  case CONS:
    if (ctx != 0) {
      val sym = car(obj);
      if (sym == hash_lit_s) {
        val save_indent;
        int force_br = 0;
        val iter, next;
        put_char(chr('{'), out);
        save_indent = inc_indent(out, zero);
        for (iter = cddr(obj), next = nil; iter; iter = next) {
          val pair = car(iter);
          val k = car(pair), v = cadr(pair);
          if (consp(k) || consp(v)) {
            if (next)
              put_char(chr(' '), out);
            out_json_rec(k, out, ctx);
            put_string(lit(" : "), out);
          } else {
            out_json_rec(k, out, ctx);
            put_char(chr(':'), out);
          }
          out_json_rec(v, out, ctx);
          if ((next = cdr(iter)) != 0) {
            put_char(chr(','), out);
            if (width_check(out, nil))
              force_br = 1;
          }
        }
        put_char(chr('}'), out);
        if (force_br)
          force_break(out);
        if (save_indent)
          set_indent(out, save_indent);
        return;
      }
      if (sym == vector_lit_s) {
        val save_indent;
        int force_br = 0;
        val iter, next;
        put_char(chr('['), out);
        save_indent = inc_indent(out, zero);
        for (iter = cadr(obj), next = nil; iter; iter = next) {
          val elem = car(iter);
          next = cdr(iter);
          out_json_rec(elem, out, ctx);
          if (next) {
            put_char(chr(','), out);
            if (width_check(out, nil))
              force_br = 1;
          }
        }
        put_char(chr(']'), out);
        if (force_br)
          force_break(out);
        if (save_indent)
          set_indent(out, save_indent);
        return;
      }
      if (sym == sys_unquote_s) {
        put_char(chr('~'), out);
        obj_print_impl(cadr(obj), out, nil, ctx);
        return;
      }
      if (sym == sys_splice_s) {
        put_string(lit("~*"), out);
        obj_print_impl(cadr(obj), out, nil, ctx);
        return;
      }
    }
    break;
  case VEC:
    {
      val save_indent;
      int force_br = 0;
      cnum len = c_num(length(obj), self);
      cnum i;

      put_char(chr('['), out);
      save_indent = inc_indent(out, zero);
      for (i = 0; i < len; i++) {
        val elem = obj->v.vec[i];
        out_json_rec(elem, out, ctx);
        if (i < len - 1) {
          put_char(chr(','), out);
          if (width_check(out, nil))
            force_br = 1;
        }
      }
      put_char(chr(']'), out);
      if (force_br)
        force_break(out);
      if (save_indent)
        set_indent(out, save_indent);
      return;
    }
    break;
  case COBJ:
    if (hashp(obj)) {
      val save_indent;
      int force_br = 0;
      val cell, next;
      struct hash_iter hi;

      us_hash_iter_init(&hi, obj);

      put_char(chr('{'), out);
      save_indent = inc_indent(out, zero);
      for (next = nil, cell = hash_iter_next(&hi); cell; cell = next) {
        val k = car(cell), v = cdr(cell);
        if (consp(k) || consp(v)) {
          if (next)
            put_char(chr(' '), out);
          out_json_rec(k, out, ctx);
          put_string(lit(" : "), out);
        } else {
          out_json_rec(k, out, ctx);
          put_char(chr(':'), out);
        }
        out_json_rec(v, out, ctx);
        if ((next = hash_iter_next(&hi)) != 0) {
          put_char(chr(','), out);
          if (width_check(out, nil))
            force_br = 1;
        }
      }
      put_char(chr('}'), out);
      if (force_br)
        force_break(out);
      if (save_indent)
        set_indent(out, save_indent);
      return;
    }
    break;
  case FLNUM:
    format(out, lit("~a"), obj, nao);
    return;
  case LIT:
  case STR:
  case LSTR:
    out_json_str(obj, out);
    return;
  default:
    break;
  }

  uw_throwf(type_error_s, lit("~a: invalid object ~s in JSON"),
            self, obj, nao);
}

static void out_json(val op, val obj, val out, struct strm_ctx *ctx)
{
  val save_mode = test_set_indent_mode(out, num_fast(indent_off),
                                       num_fast(indent_data));
  if (op == sys_qquote_s)
    put_char(chr('^'), out);
  out_json_rec(obj, out, ctx);
  set_indent_mode(out, save_mode);
}

static int unquote_star_check(val obj, val pretty)
{
  if (!obj || !symbolp(obj))
    return 0;
  if (car(obj->s.name) != chr('*'))
    return 0;
  return pretty || !symbol_needs_prefix(lit("print"), cur_package, obj);
}

val obj_print_impl(val obj, val out, val pretty, struct strm_ctx *ctx)
{
  val self = lit("print");
  val ret = obj;
  cnum save_depth = ctx->depth;

  gc_stack_check();

  if (check_emit_circle(obj, out, ctx, self))
    return ret;

  if (ctx->strm->max_depth) {
    if (ctx->depth > ctx->strm->max_depth) {
      put_string(lit("..."), out);
      return obj;
    }

    if (ctx->depth == ctx->strm->max_depth) {
      switch (type(obj)) {
      case CONS:
      case LCONS:
        put_string(lit("(...)"), out);
        return obj;
      case VEC:
        put_string(lit("#(...)"), out);
        return obj;
      case COBJ:
        if (hashp(obj)) {
          put_string(lit("#H(...)"), out);
          return obj;
        } else if (structp(obj)) {
          put_string(lit("#S(...)"), out);
          return obj;
        }
      default:
        break;
      }
    }

    ctx->depth++;
  }

  switch (type(obj)) {
  case NIL:
    put_string(if3(get_indent_mode(out) == num_fast(indent_code),
                   lit("()"), lit("nil")), out);
    break;
  case CONS:
  case LCONS:
    {
      val sym = car(obj);
      val save_mode = test_set_indent_mode(out, num_fast(indent_off),
                                           num_fast(indent_data));
      val save_indent = nil;
      val args = cdr(obj);
      int have_args = consp(args) != nil;
      int two_elem = have_args && cdr(args) == nil;
      val arg = if2(have_args, car(args));

      if (sym == quote_s && two_elem) {
        put_char(chr('\''), out);
        obj_print_impl(arg, out, pretty, ctx);
      } else if (sym == sys_qquote_s && two_elem) {
        put_char(chr('^'), out);
        obj_print_impl(arg, out, pretty, ctx);
      } else if (sym == sys_unquote_s && two_elem) {
        val arg = car(args);
        put_char(chr(','), out);
        if (unquote_star_check(arg, pretty))
          put_char(chr(' '), out);
        obj_print_impl(arg, out, pretty, ctx);
      } else if (sym == sys_splice_s && two_elem) {
        put_string(lit(",*"), out);
        obj_print_impl(arg, out, pretty, ctx);
      } else if (sym == vector_lit_s && two_elem) {
        put_char(chr('#'), out);
        obj_print_impl(arg, out, pretty, ctx);
      } else if (sym == hash_lit_s) {
        put_string(lit("#H"), out);
        obj_print_impl(rest(obj), out, pretty, ctx);
      } else if (sym == struct_lit_s) {
        put_string(lit("#S"), out);
        obj_print_impl(rest(obj), out, pretty, ctx);
      } else if (sym == json_s && have_args &&
                 consp(cdr(args)) && nilp(cddr(args)))
      {
        put_string(lit("#J"), out);
        out_json(arg, cadr(args), out, ctx);
      } else if (sym == var_s && two_elem &&
                 (symbolp(arg) || integerp(arg)))
      {
        put_char(chr('@'), out);
        obj_print_impl(arg, out, pretty, ctx);
      } else if (sym == expr_s && two_elem && consp(arg)) {
        val inarg = car(arg);
        put_char(chr('@'), out);
        if (inarg != rcons_s) {
          obj_print_impl(arg, out, pretty, ctx);
        } else {
          obj = arg;
          sym = inarg;
          args = cdr(obj);
          arg = car(obj);
          goto list;
        }
      } else if (sym == rcons_s && have_args
                 && consp(cdr(args)) && !(cddr(args)))
      {
        obj_print_impl(arg, out, pretty, ctx);
        put_string(lit(".."), out);
        obj_print_impl(cadr(args), out, pretty, ctx);
      } else if ((sym == uref_s || sym == qref_s) &&
                 simple_qref_args_p(args, if3(sym == uref_s, one, zero)))
      {
        val iter = args, next;

        if (sym == uref_s) {
          if (car(iter) == t) {
            put_string(lit(".?"), out);
            iter = cdr(iter);
          } else {
            put_char(chr('.'), out);
          }
        }

        for (; iter; iter = next) {
          val el = car(iter);
          val qmark = nil;
          next = cdr(iter);
          if (next && consp(el) && car(el) == t && consp(cdr(el)) && !cddr(el)) {
            el = cadr(el);
            qmark = t;
          }
          obj_print_impl(el, out, pretty, ctx);
          if (next) {
            put_char(chr('.'), out);
            if (qmark)
              put_char(chr('?'), out);
          }
        }
      } else if (sym == quasi_s && have_args) {
        put_char(chr('`'), out);
        out_quasi_str(obj, out, ctx);
        put_char(chr('`'), out);
      } else if (sym == quasilist_s && have_args) {
        cnum max_length = ctx->strm->max_length;
        put_string(lit("#`"), out);
        out_quasi_str(arg, out, ctx);
        args = cdr(args);

        while (args) {
          put_char(chr(' '), out);
          if (max_length && --max_length == 0) {
            put_string(lit("..."), out);
            break;
          }
          out_quasi_str(car(args), out, ctx);
          args = cdr(args);
        }
        put_char(chr('`'), out);
      } else list: {
        val iter;
        val closepar = chr(')');
        val indent = zero;
        int force_br = 0;
        cnum max_len = ctx->strm->max_length;
        cnum max_count = max_len;

        if (sym == dwim_s && have_args) {
          put_char(chr('['), out);
          obj = args;
          closepar = chr(']');
        } else {
          put_char(chr('('), out);
        }

        if (sym == lambda_s && consp(cdr(obj)) && symbolp(second(obj))) {
          indent = one;
          save_indent = inc_indent(out, indent);
          test_neq_set_indent_mode(out, num_fast(indent_foff), num_fast(indent_code));
          obj_print_impl(sym, out, pretty, ctx);
          if (second(obj)) {
            put_string(lit(" (. "), out);
            obj_print_impl(second(obj), out, pretty, ctx);
            put_char(chr(')'), out);
          } else {
            put_string(lit(" ()"), out);
          }
          iter = cdr(obj);
          goto finish;
        } else if (special_operator_p(sym) || macro_form_p(obj, nil)) {
          indent = one;
          test_neq_set_indent_mode(out, num_fast(indent_foff), num_fast(indent_code));
        } else if (symbolp(sym) && fboundp(sym)) {
          obj_print_impl(sym, out, pretty, ctx);
          indent = one;
          save_indent = inc_indent(out, indent);
          test_neq_set_indent_mode(out, num_fast(indent_foff), num_fast(indent_code));
          iter = obj;
          goto finish;
        }

        save_indent = inc_indent(out, indent);

        for (iter = obj; consp(iter); iter = cdr(iter)) {
          val d;
          {
            val a = car(iter);
            val unq = nil;

            if (a == sys_unquote_s)
              unq = lit(". ,");
            else if (a == sys_splice_s)
              unq = lit(". ,*");

            if (unq) {
              val d = cdr(iter);
              val ad = car(d);

              if (consp(d) && !cdr(d)) {
                put_string(unq, out);
                if (a == sys_unquote_s && unquote_star_check(ad, pretty))
                  put_char(chr(' '), out);
                obj_print_impl(ad, out, pretty, ctx);
                put_char(closepar, out);
                break;
              }
            }

            if (max_len && --max_count < 0) {
              put_string(lit("..."), out);
              iter = nil;
            } else {
              obj_print_impl(a, out, pretty, ctx);
            }
          }
finish:
          d = cdr(iter);
          if (nilp(d)) {
            put_char(closepar, out);
          } else if (ctx->obj_hash && gethash(ctx->obj_hash, d)) {
            iter = nil;
            goto dot;
          } else if (consp(d)) {
            if (width_check(out, chr(' ')))
              force_br = 1;
          } else {
dot:
            put_string(lit(" . "), out);
            obj_print_impl(d, out, pretty, ctx);
            put_char(closepar, out);
          }
        }

        if (force_br)
          force_break(out);
      }

      if (save_indent)
        set_indent(out, save_indent);
      set_indent_mode(out, save_mode);
      break;
    }
  case LIT:
  case STR:
    {
      cnum max_length = ctx->strm->max_length;
      cnum max_chr = max_str_chars(max_length);

      if (pretty) {
        if (!max_length || le(length_str(obj), num(max_chr))) {
          put_string(obj, out);
        } else {
          put_string(sub_str(obj, zero, num(max_chr)), out);
          put_string(lit("..."), out);
        }
      } else {
        int semi_flag = 0;
        put_char(chr('"'), out);

        if (!max_length || le(length_str(obj), num(max_chr))) {
          out_str_readable(c_str(obj, self), out, &semi_flag);
        } else {
          out_str_readable(c_str(sub_str(obj, zero, num(max_chr)), self),
                           out, &semi_flag);
          put_string(lit("\\..."), out);
        }

        put_char(chr('"'), out);
      }
    }
    break;
  case CHR:
    if (pretty) {
      put_char(obj, out);
    } else {
      wchar_t ch = c_chr(obj);

      put_string(lit("#\\"), out);
      switch (ch) {
      case '\0': put_string(lit("nul"), out); break;
      case '\a': put_string(lit("alarm"), out); break;
      case '\b': put_string(lit("backspace"), out); break;
      case '\t': put_string(lit("tab"), out); break;
      case '\n': put_string(lit("newline"), out); break;
      case '\v': put_string(lit("vtab"), out); break;
      case '\f': put_string(lit("page"), out); break;
      case '\r': put_string(lit("return"), out); break;
      case 27: put_string(lit("esc"), out); break;
      case ' ': put_string(lit("space"), out); break;
      case 0xDC00: put_string(lit("pnul"), out); break;
      case 0xFEFF: case 0xFFFE: case 0xFFFF:
      hex:
        format(out, lit("x~X"), num(ch), nao);
        break;
      default:
        if ((ch < 0x20) || (ch >= 0x7F && ch < 0xA0))
          goto hex;
        else if (ch >= 0xD800 && ch < 0xE000)
          goto hex;
        else if (ch >= 0xFFFF)
          goto hex;
        else
          put_char(chr(ch), out);
      }
    }
    break;
  case NUM:
  case BGNUM:
    format(out, lit("~s"), obj, nao);
    break;
  case FLNUM:
    {
      val fmt = cdr(lookup_var(nil,
                               if3(pretty,
                                   pprint_flo_format_s, print_flo_format_s)));
      format(out, fmt, obj, nao);
    }
    break;
  case SYM:
    if (pretty) {
      if (!opt_compat || opt_compat > 202)
        if (obj->s.package == keyword_package)
          put_char(chr(':'), out);
    } else {
      val prefix = symbol_needs_prefix(self, cur_package, obj);

      if (prefix) {
        put_string(prefix, out);
        put_char(chr(':'), out);
      }
    }
    put_string(symbol_name(obj), out);
    break;
  case PKG:
    format(out, lit("#<package: ~a>"), obj->pk.name, nao);
    break;
  case FUN:
    {
      struct func *f = &obj->f;
      if (f->functype == FINTERP) {
        val fun = f->f.interp_fun;
        format(out, lit("#<interpreted fun: ~s ~s>"),
               car(fun), cadr(fun), nao);
      } else {
        format(out, lit("#<~a fun: ~a param"),
               if3(f->functype == FVM, lit("vm"), lit("intrinsic")),
               num_fast(f->fixparam - f->optargs), nao);
        if (f->optargs)
          format(out, lit(" + ~a optional"),
               num_fast(f->optargs), nao);
        if (obj->f.variadic)
          put_string(lit(" + variadic>"), out);
        else
          put_char(chr('>'), out);
      }
    }
    break;
  case VEC:
    {
      cnum i, length = c_num(obj->v.vec[vec_length], self);
      cnum max_length = ctx->strm->max_length;
      val save_mode = test_set_indent_mode(out, num_fast(indent_off),
                                           num_fast(indent_data));
      val save_indent;
      int force_br = 0;

      put_string(lit("#("), out);

      save_indent = inc_indent(out, zero);

      for (i = 0; i < length; i++) {
        val elem = obj->v.vec[i];
        if (max_length && i >= max_length) {
          put_string(lit("..."), out);
          break;
        }
        obj_print_impl(elem, out, pretty, ctx);
        if (i < length - 1)
          if (width_check(out, chr(' ')))
            force_br = 1;
      }

      put_char(chr(')'), out);

      if (force_br)
        force_break(out);

      set_indent(out, save_indent);
      set_indent_mode(out, save_mode);
    }
    break;
  case LSTR:
    if (pretty) {
      lazy_str_put(obj, out, ctx->strm);
    } else {
      out_lazy_str(obj, out, ctx->strm);
    }
    break;
  case COBJ:
  case CPTR:
    obj->co.ops->print(obj, out, pretty, ctx);
    break;
  case ENV:
    format(out, lit("#<environment: ~p>"), obj, nao);
    break;
  case RNG:
    format(out, if3(pretty, lit("#R(~a ~a)"), lit("#R(~s ~s)")),
           from(obj), to(obj), nao);
    break;
  case BUF:
    if (pretty)
      buf_pprint(obj, out);
    else
      buf_print(obj, out);
    break;
  case TNOD:
    format(out, if3(pretty, lit("#N(~a ~a ~a)"), lit("#N(~s ~s ~s)")),
           obj->tn.key, obj->tn.left, obj->tn.right, nao);
    break;
  default:
    format(out, lit("#<garbage: ~p>"), obj, nao);
    break;
  }

  if (ctx->depth != save_depth)
    ctx->depth = save_depth;

  return ret;
}

static void populate_obj_hash(val obj, struct strm_ctx *ctx)
{
  val self = lit("print");
tail:
  if (circle_print_eligible(obj)) {
    if (ctx->obj_hash_prev) {
      val prev_cell = gethash_e(self, ctx->obj_hash_prev, obj);
      val label = cdr(prev_cell);

      if (label == colon_k)
        uw_throwf(error_s, lit("~a: unexpected duplicate object "
                               "(misbehaving print method?)"), self, nao);
      if (prev_cell)
        return;
    } else {
      val new_p;
      val cell = gethash_c(self, ctx->obj_hash, obj, mkcloc(new_p));

      if (!new_p) {
        rplacd(cell, t);
        return;
      }
    }
  } else {
    return;
  }

  switch (type(obj)) {
  case CONS:
  case LCONS:
    {
      populate_obj_hash(car(obj), ctx);
      obj = cdr(obj);
      goto tail;
    }
  case VEC:
    {
      cnum i;
      cnum l = c_num(length_vec(obj), self);

      for (i = 0; i < l; i++) {
        val in = num(i);
        populate_obj_hash(vecref(obj, in), ctx);
      }

      break;
    }
  case RNG:
    {
      populate_obj_hash(from(obj), ctx);
      obj = to(obj);
      goto tail;
    }
  case TNOD:
    {
      populate_obj_hash(obj->tn.left, ctx);
      populate_obj_hash(obj->tn.right, ctx);
      obj = obj->tn.key;
      goto tail;
    }
  case COBJ:
    if (hashp(obj)) {
      struct hash_iter hi;
      val cell;
      us_hash_iter_init(&hi, obj);
      while ((cell = hash_iter_next(&hi))) {
        populate_obj_hash(us_car(cell), ctx);
        populate_obj_hash(us_cdr(cell), ctx);
      }
      obj = get_hash_userdata(obj);
      goto tail;
    } else if (obj_struct_p(obj)) {
      val stype = struct_type(obj);
      val iter;

      for (iter = slots(stype); iter; iter = cdr(iter)) {
        val sn = car(iter);
        populate_obj_hash(slot(obj, sn), ctx);
      }
    } else if (treep(obj)) {
      val iter = tree_begin(obj, colon_k, colon_k);
      val node;
      while ((node = tree_next(iter)))
        populate_obj_hash(key(node), ctx);
    }
    break;
  case FUN:
    if (obj->f.functype == FINTERP) {
      val fun = obj->f.f.interp_fun;
      populate_obj_hash(car(fun), ctx);
      obj = cadr(fun);
      goto tail;
    }
  default:
    break;
  }
}

static void obj_hash_merge(val parent_hash, val child_hash)
{
  val self = lit("print");
  struct hash_iter hi;
  val cell;

  for (us_hash_iter_init(&hi, child_hash); (cell = hash_iter_next(&hi));) {
    val new_p;
    loc pcdr = gethash_l(self, parent_hash, us_car(cell), mkcloc(new_p));
    if (new_p)
      set(pcdr, us_cdr(cell));
    else
      uw_throwf(error_s, lit("~a: unexpected duplicate object "
                             "(internal error?)"), self, nao);
  }
}

val obj_print(val obj, val out, val pretty)
{
  val self = lit("print");
  val ret = nil;
  val save_mode = get_indent_mode(out);
  val save_indent = get_indent(out);
  struct strm_ctx *ctx_orig = get_ctx(out);
  struct strm_ctx *volatile ctx = ctx_orig, ctx_struct;
  uw_frame_t cont_guard;

  if (ctx_orig)
    uw_push_guard(&cont_guard, 1);

  uw_simple_catch_begin;

  if (ctx) {
    if (cdr(lookup_var(nil, print_circle_s))) {
      ctx->obj_hash_prev = ctx->obj_hash;
      ctx->obj_hash = make_eq_hash(nil, nil);
      populate_obj_hash(obj, ctx);
      obj_hash_merge(ctx->obj_hash_prev, ctx->obj_hash);
      ctx->obj_hash = ctx->obj_hash_prev;
      ctx->obj_hash_prev = nil;
    }
  } else {
    struct strm_base *s = coerce(struct strm_base *,
                                 cobj_handle(self, out, stream_cls));
    ctx = &ctx_struct;
    ctx->strm = s;
    ctx->counter = zero;
    ctx->obj_hash_prev = nil;
    ctx->obj_hash = if2(print_circle_s &&
                        cdr(lookup_var(nil, print_circle_s)),
                        make_eq_hash(nil, nil));
    ctx->depth = 0;
    get_set_ctx(out, ctx);
    if (ctx->obj_hash)
      populate_obj_hash(obj, ctx);
  }

  ret = obj_print_impl(obj, out, pretty, ctx);

  uw_unwind {
    set_indent_mode(out, save_mode);
    set_indent(out, save_indent);
    if (ctx != ctx_orig)
      get_set_ctx(out, ctx_orig);
  }

  uw_catch_end;

  if (ctx_orig)
    uw_pop_frame(&cont_guard);

  return ret;
}

val print(val obj, val stream, val pretty)
{
  return obj_print(obj, default_arg_strict(stream, std_output),
                   default_null_arg(pretty));
}

val pprint(val obj, val stream)
{
  return obj_print(obj, default_arg_strict(stream, std_output), t);
}

val tostring(val obj)
{
  val ss = make_string_output_stream();
  obj_print(obj, ss, nil);
  return get_string_from_stream(ss);
}

val tostringp(val obj)
{
  val ss = make_string_output_stream();
  pprint(obj, ss);
  return get_string_from_stream(ss);
}

val put_json(val obj, val stream_in, val flat)
{
  val stream = default_arg_strict(stream_in, std_output);
  val imode = if3(default_null_arg(flat),
                  set_indent_mode(stream, num_fast(indent_foff)),
                  test_set_indent_mode(stream, num_fast(indent_off),
                                       num_fast(indent_data)));
  out_json_rec(obj, stream, 0);
  set_indent_mode(stream, imode);
  return t;
}

val put_jsonl(val obj, val stream, val flat)
{
  put_json(obj, stream, flat);
  put_char(chr('\n'), stream);
  return t;
}

val tojson(val obj, val flat)
{
  val ss = make_string_output_stream();
  put_json(obj, ss, flat);
  return get_string_from_stream(ss);
}

val display_width(val obj)
{
  val self = lit("display-width");

  if (stringp(obj)) {
    const wchar_t *s = c_str(obj, self);
    cnum width = 0;
    for (; *s; s++) {
      if (iswcntrl(*s))
        continue;
      width += 1 + wide_display_char_p(*s);
    }
    return num(width);
  } else if (chrp(obj)) {
    wchar_t ch = c_chr(obj);
    if (iswcntrl(ch))
      return zero;
    return num_fast(1 + wide_display_char_p(ch));
  }

  uw_throwf(type_error_s, lit("~a: ~s isn't a character or string"),
            self, obj, nao);
}

void init(val *stack_bottom)
{
  int gc_save;
  gc_save = gc_state(0);

  t = one;
  gc_init(stack_bottom);
  hash_early_init();
#if CONFIG_LOCALE_TOLERANCE
  locale_init();
#endif
  obj_init();
  uw_init();
  eval_init();
  hash_init();
  struct_init();
  tree_init();
  buf_init();
  ffi_init();
  sysif_init();
  arith_init();
  rand_init();
  stream_init();
  strudel_init();
  vm_init();
#if HAVE_POSIX_SIGS
  sig_init();
#endif
  filter_init();
  regex_init();
  gc_late_init();
  parse_init();
  uw_late_init();
  less_tab_init();
#if HAVE_SYSLOG
  syslog_init();
#endif
#if HAVE_GLOB
  glob_init();
#endif
#if HAVE_FTW
  ftw_init();
#endif
#if HAVE_TERMIOS
  termios_init();
#endif
  cadr_init();
  time_init();
  chksum_init();

  cobj_populate_hash();

  gc_state(gc_save);
}

int compat_fixup(int compat_ver)
{
  if (compat_ver < 97)
    return 97;

  if (compat_ver == 97) {
    symbol_setname(type_error_s, lit("type_error"));
    symbol_setname(internal_error_s, lit("internal_error"));
    symbol_setname(numeric_error_s, lit("numeric_error"));
    symbol_setname(range_error_s, lit("range_error"));
    symbol_setname(query_error_s, lit("query_error"));
    symbol_setname(file_error_s, lit("file_error"));
    symbol_setname(process_error_s, lit("process_error"));
  }

  eval_compat_fixup(compat_ver);
  rand_compat_fixup(compat_ver);
  parse_compat_fixup(compat_ver);
  return 0;
}

void dump(val obj, val out)
{
  obj_print(obj, out, nil);
  put_char(chr('\n'), out);
}

/*
 * Handy function for debugging in gdb,
 * so we don't have to keep typing:
 * (gdb) p dump(something, stdout)
 */
void d(val obj)
{
  int save = gc_state(0);
  dump(obj, std_output);
  gc_state(save);
}

/*
 * Function for software breakpoints.
 * Debugging routines call here when a breakpoint
 * is requested. For this to work, set a breakpoint
 * on this function.
 */
void breakpt(void)
{
}

/*
 * Function for dissembling VM functions
 * when debugging in gdb.
 */

void dis(val obj)
{
  val sym = intern(lit("disassemble"), user_package);
  val fun = cdr(if2(sym, lookup_fun(nil, sym)));
  if (fun)
    funcall1(fun, obj);
}
