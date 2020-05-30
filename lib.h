/* Copyright 2009-2020
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

#include "mpi/mpi.h"

typedef int_ptr_t cnum;
typedef uint_ptr_t ucnum;

#if HAVE_DOUBLE_INTPTR_T
typedef double_intptr_t dbl_cnum;
typedef double_uintptr_t dbl_ucnum;
#endif

#ifdef __cplusplus
#define strip_qual(TYPE, EXPR) (const_cast<TYPE>(EXPR))
#define convert(TYPE, EXPR) (static_cast<TYPE>(EXPR))
#define coerce(TYPE, EXPR) (reinterpret_cast<TYPE>(EXPR))
#else
#define strip_qual(TYPE, EXPR) ((TYPE) (EXPR))
#define convert(TYPE, EXPR) ((TYPE) (EXPR))
#define coerce(TYPE, EXPR) ((TYPE) (EXPR))
#endif

#define TAG_SHIFT 2
#define TAG_MASK ((convert(cnum, 1) << TAG_SHIFT) - 1)
#define TAG_PTR 0
#define TAG_NUM 1
#define TAG_CHR 2
#define TAG_LIT 3
#define NUM_MAX (INT_PTR_MAX/4)
#define NUM_MIN (INT_PTR_MIN/4)

#define PTR_BIT (SIZEOF_PTR * CHAR_BIT)

#ifdef __GNUC__
#define noreturn __attribute__((noreturn))
#define NOINLINE __attribute__((noinline))
#else
#define noreturn
#define NOINLINE
#endif

typedef enum type {
  NIL = TAG_PTR, NUM = TAG_NUM, CHR = TAG_CHR, LIT = TAG_LIT, CONS,
  STR, SYM, PKG, FUN, VEC, LCONS, LSTR, COBJ, CPTR, ENV,
  BGNUM, FLNUM, RNG, BUF, TNOD, DARG, MAXTYPE = TNOD
  /* If extending, check TYPE_SHIFT and all ocurrences of MAX_TYPE */
} type_t;

#define TYPE_SHIFT 5
#define TYPE_PAIR(A, B) ((A) << TYPE_SHIFT | (B))

typedef enum functype
{
   FINTERP,             /* Interpreted function. */
   FVM,                 /* VM function. */
   F0, F1, F2, F3, F4,  /* Intrinsic functions with env. */
   N0, N1, N2, N3, N4, N5, N6, N7, N8   /* No-env intrinsics. */
} functype_t;

typedef union obj obj_t;

typedef obj_t *val;

#ifndef MEM_T_DEFINED
typedef unsigned char mem_t;
#define MEM_T_DEFINED
#endif

#if CONFIG_GEN_GC
#define obj_common \
  type_t type : PTR_BIT/2; \
  int gen : PTR_BIT/2
#else
#define obj_common \
  type_t type
#endif

struct any {
  obj_common;
  mem_t *dummy[2];
  val next; /* GC free list */
};

struct cons {
  obj_common;
  val car, cdr;
};

struct cons_hash_entry {
  obj_common;
  val car, cdr;
  ucnum hash;
};

struct string {
  obj_common;
  wchar_t *str;
  val len;
  val alloc;
};

typedef struct {
  cnum id;
  cnum slot;
} slot_cache_entry_t;
typedef slot_cache_entry_t slot_cache_set_t[4];
typedef slot_cache_set_t *slot_cache_t;

struct sym {
  obj_common;
  val name;
  val package;
  slot_cache_t slot_cache;
};

struct package {
  obj_common;
  val name;
  val symhash;
  val hidhash;
};

typedef struct args *varg;

#define FIXPARAM_BITS 7
#define FIXPARAM_MAX  ((1 << FIXPARAM_BITS) - 1)

struct func {
  obj_common;
  unsigned fixparam : FIXPARAM_BITS; /* total non-variadic parameters */
  unsigned optargs : FIXPARAM_BITS;  /* fixparam - optargs = required args */
  unsigned variadic : 1;
  unsigned : 1;
  unsigned functype : 16;
  val env;
  union {
    val interp_fun;
    val vm_desc;
    val (*f0)(val);
    val (*f1)(val, val);
    val (*f2)(val, val, val);
    val (*f3)(val, val, val, val);
    val (*f4)(val, val, val, val, val);
    val (*n0)(void);
    val (*n1)(val);
    val (*n2)(val, val);
    val (*n3)(val, val, val);
    val (*n4)(val, val, val, val);
    val (*n5)(val, val, val, val, val);
    val (*n6)(val, val, val, val, val, val);
    val (*n7)(val, val, val, val, val, val, val);
    val (*n8)(val, val, val, val, val, val, val, val);
    val (*f0v)(val, varg);
    val (*f1v)(val, val, varg);
    val (*f2v)(val, val, val, varg);
    val (*f3v)(val, val, val, val, varg);
    val (*f4v)(val, val, val, val, val, varg);
    val (*n0v)(varg);
    val (*n1v)(val, varg);
    val (*n2v)(val, val, varg);
    val (*n3v)(val, val, val, varg);
    val (*n4v)(val, val, val, val, varg);
    val (*n5v)(val, val, val, val, val, varg);
    val (*n6v)(val, val, val, val, val, val, varg);
    val (*n7v)(val, val, val, val, val, val, val, varg);
    val (*n8v)(val, val, val, val, val, val, val, val, varg);
  } f;
};

enum vecindex { vec_alloc = -2, vec_length = -1 };

struct vec {
  obj_common;
  /* vec points two elements down */
  /* vec[-2] is allocated size */
  /* vec[-1] is fill pointer */
  val *vec;
#if HAVE_VALGRIND
  val *vec_true_start;
#endif
};

/*
 * Lazy cons. When initially constructed, acts as a promise. The car and cdr
 * cache pointers are nil, and func points to a function. The job of the
 * function is to force the promise: fill car and cdr, and then flip func to
 * nil. After that, the lazy cons resembles an ordinary cons. Of course, either
 * car or cdr can point to more lazy conses.
 */

struct lazy_cons {
  obj_common;
  val car, cdr;
  val func; /* when nil, car and cdr are valid */
};

/*
 * Lazy string: virtual string which dynamically grows as a catentation
 * of a list of strings.
 */

struct lazy_string_props {
  val term;
  val limit;
};

struct lazy_string {
  obj_common;
  val prefix;           /* actual string part */
  val list;             /* remaining list */
  struct lazy_string_props *props;
};

struct cobj {
  obj_common;
  mem_t *handle;
  struct cobj_ops *ops;
  val cls;
};

struct dyn_args {
  obj_common;
  val car;
  val cdr;
  struct args *args;
};

struct strm_ctx;

struct cobj_ops {
  val (*equal)(val self, val other);
  void (*print)(val self, val stream, val pretty, struct strm_ctx *);
  void (*destroy)(val self);
  void (*mark)(val self);
  ucnum (*hash)(val self, int *count, ucnum seed);
  val (*equalsub)(val self);
};

#define cobj_ops_init(equal, print, destroy, mark, hash) \
  { equal, print, destroy, mark, hash, 0 }

#define cobj_ops_init_ex(equal, print, destroy, mark, hash, \
                         equalsub) \
  { equal, print, destroy, mark, hash, equalsub }

/* Default operations for above structure.
 * Default equal is eq
 */

void cobj_print_op(val, val, val, struct strm_ctx *);
void cptr_print_op(val, val, val, struct strm_ctx *);
val cobj_equal_handle_op(val left, val right);
void cobj_destroy_stub_op(val);
void cobj_destroy_free_op(val);
void cobj_mark_op(val);
ucnum cobj_eq_hash_op(val, int *, ucnum);
ucnum cobj_handle_hash_op(val, int *, ucnum);

struct env {
  obj_common;
  val vbindings;
  val fbindings;
  val up_env;
};

struct bignum {
  obj_common;
  mp_int mp;
};

struct flonum {
  obj_common;
  double n;
};

struct range {
  obj_common;
  val from, to;
};

struct buf {
  obj_common;
  mem_t *data;
  val len;
  val size;
};

struct tnod {
  obj_common;
  val left, right;
  val key;
};

union obj {
  struct any t;
  struct cons c;
  struct cons_hash_entry ch;
  struct string st;
  struct sym s;
  struct package pk;
  struct func f;
  struct vec v;
  struct lazy_cons lc;
  struct lazy_string ls;
  struct cobj co;
  struct env e;
  struct bignum bn;
  struct flonum fl;
  struct range rn;
  struct buf b;
  struct tnod tn;
  struct dyn_args a;
};

#if CONFIG_GEN_GC
typedef struct {
  val *ptr;
  val obj;
} loc;

val gc_set(loc, val);

INLINE loc mkloc_fun(val *ptr, val obj)
{
  loc l = { ptr, obj };
  return l;
}

#define mkloc(expr, obj) mkloc_fun(&(expr), obj)
#define mkcloc(expr) mkloc_fun(&(expr), 0)
#define nulloc mkloc_fun(0, 0)
#define nullocp(lo) (!(lo).ptr)
#define deref(lo) (*(lo).ptr)
#define valptr(lo) ((lo).ptr)
#define set(lo, val) (gc_set(lo, val))
#define setcheck(tgt, src) (gc_assign_check(tgt, src))
#define mut(obj) (gc_mutated(obj));
#define mpush(val, lo) (gc_push(val, lo))
#else
typedef val *loc;
#define mkloc(expr, obj) (&(expr))
#define mkcloc(expr) (&(expr))
#define nulloc ((loc) 0)
#define nullocp(lo) (!(lo))
#define deref(lo) (*(lo))
#define valptr(lo) (lo)
#define set(lo, val) (*(lo) = (val))
#define setcheck(tgt, src) ((void) 0)
#define mut(obj) ((void) (obj))
#define mpush(val, lo) (push(val, lo))
#endif

typedef enum seq_kind {
  SEQ_NIL, SEQ_LISTLIKE, SEQ_VECLIKE, SEQ_HASHLIKE, SEQ_NOTSEQ
} seq_kind_t;

typedef struct seq_info {
  val obj;
  type_t type;
  seq_kind_t kind;
} seq_info_t;

typedef struct seq_iter {
  seq_info_t inf;
  union {
    val iter;
    cnum index;
  } ui;
  cnum len;
  int (*get)(struct seq_iter *, val *pval);
  int (*peek)(struct seq_iter *, val *pval);
} seq_iter_t;

extern const seq_kind_t seq_kind_tab[MAXTYPE+1];

#define SEQ_KIND_PAIR(A, B) ((A) << 3 | (B))

INLINE cnum tag(val obj) { return coerce(cnum, obj) & TAG_MASK; }
INLINE int is_ptr(val obj) { return obj && tag(obj) == TAG_PTR; }
INLINE int is_num(val obj) { return tag(obj) == TAG_NUM; }
INLINE int is_chr(val obj) { return tag(obj) == TAG_CHR; }
INLINE int is_lit(val obj) { return tag(obj) == TAG_LIT; }

INLINE type_t type(val obj)
{
  cnum tg = tag(obj);
  return obj ? tg
               ? convert(type_t, tg)
               : obj->t.type
             : NIL;
}

typedef struct wli wchli_t;

#if LIT_ALIGN < 4
#define wli_noex(lit) (coerce(const wchli_t *,\
                              convert(const wchar_t *,\
                                      "\0" L ## lit L"\0" + 1)))
#define wini(ini) L"\0" L ## ini L"\0"
#define wref(arr) ((arr) + 1)
#else
#define wli_noex(lit) (coerce(const wchli_t *,\
                              convert(const wchar_t *,\
                                      L ## lit)))
#define wini(ini) L ## ini
#define wref(arr) (arr)
#endif
#define wli(lit) wli_noex(lit)

INLINE val auto_str(const wchli_t *str)
{
  return coerce(val, coerce(cnum, str) | TAG_LIT);
}

INLINE val static_str(const wchli_t *str)
{
  return coerce(val, coerce(cnum, str) | TAG_LIT);
}

INLINE wchar_t *litptr(val obj)
{
#if LIT_ALIGN < 4 && SIZEOF_WCHAR_T < 4
 wchar_t *ret = coerce(wchar_t *, (coerce(cnum, obj) & ~TAG_MASK));
 return (*ret == 0) ? ret + 1 : ret;
#elif LIT_ALIGN < 4 && SIZEOF_WCHAR_T == 4
 short *ret = coerce(short *, (coerce(cnum, obj) & ~TAG_MASK));
 return coerce(wchar_t *, (*ret == 0) ? ret + 1 : ret);
#else
 return coerce(wchar_t *, coerce(cnum, obj) & ~TAG_MASK);
#endif
}

INLINE val num_fast(cnum n)
{
  return coerce(val, (n << TAG_SHIFT) | TAG_NUM);
}

INLINE mp_int *mp(val bign)
{
  return &bign->bn.mp;
}

INLINE val chr(wchar_t ch)
{
  return coerce(val, (convert(cnum, ch) << TAG_SHIFT) | TAG_CHR);
}

INLINE cnum c_n(val num)
{
    return coerce(cnum, num) >> TAG_SHIFT;
}
#if LIT_ALIGN < 4
#define lit_noex(strlit) coerce(obj_t *,\
                                coerce(cnum, L"\0" L ## strlit L"\0" + 1) | \
                                TAG_LIT)
#else
#define lit_noex(strlit) coerce(obj_t *, coerce(cnum, L ## strlit) | TAG_LIT)
#endif

#define lit(strlit) lit_noex(strlit)

#define cur_package (get_current_package())
#define cur_package_alist_loc (get_current_package_alist_loc())

extern val packages, system_package, keyword_package, user_package;
extern val public_package, package_alist_s;
extern val package_s, keyword_package_s, system_package_s, user_package_s;
extern val null_s, t, cons_s, str_s, chr_s, fixnum_sl;
extern val sym_s, pkg_s, fun_s, vec_s;
extern val stream_s, hash_s, hash_iter_s, lcons_s, lstr_s, cobj_s, cptr_s;
extern val atom_s, integer_s, number_s, sequence_s, string_s;
extern val env_s, bignum_s, float_s, range_s, rcons_s, buf_s, tnode_s;
extern val var_s, expr_s, regex_s, chset_s, set_s, cset_s, wild_s, oneplus_s;
extern val nongreedy_s;
extern val quote_s, qquote_s, unquote_s, splice_s;
extern val sys_qquote_s, sys_unquote_s, sys_splice_s;
extern val zeroplus_s, optional_s, compl_s, compound_s;
extern val or_s, and_s, quasi_s, quasilist_s;
extern val skip_s, trailer_s, block_s, next_s, freeform_s, fail_s, accept_s;
extern val all_s, some_s, none_s, maybe_s, cases_s, collect_s, until_s, coll_s;
extern val define_s, output_s, single_s, first_s, last_s, empty_s;
extern val repeat_s, rep_s, flatten_s, forget_s;
extern val local_s, merge_s, bind_s, rebind_s, cat_s;
extern val try_s, catch_s, finally_s, throw_s, defex_s, deffilter_s;
extern val eof_s, eol_s, assert_s, name_s;
extern val error_s, type_error_s, internal_error_s, panic_s;
extern val numeric_error_s, range_error_s;
extern val query_error_s, file_error_s, process_error_s, syntax_error_s;
extern val timeout_error_s, system_error_s, alloc_error_s;
extern val path_not_found_s, path_exists_s, path_permission_s;
extern val warning_s, defr_warning_s, restart_s, continue_s;
extern val gensym_counter_s, length_s;
extern val rplaca_s, rplacd_s, seq_iter_s;

#define gensym_counter (deref(lookup_var_l(nil, gensym_counter_s)))

extern val nothrow_k, args_k, colon_k, auto_k, fun_k;

extern val null_string;
extern val null_list; /* (nil) */

extern val identity_f, identity_star_f;
extern val equal_f, eql_f, eq_f, car_f, cdr_f, null_f;
extern val list_f, less_f, greater_f;

extern val prog_string;

#if HAVE_ULONGLONG_T
typedef ulonglong_t alloc_bytes_t;
#define SIZEOF_ALLOC_BYTES_T SIZEOF_LONGLONG_T
#else
typedef unsigned long alloc_bytes_t;
#define SIZEOF_ALLOC_BYTES_T SIZEOF_LONG
#endif

extern alloc_bytes_t malloc_bytes;
extern alloc_bytes_t gc_bytes;

val identity(val obj);
val typeof(val obj);
val subtypep(val sub, val sup);
val typep(val obj, val type);
seq_info_t seq_info(val cobj);
void seq_iter_init(val self, seq_iter_t *it, val obj);
void seq_iter_rewind(seq_iter_t *it);
INLINE int seq_get(seq_iter_t *it, val *pval) { return it->get(it, pval); }
INLINE int seq_peek(seq_iter_t *it, val *pval) { return it->peek(it, pval); }
val seq_geti(seq_iter_t *it);
val seq_getpos(val self, seq_iter_t *it);
void seq_setpos(val self, seq_iter_t *it, val pos);
val seq_begin(val obj);
val seq_next(val iter, val end_val);
val seq_reset(val iter, val obj);
val throw_mismatch(val self, val obj, type_t);
INLINE val type_check(val self, val obj, type_t typecode)
{
  if (type(obj) != typecode)
    throw_mismatch(self, obj, typecode);
  return t;
}
val class_check(val self, val cobj, val class_sym);
val car(val cons);
val cdr(val cons);
INLINE val us_car(val cons) { return cons->c.car; }
INLINE val us_cdr(val cons) { return cons->c.cdr; }
INLINE val *us_car_p(val cons) { return &cons->c.car; }
INLINE val *us_cdr_p(val cons) { return &cons->c.cdr; }
val rplaca(val cons, val new_car);
val rplacd(val cons, val new_car);
val us_rplaca(val cons, val new_car);
val us_rplacd(val cons, val new_cdr);
val sys_rplaca(val cons, val new_car);
val sys_rplacd(val cons, val new_car);
loc car_l(val cons);
loc cdr_l(val cons);
val first(val cons);
val rest(val cons);
val second(val cons);
val third(val cons);
val fourth(val cons);
val fifth(val cons);
val sixth(val cons);
val seventh(val cons);
val eighth(val cons);
val ninth(val cons);
val tenth(val cons);
val conses(val list);
val lazy_conses(val list);
val listref(val list, val ind);
loc tail(val cons);
val lastcons(val list);
val last(val list, val n);
val nthlast(val pos, val list);
val nthcdr(val pos, val list);
val nth(val pos, val list);
val butlastn(val n, val list);
val pop(val *plist);
val upop(val *plist, val *pundo);
val rcyc_pop(val *plist);
val push(val v, val *plist);
val copy_list(val list);
val make_like(val list, val thatobj);
val tolist(val seq);
val nullify(val seq);
val seqp(val obj);
val nreverse(val in);
val reverse(val in);
val us_nreverse(val in);
val append2(val list1, val list2);
val nappend2(val list1, val list2);
val revappend(val list1, val list2);
val nreconc(val list1, val list2);
val appendv(struct args *lists);
val nconcv(struct args *lists);
val sub_list(val list, val from, val to);
val replace_list(val list, val items, val from, val to);
val lazy_appendl(val lists);
val lazy_appendv(struct args *lists);
val ldiff(val list1, val list2);
val ldiff_old(val list1, val list2);
val flatten(val list);
val lazy_flatten(val list);
val flatcar(val list);
val lazy_flatcar(val tree);
val tuples(val n, val seq, val fill);
val partition_by(val func, val seq);
val partition(val seq, val indices);
val split(val seq, val indices);
val partition_star(val seq, val indices);
val split_star(val seq, val indices);
val tailp(val obj, val list);
val memq(val obj, val list);
val rmemq(val obj, val list);
val memql(val obj, val list);
val rmemql(val obj, val list);
val memqual(val obj, val list);
val rmemqual(val obj, val list);
val member(val item, val list, val testfun, val keyfun);
val rmember(val item, val list, val testfun, val keyfun);
val member_if(val pred, val list, val key);
val rmember_if(val pred, val list, val key);
val remq(val obj, val seq, val keyfun);
val remql(val obj, val seq, val keyfun);
val remqual(val obj, val seq, val keyfun);
val remove_if(val pred, val seq, val keyfun);
val keepq(val obj, val seq, val keyfun);
val keepql(val obj, val seq, val keyfun);
val keepqual(val obj, val seq, val keyfun);
val keep_if(val pred, val seq, val keyfun);
val remq_lazy(val obj, val list);
val remql_lazy(val obj, val list);
val remqual_lazy(val obj, val list);
val remove_if_lazy(val pred, val list, val key);
val keep_if_lazy(val pred, val list, val key);
val tree_find(val obj, val tree, val testfun);
val countqual(val obj, val list);
val countql(val obj, val list);
val countq(val obj, val list);
val count_if(val pred, val list, val key);
val some_satisfy(val list, val pred, val key);
val all_satisfy(val list, val pred, val key);
val none_satisfy(val list, val pred, val key);
val multi(val func, struct args *lists);
val eql(val left, val right);
val equal(val left, val right);
val meq(val item, varg args);
val meql(val item, varg args);
val mequal(val item, varg args);
mem_t *chk_malloc(size_t size);
mem_t *chk_malloc_gc_more(size_t size);
mem_t *chk_calloc(size_t n, size_t size);
mem_t *chk_realloc(mem_t *, size_t size);
mem_t *chk_grow_vec(mem_t *old, size_t oldelems, size_t newelems,
                    size_t elsize);
mem_t *chk_manage_vec(mem_t *old, size_t oldfilled, size_t newfilled,
                      size_t elsize, mem_t *fillval);
wchar_t *chk_wmalloc(size_t nwchar);
wchar_t *chk_wrealloc(wchar_t *, size_t nwchar);
wchar_t *chk_strdup(const wchar_t *str);
char *chk_strdup_utf8(const char *str);
unsigned char *chk_strdup_8bit(const wchar_t *str);
mem_t *chk_copy_obj(mem_t *orig, size_t size);
mem_t *chk_xalloc(ucnum m, ucnum n, val self);
val cons(val car, val cdr);
val make_lazy_cons(val func);
val make_lazy_cons_car(val func, val car);
val make_lazy_cons_car_cdr(val func, val car, val cdr);
val make_lazy_cons_pub(val func, val car, val cdr);
val lcons_car(val lcons);
val lcons_cdr(val lcons);
void rcyc_cons(val cons);
void rcyc_list(val list);
void rcyc_empty(void);
val lcons_fun(val lcons);
INLINE val us_lcons_fun(val lcons) { return lcons->lc.func; }
val list(val first, ...); /* terminated by nao */
val listv(struct args *);
val consp(val obj);
val lconsp(val obj);
val atom(val obj);
val listp(val obj);
val endp(val obj);
val proper_list_p(val obj);
val length_list(val list);
val getplist(val list, val key);
val getplist_f(val list, val key, loc found);
val memp(val key, val plist);
val plist_to_alist(val list);
val improper_plist_to_alist(val list, val boolean_keys);
val num(cnum val);
val unum(ucnum u);
val flo(double val);
cnum c_num(val num);
ucnum c_unum(val num);
cnum c_fixnum(val num, val self);
double c_flo(val self, val num);
val fixnump(val num);
val bignump(val num);
val floatp(val num);
val integerp(val num);
val numberp(val num);
val nary_op(val self, val (*bfun)(val, val),
            val (*ufun)(val self, val),
            struct args *args, val emptyval);
val nary_simple_op(val (*bfun)(val, val),
                   struct args *args, val emptyval);
val plus(val anum, val bnum);
val plusv(struct args *);
val minus(val anum, val bnum);
val minusv(val minuend, struct args *nlist);
val neg(val num);
val abso(val num);
val mul(val anum, val bnum);
val mulv(struct args *);
val divv(val dividend, struct args *);
val trunc(val anum, val bnum);
val mod(val anum, val bnum);
val wrap_star(val start, val end, val num);
val wrap(val start, val end, val num);
val divi(val anum, val bnum);
val zerop(val num);
val nzerop(val num);
val plusp(val num);
val minusp(val num);
val evenp(val num);
val oddp(val num);
val succ(val num);
val ssucc(val num);
val sssucc(val num);
val pred(val num);
val ppred(val num);
val pppred(val num);
val gt(val anum, val bnum);
val lt(val anum, val bnum);
val ge(val anum, val bnum);
val le(val anum, val bnum);
val numeq(val anum, val bnum);
val gtv(val first, struct args *rest);
val ltv(val first, struct args *rest);
val gev(val first, struct args *rest);
val lev(val first, struct args *rest);
val numeqv(val first, struct args *rest);
val numneqv(struct args *list);
val sum(val seq, val keyfun);
val prod(val seq, val keyfun);
val max2(val a, val b);
val min2(val a, val b);
val maxv(val first, struct args *rest);
val minv(val first, struct args *rest);
val maxl(val first, val rest);
val minl(val first, val rest);
val clamp(val low, val high, val num);
val bracket(val larg, struct args *args);
val expt(val base, val exp);
val exptv(struct args *nlist);
val exptmod(val base, val exp, val mod);
val sqroot(val anum);
val isqrt(val anum);
val square(val anum);
val gcd(val anum, val bnum);
val gcdv(struct args *nlist);
val lcm(val anum, val bnum);
val lcmv(struct args *nlist);
val floorf(val);
val floordiv(val, val);
val ceili(val);
val ceildiv(val anum, val bnum);
val roundiv(val anum, val bnum);
val trunc_rem(val anum, val bnum);
val floor_rem(val anum, val bnum);
val ceil_rem(val anum, val bnum);
val round_rem(val anum, val bnum);
val sine(val);
val cosi(val);
val tang(val);
val asine(val);
val acosi(val);
val atang(val);
val atang2(val, val);
val sineh(val);
val cosih(val);
val tangh(val);
val asineh(val);
val acosih(val);
val atangh(val);
val loga(val);
val logten(val num);
val logtwo(val num);
val expo(val);
val logand(val, val);
val logior(val, val);
val logandv(struct args *nlist);
val logiorv(struct args *nlist);
val logxor(val, val);
val logxor_old(val, val);
val logtest(val, val);
val lognot(val, val);
val logtrunc(val a, val bits);
val sign_extend(val num, val nbits);
val ash(val a, val bits);
val bit(val a, val bit);
val maskv(struct args *bits);
val logcount(val n);
val bitset(val n);
val string_own(wchar_t *str);
val string(const wchar_t *str);
val string_utf8(const char *str);
val string_8bit(const unsigned char *str);
val string_8bit_size(const unsigned char *str, size_t sz);
val mkstring(val len, val ch);
val mkustring(val len); /* must initialize immediately with init_str! */
val init_str(val str, const wchar_t *);
val copy_str(val str);
val upcase_str(val str);
val downcase_str(val str);
val string_extend(val str, val tail);
val stringp(val str);
val lazy_stringp(val str);
val length_str(val str);
val coded_length(val str);
const wchar_t *c_str(val str);
val search_str(val haystack, val needle, val start_num, val from_end);
val search_str_tree(val haystack, val tree, val start_num, val from_end);
val match_str(val bigstr, val str, val pos);
val match_str_tree(val bigstr, val tree, val pos);
val replace_str(val str_in, val items, val from, val to);
val sub_str(val str_in, val from_num, val to_num);
val cat_str(val list, val sep);
val scat(val sep, ...);
val scat2(val s1, val s2);
val scat3(val s1, val sep, val s2);
val fmt_join(struct args *args);
val split_str(val str, val sep);
val split_str_keep(val str, val sep, val keep_sep);
val spl(val sep, val arg1, val arg2);
val split_str_set(val str, val set);
val tok_str(val str, val tok_regex, val keep_sep);
val tok(val tok_regex, val arg1, val arg2);
val tok_where(val str, val tok_regex);
val list_str(val str);
val trim_str(val str);
val cmp_str(val astr, val bstr);
val str_eq(val astr, val bstr);
val str_lt(val astr, val bstr);
val str_gt(val astr, val bstr);
val str_le(val astr, val bstr);
val str_ge(val astr, val bstr);
val int_str(val str, val base);
val flo_str(val str);
val num_str(val str);
val int_flo(val f);
val flo_int(val i);
val less(val left, val right);
val greater(val left, val right);
val lequal(val left, val right);
val gequal(val left, val right);
val lessv(val first, struct args *rest);
val greaterv(val first, struct args *rest);
val lequalv(val first, struct args *rest);
val gequalv(val first, struct args *rest);
val chrp(val chr);
wchar_t c_chr(val chr);
val chr_isalnum(val ch);
val chr_isalpha(val ch);
val chr_isascii(val ch);
val chr_iscntrl(val ch);
val chr_isdigit(val ch);
val chr_digit(val ch);
val chr_isgraph(val ch);
val chr_islower(val ch);
val chr_isprint(val ch);
val chr_ispunct(val ch);
val chr_isspace(val ch);
val chr_isblank(val ch);
val chr_isunisp(val ch);
val chr_isupper(val ch);
val chr_isxdigit(val ch);
val chr_xdigit(val ch);
val chr_toupper(val ch);
val chr_tolower(val ch);
val int_chr(val ch);
val chr_int(val num);
val chr_str(val str, val index);
val chr_str_set(val str, val index, val chr);
val span_str(val str, val set);
val compl_span_str(val str, val set);
val break_str(val str, val set);
val make_sym(val name);
val gensym(val prefix);
val make_package(val name);
val make_anon_package(void);
val packagep(val obj);
val find_package(val name);
val delete_package(val package);
val merge_delete_package(val to, val victim);
val package_alist(void);
val package_name(val package);
val package_symbols(val package);
val package_local_symbols(val package);
val use_sym(val use_list, val package);
val unuse_sym(val symbol, val package);
val use_package(val use_list, val package);
val unuse_package(val unuse_list, val package);
val symbol_visible(val package, val sym);
val symbol_needs_prefix(val self, val package, val sym);
val find_symbol(val name, val package, val notfound_val);
val find_symbol_fb(val name, val package, val notfound_val);
val intern(val str, val package);
val intern_intrinsic(val str, val package_in);
val unintern(val sym, val package);
val rehome_sym(val sym, val package);
val package_foreign_symbols(val package);
val package_fallback_list(val package);
val set_package_fallback_list(val package, val list);
val intern_fallback(val str, val package);
val intern_fallback_intrinsic(val str, val package_in);
val symbolp(val sym);
val symbol_name(val sym);
val symbol_package(val sym);
val keywordp(val sym);
val get_current_package(void);
loc get_current_package_alist_loc(void);
val func_f0(val, val (*fun)(val env));
val func_f1(val, val (*fun)(val env, val));
val func_f2(val, val (*fun)(val env, val, val));
val func_f3(val, val (*fun)(val env, val, val, val));
val func_f4(val, val (*fun)(val env, val, val, val, val));
val func_n0(val (*fun)(void));
val func_n1(val (*fun)(val));
val func_n2(val (*fun)(val, val));
val func_n3(val (*fun)(val, val, val));
val func_n4(val (*fun)(val, val, val, val));
val func_n5(val (*fun)(val, val, val, val, val));
val func_n6(val (*fun)(val, val, val, val, val, val));
val func_n7(val (*fun)(val, val, val, val, val, val, val));
val func_n8(val (*fun)(val, val, val, val, val, val, val, val));
val func_f0v(val, val (*fun)(val env, varg));
val func_f1v(val, val (*fun)(val env, val, varg));
val func_f2v(val, val (*fun)(val env, val, val, varg));
val func_f3v(val, val (*fun)(val env, val, val, val, varg));
val func_f4v(val, val (*fun)(val env, val, val, val, val, varg));
val func_n0v(val (*fun)(varg));
val func_n1v(val (*fun)(val, varg));
val func_n2v(val (*fun)(val, val, varg));
val func_n3v(val (*fun)(val, val, val, varg));
val func_n4v(val (*fun)(val, val, val, val, varg));
val func_n5v(val (*fun)(val, val, val, val, val, varg));
val func_n6v(val (*fun)(val, val, val, val, val, val, varg));
val func_n7v(val (*fun)(val, val, val, val, val, val, val, varg));
val func_n8v(val (*fun)(val, val, val, val, val, val, val, val, varg));
val func_n1o(val (*fun)(val), int reqargs);
val func_n2o(val (*fun)(val, val), int reqargs);
val func_n3o(val (*fun)(val, val, val), int reqargs);
val func_n4o(val (*fun)(val, val, val, val), int reqargs);
val func_n5o(val (*fun)(val, val, val, val, val), int reqargs);
val func_n6o(val (*fun)(val, val, val, val, val, val), int reqargs);
val func_n7o(val (*fun)(val, val, val, val, val, val, val), int reqargs);
val func_n8o(val (*fun)(val, val, val, val, val, val, val, val), int reqargs);
val func_n1ov(val (*fun)(val, varg), int reqargs);
val func_n2ov(val (*fun)(val, val, varg), int reqargs);
val func_n3ov(val (*fun)(val, val, val, varg), int reqargs);
val func_n4ov(val (*fun)(val, val, val, val, varg), int reqargs);
val func_interp(val env, val form);
val func_vm(val closure, val desc, int fixparam, int reqargs, int variadic);
val copy_fun(val ofun);
val func_get_form(val fun);
val func_get_env(val fun);
val func_set_env(val fun, val env);
val us_func_set_env(val fun, val env);
val functionp(val);
val interp_fun_p(val);
val vm_fun_p(val);
val fun_fixparam_count(val obj);
val fun_optparam_count(val obj);
val fun_variadic(val obj);
val generic_funcall(val fun, struct args *);
val funcall(val fun);
val funcall1(val fun, val arg);
val funcall2(val fun, val arg1, val arg2);
val funcall3(val fun, val arg1, val arg2, val arg3);
val funcall4(val fun, val arg1, val arg2, val arg3, val arg4);
val reduce_left(val fun, val list, val init, val key);
val reduce_right(val fun, val list, val init, val key);
val transposev(struct args *lists);
val transpose(val lists);
/* The notation pa_12_2 means take some function f(arg1, arg2) and
   fix a value for argument 1 to create a g(arg2).
   Other variations follow by analogy. */
val pa_12_2(val fun2, val arg);
val pa_12_1(val fun2, val arg2);
val pa_123_3(val fun3, val arg1, val arg2);
val pa_123_2(val fun3, val arg1, val arg3);
val pa_123_1(val fun3, val arg2, val arg3);
val pa_123_23(val fun3, val arg1);
val pa_1234_1(val fun4, val arg2, val arg3, val arg4);
val pa_1234_34(val fun3, val arg1, val arg2);
val chain(val first_fun, ...);
val chainv(struct args *funlist);
val chandv(struct args *funlist);
val juxtv(struct args *funlist);
val andf(val first_fun, ...);
val andv(struct args *funlist);
val orf(val first_fun, ...);
val orv(struct args *funlist);
val notf(val fun);
val iff(val condfun, val thenfun, val elsefun);
val iffi(val condfun, val thenfun, val elsefun);
val dupl(val fun);
val swap_12_21(val fun);
val vector(val length, val initval);
val vectorp(val vec);
val vec_set_length(val vec, val fill);
val vecref(val vec, val ind);
loc vecref_l(val vec, val ind);
val vec_push(val vec, val item);
val length_vec(val vec);
val size_vec(val vec);
val vectorv(struct args *);
val vec(val first, ...);
val vec_list(val list);
val list_vec(val vector);
val copy_vec(val vec);
val sub_vec(val vec_in, val from, val to);
val replace_vec(val vec_in, val items, val from, val to);
val replace_obj(val obj, val items, val from, val to);
val cat_vec(val list);
val lazy_stream_cons(val stream);
val lazy_str(val list, val term, val limit);
val lazy_str_force_upto(val lstr, val index);
val lazy_str_force(val lstr);
struct strm_base;
val lazy_str_put(val lstr, val stream, struct strm_base *);
val lazy_str_get_trailing_list(val lstr, val index);
val length_str_gt(val str, val len);
val length_str_ge(val str, val len);
val length_str_lt(val str, val len);
val length_str_le(val str, val len);
val cobj(mem_t *handle, val cls_sym, struct cobj_ops *ops);
val cobjp(val obj);
val cobjclassp(val obj, val cls_sym);
mem_t *cobj_handle(val self, val cobj, val cls_sym);
struct cobj_ops *cobj_ops(val self, val cobj, val cls_sym);
val cptr(mem_t *ptr);
val cptr_typed(mem_t *handle, val type_sym, struct cobj_ops *ops);
val cptrp(val obj);
val cptr_type(val cptr);
val cptr_size_hint(val cptr, val size);
val cptr_int(val n, val type_sym);
val cptr_obj(val obj, val type_sym);
val cptr_buf(val buf, val type_sym);
val cptr_zap(val cptr);
val cptr_free(val cptr);
val cptr_cast(val to_type, val cptr);
val int_cptr(val cptr);
mem_t *cptr_get(val cptr);
mem_t *cptr_handle(val cobj, val type_sym, val self);
mem_t **cptr_addr_of(val cptr, val type_sym, val self);
val assoc(val key, val list);
val assql(val key, val list);
val assq(val key, val list);
val rassoc(val key, val list);
val rassql(val key, val list);
val rassq(val key, val list);
val acons(val car, val cdr, val list);
val acons_new(val key, val value, val list);
val acons_new_c(val key, loc new_p, loc list);
val aconsql_new(val key, val value, val list);
val aconsql_new_c(val key, loc new_p, loc list);
val alist_remove(val list, val keys);
val alist_removev(val list, struct args *keys);
val alist_remove1(val list, val key);
val alist_nremove(val list, val keys);
val alist_nremovev(val list, struct args *keys);
val alist_nremove1(val list, val key);
val copy_cons(val cons);
val copy_tree(val tree);
val copy_alist(val list);
val mapcar_listout(val fun, val list);
val mapcar(val fun, val list);
val mapcon(val fun, val list);
val mappend(val fun, val list);
val mapdo(val fun, val list);
val window_map(val range, val boundary, val fun, val seq);
val window_mappend(val range, val boundary, val fun, val seq);
val window_mapdo(val range, val boundary, val fun, val seq);
val interpose(val sep, val seq);
val merge(val list1, val list2, val lessfun, val keyfun);
val nsort(val seq, val lessfun, val keyfun);
val sort(val seq, val lessfun, val keyfun);
val nshuffle(val seq);
val shuffle(val seq);
val multi_sort(val lists, val funcs, val key_funcs);
val sort_group(val seq, val keyfun, val lessfun);
val unique(val seq, val keyfun, struct args *hashv_args);
val uniq(val seq);
val grade(val seq, val lessfun, val keyfun_in);
val find(val list, val key, val testfun, val keyfun);
val rfind(val list, val key, val testfun, val keyfun);
val find_if(val pred, val list, val key);
val rfind_if(val pred, val list, val key);
val find_max(val seq, val testfun, val keyfun);
val find_min(val seq, val testfun, val keyfun);
val posqual(val obj, val list);
val rposqual(val obj, val list);
val posql(val obj, val list);
val rposql(val obj, val list);
val posq(val obj, val list);
val rposq(val obj, val list);
val pos(val list, val key, val testfun, val keyfun);
val rpos(val list, val key, val testfun, val keyfun);
val pos_if(val pred, val list, val key);
val rpos_if(val pred, val list, val key);
val pos_max(val seq, val testfun, val keyfun);
val pos_min(val seq, val testfun, val keyfun);
val mismatch(val left, val right, val testfun, val keyfun);
val rmismatch(val left, val right, val testfun, val keyfun);
val starts_with(val little, val big, val testfun, val keyfun);
val ends_with(val little, val big, val testfun, val keyfun);
val take(val count, val seq);
val take_while(val pred, val seq, val keyfun);
val take_until(val pred, val seq, val keyfun);
val drop(val count, val seq);
val drop_while(val pred, val seq, val keyfun);
val drop_until(val pred, val seq, val keyfun);
val in(val seq, val key, val testfun, val keyfun);
val set_diff(val list1, val list2, val testfun, val keyfun);
val diff(val seq1, val seq2, val testfun, val keyfun);
val symdiff(val seq1, val seq2, val testfun, val keyfun);
val isec(val list1, val list2, val testfun, val keyfun);
val uni(val list1, val list2, val testfun, val keyfun);
val copy(val seq);
val length(val seq);
val empty(val seq);
val sub(val seq, val from, val to);
val ref(val seq, val ind);
val refset(val seq, val ind, val newval);
val dwim_set(val place_p, val seq, varg);
val dwim_del(val place_p, val seq, val ind_range);
val butlast(val seq, val idx);
val replace(val seq, val items, val from, val to);
val update(val seq, val fun);
val search(val seq, val key, val from, val to);
val contains(val key, val seq, val testfun, val keyfun);
val rsearch(val seq, val key, val from, val to);
val where(val func, val seq);
val sel(val seq, val where);
val relate(val domain_seq, val range_seq, val dfl_val);
val rcons(val from, val to);
val rangep(val obj);
val from(val range);
val to(val range);
val set_from(val range, val from);
val set_to(val range, val to);
val in_range(val range, val num);
val in_range_star(val range, val num);
val env(void);
void out_str_char(wchar_t ch, val out, int *semi_flag, int regex);
val obj_print_impl(val obj, val out, val pretty, struct strm_ctx *);
val obj_print(val obj, val stream, val pretty);
val print(val obj, val stream, val pretty);
val pprint(val obj, val stream);
val tostring(val obj);
val tostringp(val obj);
val display_width(val obj);
#if !HAVE_SETENV
void setenv(const char *name, const char *value, int overwrite);
void unsetenv(const char *name);
#endif
val time_sec(void);
val time_sec_usec(void);
val time_string_local(val time, val format);
val time_string_utc(val time, val format);
val time_fields_local(val time);
val time_fields_utc(val time);
val time_struct_local(val time);
val time_struct_utc(val time);
val make_time(val year, val month, val day,
              val hour, val minute, val second,
              val isdst);
val make_time_utc(val year, val month, val day,
                  val hour, val minute, val second,
                  val isdst);
#if HAVE_STRPTIME
val time_parse(val format, val string);
val time_parse_local(val format, val string);
val time_parse_utc(val format, val string);
#endif

void init(val *stack_bottom);
int compat_fixup(int compat_ver);
void dump(val obj, val stream);
void d(val obj);
void breakpt(void);

#define nil convert(obj_t *, 0)

INLINE val eq(val a, val b) { return a == b ? t : nil; }
INLINE val neq(val a, val b) { return a != b ? t : nil; }
INLINE val neql(val left, val right) { return eql(left, right) ? nil : t; }
INLINE val nequal(val left, val right) { return equal(left, right) ? nil : t; }
INLINE val null(val v) { return v ? nil : t; }

#define nilp(o) ((o) == nil)

#define nao coerce(obj_t *, 1 << TAG_SHIFT) /* "not an object" sentinel value. */

#define missingp(v) ((v) == colon_k)

INLINE int null_or_missing_p(val v) { return (nilp(v) || missingp(v)); }

#define if2(a, b) ((a) ? (b) : nil)

#define if3(a, b, c) ((a) ? (b) : (c))

#define uses_or2 val or2_temp

#define or2(a, b) ((or2_temp = (a)) ? or2_temp : (b))

#define or3(a, b, c) or2(a, or2(b, c))

#define or4(a, b, c, d) or2(a, or3(b, c, d))

#define and2(a, b) ((a) ? (b) : nil)

#define and3(a, b, c) ((a) && (b) ? (c) : nil)

#define tnil(c_cond) ((c_cond) ? t : nil)

#define default_arg(arg, dfl) if3(null_or_missing_p(arg), dfl, arg)

INLINE val default_null_arg(val arg)
{
  return if3(missingp(arg), nil, arg);
}

INLINE val default_arg_strict(val arg, val dfl)
{
  return if3(missingp(arg), dfl, arg);
}

#define list_collect_decl(OUT, PTAIL)           \
  val OUT = nil;                                \
  loc PTAIL = mkcloc(OUT)

loc list_collect(loc ptail, val obj);
loc list_collect_nconc(loc ptail, val obj);
loc list_collect_append(loc ptail, val obj);
loc list_collect_nreconc(loc ptail, val obj);
loc list_collect_revappend(loc ptail, val obj);

#define cons_bind(CAR, CDR, CONS)               \
  obj_t *c_o_n_s ## CAR ## CDR = CONS;          \
  obj_t *CAR = car(c_o_n_s ## CAR ## CDR);      \
  obj_t *CDR = cdr(c_o_n_s ## CAR ## CDR)
#define cons_set(CAR, CDR, CONS)                \
  do {                                          \
     obj_t *c_o_n_s ## CAR ## CDR = CONS;       \
     CAR = car(c_o_n_s ## CAR ## CDR);          \
     CDR = cdr(c_o_n_s ## CAR ## CDR);          \
  } while (0)

#define range_bind(FROM, TO, RANGE)             \
  obj_t *r_n_g ## FROM ## TO = RANGE;           \
  obj_t *FROM = from(r_n_g ## FROM ## TO);      \
  obj_t *TO = ((r_n_g ## FROM ## TO)->rn.to)

#define us_cons_bind(CAR, CDR, CONS)            \
  obj_t *c_o_n_s ## CAR ## CDR = CONS;          \
  obj_t *CAR = us_car(c_o_n_s ## CAR ## CDR);   \
  obj_t *CDR = us_cdr(c_o_n_s ## CAR ## CDR)

#define zero num_fast(0)
#define one num_fast(1)
#define two num_fast(2)
#define three num_fast(3)
#define four num_fast(4)
#define negone num_fast(-1)

#ifdef __cplusplus
#define static_forward(decl) namespace { extern decl; }
#define static_def(def) namespace { def; }
#else
#define static_forward(decl) static decl
#define static_def(def) static def
#endif

#ifdef __cplusplus
#define all_zero_init { }
#else
#define all_zero_init { 0 }
#endif
