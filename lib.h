/* Copyright 2011
 * Kaz Kylheku <kaz@kylheku.com>
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

typedef int_ptr_t cnum;

#define TAG_SHIFT 2
#define TAG_MASK (((cnum) 1 << TAG_SHIFT) - 1)
#define TAG_PTR 0
#define TAG_NUM 1
#define TAG_CHR 2
#define TAG_LIT 3
#define NUM_MAX (INT_PTR_MAX/4)
#define NUM_MIN (INT_PTR_MIN/4)

typedef enum type {
  NUM = TAG_NUM, CHR = TAG_CHR, LIT = TAG_LIT, CONS,
  STR, SYM, PKG, FUN, VEC, LCONS, LSTR, COBJ, ENV
} type_t;

typedef enum functype
{
   FINTERP,             /* Interpreted function. */
   F0, F1, F2, F3, F4,  /* Intrinsic functions with env. */
   N0, N1, N2, N3, N4   /* No-env intrinsics. */
} functype_t;

typedef union obj obj_t;

typedef obj_t *val;

typedef unsigned char mem_t;

struct any {
  type_t type;
  void *dummy[2];
  val next; /* GC free list */
};

struct cons {
  type_t type;
  val car, cdr;
};

struct string {
  type_t type;
  wchar_t *str;
  val len;
  val alloc;
};

struct sym {
  type_t type;
  val name;
  val package;
  val value;
};

struct package {
  type_t type;
  val name;
  val symhash;
};

struct func {
  type_t type;
  unsigned minparam : 15;
  unsigned variadic : 1;
  functype_t functype : 16;
  val env;
  union {
    val interp_fun;
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
    val (*f0v)(val, val);
    val (*f1v)(val, val, val);
    val (*f2v)(val, val, val, val);
    val (*f3v)(val, val, val, val, val);
    val (*f4v)(val, val, val, val, val, val);
    val (*n0v)(val);
    val (*n1v)(val, val);
    val (*n2v)(val, val, val);
    val (*n3v)(val, val, val, val);
    val (*n4v)(val, val, val, val, val);
  } f;
};

enum vecindex { vec_alloc = -2, vec_fill = -1 };

struct vec {
  type_t type;
  /* vec points two elements down */
  /* vec[-2] is allocated size */
  /* vec[-1] is fill pointer */
  val *vec;
#ifdef HAVE_VALGRIND
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
  type_t type;
  val car, cdr;
  val func; /* when nil, car and cdr are valid */
};

/*
 * Lazy string: virtual string which dynamically grows as a catentation
 * of a list of strings.
 */
struct lazy_string {
  type_t type;
  val prefix;           /* actual string part */
  val list;             /* remaining list */
  val opts;             /* ( separator . limit ) */
};

struct cobj {
  type_t type;
  mem_t *handle;
  struct cobj_ops *ops;
  val cls;
};

struct cobj_ops {
  val (*equal)(val self, val other);
  void (*print)(val self, val stream);
  void (*destroy)(val self);
  void (*mark)(val self);
  cnum (*hash)(val self);
};

/* Default operations for above structure. */
val cobj_equal_op(val, val);
void cobj_print_op(val, val);
void cobj_destroy_stub_op(val);
void cobj_destroy_free_op(val);
void cobj_mark_op(val);
cnum cobj_hash_op(val);

struct env {
  type_t type;
  val vbindings;
  val fbindings;
  val up_env;
};

union obj {
  struct any t;
  struct cons c;
  struct string st;
  struct sym s;
  struct package pk;
  struct func f;
  struct vec v;
  struct lazy_cons lc;
  struct lazy_string ls;
  struct cobj co;
  struct env e;
};

INLINE cnum tag(val obj) { return ((cnum) obj) & TAG_MASK; }
INLINE int is_ptr(val obj) { return obj && tag(obj) == TAG_PTR; }
INLINE int is_num(val obj) { return tag(obj) == TAG_NUM; }
INLINE int is_chr(val obj) { return tag(obj) == TAG_CHR; }
INLINE int is_lit(val obj) { return tag(obj) == TAG_LIT; }

INLINE type_t type(val obj)
{
  return tag(obj) ? (type_t) tag(obj) : obj->t.type;
}

typedef struct wli wchli_t;

#if LIT_ALIGN < 4
#define wli(lit) ((const wchli_t *) (L"\0" L ## lit L"\0" + 1))
#define wini(ini) L"\0" L ## ini L"\0"
#define wref(arr) ((arr) + 1)
#else
#define wli(lit) ((const wchli_t *) L ## lit)
#define wini(ini) L ## ini
#define wref(arr) (arr)
#endif

INLINE val auto_str(const wchli_t *str)
{
  return (val) (((cnum) str) | TAG_LIT);
}

INLINE val static_str(const wchli_t *str)
{
  return (val) (((cnum) str) | TAG_LIT);
}

INLINE wchar_t *litptr(val obj)
{
#if LIT_ALIGN < 4
 wchar_t *ret = (wchar_t *) ((cnum) obj & ~TAG_MASK);
 return (*ret == 0) ? ret + 1 : ret;
#else
 return (wchar_t *) ((cnum) obj & ~TAG_MASK);
#endif
}

INLINE val num_fast(cnum n)
{
  return (val) ((n << TAG_SHIFT) | TAG_NUM);
}

INLINE val chr(wchar_t ch)
{
  return (val) (((cnum) ch << TAG_SHIFT) | TAG_CHR);
}

#if LIT_ALIGN < 4
#define lit_noex(strlit) ((obj_t *) ((cnum) (L"\0" L ## strlit L"\0" + 1) | TAG_LIT))
#else
#define lit_noex(strlit) ((obj_t *) ((cnum) (L ## strlit) | TAG_LIT))
#endif

#define lit(strlit) lit_noex(strlit)

extern val keyword_package, system_package, user_package;
extern val null, t, cons_s, str_s, chr_s, num_s, sym_s, pkg_s, fun_s, vec_s;
extern val stream_s, hash_s, hash_iter_s, lcons_s, lstr_s, cobj_s, cptr_s;
extern val env_s;
extern val var_s, expr_s, regex_s, chset_s, set_s, cset_s, wild_s, oneplus_s;
extern val nongreedy_s, compiled_regex_s;
extern val quote_s, qquote_s, unquote_s, splice_s;
extern val zeroplus_s, optional_s, compl_s, compound_s, or_s, and_s, quasi_s;
extern val skip_s, trailer_s, block_s, next_s, freeform_s, fail_s, accept_s;
extern val all_s, some_s, none_s, maybe_s, cases_s, collect_s, until_s, coll_s;
extern val define_s, output_s, single_s, first_s, last_s, empty_s;
extern val repeat_s, rep_s, flatten_s, forget_s;
extern val local_s, merge_s, bind_s, cat_s;
extern val try_s, catch_s, finally_s, throw_s, defex_s, deffilter_s;
extern val eof_s, eol_s;
extern val error_s, type_error_s, internal_error_s;
extern val numeric_error_s, range_error_s;
extern val query_error_s, file_error_s, process_error_s;

extern val nothrow_k, args_k;

extern val null_string;
extern val null_list; /* (nil) */

extern val identity_f, equal_f, eq_f, car_f, cdr_f;

extern const wchar_t *progname;
extern val prog_string;

extern mem_t *(*oom_realloc)(mem_t *, size_t);

val identity(val obj);
val typeof(val obj);
val type_check(val obj, int);
val type_check2(val obj, int, int);
val type_check3(val obj, int, int, int);
val class_check(val cobj, val class_sym);
val car(val cons);
val cdr(val cons);
val rplaca(val cons, val new_car);
val rplacd(val cons, val new_car);
val *car_l(val cons);
val *cdr_l(val cons);
val first(val cons);
val rest(val cons);
val second(val cons);
val third(val cons);
val fourth(val cons);
val fifth(val cons);
val sixth(val cons);
val *tail(val cons);
val pop(val *plist);
val push(val v, val *plist);
val copy_list(val list);
val nreverse(val in);
val reverse(val in);
val append2(val list1, val list2);
val nappend2(val list1, val list2);
val appendv(val lists);
val ldiff(val list1, val list2);
val flatten(val list);
val memq(val obj, val list);
val memqual(val obj, val list);
val tree_find(val obj, val tree, val testfun);
val some_satisfy(val list, val pred, val key);
val all_satisfy(val list, val pred, val key);
val none_satisfy(val list, val pred, val key);
val eql(val left, val right);
val equal(val left, val right);
mem_t *chk_malloc(size_t size);
mem_t *chk_realloc(mem_t *, size_t size);
wchar_t *chk_strdup(const wchar_t *str);
val cons(val car, val cdr);
val make_lazy_cons(val func);
val lcons_fun(val lcons);
val list(val first, ...); /* terminated by nao */
val consp(val obj);
val nullp(val obj);
val atom(val obj);
val listp(val obj);
val proper_listp(val obj);
val length(val list);
val getplist(val list, val key);
val getplist_f(val list, val key, val *found);
val proper_plist_to_alist(val list);
val improper_plist_to_alist(val list, val boolean_keys);
val num(cnum val);
cnum c_num(val num);
val nump(val num);
val plus(val anum, val bnum);
val plusv(val nlist);
val minus(val anum, val bnum);
val minusv(val minuend, val nlist);
val neg(val num);
val mul(val anum, val bnum);
val mulv(val nlist);
val trunc(val anum, val bnum);
val mod(val anum, val bnum);
val zerop(val num);
val gt(val anum, val bnum);
val lt(val anum, val bnum);
val ge(val anum, val bnum);
val le(val anum, val bnum);
val gtv(val first, val rest);
val ltv(val first, val rest);
val gev(val first, val rest);
val lev(val first, val rest);
val numeq(val anum, val bnum);
val max2(val anum, val bnum);
val min2(val anum, val bnum);
val maxv(val first, val rest);
val minv(val first, val rest);
val string_own(wchar_t *str);
val string(const wchar_t *str);
val string_utf8(const char *str);
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
const wchar_t *c_str(val str);
val search_str(val haystack, val needle, val start_num, val from_end);
val search_str_tree(val haystack, val tree, val start_num, val from_end);
val sub_str(val str_in, val from_num, val to_num);
val cat_str(val list, val sep);
val split_str(val str, val sep);
val split_str_set(val str, val set);
val trim_str(val str);
val string_lt(val astr, val bstr);
val int_str(val str, val base);
val chrp(val chr);
wchar_t c_chr(val chr);
val chr_str(val str, val index);
val chr_str_set(val str, val index, val chr);
val span_str(val str, val set);
val compl_span_str(val str, val set);
val break_str(val str, val set);
val make_sym(val name);
val make_package(val name);
val find_package(val name);
val intern(val str, val package);
val symbolp(val sym);
val symbol_name(val sym);
val symbol_package(val sym);
val keywordp(val sym);
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
val func_f0v(val, val (*fun)(val env, val rest));
val func_f1v(val, val (*fun)(val env, val, val rest));
val func_f2v(val, val (*fun)(val env, val, val, val rest));
val func_f3v(val, val (*fun)(val env, val, val, val, val rest));
val func_f4v(val, val (*fun)(val env, val, val, val, val, val rest));
val func_n0v(val (*fun)(val rest));
val func_n1v(val (*fun)(val, val rest));
val func_n2v(val (*fun)(val, val, val rest));
val func_n3v(val (*fun)(val, val, val, val rest));
val func_n4v(val (*fun)(val, val, val, val, val rest));
val func_interp(val env, val form);
val functionp(val);
val funcall(val fun);
val funcall1(val fun, val arg);
val funcall2(val fun, val arg1, val arg2);
val funcall3(val fun, val arg1, val arg2, val arg3);
val funcall4(val fun, val arg1, val arg2, val arg3, val arg4);
val reduce_left(val fun, val list, val init, val key);
/* The notation curry_12_2 means take some function f(arg1, arg2) and
   fix a value for argument 1 to create a g(arg2).
   Other variations follow by analogy. */
val curry_12_2(val fun2, val arg);
val curry_12_1(val fun2, val arg2);
val curry_123_3(val fun3, val arg1, val arg2);
val curry_123_2(val fun3, val arg1, val arg3);
val curry_123_1(val fun3, val arg2, val arg3);
val curry_123_23(val fun3, val arg1);
val curry_1234_34(val fun3, val arg1, val arg2);
val chain(val first_fun, ...);
val andf(val first_fun, ...);
val orf(val first_fun, ...);
val swap_12_21(val fun);
val vector(val alloc);
val vec_get_fill(val vec);
val vec_set_fill(val vec, val fill);
val vecref(val vec, val ind);
val *vecref_l(val vec, val ind);
val vec_push(val vec, val item);
val lazy_stream_cons(val stream);
val lazy_str(val list, val term, val limit);
val lazy_str_force_upto(val lstr, val index);
val lazy_str_force(val lstr);
val lazy_str_get_trailing_list(val lstr, val index);
val length_str_gt(val str, val len);
val length_str_ge(val str, val len);
val length_str_lt(val str, val len);
val length_str_le(val str, val len);
val cobj(mem_t *handle, val cls_sym, struct cobj_ops *ops);
val cobjp(val obj);
mem_t *cobj_handle(val cobj, val cls_sym);
val cptr(mem_t *ptr);
mem_t *cptr_get(val cptr);
val assoc(val list, val key);
val assq(val list, val key);
val acons(val car, val cdr, val list);
val acons_new(val key, val value, val list);
val *acons_new_l(val key, val *new_p, val *list);
val aconsq_new(val key, val value, val list);
val *aconsq_new_l(val key, val *new_p, val *list);
val alist_remove(val list, val keys);
val alist_remove1(val list, val key);
val alist_nremove(val list, val keys);
val alist_nremove1(val list, val key);
val copy_cons(val cons);
val copy_alist(val list);
val mapcar(val fun, val list);
val mapcon(val fun, val list);
val mappend(val fun, val list);
val merge(val list1, val list2, val lessfun, val keyfun);
val sort(val list, val lessfun, val keyfun);
val find(val list, val key, val testfun, val keyfun);
val set_diff(val list1, val list2, val testfun, val keyfun);
val env(void);

val obj_print(val obj, val stream);
val obj_pprint(val obj, val stream);
void init(const wchar_t *progname, mem_t *(*oom_realloc)(mem_t *, size_t),
          val *stack_bottom);
void dump(val obj, val stream);
void d(val obj);
void breakpt(void);

#define nil ((obj_t *) 0)

INLINE val eq(val a, val b) { return ((a) == (b) ? t : nil); }

#define nao ((obj_t *) (1 << TAG_SHIFT)) /* "not an object" sentinel value. */

#define if2(a, b) ((a) ? (b) : nil)

#define if3(a, b, c) ((a) ? (b) : (c))

#define uses_or2 val or2_temp

#define or2(a, b) ((or2_temp = (a)) ? or2_temp : (b))

#define or3(a, b, c) or2(a, or2(b, c))

#define or4(a, b, c, d) or2(a, or3(b, c, d))

#define list_collect_decl(OUT, PTAIL)           \
  obj_t *OUT = nil, **PTAIL = &OUT

#define list_collect(PTAIL, OBJ)                \
  do {                                          \
    *PTAIL = cons(OBJ, nil);                    \
    PTAIL = cdr_l(*PTAIL);                      \
  } while(0)

#define list_collect_nconc(PTAIL, OBJ)          \
  do {                                          \
    obj_t *o_b_j = (OBJ);                       \
    *PTAIL = o_b_j;                             \
    if (o_b_j)                                  \
      PTAIL = tail(o_b_j);                      \
  } while (0)

#define list_collect_append(PTAIL, OBJ)         \
  do {                                          \
    obj_t *o_b_j = copy_list(OBJ);              \
    *PTAIL = o_b_j;                             \
    if (o_b_j)                                  \
      PTAIL = tail(o_b_j);                      \
  } while (0)

#define list_collect_terminate(PTAIL, OBJ)      \
  do *PTAIL = (OBJ); while(0)

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


#define zero num_fast(0)
#define one num_fast(1)
#define two num_fast(2)
#define negone num_fast(-1)
#define maxint num_fast(NUM_MAX)
#define minint num_fast(NUM_MIN)
