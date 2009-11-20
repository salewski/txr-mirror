/* Copyright 2009
 * Kaz Kylheku <kkylheku@gmail.com>
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

#define TAG_SHIFT 2
#define TAG_MASK ((1L << TAG_SHIFT) - 1)
#define TAG_PTR 0
#define TAG_NUM 1
#define TAG_CHR 2
#define TAG_LIT 3
#define NUM_MAX (LONG_MAX/4)
#define NUM_MIN (LONG_MIN/4)

typedef enum type {
  NUM = TAG_NUM, CHR = TAG_CHR, LIT = TAG_LIT, CONS,
  STR, SYM, FUN, VEC, LCONS, LSTR, COBJ
} type_t;

typedef enum functype
{
   FINTERP,             /* Interpreted function. */
   F0, F1, F2, F3, F4,  /* Intrinsic functions with env. */
   N0, N1, N2, N3, N4   /* No-env intrinsics. */
} functype_t;

typedef union obj obj_t;

typedef obj_t *val;

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
};

struct sym {
  type_t type;
  val name;
  val val;
};

struct func {
  type_t type;
  functype_t functype;
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
  } f;
};

enum vecindex { vec_alloc = -2, vec_fill = -1 };

struct vec {
  type_t type;
  /* vec points two elements down */
  /* vec[-2] is allocated size */
  /* vec[-1] is fill pointer */
  val *vec;
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
  val opts;             /* ( limit . sepstring ) */
};

struct cobj {
  type_t type;
  void *handle;
  struct cobj_ops *ops;
  val cls;
};

struct cobj_ops {
  val (*equal)(val self, val other);
  void (*print)(val self, val stream);
  void (*destroy)(val self);
  void (*mark)(val self);
  long (*hash)(val self);
};

union obj {
  struct any t;
  struct cons c;
  struct string st;
  struct sym s;
  struct func f;
  struct vec v;
  struct lazy_cons lc;
  struct lazy_string ls;
  struct cobj co;
};

inline long tag(val obj) { return ((long) obj) & TAG_MASK; }
inline int is_ptr(val obj) { return obj && tag(obj) == TAG_PTR; }
inline int is_num(val obj) { return tag(obj) == TAG_NUM; }
inline int is_chr(val obj) { return tag(obj) == TAG_CHR; }
inline int is_lit(val obj) { return tag(obj) == TAG_LIT; }

inline type_t type(val obj)
{ 
 return tag(obj) ? (type_t) tag(obj) : obj->t.type;
}

inline val auto_str(const wchar_t *str)
{
  return (val) ((long) (str) | TAG_LIT);
}

inline val static_str(const wchar_t *str)
{
  return (val) ((long) (str) | TAG_LIT);
}

inline wchar_t *litptr(val obj)
{
 return (wchar_t *) ((long) obj & ~TAG_MASK);
}

#define lit_noex(strlit) ((obj_t *) ((long) (L ## strlit) | TAG_LIT))
#define lit(strlit) lit_noex(strlit)

extern val interned_syms;

extern val t, cons_t, str_t, chr_t, num_t, sym_t, fun_t, vec_t;
extern val stream_t, hash_t, lcons_t, lstr_t, cobj_t;
extern val var, regex, set, cset, wild, oneplus;
extern val zeroplus, optional, compound, or, quasi;
extern val skip, trailer, block, next, freeform, fail, accept;
extern val all, some, none, maybe, cases, collect, until, coll;
extern val define, output, single, frst, lst, empty, repeat, rep;
extern val flattn, forget, local, mrge, bind, cat, args;
extern val try, catch, finally, nothrow, throw, defex;
extern val error, type_error, internal_err, numeric_err, range_err;
extern val query_error, file_error, process_error;

extern val zero, one, two, negone, maxint, minint;
extern val null_string;
extern val null_list; /* (nil) */

extern val identity_f;
extern val equal_f;

extern const wchar_t *progname;
extern val prog_string;

extern void *(*oom_realloc)(void *, size_t);

val identity(val obj);
val typeof(val obj);
val type_check(val obj, int);
val type_check2(val obj, int, int);
val type_check3(val obj, int, int, int);
val car(val cons);
val cdr(val cons);
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
val flatten(val list);
val memq(val obj, val list);
val tree_find(val obj, val tree);
val some_satisfy(val list, val pred, val key);
val all_satisfy(val list, val pred, val key);
val none_satisfy(val list, val pred, val key);
long c_num(val num);
val nump(val num);
val equal(val left, val right);
unsigned char *chk_malloc(size_t size);
unsigned char *chk_realloc(void *, size_t size);
wchar_t *chk_strdup(const wchar_t *str);
val cons(val car, val cdr);
val list(val first, ...); /* terminated by nao */
val consp(val obj);
val nullp(val obj);
val atom(val obj);
val listp(val obj);
val proper_listp(val obj);
val length(val list);
val num(long val);
long c_num(val num);
val plus(val anum, val bnum);
val minus(val anum, val bnum);
val neg(val num);
val zerop(val num);
val gt(val anum, val bnum);
val lt(val anum, val bnum);
val ge(val anum, val bnum);
val le(val anum, val bnum);
val numeq(val anum, val bnum);
val max2(val anum, val bnum);
val min2(val anum, val bnum);
val string_own(wchar_t *str);
val string(const wchar_t *str);
val string_utf8(const char *str);
val mkstring(val len, val ch);
val mkustring(val len); /* must initialize immediately with init_str! */
val init_str(val str, const wchar_t *);
val copy_str(val str);
val stringp(val str);
val lazy_stringp(val str);
val length_str(val str);
const wchar_t *c_str(val str);
val search_str(val haystack, val needle, val start_num, val from_end);
val search_str_tree(val haystack, val tree, val start_num, val from_end);
val sub_str(val str_in, val from_num, val to_num);
val cat_str(val list, val sep);
val split_str(val str, val sep);
val trim_str(val str);
val string_lt(val astr, val bstr);
val chr(wchar_t ch);
val chrp(val chr);
wchar_t c_chr(val chr);
val chr_str(val str, val index);
val chr_str_set(val str, val index, val chr);
val sym_name(val sym);
val make_sym(val name);
val intern(val str);
val symbolp(val sym);
val symbol_name(val sym);
val func_f0(val, val (*fun)(val));
val func_f1(val, val (*fun)(val, val));
val func_f2(val, val (*fun)(val, val, val));
val func_f3(val, val (*fun)(val, val, val, val));
val func_f4(val, val (*fun)(val, val, val, val, val));
val func_n0(val (*fun)(void));
val func_n1(val (*fun)(val));
val func_n2(val (*fun)(val, val));
val func_n3(val (*fun)(val, val, val));
val func_n4(val (*fun)(val, val, val, val));
val apply(val fun, val arglist);
val funcall(val fun);
val funcall1(val fun, val arg);
val funcall2(val fun, val arg1, val arg2);
val reduce_left(val fun, val list, val init, val key);
val bind2(val fun2, val arg);
val bind2other(val fun2, val arg2);
val chain(val fun1_list);
val vector(val alloc);
val vec_get_fill(val vec);
val vec_set_fill(val vec, val fill);
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
val cobj(void *handle, val cls_sym, struct cobj_ops *ops);
void cobj_print_op(val, val); /* Default function for struct cobj_ops */
val assoc(val list, val key);
val acons_new(val list, val key, val value);
val *acons_new_l(val *list, val key);
val alist_remove(val list, val keys);
val alist_remove1(val list, val key);
val copy_cons(val cons);
val copy_alist(val list);
val mapcar(val fun, val list);
val mappend(val fun, val list);
val sort(val list, val lessfun, val keyfun);

void obj_print(val obj, val stream);
void obj_pprint(val obj, val stream);
void init(const wchar_t *progname, void *(*oom_realloc)(void *, size_t),
          val *stack_bottom);
void dump(val obj, val stream);
val match(val spec, val data);

#define nil ((obj_t *) 0)

#define nao ((obj_t *) (-1 << TAG_SHIFT)) /* "not an object" sentinel value. */

#define eq(a, b) ((a) == (b) ? t : nil)

#define if2(a, b) ((a) ? (b) : nil)

#define if3(a, b, c) ((a) ? (b) : (c))

#define or2(a, b) ((a) ? (a) : (b))

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
