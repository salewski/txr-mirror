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

#define tag(obj) (((long) (obj)) & TAG_MASK)
#define is_ptr(obj) ((obj) && (tag(obj) == TAG_PTR))
#define is_num(obj) (tag(obj) == TAG_NUM)
#define is_chr(obj) (tag(obj) == TAG_CHR)
#define is_lit(obj) (tag(obj) == TAG_LIT)
#define type(obj) (tag(obj) ? ((type_t) tag(obj)) : (obj)->t.type)
#define lit_noex(strlit) ((obj_t *) ((long) (L ## strlit) | TAG_LIT))
#define lit(strlit) lit_noex(strlit)
#define auto_str(str) ((obj_t *) ((long) (str) | TAG_LIT))
#define static_str(str) ((obj_t *) ((long) (str) | TAG_LIT))
#define litptr(obj) ((wchar_t *) ((long) obj & ~TAG_MASK))

typedef union obj obj_t;

struct any {
  type_t type;
  void *dummy[2];
  obj_t *next; /* GC free list */
};

struct cons {
  type_t type;
  obj_t *car, *cdr;
};

struct string {
  type_t type;
  wchar_t *str;
  obj_t *len;
};

struct sym {
  type_t type;
  obj_t *name;
  obj_t *val;
};

struct func {
  type_t type;
  functype_t functype;
  obj_t *env;
  union {
    obj_t *interp_fun;
    obj_t *(*f0)(obj_t *);
    obj_t *(*f1)(obj_t *, obj_t *);
    obj_t *(*f2)(obj_t *, obj_t *, obj_t *);
    obj_t *(*f3)(obj_t *, obj_t *, obj_t *, obj_t *);
    obj_t *(*f4)(obj_t *, obj_t *, obj_t *, obj_t *, obj_t *);
    obj_t *(*n0)(void);
    obj_t *(*n1)(obj_t *);
    obj_t *(*n2)(obj_t *, obj_t *);
    obj_t *(*n3)(obj_t *, obj_t *, obj_t *);
    obj_t *(*n4)(obj_t *, obj_t *, obj_t *, obj_t *);
  } f;
};

enum vecindex { vec_alloc = -2, vec_fill = -1 };

struct vec {
  type_t type;
  /* vec points two elements down */
  /* vec[-2] is allocated size */
  /* vec[-1] is fill pointer */
  obj_t **vec;
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
  obj_t *car, *cdr;
  obj_t *func; /* when nil, car and cdr are valid */
};

/*
 * Lazy string: virtual string which dynamically grows as a catentation
 * of a list of strings.
 */
struct lazy_string {
  type_t type;
  obj_t *prefix;        /* actual string part */
  obj_t *list;          /* remaining list */
  obj_t *opts;          /* ( limit . sepstring ) */
};

struct cobj {
  type_t type;
  void *handle;
  struct cobj_ops *ops;
  obj_t *cls;
};

struct cobj_ops {
  obj_t *(*equal)(obj_t *self, obj_t *other);
  void (*print)(obj_t *self, obj_t *stream);
  void (*destroy)(obj_t *self);
  void (*mark)(obj_t *self);
  long (*hash)(obj_t *self);
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

extern obj_t *interned_syms;

extern obj_t *t, *cons_t, *str_t, *chr_t, *num_t, *sym_t, *fun_t, *vec_t;
extern obj_t *stream_t, *hash_t, *lcons_t, *lstr_t, *cobj_t;
extern obj_t *var, *regex, *set, *cset, *wild, *oneplus;
extern obj_t *zeroplus, *optional, *compound, *or, *quasi;
extern obj_t *skip, *trailer, *block, *next, *freeform, *fail, *accept;
extern obj_t *all, *some, *none, *maybe, *cases, *collect, *until, *coll;
extern obj_t *define, *output, *single, *frst, *lst, *empty, *repeat, *rep;
extern obj_t *flattn, *forget, *local, *mrge, *bind, *cat, *args;
extern obj_t *try, *catch, *finally, *nothrow, *throw, *defex;
extern obj_t *error, *type_error, *internal_err, *numeric_err, *range_err;
extern obj_t *query_error, *file_error, *process_error;

extern obj_t *zero, *one, *two, *negone, *maxint, *minint;
extern obj_t *null_string;
extern obj_t *null_list; /* (nil) */

extern obj_t *identity_f;
extern obj_t *equal_f;

extern const wchar_t *progname;
extern obj_t *prog_string;

extern void *(*oom_realloc)(void *, size_t);

obj_t *identity(obj_t *obj);
obj_t *typeof(obj_t *obj);
obj_t *type_check(obj_t *obj, int);
obj_t *type_check2(obj_t *obj, int, int);
obj_t *type_check3(obj_t *obj, int, int, int);
obj_t *car(obj_t *cons);
obj_t *cdr(obj_t *cons);
obj_t **car_l(obj_t *cons);
obj_t **cdr_l(obj_t *cons);
obj_t *first(obj_t *cons);
obj_t *rest(obj_t *cons);
obj_t *second(obj_t *cons);
obj_t *third(obj_t *cons);
obj_t *fourth(obj_t *cons);
obj_t *fifth(obj_t *cons);
obj_t *sixth(obj_t *cons);
obj_t **tail(obj_t *cons);
obj_t *pop(obj_t **plist);
obj_t *push(obj_t *val, obj_t **plist);
obj_t *copy_list(obj_t *list);
obj_t *nreverse(obj_t *in);
obj_t *reverse(obj_t *in);
obj_t *append2(obj_t *list1, obj_t *list2);
obj_t *nappend2(obj_t *list1, obj_t *list2);
obj_t *flatten(obj_t *list);
obj_t *memq(obj_t *obj, obj_t *list);
obj_t *tree_find(obj_t *obj, obj_t *tree);
obj_t *some_satisfy(obj_t *list, obj_t *pred, obj_t *key);
obj_t *all_satisfy(obj_t *list, obj_t *pred, obj_t *key);
obj_t *none_satisfy(obj_t *list, obj_t *pred, obj_t *key);
long c_num(obj_t *num);
obj_t *nump(obj_t *num);
obj_t *equal(obj_t *left, obj_t *right);
void *chk_malloc(size_t size);
void *chk_realloc(void*, size_t size);
void *chk_strdup(const wchar_t *str);
obj_t *cons(obj_t *car, obj_t *cdr);
obj_t *list(obj_t *first, ...); /* terminated by nao */
obj_t *consp(obj_t *obj);
obj_t *nullp(obj_t *obj);
obj_t *atom(obj_t *obj);
obj_t *listp(obj_t *obj);
obj_t *proper_listp(obj_t *obj);
obj_t *length(obj_t *list);
obj_t *num(long val);
long c_num(obj_t *num);
obj_t *plus(obj_t *anum, obj_t *bnum);
obj_t *minus(obj_t *anum, obj_t *bnum);
obj_t *neg(obj_t *num);
obj_t *zerop(obj_t *num);
obj_t *gt(obj_t *anum, obj_t *bnum);
obj_t *lt(obj_t *anum, obj_t *bnum);
obj_t *ge(obj_t *anum, obj_t *bnum);
obj_t *le(obj_t *anum, obj_t *bnum);
obj_t *numeq(obj_t *anum, obj_t *bnum);
obj_t *max2(obj_t *anum, obj_t *bnum);
obj_t *min2(obj_t *anum, obj_t *bnum);
obj_t *string_own(wchar_t *str);
obj_t *string(const wchar_t *str);
obj_t *string_utf8(const char *str);
obj_t *mkstring(obj_t *len, obj_t *ch);
obj_t *mkustring(obj_t *len); /* must initialize immediately with init_str! */
obj_t *init_str(obj_t *str, const wchar_t *);
obj_t *copy_str(obj_t *str);
obj_t *stringp(obj_t *str);
obj_t *lazy_stringp(obj_t *str);
obj_t *length_str(obj_t *str);
const wchar_t *c_str(obj_t *str);
obj_t *search_str(obj_t *haystack, obj_t *needle, obj_t *start_num,
                  obj_t *from_end);
obj_t *search_str_tree(obj_t *haystack, obj_t *tree, obj_t *start_num,
                       obj_t *from_end);
obj_t *sub_str(obj_t *str_in, obj_t *from_num, obj_t *to_num);
obj_t *cat_str(obj_t *list, obj_t *sep);
obj_t *split_str(obj_t *str, obj_t *sep);
obj_t *trim_str(obj_t *str);
obj_t *string_lt(obj_t *astr, obj_t *bstr);
obj_t *chr(wchar_t ch);
obj_t *chrp(obj_t *chr);
wchar_t c_chr(obj_t *chr);
obj_t *chr_str(obj_t *str, obj_t *index);
obj_t *chr_str_set(obj_t *str, obj_t *index, obj_t *chr);
obj_t *sym_name(obj_t *sym);
obj_t *make_sym(obj_t *name);
obj_t *intern(obj_t *str);
obj_t *symbolp(obj_t *sym);
obj_t *symbol_name(obj_t *sym);
obj_t *func_f0(obj_t *, obj_t *(*fun)(obj_t *));
obj_t *func_f1(obj_t *, obj_t *(*fun)(obj_t *, obj_t *));
obj_t *func_f2(obj_t *, obj_t *(*fun)(obj_t *, obj_t *, obj_t *));
obj_t *func_f3(obj_t *, obj_t *(*fun)(obj_t *, obj_t *, obj_t *, obj_t *));
obj_t *func_f4(obj_t *, obj_t *(*fun)(obj_t *, obj_t *, obj_t *, obj_t *,
               obj_t *));
obj_t *func_n0(obj_t *(*fun)(void));
obj_t *func_n1(obj_t *(*fun)(obj_t *));
obj_t *func_n2(obj_t *(*fun)(obj_t *, obj_t *));
obj_t *func_n3(obj_t *(*fun)(obj_t *, obj_t *, obj_t *));
obj_t *func_n4(obj_t *(*fun)(obj_t *, obj_t *, obj_t *, obj_t *));
obj_t *apply(obj_t *fun, obj_t *arglist);
obj_t *funcall(obj_t *fun);
obj_t *funcall1(obj_t *fun, obj_t *arg);
obj_t *funcall2(obj_t *fun, obj_t *arg1, obj_t *arg2);
obj_t *reduce_left(obj_t *fun, obj_t *list, obj_t *init, obj_t *key);
obj_t *bind2(obj_t *fun2, obj_t *arg);
obj_t *bind2other(obj_t *fun2, obj_t *arg2);
obj_t *chain(obj_t *fun1_list);
obj_t *vector(obj_t *alloc);
obj_t *vec_get_fill(obj_t *vec);
obj_t *vec_set_fill(obj_t *vec, obj_t *fill);
obj_t **vecref_l(obj_t *vec, obj_t *ind);
obj_t *vec_push(obj_t *vec, obj_t *item);
obj_t *lazy_stream_cons(obj_t *stream);
obj_t *lazy_str(obj_t *list, obj_t *term, obj_t *limit);
obj_t *lazy_str_force_upto(obj_t *lstr, obj_t *index);
obj_t *lazy_str_force(obj_t *lstr);
obj_t *lazy_str_get_trailing_list(obj_t *lstr, obj_t *index);
obj_t *length_str_gt(obj_t *str, obj_t *len);
obj_t *length_str_ge(obj_t *str, obj_t *len);
obj_t *length_str_lt(obj_t *str, obj_t *len);
obj_t *length_str_le(obj_t *str, obj_t *len);
obj_t *cobj(void *handle, obj_t *cls_sym, struct cobj_ops *ops);
void cobj_print_op(obj_t *, obj_t *); /* Default function for struct cobj_ops */
obj_t *assoc(obj_t *list, obj_t *key);
obj_t *acons_new(obj_t *list, obj_t *key, obj_t *value);
obj_t **acons_new_l(obj_t **list, obj_t *key);
obj_t *alist_remove(obj_t *list, obj_t *keys);
obj_t *alist_remove1(obj_t *list, obj_t *key);
obj_t *copy_cons(obj_t *cons);
obj_t *copy_alist(obj_t *list);
obj_t *mapcar(obj_t *fun, obj_t *list);
obj_t *mappend(obj_t *fun, obj_t *list);
obj_t *sort(obj_t *list, obj_t *lessfun, obj_t *keyfun);

void obj_print(obj_t *obj, obj_t *stream);
void obj_pprint(obj_t *obj, obj_t *stream);
void init(const wchar_t *progname, void *(*oom_realloc)(void *, size_t),
          obj_t **stack_bottom);
void dump(obj_t *obj, obj_t *stream);
obj_t *snarf(obj_t *in);
obj_t *match(obj_t *spec, obj_t *data);

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
