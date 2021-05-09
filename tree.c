/* Copyright 2019-2021
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
#include <stdarg.h>
#include <stdlib.h>
#include <limits.h>
#include <signal.h>
#include "config.h"
#include "alloca.h"
#include "lib.h"
#include "gc.h"
#include "signal.h"
#include "unwind.h"
#include "stream.h"
#include "eval.h"
#include "hash.h"
#include "tree.h"

#if SIZEOF_PTR == 4
#define TREE_DEPTH_MAX 28
#elif SIZEOF_PTR == 8
#define TREE_DEPTH_MAX 60
#else
#error portme
#endif

struct tree {
  val root;
  ucnum size, max_size;
  val key_fn, less_fn, equal_fn;
  val key_fn_name, less_fn_name, equal_fn_name;
};

enum tree_iter_state {
  tr_visited_nothing,
  tr_visited_left,
  tr_find_low_prepared
};

struct tree_iter {
  val self;
  int depth;
  enum tree_iter_state state;
  val path[TREE_DEPTH_MAX];
};

struct tree_diter {
  struct tree_iter ti;
  val lastnode;
};

#define tree_iter_init(self) { (self), 0, tr_visited_nothing, { 0 } }

val tree_s, tree_iter_s, tree_fun_whitelist_s;

val tnode(val key, val left, val right)
{
  val obj = make_obj();
  obj->tn.type = TNOD;
  obj->tn.left = left;
  obj->tn.right = right;
  obj->tn.key = key;
  return obj;
}

val tnodep(val obj)
{
  return tnil(type(obj) == TNOD);
}

val left(val node)
{
  type_check(lit("left"), node, TNOD);
  return node->tn.left;
}

val right(val node)
{
  type_check(lit("right"), node, TNOD);
  return node->tn.right;
}

val key(val node)
{
  type_check(lit("key"), node, TNOD);
  return node->tn.key;
}

val set_left(val node, val nleft)
{
  type_check(lit("set-left"), node, TNOD);
  set(mkloc(node->tn.left, node), nleft);
  return node;
}

val set_right(val node, val nright)
{
  type_check(lit("set-right"), node, TNOD);
  set(mkloc(node->tn.right, node), nright);
  return node;
}

val set_key(val node, val nkey)
{
  type_check(lit("set-key"), node, TNOD);
  set(mkloc(node->tn.key, node), nkey);
  return node;
}

val copy_tnode(val node)
{
  val obj = (type_check(lit("copy-tnode"), node, TNOD), make_obj());
  obj->tn = node->tn;
  return obj;
}

static ucnum tn_size(val node)
{
  return 1 + if3(node->tn.right, tn_size(node->tn.right), 0) +
         if3(node->tn.left, tn_size(node->tn.left), 0);
}

static ucnum tn_size_one_child(val node, val child, ucnum size)
{
  return 1 + size + if3(child == node->tn.left,
                        if3(node->tn.right, tn_size(node->tn.right), 0),
                        if3(node->tn.left, tn_size(node->tn.left), 0));
}

static val tn_lookup(struct tree *tr, val node, val key)
{
  val tr_key = if3(tr->key_fn,
                   funcall1(tr->key_fn, node->tn.key),
                   node->tn.key);

  if (if3(tr->less_fn,
          funcall2(tr->less_fn, key, tr_key),
          less(key, tr_key)))
  {
    return if2(node->tn.left, tn_lookup(tr, node->tn.left, key));
  } else if (if3(tr->equal_fn == nil,
                 equal(key, tr_key),
                 funcall2(tr->equal_fn, key, tr_key))) {
    return node;
  } else {
    return if2(node->tn.right, tn_lookup(tr, node->tn.right, key));
  }
}

static val tn_find_next(val node, struct tree_iter *trit)
{
  for (;;) {
    switch (trit->state) {
    case tr_visited_nothing:
      if (!node)
        return nil;
      while (node->tn.left) {
        bug_unless (trit->depth < TREE_DEPTH_MAX);
        set(mkloc(trit->path[trit->depth++], trit->self), node);
        node = node->tn.left;
      }
      trit->state = tr_visited_left;
      return node;
    case tr_visited_left:
      if (node->tn.right) {
        trit->state = tr_visited_nothing;
        set(mkloc(trit->path[trit->depth++], trit->self), node);
        node = node->tn.right;
        continue;
      } else {
        while (trit->depth > 0) {
          val parent = trit->path[--trit->depth];
          if (node == parent->tn.right) {
            node = parent;
            continue;
          }
          trit->state = tr_visited_left;
          return parent;
        }
        return nil;
      }
    case tr_find_low_prepared:
      trit->state = tr_visited_left;
      return node;
    default:
      internal_error("invalid tree iterator state");
    }
  }
}

static void tn_find_low(val node, struct tree_diter *tdi,
                        struct tree *tr, val key)
{
  struct tree_iter *trit = &tdi->ti;

  if (node == 0) {
    return;
  } else {
    val tr_key = if3(tr->key_fn,
                     funcall1(tr->key_fn, node->tn.key),
                     node->tn.key);

    set(mkloc(trit->path[trit->depth++], trit->self), node);

    if (if3(tr->less_fn,
            funcall2(tr->less_fn, key, tr_key),
            less(key, tr_key)))
    {
      set(mkloc(tdi->lastnode, tdi->ti.self), node);
      if (node->tn.left) {
        tn_find_low(node->tn.left, tdi, tr, key);
        return;
      }
    } else if (if3(tr->equal_fn == nil,
                   equal(key, tr_key),
                   funcall2(tr->equal_fn, key, tr_key))) {
      set(mkloc(tdi->lastnode, tdi->ti.self), node);
      if (node->tn.left) {
        tn_find_low(node->tn.left, tdi, tr, key);
        return;
      }
    } else {
      if (node->tn.right) {
        tn_find_low(node->tn.right, tdi, tr, key);
        return;
      }
    }

    while (trit->path[trit->depth - 1] != tdi->lastnode)
      trit->depth--;
    trit->depth--;
    trit->state = tr_find_low_prepared;
  }
}

static val tn_flatten(val x, val y)
{
  if (x == nil)
    return y;
  set(mkloc(x->tn.right, x), tn_flatten(x->tn.right, y));
  return tn_flatten(x->tn.left, x);
}

static val tn_build_tree(ucnum n, val x)
{
  if (n == 0) {
    x->tn.left = nil;
    return x;
  } else {
    val r = tn_build_tree(n / 2, x);
    val s = tn_build_tree((n - 1) / 2, r->tn.right);

    set(mkloc(r->tn.right, r), s->tn.left);
    set(mkloc(s->tn.left, s), r);

    return s;
  }
}

static void tr_rebuild(val tree, struct tree *tr, val node,
                       val parent, ucnum size)
{
#if CONFIG_GEN_GC
  obj_t dummy = { { TNOD, 0, 0, { 0 }, 0 } };
#else
  obj_t dummy = { { TNOD, { 0 }, 0 } };
#endif
  val flat = tn_flatten(node, &dummy);
  val new_root = (tn_build_tree(size, flat), dummy.tn.left);

  if (parent) {
    if (parent->tn.left == node)
      set(mkloc(parent->tn.left, parent), new_root);
    else
      set(mkloc(parent->tn.right, parent), new_root);
  } else {
    set(mkloc(tr->root, tree), new_root);
  }
}

static void tr_find_rebuild_scapegoat(val tree, struct tree *tr,
                                      struct tree_iter *ti,
                                      val child, ucnum child_size)
{
  val parent = ti->path[--ti->depth];
  ucnum parent_size = tn_size_one_child(parent, child, child_size);
  ucnum sib_size = parent_size - child_size;

  if (2 * child_size > parent_size || 2 * sib_size > parent_size)
    tr_rebuild(tree, tr, parent, ti->path[ti->depth - 1], parent_size);
  else
    tr_find_rebuild_scapegoat(tree, tr, ti, parent, parent_size);
}

static void tr_insert(val tree, struct tree *tr, struct tree_iter *ti,
                      val subtree, val node)
{
  val tn_key = if3(tr->key_fn,
                   funcall1(tr->key_fn, node->tn.key),
                   node->tn.key);
  val tr_key = if3(tr->key_fn,
                   funcall1(tr->key_fn, subtree->tn.key),
                   subtree->tn.key);

  if (if3(tr->less_fn,
          funcall2(tr->less_fn, tn_key, tr_key),
          less(tn_key, tr_key)))
  {
    if (subtree->tn.left) {
      set(mkloc(ti->path[ti->depth++], ti->self), subtree);
      tr_insert(tree, tr, ti, subtree->tn.left, node);
    } else {
      int dep = ti->depth + 1;
      set(mkloc(subtree->tn.left, subtree), node);
      if (subtree->tn.right == nil && (((ucnum) 1) << dep) > tr->size) {
        set(mkloc(ti->path[ti->depth++], ti->self), subtree);
        tr_find_rebuild_scapegoat(tree, tr, ti, node, 1);
      }
    }
  } else if (if3(tr->equal_fn == nil,
                 equal(tn_key, tr_key),
                 funcall2(tr->equal_fn, tn_key, tr_key))) {
    set(mkloc(node->tn.left, node), subtree->tn.left);
    set(mkloc(node->tn.right, node), subtree->tn.right);
    if (ti->depth > 0) {
      val parent = ti->path[ti->depth - 1];

      if (parent->tn.left == subtree)
        set(mkloc(parent->tn.left, parent), node);
      else
        set(mkloc(parent->tn.right, parent), node);
    } else {
      set(mkloc(tr->root, tree), node);
    }
  } else {
    if (subtree->tn.right) {
      set(mkloc(ti->path[ti->depth++], ti->self), subtree);
      tr_insert(tree, tr, ti, subtree->tn.right, node);
    } else {
      int dep = ti->depth + 1;
      set(mkloc(subtree->tn.right, subtree), node);
      if (subtree->tn.left == nil && (((ucnum) 1) << dep) > tr->size) {
        set(mkloc(ti->path[ti->depth++], ti->self), subtree);
        tr_find_rebuild_scapegoat(tree, tr, ti, node, 1);
      }
    }
  }
}

static val tr_lookup(struct tree *tree, val key)
{
  return if2(tree->root, tn_lookup(tree, tree->root, key));
}

static val tr_do_delete(val tree, struct tree *tr, val subtree,
                        val parent, val key)
{
  val tr_key = if3(tr->key_fn,
                   funcall1(tr->key_fn, subtree->tn.key),
                   subtree->tn.key);

  if (if3(tr->less_fn,
          funcall2(tr->less_fn, key, tr_key),
          less(key, tr_key)))
  {
    if (subtree->tn.left)
      return tr_do_delete(tree, tr, subtree->tn.left, subtree, key);
    return nil;
  } else if (if3(tr->equal_fn == nil,
                 equal(key, tr_key),
                 funcall2(tr->equal_fn, key, tr_key))) {
    val le = subtree->tn.left;
    val ri = subtree->tn.right;

    if (le && ri) {
      struct tree_iter trit = tree_iter_init(0);
      val succ = tn_find_next(ri, &trit);
      val succ_par = if3(trit.depth, trit.path[trit.depth - 1], subtree);

      if (succ_par == subtree)
        set(mkloc(succ_par->tn.right, succ_par), succ->tn.right);
      else
        set(mkloc(succ_par->tn.left, succ_par), succ->tn.right);

      set(mkloc(succ->tn.left, succ), subtree->tn.left);
      set(mkloc(succ->tn.right, succ), subtree->tn.right);

      if (parent) {
        if (parent->tn.left == subtree)
          set(mkloc(parent->tn.left, parent), succ);
        else
          set(mkloc(parent->tn.right, parent), succ);
      } else {
        tr->root = succ;
      }
    } else {
      uses_or2;
      val chld = or2(le, ri);

      if (parent) {
        if (parent->tn.left == subtree)
          set(mkloc(parent->tn.left, parent), chld);
        else
          set(mkloc(parent->tn.right, parent), chld);
      } else {
        set(mkloc(tr->root, tree), chld);
      }
    }

    subtree->tn.left = subtree->tn.right = nil;
    return subtree;
  } else {
    if (subtree->tn.right)
      return tr_do_delete(tree, tr, subtree->tn.right, subtree, key);
    return nil;
  }
}

static val tr_delete(val tree, struct tree *tr, val key)
{
  if (tr->root) {
    val node = tr_do_delete(tree, tr, tr->root, nil, key);
    if (node) {
      if (2 * --tr->size < tr->max_size) {
        tr_rebuild(tree, tr, tr->root, nil, tr->size);
        tr->max_size = tr->size;
      }
    }
    return node;
  }

  return nil;
}

val tree_insert_node(val tree, val node)
{
  val self = lit("tree-insert-node");
  struct tree *tr = coerce(struct tree *, cobj_handle(self, tree, tree_s));

  type_check(self, node, TNOD);

  node->tn.left = nil;
  node->tn.right = nil;

  if (tr->root == nil) {
    tr->size = 1;
    tr->max_size = 1;
    set(mkloc(tr->root, tree), node);
  } else {
    struct tree_iter ti = tree_iter_init(0);
    if (++tr->size > tr->max_size)
      tr->max_size = tr->size;
    tr_insert(tree, tr, &ti, tr->root, node);
  }

  return node;
}

static val tree_insert(val tree, val key)
{
  return tree_insert_node(tree, tnode(key, nil, nil));
}

static val tree_lookup_node(val tree, val key)
{
  val self = lit("tree-lookup-node");
  struct tree *tr = coerce(struct tree *, cobj_handle(self, tree, tree_s));
  return tr_lookup(tr, key);
}

static val tree_lookup(val tree, val key)
{
  val node = tree_lookup_node(tree, key);
  return if2(node, node->tn.key);
}

static val tree_delete_node(val tree, val key)
{
  val self = lit("tree-delete-node");
  struct tree *tr = coerce(struct tree *, cobj_handle(self, tree, tree_s));
  return tr_delete(tree, tr, key);
}

static val tree_delete(val tree, val key)
{
  val node = tree_delete_node(tree, key);
  return if2(node, node->tn.key);
}

static val tree_root(val tree)
{
  val self = lit("tree-root");
  struct tree *tr = coerce(struct tree *, cobj_handle(self, tree, tree_s));
  return tr->root;
}

static val tree_equal_op(val left, val right)
{
  val self = lit("equal");
  struct tree *ltr = coerce(struct tree *, cobj_handle(self, left, tree_s));
  struct tree *rtr = coerce(struct tree *, cobj_handle(self, right, tree_s));

  if (ltr->size != rtr->size)
    return nil;

  if (ltr->key_fn != rtr->key_fn)
    return nil;

  if (ltr->less_fn != rtr->less_fn)
    return nil;

  if (ltr->equal_fn != rtr->equal_fn)
    return nil;

  {
    struct tree_iter liter = tree_iter_init(0), riter = tree_iter_init(0);
    val lnode = ltr->root, rnode = rtr->root;

    while ((lnode = tn_find_next(lnode, &liter)) &&
           (rnode = tn_find_next(rnode, &riter)))
    {
      if (!equal(lnode->tn.key, rnode->tn.key))
        return nil;
    }

    return t;
  }
}

static void tree_print_op(val tree, val out, val pretty, struct strm_ctx *ctx)
{
  struct tree *tr = coerce(struct tree *, tree->co.handle);
  val save_mode = test_set_indent_mode(out, num_fast(indent_off),
                                       num_fast(indent_data));
  val save_indent;
  int force_br = 0;

  put_string(lit("#T("), out);

  save_indent = inc_indent(out, zero);
  put_char(chr('('), out);
  if (tr->key_fn_name || tr->less_fn_name || tr->equal_fn_name) {
    obj_print_impl(tr->key_fn_name, out, pretty, ctx);
    if (tr->less_fn_name || tr->equal_fn_name) {
      put_char(chr(' '), out);
      obj_print_impl(tr->less_fn_name, out, pretty, ctx);
      if (tr->equal_fn_name) {
        put_char(chr(' '), out);
        obj_print_impl(tr->equal_fn_name, out, pretty, ctx);
      }
    }
  }
  put_char(chr(')'), out);

  {
    struct tree_iter trit = tree_iter_init(0);
    val node = tr->root;

    while ((node = tn_find_next(node, &trit))) {
      if (width_check(out, chr(' ')))
        force_br = 1;
      obj_print_impl(node->tn.key, out, pretty, ctx);
    }
  }

  put_char(chr(')'), out);

  if (force_br)
    force_break(out);

  set_indent_mode(out, save_mode);
  set_indent(out, save_indent);
}

static void tree_mark(val tree)
{
  struct tree *ltr = coerce(struct tree *, tree->co.handle);
  gc_mark(ltr->root);
  gc_mark(ltr->key_fn);
  gc_mark(ltr->less_fn);
  gc_mark(ltr->equal_fn);
  gc_mark(ltr->key_fn_name);
  gc_mark(ltr->less_fn_name);
  gc_mark(ltr->equal_fn_name);
}

static ucnum tree_hash_op(val obj, int *count, ucnum seed)
{
  struct tree *tr = coerce(struct tree *, obj->co.handle);
  ucnum hash = 0;

  if ((*count)-- <= 0)
    return hash;

  hash += equal_hash(tr->key_fn, count, seed);
  hash += equal_hash(tr->less_fn, count, seed);
  hash += equal_hash(tr->equal_fn, count, seed);

  {
    struct tree_iter trit = tree_iter_init(0);
    val node = tr->root;

    while ((node = tn_find_next(node, &trit)) && (*count)-- <= 0)
      hash += equal_hash(node->tn.key, count, seed);
  }

  return hash;
}

static struct cobj_ops tree_ops = cobj_ops_init(tree_equal_op,
                                                tree_print_op,
                                                cobj_destroy_free_op,
                                                tree_mark,
                                                tree_hash_op);

val tree(val keys_in, val key_fn, val less_fn, val equal_fn)
{
  struct tree *tr = coerce(struct tree *, chk_calloc(1, sizeof *tr));
  val keys = default_null_arg(keys_in), key;
  val tree = cobj(coerce(mem_t *, tr), tree_s, &tree_ops);
  seq_iter_t ki;
  uses_or2;

  tr->key_fn = default_null_arg(key_fn);
  tr->less_fn = default_null_arg(less_fn);
  tr->equal_fn = default_null_arg(equal_fn);

  tr->key_fn_name = if2(tr->key_fn,
                        or2(func_get_name(tr->key_fn, nil), tr->key_fn));
  tr->less_fn_name = if2(tr->less_fn,
                         or2(func_get_name(tr->less_fn, nil), tr->key_fn));
  tr->equal_fn_name = if2(tr->equal_fn,
                          or2(func_get_name(tr->equal_fn, nil), tr->key_fn));

  seq_iter_init(tree_s, &ki, keys);

  while (seq_get(&ki, &key))
    tree_insert(tree, key);

  return tree;
}

static val tree_construct_fname(val name)
{
  if (!name) {
    return nil;
  } else if (bindable(name)) {
    val fun = cdr(lookup_fun(nil, name));
    if (fun)
      return fun;
    uw_throwf(error_s, lit("#T: function named ~s doesn't exist"), name, nao);
  } else if (functionp(name)) {
    return name;
  } else {
    uw_throwf(error_s, lit("#T: ~s isn't a function name"), name, nao);
  }
}

static val tree_construct(val opts, val keys)
{
  val key_fn = tree_construct_fname(pop(&opts));
  val less_fn = tree_construct_fname(pop(&opts));
  val equal_fn = tree_construct_fname(pop(&opts));
  return tree(keys, key_fn, less_fn, equal_fn);
}

static val deep_copy_tnode(val node)
{
  if (node == nil)
    return nil;
  return tnode(node->tn.key,
               deep_copy_tnode(node->tn.left),
               deep_copy_tnode(node->tn.right));
}

val copy_search_tree(val tree)
{
  val self = lit("copy-search-tree");
  struct tree *ntr = coerce(struct tree *, malloc(sizeof *ntr));
  struct tree *otr = coerce(struct tree *, cobj_handle(self, tree, tree_s));
  val nroot = deep_copy_tnode(otr->root);
  val ntree = cobj(coerce(mem_t *, ntr), tree_s, &tree_ops);
  *ntr = *otr;
  ntr->root = nroot;
  return ntree;
}

val make_similar_tree(val tree)
{
  val self = lit("make-similar-tree");
  struct tree *ntr = coerce(struct tree *, malloc(sizeof *ntr));
  struct tree *otr = coerce(struct tree *, cobj_handle(self, tree, tree_s));
  val ntree = cobj(coerce(mem_t *, ntr), tree_s, &tree_ops);
  *ntr = *otr;
  ntr->root = nil;
  ntr->size = ntr->max_size = 0;
  return ntree;
}

val treep(val obj)
{
  return tnil(type(obj) == COBJ && obj->co.cls == tree_s);
}

static void tree_iter_mark(val tree_iter)
{
  struct tree_diter *tdi = coerce(struct tree_diter *, tree_iter->co.handle);
  int i;

  for (i = 0; i < tdi->ti.depth; i++)
    gc_mark(tdi->ti.path[i]);

  gc_mark(tdi->lastnode);
}

static struct cobj_ops tree_iter_ops = cobj_ops_init(eq,
                                                     cobj_print_op,
                                                     cobj_destroy_free_op,
                                                     tree_iter_mark,
                                                     cobj_eq_hash_op);

val tree_begin(val tree)
{
  val self = lit("tree-begin");
  struct tree *tr = coerce(struct tree *, cobj_handle(self, tree, tree_s));
  struct tree_diter *tdi = coerce(struct tree_diter *,
                                  chk_calloc(1, sizeof *tdi));
  val iter = cobj(coerce(mem_t *, tdi), tree_iter_s, &tree_iter_ops);

  tdi->ti.self = iter;
  tdi->lastnode = tr->root;

  return iter;
}

val tree_begin_at(val tree, val lowkey)
{
  val self = lit("tree-begin-at");
  struct tree *tr = coerce(struct tree *, cobj_handle(self, tree, tree_s));
  struct tree_diter *tdi = coerce(struct tree_diter *,
                                  chk_calloc(1, sizeof *tdi));
  val iter = cobj(coerce(mem_t *, tdi), tree_iter_s, &tree_iter_ops);

  tdi->ti.self = iter;

  tn_find_low(tr->root, tdi, tr, lowkey);

  return iter;
}

val tree_reset(val iter, val tree)
{
  val self = lit("tree-reset");
  struct tree *tr = coerce(struct tree *, cobj_handle(self, tree, tree_s));
  struct tree_diter *tdi = coerce(struct tree_diter *,
                                  cobj_handle(self, iter, tree_iter_s));
  const struct tree_iter it = tree_iter_init(0);

  tdi->ti = it;
  set(mkloc(tdi->ti.self, iter), iter);
  set(mkloc(tdi->lastnode, iter), tr->root);

  return iter;
}

val tree_reset_at(val iter, val tree, val lowkey)
{
  val self = lit("tree-reset-at");
  struct tree *tr = coerce(struct tree *, cobj_handle(self, tree, tree_s));
  struct tree_diter *tdi = coerce(struct tree_diter *,
                                  cobj_handle(self, iter, tree_iter_s));
  const struct tree_iter it = tree_iter_init(0);

  tdi->ti = it;
  tdi->lastnode = tr->root;

  tn_find_low(tr->root, tdi, tr, lowkey);

  return iter;
}

val tree_next(val iter)
{
  val self = lit("tree-next");
  struct tree_diter *tdi = coerce(struct tree_diter *,
                                  cobj_handle(self, iter, tree_iter_s));

  if (tdi->lastnode) {
    val node = tn_find_next(tdi->lastnode, &tdi->ti);
    set(mkloc(tdi->lastnode, iter), node);
    return node;
  }

  return nil;
}

val tree_clear(val tree)
{
  val self = lit("tree-clear");
  struct tree *tr = coerce(struct tree *, cobj_handle(self, tree, tree_s));
  cnum oldsize = tr->size;
  tr->root = nil;
  tr->size = tr->max_size = 0;
  return oldsize ? num(oldsize) : nil;
}

void tree_init(void)
{
  tree_s = intern(lit("tree"), user_package);
  tree_iter_s = intern(lit("tree-iter"), user_package);
  tree_fun_whitelist_s = intern(lit("*tree-fun-whitelist*"), user_package);
  reg_fun(tnode_s, func_n3(tnode));
  reg_fun(intern(lit("left"), user_package), func_n1(left));
  reg_fun(intern(lit("right"), user_package), func_n1(right));
  reg_fun(intern(lit("key"), user_package), func_n1(key));
  reg_fun(intern(lit("set-left"), user_package), func_n2(set_left));
  reg_fun(intern(lit("set-right"), user_package), func_n2(set_right));
  reg_fun(intern(lit("set-key"), user_package), func_n2(set_key));
  reg_fun(intern(lit("copy-tnode"), user_package), func_n1(copy_tnode));
  reg_fun(tree_s, func_n4o(tree, 0));
  reg_fun(tree_construct_s, func_n2(tree_construct));
  reg_fun(intern(lit("copy-search-tree"), user_package), func_n1(copy_search_tree));
  reg_fun(intern(lit("make-similar-tree"), user_package), func_n1(make_similar_tree));
  reg_fun(intern(lit("treep"), user_package), func_n1(treep));
  reg_fun(intern(lit("tree-insert-node"), user_package), func_n2(tree_insert_node));
  reg_fun(intern(lit("tree-insert"), user_package), func_n2(tree_insert));
  reg_fun(intern(lit("tree-lookup-node"), user_package), func_n2(tree_lookup_node));
  reg_fun(intern(lit("tree-lookup"), user_package), func_n2(tree_lookup));
  reg_fun(intern(lit("tree-delete-node"), user_package), func_n2(tree_delete_node));
  reg_fun(intern(lit("tree-delete"), user_package), func_n2(tree_delete));
  reg_fun(intern(lit("tree-root"), user_package), func_n1(tree_root));
  reg_fun(intern(lit("tree-begin"), user_package), func_n1(tree_begin));
  reg_fun(intern(lit("tree-begin-at"), user_package), func_n2(tree_begin_at));
  reg_fun(intern(lit("tree-reset"), user_package), func_n2(tree_reset));
  reg_fun(intern(lit("tree-reset-at"), user_package), func_n3(tree_reset_at));
  reg_fun(intern(lit("tree-next"), user_package), func_n1(tree_next));
  reg_fun(intern(lit("tree-clear"), user_package), func_n1(tree_clear));
  reg_var(tree_fun_whitelist_s, list(identity_s, equal_s, less_s, nao));
}
