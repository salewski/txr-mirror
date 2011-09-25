/* Copyright 2011
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

#include <stddef.h>
#include <setjmp.h>
#include "config.h"
#include "lib.h"
#include "hash.h"
#include "unwind.h"
#include "filter.h"

static val make_trie(void)
{
  return make_hash(nil, nil);
}

static val trie_add(val trie, val key, val value)
{
  val node, i, len = length_str(key);

  for (node = trie, i = zero; lt(i, len); i = plus(i, one)) {
    val ch = chr_str(key, i);
    val newnode_p;
    val *loc = gethash_l(node, ch, &newnode_p);
    if (newnode_p)
      *loc = make_hash(nil, nil);
    node = *loc;
  }

  set_hash_userdata(node, value);
  return node;
}

val trie_lookup_begin(val trie)
{
  return trie;
}

val trie_value_at(val node)
{
  return get_hash_userdata(node);
}

val trie_lookup_feed_char(val node, val ch)
{
  return gethash(node, ch);
}

val get_filter_trie(val sym)
{
  return gethash(filters, sym);
}

struct filter_pair {
  wchar_t *key, *value;
};

static val build_filter(struct filter_pair *pair)
{
  int i;
  val trie = make_trie();

  for (i = 0; pair[i].key; i++)
    trie_add(trie, static_str(pair[i].key), static_str(pair[i].value));

  return trie;
}

static struct filter_pair to_html_table[] = {
  { L"<", L"&lt;" },
  { L">", L"&gt;" },
  { L"&", L"&amp;" },
  { L"\"", L"&quot;" },
  { 0, 0 }
};

static val trie_filter_string(val filter, val str)
{
  val len = length_str(str);
  val i;
  val out = string(L"");

  for (i = zero; lt(i, len); ) {
    val node = trie_lookup_begin(filter);
    val match = nil;
    val subst;
    val j;

    for (j = i; lt(j, len); j = plus(j, one)) {
      val ch = chr_str(str, j);
      val nnode = trie_lookup_feed_char(node, ch);
      val nsubst;

      if (!nnode)
        break;

      if ((nsubst = trie_value_at(nnode))) {
        match = j;
        subst = nsubst;
      }

      node = nnode;
    }

    if (match) {
      string_extend(out, subst);
      i = plus(match, one);
    } else {
      string_extend(out, chr_str(str, i));
      i = plus(i, one);
    }
  }

  return out;
}

val filters;
val filter_k, to_html_k;

val filter_string(val filter, val str)
{
  val type = typeof(filter);

  if (type == null)
    return str;
  if (type == hash_s)
    return trie_filter_string(filter, str);
  else if (type == fun_s)
    return funcall1(filter, str);
  return str;
  uw_throwf(error_s, lit("filter_string: invalid filter ~a"), filter, nao);
}

void filter_init(void)
{
  filters = make_hash(nil, nil);
  filter_k = intern(lit("filter"), keyword_package);
  to_html_k = intern(lit("to_html"), keyword_package);
  sethash(filters, to_html_k, build_filter(to_html_table));
}
