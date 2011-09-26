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

/* 
 * Reduce the storage requirement for a the trie, by applying
 * these rules:
 *
 * 1. All leaf-level nodes (i.e. empty hash tables) are replaced by
 *    just the node value (hash user data).
 * 2. All hash tables with a single transition (i.e. hash tables
 *    containing one element) and which have no node value
 *    are replaced by a cons cell, whose CAR is the transition
 *    character, and whose CDR is the transition.
 */

static void trie_compress(val *ptrie)
{
  val trie = *ptrie;

  if (hashp(trie)) {
    val count = hash_count(trie);
    val value = get_hash_userdata(trie);

    if (zerop(count)) {
      *ptrie = value;
    } else if (eq(count, one) && nullp(value)) {
      val iter = hash_begin(trie);
      val cell = hash_next(&iter);
      *ptrie = cons(car(cell), cdr(cell));
      trie_compress(cdr_l(*ptrie));
    } else {
      val cell, iter = hash_begin(trie);

      for (cell = hash_next(&iter); iter; cell = hash_next(&iter))
        trie_compress(cdr_l(cell));
    }
  } else if (consp(trie)) {
    trie_compress(cdr_l(trie));
  } 
}

val trie_lookup_begin(val trie)
{
  return trie;
}

val trie_value_at(val node)
{
  if (hashp(node))
    return get_hash_userdata(node);
  if (consp(node))
    return nil;
  return node;
}

val trie_lookup_feed_char(val node, val ch)
{
  if (hashp(node))
    return gethash(node, ch);
  if (consp(node) && eq(ch,car(node)))
    return cdr(node);
  return nil;
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

  trie_compress(&trie);
  return trie;
}

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

val filter_string(val filter, val str)
{
  val type = typeof(filter);

  if (eq(type, null))
    return str;
  if (eq(type, hash_s) || eq(type, cons_s))
    return trie_filter_string(filter, str);
  else if (type == fun_s)
    return funcall1(filter, str);
  return str;
  uw_throwf(error_s, lit("filter_string: invalid filter ~a"), filter, nao);
}

static struct filter_pair to_html_table[] = {
  { L"<", L"&lt;" },
  { L">", L"&gt;" },
  { L"&", L"&amp;" },
  { L"\"", L"&quot;" },
  { 0, 0 }
};

static struct filter_pair from_html_table[] = {
  { L"&quot;",     L"\"" },
  { L"&amp;",      L"&" },
  { L"&apos;",     L"'" },
  { L"&lt;",       L"<" },
  { L"&gt;",       L">" },
  { L"&nbsp;",     L"\x00A0" },
  { L"&iexcl;",    L"\x00A1" },
  { L"&cent;",     L"\x00A2" },
  { L"&pound;",    L"\x00A3" },
  { L"&curren;",   L"\x00A4" },
  { L"&yen;",      L"\x00A5" },
  { L"&brvbar;",   L"\x00A6" },
  { L"&sect;",     L"\x00A7" },
  { L"&uml;",      L"\x00A8" },
  { L"&copy;",     L"\x00A9" },
  { L"&ordf;",     L"\x00AA" },
  { L"&laquo;",    L"\x00AB" },
  { L"&not;",      L"\x00AC" },
  { L"&shy;",      L"\x00AD" },
  { L"&reg;",      L"\x00AE" },
  { L"&macr;",     L"\x00AF" },
  { L"&deg;",      L"\x00B0" },
  { L"&plusmn;",   L"\x00B1" },
  { L"&sup2;",     L"\x00B2" },
  { L"&sup3;",     L"\x00B3" },
  { L"&acute;",    L"\x00B4" },
  { L"&micro;",    L"\x00B5" },
  { L"&para;",     L"\x00B6" },
  { L"&middot;",   L"\x00B7" },
  { L"&cedil;",    L"\x00B8" },
  { L"&sup1;",     L"\x00B9" },
  { L"&ordm;",     L"\x00BA" },
  { L"&raquo;",    L"\x00BB" },
  { L"&frac14;",   L"\x00BC" },
  { L"&frac12;",   L"\x00BD" },
  { L"&frac34;",   L"\x00BE" },
  { L"&iquest;",   L"\x00BF" },
  { L"&Agrave;",   L"\x00C0" },
  { L"&Aacute;",   L"\x00C1" },
  { L"&Acirc;",    L"\x00C2" },
  { L"&Atilde;",   L"\x00C3" },
  { L"&Auml;",     L"\x00C4" },
  { L"&Aring;",    L"\x00C5" },
  { L"&AElig;",    L"\x00C6" },
  { L"&Ccedil;",   L"\x00C7" },
  { L"&Egrave;",   L"\x00C8" },
  { L"&Eacute;",   L"\x00C9" },
  { L"&Ecirc;",    L"\x00CA" },
  { L"&Euml;",     L"\x00CB" },
  { L"&Igrave;",   L"\x00CC" },
  { L"&Iacute;",   L"\x00CD" },
  { L"&Icirc;",    L"\x00CE" },
  { L"&Iuml;",     L"\x00CF" },
  { L"&ETH;",      L"\x00D0" },
  { L"&Ntilde;",   L"\x00D1" },
  { L"&Ograve;",   L"\x00D2" },
  { L"&Oacute;",   L"\x00D3" },
  { L"&Ocirc;",    L"\x00D4" },
  { L"&Otilde;",   L"\x00D5" },
  { L"&Ouml;",     L"\x00D6" },
  { L"&times;",    L"\x00D7" },
  { L"&Oslash;",   L"\x00D8" },
  { L"&Ugrave;",   L"\x00D9" },
  { L"&Uacute;",   L"\x00DA" },
  { L"&Ucirc;",    L"\x00DB" },
  { L"&Uuml;",     L"\x00DC" },
  { L"&Yacute;",   L"\x00DD" },
  { L"&THORN;",    L"\x00DE" },
  { L"&szlig;",    L"\x00DF" },
  { L"&agrave;",   L"\x00E0" },
  { L"&aacute;",   L"\x00E1" },
  { L"&acirc;",    L"\x00E2" },
  { L"&atilde;",   L"\x00E3" },
  { L"&auml;",     L"\x00E4" },
  { L"&aring;",    L"\x00E5" },
  { L"&aelig;",    L"\x00E6" },
  { L"&ccedil;",   L"\x00E7" },
  { L"&egrave;",   L"\x00E8" },
  { L"&eacute;",   L"\x00E9" },
  { L"&ecirc;",    L"\x00EA" },
  { L"&euml;",     L"\x00EB" },
  { L"&igrave;",   L"\x00EC" },
  { L"&iacute;",   L"\x00ED" },
  { L"&icirc;",    L"\x00EE" },
  { L"&iuml;",     L"\x00EF" },
  { L"&eth;",      L"\x00F0" },
  { L"&ntilde;",   L"\x00F1" },
  { L"&ograve;",   L"\x00F2" },
  { L"&oacute;",   L"\x00F3" },
  { L"&ocirc;",    L"\x00F4" },
  { L"&otilde;",   L"\x00F5" },
  { L"&ouml;",     L"\x00F6" },
  { L"&divide;",   L"\x00F7" },
  { L"&oslash;",   L"\x00F8" },
  { L"&ugrave;",   L"\x00F9" },
  { L"&uacute;",   L"\x00FA" },
  { L"&ucirc;",    L"\x00FB" },
  { L"&uuml;",     L"\x00FC" },
  { L"&yacute;",   L"\x00FD" },
  { L"&thorn;",    L"\x00FE" },
  { L"&yuml;",     L"\x00FF" },
  { L"&OElig;",    L"\x0152" },
  { L"&oelig;",    L"\x0153" },
  { L"&Scaron;",   L"\x0160" },
  { L"&scaron;",   L"\x0161" },
  { L"&Yuml;",     L"\x0178" },
  { L"&fnof;",     L"\x0192" },
  { L"&circ;",     L"\x02C6" },
  { L"&tilde;",    L"\x02DC" },
  { L"&Alpha;",    L"\x0391" },
  { L"&Beta;",     L"\x0392" },
  { L"&Gamma;",    L"\x0393" },
  { L"&Delta;",    L"\x0394" },
  { L"&Epsilon;",  L"\x0395" },
  { L"&Zeta;",     L"\x0396" },
  { L"&Eta;",      L"\x0397" },
  { L"&Theta;",    L"\x0398" },
  { L"&Iota;",     L"\x0399" },
  { L"&Kappa;",    L"\x039A" },
  { L"&Lambda;",   L"\x039B" },
  { L"&Mu;",       L"\x039C" },
  { L"&Nu;",       L"\x039D" },
  { L"&Xi;",       L"\x039E" },
  { L"&Omicron;",  L"\x039F" },
  { L"&Pi;",       L"\x03A0" },
  { L"&Rho;",      L"\x03A1" },
  { L"&Sigma;",    L"\x03A3" },
  { L"&Tau;",      L"\x03A4" },
  { L"&Upsilon;",  L"\x03A5" },
  { L"&Phi;",      L"\x03A6" },
  { L"&Chi;",      L"\x03A7" },
  { L"&Psi;",      L"\x03A8" },
  { L"&Omega;",    L"\x03A9" },
  { L"&alpha;",    L"\x03B1" },
  { L"&beta;",     L"\x03B2" },
  { L"&gamma;",    L"\x03B3" },
  { L"&delta;",    L"\x03B4" },
  { L"&epsilon;",  L"\x03B5" },
  { L"&zeta;",     L"\x03B6" },
  { L"&eta;",      L"\x03B7" },
  { L"&theta;",    L"\x03B8" },
  { L"&iota;",     L"\x03B9" },
  { L"&kappa;",    L"\x03BA" },
  { L"&lambda;",   L"\x03BB" },
  { L"&mu;",       L"\x03BC" },
  { L"&nu;",       L"\x03BD" },
  { L"&xi;",       L"\x03BE" },
  { L"&omicron;",  L"\x03BF" },
  { L"&pi;",       L"\x03C0" },
  { L"&rho;",      L"\x03C1" },
  { L"&sigmaf;",   L"\x03C2" },
  { L"&sigma;",    L"\x03C3" },
  { L"&tau;",      L"\x03C4" },
  { L"&upsilon;",  L"\x03C5" },
  { L"&phi;",      L"\x03C6" },
  { L"&chi;",      L"\x03C7" },
  { L"&psi;",      L"\x03C8" },
  { L"&omega;",    L"\x03C9" },
  { L"&thetasym;", L"\x03D1" },
  { L"&upsih;",    L"\x03D2" },
  { L"&piv;",      L"\x03D6" },
  { L"&ensp;",     L"\x2002" },
  { L"&emsp;",     L"\x2003" },
  { L"&thinsp;",   L"\x2009" },
  { L"&zwnj;",     L"\x200C" },
  { L"&zwj;",      L"\x200D" },
  { L"&lrm;",      L"\x200E" },
  { L"&rlm;",      L"\x200F" },
  { L"&ndash;",    L"\x2013" },
  { L"&mdash;",    L"\x2014" },
  { L"&lsquo;",    L"\x2018" },
  { L"&rsquo;",    L"\x2019" },
  { L"&sbquo;",    L"\x201A" },
  { L"&ldquo;",    L"\x201C" },
  { L"&rdquo;",    L"\x201D" },
  { L"&bdquo;",    L"\x201E" },
  { L"&dagger;",   L"\x2020" },
  { L"&Dagger;",   L"\x2021" },
  { L"&bull;",     L"\x2022" },
  { L"&hellip;",   L"\x2026" },
  { L"&permil;",   L"\x2030" },
  { L"&prime;",    L"\x2032" },
  { L"&Prime;",    L"\x2033" },
  { L"&lsaquo;",   L"\x2039" },
  { L"&rsaquo;",   L"\x203A" },
  { L"&oline;",    L"\x203E" },
  { L"&frasl;",    L"\x2044" },
  { L"&euro;",     L"\x20AC" },
  { L"&image;",    L"\x2111" },
  { L"&weierp;",   L"\x2118" },
  { L"&real;",     L"\x211C" },
  { L"&trade;",    L"\x2122" },
  { L"&alefsym;",  L"\x2135" },
  { L"&larr;",     L"\x2190" },
  { L"&uarr;",     L"\x2191" },
  { L"&rarr;",     L"\x2192" },
  { L"&darr;",     L"\x2193" },
  { L"&harr;",     L"\x2194" },
  { L"&crarr;",    L"\x21B5" },
  { L"&lArr;",     L"\x21D0" },
  { L"&uArr;",     L"\x21D1" },
  { L"&rArr;",     L"\x21D2" },
  { L"&dArr;",     L"\x21D3" },
  { L"&hArr;",     L"\x21D4" },
  { L"&forall;",   L"\x2200" },
  { L"&part;",     L"\x2202" },
  { L"&exist;",    L"\x2203" },
  { L"&empty;",    L"\x2205" },
  { L"&nabla;",    L"\x2207" },
  { L"&isin;",     L"\x2208" },
  { L"&notin;",    L"\x2209" },
  { L"&ni;",       L"\x220B" },
  { L"&prod;",     L"\x220F" },
  { L"&sum;",      L"\x2211" },
  { L"&minus;",    L"\x2212" },
  { L"&lowast;",   L"\x2217" },
  { L"&radic;",    L"\x221A" },
  { L"&prop;",     L"\x221D" },
  { L"&infin;",    L"\x221E" },
  { L"&ang;",      L"\x2220" },
  { L"&and;",      L"\x2227" },
  { L"&or;",       L"\x2228" },
  { L"&cap;",      L"\x2229" },
  { L"&cup;",      L"\x222A" },
  { L"&int;",      L"\x222B" },
  { L"&there4;",   L"\x2234" },
  { L"&sim;",      L"\x223C" },
  { L"&cong;",     L"\x2245" },
  { L"&asymp;",    L"\x2248" },
  { L"&ne;",       L"\x2260" },
  { L"&equiv;",    L"\x2261" },
  { L"&le;",       L"\x2264" },
  { L"&ge;",       L"\x2265" },
  { L"&sub;",      L"\x2282" },
  { L"&sup;",      L"\x2283" },
  { L"&nsub;",     L"\x2284" },
  { L"&sube;",     L"\x2286" },
  { L"&supe;",     L"\x2287" },
  { L"&oplus;",    L"\x2295" },
  { L"&otimes;",   L"\x2297" },
  { L"&perp;",     L"\x22A5" },
  { L"&sdot;",     L"\x22C5" },
  { L"&lceil;",    L"\x2308" },
  { L"&rceil;",    L"\x2309" },
  { L"&lfloor;",   L"\x230A" },
  { L"&rfloor;",   L"\x230B" },
  { L"&lang;",     L"\x2329" },
  { L"&rang;",     L"\x232A" },
  { L"&loz;",      L"\x25CA" },
  { L"&spades;",   L"\x2660" },
  { L"&clubs;",    L"\x2663" },
  { L"&hearts;",   L"\x2665" },
  { L"&diams;",    L"\x2666" },
  { 0,             0 }
};

val filters;
val filter_k, to_html_k, from_html_k;

void filter_init(void)
{
  filters = make_hash(nil, nil);
  filter_k = intern(lit("filter"), keyword_package);
  to_html_k = intern(lit("to_html"), keyword_package);
  from_html_k = intern(lit("from_html"), keyword_package);
  sethash(filters, to_html_k, build_filter(to_html_table));
  sethash(filters, from_html_k, build_filter(from_html_table));
}
