/* Copyright 2012
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

#include <stddef.h>
#include <setjmp.h>
#include <string.h>
#include <wctype.h>
#include <wchar.h>
#include <stdarg.h>
#include <dirent.h>
#include <stdio.h>
#include "config.h"
#include "lib.h"
#include "hash.h"
#include "unwind.h"
#include "match.h"
#include "filter.h"
#include "gc.h"
#include "stream.h"

val filters;
val filter_k, lfilt_k, rfilt_k, to_html_k, from_html_k;
val upcase_k, downcase_k, fun_k;
val topercent_k, frompercent_k, tourl_k, fromurl_k;
val tonumber_k, tointeger_k, tofloat_k, hextoint_k;

static val make_trie(void)
{
  return make_hash(nil, nil, nil);
}

static val trie_add(val trie, val key, val value)
{
  val node, i, len = length_str(key);

  for (node = trie, i = zero; lt(i, len); i = plus(i, one)) {
    val ch = chr_str(key, i);
    val newnode_p;
    val *loc = gethash_l(node, ch, &newnode_p);
    if (newnode_p)
      set(*loc, make_hash(nil, nil, nil));
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
      set(*ptrie, value);
    } else if (eq(count, one) && nullp(value)) {
      val iter = hash_begin(trie);
      val cell = hash_next(iter);
      set(*ptrie, cons(car(cell), cdr(cell)));
      trie_compress(cdr_l(*ptrie));
    } else {
      val cell, iter = hash_begin(trie);

      for (cell = hash_next(iter); cell; cell = hash_next(iter))
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
  if (functionp(node))
    return nil;
  return node;
}

val trie_lookup_feed_char(val node, val ch)
{
  if (hashp(node))
    return gethash(node, ch);
  if (functionp(node))
    return funcall1(node, ch);
  if (consp(node) && eq(ch,car(node)))
    return cdr(node);
  return nil;
}

static val string_tree_filter(val tree, val filter)
{
  return filter_string_tree(filter, tree);
}

static val compound_filter(val filter_list, val string)
{
  return reduce_left(func_n2(string_tree_filter), filter_list, string, nil);
}

val get_filter(val spec)
{
  if (consp(spec)) {
    if (car(spec) == fun_k) {
      return curry_123_2(func_n3(match_filter), second(spec), rest(rest(spec)));
    } else {
      val filter_list = mapcar(func_n1(get_filter), spec);

      if (memqual(nil, filter_list))
    return nil;

      return curry_12_2(func_n2(compound_filter), filter_list);
    }
  }

  return gethash(filters, spec);
}

struct filter_pair {
  const wchli_t *key, *value;
};

static val build_filter(struct filter_pair *pair, val compress_p)
{
  int i;
  val trie = make_trie();

  for (i = 0; pair[i].key; i++)
    trie_add(trie, static_str(pair[i].key), static_str(pair[i].value));

  if (compress_p)
    trie_compress(&trie);
  return trie;
}

static val build_filter_from_list(val list)
{
  val trie = make_trie();
  val iter;

  for (iter = list; iter; iter = cdr(iter)) {
    val tuple = reverse(car(iter));
    mapcar(curry_123_2(func_n3(trie_add), trie, first(tuple)), rest(tuple));
  }

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
    val subst = nil;
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

val filter_string_tree(val filter, val obj)
{
  switch (type(obj)) {
  case NIL: 
    return nil;
  case CONS:
    return mapcar(curry_12_2(func_n2(filter_string_tree), filter), obj);
  default:
    {
      val type = typeof(filter);

      if (eq(type, null))
        return obj;
      if (eq(type, hash_s) || eq(type, cons_s))
        return trie_filter_string(filter, obj);
      else if (type == fun_s)
        return funcall1(filter, obj);
      return obj;
      uw_throwf(error_s, lit("filter_string: invalid filter ~a"), filter, nao);
    }
  }
}

val filter_equal(val lfilt, val rfilt, val left, val right)
{
  return equal(filter_string_tree(lfilt, left),
               filter_string_tree(rfilt, right));
}

val register_filter(val sym, val table)
{
  return sethash(filters, sym, build_filter_from_list(table));
}

static struct filter_pair to_html_table[] = {
  { wli("<"), wli("&lt;") },
  { wli(">"), wli("&gt;") },
  { wli("&"), wli("&amp;") },
  { wli("\""), wli("&quot;") },
  { 0, 0 }
};

static struct filter_pair from_html_table[] = {
  { wli("&quot;"),      wli("\"") },
  { wli("&amp;"),       wli("&") },
  { wli("&apos;"),      wli("'") },
  { wli("&lt;"),        wli("<") },
  { wli("&gt;"),        wli(">") },
  { wli("&nbsp;"),      wli("\x00A0") },
  { wli("&iexcl;"),     wli("\x00A1") },
  { wli("&cent;"),      wli("\x00A2") },
  { wli("&pound;"),     wli("\x00A3") },
  { wli("&curren;"),    wli("\x00A4") },
  { wli("&yen;"),       wli("\x00A5") },
  { wli("&brvbar;"),    wli("\x00A6") },
  { wli("&sect;"),      wli("\x00A7") },
  { wli("&uml;"),       wli("\x00A8") },
  { wli("&copy;"),      wli("\x00A9") },
  { wli("&ordf;"),      wli("\x00AA") },
  { wli("&laquo;"),     wli("\x00AB") },
  { wli("&not;"),       wli("\x00AC") },
  { wli("&shy;"),       wli("\x00AD") },
  { wli("&reg;"),       wli("\x00AE") },
  { wli("&macr;"),      wli("\x00AF") },
  { wli("&deg;"),       wli("\x00B0") },
  { wli("&plusmn;"),    wli("\x00B1") },
  { wli("&sup2;"),      wli("\x00B2") },
  { wli("&sup3;"),      wli("\x00B3") },
  { wli("&acute;"),     wli("\x00B4") },
  { wli("&micro;"),     wli("\x00B5") },
  { wli("&para;"),      wli("\x00B6") },
  { wli("&middot;"),    wli("\x00B7") },
  { wli("&cedil;"),     wli("\x00B8") },
  { wli("&sup1;"),      wli("\x00B9") },
  { wli("&ordm;"),      wli("\x00BA") },
  { wli("&raquo;"),     wli("\x00BB") },
  { wli("&frac14;"),    wli("\x00BC") },
  { wli("&frac12;"),    wli("\x00BD") },
  { wli("&frac34;"),    wli("\x00BE") },
  { wli("&iquest;"),    wli("\x00BF") },
  { wli("&Agrave;"),    wli("\x00C0") },
  { wli("&Aacute;"),    wli("\x00C1") },
  { wli("&Acirc;"),     wli("\x00C2") },
  { wli("&Atilde;"),    wli("\x00C3") },
  { wli("&Auml;"),      wli("\x00C4") },
  { wli("&Aring;"),     wli("\x00C5") },
  { wli("&AElig;"),     wli("\x00C6") },
  { wli("&Ccedil;"),    wli("\x00C7") },
  { wli("&Egrave;"),    wli("\x00C8") },
  { wli("&Eacute;"),    wli("\x00C9") },
  { wli("&Ecirc;"),     wli("\x00CA") },
  { wli("&Euml;"),      wli("\x00CB") },
  { wli("&Igrave;"),    wli("\x00CC") },
  { wli("&Iacute;"),    wli("\x00CD") },
  { wli("&Icirc;"),     wli("\x00CE") },
  { wli("&Iuml;"),      wli("\x00CF") },
  { wli("&ETH;"),       wli("\x00D0") },
  { wli("&Ntilde;"),    wli("\x00D1") },
  { wli("&Ograve;"),    wli("\x00D2") },
  { wli("&Oacute;"),    wli("\x00D3") },
  { wli("&Ocirc;"),     wli("\x00D4") },
  { wli("&Otilde;"),    wli("\x00D5") },
  { wli("&Ouml;"),      wli("\x00D6") },
  { wli("&times;"),     wli("\x00D7") },
  { wli("&Oslash;"),    wli("\x00D8") },
  { wli("&Ugrave;"),    wli("\x00D9") },
  { wli("&Uacute;"),    wli("\x00DA") },
  { wli("&Ucirc;"),     wli("\x00DB") },
  { wli("&Uuml;"),      wli("\x00DC") },
  { wli("&Yacute;"),    wli("\x00DD") },
  { wli("&THORN;"),     wli("\x00DE") },
  { wli("&szlig;"),     wli("\x00DF") },
  { wli("&agrave;"),    wli("\x00E0") },
  { wli("&aacute;"),    wli("\x00E1") },
  { wli("&acirc;"),     wli("\x00E2") },
  { wli("&atilde;"),    wli("\x00E3") },
  { wli("&auml;"),      wli("\x00E4") },
  { wli("&aring;"),     wli("\x00E5") },
  { wli("&aelig;"),     wli("\x00E6") },
  { wli("&ccedil;"),    wli("\x00E7") },
  { wli("&egrave;"),    wli("\x00E8") },
  { wli("&eacute;"),    wli("\x00E9") },
  { wli("&ecirc;"),     wli("\x00EA") },
  { wli("&euml;"),      wli("\x00EB") },
  { wli("&igrave;"),    wli("\x00EC") },
  { wli("&iacute;"),    wli("\x00ED") },
  { wli("&icirc;"),     wli("\x00EE") },
  { wli("&iuml;"),      wli("\x00EF") },
  { wli("&eth;"),       wli("\x00F0") },
  { wli("&ntilde;"),    wli("\x00F1") },
  { wli("&ograve;"),    wli("\x00F2") },
  { wli("&oacute;"),    wli("\x00F3") },
  { wli("&ocirc;"),     wli("\x00F4") },
  { wli("&otilde;"),    wli("\x00F5") },
  { wli("&ouml;"),      wli("\x00F6") },
  { wli("&divide;"),    wli("\x00F7") },
  { wli("&oslash;"),    wli("\x00F8") },
  { wli("&ugrave;"),    wli("\x00F9") },
  { wli("&uacute;"),    wli("\x00FA") },
  { wli("&ucirc;"),     wli("\x00FB") },
  { wli("&uuml;"),      wli("\x00FC") },
  { wli("&yacute;"),    wli("\x00FD") },
  { wli("&thorn;"),     wli("\x00FE") },
  { wli("&yuml;"),      wli("\x00FF") },
  { wli("&OElig;"),     wli("\x0152") },
  { wli("&oelig;"),     wli("\x0153") },
  { wli("&Scaron;"),    wli("\x0160") },
  { wli("&scaron;"),    wli("\x0161") },
  { wli("&Yuml;"),      wli("\x0178") },
  { wli("&fnof;"),      wli("\x0192") },
  { wli("&circ;"),      wli("\x02C6") },
  { wli("&tilde;"),     wli("\x02DC") },
  { wli("&Alpha;"),     wli("\x0391") },
  { wli("&Beta;"),      wli("\x0392") },
  { wli("&Gamma;"),     wli("\x0393") },
  { wli("&Delta;"),     wli("\x0394") },
  { wli("&Epsilon;"),   wli("\x0395") },
  { wli("&Zeta;"),      wli("\x0396") },
  { wli("&Eta;"),       wli("\x0397") },
  { wli("&Theta;"),     wli("\x0398") },
  { wli("&Iota;"),      wli("\x0399") },
  { wli("&Kappa;"),     wli("\x039A") },
  { wli("&Lambda;"),    wli("\x039B") },
  { wli("&Mu;"),        wli("\x039C") },
  { wli("&Nu;"),        wli("\x039D") },
  { wli("&Xi;"),        wli("\x039E") },
  { wli("&Omicron;"),   wli("\x039F") },
  { wli("&Pi;"),        wli("\x03A0") },
  { wli("&Rho;"),       wli("\x03A1") },
  { wli("&Sigma;"),     wli("\x03A3") },
  { wli("&Tau;"),       wli("\x03A4") },
  { wli("&Upsilon;"),   wli("\x03A5") },
  { wli("&Phi;"),       wli("\x03A6") },
  { wli("&Chi;"),       wli("\x03A7") },
  { wli("&Psi;"),       wli("\x03A8") },
  { wli("&Omega;"),     wli("\x03A9") },
  { wli("&alpha;"),     wli("\x03B1") },
  { wli("&beta;"),      wli("\x03B2") },
  { wli("&gamma;"),     wli("\x03B3") },
  { wli("&delta;"),     wli("\x03B4") },
  { wli("&epsilon;"),   wli("\x03B5") },
  { wli("&zeta;"),      wli("\x03B6") },
  { wli("&eta;"),       wli("\x03B7") },
  { wli("&theta;"),     wli("\x03B8") },
  { wli("&iota;"),      wli("\x03B9") },
  { wli("&kappa;"),     wli("\x03BA") },
  { wli("&lambda;"),    wli("\x03BB") },
  { wli("&mu;"),        wli("\x03BC") },
  { wli("&nu;"),        wli("\x03BD") },
  { wli("&xi;"),        wli("\x03BE") },
  { wli("&omicron;"),   wli("\x03BF") },
  { wli("&pi;"),        wli("\x03C0") },
  { wli("&rho;"),       wli("\x03C1") },
  { wli("&sigmaf;"),    wli("\x03C2") },
  { wli("&sigma;"),     wli("\x03C3") },
  { wli("&tau;"),       wli("\x03C4") },
  { wli("&upsilon;"),   wli("\x03C5") },
  { wli("&phi;"),       wli("\x03C6") },
  { wli("&chi;"),       wli("\x03C7") },
  { wli("&psi;"),       wli("\x03C8") },
  { wli("&omega;"),     wli("\x03C9") },
  { wli("&thetasym;"),  wli("\x03D1") },
  { wli("&upsih;"),     wli("\x03D2") },
  { wli("&piv;"),       wli("\x03D6") },
  { wli("&ensp;"),      wli("\x2002") },
  { wli("&emsp;"),      wli("\x2003") },
  { wli("&thinsp;"),    wli("\x2009") },
  { wli("&zwnj;"),      wli("\x200C") },
  { wli("&zwj;"),       wli("\x200D") },
  { wli("&lrm;"),       wli("\x200E") },
  { wli("&rlm;"),       wli("\x200F") },
  { wli("&ndash;"),     wli("\x2013") },
  { wli("&mdash;"),     wli("\x2014") },
  { wli("&lsquo;"),     wli("\x2018") },
  { wli("&rsquo;"),     wli("\x2019") },
  { wli("&sbquo;"),     wli("\x201A") },
  { wli("&ldquo;"),     wli("\x201C") },
  { wli("&rdquo;"),     wli("\x201D") },
  { wli("&bdquo;"),     wli("\x201E") },
  { wli("&dagger;"),    wli("\x2020") },
  { wli("&Dagger;"),    wli("\x2021") },
  { wli("&bull;"),      wli("\x2022") },
  { wli("&hellip;"),    wli("\x2026") },
  { wli("&permil;"),    wli("\x2030") },
  { wli("&prime;"),     wli("\x2032") },
  { wli("&Prime;"),     wli("\x2033") },
  { wli("&lsaquo;"),    wli("\x2039") },
  { wli("&rsaquo;"),    wli("\x203A") },
  { wli("&oline;"),     wli("\x203E") },
  { wli("&frasl;"),     wli("\x2044") },
  { wli("&euro;"),      wli("\x20AC") },
  { wli("&image;"),     wli("\x2111") },
  { wli("&weierp;"),    wli("\x2118") },
  { wli("&real;"),      wli("\x211C") },
  { wli("&trade;"),     wli("\x2122") },
  { wli("&alefsym;"),   wli("\x2135") },
  { wli("&larr;"),      wli("\x2190") },
  { wli("&uarr;"),      wli("\x2191") },
  { wli("&rarr;"),      wli("\x2192") },
  { wli("&darr;"),      wli("\x2193") },
  { wli("&harr;"),      wli("\x2194") },
  { wli("&crarr;"),     wli("\x21B5") },
  { wli("&lArr;"),      wli("\x21D0") },
  { wli("&uArr;"),      wli("\x21D1") },
  { wli("&rArr;"),      wli("\x21D2") },
  { wli("&dArr;"),      wli("\x21D3") },
  { wli("&hArr;"),      wli("\x21D4") },
  { wli("&forall;"),    wli("\x2200") },
  { wli("&part;"),      wli("\x2202") },
  { wli("&exist;"),     wli("\x2203") },
  { wli("&empty;"),     wli("\x2205") },
  { wli("&nabla;"),     wli("\x2207") },
  { wli("&isin;"),      wli("\x2208") },
  { wli("&notin;"),     wli("\x2209") },
  { wli("&ni;"),        wli("\x220B") },
  { wli("&prod;"),      wli("\x220F") },
  { wli("&sum;"),       wli("\x2211") },
  { wli("&minus;"),     wli("\x2212") },
  { wli("&lowast;"),    wli("\x2217") },
  { wli("&radic;"),     wli("\x221A") },
  { wli("&prop;"),      wli("\x221D") },
  { wli("&infin;"),     wli("\x221E") },
  { wli("&ang;"),       wli("\x2220") },
  { wli("&and;"),       wli("\x2227") },
  { wli("&or;"),        wli("\x2228") },
  { wli("&cap;"),       wli("\x2229") },
  { wli("&cup;"),       wli("\x222A") },
  { wli("&int;"),       wli("\x222B") },
  { wli("&there4;"),    wli("\x2234") },
  { wli("&sim;"),       wli("\x223C") },
  { wli("&cong;"),      wli("\x2245") },
  { wli("&asymp;"),     wli("\x2248") },
  { wli("&ne;"),        wli("\x2260") },
  { wli("&equiv;"),     wli("\x2261") },
  { wli("&le;"),        wli("\x2264") },
  { wli("&ge;"),        wli("\x2265") },
  { wli("&sub;"),       wli("\x2282") },
  { wli("&sup;"),       wli("\x2283") },
  { wli("&nsub;"),      wli("\x2284") },
  { wli("&sube;"),      wli("\x2286") },
  { wli("&supe;"),      wli("\x2287") },
  { wli("&oplus;"),     wli("\x2295") },
  { wli("&otimes;"),    wli("\x2297") },
  { wli("&perp;"),      wli("\x22A5") },
  { wli("&sdot;"),      wli("\x22C5") },
  { wli("&lceil;"),     wli("\x2308") },
  { wli("&rceil;"),     wli("\x2309") },
  { wli("&lfloor;"),    wli("\x230A") },
  { wli("&rfloor;"),    wli("\x230B") },
  { wli("&lang;"),      wli("\x2329") },
  { wli("&rang;"),      wli("\x232A") },
  { wli("&loz;"),       wli("\x25CA") },
  { wli("&spades;"),    wli("\x2660") },
  { wli("&clubs;"),     wli("\x2663") },
  { wli("&hearts;"),    wli("\x2665") },
  { wli("&diams;"),     wli("\x2666") },
  { 0,                  0 }
};

static int digit_value(int digit)
{
  if (digit >= '0' && digit <= '9')
    return digit - '0';
  if (digit >= 'A' && digit <= 'F')
    return digit - 'A' + 10;
  if (digit >= 'a' && digit <= 'f')
    return digit - 'a' + 10;
  internal_error("bad digit");
}

static val html_hex_continue(val hexlist, val ch)
{
  if (iswxdigit(c_chr(ch))) {
    return func_f1(cons(ch, hexlist), html_hex_continue);
  } if (eq(ch, chr(';'))) {
    wchar_t out[2] = { 0 };
    val iter;

    if (nullp(hexlist))
      return nil;

    for (iter = nreverse(hexlist); iter; iter = cdr(iter)) {
      val hexch = car(iter);
      int val = digit_value(c_chr(hexch));
      out[0] <<= 4;
      out[0] |= val;
    }

    return string(out);
  } else {
    return nil;
  }
}

static val html_dec_continue(val declist, val ch)
{
  if (iswdigit(c_chr(ch))) {
    return func_f1(cons(ch, declist), html_dec_continue);
  } if (eq(ch, chr(';'))) {
    wchar_t out[2] = { 0 };
    val iter;

    for (iter = nreverse(declist); iter; iter = cdr(iter)) {
      val decch = car(iter);
      int val = c_chr(decch) - '0';
      out[0] *= 10;
      out[0] += val;
    }

    return string(out);
  } else {
    return nil;
  }
}

static val html_numeric_handler(val ch)
{
  if (eq(ch, chr('x')))
    return func_f1(nil, html_hex_continue);
  if (!iswdigit(c_chr(ch)))
    return nil;
  return func_f1(cons(ch, nil), html_dec_continue);
}

static int is_url_reserved(int ch)
{
  return (ch <= 0x20 || ch >= 0x7F || strchr(":/?#[]@!$&'()*+,;=%", ch) != 0);
}

val url_encode(val str, val space_plus)
{
  val in_byte = make_string_byte_input_stream(str);
  val out = make_string_output_stream();
  val ch;

  while ((ch = get_byte(in_byte)) != nil) {
    int c = c_num(ch);

    if (space_plus && c == ' ')
      put_char(chr('+'), out);
    else if (is_url_reserved(c))
      format(out, lit("%~1X~1X"), num_fast(c >> 4), num_fast(c & 0xf), nao);
    else
      put_char(chr_num(ch), out);
  }

  return get_string_from_stream(out);
}

val url_decode(val str, val space_plus)
{
  val in = make_string_input_stream(str);
  val out = make_string_output_stream();

  for (;;) {
    val ch = get_char(in);

    if (ch == chr('%')) {
      val ch2 = get_char(in);
      val ch3 = get_char(in);

      if (ch2 && ch3 && chr_isxdigit(ch2) && chr_isxdigit(ch3)) {
        int byte = digit_value(c_num(ch2)) << 4 | digit_value(c_num(ch3));
        put_byte(num_fast(byte), out);
      } else {
        put_char(ch, out);
        if (!ch2)
          break;
        put_char(ch2, out);
        if (!ch3)
          break;
        put_char(ch3, out);
      }
      continue;
    }
    if (space_plus && ch == chr('+')) {
      put_char(chr(' '), out);
      continue;
    }
    if (!ch)
      break;

    put_char(ch, out);
  }

  return get_string_from_stream(out);
}

void filter_init(void)
{
  protect(&filters, (val *) 0);

  filters = make_hash(nil, nil, nil);
  filter_k = intern(lit("filter"), keyword_package);
  lfilt_k = intern(lit("lfilt"), keyword_package);
  rfilt_k = intern(lit("rfilt"), keyword_package);
  to_html_k = intern(lit("to_html"), keyword_package);
  from_html_k = intern(lit("from_html"), keyword_package);
  upcase_k = intern(lit("upcase"), keyword_package);
  downcase_k = intern(lit("downcase"), keyword_package);
  fun_k = intern(lit("fun"), keyword_package);
  topercent_k = intern(lit("topercent"), keyword_package);
  frompercent_k = intern(lit("frompercent"), keyword_package);
  tourl_k = intern(lit("tourl"), keyword_package);
  fromurl_k = intern(lit("fromurl"), keyword_package);
  tonumber_k = intern(lit("tonumber"), keyword_package);
  tointeger_k = intern(lit("toinger"), keyword_package);
  tofloat_k = intern(lit("tofloat"), keyword_package);
  hextoint_k = intern(lit("hextoint"), keyword_package);

  sethash(filters, to_html_k, build_filter(to_html_table, t));
  {
    val trie = build_filter(from_html_table, nil);
    trie_add(trie, lit("&#"), func_n1(html_numeric_handler));
    trie_compress(&trie);
    sethash(filters, from_html_k, trie);
  }
  sethash(filters, upcase_k, func_n1(upcase_str));
  sethash(filters, downcase_k, func_n1(downcase_str));
  sethash(filters, topercent_k, curry_12_1(func_n2(url_encode), nil));
  sethash(filters, frompercent_k, curry_12_1(func_n2(url_decode), nil));
  sethash(filters, tourl_k, curry_12_1(func_n2(url_encode), t));
  sethash(filters, fromurl_k, curry_12_1(func_n2(url_decode), t));
  sethash(filters, tonumber_k, func_n1(num_str));
  sethash(filters, tointeger_k, curry_12_1(func_n2(int_str), nil));
  sethash(filters, tofloat_k, func_n1(flo_str));
  sethash(filters, hextoint_k, curry_12_1(func_n2(int_str), num_fast(16)));
}
