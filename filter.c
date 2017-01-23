/* Copyright 2009-2017
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

#include <string.h>
#include <wctype.h>
#include <wchar.h>
#include <stdarg.h>
#include <dirent.h>
#include <stdio.h>
#include <signal.h>
#include "config.h"
#include "lib.h"
#include "hash.h"
#include "signal.h"
#include "unwind.h"
#include "match.h"
#include "filter.h"
#include "gc.h"
#include "eval.h"
#include "stream.h"

val filters;
val filter_k, lfilt_k, rfilt_k, tohtml_k, fromhtml_k;
val tohtml_star_k;
val upcase_k, downcase_k;
val topercent_k, frompercent_k, tourl_k, fromurl_k, tobase64_k, frombase64_k;
val tonumber_k, toint_k, tofloat_k, hextoint_k;

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
    loc place = gethash_l(node, ch, mkcloc(newnode_p));
    if (newnode_p)
      set(place, make_hash(nil, nil, nil));
    node = deref(place);
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

static void trie_compress(loc ptrie)
{
  val trie = deref(ptrie);

  if (hashp(trie)) {
    val count = hash_count(trie);
    val value = get_hash_userdata(trie);

    if (zerop(count)) {
      set(ptrie, value);
    } else if (count == one && nilp(value)) {
      val iter = hash_begin(trie);
      val cell = hash_next(iter);
      set(ptrie, cons(car(cell), cdr(cell)));
      trie_compress(cdr_l(deref(ptrie)));
    } else {
      val cell, iter = hash_begin(trie);

      for (cell = hash_next(iter); cell; cell = hash_next(iter))
        trie_compress(cdr_l(cell));
    }
  } else if (consp(trie)) {
    trie_compress(cdr_l(trie));
  }
}

static val trie_compress_intrinsic(val ptrie)
{
  trie_compress(mkcloc(ptrie));
  return ptrie;
}

static val regex_from_trie(val trie)
{
  switch (type(trie)) {
  case NIL:
    return nil;
  case CONS:
    {
      val a = car(trie);
      val d = cdr(trie);
      switch (type(d)) {
      case CONS:
        {
          val rx = regex_from_trie(d);
          if (consp(rx) && car(rx) == compound_s)
            return cons(compound_s, cons(a, cdr(rx)));
          return list(compound_s, a, rx, nao);
        }
      case COBJ:
        if (d->co.cls == hash_s)
          return list(compound_s, a, regex_from_trie(d), nao);
        /* fallthrough */
      default:
        return a;
      }
    }
  case COBJ:
    if (trie->co.cls == hash_s) {
      if (zerop(hash_count(trie))) {
        return nil;
      } else {
        list_collect_decl (out, ptail);
        val iter = hash_begin(trie);
        val cell;
        while ((cell = hash_next(iter)) != nil) {
          val rx = regex_from_trie(cdr(cell));
          ptail = list_collect(ptail,
                               if3(consp(rx) && car(rx) == compound_s,
                                   cons(compound_s, cons(car(cell), cdr(rx))),
                                   list(compound_s, car(cell), rx, nao)));
        }
        return cons(or_s, out);
      }
    }
    /* fallthrough */
  default:
    uw_throwf(error_s, lit("regex-from-trie: bad trie element ~s"), trie, nao);
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
  if (consp(node) && ch == car(node))
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
    trie_compress(mkcloc(trie));
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

  trie_compress(mkcloc(trie));
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

      if (type == null_s)
        return obj;
      if (type == hash_s || type == cons_s)
        return trie_filter_string(filter, obj);
      else if (type == fun_s)
        return funcall1(filter, obj);
      return obj;
      uw_throwf(error_s, lit("invalid filter ~a"), filter, nao);
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

static struct filter_pair tohtml_table[] = {
  { wli("<"), wli("&lt;") },
  { wli(">"), wli("&gt;") },
  { wli("&"), wli("&amp;") },
  { wli("\""), wli("&quot;") },
  { wli("'"), wli("&#39;") },
  { 0, 0 }
};

static struct filter_pair tohtml_star_table[] = {
  { wli("<"), wli("&lt;") },
  { wli(">"), wli("&gt;") },
  { wli("&"), wli("&amp;") },
  { 0, 0 }
};

static struct filter_pair fromhtml_table[] = {
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
  } if (ch == chr(';')) {
    wchar_t out[2] = { 0 };
    val iter;

    if (nilp(hexlist))
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
  } if (ch == chr(';')) {
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
  if (ch == chr('x'))
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
      put_char(chr_int(ch), out);
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

INLINE void col_check(cnum *pcol, cnum wcol, val out)
{
  if (wcol && ++(*pcol) >= wcol) {
    *pcol = 0;
    put_char(chr('\n'), out);
  }
}

val base64_encode(val str, val wrap_cols)
{
  static const char *b64 = {
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/="
  };
  cnum col = 0;
  cnum wcol = c_num(default_arg(wrap_cols, zero));
  val in_byte = make_string_byte_input_stream(str);
  val out = make_string_output_stream();

  for (;;) {
    val bv0 = get_byte(in_byte);
    val bv1 = get_byte(in_byte);
    val bv2 = get_byte(in_byte);

    if (bv2) {
      cnum b0 = c_num(bv0);
      cnum b1 = c_num(bv1);
      cnum b2 = c_num(bv2);
      cnum word = (b0 << 16) | (b1 << 8) | b2;

      put_char(chr(b64[(word >> 18)       ]), out); col_check(&col, wcol, out);
      put_char(chr(b64[(word >> 12) & 0x3F]), out); col_check(&col, wcol, out);
      put_char(chr(b64[(word >>  6) & 0x3F]), out); col_check(&col, wcol, out);
      put_char(chr(b64[(word      ) & 0x3F]), out); col_check(&col, wcol, out);
    } else if (bv1) {
      cnum b0 = c_num(bv0);
      cnum b1 = c_num(bv1);
      cnum word = (b0 << 16) | (b1 << 8);
      put_char(chr(b64[(word >> 18)       ]), out); col_check(&col, wcol, out);
      put_char(chr(b64[(word >> 12) & 0x3F]), out); col_check(&col, wcol, out);
      put_char(chr(b64[(word >>  6) & 0x3F]), out); col_check(&col, wcol, out);
      put_char(chr('='), out);                      col_check(&col, wcol, out);
      break;
    } else if (bv0) {
      cnum b0 = c_num(bv0);
      cnum word = (b0 << 16);
      put_char(chr(b64[(word >> 18)       ]), out); col_check(&col, wcol, out);
      put_char(chr(b64[(word >> 12) & 0x3F]), out); col_check(&col, wcol, out);
      put_char(chr('='), out);                      col_check(&col, wcol, out);
      put_char(chr('='), out);                      col_check(&col, wcol, out);
      break;
    } else {
      break;
    }
  }

  if (wcol && col > 0)
    put_char(chr('\n'), out);

  return get_string_from_stream(out);
}

INLINE cnum get_base64_char(val in)
{
  for (;;) {
    val ch = get_char(in);
    if (ch == nil)
      return 0;
    if (!chr_isspace(ch) && ch != chr('='))
      return c_chr(ch);
  }
}

INLINE int b64_code(cnum c)
{
  if ('A' <= c && c <= 'Z')
    return c - 'A';
  if ('a' <= c && c <= 'z')
    return c - 'a' + 26;
  if ('0' <= c && c <= '9')
    return c - '0' + 26 + 26;
  switch (c) {
  case '+':
    return 62;
  case '/':
    return 63;
  default:
    uw_throwf(error_s, lit("base64-decode: invalid character ~s"),
              chr(c), nao);
  }
}

val base64_decode(val str)
{
  val in = make_string_input_stream(str);
  val out = make_string_output_stream();

  for (;;) {
    cnum c0 = get_base64_char(in);
    cnum c1 = get_base64_char(in);
    cnum c2 = get_base64_char(in);
    cnum c3 = get_base64_char(in);

    if (c3) {
      long f0 = b64_code(c0);
      long f1 = b64_code(c1);
      long f2 = b64_code(c2);
      long f3 = b64_code(c3);
      long word = (f0 << 18) | (f1 << 12) | (f2 << 6) | f3;

      put_byte(num_fast((word >> 16)       ), out);
      put_byte(num_fast((word >>  8) & 0xff), out);
      put_byte(num_fast( word        & 0xff), out);
    } else if (c2) {
      long f0 = b64_code(c0);
      long f1 = b64_code(c1);
      long f2 = b64_code(c2);
      long word = (f0 << 18) | (f1 << 12) | (f2 << 6);

      put_byte(num_fast((word >> 16)       ), out);
      put_byte(num_fast((word >>  8) & 0xff), out);
      break;
    } else if (c0 || c1) {
      long f0 = b64_code(c0);
      long f1 = b64_code(c1);
      long word = (f0 << 18) | (f1 << 12);
      put_byte(num_fast((word >> 16)       ), out);
      break;
    } else {
      break;
    }
  }

  return get_string_from_stream(out);
}

static val html_encode(val str)
{
  return trie_filter_string(get_filter(tohtml_k), str);
}

static val html_encode_star(val str)
{
  return trie_filter_string(get_filter(tohtml_star_k), str);
}

static val html_decode(val str)
{
  return trie_filter_string(get_filter(fromhtml_k), str);
}

void filter_init(void)
{
  protect(&filters, convert(val *, 0));

  filters = make_hash(nil, nil, nil);
  filter_k = intern(lit("filter"), keyword_package);
  lfilt_k = intern(lit("lfilt"), keyword_package);
  rfilt_k = intern(lit("rfilt"), keyword_package);
  tohtml_k = intern(lit("tohtml"), keyword_package);
  tohtml_star_k = intern(lit("tohtml*"), keyword_package);
  fromhtml_k = intern(lit("fromhtml"), keyword_package);
  upcase_k = intern(lit("upcase"), keyword_package);
  downcase_k = intern(lit("downcase"), keyword_package);
  topercent_k = intern(lit("topercent"), keyword_package);
  frompercent_k = intern(lit("frompercent"), keyword_package);
  tourl_k = intern(lit("tourl"), keyword_package);
  fromurl_k = intern(lit("fromurl"), keyword_package);
  tonumber_k = intern(lit("tonumber"), keyword_package);
  tobase64_k = intern(lit("tobase64"), keyword_package);
  frombase64_k = intern(lit("frombase64"), keyword_package);
  toint_k = intern(lit("toint"), keyword_package);
  tofloat_k = intern(lit("tofloat"), keyword_package);
  hextoint_k = intern(lit("hextoint"), keyword_package);

  sethash(filters, tohtml_k, build_filter(tohtml_table, t));
  sethash(filters, tohtml_star_k, build_filter(tohtml_star_table, t));
  {
    val trie = build_filter(fromhtml_table, nil);
    trie_add(trie, lit("&#"), func_n1(html_numeric_handler));
    trie_compress(mkcloc(trie));
    sethash(filters, fromhtml_k, trie);
  }
  sethash(filters, intern(lit("to_html"), keyword_package),
          get_filter(tohtml_k));
  sethash(filters, intern(lit("from_html"), keyword_package),
          get_filter(fromhtml_k));
  sethash(filters, upcase_k, func_n1(upcase_str));
  sethash(filters, downcase_k, func_n1(downcase_str));
  sethash(filters, topercent_k, curry_12_1(func_n2(url_encode), nil));
  sethash(filters, frompercent_k, curry_12_1(func_n2(url_decode), nil));
  sethash(filters, tourl_k, curry_12_1(func_n2(url_encode), t));
  sethash(filters, fromurl_k, curry_12_1(func_n2(url_decode), t));
  sethash(filters, tobase64_k, curry_12_1(func_n2(base64_encode), 0));
  sethash(filters, frombase64_k, func_n1(base64_decode));
  sethash(filters, tonumber_k, func_n1(num_str));
  sethash(filters, toint_k, curry_12_1(func_n2(int_str), nil));
  sethash(filters, tofloat_k, func_n1(flo_str));
  sethash(filters, hextoint_k, curry_12_1(func_n2(int_str), num_fast(16)));

  reg_fun(intern(lit("make-trie"), user_package), func_n0(make_trie));
  reg_fun(intern(lit("trie-add"), user_package), func_n3(trie_add));
  reg_fun(intern(lit("trie-compress"), user_package),
          func_n1(trie_compress_intrinsic));
  reg_fun(intern(lit("regex-from-trie"), user_package), func_n1(regex_from_trie));
  reg_fun(intern(lit("trie-lookup-begin"), user_package), func_n1(trie_lookup_begin));
  reg_fun(intern(lit("trie-value-at"), user_package), func_n1(trie_value_at));
  reg_fun(intern(lit("trie-lookup-feed-char"), user_package), func_n2(trie_lookup_feed_char));
  reg_fun(intern(lit("filter-string-tree"), user_package), func_n2(filter_string_tree));
  reg_fun(intern(lit("filter-equal"), user_package), func_n4(filter_equal));
  reg_fun(intern(lit("url-encode"), user_package), func_n2o(url_encode, 1));
  reg_fun(intern(lit("url-decode"), user_package), func_n2o(url_decode, 1));
  reg_fun(intern(lit("base64-encode"), user_package), func_n2o(base64_encode, 1));
  reg_fun(intern(lit("base64-decode"), user_package), func_n1(base64_decode));
  reg_fun(intern(lit("html-encode"), user_package), func_n1(html_encode));
  reg_fun(intern(lit("html-encode*"), user_package),
          func_n1(html_encode_star));
  reg_fun(intern(lit("html-decode"), user_package), func_n1(html_decode));
}
