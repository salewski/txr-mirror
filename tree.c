/* Copyright 2019
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
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#include "lib.h"
#include "gc.h"
#include "args.h"
#include "txr.h"
#include "signal.h"
#include "unwind.h"
#include "stream.h"
#include "eval.h"
#include "itypes.h"
#include "arith.h"
#include "tree.h"

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

void tree_init(void)
{
  reg_fun(tnode_s, func_n3(tnode));
  reg_fun(intern(lit("left"), user_package), func_n1(left));
  reg_fun(intern(lit("right"), user_package), func_n1(right));
  reg_fun(intern(lit("key"), user_package), func_n1(key));
}
