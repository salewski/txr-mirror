;; Copyright 2021-2024
;; Kaz Kylheku <kaz@kylheku.com>
;; Vancouver, Canada
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;
;; 1. Redistributions of source code must retain the above copyright notice,
;;    this list of conditions and the following disclaimer.
;;
;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;    this list of conditions and the following disclaimer in the documentation
;;    and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

(defvarl %const-foldable-funs%
  '(+ - * / sum prod abs trunc mod zerop nzerop plusp minusp evenp oddp
    > < >= <= = /= wrap wrap* expt exptmod isqrt square gcd lcm floor ceil
    round trunc-rem floor-rem ceil-rem round-rem sin cos tan asin acos atan
    atan2 sinh cosh tanh asinh acosh atanh log log10 log2 exp sqrt
    logand logior logxor logtest lognot logtrunc sign-extend ash bit mask
    width logcount bitset cum-norm-dist inv-cum-norm n-choose-k n-perm-k
    fixnump bignump floatp integerp numberp signum bignum-len divides sys:bits
    digpow digits poly rpoly b< b> b<= b=> b= b+ b- b* b/ neg
    pred ppred ppred pppred succ ssucc ssucc sssucc
    car cdr caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr
    caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr cdaaar cdaadr
    cdadar cdaddr cddaar cddadr cdddar cddddr caaaaar caaaadr caaadar caaaddr
    caadaar caadadr caaddar caadddr cadaaar cadaadr cadadar cadaddr caddaar
    caddadr cadddar caddddr cdaaaar cdaaadr cdaadar cdaaddr cdadaar cdadadr
    cdaddar cdadddr cddaaar cddaadr cddadar cddaddr cdddaar cdddadr cddddar
    cdddddr first rest identity use typeof atom null false true have
    consp listp endp proper-listp length-list second third fourth fifth
    sixth seventh eighth ninth tenth nthcdr nth tailp
    memq memql memqual rmemq rmemql rmemqual countq countql countqual
    posq posql posqual rposq rposql rposqual eq eql equal meq meql mequal
    neq neql nequal max min clamp bracket take drop if or and progn
    prog1 prog2 nilf ignore tf display-width sys:fmt-simple
    sys:fmt-flex sys:fmt-join packagep
    symbolp keywordp bindable stringp length-str
    coded-length cmp-str string-lt str= str< str> str<= str>= int-str
    flo-str num-str int-flo flo-int tofloat toint tointz less greater
    lequal gequal chrp chr-isalnum chr-isalnum chr-isalpha chr-isascii
    chr-iscntrl chr-isdigit chr-digit chr-isgraph chr-islower chr-isprint
    chr-ispunct chr-isspace chr-isblank chr-isunisp chr-isupper chr-isxdigit
    chr-xdigit chr-toupper chr-tolower num-chr int-chr chr-num chr-int
    chr-str span-str compl-span-str break-str vectorp length-vec size-vec
    assq assql assoc rassq rassql rassoc prop memp length len empty ref
    rangep from to in-range in-range* nullify))

(defvarl %const-foldable% (hash-list %const-foldable-funs% :eq-based))

(defvarl %effect-free-funs%
  '(append append* revappend list list* zip interpose copy-list reverse
   flatten flatten* flatcar flatcar* tuples remq remql remqual
   keepq keepq keepqual remq* remql* remq* make-sym gensym
   mkstring copy-str upcase-str downcase-str cat-str split-str spl
   split-str-set sspl tok-str tok tok-where list-str trim-str
   get-lines lazy-str length-str-> length-str->= length-str-<
   length-str-<= vector vec vector-list list-vector list-vec
   copy-vec sub-vec cat-vec acons acons-new aconsql-new alist-remove
   copy-cons copy-tree copy-alist plist-to-alist improper-plist-to-alist
   merge sort shuffle list-seq vec-seq str-seq copy sub seq-begin
   iter-begin rcons make-like nullify symbol-value symbol-function
   symbol-macro boundp fboundp mboundp special-operator-p special-var-p
   copy-fun func-get-form func-get-name func-get-env functionp
   interp-fun-p vm-fun-p fun-fixparam-count fun-optparam-count
   fun-variadic sys:ctx-form sys:ctx-name range range* rlist rlist*
   repeat pad weave promisep rperm perm comb rcomb source-loc
   source-loc-str macro-ancestor cptr-int cptr-obj cptr-buf
   int-cptr cptrp cptr-type cptr-size-hint))

(defvarl %effect-free% (hash-uni %const-foldable%
                                 (hash-list %effect-free-funs% :eq-based)))

(defvarl %functional-funs%
  '(chain chand juxt andf orf notf iff iffi dup flipargs if or and
    progn prog1 prog2 retf apf ipf callf mapf tf umethod uslot))

(defvarl %functional% (hash-list %functional-funs% :eq-based))

