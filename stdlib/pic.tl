;; Copyright 2021-2023
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

(defun expand-pic-num (fmt val)
  (let* ((zero (or (starts-with "0" fmt)
                   (starts-with "+0" fmt)
                   (starts-with "-0" fmt)))
         (plus (eql [fmt 0] #\+))
         (minus (eql [fmt 0] #\-))
         (exc (pos #\! fmt))
         (dot (or exc (pos #\. fmt)))
         (fmt (if (and exc (eq #\! [fmt -1])) [fmt 0..-1] fmt))
         (fra (if dot [fmt (succ dot)..:] "")))
    (let ((code (if (or minus plus (not zero))
                  ^(fmt ,`~@(len fmt),@(if plus "+")@(if zero "0")@(len fra)f`
                        ,val)
                  ^(fmt ,`~@(len fmt),-0@(len fra)f`
                        ,val))))
      (if exc
        (with-gensyms (str)
          ^(let ((,str ,code))
             (if (> (len ,str) ,(len fmt))
               ,(let ((fill (mkstring (len fmt) #\#)))
                  (if (plusp (len fra))
                    (set [fill dot] #\.))
                  fill)
               ,str)))
        code))))

(defun comma-positions (fmt)
  (let* ((fmtx (regsub #/,./ #\, fmt))
         (len (len fmtx))
         (ppos (or (pos #\. fmtx) (pos #\! fmtx) len))
         (out (vec)))
    (each ((i len..0))
      (if (meql [fmtx i] #\,)
        (vec-push out (- ppos i))))
    out))

(defun insert-commas (num positions)
  (let* ((len (len num))
         (pn (len positions))
         (ppos (or (pos #\. num) len))
         (out (mkstring 0))
         (j 0)
         (comma #\,))
    (each ((i len..0)
           (p (- ppos len -1)))
      (cond
        ((meq comma #\- #\+ #\space)
         (string-extend out comma)
         (set comma #\space))
        (t
          (string-extend out [num i])))
      (when (plusp i)
        (when (< j pn)
          (ifa (meq [num (pred i)] #\space #\- #\+)
            (set comma it))
          (let ((pj [positions j]))
            (cond
              ((eql pj p)
               (string-extend out comma)
               (if (neq comma #\,)
                 (set comma #\space))
               (inc j))
              ((< pj p)
               (inc j)))))))
    (nreverse out)))

(defun add-neg-parens (width str)
  (let ((sig (pos #\- str))
        (w (len str)))
    (cond
      (sig
        (set [str sig] #\space)
        `(@[str 1..:])`)
      ((> w width) str)
      (t `@str `))))

(defun expand-pic-num-commas (fmt val)
  (let* ((fmt-nc (remq #\, fmt))
         (exp-nc (expand-pic-num fmt-nc val)))
    ^(insert-commas ,exp-nc ,(comma-positions fmt))))

(defun expand-neg-parens (width exp-n)
  ^(add-neg-parens ,width ,exp-n))

(defun expand-pic-align (chr fmt val)
  ^(fmt ,`~@(if chr chr)@(len fmt)a` ,val))

(defun pic-join-opt (join-form)
  (labels ((et (str) (regsub "~" "~~" str)))
    (match-case join-form
      ((join @(stringp @s) (fmt `@fmt` . @args) . @rest)
       (pic-join-opt ^(join (fmt ,`@(et s)@fmt` ,*args) ,*rest)))
      ((join (fmt `@fmt` . @args) @(stringp @s) . @rest)
       (pic-join-opt ^(join (fmt ,`@fmt@(et s)` ,*args) ,*rest)))
      ((join (fmt `@fmt1` . @args1) (fmt `@fmt2` . @args2) . @rest)
       (pic-join-opt ^(join (fmt ,`@fmt1@fmt2` ,*args1 ,*args2) ,*rest)))
      ((join @(stringp @s1) @(stringp @s2) . @rest)
       (pic-join-opt ^(join ,`@s1@s2` ,*rest)))
      ((join "" @item . @rest)
       (pic-join-opt ^(join ,item ,*rest)))
      ((join @item "" . @rest)
       (pic-join-opt ^(join ,item ,*rest)))
      ((join @item) item)
      (@else else))))

(defun expand-pic (f fmt val)
  (unless (stringp fmt)
    (compile-error f "~s is required to be a format string" fmt))
  (cond
    ([m^$ #/\~[~#<>\|\-+0.!,()]/ fmt] [fmt 1..2])
    ([m^$ #/\~./ fmt] (compile-error f "unrecognized escape sequence ~a" fmt))
    ([m^$ #/\~/ fmt] (compile-error f "incomplete ~~ escape"))
    ([m^$ #/[+\-]?(0,?)?#+(,#+)*([.!]#+(,#+)*|!)?/ fmt]
     (if (contains "," fmt)
       (expand-pic-num-commas fmt val)
       (expand-pic-num fmt val)))
    ([m^$ #/\((0,?)?#+(,#+)*([.!]#+(,#+)*|!)?\)/ fmt]
     (let ((fmt `-@[fmt 1..-1]`))
       (expand-neg-parens
         (len fmt)
         (if (contains "," fmt)
           (expand-pic-num-commas fmt val)
           (expand-pic-num fmt val)))))
    ([m^$ #/<+/ fmt] (expand-pic-align "<" fmt val))
    ([m^$ #/>+/ fmt] (expand-pic-align nil fmt val))
    ([m^$ #/\|+/ fmt] (expand-pic-align "^" fmt val))
    (t (compile-error f "unrecognized format string ~s" fmt))))

(defmacro pic (:form f bigfmt . args)
  (let* ((regex #/[+\-]?(0,?)?#+(,#+)*([.!]#+(,#+)*|!)?| \
                  \((0,?)?#+(,#+)*([.!]#+(,#+)*|!)?\)| \
                  <+| \
                  >+| \
                  \|+| \
                  \~.|\~/))
    (labels ((pic-compile-string (fmtstr)
               (let ((items (collect-each ((piece (tok regex t fmtstr)))
                              (cond
                                ((m^$ regex piece)
                                 (cond
                                   ((starts-with "~" piece)
                                    (expand-pic f piece nil))
                                   (args
                                     (expand-pic f piece (pop args)))
                                   (t (compile-error
                                        f "insufficient arguments for format"))))
                                (t piece)))))
                 (pic-join-opt ^(join ,*items)))))
      (match-case bigfmt
        (@(stringp @s)
           (let ((out (pic-compile-string s)))
             (if args
               (compile-warning f "excess arguments"))
             out))
        ((@(or sys:quasi) . @qargs)
           (let ((nqargs (build (each ((q qargs))
                                  (if (stringp q)
                                    (add (pic-compile-string q))
                                    (add q))))))
             (if args
               (compile-warning f "excess arguments"))
             ^(sys:quasi ,*nqargs)))
        (@else (compile-error
                 f "~s is required to be a string or quasiliteral" else))))))
