;; Copyright 2021
;; Kaz Kylheku <kaz@kylheku.com>
;; Vancouver, Canada
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;
;; 1. Redistributions of source code must retain the above copyright notice, this
;;    list of conditions and the following disclaimer.
;;
;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;    this list of conditions and the following disclaimer in the documentation
;;    and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
;; OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(defstruct match-vars ()
  vars
  var-exprs

  (:method assignments (me)
    (mapcar (ret ^(set ,@1 ,@2)) me.vars me.var-exprs)))

(defstruct match-guard (match-vars)
  guard-expr)

(defstruct compiled-match (match-vars)
  pattern
  obj-var
  guard-chain
  test-expr

  (:method get-temps (me)
    (mappend .vars me.guard-chain))

  (:method get-vars (me)
    (append me.(get-temps) me.vars))

  (:method wrap-guards (me exp)
    (let ((rev-guard-chain (reverse me.guard-chain))
          (out exp))
      (each ((g rev-guard-chain))
        (set out ^(when ,g.guard-expr
                    (progn
                      ,*g.(assignments)
                      ,out))))
      out)))

(defvar *match-form*)

(defun compile-struct-match (struct-pat obj-var)
  (let* ((required-type (cadr struct-pat))
         (slot-pairs (plist-to-alist (cddr struct-pat)))
         (required-slots [mapcar car slot-pairs])
         (slot-gensyms [mapcar gensym required-slots])
         (slot-patterns [mapcar cdr slot-pairs])
         (slot-matches [mapcar compile-match slot-patterns slot-gensyms])
         (slot-val-exprs [mapcar (ret ^(slot ,obj-var ',@1))
                                 required-slots])
         (guard (new match-guard
                     vars slot-gensyms
                     var-exprs slot-val-exprs
                     guard-expr  ^(subtypep (typeof ,obj-var)
                                            ',required-type))))
    (new compiled-match
         pattern struct-pat
         obj-var obj-var
         guard-chain (cons guard (mappend .guard-chain slot-matches))
         test-expr ^(and ,*(mapcar .test-expr slot-matches))
         vars [mappend .vars slot-matches]
         var-exprs [mappend .var-exprs slot-matches])))

(defun compile-var-match (sym obj-var)
  (or (null sym) (bindable sym)
      (compile-error *match-form* "~s is not a symbol" sym))
  (new compiled-match
       pattern sym
       obj-var obj-var
       test-expr t
       vars (if sym (list sym))
       var-exprs (if sym (list obj-var))))

(defun compile-vec-match (vec-pat obj-var)
  (let* ((elem-gensyms (mapcar (op gensym `elem-@1-`) (range* 0 (len vec-pat))))
         (elem-matches (list-vec [mapcar compile-match vec-pat elem-gensyms]))
         (guard (new match-guard
                     vars elem-gensyms
                     var-exprs (mapcar (ret ^[,obj-var ,@1])
                                        (range* 0 (len vec-pat)))
                     guard-expr ^(and (vectorp ,obj-var)
                                      (eql (len ,obj-var) ,(len vec-pat))))))
    (new compiled-match
         pattern vec-pat
         obj-var obj-var
         guard-chain (cons guard (mappend .guard-chain elem-matches))
         test-expr ^(and ,*(mapcar .test-expr elem-matches))
         vars (mappend .vars elem-matches)
         var-exprs (mappend .var-exprs elem-matches))))

(defun compile-atom-match (atom obj-var)
  (typecase atom
    (vec (compile-vec-match atom obj-var))
    (t (new compiled-match
            pattern atom
            obj-var obj-var
            test-expr ^(equal ,obj-var ',atom)))))

(defun compile-predicate-match (pred-expr obj-var)
  (tree-bind (fun : sym) pred-expr
    (or (null sym) (bindable sym)
        (compile-error *match-form* "~s is not a symbol" sym))
    (let ((var-match (compile-var-match sym obj-var)))
      (set var-match.test-expr ^(,fun ,obj-var))
      var-match)))

(defun compile-cons-structure (cons-pat obj-var)
  (tree-bind (car . cdr) cons-pat
    (let* ((car-gensym (gensym))
           (cdr-gensym (gensym))
           (car-match (compile-match car car-gensym))
           (cdr-match (if (consp cdr)
                        (caseq (car cdr)
                          ((sys:expr sys:var) (compile-match cdr cdr-gensym))
                          (t (compile-cons-structure cdr cdr-gensym)))
                        (compile-atom-match cdr cdr-gensym)))
           (guard (new match-guard
                       vars ^(,car-gensym ,cdr-gensym)
                       var-exprs ^((car ,obj-var) (cdr ,obj-var))
                       guard-expr ^(consp ,obj-var))))
      (new compiled-match
           pattern cons-pat
           obj-var obj-var
           guard-chain (cons guard (append car-match.guard-chain
                                           cdr-match.guard-chain))
           test-expr ^(and ,car-match.test-expr ,cdr-match.test-expr)
           vars (append car-match.vars cdr-match.vars)
           var-exprs (append car-match.var-exprs cdr-match.var-exprs)))))

(defun compile-require-match (exp obj-var)
  (tree-bind (op match condition) exp
    (let ((match (compile-match match obj-var)))
      (set match.test-expr ^(and ,condition ,match.test-expr))
      match)))

(defun compile-let-match (exp obj-var)
  (tree-bind (op sym match) exp
    (or (null sym) (bindable sym)
        (compile-error *match-form* "~s is not a symbol" sym))
    (let ((match (compile-match match obj-var)))
      (push sym match.vars)
      (push obj-var match.var-exprs)
      match)))

(defun compile-all-match (exp obj-var)
  (tree-bind (op match) exp
    (let* ((list-test (if (eq op 'all) 'listp 'consp))
           (item-var (gensym "item-"))
           (cm (compile-match match item-var))
           (all-match-p-var (gensym "all-match-p-"))
           (matched-p-var (gensym "matched-p-"))
           (iter-var (gensym "iter-"))
           (collect-vars [mapcar gensym cm.vars])
           (loop ^(for ((,iter-var ,obj-var)
                        (,all-match-p-var t))
                       ((and ,all-match-p-var ,iter-var) ,all-match-p-var)
                       ((set ,iter-var (cdr ,iter-var)))
                    (let ((,cm.obj-var (car ,iter-var))
                          ,matched-p-var
                          ,*cm.(get-vars))
                      ,cm.(wrap-guards
                            ^(progn ,*cm.(assignments)
                               (if ,cm.test-expr
                                 (progn
                                   (set ,matched-p-var t)
                                   ,*(mapcar (ret ^(push ,@1 ,@2))
                                             cm.vars
                                             collect-vars)))))
                      (unless ,matched-p-var
                        (set ,all-match-p-var nil)))))
           (guard (new match-guard
                       vars (cons all-match-p-var collect-vars)
                       var-exprs (list loop)
                       guard-expr ^(,list-test ,obj-var))))
      (new compiled-match
           pattern exp
           obj-var obj-var
           guard-chain (list guard)
           test-expr all-match-p-var
           vars cm.vars
           var-exprs (mapcar (ret ^(nreverse ,@1)) collect-vars)))))

(defun compile-match (pat : (obj-var (gensym)))
  (cond
    ((consp pat)
       (caseq (car pat)
         (sys:expr
           (let ((exp (cadr pat)))
             (if (consp exp)
               (caseq (car exp)
                 (struct (compile-struct-match exp obj-var))
                 (require (compile-require-match exp obj-var))
                 (let (compile-let-match exp obj-var))
                 (all (compile-all-match exp obj-var))
                 (usr:all* (compile-all-match exp obj-var))
                 (t (compile-predicate-match exp obj-var)))
               (compile-error *match-form*
                              "unrecognized pattern syntax ~s" pat))))
         (sys:var (compile-var-match (cadr pat) obj-var))
         (t (compile-cons-structure pat obj-var))))
    (t (compile-atom-match pat obj-var))))

(defmacro when-match (:form *match-form* pat obj . body)
  (let ((cm (compile-match pat)))
    ^(let ((,cm.obj-var ,obj)
           ,*cm.(get-vars))
       ,cm.(wrap-guards
             ^(progn ,*cm.(assignments)
                (if ,cm.test-expr ,*body))))))
