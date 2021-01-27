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
  pure-vars
  pure-var-exprs

  (:method assignments (me)
    (mapcar (ret ^(set ,@1 ,@2)) me.vars me.var-exprs))

  (:method lets (me)
    (mapcar (ret ^(,@1 ,@2)) me.pure-vars me.pure-var-exprs)))

(defstruct match-guard (match-vars)
  (guard-expr t))

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
                    (alet ,g.(lets)
                      ,*g.(assignments)
                      ,out))))
      out)))

(defstruct var-list ()
  vars

  (:method exists (me sym) (member sym me.vars))
  (:method record (me sym) (push sym me.vars)))

(defvar *match-form*)

(defun compile-struct-match (struct-pat obj-var var-list)
  (mac-param-bind *match-form* (op required-type . pairs) struct-pat
    (let* ((loose-p (not (bindable required-type)))
           (slot-pairs (plist-to-alist pairs))
           (required-slots [mapcar car slot-pairs])
           (slot-gensyms [mapcar gensym required-slots])
           (type-gensym (if loose-p
                          (gensym "type-")))
           (slot-patterns [mapcar cdr slot-pairs])
           (slot-matches [mapcar (lop compile-match var-list)
                                 slot-patterns slot-gensyms])
           (type-match (if loose-p
                         (compile-match required-type type-gensym var-list)))
           (all-matches (if loose-p
                          (cons type-match slot-matches)
                          slot-matches))
           (slot-val-exprs [mapcar (ret ^(slot ,obj-var ',@1)) required-slots])
           (guard0 (if loose-p
                     (new match-guard
                          pure-vars (list type-gensym)
                          pure-var-exprs (list ^(struct-type ,obj-var))
                          guard-expr ^(structp ,obj-var))))
           (guard1 (new match-guard
                        pure-vars slot-gensyms
                        pure-var-exprs slot-val-exprs
                        guard-expr (if loose-p
                                     ^(and ,*(mapcar
                                               (ret ^(slotp ,type-gensym ',@1))
                                               required-slots))
                                     ^(subtypep (typeof ,obj-var)
                                                ',required-type)))))
      (unless loose-p
        (let ((type (find-struct-type required-type)))
          (if type
            (each ((slot required-slots))
              (unless (slotp type slot)
                (compile-defr-warning *match-form* ^(slot . ,slot)
                                      "~s has no slot ~s"
                                      required-type slot)))
            (compile-defr-warning *match-form* ^(struct-type . ,required-type)
                                  "no such struct type: ~s"
                                  required-type))))
      (new compiled-match
           pattern struct-pat
           obj-var obj-var
           guard-chain ^(,*(if guard0 (list guard0)) ,guard1
                         ,*(mappend .guard-chain all-matches))
           test-expr ^(and ,*(mapcar .test-expr all-matches))
           vars [mappend .vars all-matches]
           var-exprs [mappend .var-exprs all-matches]))))

(defun compile-var-match (sym obj-var var-list)
  (or (null sym) (bindable sym)
      (compile-error *match-form* "~s is not a symbol" sym))
  (cond
    ((or (null sym)
         (not var-list.(exists sym)))
       var-list.(record sym)
       (new compiled-match
            pattern sym
            obj-var obj-var
            test-expr t
            vars (if sym (list sym))
            var-exprs (if sym (list obj-var))))
    (t (new compiled-match
            pattern sym
            obj-var obj-var
            test-expr ^(equal ,obj-var ,sym)))))

(defun compile-vec-match (vec-pat obj-var var-list)
  (let* ((elem-gensyms (mapcar (op gensym `elem-@1-`) (range* 0 (len vec-pat))))
         (elem-matches (list-vec [mapcar (lop compile-match var-list)
                                         vec-pat elem-gensyms]))
         (guard (new match-guard
                     pure-vars elem-gensyms
                     pure-var-exprs (mapcar (ret ^[,obj-var ,@1])
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

(defun compile-range-match (rcons-expr obj-var var-list)
  (let ((from (from rcons-expr))
        (to (to rcons-expr)))
    (let* ((from-match (compile-match from (gensym "from") var-list))
           (to-match (compile-match to (gensym "to") var-list))
           (guard (new match-guard
                       guard-expr ^(rangep ,obj-var)
                       pure-vars (list from-match.obj-var to-match.obj-var)
                       pure-var-exprs (list ^(from ,obj-var) ^(to ,obj-var)))))
      (new compiled-match
           pattern rcons-expr
           obj-var obj-var
           guard-chain (cons guard (append from-match.guard-chain
                                           to-match.guard-chain))
           test-expr ^(and ,from-match.test-expr ,to-match.test-expr)
           vars (append from-match.vars to-match.vars)
           var-exprs (append from-match.var-exprs to-match.var-exprs)))))

(defun compile-atom-match (atom obj-var var-list)
  (flet ((compile-as-atom ()
           (new compiled-match
                pattern atom
                obj-var obj-var
                test-expr ^(equal ,obj-var ',atom))))
    (typecase atom
      (vec (if (non-triv-pat-p atom)
             (compile-vec-match atom obj-var var-list)
             (compile-as-atom)))
      (range (if (non-triv-pat-p atom)
               (compile-range-match atom obj-var var-list)
               (compile-as-atom)))
      (t (compile-as-atom)))))

(defun compile-op-match (op-expr obj-var var-list)
  (let ((var-match (compile-var-match nil obj-var var-list)))
    (set var-match.test-expr ^(and ,var-match.test-expr [,op-expr ,obj-var]))
    var-match))

(defun compile-dwim-predicate-match (pred-expr obj-var var-list)
  (mac-param-bind *match-form* (dwim fun : sym pat) pred-expr
    (let ((var-match (compile-var-match sym obj-var var-list)))
      (if pat
        (let* ((pat-match (if (symbolp pat)
                            (compile-var-match pat (gensym) var-list)
                            (compile-match pat (gensym) var-list)))
               (guard (new match-guard
                           vars (list pat-match.obj-var)
                           var-exprs (list ^[,fun ,obj-var]))))
          (new compiled-match
               pattern pred-expr
               obj-var obj-var
               guard-chain (cons guard (append var-match.guard-chain
                                               pat-match.guard-chain))
               vars (append var-match.vars pat-match.vars)
               var-exprs (append var-match.var-exprs pat-match.var-exprs)
               test-expr ^(and ,var-match.test-expr
                               ,pat-match.test-expr
                               ,pat-match.obj-var)))
        (progn
          (set var-match.test-expr ^(and ,var-match.test-expr [,fun ,obj-var]))
          var-match)))))

(defun compile-predicate-match (pred-expr obj-var var-list)
  (mac-param-bind *match-form* (fun : sym) pred-expr
    (or (null sym) (bindable sym)
        (compile-error *match-form* "~s is not a symbol" sym))
    (let ((var-match (compile-var-match sym obj-var var-list)))
      (set var-match.test-expr ^(and ,var-match.test-expr (,fun ,obj-var)))
      var-match)))

(defun compile-cons-structure (cons-pat obj-var var-list)
  (mac-param-bind *match-form* (car . cdr) cons-pat
    (let* ((car-gensym (gensym))
           (cdr-gensym (gensym))
           (car-match (compile-match car car-gensym var-list))
           (cdr-match (if (consp cdr)
                        (caseq (car cdr)
                          ((sys:expr sys:var) (compile-match cdr cdr-gensym
                                                             var-list))
                          (t (compile-cons-structure cdr cdr-gensym var-list)))
                        (compile-atom-match cdr cdr-gensym var-list)))
           (guard (new match-guard
                       pure-vars ^(,car-gensym ,cdr-gensym)
                       pure-var-exprs ^((car ,obj-var) (cdr ,obj-var))
                       guard-expr ^(consp ,obj-var))))
      (new compiled-match
           pattern cons-pat
           obj-var obj-var
           guard-chain (cons guard (append car-match.guard-chain
                                           cdr-match.guard-chain))
           test-expr ^(and ,car-match.test-expr ,cdr-match.test-expr)
           vars (append car-match.vars cdr-match.vars)
           var-exprs (append car-match.var-exprs cdr-match.var-exprs)))))

(defun compile-require-match (exp obj-var var-list)
  (mac-param-bind *match-form* (op match . conditions) exp
    (let ((match (compile-match match obj-var var-list)))
      (set match.test-expr ^(and ,match.test-expr ,*conditions))
      match)))

(defun compile-let-match (exp obj-var var-list)
  (mac-param-bind *match-form* (op sym pat) exp
    (let ((var-match (compile-var-match sym obj-var var-list))
          (pat-match (compile-match pat obj-var var-list)))
      (new compiled-match
           pattern exp
           obj-var obj-var
           guard-chain (append var-match.guard-chain
                               pat-match.guard-chain)
           test-expr ^(and ,var-match.test-expr ,pat-match.test-expr)
           vars (append var-match.vars pat-match.vars)
           var-exprs (append var-match.var-exprs pat-match.var-exprs)))))

(defun compile-loop-match (exp obj-var var-list)
  (mac-param-bind *match-form* (op match) exp
    (let* ((no-vac-p (memq op '(coll usr:all*)))
           (some-p (eq op 'some))
           (coll-p (eq op 'coll))
           (item-var (gensym "item-"))
           (in-vars var-list.vars)
           (cm (compile-match match item-var var-list))
           (loop-success-p-var (gensym "loop-success-p-"))
           (loop-continue-p-var (gensym "loop-terminate-p"))
           (loop-iterated-var (if no-vac-p (gensym "loop-iterated-p")))
           (matched-p-var (gensym "matched-p-"))
           (iter-var (gensym "iter-"))
           (cm-vars cm.(get-vars))
           (collect-vars (diff cm-vars in-vars))
           (collect-gens [mapcar gensym collect-vars])
           (loop ^(for ((,iter-var (iter-begin ,obj-var))
                        (,loop-continue-p-var t)
                        ,*(if no-vac-p ^((,loop-iterated-var nil))))
                       ((and ,loop-continue-p-var (iter-more ,iter-var))
                        ,(cond
                           (some-p ^(not ,loop-continue-p-var))
                           (no-vac-p ^(and ,loop-iterated-var
                                         ,loop-continue-p-var))
                           (t loop-continue-p-var)))
                       ((set ,iter-var (iter-step ,iter-var)))
                    (let ((,cm.obj-var (iter-item ,iter-var))
                          ,matched-p-var
                          ,*(if some-p cm.(get-temps) cm-vars))
                      ,cm.(wrap-guards
                            ^(progn ,*cm.(assignments)
                               (if ,cm.test-expr
                                 (progn
                                   (set ,matched-p-var t)
                                   ,*(if no-vac-p
                                       ^((set ,loop-iterated-var t)))
                                   ,*(unless some-p
                                       (mapcar (ret ^(push ,@1 ,@2))
                                               collect-vars
                                               collect-gens))))))
                      ,(unless coll-p ^(,(if some-p 'when 'unless) ,matched-p-var
                         (set ,loop-continue-p-var nil))))))
           (guard (new match-guard
                       vars (append cm-vars (unless some-p collect-gens))
                       guard-expr ^(seqp ,obj-var))))
      (new compiled-match
           pattern exp
           obj-var obj-var
           guard-chain (list guard)
           vars (list loop-success-p-var)
           var-exprs (list loop)
           test-expr (if some-p
                       loop-success-p-var
                       ^(when ,loop-success-p-var
                          ,*(mapcar (ret ^(set ,@1 (nreverse ,@2)))
                                    collect-vars collect-gens)
                          t))))))

(defun compile-parallel-match (par-pat obj-var var-list)
  (mac-param-bind *match-form* (op . pats) par-pat
    (let* ((var-lists (mapcar (ret (copy var-list)) pats))
           (par-matches (mapcar (op compile-match @1 obj-var @2)
                                pats var-lists))
           (all-vars (uniq (mappend .(get-vars) par-matches))))
      (flet ((submatch-fun (pm)
               ^(let ,pm.(get-temps)
                  ,pm.(wrap-guards
                        ^(progn ,*pm.(assignments)
                           (when ,pm.test-expr
                             ,*(if (eq op 'or)
                                 (mapcar (ret ^(set ,@1 nil))
                                         (diff all-vars pm.(get-vars))))
                             t))))))
        (let ((guard (new match-guard
                          vars all-vars)))
          (new compiled-match
               pattern par-pat
               obj-var obj-var
               guard-chain (list guard)
               test-expr ^(,op ,*[mapcar submatch-fun par-matches])))))))

(defun compile-not-match (pattern obj-var var-list)
  (mac-param-bind *match-form* (op pattern) pattern
    (let* ((pm (compile-match pattern obj-var var-list))
           (guard (new match-guard
                       guard-expr ^(not (let ,pm.(get-vars)
                                          ,pm.(wrap-guards
                                                ^(progn ,*pm.(assignments)
                                                   (when ,pm.test-expr
                                                     t))))))))
      (new compiled-match
           pattern pattern
           obj-var obj-var
           guard-chain (list guard)
           test-expr t
           vars nil))))

(defun compile-hash-match (hash-expr obj-var var-list)
  (tree-bind (op . pairs) hash-expr
    (let* ((hash-alist-var (gensym "hash-alist-"))
           (hash-alt-val ^',(gensym "alt"))
           (need-alist-p nil)
           (hash-matches
             (collect-each ((pair pairs))
               (mac-param-bind *match-form* (key val) pair
                 (let ((key-pat-p (non-triv-pat-p key))
                       (val-pat-p (non-triv-pat-p val))
                       (key-var-sym (var-pat-p key)))
                   (cond
                     ((and key-var-sym var-list.(exists key-var-sym))
                       (let ((vm (compile-match val (gensym "val") var-list))
                             (val-sym (gensym "val")))
                         (push
                           (new match-guard
                                guard-expr ^(neq ,vm.obj-var ,hash-alt-val))
                           vm.guard-chain)
                         (push vm.obj-var vm.vars)
                         (push ^(gethash ,obj-var ,key-var-sym
                                         ,hash-alt-val) vm.var-exprs)
                         (set vm.test-expr ^(and (neq ,vm.obj-var ,hash-alt-val)
                                                 ,vm.test-expr))
                       vm))
                     ((and key-pat-p val-pat-p)
                      (set need-alist-p t)
                      (compile-match ^@(coll (,key . ,val))
                                     hash-alist-var var-list))
                     (key-pat-p
                       (let ((km (compile-match key (gensym "keys")
                                                var-list)))
                         (push
                           (new match-guard
                                pure-vars (list km.obj-var)
                                pure-var-exprs ^((hash-keys-of ,obj-var ',val)))
                           km.guard-chain)
                         km))
                     (t
                       (let ((vm (compile-match val (gensym "val") var-list)))
                         (push
                           (new match-guard
                                guard-expr ^(neq ,vm.obj-var ,hash-alt-val))
                           vm.guard-chain)
                         (push
                           (new match-guard
                                pure-vars (list vm.obj-var)
                                pure-var-exprs ^((gethash ,obj-var ',key, hash-alt-val)))
                           vm.guard-chain)
                         vm)))))))
           (guard (new match-guard
                       guard-expr ^(hashp ,obj-var)
                       vars (if need-alist-p
                              (list hash-alist-var))
                       var-exprs (if need-alist-p
                                   (list ^(hash-alist ,obj-var))))))
      (new compiled-match
           pattern hash-expr
           obj-var obj-var
           guard-chain (cons guard (mappend .guard-chain hash-matches))
           test-expr ^(and ,*(mapcar .test-expr hash-matches))
           vars (mappend .vars hash-matches)
           var-exprs (mappend .var-exprs hash-matches)))))

(defun compile-match (pat : (obj-var (gensym)) (var-list (new var-list)))
  (cond
    ((consp pat)
       (caseq (car pat)
         (sys:expr
           (let ((exp (cadr pat)))
             (if (consp exp)
               (caseq (car exp)
                 (struct (compile-struct-match exp obj-var var-list))
                 (require (compile-require-match exp obj-var var-list))
                 (let (compile-let-match exp obj-var var-list))
                 (all (compile-loop-match exp obj-var var-list))
                 (usr:all* (compile-loop-match exp obj-var var-list))
                 (some (compile-loop-match exp obj-var var-list))
                 (coll (compile-loop-match exp obj-var var-list))
                 (or (compile-parallel-match exp obj-var var-list))
                 (and (compile-parallel-match exp obj-var var-list))
                 (not (compile-not-match exp obj-var var-list))
                 (op (compile-op-match exp obj-var var-list))
                 (hash (compile-hash-match exp obj-var var-list))
                 (rcons (compile-range-match exp obj-var var-list))
                 (dwim (compile-dwim-predicate-match exp obj-var var-list))
                 (t (compile-predicate-match exp obj-var var-list)))
               (compile-error *match-form*
                              "unrecognized pattern syntax ~s" pat))))
         (sys:var (compile-var-match (cadr pat) obj-var var-list))
         (t (if (non-triv-pat-p pat)
              (compile-cons-structure pat obj-var var-list)
              (compile-atom-match pat obj-var var-list)))))
    (t (compile-atom-match pat obj-var var-list))))

(defmacro when-match (:form *match-form* pat obj . body)
  (let ((cm (compile-match pat)))
    ^(alet ((,cm.obj-var ,obj))
       (let ,cm.(get-vars)
         ,cm.(wrap-guards
               ^(progn ,*cm.(assignments)
                  (when ,cm.test-expr ,*body)))))))

(defmacro if-match (:form *match-form* pat obj then : else)
  (let ((cm (compile-match pat))
        (match-p (gensym "match-p-"))
        (result (gensym "result-")))
    ^(alet ((,cm.obj-var ,obj))
       (let* (,match-p ,*cm.(get-vars)
              (,result ,cm.(wrap-guards
                             ^(progn ,*cm.(assignments)
                                (when ,cm.test-expr
                                  (set ,match-p t)
                                  ,then)))))
         (if ,match-p ,result ,else)))))

(defmacro match-case (:form *match-form* obj . clauses)
  (unless [all clauses [andf proper-listp [chain len plusp]]]
    (compile-error *match-form* "bad clause syntax"))
  (let* ((flag (gensym "flag-"))
         (result (gensym "result-"))
         (clause-matches [mapcar (op compile-match (car @1) obj) clauses])
         (clause-code (collect-each ((cl clauses)
                                     (cm clause-matches)
                                     (i 0))
                        (mac-param-bind *match-form* (match . forms) cl
                          ^(unless ,(unless (zerop i) flag)
                             (let (,*cm.(get-vars))
                               (set ,result ,cm.(wrap-guards
                                                  ^(progn ,*cm.(assignments)
                                                     (when ,cm.test-expr
                                                       (set ,flag t)
                                                       ,*forms))))))))))
    ^(let (,flag ,result)
       ,*clause-code
       ,result)))

(defmacro lambda-match (. clauses)
  (with-gensyms (args)
    ^(lambda (. ,args)
       (match-case ,args ,*clauses))))

(defmacro defun-match (name . clauses)
  (with-gensyms (args)
    ^(defun ,name (. ,args)
       (match-case ,args ,*clauses))))

(defun non-triv-pat-p (syntax) t)

(defun non-triv-pat-p (syntax)
  (match-case syntax
    ((@(op eq 'sys:expr) (@(bindable) . @nil)) t)
    ((@(op eq 'sys:var) @(bindable) . @nil) t)
    ((@pat . @rest) (or (non-triv-pat-p pat)
                        (non-triv-pat-p rest)))
    (#R(@from @to) (or (non-triv-pat-p from)
                       (non-triv-pat-p to)))
    (@(some @(non-triv-pat-p)) t)))

(defun var-pat-p (syntax)
  (when-match (@(op eq 'sys:var) @(bindable sym) . @nil) syntax
    sym))
