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

(defvar *match-form*)

(defvar *match-macro* (hash))

(defstruct match-guard ()
  temps
  vars
  var-exprs
  pure-temps
  pure-temp-exprs
  (guard-expr t)
  (test-expr t)

  (:method assignments (me)
    (mapcar (op list 'set) me.vars me.var-exprs))

  (:method lets (me)
    (zip me.pure-temps me.pure-temp-exprs))

  (:method wrap-expr (g exp)
    (let ((lets g.(lets))
          (temps g.temps))
      (if (neq t g.test-expr)
        (set exp ^(if ,g.test-expr ,exp)))
      (cond
        ((and lets temps)
         (set exp ^(alet ,lets
                     (let ,temps
                       ,*g.(assignments)
                       ,exp))))
        (lets
          (set exp ^(alet ,lets
                      ,*g.(assignments)
                      ,exp)))
        (temps
          (set exp ^(let ,temps
                      ,*g.(assignments)
                      ,exp)))
        (t
          (set exp ^(progn ,*g.(assignments)
                      ,exp))))
      (when (neq t g.guard-expr)
        (set exp ^(if ,g.guard-expr ,exp)))
      exp)))

(defstruct guard-disjunction ()
  guard-chains
  sub-patterns
  all-vars

  (:method wrap-expr (g exp)
    (let* ((vars [mapcar get-vars g.guard-chains])
           (back-vars (cons nil
                            (reverse
                              [mapcar (ap append) (conses (reverse vars))])))
           (branches (collect-each ((gc g.guard-chains)
                                    (v vars)
                                    (bv back-vars))
                       ^(progn
                          (set ,*(mappend (ret ^(,@1 nil)) (diff bv v)))
                          ,(reduce-right (umeth wrap-expr) gc t)))))
      (set exp ^(when (or ,*branches)
                  ,exp))
      exp)))

(defstruct compiled-match ()
  pattern
  obj-var
  guard-chain

  (:method get-vars (me)
    (uniq (get-vars me.guard-chain)))

  (:method wrap-guards (me . forms)
    (reduce-right (umeth wrap-expr) me.guard-chain ^(progn ,*forms)))

  (:method add-guard-pre (me guard)
    (push guard me.guard-chain))

  (:method add-guards-pre (me . guards)
    (set me.guard-chain
         (append guards
                 me.guard-chain)))

  (:method add-guards-post (me . guards)
    (set me.guard-chain
         (append me.guard-chain
                 guards))))

(defstruct var-list ()
  vars
  menv

  (:method exists (me sym) (or (member sym me.vars)
                               (lexical-var-p me.menv sym)
                               (boundp sym)))
  (:method record (me sym) (push sym me.vars))
  (:method merge (me copy) (each ((v copy.vars)) (pushnew v me.vars))))

(defun get-vars (guard-chain)
  (append-each ((g guard-chain))
    (typecase g
      (match-guard
        g.vars)
      (guard-disjunction
        (append-each ((gc g.guard-chains)) (get-vars gc)))
      (t (compile-error *match-form*
                        "internal error: bad guard ~s" g)))))

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
           (slot-val-exprs [mapcar (ret ^(slot ,obj-var ',@1)) required-slots])
           (guard0 (if loose-p
                     (list (new match-guard
                                pure-temps (list type-gensym)
                                pure-temp-exprs (list ^(struct-type ,obj-var))
                                guard-expr ^(structp ,obj-var)))))
           (guard1 (list (new match-guard
                              pure-temps slot-gensyms
                              pure-temp-exprs slot-val-exprs
                              guard-expr (if loose-p
                                           ^(and ,*(mapcar
                                                     (ret ^(slotp ,type-gensym
                                                                  ',@1))
                                               required-slots))
                                     ^(subtypep (typeof ,obj-var)
                                                ',required-type))))))
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
           guard-chain (append guard0
                               type-match.?guard-chain
                               guard1
                               (mappend .guard-chain slot-matches))))))

(defun compile-var-match (sym obj-var var-list)
  (cond
    ((null sym)
       (new compiled-match
            obj-var obj-var))
    ((not (bindable sym))
       (compile-error *match-form* "~s is not a bindable symbol" sym))
    ((not var-list.(exists sym))
       var-list.(record sym)
       (new compiled-match
            pattern sym
            obj-var obj-var
            guard-chain (if sym (list (new match-guard
                                           vars (list sym)
                                           var-exprs (list obj-var))))))
    (t (new compiled-match
            pattern sym
            obj-var obj-var
            guard-chain (list (new match-guard
                                   guard-expr ^(equal ,obj-var ,sym)))))))

(defun compile-new-var-match (sym obj-var var-list)
  (cond
    ((null sym)
       (new compiled-match
            obj-var obj-var))
    ((not (bindable sym))
       (compile-error *match-form* "~s is not a bindable symbol" sym))
    (t var-list.(record sym)
       (new compiled-match
            pattern sym
            obj-var obj-var
            guard-chain (if sym (list (new match-guard
                                           vars (list sym)
                                           var-exprs (list obj-var))))))))

(defun compile-vec-match (vec-pat obj-var var-list)
  (let* ((elem-gensyms (mapcar (op gensym `elem-@1-`) (range* 0 (len vec-pat))))
         (elem-exprs (mapcar (ret ^[,obj-var ,@1]) (range* 0 (len vec-pat))))
         (elem-matches (list-vec [mapcar (lop compile-match var-list)
                                         vec-pat elem-gensyms])) 
         (pruned-triple (multi (op keep-if .guard-chain @1 third)
                               elem-gensyms
                               elem-exprs
                               elem-matches))
         (guard (new match-guard
                     pure-temps (first pruned-triple)
                     pure-temp-exprs (second pruned-triple)
                     guard-expr ^(and (vectorp ,obj-var)
                                      (eql (len ,obj-var) ,(len vec-pat))))))
    (new compiled-match
         pattern vec-pat
         obj-var obj-var
         guard-chain (cons guard (mappend .guard-chain elem-matches)))))

(defun compile-range-match (range-expr obj-var var-list)
  (let ((from (from range-expr))
        (to (to range-expr)))
    (let* ((from-match (compile-match from (gensym "from") var-list))
           (to-match (compile-match to (gensym "to") var-list))
           (guard (new match-guard
                       guard-expr ^(rangep ,obj-var)
                       pure-temps (list from-match.obj-var to-match.obj-var)
                       pure-temp-exprs (list ^(from ,obj-var) ^(to ,obj-var)))))
      (new compiled-match
           pattern range-expr
           obj-var obj-var
           guard-chain (cons guard (append from-match.guard-chain
                                           to-match.guard-chain))))))

(defun compile-atom-match (atom obj-var var-list)
  (flet ((compile-as-atom ()
           (new compiled-match
                pattern atom
                obj-var obj-var
                guard-chain (list (new match-guard
                                       guard-expr ^(equal ,obj-var ',atom))))))
    (typecase atom
      (vec (if (non-triv-pat-p atom)
             (compile-vec-match atom obj-var var-list)
             (compile-as-atom)))
      (range (if (non-triv-pat-p atom)
               (compile-range-match atom obj-var var-list)
               (compile-as-atom)))
      (t (compile-as-atom)))))

(defun compile-predicate-match (exp obj-var var-list)
  (let ((head (car exp)))
    (if (and (consp head) (eq (car head) 'sys:var))
      (tree-case exp
        (((sv rvar) (op . args))
         (let* ((avar
                  (condlet
                    (((vm (member-if [andf consp (op eq (car @1) 'sys:var)]
                                     args)))
                     (let ((sym (cadar vm)))
                       (set args (append (ldiff args vm)
                                         (list sym)
                                         (cdr vm)))
                       sym))
                    (((vm (memq 'sys:var args)))
                     (let ((sym (cadr vm)))
                       (set args (append (ldiff args vm) sym))
                       sym))))
                (res-var (gensym "res-"))
                (arg-var (if avar avar (gensym "obj-"))))
           (unless avar
             (set args (append args (list arg-var))))
           (let* ((guard (new match-guard
                              pure-temps (list res-var)
                              pure-temp-exprs ^((alet ((,arg-var ,obj-var))
                                                  (,op ,*args)))
                              test-expr res-var))
                  (avar-match (compile-var-match avar obj-var var-list))
                  (rvar-match (compile-var-match rvar res-var var-list)))
             (new compiled-match
                  pattern exp
                  obj-var obj-var
                  guard-chain  (append avar-match.guard-chain
                                       (list guard)
                                       rvar-match.guard-chain)))))
        (els (compile-error *match-form* "invalid predicate syntax: ~s" exp)))
      (compile-predicate-match (list '@nil exp) obj-var var-list))))

(defun compile-cons-structure (cons-pat obj-var var-list)
  (mac-param-bind *match-form* (car . cdr) cons-pat
    (let* ((car-gensym (gensym))
           (cdr-gensym (gensym))
           (car-match (compile-match car car-gensym var-list))
           (cdr-match (if (consp cdr)
                        (caseq (car cdr)
                          ((sys:expr sys:var sys:quasi)
                             (compile-match cdr cdr-gensym var-list))
                          (t (compile-cons-structure cdr cdr-gensym var-list)))
                        (compile-atom-match cdr cdr-gensym var-list)))
           (guard (new match-guard
                       pure-temps (append (if car-match.guard-chain
                                            (list car-gensym))
                                          (if cdr-match.guard-chain
                                            (list cdr-gensym)))
                       pure-temp-exprs (append (if car-match.guard-chain
                                                 ^((car ,obj-var)))
                                               (if cdr-match.guard-chain
                                                 ^((cdr ,obj-var))))
                       guard-expr ^(consp ,obj-var))))
      (new compiled-match
           pattern cons-pat
           obj-var obj-var
           guard-chain (cons guard (append car-match.guard-chain
                                           cdr-match.guard-chain))))))

(defun compile-require-match (exp obj-var var-list)
  (mac-param-bind *match-form* (op match . conditions) exp
    (let ((match (compile-match match obj-var var-list)))
      match.(add-guards-post (new match-guard
                                  guard-expr ^(and ,*conditions)))
      match)))

(defun compile-as-match (exp obj-var var-list)
  (mac-param-bind *match-form* (op sym pat) exp
    (let ((var-match (compile-new-var-match sym obj-var var-list))
          (pat-match (compile-match pat obj-var var-list)))
      (new compiled-match
           pattern exp
           obj-var obj-var
           guard-chain (append var-match.guard-chain
                               pat-match.guard-chain)))))

(defun compile-with-match (exp obj-var var-list)
  (tree-case exp
    ((op main-pat side-pat-var side-expr)
       (let* ((side-var (gensym))
              (side-pat (if (or (null side-pat-var) (bindable side-pat-var))
                          ^(sys:var ,side-pat-var)
                          side-pat-var))
              (main-match (compile-match main-pat obj-var var-list))
              (side-match (compile-match side-pat side-var var-list))
              (guard (new match-guard
                          pure-temps (list side-var)
                          pure-temp-exprs (list side-expr))))
         (new compiled-match
              pattern exp
              obj-var obj-var
              guard-chain (append main-match.guard-chain
                                  (list guard)
                                  side-match.guard-chain))))
    ((op side-pat-var side-expr)
       (compile-with-match ^(,op @nil ,side-pat-var ,side-expr) obj-var var-list))
    (x (compile-error *match-form* "bad syntax: ~s" exp))))

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
                          ,*(unless some-p cm-vars))
                      ,cm.(wrap-guards
                            ^(progn
                               (set ,matched-p-var t)
                               ,*(if no-vac-p
                                   ^((set ,loop-iterated-var t)))
                               ,*(unless some-p
                                   (mapcar (ret ^(push ,@1 ,@2))
                                           collect-vars
                                           collect-gens))))
                      ,(unless coll-p ^(,(if some-p 'when 'unless)
                                         ,matched-p-var
                                         (set ,loop-continue-p-var nil))))))
           (guard0 (new match-guard
                        vars cm-vars
                        temps (unless some-p collect-gens)
                        guard-expr ^(seqp ,obj-var)))
           (guard1 (new match-guard
                        vars (list loop-success-p-var)
                        var-exprs (list loop)
                        test-expr (if some-p
                                    loop-success-p-var
                                    ^(when ,loop-success-p-var
                                       ,*(mapcar (ret ^(set ,@1 (nreverse ,@2)))
                                                 collect-vars collect-gens)
                                       t)))))
      (new compiled-match
           pattern exp
           obj-var obj-var
           guard-chain (list guard0 guard1)))))

(defun compile-or-match (par-pat obj-var var-list)
  (mac-param-bind *match-form* (op . pats) par-pat
    (let* ((var-lists (mapcar (ret (copy var-list)) pats))
           (par-matches (mapcar (op compile-match @1 obj-var @2)
                                pats var-lists))
           (dj-guard (new guard-disjunction
                          guard-chains (mapcar .guard-chain par-matches)
                          sub-patterns par-matches)))
      (each ((vl var-lists))
        var-list.(merge vl))
      (new compiled-match
           pattern par-pat
           obj-var obj-var
           guard-chain (list dj-guard)))))

(defun compile-and-match (and-pat obj-var var-list)
  (mac-param-bind *match-form* (op . pats) and-pat
    (let* ((par-matches (mapcar (lop compile-match obj-var var-list) pats)))
      (new compiled-match
           pattern and-pat
           obj-var obj-var
           guard-chain (mappend .guard-chain par-matches)))))

(defun compile-not-match (pattern obj-var var-list)
  (mac-param-bind *match-form* (op pattern) pattern
    (let* ((pm (compile-match pattern obj-var var-list))
           (guard (new match-guard
                       guard-expr ^(not (let ,pm.(get-vars)
                                          ,pm.(wrap-guards t))))))
      (new compiled-match
           pattern pattern
           obj-var obj-var
           guard-chain (list guard)))))

(defun compile-hash-match (hash-expr obj-var var-list)
  (mac-param-bind *match-form* (op . pairs) hash-expr
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
                         vm.(add-guards-pre
                              (new match-guard
                                   vars (list vm.obj-var)
                                   var-exprs ^((gethash ,obj-var ,key-var-sym
                                                        ,hash-alt-val))
                                   test-expr ^(neq ,vm.obj-var
                                                   ,hash-alt-val)))
                         vm))
                     ((and key-pat-p val-pat-p)
                      (set need-alist-p t)
                      (compile-match ^@(coll (,key . ,val))
                                     hash-alist-var var-list))
                     (key-pat-p
                       (let ((km (compile-match key (gensym "keys")
                                                var-list)))
                         km.(add-guards-pre
                              (new match-guard
                                   pure-temps (list km.obj-var)
                                   pure-temp-exprs ^((hash-keys-of ,obj-var
                                                       ',val))))
                         km))
                     (t
                       (let ((vm (compile-match val (gensym "val") var-list)))
                         vm.(add-guards-pre
                              (new match-guard
                                   pure-temps (list vm.obj-var)
                                   pure-temp-exprs ^((gethash ,obj-var ',key,
                                                              hash-alt-val))
                                   test-expr ^(neq ,vm.obj-var ,hash-alt-val)))
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
           guard-chain (cons guard (mappend .guard-chain hash-matches))))))

(defun compile-scan-match (scan-syntax obj-var var-list)
  (mac-param-bind *match-form* (op pattern) scan-syntax
    (with-gensyms (iter found-p cont-p success-p)
      (let* ((cm (compile-match pattern iter var-list))
             (loop ^(for ((,iter ,obj-var) (,cont-p t) ,found-p)
                         (,cont-p ,found-p)
                         ((cond
                            ((null ,cont-p))
                            ((consp ,iter) (set ,iter (cdr ,iter)))
                            (t (zap ,cont-p))))
                       ,cm.(wrap-guards ^(set ,found-p t ,cont-p nil))))
             (guard (new match-guard
                         vars (cons success-p cm.(get-vars))
                         var-exprs (list loop)
                         test-expr success-p)))
      (new compiled-match
           pattern scan-syntax
           obj-var obj-var
           guard-chain (list guard))))))

(defun compile-exprs-match (exprs-syntax uexprs var-list)
  (let ((upats (cdr exprs-syntax))
        (utemps (mapcar (ret (gensym)) uexprs)))
    (tree-bind (pats temps exprs) (multi-sort (list upats utemps uexprs)
                                              [list less]
                                              [list non-triv-pat-p])
      (let* ((matches (mapcar (op compile-match @1 @2 var-list)
                              pats temps)))
        (new compiled-match
             pattern exprs-syntax
             obj-var nil
             guard-chain (cons (new match-guard
                                    pure-temps utemps
                                    pure-temp-exprs uexprs)
                               (mappend .guard-chain matches)))))))

(defun compile-match (pat : (obj-var (gensym)) (var-list (new var-list)))
  (cond
    ((consp pat)
       (caseq (car pat)
         (sys:expr
           (let ((exp (cadr pat)))
             (if (consp exp)
               (let ((op (car exp)))
                 (caseq op
                   (struct (compile-struct-match exp obj-var var-list))
                   (require (compile-require-match exp obj-var var-list))
                   (usr:as (compile-as-match exp obj-var var-list))
                   (usr:with (compile-with-match exp obj-var var-list))
                   (all (compile-loop-match exp obj-var var-list))
                   (usr:all* (compile-loop-match exp obj-var var-list))
                   (some (compile-loop-match exp obj-var var-list))
                   (coll (compile-loop-match exp obj-var var-list))
                   (or (compile-or-match exp obj-var var-list))
                   (and (compile-and-match exp obj-var var-list))
                   (not (compile-not-match exp obj-var var-list))
                   (hash (compile-hash-match exp obj-var var-list))
                   (usr:scan (compile-scan-match exp obj-var var-list))
                   (exprs (compile-exprs-match exp obj-var var-list))
                   (t (iflet ((xfun [*match-macro* op]))
                        (let* ((var-env (make-env (mapcar (lop cons
                                                               'sys:special)
                                                          var-list.vars)
                                                  nil var-list.menv))
                               (xexp [xfun exp var-env]))
                          (if (neq xexp exp)
                            (compile-match xexp obj-var var-list)
                            (compile-predicate-match exp obj-var var-list)))
                        (compile-predicate-match exp obj-var var-list)))))
               (compile-error *match-form*
                              "unrecognized pattern syntax ~s" pat))))
         (sys:var (compile-var-match (cadr pat) obj-var var-list))
         (sys:quasi (compile-match (expand-quasi-match (cdr pat) var-list)
                                   obj-var var-list))
         (t (if (non-triv-pat-p pat)
              (compile-cons-structure pat obj-var var-list)
              (compile-atom-match pat obj-var var-list)))))
    (t (compile-atom-match pat obj-var var-list))))

(defun get-var-list (env)
  (new var-list menv env))

(defmacro when-match (:form *match-form* :env e pat obj . body)
  (let ((cm (compile-match pat : (get-var-list e))))
    ^(alet ((,cm.obj-var ,obj))
       (let ,cm.(get-vars)
         ,cm.(wrap-guards . body)))))

(defmacro if-match (:form *match-form* :env e pat obj then : else)
  (let ((cm (compile-match pat : (get-var-list e)))
        (match-p (gensym "match-p-"))
        (result (gensym "result-")))
    ^(alet ((,cm.obj-var ,obj))
       (let* (,result ,*cm.(get-vars))
         (if ,cm.(wrap-guards
                   ^(set ,result ,then)
                   t)
           ,result
           ,else)))))

(defmacro match-case (:form *match-form* :env e obj . clauses)
  (unless [all clauses [andf proper-listp [chain len plusp]]]
    (compile-error *match-form* "bad clause syntax"))
  (let* ((matched-p-temp (gensym "matched-p-"))
         (result-temp (gensym "result-"))
         (objvar (gensym "obj-"))
         (var-list (get-var-list e))
         (clause-matches [mapcar (op compile-match (car @1)
                                     objvar (copy var-list))
                                 clauses])
         (nclauses (len clauses))
         (clause-code (collect-each ((cl clauses)
                                     (cm clause-matches))
                        (mac-param-bind *match-form* (match . forms) cl
                          ^(let (,*cm.(get-vars))
                             ,cm.(wrap-guards ^(set ,result-temp
                                                    (progn ,*forms))
                                              t))))))
    ^(alet ((,objvar ,obj))
       (let (,result-temp)
         (or ,*clause-code)
         ,result-temp))))

(defmacro when-exprs-match (:form *match-form* :env e pats exprs . forms)
  (let ((em (compile-match ^@(exprs ,*pats) exprs (get-var-list e))))
    ^(let* (,*em.(get-vars))
       ,em.(wrap-guards . forms))))

(defstruct lambda-clause ()
  orig-syntax
  fixed-patterns
  variadic-pattern
  nfixed
  forms

  (:postinit (me)
    (set me.nfixed (len me.fixed-patterns))))

(defun parse-lambda-match-clause (clause)
  (mac-param-bind *match-form* (args . body) clause
    (cond
      ((atom args) (new lambda-clause
                          orig-syntax args
                          variadic-pattern args
                          forms body))
      ((proper-list-p args)
       (let* ((vpos (pos-if (lop meq 'sys:expr 'sys:var 'sys:quasi) args)))
         (tree-bind (fixed-pats . variadic-pat) (split args vpos)
           (new lambda-clause
                orig-syntax args
                fixed-patterns fixed-pats
                variadic-pattern (car variadic-pat)
                forms body))))
      (t (new lambda-clause
              orig-syntax args
              fixed-patterns (butlast args 0)
              variadic-pattern (last args 0)
              forms body)))))

(defun expand-lambda-match (clauses)
  (let* ((parsed-clauses [mapcar parse-lambda-match-clause clauses])
         (max-args (or [find-max parsed-clauses : .nfixed].?nfixed 0))
         (min-args (or [find-min parsed-clauses : .nfixed].?nfixed 0))
         (variadic [some parsed-clauses .variadic-pattern])
         (fix-arg-temps (mapcar (op gensym `arg-@1`)
                                (range* 0 min-args)))
         (opt-arg-temps (mapcar (op gensym `arg-@1`)
                                (range* min-args max-args)))
         (rest-temp (if variadic (gensym `rest`)))
         (present-p-temps (mapcar (op gensym `have-@1`)
                                  (range* min-args max-args)))
         (arg-temps (append fix-arg-temps opt-arg-temps))
         (present-vec (vec-list (append (repeat '(t) min-args)
                                        present-p-temps)))
         (result-temp (gensym "result"))
         (nclauses (len parsed-clauses))
         (ex-clauses (collect-each ((pc parsed-clauses))
                       (let* ((vp pc.variadic-pattern)
                              (exp ^(when-exprs-match
                                      (,*pc.fixed-patterns
                                        ,*(if vp (list vp)))
                                      (,*[arg-temps 0..pc.nfixed]
                                        ,*(if vp
                                            ^((list* ,*[arg-temps pc.nfixed..:]
                                                     ,rest-temp))))
                                      (set ,result-temp (progn ,*pc.forms))
                                      t)))
                         (sys:set-macro-ancestor exp pc.orig-syntax)
                         (when (> pc.nfixed min-args)
                           (set exp ^(when ,[present-vec (pred pc.nfixed)]
                                        ,exp)))
                         (when (< pc.nfixed max-args)
                           (set exp ^(unless ,[present-vec pc.nfixed]
                                        ,exp)))
                         (when (and variadic (not vp) (= pc.nfixed max-args))
                           (set exp ^(unless ,rest-temp
                                       ,exp)))
                         exp))))
    ^(lambda (,*fix-arg-temps
              ,*(if opt-arg-temps
                  (cons : (mapcar (ret ^(,@1 nil ,@2))
                                  opt-arg-temps present-p-temps)))
              . ,rest-temp)
       (let (,result-temp)
         (or ,*ex-clauses)
         ,result-temp))))

(defmacro lambda-match (:form *match-form* . clauses)
  (expand-lambda-match clauses))

(defmacro defun-match (:form *match-form* name . clauses)
  (tree-bind (lambda args . body) (expand-lambda-match clauses)
    ^(defun ,name ,args . ,body)))

(define-param-expander :match (params clauses menv form)
  (let ((*match-form* form))
    (unless (proper-list-p params)
      (compile-error form
                     "~s is incompatible with dotted parameter lists"
                     :match))
    (when (find : params)
      (compile-error form
                     "~s is incompatible with optional parameters"
                     :match))
    (tree-bind (lambda lparams . body) (expand-lambda-match clauses)
      (let ((dashdash (member '-- params)))
        (cons (append (ldiff params dashdash)
                      (butlastn 0 lparams)
                      dashdash
                      (nthlast 0 lparams))
              body)))))

(defmacro defmatch (name destructuring-args . body)
  (with-gensyms (name-dummy args)
    ^(progn
       (sethash *match-macro* ',name
                (lambda (,args vars-env)
                  (mac-env-param-bind *match-form* vars-env
                                     (,name-dummy ,*destructuring-args)
                                     ,args ,*body)))
       ',name)))

(defun check (f op pat)
  (if (or (not (listp pat))
          (meq (car pat) 'sys:expr 'sys:var 'sys:quasi))
    (compile-error f "~s: list pattern expected, not ~s" op pat)
    pat))

(defun check-end (f op pat)
  (if (and (listp pat)
           (meq (car pat) 'sys:expr 'sys:var 'sys:quasi))
    (compile-error f "~s: list or atom pattern expected, not ~s" op pat)
    pat))

(defun check-sym (f op sym : nil-ok)
  (cond
    ((bindable sym) sym)
    ((and (null sym) nil-ok) sym)
    (t (compile-error f "~s: bindable symbol expected, not ~s" op sym))))

(defun loosen (f pat)
  (if (proper-list-p pat)
    (append pat '@nil)
    pat))

(defun pat-len (f pat)
  (if (consp pat)
    (let ((var-op-pos (pos-if (op meq 'sys:var 'sys:expr 'sys:quasi)
                              (butlastn 0 pat))))
      (if var-op-pos var-op-pos (len pat)))
    0))

(defmatch sme (:form f sta mid end : (mvar (gensym)) eobj)
  (let* ((psta (loosen f (check f 'sme sta)))
         (pmid (loosen f (check f 'sme mid)))
         (pend (check-end f 'sme end))
         (lsta (pat-len f psta))
         (lmid (pat-len f pmid))
         (lend (pat-len f pend))
         (obj (gensym)))
    ^@(as ,(check-sym f 'sme obj)
          @(and ,psta
                @(with @(scan @(as ,(check-sym f 'sme mvar) ,pmid))
                       (nthcdr ,lsta ,obj))
                @(with @(as ,(check-sym f 'sme eobj t) ,pend)
                       (nthlast ,lend (nthcdr ,lmid ,mvar)))))))

(defmatch end (:form f end : evar)
  (let* ((pend (check-end f 'end end))
         (lend (pat-len f pend))
         (obj (gensym)))
    ^@(as ,(check-sym f 'end obj)
          @(with @(as ,(check-sym f 'end evar t) ,pend)
                 (nthlast ,lend ,obj)))))

(defun non-triv-pat-p (syntax) t)

(defun non-triv-pat-p (syntax)
  (match-case syntax
    ((@(eq 'sys:expr) (@(bindable) . @nil)) t)
    ((@(eq 'sys:var) @(or @(bindable) nil) . @nil) t)
    ((@(eq 'sys:quasi) . @(some @(consp))) t)
    ((@pat . @rest) (or (non-triv-pat-p pat)
                        (non-triv-pat-p rest)))
    (#R(@from @to) (or (non-triv-pat-p from)
                       (non-triv-pat-p to)))
    (@(some @(non-triv-pat-p)) t)))

(defun var-pat-p (syntax)
  (when-match (@(eq 'sys:var) @(bindable @sym) . @nil) syntax
    sym))

(defun expand-quasi-match (args var-list)
  (labels ((bound-p (vlist vars sym)
             (cond
               ((bindable sym) (or (member sym vars) vlist.(exists sym)))
               ((null sym) t)
               ((compile-error *match-form* "bindable symbol expected, not ~s"
                               sym))))
           (normalize (args)
             (mapcar (do if-match (@(eq 'sys:var) @sym nil) @1
                       ^(sys:var ,sym)
                       @1)
                     args))
           (quasi-match (vlist args vars str pos)
             (match-case args
               ;; `text`
               ((@(stringp @txt))
                (list ^@(require @nil (str= ,txt (sub-str ,str ,pos t)))))
               ;; `txt@...`
               ((@(stringp @txt) . @rest)
                (with-gensyms (npos)
                  (cons ^@(require @(with ,npos ,(len txt))
                            (starts-with ,txt ,str))
                        (quasi-match vlist rest vars str npos))))
               ;; `@var` (existing binding)
               (((@(eq 'sys:var) @(bound-p vlist vars @sym) . @nil))
                (list ^@(require @nil (str= (sys:quasi ,(car args))
                                            (sub-str ,str ,pos t)))))
               ;; `@var@...` (existing binding)
               ((@(as avar (@(eq 'sys:var) @(bound-p vlist vars @sym) . @nil))
                 . @rest)
                (with-gensyms (txt len npos)
                  (list* ^@(with ,txt (sys:quasi ,avar))
                         ^@(with ,len (len ,txt))
                         ^@(with ,npos (+ ,pos ,len))
                         ^@(require @nil
                             (str= ,txt (sub-str ,str ,pos ,npos)))
                         (quasi-match vlist rest vars str npos))))
               ;; `@var` (new binding)
               (((@(eq 'sys:var) @sym))
                (list ^@(with ,sym (sub-str ,str ,pos t))))
               ;; `@{var #/rx/}` (new binding)
               (((@(eq 'sys:var) @sym (@(regexp @reg))))
                (list ^@(require @(with ,sym (sub-str ,str ,pos t))
                          (m^$ ,reg ,sym))))
               ;; `@{var #/rx/}@...` (new binding)
               (((@(eq 'sys:var) @sym (@(regexp @reg))) . @rest)
                (with-gensyms (len npos)
                  (list* ^@(require @(with ,len (match-regex ,str ,reg ,pos))
                            ,len)
                         ^@(with ,npos (+ ,pos ,len))
                         ^@(with ,sym (sub-str ,str ,pos ,npos))
                         (quasi-match vlist rest (cons sym vars) str npos))))
               ;; `@{var 123}` (new binding)
               (((@(eq 'sys:var) @sym (@(integerp @len))))
                (unless (plusp len)
                  (compile-error *match-form*
                                 "variable ~s: positive integer required,\ \
                                  not ~a" sym))
                (with-gensyms (npos)
                  (list ^@(require @(with ,npos (+ ,pos ,len))
                            (eql ,npos (len ,str)))
                        ^@(with ,sym (sub-str ,str ,pos t)))))
               ;; `@{var 123}@...`` (new binding)
               (((@(eq 'sys:var) @sym (@(integerp @len))) . @rest)
                (unless (plusp len)
                  (compile-error *match-form*
                                 "variable ~s: positive integer required,\ \
                                  not ~a" sym))
                (with-gensyms (npos)
                  (list* ^@(require @(with ,npos (+ ,pos ,len))
                             (<= ,npos (len ,str)))
                         ^@(with ,sym (sub-str ,str ,pos ,npos))
                         (quasi-match vlist rest (cons sym vars) str npos))))
               ;; `@{var}txt` (new binding)
               (((@(eq 'sys:var) @sym) @(stringp @txt))
                (with-gensyms (len end)
                  (list ^@(require @(with ,end (search-str ,str ,txt ,pos))
                           ,end (eql (+ ,end ,(len txt)) (len ,str)))
                        ^@(with ,sym (sub-str ,str ,pos ,end)))))
               ;; `@{var}txt...` (new binding)
               (((@(eq 'sys:var) @sym) @(stringp @txt) . @rest)
                (with-gensyms (len end npos)
                  (list* ^@(require @(with ,end (search-str ,str ,txt ,pos))
                            ,end)
                         ^@(with ,npos (+ ,end ,(len txt)))
                         ^@(with ,sym (sub-str ,str ,pos ,end))
                         (quasi-match vlist rest (cons sym vars) str npos))))
               ;; `@var0@var1` (unbound followed by bound)
               (((@(eq 'sys:var) @sym)
                 @(as bvar (@(eq 'sys:var) @(bound-p vlist vars @bsym) . @mods)))
                (with-gensyms (txt end)
                  (list ^@(with ,txt (sys:quasi ,bvar))
                        ^@(require @(with ,end (search-str ,str ,txt ,pos))
                           ,end (eql (+ , end (len ,txt)) (len ,str)))
                        ^@(with ,sym (sub-str ,str ,pos ,end)))))
               ;; `@var0@var1...` (unbound followed by bound)
               (((@(eq 'sys:var) @sym)
                 @(as bvar (@(eq 'sys:var) @(bound-p vlist vars @bsym) . @mods))
                 . @rest)
                (with-gensyms (txt end npos)
                  (list* ^@(with ,txt (sys:quasi ,bvar))
                         ^@(require @(with ,end (search-str ,str ,txt ,pos))
                            ,end)
                         ^@(with ,npos (+ ,end (len ,txt)))
                         ^@(with ,sym (sub-str ,str ,pos ,end))
                         (quasi-match vlist rest (cons sym vars) str npos))))
               ;; `@{var whatever}@...`(new binding, unsupported modifiers)
               (((@(eq 'sys:var) @sym @mods . @nil) . @rest)
                (compile-error *match-form*
                               "variable ~s: unsupported modifiers ~s"
                               sym mods))

               ;; `@var0@var1` (unbound followed by unbound)
               (((@(eq 'sys:var) @sym0)
                 (@(eq 'sys:var) @sym1 . @mods)
                 . @rest)
                (compile-error *match-form*
                               "consecutive unbound variables ~s and ~s"
                               sym0 sym1)))))
    (with-gensyms (str pos)
      ^@(and @(require (sys:var ,str)
                (stringp ,str))
             @(with ,pos 0)
             ,*(quasi-match var-list (normalize args) nil str pos)))))
