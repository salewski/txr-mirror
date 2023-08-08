;; Copyright 2017-2023
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

(defvar sys:*op-ctx*)

(sys:make-struct-type
  'sys:op-ctx nil nil '(form gens up meta rec recvar nested) nil
  (lambda (me)
    (slotset me 'up sys:*op-ctx*)
    (slotset me 'meta (gensym "meta-")))
  nil nil)

(defun sys:ensure-op-arg (ctx n)
  (let ((ag (slot ctx 'gens)))
    (when (> n 1024)
      ['compile-error (slot ctx 'form)
                      "@~a calls for function with too many arguments" n])
    (for ((i (len ag)) (l))
         ((<= i n)
          (sys:setq ag (append ag (nreverse l)))
          (slotset ctx 'gens ag)
          [ag n])
         ((sys:setq i (succ i)))
      (sys:setq l (cons (gensym `arg-@(if (plusp i) i "rest")-`) l)))))

(defun sys:op-meta-p (exp)
  (tree-case exp
    ((x y . r) (and (null r)
                    (cond
                      ((eq x 'sys:expr) (let ((depth (sys:op-meta-p y)))
                                          (if depth (succ depth))))
                      ((eq x 'sys:var) (if (or (integerp y)
                                               (eq y 'rest))
                                         0)))))))

(defun sys:op-rec-p (exp)
  (or (tree-case exp
        ((x (y . t)) (and (eq x 'sys:expr) (eq y 'usr:rec))))
      (equal exp '(sys:var usr:rec))))

(defun sys:op-ensure-rec (ctx : recvar)
  (when recvar
    (slotset ctx 'recvar t))
  (or (slot ctx 'rec) (slotset ctx 'rec (gensym "rec-"))))

(defun sys:find-parent (ctx depth)
  (for ((more t)) (more ctx) ((if (minusp (sys:setq depth (pred depth)))
                                (sys:setq more nil)))
    (sys:setq ctx (slot ctx 'up))))

(defun sys:op-alpha-rename (e op-args do-nested-metas)
  (let* ((ctx sys:*op-ctx*)
         (code ^(macrolet ((sys:expr (:form f arg)
                             (let* ((ctx ,ctx)
                                    (depth (sys:op-meta-p arg))
                                    (rec (sys:op-rec-p arg))
                                    (up (slot ctx 'up))
                                    (par (cond
                                           (depth (sys:find-parent ctx depth))
                                           (rec up))))
                               (cond
                                 ((and par (or depth rec))
                                  (slotset par 'nested t)
                                  ^(,(slot (slot ctx 'up) 'meta) (quote ,arg)))
                                 ((sys:op-rec-p f)
                                  ^(,(sys:op-ensure-rec ctx) ,*(rest arg)))
                                 (t f))))
                           (sys:var (:form f arg . mods)
                             (cond
                               ((sys:op-meta-p f)
                                (unless (integerp arg)
                                  (sys:setq arg 0))
                                (sys:ensure-op-arg ,ctx arg))
                               ((sys:op-rec-p f)
                                (sys:op-ensure-rec ,ctx t))
                               (t f)))
                           ,*(if do-nested-metas
                               ^((,(slot ctx 'meta) ((quote arg)) arg))))
                  ,op-args)))
    (expand code e)))

(eval-only
  (defmacro op-ignerr (x)
    ^(sys:catch (error) ,x () (error (. args) (ignore args)))))

(defun sys:op-expand (f e args)
  (unless args
    ['compile-error f "arguments required"])
  (let* ((compat (and (plusp sys:compat) (<= sys:compat 225)))
         (ctx (make-struct 'sys:op-ctx ^(form ,f)))
         (sys:*op-ctx* ctx)
         (sym (car f))
         (do-gen (if (eq sym 'do) (gensym)))
         (syntax-0 (if (eq sym 'do) args ^[,*args]))
         (syntax-1 (if (or (null syntax-0) (neq sym 'do) compat)
                     ;; Not do, or empty do syntax, or compat mode.
                     (sys:op-alpha-rename e syntax-0 nil)
                     ;; Try to expand args as-is, catching errors.
                     (let ((syn (op-ignerr (sys:op-alpha-rename e
                                                                syntax-0
                                                                nil))))
                       (if syn
                         ;; Args expanded.
                         (if (or (slot ctx 'gens) (slot ctx 'nested))
                           ;; There are metas: okay, use expansion as-is.
                           syn
                           ;; No metas: add do-gen at the end and expand
                           ;; again, without catching errors.
                           (sys:op-alpha-rename e
                                                (append syntax-0
                                                        (list do-gen))
                                                nil))
                         ;; Args didn't expand, so let's try it with
                         ;; do-gen added.
                         (let ((syn (sys:op-alpha-rename
                                      e (append syntax-0
                                                (list do-gen))
                                      nil)))
                           ;; It didn't blow up with the do-gen. However, if
                           ;; there are metas, we must not be adding this
                           ;; gensym. Thus, this case is erroneous: it doesn't
                           ;; expand unless we add an element, which we must not.
                           ;; Thus we just expand it again without the do-gen,
                           ;; without op-ignerr, to let the error propagate.
                           (when (or (slot ctx 'gens) (slot ctx 'nested))
                             (sys:op-alpha-rename e syntax-0 nil)
                             ;; Just in case: we don't expect to reach this:
                             ['compile-error f "internal error"])
                           ;; There were no metas. Let's return the
                           ;; form augmented with do-gen.
                           syn)))))
         (syntax-2 (sys:op-alpha-rename e syntax-1 t))
         (metas (slot ctx 'gens))
         (rec (slot ctx 'rec))
         (recvar (slot ctx 'recvar))
         (rest-sym (sys:ensure-op-arg ctx 0))
         (lambda-interior (let ((fargs (tree-case syntax-2
                                         ((t t . fa) fa))))
                            (cond
                              ((and (eq sym 'lop) fargs)
                               (let ((fargs-l1 (mapcar (lambda (farg)
                                                         ^(sys:l1-val ,farg))
                                                       fargs)))
                                 ;; no cadr here to avoid circular autoload
                                 ^[sys:apply ,(car (cdr syntax-2))
                                             (append ,rest-sym (list ,*fargs-l1))]))
                              (metas syntax-2)
                              ((eq sym 'do)
                               (let ((arg1 (sys:ensure-op-arg ctx 1)))
                                 ^(symacrolet ((,do-gen ,arg1))
                                    ,syntax-2)))
                              (t (append syntax-2 rest-sym))))))
    (let ((metas (slot ctx 'gens)))
      (cond
        (recvar ^(sys:lbind ((,rec (lambda (,*(cdr metas) . ,rest-sym)
                                     (let ((,rec (fun ,rec)))
                                       ,lambda-interior))))
                   (fun ,rec)))
        (rec ^(sys:lbind ((,rec (lambda (,*(cdr metas) . ,rest-sym)
                                  ,lambda-interior)))
                (fun ,rec)))
        (t ^(lambda (,*(cdr metas) . ,rest-sym)
              ,lambda-interior))))))

(defmacro op (:form f :env e . args)
  (sys:op-expand f e args))

(defmacro do (:form f :env e . args)
  (sys:op-expand f e args))

(defmacro lop (:form f :env e . args)
  (sys:op-expand f e args))

(defmacro ldo (op . args)
  ^(do ,op @1 ,*args))

(defmacro ap (. args)
  ^(apf (op ,*args)))

(defmacro ip (. args)
  ^(ipf (op ,*args)))

(defmacro ado (. args)
  ^(apf (do ,*args)))

(defmacro ido (. args)
  ^(ipf (do ,*args)))

(defmacro ret (arg)
  ^(op identity* ,arg))

(defmacro aret (arg)
  ^(ap identity* ,arg))

(defun sys:opip-single-let-p (c)
  (tree-case c
    ((op sym)
     (and (eq op 'let)
          (atom sym)))
    (t nil)))

(defun sys:opip-let-p (c)
  (tree-case c
    ((op (sym t) . rest)
     (and (eq op 'let)
          (atom sym)
          (listp rest)))
    (t nil)))

(defun sys:opip-expand (e opsym dosym clauses)
  (tree-case clauses
    (nil nil)
    ((c . rest)
     (if (atom c)
       (cons c (sys:opip-expand e opsym dosym rest))
       (let ((sym (car c)))
         (cond
           ((memq sym '(dwim uref qref op do lop ldo ap ip ado ido ret aret))
            (cons c (sys:opip-expand e opsym dosym rest)))
           ((sys:opip-single-let-p c)
            (tree-bind (t sym) c
              (sys:opip-expand e opsym dosym ^((let (,sym @1)) ,*rest))))
           ((sys:opip-let-p c)
            (tree-bind (t . vars) c
              ^((do let* ,vars
                  [(opip ,*(sys:opip-expand e opsym dosym rest)) @1]))))
           (t (let ((opdo (if (or (special-operator-p (car c))
                                  (macro-form-p c e)) dosym opsym)))
                (cons ^(,opdo ,*c) (sys:opip-expand e opsym dosym
                                                    rest))))))))))

(defmacro opip (:env e . clauses)
  ^[chain ,*(sys:opip-expand e 'op 'do clauses)])

(defmacro oand (:env e . clauses)
  ^[chand ,*(sys:opip-expand e 'op 'do clauses)])

(defmacro lopip (:env e . clauses)
  ^[chain ,*(sys:opip-expand e 'lop 'ldo clauses)])

(defmacro loand (:env e . clauses)
  ^[chand ,*(sys:opip-expand e 'lop 'ldo clauses)])

(defmacro flow (val . opip-args)
  ^(call (opip ,*opip-args) ,val))

(defmacro lflow (val . opip-args)
  ^(call (lopip ,*opip-args) ,val))
