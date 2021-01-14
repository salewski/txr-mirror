;; Copyright 2017-2021
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

(defvar sys:*op-ctx*)

(sys:make-struct-type
  'sys:op-ctx nil nil '(form gens up meta rec recvar) nil
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

(defun sys:op-meta-p (expr)
  (tree-case expr
    ((x y . r) (and (null r)
                    (cond
                      ((eq x 'sys:expr) (sys:op-meta-p y))
                      ((eq x 'sys:var) (or (integerp y)
                                           (eq y 'rest))))))))

(defun sys:op-rec-p (expr)
  (tree-case expr
    ((x (y . r)) (and (eq x 'sys:expr) (eq y 'usr:rec)))))

(defun sys:op-ensure-rec (ctx : recvar)
  (when recvar
    (slotset ctx 'recvar t))
  (or (slot ctx 'rec) (slotset ctx 'rec (gensym "rec-"))))

(defun sys:op-alpha-rename (f e op-args do-nested-metas)
  (let* ((ctx sys:*op-ctx*)
         (code ^(macrolet ((sys:expr (:form f arg)
                             (let ((ctx ,ctx))
                               (cond
                                 ((and (slot ctx 'up)
                                       (or (sys:op-meta-p arg)
                                           (sys:op-rec-p arg)
                                           (equal arg '(sys:var usr:rec))))
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
                               ((equal f '(sys:var usr:rec))
                                (sys:op-ensure-rec ,ctx t))
                               (t f)))
                           ,*(if do-nested-metas
                               ^((,(slot ctx 'meta) ((quote arg)) arg))))
                  ,op-args)))
    (expand code e)))

(eval-only
  (defmacro op-ignerr (x)
    ^(sys:catch (error) ,x () (error (. args)))))

(defun sys:op-expand (f e args)
  (unless args
    ['compile-error f "arguments required"])
  (let* ((compat (and (plusp sys:compat) (<= sys:compat 225)))
         (ctx (make-struct 'sys:op-ctx ^(form ,f)))
         (do-gen)
         (sys:*op-ctx* ctx)
         (sym (car f))
         (syntax-0 (if (eq sym 'do) args ^[,*args]))
         (syntax-1 (if (or (null syntax-0) (neq sym 'do) compat)
                     (sys:op-alpha-rename f e syntax-0 nil)
                     (or (op-ignerr (sys:op-alpha-rename f e syntax-0 nil))
                         (let ((syn (sys:op-alpha-rename
                                      f e (append syntax-0
                                                  (list (sys:setq do-gen
                                                                  (gensym))))
                                      nil)))
                           (when (slot ctx 'gens)
                             (sys:op-alpha-rename f e syntax-0 nil))
                           syn))))
         (syntax-2 (sys:op-alpha-rename f e syntax-1 t))
         (metas (slot ctx 'gens))
         (rec (slot ctx 'rec))
         (recvar (slot ctx 'recvar))
         (rest-sym (sys:ensure-op-arg ctx 0))
         (lambda-interior (let ((fargs (cdr (cdr syntax-2))))
                            (cond
                              ((and (eq sym 'lop) fargs)
                               (let ((fargs-l1 (mapcar (lambda (farg)
                                                         ^(sys:l1-val ,farg))
                                                       fargs)))
                                 ^[sys:apply ,(car (cdr syntax-2))
                                             (append ,rest-sym (list ,*fargs-l1))]))
                              (metas syntax-2)
                              ((eq sym 'do)
                               (cond
                                 (compat syntax-2)
                                 (do-gen
                                   (let ((arg1 (sys:ensure-op-arg ctx 1)))
                                     ^(symacrolet ((,do-gen ,arg1))
                                        ,syntax-2)))
                                 (t (let ((arg1 (sys:ensure-op-arg ctx 1)))
                                      (append syntax-2 (list arg1))))))
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

(defmacro ret (. args)
  ^(op identity (progn @rest ,*args)))

(defmacro aret (. args)
  ^(ap identity (progn @rest ,*args)))

(defun sys:opip-expand (e clauses)
  (collect-each ((c clauses))
    (if (atom c)
      c
      (let ((sym (car c)))
        (if (member sym '(dwim uref qref))
          c
          (let ((opdo (if (or (special-operator-p (car c))
                              (macro-form-p c e)) 'do 'op)))
            ^(,opdo ,*c)))))))

(defmacro opip (:env e . clauses)
  ^[chain ,*(sys:opip-expand e clauses)])

(defmacro oand (:env e . clauses)
  ^[chand ,*(sys:opip-expand e clauses)])
