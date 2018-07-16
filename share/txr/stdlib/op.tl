;; Copyright 2017-2018
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

(sys:make-struct-type
  'sys:op-ctx nil nil '(form gens up meta) nil
  (lambda (me)
    (slotset me 'up sys:*op-ctx*)
    (slotset me 'meta (gensym "meta-")))
  nil nil)

(defvar sys:*op-ctx*)

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

(defun sys:op-alpha-rename (f e op-args do-nested-metas)
  (let* ((ctx sys:*op-ctx*)
         (code ^(macrolet ((sys:expr (:form f arg)
                             (let ((ctx ,ctx))
                               (if (and (slot ctx 'up) (sys:op-meta-p arg))
                                 ^(,(slot (slot ctx 'up) 'meta) (quote ,arg))
                               f)))
                           (sys:var (:form f arg . mods)
                             (cond
                               ((and (not mods) (sys:op-meta-p f))
                                (unless (integerp arg)
                                  (sys:setq arg 0))
                                (sys:ensure-op-arg ,ctx arg))
                               (t f)))
                           ,*(if do-nested-metas
                               ^((,(slot ctx 'meta) ((quote arg)) arg))))
                  ,op-args)))
    (sys:expand code e)))

(defun sys:op-expand (f e args)
  (let* ((ctx (make-struct 'sys:op-ctx ^(form ,f)))
         (sys:*op-ctx* ctx)
         (sym (car f))
         (syntax-0 (if (eq sym 'do) ^(,*args) ^[,*args]))
         (syntax-1 (sys:op-alpha-rename f e syntax-0 nil))
         (syntax-2 (sys:op-alpha-rename f e syntax-1 t))
         (metas (slot ctx 'gens))
         (rest-sym (sys:ensure-op-arg ctx 0)))
     (unless args
       ['compile-error f "arguments required"])
    ^(lambda (,*(cdr metas) . ,rest-sym)
       ,(let ((fargs (cdr (cdr syntax-2))))
          (cond
            ((and (eq sym 'lop) fargs)
             (let ((fargs-l1 (mapcar (lambda (farg)
                                       ^(sys:l1-val ,farg))
                                     fargs)))
               ^[sys:apply ,(car (cdr syntax-2))
                           (append ,rest-sym (list ,*fargs-l1))]))
            ((or metas (eq sym 'do))
             syntax-2)
            (t (append syntax-2 rest-sym)))))))

(defmacro op (:form f :env e . args)
  (sys:op-expand f e args))

(defmacro do (:form f :env e . args)
  (sys:op-expand f e args))

(defmacro lop (:form f :env e . args)
  (sys:op-expand f e args))

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
