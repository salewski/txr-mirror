;; Copyright 2020-2022
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
(defun sys:vars-check (form vars)
  (unless (listp vars)
    (compile-error form "~s is expected to be variable binding syntax" vars))
  (whenlet ((bad (find-if [notf consp] vars)))
    (compile-error form "~s isn't a var-initform pair" bad)))

(defun sys:bindable-check (form syms)
  (whenlet ((bad (find-if [notf bindable] syms)))
    (compile-error form "~s isn't a bindable symbol" bad)))

(defun sys:expand-each-prod (form vars body)
  (sys:vars-check form vars)
  (let ((syms [mapcar car vars])
        (inits [mapcar cadr vars])
        (gens [mapcar (ret (gensym)) vars]))
    (sys:bindable-check form syms)
    (let ((fun (caseq (car form)
                 (each-prod 'maprodo)
                 (collect-each-prod 'maprod)
                 (append-each-prod 'maprend))))
      ^(let ,(zip gens inits)
         (block nil
           (,fun (lambda (,*syms) ,*body) ,*gens))))))

(defun sys:expand-each-prod* (form vars body)
  (sys:vars-check form vars)
  (let* ((each-prod-op (caseq (car form)
                         (each-prod* 'each-prod)
                         (collect-each-prod* 'collect-each-prod)
                         (append-each-prod* 'append-each-prod)
                         (sum-each-prod* 'sum-each-prod)
                         (mul-each-prod* 'mul-each-prod)))
         (syms [mapcar car vars])
         (inits [mapcar cadr vars]))
    ^(let* ,vars
       (,each-prod-op ,(zip syms syms) ,*body))))

(defun sys:expand-arith-each-prod (form vars body)
  (sys:vars-check form vars)
  (let ((syms [mapcar car vars])
        (inits [mapcar cadr vars])
        (gens [mapcar (ret (gensym)) vars]))
    (sys:bindable-check form syms)
    (let ((op-iv (caseq (car form)
                   (sum-each-prod '(+ . 0))
                   (mul-each-prod '(* . 1)))))
      (with-gensyms (acc)
        ^(let ((,acc ,(cdr op-iv))
               ,*(zip gens inits))
           (block nil
             (maprodo (lambda (,*syms)
                        (set ,acc (,(car op-iv) ,acc (progn ,*body))))
                      ,*gens)
             ,acc))))))

(defmacro each-prod (:form f vars . body)
  (sys:expand-each-prod f vars body))

(defmacro collect-each-prod (:form f vars . body)
  (sys:expand-each-prod f vars body))

(defmacro append-each-prod (:form f vars . body)
  (sys:expand-each-prod f vars body))

(defmacro sum-each-prod (:form f vars . body)
  (sys:expand-arith-each-prod f vars body))

(defmacro mul-each-prod (:form f vars . body)
  (sys:expand-arith-each-prod f vars body))

(defmacro each-prod* (:form f vars . body)
  (sys:expand-each-prod* f vars body))

(defmacro collect-each-prod* (:form f vars . body)
  (sys:expand-each-prod* f vars body))

(defmacro append-each-prod* (:form f vars . body)
  (sys:expand-each-prod* f vars body))

(defmacro sum-each-prod* (:form f vars . body)
  (sys:expand-each-prod* f vars body))

(defmacro mul-each-prod* (:form f vars . body)
  (sys:expand-each-prod* f vars body))
