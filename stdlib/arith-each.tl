;; Copyright 2021-2022
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

(defmacro sys:arith-each (:form f op-iv vars . body)
  (let* ((gens (mapcar (ret (gensym)) vars))
         (syms [mapcar car vars])
         (accum (gensym))
         (op (car op-iv))
         (iv (cdr op-iv)))
    (if (null vars)
      iv
      ^(let* (,*(mapcar (ret ^(,@1 (iter-begin ,@2))) gens syms)
               (,accum ,iv))
         (block nil
           (sys:for-op ()
             ((and ,*(mapcar (op list 'iter-more) gens)) ,accum)
             (,*(mapcar (ret ^(sys:setq ,@1 (iter-step ,@1))) gens))
             ,*(mapcar (ret ^(sys:setq ,@1 (iter-item ,@2))) syms gens)
             (set ,accum (,op ,accum (progn ,*body)))))))))

(defmacro sum-each (:form f vars . body)
  (sys:vars-check f vars)
  ^(let ,vars
     (block nil
       (sys:arith-each (+ . 0) ,vars ,*body))))

(defmacro sum-each* (:form f vars . body)
  (sys:vars-check f vars)
  ^(let* ,vars
     (block nil
       (sys:arith-each (+ . 0) ,vars ,*body))))

(defmacro mul-each (:form f vars . body)
  (sys:vars-check f vars)
  ^(let ,vars
     (block nil
       (sys:arith-each (* . 1) ,vars ,*body))))

(defmacro mul-each* (:form f vars . body)
  (sys:vars-check f vars)
  ^(let* ,vars
     (block nil
       (sys:arith-each (* . 1) ,vars ,*body))))
