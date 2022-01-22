;; Copyright 2015-2022
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
(defun sys:typecase-expander (form obj clauses)
  (let* ((val (gensym))
         (cond-pairs (collect-each ((cl clauses))
                       (tree-case cl
                         ((type . body)
                          (cond
                            ((eq type t)
                             ^(t ,*(or body '(nil))))
                            ((symbolp type)
                             ^((typep ,val ',type) ,*(or body '(nil))))
                            (t :)))
                         (else (compile-error form
                                              "~s: bad clause syntax: ~s"
                                              'typecase cl))))))
    ^(let ((,val ,obj))
       (cond ,*cond-pairs
         ,*(if (eq (car form) 'etypecase)
             ^((t (throwf 'case-error
                          "~s: unhandled type: ~s"
                          'etypecase (typeof ,val)))))))))

(defmacro typecase (:form f obj . clauses)
  (sys:typecase-expander f obj clauses))

(defmacro etypecase (:form f obj . clauses)
  (sys:typecase-expander f obj clauses))
