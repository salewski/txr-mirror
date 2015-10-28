;; Copyright 2015
;; Kaz Kylheku <kaz@kylheku.com>
;; Vancouver, Canada
;; All rights reserved.
;;
;; Redistribution of this software in source and binary forms, with or without
;; modification, is permitted provided that the following two conditions are met.
;;
;; Use of this software in any manner constitutes agreement with the disclaimer
;; which follows the two conditions.
;;
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in
;;    the documentation and/or other materials provided with the
;;    distribution.
;;
;; THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
;; WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  IN NO EVENT SHALL THE
;; COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DAMAGES, HOWEVER CAUSED,
;; AND UNDER ANY THEORY OF LIABILITY, ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(defstruct (sys:yld-item val cont) nil val cont)

(defun sys:obtain-impl (fun)
  (lambda (: reply)
    (let ((yi (call fun reply)))
      (cond
        ((eq (typeof yi) 'sys:yld-item)
         (set fun yi.cont)
         yi.val)
        (t yi)))))

(defun sys:yield-impl (name fun ctx-form)
  (let ((cont (sys:capture-cont name ctx-form)))
    (if (car cont)
      (call fun cont)
      (cdr cont))))

(defmacro obtain (. body)
  (let ((ignored (gensym "ign")))
    ^(sys:obtain-impl (lambda (,ignored) ,*body))))

(defmacro obtain-block (name . body)
  ^(obtain (block ,name ,*body)))

(defmacro yield-from (:form ctx-form name form)
  (let ((cont-sym (gensym)))
    ^(sys:yield-impl ',name
                    (lambda (,cont-sym)
                      (return-from ,name (new (sys:yld-item
                                                ,form (cdr ,cont-sym)))))
                    ',ctx-form)))

(defmacro yield (form)
  ^(yield-from nil ,form))
