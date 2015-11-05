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

(defstruct (sys:rcv-item val cont) nil val cont)

(defun sys:obtain-impl (fun)
  (finalize
    (lambda (: resume-val)
      (let ((yi (call fun resume-val)))
        (while t
          (cond
            ((eq (typeof yi) 'sys:yld-item)
             (call fun 'sys:cont-free)
             (set fun yi.cont)
             (return yi.val))
            ((eq (typeof yi) 'sys:rcv-item)
             (call fun 'sys:cont-free)
             (set fun yi.cont)
             (set yi (call fun resume-val)))
            (t (return yi))))))
    (lambda (cont)
      (call cont 'sys:cont-poison))))

(defmacro obtain (. body)
  (let ((arg (gensym "arg")))
    ^(sys:obtain-impl (lambda (,arg)
                        (unless (eq ,arg 'sys:cont-free)
                          ,*body)))))

(defmacro obtain-block (name . body)
  ^(obtain (block ,name ,*body)))

(defmacro yield-from (:form ctx-form name : (form nil have-form-p))
  (let ((cont-sym (gensym)))
    ^(sys:capture-cont ',name
                       (lambda (,cont-sym)
                         (sys:abscond-from ,name
                                           ,(if have-form-p
                                              ^(new (sys:yld-item
                                                      ,form ,cont-sym))
                                              ^(new (sys:rcv-item
                                                      nil ,cont-sym)))))
                     ',ctx-form)))

(defmacro yield (: (form nil have-form-p))
  (if have-form-p
    ^(yield-from nil ,form)
    ^(yield-from nil)))

(defmacro suspend (:form form name var . body)
  ^(sys:capture-cont ',name (lambda (,var)
                              (sys:abscond-from ,name ,*body))
                     ',form))
