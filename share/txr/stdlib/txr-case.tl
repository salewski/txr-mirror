;; Copyright 2015-2016
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

(defmacro txr-if (name args input : then else)
  (let ((syms (keep-if [andf true symbolp [notf keywordp] [notf (op eq t)]]
                       args))
        (arg-exprs [mapcar [iffi symbolp (ret ^',@1)] args])
        (result (gensym "res-"))
        (bindings (gensym "bindings-"))
        (insym (gensym "input-")))
    ^(let* ((,insym ,input)
            (,result (match-fun ',name (list ,*arg-exprs)
                                (if (stringp ,insym) (list ,insym) ,insym)
                                nil)))
       (if ,result
         (let ((,bindings (car ,result)))
           (let (,*[mapcar (ret ^(,@1 (cdr (assoc ',@1 ,bindings))))
                           syms])
             ,then))
         ,else))))

(defmacro txr-when (name args input . body)
  ^(txr-if ,name ,args ,input (progn ,*body)))

(defmacro txr-case-impl (sym . clauses)
  (tree-case clauses
    (((name args . body) . other-clauses)
     (if (eq name t) :
       ^(txr-if ,name ,args ,sym
          (progn ,*body)
          (txr-case-impl ,sym ,*other-clauses))))
    (((sym . rest) . other-clauses)
     (if (eq sym t)
       (if other-clauses
         (error "txr-case: clauses after (t ...) clause ignored")
         ^(progn ,*rest))
       (error "txr-case: bad syntax: ~s" (car clauses))))
    (() ())
    (atom
       (error "txr-case: unexpected atom in syntax: ~s" atom))))

(defmacro txr-case (input-expr . clauses)
  (let ((input (gensym "input-")))
    ^(let ((,input ,input-expr))
       (txr-case-impl ,input ,*clauses))))
