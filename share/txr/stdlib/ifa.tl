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

(defmacro ifa (:env e test then : else)
  (flet ((candidate-p (expr)
           (not (or (constantp expr e) (symbolp expr)))))
    (cond
      ((or (atom test) (null (cdr test))) ^(let ((it ,test))
                                             (if it ,then ,else)))
      ((member (first test) '(not null false))
       (unless (eql (length test) 2)
         (throwf 'eval-error "ifa: wrong number of arguments to ~s"
                 (first test)))
       ^(ifa ,(second test) ,else ,then))
      (t (let* ((sym (first test))
                (args (rest test))
                (n-candidate-args [count-if candidate-p args])
                (pos-candidate (or [pos-if candidate-p args] 0)))
           (unless (or (lexical-fun-p e sym)
                       (and (or (functionp (symbol-function sym))
                                (null (symbol-function sym)))))
             (throwf 'eval-error "ifa: test expression must be \
                                 \ a simple function call"))
           (when (> n-candidate-args 1)
             (throwf 'eval-error "ifa: ambiguous situation: \
                                 \ not clear what can be \"it\""))
           (let* ((temps (mapcar (ret (gensym)) args))
                  (it-temp [temps pos-candidate]))
             ^(let* (,*(zip temps args) (it ,it-temp))
                (if (,sym ,*temps) ,then ,else))))))))

(defmacro conda (. pairs)
  (tree-case pairs
    (((test . forms) . rest) ^(ifa ,test (progn ,*forms)
                                   (conda ,*rest)))
    (() ())
    (else (throwf 'eval-error "conda: bad syntax: ~s" pairs))))
