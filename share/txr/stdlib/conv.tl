;; Copyright 2016
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

(defun sys:conv-let (. body)
  ^(flet ((i (arg : radix)
            (toint arg radix))
          (o (arg)
            (toint arg 8))
          (x (arg)
            (toint arg 16))
          (b (arg)
            (toint arg 2))
          (r (arg)
            (tofloat arg)))
     ,*body))

(defun sys:do-conv (lfl mfl tfl nm list)
  (while (and list lfl)
    (set (car list) (call (car lfl) (car list)))
    (set list (cdr list))
    (set lfl (cdr lfl)))
  (dotimes (i nm)
    (unless list
      (return))
    (when mfl
      (set (car list) (call (car mfl) (car list)))
      (set mfl (cdr mfl)))
    (set list (cdr list)))
  (while (and list tfl)
    (set (car list) (call (car tfl) (car list)))
    (set list (cdr list))
    (set tfl (cdr tfl))))

(defun sys:conv-expand (form specs list-sym)
  (mac-param-bind form (lead : mid trail)
                       (split* (mapcar [iff (op eq :)
                                            identity
                                            [iff (op eq '-)
                                                 (retf '(fun identity)) 
                                                 (ret ^[identity ,@1])]]
                                       specs)
                               (op where (op eq :)))
    (let ((nl (length lead))
          (nt (length trail)))
      (with-gensyms (i nm lfl mfl tfl)
        (sys:conv-let
          ^(let* ((,nm (- (length ,list-sym) ,(+ nl nt)))
                  (,lfl (list ,*lead))
                  (,mfl (if (plusp ,nm) (repeat (list ,*mid))))
                  (,tfl (list ,*trail)))
             (sys:do-conv ,lfl ,mfl ,tfl ,nm ,list-sym)))))))

(defmacro sys:conv (:form form (. specs) list-expr)
  (cond
    ((null specs) list-expr)
    ((atom specs)
     (throwf 'eval-error "sys:conv: invalid conversion list: ~s" specs))
    (t (with-gensyms (list-sym)
         ^(let ((,list-sym ,list-expr))
            ,(sys:conv-expand form specs list-sym)
            ,list-sym)))))
