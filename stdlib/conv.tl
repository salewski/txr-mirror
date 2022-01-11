;; Copyright 2016-2022
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


(defmacro sys:conv-expand-sym (sym arg-expr)
  (caseq sym
    (usr:i ^(toint ,arg-expr))
    (usr:o ^(toint ,arg-expr 8))
    (usr:x ^(toint ,arg-expr 16))
    (usr:b ^(toint ,arg-expr 2))
    (usr:c ^(toint ,arg-expr #\c))
    (usr:r ^(tofloat ,arg-expr))
    (usr:iz ^(tointz ,arg-expr))
    (usr:oz ^(tointz ,arg-expr 8))
    (usr:xz ^(tointz ,arg-expr 16))
    (usr:bz ^(tointz ,arg-expr 2))
    (usr:cz ^(tointz ,arg-expr #\c))
    (usr:rz ^(tofloatz ,arg-expr))
    (t ^(,sym ,arg-expr))))

(defun sys:conv-let (. body)
  ^(flet ,(collect-each ((sym '(usr:i usr:o usr:x usr:b usr:c
                                usr:r usr:iz usr:oz usr:xz
                                usr:bz usr:cz usr:rz)))
            ^(,sym (arg) (sys:conv-expand-sym ,sym arg)))
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
     (throwf 'eval-error "~s: invalid conversion list: ~s" 'conv specs))
    (t (with-gensyms (list-sym)
         ^(let ((,list-sym ,list-expr))
            ,(sys:conv-expand form specs list-sym)
            ,list-sym)))))
