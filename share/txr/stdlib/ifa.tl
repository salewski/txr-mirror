;; Copyright 2015-2017
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
                (args (if (eq 'dwim sym) (cddr test) (cdr test)))
                (n-candidate-args [count-if candidate-p args])
                (pos-candidate (or [pos-if candidate-p args] 0)))
           (unless (or (lexical-fun-p e sym)
                       (and (or (functionp (symbol-function sym))
                                (eq sym 'dwim)
                                (null (symbol-function sym)))))
             (throwf 'eval-error "ifa: test expression must be \
                                 \ a simple function call"))
           (when (> n-candidate-args 1)
             (throwf 'eval-error "ifa: ambiguous situation: \
                                 \ not clear what can be \"it\""))
           (iflet ((it-form (macroexpand [args pos-candidate] e))
                   (is-place (place-form-p it-form e)))
             (let ((before-it [args 0..pos-candidate])
                   (after-it [args (succ pos-candidate)..:]))
               (let* ((btemps (mapcar (ret (gensym)) before-it))
                      (atemps (mapcar (ret (gensym)) after-it)))
                 ^(let (,*(zip btemps before-it))
                    (placelet ((it ,it-form))
                      (let (,*(zip atemps after-it))
                        (if (,sym ,*(if (eq 'dwim sym) ^(,(second test)))
                                  ,*btemps it ,*atemps)
                          ,then ,else))))))
             (let* ((temps (mapcar (ret (gensym)) args))
                    (it-temp [temps pos-candidate]))
               ^(let* (,*(zip temps args) (it ,it-temp))
                  (if (,sym ,*(if (eq 'dwim sym) ^(,(second test)))
                            ,*temps) ,then ,else)))))))))

(defmacro whena (test . body)
  ^(ifa ,test (progn ,*body)))

(macro-time
  (defun sys:if-to-cond (if-oper cond-oper pairs)
    (tree-case pairs
      (((test . forms) . rest) ^(,if-oper ,test (progn ,*forms)
                                          (,cond-oper ,*rest)))
      (() ())
      (else (throwf 'eval-error "~s: bad syntax: ~s" cond-oper pairs)))))

(defmacro conda (. pairs)
  (sys:if-to-cond 'ifa 'conda pairs))

(defmacro condlet (. pairs)
  (sys:if-to-cond 'iflet 'condlet pairs))
