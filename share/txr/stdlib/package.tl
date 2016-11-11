;; Copyright 2016
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

(macro-time
  (defun sys:name-str (kind sym-or-string)
    (cond
      ((symbolp sym-or-string) (symbol-name sym-or-string))
      ((stringp sym-or-string) sym-or-string)
      (t (throw 'eval-error "~s: ~s isn't a valid ~a name"
                'defpackage sym-or-string kind)))))

(defmacro defpackage (name . clauses)
  (let* ((pkg (gensym "pkg-"))
         (name-str (sys:name-str 'package name))
         (exp-clauses (append-each ((c clauses))
                        (tree-case c
                          ((keyword . rest)
                            (caseql keyword
                              (:use
                                (if rest ^((use-package ',rest ,pkg))))
                              (:use-syms
                                ^((each ((s ',rest))
                                    (use-sym s ,pkg))))
                              (:local
                                ^((each ((n ',(mapcar (op sys:name-str 'symbol)
                                                      rest)))
                                    (let ((s (intern n ,pkg)))
                                      (unless (eq (symbol-package s) ,pkg)
                                        (unuse-sym s ,pkg)
                                        (intern n ,pkg))))))))
                          (atom
                            (throwf 'eval-error "~s: invalid clause: ~s"
                                    'defpackage atom))))))
    ^(let ((,pkg (or (find-package ,name-str)
                     (make-package ,name-str))))
       ,*(unless (assoc :use clauses) ^((use-package "usr" ,pkg)))
       ,*exp-clauses
       ,pkg)))
