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

(defmacro with-resources (:form f res-bindings . body)
  (tree-case res-bindings
    (((sym init) . rest)
     ^(let ((,sym ,init))
        (with-resources ,rest ,*body)))
    (((sym init . cleanup) . rest)
     (if (and (plusp sys:compat) (<= sys:compat 265))
       ^(let ((,sym ,init))
          (when ,sym
            (unwind-protect
              (with-resources ,rest ,*body)
              ,*cleanup)))
       ^(let ((,sym ,init))
          (unwind-protect
            (with-resources ,rest ,*body)
            ,*cleanup))))
    ((sym . rest)
     ^(let (,sym)
        (with-resources ,rest ,*body)))
    (nil
      ^(progn ,*body))
    (other (compile-error f "bad syntax"))))

(defmacro with-objects (var-init-forms . body)
  (let ((gens (mapcar (ret (gensym)) var-init-forms)))
    ^(let ,gens
       (unwind-protect
         (let* ,(mapcar (aret ^(,@2 (set ,@1 ,@3))) gens var-init-forms)
           ,*body)
         ,*(reverse (mapcar (ret ^(call-finalizers ,@1)) gens))))))
