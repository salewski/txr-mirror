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

(defmacro with-resources (res-bindings . body)
  (tree-case res-bindings
    (((var init cleanup) . rest)
     ^(let ((,var ,init))
        (when ,var
          (unwind-protect
            (with-resources ,rest ,*body)
            ,cleanup))))
    (((var init) . rest)
     ^(let ((,var ,init))
        (with-resources ,rest ,*body)))
    ((var . rest)
     ^(let (,var)
        (with-resources ,rest ,*body)))
    (nil
      ^(progn ,*body))
    (other (error "with-resources: bad syntax"))))

(defmacro with-objects (var-init-forms . body)
  (let ((gens (mapcar (ret (gensym)) var-init-forms)))
    ^(let ,gens
       (unwind-protect
         (let* ,(mapcar (aret ^(,@2 (set ,@1 ,@3))) gens var-init-forms)
           ,*body)
         ,*(reverse (mapcar (ret ^(call-finalizers ,@1)) gens))))))
