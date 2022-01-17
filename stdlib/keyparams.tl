;; Copyright 2017-2022
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

(defun sys:stuff-key-params (keys args)
  (with-gensyms (cell)
    (collect-each ((k keys))
      (tree-bind (sym : init sym-p) k
        (let ((kw (intern (symbol-name sym) :keyword)))
          ^(let ((,cell (memp ,kw ,args)))
             ,(if init
               ^(cond
                  (,cell
                    (set ,sym (cadr ,cell))
                    ,*(if sym-p ^((set ,sym-p t))))
                  (t
                    (set ,sym ,init)))
               ^(when ,cell
                  (set ,sym (cadr ,cell))
                  ,*(if sym-p ^((set ,sym-p t)))))))))))

(define-param-expander :key (param body menv form)
  (let* ((excluding-rest (butlastn 0 param))
         (key-start (memq '-- excluding-rest))
         (rest-param (or (nthlast 0 param) (gensym)))
         (before-key (ldiff excluding-rest key-start))
         (key-params-raw (butlastn 0 (cdr key-start)))
         (key-params [mapcar [iffi atom (op list @1)] key-params-raw])
         (eff-param (append before-key rest-param)))
    (each ((key-spec key-params))
      (tree-case key-spec
        ((sym init var-p . junk)
         (when (consp junk)
           (compile-error form "superfluous forms in ~s" key-spec))
         (when junk
           (compile-error form "invalid dotted form ~s" key-spec))
         (unless (bindable var-p)
           (compile-error form "~s isn't a bindable symbol" var-p))
         :)
        ((sym init . more)
         (unless (listp more)
           (compile-error form "invalid dotted form ~s" key-spec))
         :)
        ((sym . more)
         (unless (listp more)
           (compile-error form "invalid dotted form ~s" key-spec))
         (unless (bindable sym)
           (compile-error form "~s isn't a bindable symbol" sym)))))
    (let* ((key-syms [mapcar first key-params])
           (key-syms-p (remq nil [mapcar third key-params])))
      (list eff-param
            ^(let (,*key-syms ,*key-syms-p)
               ,*(sys:stuff-key-params key-params rest-param)
               ,*body)))))
