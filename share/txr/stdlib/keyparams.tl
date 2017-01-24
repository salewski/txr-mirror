;; Copyright 2017
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

(defun sys:extract-keys (keys args)
  (build
    (each ((k keys))
      (iflet ((f (memp (car k) args)))
        (add (cadr f))
        (add (cdr k))))))

(defun sys:extract-keys-p (keys args)
  (build
    (each ((k keys))
      (add (if (memp k args) t)))))

(defun sys:build-key-list (key-params)
  (let ((constant (group-by [chain second constantp] key-params)))
    (let ((var-keys (mapcar (ret ^(cons ',(intern (symbol-name (first @1))
                                                  'keyword)
                                        ,(second @1)))
                            [constant nil]))
          (const-keys (mapcar (op cons
                                (intern (symbol-name (first @1)) 'keyword)
                                (second @1))
                              [constant t])))
      (cond
        ((empty const-keys)
         ^(list ,*var-keys))
        ((empty var-keys)
         ^',const-keys)
        (t ^(list* ,*var-keys ',const-keys))))))

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
        ((var init var-p . junk)
         (when (consp junk)
           (compile-error form "superfluous forms in ~s" key-spec))
         (when junk
           (compile-error form "invalid dotted form ~s" key-spec))
         (unless (bindable var-p)
           (compile-error form "~s isn't a bindable symbol" var-p))
         :)
        ((var init . more)
         (unless (listp more)
           (compile-error form "invalid dotted form ~s" key-spec))
         :)
        ((var . more)
         (unless (listp more)
           (compile-error form "invalid dotted form ~s" key-spec))
         (unless (bindable var)
           (compile-error form "~s isn't a bindable symbol" var)))))
    (let* ((key-params-p [keep-if third key-params])
           (key-vars [mapcar first key-params])
           (key-vars-p [mapcar third key-params-p])
           (keys (sys:build-key-list key-params))
           (keys-p (mapcar (op intern (symbol-name (first @1)) 'keyword)
                           key-params-p)))
      (list eff-param
            ^(tree-bind ,key-vars
                        (sys:extract-keys ,keys ,rest-param)
               ,*(if keys-p
                   ^((tree-bind ,key-vars-p
                                (sys:extract-keys-p ',keys-p ,rest-param)
                        ,*body))
                   body))))))
