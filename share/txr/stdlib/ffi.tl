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

(defmacro sys:dlib-expr (spec)
  (typecase spec
    (null ^(dlopen))
    (str ^(dlopen ,spec rtld-now))
    (t spec)))

(defmacro with-dyn-lib (lib . body)
  (let ((keep-var (gensym "lib-")))
    ^(prog1
       (defvarl ,keep-var (sys:dlib-expr ,lib))
       (symacrolet ((sys:ffi-lib ,keep-var))
         ,*body))))

(defun sys:analyze-argtypes (form argtypes)
  (let ((p (posq : argtypes)))
    (when p
      (if (zerop p)
        (compile-error form "variadic with zero fixed arguments not allowed")
        (del [argtypes p])))
    (list* (length argtypes) p argtypes)))

(defmacro deffi (:form f name fun-expr rettype argtypes)
  (let ((fun-ref (cond
                   ((stringp fun-expr)
                      ^(dlsym-checked sys:ffi-lib ,fun-expr))
                   ((consp fun-expr)
                      (mac-param-bind f (sym ver) fun-expr
                        ^(dlvsym-checked sys:ffi-lib ,sym ,ver)))
                   (t fun-expr)))
        (args-sym (gensym "args-"))
        (ret-type-sym (gensym "ret-type-"))
        (arg-types-sym (gensym "arg-types-"))
        (call-desc-sym (gensym "call-desc-"))
        (fun-sym (gensym "ffi-fun-")))
    (tree-bind (nargs nvariadic . argtypes) (sys:analyze-argtypes f argtypes)
      ^(progn
         (defvarl ,ret-type-sym (ffi-type-compile ',rettype))
         (defvarl ,arg-types-sym [mapcar ffi-type-compile ',argtypes])
         (defvarl ,call-desc-sym (ffi-make-call-desc ,nargs ,nvariadic
                                                     ,ret-type-sym
                                                     ,arg-types-sym))
         (defvarl ,fun-sym ,fun-ref)
         (defun ,name ,args-sym
           (ffi-call ,fun-sym ,call-desc-sym . ,args-sym))))))

(defmacro deffi-type (name type-expr)
  ^(ffi-typedef ',name (ffi-type-compile ',type-expr)))

(defmacro typedef (name type-expr)
  ^(ffi-typedef ',name (ffi-type-compile ',type-expr)))

(defun sys:deffi-cb-expander (f name rettype argtypes safe-p abort-retval)
  (let ((ret-type-sym (gensym "ret-type-"))
        (arg-types-sym (gensym "arg-types-"))
        (call-desc-sym (gensym "call-desc-"))
        (fun-sym (gensym "fun-")))
    (tree-bind (nargs nvariadic . argtypes) (sys:analyze-argtypes f argtypes)
      ^(progn
         (defvarl ,ret-type-sym (ffi-type-compile ',rettype))
         (defvarl ,arg-types-sym [mapcar ffi-type-compile ',argtypes])
         (defvarl ,call-desc-sym (ffi-make-call-desc ,nargs ,nvariadic
                                                     ,ret-type-sym
                                                     ,arg-types-sym))
         (defun ,name (,fun-sym)
           [ffi-make-closure ,fun-sym ,call-desc-sym ,safe-p ,abort-retval])))))

(defmacro deffi-cb (:form f name rettype argtypes : abort-retval)
  (sys:deffi-cb-expander f name rettype argtypes t abort-retval))

(defmacro deffi-cb-unsafe (:form f name rettype argtypes)
  (sys:deffi-cb-expander f name rettype argtypes nil nil))

(defmacro sizeof (type)
  (ffi-size (ffi-type-compile type)))

(defmacro ffi (type)
  ^(ffi-type-compile ',type))

(define-accessor carray-ref carray-refset)
