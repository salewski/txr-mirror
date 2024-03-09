;; Copyright 2017-2024
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

(defstruct usr:compile-opts ()
  usr:shadow-fun
  usr:shadow-var
  usr:shadow-cross
  usr:unused
  usr:constant-throws
  usr:log-level)

(defsymacro %warning-syms% '(shadow-fun shadow-var shadow-cross
                             unused log-level constant-throws))

(defvar usr:*compile-opts* (new compile-opts unused t constant-throws t))

(defmacro when-opt (compile-opt . forms)
  (with-gensyms (optval)
    ^(whenlet ((,optval usr:*compile-opts*.,compile-opt))
       (macrolet ((diag (. args)
                    ^(opt-controlled-diag ,',optval ,*args)))
         ,*forms))))

(defun opt-controlled-diag (optval . args)
  (caseq optval
    (:error (compile-error . args))
    ((t :warn) (compile-warning . args))))
