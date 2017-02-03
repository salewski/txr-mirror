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

(defun sys:loc (ctx)
  (iflet ((loc (source-loc-str (sys:ctx-form ctx))))
    `(@loc) ` ""))

(defun compile-error (ctx fmt . args)
  (let ((loc (sys:loc ctx))
        (name (sys:ctx-name ctx)))
    (dump-deferred-warnings *stderr*)
    (throwf 'eval-error `@loc~s: @fmt` name . args)))

(defun compile-warning (ctx fmt . args)
  (let ((loc (sys:loc ctx))
        (name (sys:ctx-name ctx)))
    (throwf 'warning `@loc~s: @fmt` name . args)))

(defun compile-defr-warning (ctx tag fmt . args)
  (let ((loc (sys:loc ctx))
        (name (sys:ctx-name ctx)))
    (catch
      (throw 'warning (fmt `@loc~s: @fmt` name . args) . tag)
      (continue ()))))
