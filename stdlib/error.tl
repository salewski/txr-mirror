;; Copyright 2017-2021
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

(defun sys:dig (ctx)
  (whilet ((form (sys:ctx-form ctx))
           (anc (unless (source-loc form)
                  (macro-ancestor form))))
    (set ctx anc))
  ctx)

(defun sys:loc (ctx)
  (source-loc-str (sys:ctx-form ctx)))

(defun compile-error (ctx fmt . args)
  (let* ((nctx (sys:dig ctx))
         (loc (sys:loc nctx))
         (name (sys:ctx-name nctx)))
    (let ((msg (fmt `@loc: ~s: @fmt` name . args)))
      (when (and *load-recursive*
                 (null (find-frame 'error 'catch-frame)))
        (dump-deferred-warnings *stderr*)
        (put-line msg *stderr*))
      (throw 'eval-error msg))))

(defun compile-warning (ctx fmt . args)
  (let* ((nctx (sys:dig ctx))
         (loc (sys:loc nctx))
         (name (sys:ctx-name nctx)))
    (usr:catch
      (throwf 'warning `@loc: warning: ~s: @fmt` name . args)
      (continue ()))))

(defun compile-defr-warning (ctx tag fmt . args)
  (let* ((nctx (sys:dig ctx))
         (loc (sys:loc nctx))
         (name (sys:ctx-name nctx)))
    (usr:catch
      (throw 'defr-warning (fmt `@loc: warning: ~s: @fmt` name . args)  tag)
      (continue ()))))

(defun sys:bind-mac-error (ctx-form params obj too-few-p)
  (cond
    ((atom obj)
     (compile-error ctx-form "extra element ~s not matched by params ~a"
                    obj params))
    ((null obj)
     (compile-error ctx-form "params ~a require arguments" params))
    (t (compile-error ctx-form "too ~a elements in ~s for params ~a"
                      (if too-few-p "few" "many")
                      obj params))))

(defun sys:bind-mac-check (ctx-form params obj req fix)
  (if (and obj (atom obj))
    (compile-error ctx-form "extra element ~s not matched by params ~a"
                   obj params)
    (let ((l (len obj)))
      (iflet ((problem (cond
                         ((< l req) "few")
                         ((and fix (> l fix)) "many"))))
        (if (zerop l)
          (compile-error ctx-form "params ~a require arguments" params)
          (compile-error ctx-form "too ~a elements in ~s for params ~a"
                         problem obj params))))))

(defun lambda-too-many-args (form)
  (compile-error form "excess arguments given"))

(defun lambda-too-few-args (form)
  (compile-error form "inufficient arguments given"))

(defun lambda-short-apply-list ()
  (throwf 'eval-error "~s: applied argument list too short" 'lambda))
