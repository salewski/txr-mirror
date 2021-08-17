;; Copyright 2019-2021
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
(defmacro with-disabled-debugging (. forms)
  (let ((state (gensym)))
    ^(let ((,state (dbg-clear dbg-all)))
       (unwind-protect
         (progn ,*forms)
         (dbg-restore ,state)))))

(defun make-command-env (command-table)
  (let ((env (make-env )))
    (mapdo (ado env-vbind env @1 ^(,@2)) command-table)
    env))

(defparml %dbg-commands% '((usr:? debugger-help "list command summary")
                           (usr:bt print-backtrace "print backtrace")))

(defparml %dbg-command-env% (make-command-env %dbg-commands%))

(defun debugger-help ()
  (mapdo (ap pprinl `@{@1 15} @3`) %dbg-commands%))

(defmeth fcall-frame loc (fr))

(defmeth fcall-frame print-trace (fr pr-fr nx-fr prefix)
  (let* ((fun fr.fun)
         (args fr.args)
         (name (if (functionp fun)
                 (func-get-name fun)))
         (loc (if nx-fr nx-fr.(loc)))
         (kind
           (cond
             ((interp-fun-p fun) "I")
             ((vm-fun-p fun) "V")
             ((functionp fun) "C")
             (t "O"))))
    (put-string `@prefix @kind:@(if loc `(@loc):`)`)
    (prinl ^[,(or name fun) ,*args])))

(defmeth eval-frame loc (fr)
  (source-loc-str fr.form))

(defmeth eval-frame print-trace (fr pr-fr nx-fr prefix)
  (when (or (null nx-fr)
            (and (typep pr-fr 'fcall-frame)
                 (not (interp-fun-p pr-fr.fun))
                 (not (vm-fun-p pr-fr.fun))))
    (let* ((form fr.form)
           (sym (if (consp form) (car form)))
           (loc (source-loc-str form)))
      (when sym
        (put-string `@prefix E:@(if loc `(@loc):`)`)
        (prinl (if (eq sym 'dwim)
                 ^[,(cadr form)]
                 ^(,sym)))))))

(defmeth expand-frame print-trace (fr pr-fr nx-fr prefix)
  (let* ((form fr.form)
         (loc (source-loc-str form)))
    (put-string `@prefix X:@(if loc `(@loc):`)`)
    (prinl form)))

(defmeth expand-frame loc (fr)
  (source-loc-str fr.form))

(defun print-backtrace (: (*stdout* *stdout*) (prefix ""))
  (with-resources ((imode (set-indent-mode *stdout* indent-foff)
                          (set-indent-mode *stdout* imode))
                   (depth (set-max-depth *stdout* 2)
                          (set-max-depth *stdout* depth))
                   (length (set-max-length *stdout* 10)
                           (set-max-length *stdout* length)))
    (window-mapdo 1 nil (lambda (pr el nx) el.(print-trace pr nx prefix))
                  (find-frames-by-mask (logior uw-fcall uw-eval uw-expand)))))

(defun debugger ()
  (with-disabled-debugging
    (sys:repl nil *stdin* *stdout* %dbg-command-env%)))
