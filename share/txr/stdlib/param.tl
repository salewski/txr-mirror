;; Copyright 2019
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

(compile-only
  (defstruct param-parser-base nil
    syntax form
    rest req opt key
    nreq nopt nfix

    (:postinit (me)
      (let* ((rest (nthlast 0 me.syntax))
             (fixed (ldiff me.syntax rest))
             nonkey key)
        (cond
          (me.mac-param-p
            (while fixed
              (let ((pp (pop fixed)))
                (caseq pp
                  ((:env :whole :form)
                   (unless fixed
                     (compile-error me.form "~s requires argument" pp))
                   (push (cons pp (pop fixed)) key))
                  (t (push pp nonkey)))))
            (set nonkey (nreverse nonkey)
                 key (nreverse key)))
          (t (set nonkey fixed)))
        (tree-bind (: rp opt) (split* nonkey (op where (op eq :)))
          (set me.rest rest
               me.req rp
               me.opt (mapcar [iffi atom list] opt)
               me.key key
               me.nreq (len rp)
               me.nopt (len opt)
               me.nfix (+ me.nreq me.nopt))))))

  (defstruct (fun-param-parser syntax form) param-parser-base
    (mac-param-p nil))

  (defstruct (mac-param-parser syntax form) param-parser-base
    (mac-param-p t)))

