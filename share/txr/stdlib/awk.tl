;; Copyright 2016
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

(defstruct sys:awk-state ()
  (rs "\n")
  (fs #/[ \t\n]+/)
  (ofs " ")
  (ors "\n")
  (inputs (or *args* (list *stdin*)))
  (output *stdout*)
  (file-num 0)
  (file-rec-num 0)
  (rec-num 0)
  rec fields nf rng-vec (rng-n 0)
  (:postinit (self)
    (if (plusp self.rng-n)
      (set self.rng-vec (vector self.rng-n)))
    (unless (streamp self.output)
      (set self.output (open-file self.output "w")))))

(defstruct sys:awk-compile-time ()
  inputs output name lets begin-actions end-actions cond-actions
  (nranges 0)
  rng-expr-temps
  rng-exprs)

(defmeth sys:awk-state rec-to-f (self)
  (cond
    ((equal self.rec "")
     (set self.fields nil)
     (set self.nf 0))
    (t
      (set self.fields (split-str self.rec self.fs))
      (set self.nf (length self.fields)))))

(defmeth sys:awk-state f-to-rec (self)
  (set self.rec (cat-str self.fields self.ofs)))

(defmeth sys:awk-state nf-to-f (self)
  (set self.fields (take self.nf (append self.fields (repeat '("")))))
  self.(f-to-rec))

(defmeth sys:awk-state loop (aws func)
  (whilet ((in (pop aws.inputs)))
    (block :awk-file
      (inc aws.file-num)
      (let ((recin (record-adapter (if (regexp aws.rs)
                                     aws.rs
                                     (regex-compile aws.rs))
                                   (if (streamp in)
                                     in
                                     (open-file in)))))
        (set aws.file-rec-num 0)
        (whilet ((rec (get-line recin)))
          (set aws.rec rec)
          (inc aws.rec-num)
          (inc aws.file-rec-num)
          aws.(rec-to-f)
          (block :awk-rec
            (let ((*stdout* aws.output))
              [func aws])))))))

(defmeth sys:awk-state prn (self . args)
  (put-string `@{(if args args self.rec) self.ofs}@{self.ors}`))

(defun sys:awk-expander (clauses)
  (let ((awc (new sys:awk-compile-time)))
    (each ((cl clauses))
      (tree-case cl
        ((pattern . actions) (caseql pattern
                               (:inputs
                                 (when awc.inputs
                                   (throwf 'eval-error
                                           "awk: duplicate :input clauses"))
                                 (set awc.inputs actions))
                               (:output
                                 (when awc.output
                                   (throwf 'eval-error
                                           "awk: duplicate :input clauses"))
                                 (when (or (atom actions) (cdr actions))
                                   (throwf 'eval-error
                                           "awk: bad :output syntax"))
                                 (set awc.output (car actions)))
                               (:name
                                 (when awc.name
                                   (throwf 'eval-error
                                           "awk: duplicate :name clauses"))
                                 (when (or (atom actions) (cdr actions))
                                   (throwf 'eval-error
                                           "awk: bad :name syntax"))
                                 (set awc.name (car actions)))
                               (:let (push actions awc.lets))
                               (:begin (push actions awc.begin-actions))
                               (:end (push actions awc.end-actions))
                               (t (push (if actions
                                          cl
                                          ^(,pattern (prn)))
                                        awc.cond-actions))))
        (junk (throwf 'eval-error "awk: bad clause syntax ~s" junk))))
    (set awc.lets [apply append (nreverse awc.lets)]
         awc.begin-actions [apply append (nreverse awc.begin-actions)]
         awc.end-actions [apply append (nreverse awc.end-actions)]
         awc.cond-actions (nreverse awc.cond-actions))
    awc))

(defmacro sys:awk-let (awc aws-sym . body)
  ^(symacrolet ((rec (rslot ,aws-sym 'rec 'rec-to-f))
                (f (rslot ,aws-sym 'fields 'f-to-rec))
                (nf (rslot ,aws-sym 'nf 'nf-to-f))
                (nr (qref ,aws-sym rec-num))
                (fnr (qref ,aws-sym file-rec-num))
                (arg (qref ,aws-sym file-num))
                (rs (qref ,aws-sym rs))
                (fs (qref ,aws-sym fs))
                (ofs (qref ,aws-sym ofs))
                (ors (qref ,aws-sym ors)))
     (macrolet ((next () '(return-from :awk-rec))
                (next-file () '(return-from :awk-file))
                (prn (. args) ^(qref ,',aws-sym (prn ,*args)))
                (rng (from-expr to-expr :env e)
                  (let ((ix (pinc (qref ,awc nranges)))
                        (rng-temp (gensym))
                        (from-expr-ex (sys:expand from-expr e))
                        (to-expr-ex (sys:expand to-expr e))
                        (flag-old (gensym))
                        (flag-new (gensym)))
                    (push rng-temp (qref ,awc rng-expr-temps))
                    (push ^(placelet ((flag (vecref (qref ,',aws-sym rng-vec) ,ix)))
                             (let* ((,flag-old flag) ,flag-new)
                               (when (or ,flag-old ,from-expr-ex)
                                 (set ,flag-new t))
                               (when (and ,flag-new ,to-expr-ex)
                                 (set ,flag-new nil))
                               (or (set flag ,flag-new) ,flag-old)))
                          (qref ,awc rng-exprs))
                    rng-temp)))
       ,*body)))

(defmacro awk (:env e . clauses)
  (let ((awc (sys:awk-expander clauses)))
    (with-gensyms (aws-sym awk-fun awk-retval)
      (let* ((p-actions-xform-unex (mapcar (aret ^(when ,@1 ,*@rest))
                                           awc.cond-actions))
             (p-actions-xform (sys:expand
                                ^(sys:awk-let ,awc ,aws-sym
                                   ,*p-actions-xform-unex)
                                e)))
        ^(let* (,*awc.lets ,awk-retval)
           (let* ((,aws-sym (new sys:awk-state
                                ,*(if awc.inputs ^(inputs (list ,*awc.inputs)))
                                ,*(if awc.output ^(output ,awc.output))
                                rng-n (macro-time (qref ,awc nranges))))
                  (,awk-fun (lambda (,aws-sym)
                              (sys:awk-let ,awc ,aws-sym
                                ,(if awc.rng-exprs
                                   ^(let* ,(nreverse
                                             (zip awc.rng-expr-temps
                                                  awc.rng-exprs))
                                      ,p-actions-xform)
                                   p-actions-xform)))))
             (sys:awk-let ,awc ,aws-sym
               ,*awc.begin-actions)
             (block ,awc.name
               (unwind-protect
                 (qref ,aws-sym (loop ,awk-fun))
                 (set ,awk-retval (progn ,*awc.end-actions)))
               ,awk-retval)))))))
