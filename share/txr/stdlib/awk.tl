;; Copyright 2016
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

(load "conv.tl")

(defstruct sys:awk-state ()
  (rs "\n") krs
  fs ft kfs
  (ofs " ")
  (ors "\n")
  (inputs)
  (output *stdout*)
  (file-num 0)
  (file-rec-num 0)
  (rec-num 0)
  rec orig-rec fields nf
  rng-vec (rng-n 0)
  par-mode par-mode-fs par-mode-prev-fs
  (streams (hash :equal-based))
  (:fini (self)
    (dohash (k v self.streams)
      (close-stream v)))
  (:postinit (self)
    (set self.inputs (or self.inputs (zap *args*) (list *stdin*)))
    (if (plusp self.rng-n)
      (set self.rng-vec (vector self.rng-n)))
    (unless (streamp self.output)
      (set self.output (open-file self.output "w")))))

(defstruct sys:awk-compile-time ()
  inputs output name lets
  begin-file-actions end-file-actions
  begin-actions end-actions
  cond-actions
  (nranges 0)
  (rng-rec-temp (gensym))
  rng-expr-temps
  rng-exprs)

(defmeth sys:awk-state rec-to-f (self)
  (cond
    (self.fs
      (when self.ft
        (throwf 'eval-error "awk: both fs and ft set"))
      (if (and (not self.kfs) (equal self.rec ""))
        (set self.fields nil
             self.nf 0)
        (let ((eff-fs (if self.par-mode
                        (if (equal self.fs self.par-mode-prev-fs)
                          self.par-mode-fs
                          (set self.par-mode-prev-fs self.fs
                               self.par-mode-fs
                               (regex-compile ^(or ,(if (regexp self.fs)
                                                      (regex-source self.fs)
                                                      self.fs)
                                                   "\n"))))
                        self.fs)))
          (set self.fields (split-str self.rec eff-fs self.kfs)
               self.nf (length self.fields)))))
    (self.ft
      (set self.fields (tok-str self.rec self.ft self.kfs)
           self.nf (length self.fields)))
    ((set self.fields (tok-str self.rec #/[^ \t\n]+/ self.kfs)
          self.nf (length self.fields)))))

(defmeth sys:awk-state f-to-rec (self)
  (set self.rec `@{self.fields self.ofs}`))

(defmeth sys:awk-state nf-to-f (self)
  (set self.fields (take self.nf (append self.fields (repeat '("")))))
  self.(f-to-rec))

(defmeth sys:awk-state loop (aws func beg-file-func end-file-func)
  (whilet ((in (pop aws.inputs)))
    (block :awk-file
      (inc aws.file-num)
      (when beg-file-func
        [beg-file-func aws])
      (let* ((stin (if (streamp in) in (open-file in)))
             (noted-rs (not aws.rs))
             (noted-krs (not aws.krs))
             (cached-rr nil))
        (flet ((get-rec-reader (stin)
                 (cond
                   ((and (equal noted-rs aws.rs) (eq noted-krs aws.krs))
                    cached-rr)
                   (t
                     (set noted-rs aws.rs noted-krs aws.krs)
                     (set cached-rr
                          (cond
                            ((and (equal aws.rs "\n") (not aws.krs))
                               (set aws.par-mode nil)
                               (lambda () (get-line stin)))
                            ((null aws.rs)
                               (set aws.par-mode t)
                               (let ((rin (record-adapter #/\n[ \n\t]*\n/))
                                     (flag t))
                                 (lambda ()
                                   (let ((r (get-line rin)))
                                     (cond
                                       (flag
                                         (set flag nil)
                                         (if (equal r "")
                                           (get-line rin)
                                           r))
                                       (t r))))))
                            (t
                              (set aws.par-mode nil)
                              (let ((rin (record-adapter (if (regexp aws.rs)
                                                           aws.rs
                                                           (regex-compile aws.rs))
                                                         stin
                                                         aws.krs)))
                                (lambda () (get-line rin))))))))))
          (set aws.file-rec-num 0)
          (unwind-protect
            (whilet ((rr (get-rec-reader stin))
                     (rec (call rr)))
              (set aws.rec rec aws.orig-rec rec)
              (inc aws.rec-num)
              (inc aws.file-rec-num)
              aws.(rec-to-f)
              (block :awk-rec
                [func aws]))
            (when end-file-func
              [end-file-func aws])))))))

(defmeth sys:awk-state prn (self . args)
  (cond
    (args (for ((a args) next) (a) ((set a next))
            (put-string `@(car a)`)
            (put-string (if (set next (cdr a)) self.ofs self.ors))))
    (t (put-string self.rec)
       (put-string self.ors))))

(defmeth sys:awk-state ensure-stream (self kind path mode)
  (hash-update-1 self.streams
                 ^(,kind ,path)
                 (do or @1 (caseq kind
                             (:inf (open-file path "r"))
                             (:outf (open-file path "w"))
                             (:inp (open-command path "r"))
                             (:outp (open-command path "w"))))
                 nil))

(defmeth sys:awk-state close-or-flush (self stream kind path val)
  (cond
    ((eq val :close) (whenlet ((s (del [self.streams ^(,kind ,path)])))
                       (close-stream s)))
    ((memq kind '(:outf outp)) (flush-stream stream) val)
    (val)))

(defun sys:awk-test (val rec)
  (caseq (typeof val)
    ((regex fun) (call val rec))
    (t val)))

(defmacro sys:awk-redir (aws-sym stream-var kind mode path body)
  (with-gensyms (res-sym)
    ^(let ((,stream-var (qref ,aws-sym (ensure-stream ,kind ,path, mode))))
       ,(if body
          ^(qref ,aws-sym (close-or-flush ,stream-var ,kind ,path
                                          (progn ,*body)))
          stream-var))))

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
                                 (unless (car actions)
                                   (throwf 'eval-error
                                           "awk: null :name not permitted"))
                                 (unless (symbolp (car actions))
                                   (throwf 'eval-error
                                           "awk: :name must be a symbol"))
                                 (set awc.name (car actions)))
                               (:let (push actions awc.lets))
                               (:begin (push actions awc.begin-actions))
                               (:set (push ^((set ,*actions)) awc.begin-actions))
                               (:end (push actions awc.end-actions))
                               (:begin-file (push actions awc.begin-file-actions))
                               (:set-file (push ^((set ,*actions)) awc.begin-actions))
                               (:end-file (push actions awc.end-file-actions))
                               (t (push (if actions
                                          cl
                                          ^(,pattern (prn)))
                                        awc.cond-actions))))
        (junk (throwf 'eval-error "awk: bad clause syntax ~s" junk))))
    (set awc.lets [apply append (nreverse awc.lets)]
         awc.begin-actions [apply append (nreverse awc.begin-actions)]
         awc.end-actions [apply append (nreverse awc.end-actions)]
         awc.begin-file-actions [apply append (nreverse awc.begin-file-actions)]
         awc.end-file-actions [apply append (nreverse awc.end-file-actions)]
         awc.cond-actions (nreverse awc.cond-actions))
    awc))

(defmacro sys:awk-let (awc aws-sym . body)
  ^(symacrolet ((rec (rslot ,aws-sym 'rec 'rec-to-f))
                (orec (rslot ,aws-sym 'orig-rec 'rec-to-f))
                (f (rslot ,aws-sym 'fields 'f-to-rec))
                (nf (rslot ,aws-sym 'nf 'nf-to-f))
                (nr (qref ,aws-sym rec-num))
                (fnr (qref ,aws-sym file-rec-num))
                (arg (qref ,aws-sym file-num))
                (rs (qref ,aws-sym rs))
                (krs (qref ,aws-sym krs))
                (fs (qref ,aws-sym fs))
                (ft (qref ,aws-sym ft))
                (kfs (qref ,aws-sym kfs))
                (ofs (qref ,aws-sym ofs))
                (ors (qref ,aws-sym ors)))
     (macrolet ((next () '(return-from :awk-rec))
                (next-file () '(return-from :awk-file))
                (prn (. args) ^(qref ,',aws-sym (prn ,*args)))
                (sys:rng (from-expr to-expr :env e)
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
                    rng-temp))
                (rng (from-expr to-expr)
                  ^(sys:rng (sys:awk-test ,from-expr ,(qref ,awc rng-rec-temp))
                            (sys:awk-test ,to-expr ,(qref ,awc rng-rec-temp))))
                (ff (. opip-args)
                  ^(symacrolet ((f (rslot ,',aws-sym 'fields 'f-to-rec)))
                     (set f [(opip ,*opip-args) f])))
                (mf (. opip-args)
                  ^(symacrolet ((f (rslot ,',aws-sym 'fields 'f-to-rec)))
                     (set f (mapcar (opip ,*opip-args) f))))
                (fconv (. conv-args)
                  ^(set f (sys:conv (,*conv-args) f)))
                (-> (path . body)
                  ^(sys:awk-redir ,',aws-sym *stdout* :outf "w" ,path ,body))
                (->> (path . body)
                  ^(sys:awk-redir ,',aws-sym *stdout* :apf "a" ,path ,body))
                (<- (path . body)
                  ^(sys:awk-redir ,',aws-sym *stdin* :inf "r" ,path ,body))
                (!> (path . body)
                  ^(sys:awk-redir ,',aws-sym *stdout* :outp "w" ,path ,body))
                (<! (path . body)
                  ^(sys:awk-redir ,',aws-sym *stdin* :inp "w" ,path ,body)))
       ,*body)))

(defmacro awk (:env e . clauses)
  (let ((awc (sys:awk-expander clauses)))
    (with-gensyms (aws-sym awk-begf-fun awk-fun awk-endf-fun awk-retval)
      (let* ((p-actions-xform-unex (mapcar (aret ^(when (sys:awk-test ,@1 rec)
                                                    ,*@rest))
                                           awc.cond-actions))
             (p-actions-xform (sys:expand
                                ^(sys:awk-let ,awc ,aws-sym
                                   ,*p-actions-xform-unex)
                                e)))
        ^(block ,(or awc.name 'awk)
           (let* (,*awc.lets ,awk-retval)
             (sys:awk-let ,awc ,aws-sym
               (let* ((,aws-sym (new sys:awk-state
                                     ,*(if awc.inputs ^(inputs (list ,*awc.inputs)))
                                     ,*(if awc.output ^(output ,awc.output))
                                     rng-n (macro-time (qref ,awc nranges))))
                      ,*(if awc.output
                          ^((*stdout* (qref ,aws-sym output))))
                      ,*(if (and awc.cond-actions awc.begin-file-actions)
                          ^((,awk-begf-fun (lambda (,aws-sym)
                               ,*awc.begin-file-actions))))
                      ,*(if (and awc.cond-actions awc.end-file-actions)
                          ^((,awk-endf-fun (lambda (,aws-sym)
                              ,*awc.end-file-actions))))
                      ,*(if awc.cond-actions
                          ^((,awk-fun (lambda (,aws-sym)
                                        ,(if awc.rng-exprs
                                           ^(let* ((,awc.rng-rec-temp rec)
                                               ,*(nreverse
                                                   (zip awc.rng-expr-temps
                                                        awc.rng-exprs)))
                                              ,p-actions-xform)
                                           p-actions-xform))))))
                 ,*awc.begin-actions
                   (unwind-protect
                     ,(if (or awc.cond-actions awc.begin-file-actions
                              awc.end-file-actions awc.end-actions)
                        ^(qref ,aws-sym (loop ,awk-fun
                                          ,(if awc.begin-file-actions
                                             awk-begf-fun)
                                          ,(if awc.end-file-actions
                                             awk-endf-fun))))
                     (set ,awk-retval (progn ,*awc.end-actions))
                     (call-finalizers ,aws-sym))
                   ,awk-retval))))))))
