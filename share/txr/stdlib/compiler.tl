;; Copyright 2018
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

(in-package :sys)

(defstruct (frag oreg code : fvars ffuns) nil
  oreg
  code
  fvars
  ffuns)

(defstruct binding nil
  sym
  loc
  sys:env)

(defstruct sys:env nil
  vb
  fb
  up
  co
  lev
  (v-cntr 0)

  (:postinit (me)
    (unless me.lev
      (set me.lev (if me.up (succ me.up.lev) 1)))
    (unless (or me.co (null me.up))
      (set me.co me.up.co)
      me.co.(new-env me)))

  (:method lookup-var (me sym)
    (condlet
      (((cell (assoc sym me.vb))) (cdr cell))
      (((up me.up)) up.(lookup-var sym))
      (t nil)))

  (:method lookup-fun (me sym)
    (condlet
      (((cell (assoc me.fb sym))) (cdr cell))
      (((up me.up)) up.(lookup-fun sym))
      (t nil)))

  (:method extend-var (me sym)
    (let* ((loc ^(v ,(ppred me.lev) ,(pinc me.v-cntr)))
           (bn (new binding sym sym loc loc env me)))
      (set me.vb (acons-new sym bn me.vb))))

  (:method extend-fun (me sym)
    (let* ((loc ^(v ,me.lev ,(pinc me.v-cntr)))
           (bn (new binding sym sym loc loc env me)))
      (set me.fb (acons-new sym bn me.fb)))))

(defstruct compiler nil
  (dreg-cntr 0)
  (nlev 2)
  (nreg 1)
  (tregs (mapcar (op list t) (range 1 255)))
  (dreg (hash :eql-based))
  (data (hash :eql-based))
  last-form)

(defmeth compiler get-dreg (me atom)
  (iflet ((dreg [me.dreg atom]))
    dreg
    (let* ((dreg ^(d ,(pinc me.dreg-cntr))))
      (set [me.data (cadr dreg)] atom)
      (set [me.dreg atom] dreg))))

(defmeth compiler get-datavec (me)
  (vec-list [mapcar me.data (range* 0 me.dreg-cntr)]))

(defmeth compiler alloc-treg (me)
  (let ((treg (pop me.tregs)))
    (unless treg
      (compile-error me.last-form "code too complex: out of registers"))
    (set me.nreg (max me.nreg (succ (cadr treg))))
    treg))

(defmeth compiler free-treg (me treg)
  (when (and (eq t (car treg)) (neq 0 (cadr treg)))
    (push treg me.tregs)))

(defmeth compiler free-tregs (me tregs)
  (mapdo (meth me free-treg) tregs))

(defmeth compiler new-env (me env)
  (when (>= env.lev me.nlev)
    (set me.nlev (succ env.lev))))

(defmeth compiler compile (me env form)
  (set me.last-form form)
  (cond
    ((null form) (new (frag '(t 0) nil)))
    ((and (symbolp form)
          (not (bindable form))) me.(comp-atom form))
    ((symbolp form) me.(comp-var env form))
    ((atom form) me.(comp-atom form))
    ((consp form)
     (let ((sym (car form)))
       (cond
         ((special-operator-p sym)
          (caseq sym
            (quote me.(comp-atom (cadr form)))
            (sys:setq me.(comp-setq env (cdr form)))
            (block me.(comp-block env (cdr form)))
            ((let let*) me.(comp-let env sym (cdr form)))
            (lambda me.(comp-lambda env (cdr form)))
            (sys:for-op me.(comp-for env (cdr form)))
            (progn me.(comp-progn env (cadr form)))
            (prog1 me.(comp-prog1 env form))
            (sys:dvbind me.(compile env (caddr form)))
            (sys:with-dyn-rebinds me.(comp-progn env (cddr form)))
            ((macrolet symacrolet macro-time)
             (compile-error form "unexpanded ~s encountered" sym))
            ((sys:var sys:expr)
             (compile-error form "meta with no meaning: ~s " form))
            ((usr:qquote usr:unquote usr:splice
              sys:qquote sys:unquote sys:splice)
             (compile-error form "unexpanded quasiquote encountered"))
            (t
              (compile-error form "special op ~s not handled yet" sym))))
         ((bindable sym) me.(comp-call env sym (cdr form)))
         (t (compile-error form "invalid compound form")))))))

(defmeth compiler comp-atom (me form)
  (cond
    ((or (and (integerp form)
              (< (width form) 32))
         (chrp form))
     (let ((oreg me.(alloc-treg)))
       (new (frag oreg ^((movi ,oreg ,form))))))
    (t (let ((dreg me.(get-dreg form)))
         (new (frag dreg nil))))))

(defmeth compiler comp-var (me env sym)
  (iflet ((vbin env.(lookup-var sym)))
    (new (frag vbin.loc nil (list sym)))
    (let ((oreg me.(alloc-treg))
          (dreg me.(get-dreg sym)))
      (new (frag oreg ^((getv ,oreg ,dreg)) (list sym))))))

(defmeth compiler comp-setq (me env args)
  (tree-bind (sym value) args
    (let* ((bind env.(lookup-var sym))
           (vloc (if bind
                   bind.loc
                   me.(get-dreg sym)))
           (vfrag me.(compile env value)))
      me.(free-treg vfrag.oreg)
      (new (frag vloc
                 ^(,*vfrag.code
                   ,*(if bind
                       ^((mov ,vloc ,vfrag.oreg))
                       ^((setv ,vloc ,vfrag.oreg))))
                 (uni (list sym) vfrag.fvars)
                 vfrag.ffuns)))))

(defmeth compiler comp-block (me env args)
  (tree-bind (name . body) args
    (let* ((dreg me.(get-dreg name))
           (bfrag me.(comp-progn env body))
           (lskip (gensym "l"))
           (oreg (if (equal bfrag.oreg '(t 0))
                   me.(alloc-treg)
                   bfrag.oreg)))
      me.(free-treg bfrag.oreg)
      (new (frag oreg
                 ^((block ,oreg ,dreg ,lskip)
                   ,*bfrag.code
                   (end ,bfrag.oreg)
                   ,lskip)
                 bfrag.fvars
                 bfrag.ffuns)))))

(defmeth compiler comp-let (me env sym args)
  (tree-bind (raw-vis . body) args
    (let* ((vis (mapcar [iffi atom list] raw-vis))
           (specials [keep-if special-var-p vis car])
           (lexsyms [remove-if special-var-p [mapcar car vis]])
           (specials-occur [find-if special-var-p vis car])
           (frsize (len lexsyms))
           (seq (eq sym 'let*))
           (nenv (new env up env co me))
           (fenv (if seq nenv env)))
      (unless seq
        (each ((lsym lexsyms))
          nenv.(extend-var lsym)))
      (let* (ffuns fvars
             (code (build
                     (add ^(,(if specials-occur 'dframe 'frame)
                            ,nenv.lev ,frsize))
                     (each ((vi vis))
                       (tree-bind (sym : form) vi
                         (cond
                           ((special-var-p sym)
                            (let ((frag me.(compile fenv form))
                                  (dreg me.(get-dreg sym)))
                              (pend frag.code)
                              (add ^(bindv ,frag.oreg ,dreg))
                              me.(free-treg frag.oreg)
                              (set ffuns (uni ffuns frag.ffuns)
                                   fvars (uni fvars frag.fvars))))
                           (form
                             (let ((frag me.(compile fenv form))
                                   (bind (progn
                                           (if seq nenv.(extend-var sym))
                                           nenv.(lookup-var sym))))
                               (pend frag.code)
                               (add ^(mov ,bind.loc ,frag.oreg))
                               me.(free-treg frag.oreg)
                               (set ffuns (uni ffuns frag.ffuns)
                                    fvars (uni fvars frag.fvars)))))))))
             (bfrag me.(comp-progn nenv body)))
        (new (frag bfrag.oreg
                   (append code bfrag.code ^((end ,bfrag.oreg)))
                   (uni (diff bfrag.fvars lexsyms) fvars)
                   (uni ffuns bfrag.ffuns)))))))

(defmeth compiler comp-lambda (me env args)
  (tree-bind (pars . body) args
    (let* ((rest-par (nthlast 0 pars))
           (fixed-pars (ldiff pars rest-par))
           (need-frame (or fixed-pars rest-par))
           lexsyms specials)
      (tree-bind (: req-pars raw-opt-pars) (split* fixed-pars
                                                   (op where (op eq :)))
        (let* ((opt-pars (mapcar [iffi atom list] raw-opt-pars))
               (nenv (if need-frame (new env up env co me) env))
               (nreq (len req-pars))
               (nfixed (+ nreq (len opt-pars))))
          (flet ((spec-sub (sym)
                   (cond
                     ((special-var-p sym)
                      (let ((sub (gensym)))
                        (push (cons sym sub) specials)
                        nenv.(extend-var sub)
                        sub))
                     (t
                       (push sym lexsyms)
                       nenv.(extend-var sym)
                       sym))))
            (set req-pars (collect-each ((rp req-pars))
                            (spec-sub rp)))
            (set opt-pars (collect-each ((op opt-pars))
                            (tree-bind (var-sym : init-form have-sym) op
                              (list (spec-sub var-sym)
                                    init-form
                                    (if have-sym (spec-sub have-sym))))))
            (when rest-par
              (set rest-par (spec-sub rest-par)))
            (upd specials nreverse)
            (let* ((col-reg (if opt-pars me.(get-dreg :)))
                   (tee-reg (if opt-pars me.(get-dreg t)))
                   (ifrags (collect-each ((op opt-pars))
                             (let* ((init-form (cadr op))
                                    (init-frag me.(compile env init-form)))
                               me.(free-treg init-frag.oreg)
                               init-frag)))
                   (opt-code (append-each ((op opt-pars)
                                           (ifrg ifrags))
                               (tree-bind (var-sym : init-form have-sym) op
                                 (let ((var-bind nenv.(lookup-var var-sym))
                                       (have-bind nenv.(lookup-var have-sym))
                                       (lskip (gensym "l")))
                                   ^(,*(if have-sym
                                         ^((mov ,have-bind.loc ,tee-reg)))
                                      (ifq ,var-bind.loc ,col-reg ,lskip)
                                      ,*(if have-sym
                                          ^((mov ,have-bind.loc nil)))
                                      ,*ifrg.code
                                      (mov ,var-bind.loc ,ifrg.oreg)
                                      ,lskip)))))
                   (benv (if specials (new env up nenv co me) nenv))
                   (bfrag me.(comp-progn benv body))
                   (oreg me.(alloc-treg))
                   (lskip (gensym "l-"))
                   (frsize nenv.v-cntr))
              me.(free-treg bfrag.oreg)
              (new (frag oreg
                         ^((close ,oreg ,frsize ,lskip ,nfixed ,nreq
                                  ,(if rest-par t nil)
                                  ,*(collect-each ((rp req-pars))
                                      nenv.(lookup-var rp).loc)
                                  ,*(collect-each ((op opt-pars))
                                      nenv.(lookup-var (car op)).loc)
                                  ,*(if rest-par
                                      (list nenv.(lookup-var rest-par).loc)))
                           ,*opt-code
                           ,*(if specials
                               ^((dframe ,benv.lev 0)))
                           ,*(if specials
                               (collect-each ((vs specials))
                                 (tree-bind (special . gensym) vs
                                   (let ((sub-bind nenv.(lookup-var gensym))
                                         (dreg me.(get-dreg special)))
                                     ^(bindv ,sub-bind.loc ,dreg)))))
                           ,*bfrag.code
                           ,*(if specials
                               ^((end ,bfrag.oreg)))
                           (end ,bfrag.oreg)
                           ,lskip)
                         (uni [reduce-left uni ifrags nil .fvars]
                              (diff bfrag.fvars lexsyms))
                         (uni [reduce-left uni ifrags nil .ffuns]
                              bfrag.ffuns))))))))))

(defmeth compiler comp-progn (me env args)
  (let* ((oreg me.(alloc-treg))
         ffuns fvars
         (code (build
                 (each ((form args))
                   me.(free-treg oreg)
                   (let ((frag me.(compile env form)))
                     (set oreg frag.oreg)
                     (set fvars (uni fvars frag.fvars))
                     (set ffuns (uni ffuns frag.ffuns))
                     (pend frag.code))))))
    (new (frag oreg code fvars ffuns))))

(defmeth compiler comp-prog1 (me env form)
  (tree-case form
    ((prog1 fi . re) (let ((fi-frag me.(compile env fi))
                           (re-frag me.(comp-progn env re)))
                       me.(free-treg re-frag.oreg)
                       (new (frag fi-frag.oreg
                                  (append fi-frag.code re-frag.code)
                                  (uni fi-frag.fvars re-frag.fvars)
                                  (uni fi-frag.ffuns re-frag.ffuns)))))
    ((prog1 fi) me.(compile env fi))
    ((prog1) me.(compile env nil))))

(defmeth compiler comp-call (me env sym args)
  (let ((oreg me.(alloc-treg))
        (dreg me.(get-dreg sym))
        (afrags (mapcar (meth me compile env) args)))
    (let ((aregs (mapcar .oreg afrags)))
      me.(free-tregs aregs)
      (new (frag oreg
                 ^(,*(mappend .code afrags) (call ,oreg ,dreg ,*aregs))
                 [reduce-left uni afrags nil .fvars]
                 [reduce-left uni afrags nil .ffuns])))))

(defmeth compiler comp-for (me env args)
  (tree-bind (inits (: test . rets) incs . body) args
    (let* ((ifrag me.(comp-progn env inits))
           (tfrag (progn
                    me.(free-treg ifrag.oreg)
                    me.(compile env test)))
           (rfrag me.(comp-progn env rets))
           (nfrag me.(comp-progn env incs))
           (bfrag (progn
                    me.(free-treg nfrag.oreg)
                    me.(comp-progn env body)))
           (lback (gensym "l"))
           (lskip (gensym "l"))
           (frags (list ifrag tfrag rfrag nfrag bfrag))
           (infin (equal tfrag.oreg '(t 0)))
           (oreg (if rets rfrag.oreg '(t 0))))
      me.(free-tregs (list bfrag.oreg tfrag.oreg rfrag.oreg))
      (new (frag oreg
                 ^(,*ifrag.code
                   ,lback
                   ,*tfrag.code
                   ,*(if test
                      ^((if ,tfrag.oreg ,lskip)))
                   ,*bfrag.code
                   ,*nfrag.code
                   (jmp ,lback)
                   ,*(if test
                       ^(,lskip))
                   ,*rfrag.code)
                   [reduce-left uni frags nil .fvars]
                   [reduce-left uni frags nil .ffuns])))))

(defun usr:compile-toplevel (exp)
  (let ((co (new compiler))
        (as (new assembler)))
    (let ((frag co.(compile (new env) (expand exp))))
      as.(asm ^(,*frag.code (end ,frag.oreg)))
      (vm-make-desc co.nlev co.nreg as.buf co.(get-datavec)))))
