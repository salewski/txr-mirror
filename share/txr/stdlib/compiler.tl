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
      (((cell (assoc sym me.fb))) (cdr cell))
      (((up me.up)) up.(lookup-fun sym))
      (t nil)))

  (:method extend-var (me sym)
    (let* ((loc ^(v ,(ppred me.lev) ,(pinc me.v-cntr)))
           (bn (new binding sym sym loc loc env me)))
      (set me.vb (acons-new sym bn me.vb))))

  (:method extend-fun (me sym)
    (let* ((loc ^(v ,me.lev ,(pinc me.v-cntr)))
           (bn (new binding sym sym loc loc env me)))
      (set me.fb (acons-new sym bn me.fb))))

  (:method out-of-scope (me reg)
    (if (eq (car reg) 'v)
      (let ((lev (ssucc (cadr reg))))
        (< me.lev lev)))))

(defstruct compiler nil
  (dreg-cntr 0)
  (fidx-cntr 0)
  (nlev 2)
  (nreg 1)
  (tregs (mapcar (op list t) (range 1 255)))
  (dreg (hash :eql-based))
  (data (hash :eql-based))
  (fidx (hash :eql-based))
  (ftab (hash :eql-based))
  last-form)

(defmeth compiler get-dreg (me atom)
  (iflet ((dreg [me.dreg atom]))
    dreg
    (let* ((dreg ^(d ,(pinc me.dreg-cntr))))
      (set [me.data (cadr dreg)] atom)
      (set [me.dreg atom] dreg))))

(defmeth compiler get-fidx (me atom)
  (iflet ((fidx [me.fidx atom]))
    fidx
    (let* ((fidx (pinc me.fidx-cntr)))
      (set [me.ftab fidx] atom)
      (set [me.fidx atom] fidx))))

(defmeth compiler get-datavec (me)
  (vec-list [mapcar me.data (range* 0 me.dreg-cntr)]))

(defmeth compiler get-funvec (me)
  (vec-list [mapcar me.ftab (range* 0 me.fidx-cntr)]))

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

(defmeth compiler compile (me oreg env form)
  (set me.last-form form)
  (cond
    ((symbolp form)
     (if (bindable form)
       me.(comp-var oreg env form)
       me.(comp-atom oreg form)))
    ((atom form) me.(comp-atom oreg form))
    ((consp form)
     (let ((sym (car form)))
       (cond
         ((special-operator-p sym)
          (caseq sym
            (quote me.(comp-atom oreg (cadr form)))
            (sys:setq me.(comp-setq oreg env form))
            (block me.(comp-block oreg env form))
            ((let let*) me.(comp-let oreg env form))
            (lambda me.(comp-lambda oreg env form))
            (sys:for-op me.(comp-for oreg env form))
            (progn me.(comp-progn oreg env (cadr form)))
            (prog1 me.(comp-prog1 oreg env form))
            (sys:quasi me.(comp-quasi oreg env form))
            (sys:dvbind me.(compile oreg env (caddr form)))
            (sys:with-dyn-rebinds me.(comp-progn oreg env (cddr form)))
            ((macrolet symacrolet macro-time)
             (compile-error form "unexpanded ~s encountered" sym))
            ((sys:var sys:expr)
             (compile-error form "meta with no meaning: ~s " form))
            ((usr:qquote usr:unquote usr:splice
              sys:qquote sys:unquote sys:splice)
             (compile-error form "unexpanded quasiquote encountered"))
            (t
              (compile-error form "special op ~s not handled yet" sym))))
         ((bindable sym) me.(comp-call oreg env sym (cdr form)))
         (t (compile-error form "invalid operator")))))))

(defmeth compiler comp-atom (me oreg form)
  (cond
    ((null form) (new (frag '(t 0) nil)))
    ((or (and (integerp form)
              (< (width form) 32))
         (chrp form))
     (new (frag oreg ^((movi ,oreg ,form)))))
    (t (let ((dreg me.(get-dreg form)))
         (new (frag dreg nil))))))

(defmeth compiler comp-var (me oreg env sym)
  (iflet ((vbin env.(lookup-var sym)))
    (new (frag vbin.loc nil (list sym)))
    (let ((dreg me.(get-dreg sym)))
      (new (frag oreg ^((getv ,oreg ,dreg)) (list sym))))))

(defmeth compiler comp-setq (me oreg env form)
  (mac-param-bind form (op sym value) form
    (let* ((bind env.(lookup-var sym))
           (vloc (if bind
                   bind.loc
                   me.(get-dreg sym)))
           (vfrag me.(compile (if bind vloc oreg) env value)))
      (new (frag vloc
                 ^(,*vfrag.code
                   ,*(if bind
                       (if (nequal vfrag.oreg vloc)
                         ^((mov ,vloc ,vfrag.oreg)))
                       ^((setv ,vloc ,vfrag.oreg))))
                 (uni (list sym) vfrag.fvars)
                 vfrag.ffuns)))))

(defmeth compiler comp-block (me oreg env form)
  (mac-param-bind form (op name . body) form
    (let* ((nreg (if name me.(get-dreg name) '(t 0)))
           (bfrag me.(comp-progn oreg env body))
           (lskip (gensym "l")))
      (new (frag oreg
                 ^((block ,oreg ,nreg ,lskip)
                   ,*bfrag.code
                   (end ,bfrag.oreg)
                   ,lskip)
                 bfrag.fvars
                 bfrag.ffuns)))))

(defmeth compiler comp-let (me oreg env form)
  (mac-param-bind form (sym raw-vis . body) form
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
                            (let ((frag me.(compile oreg fenv form))
                                  (dreg me.(get-dreg sym)))
                              (pend frag.code)
                              (add ^(bindv ,frag.oreg ,dreg))
                              (set ffuns (uni ffuns frag.ffuns)
                                   fvars (uni fvars frag.fvars))))
                           (form
                             (let* ((bind (progn
                                            (if seq nenv.(extend-var sym))
                                            nenv.(lookup-var sym)))
                                    (frag me.(compile bind.loc fenv form)))
                               (pend frag.code)
                               (if (nequal bind.loc frag.oreg)
                                 (add ^(mov ,bind.loc ,frag.oreg)))
                               (set ffuns (uni ffuns frag.ffuns)
                                    fvars (uni fvars frag.fvars)))))))))
             (bfrag me.(comp-progn oreg nenv body))
             (boreg (if env.(out-of-scope bfrag.oreg) oreg bfrag.oreg)))
        (new (frag oreg
                   (append code bfrag.code
                           (if (nequal boreg bfrag.oreg)
                             ^((mov ,boreg ,bfrag.oreg)))
                           ^((end ,boreg)))
                   (uni (diff bfrag.fvars lexsyms) fvars)
                   (uni ffuns bfrag.ffuns)))))))

(defmeth compiler comp-lambda (me oreg env form)
  (mac-param-bind form (op pars . body) form
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
                             (tree-bind (var-sym : init-form have-sym) op
                               (let ((vbind nenv.(lookup-var var-sym)))
                                 me.(compile vbind.loc env init-form)))))
                   (opt-code (append-each ((op opt-pars)
                                           (ifrg ifrags))
                               (tree-bind (var-sym : init-form have-sym) op
                                 (let ((vbind nenv.(lookup-var var-sym))
                                       (have-bind nenv.(lookup-var have-sym))
                                       (lskip (gensym "l")))
                                   ^(,*(if have-sym
                                         ^((mov ,have-bind.loc ,tee-reg)))
                                      (ifq ,ifrg.oreg ,col-reg ,lskip)
                                      ,*(if have-sym
                                          ^((mov ,have-bind.loc nil)))
                                      ,*ifrg.code
                                      ,*(if (nequal vbind.loc ifrg.oreg)
                                          ^((mov ,vbind.loc ,ifrg.oreg)))
                                      ,lskip)))))
                   (benv (if specials (new env up nenv co me) nenv))
                   (btreg me.(alloc-treg))
                   (bfrag me.(comp-progn btreg benv body))
                   (boreg (if env.(out-of-scope bfrag.oreg) btreg bfrag.oreg))
                   (lskip (gensym "l-"))
                   (frsize nenv.v-cntr))
              me.(free-treg btreg)
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
                               ^((end ,boreg)))
                           ,*(if (nequal boreg bfrag.oreg)
                               ^((mov ,boreg ,bfrag.oreg)))
                           (end ,boreg)
                           ,lskip)
                         (uni [reduce-left uni ifrags nil .fvars]
                              (diff bfrag.fvars lexsyms))
                         (uni [reduce-left uni ifrags nil .ffuns]
                              bfrag.ffuns))))))))))

(defmeth compiler comp-progn (me oreg env args)
  (let* (ffuns fvars
         lastfrag
         (code (build
                 (each ((form args))
                   (let ((frag me.(compile oreg env form)))
                     (set lastfrag frag)
                     (set fvars (uni fvars frag.fvars))
                     (set ffuns (uni ffuns frag.ffuns))
                     (pend frag.code))))))
    (new (frag (if lastfrag lastfrag.oreg ^(t 0)) code fvars ffuns))))

(defmeth compiler comp-prog1 (me oreg env form)
  (tree-case form
    ((prog1 fi . re) (let* ((igreg me.(alloc-treg))
                            (fi-frag me.(compile oreg env fi))
                            (re-frag me.(comp-progn igreg env re)))
                       me.(free-treg igreg)
                       (new (frag fi-frag.oreg
                                  (append fi-frag.code re-frag.code)
                                  (uni fi-frag.fvars re-frag.fvars)
                                  (uni fi-frag.ffuns re-frag.ffuns)))))
    ((prog1 fi) me.(compile oreg env fi))
    ((prog1) me.(compile oreg env nil))))

(defmeth compiler comp-quasi (me oreg env form)
  (let ((qexp (expand-quasi form)))
    me.(compile oreg env (expand qexp))))

(defmeth compiler comp-call (me oreg env sym args)
  (condlet
    (((freg env.(lookup-fun sym)))
     me.(comp-call-impl oreg env 'call freg args))
    (((fidx me.(get-fidx sym)))
     me.(comp-call-impl oreg env 'gcall fidx args))))

(defmeth compiler comp-call-impl (me oreg env opcode freg args)
  (let* ((sugg-oregs (mapcar (ret me.(alloc-treg)) args))
         (afrags (mapcar (ret me.(compile @1 env @2))
                         sugg-oregs args))
         (real-oregs (mapcar .oreg afrags)))
    me.(free-tregs sugg-oregs)
    (new (frag oreg
               ^(,*(mappend .code afrags) (,opcode ,oreg ,freg ,*real-oregs))
               [reduce-left uni afrags nil .fvars]
               [reduce-left uni afrags nil .ffuns]))))

(defmeth compiler comp-for (me oreg env form)
  (mac-param-bind form (op inits (: test . rets) incs . body) form
    (let* ((ifrag me.(comp-progn oreg env inits))
           (tfrag (if test me.(compile oreg env test)))
           (rfrag me.(comp-progn oreg env rets))
           (nfrag me.(comp-progn oreg env incs))
           (bfrag me.(comp-progn oreg env body))
           (lback (gensym "l"))
           (lskip (gensym "l"))
           (frags (list ifrag tfrag rfrag nfrag bfrag)))
      (new (frag rfrag.oreg
                 ^(,*ifrag.code
                   ,lback
                   ,*tfrag.code
                   ,*(if test
                      ^((if ,tfrag.oreg ,lskip)))
                   ,*bfrag.code
                   ,*nfrag.code
                   (jmp ,lback)
                   ,*(if test
                       ^(,lskip
                         ,*rfrag.code)))
                   [reduce-left uni frags nil .fvars]
                   [reduce-left uni frags nil .ffuns])))))

(defun expand-quasi-mods (obj mods : form)
  (let (plist num sep rng-ix scalar-ix-p flex gens)
    (flet ((get-sym (exp)
             (let ((gen (gensym)))
               (push (list gen exp) gens)
               gen)))
      (for () (mods) ((pop mods))
        (let ((mel (car mods)))
          (cond
            ((keywordp mel)
             (set plist mods)
             (return))
            ((integerp mel)
             (when num
               (compile-error form "duplicate modifier (width/alignment): ~s"
                              num))
             (set num mel))
            ((stringp mel)
             (when sep
               (compile-error form "duplicate modifier (separator): ~s"
                              num))
             (set sep mel))
            ((atom mel)
             (push (get-sym mel) flex))
            (t
              (caseq (car mel)
                (dwim
                  (when rng-ix
                    (compile-error form "duplicate modifier (range/index): ~s"
                                   mel))
                  (unless (consp (cdr mel))
                    (compile-error form "misisng argument in range/index: ~s"
                                   mel))
                  (unless (null (cddr mel))
                    (compile-error form "excess args in range/index: ~s"
                                   num))
                  (let ((arg (cadr mel)))
                    (cond
                      ((and (consp arg) (eq (car arg) 'range))
                       (set rng-ix (get-sym ^(rcons ,(cadr arg) ,(caddr arg)))))
                      (t
                        (set rng-ix (get-sym arg))
                        (set scalar-ix-p t)))))
                (sys:expr (push (get-sym flex) (cadr mel)))
                (t (push (get-sym mel) flex)))))))
      (let ((mcount (+ (if num 1 0)
                       (if sep 1 0)
                       (if rng-ix 1 0)
                       (len flex))))
        (when (> mcount 3)
          (compile-error form "too many formatting modifiers"))
        ^(alet ,(nreverse gens)
           ,(if flex
              ^(sys:fmt-flex ,obj ',plist
                             ,*(remq nil (list* num sep
                                                (if scalar-ix-p
                                                  ^(rcons ,rng-ix nil)
                                                  rng-ix)
                                                (nreverse flex))))
              ^(sys:fmt-simple ,obj ,num ,sep, rng-ix ',plist)))))))

(defun expand-quasi-args (form)
  (append-each ((el (cdr form)))
    (cond
      ((consp el)
       (caseq (car el)
         (sys:var (mac-param-bind form (sym exp : mods) el
                    (list (expand-quasi-mods exp mods))))
         (sys:quasi (expand-quasi-mods el))
         (t el)))
      ((bindable el)
       (list ^(sys:fmt-simple ,el nil nil nil nil)))
      (t
        (list el)))))

(defun expand-quasi (form)
  (let ((qa (expand-quasi-args form)))
    ^(append ,*qa)))

(defun usr:compile-toplevel (exp)
  (let ((co (new compiler))
        (as (new assembler)))
    (let* ((oreg co.(alloc-treg))
           (frag co.(compile oreg (new env) (expand* exp))))
      as.(asm ^(,*frag.code (end ,frag.oreg)))
      (vm-make-desc co.nlev co.nreg as.buf co.(get-datavec) co.(get-funvec)))))
