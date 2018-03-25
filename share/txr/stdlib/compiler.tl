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

(defstruct vbinding binding)

(defstruct fbinding binding)

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
      (set me.co me.up.co))
    me.co.(new-env me))

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

  (:method lookup-lisp1 (me sym)
    (condlet
      (((cell (or (assoc sym me.vb)
                  (assoc sym me.fb)))) (cdr cell))
      (((up me.up)) up.(lookup-lisp1 sym))
      (t nil)))

  (:method extend-var (me sym)
    (when (assoc sym me.vb)
      (compile-error me.co.last-form "duplicate variable: ~s" sym))
    (let* ((loc ^(v ,(ppred me.lev) ,(pinc me.v-cntr)))
           (bn (new vbinding sym sym loc loc env me)))
      (set me.vb (acons sym bn me.vb))))

  (:method extend-var* (me sym)
    (let* ((loc ^(v ,(ppred me.lev) ,(pinc me.v-cntr)))
           (bn (new vbinding sym sym loc loc env me)))
      (set me.vb (acons sym bn me.vb))))

  (:method extend-fun (me sym)
    (when (assoc sym me.fb)
      (compile-error me.co.last-form "duplicate function ~s" sym))
    (let* ((loc ^(v ,(ppred me.lev) ,(pinc me.v-cntr)))
           (bn (new fbinding sym sym loc loc env me)))
      (set me.fb (acons sym bn me.fb))))

  (:method rename-var (me from-sym to-sym)
    (iflet ((cell (assoc from-sym me.vb)))
      (rplaca cell to-sym)
      (let ((bn (cdr cell)))
        (set bn.sym to-sym))))

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
            (cond me.(comp-cond oreg env form))
            (if me.(comp-if oreg env form))
            (unwind-protect me.(comp-unwind-protect oreg env form))
            (block me.(comp-block oreg env form))
            (return-from me.(comp-return-from oreg env form))
            (return me.(comp-return oreg env form))
            ((let let*) me.(comp-let oreg env form))
            ((sys:fbind sys:lbind) me.(comp-fbind oreg env form))
            (lambda me.(comp-lambda oreg env form))
            (fun me.(comp-fun oreg env form))
            (sys:for-op me.(comp-for oreg env form))
            (sys:each-op me.(compile oreg env (expand-each form env)))
            (progn me.(comp-progn oreg env (cdr form)))
            (and me.(comp-and-or oreg env form))
            (or me.(comp-and-or oreg env form))
            (prog1 me.(comp-prog1 oreg env form))
            (sys:quasi me.(comp-quasi oreg env form))
            (dohash me.(compile oreg env (expand-dohash form)))
            (tree-bind me.(comp-tree-bind oreg env form))
            (mac-param-bind me.(comp-mac-param-bind oreg env form))
            (tree-case me.(comp-tree-case oreg env form))
            (sys:lisp1-value me.(comp-lisp1-value oreg env form))
            (dwim me.(comp-dwim oreg env form))
            (sys:upenv me.(compile oreg env.up (cadr form)))
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
         ((bindable sym) me.(comp-fun-form oreg env sym (cdr form)))
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
                       (maybe-mov vloc vfrag.oreg)
                       ^((setv ,vfrag.oreg ,vloc))))
                 (uni (list sym) vfrag.fvars)
                 vfrag.ffuns)))))

(defmeth compiler comp-cond (me oreg env form)
  (let* ((lout (gensym "l"))
         (raw-cases (rest form))
         (first-const [member-if constantp raw-cases car])
         (cases (ldiff raw-cases (cdr first-const)))
         (ncases (len cases))
         (frags (collect-each ((cl cases)
                               (i (range 1)))
                  (mac-param-bind form (test . forms) cl
                    (cond
                      ((and (eq test t) (null forms))
                       (let ((dreg me.(get-dreg t)))
                         (new (frag oreg
                                    ^(,*(maybe-mov oreg dreg)
                                      ,*(if (neql i ncases)
                                          ^((jmp ,lout))))))))
                      ((eq test t)
                       (let ((ffrag me.(comp-progn oreg env forms)))
                         (new (frag oreg
                                    ^(,*ffrag.code
                                      ,*(maybe-mov oreg ffrag.oreg)
                                      ,*(if (neql i ncases)
                                          ^((jmp ,lout))))
                                    ffrag.fvars
                                    ffrag.ffuns))))
                      ((null test)
                       me.(compile oreg env nil))
                      ((constantp test)
                       (let ((ffrag me.(comp-progn oreg env cl)))
                         (new (frag oreg
                                    ^(,*ffrag.code
                                      ,*(maybe-mov oreg ffrag.oreg)
                                      ,*(if (neql i ncases)
                                          ^((jmp ,lout))))
                                    ffrag.fvars
                                    ffrag.ffuns))))
                      ((null forms)
                       (let ((tfrag me.(compile oreg env test))
                             (lskip (gensym "l")))
                         (new (frag oreg
                                    ^(,*tfrag.code
                                       ,*(if (neq (car tfrag.oreg) 'd)
                                           ^((if ,tfrag.oreg ,lskip)))
                                      ,*(maybe-mov oreg tfrag.oreg)
                                      ,*(if (neql i ncases)
                                          ^((jmp ,lout)))
                                      ,lskip)
                                    tfrag.fvars
                                    tfrag.ffuns))))
                      (t (let ((tfrag me.(compile oreg env test))
                               (ffrag me.(comp-progn oreg env forms))
                               (lskip (gensym "l")))
                           (new (frag oreg
                                      ^(,*tfrag.code
                                         ,*(if (neq (car tfrag.oreg) 'd)
                                             ^((if ,tfrag.oreg ,lskip)))
                                         ,*ffrag.code
                                         ,*(maybe-mov oreg ffrag.oreg)
                                         ,*(if (neql i ncases)
                                             ^((jmp ,lout)))
                                         ,lskip)
                                      (uni tfrag.fvars ffrag.fvars)
                                      (uni tfrag.ffuns ffrag.ffuns))))))))))
    (new (frag oreg
               ^(,*(mappend .code frags)
                 ,lout)
               [reduce-left uni frags nil .fvars]
               [reduce-left uni frags nil .ffuns]))))

(defmeth compiler comp-if (me oreg env form)
  (tree-case form
    ((op test then else)
     (cond
       ((null test)
        me.(compile oreg env else))
       ((constantp test)
        me.(compile oreg env then))
       (t
         (let* ((te-oreg me.(alloc-treg))
                (lelse (gensym "l"))
                (lskip (gensym "l"))
                (te-frag me.(compile te-oreg env test))
                (th-frag me.(compile oreg env then))
                (el-frag me.(compile oreg env else)))
           me.(free-treg te-oreg)
           (new (frag oreg
                      ^(,*te-frag.code
                         (if ,te-frag.oreg ,lelse)
                         ,*th-frag.code
                         ,*(maybe-mov oreg th-frag.oreg)
                         (jmp ,lskip)
                         ,lelse
                         ,*el-frag.code
                         ,*(maybe-mov oreg el-frag.oreg)
                         ,lskip
                         ,*(maybe-mov te-oreg te-frag.oreg))
                      (uni te-frag.fvars (uni th-frag.fvars el-frag.fvars))
                      (uni te-frag.ffuns (uni th-frag.ffuns el-frag.ffuns))))))))
    ((op test then)
     (cond
       ((null test) me.(compile oreg env nil))
       ((constantp test)
        me.(compile oreg env then))
       (t (let ((lskip (gensym "l"))
                (te-frag me.(compile oreg env test))
                (th-frag me.(compile oreg env then)))
            (new (frag oreg
                       ^(,*te-frag.code
                          ,*(maybe-mov oreg te-frag.oreg)
                          (if ,oreg ,lskip)
                          ,*th-frag.code
                          ,*(maybe-mov oreg th-frag.oreg)
                          ,lskip)
                       (uni te-frag.fvars th-frag.fvars)
                       (uni te-frag.ffuns th-frag.ffuns)))))))
    ((op test)
     (let ((te-frag me.(compile oreg env test)))
       (new (frag oreg
                  ^(,*te-frag.code
                    (mov ,oreg nil))
                  te-frag.fvars
                  te-frag.ffuns))))
    ((op) me.(compile oreg env nil))
    (form (compile-error form "excess argument forms"))))

(defmeth compiler comp-unwind-protect (me oreg env form)
  (mac-param-bind form (op prot-form . cleanup-body) form
    (let* ((pfrag me.(compile oreg env prot-form))
           (cfrag me.(comp-progn oreg env cleanup-body))
           (lclean (gensym "l"))
           (lskip (gensym "l")))
      (cond
        ((null pfrag.code)
         (new (frag pfrag.oreg
                    cfrag.code
                    cfrag.fvars
                    cfrag.ffuns)))
        ((null cfrag.code) pfrag)
        (t (new (frag pfrag.oreg
                      ^((uwprot ,lclean)
                        ,*pfrag.code
                        (jmp ,lskip)
                        ,lclean
                        ,*cfrag.code
                        (end nil)
                        ,lskip)
                      (uni pfrag.fvars pfrag.fvars)
                      (uni cfrag.fvars cfrag.fvars))))))))

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

(defmeth compiler comp-return-from (me oreg env form)
  (mac-param-bind form (op name value) form
    (let* ((nreg (if (null name)
                   nil
                   me.(get-dreg name)))
           (vfrag me.(compile oreg env value)))
      (new (frag oreg
                 ^(,*vfrag.code
                   (ret ,nreg ,vfrag.oreg))
                 vfrag.fvars
                 vfrag.ffuns)))))

(defmeth compiler comp-return (me oreg env form)
  (mac-param-bind form (op value) form
    me.(comp-return-from oreg env ^(,op nil ,value))))

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
                             (let* ((tmp (if seq (gensym)))
                                    (bind (if seq
                                            (cdar nenv.(extend-var tmp))
                                            nenv.(lookup-var sym)))
                                    (frag me.(compile bind.loc fenv form)))
                               (when seq
                                 fenv.(rename-var tmp sym))
                               (pend frag.code
                                     (maybe-mov bind.loc frag.oreg))
                               (set ffuns (uni ffuns frag.ffuns)
                                    fvars (uni fvars frag.fvars))))
                           (t (if seq nenv.(extend-var* sym))))))))
             (bfrag me.(comp-progn oreg nenv body))
             (boreg (if env.(out-of-scope bfrag.oreg) oreg bfrag.oreg)))
        (new (frag boreg
                   (append code bfrag.code
                           (maybe-mov boreg bfrag.oreg)
                           ^((end ,boreg)))
                   (uni (diff bfrag.fvars lexsyms) fvars)
                   (uni ffuns bfrag.ffuns)))))))

(defmeth compiler comp-fbind (me oreg env form)
  (mac-param-bind form (sym raw-fis . body) form
    (let* ((fis (mapcar [iffi atom list] raw-fis))
           (lexfuns [mapcar car fis])
           (frsize (len lexfuns))
           (rec (eq sym 'sys:lbind))
           (nenv (new env up env co me)))
      (each ((lfun lexfuns))
        nenv.(extend-fun lfun))
      (let* (ffuns fvars
             (code (build
                     (add ^(frame ,nenv.lev ,frsize))
                     (each ((fi fis))
                       (tree-bind (sym : form) fi
                         (let* ((bind nenv.(lookup-fun sym))
                                (frag me.(compile bind.loc
                                                  (if rec nenv env)
                                                  form)))
                           (pend frag.code
                                 (maybe-mov bind.loc frag.oreg))
                           (set ffuns (uni ffuns frag.ffuns)
                                fvars (uni fvars frag.fvars)))))))
             (bfrag me.(comp-progn oreg nenv body))
             (boreg (if env.(out-of-scope bfrag.oreg) oreg bfrag.oreg)))
        (new (frag boreg
                   (append code bfrag.code
                           (maybe-mov boreg bfrag.oreg)
                           ^((end ,boreg)))
                   (uni fvars bfrag.fvars)
                   (uni (diff bfrag.ffuns lexfuns) bfrag.ffuns)))))))

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
                                      (ifq ,vbind.loc ,col-reg ,lskip)
                                      ,*(if have-sym
                                          ^((mov ,have-bind.loc nil)))
                                      ,*ifrg.code
                                      ,*(maybe-mov vbind.loc ifrg.oreg)
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
                           ,*(maybe-mov boreg bfrag.oreg)
                           (end ,boreg)
                           ,lskip)
                         (uni [reduce-left uni ifrags nil .fvars]
                              (diff bfrag.fvars lexsyms))
                         (uni [reduce-left uni ifrags nil .ffuns]
                              bfrag.ffuns))))))))))

(defmeth compiler comp-fun (me oreg env form)
  (mac-param-bind form (op sym) form
    (iflet ((fbin env.(lookup-fun sym)))
      (new (frag fbin.loc nil nil (list sym)))
      (let ((dreg me.(get-dreg sym)))
        (new (frag oreg ^((getf ,oreg ,dreg)) nil (list sym)))))))

(defmeth compiler comp-progn (me oreg env args)
  (let* (ffuns fvars
         (lead-forms (butlastn 1 args))
         (last-form (nthlast 1 args))
         (eff-lead-forms (remove-if [orf constantp symbolp] lead-forms))
         (forms (append eff-lead-forms last-form))
         (nargs (len forms))
         lastfrag
         (oreg-discard (if (eq (car oreg) t)
                         oreg
                         me.(alloc-treg)))
         (code (build
                 (each ((form forms)
                        (n (range 1)))
                   (let ((islast (eql n nargs)))
                     (let ((frag me.(compile (if islast oreg oreg-discard)
                                             env form)))
                       (when islast
                         (set lastfrag frag))
                       (set fvars (uni fvars frag.fvars))
                       (set ffuns (uni ffuns frag.ffuns))
                       (pend frag.code)))))))
    me.(free-treg oreg-discard)
    (new (frag (if lastfrag lastfrag.oreg ^(t 0)) code fvars ffuns))))

(defmeth compiler comp-and-or (me oreg env form)
  (mac-param-bind form (op . args) form
    (let* (ffuns fvars
           (nargs (len args))
           lastfrag
           (is-and (eq op 'and))
           (lout (gensym "l"))
           (code (build
                   (each ((form args)
                          (n (range 1)))
                     (let ((islast (eql n nargs)))
                       (let ((frag me.(compile oreg env form)))
                         (when islast
                           (set lastfrag frag))
                         (pend frag.code
                               (maybe-mov oreg frag.oreg))
                         (unless islast
                             (add (if is-and
                                    ^(if ,oreg ,lout)
                                    ^(ifq ,oreg ,nil ,lout))))
                         (set fvars (uni fvars frag.fvars))
                         (set ffuns (uni ffuns frag.ffuns))))))))
    (new (frag (if lastfrag oreg (if is-and me.(get-dreg t) ^(t 0)))
               (append code ^(,lout)) fvars ffuns)))))

(defmeth compiler comp-prog1 (me oreg env form)
  (tree-case form
    ((prog1 fi . re) (let* ((igreg me.(alloc-treg))
                            (fi-frag me.(compile oreg env fi))
                            (re-frag me.(comp-progn igreg env
                                                    (append re '(nil)))))
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

(defmeth compiler comp-fun-form (me oreg env sym args)
  (condlet
    (((fbind env.(lookup-fun sym)))
     me.(comp-call-impl oreg env 'call fbind.loc args))
    (((fidx me.(get-fidx sym)))
     (caseq sym
       (call me.(comp-call oreg env args))
       (t me.(comp-call-impl oreg env 'gcall fidx args))))))

(defmeth compiler comp-call (me oreg env args)
  (tree-bind (fform . fargs) args
    (let* ((ffrag me.(compile oreg env fform))
           (cfrag me.(comp-call-impl oreg env 'call ffrag.oreg fargs)))
      (new (frag cfrag.oreg
                 (append ffrag.code
                         cfrag.code)
                 (uni ffrag.fvars cfrag.fvars)
                 (uni ffrag.ffuns cfrag.ffuns))))))

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

(defmeth compiler comp-tree-bind (me oreg env form)
  (tree-bind (op params obj . body) form
    (with-gensyms (obj-var)
      (let ((expn (expand ^(let ((,obj-var ,obj))
                             ,(expand-bind-mac-params ^',form
                                                      ^',(rlcp ^(,(car form))
                                                               form)
                                                      params nil
                                                      obj-var t nil body)))))
        me.(compile oreg env expn)))))

(defmeth compiler comp-mac-param-bind (me oreg env form)
  (mac-param-bind form (op context params obj . body) form
    (with-gensyms (obj-var form-var)
      (let ((expn (expand ^(let* ((,obj-var ,obj)
                                  (,form-var ,context))
                             ,(expand-bind-mac-params form-var
                                                      form-var
                                                      params nil
                                                      obj-var t nil body)))))
        me.(compile oreg env expn)))))

(defmeth compiler comp-tree-case (me oreg env form)
  (mac-param-bind form (op obj . cases) form
    (let* ((ncases (len cases))
           (nenv (new env up env co me))
           (obj-immut-var (cdar nenv.(extend-var (gensym))))
           (obj-var (cdar nenv.(extend-var (gensym))))
           (err-blk (cdar nenv.(extend-var (gensym))))
           (lout (gensym "l"))
           (ctx-form ^',form)
           (err-form ^',(rlcp ^(,(car form)) form))
           (objfrag me.(compile oreg env obj))
           (cfrags (collect-each ((c cases)
                                  (i (range 1)))
                     (mac-param-bind form (params . body) c
                       (let* ((src (expand ^(block ,err-blk.sym
                                              (set ,obj-var.sym
                                                   ,obj-immut-var.sym)
                                              ,(expand-bind-mac-params
                                                 ctx-form err-form
                                                 params nil obj-var.sym :
                                                 err-blk.sym
                                                 body))))
                              (lerrtest (gensym "l"))
                              (lnext (gensym "l"))
                              (cfrag me.(compile oreg nenv src)))
                         (new (frag oreg
                                    ^(,*cfrag.code
                                       ,*(maybe-mov oreg cfrag.oreg)
                                       (ifq ,oreg ,me.(get-dreg :) ,lout)
                                       ,*(if (eql i ncases)
                                           ^((mov ,oreg nil))))
                                    cfrag.fvars
                                    cfrag.ffuns))))))
           (allfrags (cons objfrag cfrags)))
      (new (frag oreg
                 ^(,*objfrag.code
                   (frame ,nenv.lev ,nenv.v-cntr)
                   ,*(maybe-mov obj-immut-var.loc objfrag.oreg)
                   ,*(mappend .code cfrags)
                   ,lout
                   (end ,oreg))
                 [reduce-left uni allfrags nil .fvars]
                 [reduce-left uni allfrags nil .ffuns])))))

(defmeth compiler comp-lisp1-value (me oreg env form)
  (mac-param-bind form (op arg) form
    (cond
      ((bindable arg)
       (condlet
         (((bind env.(lookup-lisp1 arg)))
          (new (frag bind.loc
                     nil
                     (if (typep bind 'vbinding) (list arg))
                     (if (typep bind 'fbinding) (list arg)))))
        (t (new (frag oreg
                      ^((getl1 ,oreg ,me.(get-dreg arg)))
                      (list arg)
                      (list arg))))))
      (t me.(compile oreg env arg)))))

(defmeth compiler comp-dwim (me oreg env form)
  (mac-param-bind form (op obj . args) form
    (let ((l1-exprs (cdr form)))
      me.(compile oreg env
                  ^(call ,*(mapcar (op list 'sys:lisp1-value) l1-exprs))))))

(defun maybe-mov (to-reg from-reg)
  (if (nequal to-reg from-reg)
    ^((mov ,to-reg ,from-reg))))

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

(defun expand-dohash (form)
  (mac-param-bind form (op (key-var val-var hash-form : res-form) . body) form
    (with-gensyms (iter-var cell-var)
      ^(let (,key-var ,val-var (,iter-var (hash-begin ,hash-form)) ,cell-var)
         (sys:for-op ((sys:setq ,cell-var (hash-next ,iter-var)))
                     (,cell-var ,res-form)
                     ((sys:setq ,cell-var (hash-next ,iter-var)))
            (sys:setq ,key-var (car ,cell-var))
            (sys:setq ,val-var (cdr ,cell-var))
            ,*body)))))

(defun expand-each (form env)
  (mac-param-bind form (op each-type vars . body) form
    (unless vars
      (set vars [mapcar car env.vb]))
    (let* ((gens (mapcar (ret (gensym)) vars))
           (out (if (member each-type '(collect-each append-each))
                  (gensym)))
           (accum (if out (gensym))))
      ^(let* (,*(zip gens vars) ,*(if accum ^((,out (cons nil nil)) (,accum ,out))))
         (sys:for-op ()
                     ((and ,*gens) ,*(if accum ^((cdr ,out))))
                     (,*(mapcar (ret ^(sys:setq ,@1 (cdr ,@1))) gens))
           ,*(mapcar (ret ^(sys:setq ,@1 (car ,@2))) vars gens)
           ,*(caseq each-type
               (collect-each ^((rplacd ,accum (cons (progn ,*body) nil))
                               (sys:setq ,accum (cdr ,accum))))
               (append-each ^((rplacd ,accum (append (cdr ,accum) (progn ,*body)))
                              (sys:setq ,accum (last ,accum))))
               (t body)))))))

(defun expand-bind-mac-parse-params (params form)
  (let* ((rest-par (nthlast 0 params))
         (proper-pars (ldiff params rest-par))
         nonkey-pars key-pars)
    (while proper-pars
      (let ((pp (pop proper-pars)))
        (caseq pp
          ((:env :whole :form)
           (unless proper-pars
             (compile-error form "~s requires argument" pp))
           (push (cons pp (pop proper-pars)) key-pars))
          (t (push pp nonkey-pars)))))
    (tree-bind (: req-pars raw-opt-pars) (split* (nreverse nonkey-pars)
                                                 (op where (op eq :)))
      (let ((opt-pars (mapcar [iffi atom list] raw-opt-pars)))
        (list (nreverse key-pars) req-pars opt-pars rest-par)))))

(defun expand-bind-mac-params (ctx-form err-form params menv-var
                               obj-var strict err-block body)
  (let (vars gen-stk (plen (gensym)))
    (macrolet ((berr (too-few-p)
                 ^^(sys:bind-mac-error ,err-form ',params
                                       ,obj-var ,',too-few-p)))
      (labels ((get-gen ()
                 (or (pop gen-stk) (gensym)))
               (put-gen (g)
                 (push g gen-stk))
               (expand-rec (params obj-var)
                 (tree-bind (key-pars req-pars opt-pars rest-par)
                            (expand-bind-mac-parse-params params ctx-form)
                   (let* ((nreq (len req-pars))
                          (nfix (+ nreq (len opt-pars)))
                          (curs (get-gen)))
                     (unwind-protect
                       ^(,*(when strict
                             ^((set ,plen (if (consp ,obj-var)
                                            (len ,obj-var) 0))))
                         ,*(cond
                             ((eq strict t)
                              ^((if (< ,plen ,nreq)
                                  ,(berr t))
                                ,*(unless rest-par
                                    ^((if (> ,plen ,nfix)
                                        ,(berr nil))))))
                             ((null strict) nil)
                             ((symbolp strict)
                              ^((if (or (< ,plen ,nreq)
                                        (> ,plen ,nfix))
                                  (return-from ,err-block ',strict)))))
                         ,*(append-each ((k key-pars))
                             (tree-bind (key . sym) k
                               (push sym vars)
                               (caseq key
                                 (:whole ^((set ,sym ,obj-var)))
                                 (:form ^((set ,sym ,ctx-form)))
                                 (:env ^((set ,sym ,menv-var))))))
                         ,*(append-each ((p req-pars))
                             (cond
                               ((consp p)
                                ^((set ,curs (car ,obj-var))
                                  (set ,obj-var (cdr ,obj-var))
                                  ,*(expand-rec p curs)))
                               (t
                                (push p vars)
                                ^((set ,p (car ,obj-var))
                                  (set ,obj-var (cdr ,obj-var))))))
                         ,*(append-each ((o opt-pars))
                             (tree-bind (p : init-form pres-p) o
                               (cond
                                 ((consp p)
                                  (when pres-p
                                    (push pres-p vars))
                                  ^((set ,curs (or (car ,obj-var)
                                                 (sys:upenv ,init-form)))
                                    (cond
                                      ((and ,obj-var
                                            (prog1
                                              (neq (set ,curs (car ,obj-var)) :)
                                              (set ,obj-var (cdr ,obj-var))))
                                        ,*(if pres-p
                                            ^((set ,pres-p t))))
                                      (t
                                        (set ,curs (sys:upenv ,init-form))))
                                    (when ,curs
                                      ,*(expand-rec p curs))))
                                 (t
                                   (push p vars)
                                   (when pres-p
                                     (push pres-p vars))
                                   ^((cond
                                       ((and ,obj-var
                                             (prog1
                                               (neq (set ,p (car ,obj-var)) :)
                                               (set ,obj-var (cdr ,obj-var))))
                                        ,*(if pres-p
                                            ^((set ,pres-p t))))
                                       (t
                                         ,*(if init-form
                                             ^((set ,p (sys:upenv ,init-form)))))))))))
                         ,*(when rest-par
                             (push rest-par vars)
                             ^((set ,rest-par ,obj-var)))
                         ,*(unless rest-par
                             (cond
                               ((eq strict t)
                                ^((if ,obj-var ,(berr nil))))
                               ((null strict) nil)
                               ((symbolp strict)
                                ^((if ,obj-var
                                    (return-from ,err-block ',strict)))))))
                       (put-gen curs))))))
        (let ((bind-code (expand-rec params obj-var)))
          ^(let (,*(nreverse vars) ,plen ,*gen-stk)
             ,*bind-code
             ,*body))))))

(defun sys:bind-mac-error (ctx-form params obj too-few-p)
  (if (atom obj)
    (compile-error ctx-form "extra atom ~s not matched by params ~s"
                   obj  params)
    (compile-error ctx-form "object ~s too ~a for params ~s"
                   obj (if too-few-p "short" "long") params)))

(defun usr:compile-toplevel (exp)
  (let ((co (new compiler))
        (as (new assembler)))
    (let* ((oreg co.(alloc-treg))
           (xexp (prog1 (expand* exp) (unless *load-recursive*
                                        (release-deferred-warnings))))
           (frag co.(compile oreg (new env co co) xexp)))
      as.(asm ^(,*frag.code (end ,frag.oreg)))
      (vm-make-desc co.nlev co.nreg as.buf co.(get-datavec) co.(get-funvec)))))
