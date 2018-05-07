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

(load "vm-param")

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

(defstruct blockinfo nil
  sym
  used
  sys:env)

(defstruct sys:env nil
  vb
  fb
  bb
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

  (:method lookup-block (me sym)
    (condlet
      (((cell (assoc sym me.bb))) (cdr cell))
      (((up me.up)) up.(lookup-block sym))
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
        (< me.lev lev))))

  (:method extend-block (me sym)
    (let* ((bn (new blockinfo sym sym env me)))
      (set me.bb (acons sym bn me.bb)))))

(compile-only
  (defstruct compiler nil
    (treg-cntr 2)
    (dreg-cntr 0)
    (fidx-cntr 0)
    (nlev 2)
    (tregs nil)
    (dreg (hash :eql-based))
    (data (hash :eql-based))
    (fidx (hash :eql-based))
    (ftab (hash :eql-based))
    lt-frags
    last-form))

(defmacro compile-in-toplevel (comp . body)
  (with-gensyms (comp-var saved-tregs saved-treg-cntr saved-nlev)
    ^(let* ((,comp-var ,comp)
            (,saved-tregs (qref ,comp-var tregs))
            (,saved-treg-cntr (qref ,comp-var treg-cntr))
            (,saved-nlev (qref ,comp-var nlev)))
       (unwind-protect
         (progn
           (set (qref ,comp-var tregs) nil
                (qref ,comp-var treg-cntr) 2
                (qref ,comp-var nlev) 2)
           (prog1
             (progn ,*body)
             (qref ,comp-var (check-treg-leak))))
         (set (qref ,comp-var tregs) ,saved-tregs
              (qref ,comp-var treg-cntr) ,saved-treg-cntr
              (qref ,comp-var nlev) ,saved-nlev)))))

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

(defvarl %gcall-op% (relate '(apply usr:apply call) '(gapply gapply gcall)))

(defvarl %call-op% (relate '(apply usr:apply call) '(apply apply call)))

(defvarl %test-funs-pos% '(eq))

(defvarl %test-funs-neg% '(neq))

(defvarl %test-funs% (append %test-funs-pos% %test-funs-neg%))

(defvarl %test-inv% (relate %test-funs-pos% %test-funs-neg%))

(defvarl %block-using-funs% '(sys:capture-cont return* sys:abscond* match-fun
                              eval load compile compile-file compile-toplevel))

(defmeth compiler get-dreg (me atom)
  (condlet
    ((((null atom))) '(t 0))
    (((dreg [me.dreg atom])) dreg)
    ((((< me.dreg-cntr %lev-size%)))
     (let ((dreg ^(d ,(pinc me.dreg-cntr))))
      (set [me.data (cadr dreg)] atom)
      (set [me.dreg atom] dreg)))
    (t (compile-error me.last-form "code too complex: too many literals"))))

(defmeth compiler alloc-dreg (me)
  (if (< me.dreg-cntr %lev-size%)
    (let ((dreg ^(d ,(pinc me.dreg-cntr))))
      (set [me.data (cadr dreg)] nil)
      dreg)
    (compile-error me.last-form "code too complex: too many literals")))

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
  (cond
    (me.tregs (pop me.tregs))
    ((< me.treg-cntr %lev-size%) ^(t ,(pinc me.treg-cntr)))
    (t (compile-error me.last-form "code too complex: out of registers"))))

(defmeth compiler free-treg (me treg)
  (when (and (eq t (car treg)) (neq 0 (cadr treg)))
    (push treg me.tregs)))

(defmeth compiler free-tregs (me tregs)
  (mapdo (meth me free-treg) tregs))

(defmeth compiler maybe-alloc-treg (me given)
  (if (eq t (car given))
    given
    me.(alloc-treg)))

(defmeth compiler maybe-free-treg (me treg given)
  (when (nequal treg given)
    me.(free-treg treg)))

(defmeth compiler check-treg-leak (me)
  (let ((balance (- (ppred me.treg-cntr) (len me.tregs))))
    (unless (zerop balance)
      (error "t-register leak in compiler: ~s outstanding" balance))))

(defmeth compiler new-env (me env)
  (when (>= env.lev me.nlev)
    (unless (<= env.lev %max-lev%)
      (compile-error me.last-form
                     "code too complex: lexical nesting too deep"))
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
            (sys:lisp1-setq me.(comp-lisp1-setq oreg env form))
            (sys:setqf me.(comp-setqf oreg env form))
            (cond me.(comp-cond oreg env form))
            (if me.(comp-if oreg env form))
            (switch me.(comp-switch oreg env form))
            (unwind-protect me.(comp-unwind-protect oreg env form))
            ((block block*) me.(comp-block oreg env form))
            ((return-from sys:abscond-from) me.(comp-return-from oreg env form))
            (return me.(comp-return oreg env form))
            (handler-bind me.(comp-handler-bind oreg env form))
            (sys:catch me.(comp-catch oreg env form))
            ((let let*) me.(comp-let oreg env form))
            ((sys:fbind sys:lbind) me.(comp-fbind oreg env form))
            (lambda me.(comp-lambda oreg env form))
            (fun me.(comp-fun oreg env form))
            (sys:for-op me.(comp-for oreg env form))
            (sys:each-op me.(compile oreg env (expand-each form env)))
            ((progn eval-only compile-only) me.(comp-progn oreg env (cdr form)))
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
            (prof me.(comp-prof oreg env form))
            (defvarl me.(compile oreg env (expand-defvarl form)))
            (defun me.(compile oreg env (expand-defun form)))
            (defmacro me.(compile oreg env (expand-defmacro form)))
            (defsymacro me.(compile oreg env (expand-defsymacro form)))
            (sys:upenv me.(compile oreg env.up (cadr form)))
            (sys:dvbind me.(compile oreg env (caddr form)))
            (sys:load-time-lit me.(comp-load-time-lit oreg env form))
            ((macrolet symacrolet macro-time)
             (compile-error form "unexpanded ~s encountered" sym))
            ((sys:var sys:expr)
             (compile-error form "meta with no meaning: ~s " form))
            ((usr:qquote usr:unquote usr:splice
              sys:qquote sys:unquote sys:splice)
             (compile-error form "unexpanded quasiquote encountered"))
            (t
              (compile-error form "unrecognized special operator ~s" sym))))
         ((bindable sym) me.(comp-fun-form oreg env form))
         ((and (consp sym)
               (eq (car sym) 'lambda)) me.(compile oreg env ^(call ,*form)))
         (t (compile-error form "invalid operator")))))))

(defmeth compiler comp-atom (me oreg form)
  (cond
    ((null form) (new (frag '(t 0) nil)))
    ((or (and (fixnump form)
              (<= (width form) (- %imm-width% 3)))
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
      (new (frag vfrag.oreg
                 ^(,*vfrag.code
                   ,*(if bind
                       (maybe-mov vloc vfrag.oreg)
                       ^((setv ,vfrag.oreg ,vloc))))
                 (uni (list sym) vfrag.fvars)
                 vfrag.ffuns)))))

(defmeth compiler comp-lisp1-setq (me oreg env form)
  (mac-param-bind form (op sym val) form
    (let ((bind env.(lookup-lisp1 sym)))
      (cond
        ((typep bind 'fbinding)
         (compile-error form "assignment to lexical function binding"))
        ((null bind)
         (let ((vfrag me.(compile oreg env val))
               (l1loc me.(get-dreg sym)))
           (new (frag vfrag.oreg
                      ^(,*vfrag.code
                        (setl1 ,vfrag.oreg ,l1loc))
                      (uni (list sym) vfrag.fvars)
                      vfrag.ffuns))))
        (t me.(compile oreg env ^(sys:setq ,sym ,val)))))))

(defmeth compiler comp-setqf (me oreg env form)
  (mac-param-bind form (op sym val) form
    (if env.(lookup-fun sym)
      (compile-error form "assignment to lexical function binding")
      (let ((vfrag me.(compile oreg env val))
            (fname me.(get-dreg sym))
            (rplcd me.(get-fidx 'usr:rplacd))
            (treg me.(alloc-treg)))
        me.(free-treg treg)
        (new (frag vfrag.oreg
                   ^(,*vfrag.code
                      (getfb ,treg ,fname)
                      (gcall ,treg ,rplcd ,treg ,vfrag.oreg))
                   (uni (list sym) vfrag.fvars)
                   vfrag.ffuns))))))

(defmeth compiler comp-cond (me oreg env form)
  (tree-case form
    ((op) me.(comp-atom oreg nil))
    ((op (test) . more) me.(compile oreg env ^(or ,test (cond ,*more))))
    ((op (test . forms) . more) me.(compile oreg env
                                            ^(if ,test
                                               (progn ,*forms)
                                               (cond ,*more))))
    ((op atom . more)
     (compile-error form "atom in cond syntax; pair expected"))
    ((op . atom)
     (compile-error form "trailing atom in cond syntax"))))

(defmeth compiler comp-if (me oreg env form)
  (tree-case form
    ((op test then else)
     (cond
       ((null test)
        me.(compile oreg env else))
       ((constantp test)
        me.(compile oreg env then))
       ((and (consp test) (member (car test) %test-funs%))
        me.(compile oreg env ^(ift ,(car test) ,(cadr test) ,(caddr test)
                                   ,then ,else)))
       (t
         (let* ((te-oreg me.(maybe-alloc-treg oreg))
                (lelse (gensym "l"))
                (lskip (gensym "l"))
                (te-frag me.(compile te-oreg env test))
                (th-frag me.(compile oreg env then))
                (el-frag me.(compile oreg env else)))
           me.(maybe-free-treg te-oreg oreg)
           (new (frag oreg
                      ^(,*te-frag.code
                         (if ,te-frag.oreg ,lelse)
                         ,*th-frag.code
                         ,*(maybe-mov oreg th-frag.oreg)
                         (jmp ,lskip)
                         ,lelse
                         ,*el-frag.code
                         ,*(maybe-mov oreg el-frag.oreg)
                         ,lskip)
                      (uni te-frag.fvars (uni th-frag.fvars el-frag.fvars))
                      (uni te-frag.ffuns (uni th-frag.ffuns el-frag.ffuns))))))))
    ((op test then)
     (cond
       ((null test) me.(compile oreg env nil))
       ((constantp test)
        me.(compile oreg env then))
       ((and (consp test) (member (car test) %test-funs%))
        me.(compile oreg env ^(ift ,(car test) ,(cadr test) ,(caddr test)
                                   ,then)))
       (t (let* ((lskip (gensym "l"))
                 (te-oreg me.(maybe-alloc-treg oreg))
                 (te-frag me.(compile te-oreg env test))
                 (th-frag me.(compile oreg env then)))
            me.(maybe-free-treg te-oreg oreg)
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
     (cond
       ((constantp test) me.(compile oreg env nil))
       ((and (consp test) (member (car test) %test-funs%))
        me.(compile oreg env ^(ift ,(car test) ,(cadr test) ,(caddr test))))
       (t (let ((te-frag me.(compile oreg env test)))
            (new (frag oreg
                       ^(,*te-frag.code
                          (mov ,oreg nil))
                       te-frag.fvars
                       te-frag.ffuns))))))
    ((op) me.(compile oreg env nil))
    (form (compile-error form "excess argument forms"))))

(defmeth compiler comp-ift (me oreg env form)
  (mac-param-bind form (op fun left right : then else) form
    (when (member fun %test-funs-neg%)
      (set fun [%test-inv% fun])
      (swap then else))
    (if (and (constantp left) (constantp right))
      me.(compile oreg env (if (eq (eval left) (eval right)) then else))
      (let* ((le-oreg me.(alloc-treg))
             (ri-oreg me.(alloc-treg))
             (lelse (gensym "l"))
             (lskip (gensym "l"))
             (le-frag me.(compile le-oreg env left))
             (ri-frag me.(compile ri-oreg env right))
             (th-frag me.(compile oreg env then))
             (el-frag me.(compile oreg env else)))
        me.(free-treg le-oreg)
        me.(free-treg ri-oreg)
        (new (frag oreg
                   ^(,*le-frag.code
                     ,*ri-frag.code
                     (ifq ,le-frag.oreg ,ri-frag.oreg ,lelse)
                     ,*th-frag.code
                     ,*(maybe-mov oreg th-frag.oreg)
                      (jmp ,lskip)
                     ,lelse
                     ,*el-frag.code
                     ,*(maybe-mov oreg el-frag.oreg)
                     ,lskip)
                   (uni (uni le-frag.fvars ri-frag.fvars)
                        (uni th-frag.fvars el-frag.fvars))
                   (uni (uni le-frag.ffuns ri-frag.ffuns)
                        (uni th-frag.ffuns el-frag.ffuns))))))))

(defmeth compiler comp-switch (me oreg env form)
  (mac-param-bind form (op idx-form cases-vec) form
    (let* ((ncases (len cases-vec))
           (cs (and (plusp ncases) (conses [vec 0])))
           (shared (and cs (all [vec 1..:] (op memq @1 cs))))
           (cases (if shared
                    (let ((cs-nil ^(,*cs nil)))
                      [mapcar ldiff cs-nil (cdr cs-nil)])
                    cases-vec))
           (lend (gensym "l"))
           (clabels (mapcar (ret (gensym "l")) cases))
           (treg me.(maybe-alloc-treg oreg))
           (ifrag me.(compile treg env idx-form))
           (seen (unless shared (hash :eql-based)))
           last-cfrag
           (cfrags (collect-each ((cs cases)
                                  (lb clabels)
                                  (i (range 1)))
                     (iflet ((seen-lb (and seen [seen cs])))
                       (progn
                         (set [clabels (pred i)] seen-lb)
                         (new (frag oreg nil)))
                       (let ((cfrag me.(comp-progn oreg env cs)))
                         (when (eq i ncases)
                           (set last-cfrag cfrag))
                         (unless shared
                           (set [seen cs] lb))
                         (new (frag oreg
                                    ^(,lb
                                       ,*cfrag.code
                                       ,*(unless shared
                                           ^(,*(maybe-mov oreg cfrag.oreg)
                                              ,*(unless (= i ncases)
                                                  ^((jmp ,lend))))))
                                    cfrag.fvars cfrag.ffuns)))))))
      me.(maybe-free-treg treg oreg)
      (new (frag oreg
                 ^(,*ifrag.code
                   (swtch ,ifrag.oreg ,*clabels)
                   ,*(mappend .code cfrags)
                   ,*(when (and shared last-cfrag)
                       (maybe-mov oreg last-cfrag.oreg))
                   ,lend)
                 (uni ifrag.fvars [reduce-left uni cfrags nil .fvars])
                 (uni ifrag.ffuns [reduce-left uni cfrags nil .ffuns]))))))

(defmeth compiler comp-unwind-protect (me oreg env form)
  (mac-param-bind form (op prot-form . cleanup-body) form
    (let* ((treg me.(alloc-treg))
           (pfrag me.(compile oreg env prot-form))
           (cfrag me.(comp-progn treg env cleanup-body))
           (lclean (gensym "l")))
      me.(free-treg treg)
      (cond
        ((null pfrag.code)
         (new (frag cfrag.oreg
                    cfrag.code
                    cfrag.fvars
                    cfrag.ffuns)))
        ((null cfrag.code) pfrag)
        (t (new (frag pfrag.oreg
                      ^((uwprot ,lclean)
                        ,*pfrag.code
                        (end nil)
                        ,lclean
                        ,*cfrag.code
                        (end nil))
                      (uni pfrag.fvars pfrag.fvars)
                      (uni cfrag.fvars cfrag.fvars))))))))

(defmeth compiler comp-block (me oreg env form)
  (mac-param-bind form (op name . body) form
    (let* ((star (and name (eq op 'block*)))
           (nenv (unless star
                   (new env up env lev env.lev co me)))
           (binfo (unless star
                    (cdar nenv.(extend-block name))))
           (treg (if star me.(maybe-alloc-treg oreg)))
           (nfrag (if star me.(compile treg nenv name)))
           (nreg (if star nfrag.oreg me.(get-dreg name)))
           (bfrag me.(comp-progn oreg nenv body))
           (lskip (gensym "l")))
      (when treg
        me.(maybe-free-treg treg oreg))
      (if (and (not star)
               (not binfo.used)
               [all bfrag.ffuns system-symbol-p]
               [none bfrag.ffuns (op member @1 %block-using-funs%)])
        bfrag
        (new (frag oreg
                   ^(,*(if nfrag nfrag.code)
                      (block ,oreg ,nreg ,lskip)
                      ,*bfrag.code
                      ,*(maybe-mov oreg bfrag.oreg)
                      (end ,oreg)
                      ,lskip)
                   bfrag.fvars
                   bfrag.ffuns))))))

(defmeth compiler comp-return-from (me oreg env form)
  (mac-param-bind form (op name : value) form
    (let* ((nreg (if (null name)
                   nil
                   me.(get-dreg name)))
           (opcode (if (eq op 'return-from) 'ret 'abscsr))
           (vfrag me.(compile oreg env value))
           (binfo env.(lookup-block name)))
      (when binfo
        (set binfo.used t))
      (new (frag oreg
                 ^(,*vfrag.code
                   (,opcode ,nreg ,vfrag.oreg))
                 vfrag.fvars
                 vfrag.ffuns)))))

(defmeth compiler comp-return (me oreg env form)
  (mac-param-bind form (op : value) form
    me.(comp-return-from oreg env ^(return-from nil ,value))))

(defmeth compiler comp-handler-bind (me oreg env form)
  (mac-param-bind form (op func-form ex-syms . body) form
    (let* ((freg me.(maybe-alloc-treg oreg))
           (ffrag me.(compile freg env func-form))
           (sreg me.(get-dreg ex-syms))
           (bfrag me.(comp-progn oreg env body)))
      me.(maybe-free-treg freg oreg)
      (new (frag bfrag.oreg
                 ^(,*ffrag.code
                   (handle ,ffrag.oreg ,sreg)
                   ,*bfrag.code
                   (end ,bfrag.oreg))
                 (uni ffrag.fvars bfrag.fvars)
                 (uni ffrag.ffuns bfrag.ffuns))))))

(defmeth compiler comp-catch (me oreg env form)
  (mac-param-bind form (op symbols try-expr . clauses) form
    (with-gensyms (ex-sym-var ex-args-var)
      (let* ((nenv (new env up env co me))
             (esvb (cdar nenv.(extend-var ex-sym-var)))
             (eavb (cdar nenv.(extend-var ex-args-var)))
             (tfrag me.(compile oreg env try-expr))
             (lhand (gensym "l"))
             (lhend (gensym "l"))
             (treg me.(alloc-treg))
             (nclauses (len clauses))
             (cfrags (collect-each ((cl clauses)
                                    (i (range 1)))
                       (mac-param-bind form (sym params . body) cl
                         (let* ((cl-src ^(apply (lambda ,params ,*body)
                                                ,ex-sym-var ,ex-args-var))
                                (cfrag me.(compile oreg nenv (expand cl-src)))
                                (lskip (gensym "l")))
                           (new (frag oreg
                                      ^((gcall ,treg
                                               ,me.(get-fidx 'exception-subtype-p)
                                               ,esvb.loc
                                               ,me.(get-dreg sym))
                                        (if ,treg ,lskip)
                                        ,*cfrag.code
                                        ,*(maybe-mov tfrag.oreg cfrag.oreg)
                                        ,*(unless (eql i nclauses)
                                            ^((jmp ,lhend)))
                                        ,lskip)
                                      cfrag.fvars
                                      cfrag.ffuns)))))))
        me.(free-treg treg)
        (new (frag tfrag.oreg
                   ^((frame ,nenv.lev ,nenv.v-cntr)
                     (catch ,esvb.loc ,eavb.loc ,me.(get-dreg symbols) ,lhand)
                     ,*tfrag.code
                     (jmp ,lhend)
                     ,lhand
                     ,*(mappend .code cfrags)
                     ,lhend
                     (end ,tfrag.oreg)
                     (end ,tfrag.oreg))
                   (uni tfrag.fvars [reduce-left uni cfrags nil .fvars])
                   (uni tfrag.ffuns [reduce-left uni cfrags nil .ffuns])))))))

(defmeth compiler comp-let (me oreg env form)
  (mac-param-bind form (sym raw-vis . body) form
    (let* ((vis (mapcar [iffi atom list] raw-vis))
           (specials [keep-if special-var-p vis car])
           (lexsyms [remove-if special-var-p [mapcar car vis]])
           (specials-occur [find-if special-var-p vis car])
           (treg (if specials-occur me.(alloc-treg)))
           (frsize (len lexsyms))
           (seq (eq sym 'let*))
           (nenv (new env up env co me))
           (eenv (unless seq (new env up env co me)))
           (fenv (if seq nenv eenv)))
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
                            (let ((frag me.(compile treg fenv form))
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
        (when treg
          me.(free-treg treg))
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
           (eenv (unless rec (new env up env co me)))
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
                                                  (if rec nenv eenv)
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
  (mac-param-bind form (op par-syntax . body) form
    (let* ((pars (new (fun-param-parser par-syntax form)))
           (need-frame (or (plusp pars.nfix) pars.rest))
           (nenv (if need-frame (new env up env co me) env))
           lexsyms specials need-dframe)
      (flet ((spec-sub (sym)
               (cond
                 ((special-var-p sym)
                  (let ((sub (gensym)))
                    (push (cons sym sub) specials)
                    (set need-dframe t)
                    nenv.(extend-var sub)
                    sub))
                 (t
                   (push sym lexsyms)
                   nenv.(extend-var sym)
                   sym))))
        (let* ((req-pars (collect-each ((rp pars.req))
                           (spec-sub rp)))
               (opt-pars (collect-each ((op pars.opt))
                           (tree-bind (var-sym : init-form have-sym) op
                             (list (spec-sub var-sym)
                                   init-form
                                   (if have-sym (spec-sub have-sym))))))
               (rest-par (when pars.rest (spec-sub pars.rest))))
          (upd specials nreverse)
          (let* ((col-reg (if opt-pars me.(get-dreg :)))
                 (tee-reg (if opt-pars me.(get-dreg t)))
                 (ifrags (collect-each ((op opt-pars))
                           (tree-bind (var-sym init-form have-sym) op
                             (let ((vbind nenv.(lookup-var var-sym)))
                               me.(compile vbind.loc nenv init-form)))))
                 (opt-code (append-each ((op opt-pars)
                                         (ifrg ifrags))
                             (tree-bind (var-sym init-form have-sym) op
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
                                    ,lskip
                                    ,*(whenlet ((spec-sub [find var-sym specials : cdr]))
                                        (set specials [remq var-sym specials cdr])
                                        ^((bindv ,vbind.loc ,me.(get-dreg (car spec-sub)))))
                                    ,*(whenlet ((spec-sub [find have-sym specials : cdr]))
                                        (prinl 'have)
                                        (set specials [remq have-sym specials cdr])
                                        ^((bindv ,have-bind.loc ,me.(get-dreg (car spec-sub))))))))))
                 (benv (if specials (new env up nenv co me) nenv))
                 (btreg me.(alloc-treg))
                 (bfrag me.(comp-progn btreg benv body))
                 (boreg (if env.(out-of-scope bfrag.oreg) btreg bfrag.oreg))
                 (lskip (gensym "l-"))
                 (frsize (if need-frame nenv.v-cntr 0)))
            me.(free-treg btreg)
            (new (frag oreg
                       ^((close ,oreg ,frsize ,lskip ,pars.nfix ,pars.nreq
                                ,(if rest-par t nil)
                                ,*(collect-each ((rp req-pars))
                                    nenv.(lookup-var rp).loc)
                                ,*(collect-each ((op opt-pars))
                                    nenv.(lookup-var (car op)).loc)
                                ,*(if rest-par
                                    (list nenv.(lookup-var rest-par).loc)))
                         ,*(if need-dframe
                             ^((dframe ,benv.lev 0)))
                         ,*(if specials
                             (collect-each ((vs specials))
                               (tree-bind (special . gensym) vs
                                 (let ((sub-bind nenv.(lookup-var gensym))
                                       (dreg me.(get-dreg special)))
                                   ^(bindv ,sub-bind.loc ,dreg)))))
                         ,*opt-code
                         ,*bfrag.code
                         ,*(if need-dframe
                             ^((end ,boreg)))
                         ,*(maybe-mov boreg bfrag.oreg)
                         (end ,boreg)
                         ,lskip)
                       (uni [reduce-left uni ifrags nil .fvars]
                            (diff bfrag.fvars lexsyms))
                       (uni [reduce-left uni ifrags nil .ffuns]
                            bfrag.ffuns)))))))))

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
         (oreg-discard me.(alloc-treg))
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
  (tree-case form
    ((op) me.(compile oreg env (if (eq op 'and) t)))
    ((op arg) me.(compile oreg env arg))
    ((op . args)
     (let* (ffuns fvars
            (nargs (len args))
            lastfrag
            (is-and (eq op 'and))
            (lout (gensym "l"))
            (treg me.(maybe-alloc-treg oreg))
            (code (build
                    (each ((form args)
                           (n (range 1)))
                      (let ((islast (eql n nargs)))
                        (let ((frag me.(compile treg env form)))
                          (when islast
                            (set lastfrag frag))
                          (pend frag.code
                                (maybe-mov treg frag.oreg))
                          (unless islast
                            (add (if is-and
                                   ^(if ,treg ,lout)
                                   ^(ifq ,treg ,nil ,lout))))
                          (set fvars (uni fvars frag.fvars))
                          (set ffuns (uni ffuns frag.ffuns))))))))
       me.(maybe-free-treg treg oreg)
       (new (frag oreg
                  (append code ^(,lout
                                 ,*(maybe-mov oreg treg)))
                  fvars ffuns))))))

(defmeth compiler comp-prog1 (me oreg env form)
  (tree-case form
    ((prog1 fi . re) (let* ((igreg me.(alloc-treg))
                            (fireg me.(maybe-alloc-treg oreg))
                            (fi-frag me.(compile fireg env fi))
                            (re-frag me.(comp-progn igreg env
                                                    (append re '(nil)))))
                       me.(maybe-free-treg fireg oreg)
                       me.(free-treg igreg)
                       (new (frag fireg
                                  (append fi-frag.code
                                          (maybe-mov fireg fi-frag.oreg)
                                          re-frag.code)
                                  (uni fi-frag.fvars re-frag.fvars)
                                  (uni fi-frag.ffuns re-frag.ffuns)))))
    ((prog1 fi) me.(compile oreg env fi))
    ((prog1) me.(compile oreg env nil))))

(defmeth compiler comp-quasi (me oreg env form)
  (let ((qexp (expand-quasi form)))
    me.(compile oreg env (expand qexp))))

(defmeth compiler comp-fun-form (me oreg env form)
  (tree-bind (sym . args) form
    (caseql sym
      ((call apply usr:apply)
       (let ((gopcode [%gcall-op% sym])
             (opcode [%call-op% sym]))
         (tree-case (car args)
           ((op arg . more)
            (caseq op
              (fun (cond
                     (more (compile-error form "excess args in fun form"))
                     ((bindable arg)
                      (let ((fbind env.(lookup-fun arg)))
                        me.(comp-call-impl oreg env (if fbind opcode gopcode)
                                           (if fbind fbind.loc me.(get-fidx arg))
                                           (cdr args))))
                     ((and (consp arg) (eq (car arg) 'lambda))
                      me.(comp-fun-form oreg env ^(sym ,arg ,*(cdr args))))
                     (t :)))
              (lambda me.(comp-inline-lambda oreg env opcode
                                             (car args) (cdr args)))
              (t :)))
           (arg me.(comp-call oreg env
                              (if (eq sym 'usr:apply) 'apply sym) args)))))
      (ift me.(comp-ift oreg env form))
      (t (let* ((fbind env.(lookup-fun sym))
                (cfrag me.(comp-call-impl oreg env (if fbind 'call 'gcall)
                                          (if fbind fbind.loc me.(get-fidx sym))
                                          args)))
           (pushnew sym cfrag.ffuns)
           cfrag)))))

(defmeth compiler comp-call (me oreg env opcode args)
  (tree-bind (fform . fargs) args
    (let* ((foreg me.(maybe-alloc-treg oreg))
           (ffrag me.(compile foreg env fform))
           (cfrag me.(comp-call-impl oreg env opcode ffrag.oreg fargs)))
      me.(maybe-free-treg foreg oreg)
      (new (frag cfrag.oreg
                 (append ffrag.code
                         cfrag.code)
                 (uni ffrag.fvars cfrag.fvars)
                 (uni ffrag.ffuns cfrag.ffuns))))))

(defmeth compiler comp-call-impl (me oreg env opcode freg args)
  (let* ((aoregs nil)
         (afrags (collect-each ((arg args))
                   (let* ((aoreg me.(alloc-treg))
                          (afrag me.(compile aoreg env arg)))
                     (if (nequal afrag.oreg aoreg)
                       me.(free-treg aoreg)
                       (push aoreg aoregs))
                     afrag))))
    me.(free-tregs aoregs)
    (new (frag oreg
               ^(,*(mappend .code afrags)
                 (,opcode ,oreg ,freg ,*(mapcar .oreg afrags)))
               [reduce-left uni afrags nil .fvars]
               [reduce-left uni afrags nil .ffuns]))))

(defmeth compiler comp-inline-lambda (me oreg env opcode lambda args)
  (let ((reg-args args) apply-list-arg)
    (when (eql opcode 'apply)
      (unless args
        (compile-error lambda "apply requires arguments"))
      (set reg-args (butlast args)
           apply-list-arg (car (last args))))
    me.(compile oreg env (expand (lambda-apply-transform lambda
                                                         reg-args
                                                         apply-list-arg)))))

(defmeth compiler comp-for (me oreg env form)
  (mac-param-bind form (op inits (: test . rets) incs . body) form
    (let* ((treg me.(alloc-treg))
           (ifrag me.(comp-progn treg env inits))
           (tfrag (if test me.(compile oreg env test)))
           (rfrag me.(comp-progn oreg env rets))
           (nfrag me.(comp-progn treg env incs))
           (bfrag me.(comp-progn treg env body))
           (lback (gensym "l"))
           (lskip (gensym "l"))
           (frags (list ifrag tfrag rfrag nfrag bfrag)))
      me.(free-treg treg)
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
           (err-blk (gensym))
           (lout (gensym "l"))
           (ctx-form ^',form)
           (err-form ^',(rlcp ^(,(car form)) form))
           (treg me.(maybe-alloc-treg oreg))
           (objfrag me.(compile treg env obj))
           (cfrags (collect-each ((c cases)
                                  (i (range 1)))
                     (mac-param-bind form (params . body) c
                       (let* ((src (expand ^(block ,err-blk
                                              (set ,obj-var.sym
                                                   ,obj-immut-var.sym)
                                              ,(expand-bind-mac-params
                                                 ctx-form err-form
                                                 params nil obj-var.sym :
                                                 err-blk
                                                 body))))
                              (lerrtest (gensym "l"))
                              (lnext (gensym "l"))
                              (cfrag me.(compile treg nenv src)))
                         (new (frag treg
                                    ^(,*cfrag.code
                                       ,*(maybe-mov treg cfrag.oreg)
                                       (ifq ,treg ,me.(get-dreg :) ,lout))
                                    cfrag.fvars
                                    cfrag.ffuns))))))
           (allfrags (cons objfrag cfrags)))
      me.(maybe-free-treg treg oreg)
      (new (frag oreg
                 ^(,*objfrag.code
                   (frame ,nenv.lev ,nenv.v-cntr)
                   ,*(maybe-mov obj-immut-var.loc objfrag.oreg)
                   ,*(mappend .code cfrags)
                   (mov ,treg nil)
                   ,lout
                   ,*(maybe-mov oreg treg)
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

(defmeth compiler comp-prof (me oreg env form)
  (mac-param-bind form (op . forms) form
    (let ((bfrag me.(comp-progn oreg env forms)))
      (new (frag bfrag.oreg
                 ^((prof ,bfrag.oreg)
                   ,*bfrag.code
                   (end ,bfrag.oreg))
                 bfrag.fvars bfrag.ffuns)))))

(defun misleading-ref-check (frag env form)
  (each ((v frag.fvars))
    (when env.(lookup-var v)
      (compile-warning form "cannot refer to lexical variable ~s" v)))
  (each ((f frag.ffuns))
    (when env.(lookup-fun f)
      (compile-warning form "cannot refer to lexical function ~s" f))))

(defmeth compiler comp-load-time-lit (me oreg env form)
  (mac-param-bind form (op loaded-p exp) form
    (if loaded-p
      me.(compile oreg env ^(quote ,exp))
      (compile-in-toplevel me
        (let* ((oreg me.(alloc-treg))
               (dreg me.(alloc-dreg))
               (exp me.(compile oreg (new env co me) exp))
               (lt-frag (new (frag dreg
                                   ^(,*exp.code
                                     (mov ,dreg ,exp.oreg))
                                   exp.fvars
                                   exp.ffuns))))
          (misleading-ref-check exp env form)
          me.(free-treg oreg)
          (push lt-frag me.lt-frags)
          (new (frag dreg nil)))))))

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
              (cond
                (plist ^(sys:fmt-simple ,obj ,num ,sep, rng-ix ',plist))
                (rng-ix ^(sys:fmt-simple ,obj ,num ,sep, rng-ix))
                (sep ^(sys:fmt-simple ,obj ,num ,sep))
                (num ^(sys:fmt-simple ,obj ,num))
                (t ^(sys:fmt-simple ,obj ,num)))))))))

(defun expand-quasi-args (form)
  (append-each ((el (cdr form)))
    (cond
      ((consp el)
       (caseq (car el)
         (sys:var (mac-param-bind form (sym exp : mods) el
                    (list (expand-quasi-mods exp mods))))
         (sys:quasi (expand-quasi-mods el))
         (t (list ^(sys:fmt-simple ,el)))))
      ((bindable el)
       (list ^(sys:fmt-simple ,el)))
      (t
        (list el)))))

(defun expand-quasi (form)
  (let ((qa (expand-quasi-args form)))
    ^(sys:fmt-join ,*qa)))

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

(defun expand-bind-mac-params (ctx-form err-form params menv-var
                               obj-var strict err-block body)
  (let (vars gen-stk (plen (if (and strict (neq strict t)) (gensym))))
    (labels ((get-gen ()
               (or (pop gen-stk) (gensym)))
             (put-gen (g)
               (push g gen-stk))
             (expand-rec (par-syntax obj-var)
               (let* ((pars (new (mac-param-parser par-syntax ctx-form)))
                      (curs (get-gen)))
                 (unwind-protect
                   ^(,*(when plen
                         ^((set ,plen (if (consp ,obj-var)
                                        (len ,obj-var) 0))))
                     ,*(cond
                         ((eq strict t)
                          ^((sys:bind-mac-check ,err-form ',params
                                                ,obj-var ,pars.nreq
                                                ,(unless pars.rest pars.nfix))))
                         ((null strict) nil)
                         ((symbolp strict)
                          (if pars.rest
                            ^((unless (<= ,pars.nreq ,plen)
                                (return-from ,err-block ',strict)))
                            ^((unless (<= ,pars.nreq ,plen ,pars.nfix)
                                (return-from ,err-block ',strict))))))
                     ,*(append-each ((k pars.key))
                         (tree-bind (key . sym) k
                           (push sym vars)
                           (caseq key
                             (:whole ^((set ,sym ,obj-var)))
                             (:form ^((set ,sym ,ctx-form)))
                             (:env ^((set ,sym ,menv-var))))))
                     ,*(append-each ((p pars.req))
                         (cond
                           ((listp p)
                            ^((set ,curs (car ,obj-var))
                              (set ,obj-var (cdr ,obj-var))
                              ,*(expand-rec p curs)))
                           (t
                            (push p vars)
                            ^((set ,p (car ,obj-var))
                              (set ,obj-var (cdr ,obj-var))))))
                     ,*(append-each ((o pars.opt))
                         (tree-bind (p : init-form pres-p) o
                           (cond
                             ((listp p)
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
                     ,*(when pars.rest
                         (push pars.rest vars)
                         ^((set ,pars.rest ,obj-var))))
                   (put-gen curs)))))
      (let ((bind-code (expand-rec params obj-var)))
        ^(let (,*(nreverse vars) ,*(if plen ^(,plen)) ,*gen-stk)
           ,*bind-code
           ,*body)))))

(defun expand-defvarl (form)
  (mac-param-bind form (op sym : value) form
    (with-gensyms (cell)
      ^(let ((,cell (sys:rt-defvarl ',sym)))
         (if ,cell
           (usr:rplacd ,cell (cons ',sym ,value)))
         ',sym))))

(defun expand-defun (form)
  (mac-param-bind form (op name args . body) form
    (flet ((mklambda (block-name)
             ^(lambda ,args (block ,block-name ,*body))))
      (cond
        ((bindable name)
         ^(sys:rt-defun ',name ,(mklambda name)))
        ((consp name)
         (caseq (car name)
           (meth
             (mac-param-bind form (meth type slot) name
               ^(sys:define-method ',type ',slot ,(mklambda slot))))
           (macro
             (mac-param-bind form (macro sym) name
               ^(sys:rt-defmacro ',sym ',name ,(mklambda sym))))
           (t (compile-error form "~s isn't a valid compound function name"
                             name))))
        (t (compile-error form "~s isn't a valid function name" name))))))

(defun expand-defmacro (form)
  (mac-param-bind form (op name mac-args . body) form
    (with-gensyms (form menv spine-iter)
      (let ((exp-lam ^(lambda (,form ,menv)
                        (let ((,spine-iter (cdr ,form)))
                          ,(expand (expand-bind-mac-params form form mac-args
                                                           menv spine-iter
                                                           t nil
                                     ^((sys:set-macro-ancestor
                                         (block ,name ,*body)
                                         ,form))))))))
        ^(progn
           (sys:rt-defmacro ',name '(macro ,name) ,exp-lam)
           ',name)))))

(defun expand-defsymacro (form)
  (mac-param-bind form (op name def) form
    ^(sys:rt-defsymacro ',name ',def)))

(defun lambda-apply-transform (lm-expr fix-arg-exprs apply-list-expr)
  (mac-param-bind lm-expr (lambda lm-args . lm-body) lm-expr
    (let* ((pars (new (fun-param-parser lm-args lm-expr)))
           (ign-sym (gensym))
           (al-val (gensym)))
      ^(let* ,(build
                (while (and fix-arg-exprs pars.req)
                  (add ^(,(pop pars.req) ,(pop fix-arg-exprs))))
                (while (and fix-arg-exprs pars.opt)
                  (add ^(,(car (pop pars.opt)) ,(pop fix-arg-exprs))))
                (cond
                  ((and (null fix-arg-exprs)
                        (null pars.req)
                        (null pars.opt))
                   (when (or pars.rest apply-list-expr)
                     (add ^(,(or pars.rest ign-sym) ,apply-list-expr))))
                  (fix-arg-exprs
                    (lambda-too-many-args lm-expr))
                  (apply-list-expr
                    (add ^(,al-val ,apply-list-expr))
                    (when pars.req
                      (add ^(,ign-sym (if (< (len ,al-val) ,(len pars.req))
                                        (lambda-short-apply-list)))))
                    (while pars.req
                      (add ^(,(pop pars.req) (pop ,al-val))))
                    (while pars.opt
                      (add ^(,(caar pars.opt)
                              (if ,al-val
                                (pop ,al-val)
                                ,(cadar pars.opt))))
                      (pop pars.opt))
                    (when pars.rest
                      (add ^(,pars.rest ,al-val))))
                  (pars.req
                    (lambda-too-few-args lm-expr))
                  (pars.opt
                    (while pars.opt
                      (add ^(,(caar pars.opt)
                              ,(cadar pars.opt)))
                      (pop pars.opt))
                    (when pars.rest
                      (add ^(,pars.rest))))))
         ,*lm-body))))

(defun system-symbol-p (sym)
  (member (symbol-package sym)
          (load-time (list user-package system-package))))

(defun usr:compile-toplevel (exp : (expanded-p nil))
  (let ((co (new compiler))
        (as (new assembler)))
    (let* ((oreg co.(alloc-treg))
           (xexp (if expanded-p
                   exp
                   (unwind-protect
                     (expand* exp)
                     (unless *load-recursive*
                       (release-deferred-warnings)))))
           (frag co.(compile oreg (new env co co) xexp)))
      co.(free-treg oreg)
      co.(check-treg-leak)
      as.(asm ^(,*(mappend .code (nreverse co.lt-frags)) ,*frag.code (end ,frag.oreg)))
      (vm-make-desc co.nlev (succ as.max-treg) as.buf co.(get-datavec) co.(get-funvec)))))

(defvarl %file-suff-rx% #/[.][^\\\/.]+/)

(defvar *emit*)

(defvar *eval*)

(defvarl %big-endian% (equal (ffi-put 1 (ffi uint32)) #b'00000001'))

(defvarl %tlo-ver% ^(2 0 ,%big-endian%))

(defun open-compile-streams (in-path out-path)
  (let* ((rsuff (r$ %file-suff-rx% in-path))
         (suff (if rsuff [in-path rsuff]))
         (ip-nosuff (if rsuff [in-path 0..(from rsuff)] in-path))
         in-stream out-stream)
    (cond
      ((equal suff ".txr")
       (error "~s: cannot compile TXR files" 'compile-file))
      ((null suff)
       (set in-stream (or (ignerr (open-file ip-nosuff))
                          (ignerr (open-file `@{ip-nosuff}.tl`)))))
      (t
       (set in-stream (ignerr (open-file in-path)))))

    (unless in-stream
      (error "~s: unable to open input file ~s" 'compile-file in-path))

    (unless out-path
      (set out-path `@{ip-nosuff}.tlo`))

    (set out-stream (ignerr (open-file out-path "w")))

    (unless out-stream
      (close-stream in-stream)
      (error "~s: unable to open output file ~s" 'compile-file in-stream))

    (list in-stream out-stream)))

(defun list-from-vm-desc (vd)
  (list (sys:vm-desc-nlevels vd)
        (sys:vm-desc-nregs vd)
        (sys:vm-desc-bytecode vd)
        (copy (sys:vm-desc-datavec vd))
        (sys:vm-desc-funvec vd)))


(defmacro usr:with-compilation-unit (. body)
  (with-gensyms (rec)
    ^(let* ((,rec sys:*load-recursive*)
            (sys:*load-recursive* t))
       (unwind-protect
         (progn ,*body)
         (unless ,rec
           (release-deferred-warnings))))))

(defun usr:compile-file (in-path : out-path)
  (let ((streams (open-compile-streams in-path out-path))
        (err-ret (gensym))
        (*package* *package*)
        (*emit* t)
        (*eval* t)
        (*load-path* in-path)
        (*rec-source-loc* t))
    (with-compilation-unit
      (with-resources ((in-stream (car streams) (close-stream in-stream))
                       (out-stream (cadr streams) (close-stream out-stream))
                       (out (new list-builder)))
        (labels ((compile-form (form)
                   (unless (atom form)
                     (caseq (car form)
                       (progn [mapdo compile-form (cdr form)])
                       (compile-only (let ((*eval* nil))
                                       [mapdo compile-form (cdr form)]))
                       (eval-only (let ((*emit* nil))
                                    [mapdo compile-form (cdr form)]))
                       (t (when (and (or *eval* *emit*)
                                     (not (constantp form)))
                            (let* ((vm-desc (compile-toplevel form t))
                                   (flat-vd (list-from-vm-desc vm-desc)))
                              (when *eval*
                                (sys:vm-execute-toplevel vm-desc))
                              (when *emit*
                                out.(add flat-vd)))))))))
          (prinl %tlo-ver% out-stream)
          (unwind-protect
            (whilet ((obj (read in-stream *stderr* err-ret))
                     ((neq obj err-ret)))
              (compile-form (sys:expand* obj)))
            (let ((*print-circle* t)
                  (*package* (sys:make-anon-package)))
              (prinl out.(get) out-stream)
              (delete-package *package*)))

          (let ((parser (sys:get-parser in-stream)))
            (when (> (sys:parser-errors parser) 0)
              (error "~s: compilation of ~s failed" 'compile-file
                     (stream-get-prop in-stream :name)))))))))

(defun usr:compile (obj)
  (typecase obj
    (fun (tree-bind (indicator args . body) (func-get-form obj)
           (let* ((form ^(lambda ,args ,*body))
                  (vm-desc (compile-toplevel form)))
             (vm-execute-toplevel vm-desc))))
    (t (condlet
         (((fun (symbol-function obj)))
          (tree-bind (indicator args . body) (func-get-form fun)
            (let* ((form ^(lambda ,args ,*body))
                   (vm-desc (compile-toplevel form))
                   (comp-fun (vm-execute-toplevel vm-desc)))
              (set (symbol-function obj) comp-fun))))
         (t (error "~s: cannot compile ~s" 'compile obj))))))
