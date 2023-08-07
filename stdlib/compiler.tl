;; Copyright 2018-2023
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

(load "vm-param")
(load "optimize")
(load-for (usr:var %const-foldable% "constfun"))

(compile-only
  (load-for (struct sys:param-parser-base "param")))

(defstruct usr:compile-opts ()
  usr:shadow-fun
  usr:shadow-var
  usr:shadow-cross
  usr:unused
  usr:log-level)

(defsymacro %warning-syms% '(usr:shadow-fun usr:shadow-var usr:shadow-cross
                             usr:unused usr:log-level))

(defvar usr:*compile-opts* (new compile-opts usr:unused t))

(defmacro when-opt (compile-opt . forms)
  (with-gensyms (optval)
    ^(whenlet ((,optval *compile-opts*.,compile-opt))
       (macrolet ((diag (. args)
                    ^(opt-controlled-diag ,',optval ,*args)))
         ,*forms))))

(defun opt-controlled-diag (optval . args)
  (caseq optval
    (:error (compile-error . args))
    ((t :warn) (compile-warning . args))))

(defstruct (frag oreg code : fvars ffuns pars) nil
  oreg
  code
  pars
  fvars
  ffuns
  vbin
  alt-oreg)

(defstruct binding nil
  sym
  loc
  used
  sys:env)

(defstruct vbinding binding)

(defstruct fbinding binding
  pars)

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
      (set me.lev (succ (or me.up.?lev 0))))
    (unless (or me.co (null me.up))
      (set me.co me.up.co))
    me.co.(new-env me))

  (:method lookup-var (me sym : mark-used)
    (condlet
      (((cell (assoc sym me.vb)))
       (let ((bi (cdr cell)))
         (if mark-used (set bi.used t))
         bi))
      (((up me.up)) up.(lookup-var sym mark-used))
      (t nil)))

  (:method lookup-fun (me sym : mark-used)
    (condlet
      (((cell (assoc sym me.fb)))
       (let ((bi (cdr cell)))
         (if mark-used (set bi.used t))
         bi))
      (((up me.up)) up.(lookup-fun sym mark-used))
      (t nil)))

  (:method lookup-lisp1 (me sym : mark-used)
    (condlet
      (((cell (or (assoc sym me.vb)
                  (assoc sym me.fb))))
       (let ((bi (cdr cell)))
         (if mark-used (set bi.used t))
         bi))
      (((up me.up)) up.(lookup-lisp1 sym mark-used))
      (t nil)))

  (:method lookup-block (me sym : mark-used)
    (condlet
      (((cell (assoc sym me.bb)))
       (let ((bi (cdr cell)))
         (if mark-used (set bi.used t))
         bi))
      (((up me.up)) up.(lookup-block sym mark-used))
      (t nil)))

  (:method get-loc (me)
    (when (>= me.v-cntr %lev-size%)
      (compile-error me.last-form
                     "code too complex: too many lexicals in one frame"))
    ^(v ,(ppred me.lev) ,(pinc me.v-cntr)))

  (:method extend-var (me sym : (loc me.(get-loc)))
    (when (assoc sym me.vb)
      (compile-error me.co.last-form "duplicate variable: ~s" sym))
    me.(extend-var* sym loc))

  (:method extend-var* (me sym : (loc me.(get-loc)))
    (when-opt shadow-var
      (cond
        ((and me.up me.(lookup-var sym))
         (diag me.co.last-form "variable ~s shadows local variable" sym))
        ((boundp sym)
         (diag me.co.last-form "variable ~s shadows global variable" sym))))
    (when-opt shadow-cross
      (cond
        ((and me.up me.(lookup-fun sym))
         (diag me.co.last-form "variable ~s shadows local function" sym))
        ((fboundp sym)
         (diag me.co.last-form "variable ~s shadows global function" sym))))
    (let ((bn (new vbinding sym sym loc loc env me)))
      (set me.vb (acons sym bn me.vb))
      bn))

  (:method extend-fun (me sym)
    (when (assoc sym me.fb)
      (compile-error me.co.last-form "duplicate function ~s" sym))
    (when-opt shadow-fun
      (cond
        ((and me.up me.(lookup-fun sym))
         (diag me.co.last-form "function ~s shadows local function" sym))
        ((fboundp sym)
         (diag me.co.last-form "function ~s shadows global function" sym))
        ((mboundp sym)
         (diag me.co.last-form "function ~s shadows global macro" sym))))
    (when-opt shadow-cross
      (cond
        ((and me.up me.(lookup-var sym))
         (diag me.co.last-form "function ~s shadows local variable" sym))
        ((boundp sym)
         (diag me.co.last-form "function ~s shadows global variable" sym))))
    (let* ((loc ^(v ,(ppred me.lev) ,(pinc me.v-cntr)))
           (bn (new fbinding sym sym loc loc env me)))
      (set me.fb (acons sym bn me.fb))))

  (:method out-of-scope (me reg)
    (if (eq (car reg) 'v)
      (let ((lev (ssucc (cadr reg))))
        (< me.lev lev))))

  (:method extend-block (me sym)
    (let* ((bn (new blockinfo sym sym env me)))
      (set me.bb (acons sym bn me.bb))))
 
  (:method unused-check (me form nuance)
    (when-opt unused
      (each-match ((@sym . @bn) me.vb)
        (if (and (symbol-package sym) (not bn.used))
          (diag form "~a ~s unused" nuance sym))))))

(defstruct closure-spy ()
  env
  cap-vars

  (:method captured (me vbin sym)
    (when (eq vbin.env me.env)
      (pushnew sym me.cap-vars))))

(defstruct access-spy ()
  closure-spies

  (:method accessed (me vbin sym)
    (each ((spy me.closure-spies))
      (when (neq spy me)
        spy.(captured vbin sym))))

  (:method assigned (me vbin sym)
    (each ((spy me.closure-spies))
      (when (neq spy me)
        spy.(captured vbin sym)))))

(defstruct simplify-var-spy ()
  mutated-vars
  (:method accessed (me vbin sym)
    (ignore me vbin sym))

  (:method assigned (me vbin sym)
    (ignore sym)
    (pushnew vbin me.mutated-vars)))

(compile-only
  (defstruct compiler nil
    (treg-cntr 2)
    (dreg-cntr 0)
    (sidx-cntr 0)
    (nlev 2)
    (loop-nest 0)
    (tregs nil)
    (dreg (hash :eql-based))
    (data (hash :eql-based))
    (sidx (hash :eql-based))
    (stab (hash :eql-based))
    datavec
    symvec
    lt-frags
    last-form
    closure-spies
    access-spies

    (:method snapshot (me)
      (let ((snap (copy me)))
        (set snap.dreg (copy me.dreg)
             snap.data (copy me.data)
             snap.sidx (copy me.sidx)
             snap.stab (copy me.stab))
        snap))

    (:method restore (me snap)
      (replace-struct me snap))))

(defstruct eval-cache-entry ()
  orig-form
  reduced-form
  throws)

(eval-only
  (defmacro compile-in-toplevel (me . body)
    (with-gensyms (saved-tregs saved-treg-cntr)
      ^(let* ((,saved-tregs (qref ,me tregs))
              (,saved-treg-cntr (qref ,me treg-cntr)))
         (unwind-protect
           (progn
             (set (qref ,me tregs) nil
                  (qref ,me treg-cntr) 2)
             (prog1
               (progn ,*body)
               (qref ,me (check-treg-leak))))
           (set (qref ,me tregs) ,saved-tregs
                (qref ,me treg-cntr) ,saved-treg-cntr)))))

  (defmacro compile-with-fresh-tregs (me . body)
    (with-gensyms (saved-tregs saved-treg-cntr)
      ^(let* ((,saved-tregs (qref ,me tregs))
              (,saved-treg-cntr (qref ,me treg-cntr)))
         (unwind-protect
           (progn
             (set (qref ,me tregs) nil
                  (qref ,me treg-cntr) 2)
             (prog1
               (progn ,*body)
               (qref ,me (check-treg-leak))))
           (set (qref ,me tregs) ,saved-tregs
                (qref ,me treg-cntr) ,saved-treg-cntr)))))

  (defun with-spy (me flag spy spy-expr body push-meth pop-meth)
    ^(let ((,spy (if ,flag ,spy-expr)))
       (unwind-protect
         (progn
           (if ,spy (qref ,me (,push-meth ,spy)))
           ,*body)
         (if ,spy (qref ,me (,pop-meth ,spy))))))

  (defmacro with-closure-spy (me flag spy spy-expr . body)
    (with-spy me flag spy spy-expr body 'push-closure-spy 'pop-closure-spy))

  (defmacro with-access-spy (me flag spy spy-expr . body)
    (with-spy me flag spy spy-expr body 'push-access-spy 'pop-access-spy)))

(defvarl %gcall-op% (relate '(apply usr:apply call) '(gapply gapply gcall)))

(defvarl %call-op% (relate '(apply usr:apply call) '(apply apply call)))

(defvarl %test-funs-pos% '(eq eql))

(defvarl %test-funs-neg% '(neq neql))

(defvarl %test-funs-ops% '(ifq ifql))

(defvarl %test-funs% (append %test-funs-pos% %test-funs-neg%))

(defvarl %test-inv% (relate %test-funs-neg% %test-funs-pos%))

(defvarl %test-opcode% (relate %test-funs-pos% %test-funs-ops%))

(defvarl %block-using-funs% '(sys:capture-cont return* sys:abscond* match-fun
                              eval load compile compile-file compile-toplevel))

(defvarl %nary-ops% '(< > <= => = + - * /))

(defvarl %bin-ops% '(b< b> b<= b=> b= b+ b- b* b/))

(defvarl %bin-op% (relate %nary-ops% %bin-ops% nil))

(defvarl assumed-fun)

(defvar *in-compilation-unit* nil)

(defvar *dedup*)

(defvar *unchecked-calls*)

(defvarl %param-info% (hash :eq-based :weak-keys))

(defvarl %eval-cache% (hash :eql-based :weak-keys :weak-vals))

(defvar *load-time*)

(defvar *top-level*)

;; 0 - no optimization
;; 1 - constant folding, algebraics.
;; 2 - block elimination, frame elimination
;; 3 - lambda/combinator lifting
;; 4 - control-flow: jump-threading, dead code
;; 5 - data-flow: dead registers, useless regisers
;; 6 - iterate on 4-5 optimizations.
;; 7 - more expensive size or speed optimizations
(defvar usr:*opt-level* 7)

(defun dedup (obj)
  (cond
    ((null obj) nil)
    ((null *dedup*) obj)
    ((or (stringp obj) (bignump obj))
     (or [*dedup* obj] (set [*dedup* obj] obj)))
    (t obj)))

(defun null-reg (reg)
  (equal reg '(t 0)))

(defun maybe-mov (to-reg from-reg)
  (if (nequal to-reg from-reg)
    ^((mov ,to-reg ,from-reg))))

(defmeth compiler get-dreg (me obj)
  (let ((dobj (dedup obj)))
    (condlet
      ((((null dobj))) '(t 0))
      (((dreg [me.dreg dobj])) dreg)
      (t
        (let ((dreg ^(d ,(pinc me.dreg-cntr))))
          (set me.datavec nil
               [me.data (cadr dreg)] dobj
               [me.dreg dobj] dreg))))))

(defmeth compiler alloc-dreg (me)
  (if (< me.dreg-cntr %lev-size%)
    (let ((dreg ^(d ,(pinc me.dreg-cntr))))
      (set [me.data (cadr dreg)] nil)
      dreg)
    (compile-error me.last-form "code too complex: too many literals")))

(defmeth compiler null-dregs (me used-dreg)
  (each ((n 0..me.dreg-cntr))
    (unless (bit used-dreg n)
      (set [me.data n] nil
           me.datavec nil))))

(defmeth compiler get-sidx (me atom)
  (iflet ((sidx [me.sidx atom]))
    sidx
    (let* ((sidx (pinc me.sidx-cntr)))
      (set [me.stab sidx] atom)
      (set [me.sidx atom] sidx))))

(defmeth compiler get-datavec (me)
  (or me.datavec
      (set me.datavec (vec-list [mapcar me.data (range* 0 me.dreg-cntr)]))))

(defmeth compiler get-symvec (me)
  (or me.symvec
      (set me.symvec (vec-list [mapcar me.stab (range* 0 me.sidx-cntr)]))))

(defmeth compiler alloc-treg (me)
  (cond
    (me.tregs (pop me.tregs))
    ((< me.treg-cntr %lev-size%) ^(t ,(pinc me.treg-cntr)))
    (t (compile-error me.last-form "code too complex: out of registers"))))

(defmeth compiler alloc-new-treg (me)
  (cond
    ((< me.treg-cntr %lev-size%) ^(t ,(pinc me.treg-cntr)))
    (t (compile-error me.last-form "code too complex: out of registers"))))

(defmeth compiler free-treg (me treg)
  (when (and (eq t (car treg)) (neq 0 (cadr treg)))
    (push treg me.tregs)))

(defmeth compiler free-tregs (me tregs)
  (mapdo (meth me free-treg) tregs))

(defmeth compiler unalloc-reg-count (me)
  (- %lev-size% me.treg-cntr))

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

(defmeth compiler push-closure-spy (me spy)
  (push spy me.closure-spies))

(defmeth compiler pop-closure-spy (me spy)
  (let ((top (pop me.closure-spies)))
    (unless top
      (error "closure spy stack bug in compiler"))
    (unless (eq top spy)
      (error "closure spy stack balance problem in compiler"))))

(defmeth compiler push-access-spy (me spy)
  (push spy me.access-spies))

(defmeth compiler pop-access-spy (me spy)
  (let ((top (pop me.access-spies)))
    (unless top
      (error "access spy stack bug in compiler"))
    (unless (eq top spy)
      (error "access spy stack balance problem in compiler"))))

(defmeth compiler compile (me oreg env form)
  (set me.last-form form)
  (cond
    ((symbolp form)
     (if (bindable form)
       me.(comp-var oreg env form)
       me.(comp-atom oreg form)))
    ((atom form) me.(comp-atom oreg form))
    (t (let ((sym (car form)))
         (cond
           ((bindable sym)
            (caseq sym
              (quote me.(comp-atom oreg (cadr form)))
              (sys:setq me.(comp-setq oreg env form))
              (sys:lisp1-setq me.(comp-lisp1-setq oreg env form))
              (sys:setqf me.(comp-setqf oreg env form))
              (cond me.(comp-cond oreg env form))
              (if me.(comp-if oreg env form))
              (switch me.(comp-switch oreg env form))
              (unwind-protect me.(comp-unwind-protect oreg env form))
              ((block block* sys:blk) me.(comp-block oreg env form))
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
              (and me.(compile oreg env (expand-and form)))
              (or me.(comp-or oreg env form))
              (prog1 me.(comp-prog1 oreg env form))
              (progv me.(comp-progv oreg env form))
              (sys:quasi me.(comp-quasi oreg env form))
              (dohash me.(compile oreg env (expand-dohash form)))
              (tree-bind me.(comp-tree-bind oreg env form))
              (mac-param-bind me.(comp-mac-param-bind oreg env form))
              (mac-env-param-bind me.(comp-mac-env-param-bind oreg env form))
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
              ;; compiler-only special operators:
              (ift me.(comp-ift oreg env form))
              ;; specially treated functions
              ((call apply usr:apply) me.(comp-apply-call oreg env form))
              ;; error cases
              ((macrolet symacrolet macro-time)
               (compile-error form "unexpanded ~s encountered" sym))
              ((sys:var sys:expr)
               (compile-error form "meta with no meaning: ~s " form))
              ((usr:qquote usr:unquote usr:splice
                           sys:qquote sys:unquote sys:splice)
               (compile-error form "unexpanded quasiquote encountered"))
              ;; function call
              ((+ *) me.(comp-arith-form oreg env form))
              ((- /) me.(comp-arith-neg-form oreg env form))
              (typep me.(comp-typep oreg env form))
              (compiler-let me.(comp-compiler-let oreg env form))
              (t me.(comp-fun-form oreg env form))))
           ((and (consp sym)
                 (eq (car sym) 'lambda)) me.(compile oreg env ^(call ,*form)))
           (t (compile-error form "invalid operator")))))))

(defmeth compiler comp-atom (me oreg form)
  (ignore oreg)
  (cond
    ((null form) (new (frag '(t 0) nil)))
    (t (let ((dreg me.(get-dreg form)))
         (new (frag dreg nil))))))

(defmeth compiler comp-var (me oreg env sym)
  (let ((vbin env.(lookup-var sym t)))
    (cond
      (vbin
        (each ((spy me.access-spies))
          spy.(accessed vbin sym))
        (new (frag vbin.loc nil (list sym))
             vbin vbin
             alt-oreg oreg))
      ((special-var-p sym)
       (let ((dreg me.(get-dreg sym)))
         (new (frag oreg ^((getv ,oreg ,dreg)) (list sym)))))
      (t (new (frag oreg ^((getlx ,oreg ,me.(get-sidx sym))) (list sym)))))))

(defmeth compiler comp-setq (me oreg env form)
  (mac-param-bind form (t sym value) form
    (let* ((bind env.(lookup-var sym))
           (spec (special-var-p sym))
           (vloc (cond
                   (bind bind.loc)
                   (spec me.(get-dreg sym))
                   (t me.(get-sidx sym))))
           (vfrag me.(compile (if bind vloc oreg) env value)))
      (when bind
        (set vfrag.vbin bind
             vfrag.alt-oreg oreg)
        (each ((spy me.access-spies))
          spy.(assigned bind sym)))
      (new (frag vfrag.oreg
                 ^(,*vfrag.code
                   ,*(if bind
                       (maybe-mov vloc vfrag.oreg)
                       (if spec
                         ^((setv ,vfrag.oreg ,vloc))
                         ^((setlx ,vfrag.oreg ,me.(get-sidx sym))))))
                 (uni (list sym) vfrag.fvars)
                 vfrag.ffuns)))))

(defmeth compiler comp-lisp1-setq (me oreg env form)
  (mac-param-bind form (t sym val) form
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
        (t (each ((spy me.access-spies))
             spy.(assigned bind sym))
          me.(compile oreg env ^(sys:setq ,sym ,val)))))))

(defmeth compiler comp-setqf (me oreg env form)
  (mac-param-bind form (t sym val) form
    (if env.(lookup-fun sym)
      (compile-error form "assignment to lexical function binding")
      (let ((vfrag me.(compile oreg env val))
            (fname me.(get-dreg sym))
            (rplcd me.(get-sidx 'usr:rplacd))
            (treg me.(alloc-treg)))
        me.(free-treg treg)
        (new (frag vfrag.oreg
                   ^(,*vfrag.code
                      (getfb ,treg ,fname)
                      (gcall ,treg ,rplcd ,treg ,vfrag.oreg))
                   vfrag.fvars
                   (uni (list sym) vfrag.ffuns)))))))

(defmeth compiler comp-cond (me oreg env form)
  (tree-case form
    ((t) me.(comp-atom oreg nil))
    ((t (test) . more) me.(compile oreg env ^(or ,test (cond ,*more))))
    ((t (test . forms) . more) me.(compile oreg env
                                           ^(if ,test
                                              (progn ,*forms)
                                              (cond ,*more))))
    ((t t . t)
     (compile-error form "atom in cond syntax; pair expected"))
    ((t . t)
     (compile-error form "trailing atom in cond syntax"))))

(defmeth compiler comp-if (me oreg env form)
  (match-case (cdr form)
    (@(require ((@(and @(or equal nequal) @op) @a @b) . @rest)
               (or (eql-comparable a)
                   (eql-comparable b)))
      (let* ((pos (eq op 'equal))
             (cf (if (or (eq-comparable a)
                         (eq-comparable b))
                   (if pos 'eq 'neq)
                   (if pos'eql 'neql))))
        me.(compile oreg env ^(if (,cf ,a ,b) ,*rest))))
    (((not (@(and @(or eq eql equal) @op) . @eargs)) . @args)
     (let ((nop (caseq op (eq 'neq) (eql 'neql) (equal 'nequal))))
       me.(comp-if oreg env ^(if (,nop ,*eargs) ,*args))))
    ((@(safe-constantp @test) @then @else)
     me.(compile oreg env (if (safe-const-eval test) then else)))
    ((@(safe-constantp @test) @then)
     me.(compile oreg env (if (safe-const-eval test) then)))
    ((@(safe-constantp))
     me.(compile oreg env nil))
    (((@(member @op %test-funs%) @a @b) . @rest)
     me.(compile oreg env ^(ift ,op ,a ,b ,*rest)))
    ((@test @then @else)
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
                  (uni te-frag.ffuns (uni th-frag.ffuns el-frag.ffuns))))))
    ((@test @then)
     (let* ((lskip (gensym "l"))
            (te-oreg me.(maybe-alloc-treg oreg))
            (te-frag me.(compile te-oreg env test))
            (th-frag me.(compile oreg env then)))
       me.(maybe-free-treg te-oreg oreg)
       (new (frag oreg
                  ^(,*te-frag.code
                     ,*(maybe-mov oreg te-frag.oreg)
                     (if ,te-frag.oreg ,lskip)
                     ,*th-frag.code
                     ,*(maybe-mov oreg th-frag.oreg)
                     ,lskip)
                  (uni te-frag.fvars th-frag.fvars)
                  (uni te-frag.ffuns th-frag.ffuns)))))
    ((@test)
     (let ((te-frag me.(compile oreg env test)))
       (new (frag oreg
                  ^(,*te-frag.code
                     (mov ,oreg nil))
                  te-frag.fvars
                  te-frag.ffuns))))
    (() me.(compile oreg env nil))
    (@nil (compile-error form "excess argument forms"))))

(defmeth compiler comp-ift (me oreg env form)
  (mac-param-bind form (t fun left right : then else) form
    (when (member fun %test-funs-neg%)
      (set fun [%test-inv% fun])
      (swap then else))
    (if (and (safe-constantp left) (safe-constantp right))
      me.(compile oreg env (if (call fun
                                     (safe-const-eval left)
                                     (safe-const-eval right))
                             then else))
      (let* ((opcode [%test-opcode% fun])
             (le-oreg me.(alloc-treg))
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
                     (,opcode ,le-frag.oreg ,ri-frag.oreg ,lelse)
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
  (mac-param-bind form (t idx-form cases-vec) form
    (let* ((ncases (len cases-vec))
           (cs (and (plusp ncases) (conses [cases-vec 0])))
           (shared (and cs
                        (let ((c cs)
                              (d (cdr (list-vec cases-vec))))
                          (whilet ((m (if d (memq (pop d) c))))
                            (set c m))
                          (null d))))
           (cases (if shared
                    (let ((cs-nil ^(,*cs nil)))
                      (vec-list [mapcar ldiff cs-nil (cdr cs-nil)]))
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
                   (swtch ,ifrag.oreg ,*(list-vec clabels))
                   ,*(mappend .code cfrags)
                   ,*(when (and shared last-cfrag)
                       (maybe-mov oreg last-cfrag.oreg))
                   ,lend)
                 (uni ifrag.fvars [reduce-left uni cfrags nil .fvars])
                 (uni ifrag.ffuns [reduce-left uni cfrags nil .ffuns]))))))

(defmeth compiler comp-unwind-protect (me oreg env form)
  (mac-param-bind form (t prot-form . cleanup-body) form
    (let* ((treg me.(alloc-treg))
           (pfrag me.(compile oreg env prot-form))
           (cfrag me.(comp-progn treg env cleanup-body))
           (lclean (gensym "l")))
      me.(free-treg treg)
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
           (nfrag (if star me.(compile treg env name)))
           (nreg (if star nfrag.oreg me.(get-dreg name)))
           (bfrag me.(comp-progn oreg (or nenv env) body))
           (lskip (gensym "l")))
      (when treg
        me.(maybe-free-treg treg oreg))
      (if (and (>= *opt-level* 2)
               (not star)
               (not binfo.used)
               (if (eq op 'sys:blk)
                 [all bfrag.ffuns [orf system-symbol-p (op eq name)]]
                 [all bfrag.ffuns system-symbol-p])
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
           (vfrag me.(compile oreg env value)))
      env.(lookup-block name t)
      (new (frag oreg
                 ^(,*vfrag.code
                   (,opcode ,nreg ,vfrag.oreg))
                 vfrag.fvars
                 vfrag.ffuns)))))

(defmeth compiler comp-return (me oreg env form)
  (mac-param-bind form (t : value) form
    me.(comp-return-from oreg env ^(return-from nil ,value))))

(defmeth compiler comp-handler-bind (me oreg env form)
  (mac-param-bind form (t func-form ex-syms . body) form
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
  (mac-param-bind form (t symbols try-expr desc-expr . clauses) form
    (if (and (plusp *opt-level*)
             (or (null symbols)
                 (safe-constantp try-expr)))
      me.(compile oreg env try-expr)
      (with-gensyms (ex-sym-var ex-args-var)
        (let* ((nenv (new env up env co me))
               (esvb nenv.(extend-var ex-sym-var))
               (eavb nenv.(extend-var ex-args-var))
               (tfrag me.(compile oreg nenv try-expr))
               (dfrag me.(compile oreg nenv desc-expr))
               (lhand (gensym "l"))
               (lhend (gensym "l"))
               (treg me.(alloc-treg))
               (nclauses (len clauses))
               (have-one-symbol (and (plusp *opt-level*) (eql 1 (len symbols))))
               (one-symbol (if have-one-symbol (car symbols)))
               (cfrags (collect-each ((cl clauses)
                                      (i (range 1)))
                         (mac-param-bind form (sym params . body) cl
                           (let* ((cl-src (rlcp-tree ^(apply (lambda ,params ,*body)
                                                             ,ex-sym-var ,ex-args-var)
                                                     form))
                                  (cfrag me.(compile oreg nenv (expand cl-src)))
                                  (lskip (gensym "l")))
                             (new (frag oreg
                                        (cond
                                          ((and have-one-symbol
                                                (exception-subtype-p one-symbol sym))
                                           ^(,*cfrag.code
                                              ,*(maybe-mov oreg cfrag.oreg)
                                              ,*(unless (eql i nclauses)
                                                  ^((jmp ,lhend)))))
                                          (have-one-symbol
                                            (set cfrag.fvars nil
                                                 cfrag.ffuns nil)
                                            nil)
                                          (t ^((gcall ,treg
                                                      ,me.(get-sidx 'exception-subtype-p)
                                                      ,esvb.loc
                                                      ,me.(get-dreg sym))
                                               (if ,treg ,lskip)
                                               ,*cfrag.code
                                               ,*(maybe-mov oreg cfrag.oreg)
                                               ,*(unless (eql i nclauses)
                                                   ^((jmp ,lhend)))
                                               ,lskip)))
                                        cfrag.fvars
                                        cfrag.ffuns)))))))
          me.(free-treg treg)
          (new (frag oreg
                     ^((frame ,nenv.lev ,nenv.v-cntr)
                       ,*dfrag.code
                       (catch ,esvb.loc ,eavb.loc
                              ,me.(get-dreg symbols) ,dfrag.oreg ,lhand)
                       ,*tfrag.code
                       ,*(maybe-mov oreg tfrag.oreg)
                       (jmp ,lhend)
                       ,lhand
                       ,*(mappend .code cfrags)
                       ,lhend
                       (end ,oreg)
                       (end ,oreg))
                     (uni tfrag.fvars [reduce-left uni cfrags nil .fvars])
                     (uni tfrag.ffuns [reduce-left uni cfrags nil .ffuns]))))))))

(defmeth compiler eliminate-frame (me code env)
  (if (>= me.(unalloc-reg-count) (len env.vb))
    (let ((trhash (hash))
          (vbhash (hash))
          (vlev (ppred env.lev))
          (tregs nil))
      (each ((cell env.vb))
        (tree-bind (t . vbind) cell
          (let ((treg me.(alloc-new-treg)))
            (set [trhash vbind.loc] treg)
            (set [vbhash vbind.loc] vbind)
            (push treg tregs))))
      (let ((ncode (append-each ((insns (conses code)))
                     (match-case insns
                       (((frame @lev @size) . @nil)
                        ^((frame ,(pred lev) ,size)))
                       (((dframe @lev @size) . @nil)
                        ^((dframe ,(pred lev) ,size)))
                       (((@op . @args) . @nil)
                        (let ((nargs (mapcar (lambda-match
                                               ((@(as arg (v @lev @idx)))
                                                (or [trhash arg]
                                                    (if (> lev vlev)
                                                      ^(v ,(pred lev) ,idx)
                                                      arg)))
                                               ((@arg) arg))
                                             args)))
                          ^((,op ,*nargs))))
                       ((@else . @nil) (list else))))))
        (dohash (loc treg trhash)
          (let ((vb [vbhash loc]))
            (set vb.loc treg)))
        me.(free-tregs tregs)
        (if (plusp me.loop-nest)
          (append (mapcar (ret ^(mov ,@1 (t 0))) (nreverse tregs)) ncode)
          ncode)))
    code))

(defmeth compiler comp-let (me oreg env form)
  (mac-param-bind form (sym raw-vis . body) form
    (let* ((vis (mapcar [iffi atom list] raw-vis))
           (lexsyms [remove-if special-var-p [mapcar car vis]])
           allsyms
           (specials-occur [find-if special-var-p vis car])
           (treg (if specials-occur me.(alloc-treg)))
           (frsize (len lexsyms))
           (seq (eq sym 'let*))
           (nenv (new env up env co me))
           (fenv (if seq nenv (new env up env co me))))
      (with-closure-spy me (and (not specials-occur)
                                (>= *opt-level* 2))
                        cspy (new closure-spy env nenv)
        (unless seq
          (each ((lsym lexsyms))
            nenv.(extend-var lsym)))
        (let* (ffuns fvars
               (code (build
                       (add ^(,(if specials-occur 'dframe 'frame)
                              ,nenv.lev ,frsize))
                       (each ((vi vis))
                         (tree-bind (sym : form) vi
                           (push sym allsyms)
                           (cond
                             ((special-var-p sym)
                              (let ((frag me.(compile treg fenv form))
                                    (dreg me.(get-dreg sym)))
                                (pend frag.code)
                                (add ^(bindv ,frag.oreg ,dreg))
                                (set ffuns (uni ffuns frag.ffuns)
                                     fvars (uni fvars
                                                (if seq
                                                  (diff frag.fvars
                                                        (cdr allsyms))
                                                  frag.fvars)))))
                             (form
                               (let* ((loc (if seq
                                             nenv.(get-loc)
                                             nenv.(lookup-var sym).loc))
                                      (frag me.(compile loc fenv form)))
                                 (when seq
                                   nenv.(extend-var* sym loc))
                                 (pend frag.code)
                                 (unless (null-reg frag.oreg)
                                   (pend (maybe-mov loc frag.oreg)))
                                 (set ffuns (uni ffuns frag.ffuns)
                                      fvars (uni fvars
                                                 (if seq
                                                   (diff frag.fvars
                                                         (cdr allsyms))
                                                   frag.fvars)))))
                             (t (if seq nenv.(extend-var* sym))))))))
               (bfrag me.(comp-progn oreg nenv body))
               (boreg (if env.(out-of-scope bfrag.oreg) oreg bfrag.oreg))
               (code (append code bfrag.code
                             (maybe-mov boreg bfrag.oreg)
                             ^((end ,boreg)))))
          (when (and cspy (null cspy.cap-vars))
            (set code me.(eliminate-frame [code 1..-1] nenv)))
          (when treg
            me.(free-treg treg))
          nenv.(unused-check form "variable")
          (new (frag boreg
                     code
                     (uni (diff bfrag.fvars allsyms) fvars)
                     (uni ffuns bfrag.ffuns))))))))

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
             (ffrags (collect-each ((fi fis))
                       (tree-bind (sym : form) fi
                         (let* ((bind nenv.(lookup-fun sym))
                                (frag me.(compile bind.loc
                                                  (if rec nenv eenv)
                                                  form)))
                           (set bind.pars frag.pars)
                           (list bind
                                 (new (frag frag.oreg
                                            (append frag.code
                                                    (maybe-mov bind.loc frag.oreg))
                                            frag.fvars
                                            frag.ffuns)))))))
             (bfrag me.(comp-progn oreg nenv body))
             (boreg (if env.(out-of-scope bfrag.oreg) oreg bfrag.oreg)))
        (set ffrags (append-each ((bf ffrags))
                      (tree-bind (bind ff) bf
                        (when bind.used
                          (set ffuns (uni ffuns ff.ffuns)
                               fvars (uni fvars ff.fvars))
                          (list ff)))))
        (if ffrags
          (new (frag boreg
                     (append ^((frame ,nenv.lev ,frsize))
                             (mappend .code ffrags)
                             bfrag.code
                             (maybe-mov boreg bfrag.oreg)
                             ^((end ,boreg)))
                     (uni fvars bfrag.fvars)
                     (uni (diff bfrag.ffuns lexfuns)
                          (if rec (diff ffuns lexfuns) ffuns))))
          (new (frag boreg
                     (append me.(eliminate-frame bfrag.code nenv)
                             (maybe-mov boreg bfrag.oreg))
                     bfrag.fvars
                     bfrag.ffuns)))))))

(defmeth compiler comp-lambda-impl (me oreg env form)
  (mac-param-bind form (t par-syntax . body) form
    (with-access-spy me me.closure-spies
                     spy (new access-spy
                              closure-spies me.closure-spies)
      (compile-with-fresh-tregs me
        (let* ((*load-time* nil)
               (*top-level* nil)
               (pars (new (fun-param-parser par-syntax form)))
               (need-frame (or (plusp pars.nfix) pars.rest))
               (nenv (if need-frame (new env up env co me) env))
               lexsyms fvars specials need-dframe)
          (when (> pars.nfix %max-lambda-fixed-args%)
            (compile-warning form "~s arguments in a lambda (max is ~s)"
                             pars.nfix %max-lambda-fixed-args%))
          (flet ((spec-sub (sym : (loc :))
                   (cond
                     ((special-var-p sym)
                      (let ((sub (gensym)))
                        (push (cons sym sub) specials)
                        (set need-dframe t)
                        nenv.(extend-var sub loc)
                        sub))
                     (t
                       (push sym lexsyms)
                       nenv.(extend-var sym loc)
                       sym))))
            (let* ((req-pars (collect-each ((rp pars.req))
                               (spec-sub rp)))
                   (allsyms req-pars)
                   (opt-pars (collect-each ((op pars.opt))
                               (tree-bind (var-sym : init-form have-sym) op
                                 (let* ((loc nenv.(get-loc))
                                        (ifrag me.(compile loc nenv init-form)))
                                   (set fvars (uni fvars
                                                   (diff ifrag.fvars allsyms)))
                                   (push var-sym allsyms)
                                   (push have-sym allsyms)
                                   (list (spec-sub var-sym loc)
                                         ifrag
                                         (if have-sym (spec-sub have-sym)))))))
                   (rest-par (when pars.rest (spec-sub pars.rest))))
              (upd specials nreverse)
              (with-closure-spy me (and (not specials)
                                        (>= *opt-level* 2))
                                cspy (new closure-spy env nenv)
                (let* ((col-reg (if opt-pars me.(get-dreg :)))
                       (tee-reg (if opt-pars me.(get-dreg t)))
                       (ifrags [mapcar cadr opt-pars])
                       (opt-code (append-each ((op opt-pars)
                                               (ifrg ifrags))
                                   (tree-bind (var-sym t have-sym) op
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
                                              (set specials [remq have-sym specials cdr])
                                              ^((bindv ,have-bind.loc ,me.(get-dreg (car spec-sub))))))))))
                       (benv (if need-dframe (new env up nenv co me) nenv))
                       (btreg me.(alloc-treg))
                       (bfrag me.(comp-progn btreg benv body))
                       (boreg (if env.(out-of-scope bfrag.oreg) btreg bfrag.oreg))
                       (lskip (gensym "l"))
                       (frsize (if need-frame nenv.v-cntr 0))
                       (code ^((close ,oreg ,frsize ,me.treg-cntr ,lskip
                                      ,pars.nfix ,pars.nreq ,(if rest-par t nil)
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
                               (jend ,boreg)
                               ,lskip)))
                  me.(free-treg btreg)
                  (when (and cspy (plusp frsize) (null cspy.cap-vars))
                    (when-match ((close @reg @frsize @nil . @irest) . @crest)
                                me.(eliminate-frame code nenv)
                      (set code ^((close ,reg 0 ,me.treg-cntr ,*irest)
                                  ,*crest))))
                  nenv.(unused-check form "parameter")
                  (new (frag oreg code
                             (uni fvars (diff bfrag.fvars lexsyms))
                             (uni [reduce-left uni ifrags nil .ffuns]
                                  bfrag.ffuns)
                             pars)))))))))))

(defmeth compiler comp-lambda (me oreg env form)
  (if (or *load-time* *top-level* (< *opt-level* 3))
    me.(comp-lambda-impl oreg env form)
    (let* ((snap me.(snapshot))
           (lambda-frag me.(comp-lambda-impl oreg env form))
           (ok-lift-var-pov (all lambda-frag.fvars
                                 (lambda (sym)
                                   (not env.(lookup-var sym)))))
           (ok-lift-fun-pov (all lambda-frag.ffuns
                                 (lambda (sym)
                                   (not env.(lookup-fun sym))))))
      (cond
        ((and ok-lift-var-pov ok-lift-fun-pov)
         me.(restore snap)
         me.(compile oreg env ^(sys:load-time-lit nil ,form)))
        (t lambda-frag)))))

(defmeth compiler comp-fun (me oreg env form)
  (mac-param-bind form (t arg) form
    (let ((fbin env.(lookup-fun arg t)))
      (cond
        (fbin (new (frag fbin.loc nil nil (list arg))))
        ((and (consp arg) (eq (car arg) 'lambda))
         me.(compile oreg env arg))
        (t (new (frag oreg ^((getf ,oreg ,me.(get-sidx arg)))
                      nil (list arg))))))))

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

(defmeth compiler comp-or (me oreg env form)
  (tree-case (simplify-or form)
    ((t) me.(compile oreg env nil))
    ((t arg) me.(compile oreg env arg))
    ((t . args)
     (let* (ffuns fvars
            (nargs (len args))
            (lout (gensym "l"))
            (treg me.(maybe-alloc-treg oreg))
            (code (build
                    (each ((form args)
                           (n (range 1)))
                      (let ((islast (eql n nargs)))
                        (let ((frag me.(compile treg env form)))
                          (pend frag.code
                                (maybe-mov treg frag.oreg))
                          (unless islast
                            (add ^(ifq ,treg (t 0) ,lout)))
                          (set fvars (uni fvars frag.fvars))
                          (set ffuns (uni ffuns frag.ffuns))))))))
       me.(maybe-free-treg treg oreg)
       (new (frag oreg
                  (append code ^(,lout
                                 ,*(maybe-mov oreg treg)))
                  fvars ffuns))))))

(defmeth compiler comp-prog1 (me oreg env form)
  (tree-case form
    ((t fi . re) (let* ((igreg me.(alloc-treg))
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
    ((t fi) me.(compile oreg env fi))
    ((t) me.(compile oreg env nil))))

(defmeth compiler comp-progv (me oreg env form)
  (tree-case form
    ((t syms vals)
     me.(comp-progn oreg env ^(progn ,syms ,vals nil)))
    ((t syms vals . body)
     (let* ((denv (new env up env co me))
            (sreg me.(alloc-treg))
            (vreg me.(alloc-treg))
            (sfrag me.(compile sreg env syms))
            (vfrag me.(compile vreg env vals))
            (bfrag me.(comp-progn oreg denv body)))
       me.(free-treg sreg)
       me.(free-treg vreg)
       (new (frag bfrag.oreg
                  (append sfrag.code
                          vfrag.code
                          ^((dframe ,denv.lev 0)
                            (gcall ,oreg
                                   ,me.(get-sidx 'sys:rt-progv)
                                   ,sfrag.oreg
                                   ,vfrag.oreg))
                          bfrag.code
                          '((end nil)))
                  (uni sfrag.fvars (uni vfrag.fvars bfrag.fvars))
                  (uni sfrag.ffuns (uni vfrag.ffuns bfrag.ffuns))))))))

(defmeth compiler comp-quasi (me oreg env form)
  (let ((qexp (expand-quasi form)))
    me.(compile oreg env (expand qexp))))

(defmeth compiler comp-arith-form (me oreg env form)
  (if (plusp *opt-level*)
    (tree-case form
      ((op . args)
       (let* ((pargs [partition-by constantp args])
              (fargs (append-each ((pa pargs))
                       (if (and (constantp (car pa))
                                (all pa [chain safe-const-eval integerp]))
                         (list (safe-const-reduce
                                 (rlcp ^(,op ,*pa) form)))
                         pa))))
         me.(comp-fun-form oreg env (rlcp ^(,op ,*fargs) form))))
      (form me.(compile oreg env form)))
    me.(comp-fun-form oreg env form)))

(defmeth compiler comp-arith-neg-form (me oreg env form)
  (tree-case form
    ((nop a1 a2 a3 . args)
     (let* ((op (caseq nop (- '+) (/ '*)))
            (sform (rlcp ^(,op ,a2 ,a3 ,*args) form)))
       me.(comp-fun-form oreg env
                         (rlcp ^(,nop ,a1 ,sform) form))))
    (form me.(comp-fun-form oreg env form))))

(defmeth compiler comp-typep (me oreg env form)
  (match-case form
    ((typep @exp @(require @(constantp @type)
                           (eq t (safe-const-eval type))))
     me.(compile oreg env ^(progn ,exp t)))
    ((typep @exp @(require @(constantp @type)
                           (null (safe-const-eval type))))
     me.(compile oreg env ^(progn ,exp nil)))
    (@nil
     me.(comp-fun-form oreg env form))))

(defmeth compiler comp-compiler-let (me oreg env form)
  (tree-bind (t bindings . body) form
    (progv [mapcar car bindings]
           [mapcar [chain cadr no-dvbind-eval] bindings]
           me.(comp-progn oreg env body))))

(defmeth compiler comp-fun-form (me oreg env form)
  (let* ((olev *opt-level*)
         (sym (car form))
         (nargs (len (cdr form)))
         (fbin env.(lookup-fun sym t))
         (pars (or fbin.?pars
                   (get-param-info sym))))
    (if pars
      (param-check form nargs pars)
      (push (cons form nargs) *unchecked-calls*))

    (when (null fbin)
      (when (plusp olev)
        (match-case form
          ((equal @a @b)
           (cond
             ((or (eq-comparable a)
                  (eq-comparable b))
              (set form (rlcp ^(eq ,a ,b) form)))
             ((or (eql-comparable a)
                  (eql-comparable b))
              (set form (rlcp ^(eql ,a ,b) form)))))
          ((not (@(and @(or eq eql equal) @op) @a @b))
           (let ((nop (caseq op (eq 'neq) (eql 'neql) (equal 'nequal))))
             (return-from comp-fun-form me.(compile oreg env ^(,nop ,a ,b)))))
          ((@(or append cons list list*) . @nil)
           (set form (reduce-lisp form)))
          ((@(@bin [%bin-op% @sym]) @a @b)
           (set form (rlcp ^(,bin ,a ,b) form)))
          ((- @a)
           (set form (rlcp ^(neg ,a) form)))
          ((@(or ignore nilf) . @args)
           (if (eql sym 'ignore)
             (each ((a args))
               (if (bindable a)
                 env.(lookup-var a t))))
           (return-from comp-fun-form me.(compile oreg env
                                                  ^(progn ,*args nil))))
          ((@(or identity use + * min max logior logand) @a)
           (return-from comp-fun-form me.(compile oreg env a)))))

      (when (plusp olev)
        (tree-case form
          ((t . t)
           (set form (reduce-constant env form)))))

      (when (or (atom form) (special-operator-p (car form)))
        (return-from comp-fun-form me.(compile oreg env form))))

    (tree-bind (sym . args) form
      (let* ((fbind env.(lookup-fun sym t)))
        (macrolet ((comp-fun ()
                      'me.(comp-call-impl oreg env (if fbind 'call 'gcall)
                                          (if fbind fbind.loc me.(get-sidx sym))
                                          args sym)))
          (if (and (>= olev 3)
                   (not fbind)
                   (not *load-time*)
                   [%functional% sym])
            (let* ((snap me.(snapshot))
                   (cfrag (comp-fun))
                   (ok-lift-var-pov (null cfrag.fvars))
                   (ok-lift-fun-pov (all cfrag.ffuns
                                         (lambda (sym)
                                           (and (not env.(lookup-fun sym))
                                                (eq (symbol-package sym)
                                                    user-package))))))
              (cond
                ((and ok-lift-var-pov ok-lift-fun-pov)
                 me.(restore snap)
                 me.(compile oreg env ^(sys:load-time-lit nil ,form)))
                (t (pushnew sym cfrag.ffuns)
                   cfrag)))
            (let ((cfrag (comp-fun)))
              (pushnew sym cfrag.ffuns)
              cfrag)))))))

(defmeth compiler comp-apply-call (me oreg env form)
  (let ((olev *opt-level*))
    (tree-bind (sym . oargs) form
      (let ((args (if (plusp olev)
                    [mapcar (op reduce-constant env) oargs]
                    oargs)))
        (let ((gopcode [%gcall-op% sym])
              (opcode [%call-op% sym]))
          (cond
            ((and (plusp olev)
                  (eq sym 'call)
                  [all args constantp]
                  (let ((op (safe-const-eval (car args))))
                    (or [%const-foldable% op]
                        (not (bindable op)))))
             (let ((crform (safe-const-reduce form)))
               (if (eq crform form)
                 me.(comp-fun-form oreg env crform)
                 me.(compile oreg env crform))))
            (t (tree-case (car args)
                 ((op arg . more)
                  (caseq op
                    (fun (cond
                           (more (compile-error form "excess args in fun form"))
                           ((bindable arg)
                            (let ((fbind env.(lookup-fun arg t)))
                              me.(comp-call-impl oreg env (if fbind opcode gopcode)
                                                 (if fbind fbind.loc me.(get-sidx arg))
                                                 (cdr args) arg)))
                           ((and (consp arg) (eq (car arg) 'lambda))
                            me.(comp-fun-form oreg env ^(,sym ,arg ,*(cdr args))))
                           (t :)))
                    (lambda me.(comp-inline-lambda oreg env opcode
                                                   (car args) (cdr args)))
                    (t :)))
                 (t me.(comp-call oreg env
                                  (if (eq sym 'usr:apply) 'apply sym) args))))))))))

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

(defmeth compiler comp-call-impl (me oreg env opcode freg args : extra-ffun)
  (with-access-spy me t spy (new simplify-var-spy)
    (let* ((aoregs nil)
           (afrags0 (collect-each ((arg args))
                      (let* ((aoreg me.(alloc-treg))
                             (afrag me.(compile aoreg env arg)))
                        (push aoreg aoregs)
                        afrag)))
           (afrags (handle-mutated-var-args afrags0 spy.mutated-vars))
           (fvars [reduce-left uni afrags nil .fvars])
           (ffuns [reduce-left uni afrags nil .ffuns]))
      me.(free-tregs aoregs)
      (when extra-ffun
        (pushnew extra-ffun ffuns))
      (new (frag oreg
                 ^(,*(mappend .code afrags)
                   (,opcode ,oreg ,freg ,*(mapcar .oreg afrags)))
                 fvars ffuns)))))

(defmeth compiler comp-inline-lambda (me oreg env opcode lambda args)
  (let ((reg-args args) apply-list-arg)
    (when (eql opcode 'apply)
      (unless args
        (compile-error lambda "apply requires arguments"))
      (set reg-args (butlast args)
           apply-list-arg (car (last args))))
    me.(compile oreg env (expand (lambda-apply-transform lambda
                                                         reg-args
                                                         apply-list-arg
                                                         nil)))))

(defmeth compiler comp-for (me oreg env form)
  (mac-param-bind form (t inits (: (test nil test-p) . rets) incs . body) form
    (let* ((treg me.(alloc-treg))
           (ifrag me.(comp-progn treg env inits))
           (*load-time* nil)
           (tfrag (progn
                    (inc me.loop-nest)
                    (if test-p me.(compile treg env test))))
           (rfrag me.(comp-progn oreg env rets))
           (nfrag me.(comp-progn treg env incs))
           (bfrag (prog1
                    me.(comp-progn treg env body)
                    (dec me.loop-nest)))
           (lback (gensym "l"))
           (lskip (gensym "l"))
           (frags (build
                    (add ifrag)
                    (if test-p (add tfrag))
                    (add rfrag nfrag bfrag))))
      me.(free-treg treg)
      (new (frag rfrag.oreg
                 ^(,*ifrag.code
                   ,lback
                   ,*(if test-p
                      ^(,*tfrag.code
                        (if ,tfrag.oreg ,lskip)))
                   ,*bfrag.code
                   ,*nfrag.code
                   (jmp ,lback)
                   ,*(if test-p
                       ^(,lskip
                         ,*rfrag.code)))
                   [reduce-left uni frags nil .fvars]
                   [reduce-left uni frags nil .ffuns])))))

(defmeth compiler comp-tree-bind (me oreg env form)
  (tree-bind (op params obj . body) form
    (with-gensyms (obj-var)
      (let* ((simp-form (rlcp-tree ^'(,op) form))
             (expn (expand ^(let ((,obj-var ,obj))
                             ,(expand-bind-mac-params simp-form form
                                                      params nil
                                                      obj-var t nil body)))))
        me.(compile oreg env expn)))))

(defmeth compiler comp-mac-param-bind (me oreg env form)
  (mac-param-bind form (t context params obj . body) form
    (with-gensyms (obj-var form-var)
      (let ((expn (expand ^(let ((,obj-var ,obj)
                                 (,form-var ,context))
                             ,(expand-bind-mac-params form-var form
                                                      params nil
                                                      obj-var t nil body)))))
        me.(compile oreg env expn)))))

(defmeth compiler comp-mac-env-param-bind (me oreg env form)
  (mac-param-bind form (t context menv params obj . body) form
    (with-gensyms (obj-var form-var)
      (let ((expn (expand ^(let ((,obj-var ,obj)
                                 (,form-var ,context))
                             ,(expand-bind-mac-params form-var form
                                                      params menv
                                                      obj-var t nil body)))))
        me.(compile oreg env expn)))))

(defmeth compiler comp-tree-case (me oreg env form)
  (mac-param-bind form (op obj . cases) form
    (let* ((nenv (new env up env co me))
           (obj-immut-var nenv.(extend-var (gensym)))
           (obj-var nenv.(extend-var (gensym)))
           (err-blk (gensym))
           (lout (gensym "l"))
           (ctx-form (rlcp-tree ^'(,op) form))
           (treg me.(maybe-alloc-treg oreg))
           (objfrag me.(compile treg env obj))
           (cfrags (collect-each ((c cases)
                                  (i (range 1)))
                     (mac-param-bind form (params . body) c
                       (let* ((src (expand ^(block ,err-blk
                                              (set ,obj-var.sym
                                                   ,obj-immut-var.sym)
                                              ,(expand-bind-mac-params
                                                 ctx-form form
                                                 params nil obj-var.sym :
                                                 err-blk
                                                 body))))
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
  (mac-param-bind form (t arg) form
    (cond
      ((bindable arg)
       (let ((bind env.(lookup-lisp1 arg t)))
         (cond
           (bind
             (each ((spy me.access-spies))
               spy.(accessed bind arg))
             (new (frag bind.loc
                        nil
                        (if (typep bind 'vbinding) (list arg))
                        (if (typep bind 'fbinding) (list arg)))))
           ((not (boundp arg))
            (pushnew arg assumed-fun)
            (new (frag oreg
                       ^((getf ,oreg ,me.(get-sidx arg)))
                       nil
                       (list arg))))
           ((special-var-p arg)
            (new (frag oreg
                       ^((getv ,oreg ,me.(get-dreg arg)))
                       (list arg)
                       nil)))
           (t (new (frag oreg
                       ^((getlx ,oreg ,me.(get-sidx arg)))
                       (list arg)
                       nil))))))
      (t me.(compile oreg env arg)))))

(defmeth compiler comp-dwim (me oreg env form)
  (mac-param-bind form (t obj . args) form
    (ignore obj args)
    (let* ((l1-exprs (cdr form))
           (fun (car l1-exprs))
           (bind env.(lookup-lisp1 fun nil)))
      me.(compile oreg env
                  (if (and (symbolp fun)
                           (not bind)
                           (not (boundp fun)))
                    (progn
                      (pushnew fun assumed-fun)
                      ^(,fun ,*(mapcar [iffi bindable (op list 'sys:lisp1-value)] (cdr l1-exprs))))
                    ^(call ,*(mapcar [iffi bindable (op list 'sys:lisp1-value)] l1-exprs)))))))

(defmeth compiler comp-prof (me oreg env form)
  (mac-param-bind form (t . forms) form
    (let ((bfrag me.(comp-progn oreg env forms)))
      (new (frag oreg
                 ^((prof ,oreg)
                   ,*bfrag.code
                   (xend ,bfrag.oreg))
                 bfrag.fvars bfrag.ffuns)))))

(defun handle-mutated-var-args (frags mutated-vars)
  (if mutated-vars
    (build
      (each ((frag frags))
        (let* ((vbin frag.vbin)
               (oreg frag.alt-oreg))
          (add (if (and vbin (memq vbin mutated-vars))
                 (new (frag oreg (append frag.code (maybe-mov oreg vbin.loc))
                            frag.fvars frag.ffuns frag.pars))
                 frag)))))
    frags))

(defun misleading-ref-check (frag env form)
  (each ((v frag.fvars))
    (when env.(lookup-var v)
      (compile-warning form "cannot refer to lexical variable ~s" v)))
  (each ((f frag.ffuns))
    (when env.(lookup-fun f)
      (compile-warning form "cannot refer to lexical function ~s" f))))

(defmeth compiler comp-load-time-lit (me oreg env form)
  (mac-param-bind form (t loaded-p exp) form
    (cond
      (loaded-p me.(compile oreg env ^(quote ,exp)))
      ((or *load-time* (constantp exp)) me.(compile oreg env exp))
      (t (compile-in-toplevel me
           (let* ((*load-time* t)
                  (dreg me.(alloc-dreg))
                  (exp me.(compile dreg (new env co me) exp))
                  (lt-frag (new (frag dreg
                                      ^(,*exp.code
                                         ,*(maybe-mov dreg exp.oreg))
                                      exp.fvars
                                      exp.ffuns
                                      exp.pars))))
             (misleading-ref-check exp env form)
             (push lt-frag me.lt-frags)
             (new (frag dreg nil nil nil exp.pars))))))))

(defmeth compiler compact-dregs-and-syms (me insns)
  (let ((dmap (hash))
        (smap (vector (len me.sidx)))
        (used-syms 0)
        (dc 0)
        (sc 0))

    (each ((insn insns))
      (if-match @(coll @(as dr (d @nil))) insn
        (each ((d dr))
          (unless (inhash dmap d)
            (set [dmap d] ^(d ,(pinc dc))))))
      (if-match (@(or gcall gapply getf getlx setlx) @nil @fn . @nil) insn
        (set-mask used-syms (mask fn))))

    (let ((data (hash :eql-based)))
      (dohash (from-dreg to-dreg dmap)
        (set [data (cadr to-dreg)] [me.data (cadr from-dreg)]))
      (set me.data data
         me.datavec nil
         me.dreg-cntr dc)
      (each ((cell me.dreg))
        (upd (cdr cell) dmap)))

    (let ((stab (hash :eql-based))
          (sidx (hash :eql-based))
          (nsym (width used-syms)))
      (each ((from 0..nsym))
        (when (bit used-syms from)
          (let ((to (pinc sc))
                (atom [me.stab from]))
            (set [stab to] atom
                 [sidx atom] to
                 [smap from] to))))
      (set me.stab stab
           me.sidx sidx
           me.sidx-cntr sc
           me.symvec nil))

    (mapcar [iffi consp (opip
                          (mapcar [orf dmap use])
                          (do if-match (@(as op @(or gcall gapply
                                                     getf getlx setlx))
                                         @dest @fn . @args)
                                       @1
                            ^(,op ,dest ,[smap fn] ,*args)
                            @1))]
            insns)))

(defmeth compiler optimize (me insns)
  (let ((olev *opt-level*))
    (if (>= olev 4)
      (let* ((lt-dregs (mapcar .oreg me.lt-frags))
             (bb (new (basic-blocks me insns lt-dregs me.(get-symvec)))))
        (when (>= olev 4)
          bb.(thread-jumps)
          bb.(elim-dead-code))
        (when (>= olev 5)
          (let ((nblocks nil))
            (while* (and (>= olev 6)
                         (neql nblocks (set nblocks bb.(num-blocks))))
              bb.(calc-liveness)
              bb.(peephole)
              bb.(link-graph)
              bb.(thread-jumps)
              bb.(elim-dead-code))))
        (cond
          ((>= olev 7)
           bb.(merge-jump-thunks)
           bb.(compact-tregs)
           bb.(late-peephole me.(compact-dregs-and-syms bb.(get-insns))))
          ((>= olev 5)
           me.(compact-dregs-and-syms bb.(get-insns)))
          (t  bb.(get-insns))))
      insns)))

(defun true-const-p (arg)
  (and arg (constantp arg)))

(defun eq-comparable (arg)
  (and (constantp arg)
       [[orf fixnump chrp symbolp] (eval arg)]))

(defun eql-comparable (arg)
  (and (constantp arg)
       [[orf symbolp chrp numberp] (eval arg)]))

(defun expand-and (form)
  (match-case form
    ((and) t)
    ((and @a) a)
    ((and @(true-const-p) . @rest) (expand-and ^(and ,*rest)))
    ((and nil . @nil) nil)
    ((and @a . @rest) ^(if ,a ,(expand-and ^(and ,*rest))))
    (@else else)))

(defun flatten-or (form)
  (match-case form
    ((or . @args) ^(or ,*[mappend [chain flatten-or cdr] args]))
    (@else ^(or ,else))))

(defun reduce-or (form)
  (match-case form
    ((or) form)
    ((or @nil) form)
    ((or nil . @rest) (reduce-or ^(or ,*rest)))
    ((or @(true-const-p @c) . @nil) ^(or ,c))
    ((or @a . @rest) ^(or ,a ,*(cdr (reduce-or ^(or ,*rest)))))
    (@else else)))

(defun simplify-or (form)
  (reduce-or (flatten-or form)))

(defmacro fixed-point (eqfn sym exp)
  (with-gensyms (osym)
    ^(let (,osym)
       (while* (not (,eqfn ,osym ,sym))
         (set ,osym ,sym
              ,sym ,exp))
       ,sym)))

(defun reduce-lisp (form)
  (fixed-point equal form
    (rlcp
      (match-case form
        ((append (list . @largs) . @aargs)
         ^(list* ,*largs (append ,*aargs)))
        ((@(or append list*) @arg) arg)
        (@(require (list* . @(listp @args))
                   (equal '(nil) (last args)))
          ^(list ,*(butlastn 1 args)))
        (@(with (list* . @(listp @args))
                ((@(and @op @(or list list*)) . @largs)) (last args))
          ^(,op ,*(butlast args) ,*largs))
        (@(with (list* . @(listp @args))
                ((append . @aargs)) (last args))
          ^(list* ,*(butlast args) ,(reduce-lisp ^(append ,*aargs))))
        ((@(or append list list*)) nil)
        ((cons @a @b)
         (let* ((lstar ^(list* ,a ,b))
                (rstar (reduce-lisp lstar)))
           (if (eq lstar rstar) form rstar)))
        ((cons @a (cons @b @c))
         ^(list* ,a ,b ,c))
        ((cons @a (@(and @op @(or list list*)) . @args))
         ^(,op ,a ,*args))
        (@else else))
      form)))

(defun reduce-constant (env form)
  (if (consp form)
    (tree-bind (op . args) form
      (if (and [%const-foldable% op]
               (not env.(lookup-fun op)))
        (let ((cargs [mapcar (op reduce-constant env) args]))
          (if [all cargs constantp]
            (safe-const-reduce (rlcp ^(,op ,*cargs) form))
            (rlcp ^(,op ,*cargs) form)))
        form))
    form))

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
                    (compile-error form "missing argument in range/index: ~s"
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
         (sys:var (mac-param-bind form (t exp : mods) el
                    (list (expand-quasi-mods exp mods))))
         (sys:quasi (expand-quasi-args el))
         (t (list ^(sys:fmt-simple ,el)))))
      ((bindable el)
       (list ^(sys:fmt-simple ,el)))
      (t
        (list el)))))

(defun expand-quasi (form)
  (let ((qa (expand-quasi-args form)))
    (cond
      ((cdr qa) ^(sys:fmt-join ,*qa))
      (qa (car qa))
      (t '(mkstring 0)))))

(defun expand-dohash (form)
  (mac-param-bind form (t (key-var val-var hash-form : res-form) . body) form
    (with-gensyms (iter-var cell-var)
      (rlcp
        ^(let (,key-var ,val-var (,iter-var (hash-begin ,hash-form)) ,cell-var)
           (block nil
             (sys:for-op ((sys:setq ,cell-var (hash-next ,iter-var)))
                         (,cell-var ,res-form)
                         ((sys:setq ,cell-var (hash-next ,iter-var)))
                (sys:setq ,key-var (car ,cell-var))
                (sys:setq ,val-var (cdr ,cell-var))
                ,*body)))
        form))))

(defun expand-each (form env)
  (mac-param-bind form (t each-type vars . body) form
    (when (eq vars t)
      (set vars [mapcar car env.vb]))
    (let* ((gens (mapcar (ret (gensym)) vars))
           (out (if (member each-type '(collect-each append-each))
                  (gensym)))
           (accum (if out (gensym))))
      ^(let* (,*(mapcar (ret ^(,@1 (iter-begin ,@2))) gens vars)
               ,*(if accum ^((,out (cons nil nil)) (,accum ,out))))
         (block nil
           (sys:for-op ()
                       ((and ,*(mapcar (op list 'iter-more) gens))
                        ,*(if accum (if (eq each-type 'collect-each)
                                      ^((cdr ,out))
                                      ^((sys:apply (fun append) ,out)))))
                       (,*(mapcar (ret ^(sys:setq ,@1 (iter-step ,@1))) gens))
             ,*(mapcar (ret ^(sys:setq ,@1 (iter-item ,@2))) vars gens)
             ,*(caseq each-type
                 ((collect-each append-each)
                    ^((rplacd ,accum (cons (progn ,*body) nil))
                      (sys:setq ,accum (cdr ,accum))))
                 (t body))))))))

(defun expand-bind-mac-params (ctx-form rlcp-form params menv-var
                               obj-var strict err-block body)
  (let (gen-stk stmt vars)
    (labels ((get-gen ()
               (or (pop gen-stk) (gensym)))
             (put-gen (g)
               (push g gen-stk))
             (expand-rec (par-syntax obj-var check-var)
               (labels ((emit-stmt (form)
                          (when form
                            (if check-var
                              (push ^(when ,check-var ,form) stmt)
                              (push form stmt))))
                        (emit-var (sym init-form)
                          (if (eq sym t)
                            (emit-stmt init-form)
                            (push (if stmt
                                    (prog1
                                      ^(,sym (progn ,*(nreverse stmt)
                                               ,(if check-var
                                                  ^(when ,check-var ,init-form)
                                                  init-form)))
                                      (set stmt nil))
                                    ^(,sym ,(if check-var
                                              ^(when ,check-var ,init-form)
                                              init-form)))
                              vars))))
                 (let ((pars (new (mac-param-parser par-syntax rlcp-form))))
                   (progn
                     (cond
                       ((eq strict t)
                        (emit-stmt
                          ^(sys:bind-mac-check ,ctx-form ',par-syntax
                                               ,obj-var ,pars.nreq
                                               ,(unless pars.rest
                                                  pars.nfix))))
                       ((null strict))
                       ((symbolp strict)
                        (emit-stmt
                          (let ((len-expr ^(if (consp ,obj-var)
                                             (len ,obj-var) 0)))
                            (if pars.rest
                              ^(unless (<= ,pars.nreq ,len-expr)
                                 (return-from ,err-block ',strict))
                              ^(unless (<= ,pars.nreq ,len-expr ,pars.nfix)
                                 (return-from ,err-block ',strict)))))))
                     (each ((k pars.key))
                       (tree-bind (key . sym) k
                         (caseq key
                           (:whole (emit-var sym obj-var))
                           (:form (emit-var sym ctx-form))
                           (:env (emit-var sym menv-var)))))
                     (each ((p pars.req))
                       (cond
                         ((listp p)
                           (let ((curs (get-gen)))
                             (emit-stmt ^(set ,curs (car ,obj-var)))
                             (emit-stmt ^(set ,obj-var (cdr ,obj-var)))
                             (expand-rec p curs check-var)
                             (put-gen curs)))
                         (t (if (neq p t)
                              (emit-var p ^(car ,obj-var)))
                            (emit-stmt ^(set ,obj-var (cdr ,obj-var))))))
                     (each ((o pars.opt))
                       (tree-bind (p : init-form pres-p) o
                         (cond
                           ((listp p)
                             (let* ((curs (get-gen))
                                    (stmt ^(cond
                                            (,obj-var
                                              (set ,curs (car ,obj-var))
                                              (set ,obj-var (cdr ,obj-var))
                                              ,*(if pres-p '(t)))
                                            (t
                                              (set ,curs ,init-form)
                                              ,*(if pres-p '(nil))))))
                               (if pres-p
                                 (emit-var pres-p stmt)
                                 (emit-stmt stmt))
                               (let ((cv (gensym)))
                                 (emit-var cv curs)
                                 (expand-rec p curs cv)
                                 (put-gen curs))))
                           (t
                             (cond
                               (pres-p
                                 (emit-var p nil)
                                 (emit-var pres-p
                                           ^(cond
                                              (,obj-var
                                                ,(if (neq p t)
                                                   ^(set ,p (car ,obj-var)))
                                                (set ,obj-var (cdr ,obj-var))
                                                t)
                                              (t
                                                ,(cond
                                                   ((and (neq p t) init-form)
                                                    ^(set ,p ,init-form))
                                                   (init-form))
                                                nil))))
                               (t
                                 (emit-var p ^(if ,obj-var
                                                (prog1
                                                  (car ,obj-var)
                                                  (set ,obj-var (cdr ,obj-var)))
                                                (if ,init-form ,init-form)))))))))
                     (when pars.rest
                       (emit-var pars.rest obj-var)))))))
      (expand-rec params obj-var nil)
      (when stmt
        (push ^(,(gensym) (progn ,*(nreverse stmt))) vars))
      (rlcp ^(let* (,*gen-stk ,*(nreverse vars))
               ,*body)
            rlcp-form))))

(defun expand-defvarl (form)
  (mac-param-bind form (t sym : value) form
    (with-gensyms (cell)
      (if value
        ^(let ((,cell (sys:rt-defv ',sym)))
           (if ,cell
             (usr:rplacd ,cell ,value))
           ',sym)
        ^(progn (sys:rt-defv ',sym) ',sym)))))

(defun expand-defun (form)
  (mac-param-bind form (t name args . body) form
    (flet ((mklambda (block-name block-sym)
             (rlcp ^(lambda ,args (,block-sym ,block-name ,*body)) form)))
      (cond
        ((bindable name)
         ^(sys:rt-defun ',name ,(mklambda name 'sys:blk)))
        ((consp name)
         (caseq (car name)
           (meth
             (mac-param-bind form (t type slot) name
               (rlcp ^(sys:define-method ',type ',slot ,(mklambda slot 'block))
                     form)))
           (macro
             (mac-param-bind form (t sym) name
               ^(sys:rt-defmacro ',sym ',name ,(mklambda sym 'sys:blk))))
           (t (compile-error form "~s isn't a valid compound function name"
                             name))))
        (t (compile-error form "~s isn't a valid function name" name))))))

(defun expand-defmacro (form)
  (mac-param-bind form (t name mac-args . body) form
    (with-gensyms (mform menv spine-iter)
      (let ((exp-lam (rlcp ^(lambda (,mform ,menv)
                              (let ((,spine-iter (cdr ,mform)))
                                ,(expand (expand-bind-mac-params mform
                                                                 form
                                                                 mac-args
                                                                 menv spine-iter
                                                                 t nil
                                           ^((sys:set-macro-ancestor
                                               (block ,name ,*body)
                                               ,mform))))))
                           form)))
        ^(progn
           (sys:rt-defmacro ',name '(macro ,name) ,exp-lam)
           ',name)))))

(defun expand-defsymacro (form)
  (mac-param-bind form (t name def) form
    ^(sys:rt-defsymacro ',name ',def)))

(defun lambda-apply-transform (lm-expr fix-arg-exprs apply-list-expr recursed)
  (if (and (not recursed)
           apply-list-expr
           (safe-constantp apply-list-expr))
    (let* ((apply-list-val (safe-const-eval apply-list-expr))
           (apply-atom (nthlast 0 apply-list-val))
           (apply-fixed (butlastn 0 apply-list-val)))
      (lambda-apply-transform lm-expr (append fix-arg-exprs
                                              (mapcar (ret ^',@1) apply-fixed))
                              ^',apply-atom t))
    (mac-param-bind lm-expr (t lm-args . lm-body) lm-expr
      (let* ((pars (new (fun-param-parser lm-args lm-expr)))
             (fix-vals (mapcar (ret (gensym)) fix-arg-exprs))
             (fix-arg-iter fix-arg-exprs)
             (check-opts)
             (ign-1 (gensym))
             (ign-2 (gensym))
             (al-val (gensym))
             (shadow-p (let ((all-vars (append pars.req pars.(opt-syms)
                                               (if pars.rest (list pars.rest)))))
                         (or (isecp all-vars fix-arg-iter)
                             (member apply-list-expr all-vars)))))
        (rlcp ^(,(if shadow-p 'let 'alet) ,(zip fix-vals fix-arg-iter)
                 (let* ,(build
                          (if apply-list-expr
                            (add ^(,al-val ,apply-list-expr)))
                          (while (and fix-vals pars.req)
                            (add ^(,(pop pars.req) ,(pop fix-vals)))
                            (pop fix-arg-iter))
                          (while (and fix-vals pars.opt)
                            (tree-bind (var-sym : init-form have-sym) (pop pars.opt)
                              (add ^(,var-sym ,(car fix-vals)))
                              (if have-sym
                                (add ^(,have-sym t)))
                              (unless (and (safe-constantp (car fix-arg-iter))
                                           (neq (safe-const-eval (car fix-arg-iter))
                                                :))
                                (push (list* var-sym have-sym init-form) check-opts)))
                            (pop fix-vals)
                            (pop fix-arg-iter))
                          (cond
                            ((and (null pars.req)
                                  (null pars.opt))
                             (if fix-vals
                               (if pars.rest
                                 (add ^(,pars.rest
                                         (list*
                                           ,*(nthcdr pars.nfix
                                                     ^(,*fix-arg-exprs ,apply-list-expr)))))
                                 (lambda-too-many-args lm-expr))
                               (cond
                                 ((and pars.rest apply-list-expr)
                                  (add ^(,pars.rest ,al-val)))
                                 (pars.rest
                                   (add ^(,pars.rest nil)))
                                 (apply-list-expr
                                   (add ^(,ign-2 (if ,al-val
                                                   (lambda-excess-apply-list))))))))
                            ((and fix-vals apply-list-expr)
                             (lambda-too-many-args lm-expr))
                            (apply-list-expr
                              (when pars.req
                                (add ^(,ign-1 (if (< (len ,al-val) ,(len pars.req))
                                                  (lambda-short-apply-list)))))
                              (while pars.req
                                (add ^(,(pop pars.req) (pop ,al-val))))
                              (while pars.opt
                                (tree-bind (var-sym : init-form have-sym) (pop pars.opt)
                                  (cond
                                    (have-sym
                                      (add ^(,var-sym (if ,al-val
                                                        (car ,al-val)
                                                        ,init-form)))
                                      (add ^(,have-sym (when ,al-val
                                                         (pop ,al-val)
                                                         t))))
                                    (t (add ^(,var-sym (if ,al-val
                                                         (pop ,al-val)
                                                         ,init-form)))))
                                (push (list* var-sym have-sym init-form) check-opts)))
                              (if pars.rest
                                (add ^(,pars.rest ,al-val))
                                (add ^(,ign-2 (if ,al-val
                                                (lambda-excess-apply-list))))))
                            (pars.req
                              (lambda-too-few-args lm-expr))
                            (pars.opt
                              (while pars.opt
                                (tree-bind (var-sym : init-form have-sym) (pop pars.opt)
                                  (add ^(,var-sym ,init-form))
                                  (if have-sym
                                    (add ^(,have-sym)))))
                              (when pars.rest
                                (add ^(,pars.rest))))))
                   ,*(mapcar (tb ((var-sym have-sym . init-form))
                               ^(when (eq ,var-sym :)
                                  (set ,var-sym ,init-form)
                                  ,*(if have-sym
                                      ^((set ,have-sym nil)))))
                             (nreverse check-opts))
                   ,*lm-body))
              lm-expr)))))

(defun orig-form (form)
  (whilet ((anc (macro-ancestor form)))
    (set form anc))
  form)

(defun safe-const-reduce (form)
  (condlet
    ((((atom form))) form)
    (((ece [%eval-cache% form]))
     ece.reduced-form)
    (t
      (let* ((throws nil)
             (reduced-form (usr:catch (let ((result (eval form)))
                                        (if (or (consp result)
                                                (bindable result))
                                          ^(quote ,result)
                                          result))
                             (t (exc)
                                (ignore exc)
                                (set throws t)
                                form)))
             (ece (new eval-cache-entry
                       orig-form (orig-form form)
                       reduced-form reduced-form
                       throws throws)))
        (set [%eval-cache% form] ece)
        reduced-form))))

(defun safe-const-eval (form)
  (unless [%eval-cache% form].?throws
    (eval form)))

(defun safe-constantp (form)
  (if (constantp form)
    (or (atom form)
        (progn (safe-const-reduce form)
               (not [%eval-cache% form].?throws)))))

(defun eval-cache-emit-warnings ()
  (dohash (form ece %eval-cache%)
    (when ece.throws
      (del [%eval-cache% form])
      (let ((of ece.orig-form))
        (when (or (source-loc of)
                  (and (consp of)
                       (neq system-package (symbol-package (car of)))))
          (compile-warning ece.orig-form
                           "constant expression ~s throws" ece.orig-form))))))

(defun system-symbol-p (sym)
  (member (symbol-package sym)
          (load-time (list user-package system-package))))

(defun no-dvbind-eval (form)
  (eval (if-match (sys:dvbind @nil @exp) form exp form)))

(defun usr:compile-toplevel (exp : (expanded-p nil))
  (let ((co (new compiler))
        (as (new assembler))
        (*dedup* (or *dedup* (hash)))
        (*load-time* nil)
        (*top-level* t)
        (*opt-level* (or *opt-level* 0)))
    (let* ((oreg co.(alloc-treg))
           (xexp (if expanded-p
                   exp
                   (unwind-protect
                     (expand* exp)
                     (unless *load-recursive*
                       (release-deferred-warnings)))))
           (frag co.(compile oreg (new env co co) xexp)))
      (unless *load-recursive*
        (eval-cache-emit-warnings))
      co.(free-treg oreg)
      co.(check-treg-leak)
      (let ((insns co.(optimize ^(,*(mappend .code (nreverse co.lt-frags))
                                   ,*frag.code
                                   (jend ,frag.oreg)))))
        (unless (< co.dreg-cntr %lev-size%)
          (compile-error co.last-form "code too complex: too many literals"))
        as.(asm insns))
      (vm-make-desc co.nlev (succ as.max-treg) as.buf co.(get-datavec) co.(get-symvec)))))

(defun get-param-info (sym)
  (whenlet ((fun (symbol-function sym)))
    (or [%param-info% fun]
        (set [%param-info% fun]
             (new param-info fun fun)))))

(defun param-check (form nargs pars)
  (cond
    ((< nargs pars.nreq)
     (compile-warning form "too few arguments: needs ~s, given ~s"
                      pars.nreq nargs))
    (pars.rest)
    ((> nargs pars.nfix)
     (compile-warning form "too many arguments: max ~s, given ~s"
                      pars.nfix nargs))))

(defun compiler-emit-warnings ()
  (let ((warn-fun [keep-if boundp (zap assumed-fun)]))
    (when warn-fun
      (usr:catch
        (throw 'warning
               `uses of @{warn-fun ", "} compiled as functions,\
               \ then defined as vars`)
        (continue ()))))
  (each ((uc (zap *unchecked-calls*)))
    (when-match (@(as form (@sym . @nil)) . @nargs) uc
      (whenlet ((fun (symbol-function sym)))
        (param-check form nargs (get-param-info sym))))))

(defvar *emit*)

(defvar *eval*)

(defvarl %big-endian% (equal (ffi-put 1 (ffi uint32)) #b'00000001'))

(defvarl %tlo-ver% ^(7 0 ,%big-endian%))

(defvarl %package-manip% '(make-package delete-package
                           use-package unuse-package
                           set-package-fallback-list
                           intern unintern rehome-sym
                           use-sym unuse-sym))

(defmacro ign-notfound (form)
  ^(usr:catch ,form (path-not-found (. rest) (ignore rest))))

(defun open-compile-streams (in-path out-path test-fn)
  (if (and (nullify in-path)
           (find [in-path -1] path-sep-chars))
    (error "~s: invalid input pathname ~s" 'compile-file in-path))
  (let* ((parent (or *load-path* ""))
         (in-path (if (and (pure-rel-path-p in-path) (not (empty parent)))
                    (path-cat (dir-name parent) in-path)
                    in-path))
         (suff (short-suffix in-path))
         (ip-nosuff (trim-right suff in-path))
         in-stream out-stream)
    (casequal suff
      (".txr" (error "~s: cannot compile TXR files" 'compile-file))
      (".tl" (set in-stream (ign-notfound (open-file in-path))
                  out-path (or out-path `@{ip-nosuff}.tlo`)))
      (t (set in-stream (or (ign-notfound (open-file in-path))
                            (ign-notfound (open-file `@{in-path}.tl`)))
              out-path (or out-path `@{in-path}.tlo`))))

    (unless in-stream
      (error "~s: unable to open input file ~s" 'compile-file in-path))

    (unless [test-fn in-stream out-path]
      (close-stream in-stream)
      (return-from open-compile-streams nil))

    (unwind-protect
      (set out-stream (open-file out-path "w"))
      (unless out-stream
        (close-stream in-stream)))

    (list in-stream out-stream out-path)))

(defun clean-file (path)
  (let* ((lev (or *compile-opts*.log-level 0))
         (parent *load-path*)
         (path (if (and parent (pure-rel-path-p path))
                 (path-cat (dir-name parent) path)
                 path)))
    (flet ((try-clean (try-path)
             (if (remove-path try-path nil)
               (if (> lev 0)
                 (put-line `cleaned @{try-path}`)))))
      (match-case path
        (@(or `@base.tlo`
              `@base.tlo.gz`)
          (ignore base)
          (try-clean path))
        (@(or `@base.txr`
              `@base.tl`
              `@base`)
          (or (try-clean `@base.tlo`)
              (try-clean `@base.tlo.gz`)))))))

(defun list-from-vm-desc (vd)
  (list (sys:vm-desc-nlevels vd)
        (sys:vm-desc-nregs vd)
        (sys:vm-desc-bytecode vd)
        (copy (sys:vm-desc-datavec vd))
        (sys:vm-desc-symvec vd)))

(defmacro usr:with-compilation-unit (. body)
  (with-gensyms (rec)
    ^(let* ((,rec *in-compilation-unit*)
            (*in-compilation-unit* t)
            (sys:*load-recursive* t)
            (*dedup* (or *dedup* (hash))))
       (unwind-protect
         (progn ,*body)
         (unless ,rec
           (eval-cache-emit-warnings)
           (release-deferred-warnings)
           (compiler-emit-warnings))))))

(defun dump-to-tlo (out-stream out)
  (let* ((*print-circle* t)
         (*print-base* 10)
         (*print-flo-format* "~s")
         (*print-flo-precision* flo-max-dig)
         (*package* (sys:make-anon-package))
         (out-forms (partition* out.(get) (op where (op eq :fence)))))
    (prinl %tlo-ver% out-stream)
    [mapdo (op prinl @1 out-stream) out-forms]
    (delete-package *package*)))

(defun propagate-perms (in-stream out-stream)
  (let ((sti (stat in-stream)))
    (when (plusp (logand sti.mode s-ixusr))
      (let ((mode "+x")
            (suid (if (plusp (logand sti.mode s-isuid)) ",u+s"))
            (sgid (if (and (plusp (logand sti.mode s-isgid))
                           (plusp (logand sti.mode s-ixgrp))) ",g+s")))
        (when (or suid sgid)
          (let ((sto (stat out-stream)))
            (set mode (append mode
                              (if (eql sti.uid sto.uid) suid)
                              (if (eql sti.gid sto.gid) sgid)))))
        (chmod out-stream mode)))))

(defun translate-hash-bang (hbline)
  (flow hbline
    (spl " ")
    (subst "--lisp" "--compiled")
    (mapcar [iffi (op ends-with "txrlisp")
                  (opip (trim-right "lisp") (join @1 "vm"))])
    (ap join-with " ")))

(defun compile-file-conditionally (in-path out-path test-fn)
  (whenlet ((success nil)
            (perms nil)
            (streams (open-compile-streams in-path out-path test-fn)))
    (with-resources ((in-stream (car streams) (close-stream in-stream))
                     (out-stream (cadr streams) (progn
                                                  (when perms
                                                    (propagate-perms in-stream
                                                                     out-stream))
                                                  (close-stream out-stream)
                                                  (unless success
                                                    (remove-path (caddr streams))))))
      (let* ((err-ret (gensym))
             (*package* *package*)
             (*emit* t)
             (*eval* t)
             (*load-path* (stream-get-prop (car streams) :name))
             (*rec-source-loc* t)
             (lev (or *compile-opts*.log-level 0))
             (out (new list-builder)))
        (if (> lev 0)
          (put-line `compiling @{*load-path*}`))
        (with-compilation-unit
          (iflet ((line (get-line in-stream))
                  ((starts-with "#!" line)))
            (let ((cline (translate-hash-bang line)))
              (set perms t)
              (put-line cline out-stream))
            (seek-stream in-stream 0 :from-start))
          (labels ((compile-form (unex-form)
                     (let* ((form (macroexpand unex-form))
                            (sym (if (consp form) (car form))))
                       (when (and sym (> lev 1))
                         (let* ((loc (source-loc form))
                                (line (or (car loc) "unknown")))
                           (if-match @(or @(with (@(symbolp @a) @(symbolp @b) . @nil)
                                                 print-form ^(,a ,b))
                                          @(with (@(symbolp @a) . @nil)
                                                 print-form a)) form
                             (format t "~a: ~a\n" line print-form))))
                       (caseq sym
                         (progn [mapdo compile-form (cdr form)])
                         (compile-only (let ((*eval* nil))
                                         [mapdo compile-form (cdr form)]))
                         (eval-only (let ((*emit* nil))
                                      [mapdo compile-form (cdr form)]))
                         (sys:load-time-lit
                           (if (cadr form)
                             (compile-form ^(quote ,(caddr form)))
                             (compile-form (caddr form))))
                         (t (when (and (or *eval* *emit*)
                                       (not (constantp form)))
                              (let* ((vm-desc (compile-toplevel form))
                                     (flat-vd (list-from-vm-desc vm-desc))
                                     (symvec (sys:vm-desc-symvec vm-desc))
                                     (fence (isecp symvec %package-manip%)))
                                (when *eval*
                                  (let ((pa *package-alist*))
                                    (sys:vm-execute-toplevel vm-desc)
                                    (when (neq pa *package-alist*)
                                      (set fence t))))
                                (when (and *emit* (consp form))
                                  out.(add flat-vd)
                                  (when fence
                                    out.(add :fence))))))))))
            (unwind-protect
              (whilet ((obj (read in-stream *stderr* err-ret))
                       ((neq obj err-ret)))
                (compile-form obj))
              (dump-to-tlo out-stream out))

            (when (parse-errors in-stream)
              (error "~s: compilation of ~s failed" 'compile-file
                     (stream-get-prop in-stream :name))))
          (flush-stream out-stream)
          (set success t))))))

(defun usr:compile-file (in-path : out-path)
  [compile-file-conditionally in-path out-path tf])

(defun usr:compile-update-file (in-path : out-path)
  (let ((test-newer [mapf path-newer fstat identity]))
    (if (> (or *compile-opts*.log-level 0) 0)
      (set test-newer [orf test-newer
                           (do progn
                             (put-line `skipping up-to-date @2`)
                             nil)]))
    [compile-file-conditionally in-path out-path test-newer]))

(defun usr:dump-compiled-objects (out-stream . compiled-objs)
  (symacrolet ((self 'dump-compiled-objects))
    (let ((out (new list-builder)))
      (flet ((vm-from-fun (fun)
               (unless (vm-fun-p fun)
                 (error "~s: not a vm function: ~s" self fun))
               (sys:vm-closure-desc (func-get-env fun))))
        (each ((obj compiled-objs))
          (let* ((vm-desc (typecase obj
                            (vm-desc obj)
                            (fun (vm-from-fun obj))
                            (t (iflet ((fun (symbol-function obj)))
                                 (vm-from-fun fun)
                                 (error "~s: not a compiled object: ~s"
                                        self obj)))))
                 (symvec (sys:vm-desc-symvec vm-desc)))
            out.(add (list-from-vm-desc vm-desc))
            (when (isecp symvec %package-manip%)
              out.(add :fence)))))
      (dump-to-tlo out-stream out))))

(defun sys:env-to-let (env form)
  (when env
    (let ((vb (env-vbindings env))
          (fb (env-fbindings env))
          (up (env-next env)))
      (when vb
        (set form ^(let ,(mapcar (tb ((a . d)) ^(,a ',d)) vb) ,form)))
      (when fb
        (let (lbind fbind)
          (each ((pair fb))
            (tree-bind (a . d) pair
              (let* ((fun-p (interp-fun-p d))
                     (fe (if fun-p (func-get-env d)))
                     (lb-p (and fe (eq fe env)))
                     (fb-p (and fe (eq fe up))))
                (cond
                  (lb-p (push ^(,a ,(func-get-form d)) lbind))
                  (fb-p (push ^(,a ,(func-get-form d)) fbind))
                  (t (push ^(,a ',d) fbind))))))
          (when lbind
            (set form ^(sys:lbind ,(nreverse lbind) ,form)))
          (when fbind
            (set form ^(sys:fbind ,(nreverse fbind) ,form)))))
      (if up
        (set form (sys:env-to-let up form)))))
  form)

(defun usr:compile (obj)
  (match-case obj
    (@(functionp)
     (tree-bind (t args . body) (func-get-form obj)
       (let* ((form (sys:env-to-let (func-get-env obj)
                                    ^(lambda ,args ,*body)))
              (vm-desc (compile-toplevel form t)))
         (vm-execute-toplevel vm-desc))))
    ((lambda . @nil)
     [(compile-toplevel obj nil)])
    (@(@fun (symbol-function))
      (tree-bind (t args . body) (func-get-form fun)
        (let* ((form (sys:env-to-let (func-get-env fun)
                                     ^(lambda ,args ,*body)))
               (vm-desc (compile-toplevel form t))
               (comp-fun (vm-execute-toplevel vm-desc)))
          (set (symbol-function obj) comp-fun))))
    (@else (error "~s: cannot compile ~s" 'compile else))))

(defmacro usr:with-compile-opts (:form form . clauses)
  (match-case clauses
    (() ())
    (((@(as op @(or nil t :warn :error @(integerp))) . @syms) . @rest)
     (each ((s syms))
       (unless (member s %warning-syms%)
         (compile-error form
                        "~s isn't a recognized warning option" s)))
     ^(compiler-let ((*compile-opts* (let ((co (copy *compile-opts*)))
                                       (set ,*(mappend (ret ^(co.,@1 ,op))
                                                       syms))
                                       co)))
        ,*(if rest ^((with-compile-opts ,*rest)))))
    ((@first . @rest)
     ^(progn ,first ,*(if rest ^((with-compile-opts ,*rest)))))))
