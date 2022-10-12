;; Copyright 2015-2022
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

(defvar *struct-clause-expander* (hash))

(defun sys:bad-slot-syntax (form arg)
  (compile-error form "bad slot syntax ~s" arg))

(defun sys:prune-missing-inits (slot-init-forms)
  (remove-if (tb ((kind name : (init-form nil init-form-present)))
               (and (member kind '(:static :instance :function))
                    (not init-form-present)))
             slot-init-forms))

(defmacro sys:meth-lambda (struct slot params . body)
  ^(symacrolet ((%fun% '(,struct ,slot)))
     (lambda ,params ,*body)))

(defmacro defstruct (:form form name-spec super-spec . slot-specs)
  (tree-bind (name args) (tree-case name-spec
                           ((atom . args) (list atom args))
                           (atom (list atom nil)))
    (unless (bindable name)
      (compile-error form "~s isn't a bindable symbol" name))
    (if (built-in-type-p name)
      (compile-warning form "~s is a built-in type" name))
    (unless (proper-listp slot-specs)
      (compile-error form "bad syntax: dotted form"))
    (let ((instance-init-forms nil)
          (instance-postinit-forms nil)
          (instance-fini-forms nil)
          (instance-postfini-forms nil))
      (labels ((expand-slot (form slot)
                 (tree-case slot
                   ((op . args)
                    (iflet ((expander [*struct-clause-expander* op]))
                      (append-each ((exslot [expander slot form]))
                        [expand-slot form exslot])
                      :))
                   ((word slname args . body)
                    (caseq word
                      (:method
                        (when (not args)
                          (compile-error form "method ~s needs at least one parameter" slname))
                        ^((:function ,slname
                            (sys:meth-lambda ,slname ,name ,args
                              (block ,slname ,*body)))))
                      (:function ^((,word ,slname
                                          (sys:meth-lambda ,slname ,name ,args
                                            (block ,slname
                                              ,*body)))))
                      ((:static :instance)
                       (when body
                         (sys:bad-slot-syntax form slot))
                       ^((,word ,slname ,args)))
                      (t :)))
                   ((word (arg) . body)
                    (caseq word
                      (:init
                        (unless (bindable arg)
                          (sys:bad-slot-syntax form slot))
                        (if body
                          (push (cons arg body) instance-init-forms))
                        ^((,word nil nil)))
                      (:postinit
                        (unless (bindable arg)
                          (sys:bad-slot-syntax form slot))
                        (if body
                          (push (cons arg body) instance-postinit-forms))
                        ^((,word nil nil)))
                      (:fini
                        (unless (bindable arg)
                          (sys:bad-slot-syntax form slot))
                        (if body
                          (push (cons arg body) instance-fini-forms))
                        ^((,word nil nil)))
                      (:postfini
                        (unless (bindable arg)
                          (sys:bad-slot-syntax form slot))
                        (if body
                          (push (cons arg body) instance-postfini-forms))
                        ^((,word nil nil)))
                      (t (when body
                           (sys:bad-slot-syntax form slot))
                         :)))
                   ((word name)
                    (caseq word
                      ((:static)
                       ^((,word ,name)))
                      ((:instance)
                       ^((,word ,name nil)))
                      ((:method :function)
                       (sys:bad-slot-syntax form slot))
                      (t ^((:instance ,word ,name)))))
                   ((name)
                    ^((:instance ,name nil)))
                   (name
                     ^((:instance ,name nil))))))
        (let* ((slot-init-forms (append-each ((slot slot-specs))
                                  (expand-slot form slot)))
               (supers (if (and super-spec (atom super-spec))
                         (list super-spec)
                         super-spec))
               (stat-si-forms [keep-if (op member @1 '(:static :function))
                                       slot-init-forms car])
               (pruned-si-forms (sys:prune-missing-inits stat-si-forms))
               (func-si-forms [keep-if (op eq :function) pruned-si-forms car])
               (val-si-forms [keep-if (op eq :static) pruned-si-forms car])
               (inst-si-forms [keep-if (op eq :instance) slot-init-forms car])
               (stat-slots [mapcar second stat-si-forms])
               (inst-slots [mapcar second inst-si-forms]))
          (whenlet ((bad [find-if [notf bindable]
                                  (append stat-slots inst-slots)]))
            (compile-error form
                           (if (symbolp bad)
                             "slot name ~s isn't a bindable symbol"
                             "invalid slot specifier syntax: ~s")
                           bad))
          (each ((s supers))
            (or (find-struct-type s)
                (compile-defr-warning form ^(struct-type . ,s)
                                      "inheritance base ~s \
                                      \ does not name a struct type"
                                      s)))
          (let ((arg-sym (gensym))
                (type-sym (gensym)))
            (register-tentative-def ^(struct-type . ,name))
            (each ((s stat-slots))
              (register-tentative-def ^(slot . ,s)))
            (each ((s inst-slots))
              (register-tentative-def ^(slot . ,s)))
            ^(sys:make-struct-type
               ',name ',supers ',stat-slots ',inst-slots
               ,(if (or func-si-forms val-si-forms)
                   ^(lambda (,arg-sym)
                      ,*(mapcar (aret ^(when (static-slot-p ,arg-sym ',@2)
                                         (static-slot-set ,arg-sym ',@2 ,@3)))
                                (append func-si-forms val-si-forms))))
               ,(if (or inst-si-forms instance-init-forms
                        instance-fini-forms instance-postfini-forms)
                  ^(lambda (,arg-sym)
                     ,*(if instance-fini-forms
                         ^((finalize ,arg-sym
                                     (sys:meth-lambda ,name :fini (,arg-sym)
                                       ,*(append-each ((iff instance-fini-forms))
                                           ^((let ((,(car iff) ,arg-sym))
                                               ,*(cdr iff)))))
                                     t)))
                     ,*(if instance-postfini-forms
                         ^((finalize ,arg-sym
                                     (sys:meth-lambda ,name :postfini (,arg-sym)
                                       ,*(append-each ((iff (nreverse instance-postfini-forms)))
                                           ^((let ((,(car iff) ,arg-sym))
                                               ,*(cdr iff))))))))
                     ,*(if inst-si-forms
                         ^((let ((,type-sym (struct-type ,arg-sym)))
                             ,*(mapcar (aret ^(unless (static-slot-p ,type-sym ',@2)
                                                (slotset ,arg-sym ',@2 ,@3)))
                                       inst-si-forms))))
                     ,*(append-each ((iif (nreverse instance-init-forms)))
                         ^((symacrolet ((%fun% '(,name :init)))
                             (let ((,(car iif) ,arg-sym))
                               ,*(cdr iif)))))))
               ,(when args
                  (when (> (countql : args) 1)
                    (compile-error form
                                   "multiple colons in boa syntax"))
                  (let ((col-pos (posq : args)))
                    (let ((req-args [args 0..col-pos])
                          (opt-args (if col-pos [args (succ col-pos)..:])))
                      (let ((r-gens (mapcar (ret (gensym)) req-args))
                            (o-gens (mapcar (ret (gensym)) opt-args))
                            (p-gens (mapcar (ret (gensym)) opt-args)))
                        ^(lambda (,arg-sym ,*r-gens
                                  ,*(if opt-args '(:))
                                  ,*(if opt-args
                                      (mapcar (ret ^(,@1 nil ,@2))
                                              o-gens p-gens)))
                           ,*(mapcar (ret ^(slotset ,arg-sym ',@1 ,@2))
                                     req-args r-gens)
                           ,*(mapcar (ret ^(if ,@3
                                             (slotset ,arg-sym ',@1 ,@2)))
                                     opt-args o-gens p-gens))))))
               ,(if instance-postinit-forms
                  ^(sys:meth-lambda ,name :postinit (,arg-sym)
                     ,*(append-each ((ipf (nreverse instance-postinit-forms)))
                         ^((let ((,(car ipf) ,arg-sym))
                             ,*(cdr ipf)))))))))))))

(defmacro sys:struct-lit (name . plist)
  ^(sys:make-struct-lit ',name ',plist))

(defun sys:check-slot (form slot)
  (unless (or (sys:slot-types slot)
              (sys:static-slot-types slot))
    (compile-defr-warning form ^(slot . ,slot)
                          "symbol ~s isn't the name of a struct slot"
                          slot))
  slot)

(defun sys:check-struct (form stype)
  (unless (find-struct-type stype)
    (compile-defr-warning form ^(struct-type . ,stype)
                          "~s does not name a struct type"
                          stype)))

(defmacro qref (:form form obj . refs)
  (when (null refs)
    (throwf 'eval-error "~s: bad syntax" 'qref))
  (tree-case obj
    ((a b) (if (eq a 't)
             (let ((s (gensym)))
               ^(slet ((,s ,b))
                  (if ,s (qref ,s ,*refs))))
             :))
    (x (tree-case refs
         (() ())
         (((pref sym) . more)
           (if (eq pref t)
             (let ((s (gensym)))
               ^(let ((,s (qref ,obj ,sym)))
                  (if ,s (qref ,s ,*more))))
             :))
         (((dw sym . args))
          (if (eq dw 'dwim)
            (let ((osym (gensym)))
              (sys:check-slot form sym)
              ^(slet ((,osym ,obj))
                 ,(if (and (plusp sys:compat) (<= sys:compat 251))
                    ^[(slot ,osym ',sym) ,*args]
                    ^[(slot ,osym ',sym) ,osym ,*args])))
            :))
         (((dw sym . args) . more)
          (if (eq dw 'dwim)
            (let ((osym (gensym)))
              (sys:check-slot form sym)
              ^(qref (slet ((,osym ,obj))
                       ,(if (and (plusp sys:compat) (<= sys:compat 251))
                          ^[(slot ,osym ',sym) ,*args]
                          ^[(slot ,osym ',sym) ,osym ,*args])) ,*more))
            :))
         (((sym . args))
          (let ((osym (gensym)))
            (sys:check-slot form sym)
            ^(slet ((,osym ,obj))
               (call (slot ,osym ',sym) ,osym ,*args))))
         (((sym . args) . more)
          (let ((osym (gensym)))
            (sys:check-slot form sym)
            ^(qref (slet ((,osym ,obj))
                     (call (slot ,osym ',sym) ,osym ,*args)) ,*more)))
         ((sym)
          (sys:check-slot form sym)
          ^(slot ,obj ',sym))
         ((sym . more)
          (sys:check-slot form sym)
          ^(qref (slot ,obj ',sym) ,*more))
         (obj (throwf 'eval-error "~s: bad syntax: ~s" 'qref refs))))))

(defmacro uref (. args)
  (cond
    ((null args) (throwf 'eval-error "~s: bad syntax" 'uref))
    ((null (cdr args))
     (if (consp (car args))
       ^(umeth ,*(car args))
       ^(usl ,(car args))))
    ((eq t (car args))
     (with-gensyms (ovar)
       ^(lambda (,ovar) (qref (t ,ovar) ,*(cdr args)))))
    (t (with-gensyms (ovar)
         ^(lambda (,ovar) (qref ,ovar ,*args))))))

(defun sys:new-type (op form type)
  (caseq op
    ((new lnew) (sys:check-struct form type) ^',type)
    (t type)))

(defun sys:new-expander (op form spec pairs)
  (when (oddp (length pairs))
    (compile-error form "slot initform arguments must occur pairwise"))
  (let ((qpairs (mappend (aret ^(',@1 ,@2)) (tuples 2 pairs))))
    (tree-case spec
      ((texpr . args)
       (if (and (eq texpr 'dwim)
                (meq op 'new* 'lnew*))
         :
         (let ((type (sys:new-type op form texpr)))
           (caseq op
             ((new new*) (if qpairs
                           ^(make-struct ,type (list ,*qpairs) ,*args)
                           ^(struct-from-args ,type ,*args)))
             ((lnew lnew*) ^(make-lazy-struct ,type
                                              (lambda ()
                                                (cons (list ,*qpairs)
                                                      (list ,*args)))))))))
      (texpr
        (let ((type (sys:new-type op form texpr)))
          (caseq op
            ((new new*) ^(struct-from-plist ,type ,*qpairs))
            ((lnew lnew*) ^(make-lazy-struct ,type
                                             (lambda ()
                                               (list (list ,*qpairs)))))))))))

(defmacro new (:form form spec . pairs)
  (sys:new-expander (car form) form spec pairs))

(defmacro new* (:form form spec . pairs)
  (sys:new-expander (car form) form spec pairs))

(defmacro lnew (:form form spec . pairs)
  (sys:new-expander (car form) form spec pairs))

(defmacro lnew* (:form form spec . pairs)
  (sys:new-expander (car form) form spec pairs))

(defmacro meth (obj slot . bound-args)
  ^[(fun method) ,obj ',slot ,*bound-args])

(defmacro usl (:form form slot)
  (sys:check-slot form slot)
  ^(uslot ',slot))

(defmacro umeth (:form form slot . bound-args)
  (sys:check-slot form slot)
  ^[(fun umethod) ',slot ,*bound-args])

(defun sys:define-method (type-sym name fun)
  (caseq name
    (:init (struct-set-initfun type-sym fun))
    (:postinit (struct-set-postinitfun type-sym fun))
    (t (static-slot-ensure type-sym name fun)))
  ^(meth ,type-sym ,name))

(defmacro defmeth (:form form type-sym name arglist . body)
  (cond
    ((not (bindable type-sym))
      (compile-error form "~s isn't a valid struct name" type-sym))
    ((not (find-struct-type type-sym))
      (compile-defr-warning form ^(struct-type . ,type-sym)
                            "definition of struct ~s not seen here" type-sym)))
  (register-tentative-def ^(slot . ,name))
  ^(sys:define-method ',type-sym ',name (sys:meth-lambda ,type-sym ,name
                                                         ,arglist
                                          (block ,name ,*body))))

(defmacro with-slots ((. slot-specs) obj-expr . body)
  (with-gensyms (obj-sym)
    ^(let ((,obj-sym ,obj-expr))
       (symacrolet (,*(mapcar [iff consp
                                   (aret ^(,@1 (slot ,obj-sym ',@2)))
                                   (ret ^(,@1 (slot ,obj-sym ',@1)))]
                              slot-specs))
         ,*body))))

(defun sys:rslotset (struct sym meth-sym val)
  (prog1
    (slotset struct sym val)
    (call (umethod meth-sym) struct)))

(defmacro usr:rslot (struct sym meth-sym)
  ^(slot ,struct ,sym))

(define-place-macro usr:rslot (struct sym meth-sym)
  ^(sys:rslot ,struct ,sym ,meth-sym))

(defplace (sys:rslot struct sym meth-sym) body
  (getter setter
    (with-gensyms (struct-sym slot-sym meth-slot-sym)
      ^(slet ((,struct-sym ,struct)
              (,slot-sym ,sym)
              (,meth-slot-sym ,meth-sym))
         (macrolet ((,getter () ^(slot ,',struct-sym ,',slot-sym))
                    (,setter (val) ^(sys:rslotset ,',struct-sym ,',slot-sym
                                                  ,',meth-slot-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(progn
                                   (sys:rslotset ,',struct ,',sym
                                             ,',meth-sym ,val))))
       ,body)))

(defmacro define-struct-clause (:form form keyword (. params) . body)
  (if (meq keyword :static :instance :function :method :init :postinit :fini :postfini)
    (compile-error form "~s is a reserved defstruct clause keyword" keyword))
  (unless (keywordp keyword)
    (compile-error form "~s: clauses must be named by keyword symbols" keyword))
  (with-gensyms (slot form)
    ^(progn
       (set [*struct-clause-expander* ,keyword]
          (lambda (,slot ,form)
            (mac-param-bind ,form ,params (cdr ,slot) ,*body)))
       ,keyword)))

(compile-only
  (load-for (struct sys:param-parser-base "param")))

(define-struct-clause :delegate (:form form
                                 meth-name params delegate-expr
                                 : (target-method meth-name))
  (unless params
    (compile-error form "delegate method requires at least one argument"))
  (let* ((obj (car params))
         (pp (new (fun-param-parser (cdr params) form)))
         (opt pp.(opt-syms))
         (args (append pp.req opt pp.rest)))
    ^((:method ,meth-name
        (,obj ,*pp.req
              ,*(if opt
                  (cons : (collect-each ((o pp.opt))
                            (tree-case o
                              ((sym) ^(,sym :))
                              ((sym init) o)
                              ((sym init sym-p)
                               (compile-error form
                                              "~s: three-element optional \ \
                                              parameter ~s not supported"
                                              o))))))
              ,*pp.rest)
        (qref ,delegate-expr (,target-method ,*args))))))

(define-struct-clause :mass-delegate (:form form self-var delegate-expr
                                      from-struct . methods)
  (let ((from-type (find-struct-type from-struct)))
    (flet ((is-meth (slot)
             (and (static-slot-p from-type slot)
                  (let ((f (static-slot from-type slot)))
                    (and (functionp f)
                         (plusp (fun-fixparam-count f)))))))
      (unless from-type
        (compile-error form "~s doesn't name a struct type" from-struct))
      (if (starts-with '(*) methods)
        (set methods
             (diff [keep-if is-meth (slots from-type)]
                   (cdr methods)))
        (iflet ((badmeth [remove-if is-meth methods]))
          (compile-error form "~s aren't methods of type ~s" badmeth from-struct)))
      (collect-each ((m methods))
        (let* ((f (static-slot from-type m))
               (fix (fun-fixparam-count f))
               (opt (fun-optparam-count f))
               (var (fun-variadic f))
               (parms ^(,*(take (- fix opt) (cons self-var (gun (gensym))))
                        ,*(if (plusp opt)
                            ^(: ,*(take opt (gun (gensym)))))
                        ,*(if var (gensym)))))
          ^(:delegate ,m ,parms ,delegate-expr))))))
