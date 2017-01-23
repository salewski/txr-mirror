;; Copyright 2015-2017
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

(macro-time
  (defun sys:bad-slot-syntax (form arg)
    (compile-error form "bad slot syntax ~s" arg))

  (defun sys:prune-missing-inits (slot-init-forms)
    (remove-if (tb ((kind name : (init-form nil init-form-present)))
                 (and (member kind '(:static :instance :function))
                      (not init-form-present)))
               slot-init-forms)))

(defmacro defstruct (:form form name-spec super . slot-specs)
  (tree-bind (name args) (tree-case name-spec
                           ((atom . args) (list atom args))
                           (atom (list atom nil)))
    (unless (bindable name)
      (compile-error form "~s isn't a bindable symbol" name))
    (unless (proper-listp slot-specs)
      (compile-error form "bad syntax: dotted form"))
    (let* ((instance-init-form nil)
           (instance-postinit-form nil)
           (instance-fini-form nil)
           (slot-init-forms (collect-each ((slot slot-specs))
                              (tree-case slot
                                ((word name args . body)
                                 (caseq word
                                   (:method
                                     (when (not args)
                                       (compile-error form
                                                      "method ~s needs \
                                                      \ at least one parameter"
                                                      name))
                                     ^(:function ,name
                                        (lambda ,args
                                              (block ,name ,*body))))
                                   (:function ^(,word ,name
                                                 (lambda ,args
                                                   (block ,name
                                                     ,*body))))
                                   ((:static :instance)
                                     (when body
                                       (sys:bad-slot-syntax form slot))
                                     ^(,word ,name ,args))
                                   (t :)))
                                ((word (arg) . body)
                                 (caseq word
                                   (:init
                                     (unless (bindable arg)
                                       (sys:bad-slot-syntax form slot))
                                     (when instance-init-form
                                       (compile-error form
                                                      "duplicate :init"))
                                     (set instance-init-form
                                          (cons arg body))
                                     ^(,word nil nil))
                                   (:postinit
                                     (unless (bindable arg)
                                       (sys:bad-slot-syntax form slot))
                                     (when instance-postinit-form
                                       (compile-error form
                                                      "duplicate :postinit"))
                                     (set instance-postinit-form
                                          (cons arg body))
                                     ^(,word nil nil))
                                   (:fini
                                     (unless (bindable arg)
                                       (sys:bad-slot-syntax form slot))
                                     (when instance-fini-form
                                       (compile-error form
                                                      "duplicate :fini"))
                                     (set instance-fini-form
                                          (cons arg body))
                                     ^(,word nil nil))
                                   (t (when body
                                        (sys:bad-slot-syntax form slot))
                                      :)))
                                ((word name)
                                   (caseq word
                                     ((:static)
                                      ^(,word ,name))
                                     ((:instance)
                                      ^(,word ,name nil))
                                     ((:method :function)
                                      (sys:bad-slot-syntax form slot))
                                     (t ^(:instance ,word ,name))))
                                ((name)
                                  ^(:instance ,name nil))
                                (name
                                  ^(:instance ,name nil)))))
           (super-type (if super
                         (or (find-struct-type super)
                             (compile-defr-warning form ^(struct-type . ,super)
                                                   "inheritance base ~s \
                                                   \ does not name a struct type"
                                                   super))))
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
      (let ((arg-sym (gensym))
            (type-sym (gensym)))
        (register-tentative-def ^(struct-type . ,name))
        ^(sys:make-struct-type
           ',name ',super ',stat-slots ',inst-slots
           ,(if (or func-si-forms val-si-forms)
               ^(lambda (,arg-sym)
                  ,*(mapcar (aret ^(when (static-slot-p ,arg-sym ',@2)
                                     (static-slot-set ,arg-sym ',@2 ,@3)))
                            (append func-si-forms val-si-forms))))
           ,(if (or inst-si-forms instance-init-form instance-fini-form)
              ^(lambda (,arg-sym)
                 ,*(if (cdr instance-fini-form)
                     ^((finalize ,arg-sym (lambda (,(car instance-fini-form))
                                            ,*(cdr instance-fini-form))
                                 t)))
                 ,*(if inst-si-forms
                     ^((let ((,type-sym (struct-type ,arg-sym)))
                         ,*(mapcar (aret ^(unless (static-slot-p ,type-sym ',@2)
                                            (slotset ,arg-sym ',@2 ,@3)))
                                   inst-si-forms))))
                 ,*(if (cdr instance-init-form)
                     ^((let ((,(car instance-init-form) ,arg-sym))
                         ,*(cdr instance-init-form))))))
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
           ,(if instance-postinit-form
              ^(lambda (,arg-sym)
                 ,*(if (cdr instance-postinit-form)
                     ^((let ((,(car instance-postinit-form) ,arg-sym))
                         ,*(cdr instance-postinit-form)))))))))))

(defmacro sys:struct-lit (name . plist)
  ^(sys:make-struct-lit ',name ',plist))

(defmacro qref (:whole form obj . refs)
  (when (null refs)
    (throwf 'eval-error "~s: bad syntax" 'qref))
  (tree-case refs
    (() ())
    (((dw sym . args))
     (if (eq dw 'dwim) ^[(slot ,obj ',sym) ,*args] :))
    (((dw sym . args) . more)
     (if (eq dw 'dwim) ^(qref [(slot ,obj ',sym) ,*args] ,*more) :))
    (((sym . args))
     (let ((osym (gensym)))
       ^(slet ((,osym ,obj))
          (call (slot ,osym ',sym) ,osym ,*args))))
    (((sym . args) . more)
     (let ((osym (gensym)))
       ^(qref (slet ((,osym ,obj))
                (call (slot ,osym ',sym) ,osym ,*args)) ,*more)))
    ((sym) ^(slot ,obj ',sym))
    ((sym . more) ^(qref (slot ,obj ',sym) ,*more))
    (obj (throwf 'eval-error "~s: bad syntax: ~s" 'qref refs))))

(defmacro new (spec . pairs)
  (if (oddp (length pairs))
    (throwf 'eval-error "~s: slot initform arguments must occur pairwise"
            'new))
  (let ((qpairs (mappend (aret ^(',@1 ,@2)) (tuples 2 pairs))))
    (tree-case spec
      ((atom . args) ^(make-struct ',atom (list ,*qpairs) ,*args))
      (atom ^(make-struct ',atom (list ,*qpairs))))))

(defmacro lnew (spec . pairs)
  (if (oddp (length pairs))
    (throwf 'eval-error "~s: slot initform arguments must occur pairwise"
            'lnew))
  (let ((qpairs (mappend (aret ^(',@1 ,@2)) (tuples 2 pairs))))
    (tree-case spec
      ((atom . args) ^(make-lazy-struct ',atom
                                        (lambda ()
                                          (cons (list ,*qpairs)
                                                (list ,*args)))))
      (atom ^(make-lazy-struct ',atom (lambda () (list (list ,*qpairs))))))))

(defmacro meth (obj slot . bound-args)
  ^[(fun method) ,obj ',slot ,*bound-args])

(defmacro usl (slot)
  ^(uslot ',slot))

(defmacro umeth (slot . bound-args)
  ^[(fun umethod) ',slot ,*bound-args])

(defun sys:defmeth (type-sym name fun)
  (static-slot-ensure type-sym name fun)
  ^(meth ,type-sym ,name))

(defmacro defmeth (:form form type-sym name arglist . body)
  (cond
    ((not (bindable type-sym))
      (compile-error form "~s isn't a valid struct name" type-sym))
    ((not (find-struct-type type-sym))
      (compile-defr-warning form ^(struct-type . ,type-sym)
                            "definition of struct ~s not seen here" type-sym)))
  ^(sys:defmeth ',type-sym ',name (lambda ,arglist
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

(defmacro rslot (struct sym meth-sym)
  ^(slot ,struct ,sym))

(define-place-macro rslot (struct sym meth-sym)
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
