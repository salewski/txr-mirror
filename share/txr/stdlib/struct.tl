;; Copyright 2015-2016
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

(macro-time
  (defun sys:bad-slot-syntax (arg)
    (throwf 'eval-error "~s: bad slot syntax: ~s" 'defstruct arg))

  (defun sys:prune-nil-inits (slot-init-forms super-type)
    (remove-if (tb ((kind name init-form))
                 (and (member kind '(:static :instance :function))
                      (null init-form)
                      (or (not super-type)
                          (not (slotp super-type name)))))
               slot-init-forms)))

(defmacro defstruct (name-spec super . slot-specs)
  (tree-bind (name args) (tree-case name-spec
                           ((atom . args) (list atom args))
                           (atom (list atom nil)))
    (unless (bindable name)
      (throwf 'eval-error "~s: ~s isn't a bindable symbol" 'defstruct name))
    (unless (proper-listp slot-specs)
      (throwf 'eval-error "~s: bad slot syntax" 'defstruct))
    (let* ((instance-init-form nil)
           (instance-postinit-form nil)
           (instance-fini-form nil)
           (slot-init-forms (collect-each ((slot slot-specs))
                              (tree-case slot
                                ((word name args . body)
                                 (caseq word
                                   (:method
                                     (when (not args)
                                       (throwf 'eval-error
                                               "~s: method ~s needs \
                                               \ at least one parameter"
                                               'defstruct name))
                                     ^(:function ,name
                                        (lambda ,args
                                              (block ,name ,*body))))
                                   (:function ^(,word ,name
                                                 (lambda ,args
                                                   (block ,name
                                                     ,*body))))
                                   ((:static :instance)
                                     (when body
                                       (sys:bad-slot-syntax slot))
                                     ^(,word ,name ,args))
                                   (t :)))
                                ((word (arg) . body)
                                 (caseq word
                                   (:init
                                     (unless (bindable arg)
                                       (sys:bad-slot-syntax slot))
                                     (when instance-init-form
                                       (throwf 'eval-error
                                               "~s: duplicate :init"
                                               'defstruct))
                                     (set instance-init-form
                                          (cons arg body))
                                     ^(,word nil nil))
                                   (:postinit
                                     (unless (bindable arg)
                                       (sys:bad-slot-syntax slot))
                                     (when instance-postinit-form
                                       (throwf 'eval-error
                                               "~s: duplicate :postinit"
                                               'defstruct))
                                     (set instance-postinit-form
                                          (cons arg body))
                                     ^(,word nil nil))
                                   (:fini
                                     (unless (bindable arg)
                                       (sys:bad-slot-syntax slot))
                                     (when instance-fini-form
                                       (throwf 'eval-error
                                               "~s: duplicate :fini"
                                               'defstruct))
                                     (set instance-fini-form
                                          (cons arg body))
                                     ^(,word nil nil))
                                   (t (when body
                                        (sys:bad-slot-syntax slot))
                                      :)))
                                ((word name)
                                   (caseq word
                                     ((:static :instance)
                                      ^(,word ,name nil))
                                     (t ^(:instance ,word ,name))))
                                ((name)
                                  ^(:instance ,name nil))
                                (name
                                  ^(:instance ,name nil)))))
           (super-type (if super
                         (or (find-struct-type super)
                             (throwf 'eval-error
                                     "~a: inheritance base ~s \
                                     \ does not name a struct type"
                                     'defstruct super))))
           (stat-si-forms [keep-if (op member @1 '(:static :function))
                                   slot-init-forms car])
           (pruned-si-forms (sys:prune-nil-inits stat-si-forms super-type))
           (func-si-forms [keep-if (op eq :function) pruned-si-forms car])
           (val-si-forms [keep-if (op eq :static) pruned-si-forms car])
           (inst-si-forms [keep-if (op eq :instance) slot-init-forms car])
           (stat-slots [mapcar second stat-si-forms])
           (inst-slots [mapcar second inst-si-forms]))
      (whenlet ((bad [find-if [notf bindable]
                              (append stat-slots inst-slots)]))
        (throwf 'eval-error
                (if (symbolp bad)
                  "~s: slot name ~s isn't a bindable symbol"
                  "~s: invalid slot specifier syntax: ~s")
                'defstruct bad))
      (let ((arg-sym (gensym))
            (type-sym (gensym)))
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
                (throwf 'eval-error "~s: multiple colons in boa syntax"
                        'defstruct))
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
  ^(make-struct ',name ',plist))

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
       ^(let ((,osym ,obj))
          (call (slot ,osym ',sym) ,osym ,*args))))
    (((sym . args) . more)
     (let ((osym (gensym)))
       ^(qref (let ((,osym ,obj))
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

(defmacro meth (obj slot)
  ^(method ,obj ',slot))

(defmacro usl (slot)
  ^(uslot ',slot))

(defmacro umeth (slot)
  ^(umethod ',slot))

(defun sys:defmeth (type-sym name fun)
  (let ((type (find-struct-type type-sym)))
    (unless type
      (throwf 'eval-error "~s: ~s isn't a struct type" 'defmeth type-sym))
    (static-slot-ensure type-sym name fun)
    ^(meth ,type-sym ,name)))

(defmacro defmeth (type-sym name arglist . body)
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
