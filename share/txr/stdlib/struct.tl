;; Copyright 2015
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
                 (and (member kind '(:static :instance))
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
    (let* ((instance-init-form nil) (instance-fini-form nil)
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
                                     ^(:static ,name
                                               (lambda ,args
                                                 (block ,name ,*body))))
                                   (:function ^(:static ,name
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
                                       (uw-throwf 'eval-error
                                                  "~s: duplicate :init"
                                                  'defstruct))
                                     (set instance-init-form
                                          (cons arg body))
                                     ^(,word nil nil))
                                   (:fini
                                     (unless (bindable arg)
                                       (sys:bad-slot-syntax slot))
                                     (when instance-fini-form
                                       (uw-throwf 'eval-error
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
           (stat-si-forms [keep-if (op eq :static) slot-init-forms car])
           (inst-si-forms [keep-if (op eq :instance) slot-init-forms car])
           (stat-slots [mapcar second stat-si-forms])
           (inst-slots [mapcar second inst-si-forms])
           (super-type (find-struct-type super)))
      (whenlet ((bad [find-if [notf bindable]
                              (append stat-slots inst-slots)]))
        (throwf 'eval-error "~s: slot name ~s isn't a bindable symbol"
                'defstruct bad))
      (set stat-si-forms (sys:prune-nil-inits stat-si-forms super-type))
      (set inst-si-forms (sys:prune-nil-inits inst-si-forms super-type))
      (let ((arg-sym (gensym))
            (type-sym (gensym)))
        ^(sys:make-struct-type
           ',name ',super ',stat-slots ',inst-slots
           ,(if stat-si-forms
               ^(lambda (,arg-sym)
                  ,*(mapcar (aret ^(when (static-slot-p ,arg-sym ',@2)
                                     (static-slot-set ,arg-sym ',@2 ,@3)))
                            stat-si-forms)))
           ,(if (or inst-si-forms instance-init-form instance-fini-form)
               ^(lambda (,arg-sym)
                  ,*(if inst-si-forms
                      ^((let ((,type-sym (struct-type ,arg-sym)))
                          ,*(mapcar (aret ^(unless (static-slot-p ,type-sym ',@2)
                                             (slotset ,arg-sym ',@2 ,@3)))
                                    inst-si-forms))))
                  ,*(if (cdr instance-init-form)
                      ^((let ((,(car instance-init-form) ,arg-sym))
                          ,*(cdr instance-init-form))))
                  ,*(if (cdr instance-fini-form)
                      ^((finalize ,arg-sym (lambda (,(car instance-fini-form))
                                             ,*(cdr instance-fini-form))
                                  t)))))
           ,(if args
              (let ((gens (mapcar (ret (gensym)) args)))
                ^(lambda (,arg-sym ,*gens)
                   ,*(mapcar (ret ^(slotset ,arg-sym ',@1 ,@2))
                             args gens)))))))))

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

(defmacro meth (obj slot)
  ^(method ,obj ',slot))

(defmacro umeth (slot)
  ^(umethod ',slot))
