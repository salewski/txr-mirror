(defmacro defstruct (name-spec super . slot-specs)
  (tree-bind (name args) (tree-case name-spec
                           ((atom . args) (list atom args))
                           (atom (list atom nil)))
    (unless (bindable name)
      (throwf 'eval-error "~s: ~s isn't a bindable symbol" 'defstruct name))
    (unless (proper-listp slot-specs)
      (throwf 'eval-error "~s: bad slot syntax" 'defstruct))
    (let ((slot-init-forms (collect-each ((slot slot-specs))
                             (tree-case slot
                               ((sym init-form) slot)
                               (sym (list sym nil))))))
      (whenlet ((bad [find-if [notf bindable] slot-init-forms car]))
        (throwf 'eval-error "~s: slot name ~s isn't a bindable symbol"
                'defstruct (car bad)))
      (let ((arg-sym (gensym)))
        ^(make-struct-type ',name ',super ',[mapcar car slot-init-forms]
           (lambda (,arg-sym)
             ,*(mapcar (aret ^(slotset ,arg-sym ',@1 ,@2)) slot-init-forms))
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
  (let ((qpairs (mappend (aret ^(',@1 ,@2)) (tuples 2 pairs))))
    (tree-case spec
      ((atom . args) ^(make-struct ',atom (list ,*qpairs) ,*args))
      (atom ^(make-struct ',atom (list ,*qpairs))))))

(defmacro meth (obj slot)
  ^(method ,obj ',slot))
