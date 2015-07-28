(defmacro txr-if (name args input : then else)
  (let ((syms (keep-if [andf true symbolp [notf keywordp] [notf (op eq t)]]
                       args))
        (arg-exprs [mapcar [iffi symbolp (ret ^',@1)] args])
        (result (gensym "res-"))
        (bindings (gensym "bindings-"))
        (insym (gensym "input-")))
    ^(let* ((,insym ,input)
            (,result (match-fun ',name (list ,*arg-exprs)
                                (if (stringp ,insym) (list ,insym) ,insym)
                                nil)))
       (if ,result
         (let ((,bindings (car ,result)))
           (let (,*[mapcar (ret ^(,@1 (cdr (assoc ',@1 ,bindings))))
                           syms])
             ,then))
         ,else))))

(defmacro txr-when (name args input . body)
  ^(txr-if ,name ,args ,input (progn ,*body)))

(defmacro txr-case-impl (sym . clauses)
  (tree-case clauses
    (((name args . body) . other-clauses)
     (if (eq name t) :
       ^(txr-if ,name ,args ,sym
          (progn ,*body)
          (txr-case-impl ,sym ,*other-clauses))))
    (((sym . rest) . other-clauses)
     (if (eq sym t)
       (if other-clauses
         (error "txr-case: clauses after (t ...) clause ignored")
         ^(progn ,*rest))
       (error "txr-case: bad syntax: ~s" (car clauses))))
    (() ())
    (atom
       (error "txr-case: unexpected atom in syntax: ~s" atom))))

(defmacro txr-case (input-expr . clauses)
  (let ((input (gensym "input-")))
    ^(let ((,input ,input-expr))
       (txr-case-impl ,input ,*clauses))))
