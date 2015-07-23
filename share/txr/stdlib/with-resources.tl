(defmacro with-resources (res-bindings . body)
  (tree-case res-bindings
    (((var init cleanup) . rest)
     ^(let ((,var ,init))
        (when ,var
          (unwind-protect
            (with-resources ,rest ,*body)
            ,cleanup))))
    (((var init) . rest)
     ^(let ((,var ,init))
        (with-resources ,rest ,*body)))
    ((var . rest)
     ^(let (,var)
        (with-resources ,rest ,*body)))
    (nil
      ^(progn ,*body))
    (other (error "with-resources: bad syntax"))))
