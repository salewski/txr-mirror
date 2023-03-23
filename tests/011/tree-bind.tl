(load "../common")

(mtest
  (tree-bind b '(1 2) b) (1 2)
  (tree-bind (t b) '(1 2) b) 2
  (tree-bind (t . b) '(1 2) b) (2)
  (tree-bind (b t) '(1 2) b) 1
  (tree-bind (b . t) '(1 2) b) 1
  (tree-bind t '(1 2) 3) 3
  (tree-bind (t : b) '(1 2) b) 2
  (tree-bind (b : t) '(1) b) 1
  (tree-bind (b : (t 2)) '(1) b) 1
  (tree-bind (#:b : (a 2 t)) '(1) a) 2
  (let ((i 0)) (tree-bind (b : (t (inc i) t)) '(1) (cons i b))) (1 . 1)
  (let ((i 0)) (tree-bind (b : (t (inc i) t)) '(1 2) (cons i b))) (0 . 1))

(compile-only
  (eval-only
    (compile-file (base-name *load-path*) "temp.tlo")
    (remove-path "temp.tlo")))
