(load "../common")

(defvar a 42)
(defvar b 73)

(mtest
  (progv '(a) '(1) a) 1
  (progv '(a b) '(1 2) (cons a b)) (1 . 2)
  (progv '(x) '(1) (let ((x 4)) (symbol-value 'x))) 1)

(let ((n (list 'a 'b))
      (v (list 1 2)))
  (mtest
    (progv n v (cons a b)) (1 . 2)))

(defvarl x)

(let ((x 'lexical)
     (vars (list 'x))
     (vals (list 'dynamic)))
  (test
    (progv vars vals (list x (symbol-value 'x)))
    (lexical dynamic)))

(compile-only
  (eval-only
    (with-compile-opts (nil unused)
      (compile-file (base-name *load-path*) "temp.tlo"))
    (remove-path "temp.tlo")))
