(load "../common")

(defun call-lambda (fn . args)
  [fn . args])

(defmacro ltest (:match :form f)
  (([(lambda . @rest) . @args] @expected)
   (if *compile-test*
     ^(progn
        (test [(lambda ,*rest) ,*args] ,expected)
        (test (call-lambda (lambda ,*rest) ,*args) ,expected))
     ^(test [(lambda ,*rest) ,*args] ,expected)))
  ((@else . rest) (compile-error f "bad syntax")))

(defmacro mltest (. pairs)
  ^(progn ,*(mapcar (op cons 'ltest) (tuples 2 pairs))))

(mltest
  [(lambda ())] nil
  [(lambda (a) a)] :error
  [(lambda (a) a) 1] 1
  [(lambda (a b) (list a b)) 1] :error
  [(lambda (a b) (list a b)) 1 2] (1 2)
  [(lambda (a b c) (list a b c)) 1 2] :error
  [(lambda (a b c) (list a b c)) 1 2 3] (1 2 3))

(mltest
  [(lambda (: a) a)] nil
  [(lambda (: (a 1)) a)] 1
  [(lambda (: (a 1)) a) 2] 2
  [(lambda (: (a 1 a-p)) (list a a-p))] (1 nil)
  [(lambda (: (a 1 a-p)) (list a a-p)) 2] (2 t))

(mltest
  [(lambda (x : a) (list x a))] :error
  [(lambda (x : (a 1)) (list x a))] :error
  [(lambda (x : (a 1)) (list x a)) 2] (2 1)
  [(lambda (x : (a 1 a-p)) (list x a a-p))] :error
  [(lambda (x : (a 1 a-p)) (list x a a-p)) 2] (2 1 nil))

(mltest
  [(lambda (x : a) (list x a)) 0] (0 nil)
  [(lambda (x : (a 1)) (list x a)) 0] (0 1)
  [(lambda (x : (a 1)) (list x a)) 0 2] (0 2)
  [(lambda (x : (a 1 a-p)) (list x a a-p)) 0] (0 1 nil)
  [(lambda (x : (a 1 a-p)) (list x a a-p)) 0 2] (0 2 t))

(mltest
  [(lambda (x y : (a 3) (b 4) . r) (list x y a b r))] :error
  [(lambda (x y : (a 3) (b 4) . r) (list x y a b r)) 1] :error
  [(lambda (x y : (a 3) (b 4) . r) (list x y a b r)) 1 2] (1 2 3 4 nil)
  [(lambda (x y : (a 3) (b 4) . r) (list x y a b r)) 1 2 8] (1 2 8 4 nil)
  [(lambda (x y : (a 3) (b 4) . r) (list x y a b r)) 1 2 8 9] (1 2 8 9 nil)
  [(lambda (x y : (a 3) (b 4) . r) (list x y a b r)) 1 2 8 9 0] (1 2 8 9 (0)))

(defvarl vs '(a))

(mltest
  [(lambda (x y : (a 3) (b 4) . r) (list x y a b r)) . vs] :error
  [(lambda (x y : (a 3) (b 4) . r) (list x y a b r)) 1 . vs] (1 a 3 4 nil)
  [(lambda (x y : (a 3) (b 4) . r) (list x y a b r)) 1 2 . vs] (1 2 a 4 nil)
  [(lambda (x y : (a 3) (b 4) . r) (list x y a b r)) 1 2 3 . vs] (1 2 3 a nil)
  [(lambda (x y : (a 3) (b 4) . r) (list x y a b r)) 1 2 3 4 . vs] (1 2 3 4 (a)))

(mltest
  [(lambda (x y : (a 3) (b 4)) (list x y a b)) . vs] :error
  [(lambda (x y : (a 3) (b 4)) (list x y a b)) 1 . vs] (1 a 3 4)
  [(lambda (x y : (a 3) (b 4)) (list x y a b)) 1 2 . vs] (1 2 a 4)
  [(lambda (x y : (a 3) (b 4)) (list x y a b)) 1 2 3 . vs] (1 2 3 a)
  [(lambda (x y : (a 3) (b 4)) (list x y a b)) 1 2 3 4 . vs] :error)

(test
  [(lambda (x y : (a 3 u) (b 4 v) . r) (list x y a u b v r)) . vs] :error)

(mltest
  [(lambda (x y : (a 3 u) (b 4 v) . r) (list x y a u b v r)) 1 . vs]
  (1 a 3 nil 4 nil nil)
  [(lambda (x y : (a 3 u) (b 4 v) . r) (list x y a u b v r)) 1 2 . vs]
  (1 2 a t 4 nil nil)
  [(lambda (x y : (a 3 u) (b 4 v) . r) (list x y a u b v r)) 1 2 3 . vs]
  (1 2 3 t a t nil)
  [(lambda (x y : (a 3 u) (b 4 v) . r) (list x y a u b v r)) 1 2 3 4 . vs]
  (1 2 3 t 4 t (a))
  [(lambda (x y : (a 3 u) (b 4 v) . r) (list x y a u b v r)) 1 2 3 4 5 . vs]
  (1 2 3 t 4 t (5 a))
  [(lambda (x y : (a 3 u) (b 4 v) . r) (list x y a u b v r)) 1 2 : 4 . vs]
  (1 2 3 nil 4 t (a))
  [(lambda (x y : (a 3 u) (b 4 v) . r) (list x y a u b v r)) 1 2 3 : . vs]
  (1 2 3 t 4 nil (a)))

(defvarl vl '(a b c d))

(mltest
  [(lambda (x y : (a 3) (b 4) . r) (list x y a b r)) . vl] (a b c d nil)
  [(lambda (x y : (a 3) (b 4) . r) (list x y a b r)) 1 . vl] (1 a b c (d))
  [(lambda (x y : (a 3) (b 4) . r) (list x y a b r)) 1 2 . vl] (1 2 a b (c d))
  [(lambda (x y : (a 3) (b 4) . r) (list x y a b r)) 1 2 3 . vl] (1 2 3 a (b c d))
  [(lambda (x y : (a 3) (b 4) . r) (list x y a b r)) 1 2 3 4 . vl] (1 2 3 4 (a b c d))
  [(lambda (x y : (a 3) (b 4) . r) (list x y a b r)) 1 2 3 4 5 . vl] (1 2 3 4 (5 a b c d)))

(mltest
  [(lambda (x y : (a 3) (b 4)) (list x y a b)) . vl] (a b c d)
  [(lambda (x y : (a 3) (b 4)) (list x y a b)) 1 . vl] :error)

(mltest
  [(lambda (x : y) (list x y)) 1 :] (1 nil)
  [(lambda (x : y z) (list x y z)) 1 :] (1 nil nil)
  [(lambda (x : y z) (list x y z)) 1 2 :] (1 2 nil)
  [(lambda (x : y z) (list x y z)) 1 nil :] (1 nil nil)
  [(lambda (x : y z) (list x y z)) 1 nil nil] (1 nil nil))

(mltest
  [(lambda (x : (y nil)) (list x y)) 1 :] (1 nil)
  [(lambda (x : (y nil) (z)) (list x y z)) 1 :] (1 nil nil)
  [(lambda (x : (y nil) (z)) (list x y z)) 1 2 :] (1 2 nil)
  [(lambda (x : (y nil) (z)) (list x y z)) 1 nil :] (1 nil nil)
  [(lambda (x : (y nil) (z)) (list x y z)) 1 nil nil] (1 nil nil))

(defvarl vc '(: : : :))

(mltest
  [(lambda (x y : (a 3) (b 4) . r) (list x y a b r)) . vc] (: : 3 4 nil)
  [(lambda (x y : (a 3) (b 4) . r) (list x y a b r)) 1 . vc] (1 : 3 4 (:))
  [(lambda (x y : (a 3) (b 4) . r) (list x y a b r)) 1 2 . vc] (1 2 3 4 (: :))
  [(lambda (x y : (a 3) (b 4) . r) (list x y a b r)) 1 2 0 . vc] (1 2 0 4 (: : :))
  [(lambda (x y : (a 3) (b 4) . r) (list x y a b r)) 1 2 0 0 . vc] (1 2 0 0 (: : : :))
  [(lambda (x y : (a 3) (b 4) . r) (list x y a b r)) 1 2 0 0 5 . vc] (1 2 0 0 (5 : : : :)))

(test (functionp (lambda (: (n n)) n)) t)

(defvarl n)

(ltest
  [(lambda (: (n n)) n)] nil)

(cond
  (*compile-test* (exit t))
  (t (set *compile-test* t)
     (load (base-name *load-path*))))
