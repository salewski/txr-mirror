(load "../common")

(defun fi (fun)
  (assert (zerop (fun-optparam-count fun)))
  (list (fun-fixparam-count fun)
        (fun-variadic fun)))

(mtest
  (fi (op)) :error
  (fi (op list)) (0 t)
  (fi (op list @1)) (1 t)
  (fi (op list @2)) (2 t)
  (fi (op list @42)) (42 t)
  (fi (op list @rest)) (0 t)
  (fi (op list @1 @rest)) (1 t)
  (fi (op list @2 @rest)) (2 t)
  (fi (op list @42 @rest)) (42 t)
  (fi (op list @1 @3 @rest @2)) (3 t))

(mtest
  (fi (do)) :error
  (fi (do progn)) (1 t)
  (fi (do progn @1)) (1 t)
  (fi (do progn @2)) (2 t)
  (fi (do progn @42)) (42 t)
  (fi (do progn @rest)) (0 t)
  (fi (do progn @1 @rest)) (1 t)
  (fi (do progn @2 @rest)) (2 t)
  (fi (do progn @42 @rest)) (42 t)
  (fi (do progn @1 @3 @rest @2)) (3 t))

(mtest
  (fi (do if)) (1 t)
  (fi (do if @1)) (1 t)
  (fi (do if @2)) (2 t)
  (fi (do if @42)) (42 t)
  (fi (do if @rest)) (0 t)
  (fi (do if @1 @rest)) (1 t)
  (fi (do if @2 @rest)) (2 t)
  (fi (do if @42 @rest)) (42 t)
  (fi (do if @1 @3 @rest @2)) (3 t))

(mtest
  [(do quote x) 3] :error
  [(do quote @1) 3] :error
  (do pop a) :error)

(defun y (f)
  [(op @1 @1)
   (op f (op [@@1 @@1]))])

(defun fac (f)
  (do if (zerop @1)
         1
         (* @1 [f (- @1 1)])))

(defun fac-y (n)
  [(y (do do if (zerop @1)
               1
               (* @1 [@@1 (- @1 1)]))) n])

(defun fac-r (n)
  [(do if (zerop @1) 1 (* @1 [@rec (pred @1)])) n])

(defun fac-r2 (n)
  [(do if (zerop @1) 1 (* @1 @(rec (pred @1)))) n])

(mtest
  [[y fac] 4] 24
  (fac-y 4) 24
  (fac-r 4) 24
  (fac-r2 4) 24)

(mtest
  (flow 1 (+ 2) [dup *] (let ((x @1)) x)) 9
  (flow #S(time year 2021) .year succ) 2022)

(mtest
  [[(do op list)] 2] :error
  [[(do op list) 2]] (2)
  [[(do op list @@1) 1] 2] (1 2)
  [[(do op list @1)] 2] :error
  [[(do op list @1) 1] 2] (2 1)
  [[(do op list @@1 @1) 1] 2] (1 2))

(mtest
  [[[[(do do do op list @1) 1] 2] 3] 4] (4 1 2 3)
  [[[[(do do do op list @@1) 1] 2] 3] 4] (3 1 2 4)
  [[[[(do do do op list @@@1) 1] 2] 3] 4] (2 1 3 4)
  [[[[(do do do op list @@@@1) 1] 2] 3] 4] (1 2 3 4))

(mtest
  [[[[(do do do op list) 1] 2] 3] 4] (1 2 3 4)
  [[[[(do do do op list @1 @@1 @@@1 @@@@1) 1] 2] 3] 4] (4 3 2 1)
  [[[[(do do do op list @@@@1 @@@1 @@1 @1) 1] 2] 3] 4] (1 2 3 4))

(test (flow (range 1000 9999)
        (keep-if (opip digits (ap > (+ @2 @3) (+ @1 @4))))
        (partition-if (op neq 1 (- @2 @1)))
        (find-max-key @1 : len))
      80)

(mtest
  (flow 1 (+ 1) (let x) (+ 2) (let y) (+ 3) (list x y @1)) (2 4 7)
  (flow 10 (+ 1) (let (x @1) (y (* x 2))) (+ x y)) 44
  (flow 10 (+ 1) (let ((x @1) (y (* @1 2))) (+ x y))) 33
  (flow 10 (+ 1) (let ((x @1) (y (* @1 2))))) nil)

(mtest
  (lflow 10 (- 1) (- 1)) 8
  (lflow 10 (op - 100) (+ 1)) 91)

(mtest
  (remove-if (opf orf (< 10) (> 5)) (range 0 20)) (5 6 7 8 9 10)
  (remove-if (lopf orf (> 10) (< 5)) (range 0 20)) (5 6 7 8 9 10))
