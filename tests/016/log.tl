(load "../common.tl")

(mtest
  (each-true ()) t
  (each-true ((a ()))) t
  (each-true ((a ())) nil) t
  (each-true ((a '(1 2 3))) a) 3
  (each-true ((a '(nil 2 3))) a) nil
  (each-true ((a '(1 2 3)) (b '(4 5 6))) (< a b)) t
  (each-true ((a '(1 2 3)) (b '(4 0 6))) (< a b)) nil)

(mtest
  (some-true ()) :error
  (some-true ((a ()))) nil
  (some-true ((a ())) nil) nil
  (some-true ((a '(1 2 3))) a) 1
  (some-true ((a '(nil 2 3))) a) 2
  (some-true ((a '(nil nil nil))) a) nil
  (some-true ((a '(1 2 3)) (b '(4 5 6))) (< a b)) t
  (some-true ((a '(1 2 3)) (b '(4 0 6))) (< a b)) t
  (some-true ((a '(1 2 3)) (b '(0 1 2))) (< a b)) nil)

(mtest
  (each-false ()) :error
  (each-false ((a ()))) t
  (each-false ((a ())) t) t
  (each-false ((a '(1 2 3))) a) nil
  (each-false ((a '(nil))) a) t
  (each-false ((a '(nil nil))) a) t
  (each-false ((a '(1 2 3)) (b '(4 5 6))) (> a b)) t
  (each-false ((a '(1 2 3)) (b '(4 0 6))) (> a b)) nil)

(mtest
  (some-false ()) :error
  (some-false ((a ()))) nil
  (some-false ((a ())) nil) nil
  (some-false ((a '(1 2 3))) a) nil
  (some-false ((a '(nil 2 3))) a) t
  (some-false ((a '(nil nil nil))) a) t
  (some-false ((a '(1 2 3)) (b '(4 5 6))) (> a b)) t
  (some-false ((a '(1 2 3)) (b '(4 0 6))) (> a b)) t
  (some-false ((a '(1 2 3)) (b '(0 1 2))) (> a b)) nil)
