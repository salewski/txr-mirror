(load "../common")

(mtest
  (constantp nil) t
  (constantp t) t
  (constantp :key) t
  (constantp 'a) nil)

(mtest
  (constantp ''a) t
  (constantp '(+)) t
  (constantp '(+ 2)) t
  (constantp '(+ 2 2)) t)

(mtest
  (constantp '(+ a)) nil
  (constantp '(+ (* 2 2) (* 3 3))) t
  (constantp '(+ (* 2 2) (* 3 a))) nil)

(mtest
  (constantp '(list 1 2 3)) nil
  (constantp '(symacrolet ((a 1)) (+ a))) t
  (constantp '(let ((a 1)) (+ a))) nil)
