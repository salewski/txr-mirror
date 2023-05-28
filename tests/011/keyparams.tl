(load "../common")

(defvarl v :v)
(defsymacro u (identity :u))
(defvarl x :x)
(defvarl y :y)

(mtest
  [(lambda (:key))] nil
  [(lambda (:key a))] :error
  [(lambda (:key a) a) 1] 1)

(mtest
  [(lambda (:key -- (a v)) a)] :v
  [(lambda (:key -- (a 'v)) a)] v
  [(lambda (:key -- (a v a-p)) (list a a-p))] (:v nil)
  [(lambda (:key -- (a 'v a-p)) (list a a-p))] (v nil))

(mtest
  [(lambda (:key -- (a v)) a) :a 1] 1
  [(lambda (:key -- (a 'v)) a) :a 1] 1
  [(lambda (:key -- (a v a-p)) (list a a-p)) :a 1] (1 t)
  [(lambda (:key -- (a 'v a-p)) (list a a-p)) :a 1] (1 t))

(mtest
  [(lambda (:key -- (a v) (b u)) (list a b)) :a 1] (1 :u)
  [(lambda (:key -- (a 'v) (b 'u)) (list a b)) :b 1] (v 1)
  [(lambda (:key -- (a v a-p) (b u b-p)) (list a a-p b b-p)) :a 1] (1 t :u nil)
  [(lambda (:key -- (a v a-p) (b u b-p)) (list a a-p b b-p)) :b 1] (:v nil 1 t))

(test
  [(lambda (:key -- (a v) . r) (list a r)) :a 1] (1 (:a 1)))

(defun key-place (:key -- x y (s nil s-p)) ^(,x ,y ,s ,s-p))

(defset key-place (:key -- x y) s
  ^(key-place :x ,x :y ,y :s ,s))

(test
  (set (key-place :x 3 :y 4) 42) (3 4 42 t))

(defmacro kp (r (:key -- (a v a-p) (b u b-p)) : ((:key -- (c x c-p) (d y d-p))))
  ^'(r ,a ,a-p ,b ,b-p ,c ,c-p ,d ,d-p))

(mtest
  (kp :r ()) (r :v nil :u nil :x nil :y nil)
  (kp 0 (:a 1 :b 2) (:d 3)) (r 1 t 2 t :x nil 3 t))
