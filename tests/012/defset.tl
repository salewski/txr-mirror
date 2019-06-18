(load "../common")

(defset foo (:key x y -- a b c (d 4)) n ^(bar ,x ,y, a, b, c ,d ,n))

(test
  (expand '(set (foo 1 2 :a 3 :b 4) 5))
  (bar 1 2 3 4 nil 4 5))

(test
  (expand '(inc (foo 1 2 :a 3 :b 4) 5))
  (bar 1 2 3 4 nil 4 (+ (foo 1 2 :a 3 :b 4) 5)))
