(load "../common")

(test
  (mac-env-param-bind '(foo) 42 (:env e :form f x y) '(1 2) (list x y e f))
  (1 2 42 (foo)))
