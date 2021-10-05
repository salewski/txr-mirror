(load "../common")

(mtest
  (awk (:fields) (:begin (return-from awk))) nil
  (awk (:fields t) (:begin (return-from awk))) :error
  (awk (:fields nil) (:begin (return-from awk))) :error
  (awk (:fields - -) (:begin (return-from awk))) nil
  (awk (:fields a - - b) (:begin (return-from awk))) nil
  (awk (:fields (a foo) - - (a bar)) (:begin (return-from awk))) :error
  (awk (:fields (a foo) - - (b bar)) (:begin (return-from awk))) nil
  (awk (:fields (a foo) (-) - (b bar)) (:begin (return-from awk))) :error
  (awk (:fields (a foo) (- i) - (b bar)) (:begin (return-from awk))) :error
  (awk (:fields a) (:fields b) (:begin (return-from awk))) :error)

(test
  (build
    (awk
      (:inputs '("How now brown cow"))
      (:fields h n - c)
      (t (add h n c))))
  ("How" "now" "cow"))

(test
  (build
    (awk
      (:inputs '("1 x 2" "a x 1" "4 x b"))
      (:fields (foo iz) - (bar iz))
      (t (add (+ foo bar)))))
  (3 1 4))

(test
  (build
    (awk
      (:inputs '("1"))
      (:fields (a iz) - (c iz))
      (t (add (+ a c) nf))))
  (1 3))
