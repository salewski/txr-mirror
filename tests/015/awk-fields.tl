(load "../common")

(mtest
  (awk (:fields) (:begin (return-from awk))) nil
  (awk (:fields t) (:begin (return-from awk))) :error
  (awk (:fields nil) (:begin (return-from awk))) :error
  (awk (:fields - -) (:begin (return-from awk))) nil
  (awk (:fields a - - b) (:begin (return-from awk))) nil
  (awk (:fields a - - a) (:begin (return-from awk))) :error
  (awk (:fields a) (:fields b) (:begin (return-from awk))) :error)

(test
  (build
    (awk
      (:inputs '("How now brown cow"))
      (:fields h n - c)
      (t (add h n c))))
  ("How" "now" "cow"))
