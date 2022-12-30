(load "../common")

(test
  (build
    (awk
      (:inputs '#"How about it now brown cow")
      (#/.ow/ (add res))))
  ("How" "now" "row" "cow"))
