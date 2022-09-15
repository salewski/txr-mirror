(load "../common")

(set *compile-test* t)

(test
  (prof (for ((i 42)) ((< i 42) i) ())) (42 0 0 0))
