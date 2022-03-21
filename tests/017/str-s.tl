(load "../common")

(with-dyn-lib nil
  (deffi strtol "strtol" long (str (ptr-out (array 1 str-s)) int))
  (deffi bcstol "strtol" long (bstr (ptr-out (array 1 bstr-s)) int))
  (deffi wcstol "wcstol" long (wstr (ptr-out (array 1 wstr-s)) int)))

(mtest
  (let ((v (vec nil))) (list (strtol "-345x" v 0) v)) (-345 #("x"))
  (let ((v (vec nil))) (list (bcstol "-345x" v 0) v)) (-345 #("x"))
  (let ((v (vec nil))) (list (wcstol "-345x" v 0) v)) (-345 #("x")))
