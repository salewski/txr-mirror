(load "../common")

(let ((*stdout* *stdnull*))
  (test
    (build
      (awk
        (:inputs '(""))
        (t (add (prn 1 2 3))
           (add (prn)))))
    (nil nil)))
