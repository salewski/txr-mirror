(load "../common")

(test (with-resources ((a nil (list a))
                       (b nil)
                       (c nil (list c)))
        (list a b c))
      (nil nil nil))

(test (build
        (catch
          (with-resources ((a 1 (add a))
                           (x nil)
                           (b 2 (add b))
                           (y (throw 'out))
                           (z nil t)
                           (c 3 (add c))))
          (out () (add 4))))
      (2 1 4))
