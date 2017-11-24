(load "../common")

(defmacro m () 42)

(test
  (macrolet ((m (:form f) f))
    (let ((n 3))
      (macrolet ((m (:form f) f))
        (let ((n 3))
          (macrolet ((m (:form f) f))
            (m))))))
  42)
