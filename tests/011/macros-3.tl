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

(defvarl x 0)
(defmacro mac-time-counter () (inc x))
(defsymacro s (mac-time-counter))

(mtest s 1
       s 2
       s 3)

(test (symacrolet ((a 42))
        (labels ()
          a))
      42)
