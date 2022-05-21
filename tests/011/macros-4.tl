(load "../common")

(defmacro xsqrt (:match :form f)
  (((* @exp @exp)) exp)
  (@else f))

(defmacro xexpt (:match :form f)
   ((@exp 2) ^(* ,exp ,exp))
   (@else f))

(test (expand '(xsqrt (xexpt x 2))) x)
