(defmacro error-to-sym (expr)
 ^(catch ,expr
    (error (cond) :error)))

;; test framework for ifa uses ifa!
(defmacro test (:env env expr expected)
 (catch
   (let ((expr-expn (macroexpand expr env)))
     ^(ifa (not (equal (error-to-sym ,expr-expn) ',expected))
           (error "test case ~s failed: produced ~s; expected ~s"
                  ',expr it ',expected)))
   (error (exc)
          (unless (eq expected :error)
            (error "test case ~s failed to expand: expected is ~s" expr expected)))))

;; "it" is (+ 2 2)
(ifa (> (+ 2 2) 0) (* it 2))

(test (ifa (> (+ 2 2) 0) (* it 2))
  8)

;; "it" is (* x x)
(test (let ((x 7))
    (ifa (>= (* x x) 49)
      (isqrt it)))
  7)

;; ambiguous: is "it" x or is "it" y?
(test (ifa (> x y) (print it)) :error)

;; "it" is (+ 3 (* 2 x))
(test (let ((x 5))
    (ifa (< 0 (+ 3 (* 2 x)) 20) (* 100 it)))
  1300)

;; "it" is (length '(a b c d))
;; Intuition: it" could also be '(a b c d)
;; TODO: deal specially with chains of unary functions.
;; How about it = (length ...), itt = '(a b c d)
(test (ifa (not (oddp (length '(a b c d)))) it)
  4)

;; "it" is y because %x% is constantp
(test (symacrolet ((%x% 42))
      (let ((y 41))
        (ifa (> %x% y) it)))
    42)
