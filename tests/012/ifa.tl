(load "../common")

;; "it" is (+ 2 2)
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

(test (let ((x 5))
        (conda
          ((not (integerp x)) (list it))
          ((oddp (+ 2 x)) (list it))))
      (7))

(test (let ((l (list 1 2 3)))
        (ifa (evenp (second l))
          (inc it))
        l)
      (1 3 3))
