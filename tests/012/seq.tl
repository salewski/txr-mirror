(load "../common")

(test (append "abc" "d") "abcd")
(test (append "abc" #(#\d)) "abcd")
(test (append '(1 2 . "abc") #(#\d)) (1 2 . "abcd"))
(test (append 3 4) :error)
(test (append '(1) 2) (1 . 2))
(test (append '(1 . 2) 2) :error)
(test (append '(1 . #(3 4 5)) "d") (1 . #(3 4 5 #\d)))

(test (build (add 1) (add 2) (pend (get))) (1 2 1 2))
(test (build (add 1) (add 2) (pend* (get))) (1 2 1 2))
(test (build (add 1) (add 2) (pend (get) (get))) (1 2 1 2 1 2))
(test (build (add 1) (add 2) (pend* (get) (get))) (1 2 1 2 1 2))

(set *print-circle* t)

(stest (build (add 1) (add 2) (ncon (get))) "#1=(1 2 . #1#)")
(stest (build (add 1) (add 2) (ncon* (get))) "#1=(1 2 . #1#)")

(test (mapcar (lambda (. args) (list . args)) '#(1 2 3) '#(4 5 6))
      #((1 4) (2 5) (3 6)))

(test [window-map 2 '(x x) list '(a b c d e f g)]
      ((x x a b c) (x a b c d) (a b c d e)
       (b c d e f) (c d e f g) (d e f g nil)
       (e f g nil nil)))

(test [window-map 2 '(x x y y) list '(a b c d e f g)]
      ((x x a b c) (x a b c d) (a b c d e)
       (b c d e f) (c d e f g) (d e f g y)
       (e f g y y)))

(test [window-map 2 nil list '(a b c d e f g)]
      ((nil nil a b c) (nil a b c d) (a b c d e)
       (b c d e f) (c d e f g)
       (d e f g nil) (e f g nil nil)))

(test [window-map 2 :wrap list '(a b c d e f g)]
      ((f g a b c) (g a b c d) (a b c d e) (b c d e f)
       (c d e f g) (d e f g a) (e f g a b)))

(test [window-map 2 :reflect list '(a b c d e f g)]
      ((b a a b c) (a a b c d) (a b c d e) (b c d e f)
       (c d e f g) (d e f g g) (e f g g f)))

(test [window-map 7 :wrap list '(a b c)]
      ((c a b c a b c a b c a b c a b)
       (a b c a b c a b c a b c a b c)
       (b c a b c a b c a b c a b c a)))

(test [window-map 7 :reflect list '(a b c)]
      ((a c b a c b a a b c c b a c b)
       (c b a c b a a b c c b a c b a)
       (b a c b a a b c c b a c b a c)))

(test [window-map 1 nil (lambda (x y z)
                          (if (and (eq x #\<)
                                   (eq z #\>))
                            (chr-toupper y)
                            y))
                  "ab<c>de<f>g"]
      "ab<C>de<F>g")

(test [window-mappend 1 :reflect (lambda (x y z)
                                   (if (< x y z)
                                     (list y)))
                      '(1 2 1 3 4 2 1 9 7 5 7 8 5)]
      (3 7))

(test [window-map 2 #(0 0 0 0)
                  (lambda (. args) (/ (sum args) 5))
                  #(4 7 9 13 5 1 6 11 10 3 8)]
      #(4.0 6.6 7.6 7.0 6.8 7.2 6.6 6.2 7.6 6.4 4.2))

(mtest
  [reduce-left + () 0] 0
  [reduce-left + ()] 0
  [reduce-left cons ()] :error
  [reduce-left cons '(1)] 1
  [reduce-left cons #(1)] 1
  [reduce-left cons #(1) : (op * 10)] 10
  [reduce-left cons #(1) 2 (op * 10)] (2 . 10)
  [reduce-left cons #(2 3) 10 (op * 10)] ((10 . 20) . 30))
