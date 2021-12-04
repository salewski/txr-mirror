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

(mtest
  (starts-with "" "") t
  (starts-with "" "a") t
  (starts-with "a" "") nil
  (starts-with "a" "a") t
  (starts-with "" "abc") t
  (starts-with "abc" "") nil
  (starts-with "abc" "abc") t
  (starts-with "ab" "abc") t
  (starts-with "bc" "abc") nil
  )

(mtest
  (ends-with "" "") t
  (ends-with "" "a") t
  (ends-with "a" "") nil
  (ends-with "a" "a") t
  (ends-with "" "abc") t
  (ends-with "abc" "") nil
  (ends-with "abc" "abc") t
  (ends-with "ab" "abc") nil
  (ends-with "bc" "abc") t)

(mtest
  (rmismatch #() #()) nil
  (rmismatch #(1) #()) -1
  (rmismatch #() #(1)) -1
  (rmismatch #(1) #(1)) nil
  (rmismatch #(1 2) #(1 2)) nil
  (rmismatch #(2 2) #(1 2)) -2
  (rmismatch #(1 2) #(2 2)) -2
  (rmismatch #(3 2 1) #(1 1)) -2
  (rmismatch #(1 1) #(3 2 1)) -2
  (rmismatch #(3 2 1) #(2 1)) -3
  (rmismatch #(2 1) #(3 2 1)) -3)

(mtest
  (rmismatch '() '()) nil
  (rmismatch '(1) '()) -1
  (rmismatch '() '(1)) -1
  (rmismatch '(1) '(1)) nil
  (rmismatch '(1 2) '(1 2)) nil
  (rmismatch '(2 2) '(1 2)) -2
  (rmismatch '(1 2) '(2 2)) -2
  (rmismatch '(3 2 1) '(1 1)) -2
  (rmismatch '(1 1) '(3 2 1)) -2
  (rmismatch '(3 2 1) '(2 1)) -3
  (rmismatch '(2 1) '(3 2 1)) -3)

(mtest
  (rmismatch '() #()) nil
  (rmismatch '(1) #()) -1
  (rmismatch '() #(1)) -1
  (rmismatch '(1) #(1)) nil
  (rmismatch '(1 2) #(1 2)) nil
  (rmismatch '(2 2) #(1 2)) -2
  (rmismatch '(1 2) #(2 2)) -2
  (rmismatch '(3 2 1) #(1 1)) -2
  (rmismatch '(1 1) #(3 2 1)) -2
  (rmismatch '(3 2 1) #(2 1)) -3
  (rmismatch '(2 1) #(3 2 1)) -3)

(mtest
  (rmismatch #() '()) nil
  (rmismatch #(1) '()) -1
  (rmismatch #() '(1)) -1
  (rmismatch #(1) '(1)) nil
  (rmismatch #(1 2) '(1 2)) nil
  (rmismatch #(2 2) '(1 2)) -2
  (rmismatch #(1 2) '(2 2)) -2
  (rmismatch #(3 2 1) '(1 1)) -2
  (rmismatch #(1 1) '(3 2 1)) -2
  (rmismatch #(3 2 1) '(2 1)) -3
  (rmismatch #(2 1) '(3 2 1)) -3)

(mtest
  (rmismatch "" "") nil
  (rmismatch "1" "") -1
  (rmismatch "" "1") -1
  (rmismatch "1" "1") nil
  (rmismatch "12" "12") nil
  (rmismatch "22" "12") -2
  (rmismatch "12" "22") -2
  (rmismatch "321" "11") -2
  (rmismatch "11" "321") -2
  (rmismatch "321" "21") -3
  (rmismatch "21" "321") -3)

(mtest
  [keep-if oddp (range 1 10)] (1 3 5 7 9)
  [keep-if oddp nil] nil
  [keep-if oddp #()] #()
  [keep-if oddp #(1)] #(1)
  [keep-if oddp #(2)] #()
  [keep-if chr-isalpha "a1b2c3d"] "abcd"
  [keep-if chr-isalpha ""] ""
  [keep-if chr-isalpha "abc"] "abc"
  [keep-if chr-isalpha "1234"] "")

(mtest
  [remove-if oddp (range 1 10)] (2 4 6 8 10)
  [remove-if oddp nil] nil
  [remove-if oddp #()] #()
  [remove-if oddp #(1)] #()
  [remove-if oddp #(2)] #(2)
  [remove-if chr-isalpha "a1b2c3d"] "123"
  [remove-if chr-isalpha ""] ""
  [remove-if chr-isalpha "1234"] "1234"
  [remove-if chr-isalpha "abcd"] "")

(mtest
  [keep-if* chr-isalpha ""] nil
  [keep-if* chr-isalpha "abcd"] (#\a #\b #\c #\d)
  (take 3 [keep-if* oddp (range 1)]) (1 3 5))

(mtest
  [remove-if* chr-isalpha ""] nil
  [remove-if* chr-isalpha "abcd"] nil
  [remove-if* chr-isdigit "a1b2c3d4"] (#\a #\b #\c #\d)
  (take 3 [remove-if* oddp (range 1)]) (2 4 6))

(mtest
  [separate oddp (range 1 10)] ((1 3 5 7 9) (2 4 6 8 10))
  [separate integerp (range 1 10)] ((1 2 3 4 5 6 7 8 9 10) ())
  [separate chrp (range 1 10)] (() (1 2 3 4 5 6 7 8 9 10))
  [separate oddp (vec-list (range 1 10))] (#(1 3 5 7 9) #(2 4 6 8 10))
  [separate chr-isalpha "a1b2c3d4"] ("abcd" "1234")
  [separate chrp "a1b2c3d4"] ("a1b2c3d4" "")
  [separate integerp "a1b2c3d4"] ("" "a1b2c3d4"))

(mtest
  (tuples 0 nil) :error
  (tuples 3.5 '(1 2 3)) :error
  (tuples -1 "abc") :error)

(mtest
  (tuples 1 nil) nil
  (tuples 1 "") nil
  (tuples 1 #()) nil)

(mtest
  (tuples 1 '(a)) ((a))
  (tuples 1 "a") ("a")
  (tuples 1 #(1)) (#(1)))

(mtest
  (tuples 1 '(a b c)) ((a) (b) (c))
  (tuples 1 "abc") ("a" "b" "c")
  (tuples 1 #(1 2 3)) (#(1) #(2) #(3)))

(mtest
  (tuples 1 '(a b c) 'd) ((a) (b) (c))
  (tuples 1 "abc" #\d) ("a" "b" "c")
  (tuples 1 #(1 2 3) 4) (#(1) #(2) #(3)))

(mtest
  (tuples 2 '(a b c)) ((a b) (c))
  (tuples 2 "abc") ("ab" "c")
  (tuples 2 #(1 2 3)) (#(1 2) #(3)))

(mtest
  (tuples 3 '(a b c)) ((a b c))
  (tuples 3 "abc") ("abc")
  (tuples 3 #(1 2 3)) (#(1 2 3)))

(mtest
  (tuples 2 '(a b c) 'd) ((a b) (c d))
  (tuples 2 "abc" #\d) ("ab" "cd")
  (tuples 2 #(1 2 3) 4) (#(1 2) #(3 4)))

(defun lforce (list)
  [mapdo identity list]
  list)

(test
  (lforce (tuples 2 "abc" 3)) :error)

(test
  (take 3 (tuples 3 (range 0))) ((0 1 2) (3 4 5) (6 7 8)))

(mtest
  (tuples* 0 nil) :error
  (tuples* 3.5 '(1 2 3)) :error
  (tuples* -1 "abc") :error)

(mtest
  (tuples* 1 nil) nil
  (tuples* 1 "") nil
  (tuples* 1 #()) nil)

(mtest
  (tuples* 1 '(a)) ((a))
  (tuples* 1 "a") ("a")
  (tuples* 1 #(1)) (#(1)))

(mtest
  (tuples* 1 '(a b c)) ((a) (b) (c))
  (tuples* 1 "abc") ("a" "b" "c")
  (tuples* 1 #(1 2 3)) (#(1) #(2) #(3)))

(mtest
  (tuples* 1 '(a b c) 'd) ((a) (b) (c))
  (tuples* 1 "abc" #\d) ("a" "b" "c")
  (tuples* 1 #(1 2 3) 4) (#(1) #(2) #(3)))

(mtest
  (tuples* 2 '(a b c)) ((a b) (b c))
  (tuples* 2 "abc") ("ab" "bc")
  (tuples* 2 #(1 2 3)) (#(1 2) #(2 3)))

(mtest
  (tuples* 3 '(a b c)) ((a b c))
  (tuples* 3 "abc") ("abc")
  (tuples* 3 #(1 2 3)) (#(1 2 3)))

(mtest
  (tuples* 3 '(a b) 'c) ((a b c))
  (tuples* 3 "a" #\c) ("acc")
  (tuples* 3 #() 1) (#(1 1 1)))

(test
  (lforce (tuples* 3 "a" 1)) :error)

(mtest
  (take 3 (tuples* 3 (range 0))) ((0 1 2) (1 2 3) (2 3 4))
  (take 3 (tuples* 3 0)) ((0 1 2) (1 2 3) (2 3 4)))
