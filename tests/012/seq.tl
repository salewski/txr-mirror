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

(mtest
  (build (add 1 2) (oust)) nil
  (build (add 1 2) (oust '(3 4)) (add 5)) (3 4 5)
  (build (add 1 2) (oust '(3 4) '(5)) (add 6)) (3 4 5 6))

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
  (lforce (tuples 2 "abc" 3)) ("ab" (#\c 3)))

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

(mtest
  (nrot nil) nil
  (nrot (vec)) #()
  (nrot "") ""
  (nrot nil 2) nil
  (nrot (vec) 2) #()
  (nrot "" 2) ""
  (nrot nil -1) nil
  (nrot (vec) -1) #()
  (nrot "" -1) "")

(mtest
  (let ((s '(a))) (nrot s)) (a)
  (let ((s (vec 1))) (nrot s) s) #(1)
  (let ((s "x")) (nrot s) s) "x"
  (let ((s '(a))) (nrot s -1)) (a)
  (let ((s (vec 1))) (nrot s -1) s) #(1)
  (let ((s "x")) (nrot s -1) s) "x")

(mtest
  (let ((s '(a b))) (nrot s)) (b a)
  (let ((s (vec 1 2))) (nrot s) s) #(2 1)
  (let ((s (copy "xy"))) (nrot s) s) "yx"
  (let ((s '(a b))) (nrot s -1)) (b a)
  (let ((s (vec 1 2))) (nrot s -1) s) #(2 1)
  (let ((s (copy "xy"))) (nrot s -1) s) "yx")

(mtest
  (let ((s '(a b c))) (nrot s)) (b c a)
  (let ((s (vec 1 2 3))) (nrot s) s) #(2 3 1)
  (let ((s (copy "xyz"))) (nrot s) s) "yzx"
  (let ((s '(a b c))) (nrot s -1)) (c a b)
  (let ((s (vec 1 2 3))) (nrot s -1) s) #(3 1 2)
  (let ((s (copy "xyz"))) (nrot s -1) s) "zxy")

(mtest
  (let ((s (list 'a 'b 'c))) (nrot s 33)) (a b c)
  (let ((s (list 'a 'b 'c))) (nrot s 34)) (b c a))

(mtest
  (rot nil) nil
  (rot #()) #()
  (rot "") ""
  (rot nil 2) nil
  (rot #() 2) #()
  (rot "" 2) ""
  (rot nil -1) nil
  (rot #() -1) #()
  (rot "" -1) "")

(mtest
  (let ((s '(a))) (list (rot s) s)) ((a) (a))
  (let ((s #(1))) (list (rot s) s)) (#(1) #(1))
  (let ((s "x")) (list (rot s) s)) ("x" "x")
  (let ((s '(a))) (list (rot s -1) s)) ((a) (a))
  (let ((s #(1))) (list (rot s -1) s)) (#(1) #(1))
  (let ((s "x")) (list (rot s -1) s)) ("x" "x"))

(mtest
  (let ((s '(a b))) (list (rot s) s)) ((b a) (a b))
  (let ((s #(1 2))) (list (rot s) s)) (#(2 1) #(1 2))
  (let ((s "xy")) (list (rot s) s)) ("yx" "xy")
  (let ((s '(a b))) (list (rot s -1) s)) ((b a) (a b))
  (let ((s #(1 2))) (list (rot s -1) s)) (#(2 1) #(1 2))
  (let ((s "xy")) (list (rot s -1) s)) ("yx" "xy"))

(mtest
  (let ((s '(a b c))) (list (rot s) s)) ((b c a) (a b c))
  (let ((s #(1 2 3))) (list (rot s) s)) (#(2 3 1) #(1 2 3))
  (let ((s "xyz")) (list (rot s) s)) ("yzx" "xyz")
  (let ((s '(a b c))) (list (rot s -1) s)) ((c a b) (a b c))
  (let ((s #(1 2 3))) (list (rot s -1) s)) (#(3 1 2) #(1 2 3))
  (let ((s "xyz")) (list (rot s -1) s)) ("zxy" "xyz"))

(mtest
  (let ((s '(a b c))) (list (rot s 33) s)) ((a b c) (a b c))
  (let ((s '(a b c))) (list (rot s 34) s)) ((b c a) (a b c)))

(mtest
  (subq #\a #\b "") ""
  (subq #\a #\b "a") "b"
  (subq #\a #\b "aaa") "bbb"
  (subq #\a #\b "abc") "bbc")

(mtest
  (subql #\a #\b "") ""
  (subql #\a #\b "a") "b"
  (subql #\a #\b "aaa") "bbb"
  (subql #\a #\b "abc") "bbc")

(mtest
  (subqual #\a #\b "") ""
  (subqual #\a #\b "a") "b"
  (subqual #\a #\b "aaa") "bbb"
  (subqual #\a #\b "abc") "bbc")

(mtest
  (subq 0 1 nil) nil
  (subq 0 1 '(0)) (1)
  (subq 0 1 '(0 0 0)) (1 1 1)
  (subq 0 1 '(0 1 2)) (1 1 2))

(mtest
  (subql 0 1 nil) nil
  (subql 0 1 '(0)) (1)
  (subql 0 1 '(0 0 0)) (1 1 1)
  (subql 0 1 '(0 1 2)) (1 1 2))

(mtest
  (subqual 0 1 nil) nil
  (subqual 0 1 '(0)) (1)
  (subqual 0 1 '(0 0 0)) (1 1 1)
  (subqual 0 1 '(0 1 2)) (1 1 2))

(mtest
  (subqual "foo" "bar" nil) nil
  (subqual "foo" "bar" '#"foo") #"bar"
  (subqual "foo" "bar" '#"foo foo foo") #"bar bar bar"
  (subqual "foo" "bar" '#"xyzzy foo quuz") #"xyzzy bar quuz")

(mtest
  (subqual "brown" "black" #("how" "now" "brown" "cow")) #("how" "now" "black" "cow")
  (subst "brown" "black" #("how" "now" "brown" "cow")) #("how" "now" "black" "cow"))

(mtest
  [subst "brown" "black" #("how" "now" "BROWN" "cow") : downcase-str] #("how" "now" "black" "cow")
  [subst 5 0 '(1 2 3 4 5 6 7 8 9 10) <] (1 2 3 4 5 0 0 0 0 0))

(mtest
  (pairlis nil nil) nil
  (pairlis "abc" #(1 2 3 4)) ((#\a . 1) (#\b . 2) (#\c . 3))
  (pairlis "abcd" #(1 2 3)) ((#\a . 1) (#\b . 2) (#\c . 3))
  (pairlis "" #(1 2 3)) nil
  (pairlis "abcd" #()) nil
  (pairlis '(1 2 3) '(a b c) '(4 5 6)) ((1 . a) (2 . b) (3 . c) 4 5 6))

(mtest
  (find-max nil) nil
  [find-max '("alpha" "charlie" "aardvark" "bravo") less] "aardvark"
  [find-max '("alpha" "charlie" "aardvark" "bravo") less reverse] "alpha"
  [find-max '("alpha" "charlie" "aardvark" "bravo") : reverse] "bravo"
  (find-max 1..10) 9
  [find-max #H(() (a 1) (b 2) (c 3)) : cdr] (c . 3))

(mtest
  (find-max-key nil) nil
  [find-max-key '("alpha" "charlie" "aardvark" "bravo") less upcase-str] "AARDVARK"
  [find-max-key #H(() (a 1) (b 2) (c 3)) : cdr] 3)

(defvarl fn (do and
              (chr-isdigit @1)
              (not (chr-isdigit @2))))

(mtest
  [partition-if tf nil] nil
  [partition-if tf "abc"] ("a" "b" "c")
  [partition-if nilf "abc"] ("abc")
  [partition-if neql "aaaabbcdee"] ("aaaa" "bb" "c" "d" "ee")
  (partition-if fn "a13cd9foo42z") ("a13" "cd9" "foo42" "z"))

(mtest
  (partition-if (op /= (- @2 @1) 1)
                '(1 3 4 5 7 8 9 10 9 8 6 5 3 2))
  ((1) (3 4 5) (7 8 9 10) (9) (8) (6) (5) (3) (2))
  (partition-if (op > (abs (- @2 @1)) 1)
                              '(1 3 4 5 7 8 9 10 9 8 6 5 3 2))
  ((1) (3 4 5) (7 8 9 10 9 8) (6 5) (3 2)))

(mtest
  [partition-if neql "aaaabbcdee" 2] ("aaaa" "bb" "cdee")
  [partition-if neql "aaaabbcdee" 1] ("aaaa" "bbcdee")
  [partition-if fn "a13cd9foo42z" 2] ("a13" "cd9" "foo42z")
  [partition-if fn "a13cd9foo42z" 1] ("a13" "cd9foo42z")
  [partition-if fn "a13cd9foo42z" 0] ("a13cd9foo42z"))

(mtest
  [count 1 nil] 0
  [count 1 '(1 2 3 4 1 5)] 2
  [count "abc" '("foo" "bar" "ABC" "abc" "def" "abc")] 2
  [count "ABC" '("foo" "bar" "ABC" "abc" "def" "abc") : upcase-str] 3)

(compile-only
  (test
    [count #1="abc" '("abc" "abc" "abc" #1# "abc" #1#" abc") eq] 2))

(mtest
  (search "" "") 0
  (search "abcde" "ab") 0
  (search "abcde" "bc") 1
  (search "abcde" "cd") 2
  (search "abcde" "de") 3
  (search "abcde" "e") 4
  (search "abcde" "") 0
  (search "abcde" "x") nil)

(mtest
  (search nil nil) 0
  (search '#"a b c d e" '#"a b") 0
  (search '#"a b c d e" '#"b c") 1
  (search '#"a b c d e" '#"c d") 2
  (search '#"a b c d e" '#"d e") 3
  (search '#"a b c d e" '#"e") 4
  (search '#"a b c d e" nil) 0
  (search '#"a b c d e" '#"x") nil)

(mtest
  (rsearch nil nil) 0
  (rsearch "abcde" "ab") 0
  (rsearch "abcde" "bc") 1
  (rsearch "abcde" "cd") 2
  (rsearch "abcde" "de") 3
  (rsearch "abcde" "e") 4
  (rsearch "abcde" "") 5
  (rsearch "abcde" "x") nil)

(mtest
  (rsearch '#"a b c d e" '#"a b") 0
  (rsearch '#"a b c d e" '#"b c") 1
  (rsearch '#"a b c d e" '#"c d") 2
  (rsearch '#"a b c d e" '#"d e") 3
  (rsearch '#"a b c d e" '#"e") 4
  (rsearch '#"a b c d e" nil) 5
  (rsearch '#"a b c d e" '#"x") nil)

(mtest
  (search-all "" "") (0)
  (search-all "xxxxx" "y") nil
  (search-all "xxxxx" "x") (0 1 2 3 4)
  (search-all "xxx" "") (0 1 2 3))

(mtest
  (search-all nil nil) (0)
  (search-all '#"x x x x x" '#"y") nil
  (search-all '#"x x x x x" '#"x") (0 1 2 3 4)
  (search-all '#"x x x" "") (0 1 2 3))

(mtest
  [keep-keys-if evenp (range 1 20) square] (4 16 36 64 100 144 196 256 324 400)
  [keep-keys-if chr-isupper "foo bar" chr-toupper] "FOOBAR"
  [keep-keys-if evenp (vec-list (range 1 20)) square] #(4 16 36 64 100 144 196 256 324 400))


(mtest
  [separate-keys evenp (range 1 20) square] ((4 16 36 64 100 144 196 256 324 400)
                                             (1 9 25 49 81 121 169 225 289 361))
  [separate-keys chr-isupper "foo bar" chr-toupper] ("FOOBAR" " ")
  [separate-keys evenp (vec-list (range 1 20)) square] (#(4 16 36 64 100 144 196 256 324 400)
                                                        #(1 9 25 49 81 121 169 225 289 361)))

(mtest
  (flatten '()) ()
  (flatten '(nil)) ()
  (flatten '(a)) (a)
  (flatten '(a b)) (a b)
  (flatten '(nil b)) (b)
  (flatten '(a nil)) (a)

  (flatten '((nil))) ()
  (flatten '((a))) (a)
  (flatten '((a) (b))) (a b)
  (flatten '((nil) (b))) (b)
  (flatten '((a) (nil))) (a)

  (flatten '((a b))) (a b)
  (flatten '((nil b))) (b)
  (flatten '((a nil))) (a)

  (flatten '(((())))) nil
  (flatten '(((())) a)) (a)
  (flatten '(((()) a))) (a)
  (flatten '(((() a)))) (a)
  (flatten '((((a))))) (a)

  (flatten 3) (3)
  (flatten '(1 . 2)) :error
  (flatten '(1 2 . 3)) :error
  (flatten '(1 (2 . 3))) :error)

(mtest
  (flatten* '()) ()
  (flatten* '(nil)) ()
  (flatten* '(a)) (a)
  (flatten* '(a b)) (a b)
  (flatten* '(nil b)) (b)
  (flatten* '(a nil)) (a)

  (flatten* '((nil))) ()
  (flatten* '((a))) (a)
  (flatten* '((a) (b))) (a b)
  (flatten* '((nil) (b))) (b)
  (flatten* '((a) (nil))) (a)

  (flatten* '((a b))) (a b)
  (flatten* '((nil b))) (b)
  (flatten* '((a nil))) (a)

  (flatten* '(((())))) nil
  (flatten* '(((())) a)) (a)
  (flatten* '(((()) a))) (a)
  (flatten* '(((() a)))) (a)
  (flatten* '((((a))))) (a)

  (flatten* 3) (3)
  (lforce (flatten* '(1 . 2))) :error
  (lforce (flatten* '(1 2 . 3))) :error
  (lforce (flatten* '(1 (2 . 3)))) :error)

(mtest
  (flatcar ()) (nil)
  (flatcar 'a) (a)
  (flatcar '(a . b)) (a b)
  (flatcar '(nil . nil)) (nil)
  (flatcar '(nil . b)) (nil b)
  (flatcar '(b . nil)) (b)
  (flatcar '(a b . c)) (a b c)
  (flatcar '(() b . c)) (nil b c)
  (flatcar '((()) b . c)) (nil b c)
  (flatcar '(((a)) b . c)) (a b c))

(mtest
  (flatcar* ()) (nil)
  (flatcar* 'a) (a)
  (flatcar* '(a . b)) (a b)
  (flatcar* '(nil . nil)) (nil)
  (flatcar* '(nil . b)) (nil b)
  (flatcar* '(b . nil)) (b)
  (flatcar* '(a b . c)) (a b c)
  (flatcar* '(() b . c)) (nil b c)
  (flatcar* '((()) b . c)) (nil b c)
  (flatcar* '(((a)) b . c)) (a b c))

(mtest
  (length-< nil 0) nil
  (length-< nil 1) t
  (length-< '(a) 1) nil
  (length-< '(a) 2) t
  (length-< '(a . b) 1) nil
  (length-< '(a . b) 2) t)

(mtest
  (length-< "" 0) nil
  (length-< "" 1) t
  (length-< "a" 1) nil
  (length-< "a" 2) t)

(mtest
  (length-< #() 0) nil
  (length-< #() 1) t
  (length-< #(a) 1) nil
  (length-< #(a) 2) t)

(let ((l (list 1 2 3 4)))
  (del (ref l 1))
  (test l (1 3 4))
  (del (second l))
  (test l (1 4)))

(let ((nl (list (list (list 1 2)
                      (list 3 4)
                      (list 5 6))
                (list (list 7 8)
                      (list 9 10)
                      (list 11 12)))))
  (mtest
    (mref nl 0 0 0) 1
    (mref nl 0 0 1) 2
    (mref nl 0 1 0) 3
    (mref nl 0 1 1) 4
    (mref nl 0 2 0) 5
    (mref nl 0 2 1) 6
    (mref nl 1 0 0) 7
    (mref nl 1 0 1) 8
    (mref nl 1 1 0) 9
    (mref nl 1 1 1) 10
    (mref nl 1 2 0) 11
    (mref nl 0 2 1) 6)

  (mtest
    (set (mref nl 0 0 0) 101) 101
    (mref nl 0 0 0) 101

    (del (mref nl 0 0 0..:)) (101 2)
    nl ((nil (3 4) (5 6)) ((7 8) (9 10) (11 12)))

    (set (mref nl 1 0..2) '(4)) (4)
    nl ((nil (3 4) (5 6)) (4 (11 12)))

    (del (mref nl 1)) (4 (11 12))
    nl ((nil (3 4) (5 6)))

    (set (mref nl 1..:) '(a b c)) (a b c)
    nl ((nil (3 4) (5 6)) a b c)

    (set (mref nl 1..3) '(e f)) (e f)
    nl ((nil (3 4) (5 6)) e f c)))

(flet ((get-vec () (vec 1 2 3))
       (get-list () (list 1 2 3)))
  (mtest
    (inc (mref (get-vec) 0)) 2
    (set (mref (get-vec) 0) 10) 10
    (inc (mref (get-list) 0)) 2
    (set (mref (get-list) 0) 10) 10
    (push 3 (mref (get-vec) 1..2)) (3 . #(2))
    (set (mref (get-vec) 1..2) '(30)) (30)
    (push 3 (mref (get-list) 1..2)) :error
    (set (mref (get-list) 1..2) '(30)) :error))


(let ((nv (nested-vec 4 4 4)))
  (let ((x 0))
    (each-prod ((i 0..4)
                (j 0..4)
                (k 0..4))
      (vtest (set (mref nv i j k) (inc x)) (succ x))))
  (mtest
    nv #(#(#( 1  2  3  4) #( 5  6  7  8) #( 9 10 11 12) #(13 14 15 16))
         #(#(17 18 19 20) #(21 22 23 24) #(25 26 27 28) #(29 30 31 32))
         #(#(33 34 35 36) #(37 38 39 40) #(41 42 43 44) #(45 46 47 48))
         #(#(49 50 51 52) #(53 54 55 56) #(57 58 59 60) #(61 62 63 64)))
    (set (mref nv 0 0 1..3) #(20 30)) #(20 30)
    nv #(#(#( 1 20 30  4) #( 5  6  7  8) #( 9 10 11 12) #(13 14 15 16))
         #(#(17 18 19 20) #(21 22 23 24) #(25 26 27 28) #(29 30 31 32))
         #(#(33 34 35 36) #(37 38 39 40) #(41 42 43 44) #(45 46 47 48))
         #(#(49 50 51 52) #(53 54 55 56) #(57 58 59 60) #(61 62 63 64)))
    (set (mref nv 1 1..3) "AB") "AB"
    nv #(#(#( 1 20 30  4) #( 5  6  7  8) #( 9 10 11 12) #(13 14 15 16))
         #(#(17 18 19 20)            #\A            #\B #(29 30 31 32))
         #(#(33 34 35 36) #(37 38 39 40) #(41 42 43 44) #(45 46 47 48))
         #(#(49 50 51 52) #(53 54 55 56) #(57 58 59 60) #(61 62 63 64)))
    (set (mref nv 1..3) '(B C)) (B C)
    nv #(#(#( 1 20 30  4) #( 5  6  7  8) #( 9 10 11 12) #(13 14 15 16))
         B
         C
         #(#(49 50 51 52) #(53 54 55 56) #(57 58 59 60) #(61 62 63 64)))))

(let ((cf (lambda (x)
            (lambda (y)
              (lambda (z)
                (+ x y z))))))
  (test [mref cf 1 2 3] 6))

(test
  (zip) nil)

(mtest
  (zip '()) nil
  (zip #()) #()
  (zip "") ""
  (zip #b'') #b'')

(mtest
  (zip '(a)) ((a))
  (zip '(a b)) ((a) (b))
  (zip '(a b c)) ((a) (b) (c)))

(mtest
  (zip #(a)) #(#(a))
  (zip #(a b)) #(#(a) #(b))
  (zip #(a b c)) #(#(a) #(b) #(c)))

(mtest
  (zip "a") ("a")
  (zip "ab") ("a" "b")
  (zip "abc") ("a" "b" "c"))

(mtest
  (zip #b'aa') (#b'aa')
  (zip #b'aabb') (#b'aa' #b'bb')
  (zip #b'aabbcc') (#b'aa' #b'bb' #b'cc'))

(mtest
  (zip '(a) '(b)) ((a b))
  (zip '(a c) '(b d)) ((a b) (c d))
  (zip '(a c e) '(b d f)) ((a b) (c d) (e f))
  (zip '(a d) '(b e) '(c f)) ((a b c) (d e f)))

(mtest
  (zip #(a) #(b)) #(#(a b))
  (zip #(a c) #(b d)) #(#(a b) #(c d))
  (zip #(a c e) #(b d f)) #(#(a b) #(c d) #(e f))
  (zip #(a d) #(b e) #(c f)) #(#(a b c) #(d e f)))

(mtest
  (zip #(a) #(b)) #(#(a b))
  (zip #(a c) #(b d)) #(#(a b) #(c d))
  (zip #(a c e) #(b d f)) #(#(a b) #(c d) #(e f))
  (zip #(a d) #(b e) #(c f)) #(#(a b c) #(d e f)))

(mtest
  (zip "a" "b") ("ab")
  (zip "ac" "bd") ("ab" "cd")
  (zip "ace" "bdf") ("ab" "cd" "ef")
  (zip "ad" "bef" "cf") ("abc" "def"))

(mtest
  (zip #b'aa' #b'bb') (#b'aabb')
  (zip #b'aacc' #b'bbdd') (#b'aabb' #b'ccdd')
  (zip #b'aaccee' #b'bbddff') (#b'aabb' #b'ccdd' #b'eeff')
  (zip #b'aaddee' #b'bbeeff' #b'ccff') (#b'aabbcc' #b'ddeeff'))

(test
  (zip "ab" "ijklm" "xy") ("aix" "bjy"))

(test
  (zip "ab" '(#\i #\j) #("x" "y")) ("aix" "bjy"))

(vtest
  [apply mapcar join (list-seq "aaa".."zzz")]
  (transpose (list-seq "aaa".."zzz")))

(eval-only (set *compile-opts*.constant-throws nil))

(mtest
  (ref "a".."z" 0) :error
  (ref (rcons 'foo 'bar) 0) :error)

(mtest
  (ref 1..6 0) 1
  (ref 1..6 1) 2
  (ref 1..6 4) 5
  (ref 1..6 5) :error
  (ref 1..6 -1) 5
  (ref 1..6 -2) 4
  (ref 1..6 -5) 1
  (ref 1..6 -6) :error)

(mtest
  (ref 1..: 0) 1
  (ref 1..: 1) 2
  (ref 1..: 4) 5
  (ref 1..: -1) :error
  (ref 1..: -2) :error)

(mtest
  (ref 1..t 0) 1
  (ref 1..t 1) 2
  (ref 1..t 4) 5
  (ref 1..t -1) :error
  (ref 1..: -2) :error)

(mtest
  (ref #\a..#\f 0) #\a
  (ref #\a..#\f 1) #\b
  (ref #\a..#\f 4) #\e
  (ref #\a..#\f 5) :error
  (ref #\a..#\f -1) #\e
  (ref #\a..#\f -2) #\d
  (ref #\a..#\f -5) #\a
  (ref #\a..#\f -6) :error)

(mtest
  (ref #\a..: 0) #\a
  (ref #\a..: 1) #\b
  (ref #\a..: 4) #\e
  (ref #\a..: -1) :error
  (ref #\a..: -2) :error)

(mtest
  (ref #\a..t 0) #\a
  (ref #\a..t 1) #\b
  (ref #\a..t 4) #\e
  (ref #\a..t -1) :error
  (ref #\a..: -2) :error)


(mtest
  (ref 1..6 0.0) (1.0 2.0 3.0 4.0 5.0))
