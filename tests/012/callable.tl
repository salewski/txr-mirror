(load "../common")

(mtest
  [0 '(1 2 3)] 1
  [1 '(1 2 3)] 2
  [2 '(1 2 3)] 3)

(mtest
  [0 "abc"] #\a
  [1 "abc"] #\b
  [2 "abc"] #\c)

(mtest
  [0..1 '(1 2 3)] (1)
  [1..3 '(1 2 3)] (2 3))

(mtest
  [0..0 "abc"] ""
  [0..2 "abc"] "ab"
  [-1..: "abc"] "c")

(test (mapcar [callf list* 2 0 1 3..:] '((A B C X) (D E F Y) (G H I Z)))
      ((C A B X) (F D E Y) (I G H Z)))

(mtest
  (set [1 1] 2) :error
  (set [1 1..2] 2) :error
  (set [1..2 1] 2) :error
  (set [1..2 1..2] 2) :error
  (let ((abc "abc")) (set [1..2 abc] "42") abc) "a42c"
  (let ((abc "abc")) (set [1 abc] #\d) abc) "adc")
