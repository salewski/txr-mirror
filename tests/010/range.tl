(load "../common")

(mtest
  (range 1 1) (1)
  (range 1.0 1.0) (1.0)
  (range #\a #\a) (#\a)
  (range #R(1 1) #R(1 1)) (#R(1 1)))

(mtest
  (range 0 4) (0 1 2 3 4)
  (range 4 0) (4 3 2 1 0)
  (range 0.0 4.0) (0.0 1.0 2.0 3.0 4.0)
  (range 4.0 0.0) (4.0 3.0 2.0 1.0 0.0)
  (range #\a #\c) (#\a #\b #\c)
  (range #\c #\a) (#\c #\b #\a)
  (range #R(0 0) #R(4 4)) (#R(0 0) #R(1 1) #R(2 2) #R(3 3) #R(4 4))
  (range #R(4 4) #R(0 0)) (#R(4 4) #R(3 3) #R(2 2) #R(1 1) #R(0 0)))

(mtest
  (len (range 1 1 "")) :error
  (len (range 1 2 "")) :error)

(mtest
  (range 0 4 2) (0 2 4)
  (range 4 0 -2) (4 2 0)
  (range 0.0 4.0 2) (0.0 2.0 4.0)
  (range 4.0 0.0 -2) (4.0 2.0 0.0)
  (range #\a #\e 2) (#\a #\c #\e)
  (range #\e #\a -2) (#\e #\c #\a)
  (range #R(0 0) #R(4 4) 2) (#R(0 0) #R(2 2) #R(4 4))
  (range #R(4 4) #R(0 0) -2) (#R(4 4) #R(2 2) #R(0 0))
  (range 1 32 (op * 2)) (1 2 4 8 16 32))

(mtest
  (range* 1 1) nil
  (range* 1.0 1.0) nil
  (range* #\a #\a) nil
  (range* #R(1 1) #R(1 1)) nil)

(mtest
  (range* 0 4) (0 1 2 3)
  (range* 4 0) (4 3 2 1)
  (range* 0.0 4.0) (0.0 1.0 2.0 3.0)
  (range* 4.0 0.0) (4.0 3.0 2.0 1.0)
  (range* #\a #\c) (#\a #\b)
  (range* #\c #\a) (#\c #\b)
  (range* #R(0 0) #R(4 4)) (#R(0 0) #R(1 1) #R(2 2) #R(3 3))
  (range* #R(4 4) #R(0 0)) (#R(4 4) #R(3 3) #R(2 2) #R(1 1)))

(mtest
  (len (range* 1 1 "")) 0
  (len (range* 1 2 "")) :error)

(mtest
  (range* 0 4 2) (0 2)
  (range* 4 0 -2) (4 2)
  (range* 0.0 4.0 2) (0.0 2.0)
  (range* 4.0 0.0 -2) (4.0 2.0)
  (range* #\a #\e 2) (#\a #\c)
  (range* #\e #\a -2) (#\e #\c)
  (range* #R(0 0) #R(4 4) 2) (#R(0 0) #R(2 2))
  (range* #R(4 4) #R(0 0) -2) (#R(4 4) #R(2 2))
  (range* 1 32 (op * 2)) (1 2 4 8 16))

(mtest
  (range 0 1.25 0.5) (0 0.5 1.0)
  (range* 0 1.25 0.5) (0 0.5 1.0))

(mtest
  (range "A" "A") nil
  (range "A" "A" 0) :error
  (range "A" "A" -2) :error)

(mtest
  (range "A" "C") ("A" "B" "C")
  (range "A" "C" 2) ("A" "C")
  (range "A" "D" 2) ("A" "C")
  (range "A" "E" 2) ("A" "C" "E")
  (range "A" "C" 3) ("A")
  (range "A" "E" 3) ("A" "D"))

(mtest
  (range* "A" "C") ("A" "B")
  (range* "A" "C" 2) ("A")
  (range* "A" "D" 2) ("A" "C")
  (range* "A" "E" 2) ("A" "C")
  (range* "A" "C" 3) ("A")
  (range* "A" "E" 3) ("A" "D"))

(mtest
  [range "ABCD" nil rest] ("ABCD" "BCD" "CD" "D" nil)
  [range* "ABCD" nil rest] ("ABCD" "BCD" "CD" "D"))

(defstruct udnum nil
  v
  (:method + (me arg) (new udnum v (+ me.v arg)))
  (:method > (me arg) (> me.v arg))
  (:method < (me arg) (< me.v arg))
  (:method = (me arg) (= me.v arg)))

(mtest
  (range (new udnum v 0) (new udnum v 3)) (#S(udnum v 0) #S(udnum v 1) #S(udnum v 2) #S(udnum v 3))
  (range* (new udnum v 0) (new udnum v 3)) (#S(udnum v 0) #S(udnum v 1) #S(udnum v 2)))
