(load "../common.tl")

(each ((b 2..36))
  (mtest
    (int-str "" b) nil
    (int-str "$" b) nil
    (int-str "-" b) nil
    (int-str "+" b) nil
    (int-str "0" b) 0
    (int-str "00" b) 0
    (int-str "0x" b) 0
    (int-str "0x3" b) 0
    (int-str "0xz" b) 0))

(mtest
  (int-str "+123") 123
  (int-str "-123") -123
  (int-str "0123") 123
  (int-str "00123") 123
  (int-str "999999999999999999999999999999") 999999999999999999999999999999
  (int-str "+999999999999999999999999999999") 999999999999999999999999999999
  (int-str "-999999999999999999999999999999") -999999999999999999999999999999)

(let ((c #\c))
  (mtest
    (int-str "+123" c) 123
    (int-str "-123" c) -123
    (int-str "0123" c) 83
    (int-str "00123" c) 83
    (int-str "0x123" c) 291
    (int-str "-0x123" c) -291
    (int-str "+0xFFFFFFFFFFFFFFFFFFFF" c) #xFFFFFFFFFFFFFFFFFFFF
    (int-str "-0xFFFFFFFFFFFFFFFFFFFF" c) #x-FFFFFFFFFFFFFFFFFFFF))

(mtest
  (int-str "zZz" 35) nil
  (int-str "zZz" 36) 46655
  (int-str "-zZz" 36) -46655
  (int-str "+zZz" 36) 46655
  (int-str "+0zZz" 36) 46655
  (int-str "-0zZz" 36) -46655
  (int-str "0zZz" 36) 46655
  (int-str "1" 36) 1
  (int-str "10" 36) 36
  (int-str "100" 36) 1296
  (int-str "zzzzzzzzzzzzzzzzzzzzzzzz" 36) 22452257707354557240087211123792674815
  (int-str "-zzzzzzzzzzzzzzzzzzzzzzzz" 36) -22452257707354557240087211123792674815
  (int-str "0zzzzzzzzzzzzzzzzzzzzzzzz" 36) 22452257707354557240087211123792674815
  (int-str "-0zzzzzzzzzzzzzzzzzzzzzzzz" 36) -22452257707354557240087211123792674815
  (int-str "+0zzzzzzzzzzzzzzzzzzzzzzzz" 36) 22452257707354557240087211123792674815)
