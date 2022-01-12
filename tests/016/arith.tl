(load "../common.tl")

(for ((i 0) (j 1))
     ((< i 512))
     ((inc i) (inc j j))
  (let ((k (expt 2 i)))
    (vtest j k)))

(for ((h 0) (i 1) (j 1) (c 0))
     ((< c 100))
     ((inc c)
      (pset h i
            i j
            j (+ i j)))
  (vtest h (- j i)))

(defvarl p100 '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79
                83 89 97 101 103 107 109 113 127 131 137 139 149 151 157 163
                167 173 179 181 191 193 197 199 211 223 227 229 233 239 241 251
                257 263 269 271 277 281 283 293 307 311 313 317 331 337 347 349
                353 359 367 373 379 383 389 397 401 409 419 421 431 433 439 443
                449 457 461 463 467 479 487 491 499 503 509 521 523 541))

(vtest [reduce-left * p100] (lcm . p100))

(for ((prod [reduce-left * p100])
      (l100 p100))
     (l100)
     ((set prod (trunc prod (pop l100))))
  (vtest prod (lcm . l100)))

(each ((i (range 0 (length p100))))
  (let* ((a [apply * [p100 0..i]])
         (b [apply * [p100 i..:]])
         (c (* a b)))
    (vtest (trunc c a) b)
    (vtest (trunc c b) a)
    (vtest (mod c a) 0)
    (vtest (mod c b) 0)
    (vtest (mod (pred c) a) (pred a))
    (vtest (mod (pred c) b) (pred b))))

(test (digits 0) (0))
(test (digits 1) (1))
(test (digits 9) (9))
(test (digits 10) (1 0))
(test (digits 19) (1 9))
(test (digits 20) (2 0))
(test (digits 37) (3 7))
(test (digits 100) (1 0 0))
(test (digits 101) (1 0 1))
(test (digits 999) (9 9 9))
(test (digits 1000) (1 0 0 0))
(test (digits 0 2) (0))
(test (digits 1 2) (1))
(test (digits 2 2) (1 0))
(test (digits 3 2) (1 1))
(test (digits 7 2) (1 1 1))
(test (digits 8 2) (1 0 0 0))

(test (bracket 0 10 20 30) 0)
(test (bracket 9 10 20 30) 0)
(test (bracket 10 10 20 30) 1)
(test (bracket 15 10 20 30) 1)
(test (bracket 25 10 20 30) 2)
(test (bracket 30 10 20 30) 3)

(test [apply bracket '(0 10 20 30)] 0)
(test [apply bracket '(9 10 20 30)] 0)
(test [apply bracket '(10 10 20 30)] 1)
(test [apply bracket '(15 10 20 30)] 1)
(test [apply bracket '(25 10 20 30)] 2)
(test [apply bracket '(30 10 20 30)] 3)

(test (typeof fixnum-max) fixnum)
(test (typeof (succ fixnum-max)) bignum)
(test (typeof fixnum-min) fixnum)
(test (typeof (pred fixnum-min)) bignum)

(test (< fixnum-min fixnum-max) t)
(test (< (pred fixnum-min) fixnum-min) t)
(test (> (succ fixnum-max) fixnum-max) t)

(test (ffi-put #xA5 (ffi le-int16))
      #b'A500')
(test (ffi-put #xA5 (ffi be-int16))
      #b'00A5')
(test (mequal (ffi-put #xA5 (ffi int16))
              #b'A500'
              #b'00A5') t)

(test (ffi-put #xAABBCC (ffi le-int32))
      #b'CCBBAA00')
(test (ffi-put #xAABBCC (ffi be-int32))
      #b'00AABBCC')
(test (mequal (ffi-put #xAABBCC (ffi int32))
              #b'CCBBAA00'
              #b'00AABBCC') t)

(test (ffi-put #xAABBCCDDEE (ffi le-int64))
      #b'EEDDCCBBAA000000')
(test (ffi-put #xAABBCCDDEE (ffi be-int64))
      #b'000000AABBCCDDEE')
(test (mequal (ffi-put #xAABBCCDDEE (ffi int64))
              #b'EEDDCCBBAA000000'
              #b'000000AABBCCDDEE') t)

(test (ffi-get #b'A500' (ffi le-int16))
      #xA5)
(test (ffi-get #b'00A5' (ffi be-int16))
      #xA5)


(test (ffi-get #b'CCBBAA00' (ffi le-int32))
      #xAABBCC)
(test (ffi-get #b'00AABBCC' (ffi be-int32))
      #xAABBCC)


(test (ffi-get #b'EEDDCCBBAA000000' (ffi le-int64))
      #xAABBCCDDEE)
(test (ffi-get #b'000000AABBCCDDEE' (ffi be-int64))
      #xAABBCCDDEE)

(test (mequal (ffi-put #x-8000 (ffi int16))
              #b'0080'
              #b'8000') t)

(test (ffi-get (ffi-put #x-8000 (ffi int16)) (ffi int16))
      #x-8000)

(test (mequal (ffi-put #x-80000000 (ffi int32))
              #b'00000080'
              #b'80000000') t)

(test (ffi-get (ffi-put #x-80000000 (ffi int32)) (ffi int32))
      #x-80000000)

(test (mequal (ffi-put #x-8000000000000000 (ffi int64))
              #b'0000000000000080'
              #b'8000000000000000') t)

(test (ffi-get (ffi-put #x-8000000000000000 (ffi int64)) (ffi int64))
      #x-8000000000000000)

(mtest
  (sum #()) 0
  (sum #(1)) 1
  (sum #(1 2)) 3
  (sum #(1 2 3)) 6
  (sum #() (op * 10)) 0
  (sum #(1) (op * 10)) 10
  (sum #(1 2) (op * 10)) 30
  (sum #(1 2 3) (op * 10)) 60
  (sum 1..10) 45
  (sum 2..10) 44)

(mtest
  (prod #()) 1
  (prod #(1)) 1
  (prod #(1 2)) 2
  (prod #(1 2 3)) 6
  (prod #() (op * 10)) 1
  (prod #(1) (op * 10)) 10
  (prod #(1 2) (op * 10)) 200
  (prod #(1 2 3) (op * 10)) 6000
  (prod 2..8) 5040
  (prod 3..8) 2520)

(mtest
  (< 1 2) t
  (< 2 1) nil
  (< 1.0 2) t
  (< 2 1.0) nil
  (< #\c #\d) t
  (< #\d #\c) nil
  (< 1.0 1) nil
  (< #R(0 0) #R(0 0)) nil
  (< #R(0 0) #R(0 1)) t
  (< #R(0 0) #R(1 0)) t
  (< #R(0 0) #R(1 1)) t
  (< #R(1 0) #R(1 0)) nil
  (< #R(1 0) #R(1 1)) t
  (< 1 #R(1 0)) :error
  (< #R(1 0) 1) :error
  (< 1.0 #R(1 0)) :error
  (< #R(1 0) 1.0) :error
  (< #\c #R(1 0)) :error
  (< #R(1 0) #\c) :error
  (< 1 "abc") :error
  (< "abc" 1) :error
  (< 1 nil) :error
  (< nil 1) :error
  (< 1 '(1 2 3)) :error
  (< '(1 2 3) 1) :error
  (< 1 #(1 2 3)) :error
  (< #(1 2 3) 1) :error)

(mtest
  (< #\A 66 67.0) t
  (> 67.0 66 #\A) t
  (>= #\A 65.0 65) t)

(mtest
  (< "abc" "abc") nil
  (<= "abc" "abc") t
  (< "abc" "abcd") t
  (< "abc" "abd") t
  (< #(1 2 3) #(1 2 3)) nil
  (< #(1 2 3) #(1 2 3.0)) nil
  (< #(1 2 3) #(1 2 3 4)) t
  (< #(1 2 3) #(1 2 4)) t
  (< #(1 2 3) '(1 2 3)) nil
  (< #(1 2 3) '(1 2 3.0)) nil
  (< #(1 2 3) '(1 2 3 4)) t
  (< #(1 2 3) '(1 2 4)) t
  (< '(1 2 3) '(1 2 3)) nil
  (< '(1 2 3) '(1 2 3.0)) nil
  (< '(1 2 3) '(1 2 3 4)) t
  (< '(1 2 3) '(1 2 4)) t
  (< '(1 2 3) #(1 2 3)) nil
  (< '(1 2 3) #(1 2 3.0)) nil
  (< '(1 2 3) #(1 2 3 4)) t
  (< '(1 2 3) #(1 2 4)) t)

(let ((*print-flo-precision* 14))
  (sstest
    (let ((q (quantile 0.5)))
      [q 0.02 0.5 0.74 3.39 0.83]
      [mapcar q '(22.37 10.15 15.43 38.62 15.92
                  34.60 10.28 1.47 0.40 0.05 11.39
                  0.27 0.42 0.09 11.37)])
    (0.74 0.74 2.0616666666667 4.5517592592593 4.5517592592593 9.1519618055556
     9.1519618055556 9.1519618055556 9.1519618055556 6.1797614914021
     6.1797614914021 6.1797614914021 6.1797614914021 4.2462394088036
     4.2462394088036)))

(test
  (let ((q (quantile 0)))
    (cons [q] [mapcar q '(1 2 3 4 5)]))
  (0.0 1.0 1.5 2.0 2.5 3.0))

(test
  (let ((q (quantile 0 5 0.5)))
    [mapcar q '(1.0 2.0 3.0 4.0 5.0
                0.0 0.0 0.0 0.0 0.0)])
  (1.0 1.5 2.0 2.5 3.0
   1.5 0.75 0.375 0.1875 0.09375))

(test
  (let ((q (quantile 0 5 0.5)))
    [mapcar q '(0.0 0.0 0.0 0.0 0.0
                3.0 3.0 3.0 3.0 3.0)])
  (0.0 0.0 0.0 0.0 0.0
   1.5 2.25 2.625 2.8125 2.90625))

(mtest
  (sum-each ())
  0
  (sum-each ((x nil)))
  0
  (sum-each ((x '(1 2 3))
             (y '(4 5 6)))
    (* x y))
  32
  (mul-each ())
  1
  (mul-each ((x nil)))
  1
  (mul-each ((x '(1 2 3))
             (y '(4 5 6)))
    (+ x y))
  315
  (sum-each* ((x '(1 2 3))
              (y (cdr x)))
    (* x y))
  8
  (mul-each* ((x '(1 2 3))
              (y (cdr x)))
    (+ x y))
  15
  (sum-each ((x '(1 2 3))
              (y (cdr x)))
    (* x y))
  :error
  (mul-each ((x '(1 2 3))
              (y (cdr x)))
    (+ x y))
  :error)

(mtest
  (sum-each-prod ())
  0
  (sum-each-prod ((x nil)))
  0
  (sum-each-prod ((x '(4))) x)
  4
  (sum-each-prod ((x '(1 2 3))
                  (y '(4 3 2)))
    (* x y))
  54
  (sum-each-prod* ((x '(1 2 3 4))
                   (y (cdr x)))
    (* x y))
  90
  (sum-each-prod ((x '(1 2 3 4))
                  (y (cdr x)))
    (* x y))
  :error)

(mvtest
  (mul-each-prod ())
  1
  (mul-each-prod ((x nil)))
  1
  (mul-each-prod ((x '(4))) x)
  4
  (mul-each-prod ((x '(1 2 3))
                  (y '(4 3 2)))
    (+ x y))
  (* (+ 1 4) (+ 1 3) (+ 1 2)
     (+ 2 4) (+ 2 3) (+ 2 2)
     (+ 3 4) (+ 3 3) (+ 3 2))
  (mul-each-prod* ((x '(1 2 3))
                   (y (cdr x)))
    (+ x y))
  (* (+ 1 2) (+ 1 3)
     (+ 2 2) (+ 2 3)
     (+ 3 2) (+ 3 3))
  (sum-each-prod ((x '(1 2 3))
                  (y (cdr x)))
    (* x y))
  :error)
