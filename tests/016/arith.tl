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
