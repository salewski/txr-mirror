(load "../common")

(let* ((bf (make-buf 16))
       (ca (carray-buf bf (ffi uchar))))
  (mtest
    (buf-put-buf bf (make-buf 8 255) 8) #b'ffffffffffffffff'
    bf #b'0000000000000000 ffffffffffffffff'
    (carray-set-length ca 8) nil
    (set [ca -1..10] #(1 2 3)) #(1 2 3)
    bf #b'0000000000000001 ffffffffffffffff'
    (set [ca 2..7] #(1 2 3)) #(1 2 3)
    bf #b'0000010203010000 ffffffffffffffff'
    (set [ca 3..4] #(10 11 12)) #(10 11 12)
    bf #b'0000010A0B0C0301 ffffffffffffffff'
    (set [ca 3..3] #(9)) #(9)
    bf #b'000001090A0B0C03 ffffffffffffffff'))
