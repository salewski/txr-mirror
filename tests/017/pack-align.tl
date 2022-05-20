(load "../common")

(mtest
  (alignof int) 4
  (alignof (align int)) 16
  (alignof (align 1 int)) 4
  (alignof (align 6 int)) :error
  (alignof (align 8 int)) 8)

(mtest
  (alignof (pack int)) 1
  (alignof (pack 1 int)) 1
  (alignof (pack 6 int)) :error
  (alignof (pack 8 int)) 8)

(mtest
  (alignof (pack 1 (align 8 int))) 8
  (alignof (align 8 (pack 1 int))) 8)

(typedef s0 (pack (struct s0
                    (a char)
                    (b short)
                    (c int)
                    (d longlong))))

(mtest
  (alignof s0.a) 1
  (alignof s0.b) 1
  (alignof s0.c) 1
  (alignof s0.d) 1)

(mtest
  (offsetof s0 a) 0
  (offsetof s0 b) 1
  (offsetof s0 c) 3
  (offsetof s0 d) 7
  (sizeof s0) 15)

(typedef s1 (pack 2 (struct s1
                      (a char)
                      (b short)
                      (c int)
                      (d longlong))))

(mtest
  (alignof s1.a) 2
  (alignof s1.b) 2
  (alignof s1.c) 2
  (alignof s1.d) 2)

(mtest
  (offsetof s1 a) 0
  (offsetof s1 b) 2
  (offsetof s1 c) 4
  (offsetof s1 d) 8
  (sizeof s1) 16)

(typedef s2 (pack 32 (struct s2
                       (a char)
                       (b short)
                       (c int)
                       (d longlong))))

(mtest
  (alignof s2.a) 32
  (alignof s2.b) 32
  (alignof s2.c) 32
  (alignof s2.d) 32)

(mtest
  (offsetof s2 a) 0
  (offsetof s2 b) 32
  (offsetof s2 c) 64
  (offsetof s2 d) 96
  (sizeof s2) 128)

(typedef s3 (pack 1 (struct s3
                      (a char)
                      (b (align 2 char))
                      (c (align int))
                      (d longlong))))

(mtest
  (alignof s3.a) 1
  (alignof s3.b) 2
  (alignof s3.c) 16
  (alignof s3.d) 1)

(mtest
  (offsetof s3 a) 0
  (offsetof s3 b) 2
  (offsetof s3 c) 16
  (offsetof s3 d) 20
  (sizeof s3) 32)

(typedef s4 (align 256 s3))

(mtest
  (sizeof s4) 32
  (alignof s4) 256)

(typedef s5 (pack s3))

(mtest
  (sizeof s5) 32
  (alignof s5) 1)

(typedef s6 (pack (struct s3)))

(mtest
  (sizeof s6) 32
  (alignof s6) 1)
