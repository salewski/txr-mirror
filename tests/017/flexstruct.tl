(load "../common")

(typedef fs0 (struct fs0
               (a uint8)
               (b (array char))))

(mtest
  (sizeof fs0) 1
  (ffi-put #S(fs0 a 3 b "ABC") (ffi fs0)) #b'03414243'
  (ffi-get #b'03414243' (ffi fs0)) #S(fs0 a 3 b ""))

(defmeth fs0 length (s)
  s.a)

(mtest
  (ffi-get #b'03414243' (ffi fs0)) #S(fs0 a 3 b "ABC")
  (ffi-get #b'02e6bca2e5ad97' (ffi fs0)) #S(fs0 a 2 b "\xDCE6\xDCBC")
  (ffi-get #b'06e6bca2e5ad97' (ffi fs0)) #S(fs0 a 6 b "漢字"))

(typedef fs1 (struct fs1
               (a uint8)
               (b (zarray char))))

(mtest
  (sizeof fs1) 1
  (ffi-put #S(fs1 a 3 b "ABCDEF") (ffi fs1)) #b'0341424344454600'
  (ffi-get #b'FF41424300' (ffi fs1)) #S(fs1 a 255  b "ABC"))

(mtest
  (ffi-get #b'0341424300' (ffi fs1)) #S(fs1 a 3 b "ABC")
  (ffi-get #b'02e6bc00' (ffi fs1)) #S(fs1 a 2 b "\xDCE6\xDCBC")
  (ffi-get #b'06e6bca2e5ad9700' (ffi fs1)) #S(fs1 a 6 b "漢字"))

(typedef fs2 (struct fs2
               (a int8)
               (b (array int8))))

(mtest
  (sizeof fs2) 1
  (ffi-put #S(fs2 a 3 b "ABCD") (ffi fs2)) #b'0341424344'
  (ffi-put #S(fs2 a 3 b #(65 66 67 68)) (ffi fs2)) #b'0341424344'
  (ffi-get #b'FF414243' (ffi fs2)) #S(fs2 a 255  b #()))

(defmeth fs2 length (s)
  s.a)

(mtest
  (ffi-get #b'03010203' (ffi fs2)) #S(fs2 a 3 b #(1 2 3)))

(typedef fs3 (struct fs3
               (a int8)
               (b (array le-int16))))

(mtest
  (sizeof fs3) 2
  (ffi-put #S(fs3 a 3 b "ABCD") (ffi fs3)) #b'03004100420043004400'
  (ffi-put #S(fs3 a 3 b #(65 66 67 68)) (ffi fs3)) #b'03004100420043004400'
  (ffi-get #b'FF414243' (ffi fs3)) #S(fs3 a 255 b #()))

(defmeth fs3 length (s)
  s.a)

(mtest
  (ffi-get #b'0300010002000300' (ffi fs3)) #S(fs3 a 3 b #(1 2 3)))
