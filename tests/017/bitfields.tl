(load "../common")

(defmacro conv-test (struct buf)
  (let ((type (typeof struct)))
    ^(mtest (sizeof ,type) ,(len buf)
            (ffi-put ,struct (ffi ,type)) ,buf
            (ffi-get ,buf (ffi ,type)) ,struct)))

(typedef s0 (struct s0
              (a (bit 1 be-uint32))
              (nil (bit 0 uint32))
              (b (bit 1 be-uint32))))

(conv-test #S(s0 a 1 b 1) #b'8000000080000000')

(typedef s1 (struct s1
              (nil (bit 0 uint32))
              (b (bit 1 be-uint32))))

(conv-test #S(s1 b 1) #b'80000000')

(typedef s2 (struct s2
              (a (bit 1 be-uint32))
              (nil (bit 0 uint32))))

(conv-test #S(s2 a 1) #b'80000000')

(typedef s3 (struct s3
              (b00 (bit 1 be-uint32)) (b01 (bit 1 be-uint32))
              (b02 (bit 1 be-uint32)) (b03 (bit 1 be-uint32))
              (b04 (bit 1 be-uint32)) (b05 (bit 1 be-uint32))
              (b06 (bit 1 be-uint32)) (b07 (bit 1 be-uint32))
              (b08 (bit 1 be-uint32)) (b09 (bit 1 be-uint32))
              (b10 (bit 1 be-uint32)) (b11 (bit 1 be-uint32))
              (b12 (bit 1 be-uint32)) (b13 (bit 1 be-uint32))
              (b14 (bit 1 be-uint32)) (b15 (bit 1 be-uint32))
              (b16 (bit 1 be-uint32)) (b17 (bit 1 be-uint32))
              (b18 (bit 1 be-uint32)) (b19 (bit 1 be-uint32))
              (b20 (bit 1 be-uint32)) (b21 (bit 1 be-uint32))
              (b22 (bit 1 be-uint32)) (b23 (bit 1 be-uint32))
              (b24 (bit 1 be-uint32)) (b25 (bit 1 be-uint32))
              (b26 (bit 1 be-uint32)) (b27 (bit 1 be-uint32))
              (b28 (bit 1 be-uint32)) (b29 (bit 1 be-uint32))
              (b30 (bit 1 be-uint32)) (b31 (bit 1 be-uint32))))

(conv-test #S(s3 b00 1 b01 1 b02 1 b03 1 b04 1 b05 1 b06 1 b07 1
                 b08 1 b09 1 b10 1 b11 1 b12 1 b13 1 b14 1 b15 1
                 b16 1 b17 1 b18 1 b19 1 b20 1 b21 1 b22 1 b23 1
                 b24 1 b25 1 b26 1 b27 1 b28 1 b29 1 b30 1 b31 1)
           #b'FFFFFFFF')

(conv-test #S(s3 b00 1 b01 0 b02 1 b03 0 b04 1 b05 0 b06 1 b07 0
                 b08 1 b09 0 b10 1 b11 0 b12 1 b13 0 b14 1 b15 0
                 b16 1 b17 0 b18 1 b19 0 b20 1 b21 0 b22 1 b23 0
                 b24 1 b25 0 b26 1 b27 0 b28 1 b29 0 b30 1 b31 0)
           #b'AAAAAAAA')

(conv-test #S(s3 b00 0 b01 1 b02 0 b03 1 b04 0 b05 1 b06 0 b07 1
                 b08 0 b09 1 b10 0 b11 1 b12 0 b13 1 b14 0 b15 1
                 b16 0 b17 1 b18 0 b19 1 b20 0 b21 1 b22 0 b23 1
                 b24 0 b25 1 b26 0 b27 1 b28 0 b29 1 b30 0 b31 1)
           #b'55555555')

(conv-test #S(s3 b00 0 b01 1 b02 0 b03 1 b04 0 b05 1 b06 0 b07 1
                 b08 0 b09 0 b10 0 b11 0 b12 0 b13 0 b14 0 b15 0
                 b16 0 b17 1 b18 0 b19 1 b20 0 b21 1 b22 0 b23 1
                 b24 0 b25 0 b26 0 b27 0 b28 0 b29 0 b30 0 b31 0)
           #b'55005500')

(conv-test #S(s3 b00 0 b01 0 b02 0 b03 0 b04 0 b05 0 b06 0 b07 0
                 b08 0 b09 0 b10 0 b11 0 b12 0 b13 0 b14 0 b15 0
                 b16 0 b17 0 b18 0 b19 0 b20 0 b21 0 b22 0 b23 0
                 b24 0 b25 0 b26 0 b27 0 b28 0 b29 0 b30 0 b31 0)
           #b'00000000')

(conv-test #S(s3 b00 1 b01 0 b02 0 b03 0 b04 0 b05 0 b06 0 b07 0
                 b08 1 b09 0 b10 0 b11 0 b12 0 b13 0 b14 0 b15 0
                 b16 1 b17 0 b18 0 b19 0 b20 0 b21 0 b22 0 b23 0
                 b24 1 b25 0 b26 0 b27 0 b28 0 b29 0 b30 0 b31 0)
           #b'80808080')

(conv-test #S(s3 b00 0 b01 1 b02 0 b03 0 b04 0 b05 0 b06 0 b07 0
                 b08 0 b09 1 b10 0 b11 0 b12 0 b13 0 b14 0 b15 0
                 b16 0 b17 1 b18 0 b19 0 b20 0 b21 0 b22 0 b23 0
                 b24 0 b25 1 b26 0 b27 0 b28 0 b29 0 b30 0 b31 0)
           #b'40404040')

(conv-test #S(s3 b00 0 b01 0 b02 1 b03 0 b04 0 b05 0 b06 0 b07 0
                 b08 0 b09 0 b10 1 b11 0 b12 0 b13 0 b14 0 b15 0
                 b16 0 b17 0 b18 1 b19 0 b20 0 b21 0 b22 0 b23 0
                 b24 0 b25 0 b26 1 b27 0 b28 0 b29 0 b30 0 b31 0)
           #b'20202020')

(conv-test #S(s3 b00 0 b01 0 b02 0 b03 1 b04 0 b05 0 b06 0 b07 0
                 b08 0 b09 0 b10 0 b11 1 b12 0 b13 0 b14 0 b15 0
                 b16 0 b17 0 b18 0 b19 1 b20 0 b21 0 b22 0 b23 0
                 b24 0 b25 0 b26 0 b27 1 b28 0 b29 0 b30 0 b31 0)
           #b'10101010')

(conv-test #S(s3 b00 0 b01 0 b02 0 b03 0 b04 1 b05 0 b06 0 b07 0
                 b08 0 b09 0 b10 0 b11 0 b12 1 b13 0 b14 0 b15 0
                 b16 0 b17 0 b18 0 b19 0 b20 1 b21 0 b22 0 b23 0
                 b24 0 b25 0 b26 0 b27 0 b28 1 b29 0 b30 0 b31 0)
           #b'08080808')

(conv-test #S(s3 b00 0 b01 0 b02 0 b03 0 b04 0 b05 1 b06 0 b07 0
                 b08 0 b09 0 b10 0 b11 0 b12 0 b13 1 b14 0 b15 0
                 b16 0 b17 0 b18 0 b19 0 b20 0 b21 1 b22 0 b23 0
                 b24 0 b25 0 b26 0 b27 0 b28 0 b29 1 b30 0 b31 0)
           #b'04040404')

(conv-test #S(s3 b00 0 b01 0 b02 0 b03 0 b04 0 b05 0 b06 1 b07 0
                 b08 0 b09 0 b10 0 b11 0 b12 0 b13 0 b14 1 b15 0
                 b16 0 b17 0 b18 0 b19 0 b20 0 b21 0 b22 1 b23 0
                 b24 0 b25 0 b26 0 b27 0 b28 0 b29 0 b30 1 b31 0)
           #b'02020202')

(conv-test #S(s3 b00 0 b01 0 b02 0 b03 0 b04 0 b05 0 b06 0 b07 1
                 b08 0 b09 0 b10 0 b11 0 b12 0 b13 0 b14 0 b15 1
                 b16 0 b17 0 b18 0 b19 0 b20 0 b21 0 b22 0 b23 1
                 b24 0 b25 0 b26 0 b27 0 b28 0 b29 0 b30 0 b31 1)
           #b'01010101')

(conv-test #S(s3 b00 1 b01 0 b02 0 b03 0 b04 0 b05 0 b06 0 b07 1
                 b08 0 b09 1 b10 0 b11 0 b12 0 b13 0 b14 1 b15 0
                 b16 0 b17 0 b18 1 b19 0 b20 0 b21 1 b22 0 b23 0
                 b24 0 b25 0 b26 0 b27 1 b28 1 b29 0 b30 0 b31 0)
           #b'81422418')

(conv-test #S(s3 b00 0 b01 0 b02 0 b03 1 b04 1 b05 0 b06 0 b07 0
                 b08 0 b09 0 b10 1 b11 0 b12 0 b13 1 b14 0 b15 0
                 b16 0 b17 1 b18 0 b19 0 b20 0 b21 0 b22 1 b23 0
                 b24 1 b25 0 b26 0 b27 0 b28 0 b29 0 b30 0 b31 1)
           #b'18244281')

(typedef s4 (struct s4
              (b00 (bit 1 le-uint32)) (b01 (bit 1 le-uint32))
              (b02 (bit 1 le-uint32)) (b03 (bit 1 le-uint32))
              (b04 (bit 1 le-uint32)) (b05 (bit 1 le-uint32))
              (b06 (bit 1 le-uint32)) (b07 (bit 1 le-uint32))
              (b08 (bit 1 le-uint32)) (b09 (bit 1 le-uint32))
              (b10 (bit 1 le-uint32)) (b11 (bit 1 le-uint32))
              (b12 (bit 1 le-uint32)) (b13 (bit 1 le-uint32))
              (b14 (bit 1 le-uint32)) (b15 (bit 1 le-uint32))
              (b16 (bit 1 le-uint32)) (b17 (bit 1 le-uint32))
              (b18 (bit 1 le-uint32)) (b19 (bit 1 le-uint32))
              (b20 (bit 1 le-uint32)) (b21 (bit 1 le-uint32))
              (b22 (bit 1 le-uint32)) (b23 (bit 1 le-uint32))
              (b24 (bit 1 le-uint32)) (b25 (bit 1 le-uint32))
              (b26 (bit 1 le-uint32)) (b27 (bit 1 le-uint32))
              (b28 (bit 1 le-uint32)) (b29 (bit 1 le-uint32))
              (b30 (bit 1 le-uint32)) (b31 (bit 1 le-uint32))))

(conv-test #S(s4 b00 1 b01 1 b02 1 b03 1 b04 1 b05 1 b06 1 b07 1
                 b08 1 b09 1 b10 1 b11 1 b12 1 b13 1 b14 1 b15 1
                 b16 1 b17 1 b18 1 b19 1 b20 1 b21 1 b22 1 b23 1
                 b24 1 b25 1 b26 1 b27 1 b28 1 b29 1 b30 1 b31 1)
           #b'FFFFFFFF')

(conv-test #S(s4 b00 1 b01 0 b02 1 b03 0 b04 1 b05 0 b06 1 b07 0
                 b08 1 b09 0 b10 1 b11 0 b12 1 b13 0 b14 1 b15 0
                 b16 1 b17 0 b18 1 b19 0 b20 1 b21 0 b22 1 b23 0
                 b24 1 b25 0 b26 1 b27 0 b28 1 b29 0 b30 1 b31 0)
           #b'55555555')

(conv-test #S(s4 b00 0 b01 1 b02 0 b03 1 b04 0 b05 1 b06 0 b07 1
                 b08 0 b09 1 b10 0 b11 1 b12 0 b13 1 b14 0 b15 1
                 b16 0 b17 1 b18 0 b19 1 b20 0 b21 1 b22 0 b23 1
                 b24 0 b25 1 b26 0 b27 1 b28 0 b29 1 b30 0 b31 1)
           #b'AAAAAAAA')

(conv-test #S(s4 b00 0 b01 1 b02 0 b03 1 b04 0 b05 1 b06 0 b07 1
                 b08 0 b09 0 b10 0 b11 0 b12 0 b13 0 b14 0 b15 0
                 b16 0 b17 1 b18 0 b19 1 b20 0 b21 1 b22 0 b23 1
                 b24 0 b25 0 b26 0 b27 0 b28 0 b29 0 b30 0 b31 0)
           #b'AA00AA00')

(conv-test #S(s4 b00 0 b01 0 b02 0 b03 0 b04 0 b05 0 b06 0 b07 0
                 b08 0 b09 0 b10 0 b11 0 b12 0 b13 0 b14 0 b15 0
                 b16 0 b17 0 b18 0 b19 0 b20 0 b21 0 b22 0 b23 0
                 b24 0 b25 0 b26 0 b27 0 b28 0 b29 0 b30 0 b31 0)
           #b'00000000')

(conv-test #S(s4 b00 1 b01 0 b02 0 b03 0 b04 0 b05 0 b06 0 b07 0
                 b08 1 b09 0 b10 0 b11 0 b12 0 b13 0 b14 0 b15 0
                 b16 1 b17 0 b18 0 b19 0 b20 0 b21 0 b22 0 b23 0
                 b24 1 b25 0 b26 0 b27 0 b28 0 b29 0 b30 0 b31 0)
           #b'01010101')

(conv-test #S(s4 b00 0 b01 1 b02 0 b03 0 b04 0 b05 0 b06 0 b07 0
                 b08 0 b09 1 b10 0 b11 0 b12 0 b13 0 b14 0 b15 0
                 b16 0 b17 1 b18 0 b19 0 b20 0 b21 0 b22 0 b23 0
                 b24 0 b25 1 b26 0 b27 0 b28 0 b29 0 b30 0 b31 0)
           #b'02020202')

(conv-test #S(s4 b00 0 b01 0 b02 1 b03 0 b04 0 b05 0 b06 0 b07 0
                 b08 0 b09 0 b10 1 b11 0 b12 0 b13 0 b14 0 b15 0
                 b16 0 b17 0 b18 1 b19 0 b20 0 b21 0 b22 0 b23 0
                 b24 0 b25 0 b26 1 b27 0 b28 0 b29 0 b30 0 b31 0)
           #b'04040404')

(conv-test #S(s4 b00 0 b01 0 b02 0 b03 1 b04 0 b05 0 b06 0 b07 0
                 b08 0 b09 0 b10 0 b11 1 b12 0 b13 0 b14 0 b15 0
                 b16 0 b17 0 b18 0 b19 1 b20 0 b21 0 b22 0 b23 0
                 b24 0 b25 0 b26 0 b27 1 b28 0 b29 0 b30 0 b31 0)
           #b'08080808')

(conv-test #S(s4 b00 0 b01 0 b02 0 b03 0 b04 1 b05 0 b06 0 b07 0
                 b08 0 b09 0 b10 0 b11 0 b12 1 b13 0 b14 0 b15 0
                 b16 0 b17 0 b18 0 b19 0 b20 1 b21 0 b22 0 b23 0
                 b24 0 b25 0 b26 0 b27 0 b28 1 b29 0 b30 0 b31 0)
           #b'10101010')

(conv-test #S(s4 b00 0 b01 0 b02 0 b03 0 b04 0 b05 1 b06 0 b07 0
                 b08 0 b09 0 b10 0 b11 0 b12 0 b13 1 b14 0 b15 0
                 b16 0 b17 0 b18 0 b19 0 b20 0 b21 1 b22 0 b23 0
                 b24 0 b25 0 b26 0 b27 0 b28 0 b29 1 b30 0 b31 0)
           #b'20202020')

(conv-test #S(s4 b00 0 b01 0 b02 0 b03 0 b04 0 b05 0 b06 1 b07 0
                 b08 0 b09 0 b10 0 b11 0 b12 0 b13 0 b14 1 b15 0
                 b16 0 b17 0 b18 0 b19 0 b20 0 b21 0 b22 1 b23 0
                 b24 0 b25 0 b26 0 b27 0 b28 0 b29 0 b30 1 b31 0)
           #b'40404040')

(conv-test #S(s4 b00 0 b01 0 b02 0 b03 0 b04 0 b05 0 b06 0 b07 1
                 b08 0 b09 0 b10 0 b11 0 b12 0 b13 0 b14 0 b15 1
                 b16 0 b17 0 b18 0 b19 0 b20 0 b21 0 b22 0 b23 1
                 b24 0 b25 0 b26 0 b27 0 b28 0 b29 0 b30 0 b31 1)
           #b'80808080')

(conv-test #S(s4 b00 1 b01 0 b02 0 b03 0 b04 0 b05 0 b06 0 b07 1
                 b08 0 b09 1 b10 0 b11 0 b12 0 b13 0 b14 1 b15 0
                 b16 0 b17 0 b18 1 b19 0 b20 0 b21 1 b22 0 b23 0
                 b24 0 b25 0 b26 0 b27 1 b28 1 b29 0 b30 0 b31 0)
           #b'81422418')

(conv-test #S(s4 b00 0 b01 0 b02 0 b03 1 b04 1 b05 0 b06 0 b07 0
                 b08 0 b09 0 b10 1 b11 0 b12 0 b13 1 b14 0 b15 0
                 b16 0 b17 1 b18 0 b19 0 b20 0 b21 0 b22 1 b23 0
                 b24 1 b25 0 b26 0 b27 0 b28 0 b29 0 b30 0 b31 1)
           #b'18244281')

(typedef s5 (struct s5
              (b00 (bit 1 be-uint64)) (b01 (bit 1 be-uint64))
              (b02 (bit 1 be-uint64)) (b03 (bit 1 be-uint64))
              (b04 (bit 1 be-uint64)) (b05 (bit 1 be-uint64))
              (b06 (bit 1 be-uint64)) (b07 (bit 1 be-uint64))
              (b08 (bit 1 be-uint64)) (b09 (bit 1 be-uint64))
              (b10 (bit 1 be-uint64)) (b11 (bit 1 be-uint64))
              (b12 (bit 1 be-uint64)) (b13 (bit 1 be-uint64))
              (b14 (bit 1 be-uint64)) (b15 (bit 1 be-uint64))
              (b16 (bit 1 be-uint64)) (b17 (bit 1 be-uint64))
              (b18 (bit 1 be-uint64)) (b19 (bit 1 be-uint64))
              (b20 (bit 1 be-uint64)) (b21 (bit 1 be-uint64))
              (b22 (bit 1 be-uint64)) (b23 (bit 1 be-uint64))
              (b24 (bit 1 be-uint64)) (b25 (bit 1 be-uint64))
              (b26 (bit 1 be-uint64)) (b27 (bit 1 be-uint64))
              (b28 (bit 1 be-uint64)) (b29 (bit 1 be-uint64))
              (b30 (bit 1 be-uint64)) (b31 (bit 1 be-uint64))
              (b32 (bit 1 be-uint64)) (b33 (bit 1 be-uint64))
              (b34 (bit 1 be-uint64)) (b35 (bit 1 be-uint64))
              (b36 (bit 1 be-uint64)) (b37 (bit 1 be-uint64))
              (b38 (bit 1 be-uint64)) (b39 (bit 1 be-uint64))
              (b40 (bit 1 be-uint64)) (b41 (bit 1 be-uint64))
              (b42 (bit 1 be-uint64)) (b43 (bit 1 be-uint64))
              (b44 (bit 1 be-uint64)) (b45 (bit 1 be-uint64))
              (b46 (bit 1 be-uint64)) (b47 (bit 1 be-uint64))
              (b48 (bit 1 be-uint64)) (b49 (bit 1 be-uint64))
              (b50 (bit 1 be-uint64)) (b51 (bit 1 be-uint64))
              (b52 (bit 1 be-uint64)) (b53 (bit 1 be-uint64))
              (b54 (bit 1 be-uint64)) (b55 (bit 1 be-uint64))
              (b56 (bit 1 be-uint64)) (b57 (bit 1 be-uint64))
              (b58 (bit 1 be-uint64)) (b59 (bit 1 be-uint64))
              (b60 (bit 1 be-uint64)) (b61 (bit 1 be-uint64))
              (b62 (bit 1 be-uint64)) (b63 (bit 1 be-uint64))))

(conv-test #S(s5 b00 0 b01 0 b02 0 b03 0 b04 0 b05 0 b06 0 b07 0
                 b08 0 b09 0 b10 0 b11 0 b12 0 b13 0 b14 0 b15 0
                 b16 0 b17 0 b18 0 b19 0 b20 0 b21 0 b22 0 b23 0
                 b24 0 b25 0 b26 0 b27 0 b28 0 b29 0 b30 0 b31 0
                 b32 0 b33 0 b34 0 b35 0 b36 0 b37 0 b38 0 b39 0
                 b40 0 b41 0 b42 0 b43 0 b44 0 b45 0 b46 0 b47 0
                 b48 0 b49 0 b50 0 b51 0 b52 0 b53 0 b54 0 b55 0
                 b56 0 b57 0 b58 0 b59 0 b60 0 b61 0 b62 0 b63 0)
           #b'0000000000000000')

(conv-test #S(s5 b00 1 b01 0 b02 0 b03 0 b04 0 b05 0 b06 0 b07 0
                 b08 1 b09 0 b10 0 b11 0 b12 0 b13 0 b14 0 b15 0
                 b16 1 b17 0 b18 0 b19 0 b20 0 b21 0 b22 0 b23 0
                 b24 1 b25 0 b26 0 b27 0 b28 0 b29 0 b30 0 b31 0
                 b32 1 b33 0 b34 0 b35 0 b36 0 b37 0 b38 0 b39 0
                 b40 1 b41 0 b42 0 b43 0 b44 0 b45 0 b46 0 b47 0
                 b48 1 b49 0 b50 0 b51 0 b52 0 b53 0 b54 0 b55 0
                 b56 1 b57 0 b58 0 b59 0 b60 0 b61 0 b62 0 b63 0)
           #b'8080808080808080')

(conv-test #S(s5 b00 0 b01 1 b02 0 b03 0 b04 0 b05 0 b06 0 b07 0
                 b08 0 b09 1 b10 0 b11 0 b12 0 b13 0 b14 0 b15 0
                 b16 0 b17 1 b18 0 b19 0 b20 0 b21 0 b22 0 b23 0
                 b24 0 b25 1 b26 0 b27 0 b28 0 b29 0 b30 0 b31 0
                 b32 0 b33 1 b34 0 b35 0 b36 0 b37 0 b38 0 b39 0
                 b40 0 b41 1 b42 0 b43 0 b44 0 b45 0 b46 0 b47 0
                 b48 0 b49 1 b50 0 b51 0 b52 0 b53 0 b54 0 b55 0
                 b56 0 b57 1 b58 0 b59 0 b60 0 b61 0 b62 0 b63 0)
           #b'4040404040404040')

(conv-test #S(s5 b00 0 b01 0 b02 1 b03 0 b04 0 b05 0 b06 0 b07 0
                 b08 0 b09 0 b10 1 b11 0 b12 0 b13 0 b14 0 b15 0
                 b16 0 b17 0 b18 1 b19 0 b20 0 b21 0 b22 0 b23 0
                 b24 0 b25 0 b26 1 b27 0 b28 0 b29 0 b30 0 b31 0
                 b32 0 b33 0 b34 1 b35 0 b36 0 b37 0 b38 0 b39 0
                 b40 0 b41 0 b42 1 b43 0 b44 0 b45 0 b46 0 b47 0
                 b48 0 b49 0 b50 1 b51 0 b52 0 b53 0 b54 0 b55 0
                 b56 0 b57 0 b58 1 b59 0 b60 0 b61 0 b62 0 b63 0)
           #b'2020202020202020')

(conv-test #S(s5 b00 0 b01 0 b02 0 b03 1 b04 0 b05 0 b06 0 b07 0
                 b08 0 b09 0 b10 0 b11 1 b12 0 b13 0 b14 0 b15 0
                 b16 0 b17 0 b18 0 b19 1 b20 0 b21 0 b22 0 b23 0
                 b24 0 b25 0 b26 0 b27 1 b28 0 b29 0 b30 0 b31 0
                 b32 0 b33 0 b34 0 b35 1 b36 0 b37 0 b38 0 b39 0
                 b40 0 b41 0 b42 0 b43 1 b44 0 b45 0 b46 0 b47 0
                 b48 0 b49 0 b50 0 b51 1 b52 0 b53 0 b54 0 b55 0
                 b56 0 b57 0 b58 0 b59 1 b60 0 b61 0 b62 0 b63 0)
           #b'1010101010101010')

(conv-test #S(s5 b00 0 b01 0 b02 0 b03 0 b04 1 b05 0 b06 0 b07 0
                 b08 0 b09 0 b10 0 b11 0 b12 1 b13 0 b14 0 b15 0
                 b16 0 b17 0 b18 0 b19 0 b20 1 b21 0 b22 0 b23 0
                 b24 0 b25 0 b26 0 b27 0 b28 1 b29 0 b30 0 b31 0
                 b32 0 b33 0 b34 0 b35 0 b36 1 b37 0 b38 0 b39 0
                 b40 0 b41 0 b42 0 b43 0 b44 1 b45 0 b46 0 b47 0
                 b48 0 b49 0 b50 0 b51 0 b52 1 b53 0 b54 0 b55 0
                 b56 0 b57 0 b58 0 b59 0 b60 1 b61 0 b62 0 b63 0)
           #b'0808080808080808')

(conv-test #S(s5 b00 0 b01 0 b02 0 b03 0 b04 0 b05 1 b06 0 b07 0
                 b08 0 b09 0 b10 0 b11 0 b12 0 b13 1 b14 0 b15 0
                 b16 0 b17 0 b18 0 b19 0 b20 0 b21 1 b22 0 b23 0
                 b24 0 b25 0 b26 0 b27 0 b28 0 b29 1 b30 0 b31 0
                 b32 0 b33 0 b34 0 b35 0 b36 0 b37 1 b38 0 b39 0
                 b40 0 b41 0 b42 0 b43 0 b44 0 b45 1 b46 0 b47 0
                 b48 0 b49 0 b50 0 b51 0 b52 0 b53 1 b54 0 b55 0
                 b56 0 b57 0 b58 0 b59 0 b60 0 b61 1 b62 0 b63 0)
           #b'0404040404040404')

(conv-test #S(s5 b00 0 b01 0 b02 0 b03 0 b04 0 b05 0 b06 1 b07 0
                 b08 0 b09 0 b10 0 b11 0 b12 0 b13 0 b14 1 b15 0
                 b16 0 b17 0 b18 0 b19 0 b20 0 b21 0 b22 1 b23 0
                 b24 0 b25 0 b26 0 b27 0 b28 0 b29 0 b30 1 b31 0
                 b32 0 b33 0 b34 0 b35 0 b36 0 b37 0 b38 1 b39 0
                 b40 0 b41 0 b42 0 b43 0 b44 0 b45 0 b46 1 b47 0
                 b48 0 b49 0 b50 0 b51 0 b52 0 b53 0 b54 1 b55 0
                 b56 0 b57 0 b58 0 b59 0 b60 0 b61 0 b62 1 b63 0)
           #b'0202020202020202')

(conv-test #S(s5 b00 0 b01 0 b02 0 b03 0 b04 0 b05 0 b06 0 b07 1
                 b08 0 b09 0 b10 0 b11 0 b12 0 b13 0 b14 0 b15 1
                 b16 0 b17 0 b18 0 b19 0 b20 0 b21 0 b22 0 b23 1
                 b24 0 b25 0 b26 0 b27 0 b28 0 b29 0 b30 0 b31 1
                 b32 0 b33 0 b34 0 b35 0 b36 0 b37 0 b38 0 b39 1
                 b40 0 b41 0 b42 0 b43 0 b44 0 b45 0 b46 0 b47 1
                 b48 0 b49 0 b50 0 b51 0 b52 0 b53 0 b54 0 b55 1
                 b56 0 b57 0 b58 0 b59 0 b60 0 b61 0 b62 0 b63 1)
           #b'0101010101010101')

(conv-test #S(s5 b00 1 b01 0 b02 0 b03 0 b04 0 b05 0 b06 0 b07 0
                 b08 0 b09 1 b10 0 b11 0 b12 0 b13 0 b14 0 b15 0
                 b16 0 b17 0 b18 1 b19 0 b20 0 b21 0 b22 0 b23 0
                 b24 0 b25 0 b26 0 b27 1 b28 0 b29 0 b30 0 b31 0
                 b32 0 b33 0 b34 0 b35 0 b36 1 b37 0 b38 0 b39 0
                 b40 0 b41 0 b42 0 b43 0 b44 0 b45 1 b46 0 b47 0
                 b48 0 b49 0 b50 0 b51 0 b52 0 b53 0 b54 1 b55 0
                 b56 0 b57 0 b58 0 b59 0 b60 0 b61 0 b62 0 b63 1)
           #b'8040201008040201')

(conv-test #S(s5 b00 0 b01 0 b02 0 b03 0 b04 0 b05 0 b06 0 b07 1
                 b08 0 b09 0 b10 0 b11 0 b12 0 b13 0 b14 1 b15 0
                 b16 0 b17 0 b18 0 b19 0 b20 0 b21 1 b22 0 b23 0
                 b24 0 b25 0 b26 0 b27 0 b28 1 b29 0 b30 0 b31 0
                 b32 0 b33 0 b34 0 b35 1 b36 0 b37 0 b38 0 b39 0
                 b40 0 b41 0 b42 1 b43 0 b44 0 b45 0 b46 0 b47 0
                 b48 0 b49 1 b50 0 b51 0 b52 0 b53 0 b54 0 b55 0
                 b56 1 b57 0 b58 0 b59 0 b60 0 b61 0 b62 0 b63 0)
           #b'0102040810204080')

(typedef s6 (struct s6
              (b00 (bit 1 le-uint64)) (b01 (bit 1 le-uint64))
              (b02 (bit 1 le-uint64)) (b03 (bit 1 le-uint64))
              (b04 (bit 1 le-uint64)) (b05 (bit 1 le-uint64))
              (b06 (bit 1 le-uint64)) (b07 (bit 1 le-uint64))
              (b08 (bit 1 le-uint64)) (b09 (bit 1 le-uint64))
              (b10 (bit 1 le-uint64)) (b11 (bit 1 le-uint64))
              (b12 (bit 1 le-uint64)) (b13 (bit 1 le-uint64))
              (b14 (bit 1 le-uint64)) (b15 (bit 1 le-uint64))
              (b16 (bit 1 le-uint64)) (b17 (bit 1 le-uint64))
              (b18 (bit 1 le-uint64)) (b19 (bit 1 le-uint64))
              (b20 (bit 1 le-uint64)) (b21 (bit 1 le-uint64))
              (b22 (bit 1 le-uint64)) (b23 (bit 1 le-uint64))
              (b24 (bit 1 le-uint64)) (b25 (bit 1 le-uint64))
              (b26 (bit 1 le-uint64)) (b27 (bit 1 le-uint64))
              (b28 (bit 1 le-uint64)) (b29 (bit 1 le-uint64))
              (b30 (bit 1 le-uint64)) (b31 (bit 1 le-uint64))
              (b32 (bit 1 le-uint64)) (b33 (bit 1 le-uint64))
              (b34 (bit 1 le-uint64)) (b35 (bit 1 le-uint64))
              (b36 (bit 1 le-uint64)) (b37 (bit 1 le-uint64))
              (b38 (bit 1 le-uint64)) (b39 (bit 1 le-uint64))
              (b40 (bit 1 le-uint64)) (b41 (bit 1 le-uint64))
              (b42 (bit 1 le-uint64)) (b43 (bit 1 le-uint64))
              (b44 (bit 1 le-uint64)) (b45 (bit 1 le-uint64))
              (b46 (bit 1 le-uint64)) (b47 (bit 1 le-uint64))
              (b48 (bit 1 le-uint64)) (b49 (bit 1 le-uint64))
              (b50 (bit 1 le-uint64)) (b51 (bit 1 le-uint64))
              (b52 (bit 1 le-uint64)) (b53 (bit 1 le-uint64))
              (b54 (bit 1 le-uint64)) (b55 (bit 1 le-uint64))
              (b56 (bit 1 le-uint64)) (b57 (bit 1 le-uint64))
              (b58 (bit 1 le-uint64)) (b59 (bit 1 le-uint64))
              (b60 (bit 1 le-uint64)) (b61 (bit 1 le-uint64))
              (b62 (bit 1 le-uint64)) (b63 (bit 1 le-uint64))))

(conv-test #S(s6 b00 0 b01 0 b02 0 b03 0 b04 0 b05 0 b06 0 b07 0
                 b08 0 b09 0 b10 0 b11 0 b12 0 b13 0 b14 0 b15 0
                 b16 0 b17 0 b18 0 b19 0 b20 0 b21 0 b22 0 b23 0
                 b24 0 b25 0 b26 0 b27 0 b28 0 b29 0 b30 0 b31 0
                 b32 0 b33 0 b34 0 b35 0 b36 0 b37 0 b38 0 b39 0
                 b40 0 b41 0 b42 0 b43 0 b44 0 b45 0 b46 0 b47 0
                 b48 0 b49 0 b50 0 b51 0 b52 0 b53 0 b54 0 b55 0
                 b56 0 b57 0 b58 0 b59 0 b60 0 b61 0 b62 0 b63 0)
           #b'0000000000000000')

(conv-test #S(s6 b00 1 b01 0 b02 0 b03 0 b04 0 b05 0 b06 0 b07 0
                 b08 1 b09 0 b10 0 b11 0 b12 0 b13 0 b14 0 b15 0
                 b16 1 b17 0 b18 0 b19 0 b20 0 b21 0 b22 0 b23 0
                 b24 1 b25 0 b26 0 b27 0 b28 0 b29 0 b30 0 b31 0
                 b32 1 b33 0 b34 0 b35 0 b36 0 b37 0 b38 0 b39 0
                 b40 1 b41 0 b42 0 b43 0 b44 0 b45 0 b46 0 b47 0
                 b48 1 b49 0 b50 0 b51 0 b52 0 b53 0 b54 0 b55 0
                 b56 1 b57 0 b58 0 b59 0 b60 0 b61 0 b62 0 b63 0)
           #b'0101010101010101')

(conv-test #S(s6 b00 0 b01 1 b02 0 b03 0 b04 0 b05 0 b06 0 b07 0
                 b08 0 b09 1 b10 0 b11 0 b12 0 b13 0 b14 0 b15 0
                 b16 0 b17 1 b18 0 b19 0 b20 0 b21 0 b22 0 b23 0
                 b24 0 b25 1 b26 0 b27 0 b28 0 b29 0 b30 0 b31 0
                 b32 0 b33 1 b34 0 b35 0 b36 0 b37 0 b38 0 b39 0
                 b40 0 b41 1 b42 0 b43 0 b44 0 b45 0 b46 0 b47 0
                 b48 0 b49 1 b50 0 b51 0 b52 0 b53 0 b54 0 b55 0
                 b56 0 b57 1 b58 0 b59 0 b60 0 b61 0 b62 0 b63 0)
           #b'0202020202020202')

(conv-test #S(s6 b00 0 b01 0 b02 1 b03 0 b04 0 b05 0 b06 0 b07 0
                 b08 0 b09 0 b10 1 b11 0 b12 0 b13 0 b14 0 b15 0
                 b16 0 b17 0 b18 1 b19 0 b20 0 b21 0 b22 0 b23 0
                 b24 0 b25 0 b26 1 b27 0 b28 0 b29 0 b30 0 b31 0
                 b32 0 b33 0 b34 1 b35 0 b36 0 b37 0 b38 0 b39 0
                 b40 0 b41 0 b42 1 b43 0 b44 0 b45 0 b46 0 b47 0
                 b48 0 b49 0 b50 1 b51 0 b52 0 b53 0 b54 0 b55 0
                 b56 0 b57 0 b58 1 b59 0 b60 0 b61 0 b62 0 b63 0)
           #b'0404040404040404')

(conv-test #S(s6 b00 0 b01 0 b02 0 b03 1 b04 0 b05 0 b06 0 b07 0
                 b08 0 b09 0 b10 0 b11 1 b12 0 b13 0 b14 0 b15 0
                 b16 0 b17 0 b18 0 b19 1 b20 0 b21 0 b22 0 b23 0
                 b24 0 b25 0 b26 0 b27 1 b28 0 b29 0 b30 0 b31 0
                 b32 0 b33 0 b34 0 b35 1 b36 0 b37 0 b38 0 b39 0
                 b40 0 b41 0 b42 0 b43 1 b44 0 b45 0 b46 0 b47 0
                 b48 0 b49 0 b50 0 b51 1 b52 0 b53 0 b54 0 b55 0
                 b56 0 b57 0 b58 0 b59 1 b60 0 b61 0 b62 0 b63 0)
           #b'0808080808080808')

(conv-test #S(s6 b00 0 b01 0 b02 0 b03 0 b04 1 b05 0 b06 0 b07 0
                 b08 0 b09 0 b10 0 b11 0 b12 1 b13 0 b14 0 b15 0
                 b16 0 b17 0 b18 0 b19 0 b20 1 b21 0 b22 0 b23 0
                 b24 0 b25 0 b26 0 b27 0 b28 1 b29 0 b30 0 b31 0
                 b32 0 b33 0 b34 0 b35 0 b36 1 b37 0 b38 0 b39 0
                 b40 0 b41 0 b42 0 b43 0 b44 1 b45 0 b46 0 b47 0
                 b48 0 b49 0 b50 0 b51 0 b52 1 b53 0 b54 0 b55 0
                 b56 0 b57 0 b58 0 b59 0 b60 1 b61 0 b62 0 b63 0)
           #b'1010101010101010')

(conv-test #S(s6 b00 0 b01 0 b02 0 b03 0 b04 0 b05 1 b06 0 b07 0
                 b08 0 b09 0 b10 0 b11 0 b12 0 b13 1 b14 0 b15 0
                 b16 0 b17 0 b18 0 b19 0 b20 0 b21 1 b22 0 b23 0
                 b24 0 b25 0 b26 0 b27 0 b28 0 b29 1 b30 0 b31 0
                 b32 0 b33 0 b34 0 b35 0 b36 0 b37 1 b38 0 b39 0
                 b40 0 b41 0 b42 0 b43 0 b44 0 b45 1 b46 0 b47 0
                 b48 0 b49 0 b50 0 b51 0 b52 0 b53 1 b54 0 b55 0
                 b56 0 b57 0 b58 0 b59 0 b60 0 b61 1 b62 0 b63 0)
           #b'2020202020202020')

(conv-test #S(s6 b00 0 b01 0 b02 0 b03 0 b04 0 b05 0 b06 1 b07 0
                 b08 0 b09 0 b10 0 b11 0 b12 0 b13 0 b14 1 b15 0
                 b16 0 b17 0 b18 0 b19 0 b20 0 b21 0 b22 1 b23 0
                 b24 0 b25 0 b26 0 b27 0 b28 0 b29 0 b30 1 b31 0
                 b32 0 b33 0 b34 0 b35 0 b36 0 b37 0 b38 1 b39 0
                 b40 0 b41 0 b42 0 b43 0 b44 0 b45 0 b46 1 b47 0
                 b48 0 b49 0 b50 0 b51 0 b52 0 b53 0 b54 1 b55 0
                 b56 0 b57 0 b58 0 b59 0 b60 0 b61 0 b62 1 b63 0)
           #b'4040404040404040')

(conv-test #S(s6 b00 0 b01 0 b02 0 b03 0 b04 0 b05 0 b06 0 b07 1
                 b08 0 b09 0 b10 0 b11 0 b12 0 b13 0 b14 0 b15 1
                 b16 0 b17 0 b18 0 b19 0 b20 0 b21 0 b22 0 b23 1
                 b24 0 b25 0 b26 0 b27 0 b28 0 b29 0 b30 0 b31 1
                 b32 0 b33 0 b34 0 b35 0 b36 0 b37 0 b38 0 b39 1
                 b40 0 b41 0 b42 0 b43 0 b44 0 b45 0 b46 0 b47 1
                 b48 0 b49 0 b50 0 b51 0 b52 0 b53 0 b54 0 b55 1
                 b56 0 b57 0 b58 0 b59 0 b60 0 b61 0 b62 0 b63 1)
           #b'8080808080808080')

(conv-test #S(s6 b00 1 b01 0 b02 0 b03 0 b04 0 b05 0 b06 0 b07 0
                 b08 0 b09 1 b10 0 b11 0 b12 0 b13 0 b14 0 b15 0
                 b16 0 b17 0 b18 1 b19 0 b20 0 b21 0 b22 0 b23 0
                 b24 0 b25 0 b26 0 b27 1 b28 0 b29 0 b30 0 b31 0
                 b32 0 b33 0 b34 0 b35 0 b36 1 b37 0 b38 0 b39 0
                 b40 0 b41 0 b42 0 b43 0 b44 0 b45 1 b46 0 b47 0
                 b48 0 b49 0 b50 0 b51 0 b52 0 b53 0 b54 1 b55 0
                 b56 0 b57 0 b58 0 b59 0 b60 0 b61 0 b62 0 b63 1)
           #b'0102040810204080')

(conv-test #S(s6 b00 0 b01 0 b02 0 b03 0 b04 0 b05 0 b06 0 b07 1
                 b08 0 b09 0 b10 0 b11 0 b12 0 b13 0 b14 1 b15 0
                 b16 0 b17 0 b18 0 b19 0 b20 0 b21 1 b22 0 b23 0
                 b24 0 b25 0 b26 0 b27 0 b28 1 b29 0 b30 0 b31 0
                 b32 0 b33 0 b34 0 b35 1 b36 0 b37 0 b38 0 b39 0
                 b40 0 b41 0 b42 1 b43 0 b44 0 b45 0 b46 0 b47 0
                 b48 0 b49 1 b50 0 b51 0 b52 0 b53 0 b54 0 b55 0
                 b56 1 b57 0 b58 0 b59 0 b60 0 b61 0 b62 0 b63 0)
           #b'8040201008040201')

(conv-test #S(s6 b00 0 b01 0 b02 0 b03 0 b04 0 b05 0 b06 0 b07 1
                 b08 0 b09 0 b10 0 b11 0 b12 0 b13 0 b14 1 b15 0
                 b16 0 b17 0 b18 0 b19 0 b20 0 b21 1 b22 0 b23 0
                 b24 0 b25 0 b26 0 b27 0 b28 1 b29 0 b30 0 b31 0
                 b32 0 b33 0 b34 0 b35 1 b36 0 b37 0 b38 0 b39 0
                 b40 0 b41 0 b42 1 b43 0 b44 0 b45 0 b46 0 b47 0
                 b48 0 b49 1 b50 0 b51 0 b52 0 b53 0 b54 0 b55 0
                 b56 1 b57 0 b58 0 b59 0 b60 0 b61 0 b62 0 b63 0)
           #b'8040201008040201')

(typedef s7 (struct s7
              (x uint8)
              (b0 (bit 8 uint32)) (b1 (bit 8 uint32))
              (b2 (bit 8 uint32)) (b3 (bit 8 uint32))))

(conv-test #S(s7 x #xff b0 #xaa b1 #xbb b2 #xcc b3 #xdd) #b'ffaabbccdd000000')

(typedef s8 (pack (struct s8
                    (x uint8)
                    (b0 (bit 8 uint32)) (b1 (bit 8 uint32))
                    (b2 (bit 8 uint32)) (b3 (bit 8 uint32)))))

(conv-test #S(s8 x #xff b0 #xaa b1 #xbb b2 #xcc b3 #xdd) #b'ffaabbccdd')

(typedef s9 (pack (struct s9
                    (x uint8)
                    (b0 (bit 8 uint32)) (b1 (bit 8 uint32))
                    (b2 (bit 8 uint32)))))

(conv-test #S(s9 x #xff b0 #xaa b1 #xbb b2 #xcc) #b'ffaabbcc')

(typedef s10 (struct s10
               (x uint8)
               (b0 (bit 8 (pack 2 uint32))) (b1 (bit 8 (pack 2 uint32)))
               (b2 (bit 8 (pack 2 uint32)))))

(conv-test #S(s10 x #xff b0 #xaa b1 #xbb b2 #xcc) #b'ff00aa00bb00cc00')

(typedef s11 (struct s11
               (x uint8)
               (b0 (bit 8 (pack 2 uint32))) (b1 (bit 8 uint32))
               (b2 (bit 8 uint32))))

(conv-test #S(s11 x #xff b0 #xaa b1 #xbb b2 #xcc) #b'ff00aabbcc000000')

(typedef s12 (struct s12
               (x (bit 7 le-uint32))
               (b0 (bit 8 (pack 1 (bit 8 le-uint32))))))

(conv-test #S(s12 x #x7f b0 #xff) #b'ff7f0000')

(typedef s13 (struct s13
               (x (bit 7 le-uint32))
               (b0 (bit 8 (pack 1 (align 1 (bit 8 le-uint32)))))))

(conv-test #S(s13 x #x7f b0 #xff) #b'7fff0000')

(typedef s14 (pack (struct s14
                     (a (bit 9 le-uint32))
                     (b (bit 7 le-uint32)))))

(conv-test #S(s14 a #x1ff b #x7f) #b'ffff')

(typedef s15 (pack (struct s15
                     (x uint8)
                     (a (bit 9 le-uint32))
                     (b (bit 7 le-uint32)))))

(conv-test #S(s15 x 0 a #x1ff b #x7f) #b'00ffff')

(typedef s16 (struct s16
               (x uint8)
               (a (align 2 (bit 9 le-uint32)))
               (b (align 2 (bit 7 le-uint32)))))

(conv-test #S(s16 x 0 a #x1ff b #x7f) #b'0000ff017f000000')
