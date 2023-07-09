(load "../common")

(mtest
  (crc32 "") 0
  (sha1 "") #b'da39a3ee 5e6b4b0d 3255bfef 95601890 afd80709'
  (sha256 "") #b'e3b0c442 98fc1c14 9afbf4c8 996fb924 27ae41e4 649b934c a495991b 7852b855'
  (md5 "") #b'd41d8cd9 8f00b204 e9800998 ecf8427e')

(mtest
  (crc32 "abc") #x352441c2
  (sha1 "abc") #b'a9993e36 4706816a ba3e2571 7850c26c 9cd0d89d'
  (sha256 "abc") #b'ba7816bf 8f01cfea 414140de 5dae2223 b00361a3 96177a9c b410ff61 f20015ad'
  (md5 "abc") #b'90015098 3cd24fb0 d6963f7d 28e17f72')

(defvarl algs (list
                [list sha1   sha1-begin    sha1-hash   sha1-end   sha1-stream]
                [list sha256 sha256-begin  sha256-hash sha256-end sha256-stream]
                [list md5    md5-begin     md5-hash    md5-end    md5-stream]))

(defvarl testbuf (make-buf 20000 #xAA))

(test
  (crc32 testbuf) #xf35324f0)

(each-match ((@hashfn @begfn @updatefn @endfn @streamfn) algs)
  (let ((h0 [hashfn testbuf])
        (h1 (let ((ctx [begfn]))
              (each ((piece (tuples 1 testbuf)))
                [updatefn ctx piece])
              [endfn ctx]))
        (h2 (let ((ctx [begfn]))
              (each ((piece (tuples 100 testbuf)))
                [updatefn ctx piece])
              [endfn ctx]))
        (h3 (let ((s (make-buf-stream testbuf)))
              [streamfn s])))
    (mtest
      (equal h0 h1) t
      (equal h0 h2) t
      (equal h0 h3) t)))
