(load "../common")

(vtest (uint-buf (make-buf 8 255 16)) (pred (expt 2 64)))
(test (int-buf (make-buf 8 255 16)) -1)

(mtest
  (str-buf #b'E6BC') "\xDCE6\xDCBC"
  (buf-str "\xDCE6\xDCBC") #b'E6BC'
  (str-buf #b'E6') "\xDCE6"
  (buf-str "\xDCE6") #b'E6')

(when (fboundp 'usr:buf-compress)
  (mtest
    (< (len (buf-compress (make-buf 1024))) 100) t
    (buf-compress (make-buf 1024) -2) :error
    (buf-compress (make-buf 1024) 10) :error)

  (each ((i 0..65535))
    (let* ((buf (ffi-put i (ffi uint16)))
           (zbuf (buf-compress buf)))
      (vtest (buf-decompress zbuf) buf)))

  (let ((buf (random-buf 65536)))
    (vtest (buf-decompress (buf-compress buf)) buf))

  (mtest
    (buf-decompress (make-buf 1024)) :error
    (buf-decompress (make-buf 1024 255)) :error))
