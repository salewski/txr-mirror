(load "../common")

(vtest (uint-buf (make-buf 8 255 16)) (pred (expt 2 64)))
(test (int-buf (make-buf 8 255 16)) -1)

(mtest
  (str-buf #b'E6BC') "\xDCE6\xDCBC"
  (buf-str "\xDCE6\xDCBC") #b'E6BC'
  (str-buf #b'E6') "\xDCE6"
  (buf-str "\xDCE6") #b'E6')
