(load "../common")

(vtest (uint-buf (make-buf 8 255 16)) (pred (expt 2 64)))
(test (int-buf (make-buf 8 255 16)) -1)
