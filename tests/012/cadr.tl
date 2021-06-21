(load "../common")

(mtest
  (cxr 1 42) 42
  (cxr #b11 '(a . b)) a
  (cxr #b10 '(a . b)) b
  (cxr #b11000 '(1 2 3 4 5)) 4
  (cyr #b100001 '(1 2 3 4 5)) 5
  (cyr #b1111 '(((a)))) a
  (cyr #b111 '(a)) :error)

(let ((r (range* 0 100)))
  (vtest (mapcar (op cyr (succ (expt 2 (succ @1))) r) 0..100) r)
  (vtest (mapcar (op cxr (* 3 (expt 2 @1)) r) 0..100) r))
