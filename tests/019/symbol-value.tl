(load "../common")

(defparm v 42)

(mtest
  v 42
  (symbol-value 'v) 42
  (set (symbol-value 'v) 73) 73
  (symbol-value 'v) 73
  v 73)

(mtest
  (let ((v 2)) v) 2
  (let ((v 2)) (symbol-value 'v)) 2
  (progn (let ((v 2)) (set (symbol-value 'v) 1)) v) 73
  (let ((v 2)) (set (symbol-value 'v) 1) v) 1
  v 73)

(test
  (progn
    (let ((v 2))
      (set (symbol-value 'x) 73))
    x)
  73)
