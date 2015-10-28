(load "../common")

(vtest (let ((f (obtain (for ((i 0)) () ((inc i)) (yield i)))))
         (take 2000 (gun (call f))))
       (range* 0 2000))

(defun yflatten (obj)
  (labels ((flatten-rec (obj)
             (cond
               ((null obj))
               ((atom obj) (yield-from yflatten obj))
               (t (flatten-rec (car obj))
                  (flatten-rec (cdr obj))))))
    (flatten-rec obj)
    nil))

(test (let ((f (obtain (yflatten '(a b (c . d) (e (f (g))))))))
        (gun [f]))
      (a b c d e f g))
