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

(defmacro amb-scope (. forms)
  ^(block amb-scope ,*forms))

(defun amb (. args)
  (suspend amb-scope cont
    (each ((a args))
      (when (and a (call cont a))
        (return-from amb a)))))

(test (amb-scope
        (let ((w1 (amb "the" "that" "a"))
              (w2 (amb "frog" "elephant" "thing"))
              (w3 (amb "walked" "treaded" "grows"))
              (w4 (amb "slowly" "quickly")))
          (amb (and (eql [w1 -1] [w2 0])
                    (eql [w2 -1] [w3 0])
                    (eql [w3 -1] [w4 0])))
          (list w1 w2 w3 w4)))
      ("that" "thing" "grows" "slowly"))
