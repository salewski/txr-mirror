(load "../common")

(defex d c b a)

(defmacro cont (. forms)
  ^(catch (progn ,*forms) (cont (arg) arg)))

(test
  (handle (list (cont (throw 'd 1))
                (cont (throw 'c 2)))
    (a (exc arg)
      (let ((cont (find-frame 'cont)))
        (if cont
          (progn
            (prinl arg)
            (invoke-catch cont 'cont (+ 100 arg)))))))
  (101 102))
