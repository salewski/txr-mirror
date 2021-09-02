(load "../common")

(let ((x (list* 1 2 3 4)))
  (mtest
    (set x (delcons x x)) (2 3 . 4)
    (set x (delcons x x)) (3 . 4)
    (set x (delcons x x)) 4
    (set x (delcons x x)) 4))

(let ((x (list* 1 2 3 4 5)))
  (mtest
    (delcons (cdr x) x) (1 3 4 . 5)
    (delcons (cddr x) x) (1 3 . 5)
    (delcons (cdr x) x) (1 . 5)))
