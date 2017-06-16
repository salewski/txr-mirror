(load "../common")

(defstruct (add n list) nil
  n list
  (:method cdr (me) (if (cdr me.list) (new (add me.n (cdr me.list)))))
  (:method car (me) (+ me.n (car me.list)))
  (:method nullify (me) (if me.list me))
  (:method lambda (me i) (+ me.n (ref me.list i))))

(defvarl o (new (add 3 (range 10 100 10))))

(test (car o) 13)
(test (cadr o) 23)
(test [o 4] 53)
(test (cadr (last o)) nil)
