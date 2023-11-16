(load "../common")

(defstruct (add n list) nil
  n list
  (:method cdr (me) (if (cdr me.list) (new (add me.n (cdr me.list)))))
  (:method car (me) (+ me.n (car me.list)))
  (:method nullify (me) (if me.list me))
  (:method lambda (me i)
    (if (rangep i)
      (mapcar (op + me.n) [me.list i])
      (+ me.n (ref me.list i))))
  (:method lambda-set (me i nv)
    (if (rangep i)
      (set [me.list i] (mapcar (lop - me.n) nv))
      (set [me.list i] (- nv me.n)))))

(defvarl o (new (add 3 (range 10 100 10))))

(test (car o) 13)
(test (cadr o) 23)
(test [o 4] 53)
(test (cadr (last o)) nil)

(test (set [o 0..3] '(1003 1103 1203)) (1003 1103 1203))
(test o.list (1000 1100 1200 40 50 60 70 80 90 100))
(test (del [o 1..4]) (1103 1203 43))
(test o.list (1000 50 60 70 80 90 100))
