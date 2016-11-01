(defstruct circ-print nil
  a
  (:method print (me stream pretty-p)
    (put-string "[[" stream)
    (print me.a stream pretty-p)
    (put-string "]]" stream)))

(defvar x (let* ((l (list "a"))
                 (c (new circ-print a l)))
            (list l c)))

(let ((*print-circle* t))
  (prinl (new circ-print a "a"))
  (prinl x))
