(load "../common")

(let ((x (with-out-string-stream (*stdout*)
           [format *stdout* "wo"]
           (format t "rld!"))))
  (format *stdout* "Hello, ")
  (put-line x))

(defvar *spec* :global)

(defvar *fun* (let* ((*spec* :local)
                     (fun (lambda () *spec*))
                     (x *spec*))
                (test (call fun) :local)
                (test x :local)
                (set *spec* :local2)
                fun))

(let ((*spec* *spec*))
  (test *spec* :global)
  (set *spec* :clobber)
  (test (call *fun*) :clobber))

(test *spec* :global)

(test (call *fun*) :global)

(let ((x *spec*)
      (*spec* :local)
      (y *spec*))
  (let ((z *spec*)
        (*spec* nil)
        (w *spec*))
    (test (list *spec* x y z w) (nil :global :global :local :local))))
