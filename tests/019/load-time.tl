(load "../common")

(defvarl list)

(test
  [(compile-toplevel '(progn
                        (push 0 list)
                        (load-time (push 1 list))
                        list))]
  (0 1))

(zap list)

(test
  [(compile-toplevel '(progn
                        (push 0 list)
                        (lambda ()
                          (load-time (push 1 list)))
                        list))]
  (0 1))

(zap list)

(test
  [(compile-toplevel '(progn
                        (load-time (push 0 list))
                        (lambda ()
                          (load-time (push 1 list)))
                        list))]
  (1 0))
