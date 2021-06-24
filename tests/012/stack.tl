(load "../common")

(defun recur () (recur))

(defmacro so (expr)
  ^(catch ,expr
     (stack-overflow (exc) :so)))

(test (so (recur)) :so)

(test (let ((pid (fork)))
        (cond
          ((zerop pid) (set-stack-limit 0) (recur))
          (t (let ((status (wait pid)))
               (w-ifsignaled status)))))
      t)

(defmacro infexp ()
  ^(foo (infexp)))

(test (so (expand '(infexp))) :so)

(defvarl orig (get-stack-limit))

(mvtest
  (set-stack-limit nil) orig
  (set-stack-limit orig) nil
  (set-stack-limit 0) orig
  (set-stack-limit orig) nil
  (set-stack-limit 65536) orig
  (set-stack-limit orig) 65536)
