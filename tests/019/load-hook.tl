(load "../common")

(defvarl %dir% (dir-name *load-path*))

(compile-file "../load-hook")
(test counter 0)

(push (lambda ()
        (remove-path (path-cat %dir% "../load-hook.tlo")))
  *load-hooks*)

(set counter nil)
(load "../load-hook.tl")
(test counter 1)

(set counter nil)
(load "../load-hook.tlo")
(test counter 1)

(mtest
  (macroexpand-1 '(push-after-load))
  (sys:setq *load-hooks* (cons (lambda ()) *load-hooks*))

  (macroexpand-1 '(push-after-load x))
  (sys:setq *load-hooks* (cons (lambda () x) *load-hooks*))

  (macroexpand-1 '(pop-after-load))
  (sys:setq *load-hooks* (cdr *load-hooks*)))
