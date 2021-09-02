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
