(defparml counter 0)
(push (lambda () (inc counter)) *load-hooks*)
(push (lambda () (dec counter)) *load-hooks*)
(pop *load-hooks*)
