(defparml %this-dir% (dir-name *load-path*))
(defparml %expected-file% `@(m^ #/.*[.]/ *load-path*)expected`)

(file-put-string %expected-file% "")

(each ((f '#"aseq.tl ashwin.tl circ.tl cont.tl defset.tl except.tl \
             fini.tl ifa.tl man-or-boy.tl oop-mi.tl oop-seq.tl oop.tl \
             parse.tl quasi.tl quine.tl seq.tl stslot.tl"))
  (let ((exf `@{%this-dir%}/@(m^ #/.*[.]/ f)expected`))
    (when (path-exists-p exf)
      (file-append-string %expected-file%
                          (file-get-string exf))))
  (compile-file f "temp.tlo")
  (remove-path "temp.tlo"))
