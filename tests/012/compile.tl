(defparml %this-dir% (dir-name *load-path*))
(defparml %expected-file% `@(m^ #/.*[.]/ *load-path*)expected`)

(file-put-string %expected-file% "")

(each ((f '#"aseq ashwin circ cont defset except \
             fini ifa man-or-boy oop-mi oop-seq oop \
             parse syms quasi quine seq stslot const type"))
  (let ((exf `@{%this-dir%}/@f.expected`))
    (when (path-exists-p exf)
      (file-append-string %expected-file%
                          (file-get-string exf))))
  (with-compile-opts (nil unused)
    (compile-file `@f.tl` "temp.tlo"))
  (remove-path "temp.tlo"))
