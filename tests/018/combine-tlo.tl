(load "../common")

(push-after-load
  (remove-path "libfile.tl")
  (remove-path "libfile.tlo")
  (remove-path "mainfile.tl")
  (remove-path "mainfile.tlo")
  (remove-path "progfile"))

(file-put-lines
  "libfile.tl"
  ^(,`#!@{txr-exe-path} --lisp`
    "(defun libfun ()"
    "  (put-line \"libfun\"))"))

(file-put-lines
  "mainfile.tl"
  '("(compile-only (libfun))"))

(compile-file "./libfile")
(compile-file "./mainfile")

(cat-files "progfile" "libfile.tlo" "mainfile.tlo")

(chmod "progfile" "+x")

(test (command-get-lines "./progfile") ("libfun"))
