(defpackage :foo)

(compile-file "data/program.tl")

(delete-package :foo)

(load "data/program.tlo")
