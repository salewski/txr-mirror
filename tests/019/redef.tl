(load "../common")

(defun foo () :foo)

(defvar foov :foov)

(defun bar () (list (foo) foov))

(compile 'bar)

(test (bar) (:foo :foov))

(defun foo () :bar)

(makunbound 'foov)

(defparml foov :barv)

(test (bar) (:bar :barv))

(defsymacro foov 42)

(test (bar) :error)
