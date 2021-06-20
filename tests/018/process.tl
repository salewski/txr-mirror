(load "../common")

(defun cmd (c : (m "r"))
  (with-stream (s (open-command c m))
    (get-string s)))

(mtest
  (cmd "echo foo") "foo\n"
  (cmd "echo foo" ">1n") ""
  (cmd "echo foo 1>&2" ">21") "foo\n")

(defmacro fcmd (. forms)
  ^(with-stream (s (open-subprocess nil "r" nil (lambda () ,*forms)))
     (get-string s)))

(mtest
  (fcmd (let ((*stdout* *stdnull*)) (sh "echo foo"))) ""
  (fcmd (let ((*stderr* *stdout*)) (sh "echo foo 1>&1"))) "foo\n")
