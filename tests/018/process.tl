(load "../common")

(unless (path-executable-to-me-p "/bin/sh")
  (exit 0))

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

(caseq (os-symbol)
  ((:cygwin :cygnal)
    (mtest
      (fcmd (let ((*stdout* *stdnull*)) (sh "echo foo"))) ""
      (fcmd (let ((*stderr* *stdout*)) (sh "echo foo 1>&2"))) "foo \r\n"))
  (t
    (mtest
      (fcmd (let ((*stdout* *stdnull*)) (sh "echo foo"))) ""
      (fcmd (let ((*stderr* *stdout*)) (sh "echo foo 1>&2"))) "foo\n")))

(caseq (os-symbol)
  ((:cygwin :cygnal))
  (t (mtest
       (let ((*child-env* '("a=b")))
	 (get-lines (open-process "/usr/bin/env" "r")))
       ("a=b")
       (let ((*child-env* nil))
	 (get-lines (open-process "/usr/bin/env" "r")))
       nil)
     (test
       (fcmd
	 (let ((*child-env* '("a=b")))
	   (run "/usr/bin/env")))
       "a=b\n")))
