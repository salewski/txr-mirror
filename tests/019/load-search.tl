(load "../common")

(defvarl cur (dir-name *load-path*))

(defun txr (. args)
  (command-get-string `@{txr-exe-path} @{args " "}`))

(mtest
  (txr `@cur/data/a`)     "a\n"
  (txr `@cur/data/a.txr`) "a.txr\n"
  (txr `@cur/data/a.tl`)  "a.tl\n"
  (txr `@cur/data/a.tlo`) "a.tlo\n")

(mtest
  (txr `@cur/data/b`)     "b.txr\n"
  (txr `@cur/data/b.txr`) "b.txr\n"
  (txr `@cur/data/b.tl`)  "b.tl\n"
  (txr `@cur/data/b.tlo`) "b.tlo\n")

(mtest
  (txr `@cur/data/c`)     "c.txr\n"
  (txr `@cur/data/c.txr`) "c.txr\n"
  (txr `@cur/data/c.tl`)  "c.tl\n")

(let ((*stderr* *stdnull*))
  (mtest
    (txr "--lisp" `@cur/data/a`)      ""
    (txr "--compiled" `@cur/data/a`)  ""))

(mtest
  (txr "--lisp" `@cur/data/b`)      "b.tlo\n"
  (txr "--compiled" `@cur/data/b`)  "b.tlo\n")

(mtest
  (txr "--lisp" `@cur/data/c`)      "c.tl\n"
  (txr "--compiled" `@cur/data/c`)  "c.tl\n")
