(load "../common")

(defvarl cur (dir-name *load-path*))

(defun txr (. args)
  (command-get-string `@{txr-exe-path} @{args " "}`))

(defun lod (x)
  (with-out-string-stream (*stdout*)
    (load x)))

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

(mtest
  (length *load-search-dirs*) 1
  (base-name (car *load-search-dirs*)) "lib")

(set *load-search-dirs* (list `@cur/data`))

(mtest
  (lod "a")     :error
  (lod "a.tl")  "a.tl\n"
  (lod "c")     "c.tl\n")

(push `@cur/nonexistent` *load-search-dirs*)

(mtest
  (lod "a")     :error
  (lod "a.tl")  "a.tl\n"
  (lod "c")     "c.tl\n")

(push `@cur/unreadable` *load-search-dirs*)

(push-after-load (rmdir `@cur/unreadable`))

(ensure-dir `@cur/unreadable` 0)

(mtest
  (lod "a")     :error
  (lod "a.tl")  :error
  (lod "c")     :error)
