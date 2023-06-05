(load "../common")

(defvar *trace*)

(defmacro deftrace (fun)
  ^(defun ,fun (. args)
     (push ^(,%fun% ,*args) *trace*)))

(handle
  (eval '(progn
           (deftrace load)
           (deftrace compile-update-file)
           (deftrace clean-file)))
  (warning (x . rest)
    (throw 'continue)))

(defmacro tr (form)
  ^(let ((*trace* nil))
     ,form
     (reverse *trace*)))

(mtest
  (tr (load-args-recurse '("abc"))) ((load "abc"))
  (tr (load-args-recurse "abc")) ((load "abc"))
  (tr (load-args-recurse "abc" "def")) ((load "abc") (load "def"))
  (tr (load-args-recurse '("abc") "def")) ((load ("abc")) (load "def")))

(let ((*load-args* '(1 2)))
  (mtest
    (tr (load-args-recurse '("abc"))) ((load "abc" 1 2))
    (tr (load-args-recurse "abc")) ((load "abc" 1 2))
    (tr (load-args-recurse "abc" "def")) ((load "abc" 1 2) (load "def" 1 2))
    (tr (load-args-recurse '("abc") "def")) ((load ("abc") 1 2) (load "def" 1 2))))

(mtest
  (tr (load-args-process '("abc"))) ((load "abc"))
  (tr (load-args-process "abc")) ((load "abc"))
  (tr (load-args-process "abc" "def")) ((load "abc") (load "def"))
  (tr (load-args-process '("abc") "def")) ((load ("abc")) (load "def")))

(let ((*load-args* '(1 2)))
  (mtest
    (tr (load-args-process '("abc"))) ((load "abc" 1 2))
    (tr (load-args-process "abc")) ((load "abc" 1 2))
    (tr (load-args-process "abc" "def")) ((load "abc" 1 2) (load "def" 1 2))
    (tr (load-args-process '("abc") "def")) ((load ("abc") 1 2) (load "def" 1 2))))

(let ((*load-args* '(:compile)))
  (mtest
    (tr (load-args-process '("abc"))) ((compile-update-file "load-args.tl")
                                       (compile-update-file "abc"))
    (tr (load-args-process "abc")) ((compile-update-file "load-args.tl")
                                    (compile-update-file "abc"))
    (tr (load-args-process "abc" "def")) ((compile-update-file "load-args.tl")
                                          (compile-update-file "abc")
                                          (compile-update-file "def"))
    (tr (load-args-process '("abc") "def")) ((compile-update-file "load-args.tl")
                                             (compile-update-file ("abc"))
                                             (compile-update-file "def"))))

(let ((*load-args* '(:clean)))
  (mtest
    (tr (load-args-process '("abc"))) ((clean-file "load-args.tl")
                                       (clean-file "abc"))
    (tr (load-args-process "abc")) ((clean-file "load-args.tl")
                                    (clean-file "abc"))
    (tr (load-args-process "abc" "def")) ((clean-file "load-args.tl")
                                          (clean-file "abc")
                                          (clean-file "def"))
    (tr (load-args-process '("abc") "def")) ((clean-file "load-args.tl")
                                             (clean-file ("abc"))
                                             (clean-file "def"))))
