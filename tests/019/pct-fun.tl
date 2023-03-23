(load "../common")

(defstruct foo ()
  (:init (me) (prinl %fun%))
  (:fini (me) (prinl %fun%))
  (:postinit (me) (prinl %fun%))
  (:postfini (me) (prinl %fun%))
  (:method foo (me) (prinl %fun%)))

(defmeth foo bar (me)
  (prinl %fun%))

(defmeth foo pat (:match)
  (prinl %fun%))

(defun function (: (optarg %fun%))
  (prinl %fun%))

(defun function2 (: (optarg %fun%))
  (prinl optarg))

(defmacro mac ()
  (prinl %fun%)
  nil)

(with-objects ((f (new foo)))
  f.(foo)
  f.(pat)
  f.(bar))

(function)
(function2)

(mac)

(test %fun% nil)

(compile-only
  (eval-only
    (with-compile-opts (nil unused)
      (compile-file (base-name *load-path*) "temp.tlo"))
    (remove-path "temp.tlo")))
