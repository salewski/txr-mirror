(load "../common")

(defstruct glob-t nil
  pathc pathv reserve)

(deffi-type glob-t (struct glob-t
                     (pathc size-t)
                     (pathv (carray str-d))
                     (reserve size-t)))

(with-dyn-lib nil
  (deffi globb "glob" int (str int closure (ptr-out glob-t)))
  (deffi globfree "globfree" void ((ptr-in glob-t)))
  (deffi-cb glob-cb int (str int) -1))

(let* ((g (new glob-t)))
  (prinl (globb "tests/001/*.txr" 0 (glob-cb (lambda (path err))) g))
  (carray-set-length g.pathv g.pathc)
  (prinl g)
  (prinl (vec-carray g.pathv))
  (globfree g))
