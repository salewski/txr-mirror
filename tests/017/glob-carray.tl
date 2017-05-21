(load "../common")

(defstruct glob-t nil
  pathc pathv (reserve 0))

(caseq (os-symbol)
  (:macos
    (deffi-type glob-t (struct glob-t
                         (pathc size-t)
			 (nil int)
			 (nil size-t)
			 (nil int)
			 (pathv (carray str-d))
			 (nil (array 4 cptr)))))
  ((:cygnal :cygwin)
    (deffi-type glob-t (struct glob-t
                         (pathc size-t)
			 (nil size-t)
			 (nil size-t)
			 (nil int)
			 (pathv (carray str-d))
			 (nil (array 6 cptr)))))
  (t (deffi-type glob-t (struct glob-t
                          (pathc size-t)
                          (pathv (carray str-d))
                          (reserve size-t)
                          (nil (array 5 cptr))))))

(with-dyn-lib (libc)
  (deffi globb "glob" int (str int closure (ptr-out glob-t)))
  (deffi globfree "globfree" void ((ptr-in glob-t)))
  (deffi-cb glob-cb int (str int) -1))

(let* ((g (new glob-t)))
  (prinl (globb "tests/001/*.txr" 0 (glob-cb (lambda (path err))) g))
  (carray-set-length g.pathv g.pathc)
  (prinl g)
  (prinl (vec-carray g.pathv))
  (globfree g))
