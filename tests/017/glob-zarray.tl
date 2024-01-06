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
			 (pathv (ptr-out (zarray str)))
                         (nil (array 4 cptr)))))
  ((:cygnal :cygwin :android :bsd :openbsd)
    (deffi-type glob-t (struct glob-t
                         (pathc size-t)
			 (nil size-t)
			 (nil size-t)
			 (nil int)
			 (pathv (ptr-out (zarray str)))
			 (nil (array 6 cptr)))))
  (t (deffi-type glob-t (struct glob-t
                          (pathc size-t)
			  (pathv (ptr-out (zarray str)))
                          (reserve size-t)
                          (nil int)
                          (nil (array 5 cptr))))))

(with-dyn-lib (libc)
  (deffi globb "glob" int (str int closure (ptr-out glob-t)))
  (deffi globfree "globfree" void ((ptr-in glob-t)))
  (deffi-cb glob-cb int (str int) -1))

;; Note: we deliberately don't call (globfree g) in this
;; test case because it won't reliably work. The problem is
;; that FFI will create a brand new C struct and populate
;; the array with newly allocated strings.
;;
;; In the glob-carray.tl test case, we do call globfree.
;; Though a different C struct is allocated, which makes
;; that usage not strictly correct, that structure is
;; populated with the original string pointers due to the
;; carray type being used.
;;
(let* ((g (new glob-t)))
  (prinl (globb "tests/001/*.txr" 0 (glob-cb (lambda (path err))) g))
  (prinl g))
