(load "../common")

(with-dyn-lib (libc)
  ;; FFI for sorting char *array[] with qsort.
  (deffi qsort "qsort" void ((ptr (array str)) size-t size-t closure))
  (deffi-cb qsort-cb int ((ptr str-d) (ptr str-d)))

  ;; FFI for sorting wchar_t *array[] with qsort.
  (deffi qsortw "qsort" void ((ptr (array wstr)) size-t size-t closure))
  (deffi-cb qsortw-cb int ((ptr wstr-d) (ptr wstr-d))))

;; sort vector of strings as char *
(let ((vec #("the" "quick" "brown" "fox" "jumped" "over" "the" "lazy" "dogs")))
  (prinl vec)
  (qsort vec (length vec) (sizeof str) [qsort-cb cmp-str])
  (prinl vec))

;; sort vector of strings as wchar_t *
(let ((vec #("the" "quick" "brown" "fox" "jumped" "over" "the" "lazy" "dogs")))
  (prinl vec)
  (qsortw vec (length vec) (sizeof str) [qsortw-cb cmp-str])
  (prinl vec))

;; abort callback with non-local transfer
(let ((vec #("the" "quick" "brown" "fox" "jumped" "over" "the" "lazy" "dogs")))
  (prinl vec)
  (prinl (block foo
           (qsort vec (length vec) (sizeof str)
                  [qsort-cb (lambda (x y) (return-from foo 42))])))
  (prinl vec))

;; abort callback with non-local transfer
(let ((vec #("the" "quick")))
  (prinl vec)
  (prinl (block foo
           (qsortw vec (length vec) (sizeof str)
                   [qsortw-cb (lambda (x y) (return-from foo 42))])))
  (prinl vec))
