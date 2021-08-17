(load "../common")

(with-dyn-lib (libc)
  (deffi printf-int-double "printf" int (str : int double)))

(printf-int-double "foo-%d = %4.3f\n" 123 4.56)
