(load "../common")

(when (or (memq (os-symbol) '(:cygwin :solaris :android))
          (not (path-exists-p "/usr/bin")))
  (put-string (file-get-string "tests/017/realpath.expected"))
  (exit 0))

(with-dyn-lib (libc)
  (deffi realpath-null "realpath" str-d (str str))
  (deffi realpath-buf "realpath" str-d (str (ptr-in-d (zarray 8192 char)))))

(prinl (realpath-null "/usr/bin" nil))
(prinl (realpath-buf "/usr/bin" (copy "")))
