(load "../common")
(load "../perm")

(defvarl tgt [*args* 0])

(remove-path tgt)
(with-stream (s (open-file tgt "w")))
(umask #o022)

(defvarl os (os-symbol))

(defvarl test-sticky (unless (eq os :bsd)
                       (chmod tgt s-isvtx)
                       (let ((st (stat tgt)))
                         (plusp (logand s-isvtx st.mode)))))

(defmacro mode-bits (st-mode)
  ^(logand ,st-mode #xFFF))

(defun cht (init mode expected)
  (when (or test-sticky
            (not (find #\t `@init@mode@expected`)))
    (let ((ini (dec-perm init))
          (exp (dec-perm expected)))
      (chmod tgt ini)
      (let* ((st (stat tgt))
             (m (mode-bits st.mode)))
        (unless (eql m ini)
          (error "failed to set initial mode: expected: ~s, actual: ~s "
                 init (enc-perm m))))
      (chmod tgt mode)
      (let* ((st (stat tgt))
             (m (mode-bits st.mode)))
        (unless (eql m exp)
          (error "failed to set mode with ~s: expected ~s, actual ~s"
                 mode expected (enc-perm m)))))))

(cht "------------" "a+strwx"           "sgtrwxrwxrwx")
(cht "------------" "+strwx"            "sgtrwxr-xr-x")
(cht "------------" "u+s"               "s-----------")
(cht "------------" "g+s"               "-g----------")
(cht "------------" "+t"                "--t---------")
(cht "sgtrwxrwxrwx" "="                 "------------")
;; These tests don't work on Cygwin 3.1.7, Windows 10.
;; They worked on Cygwin 2.5 on Windows 7.
(unless (eq os :cygwin)
  (cht "sgtrwxrwxrwx" "u="                "-gt---rwxrwx")
  (cht "sgtrwxrwxrwx" "g="                "s-trwx---rwx")
  (cht "sgtrwxrwxrwx" "o="                "sg-rwxrwx---"))
(cht "------------" "u+s,g+s"           "sg----------")
(cht "------------" "u+r,g+r,o+r,+t,+s" "sgtr--r--r--")
(cht "------------" "+rwx,g-r+w,o-r+w"  "---rwx-wx-wx")
(cht "---------rwx" "u=rwsx"            "s--rwx---rwx")
(unless (eq os :cygwin)
  (cht "---------rwx" "u=rwsx,g=rwx,go-x" "s--rwxrw-rw-")
  (cht "---------rwx" "g=o,g-w+s,u=g,o-x" "-g-r-xr-xrw-"))
(cht "---------rwx" "o=o"               "---------rwx")
(cht "-----x------" "a+X"               "-----x--x--x")
(cht "-----x------" "=,a+X"             "------------")
(cht "-----x------" "a-x+X"             "------------")
(cht "------------" "u+x-X"             "------------")
(cht "------------" "o+x=o"             "-----------x")
