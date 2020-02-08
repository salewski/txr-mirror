(load "../common")
(load "../perm")

(defvarl tgt [*args* 0])

(defmacro mode-bits (st-mode)
  (logand st-mode #xFFF))

(defun cht (init mode expected)
  (let ((ini (dec-perm init))
        (exp (dec-perm expected)))
    (chmod tgt ini)
    (let* ((st (stat tgt))
           (m (logand st.mode #xFFF)))
      (unless (eql m ini)
        (error "failed to set initial mode: expected: ~s, actual: ~s "
               init (enc-perm m))))
    (chmod tgt mode)
    (let* ((st (stat tgt))
           (m (logand st.mode #xFFF)))
      (unless (eql m exp)
        (error "failed to set mode: expected ~s, actual ~s"
               expected (enc-perm m))))))

(remove-path tgt)
(with-stream (s (open-file tgt "w")))
(umask #o022)

(cht "------------" "a+strwx"           "sgtrwxrwxrwx")
(cht "------------" "+strwx"            "sgtrwxr-xr-x")
(cht "------------" "u+s"               "s-----------")
(cht "------------" "g+s"               "-g----------")
(cht "------------" "+t"                "--t---------")
(cht "sgtrwxrwxrwx" "="                 "------------")
