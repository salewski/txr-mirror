(defmacro error-to-sym (expr)
 ^(catch ,expr
    (error (cond) :error)))

(defmacro vtest (:env env expr expected)
 (catch
   (let ((expr-expn (macroexpand expr env))
         (expval (gensym)))
     ^(let ((,expval ,expected))
        (ifa (not (equal (error-to-sym ,expr-expn) ,expval))
           (error "test case ~s failed: produced ~s; expected ~s"
                  ',expr it ,expval))))
   (error (exc)
          (unless (eq expected :error)
            (error "test case ~s failed to expand: expected is ~s" expr expected)))))

(defmacro test (expr expected)
  ^(vtest ,expr ',expected))

(defmacro stest (expr expected)
  ^(vtest ,^(tostring ,expr) ,expected))

(defmacro mtest (. pairs)
  ^(progn ,*(mapcar (op cons 'test) (tuples 2 pairs))))

(defun os-symbol ()
  (let ((u (uname)))
    [(orf (iff (f^ #/Linux/) (ret :linux))
          (iff (f^ #/SunOS/) (ret :solaris))
          (iff (f^ #/CYGWIN/) (ret :cygwin))
          (iff (f^ #/CYGNAL/) (ret :cygnal))
          (iff (f^ #/Darwin/) (ret :macos))
          (ret :unknown))
     u.sysname]))

(defun libc ()
  (caseql (os-symbol)
    ((:linux :solaris :macos) (dlopen nil))
    ((:cygwin) (dlopen "cygwin1.dll"))))
