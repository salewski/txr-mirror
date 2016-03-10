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

(defun osname ()
  (with-stream (u (open-command "(uname -o || uname) 2> /dev/null"))
    (get-line u)))
