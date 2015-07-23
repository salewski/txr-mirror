(defmacro error-to-sym (expr)
 ^(catch ,expr
    (error (cond) :error)))

(defmacro test (:env env expr expected)
 (catch
   (let ((expr-expn (macroexpand expr env)))
     ^(ifa (not (equal (error-to-sym ,expr-expn) ',expected))
           (error "test case ~s failed: produced ~s; expected ~s"
                  ',expr it ',expected)))
   (error (exc)
          (unless (eq expected :error)
            (error "test case ~s failed to expand: expected is ~s" expr expected)))))
