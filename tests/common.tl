(defvar *compile-test*)

(defmacro error-to-sym (expr)
  ^(catch ,expr
     (error (cond) :error)
     (warning (cond) :warning)))

(defmacro vtest (:env env expr expected)
  (if-match (quote @(as sym @(or :error :warning))) expected
    (set expected sym))
  (if *compile-test*
    (with-compile-opts (nil unused)
      (if (meq expected :error :warning)
        (with-gensyms (code)
          ^(let ((,code (catch
                          (compile-toplevel ',expr)
                          (error (exc)
                            (if (eq ,expected :warning)
                              (error "test case ~s produced error during compilation, expected ~s"
                                     ',expr ,expected)))
                          (warning (exc)
                            (if (eq ,expected :error)
                              (error "test case ~s warned during compilation, expected ~s"
                                     ',expr ,expected))))))
             (ifa (not (equal (error-to-sym (call ,code)) ,expected))
               (error "test case ~s failed: produced ~s; expected ~s"
                      ',expr it ,expected))))
        (with-gensyms (expval)
          ^(let ((,expval ,expected))
             (ifa (not (equal (call (compile-toplevel ',expr)) ,expval))
               (error "test case ~s failed: produced ~s; expected ~s"
                      ',expr it ,expval))))))
    (if (meq expected :error :warning)
      (catch
        (let ((expr-expn (expand expr env)))
          ^(ifa (not (equal (error-to-sym ,expr-expn) ,expected))
             (error "test case ~s failed: produced ~s; expected ~s"
                    ',expr it ,expected)))
        (error (exc)
          (if (eq expected :warning)
            (error "test case ~s produced error during expansion, expected ~s"
                   expr expected)))
        (warning (exc)
          (if (eq expected :error)
            (error "test case ~s warned during expansion, expected ~s"
                   expr expected))))
      (let ((expr-expn (expand expr env))
            (expval (gensym)))
        ^(let ((,expval ,expected))
           (ifa (not (equal ,expr-expn ,expval))
             (error "test case ~s failed: produced ~s; expected ~s"
                    ',expr it ,expval)))))))

(defmacro test (expr expected)
  ^(vtest ,expr ',expected))

(defmacro stest (expr expected)
  ^(vtest ,^(tostring ,expr) ,expected))

(defmacro sstest (expr expected)
  ^(vtest ,^(tostring ,expr) ,(tostring expected)))

(defmacro mtest (. pairs)
  ^(progn ,*(mapcar (op cons 'test) (tuples 2 pairs))))

(defmacro mvtest (. pairs)
  ^(progn ,*(mapcar (op cons 'vtest) (tuples 2 pairs))))

(defmacro mstest (. pairs)
  ^(progn ,*(mapcar (op cons 'stest) (tuples 2 pairs))))

(defun os-symbol ()
  (if (ignerr (dlsym (dlopen "libandroid.so") "AAsset_close"))
    :android
    (let ((u (uname)))
      [(orf (iff (f^ #/Linux/) (ret :linux))
            (iff (f^ #/SunOS/) (ret :solaris))
            (iff (f^ #/CYGWIN/) (ret :cygwin))
            (iff (f^ #/CYGNAL/) (ret :cygnal))
            (iff (f^ #/Darwin/) (ret :macos))
            (iff (f^ #/OpenBSD/) (ret :openbsd))
            (iff #/BSD/ (ret :bsd))
            (ret :unknown))
       u.sysname])))

(defun libc ()
  (caseql (os-symbol)
    ((:cygwin :cygnal) (dlopen "cygwin1.dll"))
    (t (dlopen nil))))

(defmacro with-temp-file ((name-var stream-var prefix) . body)
  ^(let* ((,stream-var (mkstemp ,prefix))
          (,name-var (stream-get-prop ,stream-var :name)))
     (unwind-protect
       (progn ,*body)
       (close-stream ,stream-var)
       (remove-path ,name-var))))
