(defvar *trace-output* *stdout*)

(defvar sys:*trace-hash* (hash :equal-based))
(defvar sys:*trace-level* -1)

(defun sys:trace-enter (name args)
  (format *trace-output* "~*a(~s ~s\n" (* sys:*trace-level* 2) "" name args))

(defun sys:trace-leave (name val)
  (format *trace-output* "~*a  ~s)\n" (* sys:*trace-level* 2) "" val))

(defun sys:trace-canonicalize-name (name)
  (if (and (consp name)
           (eq (car name) 'meth))
    (let* ((req-type (cadr name))
           (sym (caddr name)))
      (let ((actual-type (static-slot-home req-type sym)))
        (if (eq req-type actual-type)
          name
          ^(meth ,actual-type ,sym))))
    name))

(defun sys:trace (names)
  (cond
    ((null names) (hash-keys sys:*trace-hash*))
    (t
      (each ((orig-n names)
             (n [mapcar sys:trace-canonicalize-name names]))
        (unless [sys:*trace-hash* n]
          (when (neq n orig-n)
            (catch
              (throwf 'warning "~s: ~s is actually ~s: tracing that instead"
                      'trace orig-n n)
              (continue ())))
          (let* ((prev (or (symbol-function n)
                           (throwf 'eval-error
                                   "~s: ~s does not name a function" 'trace n)))
                 (lex-n n)
                 (hook (lambda (. args)
                         (let ((abandoned t)
                               (sys:*trace-level* (succ sys:*trace-level*)))
                           (unwind-protect
                             (progn
                               (sys:trace-enter lex-n args)
                               (let ((val (apply prev args)))
                                 (sys:trace-leave lex-n val)
                                 (set abandoned nil)
                                 val))
                             (if abandoned
                               (sys:trace-leave lex-n :abandoned)))))))
            (set [sys:*trace-hash* n] prev)
            (set (symbol-function n) hook)))))))

(defun sys:untrace (names)
  (flet ((disable (name-orig name)
           (let ((prev (del [sys:*trace-hash* name])))
             (when prev
               (when (neq name-orig name)
                 (catch
                   (throwf 'warning "~s: ~s is actually ~s: untracing that instead"
                           'trace name-orig name)
                   (continue ())))
               (set (symbol-function name) prev)))))
    (if names
      (each ((n-orig names)
             (n [mapcar sys:trace-canonicalize-name names]))
        (disable n-orig n))
      (dohash (n v sys:*trace-hash*)
        (disable n n)))))

(defun sys:trace-redefine-check (orig-name)
  (let ((name (sys:trace-canonicalize-name orig-name)))
    (when [sys:*trace-hash* name]
      (catch
        (cond
          ((neq name orig-name)
             (throwf 'warning "~!~s won't be traced, though it overrides\n\
                               ~s which is currently traced"
                      name orig-name))
          (t (throwf 'warning "previously traced ~s is redefined and no\ \
                               longer traced"
                      name)
             (sys:untrace (list name))))
        (continue ())))))

(defmacro trace (. names)
  ^(sys:trace ',names))

(defmacro untrace (. names)
  ^(sys:untrace ',names))
