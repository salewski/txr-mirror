(defvar *trace-output* *stdout*)

(defvar sys:*trace-hash* (hash))
(defvar sys:*trace-level* -1)

(defun sys:trace-enter (name args)
  (format *trace-output* "~*a(~s ~s\n" (* sys:*trace-level* 2) "" name args))

(defun sys:trace-leave (name val)
  (format *trace-output* "~*a  ~s)\n" (* sys:*trace-level* 2) "" val))

(defun sys:trace (names)
  (cond
    ((null names) (hash-keys sys:*trace-hash*))
    (t
      (each ((n names))
        (unless [sys:*trace-hash* n]
          (let* ((name n)
                 (prev (symbol-function n))
                 (hook (lambda (. args)
                         (let ((abandoned t)
                               (sys:*trace-level* (succ sys:*trace-level*)))
                           (unwind-protect
                             (progn
                               (sys:trace-enter name args)
                               (let ((val (apply prev args)))
                                 (sys:trace-leave name val)
                                 (set abandoned nil)
                                 val))
                             (if abandoned
                               (sys:trace-leave name :abandoned)))))))
            (set [sys:*trace-hash* n] prev)
            (set (symbol-function n) hook)))))))

(defun sys:untrace (names)
  (flet ((disable (name)
           (let ((prev (del [sys:*trace-hash* name])))
             (when prev
               (set (symbol-function name) prev)))))
    (if names
      (each ((n names))
        (disable n))
      (dohash (n v sys:*trace-hash*)
        (disable n)))))

(defmacro trace (. names)
  ^(sys:trace ',names))

(defmacro untrace (. names)
  ^(sys:untrace ',names))
