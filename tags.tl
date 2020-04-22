#!/usr/bin/env txr

(define-option-struct tags-opts nil
  (nil help    :bool "List this help text.")
  (a   append  :bool "Append to existing tags file, without sorting.")
  (m   merge   :bool "Merge with existing tags file, sorting combined content.")
  (nil exclude (cumul :text) "Skip paths matching glob pattern given \ \
                              in TEXT. Multiple patterns can be specified."))

(defstruct tag ()
  ident
  path
  pattern
  (type "?")

  (:postinit (me)
    (upd me.ident tostringp))

  (:method text (me)
    `@{me.ident}\t@{me.path}\t/^@{me.pattern}$/;"\t@{me.type}`))

(defun escape (str)
  (mappend (do caseql @1
             ((#\^ #\$ #\/ #\\) (list #\\ @1))
             (t (list @1)))
           str))

(defstruct file-tag tag
  (:postinit (me)
    (set me.ident (base-name me.path)))
  (:method text (me)
    `@{me.ident}\t@{me.path}\t;"\tF`))

(defstruct fun-tag tag
  (type "f"))

(defstruct var-tag tag
  (type "v"))

(defstruct struct-tag tag
  (type "s"))

(defstruct type-tag tag
  (type "t"))

(defstruct slot-tag tag
  (type "m")
  parent
  expattern
  (:method text (me)
    `@{me.ident}\t@{me.path}\t/^@{me.pattern}$/ \
     @(if me.expattern `;/@(escape me.ident)/`);"\t \
     @{me.type}\tstruct:@{me.parent}`))

(defstruct orig-tag tag
  line
  (:method text (me) me.line))

(defvarl err-ret (gensym))

(defvar *fake-load-path*)

(defun get-pat (lines form)
  (tree-case (source-loc form)
    ((line . file) (escape [lines line]))))

(defmacro in-anon-package (. body)
  (with-gensyms (pkg)
    ^(let* ((*package-alist* *package-alist*)
            (,pkg (sys:make-anon-package))
            (*package* ,pkg))
       (set-package-fallback-list *package* '(:usr))
       ,*body)))

(defmacro with-tag-shorthand-macro ((name-sym path-var lines-var obj-var)
                                    . body)
  ^(macrolet ((,name-sym (type ident : parent pattern-obj)
                ^(new ,type ident ,ident
                            path ,',path-var
                            pattern ,*(if pattern-obj
                                        ^((get-pat ,',lines-var ,pattern-obj))
                                        ^((get-pat ,',lines-var ,',obj-var)))
                            ,*(if parent ^(parent ,parent))
                            ,*(if pattern-obj ^(expattern t)))))
    ,*body))

(defun process-package-influencing-form (form)
  (caseq (car form)
    (load (fake-load (cadr form)))
    (load-for (each ((clause (cdr form)))
                (tree-bind (kind sym arg) clause
                  (when (and (eq kind 'pkg)
                             (not (find-package sym)))
                    (fake-load (caddr clause))))))
    (defpackage (make-package (symbol-name (cadr form))))))

(defun fake-load (path)
  (unless (abs-path-p path)
    (set path (path-cat (dir-name *fake-load-path*) path))
    (let ((*fake-load-path* path)
          (stream (if (ends-with ".tl" path)
                    (open-file path)
                    (or (ignerr (open-file `@path.tl`))
                        (open-file path)))))
      (whilet ((obj (read stream *stderr* err-ret path))
               ((neq obj err-ret)))
        (when (consp obj)
          (process-package-influencing-form obj))))))

(defun process-form (path lines obj)
  (build
    (with-tag-shorthand-macro (ntag path lines obj)
      (when (consp obj)
        (process-package-influencing-form obj)
        (caseq (car obj)
          ((progn eval-only compile-only with-dyn-lib macro-time)
           (pend [mappend (op process-form path lines) (cdr obj)]))
          ((defun defmacro define-place-macro deffi deffi-cb)
           (add (ntag fun-tag (cadr obj))))
          ((defvar defvarl defparm defparml defsymacro)
           (add (ntag var-tag (cadr obj))))
          ((defmeth)
           (add (ntag slot-tag (caddr obj) (cadr obj))))
          ((defplace)
           (tree-bind (op (name . args) . body) obj
             (add (ntag fun-tag name))))
          ((typedef)
           (add (ntag type-tag (cadr obj))))
          ((defpackage)
           (add (ntag struct-tag (cadr obj))))
          ((define-option-struct)
           (let ((struct-name (cadr obj)))
             (add (ntag struct-tag struct-name))
             (each ((obj (cdddr obj)))
               (tree-bind (short long . rest) obj
                 (cond
                   (long (add (ntag slot-tag long struct-name)))
                   (short (add (ntag slot-tag short struct-name))))))))
          ((defstruct)
           (let ((struct-obj obj)
                 (struct-name (tree-case (cadr obj)
                                ((atom . rest) atom)
                                (atom atom))))
             (add (ntag struct-tag struct-name))
             (each ((obj (cdddr obj)))
               (tree-case obj
                 ((word name . rest)
                  (caseq word
                    ((:method :function :static :instance)
                     (add (ntag slot-tag name struct-name)))
                    (t :)))
                 ((word (arg) . body)
                  (caseq word
                    ((:init :postinit :fini))
                    (t :)))
                 ((name . rest)
                  (add (ntag slot-tag name struct-name)))
                 (name
                   (add (ntag slot-tag name struct-name struct-obj))))))))))))

(defun unexpand (form)
  (whilet ((anc (macro-ancestor form)))
    (set form anc))
  form)

(defun process-clause (path lines clause)
  (when (consp clause)
    (let ((elem (car clause)))
      (build
        (with-tag-shorthand-macro (ntag path lines elem)
          (when (consp elem)
            (caseq (car elem)
              (define (let ((args (if (eq t (cadr elem))
                                    (cadddr elem)
                                    (cadr elem))))
                        (add (ntag fun-tag (car args)))))
              (bind (let ((syms (flatcar (cadr elem))))
                      (each ((sym syms))
                        (add (ntag var-tag sym)))))
              (do (let ((forms [mapcar unexpand (cdr elem)]))
                    (each ((form forms))
                      (pend (process-form path lines form))))))))))))

(defun collect-tags-tl (path)
  (let* ((text (file-get-string path))
         (text (if (starts-with "#!" text) `;@text` text))
         (lines (cons "" (spl #\newline text)))
         (stream (make-string-byte-input-stream text))
         (*rec-source-loc* t)
         (*fake-load-path* path))
    (build
      (add (new file-tag
                path path))
      (in-anon-package
        (whilet ((obj (read stream *stderr* err-ret path))
                 ((neq obj err-ret)))
          (pend (process-form path lines obj)))))))

(defun collect-tags-txr (path)
  (let* ((text (file-get-string path))
         (text (if (starts-with "#!" text) `\@;@text` text))
         (lines (cons "" (spl #\newline text)))
         (stream (make-string-byte-input-stream text))
         (*rec-source-loc* t)
         (syntax (in-anon-package (txr-parse stream *stderr* nil path))))
    (build
      (each ((clause syntax))
        (pend (process-clause path lines clause))))))

(defun collect-tags-guess (path)
  (with-stream (s (open-file path))
    (iflet ((line (get-line s)))
      (if (and (starts-with "#!" line)
               (search-str line "txr"))
        (if (search-str line "--lisp")
          (collect-tags-tl path)
          (collect-tags-txr path))
        (progn
          (put-line `@path: unable to determine file type` *stderr*)
          nil)))))

(defun write-tagfile (tags o)
  (when o.merge
    (catch
      (let* ((lines (file-get-lines "tags"))
             (orig-tags (collect-each ((line lines))
                          (new orig-tag ident (m^ #/[^\t]*/ line)
                                        line line))))
        (set tags (merge tags orig-tags : .ident)))
      (path-not-found (e))))
  (with-stream (stream (open-file "tags" (if o.append "a" "w")))
    (each ((tag tags))
      (put-line tag.(text) stream))))

(defvarl ftw-actionretval 0)
(defvarl ftw-continue 0)
(defvarl ftw-skip-subtree 0)

(defmacro static-when (expr . body)
  (when (eval expr) ^(progn ,*body)))

(compile-only
  (let ((o (new tags-opts)))
    o.(getopts *args*)
    (when o.help
      (put-line "\nUsage:\n")
      (put-line `  @{*load-path*} [options] {file|dir}*\n`)
      (put-line "Directory arguments are recursively searched for .tl and .txr files.")
      (put-line "If no arguments are given, the current directory is searched.")
      o.(opthelp)
      (exit t))

    (unless o.out-args
      (push "." o.out-args))

    (when (and o.merge o.append)
      (put-line `@{*load-path*}: --append and --merge are mutually exclusive`)
      (exit nil))

    (let* ((have-arv (boundp 'ftw-actionretval))
           (excf [apply orf (mapcar (do op fnmatch @@1 @1) o.exclude)])
           (skips ())
           (tags (build
                   (ftw o.out-args
                        (lambda (path type stat . rest)
                          (caseql* type
                            (ftw-f (when (and (not [excf path])
                                              (not [excf (base-name path)])
                                              (not (some skips (op starts-with @1 path))))
                                     (cond
                                       ((ends-with ".tl" path)
                                        (pend (ignerr (collect-tags-tl path))))
                                       ((ends-with ".txr" path)
                                        (pend (ignerr (collect-tags-txr path))))
                                       ((member path o.out-args)
                                        (pend (ignerr (collect-tags-guess path))))
                                       (t ftw-continue))))
                            (ftw-d (while (and skips (starts-with path (car skips)))
                                     (pop skips))
                                   (cond
                                     ((or [excf path] [excf (base-name path)])
                                      (static-when (zerop ftw-actionretval)
                                        (push `@path/` skips))
                                      ftw-skip-subtree)))
                            (t ftw-continue)))
                        (logior ftw-phys ftw-actionretval)))))
      (write-tagfile (sort tags : .ident) o))))
