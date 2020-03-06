#!/usr/bin/env txr

(define-option-struct tags-opts nil
  (nil help    :bool "List this help text.")
  (a   append  :bool "Append to existing tags file, without sorting.")
  (m   merge   :bool "Merge with existing tags file, sorting combined content."))

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
             ((#\^ #\$ #\/) (list #\\ @1))
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

(defun get-pat (lines form)
  (tree-case (source-loc form)
    ((line . file) (escape [lines line]))))

(defun collect-tags (path)
  (let* ((lines (vec-list (cons "" (file-get-lines path))))
         (stream (make-strlist-input-stream lines))
         (*rec-source-loc* t))
    (with-stream (stream (open-file path))
      (if (starts-with "#!" (get-line stream))
        (pop lines)
        (seek-stream stream 0 :from-start))
      (build
        (add (new file-tag
                  path path))
        (macrolet ((ntag (type ident : parent pattern-obj)
                     ^(new ,type ident ,ident
                                 path path
                                 pattern ,*(if pattern-obj
                                             ^((get-pat lines ,pattern-obj))
                                             ^((get-pat lines obj)))
                                 ,*(if parent ^(parent ,parent))
                                 ,*(if pattern-obj ^(expattern t)))))
          (labels ((process-form (obj)
                     (when (consp obj)
                       (caseq (car obj)
                         ((progn eval-only compile-only with-dyn-lib)
                          [mapdo process-form (cdr obj)])
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
                              (add (ntag slot-tag (car obj) struct-name))
                              (add (ntag slot-tag (cadr obj) struct-name)))))
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
                                 (add (ntag slot-tag name struct-name struct-obj)))))))))))
            (whilet ((obj (read stream *stderr* err-ret))
                     ((neq obj err-ret)))
              (process-form obj))))))))

(defun write-tagfile (tags o)
  (when o.merge
    (catch
      (let* ((lines (file-get-lines "tags"))
             (orig-tags (collect-each ((line lines))
                          (new orig-tag ident (m^ #/[^\t]*/ line)
                                        line line))))
        (set tags (append tags orig-tags)))
      (path-not-found (e))))
  (with-stream (stream (open-file "tags" (if o.append "a" "w")))
    (each ((tag (sort tags : .ident)))
      (put-line tag.(text) stream))))

(let ((o (new tags-opts)))
  o.(getopts *args*)
  (when o.help
    (put-line "\nUsage:\n")
    (put-line `  @{*load-path*} [options] {file|dir}*\n`)
    (put-line `Directory arguments are recursively searched for *.tl files.`)
    (put-line `If no arguments are given, the current directory is searched.`)
    o.(opthelp)
    (exit t))

  (unless o.out-args
    (push "." o.out-args))

  (when (and o.merge o.append)
    (put-line `@{*load-path*}: --append and --merge are mutually exclusive`)
    (exit nil))

  (let ((tags (build
                (ftw o.out-args
                     (lambda (path type stat . rest)
                       (when (and (eql type ftw-f)
                                  (or (member path o.out-args)
                                      (ends-with ".tl" path)))
                         (pend (ignerr (collect-tags path)))))
                     ftw-phys))))
    (write-tagfile (sort tags : .ident) o)))
