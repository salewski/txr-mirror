#!/usr/bin/env txr

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

(defvarl err-ret (gensym))

(defun get-pat (lines form)
  (tree-case (source-loc form)
    ((line . file) (escape [lines line]))))

(defun collect-tags (path)
  (let* ((lines (vec-list (cons "" (file-get-lines path))))
         (stream (make-strlist-input-stream lines))
         (*rec-source-loc* t))
    (with-stream (stream (open-file path))
      (unless (starts-with "#!" (get-line stream))
        (seek-stream stream 0 :from-start))
      (build
        (add (new file-tag
                  path path))
        (labels ((process-form (obj)
                   (when (consp obj)
                     (caseq (car obj)
                       ((progn eval-only compile-only with-dyn-lib)
                        [mapdo process-form (cdr obj)])
                       ((defun defmacro define-place-macro deffi deffi-cb)
                        (add (new fun-tag
                                  ident (cadr obj)
                                  path path
                                  pattern (get-pat lines obj))))
                       ((defvar defvarl defparm defparml defsymacro)
                        (add (new var-tag
                                  ident (cadr obj)
                                  path path
                                  pattern (get-pat lines obj))))
                       ((defmeth)
                        (add (new slot-tag
                                  ident (caddr obj)
                                  path path
                                  pattern (get-pat lines obj)
                                  parent (cadr obj))))
                       ((defplace)
                        (tree-bind (op (name . args) . body) obj
                          (add (new fun-tag
                                    ident name
                                    path path
                                    pattern (get-pat lines obj)))))
                       ((typedef)
                        (add (new type-tag
                                  ident (cadr obj)
                                  path path
                                  pattern (get-pat lines obj))))
                       ((defpackage)
                        (add (new struct-tag
                                  ident (cadr obj)
                                  path path
                                  pattern (get-pat lines obj))))
                       ((defstruct)
                        (let ((struct-name (tree-case (cadr obj)
                                             ((atom . rest) atom)
                                             (atom atom))))
                          (add (new struct-tag
                                    ident struct-name
                                    path path
                                    pattern (get-pat lines obj)))
                          (each ((slot (cdddr obj)))
                            (tree-case slot
                              ((word name . rest)
                               (caseq word
                                 ((:method :function :static :instance)
                                  (add (new slot-tag
                                            ident name
                                            path path
                                            pattern (get-pat lines slot)
                                            parent struct-name)))
                                 (t :)))
                              ((word (arg) . body)
                               (caseq word
                                 ((:init :postinit :fini))
                                 (t :)))
                              ((name . rest)
                               (add (new slot-tag
                                         ident name
                                         path path
                                         pattern (get-pat lines slot)
                                         parent struct-name)))
                              (name
                                (add (new slot-tag
                                          ident name
                                          path path
                                          pattern (get-pat lines obj)
                                          expattern t
                                          parent struct-name)))))))))))
          (whilet ((obj (read stream *stderr* err-ret))
                   ((neq obj err-ret)))
            (process-form obj)))))))

(defun write-tagfile (tags)
  (with-stream (stream (open-file "tags" "w"))
    (each ((tag tags))
      (put-line tag.(text) stream))))

(let ((tags (build
              (ftw *args*
                   (lambda (path type stat . rest)
                     (when (and (eql type ftw-f)
                                (ends-with ".tl" path))
                       (pend (ignerr (collect-tags path)))))
                   ftw-phys))))
  (write-tagfile (sort tags : .ident)))
