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
