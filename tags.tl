#!/usr/bin/env txr

(defvar *tags-lib*)

;; The etags format is described here:
;; https://git.savannah.gnu.org/cgit/emacs.git/tree/etc/ETAGS.EBNF.
;;
;; Unmentioned in the document is that the line number is 1-based and
;; the byte offset 0-based.
(defparml etag-sec-start #\x0c)
(defparml etag-pat-end #\x7f)
(defparml etag-name-end #\x01)
(defparml etag-nonname-chars " \f\t\n\r()=,;'")

(define-option-struct tags-opts nil
  (nil help    :bool "List this help text and exit.")
  (o   output  :text "Act on the tags file named TEXT.")
  (a   append  :bool "Append to existing tags file, without sorting.")
  (m   merge   :bool "Merge with existing tags file, sorting combined content.")
  (nil exclude (cumul :text) "Skip paths matching glob pattern given \ \
                              in TEXT. Multiple patterns can be specified.")
  (e   emacs   :bool "Write the tags file in Emacs's etags format.")
  (q   qual    :bool "Also generate struct:slot tags for each slot."))

(defun escape (str)
  (mappend (do caseql @1
             ((#\^ #\$ #\/ #\\) (list #\\ @1))
             (t (list @1)))
           str))

(defstruct tag ()
  ident
  path
  linum
  byte
  line
  (type "?")

  (:postinit (me)
    (upd me.ident tostringp))

  (:method text (me)
    `@{me.ident}\t@{me.path}\t/^@(escape me.line)$/;"\t@{me.type}`)

  (:method etext (me)
    `@{me.line}@{etag-pat-end} \
     @{me.ident}@{etag-name-end} \
     @{me.linum},@{me.byte}`))

(defstruct file-tag tag
  (type "F")
  (:postinit (me)
    (set me.ident (base-name me.path)))
  (:method text (me)
    `@{me.ident}\t@{me.path}\t;"\t@{me.type}`))

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
    `@{me.ident}\t@{me.path}\t/^@(escape me.line)$/@(if me.expattern `\x3b/@(escape me.ident)/`)\x3b"\t@{me.type}\tstruct:@{me.parent}`)
  (:method make-qual-tag (me)
    (if me.parent
      (let ((qt (copy me)))
        (set qt.ident `@{me.parent}:@{me.ident}`)
        qt))))

(defstruct orig-tag tag
  ;; We reuse the line slot as the already-escaped ctag pattern.
  orig-fields
  (:method text (me)
    `@{me.ident}\t@{me.path}\t@{me.line} \
     @(if me.orig-fields `\t@(cat-str me.orig-fields #\tab)`)`))

(defvarl err-ret (gensym))

(defvar *fake-load-path*)

(defun get-pos-line (lines form)
  (tree-case (source-loc form)
    ((line . file)
     ;; The file-get-string function keeps carriage returns, so the byte
     ;; offset is correct even with \r\n line separators.
     (let ((byte (+ line ; Count the newlines.
                    -1   ; Adjust the byte offset to be 0-based.
                    [sum (take line lines) coded-length])))
       (cons (cons line byte) [lines line])))))

(defmacro in-anon-package (. body)
  (with-gensyms (pkg)
    ^(let* ((*package-alist* *package-alist*)
            (,pkg (sys:make-anon-package t))
            (*package* ,pkg))
       (set-package-fallback-list *package* '(:usr))
       ,*body)))

(defmacro with-tag-shorthand-macro ((name-sym path-var lines-var obj-var)
                                    . body)
  ^(macrolet ((,name-sym (type ident : parent pattern-obj)
                (with-gensyms (linum byte line)
                  ^(tree-case ,(if pattern-obj
                                 ^(get-pos-line ,',lines-var ,pattern-obj)
                                 ^(get-pos-line ,',lines-var ,',obj-var))
                     (((,linum . ,byte) . ,line)
                      (new ,type ident ,ident
                                 path ,',path-var
                                 linum ,linum
                                 byte ,byte
                                 line ,line
                                 ,*(if parent ^(parent ,parent))
                                 ,*(if pattern-obj ^(expattern t))))))))
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
          ((defun defmacro define-place-macro defmatch deffi deffi-cb)
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
          ((defstruct deffi-struct)
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
         ;; Make line numbers and byte offsets 1-based.
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
         ;; Make line numbers and byte offsets 1-based.
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

  (defun parse-etag-path (stream)
    (let ((line (get-line stream)))
      (unless line
        (throw 'syntax-error "trailing etag section starter"))
      (match-case line
        (`@{path #/[^,]+/},@{size #/\d+/}` path)
        (@otherwise
         (throwf 'syntax-error "bad etag path line: ~s" line)))))

  (defun get-etag-name (line)
    (let ((etag-pat-end etag-pat-end)
          (etag-name-end etag-name-end))
      (match-case line
        (`@pat@{etag-pat-end}@ident@{etag-name-end}@rest` ident)
        (`@pat@{etag-pat-end}@rest`
         (labels ((nonname-char-p (ch) (in etag-nonname-chars ch)))
           (when (nonname-char-p [pat -1])
             (set pat [pat 0..-1]))
           (let ((pos [rpos-if nonname-char-p pat]))
             (when pos (inc pos))
             [pat pos..:])))
        (@otherwise
         (throwf 'syntax-error "bad etag line: ~s" line)))))

  ;; Does not support include sections.
  ;; Does not support file properties.
  (defun read-etagfile (path)
    (with-stream (stream (open-file path))
      (let ((line (get-line stream)))
        (unless line (return nil))
        (unless (equal line (tostringp etag-sec-start))
          (throwf 'syntax-error "bad etag section starter: ~s" line)))
      (let ((all-tags ()))
        (whilet ((path (parse-etag-path stream))
                 (tags ())
                 (t))
          (whilet ((line (get-line stream))
                   (next (equal line (tostringp etag-sec-start)))
                   (t))
            (when (or (not line) next)
              (push (cons path tags) all-tags)
              (if next
                (return)
                (return-from read-etagfile all-tags)))
            (push (new orig-tag
                       ident (get-etag-name line)
                       orig-line line)
                  tags))))))

(defun read-tagfile (path)
  (catch (let ((lines (file-get-lines path)))
           (collect-each ((line lines))
             (tree-bind (ident path pat . fields) (split-str line #\tab)
               (new orig-tag
                    ident ident
                    path path
                    line pat
                    orig-fields fields))))
    (path-not-found (_))))

(defun write-tagfile (tags o)
  (when o.merge
    (whenlet ((orig-tags (read-tagfile o.output)))
      (set tags (merge tags orig-tags : .ident))))
  (with-stream (stream (open-file o.output (if o.append "a" "w")))
    (each ((tag tags))
      (put-line tag.(text) stream))))

(defun write-etagfile (grouped-etags o)
  (with-stream (stream (open-file o.output (if o.append "a" "w")))
    (each ((pair grouped-etags))
      (tree-bind (path . etags) pair
        (let ((str (with-out-string-stream (s)
                     (each ((etag etags))
                       (put-line etag.(etext) s)))))
          (put-string `@{etag-sec-start}\n@{path},@(len str)\n@{str}`
                      stream))))))

(defvarl ftw-actionretval 0)
(defvarl ftw-continue 0)
(defvarl ftw-skip-subtree 0)

(defmacro static-when (expr . body)
  (when (eval expr) ^(progn ,*body)))

(compile-only
  (unless *tags-lib*
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

      (unless (plusp (len o.output))
        (set o.output (if o.emacs "TAGS" "tags")))

      (when (and o.merge (or o.append o.emacs))
        ;; The --merge option (without --emacs) currently results in
        ;; duplicate tags if a file is retagged (e.g., "txr tags.tl foo.tl
        ;; && txr tags.tl --merge foo.tl").
        ;; We could have --merge replace all existing tags of a retagged
        ;; file with the latest ones, with and without --emacs, but for
        ;; now don't bother (and therefore forbid combining --emacs with
        ;; --merge).
        (put-line `@{*load-path*}: @(if o.append `--append` `--emacs`)\ \
                   and --merge are mutually exclusive`)
        (exit nil))

      (let* ((excf [apply orf (mapcar (do op fnmatch @@1) o.exclude)])
             (skips ())
             (*read-unknown-structs* t)
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
        (if o.qual
          (set tags (build
                      (pend tags)
                      (each ((tg tags))
                        (if (typep tg 'slot-tag)
                          (iflet ((qt tg.(make-qual-tag)))
                            (add qt)))))))
        (if o.emacs
          (write-etagfile (flow tags
                                (remove-if (op equal @1.type "F"))
                                (nsort @1 : .linum)
                                (group-by .path)
                                (hash-alist)
                                (nsort @1 : car))
                          o)
          (write-tagfile (nsort tags : .ident) o))))))
