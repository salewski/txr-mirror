;; Copyright 2018-2021
;; Kaz Kylheku <kaz@kylheku.com>
;; Vancouver, Canada
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;
;; 1. Redistributions of source code must retain the above copyright notice,
;;    this list of conditions and the following disclaimer.
;;
;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;    this list of conditions and the following disclaimer in the documentation
;;    and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.
(eval-only
  (defsymacro copy-size 65536))

(defstruct copy-path-opts ()
  perms times owner symlinks (euid (geteuid)))

(defstruct copy-path-stack-node ()
  path stat new-p)

(defun make-copy-path-opts (opt-list)
  (if opt-list
    (let (opts)
      (each ((opt opt-list))
        (if (structp opt)
          (set opts opt)
          (progn
            (unless opts
              (set opts (new copy-path-opts)))
            (caseql opt
              (:perms (set opts.perms t))
              (:times (set opts.times t))
              (:owner (set opts.owner t))
              (:symlinks (set opts.symlinks t))
              (:all (set opts.perms t
                         opts.times t
                         opts.owner t
                         opts.symlinks t))
              (t (error "~s: unrecognized option ~s" 'copy-path opt))))))
      opts)
    (load-time (new copy-path-opts))))

(defun copy-file (from-path to-path : preserve-perms preserve-times)
  (with-resources ((buf (make-buf copy-size)
                        (buf-set-length buf 0) (buf-trim buf))
                   (ist (open-file from-path "b") (close-stream ist))
                   (ista (fstat ist))
                   (ost (if (path-dir-p ista)
                          (throwf 'path-permission `~s: ~a is a directory`
                                  'copy-file from-path)
                          (open-file to-path "wb"))
                        (close-stream ost)))
    (while (eql (len buf) copy-size)
      (fill-buf-adjust buf 0 ist)
      (put-buf buf 0 ost))
    (when preserve-perms
      (chmod ost ista.mode))
    (when preserve-times
      (flush-stream ost)
      (utimes ost
              ista.atime (or ista.atime-nsec 0)
              ista.mtime (or ista.mtime-nsec 0)))
    nil))

(defun copy-files (paths dest-dir : preserve-perms preserve-times)
  (each ((path paths))
    (while t
      (catch**
        (return (copy-file path (path-cat dest-dir (base-name path))
                           preserve-perms preserve-times))
        (skip `skip copying @path` (exc . args) (return))
        (retry `retry copying @path` (exc . args))))))

(defun do-tweak-obj (to-path st opts link-p)
  (when (and opts.perms (not link-p))
    (chmod to-path st.mode))
  (when opts.times
    (lutimes to-path
             st.atime (or st.atime-nsec 0)
             st.mtime (or st.mtime-nsec 0)))
  (when (and opts.owner
             (or (zerop opts.euid)
                 (and (path-mine-p st)
                      (path-my-group-p st))))
    (lchown to-path st.uid st.gid)))

(defun do-copy-obj (from-path to-path st opts)
  (let ((type (logand st.mode s-ifmt))
        (initial-perms (if opts.perms #o700 #o777))
        (tweak t))
    (caseql* type
      (s-ifreg
        (copy-file from-path to-path opts.perms opts.times))
      (s-ifsock
        (mknod to-path (logior type initial-perms)))
      (s-ififo
        (mkfifo to-path initial-perms))
      (s-iflnk
        (if opts.symlinks
          (symlink (readlink from-path) to-path)
          (progn
            (do-copy-obj from-path to-path (stat from-path) opts)
            (set tweak nil))))
      ((s-ifblk s-ifchr)
       (mknod to-path (logior type initial-perms) st.rdev))
      (s-ifdir
        (ensure-dir to-path)))
    (when tweak
      (do-tweak-obj to-path st opts (eq type s-iflnk)))))

(defun copy-path-rec (from-dir to-dir . opt-list)
  (let* ((opts (make-copy-path-opts opt-list))
         (dir-stack nil))
    (unwind-protect
      (ftw from-dir
           (lambda (path type stat . rest)
             (while t
               (catch**
                 (let* ((rel-path (let ((p [path (len from-dir)..:]))
                                    (if (pure-rel-path-p p) p [p 1..:])))
                        (tgt-path (path-cat to-dir rel-path)))
                   (unless (starts-with from-dir path)
                     (error "~s: problem with directory traversal" 'copy-path))
                   (caseql* type
                     ((ftw-dnr ftw-ns) (error "~s: unable to access ~s"
                                              'copy-path path))
                     (ftw-d (let ((new-p (ensure-dir tgt-path)))
                              (whilet ((top (car dir-stack))
                                       ((and top
                                             (not (starts-with tgt-path
                                                               top.path)))))
                                (do-tweak-obj top.path top.stat opts nil)
                                (pop dir-stack))
                              (push (new copy-path-stack-node
                                         path tgt-path
                                         stat stat
                                         new-p new-p)
                                    dir-stack)))
                     (t (iflet ((cur (car dir-stack)))
                          (unless cur.new-p
                            (remove-path tgt-path)))
                        (do-copy-obj path tgt-path stat opts)))
                   (return))
                 (skip `skip copying @path` (exc . args) (return))
                 (retry `retry copying @path` (exc . args)))))
           ftw-phys)
      (whilet ((top (pop dir-stack)))
        (do-tweak-obj top.path top.stat opts nil)))))

(defun remove-path-rec (path)
  (ftw path
       (lambda (path type stat . rest)
         (while t
           (catch**
             (return
               (caseql* type
                 ((ftw-dnr ftw-ns) (error "~s: unable to access ~s"
                                          'remove-rec path))
                 (ftw-dp (rmdir path))
                 (t (remove-path path))))
             (skip `skip removing @path` (exc . args) (return))
             (retry `retry copying @path` (exc . args)))))
       (logior ftw-phys ftw-depth)))

(defun chmod-rec (path perm)
  (ftw path
       (lambda (path type stat . rest)
         (while t
           (catch**
             (return
               (caseql* type
                 ((ftw-dnr ftw-ns) (error "~s: unable to access ~s"
                                          'remove-rec path))
                 (ftw-sl)
                 (t (chmod path perm))))
             (skip `skip chmod @path` (exc . args) (return))
             (retry `retry chmod @path` (exc . args)))))
       (logior ftw-phys)))

(defun chown-rec (path uid gid)
  (ftw path
       (lambda (path type stat . rest)
         (while t
           (catch**
             (return
               (caseql* type
                 ((ftw-dnr ftw-ns) (error "~s: unable to access ~s"
                                          'remove-rec path))
                 (t (lchown path uid gid))))
             (skip `skip chown @path` (exc . args) (return))
             (retry `retry chown @path` (exc . args)))))
       (logior ftw-phys)))

(defun touch (path : ref-path)
  (with-stream (s (or (ignerr (open-file path "mn")) (open-file path "n")))
    (if ref-path
      (let ((rst (stat ref-path)))
        (utimes s 0 nil rst.mtime rst.mtime-nsec))
      (utimes s 0 nil 0 t))))

(defun path-simplify (comp)
  (let (out)
    (each ((c comp))
      (casequal c
        (".." (if (and out (nequal (car out) ".."))
                (pop out)
                (push c out)))
        (("." ""))
        (t (push c out))))
    (nreverse out)))

(defun rel-path (from to)
  (unless (eq (abs-path-p from) (abs-path-p to))
    (error "~s: mixture of absolute and relative paths ~s ~s given"
           'rel-path from to))
  (if (macro-time (find #\\ path-sep-chars))
    (if-match `@{fdrv #/[A-Za-z0-9]+:/}@nil` from
      (if-match `@{tdrv #/[A-Za-z0-9]+:/}@nil` to
        (unless (equal fdrv tdrv)
          (error "~s: paths on different drives ~s ~s given"
                 'rel-path from to)))))
  (let* ((fcomp (path-simplify (spl path-sep-chars from)))
         (tcomp (path-simplify (spl path-sep-chars to)))
         (ncommon (mismatch fcomp tcomp)))
    (cond
      ((null ncommon) ".")
      ((find ".." (nthcdr ncommon fcomp))
       (error "~s: from path uses .. to escape common prefix: ~s ~s"
              'rel-path from to))
      (t (let ((nup (- (len fcomp) ncommon))
               (down [tcomp ncommon..:]))
           (cat-str (append (repeat '("..") nup) down)
                    [path-sep-chars 0]))))))
