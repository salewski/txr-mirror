;; Copyright 2015-2023
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

(defun sys:do-path-test (statfun path testfun)
  [testfun (if (typep path 'stat)
             path
             (ignerr [statfun path]))])

(eval-only
  (defmacro sys:path-test ((sym statfun path) . body)
    ^[sys:do-path-test ,statfun ,path
       (lambda (,sym) (when ,sym ,*body))]))

(defun sys:path-test-type (statfun path code)
  (sys:path-test (s statfun path)
    (eql (logand s.mode s-ifmt) code)))

(defun sys:path-test-mode (statfun path mask)
  (sys:path-test (s statfun path)
    (plusp (logand s.mode mask))))

(defun path-exists-p (path)
  (sys:path-test (s stat path) t))

(defun path-file-p (path)
  [sys:path-test-type stat path s-ifreg])

(defun path-dir-p (path)
  [sys:path-test-type stat path s-ifdir])

(defun path-symlink-p (path)
  [sys:path-test-type lstat path s-iflnk])

(defun path-blkdev-p (path)
  [sys:path-test-type stat path s-ifblk])

(defun path-chrdev-p (path)
  [sys:path-test-type stat path s-ifchr])

(defun path-sock-p (path)
  [sys:path-test-type stat path s-ifsock])

(defun path-pipe-p (path)
  [sys:path-test-type stat path s-ififo])

(defun path-setgid-p (path)
  [sys:path-test-mode stat path s-isgid])

(defun path-setuid-p (path)
  [sys:path-test-mode stat path s-isuid])

(defun path-sticky-p (path)
  [sys:path-test-mode stat path s-isvtx])

(defun path-mine-p (path)
  (sys:path-test (s stat path)
    (= s.uid (getuid))))

(defun path-my-group-p (path)
  (sys:path-test (s stat path)
    (let ((g s.gid))
      (or (= g (getgid))
          (find g (getgroups))))))

;; umask, gmask and omask must test identical permissions
;; multiple permissions may be tested, but not a combination
;; of x with any other permission.
(defun sys:path-access (path umask gmask omask)
  (sys:path-test (s stat path)
    (let ((m s.mode)
          (uid (getuid)))
      (cond
        ((zerop uid) (or (zerop (logand umask s-ixusr))
                         (plusp (logand m (logior umask gmask omask)))))
        ((= uid s.uid) (= umask (logand m umask)))
        ((let ((g s.gid))
           (or (= g (getgid))
               (find g (getgroups))))
         (= gmask (logand m gmask)))
        (t (= omask (logand m omask)))))))

(defun path-executable-to-me-p (path)
  (sys:path-access path s-ixusr s-ixgrp s-ixoth))

(defun path-writable-to-me-p (path)
  (sys:path-access path s-iwusr s-iwgrp s-iwoth))

(defun path-readable-to-me-p (path)
  (sys:path-access path s-irusr s-irgrp s-iroth))

(defun path-read-writable-to-me-p (path)
  (sys:path-access path
                   (logior s-irusr s-iwusr)
                   (logior s-irgrp s-iwgrp)
                   (logior s-iroth s-iwoth)))

(defun path-private-to-me-p (path)
  (sys:path-test (s stat path)
    (let ((m s.mode)
          (uid (geteuid)))
      (mlet ((g (getgrgid s.gid))
             (name (let ((pw (getpwuid uid)))
                     (if pw pw.name)))
             (suname (let ((pw (getpwuid 0)))
                       (if pw pw.name))))
        (and (or (zerop s.uid)
                 (eql uid s.uid))
             (zerop (logand m s-iwoth))
             (or (zerop (logand m s-iwgrp))
                 (null g.mem)
                 (and (all g.mem (orf (op equal name)
                                      (op equal suname))))))))))

(defun path-strictly-private-to-me-p (path)
  (sys:path-test (s stat path)
    (let ((m s.mode)
          (uid (geteuid)))
      (mlet ((g (getgrgid s.gid))
             (name (let ((pw (getpwuid uid)))
                     (if pw pw.name)))
             (suname (let ((pw (getpwuid 0)))
                       (if pw pw.name))))
        (and (or (zerop s.uid)
                 (eql uid s.uid))
             (zerop (logand m (logior s-iroth s-iwoth)))
             (or (zerop (logand m (logior s-irgrp s-iwgrp)))
                 (null g.mem)
                 (and (all g.mem (orf (op equal name)
                                      (op equal suname))))))))))

(defun path-simplify (comp)
  (let ((abs (equal (car comp) ""))
        out)
    (each ((c comp))
      (casequal c
        (".." (if (and (or out abs) (nequal (car out) ".."))
                (pop out)
                (push c out)))
        (("." ""))
        (t (push c out))))
    (nreverse out)))

(eval-only
  (defmacro if-windows (then : else)
    (if (eql 2 (sizeof wchar))
      then
      else))

  (defmacro if-native-windows (then : else)
    (if-windows
      ^(if (find #\\ path-sep-chars) ,then ,else)
      else)))

(defun path-safe-sticky-dir (st)
  (let ((sdir (logior s-ifdir s-isvtx)))
    (and (eql (logand st.mode sdir) sdir)
         (zerop st.uid))))

(if (starts-with "Linux" (uname).sysname)
  (defun safe-abs-path (comps)
    (flet ((digstr (s) [all s chr-isdigit]))
      (let ((safe t))
        (when-match ("proc" @(or @(digstr) "self") . @rest)
                    (path-simplify comps)
          (match-case rest
            (@(or ("cwd" . @nil)
                  ("root" . @nil)
                  ("map_files" . @nil)
                  ("fd" @(digstr) . @nil))
             (zap safe))
            (("task" @(digstr) . @trest)
             (match-case trest
               (@(or ("cwd" . @nil)
                     ("root" . @nil)
                     ("fd" @(digstr) . @nil))
                (zap safe))))))
        safe)))
  (set (symbol-function 'safe-abs-path) (fun tf)))

(defun path-components-safe (path)
  (if-native-windows
    t
    (let* ((abs-p (abs-path-p path))
           (comps (remqual "" (sspl path-sep-chars path)))
           (start (if abs-p "/" ".")))
      (if (and
            (nullify path)
            (or (not abs-p) (safe-abs-path comps))
            (let ((st (stat start)))
              (or (path-private-to-me-p st)
                  (if (nequal start "/")
                    (path-safe-sticky-dir st)))))
        (for ((ok t) (count 0) next (orig-start start))
             ((and ok (set next (pop comps))) ok)
             ()
          (let* ((nxpath (path-cat start next))
                 (st (lstat nxpath)))
            (cond
              ((eql (logand st.mode s-ifmt) s-iflnk)
               (if (> (inc count) 16)
                 (throwf 'file-error "~a: too many symbolic links"
                         'path-components-safe))
               (if (and (eql 1 st.nlink)
                        (or (zerop st.uid)
                            (eql st.uid (geteuid))))
                 (let* ((target (readlink nxpath))
                        (abs-p (abs-path-p target))
                        (tcomps (remqual "" (sspl path-sep-chars target))))
                   (when abs-p
                     (set start "/"
                          ok (and (safe-abs-path comps)
                                  (if (nequal orig-start "/")
                                    (set orig-start nil)
                                    t))))
                   (when ok
                     (set comps (nconc tcomps comps))))
                 (set ok nil)))
              ((or (path-private-to-me-p st)
                   (path-safe-sticky-dir st))
               (set start nxpath))
              (t (zap ok)))))))))

(defmacro sys:path-examine ((sym statfun path) . body)
  ^[sys:do-path-test ,statfun ,path
     (lambda (,sym) ,*body)])

(defun path-newer (path-0 path-1)
  (sys:path-examine (s0 stat path-0)
    (sys:path-examine (s1 stat path-1)
      (if s0
        (or (null s1)
            (let ((mt0 s0.mtime)
                  (mt1 s1.mtime))
              (or (> mt0 mt1)
                  (and (= mt0 mt1)
                       (> s0.mtime-nsec s1.mtime-nsec)))))))))

(defun path-older (path-0 path-1)
  (path-newer path-1 path-0))

(defun path-same-object (path-0 path-1)
  (sys:path-examine (s0 stat path-0)
    (sys:path-examine (s1 stat path-1)
      (and s0 s1
           (eql s0.dev s1.dev)
           (eql s0.ino s1.ino)))))

(defun path-dir-empty (path)
  (when (path-dir-p path)
    (let ((name (if (stringp path) path path.path)))
      (with-stream (ds (open-directory name))
        (for (ent) ((set ent (get-line ds)) t) ()
          (casequal ent
            (("." ".."))
            (t (return nil))))))))

(defun path-split (str)
  (let ((spl0 (sspl path-sep-chars str)))
    (if-native-windows
      (iflet ((head (car spl0))
              (p (pos #\: head)))
        (list* [head 0..(succ p)]
               [head (succ p)..:]
               (cdr spl0))
        spl0)
      spl0)))

(defun path-volume (comp)
  (let ((head (car comp)))
    (if-native-windows
      (let ((next (cadr comp))
            (more (cddr comp)))
        (cond
          ((and (equal head "") (equal next "") more)
           (let ((vol (car more)))
	     (cond
	       ((nequal "" vol)
		(set (car comp) "")
		(set (cdr comp) (cdr more))
		vol)
	       (t :abs))))
          ((and (m^ #/[A-Za-z0-9]+:/ head) head)
           (set (car comp) next)
           (set (cdr comp) more)
           (if (and (equal "" next) more)
             ^(:abs . ,head)
             ^(:rel . ,head)))))
      (if (equal head "") :abs))))

(defun rel-path (from to)
  (let* ((fspl (path-split from))
         (tspl (path-split to))
         (fvol (path-volume fspl))
         (tvol (path-volume tspl)))
    (when (nequal fvol tvol)
      (if (and (meq :abs fvol tvol) (meq nil fvol tvol))
        (error "~s: mixture of absolute and relative paths ~s ~s given"
               'rel-path from to))
      (if (meq :abs fvol tvol)
        (error "~s: mixture of absolute and volume paths ~s ~s given"
               'rel-path from to))
      (if-windows
        (progn
          (when (and (consp fvol) (consp tvol))
            (if (neq (car fvol) (car tvol))
              (error "~s: mixture of volume absolute and relative paths \
                     \ ~s ~s given"
                     'rel-path from to)))
        (when (neq (null fvol) (null tvol))
          (error "~s: mixture of volume and non-volume paths ~s ~s given"
                 'rel-path from to))
        (error "~s: paths on different volumes ~s ~s given"
               'rel-path from to))))
    (let* ((fcomp (path-simplify fspl))
           (tcomp (path-simplify tspl))
           (ncommon (mismatch fcomp tcomp)))
      (cond
        ((null ncommon) ".")
        ((find ".." (nthcdr ncommon fcomp))
         (error "~s: from path uses .. to escape common prefix: ~s ~s"
                'rel-path from to))
        (t (let ((nup (- (len fcomp) ncommon))
                 (down [tcomp ncommon..:]))
             (cat-str (append (repeat '("..") nup) down)
                      [path-sep-chars 0])))))))

(defun path-equal (left right)
  (if (and (stringp left) (equal left right))
    t
    (let* ((lspl (path-split left))
           (rspl (path-split right))
           (lvol (path-volume lspl))
           (rvol (path-volume rspl)))
      (if (nequal lvol rvol)
        nil
        (equal (path-simplify lspl)
               (path-simplify rspl))))))
