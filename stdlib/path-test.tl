;; Copyright 2015-2022
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
