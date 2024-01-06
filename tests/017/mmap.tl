(load "../common")

(defun parent (wp mm)
  (with-stream (s (open-fileno wp "w"))
    (each ((i 0..1024))
      (set [mm i] i))
    (put-char #\X s)))

(defun child (rp mm)
  (let ((s (open-fileno rp "r")))
    (assert (eq (get-char s) #\X))
    (each ((i 0..1024))
      (assert (eql [mm i] i)))))

(let ((mm (mmap (ffi uint32) 4096
                (logior prot-read prot-write)
                (logior map-anon map-shared))))
  (tree-bind (rp . wp) (pipe)
    (match-ecase (fork)
      (0 (child rp mm)
         (exit t))
      (-1 (error "fork failed"))
      (@pid (parent wp mm)
            (tree-bind (p . s) (wait pid)
              (unless (zerop s)
                (error "child failed")))))))

(assert (plusp page-size))

(let* ((rndbuf0 (random-buf page-size))
       (rndbuf1 (random-buf page-size))
       (fname "rand.bin"))
  (unwind-protect
    (progn
      (file-put-buf fname rndbuf0)
      (let* ((mm (mmap (ffi uchar) page-size
                       (logior prot-read prot-write)
                       (logior map-shared)
                       fname)))
        (each ((i 0..page-size))
          (assert (eq [rndbuf0 i] [mm i]))
          (set [mm i] [rndbuf1 i]))
        (msync mm ms-sync)
        (assert (equal (file-get-buf fname) rndbuf1))
        (each ((i 0..page-size))
          (set [mm i] [rndbuf0 i]))
        (munmap mm))
      (assert (equal (file-get-buf fname) rndbuf0)))
    (remove-path fname)))

(unless (meq (os-symbol) :bsd :openbsd)
  (test (ignerr (mmap (ffi char) 4096 prot-read map-anon)) nil))
