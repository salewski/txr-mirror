(load "../common")

(if (not (fboundp 'usr:buf-compress))
  (exit))

(defvarl %have-gzip% (sh "gzip -h > /dev/null  2> /dev/null"))

(defun shchk (cmd)
  (unless (zerop (sh cmd))
    (throw `command @cmd failed`)))

(defun gzip (. files)
  (each ((file files))
    (file-put-string `@file.gz` (file-get-string file) "z")))

(defun cat (dest . files)
  (remove-path dest)
  (each ((file files))
    (file-append-string dest (file-get-string file))))

(push-after-load
  (each ((file '#"test-file test-file.gz \
                  test-file-a.tl test-file-a.tlo test-file-a.tlo.gz \
                  test-file-b.tl test-file-b.tlo test-file-b.tlo.gz \
                  test-file-combined.tlo.gz"))
    (remove-path file)))

(when %have-gzip%
  (file-put-string "test-file" "Hello, World!")
  (remove-path "test-file.gz")
  (shchk "gzip test-file")
  (test (file-get-string "test-file.gz" "z") "Hello, World!")

  (each ((mode (cons "z" (list-seq "z0".."z9"))))
    (file-put-string "test-file.gz" "Goodbye, World!" mode)
    (remove-path "test-file")
    (shchk "gunzip test-file.gz")
    (test (file-get-string "test-file") "Goodbye, World!"))

  (file-put "test-file-a.tl" '(compile-only (put-line "a")))
  (file-put "test-file-b.tl" '(compile-only (put-line "b")))

  (compile-update-file "./test-file-a")
  (compile-update-file "./test-file-b")

  (gzip "test-file-a.tlo" "test-file-b.tlo")

  (cat "test-file-combined.tlo.gz" "test-file-a.tlo.gz" "test-file-b.tlo.gz")

  (test
    (with-out-string-stream (*stdout*)
      (load "./test-file-combined.tlo.gz"))
    "a\nb\n"))
