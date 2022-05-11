(load "../common")

(defvarl os (os-symbol))

(push-after-load (remove-path "tmpfile"))

(with-stream (*stdout* (open-file "tmpfile" "w"))
  (cond
    ((eq os :cygwin)
       (put-string "A\nB\nC\nD\n"))
    (t (put-line "A")
       (sh "echo B")
       (put-line "C")
       (sh "echo D"))))

(put-string (file-get-string "tmpfile"))

(with-stream (*stdout* (open-file "tmpfile" "w"))
  (put-line "E")
  (with-stream (s (open-process "cat" "w"))
    (put-line "F" s))
  (put-line "G")
  (with-stream (s (open-process "cat" "w"))
    (put-line "H" s)))

(put-string (file-get-string "tmpfile"))

(with-stream (*stdout* (open-file "tmpfile" "w"))
  (put-line "I")
  (with-stream (s (open-command "cat" "w"))
    (put-line "J" s))
  (put-line "K")
  (with-stream (s (open-command "cat" "w"))
    (put-line "L" s)))

(put-string (file-get-string "tmpfile"))
