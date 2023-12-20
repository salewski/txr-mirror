(load "../common")

(defvarl file "getput.data")

(push-after-load (remove-path file))

(file-put-objects file '(1 2.3 (a . b) "foo"))

(test
  (file-get-lines file) ("1" "2.3" "(a . b)" "\"foo\""))

(file-append-objects file '(#(nil)))

(mtest
  (file-get-lines file) ("1" "2.3" "(a . b)" "\"foo\"" "#(nil)")
  (file-get-objects file) (1 2.3 (a . b) "foo" #(nil)))

(mtest
  (read-objects "(a . b) #\\c") ((a . b) #\c)
  (read-objects "(a") :error)

(file-put-string file "(a")

(mtest
  (file-get file) :error
  (file-get-objects file) :error)

(let ((errors (with-out-string-stream (err)
                (ignerr (file-get-objects file : err)))))
  (mtest
    (true (contains "syntax error" errors)) t
    (true (contains "unterminated" errors)) t
    (true (contains ":1" errors)) t))
