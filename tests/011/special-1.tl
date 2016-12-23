(let ((x (with-out-string-stream (*stdout*)
           [format *stdout* "wo"]
           (format t "rld!"))))
  (format *stdout* "Hello, ")
  (put-line x))
