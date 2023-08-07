(load "../common")

(defstruct refcount-close stream-wrap
  stream
  (count 1)

  (:method close (me throw-on-error-p)
    (put-line `close called, count @{me.count}`)
    (when (plusp me.count)
      (if (zerop (dec me.count))
        (close-stream me.stream throw-on-error-p)))))

(flow
  (with-stream (s (make-struct-delegate-stream
                    (new refcount-close
                         count 2
                         stream (open-file *load-path*))))
    (get-lines s))
  len
  prinl)
