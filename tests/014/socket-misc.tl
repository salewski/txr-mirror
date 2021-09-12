(load "../sock-common")
(load "../common")

(defmacro set-and-get (:env env place val)
  (with-update-expander (getter setter) place env
    ^(progn (,setter ,val) (,getter))))

(with-stream (s (open-socket af-inet (logior sock-dgram sock-nonblock)))
  (test (sock-listen s) t)
  (let* ((orig #S(sockaddr-in))
         (addr orig))
    (rotate addr (sock-peer s))
    (vtest (sock-peer s) orig)))

(with-stream (s (open-socket af-inet sock-stream))
  (test (set-and-get (sock-opt s sol-socket so-reuseaddr) 1) 1)
  (test (set-and-get (sock-opt s sol-socket so-reuseaddr (ffi int)) 0) 0)
  (whenlet ((addr (bindfree s 1025 65535)))
    ;; sock-bind enables so-reuseaddr.
    (test (sock-opt s sol-socket so-reuseaddr) 1)))
