(load "../common")

(with-stream (s (open-socket af-inet (logior sock-dgram sock-nonblock)))
  (test (sock-listen s) t)
  (let* ((orig #S(sockaddr-in))
         (addr orig))
    (rotate addr (sock-peer s))
    (vtest (sock-peer s) orig)))
