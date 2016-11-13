(defun local-addr (family port)
  (caseql* family
    (af-inet (new sockaddr-in addr inaddr-loopback port port))
    (af-inet6 (new sockaddr-in6 addr in6addr-loopback port port))))

(defun bindfree (sock from to)
  (for ((port from))
       ((<= port to) (error "unable to bind socket"))
       ((inc port))
    (let ((addr (local-addr (sock-family sock) port)))
      (when (ignerr (sock-bind sock addr))
        (sock-listen sock)
        (return-from bindfree addr)))))
