(load "../sock-common.tl")
(load "../common.tl")

(defvar socktype)

(defvar %iters% (if (eql :macos (os-symbol)) 2000 5000))

(defun client (addr)
  (with-stream (cli-sock (open-socket af-inet socktype))
    (sock-connect cli-sock addr)
    (put-string "5000" cli-sock)
    (sock-shutdown cli-sock)
    (equal (read cli-sock) (range 1 %iters%))))

(defun server (svc-sock)
  (let* ((acc-sock (sock-accept svc-sock))
         (query (read acc-sock)))
    (print (range 1 %iters%) acc-sock)
    (close-stream acc-sock)))

(defun sock-test ()
  (let* ((svc-sock (open-socket af-inet socktype))
         (svc-addr (bindfree svc-sock 1025 65535))
         (child-pid (fork)))
    (cond
      ((null child-pid) (error "fork failed"))
      ((zerop child-pid) (server svc-sock) (exit* t))
      (t (prog1 (client svc-addr) (wait child-pid))))))

(if (and (fboundp 'open-socket)
         (fboundp 'fork))
  (each ((socktype (list sock-dgram sock-stream)))
    (unless (sock-test)
      (error "test failed"))))
