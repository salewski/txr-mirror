(load "../common")

(with-stream (s (open-socket af-inet (logior sock-dgram sock-nonblock)))
  (test (sock-listen s) t))
