(dotimes (i 20000)
  (close-lazy-streams
    (file-get-lines self-path)))
