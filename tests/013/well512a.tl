(let ((rs (make-random-state #(#x01010101 #x11111111 #x22222222 #x33333333
                               #x44444444 #x55555555 #x66666666 #x77777777
                               #x88888888 #x99999999 #xAAAAAAAA #xBBBBBBBB
                               #xCCCCCCCC #xDDDDDDDD #xEEEEEEEE #xFFFFFFFF
                               0)))
      (mod (expt 2 32)))
  (for ((i 0)) ((< i 16)) ((inc i))
    (pprinl (rand mod rs))))
