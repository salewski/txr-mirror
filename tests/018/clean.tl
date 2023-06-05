(trace remove-path)

(each ((*load-path* '(nil "/tmp/foo.tlo")))
  (each ((name '#"nabuchodonosor.tl nabuchodonosor.txr \
                  nabuchodonosor.tlo nabuchodonosor.tlo.gz \
                  nabuchodonosor.abc nabuchodonosor"))
    (prinl ^(let ((*load-path* , *load-path*)) (clean-file ,name)))
    (put-line "-->")
    (clean-file name)
    (put-line)))
