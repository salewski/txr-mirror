(load "../common")

(define-struct-prelude init-fini-log (fox bear)
  (:init (me) (put-line `@me created`))
  (:fini (me) (put-line `@me finalized`)))

(defstruct fox ())

(defstruct bear ())

(with-objects ((f (new fox))
               (b (new bear)))
  (put-line "inside with-object"))
