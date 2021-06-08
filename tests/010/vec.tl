(load "../common")

(let ((v0 (vec))
      (v3 (vec 1 2 3)))
  (mtest
    v0 #()
    v3 #(1 2 3)
    (fill-vec v0 nil) #()
    (fill-vec v0 nil -1 -1) :error
    (fill-vec v0 nil 1 1) :error
    (fill-vec v3 nil 0 0) #(1 2 3)
    (fill-vec v3 nil 1 1) #(1 2 3)
    (fill-vec v3 nil 2 2) #(1 2 3)
    (fill-vec v3 nil 3 3) #(1 2 3)
    (fill-vec v3 nil -3 -3) #(1 2 3)
    (fill-vec v3 nil 4 0) :error
    (fill-vec v3 nil 4 4) :error
    (fill-vec v3 nil 0 4) :error
    (fill-vec v3 nil -1 0) #(1 2 3)
    (fill-vec v3 nil 1 0) #(1 2 3)
    (fill-vec v3 nil 2 1) #(1 2 3)
    (fill-vec v3 nil 3 2) #(1 2 3)
    (fill-vec v3 nil -4 -3) :error
    (fill-vec v3 nil -3 -4) :error
    (fill-vec v3 nil) #(nil nil nil)
    (fill-vec v3 1 0 1) #(1 nil nil)
    (fill-vec v3 2 1 2) #(1 2 nil)
    (fill-vec v3 3 2 3) #(1 2 3)
    (fill-vec v3 0 1) #(1 0 0)
    (fill-vec v3 2 -1) #(1 0 2)
    (fill-vec v3 3 -3) #(3 3 3))
    (fill-vec v3 0 -2 -1) #(3 0 3))
