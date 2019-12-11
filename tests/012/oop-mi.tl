(load "../common")

(defstruct grand nil
  (gx 'gx)
  (gy 'gy)
  (:static gs0 'gs0)
  (:static gs1 'gs1))

(defstruct base0 nil)

(defstruct base1 grand
  (x 'b1x)
  (:static gs1 'gs1-b1))

(defstruct base2 grand
  (y 'b2y)
  (:static gs1 'gs1-b2))

(defstruct base3 nil
  (x 'b3x)
  (gx 'b3gx)
  (:method b3m0 (me))
  (:method b3m1 (me)))

(defstruct der0 (base0 base1 base2 base3)
  (x 'dx)
  (y 'dy)
  (z 'dz)
  (gy 'dgy)
  (:static gs0 'dgs0))

(defstruct der1 (base3 base1 base2)
  (:method b3m1 (me)))

(defvarl d0 (new der0))
(defvarl d1 (new der1))

(prinl d0)
(prinl d0.gs0)
(prinl d0.gs1)

(prinl d1)
(prinl d1.gs0)
(prinl d1.gs1)

(prinl (func-get-name d0.b3m0))
(prinl (func-get-name d1.b3m1))
