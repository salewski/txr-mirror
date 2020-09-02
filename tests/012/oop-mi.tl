(load "../common")

(defstruct grand nil
  (li 'nil)
  (gx 'gx)
  (gy 'gy)
  (:init (me) (push 'g me.li))
  (:fini (me) (push 'gf me.li))
  (:static gs0 'gs0)
  (:static gs1 'gs1)
  (:method gm (me) 'gm))

(defstruct base0 nil
  (:method m (me) 'm0))

(defstruct base1 grand
  (x 'b1x)
  (:init (me) (push 'b1 me.li))
  (:fini (me) (push 'b1f me.li))
  (:static gs1 'gs1-b1)
  (:method m (me) 'm1))

(defstruct base2 grand
  (y 'b2y)
  (:init (me) (push 'b2 me.li))
  (:fini (me) (push 'b2f me.li))
  (:static gs1 'gs1-b2)
  (:method m (me) 'm2))

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

(defstruct der2 (base3 base1 base2))
(defstruct der3 (base3 base2 base1))

(prinl (call-super-method (new der2) 'm))
(prinl (call-super-method (new der3) 'm))
(prinl (call-super-method (new der3) 'gm))
(prinl (call-super-fun 'der2 'm nil))
(prinl (call-super-fun 'der3 'm nil))
(prinl (call-super-fun 'der3 'gm nil))

(prinl (static-slot-home 'der2 'm))
(prinl (static-slot-home 'der3 'm))
(prinl (static-slot-home 'der3 'gm))

(each ((ty '(der0 der1 der2 der3)))
  (let ((ob (new* ty)))
    (prinl ob.li)
    (call-finalizers ob)
    (prinl ob.li)))
