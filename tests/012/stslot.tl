(load "../common")

(defstruct base nil)

(defstruct derived base)

(defstruct child derived)

(defmeth child omega (me) 'child-omega)

(defmeth derived alpha (me) 'derived-alpha)
(defmeth derived beta (me) 'derived-beta)

(defmeth base gamma (me) 'base-gamma)
(defmeth base delta (me) 'base-delta)

(defmeth derived gamma (me) 'derived-gamma)
(defmeth derived delta (me) 'derived-delta)

(let ((o (new child)))
  (test (list o.(omega) o.(alpha) o.(beta) o.(gamma) o.(delta))
        (child-omega derived-alpha derived-beta derived-gamma derived-delta)))

