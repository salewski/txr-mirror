(load "../common")

(defstruct counter-iter-fast ()
  cur-val
  step
  limit
  (:method iter-item (me)
    me.cur-val)
  (:method iter-step (me)
    (inc me.cur-val me.step)
    (if (< me.cur-val me.limit) me)))

(defstruct counter-fast ()
  init
  step
  limit
  (:method iter-begin (me)
    (if (< me.init me.limit)
      (new counter-iter-fast
           cur-val me.init
           step me.step
           limit me.limit))))

(defstruct counter-iter-canon ()
  cur-val
  step
  limit
  (:method iter-item (me)
    me.cur-val)
  (:method iter-more (me)
    (< me.cur-val me.limit))
  (:method iter-step (me)
    (inc me.cur-val me.step)
    me))

(defstruct counter-canon ()
  init
  step
  limit
  (:method iter-begin (me)
    (new counter-iter-canon
         cur-val me.init
         step me.step
         limit me.limit)))

(test (list-seq (new counter-canon init 0 step 2 limit 10))
      (0 2 4 6 8))

(test (list-seq (new counter-fast init 0 step 2 limit 10))
      (0 2 4 6 8))

(test (list-seq (new counter-canon init 0 step 1 limit 0))
      nil)

(test (list-seq (new counter-fast init 0 step 1 limit 0))
      nil)

(defstruct integers ()
  item to next
  (:method length-< (me len)
    (cond
      ((<= len 1) nil)
      (me.next me.next.(length-< (pred len)))
      (t)))
  (:postinit (me)
    (if (< me.item me.to)
      (set me.next (lnew integers to me.to item (succ me.item))))))

(let ((ints (new integers item 1 to 10)))
  (mtest
    (length-< ints 11) t)
    (length-< ints 10) nil
    (length-< ints 9) nil)

;; The following reproduced a segfault when the change was made to allow del to
;; work with structs that have lambda and lambda-set.

(defstruct blah ()
  (:method lambda-set (me . args)))

(defparm o (new blah))

(set [o 1..20] 42)

(defmeth blah lambda (me . args))

(set [o 1..20] 42)
