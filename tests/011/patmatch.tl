(load "../common")

(test (if-match 1 1 'yes 'no) yes)
(test (if-match 1 0 'yes 'no) no)

(test (let ((sym 'a))
        (list (if-match a sym 'yes 'no)
              (if-match b sym 'yes 'no)))
      (yes no))

(test (when-match @a 42 (list a)) (42))
(test (when-match (@nil) '(1) 'yes) yes)
(test (when-match (@a @b @c) '(1 2 3) (list c b a)) (3 2 1))
(test (if-match (@a @b @c . @d) '(1 2 3 . 4) (list d c b a)) (4 3 2 1))

(test (if-match (@(oddp @a) @b @c . @d) '(2 x y z)
        (list a b c d)
        :no-match)
      :no-match)

(test (if-match (1 2 . @a) '(1 2 3 4) a) (3 4))
(test (if-match ((1 2 @a) @b) '((1 2 3) 4) (list a b)) (3 4))
(test (if-match #() #() :yes :no) :yes)
(test (if-match #() #(1) :yes :no) :no)
(test (if-match #((1 @a) #(3 @b)) #((1 2) #(3 4)) (list a b)) (2 4))

(test (when-match @(struct time year 2021 month @m) #S(time year 2021 month 1)
        m)
      1)

(defstruct widget ()
  name
  value)

(defstruct grommet ()
  name
  value)

(vtest (append-each ((obj (list (new grommet name "foo" value :grom)
                                (new widget name "foo" value :widg))))
         (when-match @(struct @type name "foo" value @v) obj
           (list (list type v))))
       ^((,(find-struct-type 'grommet) :grom)
         (,(find-struct-type 'widget) :widg)))

(test (when-match @(as w (@a @b @c)) '(1 2 3) (list w a b c)) ((1 2 3) 1 2 3))
(test (when-match @(require (+ @a @b) (equal a b)) '(+ z z) (list a b)) (z z))

(test (if-match @(require (+ @a @b) (equal a b)) '(+ y z)
        (list a b)
        :no-match)
      :no-match)

(test (when-match @(all (x @a @b)) '((x 1 a) (x 2 b) (x 3 c))
        (list a b))
      ((1 2 3) (a b c)))

(test (when-match (@x @(all @x)) '(1 (1 1 1 1)) x) 1)

(test (when-match (@x @(all @x)) '(1 (1 1 1 2)) x) nil)

(test (when-match @(some (x @a @b)) '((y 1 a) (x 2 b) (z 3 c))
        (list a b))
      (2 b))

(test (when-match @(coll (x @a @b)) '((y 1 a) (x 2 b) (z 3 c) (x 4 d))
            (list a b))
      ((2 4) (b d)))

(test (if-match @(and (@x 2 3) (1 @y 3) (1 2 @z)) '(1 2 3)
        (list x y z))
      (1 2 3))

(test (if-match @(and (@x 1) (1 @x)) '(1 1) x) 1)
(test (if-match @(and (@x 1) (1 @x)) '(1 2) x) nil)

(test (when-match @(all @(or (@x @y) @z)) '((1 2) (3 4)) (list x y z))
      ((1 3) (2 4) (nil nil)))

(test (when-match @(or @(all @x)) '(1 2 3) x) (1 2 3))

(test (when-match (foo @(all @x)) '(bar (1 2 . 3)) x) nil)

(test (when-match (@(or foo) @(all @x)) '(bar (1 2 . 3)) x) nil)

(test (when-match (@(oddp) @(all @x)) '(2 (1 2 . 3)) x) nil)

(test (if-match @(or (@x 3 3) (1 @x 3) (1 2 @x)) '(1 2 3) x) 2)
(test (if-match @(<= 10 @a 13) 11 :yes :no) :yes)
(test (when-match @(as x @(<= 10 @a 13)) 11 x) 11)
(test (when-match (@(evenp) @(oddp @x)) '(2 3) x) 3)
(test (when-match @(<= 1 @x 10) 4 x) 4)
(test (when-match @(@d (chr-digit @c)) #\5 (list d c)) (5 #\5))
(test (when-match @(or @(require @a (oddp a)) @b @c) 2 (list a b c))
      (nil 2 nil))
(test (when-match @(@x (< . @sym)) '(1 2 3) (list x sym))
      (t (1 2 3)))
(test (when-match @(@x (< . @sym)) '(3 2 1) (list x sym))
      nil)
(test (let ((x t))
        (when-match @(@x (< . @sym)) '(1 2 3) (list x sym)))
      (t (1 2 3)))
(test (let ((x nil))
        (when-match @(@x (< . @sym)) '(1 2 3) (list x sym)))
      nil)

(test (if-match (@(or @a) @a) '(1 2) a :no) :no)
(test (if-match (@(and @a) @a) '(1 2) a :no) :no)


(test
  (collect-each ((obj (list '(1 2 3)
                            '(4 5)
                            '(3 5)
                            #S(time year 2021 month 1 day 1)
                            #(vec tor))))
    (match-case obj
      (@(struct time year @y) y)
      (#(@x @y) (list x y))
      ((@nil @nil @x) x)
      ((4 @x) x)
      ((@x 5) x)))
  (3 5 3 2021 (vec tor)))

(test (when-match (@(and @a @b) (x . @c)) '(1 (x 2 3 4)) c) (2 3 4))

(test (when-match (@(some @a) . @b) '((1 2 3) 2) (list a b)) (1 (2)))

(set *print-circle* t)

(test (when-match @(as a @(some @a)) '#1=(1 2 #1# 3) :yes) :yes)

(test (when-match (@a @(as a @(some @a))) '(#1=(1 2 #1# 3) #1#) :yes) :yes)

(test (when-match (@a @(as a @(or x @a))) '(#1=(1 2 #1# 3) #1#) :yes) :yes)

(test (when-match (@(with @a x 42) @b @c) '(1 2 3) (list a b c x))
      (1 2 3 42))

(test (let ((o 3))
        (when-match (@(evenp @x) @(with @z @(oddp @y) o)) '(4 6)
          (list x y z)))
      (4 3 6))

(test (let ((o 3))
        (when-match (@(evenp @x) @(with @(oddp @y) o)) '(4 6)
          (list x y)))
      (4 3))

(defstruct node ()
  left right)

(mlet ((n (lnew node left (new node left n))))
  (test (when-match @(as x @(struct node
                                     left @(struct node left @x)))
                    n :yes)
        :yes))

(test
  (collect-each ((obj (list '(1 2 3)
                            '(4 5)
                            '(3 5)
                            '(6 2 6)
                            #(11 12)
                            #S(time year 2021 month 1 day 2)
                            #S(time year 2020 month 1 day 1)
                            #(vec tor))))
    (match-case obj
      (@(struct @s year 2021 day @d) (list d (struct-type-name s)))
      (@(struct time year @y month @x day @x) (list y x))
      (#(@(integerp @x) @(require @y (succ x))) (list x y))
      (#(@x @y) (list x y))
      ((@x @nil @x) x)
      ((@nil @nil @x) x)
      ((4 @x) x)
      ((@x 5) x)))
  (3 5 3 6 (11 12) (2 time) (2020 1) (vec tor)))

(test (when-match @(hash (x @y) (@y @datum)) #H(() (x k) (k 42)) datum)
      42)

(test (when-match @(hash (x @y) (@(symbolp @y) @datum)) #H(() (x k) (k 42)) datum)
      (42))

(test (if-match #R(10 20) 10..20 :yes :no) :yes)
(test (if-match #R(10 20) #R(10 20) :yes :no) :yes)
(test (if-match #R(10 20) #R(1 2) :yes :no) :no)
(test (when-match #R(@a @b) 1..2 (list a b)) (1 2))
(test (when-match #R(@a 2) 1..2 a) 1)
(test (when-match #R(1 @a) 1..2 a) 2)
(test (when-match #R(2 @a) 1..2 a) nil)
(test (when-match #R(@a 1) 1..2 a) nil)

(test (when-match @a..@b '1..2 (list a b)) (1 2))
(test (when-match (rcons @a @b) '(rcons 1 2) (list a b)) (1 2))

(test (let ((h #H(() (a 1) (b 2))))
        (when-match @[h @x] 'a x))
      a)

(test (let ((h #H(() (a 1) (b 2))))
        (when-match @(@y [h @x]) 'a (list x y)))
      (a 1))

(test
  (let ((f (lambda-match
             (() (list 0 :args))
             ((@a) (list 1 :arg a))
             ((@a @b) (list 2 :args a b))
             ((@a @b . @c) (list* '> 2 :args a b c)))))
    (list [f] [f 1] [f 1 2] [f 1 2 3]))
  ((0 :args) (1 :arg 1) (2 :args 1 2) (> 2 :args 1 2 3)))

(test
  [(lambda-match
     ((0 1) :zero-one)
     ((1 0) :one-zero)
     ((@x @y) :no-match)) 1 0]
  :one-zero)

(test
  [(lambda-match
     ((0 1) :zero-one)
     ((1 0) :one-zero)
     ((@x @y) :no-match)) 1 1]
  :no-match)

(test
  [(lambda-match
     ((0 1) :zero-one)
     ((1 0) :one-zero)
     ((@x @y) :no-match)) 1 2 3]
  :error)

(defun-match fib
  ((0) 1)
  ((1) 1)
  ((@x) (+ (fib (pred x)) (fib (ppred x)))))

(test (fib 0) 1)
(test (fib 1) 1)
(test (fib 2) 2)
(test (fib 3) 3)
(test (fib 4) 5)
(test (fib 5) 8)

(defun-match ack
  ((0 @n) (+ n 1))
  ((@m 0) (ack (- m 1) 1))
  ((@m @n) (ack (- m 1) (ack m (- n 1)))))

(test (ack 1 1) 3)
(test (ack 2 2) 7)

(defun x-x-y (list x)
  (when-match (@x @x @y) list y))

(test (x-x-y '(1 1 2) 1) 2)
(test (x-x-y '(1 2 3) 1) nil)
(test (x-x-y '(1 1 2 r2) 1) nil)

(test (let ((a 3) (x 0))
        (match-case '(3 2 1)
          ((@x 2 @b) ^(1 ,b))
          ((@a 2 @b) ^(2 ,a))))
      (2 3))

(test
  (let ((a 3) (x 0))
    (labels ((local (:match)
               ((@x 2 @b) ^(1 ,b))
               ((@a 2 @b) ^(2 ,a))))
      (local 3 2 1)))
  (2 3))
