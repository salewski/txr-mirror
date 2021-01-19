(load "../common")

(test (if-match 1 1 'yes 'no) yes)
(test (if-match 1 0 'yes 'no) no)

(test (let ((sym 'a))
        (list (if-match a sym 'yes 'no)
              (if-match b sym 'yes 'no)))
      (yes no))

(test (when-match @a 42 (list a)) (42))
(test (when-match (@a @b @c) '(1 2 3) (list c b a)) (3 2 1))
(test (if-match (@a @b @c . @d) '(1 2 3 . 4) (list d c b a)) (4 3 2 1))

(test (if-match (@(oddp a) @b @c . @d) '(2 x y z)
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

(test (when-match @(let w (@a @b @c)) '(1 2 3) (list w a b c)) ((1 2 3) 1 2 3))
(test (when-match @(require (+ @a @b) (equal a b)) '(+ z z) (list a b)) (z z))

(test (if-match @(require (+ @a @b) (equal a b)) '(+ y z)
        (list a b)
        :no-match)
      :no-match)

(test (when-match @(all (x @a @b)) '((x 1 a) (x 2 b) (x 3 c))
        (list a b))
      ((1 2 3) (a b c)))

(test (when-match @(some (x @a @b)) '((y 1 a) (x 2 b) (z 3 c))
        (list a b))
      (2 b))

(test (if-match @(and (@x 2 3) (1 @y 3) (1 2 @z)) '(1 2 3)
        (list x y z))
      (1 2 3))

(test (if-match @(or (@x 3 3) (1 @x 3) (1 2 @x)) '(1 2 3) x) 2)
(test (if-match @(op <= 10 @1 13) 11 :yes :no) :yes)
(test (when-match @(let x @(op <= 10 @1 13)) 11 x) 11)
(test (when-match (@(evenp) @(oddp x)) '(2 3) x) 3)

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
