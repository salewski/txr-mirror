(load "../common")

(mtest
  (if-match 1 1 'yes 'no) yes
  (if-match 1 0 'yes 'no) no)

(test (let ((sym 'a))
        (list (if-match a sym 'yes 'no)
              (if-match b sym 'yes 'no)))
      (yes no))

(mtest
  (when-match @a 42 (list a)) (42)
  (when-match (@nil) '(1) 'yes) yes
  (when-match (@a @b @c) '(1 2 3) (list c b a)) (3 2 1)
  (if-match (@a @b @c . @d) '(1 2 3 . 4) (list d c b a)) (4 3 2 1))

(test (if-match (@(oddp @a) @b @c . @d) '(2 x y z)
        (list a b c d)
        :no-match)
      :no-match)

(mtest
  (if-match (1 2 . @a) '(1 2 3 4) a) (3 4)
  (if-match ((1 2 @a) @b) '((1 2 3) 4) (list a b)) (3 4)
  (if-match #() #() :yes :no) :yes
  (if-match #() #(1) :yes :no) :no
  (if-match #((1 @a) #(3 @b)) #((1 2) #(3 4)) (list a b)) (2 4))

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

(mtest
  (when-match @(as w (@a @b @c)) '(1 2 3) (list w a b c)) ((1 2 3) 1 2 3)
  (when-match @(require (+ @a @b) (equal a b)) '(+ z z) (list a b)) (z z))

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

(test (let ((a 1) (b 2) (c 3))
        (if-match @(or @a @b @c) 2
          (list a b c)))
      (1 2 3))

(test (when-match @(or @(all @x)) '(1 2 3) x) (1 2 3))

(test (when-match (foo @(all @x)) '(bar (1 2 . 3)) x) nil)

(test (when-match (@(or foo) @(all @x)) '(bar (1 2 . 3)) x) nil)

(test (when-match (@(oddp) @(all @x)) '(2 (1 2 . 3)) x) nil)

(mtest
  (if-match @(or (@x 3 3) (1 @x 3) (1 2 @x)) '(1 2 3) x) 2
  (if-match @(<= 10 @a 13) 11 :yes :no) :yes
  (when-match @(as x @(<= 10 @a 13)) 11 x) 11
  (when-match (@(evenp) @(oddp @x)) '(2 3) x) 3
  (when-match @(<= 1 @x 10) 4 x) 4
  (when-match @(@d (chr-digit @c)) #\5 (list d c)) (5 #\5)
  (when-match @(or @(require @a (oddp a)) @b @c) 2 (list a b c)) (nil 2 nil)
  (when-match @(@x (< . @sym)) '(1 2 3) (list x sym)) (t (1 2 3))
  (when-match @(@x (< . @sym)) '(3 2 1) (list x sym)) nil
  (let ((x t))
    (when-match @(@x (< . @sym)) '(1 2 3) (list x sym))) (t (1 2 3))
  (let ((x nil))
    (when-match @(@x (< . @sym)) '(1 2 3) (list x sym))) nil
  (if-match (@(or @a) @a) '(1 2) a :no) :no
  (if-match (@(and @a) @a) '(1 2) a :no) :no)


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

(mtest
  (when-match @(hash (a)) #H(() (a b)) t) t
  (when-match @(hash (c)) #H(() (a b)) t) nil
  (let ((x 'a)) (when-match @(hash (@x)) #H(() (a b)) t)) t
  (let ((x 'd)) (when-match @(hash (@x)) #H(() (a b)) t)) nil
  (when-match @(hash (@x)) #H(() (a b)) x) (a))

(mtest
  (if-match #R(10 20) 10..20 :yes :no) :yes
  (if-match #R(10 20) #R(10 20) :yes :no) :yes
  (if-match #R(10 20) #R(1 2) :yes :no) :no
  (when-match #R(@a @b) 1..2 (list a b)) (1 2)
  (when-match #R(@a 2) 1..2 a) 1
  (when-match #R(1 @a) 1..2 a) 2
  (when-match #R(2 @a) 1..2 a) nil
  (when-match #R(@a 1) 1..2 a) nil)

(mtest
  (when-match @a..@b '1..2 (list a b)) (1 2)
  (when-match (rcons @a @b) '(rcons 1 2) (list a b)) (1 2))

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

(compile-only
  (eval-only
    (test
      [(lambda-match
         ((0 1) :zero-one)
         ((1 0) :one-zero)
         ((@x @y) :no-match)) 1 2 3]
      :error)))

(defun-match fib
  ((0) 1)
  ((1) 1)
  ((@x) (+ (fib (pred x)) (fib (ppred x)))))

(mtest
  (fib 0) 1
  (fib 1) 1
  (fib 2) 2
  (fib 3) 3
  (fib 4) 5
  (fib 5) 8)

(defun-match ack
  ((0 @n) (+ n 1))
  ((@m 0) (ack (- m 1) 1))
  ((@m @n) (ack (- m 1) (ack m (- n 1)))))

(mtest
  (ack 1 1) 3
  (ack 2 2) 7)

(defun x-x-y (list x)
  (when-match (@x @x @y) list y))

(mtest
  (x-x-y '(1 1 2) 1) 2
  (x-x-y '(1 2 3) 1) nil
  (x-x-y '(1 1 2 r2) 1) nil)

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

(test
  (when-match @(sme (1 2) (3 4) (5 . 6) m e)
              '(1 2 3 4 5 . 6)
    (list m e))
   ((3 4 5 . 6) (5 . 6)))

(test
  (when-match @(sme (1 2) (3 4) (5 . 6) m d)
              '(1 2 abc 3 4 def 5 . 6)
    (list m d))
  ((3 4 def 5 . 6) (5 . 6)))

(test
  (when-match @(sme (1 2 @x . @y) (4 @z) 6)
              '(1 2 abc 3 4 def 5 . 6)
    (list x y z))
  (abc (3 4 def 5 . 6) def))

(mtest
  (when-match @(sme (1 2) (2 3) (4)) '(1 2 3 4) t) nil
  (when-match @(sme (1 2) (3 4) (4)) '(1 2 3 4) t) nil
  (when-match @(sme (1 2) (2 3) (3 4)) '(1 2 3 4) t) nil
  (when-match @(sme (1 2 . @x) (3 . @y) (4)) '(1 2 3 4) t) t
  (when-match @(sme (1 2 . @x) (3 . @y) ()) '(1 2 3 4) t) t
  (when-match @(sme (1 2 . @x) (3 . @y) ()) '(1 2 3 4 . 5) t) nil)

(test (when-match @(sme (1 @y) (@z @x @y @z) (@x @y)) '(1 2 3 1 2 3 1 2)
        (list x y z))
      (1 2 3))

(test (when-match @(and @(sme (1 @x) (3) (7) m n)
                        @(with @(coll @(oddp @y)) (ldiff m n)))
                  '(1 2 3 4 5 6 7)
        (list x y))
      (2 (3 5)))

(test (when-match @(sme () () 5) 5 t) t)

(mtest
  (when-match @(end 3 x) 3 x) 3
  (when-match @(end (2 @x) y) '(1 2 3) (list x y)) (3 (2 3))
  (when-match @(end (2 . @x) y) '(1 2 . 3) (list x y)) (3 (2 . 3)))

(test (when-match @(as z @(end (2 @x) y)) '(1 2 3) (list x y z))
      (3 (2 3) (1 2 3)))

(defmatch env (var :env e)
  ^@(with ,var ',e))

(test (when-match @(and @a @(env e) @b) 42
        (list a (env-vbindings e) (lexical-var-p e 'a) (lexical-var-p e 'b) b))
      (42 ((a . sys:special)) t nil 42))

(defmatch var= (sym :env e)
  (if (lexical-var-p e sym)
    (with-gensyms (obj)
      ^@(require (sys:var ,obj) (= ,sym ,obj)))
    ^(sys:var ,sym)))

(test (when-match (@(var= a) @(var= a)) '(1 1.0) a) 1)

(mtest
  (when-match `` "" t) t
  (when-match `abc` "abc" t) t
  (when-match `@a` "abc" a) "abc")

(macro-time-let ((*stderr* *stdnull*))
  (mtest
    (when-match `@a@b` "abc" a) :error
    (when-match `@nil@b` "abc" a) :error
    (when-match `@nil@nil` "abc" a) :error
    (when-match `@a@nil` "abc" a) :error))

(mtest
  (when-match `@a-$` "a-$" a) "a"
  (when-match `#@a-$` "#a-$" a) "a"
  (when-match `#@a-$` "#a-$$" a) nil
  (when-match `#@a-$` "#a-" a) nil
  (when-match `#@a-@b` "#a-$" (list a b)) ("a" "$")
  (when-match `#@{a #/ab*c/}` "#abbbc" a) "abbbc"
  (when-match `#@{a #/ab*c/}d` "#abbbcd" a) "abbbc"
  (when-match `#@{a 3}@b` "#abb" a) "abb"
  (when-match `#@{a 3}@b` "#abbbc" (list a b)) ("abb" "bc")
  (when-match `#@{a 4}@b` "#abb" a) nil
  (when-match `#@{a 3}` "#abb" a) "abb"
  (when-match `#@{a 2}` "#abb" a) nil
  (when-match `#@{a 4}` "#abb" a) nil)

(let ((z 0))
  (mtest
    (when-match `@z#@a-$` "0#a-$" a) "a"
    (when-match `@z#@a-$` "0#a-$$" a) nil
    (when-match `@z#@a-$` "0#a-" a) nil
    (when-match `@z#@a-@b` "0#a-$" (list a b)) ("a" "$")
    (when-match `@z#@{a #/ab*c/}` "0#abbbc" a) "abbbc"
    (when-match `@z#@{a #/ab*c/}d` "0#abbbcd" a) "abbbc"
    (when-match `@z#@{a 3}@b` "0#abb" a) "abb"
    (when-match `@z#@{a 3}@b` "0#abbbc" (list a b)) ("abb" "bc")
    (when-match `@z#@{a 4}@b` "0#abb" a) nil
    (when-match `@z#@{a 3}` "0#abb" a) "abb"
    (when-match `@z#@{a 2}` "0#abb" a) nil
    (when-match `@z#@{a 4}` "0#abb" a) nil))

(macro-time-let ((*stderr* *stdnull*))
  (test (when-match `#@{a 4 5}` "#abb" a) :error))

(let ((b "bcd"))
  (mtest
    (when-match `@a@b` "abcd" a) "a"
    (when-match `@a@{b [1..:]}` "acd" a) "a"
    (when-match `@a@{b [1..:]}` "abcd" a) "ab"
    (when-match `@a@{b [0..1]}` "abcd" a) nil
    (when-match `@a@{b [0..2]}d` "abcd" a) "a"))

(let ((x 123) (z 0))
  (mtest
    (when-match `^@{x 5}$` "^123  $" t) t
    (when-match `^@{x -5}$` "^  123$" t) t
    (when-match `@x@x` "123123" t) t
    (when-match `@x@{x [1..:]}` "12323" t) t
    (when-match `@z^@{x 5}$` "0^123  $" t) t
    (when-match `@z^@{x -5}$` "0^  123$" t) t
    (when-match `@z@x@x` "0123123" t) t
    (when-match `@z@x@{x [1..:]}` "012323" t) t))

(let ((a "$"))
  (test (when-match `@a-@b` "$-@" b) "@"))

(test
  (build
    (each-match (`(@a) @b-@c` '("x"
                                ""
                                "(311) 555-5353"
                                "(604) 923-2323"
                                "133"
                                "4-5-6-7")
                 @x 1)
     (add (list x a b c))))
  ((3 "311" "555" "5353") (4 "604" "923" "2323")))

(test
  (append-matches ((:foo @y) '((:foo a) (:bar b) (:foo c) (:foo d))
                   (@x :bar) '((1 :bar)  (2 :bar) (3 :bar) (4 :foo)))
    (list x y))
  (1 a 3 c))

(test
  (append-matches (@x '((1) (2) (3) 4)) x)
  (1 2 3 . 4))

(test
  (keep-matches ((:foo @y) '((:foo a) (:bar b) (:foo c) (:foo d))
                 (@x :bar) '((1 :bar)  (2 :bar) (3 :bar) (4 :foo)))
    (list x y))
  ((1 a) (3 c)))

(test
  (build
    (each-match-product (`(@a) @b-@c` '("x"
                                        ""
                                        "(311) 555-5353"
                                        "(604) 923-2323"
                                        "133"
                                        "4-5-6-7")
                         @(oddp @x) '(1 2 3))
     (add (list x a b c))))
  ((1 "311" "555" "5353") (3 "311" "555" "5353")
   (1 "604" "923" "2323") (3 "604" "923" "2323")))

(test
  (append-match-products (@(oddp @x) (range 1 5)
                          @(evenp @y) (range 1 5))
    (list x y))
  (1 2 1 4 3 2 3 4 5 2 5 4))

(test
  (keep-match-products (@(oddp @x) (range 1 5)
                        @(evenp @y) (range 1 5))
    (list x y))
  ((1 2) (1 4) (3 2) (3 4) (5 2) (5 4)))

(compile-only
  (eval-only
    (compile-file (base-name *load-path*) "temp.tlo")
    (remove-path "temp.tlo")))
