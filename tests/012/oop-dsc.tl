(load "../common")

(define-struct-clause :nothing (. ignored-args))

(defstruct s0 ()
  (:nothing 1 "foo" :junk)
  x)

(test (new s0) #S(s0 x nil))

(define-struct-clause :multi (init-val . names)
  (mapcar (lop list init-val) names))

(defstruct s1 ()
  (:multi 0 a b c))

(test (new s1) #S(s1 a 0 b 0 c 0))

(define-struct-clause :getset (slot getter setter : init-val)
  ^((,slot ,init-val)
    (:method ,getter (obj) obj.,slot)
    (:method ,setter (obj new) (set obj.,slot new))))

(defstruct s2 ()
  (:getset a get-a set-a 0)
  (:getset b get-b set-b 0))

(let ((s2 (new s2)))
  (mtest
    s2.a 0
    s2.b 0
    s2.(get-a) 0
    s2.(get-b) 0
    s2.(set-a 42) 42
    s2.(set-b 73) 73
    s2.a 42
    s2.b 73
    s2.(get-a) 42
    s2.(get-b) 73))

(define-struct-clause :hash (hash-name by-slot)
  ^((:static ,hash-name (hash))
    (:postinit (me)
      (set [me.,hash-name me.,by-slot] me))
    (:postfini (me)
      (del [me.,hash-name me.,by-slot]))))

(defstruct s3 ()
  a b
  (:hash a-hash a)
  (:hash b-hash b))

(let* ((s3-list (list (new s3 a "one" b 1)
                      (new s3 a "two" b 2)
                      (new s3 a "three" b 3)))
       (s3 (first s3-list)))
  (mtest
    [s3.a-hash "one"].a "one"
    [s3.a-hash "two"].b 2
    [s3.a-hash "three"].b 3
    [s3.b-hash 1].a "one"
    [s3.b-hash 2].b 2
    [s3.b-hash 3].a "three")
  (call-finalizers s3)
  (test [s3.a-hash "one"] nil))

(define-struct-clause :s3 ()
  '((:inherit s3)
    (:inherit passwd group)))

(defstruct s4 (time)
  (:s3))

(let ((s4 (new s4 a "x" b 999)))
  (mtest
    [s4.a-hash "two"].a "two"
    [s4.a-hash "x"].a "x"
    [s4.b-hash 999].a "x"
    s4.uid nil
    s4.gid nil))
