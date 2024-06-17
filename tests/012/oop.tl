(load "../common")

(defstruct animal nil
  (:function whoami () "n/a")
  (:method print (self stream : pretty-p) (put-string [self.whoami] stream)))

(defstruct dog animal
  (:function whoami () "dog"))

(defstruct collie dog
  (:function whoami () "collie"))

(defstruct poodle dog)

(defvarl a (new animal))
(defvarl d (new dog))
(defvarl c (new collie))

(defun print-all ()
  (pprinl a)
  (pprinl d)
  (pprinl c))

(print-all)

(defmeth animal whoami ()
  "animal")

(print-all)

(defmeth dog whoami ()
  "canine")

(print-all)

(defmeth poodle whoami ()
  "poodle")

(print-all)

(pprinl (new poodle))

(mapcar (umeth print *stdout*) (list (new collie) (new dog)))
(put-line)

(let* ((ssl (gun (make-string-output-stream)))
       (s1 (pop ssl))
       (s2 (pop ssl))
       (s3 (pop ssl))
       (d (new collie)))
  [(meth d print s1)]
  [(meth d print s2)]
  [(meth d print s3)]
  (tprint [mapcar get-string-from-stream (list s1 s2 s3)]))

(defstruct b nil
  (:instance a 1)
  (:instance b 2)
  (:instance c 3)
  (:static sa 10)
  (:static sb 20)
  (:static sc 30))

(defstruct d b
  (a)
  (b -2)
  (:static sa)
  (:static sb -20)
  (:static y 0))

(static-slot-ensure 'b 'x 42)
(static-slot-ensure 'b 'y 42)

(let ((b (new b sc 300))
      (d (new d)))
  (prinl b)
  (prinl d)
  (prinl (list b.sa b.sb b.sc b.x b.y))
  (prinl (list d.sa d.sb d.sc d.x d.y)))

(defstruct (ab a : b) () a b)

(defvar foo)

(mtest
  (new* (foo 'ab) a 1) :error
  (new* ((find-struct-type 'ab)) a 1) #S(ab a 1 b nil)
  (new* [find-struct-type 'ab] a 1) #S(ab a 1 b nil)
  (new* ([find-struct-type 'ab] 1 2)) #S(ab a 1 b 2)
  (new* ((find-struct-type 'ab) 1 2)) #S(ab a 1 b 2)
  (new* ([find-struct-type 'ab] 1) b 2) #S(ab a 1 b 2)
  (let ((type (find-struct-type 'ab)))
    (new* type a 3 b 4)) #S(ab a 3 b 4)
  (let ((type (find-struct-type 'ab)))
    (new* (type 3 4))) #S(ab a 3 b 4))

(defstruct worker ()
  name
  (:method work (me) `worker @{me.name} works`)
  (:method relax (me : (min 15)) `worker @{me.name} relaxes for @min min`))

(defstruct contractor ()
  sub
  (:delegate work (me) me.sub.sub)
  (:delegate break (me : min) me.sub.sub relax)
  (:delegate break20 (me : (min 20)) me.sub.sub relax))

(let ((co (new contractor sub (new contractor sub (new worker name "foo")))))
  (mtest co.(work) "worker foo works"
         co.(break) "worker foo relaxes for 15 min"
         co.(break 5) "worker foo relaxes for 5 min"
         co.(break20 5) "worker foo relaxes for 5 min"
         co.(break20) "worker foo relaxes for 20 min"))

(test
  (defstruct bad-delegate ()
    (:delegate del (x : (y z w))))
  :error)

(defstruct api-x ()
  (:method get (x a b : c . d) ^(api-x get ,x ,a ,b ,c ,d))
  (:method put (x s) ^(api-x put ,x ,s)))

(defstruct api-y ()
  (:method frob (y r : s) ^(api-y frob ,y ,r ,s))
  (:method tweak (y) ^(api-y tweak ,y)))

(defstruct api-z ()
  (:method decrement (z n) ^(api-z decrement ,z ,n))
  (:method increment (z n) ^(api-z increment ,z ,n)))

(defstruct component ()
  (ax (new api-x))
  (ay (new api-y))
  (az (new api-z))
  (:mass-delegate o o.ax api-x *)
  (:mass-delegate o o.ay api-y frob)
  (:mass-delegate o o.az api-z * decrement))

(let ((c (new component)))
  (mtest
    c.(get 1 2 3 . 4) (api-x get #S(api-x) 1 2 3 4)
    c.(put 5) (api-x put #S(api-x) 5)
    c.(get) :error
    c.(put 5 6) :error
    c.(frob 7 8) (api-y frob #S(api-y) 7 8)
    c.(frob 9) (api-y frob #S(api-y) 9 nil)
    c.(frob 7 8 9) :error
    c.(tweak) :error
    c.(increment 1) (api-z increment #S(api-z) 1)
    c.(decrement) :error))

(defstruct node ()
  left right)

(test (copy (new node left 1 right 2)) #S(node left 1 right 2))

(defmeth node copy (me)
  (new node left (succ me.left) right (succ me.right)))

(test (copy (new node left 1 right 2)) #S(node left 2 right 3))
