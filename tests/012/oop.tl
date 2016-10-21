(load "../common")

(defstruct animal nil
  (:function whoami () "n/a")
  (:method print (self stream : pretty-p) (put-string self.[whoami] stream)))

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
