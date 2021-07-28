(load "../common")

(mtest
  (subtypep 'a 'a) t
  (subtypep t t) t
  (subtypep nil t) t
  (subtypep t nil) nil
  (subtypep nil nil) t
  (subtypep 'null nil) nil
  (subtypep nil 'null) t
  (subtypep 'null t) t
  (subtypep 'null 'a) nil
  (subtypep 'a 'null) nil
  (subtypep nil 'a) t
  (subtypep 'a nil) nil
  (subtypep 'a t) t)

(mtest
  (subtypep 'stream 'stdio-stream) nil
  (subtypep 'stdio-stream 'stream) t)

(defstruct xtime time)
(defstruct nottime nil)

(mtest
  (typep #S(time) 'time) t
  (typep #S(time) (find-struct-type 'time)) t
  (typep #S(xtime) 'time) t
  (typep #S(xtime) (find-struct-type 'time)) t
  (typep #S(nottime) 'time) nil
  (typep #S(nottime) (find-struct-type 'time)) nil)

(mtest
  (subtypep (find-struct-type 'time) (find-struct-type 'time)) t
  (subtypep (find-struct-type 'time) 'time) t
  (subtypep 'time (find-struct-type 'time)) t)

(mtest
  (subtypep (find-struct-type 'xtime) (find-struct-type 'time)) t
  (subtypep (find-struct-type 'xtime) 'time) t
  (subtypep 'xtime (find-struct-type 'time)) t)

(mtest
  (subtypep (find-struct-type 'time) (find-struct-type 'xtime)) nil
  (subtypep (find-struct-type 'time) 'xtime) nil
  (subtypep 'time (find-struct-type 'xtime)) nil)

(mtest
  (subtypep 'time 'struct) t
  (subtypep (find-struct-type 'time) 'struct) t
  (subtypep 'hash 'struct) nil)

(defstruct listlike nil
  (:method car (me)))

(defstruct veclike nil
  (:method length (me)))

(mtest
  (subtypep 'listlike 'sequence) t
  (subtypep (find-struct-type 'listlike) 'sequence) t
  (subtypep 'veclike 'sequence) t
  (subtypep (find-struct-type 'veclike) 'sequence) t
  (subtypep 'time 'sequence) nil
  (subtypep 'hash 'sequence) nil
  (subtypep 'str 'sequence) t
  (subtypep 'string 'sequence) t
  (subtypep 'vec 'sequence) t)
