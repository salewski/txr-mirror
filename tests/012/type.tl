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
