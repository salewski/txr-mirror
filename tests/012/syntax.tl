(load "../common")

"top level literal"

".."

"."

#;(commented out list)
#;3.14
#;abc
#;.foo
#; .foo
#;a.b

'(#;.foo)
'(#; .foo)

(test
  #;(commented out list)
  #;3.14
  #;abc
  #;.foo
  #; .foo
  #;a.b
  42 42)

(mtest
   '(#;abc) nil
   '(#; abc 1) (1)
   '(0 #; abc 1) (0 1)
   '(0 #; abc) (0))

(mtest
   '(#; .abc) nil
   '(#; .abc 1) (1)
   '(0 #; .abc 1) (0 1)
   '(0 #; .abc) (0))

(mtest
  '(-,1) (- (sys:unquote 1))
  1,2 12
  1,,2 12
  1,,,2 12
  1,2,3 1,2,3
  -0,1 -1
  '(1,a) (1 (sys:unquote a)))

(mtest
  (read "#x,ff") :error
  (read "#o,1") :error
  (read "#b,1") :error
  '(#xff,ff,z) (65535 (sys:unquote z))
  '(#xff,ff,a) (1048570))

(mtest
  #xff,ff 65535
  #o7,7,7 511
  #b1101,1110 #xDE)

(mtest
  1,234,567.890,123E13 1234567.890123E13
  '(1.234,e+12) (1.234 (sys:unquote e+12))
  '(1.,234) (1.0 (sys:unquote 234)))

