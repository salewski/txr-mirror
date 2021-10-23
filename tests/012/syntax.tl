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
