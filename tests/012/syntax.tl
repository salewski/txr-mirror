(load "../common")

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
