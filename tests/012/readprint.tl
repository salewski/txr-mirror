(load "../common")

(mstest
  '[ . a] "[. a]"
  '[. a] "[. a]"
  '[ . @a] "[. @a]"
  '[. @a] "[. @a]"
  '[] "[]"
  '[. 3] "[. 3]"
  '[3 . 4] "[3 . 4]"
  '(dwim) "[]"
  '(dwim . 3) "[. 3]"
  '(dwim 3 . 4) "[3 . 4]")

(mtest
  (tostring #b'ff') "#b'ff'"
  (tostringp #b'ff') "ff")

(let ((b #b'abcdef'))
  (mtest
    `@b` "abcdef"
    `@{b [0..1]}` "ab"
    `@{b [-1..:]}` "ef"
    `@{b ":"}` "abcdef"
    `@{b [0..2] ":"}` "abcd"
    `@{b [-1]}` "239"))
