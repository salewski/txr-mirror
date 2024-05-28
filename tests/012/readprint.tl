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
    `@{b ":"}` "ab:cd:ef"
    `@{b ""}` "abcdef"
    `@{b [0..2] ":"}` "ab:cd"
    `@{b [0..1] ":"}` "ab"
    `@{b [0..0] ":"}` ""
    `@{b [-1]}` "239"))

(let ((b "abcdef"))
  (mtest
    `@b` "abcdef"
    `@{b [0..1]}` "a"
    `@{b [-1..:]}` "f"
    `@{b ":"}` "a:b:c:d:e:f"
    `@{b ""}` "abcdef"
    `@{b [0..2] ":"}` "a:b"
    `@{b [0..1] ":"}` "a"
    `@{b [0..0] ":"}` ""
    `@{b [-1]}` "f"))
