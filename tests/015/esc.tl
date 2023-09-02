(load "../common")

(mtest
  (str-esc "$*." "~" "") ""
  (str-esc "$*." "~" "a") "a"
  (str-esc "$*." "~" "~") "~"
  (str-esc "$*." "~" "*") "~*"
  (str-esc "$*." "~" ".") "~.")

(mtest
  (str-esc "$*." "~" "aa") "aa"
  (str-esc "$*." "~" "a~") "a~"
  (str-esc "$*." "~" "a$") "a~$"
  (str-esc "$*." "~" "a*") "a~*"
  (str-esc "$*." "~" "a.") "a~.")

(mtest
  (str-esc "$*." "~" "~a") "~a"
  (str-esc "$*." "~" "$a") "~$a"
  (str-esc "$*." "~" "*a") "~*a"
  (str-esc "$*." "~" ".a") "~.a")

(mtest
  (str-esc "$*." "~" "a~b") "a~b"
  (str-esc "$*." "~" "a$b") "a~$b"
  (str-esc "$*." "~" "a*b") "a~*b"
  (str-esc "$*." "~" "a.b") "a~.b")

(mtest
  (str-esc "$*." "~" "~a~") "~a~"
  (str-esc "$*." "~" "$a$") "~$a~$"
  (str-esc "$*." "~" "*a*") "~*a~*"
  (str-esc "$*." "~" ".a.") "~.a~.")

(test
  (str-esc "$*." "~" "$*.a$*.b") "~$~*~.a~$~*~.b")

(test
  (str-esc "<>" "<" "(<<>>)") "(<<<<<><>)")
