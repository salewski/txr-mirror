(load "../common")

(mtest
  (rel-path "/abc" "abc") :error
  (rel-path "abc" "/abc") :error
  (rel-path "." ".") "."
  (rel-path "./abc" "abc") "."
  (rel-path "abc" "./abc") "."
  (rel-path "./abc" "./abc") "."
  (rel-path "abc" "abc") "."
  (rel-path "." "abc") "abc"
  (rel-path "abc/def" "abc/ghi") "../ghi"
  (rel-path "xyz/../abc/def" "abc/ghi") "../ghi"
  (rel-path "abc" "d/e/f/g/h") "../d/e/f/g/h"
  (rel-path "abc" "d/e/../g/h") "../d/g/h"
  (rel-path "d/e/../g/h" ".") "../../.."
  (rel-path "d/e/../g/h" "a/b") "../../../a/b"
  (rel-path "x" "../../../y") "../../../../y"
  (rel-path "x///" "x") "."
  (rel-path "x" "x///") "."
  (rel-path "///x" "/x") "."
  (rel-path "../../x" "y") :error
  (rel-path "" "") "."
  (rel-path "a" "") ".."
  (rel-path "" "a") "a")
