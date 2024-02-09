(load "../common")

(mtest
  (tree-find "abc" "abc") t
  (tree-find "abc" "abc" (fun eq)) nil
  (tree-find "b" '("a" "b" "c")) t
  (tree-find "b" '("a" "b" "c") (fun eq)) nil
  (tree-find "b" '(("b") "a" "c")) t
  (tree-find "b" '("a" ("b") "c")) t
  (tree-find "b" '("a" (("b")) "c")) t
  (tree-find "d" '("a" (("b")) "c")) nil
  (tree-find nil '("a" (("b")) "c")) nil)

(mtest
  (cons-find "abc" "abc") t
  (cons-find "abc" "ABC" (fun eq)) nil
  (cons-find "b" '("a" "b" "c")) t
  (cons-find "b" '("a" "b" "c") (fun eq)) nil
  (cons-find "b" '(("b") "a" "c")) t
  (cons-find "b" '("a" ("b") "c")) t
  (cons-find "b" '("a" (("b")) "c")) t
  (cons-find "d" '("a" (("b")) "c")) nil
  (cons-find "d" '("a" (("b")) "c" . "d")) t
  (cons-find "d" '("a" (("b") . "d") "c")) t
  (cons-find "d" '("a" . "d")) t
  (cons-find nil '("a" (("b")) "c")) t)
