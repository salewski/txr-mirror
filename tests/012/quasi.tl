(load "../common")

(test
  (mapcar (ap (op (ret `@@@1-@@@rest`))) '((a b) (c d) (e f)))
  ("a-b" "c-d" "e-f"))

(test
  (let ((a "abc") (b 123))
    `@{b 12} @{b -12} @{a [0] 3} @{a [0..2] -5}`)
  "123                   123 a      ab")

(test
  (let ((a "abc") (b 123))
    [(ret `@1-@2-@@1-@@2-@{@1 -4}-@{@2 -4}`) a b])
  "abc-123-abc-123- abc- 123")

(test
  [(ret `@1-@rest`) 1 2 3 4]
  "1-2 3 4")

(test
  [(ret `@1-@@rest`) 1 2 3 4]
  "1-2 3 4")
