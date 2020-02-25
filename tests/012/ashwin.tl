(load "../common")

(defvarl a-list '((a . 42) (b . 73)))
(defvarl key 'a)

(test (cdr (assq key a-list)) 42)
(test (cdr (assq 'b a-list)) 73)
(test (cdr (assq 'c a-list)) nil)
