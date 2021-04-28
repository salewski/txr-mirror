(load "../common")

(defun lz (str)
  (lazy-str (spl "" str) ""))

(defun lz* (str)
  (lazy-str (append (spl "" str) 42) ""))

(mtest
  (match-str "" "") 0
  (match-str "a" "a") 1
  (match-str "a" "") 0
  (match-str "a" "" 1) 1
  (match-str "a" "" 2) nil
  (match-str "abc" "c" 2) 3
  (match-str "abc" "b" 1) 2
  (match-str "abc" "a" 0) 1
  (match-str "abc" "b" 2) nil
  (match-str "abc" "a" 1) nil
  (match-str "abc" "c" 0) nil
  (match-str "abc" "c" 3) nil
  (match-str "abc" "c" 4) nil
  (match-str "abc" "abcd") nil)

(mtest
  (match-str (lz* "abc#") "c" 2) 3
  (match-str "abc#" (lz "c") 2) 3
  (match-str (lz* "abc#") (lz "c") 2) 3
  (match-str (lz "abc") (lz "c") 2) 3
  (match-str (lz "abc") (lz "b") 1) 2
  (match-str (lz "abc") (lz "a") 0) 1
  (match-str (lz "abc") (lz "b") 2) nil
  (match-str (lz "abc") (lz "a") 1) nil
  (match-str (lz "abc") (lz "c") 0) nil
  (match-str (lz "abc") (lz "c") 3) nil
  (match-str (lz "abc") (lz "c") 4) nil
  (match-str (lz "abc") (lz "abcd")) nil)

(mtest
  (match-str "" "" -1) nil
  (match-str "" "" -1) nil)
