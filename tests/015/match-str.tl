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
  (match-str "a" "" -1) 1
  (match-str "a" "a" -1) 0
  (match-str "ab" "a" -1) nil
  (match-str "ab" "a" -2) 0
  (match-str "abc" "abc" -1) 0
  (match-str "abc" "c" -1) 2
  (match-str "abc" "b" -2) 1
  (match-str "abc" "a" -3) 0
  (match-str "abc" "a" -4) nil
  (match-str "abcd" "cd" -1) 2
  (match-str "abcd" "bc" -2) 1
  (match-str "abcd" "ab" -3) 0
  (match-str "abcd" "ab" -4) nil
  (match-str "abcd" "ab" -5) nil
  (match-str (lz "abcd") "cd" -1) 2
  (match-str (lz "abcd") "bc" -2) 1
  (match-str (lz "abcd") "ab" -3) 0
  (match-str (lz "abcd") "ab" -4) nil
  (match-str (lz "abcd") "ab" -5) nil
  (match-str "abcd" (lz "cd") -1) 2
  (match-str "abcd" (lz "bc") -2) 1
  (match-str "abcd" (lz "ab") -3) 0
  (match-str "abcd" (lz "ab") -4) nil
  (match-str "abcd" (lz "ab") -5) nil
  (match-str (lz "abcd") (lz "cd") -1) 2
  (match-str (lz "abcd") (lz "bc") -2) 1
  (match-str (lz "abcd") (lz "ab") -3) 0
  (match-str (lz "abcd") (lz "ab") -4) nil
  (match-str (lz "abcd") (lz "ab") -5) nil)
