(load "../common")

(mtest
  (trim-left "" "") ""
  (trim-left 1 "") :error
  (trim-left "" 1) :error
  (trim-left 1 1) :error)

(mtest
  (trim-left "" "abc") "abc"
  (trim-left "a" "abc") "bc"
  (trim-left "ab" "abc") "c"
  (trim-left "abc" "abc") ""
  (trim-left "abcd" "abc") "abc"
  (trim-left "z" "abc") "abc")

(mtest
  (trim-left #// "abc") "abc"
  (trim-left #/./ "abc") "bc"
  (trim-left #/../ "abc") "c"
  (trim-left #/.../ "abc") ""
  (trim-left #/.*/ "abc") ""
  (trim-left #/..../ "abc") "abc"
  (trim-left #/z/ "abc") "abc")

(mtest
  (trim-right "" "abc") "abc"
  (trim-right "c" "abc") "ab"
  (trim-right "bc" "abc") "a"
  (trim-right "abc" "abc") ""
  (trim-right "xabc" "abc") "abc"
  (trim-right "z" "abc") "abc")

(mtest
  (trim-right #// "abc") "abc"
  (trim-right #/./ "abc") "ab"
  (trim-right #/../ "abc") "a"
  (trim-right #/.../ "abc") ""
  (trim-right #/.*/ "abc") ""
  (trim-right #/..../ "abc") "abc"
  (trim-right #/z/ "abc") "abc")
