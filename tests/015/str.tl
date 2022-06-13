(load "../common")

(mtest
  (str "x") :error
  (str 0) ""
  (str 0 5) :error
  (str 0 "abcd") ""
  (str 0 "") ""
  (str 0 #\x) ""
  (str -1) :error
  (str -1 #\x) :error
  (str -1 "") :error
  (str -1 "abc") :error)

(mtest
  (str 10) "          "
  (str 10 "") "          "
  (str 10 #\a) "aaaaaaaaaa"
  (str 10 "a") "aaaaaaaaaa"
  (str 10 "ab") "ababababab"
  (str 10 "abc") "abcabcabca"
  (str 10 "abcd") "abcdabcdab"
  (str 10 "abcde") "abcdeabcde"
  (str 10 "abcdef") "abcdefabcd"
  (str 10 "abcdefghij") "abcdefghij"
  (str 10 "abcdefghijk") "abcdefghij"
  (str 10 "abcdefghijklmnopqrst") "abcdefghij"
  (str 10 "abcdefghijklmnopqrstuvwxyz") "abcdefghij")
