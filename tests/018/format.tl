(load "../common")

(mtest
  (fmt "~x" #b'') ""
  (fmt "~4x" #b'') "    "
  (fmt "~4,02x" #b'') "  00"
  (fmt "~x" #b'AF') "af"
  (fmt "~4x" #b'AF') "  af"
  (fmt "~-4x" #b'AF') "af  "
  (fmt "~4,03x" #b'AF') " 0af"
  (fmt "~-4,03X" #b'AF') "0AF ")

(mtest
  (fmt "~x" #\xaf) "af"
  (fmt "~4x" #\xaf) "  af"
  (fmt "~-4x" #\xaf) "af  "
  (fmt "~4,03x" #\xaf) " 0af"
  (fmt "~-4,03X" #\xaf) "0AF ")

(mtest
  (fmt "~x" #xaf) "af"
  (fmt "~4x" #xaf) "  af"
  (fmt "~-4x" #xaf) "af  "
  (fmt "~4,03x" #xaf) " 0af"
  (fmt "~-4,03X" #xaf) "0AF ")

(test
  (fmt "~x" (sha256 "abc"))
  "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")

(mtest
  (pic "") ""
  (pic "~") :error
  (pic "~z") :error
  (pic "#") :error
  (pic "# #" 1) :error
  (pic "# # #" 1 2) :error
  (pic "# # #" 1 2 3 4) :warning
  (pic "~<") "<"
  (pic "~>") ">"
  (pic "~|") "|"
  (pic "~#") "#"
  (pic "~0") "0"
  (pic "~+") "+"
  (pic "~-") "-"
  (pic "~.") "."
  (pic "~!") "!"
  (pic "~~") "~"
  (pic "x~~y") "x~y"
  (pic "~~x~~~~y~~") "~x~~y~")

(mtest
  (pic "<" "a") "a"
  (pic "<<" "a") "a "
  (pic "<<<" "a") "a  "
  (pic "~<<~>" "a") "<a>"
  (pic "~<<<~>" "a") "<a >"
  (pic "~<<<<~>" "a") "<a  >")

(mtest
  (pic ">" "a") "a"
  (pic ">>" "a") " a"
  (pic ">>>" "a") "  a"
  (pic "~>>~<" "a") ">a<"
  (pic "~>>>~<" "a") "> a<"
  (pic "~>>>>~<" "a") ">  a<")

(mtest
  (pic "|" "a") "a"
  (pic "||" "a") "a "
  (pic "|||" "a") " a "
  (pic "||||" "a") " a  "
  (pic "|||||" "a") "  a  "
  (pic "|||||" "aaa") " aaa "
  (pic "|||||" "aaaaa") "aaaaa"
  (pic "|||||" "aaaaaa") "aaaaaa"
  (pic "~||~|" "a") "|a|"
  (pic "~|||~|" "a") "|a |"
  (pic "~||||~|" "a") "| a |")

(mtest
  (pic "#" 0) "0"
  (pic "#" 0.0) "0"
  (pic "#" 0.1) "0"
  (pic "#" 0.7) "1"
  (pic "+#" 0.7) "+1"
  (pic "-#" -0.7) "-1"
  (pic "+#" -0.7) "-1"
  (pic "-#" 0.7) " 1")

(mtest
  (pic "####" 1234) "1234"
  (pic "####" 1234.1) "1234"
  (pic "#" 1) "1"
  (pic "#.#" 1) "1.0"
  (pic "######" 1234.1) "  1234"
  (pic  "######.#" 1234.1)   "  1234.1"
  (pic "#######.##" 1234.1)  "   1234.10"
  (pic "#######.##" -1234.1) "  -1234.10"
  (pic "0######.##" 1234.1)  "0001234.10"
  (pic "+######.##" 1234.1)  "  +1234.10"
  (pic "-######.##" 1234.1)  "   1234.10"
  (pic "+0#####.##" 1234.1)  "+001234.10"
  (pic "-0#####.##" 1234.1)  " 001234.10")

(mtest
  (pic "#!#" 1234)   "###"
  (pic "#!#"  123)   "###"
  (pic "#.#"  123) "123.0")

(mtest
  (pic "-##!#"   12)  " 12.0"
  (pic "+##!#"   12)  "+12.0"
  (pic "-##!#" -123)  "#####"
  (pic "+##!#"  123)  "#####")

(mtest
  (pic "X##.#Y<<<Z>>>W" 1 2 3) "X 1.0Y2  Z  3W"
  (pic "~###.#~#<<<~#>>>~#" 1 2 3) "# 1.0#2  #  3#")

(test (mapcar (do pic "foo~-0##.jpg") (rlist 0..5 8 12))
      ("foo-000.jpg" "foo-001.jpg" "foo-002.jpg" "foo-003.jpg"
       "foo-004.jpg" "foo-005.jpg" "foo-008.jpg" "foo-012.jpg"))
