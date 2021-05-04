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
