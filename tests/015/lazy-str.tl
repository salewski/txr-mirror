(load "../common")

(defvarl words '#"the quick")

(defvarl lz0 (lazy-str words))

(test (lazy-str-get-trailing-list (copy lz0) 0) #"the quick")
(test (lazy-str-get-trailing-list (copy lz0) 1) #"he quick")
(test (lazy-str-get-trailing-list (copy lz0) 2) #"e quick")
(test (lazy-str-get-trailing-list (copy lz0) 3) ("" . #"quick"))
(test (lazy-str-get-trailing-list (copy lz0) 4) #"quick")
(test (lazy-str-get-trailing-list (copy lz0) 5) #"uick")
(test (lazy-str-get-trailing-list (copy lz0) 6) #"ick")
(test (lazy-str-get-trailing-list (copy lz0) 7) #"ck")
(test (lazy-str-get-trailing-list (copy lz0) 8) #"k")
(test (lazy-str-get-trailing-list (copy lz0) 9) (""))
(test (lazy-str-get-trailing-list (copy lz0) 10) ())
(test (lazy-str-get-trailing-list (copy lz0) 11) ())

(defvarl lz1 (lazy-str words ":"))

(test (lazy-str-get-trailing-list (copy lz1) 0) #"the quick")
(test (lazy-str-get-trailing-list (copy lz1) 1) #"he quick")
(test (lazy-str-get-trailing-list (copy lz1) 2) #"e quick")
(test (lazy-str-get-trailing-list (copy lz1) 3) ("" . #"quick"))
(test (lazy-str-get-trailing-list (copy lz1) 4) #"quick")
(test (lazy-str-get-trailing-list (copy lz1) 5) #"uick")
(test (lazy-str-get-trailing-list (copy lz1) 6) #"ick")
(test (lazy-str-get-trailing-list (copy lz1) 7) #"ck")
(test (lazy-str-get-trailing-list (copy lz1) 8) #"k")
(test (lazy-str-get-trailing-list (copy lz1) 9) (""))
(test (lazy-str-get-trailing-list (copy lz1) 10) ())
(test (lazy-str-get-trailing-list (copy lz1) 11) ())

(test [(copy lz1) 0..0] "")
(test [(copy lz1) 0..1] "t")
(test [(copy lz1) 0..2] "th")
(test [(copy lz1) 0..3] "the")
(test [(copy lz1) 0..4] "the:")
(test [(copy lz1) 0..5] "the:q")
(test [(copy lz1) 0..6] "the:qu")
(test [(copy lz1) 0..7] "the:qui")
(test [(copy lz1) 0..8] "the:quic")
(test [(copy lz1) 0..9] "the:quick")
(test [(copy lz1) 0..10] "the:quick:")
(test [(copy lz1) 0..11] "the:quick:")

(defvarl lz2 (lazy-str '#"the quick brown fox" ":" 2))

(test (lazy-str-get-trailing-list (copy lz2) 0) #"the quick brown fox")
(test (lazy-str-get-trailing-list (copy lz2) 1) #"he quick brown fox")
(test (lazy-str-get-trailing-list (copy lz2) 2) #"e quick brown fox")
(test (lazy-str-get-trailing-list (copy lz2) 3) ("" . #"quick brown fox"))
(test (lazy-str-get-trailing-list (copy lz2) 4) #"quick brown fox")
(test (lazy-str-get-trailing-list (copy lz2) 5) #"uick brown fox")
(test (lazy-str-get-trailing-list (copy lz2) 6) #"ick brown fox")
(test (lazy-str-get-trailing-list (copy lz2) 7) #"ck brown fox")
(test (lazy-str-get-trailing-list (copy lz2) 8) #"k brown fox")
(test (lazy-str-get-trailing-list (copy lz2) 9) ("" . #"brown fox"))
(test (lazy-str-get-trailing-list (copy lz2) 10) #"brown fox")
(test (lazy-str-get-trailing-list (copy lz2) 11) #"brown fox")

(test [(copy lz2) 0..0] "")
(test [(copy lz2) 0..1] "t")
(test [(copy lz2) 0..2] "th")
(test [(copy lz2) 0..3] "the")
(test [(copy lz2) 0..4] "the:")
(test [(copy lz2) 0..5] "the:q")
(test [(copy lz2) 0..6] "the:qu")
(test [(copy lz2) 0..7] "the:qui")
(test [(copy lz2) 0..8] "the:quic")
(test [(copy lz2) 0..9] "the:quick")
(test [(copy lz2) 0..10] "the:quick:")
(test [(copy lz2) 0..11] "the:quick:")

(defvarl lz3 (lazy-str words ""))

(test (lazy-str-get-trailing-list (copy lz3) 0) #"t h e quick")
(test (lazy-str-get-trailing-list (copy lz3) 1) #"h e quick")
(test (lazy-str-get-trailing-list (copy lz3) 2) #"e quick")
(test (lazy-str-get-trailing-list (copy lz3) 3) #"q u i c k")
(test (lazy-str-get-trailing-list (copy lz3) 4) #"u i c k")
(test (lazy-str-get-trailing-list (copy lz3) 5) #"i c k")
(test (lazy-str-get-trailing-list (copy lz3) 6) #"c k")
(test (lazy-str-get-trailing-list (copy lz3) 7) #"k")
(test (lazy-str-get-trailing-list (copy lz3) 8) ())
(test (lazy-str-get-trailing-list (copy lz3) 9) ())

(test [(copy lz3) 0..0] "")
(test [(copy lz3) 0..1] "t")
(test [(copy lz3) 0..2] "th")
(test [(copy lz3) 0..3] "the")
(test [(copy lz3) 0..4] "theq")
(test [(copy lz3) 0..5] "thequ")
(test [(copy lz3) 0..6] "thequi")
(test [(copy lz3) 0..7] "thequic")
(test [(copy lz3) 0..8] "thequick")
(test [(copy lz3) 0..9] "thequick")
