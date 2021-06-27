(load "../common")

(defvarl tr0 (make-trie))
(defvarl tr1 (make-trie))

(defvarl dat ; '#"aaa aab aac aba abb abc caa cab cac")
 '("2" "3" "5" "7" "11" "13" "17" "19" "23" "29" "31" "37" "41"
   "43" "47" "53" "59" "61" "67" "71" "73" "79" "83" "89" "97" "101"
   "103" "107" "109" "113" "127" "131" "137" "139" "149" "151" "157"
   "163" "167" "173" "179" "181" "191" "193" "197" "199" "211" "223"
   "227" "229" "233" "239" "241" "251" "257" "263" "269" "271" "277"
   "281" "283" "293" "307" "311" "313" "317" "331" "337" "347" "349"
   "353" "359" "367" "373" "379" "383" "389" "397" "401" "409" "419"
   "421" "431" "433" "439" "443" "449" "457" "461" "463" "467" "479"
   "487" "491" "499" "503" "509" "521" "523" "541" "547" "557" "563"
   "569" "571" "577" "587" "593" "599" "601" "607" "613" "617" "619"
   "631" "641" "643" "647" "653" "659" "661" "673" "677" "683" "691"
   "701" "709" "719" "727" "733" "739" "743" "751" "757" "761" "769"
   "773" "787" "797" "809" "811" "821" "823" "827" "829" "839" "853"
   "857" "859" "863" "877" "881" "883" "887" "907" "911" "919" "929"
   "937" "941" "947" "953" "967" "971" "977" "983" "991" "997"))

(defvarl enu [mapcar tostring (range* 0 (len dat))])
(defvarl ndt [mapcar (op mapcar (op + 64)) dat])
(defvarl fdt [mapcar (ret `x@{1}y`) dat])
(defvarl fen [mapcar (ret `x@{1}y`) enu])

(each ((d dat)
       (n enu))
  (trie-add tr0 d n)
  (trie-add tr1 d n))

(trie-compress tr1)

(defvarl rx0 (regex-compile (regex-from-trie tr0)))
(defvarl rx1 (regex-compile (regex-from-trie tr1)))

(mvtest
  (build (each ((d dat)) (add (filter-string-tree tr0 d)))) enu
  (build (each ((x ndt)) (add (filter-string-tree tr0 x)))) ndt
  (build (each ((f fdt)) (add (filter-string-tree tr0 f)))) fen)

(mvtest
  (build (each ((d dat)) (add (filter-string-tree tr1 d)))) enu
  (build (each ((x ndt)) (add (filter-string-tree tr1 x)))) ndt
  (build (each ((f fdt)) (add (filter-string-tree tr1 f)))) fen)

(mvtest
  (build (each ((d dat)) (add [rx0 d]))) dat
  (build (each ((n ndt)) (add [rx0 n]))) (repeat '(nil) (len dat)))

(mvtest
  (build (each ((d dat)) (add [rx1 d]))) dat
  (build (each ((n ndt)) (add [rx1 n]))) (repeat '(nil) (len dat)))

(mtest
  (regex-from-trie (make-trie)) t
  (regex-from-trie (trie-compress (make-trie))) t)
