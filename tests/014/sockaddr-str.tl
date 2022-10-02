(load "../common.tl")

(mtest
  (sockaddr-str "") :error
  (sockaddr-str "1") :error
  (sockaddr-str "x") :error
  (sockaddr-str "1:2") :error
  (sockaddr-str "[]:") :error)

(mtest
  (sockaddr-str "[::1]:-1") :error
  (sockaddr-str "[::1]:65536") :error
  (sockaddr-str "1.2.3.4:-1") :error
  (sockaddr-str "1.2.3.4:65536") :error
  (sockaddr-str "1:2/8") :error)

(mtest
  (sockaddr-str "/") #S(sockaddr-un canonname nil path "/")
  (sockaddr-str "/abc") #S(sockaddr-un canonname nil path "/abc"))

(mtest
  (sockaddr-str "[::1]:2") #S(sockaddr-in6 addr 1 port 2)
  (sockaddr-str "[::1/1]:2") #S(sockaddr-in6 addr 0 port 2 prefix 1))

(mtest
  (sockaddr-str "::1") #S(sockaddr-in6 addr 1)
  (sockaddr-str "::1/1") #S(sockaddr-in6 addr 0 prefix 1))

(mtest
  (sockaddr-str "::ffff:1.2.3.4") #S(sockaddr-in6 addr 281470698652420)
  (sockaddr-str "::ffff:1.2.3.4/96") #S(sockaddr-in6 addr 281470681743360 prefix 96))

(mtest
  (sockaddr-str "1.2.3.4") #S(sockaddr-in addr 16909060)
  (sockaddr-str "1.2.3.4/8") #S(sockaddr-in addr 16777216 prefix 8)
  (sockaddr-str "1.2.3.4:16") #S(sockaddr-in addr 16909060 port 16)
  (sockaddr-str "1.2.3.4/8:16") #S(sockaddr-in addr 16777216 prefix 8 port 16))

(mtest
  (sockaddr-str "1/8") #S(sockaddr-in addr 16777216 prefix 8)
  (sockaddr-str "1.2/8") #S(sockaddr-in addr 16777216 prefix 8)
  (sockaddr-str "1/8:5") #S(sockaddr-in addr 16777216 prefix 8 port 5)
  (sockaddr-str "1.2/8:5") #S(sockaddr-in addr 16777216 prefix 8 port 5))

(mtest
  (sockaddr-str "1:2:3:4:5:6:7:8/127")
  #S(sockaddr-in6 addr 5192455318486707404433266433261576 prefix 127)
  (sockaddr-str "[1:2:3:4:5:6:7:8/127]:5")
  #S(sockaddr-in6 addr 5192455318486707404433266433261576 prefix 127 port 5))
