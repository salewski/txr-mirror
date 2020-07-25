(load "../common.tl")

(test (in6addr-str "junk") :error)

(test (in6addr-str "0:0:0:0:0:0:0:0:0") :error)
(test (in6addr-str "0:0:0:0:0:0") :error)
(test (in6addr-str "0:0:0:0") :error)
(test (in6addr-str "") :error)

(test (in6addr-str "0:0:0:0:0:0:0:x:0") :error)

(test (in6addr-str ":0:0:0:0:0:0:0:0:0") :error)
(test (in6addr-str "0:0:0:0:0:0:0:0:0:") :error)

(test (in6addr-str "0:0:0:0:0:0:0:0:FFFFF") :error)
(test (in6addr-str "0:0:0:0:0:0:0:FFFFF:0") :error)
(test (in6addr-str "0:0:0:0:0:0:FFFFF:0:0") :error)
(test (in6addr-str "0:0:0:0:0:FFFFF:0:0:0") :error)
(test (in6addr-str "0:0:0:0:FFFFF:0:0:0:0") :error)
(test (in6addr-str "0:0:0:FFFFF:0:0:0:0:0") :error)
(test (in6addr-str "0:0:FFFFF:0:0:0:0:0:0") :error)
(test (in6addr-str "0:FFFFF:0:0:0:0:0:0:0") :error)
(test (in6addr-str "FFFFF:0:0:0:0:0:0:0:0") :error)

(test (in6addr-str "0:0:0:0:0:0:0:0/") :error)
(test (in6addr-str "0:0:0:0:0:0:0:0/129") :error)
(test (in6addr-str "[0:0:0:0:0:0:0:0]:") :error)
(test (in6addr-str "[0:0:0:0:0:0:0:0]:65536") :error)

(test (in6addr-str "0:0:0:0:0:0:0:0")
      #S(sockaddr-in6 addr 0
                      port 0 flow-info 0 scope-id 0 prefix 128))
(test (in6addr-str "1111:2222:3333:4444:5555:6666:7777:8888")
      #S(sockaddr-in6 addr 22685837286468424649968941046919825544
                      port 0 flow-info 0 scope-id 0 prefix 128))
(test (in6addr-str "FFFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF")
      #S(sockaddr-in6 addr 340282366920938463463374607431768211455
                      port 0 flow-info 0 scope-id 0 prefix 128))

(test (in6addr-str "[0:0:0:0:0:0:0:0]:42")
      #S(sockaddr-in6 addr 0
                      port 42 flow-info 0 scope-id 0 prefix 128))
(test (in6addr-str "[1111:2222:3333:4444:5555:6666:7777:8888]:42")
      #S(sockaddr-in6 addr 22685837286468424649968941046919825544
                      port 42 flow-info 0 scope-id 0 prefix 128))
(test (in6addr-str "[FFFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF]:42")
      #S(sockaddr-in6 addr 340282366920938463463374607431768211455
                      port 42 flow-info 0 scope-id 0 prefix 128))

(test (in6addr-str "0:0:0:0:0:0:0:0/16")
      #S(sockaddr-in6 addr 0
                      port 0 flow-info 0 scope-id 0 prefix 16))
(test (in6addr-str "1111:2222:3333:4444:5555:6666:7777:8888/16")
      #S(sockaddr-in6 addr 22685144974938661909049738462362599424
                      port 0 flow-info 0 scope-id 0 prefix 16))
(test (in6addr-str "FFFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF/16")
      #S(sockaddr-in6 addr 340277174624079928635746076935438991360
                      port 0 flow-info 0 scope-id 0 prefix 16))
(test (in6addr-str "ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff/16")
      #S(sockaddr-in6 addr 340277174624079928635746076935438991360
                      port 0 flow-info 0 scope-id 0 prefix 16))

(test (in6addr-str "[0:0:0:0:0:0:0:0/16]:42")
      #S(sockaddr-in6 addr 0
                      port 42 flow-info 0 scope-id 0 prefix 16))
(test (in6addr-str "[1111:2222:3333:4444:5555:6666:7777:8888/16]:42")
      #S(sockaddr-in6 addr 22685144974938661909049738462362599424
                      port 42 flow-info 0 scope-id 0 prefix 16))
(test (in6addr-str "[FFFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF/16]:42")
      #S(sockaddr-in6 addr 340277174624079928635746076935438991360
                      port 42 flow-info 0 scope-id 0 prefix 16))

(test (in6addr-str "1:2:3:4:5:6::7:8:9") :error)
(test (in6addr-str "1:2:3:4:5:6::7:8:9") :error)
(test (in6addr-str "1:2:3:4:5:6::7:8") :error)

(test (in6addr-str "::1")
      #S(sockaddr-in6 addr 1
                      port 0 flow-info 0 scope-id 0 prefix 128))

(test (in6addr-str "1::")
      #S(sockaddr-in6 addr 5192296858534827628530496329220096
                      port 0 flow-info 0 scope-id 0 prefix 128))

(test (in6addr-str "1::1")
      #S(sockaddr-in6 addr 5192296858534827628530496329220097
                      port 0 flow-info 0 scope-id 0 prefix 128))

(test (in6addr-str "1:2::3:4")
      #S(sockaddr-in6 addr 5192455314859856157205683417317380
                      port 0 flow-info 0 scope-id 0 prefix 128))

(test (in6addr-str "::ffff:1.2.3.4.5") :error)
(test (in6addr-str "::ffff:1.2.3.4:5") :error)
(test (in6addr-str "::ffff:1.2.3") :error)
(test (in6addr-str "::ffff:1.2.3:4") :error)

(test (in6addr-str "::ffff:1.2.3.4")
      #S(sockaddr-in6 addr 281470698652420
                      port 0 flow-info 0 scope-id 0 prefix 128))

(test (in6addr-str "::FFFF:1.2.3.4")
      #S(sockaddr-in6 addr 281470698652420
                      port 0 flow-info 0 scope-id 0 prefix 128))

(test (in6addr-str "::FfFf:1.2.3.4")
      #S(sockaddr-in6 addr 281470698652420
                      port 0 flow-info 0 scope-id 0 prefix 128))

(test (in6addr-str "::FFFF:1.2.3.4/96")
      #S(sockaddr-in6 addr 281470681743360
                      port 0 flow-info 0 scope-id 0 prefix 96))

(test (in6addr-str "[::ffff:1.2.3.4]:42")
      #S(sockaddr-in6 addr 281470698652420
                      port 42 flow-info 0 scope-id 0 prefix 128))

(test (in6addr-str "[::FFFF:1.2.3.4/96]:42")
      #S(sockaddr-in6 addr 281470681743360
                      port 42 flow-info 0 scope-id 0 prefix 96))
