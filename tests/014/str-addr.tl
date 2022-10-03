(load "../common.tl")

(mtest
  #S(sockaddr-un path "/foo").(str-addr) "/foo"
  #S(sockaddr-un path nil).(str-addr) :error)

(mtest
  #S(sockaddr-in addr 0).(str-addr) "0.0.0.0"
  #S(sockaddr-in addr #x01020304).(str-addr) "1.2.3.4"
  #S(sockaddr-in addr #x01020304 prefix 8).(str-addr) "1.2.3.4/8"
  #S(sockaddr-in addr #x01020304 prefix 16).(str-addr) "1.2.3.4/16"
  #S(sockaddr-in addr #x01020304 prefix 24).(str-addr) "1.2.3.4/24"
  #S(sockaddr-in addr #x01020304 prefix 31).(str-addr) "1.2.3.4/31"
  #S(sockaddr-in addr #x01020304 prefix 32).(str-addr) "1.2.3.4"
  #S(sockaddr-in addr #x01000000 prefix 8).(str-addr) "1/8"
  #S(sockaddr-in addr #x01000000 prefix 16).(str-addr) "1/16"
  #S(sockaddr-in addr #x01020000 prefix 16).(str-addr) "1.2/16"
  #S(sockaddr-in addr #x01000000 prefix 24).(str-addr) "1/24"
  #S(sockaddr-in addr #x01020000 prefix 24).(str-addr) "1.2/24"
  #S(sockaddr-in addr #x01020300 prefix 24).(str-addr) "1.2.3/24"
  #S(sockaddr-in addr #x01000000 prefix 31).(str-addr) "1/31"
  #S(sockaddr-in addr 0 port 123).(str-addr) "0.0.0.0:123"
  #S(sockaddr-in addr #x01020304 port 123).(str-addr) "1.2.3.4:123"
  #S(sockaddr-in addr #x01020304 port 123 prefix 8).(str-addr) "1.2.3.4/8:123"
  #S(sockaddr-in addr #x01020304 port 123 prefix 16).(str-addr) "1.2.3.4/16:123"
  #S(sockaddr-in addr #x01020304 port 123 prefix 24).(str-addr) "1.2.3.4/24:123"
  #S(sockaddr-in addr #x01020304 port 123 prefix 24).(str-addr) "1.2.3.4/24:123"
  #S(sockaddr-in addr #x01020304 port 123 prefix 31).(str-addr) "1.2.3.4/31:123"
  #S(sockaddr-in addr #x01020304 port 123 prefix 32).(str-addr) "1.2.3.4:123"
  #S(sockaddr-in addr #x01000000 port 123 prefix 8).(str-addr) "1/8:123"
  #S(sockaddr-in addr #x01000000 port 123 prefix 16).(str-addr) "1/16:123"
  #S(sockaddr-in addr #x01020000 port 123 prefix 16).(str-addr) "1.2/16:123"
  #S(sockaddr-in addr #x01000000 port 123 prefix 24).(str-addr) "1/24:123"
  #S(sockaddr-in addr #x01020000 port 123 prefix 24).(str-addr) "1.2/24:123"
  #S(sockaddr-in addr #x01020300 port 123 prefix 24).(str-addr) "1.2.3/24:123"
  #S(sockaddr-in addr #x01000000 port 123 prefix 31).(str-addr) "1/31:123")

(mtest
  #S(sockaddr-in6 addr 0).(str-addr) "::"
  #S(sockaddr-in6 addr #x80000000000000000000000000000001).(str-addr) "8000::1"
  #S(sockaddr-in6 addr #x00000000000000000000000000000001).(str-addr) "::1"
  #S(sockaddr-in6 addr #x80000000000000000000000000000000).(str-addr) "8000::"
  #S(sockaddr-in6 addr #x00008000000000000000000000000001).(str-addr) "0:8000::1"
  #S(sockaddr-in6 addr #x00000000000000000000000000010000).(str-addr) "::1:0"
  #S(sockaddr-in6 addr #x00008000000000000000000000010000).(str-addr) "0:8000::1:0"
  #S(sockaddr-in6 addr #x000000000000abcd0000000000000000).(str-addr) "0:0:0:abcd::"
  #S(sockaddr-in6 addr #x0000000000000000abcd000000000000).(str-addr) "::abcd:0:0:0"
  #S(sockaddr-in6 addr #x11112222333344445555666677778888).(str-addr) "1111:2222:3333:4444:5555:6666:7777:8888"
  #S(sockaddr-in6 addr #x01000200030004000500060007000800).(str-addr) "100:200:300:400:500:600:700:800"
  #S(sockaddr-in6 addr #x00008000000000000000000000010000 port 0).(str-addr) "0:8000::1:0"
  #S(sockaddr-in6 addr #x00008000000000000000000000010000 port 123).(str-addr) "[0:8000::1:0]:123"
  #S(sockaddr-in6 addr #x00008000000000000000000000010000 prefix 128).(str-addr) "0:8000::1:0"
  #S(sockaddr-in6 addr #x00008000000000000000000000010000 prefix 127).(str-addr) "0:8000::1:0/127"
  #S(sockaddr-in6 addr #x00008000000000000000000000010000 port 123 prefix 127).(str-addr) "[0:8000::1:0/127]:123")

(mtest
  #S(sockaddr-in6 addr #xffff00000000).(str-addr) "::ffff:0.0.0.0"
  #S(sockaddr-in6 addr #xffff00000000 prefix 24).(str-addr) "::ffff:0/24"
  #S(sockaddr-in6 addr #xffff01000000 prefix 24).(str-addr) "::ffff:1/24"
  #S(sockaddr-in6 addr #xffff01020000 prefix 24).(str-addr) "::ffff:1.2/24"
  #S(sockaddr-in6 addr #xffff01020300 prefix 24).(str-addr) "::ffff:1.2.3/24"
  #S(sockaddr-in6 addr #xffff01020304 prefix 24).(str-addr) "::ffff:1.2.3.4/24"
  #S(sockaddr-in6 addr #xffff00000000 port 123 ).(str-addr) "[::ffff:0.0.0.0]:123"
  #S(sockaddr-in6 addr #xffff00000000 port 123 prefix 24).(str-addr) "[::ffff:0/24]:123"
  #S(sockaddr-in6 addr #xffff01000000 port 123 prefix 24).(str-addr) "[::ffff:1/24]:123"
  #S(sockaddr-in6 addr #xffff01020000 port 123 prefix 24).(str-addr) "[::ffff:1.2/24]:123"
  #S(sockaddr-in6 addr #xffff01020300 port 123 prefix 24).(str-addr) "[::ffff:1.2.3/24]:123"
  #S(sockaddr-in6 addr #xffff01020304 port 123 prefix 24).(str-addr) "[::ffff:1.2.3.4/24]:123")
