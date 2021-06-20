(load "../common")

(mtest
  #J0 0.0
  #J"abc" "abc"
  #Jtrue t
  #Jfalse nil
  #Jnull null)

(mtest
  #J1 1.0
  #J 1 1.0
  #J123 123.0
  #J0.123 0.123
  #J1.123 1.123
  #J1E3 1000.0
  #J1.1E3 1100.0
  #J1.1E+3 1100.0
  #J1.1E+03 1100.0
  #J1.1e3 1100.0
  #J1.1e+3 1100.0
  #J1.1e+03 1100.0)

(mtest
  #J"" ""
  #J"\u0000" "\xdc00"
  #J"\u0001" "\x1"
  #J"a\u0000b" "a\xdc00;b"
  #J"a\u0001b" "a\x1;b"
  #J"\b\t\n\f\r" "\b\t\n\f\r"
  #J"\/\\\"" "/\\\"")

(when (> (sizeof wchar) 2)
  (let ((chr (read "\"\\x10437\"")))
    (vtest #J"\ud801\udc37" `@chr`)
    (vtest #J"a\ud801\udc37b" `a@{chr}b`)))

(mtest
  #J[] #()
  #J[ ] #()
  #J[  ] #()
  #J [  ] #()
  #J[null] #(null)
  #J[false] #(nil)
  #J[true] #(t)
  #J["abc"] #("abc")
  #J[1,2,3] #(1.0 2.0 3.0)
  #J[ 1 , 2 , 3 ] #(1.0 2.0 3.0)
  #J[[]] #(#())
  #J[[],[]] #(#() #())
  #J[ [] , [] ] #(#() #())
  #J[[1],[2],3] #(#(1.0) #(2.0) 3.0))

(mtest
  #J{} #H(())
  #J{ } #H(())
  #J{  } #H(())
  #J {  } #H(())
  #J{true:true} #H(() (t t)))
  #J{ true : true } #H(() (t t))
  #J{ {} : {} } #H(() (#H(()) #H(())))
  #J{ "a" : 1.0 } #H(() (a 1.0))
  #J{ "a" : 1.0, "b" : [null] } #H(() (a 1.0) (b #(null)))

(let ((*print-circle* t))
  (mstest
    #J[#1="abc", #1#] "#(#1=\"abc\" #1#)"
    #2=#J[1, #2#] "#1=#(1.0 #J#1#)"
    #J#3=[1, #3#] "#1=#(1.0 #1#)"
    #4=#J{#4#:#4#} "#1=#H(() (#2=#J#1# #2#))"
    #J#5={#5#:#5#} "#1=#H(() (#1# #1#))")

  (let ((chash #J{"foo":#6="bar", "xyzzy":#6#}))
    (mtest
      [chash "xyzzy"] "bar"
      (eq [chash "foo"] [chash "xyzzy"]) t)))

(mtest
  ^#J~(+ 1.0 1) #J2
  ^#J[1, ~(+ 2.0 2)] #J[1, 4]
  ^#J[1, ~(+ 2.0 2), 3] #J[1, 4, 3]
  (eval ^^#J~#(1.0 ,*(list 2.0 3.0) 4.0)) #J[1, 2, 3, 4]
  ^#J[1, ~*(list 2.0 3.0), 4] #J[1, 2, 3, 4]
  #J^[1, ~(+ 2.0 2)] #(1.0 4.0)
  #J^[1, ~(+ 2.0 2), 3] #(1.0 4.0 3.0)
  ^#J{~(join "abc" "def") : "ghi"} #J{"abcdef":"ghi"}
  #J^{~(join "abc" "def") : "ghi"} #H(() ("abcdef" "ghi")))

;; get-json
(mtest
  (get-json "0") 0.0
  (get-json "\"abc\"") "abc"
  (get-json "true") t
  (get-json "false") nil
  (get-json "null") null
  (get-json "[1,2,3]") #(1.0 2.0 3.0)
  (get-json "{\"a\":\"b\"}") #H(() ("a" "b")))

(mtest
  (get-json "0 \n") 0.0
  (get-json "\"abc\" \n") "abc"
  (get-json "true \n") t
  (get-json "false \n") nil
  (get-json "null \n") null
  (get-json "[1,2,3] \n") #(1.0 2.0 3.0)
  (get-json "{\"a\":\"b\"} \n") #H(() ("a" "b")))

(mtest
  (get-json "0,") :error
  (get-json "\"abc\",") :error
  (get-json "true,") :error
  (get-json "false,") :error
  (get-json "null,") :error
  (get-json "[1,2,3],") :error
  (get-json "{\"a\":\"b\"},") :error)

(mtest
  (tojson #(1.0 "abc" t)) "[1,\"abc\",true]"
  (tojson "<!--") "\"<\\u0021--\""
  (tojson "a<!--b") "\"a<\\u0021--b\""
  (tojson "<!-") "\"<!-\""
  (tojson "-->") "\"-\\u002D>\""
  (tojson "a-->b") "\"a-\\u002D>b\""
  (tojson "->") "\"->\""
  (tojson "</") "\"</\""
  (tojson "</scrip") "\"</scrip\""
  (tojson "</script") "\"<\\/script\""
  (tojson "a</scriptb") "\"a<\\/scriptb\"")

(mtest
  (get-jsons "") nil
  (get-jsons "true") (t)
  (get-jsons "1 1 [2] {3:4}") (1.0 1.0 #(2.0) #H(() (3.0 4.0))))

(mtest
  (with-out-string-stream (s) (put-json nil s)) "false"
  (with-out-string-stream (s) (put-jsons nil s)) ""
  (with-out-string-stream (s) (put-jsons '(1.0 t nil) s)) "1\ntrue\nfalse\n")

(with-temp-file (name s "json")
  (mtest
    (file-put-json name #(1.0 2.0 3.0)) t
    (file-get-string name) "[1,2,3]\n"
    (file-get-json name) #(1.0 2.0 3.0)
    (file-append-json name #H(() ("a" t))) t
    (file-get-string name) "[1,2,3]\n{\"a\":true}\n"
    (file-get-jsons name) (#(1.0 2.0 3.0)
                           #H(() ("a" t)))
    (file-put-jsons name '(1.0 t null)) t
    (file-get-jsons name) (1.0 t null)
    (file-get-string name) "1\ntrue\nnull\n"
    (command-put-json `cat > @name` #(#() #())) t
    (file-get-string name) "[[],[]]\n")
    (command-get-json `cat @name`) #(#() #())
    (command-put-jsons `cat > @name` '(#() 1.0 nil)) t
    (file-get-string name) "[]\n1\nfalse\n"
    (command-get-jsons `cat @name`) '(#() 1.0 nil))
