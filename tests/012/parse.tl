(load "../common")

(unless (< (sizeof wchar) 4)
  (test (read `"@(str-buf #b'EDB081')"`)
        "\xDCED\xDCB0\xDC81")

  (test (read `"@(str-buf #b'F3FF')"`)
        "\xDCF3\xDCFF")

  (test (regex-parse (str-buf #b'EDB081'))
        (compound "\xDCED\xDCB0\xDC81"))

  (test (regex-parse (str-buf #b'F3FF'))
        (compound #\xDCF3 #\xDCFF)))

(mtest
  (read "0") 0
  (read "0x") 0x
  (read "a") a
  (read "abc") abc
  (read "abc.def") abc.def
  (read "(1 2 3)") (1 2 3)
  (read "#;(1 2 3) 4") 4
  (read "#;(1 2 3) #; a.b 4") 4
  (read "0 ") 0
  (read "0x ") 0x
  (read "a ") a
  (read "abc ") abc
  (read "abc.def ") abc.def
  (read "(1 2 3) ") (1 2 3)
  (read "#;(1 2 3) 4 ") 4
  (read "#;(1 2 3) #; a.b 4 ") 4
  (read "0,") :error
  (read "0x,") :error
  (read "a,") :error
  (read "abc,") :error
  (read "abc.def,") :error
  (read "(1 2 3),") :error
  (read "#;(1 2 3) 4,") :error
  (read "#;(1 2 3) #; a.b 4,") :error)

(mtest
  (iread "0") 0
  (iread "0x") 0x
  (iread "a") a
  (iread "abc") abc
  (iread "abc.def") abc
  (iread "(1 2 3)") (1 2 3)
  (iread "#;(1 2 3) 4") 4
  (iread "#;(1 2 3) #; a.b 4") .b
  (iread "0 ") 0
  (iread "0x ") 0x
  (iread "a ") a
  (iread "abc ") abc
  (iread "abc.def ") abc
  (iread "(1 2 3) ") (1 2 3)
  (iread "#;(1 2 3) 4 ") 4
  (iread "#;(1 2 3) #; a.b 4 ") .b
  (iread "0,") 0
  (iread "0x,") 0x
  (iread "a,") a
  (iread "abc,") abc
  (iread "abc.def,") abc
  (iread "(1 2 3),") (1 2 3)
  (iread "#;(1 2 3) 4,") 4
  (iread "#;(1 2 3) #; a.b 4,") .b)
