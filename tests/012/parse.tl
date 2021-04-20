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
