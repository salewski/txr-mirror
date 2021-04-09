(load "../common")

(test (read `"@(str-buf #b'EDB081')"`)
      "\xDCED\xDCB0\xDC81")

(test (regex-parse (str-buf #b'EDB081'))
      (compound "\xDCED\xDCB0\xDC81"))
