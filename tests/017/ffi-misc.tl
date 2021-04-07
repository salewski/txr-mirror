(load "../common")

(defvarl ar (ffi (array char)))

(defvarl zar (ffi (zarray char)))

(test (ffi-put "\x1234@@@" ar) #b'e188b4404040')

(test (ffi-put "\x1234@@@" zar) #b'e188b440404000')

(test (ffi-get (ffi-put "\x1234@@@" zar) zar) "\x1234@@@")

(unless (meq (os-symbol) :cygwin :cygnal)
  (test (ffi-get #b'EDB08100' (ffi (zarray char)))
       "\xDCED\xDCB0\xDC81")

  (test (ffi-get #b'ED7F7FEDFF00' (ffi (zarray char)))
       "\xDCED\x7F\x7F\xDCED\xDCFF"))
