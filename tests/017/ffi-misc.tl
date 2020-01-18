(load "../common")

(defvarl ar (ffi (array char)))

(defvarl zar (ffi (zarray char)))

(test (ffi-put "\x1234@@@" ar) #b'e188b4404040')

(test (ffi-put "\x1234@@@" zar) #b'e188b440404000')

(test (ffi-get (ffi-put "\x1234@@@" zar) zar) "\x1234@@@")
