(load "../common")

(vtest
  (catch
    (open-file "/NoNeXiStEnT")
    (error (msg)
      (string-get-code msg)))
  enoent)
