(let* ((epoch (or (tointz (getenv "SOURCE_DATE_EPOCH")) 0))
       (pdf (file-get-string "txr-manpage.pdf"))
       (start (search-str pdf "<?xpacket begin="))
       (end (if start (search-str pdf "/Creator(" start)))
       (xml (if end [pdf start..end]))
       (orig-len (len xml))
       (isotime (time-string-utc epoch "%FT%T"))
       (gstime (time-string-utc epoch "%Y%m%d%H%M%SZ0000")))
  (unless xml
    (format *stderr* "XML block not found in PDF")
    (exit nil))
  (upd xml
    (regsub #/uuid:........-....-....-....-............/
            "uuid:00000000-0000-0000-0000-000000000000")
    (regsub #/Date>....-..-..T..:..:..(Z|[+\-]..:..)/
            (ret `Date>@isotime@(if (ends-with "Z" @1) "Z" "+00:00")`))
    (regsub #/Date\(D:..............[Z+\-]..../
            `Date(D:@gstime`))
  (assert (eql (len xml) orig-len))
  (set [pdf start..end] xml)
  (file-put-string "txr-manpage.pdf.temp" pdf)
  (rename-path "txr-manpage.pdf.temp" "txr-manpage.pdf"))
