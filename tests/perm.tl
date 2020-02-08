(defun enc-perm (p)
  (let ((d (digits (logior 4096 p) 2)))
    (mapcar (do if (zerop @2) #\- @1) "sgtrwxrwxrwx" (rest d))))

(defun dec-perm (s)
  (let ((d (mapcar (do cond
                     ((eql @1 @2) 1)
                     ((eql #\- @2) 0)
                     (t (error `decode-perm: invalid input @s`)))
                   "sgtrwxrwxrwx" s)))
    (poly 2 d)))
