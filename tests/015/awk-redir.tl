(load "../common")

(push-after-load
  (each ((f '#"file1.out file2.out file3.out"))
    (remove-path f)))

(file-put-lines "file1.out" '("old"))
(file-put-lines "file2.out" '("old"))

(awk
  (:begin (->> "file1.out" (prn "abc"))
          (->> "file1.out" (prn "def"))))

(test
  (file-get-lines "file1.out")
  #"old abc def")

(awk
  (:begin (-> "file2.out" (prn "abc"))
          (-> "file2.out" (prn "def"))))

(test
  (file-get-lines "file2.out")
  #"abc def")

(test
  (build
    (awk
      (:begin (<- "file1.out" (add (get-line)))
              (<- "file1.out" (add (get-line))))))
  #"old abc")


(awk (:begin (!> "cat > file3.out" (prn "out"))))

(test (file-get-string "file3.out") "out\n")

(test
  (build
    (awk
      (:begin (<! "cat file3.out" (add (get-line))))))
  #"out")
