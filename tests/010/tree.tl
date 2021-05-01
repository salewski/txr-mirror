(load "../common")

(defvarl tr (tree))
(defvarl keys '(0 6 8 11 10 2 16 3 17 7 19 12 15 13 18 4 14 5 1 9))

(test tr #T(()))

(test (treep tr) t)
(test (treep 42) nil)

(each ((n keys))
  (tree-insert tr n))

(mtest
  (tree-lookup tr 0) 0
  (tree-lookup tr 1) 1
  (tree-lookup tr 2) 2
  (tree-lookup tr 3) 3
  (tree-lookup tr 4) 4
  (tree-lookup tr 5) 5
  (tree-lookup tr 6) 6
  (tree-lookup tr 7) 7
  (tree-lookup tr 8) 8
  (tree-lookup tr 9) 9
  (tree-lookup tr 10) 10
  (tree-lookup tr 11) 11
  (tree-lookup tr 12) 12
  (tree-lookup tr 13) 13
  (tree-lookup tr 14) 14
  (tree-lookup tr 15) 15
  (tree-lookup tr 16) 16
  (tree-lookup tr 17) 17
  (tree-lookup tr 18) 18
  (tree-lookup tr 19) 19)

(vtest (build (for* ((i (tree-begin tr))
                     (n (tree-next i)))
                    (n)
                    ((set n (tree-next i)))
                (add (key n))))
       (range 0 19))

(vtest (build (for* ((j (tree-begin tr))
                     (i (progn (tree-next j) (tree-next j) (tree-reset j tr)))
                     (n (tree-next i)))
                    (n)
                    ((set n (tree-next i)))
                (add (key n))))
       (range 0 19))

(defvarl trc (copy-search-tree tr))

(vtest trc tr)

(tree-clear trc)

(test trc #T(()))

(test (tree-delete tr 6) 6)

(vtest (build (for* ((i (tree-begin-at tr 6))
                     (n (tree-next i)))
                    (n)
                    ((set n (tree-next i)))
                (add (key n))))
       (range 7 19))

(vtest (build (for* ((i (tree-begin-at tr 0))
                     (n (tree-next i)))
                    (n)
                    ((set n (tree-next i)))
                (add (key n))))
       (rlist 0..5 7..19))

(vtest (build (for* ((i (tree-begin-at tr 8))
                     (n (tree-next i)))
                    (n)
                    ((set n (tree-next i)))
                (add (key n))))
       (range 8 19))

(vtest (build (for* ((i (tree-reset-at (tree-begin #T(())) tr 8))
                     (n (tree-next i)))
                    (n)
                    ((set n (tree-next i)))
                (add (key n))))
       (range 8 19))

(test (tree-next (tree-begin-at tr 20)) nil)

(test (tree-next (tree-begin-at #T(()) 0)) nil)
(test (key (tree-next (tree-begin-at #T(() 1) 1))) 1)

(mtest
  (tree-delete tr 0) 0
  (tree-delete tr 1) 1
  (tree-delete tr 2) 2
  (tree-delete tr 3) 3
  (tree-delete tr 4) 4
  (tree-delete tr 5) 5
  (tree-delete tr 7) 7
  (tree-delete tr 8) 8
  (tree-delete tr 9) 9
  (tree-delete tr 10) 10
  (tree-delete tr 11) 11
  (tree-delete tr 12) 12
  (tree-delete tr 13) 13
  (tree-delete tr 14) 14
  (tree-delete tr 15) 15
  (tree-delete tr 16) 16
  (tree-delete tr 17) 17
  (tree-delete tr 18) 18
  (tree-delete tr 19) 19)
