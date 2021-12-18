(load "../common")

(defvarl tr (tree))
(defvarl keys '(0 6 8 11 10 2 16 3 17 7 19 12 15 13 18 4 14 5 1 9))

(test tr #T(()))

(mtest
  (treep tr) t
  (treep 42) nil)

(mtest
  (len #T()) 0
  (len #T(() 1)) 1
  (len #T(() 1 2)) 2
  (len #T(() 1 2 3)) 3
  (len #T(() 1 1 1)) 3)

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

(mtest
  [tr 0] 0
  [tr 5] 5
  [tr 19] 19)

(mtest
  [tr 0..3] (0 1 2)
  [tr 3..5] (3 4)
  [tr -2..0] ()
  [tr -2..4] (0 1 2 3)
  [tr :..4] (0 1 2 3)
  [tr 18..100] (18 19)
  [tr 18..:] (18 19)
  [tr 100..200] ())

(vtest
  [tr :..:] (range 0 19))

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

(vtest (build (for* ((j (tree-begin tr))
                     (i (progn (tree-next j) (tree-next j) (tree-reset j tr)))
                     (n (tree-peek i)))
                    ((and n (eq (tree-next i) n)))
                    ((set n (tree-peek i)))
                (add (key n))))
       (range 0 19))

(defvarl trc (copy-search-tree tr))

(vtest trc tr)

(tree-clear trc)

(test trc #T(()))

(test (tree-delete tr 6) 6)

(vtest (build (for* ((i (tree-begin tr 6))
                     (n (tree-next i)))
                    (n)
                    ((set n (tree-next i)))
                (add (key n))))
       (range 7 19))

(vtest (build (for* ((i (tree-begin tr 0))
                     (n (tree-next i)))
                    (n)
                    ((set n (tree-next i)))
                (add (key n))))
       (rlist 0..5 7..19))

(vtest (build (for* ((i (tree-begin tr 8))
                     (n (tree-next i)))
                    (n)
                    ((set n (tree-next i)))
                (add (key n))))
       (range 8 19))

(vtest (build (for* ((i (tree-reset (tree-begin #T(())) tr 8))
                     (n (tree-next i)))
                    (n)
                    ((set n (tree-next i)))
                (add (key n))))
       (range 8 19))

(test (let* ((t0 (tree-begin tr))
             (t1 (progn (tree-next t0) (copy-tree-iter t0))))
        (tree-next t0)
        (tree-next t0)
        (list (key (tree-next t1))
              (key (tree-next t1))
              (key (tree-next t1))))
      (1 2 3))

(test (let* ((t0 (tree-begin tr))
             (t1 (progn (tree-next t0) (copy-tree-iter t0)))
             (t2 (replace-tree-iter (tree-begin tr) t0)))
        (tree-next t0)
        (tree-next t0)
        (list (key (tree-next t1))
              (key (tree-next t1))
              (key (tree-next t2))
              (key (tree-next t2))))
      (1 2 1 2))

(test (tree-next (tree-begin tr 20)) nil)

(test (tree-next (tree-begin #T(()) 0)) nil)
(test (key (tree-next (tree-begin #T(() 1) 1))) 1)

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

(set *tree-fun-whitelist* [list* '= '< *tree-fun-whitelist*])

(let ((tr [tree '(1 2 3) identity < =]))
  (mtest
    tr #T((identity < =) 1 2 3)
    (copy-search-tree tr) #T((identity < =) 1 2 3)
    (make-similar-tree tr) #T((identity < =))))

(test
  (collect-each ((el (tree-begin #T(() 1 2 3 4 5) 2 5)))
    (* 10 el))
  (20 30 40))

(mtest
  (uni #T(() "a" "b") #T(() "b" "c")) ("a" "b" "c")
  (diff #T(() "a" "b") #T(() "b" "c")) ("a")
  (isec #T(() "a" "b") #T(() "b" "c")) ("b"))

(defstruct (item label key) ()
  label
  key
  (:method equal (it) it.key))

(defun make-items ()
  (vec (new (item 'a 1))
       (new (item 'b 2))
       (new (item 'c 2))
       (new (item 'd 2))
       (new (item 'e 2))
       (new (item 'f 3))))

(let* ((items (make-items))
       (tr (tree items : : : t)))
  (each ((it items))
    (vtest (tree-delete tr it) it))
  (test tr #T(())))

(let* ((items (make-items))
       (tr (tree items : : : t)))
  (each ((it items))
    (let* ((tn (tree-lookup-node tr it.key))
           (iu (key tn)))
      (vtest (tree-delete-specific-node tr tn) tn)
      (each ((iv tr))
        (test (eq iv.label iu.label) nil))))
  (test tr #T(())))

(let* ((items (make-items))
       (tr (tree items : : : t)))
  (vtest (vec-list [mapcar .label tr]) [mapcar .label items]))
