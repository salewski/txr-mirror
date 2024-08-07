(load "../common")

(defun normtype (obj)
  (etypecase obj
    (null 'list)
    (cons 'list)
    (lit 'string)
    (str 'string)
    (vec 'vec)))

(defun test-comb (s k)
  (let ((out (comb s k))
        (nCk (n-choose-k (len s) k)))
    (if (> k (len s))
      (test out nil)
      (mvtest
        (len out) nCk
        (len (uniq out)) nCk))
    (vtest
      (sort out) out)
    (unless (empty out)
      (let ((stype (normtype s))
            (otype (normtype (first out))))
        (vtest stype otype)))))

(defun test-rcomb (s k)
  (let ((out (rcomb s k))
        (nCk (n-choose-k (+ (len s) k -1) k)))
    (if (and (empty s) (plusp k))
      (test out nil)
      (mvtest
        (len out) nCk
        (len (uniq out)) nCk))
    (vtest
      (sort out) out)
    (unless (empty out)
      (let ((stype (normtype s))
            (otype (normtype (first out))))
        (vtest stype otype)))))

(defun test-perm (s k)
  (let ((out (perm s k))
        (nPk (n-perm-k (len s) k)))
    (if (> k (len s))
      (test out nil)
      (mvtest
        (len out) nPk
        (len (uniq out)) nPk))
    (unless (empty out)
      (let ((stype (normtype s))
            (otype (normtype (first out))))
        (vtest stype otype)))))

(defun test-rperm (s k)
  (let ((out (rperm s k))
        (exp (expt (len s) k)))
    (mvtest
      (len out) exp
      (len (uniq out)) exp)
    (vtest
      (sort out) out)
    (unless (empty out)
      (let ((stype (normtype s))
            (otype (normtype (first out))))
        (vtest stype otype)))))

(test-comb #() 0)
(test-comb #() 1)
(test-comb #(1) 0)
(test-comb #(1) 1)
(test-comb #(1) 2)
(test-comb #(1 2) 0)
(test-comb #(1 2) 1)
(test-comb #(1 2) 2)
(test-comb #(1 2) 3)
(test-comb #(1 2 3) 0)
(test-comb #(1 2 3) 1)
(test-comb #(1 2 3) 2)
(test-comb #(1 2 3) 3)
(test-comb #(1 2 3) 4)
(test-comb #(1 2 3 4) 0)
(test-comb #(1 2 3 4) 1)
(test-comb #(1 2 3 4) 2)
(test-comb #(1 2 3 4) 3)
(test-comb #(1 2 3 4) 4)
(test-comb #(1 2 3 4) 5)
(test-comb #(1 2 3 4 5) 0)
(test-comb #(1 2 3 4 5) 1)
(test-comb #(1 2 3 4 5) 2)
(test-comb #(1 2 3 4 5) 3)
(test-comb #(1 2 3 4 5) 4)
(test-comb #(1 2 3 4 5) 5)
(test-comb #(1 2 3 4 5) 5)

(test-comb '() 0)
(test-comb '() 1)
(test-comb '(1) 0)
(test-comb '(1) 1)
(test-comb '(1) 2)
(test-comb '(1 2) 0)
(test-comb '(1 2) 1)
(test-comb '(1 2) 2)
(test-comb '(1 2) 3)
(test-comb '(1 2 3) 0)
(test-comb '(1 2 3) 1)
(test-comb '(1 2 3) 2)
(test-comb '(1 2 3) 3)
(test-comb '(1 2 3) 4)
(test-comb '(1 2 3 4) 0)
(test-comb '(1 2 3 4) 1)
(test-comb '(1 2 3 4) 2)
(test-comb '(1 2 3 4) 3)
(test-comb '(1 2 3 4) 4)
(test-comb '(1 2 3 4) 5)
(test-comb '(1 2 3 4 5) 0)
(test-comb '(1 2 3 4 5) 1)
(test-comb '(1 2 3 4 5) 2)
(test-comb '(1 2 3 4 5) 3)
(test-comb '(1 2 3 4 5) 4)
(test-comb '(1 2 3 4 5) 5)
(test-comb '(1 2 3 4 5) 5)

(test-comb "" 0)
(test-comb "" 1)
(test-comb "1" 0)
(test-comb "1" 1)
(test-comb "1" 2)
(test-comb "12" 0)
(test-comb "12" 1)
(test-comb "12" 2)
(test-comb "12" 3)
(test-comb "123" 0)
(test-comb "123" 1)
(test-comb "123" 2)
(test-comb "123" 3)
(test-comb "123" 4)
(test-comb "1234" 0)
(test-comb "1234" 1)
(test-comb "1234" 2)
(test-comb "1234" 3)
(test-comb "1234" 4)
(test-comb "1234" 5)
(test-comb "12345" 0)
(test-comb "12345" 1)
(test-comb "12345" 2)
(test-comb "12345" 3)
(test-comb "12345" 4)
(test-comb "12345" 5)
(test-comb "12345" 5)

(mtest
  (comb #() -1) :error
  (comb #(1) -1) :error
  (comb () -1) :error
  (comb '(1) -1) :error
  (comb "" -1) :error
  (comb "a" -1) :error)

(test-rcomb #() 0)
(test-rcomb #() 1)
(test-rcomb #(1) 0)
(test-rcomb #(1) 1)
(test-rcomb #(1) 2)
(test-rcomb #(1 2) 0)
(test-rcomb #(1 2) 1)
(test-rcomb #(1 2) 2)
(test-rcomb #(1 2) 3)
(test-rcomb #(1 2 3) 0)
(test-rcomb #(1 2 3) 1)
(test-rcomb #(1 2 3) 2)
(test-rcomb #(1 2 3) 3)
(test-rcomb #(1 2 3) 4)
(test-rcomb #(1 2 3 4) 0)
(test-rcomb #(1 2 3 4) 1)
(test-rcomb #(1 2 3 4) 2)
(test-rcomb #(1 2 3 4) 3)
(test-rcomb #(1 2 3 4) 4)
(test-rcomb #(1 2 3 4) 5)
(test-rcomb #(1 2 3 4 5) 0)
(test-rcomb #(1 2 3 4 5) 1)
(test-rcomb #(1 2 3 4 5) 2)
(test-rcomb #(1 2 3 4 5) 3)
(test-rcomb #(1 2 3 4 5) 4)
(test-rcomb #(1 2 3 4 5) 5)
(test-rcomb #(1 2 3 4 5) 5)

(test-rcomb '() 0)
(test-rcomb '() 1)
(test-rcomb '(1) 0)
(test-rcomb '(1) 1)
(test-rcomb '(1) 2)
(test-rcomb '(1 2) 0)
(test-rcomb '(1 2) 1)
(test-rcomb '(1 2) 2)
(test-rcomb '(1 2) 3)
(test-rcomb '(1 2 3) 0)
(test-rcomb '(1 2 3) 1)
(test-rcomb '(1 2 3) 2)
(test-rcomb '(1 2 3) 3)
(test-rcomb '(1 2 3) 4)
(test-rcomb '(1 2 3 4) 0)
(test-rcomb '(1 2 3 4) 1)
(test-rcomb '(1 2 3 4) 2)
(test-rcomb '(1 2 3 4) 3)
(test-rcomb '(1 2 3 4) 4)
(test-rcomb '(1 2 3 4) 5)
(test-rcomb '(1 2 3 4 5) 0)
(test-rcomb '(1 2 3 4 5) 1)
(test-rcomb '(1 2 3 4 5) 2)
(test-rcomb '(1 2 3 4 5) 3)
(test-rcomb '(1 2 3 4 5) 4)
(test-rcomb '(1 2 3 4 5) 5)
(test-rcomb '(1 2 3 4 5) 5)

(test-rcomb "" 0)
(test-rcomb "" 1)
(test-rcomb "1" 0)
(test-rcomb "1" 1)
(test-rcomb "1" 2)
(test-rcomb "12" 0)
(test-rcomb "12" 1)
(test-rcomb "12" 2)
(test-rcomb "12" 3)
(test-rcomb "123" 0)
(test-rcomb "123" 1)
(test-rcomb "123" 2)
(test-rcomb "123" 3)
(test-rcomb "123" 4)
(test-rcomb "1234" 0)
(test-rcomb "1234" 1)
(test-rcomb "1234" 2)
(test-rcomb "1234" 3)
(test-rcomb "1234" 4)
(test-rcomb "1234" 5)
(test-rcomb "12345" 0)
(test-rcomb "12345" 1)
(test-rcomb "12345" 2)
(test-rcomb "12345" 3)
(test-rcomb "12345" 4)
(test-rcomb "12345" 5)
(test-rcomb "12345" 5)

(mtest
  (rcomb #() -1) :error
  (rcomb #(1) -1) :error
  (rcomb () -1) :error
  (rcomb '(1) -1) :error
  (rcomb "" -1) :error
  (rcomb "a" -1) :error)

(test-perm #() 0)
(test-perm #() 1)
(test-perm #(1) 0)
(test-perm #(1) 1)
(test-perm #(1) 2)
(test-perm #(1 2) 0)
(test-perm #(1 2) 1)
(test-perm #(1 2) 2)
(test-perm #(1 2) 3)
(test-perm #(1 2 3) 0)
(test-perm #(1 2 3) 1)
(test-perm #(1 2 3) 2)
(test-perm #(1 2 3) 3)
(test-perm #(1 2 3) 4)
(test-perm #(1 2 3 4) 0)
(test-perm #(1 2 3 4) 1)
(test-perm #(1 2 3 4) 2)
(test-perm #(1 2 3 4) 3)
(test-perm #(1 2 3 4) 4)
(test-perm #(1 2 3 4) 5)
(test-perm #(1 2 3 4 5) 0)
(test-perm #(1 2 3 4 5) 1)
(test-perm #(1 2 3 4 5) 2)
(test-perm #(1 2 3 4 5) 3)
(test-perm #(1 2 3 4 5) 4)
(test-perm #(1 2 3 4 5) 5)
(test-perm #(1 2 3 4 5) 5)

(test-perm '() 0)
(test-perm '() 1)
(test-perm '(1) 0)
(test-perm '(1) 1)
(test-perm '(1) 2)
(test-perm '(1 2) 0)
(test-perm '(1 2) 1)
(test-perm '(1 2) 2)
(test-perm '(1 2) 3)
(test-perm '(1 2 3) 0)
(test-perm '(1 2 3) 1)
(test-perm '(1 2 3) 2)
(test-perm '(1 2 3) 3)
(test-perm '(1 2 3) 4)
(test-perm '(1 2 3 4) 0)
(test-perm '(1 2 3 4) 1)
(test-perm '(1 2 3 4) 2)
(test-perm '(1 2 3 4) 3)
(test-perm '(1 2 3 4) 4)
(test-perm '(1 2 3 4) 5)
(test-perm '(1 2 3 4 5) 0)
(test-perm '(1 2 3 4 5) 1)
(test-perm '(1 2 3 4 5) 2)
(test-perm '(1 2 3 4 5) 3)
(test-perm '(1 2 3 4 5) 4)
(test-perm '(1 2 3 4 5) 5)
(test-perm '(1 2 3 4 5) 5)

(test-perm "" 0)
(test-perm "" 1)
(test-perm "1" 0)
(test-perm "1" 1)
(test-perm "1" 2)
(test-perm "12" 0)
(test-perm "12" 1)
(test-perm "12" 2)
(test-perm "12" 3)
(test-perm "123" 0)
(test-perm "123" 1)
(test-perm "123" 2)
(test-perm "123" 3)
(test-perm "123" 4)
(test-perm "1234" 0)
(test-perm "1234" 1)
(test-perm "1234" 2)
(test-perm "1234" 3)
(test-perm "1234" 4)
(test-perm "1234" 5)
(test-perm "12345" 0)
(test-perm "12345" 1)
(test-perm "12345" 2)
(test-perm "12345" 3)
(test-perm "12345" 4)
(test-perm "12345" 5)
(test-perm "12345" 5)

(mtest
  (perm #() -1) :error
  (perm #(1) -1) :error
  (perm () -1) :error
  (perm '(1) -1) :error
  (perm "" -1) :error
  (perm "a" -1) :error)

(test-rperm #() 0)
(test-rperm #() 1)
(test-rperm #(1) 0)
(test-rperm #(1) 1)
(test-rperm #(1) 2)
(test-rperm #(1 2) 0)
(test-rperm #(1 2) 1)
(test-rperm #(1 2) 2)
(test-rperm #(1 2) 3)
(test-rperm #(1 2 3) 0)
(test-rperm #(1 2 3) 1)
(test-rperm #(1 2 3) 2)
(test-rperm #(1 2 3) 3)
(test-rperm #(1 2 3) 4)
(test-rperm #(1 2 3 4) 0)
(test-rperm #(1 2 3 4) 1)
(test-rperm #(1 2 3 4) 2)
(test-rperm #(1 2 3 4) 3)
(test-rperm #(1 2 3 4) 4)
(test-rperm #(1 2 3 4) 5)
(test-rperm #(1 2 3 4 5) 0)
(test-rperm #(1 2 3 4 5) 1)
(test-rperm #(1 2 3 4 5) 2)
(test-rperm #(1 2 3 4 5) 3)
(test-rperm #(1 2 3 4 5) 4)
(test-rperm #(1 2 3 4 5) 5)
(test-rperm #(1 2 3 4 5) 5)

(test-rperm '() 0)
(test-rperm '() 1)
(test-rperm '(1) 0)
(test-rperm '(1) 1)
(test-rperm '(1) 2)
(test-rperm '(1 2) 0)
(test-rperm '(1 2) 1)
(test-rperm '(1 2) 2)
(test-rperm '(1 2) 3)
(test-rperm '(1 2 3) 0)
(test-rperm '(1 2 3) 1)
(test-rperm '(1 2 3) 2)
(test-rperm '(1 2 3) 3)
(test-rperm '(1 2 3) 4)
(test-rperm '(1 2 3 4) 0)
(test-rperm '(1 2 3 4) 1)
(test-rperm '(1 2 3 4) 2)
(test-rperm '(1 2 3 4) 3)
(test-rperm '(1 2 3 4) 4)
(test-rperm '(1 2 3 4) 5)
(test-rperm '(1 2 3 4 5) 0)
(test-rperm '(1 2 3 4 5) 1)
(test-rperm '(1 2 3 4 5) 2)
(test-rperm '(1 2 3 4 5) 3)
(test-rperm '(1 2 3 4 5) 4)
(test-rperm '(1 2 3 4 5) 5)
(test-rperm '(1 2 3 4 5) 5)

(test-rperm "" 0)
(test-rperm "" 1)
(test-rperm "1" 0)
(test-rperm "1" 1)
(test-rperm "1" 2)
(test-rperm "12" 0)
(test-rperm "12" 1)
(test-rperm "12" 2)
(test-rperm "12" 3)
(test-rperm "123" 0)
(test-rperm "123" 1)
(test-rperm "123" 2)
(test-rperm "123" 3)
(test-rperm "123" 4)
(test-rperm "1234" 0)
(test-rperm "1234" 1)
(test-rperm "1234" 2)
(test-rperm "1234" 3)
(test-rperm "1234" 4)
(test-rperm "1234" 5)
(test-rperm "12345" 0)
(test-rperm "12345" 1)
(test-rperm "12345" 2)
(test-rperm "12345" 3)
(test-rperm "12345" 4)
(test-rperm "12345" 5)
(test-rperm "12345" 5)

(mtest
  (rperm #() -1) :error
  (rperm #(1) -1) :error
  (rperm () -1) :error
  (rperm '(1) -1) :error
  (rperm "" -1) :error
  (rperm "a" -1) :error)

(mtest
  (comb "a".."c" 2) (("a" "b") ("a" "c") ("b" "c"))
  (rcomb "a".."c" 2) (("a" "a") ("a" "b") ("a" "c")
                      ("b" "b") ("b" "c") ("c" "c"))
  (perm "a".."c" 2) (("a" "b") ("a" "c") ("b" "a")
                     ("b" "c") ("c" "a") ("c" "b"))
  (rperm "a".."c" 2) (("a" "a") ("a" "b") ("a" "c")
                      ("b" "a") ("b" "b") ("b" "c")
                      ("c" "a") ("c" "b") ("c" "c")))

(mtest
  (perm '(1 2 3) 0) (nil)
  (rperm '(1 2 3) 0) (nil)
  (comb '(1 2 3) 0) (nil)
  (rcomb '(1 2 3) 0) (nil)
  (perm 1..4 0) (nil)
  (rperm 1..4 0) (nil)
  (comb 1..4 0) (nil)
  (rcomb 1..4 0) (nil))

(mtest
  (combi #() -1) :error
  (combi #(1) -1) :error
  (combi () -1) :error
  (combi '(1) -1) :error
  (combi "" -1) :error
  (combi "a" -1) :error)

(mtest
  (rcombi #() -1) :error
  (rcombi #(1) -1) :error
  (rcombi () -1) :error
  (rcombi '(1) -1) :error
  (rcombi "" -1) :error
  (rcombi "a" -1) :error)

(mtest
  (permi #() -1) :error
  (permi #(1) -1) :error
  (permi () -1) :error
  (permi '(1) -1) :error
  (permi "" -1) :error
  (permi "a" -1) :error)

(mtest
  (rpermi #() -1) :error
  (rpermi #(1) -1) :error
  (rpermi () -1) :error
  (rpermi '(1) -1) :error
  (rpermi "" -1) :error
  (rpermi "a" -1) :error)

(let ((s "abcdef")
      (v #(0 1 2 3 4))
      (l '(0 1 2 3 4)))
  (each ((fn [list comb rcomb perm rperm])
         (fi [list combi rcombi permi rpermi]))
    (each ((i 0..6))
      (each ((o [list s v l]))
        (vtest (list-seq [fi o i]) [fn o i])))))

(each ((fi [list combi rcombi permi rpermi]))
  (let* ((i0 [fi '(0 1 2 3 4) 1])
         (i1 (copy-iter i0))
         (i2 (iter-step (copy-iter i1)))
         (l0 (list-seq i0))
         (l1 (list-seq i1))
         (l2 (list-seq i2)))
   (mtest
     (equal l0 l1) t
     (equal l1 l2) nil
     (equal (cdr l1) l2) t)))
