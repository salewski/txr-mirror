(load "../common")

(test (sort ()) nil)

(let* ((list (conses '(1 2 3 4 5 6 7 8)))
       (sp (uniq [mapcar sort (perm list (len list))])))
  (mvtest (len sp) 1
          (car sp) list))

(test (sort #()) #())

(let* ((vec (conses #(1 2 3 4 5 6 7 8)))
       (sp (uniq [mapcar sort (perm vec (len vec))])))
  (mvtest (len sp) 1
          (car sp) vec))

(let* ((list (range* 0 1000))
       (slist (shuffle list))
       (vec (vec-list list))
       (svec (vec-list slist)))
  (mvtest
    (sort list) list
    (sort slist) list
    (sort list (fun greater)) (reverse list)
    (sort slist (fun greater)) (reverse list))
  (mvtest
    (sort vec) vec
    (sort svec) vec
    (sort vec (fun greater)) (reverse vec)
    (sort svec (fun greater)) (reverse vec))
  (mvtest
    (csort list) list
    (csort slist) list
    (csort list (fun greater)) (reverse list)
    (csort slist (fun greater)) (reverse list))
  (mvtest
    (csort vec) vec
    (csort svec) vec
    (csort vec (fun greater)) (reverse vec)
    (csort svec (fun greater)) (reverse vec)))


(test (ssort ()) nil)

(let* ((list (conses '(1 2 3 4 5 6 7 8)))
       (sp (uniq [mapcar ssort (perm list (len list))])))
  (mvtest (len sp) 1
          (car sp) list))

(test (ssort #()) #())

(let* ((vec (conses #(1 2 3 4 5 6 7 8)))
       (sp (uniq [mapcar ssort (perm vec (len vec))])))
  (mvtest (len sp) 1
          (car sp) vec))

(let* ((list (range* 0 1000))
       (slist (shuffle list))
       (vec (vec-list list))
       (svec (vec-list slist)))
  (mvtest
    (ssort list) list
    (ssort slist) list
    (ssort list (fun greater)) (reverse list)
    (ssort slist (fun greater)) (reverse list))
  (mvtest
    (ssort vec) vec
    (ssort svec) vec
    (ssort vec (fun greater)) (reverse vec)
    (ssort svec (fun greater)) (reverse vec))
  (mvtest
    (cssort list) list
    (cssort slist) list
    (cssort list (fun greater)) (reverse list)
    (cssort slist (fun greater)) (reverse list))
  (mvtest
    (cssort vec) vec
    (cssort svec) vec
    (cssort vec (fun greater)) (reverse vec)
    (cssort svec (fun greater)) (reverse vec)))

(mtest
  [sort-group '((a 1) (b 1) (a 2) (b 2) (a 3) (c 2) (c 1) (a 4)) car]
  (((a 1) (a 2) (a 3) (a 4))
   ((b 1) (b 2))
   ((c 2) (c 1)))
  [csort-group '((a 1) (b 1) (a 2) (b 2) (a 3) (c 2) (c 1) (a 4)) car]
  (((a 1) (a 2) (a 3) (a 4))
   ((b 1) (b 2))
   ((c 2) (c 1))))
