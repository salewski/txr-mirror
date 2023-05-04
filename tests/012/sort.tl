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
    (sort svec (fun greater)) (reverse vec)))

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
    (ssort svec (fun greater)) (reverse vec)))
