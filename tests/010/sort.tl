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
       (slist (shuffle list)))
  (mvtest
    (sort list) list
    (sort slist) list
    (sort list (fun greater)) (reverse list)
    (sort slist (fun greater)) (reverse list)))
