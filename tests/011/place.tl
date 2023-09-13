(load "../common")

(defvar h (hash))

(mtest
  (let ((x 0)) (ensure (gethash h (pinc x)) "a") x) 1
  [h 0] "a"
  (let ((x 0)) (ensure (gethash h 0) (pinc x)) x) 0
  [h 0] "a")
