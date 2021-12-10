(load "../common")

(defset foo (:key x y -- a b c (d 4)) n ^(bar ,x ,y, a, b, c ,d ,n))

;; obtain identity of new-val gensym: this is baked into defset
(defvarl %new-val-sym% (caar (last (cadr (expand '(inc (foo 1 2)))))))

(test
  (and (symbolp %new-val-sym%)
       (null (symbol-package %new-val-sym%))
       (starts-with "g" (symbol-name %new-val-sym%)))
  t)

(test
  (expand '(set (foo 1 2 :a 3 :b 4) 5))
  (bar 1 2 3 4 nil 4 5))

(vtest
  (expand '(inc (foo 1 2 :a 3 :b 4) 5))
  ^(let ((,%new-val-sym% (+ (foo 1 2 :a 3 :b 4) 5)))
    (bar 1 2 3 4 () 4 ,%new-val-sym%)))

(defvarl %data% (vec 0 0 0 0 0 0 0 0 0 1))
(defun getd (a b c) [%data% (+ a b c)])
(defun setd (a b c v) (set [%data% (+ a b c)] v))
(define-accessor getd setd)

(mtest
  (getd 2 3 4) 1
  (set (getd 2 3 4) 2) 2
  [%data% 9] 2
  (inc (getd 2 3 4) 3) 5
  [%data% 9] 5)
