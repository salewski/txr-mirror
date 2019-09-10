(load "../common")

(test (append "abc" "d") "abcd")
(test (append "abc" #(#\d)) "abcd")
(test (append '(1 2 . "abc") #(#\d)) (1 2 . "abcd"))
(test (append 3 4) :error)
(test (append '(1) 2) (1 . 2))
(test (append '(1 . 2) 2) :error)
(test (append '(1 . #(3 4 5)) "d") (1 . #(3 4 5 #\d)))

(test (build (add 1) (add 2) (pend (get))) (1 2 1 2))
(test (build (add 1) (add 2) (pend* (get))) (1 2 1 2))
(test (build (add 1) (add 2) (pend (get) (get))) (1 2 1 2 1 2))
(test (build (add 1) (add 2) (pend* (get) (get))) (1 2 1 2 1 2))

(set *print-circle* t)

(stest (build (add 1) (add 2) (ncon (get))) "#1=(1 2 . #1#)")
(stest (build (add 1) (add 2) (ncon* (get))) "#1=(1 2 . #1#)")
