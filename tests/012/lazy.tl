(load "../common")

(test [mapcar* list nil] nil)
(test [mapcar* list '(1)] ((1)))
(test [mapcar* list '(1 2 3)] ((1) (2) (3)))
(test [mapcar* list '(1 2 3) '(a b c)] ((1 a) (2 b) (3 c)))
