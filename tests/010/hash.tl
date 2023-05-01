(load "../common")

(mtest
  (uni #H(() ("a") ("b")) #H(() ("b") ("c"))) (("a") ("b") ("c"))
  (diff #H(() ("a") ("b")) #H(() ("b") ("c"))) (("a"))
  (isec #H(() ("a") ("b")) #H(() ("b") ("c"))) (("b")))

(mtest
  [group-by identity '(1 1 2 2 3 3 3)] #H(() (1 (1 1)) (2 (2 2)) (3 (3 3 3)))
  (group-by (op mod @1 3) (range 0 10)) #H(() (0 (0 3 6 9))
                                              (1 (1 4 7 10))
                                              (2 (2 5 8)))
  [group-map (op mod @1 3) sum (range 0 10)] #H(() (0 18) (1 22) (2 15)))

(mtest
  [group-reduce (hash) identity (do inc @1)
                "fourscoreandsevenyearsago" 0] #H(() (#\a 3) (#\c 1) (#\d 1)
                                                     (#\e 4) (#\f 1) (#\g 1)
                                                     (#\n 2) (#\o 3) (#\r 3)
                                                     (#\s 3) (#\u 1) (#\v 1)
                                                     (#\y 1))
  [group-reduce (hash) evenp + (range 1 10) 0] #H(() (t 30) (nil 25)))

(mtest
  (hash-props) #H(())
  (hash-props 1 2) #H(() (1 2))
  (hash-props 1 2 'a 'b) #H(() (1 2) (a b))
  (hash-props 1) :error
  (hash-props 1 2 'a) :error)
