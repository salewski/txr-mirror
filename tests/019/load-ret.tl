(load "../common")

(mtest
  (load "load-ret/module") 0
  (load "load-ret/module" 1) 1
  (load "load-ret/module" 1 2 3) 6)

(mtest
  (load-for (var abc "load-ret/module2" 'abc)) 0 
  (load-for (var def "load-ret/module2" 'def 1 2 3)) 6
  (load-for (var abc "load-ret/module2" 'abc)) nil
  (load-for (var abc "load-ret/module2" 'abc)
            (var ghi "load-ret/module2" 'ghi 2 3 4)) 9)

(defvarl here (dir-name self-path))

(mtest
  (sh `@{txr-exe-path} @here/load-ret/script.tl 0`) 0
  (sh `@{txr-exe-path} @here/load-ret/script.tl 1`) 1
  (sh `@{txr-exe-path} @here/load-ret/script.tl 7`) 7)

(test
  (sh `@{txr-exe-path} @here/load-ret/bad.tl 1 2> /dev/null`) 1)
