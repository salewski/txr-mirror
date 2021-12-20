(load "../common")

(mtest
  (less #() #(a)) t
  (greater #() #(a)) nil
  (less #(0) #(1)) t
  (greater #(0) #(1)) nil
  (less #(1) #(0)) nil
  (greater #(1) #(0)) t
  (less #(0) #(0 0)) t
  (less #(1) #(0 0)) nil
  (less #(0 0) #(0 1)) t
  (less #(0 0) #(0 0)) nil
  (less #(0 0) #(0 0 0)) t)

(mtest
  (less '() #()) t
  (less '(0) #(0)) t
  (less "a" #(#\a)) t
  (less #() #b'') t
  (less #(0) #b'00') t)
