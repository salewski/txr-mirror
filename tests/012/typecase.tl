(load "../common")

(mtest
  (typecase) :error
  (typecase nil) nil
  (typecase nil a) :error
  (typecase 0 (symbol 1)) nil
  (typecase 0 (integer 1)) 1
  (typecase 0 (integer 1) (integer 2)) 1
  (typecase 0 (t 3) (integer 1)) 3)

(mtest
  (etypecase) :error
  (etypecase nil) :error
  (etypecase nil a) :error
  (etypecase 0 (string 1)) :error
  (etypecase 0 (string 1) (integer 2)) 2
  (etypecase 0 (string 1) (t 2)) 2)
