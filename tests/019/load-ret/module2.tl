(eval ^(defvar ,(pop *load-args*)))
(return-from load [apply + *load-args*])
