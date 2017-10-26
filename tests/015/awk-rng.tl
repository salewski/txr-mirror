(awk (:inputs '#"X1 a b X1 c X2 1 2 3 Y1 r s Y2 t Y2"
              '#"X1X2 Y1Y2"
              '#"X1X2 Y1 a Y2 X1 b X2")
     (:set ofs "\t")
     (:begin
       (prn 'rec 'rng '-rng 'rng- '-rng- '--rng- '--rng 'rng+ '-rng+ '--rng+ 'Y1Y2))
     (t (prn rec
             (rng (rng #/X1/ #/X2/) (rng #/Y1/ #/Y2/))
             (-rng (rng #/X1/ #/X2/) (rng #/Y1/ #/Y2/))
             (rng- (rng #/X1/ #/X2/) (rng #/Y1/ #/Y2/))
             (-rng- (rng #/X1/ #/X2/) (rng #/Y1/ #/Y2/))
             (--rng- (rng #/X1/ #/X2/) (rng #/Y1/ #/Y2/))
             (--rng (rng #/X1/ #/X2/) (rng #/Y1/ #/Y2/))
             (rng+ (rng #/X1/ #/X2/) (rng #/Y1/ #/Y2/))
             (-rng+ (rng #/X1/ #/X2/) (rng #/Y1/ #/Y2/))
             (--rng+ (rng #/X1/ #/X2/) (rng #/Y1/ #/Y2/))
             (rng #/Y1/ #/Y2/))))

