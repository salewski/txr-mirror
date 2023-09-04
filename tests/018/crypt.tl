(load "../common")

(if (meq (os-symbol) :android :cygwin)
  (exit))

(mtest
  (crypt nil nil) :error
  (crypt "a" "bc") "bcshMw5X24ayQ"
  (crypt "a" "bcd") "bcshMw5X24ayQ")

(if (eq :linux (os-symbol))
  (mtest
    (crypt "a" "b") :error
    (crypt "a" "::") :error
    (crypt "a" "$1$") "$1$$Ij31LCAysPM23KuPlm1wA/"
    (crypt "a" "$1$bcd$") "$1$bcd$cgz778Ks3pkbWfyW.CWae/"
    (crypt "a" "$5$") "$5$$QG6CCM7eJAxpUPcBpn0Z2K29NHtaI6Mk1fCpPrpjdj3"
    (crypt "a" "$5$bcd$") "$5$bcd$OGt98FNCHtKIrT6qWAKLXOQ8eIApFT5dJngrYreMwF3"
    (crypt "a" "$6$") "$6$$ek/ucQg0IM8SQLyD2D66mpoW0vAF26eA0/pqoN95V.F0nZh1IFuENNo0OikacRkDBk5frNqziMYMdVVrQ0o.51"
    (crypt "a" "$6$bcd$") "$6$bcd$RK8RFj8wSE1NBJi8s.KDjGQK3EbpI474a6f4UP0LGOkQU50ZQrwykBaSjx7tZFVEpanpL44zd1p6A9q.sy.YH0"))
