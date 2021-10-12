(load "../common")

(defpackage fb-2
  (:local loc-0 loc-1 loc-2)
  (:fallback usr))

(defpackage fb-1
  (:local loc-0 loc-1)
  (:fallback fb-2 usr))

(defpackage main
  (:local loc-0)
  (:fallback fb-1 fb-2 usr))

(in-package fb-2)

(prinl '(loc-0 loc-1 loc-2 cons))

(in-package fb-1)

(prinl '(loc-0 loc-1 loc-2 cons))
(prinl '(fb-2:loc-0 fb-2:loc-1 fb-2:loc-2 fb-2:cons))

(in-package main)

(prinl '(loc-0 loc-1 loc-2 cons))
(prinl '(fb-2:loc-0 fb-2:loc-1 fb-2:loc-2 fb-2:cons))
(prinl '(fb-1:loc-0 fb-1:loc-1 fb-1:loc-2 fb-1:cons))
