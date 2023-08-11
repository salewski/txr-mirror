(load "../common")

(defpackage lottery
  (:local draw)
  (:fallback usr))

(defpackage graphics
  (:local draw)
  (:fallback usr))

(defpackage gui-lottery
  (:fallback lottery graphics usr pub)
  (:use-syms-as lottery:draw ldraw
                graphics:draw gdraw))

(in-package gui-lottery)

(mtest
  (package-name (symbol-package 'ldraw)) "lottery"
  (package-name (symbol-package 'gdraw)) "graphics"
  (symbol-name 'ldraw) "draw"
  (symbol-name 'gdraw) "draw")

(mtest
  (tostring 'ldraw) "draw"
  (tostring 'gdraw) "graphics:draw")

(mtest
  (use-sym-as 3 '#:foo) :error
  (use-sym-as 'ldraw 3) :error
  (use-sym-as 'x 'x) x)
