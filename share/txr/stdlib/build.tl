;; Copyright 2016
;; Kaz Kylheku <kaz@kylheku.com>
;; Vancouver, Canada
;; All rights reserved.
;;
;; Redistribution of this software in source and binary forms, with or without
;; modification, is permitted provided that the following two conditions are met.
;;
;; Use of this software in any manner constitutes agreement with the disclaimer
;; which follows the two conditions.
;;
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in
;;    the documentation and/or other materials provided with the
;;    distribution.
;;
;; THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
;; WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  IN NO EVENT SHALL THE
;; COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DAMAGES, HOWEVER CAUSED,
;; AND UNDER ANY THEORY OF LIABILITY, ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(defstruct list-builder ()
  head tail

  (:postinit (bc)
    (set bc.head (cons nil bc.head)
         bc.tail (last bc.head)))

  (:method add (self . items)
    (set self.tail (last (rplacd self.tail (copy items)))))

  (:method add* (self . items)
    (let ((ic (copy items))
          (h self.head))
      (rplacd (last ic) (cdr h))
      (rplacd h ic)))

  (:method pend (self . lists)
    (while lists
      (set self.tail (last (rplacd self.tail (copy (car lists)))))
      (set lists (cdr lists))))

  (:method pend* (self . lists)
    (let* ((h self.head)
           (nh (cons nil nil))
           (tl nh))
      (while lists
        (set tl (last (rplacd tl (copy (car lists)))))
        (set lists (cdr lists)))
      (rplacd tl (cdr h))
      (set self.head nh)))

  (:method ncon (self . lists)
    (set self.tail (last (rplacd self.tail (nconc . lists)))))

  (:method ncon* (self . lists)
    (let ((h self.head))
      (set (cdr h) (nconc (nconc . lists) (cdr h)))
      (if (eq self.tail h)
        (set self.tail (last h)))))

  (:method get (self)
    (cdr self.head)))

(defun sys:list-builder-macrolets (lb-form)
  (nconc
    (collect-each ((op '(add add* pend pend* ncon ncon*)))
      ^(,op (. forms)
         ^(qref ,',lb-form (,',op ,*forms))))
    ^((get ()
        ^(qref ,',lb-form (get))))))

(defun build-list (: init)
  (new list-builder head init))

(defmacro build (. forms)
  (with-gensyms (name)
    ^(let ((,name (new list-builder)))
       (macrolet ,(sys:list-builder-macrolets name)
         ,*forms
         (qref ,name (get))))))
