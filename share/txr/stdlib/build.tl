;; Copyright 2016-2019
;; Kaz Kylheku <kaz@kylheku.com>
;; Vancouver, Canada
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;
;; 1. Redistributions of source code must retain the above copyright notice, this
;;    list of conditions and the following disclaimer.
;;
;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;    this list of conditions and the following disclaimer in the documentation
;;    and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
;; OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(defstruct list-builder ()
  head tail

  (:postinit (bc)
    (set bc.head (cons nil bc.head)
         bc.tail bc.head))

  (:method add (self . items)
    (let ((tl self.tail))
      (usr:rplacd tl (copy (cdr tl)))
      (set tl (last tl))
      (usr:rplacd tl items)
      (set self.tail tl)))

  (:method add* (self . items)
    (let ((ic (copy items))
          (h self.head))
      (usr:rplacd (last ic) (cdr h))
      (usr:rplacd h ic)))

  (:method pend (self . lists)
    (let ((tl self.tail))
      (while lists
        (usr:rplacd tl (copy (cdr tl)))
        (set tl (last tl))
        (usr:rplacd tl (car lists))
        (set lists (cdr lists)))
      (set self.tail tl)))

  (:method pend* (self . lists)
    (let* ((h self.head)
           (nh (cons nil nil))
           (tl nh))
      (while lists
        (usr:rplacd tl (copy tl))
        (set tl (last tl))
        (usr:rplacd tl (car lists))
        (set lists (cdr lists)))

      (set tl (last tl))
      (usr:rplacd tl (cdr h))
      (set self.head nh)))

  (:method ncon (self . lists)
    (let ((tl self.tail))
      (while lists
        (set tl (last tl))
        (usr:rplacd tl (car lists))
        (set lists (cdr lists)))
      (set self.tail tl)))

  (:method ncon* (self . lists)
    (let* ((h self.head)
           (pf (nconc (nconc . lists) (cdr h))))
      (usr:rplacd h pf)
      (if (eq self.tail h)
        (set self.tail pf))))

  (:method get (self)
    (cdr self.head)))

(defun sys:list-builder-flets (lb-form)
  (nconc
    (collect-each ((op '(add add* pend pend* ncon ncon*)))
      ^(,op (. args)
         (qref ,lb-form (,op . args))))
    ^((get ()
        (qref ,lb-form (get))))))

(defun build-list (: init)
  (new list-builder head init))

(defmacro build (. forms)
  (with-gensyms (name)
    ^(let ((,name (new list-builder)))
       (flet ,(sys:list-builder-flets name)
         ,*forms
         (qref ,name (get))))))
