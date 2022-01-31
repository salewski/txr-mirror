;; Copyright 2016-2022
;; Kaz Kylheku <kaz@kylheku.com>
;; Vancouver, Canada
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;
;; 1. Redistributions of source code must retain the above copyright notice,
;;    this list of conditions and the following disclaimer.
;;
;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;    this list of conditions and the following disclaimer in the documentation
;;    and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

(defstruct list-builder ()
  head tail

  (:postinit (bc)
    (set bc.head (cons nil bc.head)
         bc.tail bc.head))

  (:method add (self . items)
    (let ((st self.tail))
      (rplacd st (append (cdr st) nil))
      (let ((tl (last st)))
        (usr:rplacd tl (append (cdr tl) items))
        (set self.tail tl)))
    self)

  (:method add* (self . items)
    (let ((h self.head))
      (usr:rplacd h (append items (cdr h))))
    self)

  (:method pend (self . lists)
    (when lists
      (let ((st self.tail))
        (rplacd st (append (cdr st) nil))
        (let* ((tl (last st))
               (cp (tailp tl (car (last lists))))
               (nl [apply append lists]))
          (usr:rplacd tl (append (cdr tl) (if cp (copy-list nl) nl)))
          (set self.tail tl)))
      self))

  (:method pend* (self . lists)
    (let* ((h self.head)
           (pf [apply append (append lists (list (cdr h)))]))
      (usr:rplacd h pf)
      (set self.tail h))
    self)

  (:method ncon (self . lists)
    (when lists
      (let* ((tl (last self.tail))
             (nl [apply nconc lists]))
        (usr:rplacd tl (nconc (cdr tl) nl))
        (set self.tail tl))
      self))

  (:method ncon* (self . lists)
    (let* ((h self.head)
           (pf [apply nconc (append lists (list (cdr h)))]))
      (usr:rplacd h pf)
      (if (eq self.tail h)
        (set self.tail pf)))
    self)

  (:method get (self)
    (cdr self.head))

  (:method del (self)
    (whenlet ((hd self.head)
              (chd (cdr self.head)))
      (when (eq self.tail chd)
        (set self.tail hd))
      (prog1 (car chd) (usr:rplacd hd (cdr chd)))))

  (:method del* (self)
    (whenlet ((hd self.head)
              (chd (cdr self.head)))
      (if (cdr chd)
        (let* ((tl self.tail)
               (l2 (nthlast 2 tl)))
          (if (cdr l2)
            (prog1
              (cadr l2)
              (usr:rplacd l2 nil))
            (let* ((l10 (nthlast 10 hd))
                   (l2 (nthlast 2 l10)))
              (prog1
                (cadr l2)
                (usr:rplacd l2 nil)
                (set self.tail l10)))))
        (prog1
          (car chd)
          (usr:rplacd hd nil)
          (set self.tail hd))))))

(defun sys:list-builder-flets (lb-form)
  (nconc
    (collect-each ((op '(add add* pend pend* ncon ncon*)))
      ^(,op (. args)
         (qref ,lb-form (,op . args))
         nil))
    ^((get ()
        (qref ,lb-form (get)))
      (del* ()
        (qref ,lb-form (del*)))
      (do-del ()
        (qref ,lb-form (del))))))

(defun build-list (: init)
  (new list-builder head init))

(defun sys:build-expander (forms return-get)
  (with-gensyms (name)
    ^(let ((,name (new list-builder)))
       (flet ,(sys:list-builder-flets name)
         (macrolet ((del (:form f : (expr nil expr-p))
                      (if expr-p f '(do-del))))
           ,*forms
           ,*(if return-get ^((qref ,name (get)))))))))

(defmacro build (. forms)
  (sys:build-expander forms t))

(defmacro buildn (. forms)
  (sys:build-expander forms nil))
