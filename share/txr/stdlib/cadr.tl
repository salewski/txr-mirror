;; Copyright 2015
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

(defplace (caar cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(rlet ((,cell-sym (car ,cell)))
         (macrolet ((,getter () ^(car ,',cell-sym))
                    (,setter (val) ^(sys:rplaca ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplaca (car ,',cell) ,val)))
      ,body))
  (deleter
    ^(macrolet ((,deleter ()
                  (with-gensyms (tmp)
                    (with-update-expander (cgetter csetter) '(car ,cell) nil
                      ^(let ((,tmp (,cgetter)))
                         (prog1 (car ,tmp) (,csetter (cdr ,tmp))))))))
       ,body)))

(defplace (cadr cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(rlet ((,cell-sym (cdr ,cell)))
         (macrolet ((,getter () ^(car ,',cell-sym))
                    (,setter (val) ^(sys:rplaca ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplaca (cdr ,',cell) ,val)))
      ,body))
  (deleter
    ^(macrolet ((,deleter ()
                  (with-gensyms (tmp)
                    (with-update-expander (cgetter csetter) '(cdr ,cell) nil
                      ^(let ((,tmp (,cgetter)))
                         (prog1 (car ,tmp) (,csetter (cdr ,tmp))))))))
       ,body)))

(defplace (cdar cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(rlet ((,cell-sym (car ,cell)))
         (macrolet ((,getter () ^(cdr ,',cell-sym))
                    (,setter (val) ^(sys:rplacd ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplacd (car ,',cell) ,val)))
      ,body))
  (deleter
    ^(macrolet ((,deleter ()
                  (with-gensyms (tmp)
                    (with-update-expander (cgetter csetter) '(car ,cell) nil
                      ^(let ((,tmp (,cgetter)))
                         (prog1 (cdr ,tmp) (,csetter (car ,tmp))))))))
       ,body)))

(defplace (cddr cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(rlet ((,cell-sym (cdr ,cell)))
         (macrolet ((,getter () ^(cdr ,',cell-sym))
                    (,setter (val) ^(sys:rplacd ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplacd (cdr ,',cell) ,val)))
      ,body))
  (deleter
    ^(macrolet ((,deleter ()
                  (with-gensyms (tmp)
                    (with-update-expander (cgetter csetter) '(cdr ,cell) nil
                      ^(let ((,tmp (,cgetter)))
                         (prog1 (cdr ,tmp) (,csetter (car ,tmp))))))))
       ,body)))

(defplace (caaar cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(rlet ((,cell-sym (caar ,cell)))
         (macrolet ((,getter () ^(car ,',cell-sym))
                    (,setter (val) ^(sys:rplaca ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplaca (caar ,',cell) ,val)))
      ,body))
  (deleter
    ^(macrolet ((,deleter ()
                  (with-gensyms (tmp)
                    (with-update-expander (cgetter csetter) '(caar ,cell) nil
                      ^(let ((,tmp (,cgetter)))
                         (prog1 (car ,tmp) (,csetter (cdr ,tmp))))))))
       ,body)))

(defplace (caadr cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(rlet ((,cell-sym (cadr ,cell)))
         (macrolet ((,getter () ^(car ,',cell-sym))
                    (,setter (val) ^(sys:rplaca ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplaca (cadr ,',cell) ,val)))
      ,body))
  (deleter
    ^(macrolet ((,deleter ()
                  (with-gensyms (tmp)
                    (with-update-expander (cgetter csetter) '(cadr ,cell) nil
                      ^(let ((,tmp (,cgetter)))
                         (prog1 (car ,tmp) (,csetter (cdr ,tmp))))))))
       ,body)))

(defplace (cadar cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(rlet ((,cell-sym (cdar ,cell)))
         (macrolet ((,getter () ^(car ,',cell-sym))
                    (,setter (val) ^(sys:rplaca ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplaca (cdar ,',cell) ,val)))
      ,body))
  (deleter
    ^(macrolet ((,deleter ()
                  (with-gensyms (tmp)
                    (with-update-expander (cgetter csetter) '(cdar ,cell) nil
                      ^(let ((,tmp (,cgetter)))
                         (prog1 (car ,tmp) (,csetter (cdr ,tmp))))))))
       ,body)))

(defplace (caddr cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(rlet ((,cell-sym (cddr ,cell)))
         (macrolet ((,getter () ^(car ,',cell-sym))
                    (,setter (val) ^(sys:rplaca ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplaca (cddr ,',cell) ,val)))
      ,body))
  (deleter
    ^(macrolet ((,deleter ()
                  (with-gensyms (tmp)
                    (with-update-expander (cgetter csetter) '(cddr ,cell) nil
                      ^(let ((,tmp (,cgetter)))
                         (prog1 (car ,tmp) (,csetter (cdr ,tmp))))))))
       ,body)))

(defplace (cdaar cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(rlet ((,cell-sym (caar ,cell)))
         (macrolet ((,getter () ^(cdr ,',cell-sym))
                    (,setter (val) ^(sys:rplacd ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplacd (caar ,',cell) ,val)))
      ,body))
  (deleter
    ^(macrolet ((,deleter ()
                  (with-gensyms (tmp)
                    (with-update-expander (cgetter csetter) '(caar ,cell) nil
                      ^(let ((,tmp (,cgetter)))
                         (prog1 (cdr ,tmp) (,csetter (car ,tmp))))))))
       ,body)))

(defplace (cdadr cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(rlet ((,cell-sym (cadr ,cell)))
         (macrolet ((,getter () ^(cdr ,',cell-sym))
                    (,setter (val) ^(sys:rplacd ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplacd (cadr ,',cell) ,val)))
      ,body))
  (deleter
    ^(macrolet ((,deleter ()
                  (with-gensyms (tmp)
                    (with-update-expander (cgetter csetter) '(cadr ,cell) nil
                      ^(let ((,tmp (,cgetter)))
                         (prog1 (cdr ,tmp) (,csetter (car ,tmp))))))))
       ,body)))

(defplace (cddar cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(rlet ((,cell-sym (cdar ,cell)))
         (macrolet ((,getter () ^(cdr ,',cell-sym))
                    (,setter (val) ^(sys:rplacd ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplacd (cdar ,',cell) ,val)))
      ,body))
  (deleter
    ^(macrolet ((,deleter ()
                  (with-gensyms (tmp)
                    (with-update-expander (cgetter csetter) '(cdar ,cell) nil
                      ^(let ((,tmp (,cgetter)))
                         (prog1 (cdr ,tmp) (,csetter (car ,tmp))))))))
       ,body)))

(defplace (cdddr cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(rlet ((,cell-sym (cddr ,cell)))
         (macrolet ((,getter () ^(cdr ,',cell-sym))
                    (,setter (val) ^(sys:rplacd ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplacd (cddr ,',cell) ,val)))
      ,body))
  (deleter
    ^(macrolet ((,deleter ()
                  (with-gensyms (tmp)
                    (with-update-expander (cgetter csetter) '(cddr ,cell) nil
                      ^(let ((,tmp (,cgetter)))
                         (prog1 (cdr ,tmp) (,csetter (car ,tmp))))))))
       ,body)))

(defplace (caaaar cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(rlet ((,cell-sym (caaar ,cell)))
         (macrolet ((,getter () ^(car ,',cell-sym))
                    (,setter (val) ^(sys:rplaca ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplaca (caaar ,',cell) ,val)))
      ,body))
  (deleter
    ^(macrolet ((,deleter ()
                  (with-gensyms (tmp)
                    (with-update-expander (cgetter csetter) '(caaar ,cell) nil
                      ^(let ((,tmp (,cgetter)))
                         (prog1 (car ,tmp) (,csetter (cdr ,tmp))))))))
       ,body)))

(defplace (caaadr cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(rlet ((,cell-sym (caadr ,cell)))
         (macrolet ((,getter () ^(car ,',cell-sym))
                    (,setter (val) ^(sys:rplaca ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplaca (caadr ,',cell) ,val)))
      ,body))
  (deleter
    ^(macrolet ((,deleter ()
                  (with-gensyms (tmp)
                    (with-update-expander (cgetter csetter) '(caadr ,cell) nil
                      ^(let ((,tmp (,cgetter)))
                         (prog1 (car ,tmp) (,csetter (cdr ,tmp))))))))
       ,body)))

(defplace (caadar cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(rlet ((,cell-sym (cadar ,cell)))
         (macrolet ((,getter () ^(car ,',cell-sym))
                    (,setter (val) ^(sys:rplaca ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplaca (cadar ,',cell) ,val)))
      ,body))
  (deleter
    ^(macrolet ((,deleter ()
                  (with-gensyms (tmp)
                    (with-update-expander (cgetter csetter) '(cadar ,cell) nil
                      ^(let ((,tmp (,cgetter)))
                         (prog1 (car ,tmp) (,csetter (cdr ,tmp))))))))
       ,body)))

(defplace (caaddr cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(rlet ((,cell-sym (caddr ,cell)))
         (macrolet ((,getter () ^(car ,',cell-sym))
                    (,setter (val) ^(sys:rplaca ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplaca (caddr ,',cell) ,val)))
      ,body))
  (deleter
    ^(macrolet ((,deleter ()
                  (with-gensyms (tmp)
                    (with-update-expander (cgetter csetter) '(caddr ,cell) nil
                      ^(let ((,tmp (,cgetter)))
                         (prog1 (car ,tmp) (,csetter (cdr ,tmp))))))))
       ,body)))

(defplace (cadaar cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(rlet ((,cell-sym (cdaar ,cell)))
         (macrolet ((,getter () ^(car ,',cell-sym))
                    (,setter (val) ^(sys:rplaca ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplaca (cdaar ,',cell) ,val)))
      ,body))
  (deleter
    ^(macrolet ((,deleter ()
                  (with-gensyms (tmp)
                    (with-update-expander (cgetter csetter) '(cdaar ,cell) nil
                      ^(let ((,tmp (,cgetter)))
                         (prog1 (car ,tmp) (,csetter (cdr ,tmp))))))))
       ,body)))

(defplace (cadadr cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(rlet ((,cell-sym (cdadr ,cell)))
         (macrolet ((,getter () ^(car ,',cell-sym))
                    (,setter (val) ^(sys:rplaca ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplaca (cdadr ,',cell) ,val)))
      ,body))
  (deleter
    ^(macrolet ((,deleter ()
                  (with-gensyms (tmp)
                    (with-update-expander (cgetter csetter) '(cdadr ,cell) nil
                      ^(let ((,tmp (,cgetter)))
                         (prog1 (car ,tmp) (,csetter (cdr ,tmp))))))))
       ,body)))

(defplace (caddar cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(rlet ((,cell-sym (cddar ,cell)))
         (macrolet ((,getter () ^(car ,',cell-sym))
                    (,setter (val) ^(sys:rplaca ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplaca (cddar ,',cell) ,val)))
      ,body))
  (deleter
    ^(macrolet ((,deleter ()
                  (with-gensyms (tmp)
                    (with-update-expander (cgetter csetter) '(cddar ,cell) nil
                      ^(let ((,tmp (,cgetter)))
                         (prog1 (car ,tmp) (,csetter (cdr ,tmp))))))))
       ,body)))

(defplace (cadddr cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(rlet ((,cell-sym (cdddr ,cell)))
         (macrolet ((,getter () ^(car ,',cell-sym))
                    (,setter (val) ^(sys:rplaca ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplaca (cdddr ,',cell) ,val)))
      ,body))
  (deleter
    ^(macrolet ((,deleter ()
                  (with-gensyms (tmp)
                    (with-update-expander (cgetter csetter) '(cdddr ,cell) nil
                      ^(let ((,tmp (,cgetter)))
                         (prog1 (car ,tmp) (,csetter (cdr ,tmp))))))))
       ,body)))

(defplace (cdaaar cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(rlet ((,cell-sym (caaar ,cell)))
         (macrolet ((,getter () ^(cdr ,',cell-sym))
                    (,setter (val) ^(sys:rplacd ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplacd (caaar ,',cell) ,val)))
      ,body))
  (deleter
    ^(macrolet ((,deleter ()
                  (with-gensyms (tmp)
                    (with-update-expander (cgetter csetter) '(caaar ,cell) nil
                      ^(let ((,tmp (,cgetter)))
                         (prog1 (cdr ,tmp) (,csetter (car ,tmp))))))))
       ,body)))

(defplace (cdaadr cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(rlet ((,cell-sym (caadr ,cell)))
         (macrolet ((,getter () ^(cdr ,',cell-sym))
                    (,setter (val) ^(sys:rplacd ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplacd (caadr ,',cell) ,val)))
      ,body))
  (deleter
    ^(macrolet ((,deleter ()
                  (with-gensyms (tmp)
                    (with-update-expander (cgetter csetter) '(caadr ,cell) nil
                      ^(let ((,tmp (,cgetter)))
                         (prog1 (cdr ,tmp) (,csetter (car ,tmp))))))))
       ,body)))

(defplace (cdadar cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(rlet ((,cell-sym (cadar ,cell)))
         (macrolet ((,getter () ^(cdr ,',cell-sym))
                    (,setter (val) ^(sys:rplacd ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplacd (cadar ,',cell) ,val)))
      ,body))
  (deleter
    ^(macrolet ((,deleter ()
                  (with-gensyms (tmp)
                    (with-update-expander (cgetter csetter) '(cadar ,cell) nil
                      ^(let ((,tmp (,cgetter)))
                         (prog1 (cdr ,tmp) (,csetter (car ,tmp))))))))
       ,body)))

(defplace (cdaddr cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(rlet ((,cell-sym (caddr ,cell)))
         (macrolet ((,getter () ^(cdr ,',cell-sym))
                    (,setter (val) ^(sys:rplacd ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplacd (caddr ,',cell) ,val)))
      ,body))
  (deleter
    ^(macrolet ((,deleter ()
                  (with-gensyms (tmp)
                    (with-update-expander (cgetter csetter) '(caddr ,cell) nil
                      ^(let ((,tmp (,cgetter)))
                         (prog1 (cdr ,tmp) (,csetter (car ,tmp))))))))
       ,body)))

(defplace (cddaar cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(rlet ((,cell-sym (cdaar ,cell)))
         (macrolet ((,getter () ^(cdr ,',cell-sym))
                    (,setter (val) ^(sys:rplacd ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplacd (cdaar ,',cell) ,val)))
      ,body))
  (deleter
    ^(macrolet ((,deleter ()
                  (with-gensyms (tmp)
                    (with-update-expander (cgetter csetter) '(cdaar ,cell) nil
                      ^(let ((,tmp (,cgetter)))
                         (prog1 (cdr ,tmp) (,csetter (car ,tmp))))))))
       ,body)))

(defplace (cddadr cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(rlet ((,cell-sym (cdadr ,cell)))
         (macrolet ((,getter () ^(cdr ,',cell-sym))
                    (,setter (val) ^(sys:rplacd ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplacd (cdadr ,',cell) ,val)))
      ,body))
  (deleter
    ^(macrolet ((,deleter ()
                  (with-gensyms (tmp)
                    (with-update-expander (cgetter csetter) '(cdadr ,cell) nil
                      ^(let ((,tmp (,cgetter)))
                         (prog1 (cdr ,tmp) (,csetter (car ,tmp))))))))
       ,body)))

(defplace (cdddar cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(rlet ((,cell-sym (cddar ,cell)))
         (macrolet ((,getter () ^(cdr ,',cell-sym))
                    (,setter (val) ^(sys:rplacd ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplacd (cddar ,',cell) ,val)))
      ,body))
  (deleter
    ^(macrolet ((,deleter ()
                  (with-gensyms (tmp)
                    (with-update-expander (cgetter csetter) '(cddar ,cell) nil
                      ^(let ((,tmp (,cgetter)))
                         (prog1 (cdr ,tmp) (,csetter (car ,tmp))))))))
       ,body)))

(defplace (cddddr cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(rlet ((,cell-sym (cdddr ,cell)))
         (macrolet ((,getter () ^(cdr ,',cell-sym))
                    (,setter (val) ^(sys:rplacd ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplacd (cdddr ,',cell) ,val)))
      ,body))
  (deleter
    ^(macrolet ((,deleter ()
                  (with-gensyms (tmp)
                    (with-update-expander (cgetter csetter) '(cdddr ,cell) nil
                      ^(let ((,tmp (,cgetter)))
                         (prog1 (cdr ,tmp) (,csetter (car ,tmp))))))))
       ,body)))

(defplace (caaaaar cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(rlet ((,cell-sym (caaaar ,cell)))
         (macrolet ((,getter () ^(car ,',cell-sym))
                    (,setter (val) ^(sys:rplaca ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplaca (caaaar ,',cell) ,val)))
      ,body))
  (deleter
    ^(macrolet ((,deleter ()
                  (with-gensyms (tmp)
                    (with-update-expander (cgetter csetter) '(caaaar ,cell) nil
                      ^(let ((,tmp (,cgetter)))
                         (prog1 (car ,tmp) (,csetter (cdr ,tmp))))))))
       ,body)))

(defplace (caaaadr cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(rlet ((,cell-sym (caaadr ,cell)))
         (macrolet ((,getter () ^(car ,',cell-sym))
                    (,setter (val) ^(sys:rplaca ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplaca (caaadr ,',cell) ,val)))
      ,body))
  (deleter
    ^(macrolet ((,deleter ()
                  (with-gensyms (tmp)
                    (with-update-expander (cgetter csetter) '(caaadr ,cell) nil
                      ^(let ((,tmp (,cgetter)))
                         (prog1 (car ,tmp) (,csetter (cdr ,tmp))))))))
       ,body)))

(defplace (caaadar cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(rlet ((,cell-sym (caadar ,cell)))
         (macrolet ((,getter () ^(car ,',cell-sym))
                    (,setter (val) ^(sys:rplaca ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplaca (caadar ,',cell) ,val)))
      ,body))
  (deleter
    ^(macrolet ((,deleter ()
                  (with-gensyms (tmp)
                    (with-update-expander (cgetter csetter) '(caadar ,cell) nil
                      ^(let ((,tmp (,cgetter)))
                         (prog1 (car ,tmp) (,csetter (cdr ,tmp))))))))
       ,body)))

(defplace (caaaddr cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(rlet ((,cell-sym (caaddr ,cell)))
         (macrolet ((,getter () ^(car ,',cell-sym))
                    (,setter (val) ^(sys:rplaca ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplaca (caaddr ,',cell) ,val)))
      ,body))
  (deleter
    ^(macrolet ((,deleter ()
                  (with-gensyms (tmp)
                    (with-update-expander (cgetter csetter) '(caaddr ,cell) nil
                      ^(let ((,tmp (,cgetter)))
                         (prog1 (car ,tmp) (,csetter (cdr ,tmp))))))))
       ,body)))

(defplace (caadaar cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(rlet ((,cell-sym (cadaar ,cell)))
         (macrolet ((,getter () ^(car ,',cell-sym))
                    (,setter (val) ^(sys:rplaca ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplaca (cadaar ,',cell) ,val)))
      ,body))
  (deleter
    ^(macrolet ((,deleter ()
                  (with-gensyms (tmp)
                    (with-update-expander (cgetter csetter) '(cadaar ,cell) nil
                      ^(let ((,tmp (,cgetter)))
                         (prog1 (car ,tmp) (,csetter (cdr ,tmp))))))))
       ,body)))

(defplace (caadadr cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(rlet ((,cell-sym (cadadr ,cell)))
         (macrolet ((,getter () ^(car ,',cell-sym))
                    (,setter (val) ^(sys:rplaca ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplaca (cadadr ,',cell) ,val)))
      ,body))
  (deleter
    ^(macrolet ((,deleter ()
                  (with-gensyms (tmp)
                    (with-update-expander (cgetter csetter) '(cadadr ,cell) nil
                      ^(let ((,tmp (,cgetter)))
                         (prog1 (car ,tmp) (,csetter (cdr ,tmp))))))))
       ,body)))

(defplace (caaddar cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(rlet ((,cell-sym (caddar ,cell)))
         (macrolet ((,getter () ^(car ,',cell-sym))
                    (,setter (val) ^(sys:rplaca ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplaca (caddar ,',cell) ,val)))
      ,body))
  (deleter
    ^(macrolet ((,deleter ()
                  (with-gensyms (tmp)
                    (with-update-expander (cgetter csetter) '(caddar ,cell) nil
                      ^(let ((,tmp (,cgetter)))
                         (prog1 (car ,tmp) (,csetter (cdr ,tmp))))))))
       ,body)))

(defplace (caadddr cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(rlet ((,cell-sym (cadddr ,cell)))
         (macrolet ((,getter () ^(car ,',cell-sym))
                    (,setter (val) ^(sys:rplaca ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplaca (cadddr ,',cell) ,val)))
      ,body))
  (deleter
    ^(macrolet ((,deleter ()
                  (with-gensyms (tmp)
                    (with-update-expander (cgetter csetter) '(cadddr ,cell) nil
                      ^(let ((,tmp (,cgetter)))
                         (prog1 (car ,tmp) (,csetter (cdr ,tmp))))))))
       ,body)))

(defplace (cadaaar cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(rlet ((,cell-sym (cdaaar ,cell)))
         (macrolet ((,getter () ^(car ,',cell-sym))
                    (,setter (val) ^(sys:rplaca ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplaca (cdaaar ,',cell) ,val)))
      ,body))
  (deleter
    ^(macrolet ((,deleter ()
                  (with-gensyms (tmp)
                    (with-update-expander (cgetter csetter) '(cdaaar ,cell) nil
                      ^(let ((,tmp (,cgetter)))
                         (prog1 (car ,tmp) (,csetter (cdr ,tmp))))))))
       ,body)))

(defplace (cadaadr cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(rlet ((,cell-sym (cdaadr ,cell)))
         (macrolet ((,getter () ^(car ,',cell-sym))
                    (,setter (val) ^(sys:rplaca ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplaca (cdaadr ,',cell) ,val)))
      ,body))
  (deleter
    ^(macrolet ((,deleter ()
                  (with-gensyms (tmp)
                    (with-update-expander (cgetter csetter) '(cdaadr ,cell) nil
                      ^(let ((,tmp (,cgetter)))
                         (prog1 (car ,tmp) (,csetter (cdr ,tmp))))))))
       ,body)))

(defplace (cadadar cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(rlet ((,cell-sym (cdadar ,cell)))
         (macrolet ((,getter () ^(car ,',cell-sym))
                    (,setter (val) ^(sys:rplaca ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplaca (cdadar ,',cell) ,val)))
      ,body))
  (deleter
    ^(macrolet ((,deleter ()
                  (with-gensyms (tmp)
                    (with-update-expander (cgetter csetter) '(cdadar ,cell) nil
                      ^(let ((,tmp (,cgetter)))
                         (prog1 (car ,tmp) (,csetter (cdr ,tmp))))))))
       ,body)))

(defplace (cadaddr cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(rlet ((,cell-sym (cdaddr ,cell)))
         (macrolet ((,getter () ^(car ,',cell-sym))
                    (,setter (val) ^(sys:rplaca ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplaca (cdaddr ,',cell) ,val)))
      ,body))
  (deleter
    ^(macrolet ((,deleter ()
                  (with-gensyms (tmp)
                    (with-update-expander (cgetter csetter) '(cdaddr ,cell) nil
                      ^(let ((,tmp (,cgetter)))
                         (prog1 (car ,tmp) (,csetter (cdr ,tmp))))))))
       ,body)))

(defplace (caddaar cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(rlet ((,cell-sym (cddaar ,cell)))
         (macrolet ((,getter () ^(car ,',cell-sym))
                    (,setter (val) ^(sys:rplaca ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplaca (cddaar ,',cell) ,val)))
      ,body))
  (deleter
    ^(macrolet ((,deleter ()
                  (with-gensyms (tmp)
                    (with-update-expander (cgetter csetter) '(cddaar ,cell) nil
                      ^(let ((,tmp (,cgetter)))
                         (prog1 (car ,tmp) (,csetter (cdr ,tmp))))))))
       ,body)))

(defplace (caddadr cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(rlet ((,cell-sym (cddadr ,cell)))
         (macrolet ((,getter () ^(car ,',cell-sym))
                    (,setter (val) ^(sys:rplaca ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplaca (cddadr ,',cell) ,val)))
      ,body))
  (deleter
    ^(macrolet ((,deleter ()
                  (with-gensyms (tmp)
                    (with-update-expander (cgetter csetter) '(cddadr ,cell) nil
                      ^(let ((,tmp (,cgetter)))
                         (prog1 (car ,tmp) (,csetter (cdr ,tmp))))))))
       ,body)))

(defplace (cadddar cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(rlet ((,cell-sym (cdddar ,cell)))
         (macrolet ((,getter () ^(car ,',cell-sym))
                    (,setter (val) ^(sys:rplaca ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplaca (cdddar ,',cell) ,val)))
      ,body))
  (deleter
    ^(macrolet ((,deleter ()
                  (with-gensyms (tmp)
                    (with-update-expander (cgetter csetter) '(cdddar ,cell) nil
                      ^(let ((,tmp (,cgetter)))
                         (prog1 (car ,tmp) (,csetter (cdr ,tmp))))))))
       ,body)))

(defplace (caddddr cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(rlet ((,cell-sym (cddddr ,cell)))
         (macrolet ((,getter () ^(car ,',cell-sym))
                    (,setter (val) ^(sys:rplaca ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplaca (cddddr ,',cell) ,val)))
      ,body))
  (deleter
    ^(macrolet ((,deleter ()
                  (with-gensyms (tmp)
                    (with-update-expander (cgetter csetter) '(cddddr ,cell) nil
                      ^(let ((,tmp (,cgetter)))
                         (prog1 (car ,tmp) (,csetter (cdr ,tmp))))))))
       ,body)))

(defplace (cdaaaar cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(rlet ((,cell-sym (caaaar ,cell)))
         (macrolet ((,getter () ^(cdr ,',cell-sym))
                    (,setter (val) ^(sys:rplacd ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplacd (caaaar ,',cell) ,val)))
      ,body))
  (deleter
    ^(macrolet ((,deleter ()
                  (with-gensyms (tmp)
                    (with-update-expander (cgetter csetter) '(caaaar ,cell) nil
                      ^(let ((,tmp (,cgetter)))
                         (prog1 (cdr ,tmp) (,csetter (car ,tmp))))))))
       ,body)))

(defplace (cdaaadr cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(rlet ((,cell-sym (caaadr ,cell)))
         (macrolet ((,getter () ^(cdr ,',cell-sym))
                    (,setter (val) ^(sys:rplacd ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplacd (caaadr ,',cell) ,val)))
      ,body))
  (deleter
    ^(macrolet ((,deleter ()
                  (with-gensyms (tmp)
                    (with-update-expander (cgetter csetter) '(caaadr ,cell) nil
                      ^(let ((,tmp (,cgetter)))
                         (prog1 (cdr ,tmp) (,csetter (car ,tmp))))))))
       ,body)))

(defplace (cdaadar cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(rlet ((,cell-sym (caadar ,cell)))
         (macrolet ((,getter () ^(cdr ,',cell-sym))
                    (,setter (val) ^(sys:rplacd ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplacd (caadar ,',cell) ,val)))
      ,body))
  (deleter
    ^(macrolet ((,deleter ()
                  (with-gensyms (tmp)
                    (with-update-expander (cgetter csetter) '(caadar ,cell) nil
                      ^(let ((,tmp (,cgetter)))
                         (prog1 (cdr ,tmp) (,csetter (car ,tmp))))))))
       ,body)))

(defplace (cdaaddr cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(rlet ((,cell-sym (caaddr ,cell)))
         (macrolet ((,getter () ^(cdr ,',cell-sym))
                    (,setter (val) ^(sys:rplacd ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplacd (caaddr ,',cell) ,val)))
      ,body))
  (deleter
    ^(macrolet ((,deleter ()
                  (with-gensyms (tmp)
                    (with-update-expander (cgetter csetter) '(caaddr ,cell) nil
                      ^(let ((,tmp (,cgetter)))
                         (prog1 (cdr ,tmp) (,csetter (car ,tmp))))))))
       ,body)))

(defplace (cdadaar cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(rlet ((,cell-sym (cadaar ,cell)))
         (macrolet ((,getter () ^(cdr ,',cell-sym))
                    (,setter (val) ^(sys:rplacd ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplacd (cadaar ,',cell) ,val)))
      ,body))
  (deleter
    ^(macrolet ((,deleter ()
                  (with-gensyms (tmp)
                    (with-update-expander (cgetter csetter) '(cadaar ,cell) nil
                      ^(let ((,tmp (,cgetter)))
                         (prog1 (cdr ,tmp) (,csetter (car ,tmp))))))))
       ,body)))

(defplace (cdadadr cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(rlet ((,cell-sym (cadadr ,cell)))
         (macrolet ((,getter () ^(cdr ,',cell-sym))
                    (,setter (val) ^(sys:rplacd ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplacd (cadadr ,',cell) ,val)))
      ,body))
  (deleter
    ^(macrolet ((,deleter ()
                  (with-gensyms (tmp)
                    (with-update-expander (cgetter csetter) '(cadadr ,cell) nil
                      ^(let ((,tmp (,cgetter)))
                         (prog1 (cdr ,tmp) (,csetter (car ,tmp))))))))
       ,body)))

(defplace (cdaddar cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(rlet ((,cell-sym (caddar ,cell)))
         (macrolet ((,getter () ^(cdr ,',cell-sym))
                    (,setter (val) ^(sys:rplacd ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplacd (caddar ,',cell) ,val)))
      ,body))
  (deleter
    ^(macrolet ((,deleter ()
                  (with-gensyms (tmp)
                    (with-update-expander (cgetter csetter) '(caddar ,cell) nil
                      ^(let ((,tmp (,cgetter)))
                         (prog1 (cdr ,tmp) (,csetter (car ,tmp))))))))
       ,body)))

(defplace (cdadddr cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(rlet ((,cell-sym (cadddr ,cell)))
         (macrolet ((,getter () ^(cdr ,',cell-sym))
                    (,setter (val) ^(sys:rplacd ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplacd (cadddr ,',cell) ,val)))
      ,body))
  (deleter
    ^(macrolet ((,deleter ()
                  (with-gensyms (tmp)
                    (with-update-expander (cgetter csetter) '(cadddr ,cell) nil
                      ^(let ((,tmp (,cgetter)))
                         (prog1 (cdr ,tmp) (,csetter (car ,tmp))))))))
       ,body)))

(defplace (cddaaar cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(rlet ((,cell-sym (cdaaar ,cell)))
         (macrolet ((,getter () ^(cdr ,',cell-sym))
                    (,setter (val) ^(sys:rplacd ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplacd (cdaaar ,',cell) ,val)))
      ,body))
  (deleter
    ^(macrolet ((,deleter ()
                  (with-gensyms (tmp)
                    (with-update-expander (cgetter csetter) '(cdaaar ,cell) nil
                      ^(let ((,tmp (,cgetter)))
                         (prog1 (cdr ,tmp) (,csetter (car ,tmp))))))))
       ,body)))

(defplace (cddaadr cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(rlet ((,cell-sym (cdaadr ,cell)))
         (macrolet ((,getter () ^(cdr ,',cell-sym))
                    (,setter (val) ^(sys:rplacd ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplacd (cdaadr ,',cell) ,val)))
      ,body))
  (deleter
    ^(macrolet ((,deleter ()
                  (with-gensyms (tmp)
                    (with-update-expander (cgetter csetter) '(cdaadr ,cell) nil
                      ^(let ((,tmp (,cgetter)))
                         (prog1 (cdr ,tmp) (,csetter (car ,tmp))))))))
       ,body)))

(defplace (cddadar cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(rlet ((,cell-sym (cdadar ,cell)))
         (macrolet ((,getter () ^(cdr ,',cell-sym))
                    (,setter (val) ^(sys:rplacd ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplacd (cdadar ,',cell) ,val)))
      ,body))
  (deleter
    ^(macrolet ((,deleter ()
                  (with-gensyms (tmp)
                    (with-update-expander (cgetter csetter) '(cdadar ,cell) nil
                      ^(let ((,tmp (,cgetter)))
                         (prog1 (cdr ,tmp) (,csetter (car ,tmp))))))))
       ,body)))

(defplace (cddaddr cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(rlet ((,cell-sym (cdaddr ,cell)))
         (macrolet ((,getter () ^(cdr ,',cell-sym))
                    (,setter (val) ^(sys:rplacd ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplacd (cdaddr ,',cell) ,val)))
      ,body))
  (deleter
    ^(macrolet ((,deleter ()
                  (with-gensyms (tmp)
                    (with-update-expander (cgetter csetter) '(cdaddr ,cell) nil
                      ^(let ((,tmp (,cgetter)))
                         (prog1 (cdr ,tmp) (,csetter (car ,tmp))))))))
       ,body)))

(defplace (cdddaar cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(rlet ((,cell-sym (cddaar ,cell)))
         (macrolet ((,getter () ^(cdr ,',cell-sym))
                    (,setter (val) ^(sys:rplacd ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplacd (cddaar ,',cell) ,val)))
      ,body))
  (deleter
    ^(macrolet ((,deleter ()
                  (with-gensyms (tmp)
                    (with-update-expander (cgetter csetter) '(cddaar ,cell) nil
                      ^(let ((,tmp (,cgetter)))
                         (prog1 (cdr ,tmp) (,csetter (car ,tmp))))))))
       ,body)))

(defplace (cdddadr cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(rlet ((,cell-sym (cddadr ,cell)))
         (macrolet ((,getter () ^(cdr ,',cell-sym))
                    (,setter (val) ^(sys:rplacd ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplacd (cddadr ,',cell) ,val)))
      ,body))
  (deleter
    ^(macrolet ((,deleter ()
                  (with-gensyms (tmp)
                    (with-update-expander (cgetter csetter) '(cddadr ,cell) nil
                      ^(let ((,tmp (,cgetter)))
                         (prog1 (cdr ,tmp) (,csetter (car ,tmp))))))))
       ,body)))

(defplace (cddddar cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(rlet ((,cell-sym (cdddar ,cell)))
         (macrolet ((,getter () ^(cdr ,',cell-sym))
                    (,setter (val) ^(sys:rplacd ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplacd (cdddar ,',cell) ,val)))
      ,body))
  (deleter
    ^(macrolet ((,deleter ()
                  (with-gensyms (tmp)
                    (with-update-expander (cgetter csetter) '(cdddar ,cell) nil
                      ^(let ((,tmp (,cgetter)))
                         (prog1 (cdr ,tmp) (,csetter (car ,tmp))))))))
       ,body)))

(defplace (cdddddr cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(rlet ((,cell-sym (cddddr ,cell)))
         (macrolet ((,getter () ^(cdr ,',cell-sym))
                    (,setter (val) ^(sys:rplacd ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplacd (cddddr ,',cell) ,val)))
      ,body))
  (deleter
    ^(macrolet ((,deleter ()
                  (with-gensyms (tmp)
                    (with-update-expander (cgetter csetter) '(cddddr ,cell) nil
                      ^(let ((,tmp (,cgetter)))
                         (prog1 (cdr ,tmp) (,csetter (car ,tmp))))))))
       ,body)))
