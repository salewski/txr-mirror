;; Copyright 2015-2024
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

(defstruct (sys:yld-item val cont) nil val cont)

(defstruct (sys:rcv-item val cont) nil val cont)

(defun sys:obtain-impl (fun)
  (finalize
    (lambda (: resume-val)
      (let ((yi (call fun resume-val)))
        (while t
          (cond
            ((eq (typeof yi) 'sys:yld-item)
             (call fun 'sys:cont-free)
             (set fun yi.cont)
             (return yi.val))
            ((eq (typeof yi) 'sys:rcv-item)
             (call fun 'sys:cont-free)
             (set fun yi.cont)
             (set yi (call fun resume-val)))
            (t (return yi))))))
    (lambda (cont)
      (call cont 'sys:cont-poison))))

(defmacro obtain (. body)
  (let ((arg (gensym "arg")))
    ^(sys:obtain-impl (lambda (,arg)
                        (unless (eq ,arg 'sys:cont-free)
                          ,*body)))))

(defmacro obtain-block (name . body)
  ^(obtain (block ,name ,*body)))

(defmacro obtain* (. body)
  (let ((arg (gensym "arg"))
        (fun (gensym "fun")))
    ^(let ((,fun (sys:obtain-impl (lambda (,arg)
                                    (unless (eq ,arg 'sys:cont-free)
                                      ,*body)))))
       (call ,fun nil)
       ,fun)))

(defmacro obtain*-block (name . body)
  ^(obtain* (block ,name ,*body)))

(defmacro yield-from (:form ctx-form name : (form nil have-form-p))
  (let ((cont-sym (gensym)))
    ^(sys:capture-cont ',name
                       (lambda (,cont-sym)
                         (sys:abscond-from ,name
                                           ,(if have-form-p
                                              ^(new (sys:yld-item
                                                      ,form ,cont-sym))
                                              ^(new (sys:rcv-item
                                                      nil ,cont-sym)))))
                     ',ctx-form)))

(defmacro yield (: (form nil have-form-p))
  (if have-form-p
    ^(yield-from nil ,form)
    ^(yield-from nil)))

(defmacro suspend (:form form name sym . body)
  ^(sys:capture-cont ',name (lambda (,sym)
                              (sys:abscond-from ,name (progn ,*body)))
                     ',form))

(defun hlet-expand (op raw-vis body)
  (let* ((vis (mapcar [iffi atom list] raw-vis))
         (nvars (len vis))
         (syms [mapcar car vis])
         (inits [mapcar cadr vis])
         (vec (gensym))
         (macs (mapcar (ret ^(,@1 (vecref ,vec ,@2)))
                       syms (range 0)))
         (inits (mapcar (ret ^(set (vecref ,vec ,@1) ,@2))
                        (range 0) inits)))
    (if (eq op 'hlet*)
      ^(let* ((,vec (vector ,nvars)))
         (symacrolet ,macs
           ,*inits
           ,*body))
      ^(let* ((,vec (vector ,nvars)))
         ,*inits
         (symacrolet ,macs
           ,*body)))))

(defmacro hlet (var-inits . body)
  (hlet-expand 'hlet var-inits body))

(defmacro hlet* (var-inits . body)
  (hlet-expand 'hlet* var-inits body))
