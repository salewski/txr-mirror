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

(progn
  (macro-time
    (defvar *place-clobber-expander* (hash))
    (defvar *place-update-expander* (hash))
    (defvar *place-delete-expander* (hash))

    (defun sys:eval-err (. params)
      (throwf 'eval-error . params))

    (defun sys:sym-update-expander (getter-name setter-name
                                    place-expr . op-body)
      ^(macrolet ((,getter-name () ',place-expr)
                  (,setter-name (val-expr) ^(sys:setq ,',place-expr ,val-expr)))
         ,*op-body))

    (defun sys:sym-clobber-expander (simple-setter-name
                                     place-expr . op-body)
      ^(macrolet ((,simple-setter-name (val-expr) ^(sys:setq ,',place-expr
                                                             ,val-expr)))
         ,*op-body))

    (defun get-update-expander (place)
      (cond
        ((symbolp place) (fun sys:sym-update-expander))
        ((consp place) (or [*place-update-expander* (car place)]
                           (sys:eval-err "~s is not an assignable place" place)))
        (t (sys:eval-err "form ~s is not syntax denoting an assignable place" place))))

    (defun get-clobber-expander (place)
      (cond
        ((symbolp place) (fun sys:sym-clobber-expander))
        ((consp place) (or [*place-clobber-expander* (car place)]
                           (iflet ((fun [*place-update-expander* (car place)]))
                              (op apply fun (gensym) @1 @2 @rest))
                           (sys:eval-err "~s is not an assignable place" place)))
        (t (sys:eval-err "form ~s is not syntax denoting an assignable place" place))))

    (defun get-delete-expander (place)
      (if (consp place)
        (or [*place-delete-expander* (car place)]
            (sys:eval-err "~s is not a deletable place" place))
        (sys:eval-err "form ~s is not syntax denoting a deletable place" place))))

  (defmacro rlet (bindings :env e . body)
    (let ((exp-bindings (mapcar (aret ^(,@1 ,(macroexpand @2 e))) bindings)))
      (let ((renames [keep-if [orf symbolp constantp] exp-bindings second])
            (regular [remove-if [orf symbolp constantp] exp-bindings second]))
        (cond ((and renames regular)
                ^(symacrolet ,renames
                   (let ,regular ,*body)))
              (renames ^(symacrolet ,renames ,*body))
              (regular ^(let ,regular ,*body))
              (t ^(progn ,*body))))))

  (defmacro with-gensyms (syms . body)
    ^(let ,(zip syms (repeat '((gensym)))) ,*body))

  (macro-time
    (defun call-update-expander (getter setter unex-place env . body)
      (let* ((place (sys:expand unex-place env))
             (expander (get-update-expander place)))
        [expander getter setter place . body]))

    (defun call-clobber-expander (ssetter unex-place env . body)
      (let* ((place (sys:expand unex-place env))
             (expander (get-clobber-expander place)))
        [expander ssetter place . body]))

    (defun call-delete-expander (deleter unex-place env . body)
      (let* ((place (sys:expand unex-place env))
             (expander (get-delete-expander place)))
        [expander deleter place . body])))

  (defmacro with-update-expander ((getter setter) unex-place env . body)
    ^(with-gensyms (,getter ,setter)
       (call-update-expander ,getter ,setter ,unex-place ,env . ,body)))

  (defmacro with-clobber-expander ((ssetter) unex-place env . body)
    ^(with-gensyms (,ssetter)
       (call-clobber-expander ,ssetter ,unex-place ,env . ,body)))

  (defmacro with-delete-expander ((deleter) unex-place env . body)
    ^(with-gensyms (,deleter)
       (call-delete-expander ,deleter ,unex-place ,env . ,body)))

  (defmacro set (place value :env env)
    (with-clobber-expander (ssetter) place env
      ^(,ssetter ,value)))

  (defmacro zap (place :env env)
    (with-update-expander (getter setter) place env
      ^(prog1 (,getter) (,setter nil))))

  (defmacro flip (place :env env)
    (with-update-expander (getter setter) place env
      ^(,setter (not (,getter)))))

  (defmacro inc (place : (delta 1) :env env)
    (with-update-expander (getter setter) place env
      (caseql delta
        (0 ^(,setter (,getter)))
        (1 ^(,setter (succ (,getter))))
        (2 ^(,setter (ssucc (,getter))))
        (3 ^(,setter (sssucc (,getter))))
        (t ^(,setter (+ (,getter) ,delta))))))

  (defmacro dec (place : (delta 1) :env env)
    (with-update-expander (getter setter) place env
      (caseql delta
        (0 ^(,setter (,getter)))
        (1 ^(,setter (pred (,getter))))
        (2 ^(,setter (ppred (,getter))))
        (3 ^(,setter (pppred (,getter))))
        (t ^(,setter (- (,getter) ,delta))))))

  (defmacro swap (place-0 place-1 :env env)
    (with-gensyms (tmp)
      (with-update-expander (getter-0 setter-0) place-0 env
        (with-update-expander (getter-1 setter-1) place-1 env
          ^(let ((,tmp (,getter-0)))
             (,setter-0 (,getter-1))
             (,setter-1 ,tmp))))))

  (defmacro push (new-item place :env env)
    (with-update-expander (getter setter) place env
      ^(,setter (cons ,new-item (,getter)))))

  (defmacro pop (place :env env)
    (with-gensyms (tmp)
      (with-update-expander (getter setter) place env
        ^(let ((,tmp (,getter)))
           (prog1 (car ,tmp) (,setter (cdr ,tmp)))))))

  (defmacro shift (:env env . places)
    (tree-case places
      (() (sys:eval-err "shift: need at least two arguments"))
      ((place) (sys:eval-err "shift: need at least two arguments"))
      ((place newvalue)
       (with-update-expander (getter setter) place env
          ^(prog1 (,getter) (,setter ,newvalue))))
      ((place . others)
        (with-update-expander (getter setter) place env
          ^(prog1 (,getter) (,setter (shift ,*others)))))))

  (defmacro rotate (:env env . places)
    (tree-case places
      (() ())
      ((fplace) fplace)
      ((fplace . rplaces)
       (with-gensyms (tmp)
         (with-update-expander (getter-f setter-f) fplace env
           ^(let ((,tmp (,getter-f)))
              (,setter-f (shift ,*rplaces ,tmp))
              ,tmp))))))

  (defmacro del (place :env env)
    (with-delete-expander (deleter) place env
      ^(,deleter)))

  (defmacro defplace (place-destructuring-args body-sym
                      (getter-sym setter-sym . update-body) :
                      ((ssetter-sym . clobber-body))
                      ((deleter-sym . delete-body)))
    (symacrolet ((name (car place-destructuring-args))
                 (args (cdr place-destructuring-args)))
      (unless (and name
                   (symbolp name)
                   (not (keywordp name))
                   (not (eq t name)))
        (sys:eval-err "~s: ~s cannot be used as a place name"
                  'defplace name))
      (with-gensyms (place)
        ^(macro-time
           (sethash *place-update-expander* ',name
             (lambda (,getter-sym ,setter-sym ,place . ,body-sym)
               (tree-bind ,args (cdr ,place)
                  ,*update-body)))
           ,*(if ssetter-sym
              ^((sethash *place-clobber-expander* ',name
                  (lambda (,ssetter-sym ,place . ,body-sym)
                    (tree-bind ,args (cdr ,place)
                       ,*clobber-body)))))
           ,*(if deleter-sym
               ^((sethash *place-delete-expander* ',name
                   (lambda (,deleter-sym ,place . ,body-sym)
                     (tree-bind ,args (cdr ,place)
                        ,*delete-body)))))))))

  (defplace (car cell) body
    (getter setter
      (with-gensyms (cell-sym)
        ^(rlet ((,cell-sym ,cell))
           (macrolet ((,getter () ^(car ,',cell-sym))
                      (,setter (val) ^(sys:rplaca ,',cell-sym ,val)))
             ,*body))))
    (ssetter
      ^(macrolet ((,ssetter (val) ^(sys:rplaca ,',cell ,val)))
        ,*body))
    (deleter
      ^(macrolet ((,deleter () ^(pop ,',cell)))
         ,*body)))

  (defplace (cdr cell) body
    (getter setter
      (with-gensyms (cell-sym)
        ^(rlet ((,cell-sym ,cell))
           (macrolet ((,getter () ^(cdr ,',cell-sym))
                      (,setter (val) ^(sys:rplacd ,',cell-sym ,val)))
             ,*body))))
    (ssetter
      ^(macrolet ((,ssetter (val) ^(sys:rplacd ,',cell ,val)))
         ,*body))
    (deleter
      ^(macrolet ((,deleter () ^(zap (cdr ,',cell))))
         ,*body)))

   (defplace (vecref vector index :whole args) body
     (getter setter
       (with-gensyms (vec-sym ind-sym)
         ^(rlet ((,vec-sym ,vector)
                 (,ind-sym ,index))
            (macrolet ((,getter () ^(vecref ,',vec-sym ,',ind-sym))
                       (,setter (val) ^(refset ,',vec-sym ,',ind-sym ,val)))
               ,*body))))
     (ssetter
       ^(macrolet ((,ssetter (val) ^(refset ,*',args ,val)))
          ,*body))
     (deleter
       (with-gensyms (vec-sym ind-sym)
         ^(rlet ((,vec-sym ,vector)
                 (,ind-sym ,index))
            (macrolet ((,deleter ()
                          ^(prog1 (vecref ,',vec-sym ,',ind-sym)
                                  (replace-vec ,',vec-sym nil
                                               ,',ind-sym (succ ,',ind-sym)))))
              ,*body)))))

   (defplace (chr-str string index :whole args) body
     (getter setter
       (with-gensyms (str-sym ind-sym)
         ^(rlet ((,str-sym ,string)
                 (,ind-sym ,index))
            (macrolet ((,getter () ^(chr-str ,',str-sym ,',ind-sym))
                       (,setter (val) ^(chr-str-set ,',str-sym ,',ind-sym ,val)))
               ,*body))))
     (ssetter
       ^(macrolet ((,ssetter (val) ^(chr-str-set ,*',args ,val)))
          ,*body))
     (deleter
       (with-gensyms (str-sym ind-sym)
         ^(rlet ((,str-sym ,string)
                 (,ind-sym ,index))
            (macrolet ((,deleter ()
                          ^(prog1 (chr-str ,',str-sym ,',ind-sym)
                                  (replace-str ,',str-sym nil
                                               ,',ind-sym (succ ,',ind-sym)))))
              ,*body)))))

   (defplace (ref seq index :whole args) body
     (getter setter
       (with-gensyms (seq-sym ind-sym)
         ^(rlet ((,seq-sym ,seq)
                 (,ind-sym ,index))
            (macrolet ((,getter () ^(ref ,',seq-sym ,',ind-sym))
                       (,setter (val) ^(refset ,',seq-sym ,',ind-sym ,val)))
               ,*body))))
     (ssetter
       ^(macrolet ((,ssetter (val) ^(refset ,*',args ,val)))
          ,*body))
     (deleter
       (with-gensyms (seq-sym ind-sym)
         ^(rlet ((,seq-sym ,seq)
                 (,ind-sym ,index))
            (macrolet ((,deleter ()
                          ^(prog1 (ref ,',seq-sym ,',ind-sym)
                                  (replace ,',seq-sym nil
                                           ,',ind-sym (succ ,',ind-sym)))))
              ,*body)))))

   (defplace (gethash hash key : (default nil have-default-p)) body
     (getter setter
       (with-gensyms (entry-sym)
         ^(let ((,entry-sym (inhash ,hash ,key ,default)))
            (macrolet ((,getter () ^(cdr ,',entry-sym))
                       (,setter (val) ^(sys:rplacd ,',entry-sym ,val)))
               ,*body))))
     :
     (deleter
       ^(macrolet ((,deleter ()
                     (if ,have-default-p
                       (with-gensyms (entry-sym
                                      dfl-sym)
                         ^(rlet ((,entry-sym (inhash ,',hash ,',key))
                                 (,dfl-sym ,',default))
                            (if ,entry-sym
                              (remhash ,',hash ,',key)
                              ,dfl-sym)))
                       ^(remhash ,',hash ,',key))))
          ,*body)))

   (defplace (dwim obj-place index : (default nil have-default-p)) body
     (getter setter
       (with-gensyms (ogetter-sym osetter-sym obj-sym
                      oldval-sym newval-sym
                      index-sym index-sym
                      oldval-sym dflval-sym)
         (with-update-expander (ogetter-sym osetter-sym) obj-place nil
           ^(rlet ((,obj-sym (,ogetter-sym))
                   (,index-sym ,index)
                   ,*(if have-default-p
                       ^((,dflval-sym ,default))))
              (let ((,oldval-sym [,obj-sym
                                 ,index-sym
                                 ,*(if have-default-p ^(,dflval-sym))]))
                (macrolet ((,getter () ',oldval-sym)
                           (,setter (val)
                              ^(rlet ((,',newval-sym ,val))
                                 (,',osetter-sym
                                   (sys:dwim-set ,',obj-sym
                                                 ,',index-sym ,',newval-sym))
                                 ,',newval-sym)))
                   ,*body))))))
     (ssetter
       (with-gensyms (osetter-sym ogetter-sym
                      obj-sym newval-sym index-sym)
         (with-update-expander (ogetter-sym osetter-sym) obj-place nil
           ^(macrolet ((,ssetter (val)
                        ^(rlet ((,',obj-sym (,',ogetter-sym))
                                (,',index-sym ,',index)
                                (,',newval-sym ,val))
                           (,',osetter-sym
                             (sys:dwim-set ,',obj-sym
                                           ,*(if ,have-default-p
                                               ^((prog1 ,',index-sym ,',default))
                                               ^(,',index-sym))
                                           ,',newval-sym))
                           ,',newval-sym)))
                ,*body))))

     (deleter
       (with-gensyms (osetter-sym ogetter-sym
                      obj-sym index-sym oldval-sym
                      dflval-sym)
         (with-update-expander (ogetter-sym osetter-sym) obj-place nil
           ^(macrolet ((,deleter ()  ;; todo: place must not have optional val
                        ^(rlet ((,',obj-sym (,',ogetter-sym)))
                           (let* ((,',index-sym ,',index)
                                  (,',oldval-sym [,',obj-sym
                                                  ,',index-sym
                                                  ,*(if ,have-default-p
                                                      ^(,',default))]))
                             (progn
                               (,',osetter-sym
                                 (sys:dwim-del ,',obj-sym ,',index-sym))
                               ,',oldval-sym)))))
                ,*body)))))

   (defplace (force promise) body
     (getter setter
       (with-gensyms (promise-sym)
         ^(rlet ((,promise-sym ,promise))
             (macrolet ((,getter ()
                           ^(force ,',promise-sym))
                        (,setter (val)
                           ^(set (car (cdr ,',promise-sym)) ,val)))
               ,*body))))
     (ssetter
       (with-gensyms (promise-sym)
         ^(rlet ((,promise-sym ,promise))
            (macrolet ((,ssetter (val)
                         ^(prog1
                            (set (car (cdr ,',promise-sym)) ,val)
                            (set (car ,',promise-sym) 'sys:promise-forced))))
              ,*body)))))

   (defplace (errno) body
     (getter setter
       ^(macrolet ((,getter () '(errno))
                   (,setter (val-expr)
                      (with-gensyms (val-sym)
                        ^(rlet ((,val-sym ,val-expr))
                            (progn (errno ,val-sym) ,val-sym)))))
          ,*body)))

   (macro-time
     (each ((from '(car cdr))
            (to '(first rest)))
       (each ((table (list *place-update-expander*
                           *place-clobber-expander*
                           *place-delete-expander*)))
         (set [table to] [table from])))))
