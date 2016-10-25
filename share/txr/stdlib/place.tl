;; Copyright 2015-2016
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

(macro-time
  (defvar *place-clobber-expander* (hash))
  (defvar *place-update-expander* (hash))
  (defvar *place-delete-expander* (hash))
  (defvar *place-macro* (hash))
  (defvar sys:*lisp1* nil)
  (defvar sys:*pl-env* nil)

  (defun sys:eval-err (. params)
    (throwf 'eval-error . params))

  (defmacro sys:l1-setq (u-expr new-val :env e)
    (let ((e-expr (macroexpand u-expr e)))
      (if (symbolp e-expr)
        (caseq (lexical-lisp1-binding e e-expr)
          (:var ^(sys:setq ,e-expr ,new-val))
          (:symacro (sys:eval-err "sys:l1-setq: invalid use on symbol macro"))
          (t (if (boundp e-expr)
               ^(sys:setq ,e-expr ,new-val)
               ^(sys:lisp1-setq ,e-expr ,new-val))))
        ^(set ,u-expr ,new-val))))

  (defmacro sys:l1-val (u-expr :env e)
    (let ((e-expr (macroexpand u-expr e)))
      (if (and (symbolp e-expr) (not (constantp e-expr)))
        (caseq (lexical-lisp1-binding e e-expr)
          (:fun ^(fun ,u-expr))
          (:var u-expr)
          (nil (if (boundp e-expr)
                 u-expr
                 ^(sys:lisp1-value ,u-expr)))
          (t (sys:eval-err "sys:l1-val: invalid case")))
        u-expr)))

  (defun sys:sym-update-expander (getter-name setter-name
                                  place-expr op-body)
    (if sys:*lisp1*
      ^(macrolet ((,getter-name () ^(sys:l1-val ,',place-expr))
                  (,setter-name (val-expr) ^(sys:l1-setq ,',place-expr
                                                         ,val-expr)))
         ,op-body)
      ^(macrolet ((,getter-name () ',place-expr)
                  (,setter-name (val-expr) ^(sys:setq ,',place-expr
                                                      ,val-expr)))
         ,op-body)))

  (defun sys:sym-clobber-expander (simple-setter-name
                                   place-expr op-body)
    ^(macrolet ((,simple-setter-name (val-expr)
                   ^(,(if sys:*lisp1* 'sys:l1-setq 'sys:setq)
                      ,',place-expr ,val-expr)))
       ,op-body))

  (defun sys:sym-delete-expander (deleter-name
                                  place-expr . op-body)
    ^(macrolet ((,deleter-name (:env env)
                  (when (lexical-var-p env ',place-expr)
                    (sys:eval-err "~s is a lexical variable, thus not deletable"
                                  ',place-expr))
                  ^(prog1
                     (symbol-value ',',place-expr)
                     (makunbound ',',place-expr))))
       ,*op-body))

  (defun place-form-p (unex-place env)
    (let ((place (sys:pl-expand unex-place env)))
      (or (bindable place)
          (and (consp place) [*place-update-expander* (car place)] t))))

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
    (cond
      ((symbolp place) (fun sys:sym-delete-expander))
      ((consp place) (or [*place-delete-expander* (car place)]
                         (sys:eval-err "~s is not a deletable place" place)))
      (t (sys:eval-err "form ~s is not syntax denoting a deletable place" place)))))

(macro-time
  (defun sys:r-s-let-expander (bindings body e pred)
    (let ((exp-bindings (mapcar (aret ^(,@1 ,(macroexpand @2 e))) bindings)))
      (let ((renames [keep-if pred exp-bindings second])
            (regular [remove-if pred exp-bindings second]))
      (cond ((and renames regular)
              ^(symacrolet ,renames
                 (let ,regular ,*body)))
            (renames ^(symacrolet ,renames ,*body))
            (regular ^(let ,regular ,*body))
            (t ^(progn ,*body)))))))

(defmacro rlet (bindings :env e . body)
  [sys:r-s-let-expander bindings body e constantp])

(defmacro slet (bindings :env e . body)
  (sys:r-s-let-expander bindings body e [orf constantp bindable]))

(defmacro alet (bindings :env e . body)
  (let ((exp-bindings (mapcar (aret ^(,@1 ,(macroexpand @2 e))) bindings)))
    ^(,(if [all exp-bindings [orf constantp bindable] second]
         'symacrolet 'let)
       ,exp-bindings ,*body)))

(defmacro with-gensyms (syms . body)
  ^(let ,(zip syms (repeat '((gensym)))) ,*body))

(macro-time
  (defun sys:pl-expand (unex-place env)
    (while t
      (let ((place unex-place))
        (let ((pm-expander [*place-macro* (if (consp unex-place)
                                            (car unex-place))]))
          (when pm-expander
            (sys:setq place (sys:set-macro-ancestor
                              [pm-expander unex-place]
                              unex-place))))
        (sys:setq place (macroexpand-1 place env))
        (when (or (eq place unex-place)
                  (null place)
                  (and (atom place) (not (symbolp place))))
          (return place))
        (sys:setq unex-place place))))

  (defun sys:cp-origin (to-tree from-form syms : circ-check)
    (unless (memq to-tree circ-check)
      (tree-case to-tree
        ((a . d) (when (memq a syms)
                   (sys:set-macro-ancestor to-tree from-form))
                 (sys:cp-origin a from-form syms (cons to-tree circ-check))
                 (sys:cp-origin d from-form syms (cons to-tree circ-check)))))
    to-tree)

  (defun call-update-expander (getter setter unex-place env body)
    (let* ((place (sys:pl-expand unex-place env))
           (expander (get-update-expander place))
           (sys:*pl-env* env))
      (sys:cp-origin [expander getter setter place body]
                     place (list setter getter))))

  (defun call-clobber-expander (ssetter unex-place env body)
    (let* ((place (sys:pl-expand unex-place env))
           (expander (get-clobber-expander place))
           (sys:*pl-env* env))
      (sys:cp-origin [expander ssetter place body]
                     place (list ssetter))))

  (defun call-delete-expander (deleter unex-place env body)
    (let* ((place (sys:pl-expand unex-place env))
           (expander (get-delete-expander place))
           (sys:*pl-env* env))
      (sys:cp-origin [expander deleter place body]
                     place (list deleter)))))

(defmacro with-update-expander ((getter setter) unex-place env body)
  ^(with-gensyms (,getter ,setter)
     (call-update-expander ,getter ,setter ,unex-place ,env ,body)))

(defmacro with-clobber-expander ((ssetter) unex-place env body)
  ^(with-gensyms (,ssetter)
     (call-clobber-expander ,ssetter ,unex-place ,env ,body)))

(defmacro with-delete-expander ((deleter) unex-place env body)
  ^(with-gensyms (,deleter)
     (call-delete-expander ,deleter ,unex-place ,env ,body)))

(defmacro set (:env env . place-value-pairs)
  (let ((assign-forms (mapcar (tb ((place : (value nil value-present-p)))
                                (unless value-present-p
                                  (sys:eval-err "set: arguments must be pairs"))
                                (with-clobber-expander (ssetter) place env
                                   ^(,ssetter ,value)))
                              (tuples 2 place-value-pairs))))
    (if (cdr assign-forms)
      ^(progn ,*assign-forms)
      (car assign-forms))))

(defmacro pset (:env env . place-value-pairs)
  (let ((len (length place-value-pairs)))
    (cond
      ((oddp len) (sys:eval-err "pset: arguments must be pairs"))
      ((<= len 2) ^(set ,*place-value-pairs))
      (t (let* ((pvtgs (mapcar (tb ((a b))
                                 (list a b (gensym) (gensym) (gensym)))
                               (tuples 2 place-value-pairs)))
                (ls (reduce-left (tb ((lets stores) (place value temp getter setter))
                                     (list ^((,temp ,value) ,*lets)
                                           ^((,setter ,temp) ,*stores)))
                                 pvtgs '(nil nil)))
                (lets (first ls))
                (stores (second ls))
                (body-form ^(rlet (,*lets) ,*stores)))
           (reduce-left (tb (accum-form (place value temp getter setter))
                          (call-update-expander getter setter
                                                place env accum-form))
                        pvtgs body-form))))))

(defmacro zap (place : (new-val nil) :env env)
  (with-update-expander (getter setter) place env
    ^(prog1 (,getter) (,setter ,new-val))))

(defmacro flip (place :env env)
  (with-update-expander (getter setter) place env
    ^(,setter (not (,getter)))))

(defmacro inc (place : (delta 1) :env env)
  (with-update-expander (getter setter) place env
    (caseql delta
      (0 place)
      (1 ^(,setter (succ (,getter))))
      (2 ^(,setter (ssucc (,getter))))
      (3 ^(,setter (sssucc (,getter))))
      (t ^(,setter (+ (,getter) ,delta))))))

(defmacro dec (place : (delta 1) :env env)
  (with-update-expander (getter setter) place env
    (caseql delta
      (0 place)
      (1 ^(,setter (pred (,getter))))
      (2 ^(,setter (ppred (,getter))))
      (3 ^(,setter (pppred (,getter))))
      (t ^(,setter (- (,getter) ,delta))))))

(defmacro pinc (place : (delta 1) :env env)
  (with-gensyms (oldval)
    (with-update-expander (getter setter) place env
      (caseql delta
        (0 place)
        (1 ^(let ((,oldval (,getter))) (,setter (succ ,oldval)) ,oldval))
        (2 ^(let ((,oldval (,getter))) (,setter (ssucc ,oldval)) ,oldval))
        (3 ^(let ((,oldval (,getter))) (,setter (sssucc ,oldval)) ,oldval))
        (t ^(let ((,oldval (,getter))) (,setter (+ ,oldval, delta)) ,oldval))))))

(defmacro pdec (place : (delta 1) :env env)
  (with-gensyms (oldval)
    (with-update-expander (getter setter) place env
      (caseql delta
        (0 place)
        (1 ^(let ((,oldval (,getter))) (,setter (pred ,oldval)) ,oldval))
        (2 ^(let ((,oldval (,getter))) (,setter (ppred ,oldval)) ,oldval))
        (3 ^(let ((,oldval (,getter))) (,setter (pppred ,oldval)) ,oldval))
        (t ^(let ((,oldval (,getter))) (,setter (- ,oldval, delta)) ,oldval))))))

(defmacro swap (place-0 place-1 :env env)
  (with-gensyms (tmp)
    (with-update-expander (getter-0 setter-0) place-0 env
      (with-update-expander (getter-1 setter-1) place-1 env
        ^(let ((,tmp (,getter-0)))
           (,setter-0 (,getter-1))
           (,setter-1 ,tmp))))))

(defmacro push (new-item place :env env)
  (with-gensyms (new-sym)
    ^(rlet ((,new-sym ,new-item))
       ,(with-update-expander (getter setter) place env
          ^(,setter (cons ,new-sym (,getter)))))))

(defmacro pop (place :env env)
  (with-gensyms (tmp)
    (with-update-expander (getter setter) place env
      ^(let ((,tmp (,getter)))
         (prog1 (car ,tmp) (,setter (cdr ,tmp)))))))

(defmacro pushnew (new-item place :env env :
                            (testfun :)
                            (keyfun :))
  (with-update-expander (getter setter) place env
    (with-gensyms (new-item-sym old-list-sym)
      ^(rlet ((,new-item-sym ,new-item))
         ,(with-update-expander (getter setter) place env
            ^(let ((,old-list-sym (,getter)))
               (if (member ,new-item-sym ,old-list-sym ,testfun ,keyfun)
                 ,old-list-sym
                 (,setter (cons ,new-item-sym ,old-list-sym)))))))))

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

(defmacro test-set (:env env place)
  (with-update-expander (getter setter) place env
    ^(unless (,getter)
       (,setter t))))

(defmacro test-clear (:env env place)
  (with-update-expander (getter setter) place env
    ^(when (,getter)
       (,setter nil)
       t)))

(defmacro compare-swap (:env env comp-fun place comp-val store-val)
  (with-update-expander (getter setter) place env
    ^(when (,comp-fun (,getter) ,comp-val)
       (,setter ,store-val)
       t)))

(defmacro test-inc (place : (delta 1) (upfrom-val 0))
  ^(eql (pinc ,place ,delta) ,above-val))

(defmacro test-dec (place : (delta 1) (downto-val 0))
  ^(eql (dec ,place ,delta) ,downto-val))

(defmacro del (place :env env)
  (with-delete-expander (deleter) place env
    ^(,deleter)))

(defmacro lset (. places-source)
  (let ((places (butlast places-source))
        (source (last places-source))
        (orig (gensym))
        (iter (gensym)))
    (unless places
      (sys:eval-err "lset: require one or more places followed by expression"))
    ^(let* ((,orig ,(car source))
            (,iter ,orig))
       ,*(butlast (mappend (ret ^((set ,@1 (car ,iter)) (set ,iter (cdr ,iter))))
                           places))
       ,orig)))

(defmacro upd (place . opip-args)
  (with-gensyms (pl)
    ^(placelet ((,pl ,place))
       (set ,pl (call (opip ,*opip-args) ,pl)))))

(defmacro defplace (place-destructuring-args body-sym
                    (getter-sym setter-sym update-body) :
                    ((ssetter-sym clobber-body))
                    ((deleter-sym delete-body)))
  (let ((name (car place-destructuring-args))
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
           (lambda (,getter-sym ,setter-sym ,place ,body-sym)
             (tree-bind ,args (cdr ,place)
                ,update-body)))
         ,*(if ssetter-sym
            ^((sethash *place-clobber-expander* ',name
                (lambda (,ssetter-sym ,place ,body-sym)
                  (tree-bind ,args (cdr ,place)
                     ,clobber-body)))))
         ,*(if deleter-sym
             ^((sethash *place-delete-expander* ',name
                 (lambda (,deleter-sym ,place ,body-sym)
                   (tree-bind ,args (cdr ,place)
                      ,delete-body)))))
         ',name))))

(defmacro define-place-macro (name place-destructuring-args . body)
  (with-gensyms (name-dummy args)
    ^(progn
       (sethash *place-macro* ',name
                (lambda (,args)
                  (mac-param-bind ,args
                                  (,name-dummy ,*place-destructuring-args)
                                  ,args ,*body)))
       ',name)))

(defplace (sys:var arg) body
  (getter setter
    ^(macrolet ((,getter () ^(sys:var ,',arg))
                (,setter (val) ^(sys:setq ,'(sys:var ,arg) ,val)))
       ,body)))

(defplace (car cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(slet ((,cell-sym ,cell))
         (macrolet ((,getter () ^(car ,',cell-sym))
                    (,setter (val) ^(sys:rplaca ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplaca ,',cell ,val)))
      ,body))
  (deleter
    ^(macrolet ((,deleter () ^(pop ,',cell)))
       ,body)))

(defplace (cdr cell) body
  (getter setter
    (with-gensyms (cell-sym)
      ^(slet ((,cell-sym ,cell))
         (macrolet ((,getter () ^(cdr ,',cell-sym))
                    (,setter (val) ^(sys:rplacd ,',cell-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(sys:rplacd ,',cell ,val)))
       ,body))
  (deleter
    ^(macrolet ((,deleter ()
                  (with-gensyms (tmp)
                    (with-update-expander (cgetter csetter) ',cell nil
                      ^(let ((,tmp (,cgetter)))
                         (prog1 (cdr ,tmp) (,csetter (car ,tmp))))))))
       ,body)))

(defplace (nthcdr index list) body
  (getter setter
    (with-gensyms (index-sym list-sym sentinel-head-sym parent-cell-sym)
      (if (place-form-p list sys:*pl-env*)
        (with-update-expander (lgetter lsetter) list nil
          ^(alet ((,index-sym ,index)
                  (,list-sym (,lgetter)))
             (let* ((,sentinel-head-sym (cons nil ,list-sym))
                    (,parent-cell-sym (nthcdr ,index-sym ,sentinel-head-sym)))
               (macrolet ((,getter () ^(cdr ,',parent-cell-sym))
                          (,setter (val)
                            ^(prog1 (sys:rplacd ,',parent-cell-sym ,val)
                                    (,',lsetter (cdr ,',sentinel-head-sym)))))
                 ,body))))
        ^(alet ((,index-sym ,index)
                (,list-sym ,list))
           (let ((,parent-cell-sym (nthcdr (pred ,index-sym) ,list-sym)))
             (macrolet ((,getter () ^(cdr ,',parent-cell-sym))
                        (,setter (val)
                          ^(sys:rplacd ,',parent-cell-sym ,val)))
               ,body)))))))

(defplace (vecref vector index :whole args) body
  (getter setter
    (with-gensyms (vec-sym ind-sym)
      ^(alet ((,vec-sym ,vector)
              (,ind-sym ,index))
         (macrolet ((,getter () ^(vecref ,',vec-sym ,',ind-sym))
                    (,setter (val) ^(refset ,',vec-sym ,',ind-sym ,val)))
            ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(refset ,*',args ,val)))
       ,body))
  (deleter
    (with-gensyms (vec-sym ind-sym)
      ^(alet ((,vec-sym ,vector)
              (,ind-sym ,index))
         (macrolet ((,deleter ()
                       ^(prog1 (vecref ,',vec-sym ,',ind-sym)
                               (replace-vec ,',vec-sym nil
                                            ,',ind-sym (succ ,',ind-sym)))))
           ,body)))))

(defplace (chr-str string index :whole args) body
  (getter setter
    (with-gensyms (str-sym ind-sym)
      ^(alet ((,str-sym ,string)
              (,ind-sym ,index))
         (macrolet ((,getter () ^(chr-str ,',str-sym ,',ind-sym))
                    (,setter (val) ^(chr-str-set ,',str-sym ,',ind-sym ,val)))
            ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(chr-str-set ,*',args ,val)))
       ,body))
  (deleter
    (with-gensyms (str-sym ind-sym)
      ^(alet ((,str-sym ,string)
              (,ind-sym ,index))
         (macrolet ((,deleter ()
                       ^(prog1 (chr-str ,',str-sym ,',ind-sym)
                               (replace-str ,',str-sym nil
                                            ,',ind-sym (succ ,',ind-sym)))))
           ,body)))))

(defplace (ref seq index :whole args) body
  (getter setter
    (with-gensyms (seq-sym ind-sym)
      ^(alet ((,seq-sym ,seq)
              (,ind-sym ,index))
         (macrolet ((,getter () ^(ref ,',seq-sym ,',ind-sym))
                    (,setter (val) ^(refset ,',seq-sym ,',ind-sym ,val)))
            ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(refset ,*',args ,val)))
       ,body))
  (deleter
    (with-gensyms (seq-sym ind-sym)
      ^(alet ((,seq-sym ,seq)
              (,ind-sym ,index))
         (macrolet ((,deleter ()
                       ^(prog1 (ref ,',seq-sym ,',ind-sym)
                               (replace ,',seq-sym nil
                                        ,',ind-sym (succ ,',ind-sym)))))
           ,body)))))

(defplace (gethash hash key : (default nil have-default-p)) body
  (getter setter
    (with-gensyms (entry-sym)
      ^(let ((,entry-sym (inhash ,hash ,key ,default)))
         (macrolet ((,getter () ^(cdr ,',entry-sym))
                    (,setter (val) ^(sys:rplacd ,',entry-sym ,val)))
            ,body))))
  :
  (deleter
    ^(macrolet ((,deleter ()
                  (if ,have-default-p
                    (with-gensyms (entry-sym
                                   dfl-sym)
                      ^(alet ((,entry-sym (inhash ,',hash ,',key))
                              (,dfl-sym ,',default))
                         (if ,entry-sym
                           (remhash ,',hash ,',key)
                           ,dfl-sym)))
                    ^(remhash ,',hash ,',key))))
       ,body)))

(defplace (hash-userdata hash) body
  (getter setter
    (with-gensyms (hash-sym)
      ^(slet ((,hash-sym ,hash))
         (macrolet ((,getter () ^(hash-userdata ,',hash-sym))
                    (,setter (val) ^(set-hash-userdata ,',hash-sym ,val)))
            ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val)
                  ^(set-hash-userdata ,',hash ,val)))
      ,body)))

(defplace (dwim obj-place index : (default nil have-default-p) :env env) body
  (getter setter
    (with-gensyms (ogetter-sym osetter-sym obj-sym
                   oldval-sym newval-sym
                   index-sym index-sym
                   oldval-sym dflval-sym)
      (let ((sys:*lisp1* t))
        (with-update-expander (ogetter-sym osetter-sym) obj-place nil
          ^(let ((,index-sym (sys:l1-val ,index))
                 ,*(if have-default-p
                     ^((,dflval-sym (sys:l1-val ,default)))))
             (macrolet ((,getter ()
                         '[(,ogetter-sym) ,index-sym
                                          ,*(if have-default-p ^(,dflval-sym))])
                        (,setter (val)
                           ^(rlet ((,',newval-sym ,val))
                              (,',osetter-sym
                                (sys:dwim-set (,',ogetter-sym)
                                              ,',index-sym ,',newval-sym))
                              ,',newval-sym)))
               ,body))))))
  (ssetter
    (with-gensyms (osetter-sym ogetter-sym
                   obj-sym newval-sym index-sym)
      (let ((sys:*lisp1* t))
        (with-update-expander (ogetter-sym osetter-sym) obj-place nil
          ^(macrolet ((,ssetter (val)
                       ^(rlet ((,',index-sym (sys:l1-val ,',index))
                               (,',newval-sym ,val))
                          (,',osetter-sym
                            (sys:dwim-set (,',ogetter-sym)
                                          ,*(if ,have-default-p
                                              ^((prog1
                                                  ,',index-sym
                                                  (sys:l1-val ,',default)))
                                              ^(,',index-sym))
                                          ,',newval-sym))
                          ,',newval-sym)))
               ,body)))))

  (deleter
    (with-gensyms (osetter-sym ogetter-sym
                   obj-sym index-sym oldval-sym
                   dflval-sym)
      (let ((sys:*lisp1* t))
        (with-update-expander (ogetter-sym osetter-sym) obj-place nil
          ^(macrolet ((,deleter ()  ;; todo: place must not have optional val
                       ^(let ((,',obj-sym (,',ogetter-sym)))
                          (let* ((,',index-sym (sys:l1-val ,',index))
                                 (,',oldval-sym [,',obj-sym
                                                 ,',index-sym
                                                 ,*(if ,have-default-p
                                                     ^(,',default))]))
                            (progn
                              (,',osetter-sym
                                (sys:dwim-del ,',obj-sym ,',index-sym))
                              ,',oldval-sym)))))
               ,body))))))

(defplace (force promise) body
  (getter setter
    (with-gensyms (promise-sym)
      ^(rlet ((,promise-sym ,promise))
          (macrolet ((,getter ()
                        ^(force ,',promise-sym))
                     (,setter (val)
                        ^(set (car (cdr ,',promise-sym)) ,val)))
            ,body))))
  (ssetter
    (with-gensyms (promise-sym)
      ^(rlet ((,promise-sym ,promise))
         (macrolet ((,ssetter (val)
                      ^(prog1
                         (set (car (cdr ,',promise-sym)) ,val)
                         (set (car ,',promise-sym) 'sys:promise-forced))))
           ,body)))))

(defplace (errno) body
  (getter setter
    ^(macrolet ((,getter () '(errno))
                (,setter (val-expr)
                   (with-gensyms (val-sym)
                     ^(slet ((,val-sym ,val-expr))
                         (progn (errno ,val-sym) ,val-sym)))))
       ,body)))

(defplace (fun sym) body
  (getter setter
    ^(macrolet ((,getter () ^(fun ,',sym))
                (,setter (val) ^(sys:setqf ,',sym ,val)))
       ,body))
  :
  (deleter
    ^(macrolet ((,deleter (:env env)
                   (when (lexical-fun-p env ',sym)
                     (sys:eval-err "~s is a lexical function, \
                                   \ thus not deletable"
                                 ',sym))
                    ^(fmakunbound ',',sym)))
       ,body)))

(defun sys:get-fb (sym)
  (or (gethash sys:top-fb sym)
      (sys:eval-err "unbound function ~s" sym)))

(defplace (symbol-function sym-expr) body
  (getter setter
    (with-gensyms (binding-sym)
      ^(let ((,binding-sym (sys:get-fb ,sym-expr)))
          (macrolet ((,getter () ^(cdr ,',binding-sym))
                     (,setter (val) ^(sys:rplacd ,',binding-sym ,val)))
            ,body))))
  :
  (deleter
    ^(macrolet ((,deleter () ^(fmakunbound ,',sym-expr)))
       ,body)))

(defun sys:get-mb (sym)
  (or (gethash sys:top-mb sym)
      (sys:eval-err "unbound macro ~s" sym)))

(defplace (symbol-macro sym-expr) body
  (getter setter
    (with-gensyms (binding-sym)
      ^(let ((,binding-sym (sys:get-mb ,sym-expr)))
          (macrolet ((,getter () ^(cdr ,',binding-sym))
                     (,setter (val) ^(sys:rplacd ,',binding-sym ,val)))
            ,body))))
  :
  (deleter
    ^(macrolet ((,deleter () ^(mmakunbound ,',sym-expr)))
       ,body)))

(defun sys:get-vb (sym)
  (or (gethash sys:top-vb sym)
      (sys:eval-err "unbound variable ~s" sym)))

(defplace (symbol-value sym-expr) body
  (getter setter
    (with-gensyms (binding-sym)
      ^(let ((,binding-sym (sys:get-vb ,sym-expr)))
          (macrolet ((,getter () ^(cdr ,',binding-sym))
                     (,setter (val) ^(sys:rplacd ,',binding-sym ,val)))
            ,body))))
  :
  (deleter
    ^(macrolet ((,deleter () ^(makunbound ,',sym-expr)))
       ,body)))

(defplace (slot struct sym) body
  (getter setter
    (with-gensyms (struct-sym slot-sym)
      ^(alet ((,struct-sym ,struct)
              (,slot-sym ,sym))
         (macrolet ((,getter () ^(slot ,',struct-sym ,',slot-sym))
                    (,setter (val) ^(slotset ,',struct-sym ,',slot-sym ,val)))
           ,body))))
  (ssetter
    ^(macrolet ((,ssetter (val) ^(slotset ,',struct ,',sym ,val)))
      ,body)))

(defmacro define-modify-macro (name lambda-list function)
  (let ((cleaned-lambda-list (mapcar [iffi consp car]
                                     (remql : lambda-list))))
    (with-gensyms (place-sym args-sym)
      ^(defmacro ,name (:env env ,place-sym ,*lambda-list)
         (with-update-expander (getter setter) ,place-sym env
           ^(,setter (,',function (,getter) ,,*cleaned-lambda-list)))))))

(defmacro sys:placelet-1 (((sym place)) :env env . body)
  (with-gensyms (tmp-place pl-getter pl-setter steal-getter)
    (unwind-protect
      (progn
        (sethash *place-update-expander* tmp-place
                 (lambda (tmp-getter tmp-setter tmp-place tmp-body)
                   ^(macrolet ((,tmp-getter () ^(,',pl-getter))
                               (,tmp-setter (val) ^(,',pl-setter ,val)))
                      ,tmp-body)))
        (call-update-expander pl-getter pl-setter place env
          ^(macrolet ((,tmp-place () ^(,',pl-getter)))
             ,(sys:expand
                ^(symacrolet ((,sym (,tmp-place)))
                   ,*body)
                env))))
      (remhash *place-update-expander* tmp-place))))

(defmacro placelet* (sym-place-pairs . body)
  (tree-case sym-place-pairs
    (() ^(progn ,*body))
    (((sym place)) ^(sys:placelet-1 ((,sym ,place)) ,*body))
    (((sym place) . rest-pairs) ^(sys:placelet-1 ((,sym ,place))
                                   (placelet* (,*rest-pairs) ,*body)))
    (obj (throwf 'eval-error "placelet*: bad syntax: ~s" obj))))

(defmacro placelet (sym-place-pairs . body)
  (unless (all sym-place-pairs
               [andf consp (opip length (= 2)) (oand first bindable)])
    (throwf 'eval-error "placelet: bad syntax: ~s" sym-place-pairs))
  (tree-bind (: syms places) (transpose sym-place-pairs)
     (let ((temps (mapcar (ret (gensym)) syms)))
       ^(placelet* (,*(zip temps places))
          (symacrolet (,*(zip syms temps))
            ,*body)))))

(defun sys:register-simple-accessor (get-fun set-fun)
  (sethash *place-update-expander* get-fun
           (lambda (getter setter place body)
             (let* ((args (cdr place))
                    (temps (mapcar (ret (gensym)) args)))
               ^(let (,(zip temps args))
                  (macrolet ((,getter () ^(,',get-fun ,*',temps))
                             (,setter (val)
                               ^(,',set-fun ,*',temps ,val)))
                    ,body)))))
  (sethash *place-clobber-expander* get-fun
           (lambda (ssetter place body)
             ^(macrolet ((,ssetter (val)
                               ^(,',set-fun ,*(cdr ',place) ,val)))
                ,body)))
  get-fun)

(defmacro define-accessor (get-fun set-fun)
  ^(sys:register-simple-accessor ',get-fun ',set-fun))

(define-place-macro first (obj) ^(car ,obj))
(define-place-macro rest (obj) ^(cdr ,obj))
(define-place-macro second (obj) ^(ref ,obj 1))
(define-place-macro third (obj) ^(ref ,obj 2))
(define-place-macro fourth (obj) ^(ref ,obj 3))
(define-place-macro fifth (obj) ^(ref ,obj 4))
(define-place-macro sixth (obj) ^(ref ,obj 5))
(define-place-macro seventh (obj) ^(ref ,obj 6))
(define-place-macro eighth (obj) ^(ref ,obj 7))
(define-place-macro ninth (obj) ^(ref ,obj 8))
(define-place-macro tenth (obj) ^(ref ,obj 9))
