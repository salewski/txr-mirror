;; Copyright 2019-2023
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

(compile-only
  (load-for (struct sys:param-parser-base "param")))

(defun mac-env-flatten (env)
  (when env
    (let ((lexvars [mapcar car
                           [keep-if (op eq 'sys:special)
                                    (env-vbindings env) cdr]]))
      (append (mac-env-flatten (env-next env)) lexvars))))

(defun analyze-params (params)
  (let* ((env (gensym))
         (lam ^(lambda ,params
                (macrolet ((,env (:env e)
                             (set (symbol-value ',env) e)))
                  (,env))))
         (explam (expand lam))
         (syms (mac-env-flatten (symbol-value env))))
    (list (cadr explam) syms)))

(defun defset-expander-simple (macform get-fun set-fun)
  (with-gensyms (getter setter params)
    ^(defplace (,get-fun . ,params) body
       (,getter ,setter
          (let ((pgens (mapcar (ret (gensym)) ,params)))
            ^(alet ,(zip pgens (list ,*params))
               (macrolet ((,,getter () ^(,',',get-fun ,*',pgens))
                          (,,setter (val) ^(,',',set-fun ,*',pgens ,val)))
                 ,body)))))))

(defun defset-expander (env macform name params newval setform)
  (with-gensyms (getter setter args gpf-pairs gpr-pairs ext-pairs
                 pgens rgens egens all-pairs agens nvsym)
    (let* ((ap (analyze-params params))
           (exp-params (car ap))
           (total-syms (cadr ap))
           (fp (new fun-param-parser form macform syntax exp-params))
           (fixpars (append fp.req fp.(opt-syms)))
           (restpar (if (symbol-package fp.rest) fp.rest))
           (extsyms [keep-if symbol-package
                             (diff total-syms (cons restpar fixpars))])
           (xsetform ^^(alet ((,',nvsym ,,newval))
                         ,,(expand ^(symacrolet ((,newval ',nvsym))
                                      ,setform)
                                   env))))
      ^(defplace (,name . ,args) body
         (,getter ,setter
           (tree-bind ,params ,args
             (let* ((,gpf-pairs (mapcar (op (fun list) (gensym)) (list ,*fixpars)))
                    (,gpr-pairs (if ',restpar
                                  (if (consp ,restpar)
                                    (mapcar (op (fun list) (gensym)) ,restpar)
                                    (list (list (gensym) ,restpar)))))
                    (,ext-pairs (mapcar (op (fun list) (gensym)) (list ,*extsyms)))
                    (,pgens (mapcar (fun car) ,gpf-pairs))
                    (,rgens (mapcar (fun car) ,gpr-pairs))
                    (,egens (mapcar (fun car) ,ext-pairs))
                    (,all-pairs (append ,gpf-pairs ,gpr-pairs ,ext-pairs))
                    (,agens (collect-each ((a ,args))
                             (let ((p (pos a ,all-pairs (fun eq) (fun cadr))))
                               (if p
                                 (car (del [,all-pairs p]))
                                 a)))))
               ^(alet (,*,gpf-pairs ,*,gpr-pairs ,*,ext-pairs)
                  ,(expand ^(symacrolet (,*(zip ',fixpars
                                                (mapcar (ret ^',@1) ,pgens))
                                         ,*(zip ',extsyms
                                                (mapcar (ret ^',@1) ,egens))
                                         ,*(if ,gpr-pairs
                                             (if (consp ,restpar)
                                               ^((,',restpar ',,rgens))
                                               ^((,',restpar ',(car ,rgens))))))
                              (macrolet ((,,getter () ^(,',',name ,',*,agens))
                                         (,,setter (,',newval)
                                            ,',xsetform))
                                ,body))
                           ,env)))))))))

(defmacro usr:defset (:env e :form mf . args)
  (tree-case args
    ((name (. params) newval setform)
     (defset-expander e mf . args))
    ((get-fun set-fun)
     (defset-expander-simple mf get-fun set-fun))
    (x (compile-error mf "invalid syntax"))))

(defset sub-list (list : (from 0) (to t)) items
  ^(progn (set ,list (replace-list ,list ,items ,from ,to)) ,items))

(defset sub-vec (vec : (from 0) (to t)) items
  ^(progn (replace-vec ,vec ,items ,from ,to) ,items))

(defset sub-str (str : (from 0) (to t)) items
  ^(progn (replace-str ,str ,items ,from ,to) ,items))

(defset left (node) nleft
  ^(progn (set-left ,node ,nleft) ,nleft))

(defset right (node) nright
  ^(progn (set-right ,node ,nright) ,nright))

(defset key (node) nkey
  ^(progn (set-key ,node ,nkey) ,nkey))

(defmacro set-mask (:env env place . integers)
  (with-update-expander (getter setter) place env
    ^(,setter (logior (,getter) ,*integers))))

(defmacro clear-mask (:env env place . integers)
  (with-update-expander (getter setter) place env
    ^(,setter (logand (,getter) (lognot (logior ,*integers))))))
