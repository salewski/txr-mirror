;; Copyright 2019
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

(defun defset-expander (env macform name params newval getform setform)
  (let* ((fp (new fun-param-parser form macform syntax params))
         (fixpars (append fp.req fp.(opt-syms)))
         (restpar fp.rest))
    (with-gensyms (getter setter)
      ^(defplace (,name ,*params) body
         (,getter ,setter
            (let* ((gpf-pairs (append (mapcar (op list (gensym) @1)
                                              (list ,*fixpars))))
                   (gpr-pairs (if ',restpar
                                (mapcar (ret ^(,(gensym) ,@1)) ,restpar)))
                   (pgens [mapcar car gpf-pairs])
                   (rgens [mapcar car gpr-pairs]))
              ^(alet (,*gpf-pairs ,*gpr-pairs)
                 ,(expand ^(symacrolet (,*(zip ',fixpars
                                               (mapcar (ret ^',@1) pgens))
                                         ,*(if gpr-pairs
                                             ^((,',restpar ',rgens))))
                             (macrolet ((,,getter () ,',getform)
                                        (,,setter (,',newval) ,',setform))
                               ,body))
                          ,env))))))))

(defun defset-expander-hairy (env macform name params newval getform setform)
  (compile-error macform "param list macro support is being researched"))

(defmacro usr:defset (:env e :form mf . args)
  (tree-case args
    ((name (param . params) newval getform setform)
     (if (and (keywordp param) (neq : param))
       (defset-expander-hairy e mf . args)
       (defset-expander e mf . args)))
    ((name (. params) newval getform setform)
     (defset-expander e mf . args))
    ((get-fun set-fun)
     (defset-expander-simple mf get-fun set-fun))
    (x (compile-error mf "invalid syntax"))))
