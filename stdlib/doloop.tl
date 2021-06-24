;; Copyright 2017-2021
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

(defun sys:expand-doloop (f vars cexp body)
  (let ((xvars (mapcar (tc
                         (((:whole w v i s . r))
                          (if r (compile-error f "excess elements in ~s" w) w))
                         (((:whole w v i . r))
                          (if r
                            (compile-error f "bad variable clause syntax ~s" w)
                            ^(,v ,i ,i)))
                         (((:whole w v . r))
                          (if r
                            (compile-error f "bad variable clause syntax ~s" w)
                            ^(,v nil ,v)))
                         ((v) ^(,v nil ,v)))
                       vars))
        (pllel (eq (car f) 'doloop)))
    ^(,(if pllel 'for 'for*)
       ,(mapcar (aret ^(,@1 ,@2)) xvars)
       ,cexp
       ((,(if pllel 'pset 'set) ,*(mappend (ado unless (eq @1 @3)
                                             ^(,@1 ,@3))
                                           xvars)))
       (tagbody ,*body))))

(defmacro doloop (:form f vars cexp . body)
  (sys:expand-doloop f vars cexp body))

(defmacro doloop* (:form f vars cexp . body)
  (sys:expand-doloop f vars cexp body))
