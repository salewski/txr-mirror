;; Copyright 2015-2016
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

(defun sys:handle-bad-syntax (item)
  (throwf 'eval-error "~s: bad clause syntax: ~s" 'handle item))

(defmacro handle (:whole form try-form . handle-clauses)
  (let* ((exc-sym (gensym))
         (exc-args (gensym))
         (syms-fragments (collect-each ((hc handle-clauses))
                          (tree-case hc
                            ((name arglist . body)
                             (unless (symbolp name)
                               (sys:handle-bad-syntax hc))
                             (list name ^(apply (lambda ,arglist ,*body)
                                                ,exc-sym ,exc-args)))
                            (else (sys:handle-bad-syntax hc))))))
    ^(handler-bind (lambda (,exc-sym . ,exc-args)
                     (cond
                       ,*(mapcar (aret ^((exception-subtype-p ,exc-sym ',@1) ,@2))
                                 syms-fragments)))
                   ,[mapcar car syms-fragments]
                   ,try-form)))
