;; Copyright 2023
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

(defun brace-expand (str)
  (bexp-expand (bexp-parse str)))

(defstruct bexp-parse-ctx ()
  str
  toks)

(defun bexp-parse (str)
  (let ((ctx (new bexp-parse-ctx
                   str str
                   toks (remqual "" (tok #/([{},]|{}|\\\\|\\.)/ t str)))))
    (build
      (whilet ((next (pop ctx.toks)))
        (add
          (if (equal next "{")
            (bexp-parse-brace ctx)
            next))))))

(defun bexp-parse-brace (ctx)
  (buildn
    (caseq (whilet ((next (pop ctx.toks)))
             (casequal next
               ("{" (add (bexp-parse-brace ctx)))
               ("}" (return :ok))
               (t (add next))))
      (:ok
        (cond
          ((memqual "," (get))
           (flow (get)
             (split* @1 (op where (op equal ",")))
             (cons '/)))
          (t
            (add* "{")
            (add "}")
            (get))))
      (nil
        (add* "{")
        (get)))))

(defun bexp-expand (tree : (path (new list-builder)))
  (build
    (match-case tree
      (() (add (cat-str path.(get))))
      (((/ . @alt) . @rest)
       (let ((saved-path path.(get)))
         (each ((elem alt))
           path.(oust saved-path)
           (pend (bexp-expand (cons elem rest) path)))))
      ((@(consp @succ) . @rest)
       (pend (bexp-expand (append succ rest) path)))
      ((@head . @rest)
       path.(add head)
       (pend (bexp-expand rest path))))))


(defun glob* (pattern-or-patterns : (flags 0))
  (let ((xflags (logior flags sys:glob-xstar))
        (patterns (if (listp pattern-or-patterns)
                    pattern-or-patterns
                    (list pattern-or-patterns))))
    (if (or (logtest flags glob-xnobrace)
            (null (find-if (op find #\{) patterns)))
      (glob patterns xflags)
      (let ((xpatterns [mappend brace-expand patterns]))
        (append-each ((p xpatterns))
          (glob p xflags))))))
