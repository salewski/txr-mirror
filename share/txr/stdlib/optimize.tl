;; Copyright 2021
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
  (defstruct (basic-blocks insns) nil
    insns
    (hash (hash))
    labels
    list
    (:static start (gensym "start-"))

    (:postinit (bb)
      (set bb.list (partition (dedup-labels (cons bb.start bb.insns))
                              (op where symbolp)))
      (set bb.labels [mapcar car bb.list])
      (mapdo (do set [bb.hash (car @1)] @1) bb.list))

    (:method get-insns (bb)
      [mappend bb.hash bb.labels])))

(defmacro rewrite-case (sym list . cases)
  ^(rewrite (lambda (,sym)
              (match-case ,sym
                ,*cases))
            ,list))

(defmeth basic-blocks thread-jumps (bb)
  (dohash (label code bb.hash)
    (set [bb.hash label]
         (rewrite-case insns code
           (((jmp @jlabel) . @rest)
            (let ((jinsns [bb.hash jlabel]))
              (match-case jinsns
                ((@(op eq jlabel)
                   (jmp @(require @jjlabel (neq jjlabel jlabel))) . @nil)
                 ^((jmp ,jjlabel) ,*rest))
                (@jelse insns))))
           (((if @reg @jlabel) . @rest)
            (let ((jinsns [bb.hash jlabel]))
              (match-case jinsns
                ((@(op eq jlabel)
                   (if @(op eq reg)
                       @(require @jjlabel (neq jjlabel jlabel))) . @nil)
                 ^((if ,reg ,jjlabel) ,*rest))
                ((@(op eq jlabel)
                   (jmp @(require @jjlabel (neq jjlabel jlabel))) . @nil)
                 ^((if ,reg ,jjlabel) ,*rest))
                (@jelse insns))))
           (((ifq @reg @creg @jlabel) . @rest)
            (let ((jinsns [bb.hash jlabel]))
              (match-case jinsns
                ((@(op eq jlabel)
                   (ifq @(op eq reg) @(op eq creg)
                        @(require @jjlabel (neq jjlabel jlabel))) . @nil)
                 ^((ifq ,reg ,creg ,jjlabel) ,*rest))
                ((@(op eq jlabel)
                   (jmp @(require @jjlabel (neq jjlabel jlabel))) . @nil)
                 ^((ifq ,reg ,creg ,jjlabel) ,*rest))
                (@jelse insns))))
           (((close @reg @nargs @jlabel . @cargs) . @rest)
            (let ((jinsns [bb.hash jlabel]))
              (match-case jinsns
                ((@(op eq jlabel)
                   (jmp @(require @jjlabel (neq jjlabel jlabel))) . @nil)
                 ^((close ,reg ,nargs ,jjlabel ,*cargs) ,*rest))
                (@jelse insns))))
           (@else insns)))))

(defun rewrite (fun list)
  (build
    (while* list
      (let ((nlist [fun list]))
        (if (eq list nlist)
          (if list (add (pop list)))
          (set list nlist))))))

(defun subst (x y list)
  (mapcar (lambda (item)
            (if (equal item x) y item))
          list))

(defun dedup-labels (insns)
  (rewrite (lambda (tail)
              (match-case tail
                ((@(symbolp label0) @(symbolp label1) . @rest)
                 (set insns (mapcar [iffi listp (op subst label1 label0)]
                                    (remq label1 insns)))
                 (list* label0 rest))
                (@else tail)))
            insns)
  insns)
