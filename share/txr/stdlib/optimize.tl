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
    rescan
    (:static start (gensym "start-"))
    (:static jump-ops '(jmp if ifq ifql close swtch ret abscsr))

    (:postinit (bb)
      (let* ((insns (dedup-labels (cons bb.start bb.insns)))
             (cuts (merge [where symbolp insns]
                          [where [andf consp
                                       (op memq (car @1) bb.jump-ops)]
                                 (cons nil insns)]))
             (parts (partition insns cuts))
             (lparts (mapcar [iff [chain car symbolp]
                                  use
                                  (op cons (gensym))]
                             parts)))
        (set bb.list lparts)
        (set bb.labels [mapcar car lparts])
        (mapdo (do set [bb.hash (car @1)] @1) lparts)))

    (:method get-insns (bb)
      [mappend bb.hash bb.labels])

    (:method cut-block (bb label at insns)
      (let ((nlabel (gensym "nl"))
            (ltail (cdr (member label bb.labels))))
        (set bb.labels (append (ldiff bb.labels ltail)
                               (list nlabel)
                               ltail))
        (set [bb.hash nlabel] (cons nlabel at))
        (set [bb.hash label] (ldiff insns at))
        (push nlabel bb.rescan)
        nlabel))

    (:method next-block (bb label)
      (let ((ltail (member label bb.labels)))
        (iflet ((next (cdr ltail)))
          (car next))))))

(defmacro rewrite-case (sym list . cases)
  ^(rewrite (lambda (,sym)
              (match-case ,sym
                ,*cases))
            ,list))

(defmeth basic-blocks peephole-block (bb label code)
  (rewrite-case insns code
    ;; dead code
    ((@(or (jmp @nil) (if (t 0) @nil)) @nil . @rest)
     (list (car insns)))
    ;; always taken if
    (((if (d @reg) @jlabel) . @rest)
     rest)
    ;; jump threading
    (((jmp @jlabel) . @rest)
     (let ((jinsns [bb.hash jlabel]))
       (match-case jinsns
         ((@jlabel
            (jmp @(and @jjlabel @(not @jlabel))) . @nil)
          ^((jmp ,jjlabel) ,*rest))
         (@jelse insns))))
    (((if @reg @jlabel0)
      (jmp @(with @jlabel1
                  @(hash (@jlabel1 (@jlabel1
                                     (if @reg @nil)
                                     (jmp @jlabel2) . @nil)))
                  bb.hash)) . @rest)
     ^(,(car insns) (jmp ,jlabel2) ,*rest))
    (((if @reg @jlabel) . @rest)
     (let ((jinsns [bb.hash jlabel]))
       (match-case jinsns
         ((@jlabel
            (if @reg
              @(and @jjlabel @(not @jlabel))) . @nil)
          ^((if ,reg ,jjlabel) ,*rest))
         ((@jlabel
            (jmp @(and @jjlabel @(not @jlabel))) . @nil)
          ^((if ,reg ,jjlabel) ,*rest))
         ((@jlabel
            (ifq @reg nil @jjlabel) . @jrest)
          (let ((xlabel (if jrest
                          bb.(cut-block jlabel jrest jinsns)
                          bb.(next-block jlabel))))
            (if xlabel
              ^((if ,reg ,xlabel) ,*rest)
              insns)))
         (@jelse insns))))
    (((ifq @reg @creg @jlabel) . @rest)
     (let ((jinsns [bb.hash jlabel]))
       (match-case jinsns
         ((@jlabel
            (ifq @reg @creg
                 @(and @jjlabel @(not @jlabel))) . @nil)
          ^((ifq ,reg ,creg ,jjlabel) ,*rest))
         ((@jlabel
            (jmp @(and @jjlabel @(not @jlabel))) . @nil)
          ^((ifq ,reg ,creg ,jjlabel) ,*rest))
         (@jelse insns))))
    (((close @reg @nargs @jlabel . @cargs) . @rest)
     (let ((jinsns [bb.hash jlabel]))
       (match-case jinsns
         ((@jlabel
            (jmp @(and @jjlabel @(not @jlabel))) . @nil)
          ^((close ,reg ,nargs ,jjlabel ,*cargs) ,*rest))
         (@jelse insns))))
    ;; wasteful moves
    (((mov @reg0 @nil) (mov @reg0 @nil) . @nil)
     (cdr insns))
    (((mov @reg0 @reg1) (mov reg1 @reg0) . @rest)
     ^(,(car insns) ,*rest))
    ;; frame reduction
    (((@(or frame dframe) @lev @size)
      (@(or call gcall mov)
        . @(require @(coll (v @vlev @nil))
                    (none vlev (op eql (ppred lev)))))
      . @rest)
     ^(,(cadr insns) ,(car insns) ,*rest))
    (((@(or frame dframe) . @nil)
      (if (t @reg) @jlabel))
     (let ((jinsns [bb.hash jlabel]))
       (match-case jinsns
         ((@jlabel
            (end (t @reg)) . @jrest)
          (let* ((xlabel (if jrest
                           bb.(cut-block jlabel jrest jinsns)
                           bb.(next-block jlabel)))
                 (ylabel bb.(next-block label))
                 (yinsns [bb.hash ylabel]))
            (cond
              ((and xlabel ylabel)
               (set [bb.hash ylabel]
                    ^(,ylabel ,(car insns) ,*(cdr yinsns)))
               ^((if (t ,reg) ,xlabel)))
              (t insns))))
         (@jelse insns))))
    (@else insns)))

(defmeth basic-blocks peephole (bb)
  (dohash (label code bb.hash)
    (set [bb.hash label]
         bb.(peephole-block label code)))
  (whilet ((rescan bb.rescan))
    (set bb.rescan nil)
    (each ((label rescan))
      (set [bb.hash label]
           bb.(peephole-block label [bb.hash label])))))

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
  (rewrite-case tail insns
    ((@(symbolp @label0) @(symbolp @label1) . @rest)
     (set insns (mapcar [iffi listp (op subst label1 label0)]
                        (remq label1 insns)))
     (cons label0 rest))
    (@else tail))
  insns)
