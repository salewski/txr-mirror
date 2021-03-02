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
  (defstruct live-info nil
    (defined 0)
    (used 0)
    def)

  (defstruct basic-block (live-info)
    live
    label
    next
    links
    rlinks
    insns)

  (defstruct (basic-blocks insns lt-dregs) nil
    insns
    lt-dregs
    root
    (hash (hash))
    (li-hash (hash :eq-based))
    list
    rescan
    (:static start (gensym "start-"))
    (:static jump-ops '(jmp if ifq ifql close swtch ret abscsr
                        uwprot catch block jend))

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
        (set bb.list (mapcar (do new basic-block
                               insns @1 label (car @1))
                             lparts))
        (mapdo (do set [bb.hash @1.label] @1) bb.list))
      bb.(link-graph))

    (:method get-insns (bb)
      [mappend .insns bb.list])

    (:method cut-block (bb bl at insns)
      (let* ((nlabel (gensym "nl"))
             (ltail (cdr (memq bl bb.list)))
             (nbl (new basic-block
                       label nlabel
                       insns (cons nlabel at))))
        (set bb.list (append (ldiff bb.list ltail) (list nbl) ltail))
        (set bl.insns (ldiff insns at))
        (set [bb.hash nlabel] nbl)
        (push bl bb.rescan)
        nbl))

    (:method next-block (bb bl)
      (let ((ltail (memq bl bb.list)))
        (iflet ((next (cdr ltail)))
          (car next))))

    (:method join-block (bb bl nxbl)
      (when (eql (car nxbl.insns) nxbl.label)
        (pop nxbl.insns))
      (set bl.insns (append bl.insns nxbl.insns))
      (set bl.next nxbl.next)
      (set bl.links nxbl.links)
      (set bb.list (remq nxbl bb.list))
      (del [bb.hash nxbl.label]))))

(defmacro rewrite-case (sym list . cases)
  ^(rewrite (lambda (,sym)
              (match-case ,sym
                ,*cases))
            ,list))

(defmeth basic-blocks link-graph (bb)
  (set bb.root (car bb.list))
  (each ((bl bb.list))
    (let* ((code bl.insns)
           (tail (last code))
           (linsn (car tail))
           (link-next t)
           (nxbl (cadr (memq bl bb.list))))
      (set bl.next nxbl)
      (match-case linsn
        ((jmp @jlabel)
         (set bl.links (list [bb.hash jlabel])
              bl.next nil))
        ((if @nil @jlabel)
         (set bl.links (list [bb.hash jlabel])))
        ((@(or ifq ifql) @nil @nil @jlabel)
         (set bl.links (list [bb.hash jlabel])))
        ((close @nil @nil @nil @jlabel . @nil)
         (set bl.links (list [bb.hash jlabel])
              link-next nil))
        ((swtch @nil . @jlabels)
         (set bl.links [mapcar bb.hash (uniq jlabels)]
              bl.next nil))
        ((catch @nil @nil @nil @nil @hlabel)
         (set bl.links (list [bb.hash hlabel])))
        ((block @nil @nil @slabel)
         (set bl.links (list [bb.hash slabel])))
        ((uwprot @clabel)
         (set bl.links (list [bb.hash clabel])))
        ((@(or abscsr ret jend) . @nil)
         (set bl.next nil)))
      (if (and bl.next link-next)
        (pushnew bl.next bl.links))
      (each ((nxbl bl.links))
        (pushnew bl nxbl.rlinks)))))

(defmeth basic-blocks local-liveness (bb bl)
  (labels ((regnum (reg)
             (when-match (t @num) reg num))
           (regnums (regs)
             (mappend (do when-match
                        (t @num) @1 (list num)) regs))
           (def (li insn def)
             (set li (copy li)
                  li.def def
                  [bb.li-hash insn] li)
             (let* ((dn (regnum def))
                    (dmask (if dn (mask dn))))
               (cond
                 (dn (new live-info
                          used (logand li.used (lognot dmask))
                          defined (logior li.defined dmask)))
                 (t (set [bb.li-hash insn] li)))))
           (refs (li insn . refs)
             (set [bb.li-hash insn] li)
             (let* ((rn (regnums refs))
                    (rmask (mask . rn)))
               (new live-info
                    used (logior li.used rmask)
                    defined (logand li.defined (lognot rmask)))))
           (def-ref (li insn def . refs)
             (set li (copy li)
                  li.def def
                  [bb.li-hash insn] li)
             (let* ((rn (regnums refs))
                    (dn (regnum def))
                    (dmask (if dn (mask dn)))
                    (rmask (mask . rn)))
               (cond
                 (dn (new live-info
                          used (logior (logand li.used (lognot dmask)) rmask)
                          defined (logior (logand li.defined (lognot rmask)) dmask)))
                 (t (set [bb.li-hash insn] li)
                    (new live-info
                         used (logior li.used rmask)
                         defined (logand li.defined (lognot rmask)))))))
           (liveness (insns)
             (if (null insns)
               (new live-info used 0)
               (let* ((li (liveness (cdr insns)))
                      (insn (car insns)))
                 (match-case insn
                   ((@(or end jend prof) @reg)
                    (refs li insn reg))
                   ((@(or apply call) @def . @refs)
                    (def-ref li insn def . refs))
                   ((@(or gapply gcall) @def @fidx . @refs)
                    (def-ref li insn def . refs))
                   ((mov @def @ref)
                    (def-ref li insn def ref))
                   ((if @reg . @nil)
                    (refs li insn reg))
                   ((@(or ifq ifql) @reg @creg . @nil)
                    (refs li insn reg creg))
                   ((swtch @reg . @nil)
                    (refs li insn reg))
                   ((block @reg @nreg . @nil)
                    (refs li insn reg nreg))
                   ((@(or ret abscsr) @nreg @reg)
                    (refs li insn reg nreg))
                   ((catch @esreg @eareg @syreg @descreg . @nil)
                    (refs li insn esreg eareg syreg descreg))
                   ((handle @funreg @syreg)
                    (refs li insn funreg syreg))
                   ((@(or getv getvb getfb getl1b getlx getf) @def . @nil)
                    (def li insn def))
                   ((@(or setv setl1 setlx bindv) @reg . @nil)
                    (refs li insn reg))
                   ((close @reg . @nil)
                    (def li insn reg))
                   ((@op . @nil)
                    (caseq op
                      ((end jend prof or apply call or gapply gcall mov if
                        ifq ifql swtch block ret abscsr catch handle getv
                        getvb getfb getl1b getlx getf setl1 setlx bindv close)
                       (error `wrongly handled @insn instruction`))
                      (t (set [bb.li-hash insn] li))))
                   (@else (set [bb.li-hash insn] li)))))))
    (let ((li (liveness bl.insns)))
      (set bl.used li.used
           bl.defined li.defined))))

(defmeth basic-blocks calc-liveness (bb)
  (each ((bl bb.list))
    bb.(local-liveness bl))
  (let (changed)
    (while* changed
      (let ((visited (hash :eq-based)))
        (labels ((upd-used (bl insns live)
                   (tree-case insns
                     ((fi . re)
                      (let* ((live (upd-used bl re live))
                             (lif [bb.li-hash fi]))
                        (set live (logand live (lognot lif.defined)))
                        (set lif.used (logior live lif.used))
                        live))
                     (else live)))
                 (visit (bl)
                   (unless [visited bl]
                     (set [visited bl] t)
                     (when bl.next
                       (visit bl.next))
                     (let ((used 0)
                           (old-live (or bl.live 0)))
                       (each ((nx bl.links))
                         (visit nx)
                         (set used (logior used nx.used)))
                       (when (neql (set bl.live (logior used old-live))
                                   old-live)
                         (let ((live-in (logand (upd-used bl bl.insns bl.live)
                                                (lognot bl.defined))))
                           (set bl.used (logior live-in bl.used)))
                         (set changed t))))))
          (set changed nil)
          (visit bb.root))))))

(defmeth basic-blocks thread-jumps-block (bb code)
  (let* ((tail (last code))
         (oinsn (car tail))
         (insn oinsn)
         (ninsn oinsn))
    (while* (nequal ninsn insn)
      (set insn ninsn
           ninsn (match-case insn
                   (@(require (if @(as reg (d @dn)) @jlabel)
                              (not (memqual reg bb.lt-dregs)))
                    nil)
                   ((if (t 0) @jlabel)
                    ^(jmp ,jlabel))
                   ((jmp @jlabel)
                    (let ((jinsns [bb.hash jlabel].insns))
                      (match-case jinsns
                        ((@jlabel
                           (jmp @(and @jjlabel @(not @jlabel))) . @nil)
                         ^(jmp ,jjlabel))
                        (@jelse insn))))
                   ((if @reg @jlabel)
                    (let ((jinsns [bb.hash jlabel].insns))
                      (match-case jinsns
                        ((@jlabel
                           (if @reg
                             @(and @jjlabel @(not @jlabel))) . @nil)
                         ^(if ,reg ,jjlabel))
                        ((@jlabel
                           (jmp @(and @jjlabel @(not @jlabel))) . @nil)
                         ^(if ,reg ,jjlabel))
                        ((@jlabel
                           (ifq @reg (t 0) @jjlabel) . @jrest)
                         (let ((xbl (if jrest
                                      bb.(cut-block [bb.hash jlabel] jrest jinsns)
                                      bb.(next-block [bb.hash jlabel]))))
                           (if xbl
                             ^(if ,reg ,xbl.label)
                             insn)))
                        (@jelse insn))))
                   ((ifq @reg @creg @jlabel)
                    (let ((jinsns [bb.hash jlabel].insns))
                      (match-case jinsns
                        ((@jlabel
                           (ifq @reg @creg
                                @(and @jjlabel @(not @jlabel))) . @nil)
                         ^(ifq ,reg ,creg ,jjlabel))
                        ((@jlabel
                           (jmp @(and @jjlabel @(not @jlabel))) . @nil)
                         ^(ifq ,reg ,creg ,jjlabel))
                        (@jelse insn))))
                   ((close @reg @frsize @ntregs @jlabel . @cargs)
                    (let ((jinsns [bb.hash jlabel].insns))
                      (match-case jinsns
                        ((@jlabel
                           (jmp @(and @jjlabel @(not @jlabel))) . @nil)
                         ^(close ,reg ,frsize ,ntregs ,jjlabel ,*cargs))
                        (@jelse insn))))
                   (@else else))))
    (cond
      ((null ninsn) (ldiff code tail))
      ((nequal ninsn oinsn) (append (ldiff code tail) (list ninsn)))
      (t code))))

(defun subst (x y list)
  (mapcar (lambda (item)
            (if (equal item x) y item))
          list))

(defun subst-preserve (x y bb li list)
  (let ((sub (subst x y list)))
    (cond
      ((equal sub list) list)
      (t (set [bb.li-hash sub] li) sub))))

(defmeth basic-blocks peephole-block (bb bl code)
  (rewrite-case insns code
    ;; dead t-reg
    (@(require ((mov (t @n) . @nil) . @nil)
               (let ((li [bb.li-hash (car insns)]))
                 (and li (not (bit li.used n)))))
      (cdr insns))
    ;; unnecessary copying t-reg
    (@(require ((mov @(as dst (t @n)) @src) . @rest)
               (let ((li [bb.li-hash (car insns)]))
                 (and li (bit li.used n) (not (bit bl.live n))))
               (or (neq (car src) 'v)
                   (none rest [andf [chain car (op eq 'end)]
                                    [chain bb.li-hash .used (lop bit n)]]))
               (not (find dst rest : [chain bb.li-hash .def]))
               (not (find src rest : [chain bb.li-hash .def])))
      (labels ((rename (insns n dst src)
                 (tree-case insns
                   ((fi . re)
                    (cons (subst-preserve dst src bb [bb.li-hash fi] fi)
                          (rename (cdr insns) n dst src)))
                   (else else))))
        (rename (cdr insns) n dst src)))
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
     (let ((jinsns [bb.hash jlabel].insns))
       (match-case jinsns
         ((@jlabel
            (end (t @reg)) . @jrest)
          (let* ((xbl (if jrest
                         bb.(cut-block [bb.hash jlabel] jrest jinsns)
                         bb.(next-block [bb.hash jlabel])))
                 (ybl bb.(next-block bl))
                 (yinsns ybl.insns))
            (cond
              ((and xbl ybl)
               (set ybl.insns ^(,ybl.label ,(car insns) ,*(cdr yinsns)))
               (push ybl bb.rescan)
               ^((if (t ,reg) ,xbl.label)))
              (t insns))))
         (@jelse insns))))
    (@(require ((if @(as reg (d @dn)) @jlabel) . @nil)
               (not (memqual reg bb.lt-dregs)))
      nil)
    (@(require ((ifq @(as reg (d @dn)) (t 0) @jlabel) . @nil)
               (not (memqual reg bb.lt-dregs)))
      ^((jmp ,jlabel)))
    (@else insns)))

(defmeth basic-blocks peephole (bb)
  (each ((bl bb.list))
    (set bl.insns bb.(peephole-block bl bl.insns)))
  (whilet ((rescan bb.rescan))
    (set bb.rescan nil)
    (each ((bl rescan))
      (set bl.insns bb.(peephole-block bl bl.insns)))))

(defmeth basic-blocks thread-jumps (bb)
  (each ((bl bb.list))
    (set bl.insns bb.(thread-jumps-block bl.insns))))

(defmeth basic-blocks elim-next-jump (bb bl)
  (let* ((tail (last bl.insns))
         (linsn (car tail)))
    (when-match (jmp @jlabel) linsn
      (let ((nxbl bb.(next-block bl)))
        (when (eql nxbl.?label jlabel)
          (set bl.insns (butlast bl.insns)))))))

(defmeth basic-blocks join-blocks (bb)
  (labels ((join (list)
             (tree-case list
               ((bl nxbl . rest)
                (cond
                  ((and (eq bl.next nxbl)
                        (eq (car bl.links) nxbl)
                        (null (cdr bl.links))
                        (null (cdr nxbl.rlinks)))
                   bb.(join-block bl nxbl)
                   (join (cons bl rest)))
                  (t (cons bl (join (cdr list))))))
               (else else))))
    (set bb.list (join bb.list))))

(defmeth basic-blocks elim-dead-code (bb)
  (each ((bl bb.list))
    (set bl.links nil)
    (set bl.rlinks nil))
  bb.(link-graph)
  (let* ((visited (hash :eq-based))
         (reachable (build
                      (labels ((visit (bl)
                                 (when (test-set [visited bl])
                                   (add bl)
                                   (when bl.next
                                     (visit bl.next))
                                   [mapcar visit bl.links])))
                        (for ((bl bb.root)) (bl) ((set bl bl.next))
                          (add bl)
                          (visit bl))
                        (visit bb.root)))))
    (set bb.list [keep-if visited bb.list])
    (each ((bl bb.list))
      bb.(elim-next-jump bl)))
  bb.(join-blocks))

(defun rewrite (fun list)
  (build
    (while* list
      (let ((nlist [fun list]))
        (if (eq list nlist)
          (if list (add (pop list)))
          (set list nlist))))))

(defun dedup-labels (insns)
  (rewrite-case tail insns
    ((@(symbolp @label0) @(symbolp @label1) . @rest)
     (set insns (mapcar [iffi listp (op subst label1 label0)]
                        (remq label1 insns)))
     (cons label0 rest))
    (@else tail))
  insns)
