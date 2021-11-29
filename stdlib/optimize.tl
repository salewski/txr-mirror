;; Copyright 2021
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
    insns

    (:method print (bl stream pretty-p)
       (put-string "#S" stream)
       (print ^(basic-block live ,bl.live
                            label ,bl.label
                            insns ,bl.insns
                            links ,(mapcar .label bl.links)
                            rlinks ,(mapcar .label bl.rlinks)
                            next ,bl.next) stream)))

  (defstruct (basic-blocks insns lt-dregs symvec) nil
    insns
    lt-dregs
    symvec
    root
    (hash (hash))
    (li-hash (hash :eq-based))
    list
    rescan
    recalc
    reelim
    tryjoin
    (:static start (gensym "start-"))
    (:static jump-ops '(jmp if ifq ifql close swtch ret abscsr
                        uwprot catch block jend))

    (:postinit (bb)
      (let* ((insns (early-peephole (dedup-labels (cons bb.start bb.insns))))
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
        (pushnew bl bb.rescan)
        (pushnew nbl bb.rescan)
        nbl))

    (:method join-block (bb bl nxbl)
      (when (eql (car nxbl.insns) nxbl.label)
        (pop nxbl.insns))
      (set bl.insns (append bl.insns nxbl.insns)
           nxbl.insns nil
           bl.next nxbl.next
           bl.links nxbl.links
           bb.list (remq nxbl bb.list))
      (del [bb.hash nxbl.label])
      (each ((nx bl.links))
        (upd nx.rlinks (remq nxbl))
        (pushnew bl nx.rlinks)))))

(defmacro rewrite-case (sym list . cases)
  ^(rewrite (lambda (,sym)
              (match-case ,sym
                ,*cases))
            ,list))

(defmeth basic-blocks link-graph (bb)
  (set bb.root (car bb.list))
  (each* ((bl bb.list)
          (nxbl (append (cdr bl) '(nil))))
    (let* ((code bl.insns)
           (tail (last code))
           (linsn (car tail))
           (link-next t))
      (match-case linsn
        ((jmp @jlabel)
         (set bl.links (list [bb.hash jlabel])
              link-next nil))
        ((if @nil @jlabel)
         (set bl.links (list [bb.hash jlabel])))
        ((@(or ifq ifql) @nil @nil @jlabel)
         (set bl.links (list [bb.hash jlabel])))
        ((close @nil @nil @nil @jlabel . @nil)
         (set bl.links (list [bb.hash jlabel])
              bl.next nxbl
              link-next nil))
        ((swtch @nil . @jlabels)
         (set bl.links [mapcar bb.hash (uniq jlabels)]
              link-next nil))
        ((catch @nil @nil @nil @nil @hlabel)
         (set bl.links (list [bb.hash hlabel])))
        ((block @nil @nil @slabel)
         (set bl.links (list [bb.hash slabel])))
        ((uwprot @clabel)
         (set bl.links (list [bb.hash clabel])))
        ((@(or abscsr ret jend) . @nil)
         (set link-next nil)))
      (when (and nxbl link-next)
        (set bl.next nxbl)
        (pushnew nxbl bl.links))
      (each ((nx bl.links))
        (pushnew bl nx.rlinks)))))

(defmeth basic-blocks local-liveness (bb bl)
  (set bl.live nil)
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

(defmeth basic-blocks calc-liveness (bb : (blist bb.list))
  (each ((bl blist))
    bb.(local-liveness bl))
  (let (changed)
    (while* changed
      (let ((visited (hash :eq-based)))
        (labels ((upd-used (bl insns live)
                   (tree-case insns
                     ((fi . re)
                      (let* ((live (upd-used bl re live))
                             (lif [bb.li-hash fi]))
                        (clear-mask live lif.defined)
                        (set-mask lif.used live)
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
                         (set-mask used nx.used))
                       (when (neql (set bl.live (logior used old-live))
                                   old-live)
                         (let ((live-in (logand (upd-used bl bl.insns bl.live)
                                                (lognot bl.defined))))
                           (set-mask bl.used live-in))
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
                    (let* ((jbl [bb.hash jlabel]))
                      (match-case jbl.insns
                        ((@jlabel
                           (if @reg
                             @(and @jjlabel @(not @jlabel))) . @nil)
                         ^(if ,reg ,jjlabel))
                        ((@jlabel
                           (jmp @(and @jjlabel @(not @jlabel))) . @nil)
                         ^(if ,reg ,jjlabel))
                        ((@jlabel
                           (ifq @reg (t 0) @jjlabel) . @nil)
                         (let ((xbl jbl.next))
                           (if xbl
                             ^(if ,reg ,xbl.label)
                             insn)))
                        (@jelse insn))))
                   ((ifq @reg @creg @jlabel)
                    (let ((jbl [bb.hash jlabel]))
                      (match-case jbl.insns
                        ((@jlabel
                           (ifq @reg @creg
                                @(and @jjlabel @(not @jlabel))) . @nil)
                         ^(ifq ,reg ,creg ,jjlabel))
                        ((@(require @jlabel (equal creg '(t 0)))
                           (if @reg
                                @(and @jjlabel @(not @jlabel))) . @nil)
                         (let ((xbl jbl.next))
                           (if xbl
                             ^(ifq ,reg ,creg ,xbl.label)
                             insn)))
                        ((@jlabel
                           (jmp @(and @jjlabel @(not @jlabel))) . @nil)
                         ^(ifq ,reg ,creg ,jjlabel))
                        (@jelse insn))))
                   ((close @reg @frsize @ntregs @jlabel . @cargs)
                    (let ((jbl [bb.hash jlabel]))
                      (match-case jbl.insns
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
  (labels ((dead-treg (insn n)
             (let ((li [bb.li-hash insn]))
               (and li (not (bit li.used n)))))
           (only-locally-used-treg (insn n)
             (let ((li [bb.li-hash insn]))
               (and li (bit li.used n) (not (bit bl.live n))))))
    (rewrite-case insns code
      ;; dead t-reg
      (@(require ((@(or mov getlx getv getf getfb) (t @n) . @nil) . @nil)
                 (dead-treg (car insns) n))
        (pushnew bl bb.rescan)
        (set bb.recalc t)
        (cdr insns))
      (@(require ((close (t @n) @nil @nil @jlabel . @nil) . @nil)
                 (dead-treg (car insns) n))
        (pushnew bl bb.rescan)
        (set bb.recalc t
             bb.reelim t)
        ^((jmp ,jlabel)))
      (@(require ((@(or gcall gapply) (t @n) @idx . @nil) . @nil)
                 (dead-treg (car insns) n)
                 [%effect-free% [bb.symvec idx]])
        (pushnew bl bb.rescan)
        (set bb.recalc t)
        (cdr insns))
      ;; unnecessary copying t-reg
      (@(require ((mov @(as dst (t @n)) @src) . @rest)
                 (only-locally-used-treg (car insns) n)
                 (or (neq (car src) 'v)
                     (none rest [andf [chain car (op eq 'end)]
                                      [chain bb.li-hash .used (lop bit n)]]))
                 (not (find dst rest : [chain bb.li-hash .def]))
                 (not (find src rest : [chain bb.li-hash .def])))
        (pushnew bl bb.rescan)
        (set bb.recalc t)
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
       (pushnew bl bb.rescan)
       (set bb.recalc t)
       ^(,(car insns) ,*rest))
      ;; frame reduction
      (((@(or frame dframe) @lev @size)
        (@(or call gcall mov)
          . @(require @(coll (v @vlev @nil))
                      (none vlev (op eql (ppred lev)))))
        . @rest)
       ^(,(cadr insns) ,(car insns) ,*rest))
      (((@(or frame dframe) . @nil)
        (if (t @reg) @jlabel) . @nil)
       (let* ((jbl [bb.hash jlabel])
              (jinsns jbl.insns))
         (match-case jinsns
           ((@jlabel
              (end (t @reg)) . @jrest)
            (let* ((ybl bl.next)
                   (xbl (if ybl
                          (if jrest
                            bb.(cut-block jbl jrest jinsns)
                            jbl.next)))
                   (yinsns ybl.insns))
              (cond
                (xbl
                  (set ybl.insns ^(,ybl.label ,(car insns) ,*(cdr yinsns)))
                  (pushnew ybl bb.rescan)
                  (set bb.recalc t)
                  (set bb.links (list ybl xbl))
                  ^((if (t ,reg) ,xbl.label)))
                (t insns))))
           (@jelse insns))))
      (@(require ((if @(as reg (d @dn)) @jlabel) . @nil)
                 (not (memqual reg bb.lt-dregs)))
        (push bl bb.tryjoin)
        (push bl bb.rescan)
        (pushnew bl.next bb.rescan)
        (set bb.recalc t)
        nil)
      (@(require ((ifq @(as reg (d @dn)) (t 0) @jlabel) . @nil)
                 (not (memqual reg bb.lt-dregs)))
        (pushnew bl.next bb.rescan)
        (set bb.recalc t
             bl.next nil
             bl.links (list [bb.hash jlabel]))
        ^((jmp ,jlabel)))
      (((jmp @jlabel) . @nil)
       (let* ((jinsns (cdr [bb.hash jlabel].insns))
              (oinsns (match-case jinsns
                        (((jend @nil) . @nil)
                         ^(,(car jinsns)))
                        ((@nil (jend @nil) . @nil)
                         ^(,(car jinsns) ,(cadr jinsns)))
                        (@else insns))))
         (when (neq insns oinsns)
           (pushnew bl bb.rescan)
           (set bb.recalc t
                bl.next nil
                bl.links nil))
         oinsns))
      (@else insns))))

(defmeth basic-blocks peephole (bb)
  (each ((bl bb.list))
    (set bl.insns bb.(peephole-block bl bl.insns)))
  (whilet ((rescan (zap bb.rescan)))
    (whilet ((bl (pop bb.tryjoin)))
      (let ((nxbl bl.next))
        (when (null (cdr nxbl.rlinks))
          bb.(join-block bl nxbl)
          (set bb.recalc t)
          (when (memq nxbl bb.tryjoin)
            (upd bb.tryjoin (remq nxbl))
            (push bl bb.tryjoin))
          (upd bb.rescan (remq nxbl)))))
    (when (zap bb.recalc)
      bb.(calc-liveness rescan))
    (each ((bl rescan))
      (set bl.insns bb.(peephole-block bl bl.insns))))
 (when bb.reelim
   bb.(elim-dead-code)))

(defmeth basic-blocks thread-jumps (bb)
  (each ((bl bb.list))
    (set bl.insns bb.(thread-jumps-block bl.insns))))

(defmeth basic-blocks check-bypass-empty (bb bl nx)
  (unless (cdr bl.insns)
    (each ((pb bl.rlinks))
      (if (eq pb.next bl)
        (set pb.next nx))
      (upd pb.links (subst bl nx))
      (upd pb.insns (mapcar [iffi consp (op subst bl.label nx.label)]))
      (push pb nx.rlinks))
    bl))

(defmeth basic-blocks elim-next-jump (bb bl nx)
  (let* ((tail (last bl.insns))
         (linsn (car tail)))
    (when-match (jmp @jlabel) linsn
      (when (eql nx.label jlabel)
        (set bl.insns (butlast bl.insns))))))

(defmeth basic-blocks join-blocks (bb)
  (labels ((joinbl (list)
             (tree-case list
               ((bl nxbl . rest)
                (cond
                  ((and (eq bl.next nxbl)
                        (eq (car bl.links) nxbl)
                        (null (cdr bl.links))
                        (null (cdr nxbl.rlinks)))
                   bb.(join-block bl nxbl)
                   (joinbl (cons bl rest)))
                  (t (cons bl (joinbl (cdr list))))))
               (else else))))
    (set bb.list (joinbl bb.list))))

(defmeth basic-blocks elim-dead-code (bb)
  (each ((bl bb.list))
    (set bl.links nil
         bl.next nil
         bl.rlinks nil))
  bb.(link-graph)
  (let* ((visited (hash :eq-based)))
    (labels ((visit (bl)
               (when (test-set [visited bl])
                 (when bl.next
                   (visit bl.next))
                 [mapcar visit bl.links])))
      (for ((bl bb.root)) (bl) ((set bl bl.next))
        (visit bl))
      (visit bb.root))
    (upd bb.list (keep-if visited))
    (let (flg)
      (each ((bl bb.list)
             (nx (cdr bb.list)))
        (when bb.(check-bypass-empty bl nx)
          (set flg t)
          (del [visited bl])))
      (if flg
        (upd bb.list (keep-if visited))))
    (while
      (let (rep)
        (each ((bl bb.list)
               (nx (cdr bb.list)))
          bb.(elim-next-jump bl nx)
          (when bb.(check-bypass-empty bl nx)
            (set rep t)
            (del [visited bl])))
        (if rep
          (upd bb.list (keep-if visited))))))
  bb.(join-blocks))

(defmeth basic-blocks merge-jump-thunks (bb)
  (let* ((candidates (mappend [andf [chain .links len (op eql 1)]
                                    [chain .insns len (lop < 4)]
                                    [chain .insns last car
                                           [iff consp
                                                [chain car (op eq 'jmp)]]]
                                    list]
                              bb.list))
         (hash (group-by [chain .insns cdr] candidates)))
    (dohash (insns bls hash)
      (let ((link (car (car bls).links)))
        (each ((bb bb.list))
          (if (and (not (member bb bls))
                   (null (cdr bb.links))
                   (eq bb.next link)
                   (starts-with (cdr bb.insns) insns)
                   (eql (len bb.insns) (len insns)))
            (push bb bls))))
      (when (cdr bls)
        (whenlet ((keep (or (keep-if (op some @1.rlinks (op eq @@1) .next) bls)
                            (list (car bls))))
                  (leader (car keep)))
          (whenlet ((dupes (diff bls keep)))
            (each ((bl dupes))
              (each ((pbl bl.rlinks))
                (let* ((code pbl.insns)
                       (tail (last code))
                       (lins (car tail))
                       (sins (subst bl.label leader.label lins)))
                  (set pbl.insns (append (ldiff code tail) (list sins))))))
            (set bb.list (remove-if (lop memq dupes) bb.list))))))))

(defmeth basic-blocks late-peephole (bb code)
  (rewrite-case insns code
    (((if @reg @lab1)
      @lab2
      (jmp @lab3)
      @lab1
      . @rest)
     (let* ((bl [bb.hash lab2]))
       (if (some bl.rlinks (op eq bb) .next)
         insns
         ^((ifq ,reg (t 0) ,lab3)
           ,lab1
           ,*rest))))
    (((mov (t @tn) (d @dn))
      (jmp @lab3)
      @lab2
      (mov (t @tn) (t 0))
      @(symbolp @lab3)
      (ifq (t @tn) (t 0) @lab4)
      . @rest)
     ^(,(car insns)
       (jmp ,lab4)
       ,*(cddr insns)))
    (((mov (t @tx) (t @ty))
      (if (t @ty) @lab2)
      @(symbolp @lab1)
      (gcall (t @tx) . @args)
      @(symbolp @lab2)
      (jend (t @tx))
      . @rest)
     ^((if (t ,ty) ,lab2)
       ,lab1
       (gcall (t ,ty) ,*args)
       ,lab2
       (end (t ,ty))
       ,*rest))
    (@else else)))

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

(defun early-peephole (code)
  (rewrite-case insns code
    (((mov (t @t1) (d @d1))
      (jmp @lab2)
      @(symbolp @lab1)
      (mov (t @t1) (t 0))
      @lab2
      (ifq (t @t1) (t 0) @lab3)
      . @rest)
    ^((mov (t ,t1) (d ,d1))
      (jmp ,lab3)
      ,lab1
      (mov (t ,t1) (t 0))
      ,lab2
      ,*rest))
    (@else else)))
