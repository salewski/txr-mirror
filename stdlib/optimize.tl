;; Copyright 2021-2023
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

(load-for (usr:var %const-foldable% "constfun"))

(compile-only
  (defstruct live-info nil
    (defined 0)
    (used 0)
    def0 def1)

  (defstruct basic-block (live-info)
    (live 0)
    label
    next
    prev
    links
    rlinks
    insns
    closer
    nojoin

    (:method print (bl stream pretty-p)
      (ignore pretty-p)
      (put-string "#S" stream)
      (print ^(basic-block live ,bl.live
                           label ,bl.label
                           insns ,bl.insns
                           links ,(mapcar .label bl.links)
                           rlinks ,(mapcar .label bl.rlinks)
                           next ,bl.next.?label) stream)))

  (defstruct (basic-blocks compiler insns lt-dregs symvec) nil
    compiler
    insns
    lt-dregs
    symvec
    (hash (hash))
    (li-hash (hash :eq-based))
    list
    closures
    (cl-hash (hash))
    rescan
    recalc
    reelim
    tryjoin
    (:static start (gensym "start-"))
    (:static jump-ops '(jmp if ifq ifql close swtch ret abscsr
                        uwprot catch block jend xend))

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

    (:method num-blocks (bb)
      (len bb.list))

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
      (if nxbl.next
        (set nxbl.next.prev bl.prev))
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
              link-next nil)
         (if nxbl
           (set nxbl.prev bl)))
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
         (set link-next nil))
        ((xend . @nil)
         (set bl.nojoin t)))
      (when (and nxbl link-next)
        (set bl.next nxbl)
        (if nxbl
          (set nxbl.prev bl))
        (pushnew nxbl bl.links))
      (each ((nx bl.links))
        (pushnew bl nx.rlinks)))))

(defmeth basic-blocks local-liveness (bb bl)
  (set bl.live 0)
  (labels ((regnum (reg)
             (when-match (t @num) reg num))
           (regnums (regs)
             (mappend (do when-match
                        (t @num) @1 (list num)) regs))
           (defs (li insn def0 : def1)
             (set [bb.li-hash insn] li)
             (let* ((dn0 (regnum def0))
                    (dn1 (regnum def1))
                    (dmask0 (if dn0 (mask dn0) 0))
                    (dmask1 (if dn1 (mask dn1) 0))
                    (dmask (logior dmask0 dmask1)))
               (cond
                 ((nzerop dmask)
                  (set li.def0 def0)
                  (set li.def1 def1)
                  (new live-info
                       used (logand li.used (lognot dmask))
                       defined dmask))
                 (t (prog1
                      (copy li)
                      (set li.def0 def0)
                      (set li.def1 def1))))))
           (refs (li insn . refs)
             (set [bb.li-hash insn] li)
             (let* ((rn (regnums refs))
                    (rmask (mask . rn)))
               (new live-info
                    used (logior li.used rmask))))
           (def-ref (li insn def . refs)
             (set li.def0 def
                  [bb.li-hash insn] li)
             (let* ((rn (regnums refs))
                    (dn (regnum def))
                    (dmask (if dn (mask dn)))
                    (rmask (mask . rn)))
               (cond
                 (dn (new live-info
                          used (logior (logand li.used (lognot dmask)) rmask)
                          defined dmask))
                 (t (new live-info
                         used (logior li.used rmask))))))
           (liveness (insns)
             (if (null insns)
               (new live-info used 0)
               (let* ((li (liveness (cdr insns)))
                      (insn (car insns)))
                 (match-case insn
                   ((@(or end jend xend prof) @reg)
                    (refs li insn reg))
                   ((@(or apply call) @def . @refs)
                    (def-ref li insn def . refs))
                   ((@(or gapply gcall) @def @nil . @refs)
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
                    (let ((ili (defs li insn esreg eareg)))
                      (refs ili insn syreg descreg)))
                   ((handle @funreg @syreg)
                    (refs li insn funreg syreg))
                   ((@(or getv getvb getfb getl1b getlx getf) @def . @nil)
                    (defs li insn def))
                   ((@(or setv setl1 setlx bindv) @reg . @nil)
                    (refs li insn reg))
                   ((close @reg . @nil)
                    (defs li insn reg))
                   ((@(or jmp frame dframe uwprot) . @nil)
                    (set [bb.li-hash insn] li)
                    (copy li))
                   ((@nil . @nil)
                    (error `unhandled/mishandled @insn instruction`))
                   (@else
                     (set [bb.li-hash else] li)
                     (copy li)))))))
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
                     (t live)))
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
          (visit (car bb.list)))))))

(defmeth basic-blocks thread-jumps-block (bb code)
  (let* ((tail (last code))
         (oinsn (car tail))
         (insn oinsn)
         (ninsn oinsn))
    (while* (nequal ninsn insn)
      (set insn ninsn
           ninsn (match-case insn
                   (@(require (if @(as reg (d @nil)) @nil)
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
                        (@nil insn))))
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
                        ((@nil
                           (ifq @reg (t 0) @nil) . @nil)
                         (let ((xbl jbl.next))
                           (if xbl
                             ^(if ,reg ,xbl.label)
                             insn)))
                        (@nil insn))))
                   ((ifq @reg @creg @jlabel)
                    (let ((jbl [bb.hash jlabel]))
                      (match-case jbl.insns
                        ((@jlabel
                           (ifq @reg @creg
                                @(and @jjlabel @(not @jlabel))) . @nil)
                         ^(ifq ,reg ,creg ,jjlabel))
                        ((@(require @jlabel (equal creg '(t 0)))
                           (if @reg @(not @jlabel)) . @nil)
                         (let ((xbl jbl.next))
                           (if xbl
                             ^(ifq ,reg ,creg ,xbl.label)
                             insn)))
                        ((@nil
                           (jmp @(and @jjlabel @(not @jlabel))) . @nil)
                         ^(ifq ,reg ,creg ,jjlabel))
                        (@nil insn))))
                   ((close @reg @frsize @ntregs @jlabel . @cargs)
                    (let ((jbl [bb.hash jlabel]))
                      (match-case jbl.insns
                        ((@jlabel
                           (jmp @(and @jjlabel @(not @jlabel))) . @nil)
                         ^(close ,reg ,frsize ,ntregs ,jjlabel ,*cargs))
                        (@nil insn))))
                   (@else else))))
    (cond
      ((null ninsn) (ldiff code tail))
      ((nequal ninsn oinsn) (append (ldiff code tail) (list ninsn)))
      (t code))))

(defun subst-preserve (x y bb li insn)
  (let ((sub (subst x y insn)))
    (cond
      ((equal sub insn) insn)
      (t (set [bb.li-hash sub] li) sub))))

(defun careful-subst-preserve (x y bb li insn)
  (let ((sub (match-case insn
               ((@(or apply call) @def . @refs)
                ^(,(car insn) ,def ,*(subst x y refs)))
               ((@(or gapply gcall) @def @fn . @refs)
                ^(,(car insn) ,def ,fn ,*(subst x y refs)))
               ((mov @def @x)
                ^(mov ,def ,y))
               ((catch @esreg @eareg . @refs)
                ^(catch ,esreg ,eareg ,*(subst x y refs)))
               (@else else))))
    (cond
      ((equal sub insn) insn)
      (t (set [bb.li-hash sub] li) sub))))

(defmeth basic-blocks rename (bb insns dst src)
  (build
    (let ((vreg (eq (car src) 'v)))
      (whilet ((insn (pop insns)))
        (let ((end (if-match (end . @nil) insn t))
              (close (if-match (close . @nil) insn t))
              (li [bb.li-hash insn]))
          (cond
            (close (add insn))
            ((and vreg end)
             (add (subst-preserve dst src bb li insn))
             (pend insns)
             (set insns nil))
            ((or (mequal dst li.def0 li.def1)
                 (mequal src li.def0 li.def1))
             (add (careful-subst-preserve dst src bb li insn))
             (pend insns)
             (set insns nil))
            (t (add (subst-preserve dst src bb li insn)))))))))

(defmeth basic-blocks peephole-block (bb bl)
  (let ((code bb.(do-peephole-block bl bl.insns)))
    (set bl.insns code)))

(defmeth basic-blocks do-peephole-block (bb bl code)
  (labels ((dead-treg (insn n)
             (let ((li [bb.li-hash insn]))
               (and li (not (bit li.used n))))))
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
      ;; wasteful moves
      (((mov @reg0 @nil) (mov @reg0 @nil) . @nil)
       (cdr insns))
      (((mov @reg0 @reg1) (mov @reg1 @reg0) . @rest)
       (pushnew bl bb.rescan)
       (set bb.recalc t)
       ^(,(car insns) ,*rest))
      ;; frame reduction
      (((@(or frame dframe) @lev @nil)
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
           ((@nil
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
           (@nil insns))))
      (@(require ((if @(as reg (d @nil)) @nil) . @nil)
                 (not (memqual reg bb.lt-dregs)))
        (pushnew bl bb.tryjoin)
        (pushnew bl bb.rescan)
        (pushnew bl.next bb.rescan)
        (set bb.recalc t)
        nil)
      (@(require @(or ((@(or ifq ifql) @(as reg (d @nil)) (t 0) @jlabel) . @nil)
                      ((@(or ifq ifql) (t 0) @(as reg (d @nil)) @jlabel) . @nil))
                 (not (memqual reg bb.lt-dregs)))
        (pushnew bl.next bb.rescan)
        (if bl.next
          (set bl.prev nil))
        (set bb.recalc t
             bl.next nil
             bl.links (list [bb.hash jlabel]))
        ^((jmp ,jlabel)))
      (@(require ((@(or ifq ifql) @(as reg0 (d @n0)) @(as reg1 (d @n1)) @label)
                  . @nil)
                 (neql n0 n1)
                 (not (and (memqual reg0 bb.lt-dregs)
                           (memqual reg1 bb.lt-dregs))))
        ^((jmp ,label)))
      (((@(or ifq ifql) @reg @reg . @nil) . @nil)
       (rest insns))
      ;; wasteful move of previously tested value
      (@(require ((ifq (t @reg) (d @n) @nil) . @nil)
                 (let* ((nxbl bl.next)
                        (nxinsns nxbl.insns))
                   (if (null (cdr nxbl.rlinks))
                     (if-match (@label (mov (t @reg) (d @n)) . @rest) nxinsns
                       (set nxbl.insns ^(,label ,*rest)
                            bb.recalc t)))))
        insns)
      (((jmp @jlabel) . @nil)
       (let* ((jinsns (cdr [bb.hash jlabel].insns))
              (oinsns (match-case jinsns
                        (((jend @nil) . @nil)
                         ^(,(car jinsns)))
                        ((@nil (jend @nil) . @nil)
                         ^(,(car jinsns) ,(cadr jinsns)))
                        (@nil insns))))
         (when (neq insns oinsns)
           (pushnew bl bb.rescan)
           (if bl.next
             (set bl.prev nil))
           (set bb.recalc t
                bl.next nil
                bl.links nil))
         oinsns))
      ;; unnecessary copying t-reg
      (@(require ((mov @(as dst (t @nil)) @src) . @rest)
                 (nequal dst src))
        (let ((ren bb.(rename rest dst src)))
          (cond
            ((nequal rest ren)
             (pushnew bl bb.rescan)
             (set bb.recalc t)
             (cons (car insns) ren))
            (t insns))))
      ;; constant folding
      (@(require ((@(as op @(or gapply gcall)) @tgt @idx
                                               . @(all @(or (d @dn)
                                                            @(with (t 0) dn nil))))
                  . @rest)
                 [%const-foldable% [bb.symvec idx]])
        (let* ((co bb.compiler)
               (dvec co.(get-datavec))
               (fun [bb.symvec idx])
               (args (mapcar [iffi true dvec] dn))
               (val (if (eq op 'gcall)
                      (apply fun args)
                      (apply fun (append [args 0..-1] [args -1]))))
               (dreg co.(get-dreg val)))
          ^((mov ,tgt ,dreg) ,*rest)))
      ;; apply to gapply
      (@(require @(with ((getf @(as treg (t @tn)) @idx) . @rest)
                        @(scan @(or @(with @(as apl ((apply @dest @treg . @args)
                                              . @arest))
                                           li [bb.li-hash (car apl)])
                                    @(require (@insn . @nil)
                                              (find treg insn))))
                        rest)
                 apl
                 (or (equal dest treg)
                     (not (bit li.used tn))))
        (set bb.recalc t)
        (let* ((gapl ^(gapply ,dest ,idx ,*args)))
          (set [bb.li-hash gapl] li)
          ^(,*(ldiff rest apl) ,gapl ,*arest)))
      (@nil insns))))

(defmeth basic-blocks peephole (bb)
  (each ((bl bb.list))
    bb.(peephole-block bl))
  (whilet ((rescan (zap bb.rescan)))
    (whilet ((bl (pop bb.tryjoin)))
      (let ((nxbl bl.next))
        (unless (or bl.nojoin (cdr nxbl.rlinks))
          bb.(join-block bl nxbl)
          (set bb.recalc t)
          (when (memq nxbl bb.tryjoin)
            (upd bb.tryjoin (remq nxbl))
            (push bl bb.tryjoin))
          (upd bb.rescan (remq nxbl)))))
    (when (zap bb.recalc)
      bb.(calc-liveness rescan))
    (each ((bl rescan))
      bb.(peephole-block bl)))
 (when bb.reelim
   bb.(elim-dead-code)))

(defmeth basic-blocks thread-jumps (bb)
  (each ((bl bb.list))
    (set bl.insns bb.(thread-jumps-block bl.insns))))

(defmeth basic-blocks join-blocks (bb)
  (labels ((joinbl (list)
             (tree-case list
               ((bl nxbl . rest)
                (cond
                  ((and (eq bl.next nxbl)
                        (eq (car bl.links) nxbl)
                        (null bl.nojoin)
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
         bl.prev nil
         bl.rlinks nil))
  bb.(link-graph)
  (let* ((visited (hash :eq-based)))
    (labels ((visit (bl)
               (when (test-set [visited bl])
                 (when bl.next
                   (visit bl.next))
                 [mapcar visit bl.links])))
      (for ((bl (car bb.list))) (bl) ((set bl bl.next))
        (visit bl)))
    (upd bb.list (keep-if visited))
    (let (flg)
      (each ((bl bb.list)
             (nx (cdr bb.list)))
        (when bl.(check-bypass-empty nx)
          (set flg t)
          (del [visited bl])))
      (if flg
        (upd bb.list (keep-if visited))))
    (while
      (let (rep)
        (each ((bl bb.list)
               (nx (cdr bb.list)))
          bl.(elim-next-jump nx)
          (when bl.(check-bypass-empty nx)
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
       (if (cdr bl.rlinks)
         insns
         ^((ifq ,reg (t 0) ,lab3)
           ,lab1
           ,*rest))))
    (((mov (t @tn) (d @nil))
      (jmp @lab3)
      @nil
      (mov (t @tn) (t 0))
      @(symbolp @lab3)
      (ifq (t @tn) (t 0) @lab4)
      . @nil)
     ^(,(car insns)
       (jmp ,lab4)
       ,*(cddr insns)))
    ((@(symbolp @lab1)
      (mov (t @tn) (t 0))
      @lab2
      (ifq (t @tn) (t 0) @lab4)
      @(symbolp)
      (gcall (t @tn) . @nil)
      . @nil)
     ^(,lab2
       (ifq (t ,tn) (t 0) ,lab4)
       ,lab1
       ,*(cddddr insns)))
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
       (jend (t ,ty))
       ,*rest))
    (@else else)))

(defmeth basic-blocks identify-closures (bb)
  (zap bb.closures)
  (each ((bl bb.list))
    (when-match @(end ((close . @nil))) bl.insns
      (let ((nx bl.next))
        (set nx.closer bl)
        (push nx bb.closures))))
  (upd bb.closures nreverse)
  (let ((visited (hash :eq-based)))
    (labels ((visit (bl clhead)
               (when (test-set [visited bl])
                 (push bl [bb.cl-hash clhead])
                 [mapcar (lop visit clhead) bl.links])))
      (each ((cb bb.closures))
        (visit cb cb))))
  [hash-update bb.cl-hash nreverse])

(defmeth basic-block fill-treg-compacting-map (bl map)
  (labels ((add-treg (reg)
            (unless [map reg]
              (if-match (t @nil) reg
                (set [map reg] ^(t ,(len map))))))
           (add-tregs (args)
             [mapcar add-treg args]))
    (iflet ((cl bl.closer))
      (let ((cloinsn (car (last cl.insns))))
        (add-tregs (cddr cloinsn))))
    (each ((insn bl.insns))
      (match-case insn
        ((close @reg . @nil)
         (add-treg reg))
        ((@nil . @args)
         (add-tregs args))))))

(defmeth basic-block apply-treg-compacting-map (bl map)
  (labels ((fix (arg) [map arg arg])
           (fix-tregs (args) [mapcar fix args]))
    (iflet ((cl bl.closer))
      (match ((close @reg @frsize @nil . @rest)) (last cl.insns)
        (set (last cl.insns)
             ^((close ,reg ,frsize ,(len map) ,*(fix-tregs rest))))))
    (set bl.insns (collect-each ((insn bl.insns))
                    (match-case insn
                      ((close @reg . @rest)
                       ^(close ,(fix reg) ,*rest))
                      ((@op . @args)
                       ^(,op ,*(fix-tregs args)))
                      (@else else))))))

(defmeth basic-block check-bypass-empty (bl nx)
  (unless (cdr bl.insns)
    (upd nx.rlinks (remq bl))
    (whenlet ((pb bl.prev))
      (set pb.next nx)
      (set nx.prev pb))
    (each ((pb bl.rlinks))
      (upd pb.links (subst bl nx))
      (upd pb.insns (mapcar [iffi consp (op subst bl.label nx.label)]))
      (push pb nx.rlinks))
    bl))

(defmeth basic-block elim-next-jump (bl nx)
  (let* ((tail (last bl.insns))
         (linsn (car tail)))
    (match-case linsn
      (@(or (jmp @jlabel)
            (if @nil @jlabel)
            (@(or ifq ifql) @nil @nil @jlabel))
       (when (eql nx.label jlabel)
         (set bl.insns (butlast bl.insns)))))))

(defmeth basic-blocks compact-tregs (bb)
  bb.(identify-closures)
  (each ((bl bb.closures))
    (let ((clist [bb.cl-hash bl]))
      (let ((map (hash-from-pairs '(((t 0) (t 0)) ((t 1) (t 1))))))
        (each ((cl clist))
          cl.(fill-treg-compacting-map map))
        (each ((cl clist))
          cl.(apply-treg-compacting-map map))))))

(defmeth basic-blocks null-unused-data (bb)
  (let ((used-dregs 0)
        (used-funs 0)
        (co bb.compiler))
    (each ((bl bb.list))
      (each ((insn bl.insns))
        (if-match @(coll (d @dn)) insn
          (set-mask used-dregs (mask . dn)))
        (if-match (@(or gcall gapply getf getlx setlx) @nil @fn . @nil) insn
          (set-mask used-funs (mask fn)))))
    co.(null-dregs used-dregs)
    co.(null-stab used-funs)))

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
    (@nil tail))
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
