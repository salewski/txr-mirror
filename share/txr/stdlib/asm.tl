;; Copyright 2018-2021
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

(load "vm-param")

(defstruct oc-base nil
  (:static deprecated nil)
  (:method synerr (me fmt . args)
    (error `opcode @{me.symbol}: @fmt` . args))

  (:method chk-arg-count (me n syntax)
    (when (neq (length (rest syntax)) n)
      me.(synerr "~s arguments required; ~s is invalid"
                 n syntax)))

  (:method chk-arg-count-min (me n syntax)
    (when (< (length (rest syntax)) n)
      me.(synerr "~s arguments required; ~s is invalid"
                 n syntax)))

  (:method backpatch (me asm at offs)
    (asm-error `@{me.symbol} doesn't backpatch`)))

(compile-only
  (defstruct assembler nil
    buf
    bstr
    (max-treg 0)
    (labdef (hash))
    (labref (hash))
    (:static imm-width (relate '(si mi bi) '(10 16 32)))
    (:static sign-bits (relate '(fixnum bignum chr) '(1 1 0)))
    (:static operand-name (relate '(si mi bi l r rs d ds n o)
                                  '("small immediate"
                                    "medium immediate"
                                    "big immediate"
                                    "label"
                                    "register operand"
                                    "register small operand"
                                    "register destination operand"
                                    "register small destination operand"
                                    "integer"
                                    "any object")))))

(defmeth assembler :postinit (me)
  (cond
    (me.buf (set me.bstr (make-buf-stream me.buf)))
    (me.bstr (set me.buf (get-buf-from-stream me.bstr)))
    (t (set me.bstr (make-buf-stream)
            me.buf (get-buf-from-stream me.bstr)))))

(defmeth assembler cur-pos (me)
  (seek-stream me.bstr 0 :from-current))

(defmeth assembler set-pos (me pos)
  (seek-stream me.bstr pos :from-start))

(defmeth assembler lookup-label (me sym oc)
  (condlet
    (((n [me.labdef sym])) n)
    (t (push (cons oc (trunc me.(cur-pos) 4)) [me.labref sym])
       0)))

(defmeth assembler define-label (me sym)
  (let* ((pos me.(cur-pos))
         (ins (trunc pos 4)))
    (set [me.labdef sym] ins)
    (each ((entry (del [me.labref sym])))
      (tree-bind (oc . offs) entry
        me.(set-pos (* 4 offs))
        oc.(backpatch me (* 4 offs) ins)))
    me.(set-pos pos)
    ins))

(defmeth assembler read-buf (me bytes)
  (let ((buf (make-buf bytes)))
    (when (neql (fill-buf buf 0 me.bstr) bytes)
      (asm-error "read past instruction block"))
    buf))

(defmeth assembler put-word (me word)
  (let* ((buf (make-buf 0)))
    (buf-put-u32 buf 0 word)
    (put-buf buf 0 me.bstr)))

(defmeth assembler put-insn (me code extension operand)
  (let ((word (logior (ash code 26) (ash extension 16) operand))
        (buf (make-buf 0)))
    (buf-put-u32 buf 0 word)
    (put-buf buf 0 me.bstr)))

(defmeth assembler put-pair (me op1 op2)
  (let ((word (logior (ash op1 16) op2))
        (buf (make-buf 0)))
    (buf-put-u32 buf 0 word)
    (put-buf buf 0 me.bstr)))

(defmeth assembler get-word (me)
  (let* ((buf me.(read-buf (sizeof uint32))))
    (buf-get-u32 buf 0)))

(defmeth assembler get-insn (me)
  (let* ((buf me.(read-buf (sizeof uint32)))
         (word (buf-get-u32 buf 0)))
    (list (ash word -26)
          (logtrunc (ash word -16) 10)
          (logtrunc word 16))))

(defmeth assembler get-pair (me)
  (let* ((buf me.(read-buf (sizeof uint32)))
         (word (buf-get-u32 buf 0)))
    (list (ash word -16) (logtrunc word 16))))

(defmeth assembler immediate-fits-type (me arg operand-type)
  (and (member (typeof arg)
               '(fixnum chr))
       (<= (+ (width arg)
              [me.sign-bits (typeof arg)]
              2)
           [me.imm-width operand-type])))

(defmeth assembler parse-args (me oc syntax pattern)
  (mapcar (lambda (type arg n)
            (let ((parg (caseql type
                          ((si mi bi)
                           (when me.(immediate-fits-type arg type)
                             arg))
                          (l (cond
                               ((is-label arg) me.(lookup-label arg oc))
                               ((integerp arg) arg)))
                          (n (if (integerp arg) arg))
                          (o arg)
                          ((r rs d ds)
                           (cond
                             ((null arg) 0)
                             ((consp arg)
                              (parse-compound-operand arg))
                             ((symbolp arg)
                              (parse-operand (symbol-name arg)))))
                          (t (asm-error "invalid arg type spec ~s" type)))))
              (unless (or parg (eq type 'o))
                oc.(synerr "argument ~a of ~s invalid; ~a expected"
                           n syntax [me.operand-name type]))
              (when (and (member type '(d ds))
                         (or (zerop parg)))
                oc.(synerr "argument ~a of ~s cannot be destination"
                           n syntax))
              (when (and (member type '(rs ds))
                         (not (small-op-p parg)))
                oc.(synerr "argument ~a of ~s isn't a small register"
                           n syntax))
              (when (and (member type '(r rs d ds)) (< parg %lev-size%))
                (set me.max-treg (max parg me.max-treg)))
              parg))
          pattern (rest syntax) (range 1)))

(defmeth assembler asm-one (me syntax)
  (let ((oc (cond
              ((is-label syntax) [%oc-hash% 'label])
              ((consp syntax) [%oc-hash% (car syntax)]))))
    (unless oc
      (asm-error "invalid instruction ~s" syntax))
    oc.(asm me syntax)))

(defmeth assembler asm (me insns)
  (each ((i insns))
    me.(asm-one i))
  (unless (empty me.labref)
    (asm-error "dangling label references"))
  (whenlet ((n (cdr [find-max me.labdef : cdr])))
    (unless (< -1 n (len me.buf))
      (asm-error "labels outside of code"))))

(defmeth assembler dis-one (me)
  (tree-bind (code extension operand) me.(get-insn)
    (let ((oc [%oc-hash% code]))
      oc.(dis me extension operand))))

(defmeth assembler dis (me)
  me.(set-pos 0)
  (build
    (while (< me.(cur-pos) (len me.buf))
      (add me.(dis-one)))))

(defmeth assembler dis-listing (me : (stream *stdout*))
  (let ((p 0)
        (c 0)
        (l (len me.buf)))
    me.(set-pos p)
    (while (< p l)
      (let* ((dis me.(dis-one))
             (dis-txt (cat-str [mapcar tostringp dis] " "))
             (q me.(cur-pos)))
        (inc c)
        me.(set-pos p)
        (format t "~,5d: ~,08X ~a\n" (trunc p 4) me.(get-word) dis-txt)
        (while (< (inc p 4) q)
          (format t "~,5d: ~,08X\n" (trunc p 4) me.(get-word)))
        me.(set-pos q)
        (set p q)))
    c))

(defvarl %oc-list-builder% (new list-builder))

(defvarl %oc-hash% (hash))

(defparml %oc-code% 0)

(defun asm-error (msg . args)
  (error `~s: @msg` 'assembler . args))

(defun register-opcode (oc)
  %oc-list-builder%.(add oc)
  (set [%oc-hash% oc.symbol] oc)
  (set [%oc-hash% oc.code] oc))

(defun is-label (obj)
  (or (keywordp obj)
      (and (symbolp obj)
           (not (symbol-package obj)))))

(defun parse-compound-operand (cons)
  (tree-case cons
    ((sym arg)
     (when (< -1 arg %lev-size%)
       (caseq sym
         ((t) arg)
         (d (+ arg %lev-size%)))))
    ((sym arg1 arg2)
     (when (and (<= 0 arg1 %max-v-lev%)
                (<= 0 arg2 %max-lev-idx%))
       (caseq sym
         (v (+ (* (ssucc arg1) %lev-size%) arg2)))))))

(defun parse-operand (str)
  (cond
    ((r^$ #/t[0-9A-Fa-f][0-9A-Fa-f]?[0-9A-Fa-f]?/ str)
     (int-str [str 1..:] 16))
    ((r^$ #/d[0-9A-Fa-f][0-9A-Fa-f]?[0-9A-Fa-f]?/ str)
     (+ %lev-size% (int-str [str 1..:] 16)))
    ((r^$ #/v[0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f]/ str)
       (let ((lv (int-str [`0@{str[1..:]}` -5..-3] 16))
             (ix (int-str [str -3..:] 16)))
         (+ (* %lev-size% (ssucc lv)) ix)))))

(eval-only
  (defmacro with-lev-idx ((lev-var idx-var) val-expr . body)
    (with-gensyms (val-var)
      ^(let* ((,val-var ,val-expr)
              (,lev-var (ash ,val-var (macro-time (- %lev-bits%))))
              (,idx-var (logtrunc ,val-var %lev-bits%)))
         ,*body))))

(defun operand-to-sym (val)
  (with-lev-idx (lv ix) val
    (caseql lv
      (0 (if (zerop ix)
           nil
           (intern (fmt "t~s" ix))))
      (1 (intern (fmt "d~s" ix)))
      (t (intern (fmt "v~,02X~,03X" (ppred lv) ix))))))

(defun operand-to-exp (val)
  (with-lev-idx (lv ix) val
    (caseql lv
      (0 (if (zerop ix)
           nil
           ^(t ,ix)))
      (1 ^(d ,ix))
      (t ^(v ,(ppred lv) ,ix)))))

(defun bits-to-obj (bits width)
  (let ((tag (logtrunc bits 2))
        (val (ash bits -2)))
    (caseq tag
      (1 (sign-extend val (- width 2)))
      (2 (chr-int val))
      (t (error "~s: bad immediate operand: ~x" 'assembler bits)))))

(defun small-op-p (val)
  (with-lev-idx (lv ix) val
    (and (<= 0 ix %max-sm-lev-idx%)
         (<= 0 lv %max-sm-lev%))))

(defun enc-small-op (val)
  (with-lev-idx (lv ix) val
    (logior (ash lv %sm-lev-bits%) ix)))

(defun small-op-to-sym (sval)
  (let ((lv (ash sval (- %sm-lev-bits%)))
        (ix (logtrunc sval %sm-lev-bits%)))
    (operand-to-sym (+ (* lv %lev-size%) ix))))

(defstruct backpatch-low16 nil
  (:method backpatch (me asm at offs)
    (tree-bind (hi lo) asm.(get-pair)
      asm.(set-pos at)
      asm.(put-pair hi offs))))

(defstruct backpatch-high16 nil
  (:method backpatch (me asm at offs)
    (tree-bind (hi lo) asm.(get-pair)
      asm.(set-pos at)
      asm.(put-pair offs lo))))

(defvarl %backpatch-low16% (new backpatch-low16))
(defvarl %backpatch-high16% (new backpatch-high16))

(eval-only
  (defmacro defopcode (class symbol code . slot-defs)
    ^(symacrolet ((auto (pinc %oc-code%)))
       (defstruct ,class oc-base
         (:static symbol ',symbol)
         (:static code ,code)
         ,*slot-defs)
       (register-opcode (new ,class))))

  (defmacro defopcode-derived (class symbol code orig-class)
    ^(symacrolet ((auto (pinc %oc-code%)))
       (defstruct ,class ,orig-class
         (:static symbol ',symbol)
         (:static code ,code))
       (register-opcode (new ,class))))

  (defmacro defopcode-alias (alias-symbol orig-symbol)
    ^(let ((oc [%oc-hash% ',orig-symbol]))
       (set [%oc-hash% ',alias-symbol] oc))))

(defopcode op-label label nil
  (:method asm (me asm syntax)
    (unless (is-label syntax)
      asm.(synerr "label must be keyword or gensym"))
    asm.(define-label syntax))

  (:method dis (me asm extension operand)))

(defopcode op-noop noop auto
  (:method asm (me asm syntax)
    me.(chk-arg-count 0 syntax)
    asm.(put-insn me.code 0 0))

  (:method dis (me asm extension operand)
    ^(,me.symbol)))

(defopcode op-frame frame auto
  (:method asm (me asm syntax)
    me.(chk-arg-count 2 syntax)
    (tree-bind (lev size) asm.(parse-args me syntax '(n n))
      (unless (<= 2 lev %max-v-lev%)
        me.(synerr "level must range from 2 to ~a"
                   %max-v-lev%))
      (unless (<= 0 size %lev-size%)
        me.(synerr "size must range from 0 to ~a"
                   %lev-size%))
      asm.(put-insn me.code lev size)))
  (:method dis (me asm lev size)
    ^(,me.symbol ,lev ,size)))

(defopcode-derived op-sframe sframe auto op-frame)

(defopcode-derived op-dframe dframe auto op-frame)

(defopcode op-end end auto
  (:method asm (me asm syntax)
    me.(chk-arg-count 1 syntax)
    (let ((res (car asm.(parse-args me syntax '(r)))))
      asm.(put-insn me.code 0 res)))
  (:method dis (me asm extension res)
    ^(,me.symbol ,(operand-to-sym res))))

(defopcode-alias jend end)

(defopcode-derived op-prof prof auto op-end)

(defopcode op-call call auto
  (:method asm (me asm syntax)
    me.(chk-arg-count-min 2 syntax)
    (let* ((nargs (pred (len syntax)))
           (syn-pat (repeat '(r) (succ nargs)))
           (funargs (ppred nargs))
           (args asm.(parse-args me syntax syn-pat)))
      asm.(put-insn me.code funargs (pop args))
      (while args
        (let ((x (pop args))
              (y (or (pop args) 0)))
          asm.(put-pair y x)))))

  (:method dis (me asm funargs arg0)
    (build
      (add me.symbol)
      (add (operand-to-sym arg0))
      (inc funargs 1)
      (while (> funargs 0)
        (dec funargs 2)
        (tree-bind (y x) asm.(get-pair)
          (add (operand-to-sym x))
          (unless (minusp funargs)
            (add (operand-to-sym y))))))))

(defopcode-derived op-apply apply auto op-call)

(defopcode op-gcall gcall auto
  (:method asm (me asm syntax)
    me.(chk-arg-count-min 2 syntax)
    (let* ((nargs (pred (len syntax)))
           (syn-pat (list* 'r 'n (repeat '(r) (sssucc nargs))))
           (funargs (ppred nargs))
           (args asm.(parse-args me syntax syn-pat)))
      asm.(put-insn me.code funargs (pop args))
      (while args
        (let ((x (pop args))
              (y (or (pop args) 0)))
          asm.(put-pair y x)))))

  (:method dis (me asm funargs arg0)
    (let ((first t))
      (build
        (add me.symbol)
        (add (operand-to-sym arg0))
        (inc funargs 1)
        (while (> funargs 0)
          (dec funargs 2)
          (tree-bind (y x) asm.(get-pair)
            (add (if (zap first) x (operand-to-sym x)))
            (unless (minusp funargs)
              (add (operand-to-sym y)))))))))

(defopcode-derived op-gapply gapply auto op-gcall)

(defopcode op-movrs movrs auto
  (:method asm (me asm syntax)
    me.(chk-arg-count 2 syntax)
    (tree-bind (dst src) asm.(parse-args me syntax '(d rs))
      asm.(put-insn me.code (enc-small-op src) dst)))

  (:method dis (me asm src dst)
    ^(,me.symbol ,(operand-to-sym dst) ,(small-op-to-sym src))))

(defopcode op-movsr movsr auto
  (:method asm (me asm syntax)
    me.(chk-arg-count 2 syntax)
    (tree-bind (dst src) asm.(parse-args me syntax '(ds r))
      asm.(put-insn me.code (enc-small-op dst) src)))

  (:method dis (me asm dst src)
    ^(,me.symbol ,(small-op-to-sym dst) ,(operand-to-sym src))))

(defopcode op-movrr movrr auto
  (:method asm (me asm syntax)
    me.(chk-arg-count 2 syntax)
    (tree-bind (dst src) asm.(parse-args me syntax '(d r))
      asm.(put-insn me.code 0 dst)
      asm.(put-pair 0 src)))

  (:method dis (me asm extension dst)
    (let ((src (cadr asm.(get-pair))))
      ^(,me.symbol ,(operand-to-sym dst) ,(operand-to-sym src)))))

(defopcode op-mov-pseudo mov nil
  (:method asm (me asm syntax)
    (tree-bind (dst src) asm.(parse-args me syntax '(d r))
      (let ((real [%oc-hash% (cond
                               ((small-op-p dst) 'movsr)
                               ((small-op-p src) 'movrs)
                               (t 'movrr))]))
        real.(asm asm syntax)))))

(defopcode op-jmp jmp auto
  (:method asm (me asm syntax)
    me.(chk-arg-count 1 syntax)
    (let ((dst (car asm.(parse-args me syntax '(l)))))
      asm.(put-insn me.code (ash dst -16) (logtrunc dst 16))))

  (:method backpatch (me asm at dst)
    asm.(put-insn me.code (ash dst -16) (logtrunc dst 16)))

  (:method dis (me asm high16 low16)
    ^(,me.symbol ,(logior (ash high16 16) low16))))

(defopcode op-if if auto
  (:method asm (me asm syntax)
    me.(chk-arg-count 2 syntax)
    (tree-bind (reg dst) asm.(parse-args me syntax '(r l))
      asm.(put-insn me.code (ash dst -16) (logtrunc dst 16))
      asm.(put-pair 0 reg)))

  (:method backpatch (me asm at dst)
    asm.(put-insn me.code (ash dst -16) (logtrunc dst 16)))

  (:method dis (me asm high16 low16)
    (let ((dst (logior (ash high16 16) low16))
          (reg (cadr asm.(get-pair))))
      ^(,me.symbol ,(operand-to-sym reg) ,dst))))

(defopcode op-ifq ifq auto
  (:method asm (me asm syntax)
    me.(chk-arg-count 3 syntax)
    (tree-bind (lreg rreg dst) asm.(parse-args me syntax '(r r l))
      asm.(put-insn me.code (ash dst -16) (logtrunc dst 16))
      asm.(put-pair lreg rreg)))

  (:method backpatch (me asm at dst)
    asm.(put-insn me.code (ash dst -16) (logtrunc dst 16)))

  (:method dis (me asm high16 low16)
    (let ((dst (logior (ash high16 16) low16)))
      (tree-bind (lreg rreg) asm.(get-pair)
      ^(,me.symbol ,(operand-to-sym lreg) ,(operand-to-sym rreg) ,dst)))))

(defopcode-derived op-ifql ifql auto op-ifq)

(defopcode op-swtch swtch auto
  (:method asm (me asm syntax)
    me.(chk-arg-count-min 1 syntax)
    (let* ((args asm.(parse-args me syntax '(r)))
           (lbls (cddr syntax))
           (tblsz (len lbls)))
      asm.(put-insn me.code tblsz (car args))
      (while lbls
        (let ((x asm.(lookup-label (pop lbls) %backpatch-low16%))
              (y (if lbls
                   asm.(lookup-label (pop lbls) %backpatch-high16%)
                   0)))
          asm.(put-pair y x)))))

  (:method dis (me asm tblsz switch-val)
    (build
      (add me.symbol)
      (add (operand-to-sym switch-val))
      (while (> tblsz 0)
        (dec tblsz 2)
        (tree-bind (y x) asm.(get-pair)
          (add x)
          (unless (minusp tblsz)
            (add y)))))))

(defopcode-derived op-uwprot uwprot auto op-jmp)

(defopcode op-block block auto
  (:method asm (me asm syntax)
    me.(chk-arg-count 3 syntax)
    (tree-bind (outreg blname exitpt) asm.(parse-args me syntax '(d r l))
      asm.(put-insn me.code (ash exitpt -16) (logtrunc exitpt 16))
      asm.(put-pair outreg blname)))

  (:method backpatch (me asm at exitpt)
    asm.(put-insn me.code (ash exitpt -16) (logtrunc exitpt 16)))

 (:method dis (me asm high16 low16)
   (let ((exitpt (logior (ash high16 16) low16)))
     (tree-bind (outreg blname) asm.(get-pair)
       ^(,me.symbol ,(operand-to-sym outreg) ,(operand-to-sym blname)
                    ,exitpt)))))

(defopcode op-retsr retsr auto
  (:method asm (me asm syntax)
    me.(chk-arg-count 2 syntax)
    (tree-bind (name reg) asm.(parse-args me syntax '(rs r))
      asm.(put-insn me.code (enc-small-op name) reg)))

  (:method dis (me asm name reg)
    ^(,me.symbol ,(small-op-to-sym name) ,(operand-to-sym reg))))

(defopcode op-retrs retrs auto
  (:method asm (me asm syntax)
    me.(chk-arg-count 2 syntax)
    (tree-bind (name reg) asm.(parse-args me syntax '(r rs))
      asm.(put-insn me.code (enc-small-op reg) name)))

  (:method dis (me asm reg name)
    ^(,me.symbol ,(operand-to-sym name) ,(small-op-to-sym reg))))

(defopcode op-retrr retrr auto
  (:method asm (me asm syntax)
    me.(chk-arg-count 2 syntax)
    (tree-bind (name reg) asm.(parse-args me syntax '(r r))
      asm.(put-insn me.code 0 reg)
      asm.(put-pair 0 name)))

  (:method dis (me asm extension reg)
    (let ((name (cadr asm.(get-pair))))
      ^(,me.symbol ,(operand-to-sym name) ,(operand-to-sym reg)))))

(defopcode op-ret-pseudo ret nil
  (:method asm (me asm syntax)
    me.(chk-arg-count 2 syntax)
    (tree-bind (name reg) asm.(parse-args me syntax '(r r))
      (let ((real [%oc-hash% (cond
                               ((small-op-p name) 'retsr)
                               ((small-op-p reg) 'retrs)
                               (t 'retrr))]))
        real.(asm asm syntax)))))

(defopcode-derived op-abscsr abscsr auto op-retsr)

(defopcode op-catch catch auto
  (:method asm (me asm syntax)
    me.(chk-arg-count 5 syntax)
    (tree-bind (sym args catch-syms desc dst)
               asm.(parse-args me syntax '(d d r r l))
      asm.(put-insn me.code (ash dst -16) (logtrunc dst 16))
      asm.(put-pair sym args)
      asm.(put-pair desc catch-syms)))

  (:method backpatch (me asm at dst)
    asm.(put-insn me.code (ash dst -16) (logtrunc dst 16)))

 (:method dis (me asm high16 low16)
   (let ((dst (logior (ash high16 16) low16)))
     (tree-bind (sym args) asm.(get-pair)
       (tree-bind (desc catch-syms) asm.(get-pair)
         ^(,me.symbol ,(operand-to-sym sym) ,(operand-to-sym args)
                      ,(operand-to-sym catch-syms)
                      ,(operand-to-sym desc) ,dst))))))

(defopcode op-handle handle auto
  (:method asm (me asm syntax)
    me.(chk-arg-count 2 syntax)
    (tree-bind (fun handle-syms) asm.(parse-args me syntax '(r r))
      asm.(put-insn me.code 0 fun)
      asm.(put-pair fun handle-syms)))

 (:method dis (me asm extension fun)
     (let ((handle-syms (cadr asm.(get-pair))))
       ^(,me.symbol ,(operand-to-sym fun) ,(operand-to-sym handle-syms)))))

(defopcode op-getv getv auto
  (:method asm (me asm syntax)
    me.(chk-arg-count 2 syntax)
    (tree-bind (reg name) asm.(parse-args me syntax '(d r))
      (unless (small-op-p name)
        asm.(asm-one ^(mov (t 1) ,(operand-to-exp name)))
        (set name 1))
      asm.(put-insn me.code (enc-small-op name) reg)))
  (:method dis (me asm name reg)
    ^(,me.symbol ,(operand-to-sym reg) ,(small-op-to-sym name))))

(defopcode-derived op-oldgetf oldgetf auto op-getv)

(defopcode-derived op-getl1 getl1 auto op-getv)

(defopcode-derived op-getvb getvb auto op-getv)

(defopcode-derived op-getfb getfb auto op-getv)

(defopcode-derived op-getl1b getl1b auto op-getv)

(defopcode op-setv setv auto
  (:method asm (me asm syntax)
    me.(chk-arg-count 2 syntax)
    (tree-bind (reg name) asm.(parse-args me syntax '(r r))
      (unless (small-op-p name)
        asm.(asm-one ^(mov (t 1) ,(operand-to-exp name)))
        (set name 1))
      asm.(put-insn me.code (enc-small-op name) reg)))
  (:method dis (me asm name reg)
    ^(,me.symbol ,(operand-to-sym reg) ,(small-op-to-sym name))))

(defopcode-derived op-setl1 setl1 auto op-setv)

(defopcode-derived op-bindv bindv auto op-setv)

(defopcode op-close close auto
  (:method asm (me asm syntax)
    me.(chk-arg-count-min 6 syntax)
    (let* ((syn-pat (repeat '(d) (- (length syntax) 7))))
      (tree-bind (reg frsize ntreg dst fix req vari . regs)
                 asm.(parse-args me syntax ^(d n n l n n o,*syn-pat))
        (unless (<= 0 frsize %lev-size%)
          me.(synerr "frame size must be 0 to ~a" %lev-size%))
        asm.(put-insn me.code (ash dst -16) (logtrunc dst 16))
        asm.(put-pair (logior (ash (if vari 1 0) %lev-bits%) frsize) reg)
        asm.(put-pair req fix)
        asm.(put-pair 0 ntreg)
        (unless (eql fix (- (len regs) (if vari 1 0)))
          me.(synerr "wrong number of registers"))
        (while regs
          (let ((x (pop regs))
                (y (or (pop regs) 0)))
            asm.(put-pair y x))))))

  (:method backpatch (me asm at dst)
    asm.(put-insn me.code (ash dst -16) (logtrunc dst 16)))

  (:method dis (me asm high16 low16)
    (let ((dst (logior (ash high16 16) low16)))
      (tree-bind (vari-frsize reg) asm.(get-pair)
        (let ((vari (bit vari-frsize %lev-bits%)))
          (tree-bind (req fix) asm.(get-pair)
            (tree-bind (ign ntreg) asm.(get-pair)
              (build
                (add me.symbol (operand-to-sym reg)
                     (logtrunc vari-frsize %lev-bits%)
                     ntreg dst fix req vari)
                (when vari
                  (inc fix))
                (while (> fix 0)
                  (dec fix 2)
                  (tree-bind (y x) asm.(get-pair)
                    (add (operand-to-sym x))
                    (unless (minusp fix)
                      (add (operand-to-sym y)))))))))))))

(defopcode op-getlx getlx auto
  (:method asm (me asm syntax)
    me.(chk-arg-count 2 syntax)
    (tree-bind (dst idx) asm.(parse-args me syntax '(d n))
      (cond
        ((small-op-p dst)
         asm.(put-insn me.code (enc-small-op dst) idx))
        (t asm.(put-insn me.code (enc-small-op 1) idx)
           asm.(asm-one ^(mov ,(operand-to-exp dst) t1))))))
  (:method dis (me asm dst idx)
    ^(,me.symbol ,(small-op-to-sym dst) ,idx)))

(defopcode op-setlx setlx auto
  (:method asm (me asm syntax)
    me.(chk-arg-count 2 syntax)
    (tree-bind (src idx) asm.(parse-args me syntax '(r n))
      (cond
        ((small-op-p src)
         asm.(put-insn me.code (enc-small-op src) idx))
        (t asm.(asm-one ^(mov t1 ,(operand-to-exp src)))
           asm.(put-insn me.code (enc-small-op 1) idx)))))
  (:method dis (me asm src idx)
    ^(,me.symbol ,(small-op-to-sym src) ,idx)))

(defopcode-derived op-getf getf auto op-getlx)

(defun disassemble-cdf (code data funv *stdout*)
  (let ((asm (new assembler buf code)))
    (put-line "data:")
    (mapdo (do format t "~5d: ~s\n" @1 @2) (range 0) data)
    (put-line "syms:")
    (mapdo (do format t "~5d: ~s\n" @1 @2) (range 0) funv)
    (put-line "code:")
    (let ((ninsn asm.(dis-listing)))
      (put-line "instruction count:")
      (format t "~5d\n" ninsn))))

(defun disassemble (obj : (stream *stdout*))
  (symacrolet ((self 'vm-disassemble-obj))
    (typecase obj
      (vm-desc (disassemble-cdf (vm-desc-bytecode obj)
                                (vm-desc-datavec obj)
                                (vm-desc-symvec obj)
                                stream))
      (fun (unless (vm-fun-p obj)
             (error "~s: not a vm function: ~s" self obj))
           (let* ((clo (func-get-env obj))
                  (desc (sys:vm-closure-desc clo))
                  (ip (sys:vm-closure-entry clo)))
             (disassemble desc stream)
             (put-line "entry point:")
             (format stream "~5d\n" ip)))
      (t (iflet ((fun (symbol-function obj)))
           (disassemble fun stream)
           (error "~s: not a compiled object: ~s" self obj))))
    obj))
