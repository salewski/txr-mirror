;; Copyright 2016
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

(defex opt-error error)

(defstruct opt-desc nil
  short
  long
  helptext
  arg-p
  (type :bool)
  (:static valid-types '(:bool :dec :hex :oct :cint :float :str))
  (:postinit (me)
    me.(check)
    (set me.arg-p (neq me.type :bool))))

(defstruct (sys:opt-parsed name arg desc : eff-type) nil
  name
  arg  ;; string, integer, real, ...
  desc ;; opt-desc
  eff-type
  (:postinit (me) me.(convert-type)))

(defstruct opts nil
  (opt-hash (hash :equal-based)) ;; string to sys:opt-parsed
  in-args
  out-args
  opt-processor) ;; sys:opt-processor

(defstruct sys:opt-processor nil
  od-list
  (od-hash (hash :equal-based)) ;; string to opt-desc
  (:postinit (me)
    me.(build-hash)))

(defun sys:opt-err (. args)
  (throwf 'opt-error . args))

(defun sys:opt-dash (name)
  `@(if (> (length name) 1) "-")-@name`)

(defmeth opt-desc check (me)
  (unless (or (functionp me.type)
              (fboundp me.type)
              (and (consp me.type) (eq (car me.type) 'list))
              (member me.type me.valid-types))
    (error "getopts: type must be a function or valid keyword, not ~s"
           me.type))
  (when me.long
    (when (< (length me.long) 2)
      (error "getopts: long option ~a has a short name" me.long))
    (when (eql [me.long 0] #\-)
      (error "getopts: long option ~a starts with - character" me.long)))
  (when me.short
    (when (neq (length me.short) 1)
      (error "getopts: short option ~a not one character long" me.short))
    (when (eql [me.short 0] #\-)
      (error "getopts: short option ~a starts with - character" me.short))))

(defmeth sys:opt-parsed convert-type (me)
  (let ((name (sys:opt-dash me.name))
        (type (or me.eff-type me.desc.type)))
    (when (and (neq type :bool)
               (eq me.arg :explicit-no))
      (sys:opt-err "Non-Boolean option ~a explicitly specified as false" name))
    (caseql type
      (:bool
        (set me.arg (neq me.arg :explicit-no)))
      (:dec (set me.arg
                 (or (and (r^$ #/[+\-]?\d+/ me.arg) (int-str me.arg))
                     (sys:opt-err "option ~a needs decimal integer arg, not ~a"
                              name me.arg))))
      (:hex (set me.arg
                 (or (and (r^$ #/[+\-]?[\da-fA-F]+/ me.arg) (int-str me.arg 16))
                     (sys:opt-err "option ~a needs hexadecimal integer arg, not ~a"
                              name me.arg))))
      (:oct (set me.arg
                 (or (and (r^$ #/[+\-]?[0-7]+/ me.arg) (int-str me.arg 8))
                     (sys:opt-err "option ~a needs octal integer arg, not ~a"
                              name me.arg))))
      (:cint (set me.arg
                  (cond
                    ((r^$ #/[+\-]?0x[\da-fA-F]+/ me.arg)
                     (int-str (regsub #/0x/ "" me.arg) 16))
                    ((r^$ #/[+\-]?0[0-7]+/ me.arg)
                     (int-str me.arg 8))
                    ((r^$ #/[+\-]?0[\da-fA-F]+/ me.arg)
                     (sys:opt-err "option ~a argument ~a non octal, but leading 0"
                              name me.arg))
                    ((r^$ #/[+\-]?\d+/ me.arg)
                     (int-str me.arg))
                    (t (sys:opt-err "option ~a needs C style numeric arg, not ~a"
                                name me.arg)))))
      (:float (set me.arg
                   (cond
                     ([[chand (orf (f^$ #/[+\-]?\d+[.]?([Ee][+\-]?\d+)?/)
                                   (f^$ #/[+\-]?\d*[.]?\d+([Ee][+\-]?\d+)?/))
                              flo-str] me.arg])
                     (t (sys:opt-err "option ~a needs floating-point arg, not ~a"
                                 name me.arg)))))
      (:str (set me.arg
                 (or (ignerr (read `"@{me.arg}"`))
                     (sys:opt-err "option ~a needs string lit syntax, ~a given"
                              name me.arg))))
      (t (cond
           ((and (consp type) (eq (car type) 'list))
              (let* ((rec-type (cadr type))
                     (pieces (split-str me.arg #/,/))
                     (sub-opts (mapcar (do new (sys:opt-parsed me.name @1
                                                               me.desc
                                                               rec-type))
                                       pieces)))
                (set me.arg (mapcar (usl arg) sub-opts))))
           ((or (symbolp me.type) (functionp me.type))
              (set me.arg (call me.desc.type me.arg))))))))

(defmeth opts lambda (me key : dfl)
  (iflet ((o [me.opt-hash key])) o.arg dfl))

(defmeth opts lambda-set (me key val)
  (iflet ((o [me.opt-hash key]))
    (set o.arg val)
    (error "opts: cannot set option ~s to ~s: no such option" key val)))

(defmeth opts add (me opt)
  (whenlet ((n opt.desc.short))
    (set [me.opt-hash n] opt))
  (whenlet ((n opt.desc.long))
    (set [me.opt-hash n] opt)))

(defmeth sys:opt-processor build-hash (me)
  (each ((od me.od-list))
    (each ((str (list od.long od.short)))
      (when (and str [me.od-hash str])
        (error "opt-processor: duplicate option ~s" str)))
    (unless (or od.long od.short)
      (error "opt-processor: no short or long name in option ~s" od))
    (each ((str (list od.long od.short)))
      (when str
        (set [me.od-hash str] od)))))

(defmeth sys:opt-processor parse-long (me out opt : arg)
  (iflet ((ieq (unless (stringp arg) (break-str opt "="))))
    (let ((oname [opt 0..ieq])
          (arg [opt (succ ieq)..:]))
      me.(parse-long out oname arg))
    (let ((od [me.od-hash opt]))
      (cond
        ((null od)
           (sys:opt-err "unrecognized option: --~a" opt))
        ((and arg od.arg-p)
           out.(add (new (sys:opt-parsed opt arg od))))
        ((stringp arg)
           (sys:opt-err "option --~a doesn't take an argument" opt))
        (od.arg-p
           (iflet ((arg (pop out.out-args)))
             out.(add (new (sys:opt-parsed opt arg od)))
             (sys:opt-err "option --~a requires an argument" opt)))
        (out.(add (new (sys:opt-parsed opt arg od))))))))

(defmeth sys:opt-processor parse-shorts (me out opts)
  (each ((o (split-str opts #//)))
    (iflet ((od [me.od-hash o]))
      (let ((arg (when od.arg-p
                   (when (> (length opts) 1)
                     (sys:opt-err "argument -~a includes -~a, which does not clump"
                            opts o))
                 (unless out.out-args
                   (sys:opt-err "option -~a requires an argument" o))
                 (pop out.out-args))))
      out.(add (new (sys:opt-parsed o arg od))))
    (sys:opt-err "unrecognized option: -~a" o))))

(defmeth sys:opt-processor parse-opts (me args)
  (let ((out (new opts in-args args out-args args opt-processor me)))
    (whilet ((arg (pop out.out-args)))
      (cond
        ((equal "--" arg) (return))
        ((r^ #/--no-/ arg) me.(parse-long out [arg 5..:] :explicit-no))
        ((r^ #/--/ arg) me.(parse-long out [arg 2..:]))
        ((r^ #/-.+/ arg) me.(parse-shorts out [arg 1..:]))
        (t (push arg out.out-args)
           (return))))
    out))

(defun sys:wdwrap (string columns)
  (let ((words (tok-str string #/\S+/))
        line)
    (build
      (whilet ((word (pop words))
               (wpart (cond
                        ((and word (r^$ #/\w+[\w\-]*\w[.,;:!?"]?/ word))
                           (split-str word #/-/))
                        (word (list word))))
               (wpart-orig wpart))
        (whilet ((wp0 (eq wpart wpart-orig))
                 (wp (pop wpart))
                 (w (if wp `@wp@(if wpart "-")`)))
          (cond
            ((not line)
               (set line w))
            ((> (+ (length line) (length w) 1) columns)
               (add line)
               (set line w))
            (t (set line `@line@(if wp0 " ")@w`)))))
      (if line
        (add line)))))

(defun opt (short long : (type :bool) helptext)
  (new opt-desc short short long long helptext helptext type type))

(defun getopts (opt-desc-list args)
  (let ((opr (new sys:opt-processor od-list opt-desc-list)))
    opr.(parse-opts args)))

(defun opthelp (opt-desc-list : (stream *stdout*))
  (let ((sorted [sort (copy-list (remove-if (op null @1.helptext)
                                            opt-desc-list)) :
                      (do if @1.long @1.long @1.short)])
        (undocumented (keep-if (op null @1.helptext) opt-desc-list)))
    (put-line "\nOptions:\n")
    (each ((od sorted))
      (let* ((type (cond
                     ((keywordp od.type) (upcase-str (tostringp od.type)))
                     ((and (consp od.type) (eq (car od.type) 'list))
                        (let ((ts (upcase-str (tostringp (cadr od.type)))))
                          `@ts[,@ts...]`))
                     (t "ARG")))
             (long (if od.long
                     `--@{od.long}@(if od.arg-p `=@type`)`))
             (short (if od.short
                      `-@{od.short}@(if od.arg-p ` @type`)`))
             (ls (cond
                   ((and long short) `@{long 21} (@short)`)
                   (long long)
                   (short `@{"" 21}  @short`)))
             (lines (if od.helptext  (sys:wdwrap od.helptext 43))))
        (put-line `  @{ls 34}@(pop lines)`)
        (while lines
          (put-line `  @{"" 34}@(pop lines)`))))
    (put-line)
    (when undocumented
      (put-line "Undocumented options:\n")
      (let* ((undoc-str `@{[mapcar sys:opt-dash
                                   (flatten (mappend (op list @1.short @1.long)
                                                     undocumented))] ", "}`))
        (each ((line (sys:wdwrap undoc-str 75)))
          (put-line `    @line`)))
      (put-line))
    (put-line "Notes:\n")
    (let* ((have-short (some sorted (usl short)))
           (have-long (some sorted (usl long)))
           (have-arg-p (some sorted (usl arg-p)))
           (have-bool (some sorted (op eq @1.type :bool)))
           (texts (list (if have-short
                          "Short options can be invoked with long syntax: \
                          \ for example, --a can be used when -a exists.\
                          \ Short no-argument options can be clumped into\
                          \ one argument as exemplified by -xyz. ")
                        (if have-bool
                          (if have-arg-p
                            "Options that take no argument are Boolean:"
                            (if undocumented
                              "All documented options are Boolean:"
                              "All options are Boolean:")))
                        (if have-bool
                          "they are true when present, false when absent.")
                        (if have-bool
                          (if have-arg-p
                            " The --no- prefix can explicitly specify \
                            \ Boolean options as false: if a Boolean option\
                            \ X exists,\
                            \ --no-X specifies it as false. This is useful\
                            \ for making false those options which default\
                            \ to true."
                            " The --no- prefix can explicitly specify \
                            \ options as false: if an X option exists,\
                            \ --no-X specifies it as false. This is useful\
                            \ for making false those options which default\
                            \ to true."))
                        (if (not have-long)
                          "Note the double dash on --no.")
                        (if (and have-short have-long)
                          "The --no- prefix can be applied to a short\
                          \ or long option name.")
                        (if have-arg-p
                          "The argument to a long option can be given in one\
                          \ argument as --option=arg or as a separate\
                          \ argument using --option arg.")
                        "The special argument -- can be used where an option\
                        \ may appear. It means \"end of options\": the\
                        \ arguments which follow are not treated as options\
                        \ even if they look like options.")))
      (mapdo (do put-line `  @1`)
             (sys:wdwrap `@{(flatten texts)}` 77)))
    (put-line)
    (whenlet ((types (keep-if [andf keywordp (op neq :bool)]
                              (uniq (mapcar (usl type) sorted)))))
      (put-line "Type legend:\n")
      (each ((ty types))
        (iflet ((ln (caseql ty
                      (:dec   "  DEC    - Decimal integer: -123, 0, 5, +73")
                      (:hex   "  HEX    - Hexadecimal integer -EF, 2D0, +9A")
                      (:oct   "  OCT    - Octal integer: -773, 5677, +326")
                      (:cint  "  CINT   - C-style integer: leading 0 octal,\
                              \ leading 0x hex, else decimal;\n\
                              \           leading sign allowed: -0777, 0xFDC, +123")
                      (:float "  FLOAT  - Floating-point: -1.3e+03, +5, 3.3,\
                              \ 3., .5, .12e9, 53.e-3, 3e-015")
                      (:str   "  STR    - String with embedded escapes, valid\
                              \ as TXR Lisp string literals\n\
                              \           syntax: foo, foo\\tbar, abc\\nxyz"))))
           (put-line ln)))
    (put-line))))
