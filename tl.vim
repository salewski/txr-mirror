" VIM Syntax file for txr
" Kaz Kylheku <kaz@kylheku.com>

" INSTALL-HOWTO:
"
" 1. Create the directory .vim/syntax in your home directory and
"    put the files txr.vim and txl.vim into this directory.
" 2. In your .vimrc, add this command to associate *.txr and *.tl
"    files with the txr and txl filetypes:
"    :au BufRead,BufNewFile *.txr set filetype=txr | set lisp
"    :au BufRead,BufNewFile *.tl set filetype=txl | set lisp
"
" If you want syntax highlighting to be on automatically (for any language)
" you need to add ":syntax on" in your .vimrc also. But you knew that already!
"
" This file is generated by the genvim.txr script in the TXR source tree.

syn case match
syn spell toplevel

setlocal iskeyword=a-z,A-Z,48-57,!,$,&,*,+,-,:,<,=,>,?,\\,_,~,/

syn keyword txl_keyword contained %e% %pi% * *args*
syn keyword txl_keyword contained *args-full* *e* *flo-dig* *flo-epsilon*
syn keyword txl_keyword contained *flo-max* *flo-min* *full-args* *gensym-counter*
syn keyword txl_keyword contained *keyword-package* *lib-version* *listener-hist-len* *listener-multi-line-p*
syn keyword txl_keyword contained *pi* *place-clobber-expander* *place-delete-expander* *place-macro*
syn keyword txl_keyword contained *place-update-expander* *print-base* *print-flo-precision* *random-state*
syn keyword txl_keyword contained *self-path* *stddebug* *stderr* *stdin*
syn keyword txl_keyword contained *stdlog* *stdnull* *stdout* *system-package*
syn keyword txl_keyword contained *txr-version* *unhandled-hook* *user-package* +
syn keyword txl_keyword contained - / /= :
syn keyword txl_keyword contained :append :args :atime :auto
syn keyword txl_keyword contained :blksize :blocks :cdigit :chars
syn keyword txl_keyword contained :continue :counter :cspace :ctime
syn keyword txl_keyword contained :cword-char :decline :dev :digit
syn keyword txl_keyword contained :downcase :env :equal-based :fd
syn keyword txl_keyword contained :filter :fini :finish :form
syn keyword txl_keyword contained :from-current :from-end :from-start :from_html
syn keyword txl_keyword contained :frompercent :fromurl :fun :function
syn keyword txl_keyword contained :gap :gid :greedy :hextoint
syn keyword txl_keyword contained :init :ino :instance :into
syn keyword txl_keyword contained :lfilt :lines :list :longest
syn keyword txl_keyword contained :maxgap :maxtimes :method :mingap
syn keyword txl_keyword contained :mintimes :mode :mtime :name
syn keyword txl_keyword contained :named :next-spec :nlink :nothrow
syn keyword txl_keyword contained :prio :rdev :real-time :repeat-spec
syn keyword txl_keyword contained :resolve :rfilt :shortest :size
syn keyword txl_keyword contained :space :static :string :symacro
syn keyword txl_keyword contained :times :to_html :tofloat :toint
syn keyword txl_keyword contained :tonumber :topercent :tourl :uid
syn keyword txl_keyword contained :upcase :var :vars :weak-keys
syn keyword txl_keyword contained :weak-vals :whole :word-char <
syn keyword txl_keyword contained <= = > >=
syn keyword txl_keyword contained abort abs abs-path-p acons
syn keyword txl_keyword contained acons-new aconsql-new acos ado
syn keyword txl_keyword contained alist-nremove alist-remove all and
syn keyword txl_keyword contained andf ap apf append
syn keyword txl_keyword contained append* append-each append-each* apply
syn keyword txl_keyword contained aret ash asin assoc
syn keyword txl_keyword contained assql atan atan2 atom
syn keyword txl_keyword contained bignump bindable bit block
syn keyword txl_keyword contained block* boundp break-str caaaaar
syn keyword txl_keyword contained caaaadr caaaar caaadar caaaddr
syn keyword txl_keyword contained caaadr caaar caadaar caadadr
syn keyword txl_keyword contained caadar caaddar caadddr caaddr
syn keyword txl_keyword contained caadr caar cadaaar cadaadr
syn keyword txl_keyword contained cadaar cadadar cadaddr cadadr
syn keyword txl_keyword contained cadar caddaar caddadr caddar
syn keyword txl_keyword contained cadddar caddddr cadddr caddr
syn keyword txl_keyword contained cadr call call-clobber-expander call-delete-expander
syn keyword txl_keyword contained call-finalizers call-super-fun call-super-method call-update-expander
syn keyword txl_keyword contained callf car caseq caseql
syn keyword txl_keyword contained casequal cat-str cat-streams cat-vec
syn keyword txl_keyword contained catch catenated-stream-p catenated-stream-push cdaaaar
syn keyword txl_keyword contained cdaaadr cdaaar cdaadar cdaaddr
syn keyword txl_keyword contained cdaadr cdaar cdadaar cdadadr
syn keyword txl_keyword contained cdadar cdaddar cdadddr cdaddr
syn keyword txl_keyword contained cdadr cdar cddaaar cddaadr
syn keyword txl_keyword contained cddaar cddadar cddaddr cddadr
syn keyword txl_keyword contained cddar cdddaar cdddadr cdddar
syn keyword txl_keyword contained cddddar cdddddr cddddr cdddr
syn keyword txl_keyword contained cddr cdr ceil chain
syn keyword txl_keyword contained chand chdir chmod chr-int
syn keyword txl_keyword contained chr-isalnum chr-isalpha chr-isascii chr-isblank
syn keyword txl_keyword contained chr-iscntrl chr-isdigit chr-isgraph chr-islower
syn keyword txl_keyword contained chr-isprint chr-ispunct chr-isspace chr-isunisp
syn keyword txl_keyword contained chr-isupper chr-isxdigit chr-num chr-str
syn keyword txl_keyword contained chr-str-set chr-tolower chr-toupper chrp
syn keyword txl_keyword contained clamp clear-error clear-struct close-stream
syn keyword txl_keyword contained closelog cmp-str collect-each collect-each*
syn keyword txl_keyword contained comb compl-span-str cond conda
syn keyword txl_keyword contained cons conses conses* consp
syn keyword txl_keyword contained constantp copy copy-alist copy-cons
syn keyword txl_keyword contained copy-hash copy-list copy-str copy-struct
syn keyword txl_keyword contained copy-vec cos count-if countq
syn keyword txl_keyword contained countql countqual cum-norm-dist daemon
syn keyword txl_keyword contained dec defex define-modify-macro define-place-macro
syn keyword txl_keyword contained defmacro defmeth defparm defparml
syn keyword txl_keyword contained defplace defstruct defsymacro defun
syn keyword txl_keyword contained defvar defvarl del delay
syn keyword txl_keyword contained delete-package display-width do dohash
syn keyword txl_keyword contained dotimes downcase-str drop drop-until
syn keyword txl_keyword contained drop-while dup dupfd dwim
syn keyword txl_keyword contained each each* eighth empty
syn keyword txl_keyword contained endgrent endpwent ensure-dir env
syn keyword txl_keyword contained env-fbind env-hash env-vbind eq
syn keyword txl_keyword contained eql equal errno error
syn keyword txl_keyword contained eval evenp exception-subtype-p exec
syn keyword txl_keyword contained exit exit* exp expand-right
syn keyword txl_keyword contained expt exptmod false fboundp
syn keyword txl_keyword contained fifth fileno filter-equal filter-string-tree
syn keyword txl_keyword contained finalize find find-frame find-if
syn keyword txl_keyword contained find-max find-min find-package find-struct-type
syn keyword txl_keyword contained first fixnump flatcar flatcar*
syn keyword txl_keyword contained flatten flatten* flet flip
syn keyword txl_keyword contained flipargs flo-dig flo-epsilon flo-int
syn keyword txl_keyword contained flo-max flo-max-dig flo-min flo-str
syn keyword txl_keyword contained floatp floor flush-stream fmakunbound
syn keyword txl_keyword contained for for* force fork
syn keyword txl_keyword contained format fourth from fstat
syn keyword txl_keyword contained fun func-get-env func-get-form func-get-name
syn keyword txl_keyword contained func-set-env functionp gcd gen
syn keyword txl_keyword contained generate gensym gequal get-byte
syn keyword txl_keyword contained get-char get-clobber-expander get-delete-expander get-error
syn keyword txl_keyword contained get-error-str get-frames get-hash-userdata get-indent
syn keyword txl_keyword contained get-indent-mode get-line get-lines get-list-from-stream
syn keyword txl_keyword contained get-sig-handler get-string get-string-from-stream get-update-expander
syn keyword txl_keyword contained getegid getenv geteuid getgid
syn keyword txl_keyword contained getgrent getgrgid getgrnam getgroups
syn keyword txl_keyword contained gethash getitimer getpid getppid
syn keyword txl_keyword contained getpwent getpwnam getpwuid getuid
syn keyword txl_keyword contained ginterate giterate glob glob-altdirfunc
syn keyword txl_keyword contained glob-brace glob-err glob-mark glob-nocheck
syn keyword txl_keyword contained glob-noescape glob-nomagic glob-nosort glob-onlydir
syn keyword txl_keyword contained glob-period glob-tilde glob-tilde-check greater
syn keyword txl_keyword contained group-by group-reduce gun handle
syn keyword txl_keyword contained handler-bind hash hash-alist hash-begin
syn keyword txl_keyword contained hash-construct hash-count hash-diff hash-eql
syn keyword txl_keyword contained hash-equal hash-from-pairs hash-isec hash-keys
syn keyword txl_keyword contained hash-list hash-next hash-pairs hash-proper-subset
syn keyword txl_keyword contained hash-revget hash-subset hash-uni hash-update
syn keyword txl_keyword contained hash-update-1 hash-values hashp have
syn keyword txl_keyword contained html-decode html-encode iapply identity
syn keyword txl_keyword contained ido if ifa iff
syn keyword txl_keyword contained iffi iflet ignerr in
syn keyword txl_keyword contained inc inc-indent indent-code indent-data
syn keyword txl_keyword contained indent-off inhash int-chr int-flo
syn keyword txl_keyword contained int-str integerp intern interp-fun-p
syn keyword txl_keyword contained interpose invoke-catch ip ipf
syn keyword txl_keyword contained iread isqrt itimer-prov itimer-real
syn keyword txl_keyword contained itimer-virtual juxt keep-if keep-if*
syn keyword txl_keyword contained keyword-package keywordp kill labels
syn keyword txl_keyword contained lambda last lazy-str lazy-str-force
syn keyword txl_keyword contained lazy-str-force-upto lazy-str-get-trailing-list lazy-stream-cons lazy-stringp
syn keyword txl_keyword contained lcm lcons lcons-fun lconsp
syn keyword txl_keyword contained ldiff length length-list length-str
syn keyword txl_keyword contained length-str-< length-str-<= length-str-> length-str->=
syn keyword txl_keyword contained length-vec lequal less let
syn keyword txl_keyword contained let* lexical-fun-p lexical-lisp1-binding lexical-var-p
syn keyword txl_keyword contained lib-version link lisp-parse list
syn keyword txl_keyword contained list* list-str list-vec list-vector
syn keyword txl_keyword contained listp load log log-alert
syn keyword txl_keyword contained log-auth log-authpriv log-cons log-crit
syn keyword txl_keyword contained log-daemon log-debug log-emerg log-err
syn keyword txl_keyword contained log-info log-ndelay log-notice log-nowait
syn keyword txl_keyword contained log-odelay log-perror log-pid log-user
syn keyword txl_keyword contained log-warning log10 log2 logand
syn keyword txl_keyword contained logior lognot logtest logtrunc
syn keyword txl_keyword contained logxor lstat mac-param-bind macro-form-p
syn keyword txl_keyword contained macro-time macroexpand macroexpand-1 macrolet
syn keyword txl_keyword contained major make-catenated-stream make-env make-hash
syn keyword txl_keyword contained make-lazy-cons make-like make-package make-random-state
syn keyword txl_keyword contained make-similar-hash make-string-byte-input-stream make-string-input-stream make-string-output-stream
syn keyword txl_keyword contained make-strlist-output-stream make-struct make-struct-type make-sym
syn keyword txl_keyword contained make-time make-time-utc make-trie makedev
syn keyword txl_keyword contained makunbound mapcar mapcar* mapdo
syn keyword txl_keyword contained mapf maphash mappend mappend*
syn keyword txl_keyword contained mask match-fun match-regex match-regex-right
syn keyword txl_keyword contained match-regst match-regst-right match-str match-str-tree
syn keyword txl_keyword contained max member member-if memq
syn keyword txl_keyword contained memql memqual merge meth
syn keyword txl_keyword contained method min minor minusp
syn keyword txl_keyword contained mkdir mknod mkstring mlet
syn keyword txl_keyword contained mod multi multi-sort n-choose-k
syn keyword txl_keyword contained n-perm-k nconc new nil
syn keyword txl_keyword contained nilf ninth none not
syn keyword txl_keyword contained notf nreconc nreverse nthcdr
syn keyword txl_keyword contained null nullify num-chr num-str
syn keyword txl_keyword contained numberp oand obtain obtain-block
syn keyword txl_keyword contained oddp op open-command open-directory
syn keyword txl_keyword contained open-file open-fileno open-files open-files*
syn keyword txl_keyword contained open-pipe open-process open-tail openlog
syn keyword txl_keyword contained opip or orf package-alist
syn keyword txl_keyword contained package-name package-symbols packagep pad
syn keyword txl_keyword contained partition partition* partition-by path-blkdev-p
syn keyword txl_keyword contained path-chrdev-p path-dir-p path-executable-to-me-p path-exists-p
syn keyword txl_keyword contained path-file-p path-mine-p path-my-group-p path-newer
syn keyword txl_keyword contained path-older path-pipe-p path-private-to-me-p path-same-object
syn keyword txl_keyword contained path-setgid-p path-setuid-p path-sock-p path-sticky-p
syn keyword txl_keyword contained path-symlink-p path-writable-to-me-p perm pipe
syn keyword txl_keyword contained place-form-p placelet placelet* plusp
syn keyword txl_keyword contained poll poll-err poll-in poll-nval
syn keyword txl_keyword contained poll-out poll-pri poll-rdband poll-wrband
syn keyword txl_keyword contained pop pos pos-if pos-max
syn keyword txl_keyword contained pos-min posq posql posqual
syn keyword txl_keyword contained pppred ppred pprinl pprint
syn keyword txl_keyword contained pprof pred prinl print
syn keyword txl_keyword contained prof prog1 progn promisep
syn keyword txl_keyword contained prop proper-listp pset push
syn keyword txl_keyword contained pushhash pushnew put-byte put-char
syn keyword txl_keyword contained put-line put-lines put-string put-strings
syn keyword txl_keyword contained pwd qquote qref quote
syn keyword txl_keyword contained raise rand random random-fixnum
syn keyword txl_keyword contained random-state-p range range* range-regex
syn keyword txl_keyword contained rangep rcomb rcons read
syn keyword txl_keyword contained readlink real-time-stream-p reduce-left reduce-right
syn keyword txl_keyword contained ref refset regex-compile regex-parse
syn keyword txl_keyword contained regexp register-exception-subtypes regsub rehome-sym
syn keyword txl_keyword contained remhash remove-if remove-if* remove-path
syn keyword txl_keyword contained remq remq* remql remql*
syn keyword txl_keyword contained remqual remqual* rename-path repeat
syn keyword txl_keyword contained replace replace-list replace-str replace-struct
syn keyword txl_keyword contained replace-vec reset-struct rest ret
syn keyword txl_keyword contained retf return return* return-from
syn keyword txl_keyword contained revappend reverse rlcp rlet
syn keyword txl_keyword contained rotate rperm rplaca rplacd
syn keyword txl_keyword contained run s-ifblk s-ifchr s-ifdir
syn keyword txl_keyword contained s-ififo s-iflnk s-ifmt s-ifreg
syn keyword txl_keyword contained s-ifsock s-irgrp s-iroth s-irusr
syn keyword txl_keyword contained s-irwxg s-irwxo s-irwxu s-isgid
syn keyword txl_keyword contained s-isuid s-isvtx s-iwgrp s-iwoth
syn keyword txl_keyword contained s-iwusr s-ixgrp s-ixoth s-ixusr
syn keyword txl_keyword contained search search-regex search-regst search-str
syn keyword txl_keyword contained search-str-tree second seek-stream select
syn keyword txl_keyword contained self-path seqp set set-diff
syn keyword txl_keyword contained set-hash-userdata set-indent set-indent-mode set-sig-handler
syn keyword txl_keyword contained setegid setenv seteuid setgid
syn keyword txl_keyword contained setgrent sethash setitimer setlogmask
syn keyword txl_keyword contained setpwent setuid seventh sh
syn keyword txl_keyword contained shift shuffle sig-abrt sig-alrm
syn keyword txl_keyword contained sig-bus sig-check sig-chld sig-cont
syn keyword txl_keyword contained sig-fpe sig-hup sig-ill sig-int
syn keyword txl_keyword contained sig-io sig-iot sig-kill sig-pipe
syn keyword txl_keyword contained sig-poll sig-prof sig-pwr sig-quit
syn keyword txl_keyword contained sig-segv sig-stkflt sig-stop sig-sys
syn keyword txl_keyword contained sig-term sig-trap sig-tstp sig-ttin
syn keyword txl_keyword contained sig-ttou sig-urg sig-usr1 sig-usr2
syn keyword txl_keyword contained sig-vtalrm sig-winch sig-xcpu sig-xfsz
syn keyword txl_keyword contained sign-extend sin sixth size-vec
syn keyword txl_keyword contained slot slotp slotset some
syn keyword txl_keyword contained sort sort-group source-loc source-loc-str
syn keyword txl_keyword contained span-str special-operator-p special-var-p splice
syn keyword txl_keyword contained split split-str split-str-set sqrt
syn keyword txl_keyword contained sssucc ssucc stat static-slot
syn keyword txl_keyword contained static-slot-ensure static-slot-p static-slot-set stdlib
syn keyword txl_keyword contained str< str<= str= str>
syn keyword txl_keyword contained str>= stream-get-prop stream-set-prop streamp
syn keyword txl_keyword contained string-extend string-lt stringp struct-type
syn keyword txl_keyword contained struct-type-p structp sub sub-list
syn keyword txl_keyword contained sub-str sub-vec subtypep succ
syn keyword txl_keyword contained super super-method suspend swap
syn keyword txl_keyword contained symacrolet symbol-function symbol-name symbol-package
syn keyword txl_keyword contained symbol-value symbolp symlink sys:*lisp1*
syn keyword txl_keyword contained sys:abscond* sys:abscond-from sys:bad-slot-syntax sys:capture-cont
syn keyword txl_keyword contained sys:do-path-test sys:dwim-del sys:dwim-set sys:eval-err
syn keyword txl_keyword contained sys:expand sys:expr sys:fbind sys:gc
syn keyword txl_keyword contained sys:gc-set-delta sys:get-fb sys:get-vb sys:handle-bad-syntax
syn keyword txl_keyword contained sys:l1-setq sys:l1-val sys:lbind sys:lisp1-setq
syn keyword txl_keyword contained sys:lisp1-value sys:load sys:make-struct-type sys:mark-special
syn keyword txl_keyword contained sys:obtain-impl sys:path-access sys:path-examine sys:path-test
syn keyword txl_keyword contained sys:path-test-mode sys:pl-expand sys:placelet-1 sys:prune-nil-inits
syn keyword txl_keyword contained sys:qquote sys:quasi sys:quasilist sys:reg-expand-nongreedy
syn keyword txl_keyword contained sys:reg-optimize sys:rplaca sys:rplacd sys:setq
syn keyword txl_keyword contained sys:setqf sys:splice sys:struct-lit sys:sym-clobber-expander
syn keyword txl_keyword contained sys:sym-delete-expander sys:sym-update-expander sys:top-fb sys:top-vb
syn keyword txl_keyword contained sys:unquote sys:var sys:with-saved-vars syslog
syn keyword txl_keyword contained system-package t take take-until
syn keyword txl_keyword contained take-while tan tb tc
syn keyword txl_keyword contained tenth test-set-indent-mode tf third
syn keyword txl_keyword contained throw throwf time time-fields-local
syn keyword txl_keyword contained time-fields-utc time-string-local time-string-utc time-struct-local
syn keyword txl_keyword contained time-struct-utc time-usec to tofloat
syn keyword txl_keyword contained toint tok-str tok-where tostring
syn keyword txl_keyword contained tostringp tprint transpose tree-bind
syn keyword txl_keyword contained tree-case tree-find trie-add trie-compress
syn keyword txl_keyword contained trie-lookup-begin trie-lookup-feed-char trie-value-at trim-str
syn keyword txl_keyword contained true trunc trunc-rem truncate-stream
syn keyword txl_keyword contained tuples txr-case txr-case-impl txr-if
syn keyword txl_keyword contained txr-sym txr-version txr-when typecase
syn keyword txl_keyword contained typeof typep umeth umethod
syn keyword txl_keyword contained unget-byte unget-char uniq unique
syn keyword txl_keyword contained unless unquote unsetenv until
syn keyword txl_keyword contained until* unwind-protect upcase-str update
syn keyword txl_keyword contained url-decode url-encode user-package usleep
syn keyword txl_keyword contained vec vec-list vec-push vec-set-length
syn keyword txl_keyword contained vecref vector vector-list vectorp
syn keyword txl_keyword contained w-continued w-coredump w-exitstatus w-ifcontinued
syn keyword txl_keyword contained w-ifexited w-ifsignaled w-ifstopped w-nohang
syn keyword txl_keyword contained w-stopsig w-termsig w-untraced wait
syn keyword txl_keyword contained weave when whenlet where
syn keyword txl_keyword contained while while* whilet width
syn keyword txl_keyword contained width-check with-clobber-expander with-delete-expander with-gensyms
syn keyword txl_keyword contained with-hash-iter with-in-string-byte-stream with-in-string-stream with-objects
syn keyword txl_keyword contained with-out-string-stream with-out-strlist-stream with-resources with-stream
syn keyword txl_keyword contained with-update-expander wrap wrap* yield
syn keyword txl_keyword contained yield-from zap zerop zip
syn match txr_nested_error "[^\t ]\+" contained
syn match txr_variable "\(@[ \t]*\)[*]\?[ \t]*[A-Za-z_][A-Za-z_0-9]*"
syn match txr_splicevar "@[ \t,*@]*[A-Za-z_][A-Za-z_0-9]*"
syn match txr_metanum "@\+[0-9]\+"
syn match txr_badesc "\\." contained
syn match txr_escat "\\@" contained
syn match txr_stresc "\\[abtnvfre\\ \n"`']" contained
syn match txr_numesc "\\x[0-9A-Fa-f]\+;\?" contained
syn match txr_numesc "\\[0-7]\+;\?" contained
syn match txr_regesc "\\[abtnvfre\\ \n/sSdDwW()\|.*?+~&%\[\]\-]" contained

syn match txr_chr "#\\x[0-9A-Fa-f]\+"
syn match txr_chr "#\\o[0-7]\+"
syn match txr_chr "#\\[^ \t\nA-Za-z_0-9]"
syn match txr_chr "#\\[A-Za-z_0-9]\+"
syn match txr_ncomment ";.*"

syn match txr_dot "\." contained
syn match txr_num "#x[+\-]\?[0-9A-Fa-f]\+"
syn match txr_num "#o[+\-]\?[0-7]\+"
syn match txr_num "#b[+\-]\?[01]\+"
syn match txr_ident "[A-Za-z_0-9!$%&*+\-<=>?\\_~]*[A-Za-z_!$%&*+\-<=>?\\_~^][A-Za-z_0-9!$%&*+\-<=>?\\_~^]*" contained
syn match txl_ident "[:@][A-Za-z_0-9!$%&*+\-<=>?\\_~^/]\+"
syn match txr_braced_ident "[:][A-Za-z_0-9!$%&*+\-<=>?\\_~^/]\+" contained
syn match txl_ident "[A-Za-z_0-9!$%&*+\-<=>?\\_~/]*[A-Za-z_!$%&*+\-<=>?\\_~^/#][A-Za-z_0-9!$%&*+\-<=>?\\_~^/#]*"
syn match txr_num "[+\-]\?[0-9]\+\([^A-Za-z_0-9!$%&*+\-<=>?\\_~^/#]\|\n\)"me=e-1
syn match txr_badnum "[+\-]\?[0-9]*[.][0-9]\+\([eE][+\-]\?[0-9]\+\)\?[A-Za-z_!$%&*+\-<=>?\\_~^/#]\+"
syn match txr_num "[+\-]\?[0-9]*[.][0-9]\+\([eE][+\-]\?[0-9]\+\)\?\([^A-Za-z_0-9!$%&*+\-<=>?\\_~^/#]\|\n\)"me=e-1
syn match txr_num "[+\-]\?[0-9]\+\([eE][+\-]\?[0-9]\+\)\([^A-Za-z_0-9!$%&*+\-<=>?\\_~^/#]\|\n\)"me=e-1
syn match txl_ident ":"
syn match txl_splice "[ \t,]\|,[*]"

syn match txr_unquote "," contained
syn match txr_splice ",\*" contained
syn match txr_quote "'" contained
syn match txr_quote "\^" contained
syn match txr_dotdot "\.\." contained
syn match txr_metaat "@" contained

syn region txr_bracevar matchgroup=Delimiter start="@[ \t]*[*]\?{" matchgroup=Delimiter end="}" contains=txr_num,txl_ident,txl_splice,txl_metanum,txr_metaat,txr_braced_ident,txr_dot,txr_dotdot,txr_string,txr_list,txr_bracket,txr_mlist,txr_mbracket,txr_regex,txr_quasilit,txr_chr,txl_splice,txr_nested_error
syn region txr_list matchgroup=Delimiter start="#\?H\?(" matchgroup=Delimiter end=")" contains=txl_keyword,txr_string,txl_regex,txr_num,txr_badnum,txl_ident,txr_metanum,txr_list,txr_bracket,txr_mlist,txr_mbracket,txr_quasilit,txr_chr,txr_quote,txr_unquote,txr_splice,txr_dot,txr_dotdot,txr_metaat,txr_ncomment,txr_nested_error
syn region txr_bracket matchgroup=Delimiter start="\[" matchgroup=Delimiter end="\]" contains=txl_keyword,txr_string,txl_regex,txr_num,txr_badnum,txl_ident,txr_metanum,txr_list,txr_bracket,txr_mlist,txr_mbracket,txr_quasilit,txr_chr,txr_quote,txr_unquote,txr_splice,txr_dot,txr_dotdot,txr_metaat,txr_ncomment,txr_nested_error
syn region txr_mlist matchgroup=Delimiter start="@[ \t^',]*(" matchgroup=Delimiter end=")" contains=txl_keyword,txr_string,txl_regex,txr_num,txr_badnum,txl_ident,txr_metanum,txr_list,txr_bracket,txr_mlist,txr_mbracket,txr_quasilit,txr_chr,txr_quote,txr_unquote,txr_splice,txr_dot,txr_dotdot,txr_metaat,txr_ncomment,txr_nested_error
syn region txr_mbracket matchgroup=Delimiter start="@[ \t^',]*\[" matchgroup=Delimiter end="\]" contains=txl_keyword,txr_string,txl_regex,txr_num,txr_badnum,txl_ident,txr_metanum,txr_list,txr_bracket,txr_mlist,txr_mbracket,txr_quasilit,txr_chr,txr_quote,txr_unquote,txr_splice,txr_dot,txr_dotdot,txr_metaat,txr_ncomment,txr_nested_error
syn region txr_string start=+#\?\*\?"+ end=+["\n]+ contains=txr_stresc,txr_numesc,txr_badesc
syn region txr_quasilit start=+#\?\*\?`+ end=+[`\n]+ contains=txr_splicevar,txr_metanum,txr_bracevar,txr_mlist,txr_mbracket,txr_escat,txr_stresc,txr_numesc,txr_badesc
syn region txr_regex start="/" end="[/\n]" contains=txr_regesc,txr_numesc,txr_badesc
syn region txl_regex start="#/" end="[/\n]" contains=txr_regesc,txr_numesc,txr_badesc

hi def link txr_at Special
hi def link txr_atstar Special
hi def link txr_atat Special
hi def link txr_comment Comment
hi def link txr_ncomment Comment
hi def link txr_hashbang Preproc
hi def link txr_contin Preproc
hi def link txr_char String
hi def link txr_keyword Keyword
hi def link txl_keyword Type
hi def link txr_string String
hi def link txr_chr String
hi def link txr_quasilit String
hi def link txr_regex String
hi def link txl_regex String
hi def link txr_regdir String
hi def link txr_variable Identifier
hi def link txr_splicevar Identifier
hi def link txr_metanum Identifier
hi def link txr_escat Special
hi def link txr_stresc Special
hi def link txr_numesc Special
hi def link txr_regesc Special
hi def link txr_badesc Error
hi def link txr_ident Identifier
hi def link txl_ident Identifier
hi def link txr_num Number
hi def link txr_badnum Error
hi def link txr_quote Special
hi def link txr_unquote Special
hi def link txr_splice Special
hi def link txr_dot Special
hi def link txr_dotdot Special
hi def link txr_metaat Special
hi def link txr_munqspl Special
hi def link txl_splice Special
hi def link txr_error Error
hi def link txr_nested_error Error

let b:current_syntax = "lisp"

set lispwords=ado,ap,append-each,append-each*,aret,block,block*,caseq,caseql,casequal,catch,collect-each,collect-each*,cond,conda,dec,defex,define-modify-macro,define-place-macro,defmacro,defmeth,defparm,defparml,defplace,defstruct,defsymacro,defun,defvar,defvarl,del,delay,do,dohash,dotimes,each,each*,flet,flip,for,for*,fun,gen,gun,handle,handler-bind,ido,if,ifa,iflet,ignerr,ip,labels,lambda,lcons,let,let*,load,mac-param-bind,macro-time,macrolet,meth,mlet,new,obtain,obtain-block,op,placelet,placelet*,pop,pprof,prof,prog1,progn,push,pushnew,ret,return,return-from,rlet,splice,suspend,symacrolet,sys:abscond-from,sys:expr,sys:fbind,sys:l1-val,sys:lbind,sys:lisp1-value,sys:path-examine,sys:path-test,sys:placelet-1,sys:splice,sys:struct-lit,sys:unquote,sys:var,sys:with-saved-vars,tb,tc,tree-bind,tree-case,txr-case,txr-case-impl,txr-if,txr-when,typecase,umeth,unless,unquote,until,until*,unwind-protect,when,whenlet,while,while*,whilet,with-clobber-expander,with-delete-expander,with-gensyms,with-hash-iter,with-in-string-byte-stream,with-in-string-stream,with-objects,with-out-string-stream,with-out-strlist-stream,with-resources,with-stream,with-update-expander,yield,yield-from,zap,:method,:function,:init,:fini
