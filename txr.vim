" VIM Syntax file for txr
" Kaz Kylheku <kaz@kylheku.com>

" INSTALL-HOWTO:
"
" 1. Create the directory .vim/syntax in your home directory and
"    put the files txr.vim and tl.vim into this directory.
" 2. In your .vimrc, add this command to associate *.txr and *.tl
"    files with the txr and tl filetypes:
"    :au BufRead,BufNewFile *.txr set filetype=txr | set lisp
"    :au BufRead,BufNewFile *.tl set filetype=tl | set lisp
"
" If you want syntax highlighting to be on automatically (for any language)
" you need to add ":syntax on" in your .vimrc also. But you knew that already!
"
" This file is generated by the genvim.txr script in the TXR source tree.

syn case match
syn spell toplevel

setlocal iskeyword=a-z,A-Z,48-57,!,$,&,*,+,-,:,<,=,>,?,\\,_,~,/

syn keyword tl_keyword contained %e% %pi% * *args*
syn keyword tl_keyword contained *args-full* *e* *flo-dig* *flo-epsilon*
syn keyword tl_keyword contained *flo-max* *flo-min* *full-args* *gensym-counter*
syn keyword tl_keyword contained *lib-version* *listener-hist-len* *listener-multi-line-p* *listener-sel-inclusive-p*
syn keyword tl_keyword contained *load-path* *package* *package-alist* *param-macro*
syn keyword tl_keyword contained *pi* *place-clobber-expander* *place-delete-expander* *place-macro*
syn keyword tl_keyword contained *place-update-expander* *print-base* *print-circle* *print-flo-digits*
syn keyword tl_keyword contained *print-flo-format* *print-flo-precision* *random-state* *random-warmup*
syn keyword tl_keyword contained *self-path* *stddebug* *stderr* *stdin*
syn keyword tl_keyword contained *stdlog* *stdnull* *stdout* *trace-output*
syn keyword tl_keyword contained *txr-version* *unhandled-hook* + -
syn keyword tl_keyword contained / /= : :abandoned
syn keyword tl_keyword contained :addr :apf :append :args
syn keyword tl_keyword contained :atime :auto :awk-file :awk-rec
syn keyword tl_keyword contained :begin :begin-file :blksize :blocks
syn keyword tl_keyword contained :bool :byte-oriented :cdigit :chars
syn keyword tl_keyword contained :cint :close :continue :counter
syn keyword tl_keyword contained :cspace :ctime :cword-char :dec
syn keyword tl_keyword contained :decline :dev :digit :downcase
syn keyword tl_keyword contained :end :end-file :env :equal-based
syn keyword tl_keyword contained :explicit-no :fallback :fd :filter
syn keyword tl_keyword contained :fini :finish :float :form
syn keyword tl_keyword contained :from-current :from-end :from-start :from_html
syn keyword tl_keyword contained :frombase64 :fromhtml :frompercent :fromurl
syn keyword tl_keyword contained :fun :function :gap :gid
syn keyword tl_keyword contained :greedy :hex :hextoint :inf
syn keyword tl_keyword contained :init :ino :inp :inputs
syn keyword tl_keyword contained :instance :into :key :let
syn keyword tl_keyword contained :lfilt :lines :list :local
syn keyword tl_keyword contained :longest :mandatory :maxgap :maxtimes
syn keyword tl_keyword contained :method :mingap :mintimes :mode
syn keyword tl_keyword contained :mtime :name :named :next-spec
syn keyword tl_keyword contained :nlink :nothrow :oct :outf
syn keyword tl_keyword contained :outp :output :postinit :prio
syn keyword tl_keyword contained :rdev :real-time :reflect :repeat-spec
syn keyword tl_keyword contained :resolve :rfilt :set :set-file
syn keyword tl_keyword contained :shortest :size :space :static
syn keyword tl_keyword contained :str :string :symacro :times
syn keyword tl_keyword contained :tlist :to_html :tobase64 :tofloat
syn keyword tl_keyword contained :tohtml :tohtml* :toint :tonumber
syn keyword tl_keyword contained :topercent :tourl :uid :upcase
syn keyword tl_keyword contained :use :use-from :use-syms :userdata
syn keyword tl_keyword contained :var :vars :weak-keys :weak-vals
syn keyword tl_keyword contained :whole :word-char :wrap <
syn keyword tl_keyword contained <= = > >=
syn keyword tl_keyword contained abort abs abs-path-p acons
syn keyword tl_keyword contained acons-new aconsql-new acos ado
syn keyword tl_keyword contained af-inet af-inet6 af-unix af-unspec
syn keyword tl_keyword contained ai-addrconfig ai-all ai-canonname ai-numerichost
syn keyword tl_keyword contained ai-numericserv ai-passive ai-v4mapped alet
syn keyword tl_keyword contained alist-nremove alist-remove all and
syn keyword tl_keyword contained andf ap apf append
syn keyword tl_keyword contained append* append-each append-each* apply
syn keyword tl_keyword contained aret ash asin assoc
syn keyword tl_keyword contained assql at-exit-call at-exit-do-not-call atan
syn keyword tl_keyword contained atan2 atom awk base64-decode
syn keyword tl_keyword contained base64-encode bignump bindable bit
syn keyword tl_keyword contained block block* boundp break-str
syn keyword tl_keyword contained brkint bs0 bs1 bsdly
syn keyword tl_keyword contained build build-list butlast butlastn
syn keyword tl_keyword contained caaaaar caaaadr caaaar caaadar
syn keyword tl_keyword contained caaaddr caaadr caaar caadaar
syn keyword tl_keyword contained caadadr caadar caaddar caadddr
syn keyword tl_keyword contained caaddr caadr caar cadaaar
syn keyword tl_keyword contained cadaadr cadaar cadadar cadaddr
syn keyword tl_keyword contained cadadr cadar caddaar caddadr
syn keyword tl_keyword contained caddar cadddar caddddr cadddr
syn keyword tl_keyword contained caddr cadr call call-clobber-expander
syn keyword tl_keyword contained call-delete-expander call-finalizers call-super-fun call-super-method
syn keyword tl_keyword contained call-update-expander callf car caseq
syn keyword tl_keyword contained caseq* caseql caseql* casequal
syn keyword tl_keyword contained casequal* cat-str cat-streams cat-vec
syn keyword tl_keyword contained catch catch* catenated-stream-p catenated-stream-push
syn keyword tl_keyword contained cbaud cbaudex cdaaaar cdaaadr
syn keyword tl_keyword contained cdaaar cdaadar cdaaddr cdaadr
syn keyword tl_keyword contained cdaar cdadaar cdadadr cdadar
syn keyword tl_keyword contained cdaddar cdadddr cdaddr cdadr
syn keyword tl_keyword contained cdar cddaaar cddaadr cddaar
syn keyword tl_keyword contained cddadar cddaddr cddadr cddar
syn keyword tl_keyword contained cdddaar cdddadr cdddar cddddar
syn keyword tl_keyword contained cdddddr cddddr cdddr cddr
syn keyword tl_keyword contained cdr ceil ceil-rem chain
syn keyword tl_keyword contained chand chdir chmod chr-digit
syn keyword tl_keyword contained chr-int chr-isalnum chr-isalpha chr-isascii
syn keyword tl_keyword contained chr-isblank chr-iscntrl chr-isdigit chr-isgraph
syn keyword tl_keyword contained chr-islower chr-isprint chr-ispunct chr-isspace
syn keyword tl_keyword contained chr-isunisp chr-isupper chr-isxdigit chr-num
syn keyword tl_keyword contained chr-str chr-str-set chr-tolower chr-toupper
syn keyword tl_keyword contained chr-xdigit chrp clamp clear-dirty
syn keyword tl_keyword contained clear-error clear-struct clearhash clocal
syn keyword tl_keyword contained close-stream closelog cmp-str cmspar
syn keyword tl_keyword contained collect-each collect-each* comb command-get
syn keyword tl_keyword contained command-get-lines command-get-string command-put command-put-lines
syn keyword tl_keyword contained command-put-string compare-swap compile-defr-warning compile-error
syn keyword tl_keyword contained compile-warning compl-span-str cond conda
syn keyword tl_keyword contained condlet cons conses conses*
syn keyword tl_keyword contained consp constantp copy copy-alist
syn keyword tl_keyword contained copy-cons copy-hash copy-list copy-str
syn keyword tl_keyword contained copy-struct copy-vec cos count-if
syn keyword tl_keyword contained countq countql countqual cr0
syn keyword tl_keyword contained cr1 cr2 cr3 crdly
syn keyword tl_keyword contained cread crtscts crypt cs5
syn keyword tl_keyword contained cs6 cs7 cs8 csize
syn keyword tl_keyword contained cstopb cum-norm-dist daemon dec
syn keyword tl_keyword contained defer-warning defex define-accessor define-modify-macro
syn keyword tl_keyword contained define-param-expander define-place-macro defmacro defmeth
syn keyword tl_keyword contained defpackage defparm defparml defplace
syn keyword tl_keyword contained defstruct defsymacro defun defvar
syn keyword tl_keyword contained defvarl del delay delete-package
syn keyword tl_keyword contained display-width do dohash dotimes
syn keyword tl_keyword contained downcase-str drop drop-until drop-while
syn keyword tl_keyword contained dump-deferred-warnings dup dupfd dwim
syn keyword tl_keyword contained each each* echo echoctl
syn keyword tl_keyword contained echoe echok echoke echonl
syn keyword tl_keyword contained echoprt eighth empty endgrent
syn keyword tl_keyword contained endp endpwent ends-with ensure-dir
syn keyword tl_keyword contained env env-fbind env-hash env-vbind
syn keyword tl_keyword contained eq eql equal equot
syn keyword tl_keyword contained errno error eval evenp
syn keyword tl_keyword contained exception-subtype-map exception-subtype-p exec exit
syn keyword tl_keyword contained exit* exp expand-left expand-right
syn keyword tl_keyword contained expt exptmod extproc f$
syn keyword tl_keyword contained f^ f^$ false fboundp
syn keyword tl_keyword contained ff0 ff1 ffdly fifth
syn keyword tl_keyword contained file-append file-append-lines file-append-string file-get
syn keyword tl_keyword contained file-get-lines file-get-string file-put file-put-lines
syn keyword tl_keyword contained file-put-string fileno filter-equal filter-string-tree
syn keyword tl_keyword contained finalize find find-frame find-frames
syn keyword tl_keyword contained find-if find-max find-min find-package
syn keyword tl_keyword contained find-struct-type first fixnum-max fixnum-min
syn keyword tl_keyword contained fixnump flatcar flatcar* flatten
syn keyword tl_keyword contained flatten* flet flip flipargs
syn keyword tl_keyword contained flo-dig flo-epsilon flo-int flo-max
syn keyword tl_keyword contained flo-max-dig flo-min flo-str floatp
syn keyword tl_keyword contained floor floor-rem flush-stream flusho
syn keyword tl_keyword contained fmakunbound fmt fnm-casefold fnm-leading-dir
syn keyword tl_keyword contained fnm-noescape fnm-pathname fnm-period fnmatch
syn keyword tl_keyword contained for for* force fork
syn keyword tl_keyword contained format fourth fr$ fr^
syn keyword tl_keyword contained fr^$ from frr fstat
syn keyword tl_keyword contained ftw ftw-actionretval ftw-chdir ftw-continue
syn keyword tl_keyword contained ftw-d ftw-depth ftw-dnr ftw-dp
syn keyword tl_keyword contained ftw-f ftw-mount ftw-ns ftw-phys
syn keyword tl_keyword contained ftw-skip-siblings ftw-skip-subtree ftw-sl ftw-sln
syn keyword tl_keyword contained ftw-stop fun func-get-env func-get-form
syn keyword tl_keyword contained func-get-name func-set-env functionp gcd
syn keyword tl_keyword contained gen generate gensym gequal
syn keyword tl_keyword contained get-byte get-char get-clobber-expander get-delete-expander
syn keyword tl_keyword contained get-error get-error-str get-frames get-hash-userdata
syn keyword tl_keyword contained get-indent get-indent-mode get-line get-lines
syn keyword tl_keyword contained get-list-from-stream get-sig-handler get-string get-string-from-stream
syn keyword tl_keyword contained get-update-expander getaddrinfo getegid getenv
syn keyword tl_keyword contained geteuid getgid getgrent getgrgid
syn keyword tl_keyword contained getgrnam getgroups gethash getitimer
syn keyword tl_keyword contained getopts getpid getppid getpwent
syn keyword tl_keyword contained getpwnam getpwuid getresgid getresuid
syn keyword tl_keyword contained getuid ginterate giterate glob
syn keyword tl_keyword contained glob-altdirfunc glob-brace glob-err glob-mark
syn keyword tl_keyword contained glob-nocheck glob-noescape glob-nomagic glob-nosort
syn keyword tl_keyword contained glob-onlydir glob-period glob-tilde glob-tilde-check
syn keyword tl_keyword contained go greater group-by group-reduce
syn keyword tl_keyword contained gun handle handle* handler-bind
syn keyword tl_keyword contained hash hash-alist hash-begin hash-construct
syn keyword tl_keyword contained hash-count hash-diff hash-eql hash-equal
syn keyword tl_keyword contained hash-from-pairs hash-isec hash-keys hash-list
syn keyword tl_keyword contained hash-next hash-pairs hash-proper-subset hash-revget
syn keyword tl_keyword contained hash-subset hash-uni hash-update hash-update-1
syn keyword tl_keyword contained hash-userdata hash-values hashp have
syn keyword tl_keyword contained html-decode html-encode html-encode* hupcl
syn keyword tl_keyword contained iapply icanon icrnl identity
syn keyword tl_keyword contained ido iexten if ifa
syn keyword tl_keyword contained iff iffi iflet ignbrk
syn keyword tl_keyword contained igncr ignerr ignpar ignwarn
syn keyword tl_keyword contained imaxbel improper-plist-to-alist in in-package
syn keyword tl_keyword contained in6addr-any in6addr-loopback inaddr-any inaddr-loopback
syn keyword tl_keyword contained inc inc-indent indent-code indent-data
syn keyword tl_keyword contained indent-off inhash inlcr inpck
syn keyword tl_keyword contained int-chr int-flo int-str integerp
syn keyword tl_keyword contained intern interp-fun-p interpose invoke-catch
syn keyword tl_keyword contained ip ipf iread isig
syn keyword tl_keyword contained isqrt istrip itimer-prov itimer-real
syn keyword tl_keyword contained itimer-virtual iuclc iutf8 ixany
syn keyword tl_keyword contained ixoff ixon juxt keep-if
syn keyword tl_keyword contained keep-if* keepq keepql keepqual
syn keyword tl_keyword contained keyword-package keywordp kill labels
syn keyword tl_keyword contained lambda last lazy-str lazy-str-force
syn keyword tl_keyword contained lazy-str-force-upto lazy-str-get-trailing-list lazy-stream-cons lazy-stringp
syn keyword tl_keyword contained lcm lcons lcons-fun lconsp
syn keyword tl_keyword contained ldiff length length-list length-str
syn keyword tl_keyword contained length-str-< length-str-<= length-str-> length-str->=
syn keyword tl_keyword contained length-vec lequal less let
syn keyword tl_keyword contained let* lexical-fun-p lexical-lisp1-binding lexical-var-p
syn keyword tl_keyword contained lib-version link lisp-parse list
syn keyword tl_keyword contained list* list-str list-vec list-vector
syn keyword tl_keyword contained listp lnew load log
syn keyword tl_keyword contained log-alert log-auth log-authpriv log-cons
syn keyword tl_keyword contained log-crit log-daemon log-debug log-emerg
syn keyword tl_keyword contained log-err log-info log-ndelay log-notice
syn keyword tl_keyword contained log-nowait log-odelay log-perror log-pid
syn keyword tl_keyword contained log-user log-warning log10 log2
syn keyword tl_keyword contained logand logior lognot logtest
syn keyword tl_keyword contained logtrunc logxor lset lstat
syn keyword tl_keyword contained m$ m^ m^$ mac-param-bind
syn keyword tl_keyword contained macro-ancestor macro-form-p macro-time macroexpand
syn keyword tl_keyword contained macroexpand-1 macrolet major make-catenated-stream
syn keyword tl_keyword contained make-env make-hash make-lazy-cons make-lazy-struct
syn keyword tl_keyword contained make-like make-package make-random-state make-similar-hash
syn keyword tl_keyword contained make-string-byte-input-stream make-string-input-stream make-string-output-stream make-strlist-input-stream
syn keyword tl_keyword contained make-strlist-output-stream make-struct make-struct-type make-sym
syn keyword tl_keyword contained make-time make-time-utc make-trie makedev
syn keyword tl_keyword contained makunbound mapcar mapcar* mapdo
syn keyword tl_keyword contained mapf maphash mappend mappend*
syn keyword tl_keyword contained mask match-fun match-regex match-regex-right
syn keyword tl_keyword contained match-regst match-regst-right match-str match-str-tree
syn keyword tl_keyword contained max mboundp member member-if
syn keyword tl_keyword contained memp memq memql memqual
syn keyword tl_keyword contained merge meth method min
syn keyword tl_keyword contained minor minusp mismatch mkdir
syn keyword tl_keyword contained mknod mkstring mlet mmakunbound
syn keyword tl_keyword contained mod multi multi-sort n-choose-k
syn keyword tl_keyword contained n-perm-k nconc neq neql
syn keyword tl_keyword contained nequal new nexpand-left nil
syn keyword tl_keyword contained nilf ninth nl0 nl1
syn keyword tl_keyword contained nldly noflsh none not
syn keyword tl_keyword contained notf nreconc nreverse nthcdr
syn keyword tl_keyword contained nthlast null nullify num-chr
syn keyword tl_keyword contained num-str numberp oand obtain
syn keyword tl_keyword contained obtain* obtain*-block obtain-block ocrnl
syn keyword tl_keyword contained oddp ofdel ofill olcuc
syn keyword tl_keyword contained onlcr onlret onocr op
syn keyword tl_keyword contained open-command open-directory open-file open-fileno
syn keyword tl_keyword contained open-files open-files* open-pipe open-process
syn keyword tl_keyword contained open-socket open-socket-pair open-tail openlog
syn keyword tl_keyword contained opip opost opt opthelp
syn keyword tl_keyword contained or orf package-alist package-fallback-list
syn keyword tl_keyword contained package-foreign-symbols package-local-symbols package-name package-symbols
syn keyword tl_keyword contained packagep pad parenb parmrk
syn keyword tl_keyword contained parodd partition partition* partition-by
syn keyword tl_keyword contained path-blkdev-p path-chrdev-p path-dir-p path-executable-to-me-p
syn keyword tl_keyword contained path-exists-p path-file-p path-mine-p path-my-group-p
syn keyword tl_keyword contained path-newer path-older path-pipe-p path-private-to-me-p
syn keyword tl_keyword contained path-read-writable-to-me-p path-readable-to-me-p path-same-object path-setgid-p
syn keyword tl_keyword contained path-setuid-p path-sock-p path-sticky-p path-strictly-private-to-me-p
syn keyword tl_keyword contained path-symlink-p path-writable-to-me-p pdec pendin
syn keyword tl_keyword contained perm pinc pipe place-form-p
syn keyword tl_keyword contained placelet placelet* plist-to-alist plusp
syn keyword tl_keyword contained poll poll-err poll-in poll-nval
syn keyword tl_keyword contained poll-out poll-pri poll-rdband poll-rdhup
syn keyword tl_keyword contained poll-wrband pop pos pos-if
syn keyword tl_keyword contained pos-max pos-min posq posql
syn keyword tl_keyword contained posqual pppred ppred pprinl
syn keyword tl_keyword contained pprint pprof pred prinl
syn keyword tl_keyword contained print prof prog prog*
syn keyword tl_keyword contained prog1 progn promisep prop
syn keyword tl_keyword contained proper-list-p proper-listp pset pure-rel-path-p
syn keyword tl_keyword contained purge-deferred-warning push pushhash pushnew
syn keyword tl_keyword contained put-byte put-char put-line put-lines
syn keyword tl_keyword contained put-string put-strings pwd qquote
syn keyword tl_keyword contained qref quote r$ r^
syn keyword tl_keyword contained r^$ raise rand random
syn keyword tl_keyword contained random-fixnum random-state-get-vec random-state-p range
syn keyword tl_keyword contained range* range-regex rangep rassoc
syn keyword tl_keyword contained rassql rcomb rcons read
syn keyword tl_keyword contained read-until-match readlink real-time-stream-p record-adapter
syn keyword tl_keyword contained reduce-left reduce-right ref refset
syn keyword tl_keyword contained regex-compile regex-from-trie regex-parse regex-source
syn keyword tl_keyword contained regexp register-exception-subtypes register-tentative-def regsub
syn keyword tl_keyword contained rehome-sym release-deferred-warnings remhash remove-if
syn keyword tl_keyword contained remove-if* remove-path remq remq*
syn keyword tl_keyword contained remql remql* remqual remqual*
syn keyword tl_keyword contained rename-path repeat replace replace-list
syn keyword tl_keyword contained replace-str replace-struct replace-vec reset-struct
syn keyword tl_keyword contained rest ret retf return
syn keyword tl_keyword contained return* return-from revappend reverse
syn keyword tl_keyword contained rfind rfind-if rlcp rlcp-tree
syn keyword tl_keyword contained rlet rmember rmember-if rmemq
syn keyword tl_keyword contained rmemql rmemqual rmismatch rotate
syn keyword tl_keyword contained round round-rem rperm rplaca
syn keyword tl_keyword contained rplacd rpos rpos-if rposq
syn keyword tl_keyword contained rposql rposqual rr rra
syn keyword tl_keyword contained rsearch rslot run s-ifblk
syn keyword tl_keyword contained s-ifchr s-ifdir s-ififo s-iflnk
syn keyword tl_keyword contained s-ifmt s-ifreg s-ifsock s-irgrp
syn keyword tl_keyword contained s-iroth s-irusr s-irwxg s-irwxo
syn keyword tl_keyword contained s-irwxu s-isgid s-isuid s-isvtx
syn keyword tl_keyword contained s-iwgrp s-iwoth s-iwusr s-ixgrp
syn keyword tl_keyword contained s-ixoth s-ixusr search search-regex
syn keyword tl_keyword contained search-regst search-str search-str-tree second
syn keyword tl_keyword contained seek-stream select self-load-path self-path
syn keyword tl_keyword contained seqp set set-diff set-hash-userdata
syn keyword tl_keyword contained set-indent set-indent-mode set-package-fallback-list set-sig-handler
syn keyword tl_keyword contained setegid setenv seteuid setgid
syn keyword tl_keyword contained setgrent setgroups sethash setitimer
syn keyword tl_keyword contained setlogmask setpwent setresgid setresuid
syn keyword tl_keyword contained setuid seventh sh shift
syn keyword tl_keyword contained shuffle shut-rd shut-rdwr shut-wr
syn keyword tl_keyword contained sig-abrt sig-alrm sig-bus sig-check
syn keyword tl_keyword contained sig-chld sig-cont sig-fpe sig-hup
syn keyword tl_keyword contained sig-ill sig-int sig-io sig-iot
syn keyword tl_keyword contained sig-kill sig-pipe sig-poll sig-prof
syn keyword tl_keyword contained sig-pwr sig-quit sig-segv sig-stkflt
syn keyword tl_keyword contained sig-stop sig-sys sig-term sig-trap
syn keyword tl_keyword contained sig-tstp sig-ttin sig-ttou sig-urg
syn keyword tl_keyword contained sig-usr1 sig-usr2 sig-vtalrm sig-winch
syn keyword tl_keyword contained sig-xcpu sig-xfsz sign-extend sin
syn keyword tl_keyword contained sixth size-vec slet slot
syn keyword tl_keyword contained slotp slots slotset sock-accept
syn keyword tl_keyword contained sock-bind sock-cloexec sock-connect sock-dgram
syn keyword tl_keyword contained sock-family sock-listen sock-nonblock sock-peer
syn keyword tl_keyword contained sock-recv-timeout sock-send-timeout sock-set-peer sock-shutdown
syn keyword tl_keyword contained sock-stream sock-type some sort
syn keyword tl_keyword contained sort-group source-loc source-loc-str span-str
syn keyword tl_keyword contained special-operator-p special-var-p splice split
syn keyword tl_keyword contained split* split-str split-str-set sqrt
syn keyword tl_keyword contained sssucc ssucc starts-with stat
syn keyword tl_keyword contained static-slot static-slot-ensure static-slot-home static-slot-p
syn keyword tl_keyword contained static-slot-set stdlib str-in6addr str-in6addr-net
syn keyword tl_keyword contained str-inaddr str-inaddr-net str< str<=
syn keyword tl_keyword contained str= str> str>= stream-get-prop
syn keyword tl_keyword contained stream-set-prop streamp string-extend string-lt
syn keyword tl_keyword contained stringp struct-from-args struct-from-plist struct-type
syn keyword tl_keyword contained struct-type-p structp sub sub-list
syn keyword tl_keyword contained sub-str sub-vec subtypep succ
syn keyword tl_keyword contained super super-method suspend swap
syn keyword tl_keyword contained symacrolet symbol-function symbol-macro symbol-name
syn keyword tl_keyword contained symbol-package symbol-value symbolp symlink
syn keyword tl_keyword contained sys:*pl-env* sys:*trace-hash* sys:*trace-level* sys:abscond*
syn keyword tl_keyword contained sys:abscond-from sys:apply sys:awk-code-move-check sys:awk-expander
syn keyword tl_keyword contained sys:awk-fun-let sys:awk-fun-shadowing-env sys:awk-mac-let sys:awk-redir
syn keyword tl_keyword contained sys:awk-test sys:bad-slot-syntax sys:bits sys:build-key-list
syn keyword tl_keyword contained sys:capture-cont sys:catch sys:circref sys:compat
syn keyword tl_keyword contained sys:conv sys:conv-expand sys:conv-let sys:ctx-form
syn keyword tl_keyword contained sys:ctx-name sys:defmeth sys:do-conv sys:do-path-test
syn keyword tl_keyword contained sys:dvbind sys:dwim-del sys:dwim-set sys:each-op
syn keyword tl_keyword contained sys:eval-err sys:expand sys:expand-handle sys:expand-params
syn keyword tl_keyword contained sys:expand-with-free-refs sys:expr sys:extract-keys sys:extract-keys-p
syn keyword tl_keyword contained sys:fbind sys:for-op sys:gc sys:gc-set-delta
syn keyword tl_keyword contained sys:get-fun-getter-setter sys:get-mb sys:get-vb sys:handle-bad-syntax
syn keyword tl_keyword contained sys:if-to-cond sys:in6addr-condensed-text sys:l1-setq sys:l1-val
syn keyword tl_keyword contained sys:lbind sys:lisp1-setq sys:lisp1-value sys:list-builder-flets
syn keyword tl_keyword contained sys:loc sys:make-struct-lit sys:make-struct-type sys:mark-special
syn keyword tl_keyword contained sys:name-str sys:obtain-impl sys:opt-dash sys:opt-err
syn keyword tl_keyword contained sys:path-access sys:path-examine sys:path-test sys:path-test-mode
syn keyword tl_keyword contained sys:pl-expand sys:placelet-1 sys:propagate-ancestor sys:prune-missing-inits
syn keyword tl_keyword contained sys:qquote sys:quasi sys:quasilist sys:r-s-let-expander
syn keyword tl_keyword contained sys:reg-expand-nongreedy sys:reg-optimize sys:register-simple-accessor sys:rplaca
syn keyword tl_keyword contained sys:rplacd sys:rslotset sys:set-hash-rec-limit sys:set-hash-str-limit
syn keyword tl_keyword contained sys:set-macro-ancestor sys:setq sys:setqf sys:splice
syn keyword tl_keyword contained sys:str-inaddr-net-impl sys:struct-lit sys:switch sys:sym-clobber-expander
syn keyword tl_keyword contained sys:sym-delete-expander sys:sym-update-expander sys:top-fb sys:top-mb
syn keyword tl_keyword contained sys:top-vb sys:trace sys:trace-canonicalize-name sys:trace-enter
syn keyword tl_keyword contained sys:trace-leave sys:trace-redefine-check sys:unquote sys:untrace
syn keyword tl_keyword contained sys:var sys:wdwrap sys:with-dyn-rebinds syslog
syn keyword tl_keyword contained system-package t tab0 tab1
syn keyword tl_keyword contained tab2 tab3 tabdly tagbody
syn keyword tl_keyword contained take take-until take-while tan
syn keyword tl_keyword contained tb tc tcdrain tcflow
syn keyword tl_keyword contained tcflush tcgetattr tciflush tcioff
syn keyword tl_keyword contained tcioflush tcion tcoflush tcooff
syn keyword tl_keyword contained tcoon tcsadrain tcsaflush tcsanow
syn keyword tl_keyword contained tcsendbreak tcsetattr tentative-def-exists tenth
syn keyword tl_keyword contained test-clear test-clear-dirty test-dec test-dirty
syn keyword tl_keyword contained test-inc test-set test-set-indent-mode tf
syn keyword tl_keyword contained third throw throwf time
syn keyword tl_keyword contained time-fields-local time-fields-utc time-parse time-string-local
syn keyword tl_keyword contained time-string-utc time-struct-local time-struct-utc time-usec
syn keyword tl_keyword contained to tofloat tofloatz toint
syn keyword tl_keyword contained tointz tok-str tok-where tostop
syn keyword tl_keyword contained tostring tostringp tprint trace
syn keyword tl_keyword contained transpose tree-bind tree-case tree-find
syn keyword tl_keyword contained trie-add trie-compress trie-lookup-begin trie-lookup-feed-char
syn keyword tl_keyword contained trie-value-at trim-str true trunc
syn keyword tl_keyword contained trunc-rem truncate-stream tuples txr-case
syn keyword tl_keyword contained txr-case-impl txr-if txr-path txr-sym
syn keyword tl_keyword contained txr-version txr-when typecase typeof
syn keyword tl_keyword contained typep umask umeth umethod
syn keyword tl_keyword contained uname unget-byte unget-char unintern
syn keyword tl_keyword contained uniq unique unless unquote
syn keyword tl_keyword contained unsetenv until until* untrace
syn keyword tl_keyword contained unuse-package unuse-sym unwind-protect upcase-str
syn keyword tl_keyword contained upd update uref url-decode
syn keyword tl_keyword contained url-encode use use-package use-sym
syn keyword tl_keyword contained user-package usl usleep uslot
syn keyword tl_keyword contained vdiscard vec vec-list vec-push
syn keyword tl_keyword contained vec-set-length vecref vector vector-list
syn keyword tl_keyword contained vectorp veof veol veol2
syn keyword tl_keyword contained verase vintr vkill vlnext
syn keyword tl_keyword contained vmin vquit vreprint vstart
syn keyword tl_keyword contained vstop vsusp vswtc vt0
syn keyword tl_keyword contained vt1 vtdly vtime vwerase
syn keyword tl_keyword contained w-continued w-coredump w-exitstatus w-ifcontinued
syn keyword tl_keyword contained w-ifexited w-ifsignaled w-ifstopped w-nohang
syn keyword tl_keyword contained w-stopsig w-termsig w-untraced wait
syn keyword tl_keyword contained weave when whena whenlet
syn keyword tl_keyword contained where while while* whilet
syn keyword tl_keyword contained width width-check window-map window-mappend
syn keyword tl_keyword contained with-clobber-expander with-delete-expander with-gensyms with-hash-iter
syn keyword tl_keyword contained with-in-string-byte-stream with-in-string-stream with-objects with-out-string-stream
syn keyword tl_keyword contained with-out-strlist-stream with-resources with-slots with-stream
syn keyword tl_keyword contained with-update-expander wrap wrap* xcase
syn keyword tl_keyword contained yield yield-from zap zerop
syn keyword tl_keyword contained zip

syn keyword txr_keyword contained accept all and assert
syn keyword txr_keyword contained bind block call cases
syn keyword txr_keyword contained cat catch choose chr
syn keyword txr_keyword contained close coll collect data
syn keyword txr_keyword contained defex deffilter define do
syn keyword txr_keyword contained elif else empty end
syn keyword txr_keyword contained eof eol fail filter
syn keyword txr_keyword contained finally first flatten forget
syn keyword txr_keyword contained freeform fuzz gather if
syn keyword txr_keyword contained include last line load
syn keyword txr_keyword contained local maybe merge mod
syn keyword txr_keyword contained modlast name next none
syn keyword txr_keyword contained or output rebind rep
syn keyword txr_keyword contained repeat require set single
syn keyword txr_keyword contained skip some text throw
syn keyword txr_keyword contained trailer try until var
syn match txr_error "\(@[ \t]*\)[*]\?[\t ]*."
syn match txr_atat "\(@[ \t]*\)@"
syn match txr_comment "\(@[ \t]*\)[#;].*"
syn match txr_contin "\(@[ \t]*\)\\$"
syn match txr_char "\(@[ \t]*\)\\."
syn match txr_error "\(@[ \t]*\)\\[xo]"
syn match txr_char "\(@[ \t]*\)\\x[0-9A-Fa-f]\+;\?"
syn match txr_char "\(@[ \t]*\)\\[0-7]\+;\?"
syn match txr_regdir "\(@[ \t]*\)/\(\\/\|[^/]\|\\\n\)*/"
syn match txr_hashbang "^#!.*"
syn match txr_nested_error "[^\t ]\+" contained
syn match txr_variable "\(@[ \t]*\)[*]\?[ \t]*[A-Za-z_][A-Za-z_0-9]*"
syn match txr_splicevar "@[ \t,*@]*[A-Za-z_][A-Za-z_0-9]*" contained
syn match txr_metanum "@\+[0-9]\+" contained
syn match txr_badesc "\\." contained
syn match txr_escat "\\@" contained
syn match txr_stresc "\\[abtnvfre\\ \n"`']" contained
syn match txr_numesc "\\x[0-9A-Fa-f]\+;\?" contained
syn match txr_numesc "\\[0-7]\+;\?" contained
syn match txr_regesc "\\[abtnvfre\\ \n/sSdDwW()\|.*?+~&%\[\]\-]" contained

syn match txr_chr "#\\x[0-9A-Fa-f]\+" contained
syn match txr_chr "#\\o[0-7]\+" contained
syn match txr_chr "#\\[^ \t\nA-Za-z_0-9]" contained
syn match txr_chr "#\\[A-Za-z_0-9]\+" contained
syn match txr_ncomment ";.*" contained

syn match txr_dot "\." contained
syn match txr_num "#x[+\-]\?[0-9A-Fa-f]\+" contained
syn match txr_num "#o[+\-]\?[0-7]\+" contained
syn match txr_num "#b[+\-]\?[01]\+" contained
syn match txr_ident "[A-Za-z_0-9!$%&*+\-<=>?\\_~]*[A-Za-z_!$%&*+\-<=>?\\_~^][A-Za-z_0-9!$%&*+\-<=>?\\_~^]*" contained
syn match tl_ident "[:@][A-Za-z_0-9!$%&*+\-<=>?\\_~^/]\+" contained
syn match txr_braced_ident "[:][A-Za-z_0-9!$%&*+\-<=>?\\_~^/]\+" contained
syn match tl_ident "[A-Za-z_0-9!$%&*+\-<=>?\\_~/]*[A-Za-z_!$%&*+\-<=>?\\_~^/#][A-Za-z_0-9!$%&*+\-<=>?\\_~^/#]*" contained
syn match txr_num "[+\-]\?[0-9]\+\([^A-Za-z_0-9!$%&*+\-<=>?\\_~^/#]\|\n\)"me=e-1 contained
syn match txr_badnum "[+\-]\?[0-9]*[.][0-9]\+\([eE][+\-]\?[0-9]\+\)\?[A-Za-z_!$%&*+\-<=>?\\_~^/#]\+" contained
syn match txr_num "[+\-]\?[0-9]*[.][0-9]\+\([eE][+\-]\?[0-9]\+\)\?\([^A-Za-z_0-9!$%&*+\-<=>?\\_~^/#]\|\n\)"me=e-1 contained
syn match txr_num "[+\-]\?[0-9]\+\([eE][+\-]\?[0-9]\+\)\([^A-Za-z_0-9!$%&*+\-<=>?\\_~^/#]\|\n\)"me=e-1 contained
syn match tl_ident ":" contained
syn match tl_splice "[ \t,]\|,[*]" contained

syn match txr_unquote "," contained
syn match txr_splice ",\*" contained
syn match txr_quote "'" contained
syn match txr_quote "\^" contained
syn match txr_dotdot "\.\." contained
syn match txr_metaat "@" contained
syn match txr_circ "#[0-9]\+[#=]"

syn region txr_bracevar matchgroup=Delimiter start="@[ \t]*[*]\?{" matchgroup=Delimiter end="}" contains=txr_num,tl_ident,tl_splice,tl_metanum,txr_metaat,txr_circ,txr_braced_ident,txr_dot,txr_dotdot,txr_string,txr_list,txr_bracket,txr_mlist,txr_mbracket,txr_regex,txr_quasilit,txr_chr,txr_nested_error
syn region txr_directive matchgroup=Delimiter start="@[ \t]*(" matchgroup=Delimiter end=")" contains=txr_keyword,txr_string,txr_list,txr_bracket,txr_mlist,txr_mbracket,txr_quasilit,txr_num,txr_badnum,tl_ident,tl_regex,txr_string,txr_chr,txr_quote,txr_unquote,txr_splice,txr_dot,txr_dotdot,txr_metaat,txr_circ,txr_ncomment,txr_nested_error
syn region txr_list contained matchgroup=Delimiter start="\(#[HSR]\?\)\?(" matchgroup=Delimiter end=")" contains=tl_keyword,txr_string,tl_regex,txr_num,txr_badnum,tl_ident,txr_metanum,txr_ign_par,txr_ign_bkt,txr_list,txr_bracket,txr_mlist,txr_mbracket,txr_quasilit,txr_chr,txr_quote,txr_unquote,txr_splice,txr_dot,txr_dotdot,txr_metaat,txr_circ,txr_ncomment,txr_nested_error
syn region txr_bracket contained matchgroup=Delimiter start="\[" matchgroup=Delimiter end="\]" contains=tl_keyword,txr_string,tl_regex,txr_num,txr_badnum,tl_ident,txr_metanum,txr_ign_par,txr_ign_bkt,txr_list,txr_bracket,txr_mlist,txr_mbracket,txr_quasilit,txr_chr,txr_quote,txr_unquote,txr_splice,txr_dot,txr_dotdot,txr_metaat,txr_circ,txr_ncomment,txr_nested_error
syn region txr_mlist contained matchgroup=Delimiter start="@[ \t^',]*(" matchgroup=Delimiter end=")" contains=tl_keyword,txr_string,tl_regex,txr_num,txr_badnum,tl_ident,txr_metanum,txr_ign_par,txr_ign_bkt,txr_list,txr_bracket,txr_mlist,txr_mbracket,txr_quasilit,txr_chr,txr_quote,txr_unquote,txr_splice,txr_dot,txr_dotdot,txr_metaat,txr_circ,txr_ncomment,txr_nested_error
syn region txr_mbracket matchgroup=Delimiter start="@[ \t^',]*\[" matchgroup=Delimiter end="\]" contains=tl_keyword,txr_string,tl_regex,txr_num,txr_badnum,tl_ident,txr_metanum,txr_ign_par,txr_ign_bkt,txr_list,txr_bracket,txr_mlist,txr_mbracket,txr_quasilit,txr_chr,txr_quote,txr_unquote,txr_splice,txr_dot,txr_dotdot,txr_metaat,txr_circ,txr_ncomment,txr_nested_error
syn region txr_string contained start=+#\?\*\?"+ end=+["\n]+ contains=txr_stresc,txr_numesc,txr_badesc
syn region txr_quasilit contained start=+#\?\*\?`+ end=+[`\n]+ contains=txr_splicevar,txr_metanum,txr_bracevar,txr_mlist,txr_mbracket,txr_escat,txr_stresc,txr_numesc,txr_badesc
syn region txr_regex contained start="/" end="[/\n]" contains=txr_regesc,txr_numesc,txr_badesc
syn region tl_regex contained start="#/" end="[/\n]" contains=txr_regesc,txr_numesc,txr_badesc
syn region txr_ign_par contained matchgroup=Comment start="#;[ \t',]*\(#[HSR]\?\)\?(" matchgroup=Comment end=")" contains=txr_ign_par_interior,txr_ign_bkt_interior
syn region txr_ign_bkt contained matchgroup=Comment start="#;[ \t',]*\(#[HSR]\?\)\?\[" matchgroup=Comment end="\]" contains=txr_ign_par_interior,txr_ign_bkt_interior
syn region txr_ign_par_interior contained matchgroup=Comment start="(" matchgroup=Comment end=")" contains=txr_ign_par_interior,txr_ign_bkt_interior
syn region txr_ign_bkt_interior contained matchgroup=Comment start="\[" matchgroup=Comment end="\]" contains=txr_ign_par_interior,txr_ign_bkt_interior

hi def link txr_at Special
hi def link txr_atstar Special
hi def link txr_atat Special
hi def link txr_comment Comment
hi def link txr_ncomment Comment
hi def link txr_hashbang Preproc
hi def link txr_contin Preproc
hi def link txr_char String
hi def link txr_keyword Keyword
hi def link tl_keyword Type
hi def link txr_string String
hi def link txr_chr String
hi def link txr_quasilit String
hi def link txr_regex String
hi def link tl_regex String
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
hi def link tl_ident Identifier
hi def link txr_num Number
hi def link txr_badnum Error
hi def link txr_quote Special
hi def link txr_unquote Special
hi def link txr_splice Special
hi def link txr_dot Special
hi def link txr_dotdot Special
hi def link txr_metaat Special
hi def link txr_circ Special
hi def link txr_munqspl Special
hi def link tl_splice Special
hi def link txr_error Error
hi def link txr_nested_error Error
hi def link txr_ign_par Comment
hi def link txr_ign_bkt_interior Comment
hi def link txr_ign_par_interior Comment
hi def link txr_ign_bkt Comment

let b:current_syntax = "lisp"

set lispwords=ado,alet,ap,append-each,append-each*,aret,awk,block,block*,build,caseq,caseq*,caseql,caseql*,casequal,casequal*,catch,catch*,collect-each,collect-each*,compare-swap,cond,conda,condlet,dec,defex,define-accessor,define-modify-macro,define-param-expander,define-place-macro,defmacro,defmeth,defpackage,defparm,defparml,defplace,defstruct,defsymacro,defun,defvar,defvarl,del,delay,do,dohash,dotimes,each,each*,equot,flet,flip,for,for*,fun,gen,go,gun,handle,handle*,handler-bind,ido,if,ifa,iflet,ignerr,ignwarn,in-package,ip,labels,lambda,lcons,let,let*,lset,mac-param-bind,macro-time,macrolet,mlet,obtain,obtain*,obtain*-block,obtain-block,op,pdec,pinc,placelet,placelet*,pop,pprof,prof,prog,prog*,prog1,progn,push,pushnew,ret,return,return-from,rlet,rslot,slet,splice,suspend,symacrolet,sys:abscond-from,sys:awk-fun-let,sys:awk-mac-let,sys:awk-redir,sys:catch,sys:conv,sys:dvbind,sys:each-op,sys:expr,sys:fbind,sys:for-op,sys:l1-val,sys:lbind,sys:lisp1-value,sys:path-examine,sys:path-test,sys:placelet-1,sys:splice,sys:struct-lit,sys:switch,sys:unquote,sys:var,sys:with-dyn-rebinds,tagbody,tb,tc,test-clear,test-dec,test-inc,test-set,trace,tree-bind,tree-case,txr-case,txr-case-impl,txr-if,txr-when,typecase,unless,unquote,until,until*,untrace,unwind-protect,upd,uref,when,whena,whenlet,while,while*,whilet,with-clobber-expander,with-delete-expander,with-gensyms,with-hash-iter,with-in-string-byte-stream,with-in-string-stream,with-objects,with-out-string-stream,with-out-strlist-stream,with-resources,with-slots,with-stream,with-update-expander,yield,yield-from,zap,:method,:function,:init,:postinit,:fini
