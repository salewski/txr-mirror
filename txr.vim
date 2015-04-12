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

setlocal iskeyword=a-z,A-Z,48-57,!,$,&,*,+,-,<,=,>,?,\\,_,~,/

syn keyword txl_keyword contained * *args* *e* *flo-dig*
syn keyword txl_keyword contained *flo-epsilon* *flo-max* *flo-min* *full-args*
syn keyword txl_keyword contained *gensym-counter* *keyword-package* *pi* *random-state*
syn keyword txl_keyword contained *self-path* *stddebug* *stderr* *stdin*
syn keyword txl_keyword contained *stdlog* *stdnull* *stdout* *txr-version*
syn keyword txl_keyword contained *unhandled-hook* *user-package* + -
syn keyword txl_keyword contained / /= < <=
syn keyword txl_keyword contained = > >= abort
syn keyword txl_keyword contained abs abs-path-p acons acons-new
syn keyword txl_keyword contained aconsql-new acos ado alist-nremove
syn keyword txl_keyword contained alist-remove all and andf
syn keyword txl_keyword contained ap apf append append*
syn keyword txl_keyword contained append-each append-each* apply aret
syn keyword txl_keyword contained ash asin assoc assql
syn keyword txl_keyword contained atan atan2 atom bignump
syn keyword txl_keyword contained bit block boundp break-str
syn keyword txl_keyword contained call callf car caseq
syn keyword txl_keyword contained caseql casequal cat-str cat-streams
syn keyword txl_keyword contained cat-vec catch cdr ceil
syn keyword txl_keyword contained chain chand chdir chr-isalnum
syn keyword txl_keyword contained chr-isalpha chr-isascii chr-isblank chr-iscntrl
syn keyword txl_keyword contained chr-isdigit chr-isgraph chr-islower chr-isprint
syn keyword txl_keyword contained chr-ispunct chr-isspace chr-isunisp chr-isupper
syn keyword txl_keyword contained chr-isxdigit chr-num chr-str chr-str-set
syn keyword txl_keyword contained chr-tolower chr-toupper chrp clear-error
syn keyword txl_keyword contained close-stream closelog cmp-str collect-each
syn keyword txl_keyword contained collect-each* comb compl-span-str cond
syn keyword txl_keyword contained cons conses conses* consp
syn keyword txl_keyword contained constantp copy copy-alist copy-cons
syn keyword txl_keyword contained copy-hash copy-list copy-str copy-vec
syn keyword txl_keyword contained cos count-if countq countql
syn keyword txl_keyword contained countqual cum-norm-dist daemon dec
syn keyword txl_keyword contained defmacro defsymacro defun defvar
syn keyword txl_keyword contained del delay delete-package do
syn keyword txl_keyword contained dohash downcase-str dup dupfd
syn keyword txl_keyword contained dwim each each* empty
syn keyword txl_keyword contained ensure-dir env env-fbind env-hash
syn keyword txl_keyword contained env-vbind eq eql equal
syn keyword txl_keyword contained errno error eval evenp
syn keyword txl_keyword contained exit exp expt exptmod
syn keyword txl_keyword contained false fbind fboundp fifth
syn keyword txl_keyword contained fileno filter-equal filter-string-tree finalize
syn keyword txl_keyword contained find find-if find-max find-min
syn keyword txl_keyword contained find-package first fixnump flatten
syn keyword txl_keyword contained flatten* flet flip flo-int
syn keyword txl_keyword contained flo-str floatp floor flush-stream
syn keyword txl_keyword contained for for* force fork
syn keyword txl_keyword contained format fourth fun func-get-env
syn keyword txl_keyword contained func-get-form func-set-env functionp gcd
syn keyword txl_keyword contained gen generate gensym gequal
syn keyword txl_keyword contained get-byte get-char get-error get-error-str
syn keyword txl_keyword contained get-hash-userdata get-line get-lines get-list-from-stream
syn keyword txl_keyword contained get-sig-handler get-string get-string-from-stream gethash
syn keyword txl_keyword contained getitimer getpid getppid giterate
syn keyword txl_keyword contained glob glob-altdirfunc glob-brace glob-err
syn keyword txl_keyword contained glob-mark glob-nocheck glob-noescape glob-nomagic
syn keyword txl_keyword contained glob-nosort glob-onlydir glob-period glob-tilde
syn keyword txl_keyword contained glob-tilde-check greater group-by gun
syn keyword txl_keyword contained hash hash-alist hash-construct hash-count
syn keyword txl_keyword contained hash-diff hash-eql hash-equal hash-isec
syn keyword txl_keyword contained hash-keys hash-pairs hash-uni hash-update
syn keyword txl_keyword contained hash-update-1 hash-values hashp html-decode
syn keyword txl_keyword contained html-encode iapply identity ido
syn keyword txl_keyword contained if iff iffi iflet
syn keyword txl_keyword contained ignerr in inc inhash
syn keyword txl_keyword contained int-flo int-str integerp intern
syn keyword txl_keyword contained interp-fun-p interpose ip ipf
syn keyword txl_keyword contained isqrt itimer-prov itimer-real itimer-virtual
syn keyword txl_keyword contained juxt keep-if keep-if* keywordp
syn keyword txl_keyword contained kill labels lambda last
syn keyword txl_keyword contained lazy-str lazy-str-force lazy-str-force-upto lazy-str-get-trailing-list
syn keyword txl_keyword contained lazy-stream-cons lazy-stringp lbind lcm
syn keyword txl_keyword contained lcons-fun lconsp ldiff length
syn keyword txl_keyword contained length-list length-str length-str-< length-str-<=
syn keyword txl_keyword contained length-str-> length-str->= length-vec lequal
syn keyword txl_keyword contained less let let* lexical-fun-p
syn keyword txl_keyword contained lexical-lisp1-binding lexical-var-p link lisp-parse
syn keyword txl_keyword contained list list* list-str list-vector
syn keyword txl_keyword contained listp log log-alert log-auth
syn keyword txl_keyword contained log-authpriv log-cons log-crit log-daemon
syn keyword txl_keyword contained log-debug log-emerg log-err log-info
syn keyword txl_keyword contained log-ndelay log-notice log-nowait log-odelay
syn keyword txl_keyword contained log-perror log-pid log-user log-warning
syn keyword txl_keyword contained log10 log2 logand logior
syn keyword txl_keyword contained lognot logtest logtrunc logxor
syn keyword txl_keyword contained macro-form-p macro-time macroexpand macroexpand-1
syn keyword txl_keyword contained macrolet major make-catenated-stream make-env
syn keyword txl_keyword contained make-hash make-lazy-cons make-like make-package
syn keyword txl_keyword contained make-random-state make-similar-hash make-string-byte-input-stream make-string-input-stream
syn keyword txl_keyword contained make-string-output-stream make-strlist-output-stream make-sym make-time
syn keyword txl_keyword contained make-time-utc make-trie makedev mapcar
syn keyword txl_keyword contained mapcar* mapdo mapf maphash
syn keyword txl_keyword contained mappend mappend* mask match-fun
syn keyword txl_keyword contained match-regex match-regex-right match-regst match-regst-right
syn keyword txl_keyword contained match-str match-str-tree max member
syn keyword txl_keyword contained member-if memq memql memqual
syn keyword txl_keyword contained merge min minor minusp
syn keyword txl_keyword contained mkdir mknod mkstring mod
syn keyword txl_keyword contained multi multi-sort n-choose-k n-perm-k
syn keyword txl_keyword contained nconc nilf none not
syn keyword txl_keyword contained notf nreverse null nullify
syn keyword txl_keyword contained num-chr num-str numberp oand
syn keyword txl_keyword contained oddp op open-command open-directory
syn keyword txl_keyword contained open-file open-fileno open-files open-files*
syn keyword txl_keyword contained open-pipe open-process open-tail openlog
syn keyword txl_keyword contained opip or orf packagep
syn keyword txl_keyword contained pad partition partition* partition-by
syn keyword txl_keyword contained perm plusp pop pos
syn keyword txl_keyword contained pos-if pos-max pos-min posq
syn keyword txl_keyword contained posql posqual pppred ppred
syn keyword txl_keyword contained pprinl pprint pprof pred
syn keyword txl_keyword contained prinl print prof prog1
syn keyword txl_keyword contained progn prop proper-listp push
syn keyword txl_keyword contained pushhash put-byte put-char put-line
syn keyword txl_keyword contained put-lines put-string put-strings pwd
syn keyword txl_keyword contained qquote quasi quasilist quote
syn keyword txl_keyword contained rand random random-fixnum random-state-p
syn keyword txl_keyword contained range range* range-regex rcomb
syn keyword txl_keyword contained read readlink real-time-stream-p reduce-left
syn keyword txl_keyword contained reduce-right ref refset regex-compile
syn keyword txl_keyword contained regex-parse regexp regsub rehome-sym
syn keyword txl_keyword contained remhash remove-if remove-if* remove-path
syn keyword txl_keyword contained remq remq* remql remql*
syn keyword txl_keyword contained remqual remqual* rename-path repeat
syn keyword txl_keyword contained replace replace-list replace-str replace-vec
syn keyword txl_keyword contained rest ret retf return
syn keyword txl_keyword contained return-from reverse rlcp rperm
syn keyword txl_keyword contained rplaca rplacd run s-ifblk
syn keyword txl_keyword contained s-ifchr s-ifdir s-ififo s-iflnk
syn keyword txl_keyword contained s-ifmt s-ifreg s-ifsock s-irgrp
syn keyword txl_keyword contained s-iroth s-irusr s-irwxg s-irwxo
syn keyword txl_keyword contained s-irwxu s-isgid s-isuid s-isvtx
syn keyword txl_keyword contained s-iwgrp s-iwoth s-iwusr s-ixgrp
syn keyword txl_keyword contained s-ixoth s-ixusr search search-regex
syn keyword txl_keyword contained search-regst search-str search-str-tree second
syn keyword txl_keyword contained seek-stream select seqp set
syn keyword txl_keyword contained set-diff set-hash-userdata set-sig-handler sethash
syn keyword txl_keyword contained setitimer setlogmask sh sig-abrt
syn keyword txl_keyword contained sig-alrm sig-bus sig-check sig-chld
syn keyword txl_keyword contained sig-cont sig-fpe sig-hup sig-ill
syn keyword txl_keyword contained sig-int sig-io sig-iot sig-kill
syn keyword txl_keyword contained sig-lost sig-pipe sig-poll sig-prof
syn keyword txl_keyword contained sig-pwr sig-quit sig-segv sig-stkflt
syn keyword txl_keyword contained sig-stop sig-sys sig-term sig-trap
syn keyword txl_keyword contained sig-tstp sig-ttin sig-ttou sig-urg
syn keyword txl_keyword contained sig-usr1 sig-usr2 sig-vtalrm sig-winch
syn keyword txl_keyword contained sig-xcpu sig-xfsz sign-extend sin
syn keyword txl_keyword contained sixth size-vec some sort
syn keyword txl_keyword contained sort-group source-loc source-loc-str span-str
syn keyword txl_keyword contained splice split-str split-str-set sqrt
syn keyword txl_keyword contained sssucc ssucc stat stdlib
syn keyword txl_keyword contained str< str<= str= str>
syn keyword txl_keyword contained str>= stream-get-prop stream-set-prop streamp
syn keyword txl_keyword contained string-extend string-lt stringp sub
syn keyword txl_keyword contained sub-list sub-str sub-vec succ
syn keyword txl_keyword contained symacrolet symbol-function symbol-name symbol-package
syn keyword txl_keyword contained symbol-value symbolp symlink sys-qquote
syn keyword txl_keyword contained sys-splice sys-unquote syslog tan
syn keyword txl_keyword contained tb tc tf third
syn keyword txl_keyword contained throw throwf time time-fields-local
syn keyword txl_keyword contained time-fields-utc time-string-local time-string-utc time-usec
syn keyword txl_keyword contained tofloat toint tok-str tok-where
syn keyword txl_keyword contained tostring tostringp tprint transpose
syn keyword txl_keyword contained tree-bind tree-case tree-find trie-add
syn keyword txl_keyword contained trie-compress trie-lookup-begin trie-lookup-feed-char trie-value-at
syn keyword txl_keyword contained trim-str true trunc trunc-rem
syn keyword txl_keyword contained tuples txr-case txr-if txr-when
syn keyword txl_keyword contained typeof unget-byte unget-char uniq
syn keyword txl_keyword contained unique unless unquote until
syn keyword txl_keyword contained until* upcase-str update url-decode
syn keyword txl_keyword contained url-encode usleep uw-protect vec
syn keyword txl_keyword contained vec-push vec-set-length vecref vector
syn keyword txl_keyword contained vector-list vectorp w-continued w-coredump
syn keyword txl_keyword contained w-exitstatus w-ifcontinued w-ifexited w-ifsignaled
syn keyword txl_keyword contained w-ifstopped w-nohang w-stopsig w-termsig
syn keyword txl_keyword contained w-untraced wait weave when
syn keyword txl_keyword contained whenlet where while while*
syn keyword txl_keyword contained whilet width with-saved-vars wrap
syn keyword txl_keyword contained wrap* zap zerop zip

syn keyword txr_keyword contained accept all and assert
syn keyword txr_keyword contained bind block cases cat
syn keyword txr_keyword contained catch choose chr close
syn keyword txr_keyword contained coll collect defex deffilter
syn keyword txr_keyword contained define do elif else
syn keyword txr_keyword contained end eof eol fail
syn keyword txr_keyword contained filter finally flatten forget
syn keyword txr_keyword contained freeform fuzz gather if
syn keyword txr_keyword contained include last line load
syn keyword txr_keyword contained local maybe merge next
syn keyword txr_keyword contained none or output rebind
syn keyword txr_keyword contained rep repeat require set
syn keyword txr_keyword contained skip some text throw
syn keyword txr_keyword contained trailer try until var
syn match txr_error "@[\t ]*[*]\?[\t ]*."
syn match txr_atat "@[ \t]*@"
syn match txr_comment "@[ \t]*[#;].*"
syn match txr_contin "@[ \t]*\\$"
syn match txr_char "@[ \t]*\\."
syn match txr_char "@[ \t]*\\x[0-9A-Fa-f]\+"
syn match txr_char "@[ \t]*\\[0-9]\+"
syn match txr_variable "@[ \t]*[*]\?[ \t]*[A-Za-z_][A-Za-z0-9_]*"
syn match txr_splicevar "@[ \t,*]*[A-Za-z_][A-Za-z0-9_]*"
syn match txr_regdir "@[ \t]*/\(\\/\|[^/]\|\\\n\)*/"
syn match txr_hashbang "^#!.*"
syn match txr_metanum "@[0-9]\+"
syn match txr_nested_error "[^\t `]\+" contained

syn match txr_chr "#\\x[A-Fa-f0-9]\+" contained
syn match txr_chr "#\\o[0-9]\+" contained
syn match txr_chr "#\\[^ \t\nA-Za-z0-9_]" contained
syn match txr_chr "#\\[A-Za-z0-9_]\+" contained
syn match txr_ncomment ";.*" contained

syn match txr_dot "\." contained
syn match txr_num "#x[+\-]\?[0-9A-Fa-f]\+" contained
syn match txr_num "#o[+\-]\?[0-7]\+" contained
syn match txr_num "#b[+\-]\?[0-1]\+" contained
syn match txr_ident "[A-Za-z0-9!$%&*+\-<=>?\\_~]*[A-Za-z!$#%&*+\-<=>?\\^_~][A-Za-z0-9!$#%&*+\-<=>?\\^_~]*" contained
syn match txl_ident "[:@][A-Za-z0-9!$%&*+\-<=>?\\\^_~/]\+" contained
syn match txr_braced_ident "[:][A-Za-z0-9!$%&*+\-<=>?\\\^_~/]\+" contained
syn match txl_ident "[A-Za-z0-9!$%&*+\-<=>?\\_~/]*[A-Za-z!$#%&*+\-<=>?\\^_~/][A-Za-z0-9!$#%&*+\-<=>?\\^_~/]*" contained
syn match txr_num "[+\-]\?[0-9]\+\([^A-Za-z0-9!$#%&*+\-<=>?\\^_~/]\|\n\)"me=e-1 contained
syn match txr_badnum "[+\-]\?[0-9]*[.][0-9]\+\([eE][+\-]\?[0-9]\+\)\?[A-Za-z!$#%&*+\-<=>?\\^_~/]\+" contained
syn match txr_num "[+\-]\?[0-9]*[.][0-9]\+\([eE][+\-]\?[0-9]\+\)\?\([^A-Za-z0-9!$#%&*+\-<=>?\\^_~/]\|\n\)"me=e-1 contained
syn match txr_num "[+\-]\?[0-9]\+\([eE][+\-]\?[0-9]\+\)\([^A-Za-z0-9!$#%&*+\-<=>?\\^_~/]\|\n\)"me=e-1 contained
syn match txl_ident ":" contained
syn match txl_splice "[ \t,]\|,[*]" contained

syn match txr_unquote "," contained
syn match txr_splice ",\*" contained
syn match txr_quote "'" contained
syn match txr_quote "\^" contained
syn match txr_dotdot "\.\." contained
syn match txr_metaat "@" contained

syn region txr_bracevar matchgroup=Delimiter start="@[ \t]*[*]\?{" matchgroup=Delimiter end="}" contains=txr_num,txr_ident,txr_braced_ident,txr_string,txr_list,txr_bracket,txr_mlist,txr_mbracket,txr_regex,txr_quasilit,txr_chr,txl_splice,txr_nested_error
syn region txr_directive matchgroup=Delimiter start="@[ \t]*(" matchgroup=Delimiter end=")" contains=txr_keyword,txr_string,txr_list,txr_bracket,txr_mlist,txr_mbracket,txr_quasilit,txr_num,txr_badnum,txl_ident,txl_regex,txr_string,txr_chr,txr_quote,txr_unquote,txr_splice,txr_dot,txr_dotdot,txr_metaat,txr_ncomment,txr_nested_error
syn region txr_list contained matchgroup=Delimiter start="#\?H\?(" matchgroup=Delimiter end=")" contains=txl_keyword,txr_string,txl_regex,txr_num,txr_badnum,txl_ident,txr_metanum,txr_list,txr_bracket,txr_mlist,txr_mbracket,txr_quasilit,txr_chr,txr_quote,txr_unquote,txr_splice,txr_dot,txr_dotdot,txr_metaat,txr_ncomment,txr_nested_error
syn region txr_bracket contained matchgroup=Delimiter start="\[" matchgroup=Delimiter end="\]" contains=txl_keyword,txr_string,txl_regex,txr_num,txr_badnum,txl_ident,txr_metanum,txr_list,txr_bracket,txr_mlist,txr_mbracket,txr_quasilit,txr_chr,txr_quote,txr_unquote,txr_splice,txr_dot,txr_dotdot,txr_metaat,txr_ncomment,txr_nested_error
syn region txr_mlist contained matchgroup=Delimiter start="@[ \t]*(" matchgroup=Delimiter end=")" contains=txl_keyword,txr_string,txl_regex,txr_num,txr_badnum,txl_ident,txr_metanum,txr_list,txr_bracket,txr_mlist,txr_mbracket,txr_quasilit,txr_chr,txr_quote,txr_unquote,txr_splice,txr_dot,txr_dotdot,txr_metaat,txr_ncomment,txr_nested_error
syn region txr_mbracket matchgroup=Delimiter start="@[ \t]*\[" matchgroup=Delimiter end="\]" contains=txl_keyword,txr_string,txl_regex,txr_num,txr_badnum,txl_ident,txr_metanum,txr_list,txr_bracket,txr_mlist,txr_mbracket,txr_quasilit,txr_chr,txr_quote,txr_unquote,txr_splice,txr_dot,txr_dotdot,txr_metaat,txr_ncomment,txr_nested_error
syn region txr_string contained start=+#\?\*\?"+ skip=+\\\\\|\\"\|\\\n+ end=+"\|\n+
syn region txr_quasilit contained start=+#\?\*\?`+ skip=+\\\\\|\\`\|\\\n+ end=+`\|\n+ contains=txr_splicevar,txr_metanum,txr_bracevar,txr_mlist,txr_mbracket
syn region txr_regex contained start="/" skip="\\\\\|\\/\|\\\n" end="/\|\n"
syn region txl_regex contained start="#/" skip="\\\\\|\\/\|\\\n" end="/\|\n"

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
