" VIM Syntax file for txr
" Kaz Kylheku <kaz@kylheku.com>

" INSTALL-HOWTO:
"
" 1. Create the directory .vim/syntax in your home directory and
"    put this file there.
" 2. In your .vimrc, add this command to associate *.txr files
"    with the txr filetype.
"    :au BufRead,BufNewFile *.txr set filetype=txr | set lisp
"
" If you want syntax highlighting to be on automatically (for any language)
" you need to add ":syntax on" in your .vimrc also. But you knew that already!
"
" This file is generated by the genvim.txr script in the TXR source tree.

syn case match
syn spell toplevel

setlocal iskeyword=a-z,A-Z,48-57,!,$,&,*,+,-,<,=,>,?,\\,_,~,/

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

syn keyword txl_keyword contained * *args* *e* *flo-dig*
syn keyword txl_keyword contained *flo-epsilon* *flo-max* *flo-min* *full-args*
syn keyword txl_keyword contained *gensym-counter* *keyword-package* *pi* *random-state*
syn keyword txl_keyword contained *self-path* *stddebug* *stderr* *stdin*
syn keyword txl_keyword contained *stdlog* *stdnull* *stdout* *txr-version*
syn keyword txl_keyword contained *user-package* + - /
syn keyword txl_keyword contained /= < <= =
syn keyword txl_keyword contained > >= abs abs-path-p
syn keyword txl_keyword contained acons acons-new aconsql-new acos
syn keyword txl_keyword contained ado alist-nremove alist-remove all
syn keyword txl_keyword contained and andf ap apf
syn keyword txl_keyword contained append append* append-each append-each*
syn keyword txl_keyword contained apply ash asin assoc
syn keyword txl_keyword contained assql atan atan2 atom
syn keyword txl_keyword contained bignump bit block boundp
syn keyword txl_keyword contained break-str call car caseq
syn keyword txl_keyword contained caseql casequal cat-str cat-vec
syn keyword txl_keyword contained catch cdr ceil chain
syn keyword txl_keyword contained chdir chr-isalnum chr-isalpha chr-isascii
syn keyword txl_keyword contained chr-isblank chr-iscntrl chr-isdigit chr-isgraph
syn keyword txl_keyword contained chr-islower chr-isprint chr-ispunct chr-isspace
syn keyword txl_keyword contained chr-isunisp chr-isupper chr-isxdigit chr-num
syn keyword txl_keyword contained chr-str chr-str-set chr-tolower chr-toupper
syn keyword txl_keyword contained chrp close-stream closelog cmp-str
syn keyword txl_keyword contained collect-each collect-each* comb compl-span-str
syn keyword txl_keyword contained cond cons conses conses*
syn keyword txl_keyword contained consp copy copy-alist copy-cons
syn keyword txl_keyword contained copy-hash copy-list copy-str copy-vec
syn keyword txl_keyword contained cos count-if countq countql
syn keyword txl_keyword contained countqual cum-norm-dist daemon dec
syn keyword txl_keyword contained defmacro defsymacro defun defvar
syn keyword txl_keyword contained del delay delete-package do
syn keyword txl_keyword contained dohash downcase-str dwim each
syn keyword txl_keyword contained each* empty env env-fbind
syn keyword txl_keyword contained env-hash env-vbind eq eql
syn keyword txl_keyword contained equal errno error eval
syn keyword txl_keyword contained evenp exit exp expt
syn keyword txl_keyword contained exptmod false fbind fboundp
syn keyword txl_keyword contained fifth filter-equal filter-string-tree find
syn keyword txl_keyword contained find-if find-max find-min find-package
syn keyword txl_keyword contained first fixnump flatten flatten*
syn keyword txl_keyword contained flet flip flo-int flo-str
syn keyword txl_keyword contained floatp floor flush-stream for
syn keyword txl_keyword contained for* force format fourth
syn keyword txl_keyword contained fun func-get-env func-get-form func-set-env
syn keyword txl_keyword contained functionp gcd gen generate
syn keyword txl_keyword contained gensym get-byte get-char get-hash-userdata
syn keyword txl_keyword contained get-line get-lines get-list-from-stream get-sig-handler
syn keyword txl_keyword contained get-string get-string-from-stream gethash getitimer
syn keyword txl_keyword contained getpid getppid giterate greater
syn keyword txl_keyword contained group-by gun hash hash-alist
syn keyword txl_keyword contained hash-construct hash-count hash-diff hash-eql
syn keyword txl_keyword contained hash-equal hash-isec hash-keys hash-pairs
syn keyword txl_keyword contained hash-uni hash-update hash-update-1 hash-values
syn keyword txl_keyword contained hashp html-decode html-encode iapply
syn keyword txl_keyword contained identity ido if iff
syn keyword txl_keyword contained iffi inc inhash int-flo
syn keyword txl_keyword contained int-str integerp intern interp-fun-p
syn keyword txl_keyword contained interpose ip ipf isqrt
syn keyword txl_keyword contained itimer-prov itimer-real itimer-virtual juxt
syn keyword txl_keyword contained keep-if keep-if* keywordp kill
syn keyword txl_keyword contained labels lambda last lazy-str
syn keyword txl_keyword contained lazy-str-force lazy-str-force-upto lazy-str-get-trailing-list lazy-stream-cons
syn keyword txl_keyword contained lazy-stringp lbind lcm lcons-fun
syn keyword txl_keyword contained lconsp ldiff length length-list
syn keyword txl_keyword contained length-str length-str-< length-str-<= length-str->
syn keyword txl_keyword contained length-str->= length-vec less let
syn keyword txl_keyword contained let* link lisp-parse list
syn keyword txl_keyword contained list* list-str list-vector listp
syn keyword txl_keyword contained log log-alert log-auth log-authpriv
syn keyword txl_keyword contained log-cons log-crit log-daemon log-debug
syn keyword txl_keyword contained log-emerg log-err log-info log-ndelay
syn keyword txl_keyword contained log-notice log-nowait log-odelay log-perror
syn keyword txl_keyword contained log-pid log-user log-warning log10
syn keyword txl_keyword contained log2 logand logior lognot
syn keyword txl_keyword contained logtest logtrunc logxor macro-form-p
syn keyword txl_keyword contained macro-time macroexpand macroexpand-1 macrolet
syn keyword txl_keyword contained major make-catenated-stream make-env make-hash
syn keyword txl_keyword contained make-lazy-cons make-like make-package make-random-state
syn keyword txl_keyword contained make-similar-hash make-string-byte-input-stream make-string-input-stream make-string-output-stream
syn keyword txl_keyword contained make-strlist-output-stream make-sym make-time make-time-utc
syn keyword txl_keyword contained make-trie makedev mapcar mapcar*
syn keyword txl_keyword contained mapdo maphash mappend mappend*
syn keyword txl_keyword contained mask match-fun match-regex match-regex-right
syn keyword txl_keyword contained match-str match-str-tree max member
syn keyword txl_keyword contained member-if memq memql memqual
syn keyword txl_keyword contained merge min minor mkdir
syn keyword txl_keyword contained mknod mkstring mod multi
syn keyword txl_keyword contained multi-sort n-choose-k n-perm-k nconc
syn keyword txl_keyword contained nilf none not nreverse
syn keyword txl_keyword contained null nullify num-chr num-str
syn keyword txl_keyword contained numberp oddp op open-command
syn keyword txl_keyword contained open-directory open-file open-files open-files*
syn keyword txl_keyword contained open-pipe open-process open-tail openlog
syn keyword txl_keyword contained or orf packagep partition
syn keyword txl_keyword contained partition* partition-by perm pop
syn keyword txl_keyword contained pos pos-if pos-max pos-min
syn keyword txl_keyword contained posq posql posqual pprinl
syn keyword txl_keyword contained pprint pprof prinl print
syn keyword txl_keyword contained prof prog1 progn prop
syn keyword txl_keyword contained proper-listp push pushhash put-byte
syn keyword txl_keyword contained put-char put-line put-lines put-string
syn keyword txl_keyword contained put-strings pwd qquote quasi
syn keyword txl_keyword contained quasilist quote rand random
syn keyword txl_keyword contained random-fixnum random-state-p range range*
syn keyword txl_keyword contained range-regex rcomb read readlink
syn keyword txl_keyword contained real-time-stream-p reduce-left reduce-right ref
syn keyword txl_keyword contained refset regex-compile regex-parse regexp
syn keyword txl_keyword contained regsub rehome-sym remhash remove-if
syn keyword txl_keyword contained remove-if* remove-path remq remq*
syn keyword txl_keyword contained remql remql* remqual remqual*
syn keyword txl_keyword contained rename-path repeat replace replace-list
syn keyword txl_keyword contained replace-str replace-vec rest ret
syn keyword txl_keyword contained retf return return-from reverse
syn keyword txl_keyword contained rlcp rperm rplaca rplacd
syn keyword txl_keyword contained run s-ifblk s-ifchr s-ifdir
syn keyword txl_keyword contained s-ififo s-iflnk s-ifmt s-ifreg
syn keyword txl_keyword contained s-ifsock s-irgrp s-iroth s-irusr
syn keyword txl_keyword contained s-irwxg s-irwxo s-irwxu s-isgid
syn keyword txl_keyword contained s-isuid s-isvtx s-iwgrp s-iwoth
syn keyword txl_keyword contained s-iwusr s-ixgrp s-ixoth s-ixusr
syn keyword txl_keyword contained search search-regex search-str search-str-tree
syn keyword txl_keyword contained second seek-stream select seqp
syn keyword txl_keyword contained set set-diff set-hash-userdata set-sig-handler
syn keyword txl_keyword contained sethash setitimer setlogmask sh
syn keyword txl_keyword contained sig-abrt sig-alrm sig-bus sig-check
syn keyword txl_keyword contained sig-chld sig-cont sig-fpe sig-hup
syn keyword txl_keyword contained sig-ill sig-int sig-io sig-iot
syn keyword txl_keyword contained sig-kill sig-lost sig-pipe sig-poll
syn keyword txl_keyword contained sig-prof sig-pwr sig-quit sig-segv
syn keyword txl_keyword contained sig-stkflt sig-stop sig-sys sig-term
syn keyword txl_keyword contained sig-trap sig-tstp sig-ttin sig-ttou
syn keyword txl_keyword contained sig-urg sig-usr1 sig-usr2 sig-vtalrm
syn keyword txl_keyword contained sig-winch sig-xcpu sig-xfsz sin
syn keyword txl_keyword contained sixth size-vec some sort
syn keyword txl_keyword contained source-loc source-loc-str span-str splice
syn keyword txl_keyword contained split-str split-str-set sqrt stat
syn keyword txl_keyword contained stdlib str< str<= str=
syn keyword txl_keyword contained str> str>= stream-get-prop stream-set-prop
syn keyword txl_keyword contained streamp string-extend string-lt stringp
syn keyword txl_keyword contained sub sub-list sub-str sub-vec
syn keyword txl_keyword contained symacrolet symbol-function symbol-name symbol-package
syn keyword txl_keyword contained symbol-value symbolp symlink sys-qquote
syn keyword txl_keyword contained sys-splice sys-unquote syslog tan
syn keyword txl_keyword contained tf third throw throwf
syn keyword txl_keyword contained time time-fields-local time-fields-utc time-string-local
syn keyword txl_keyword contained time-string-utc time-usec tofloat toint
syn keyword txl_keyword contained tok-str tok-where tostring tostringp
syn keyword txl_keyword contained transpose tree-bind tree-case tree-find
syn keyword txl_keyword contained trie-add trie-compress trie-lookup-begin trie-lookup-feed-char
syn keyword txl_keyword contained trie-value-at trim-str true trunc
syn keyword txl_keyword contained tuples typeof unget-byte unget-char
syn keyword txl_keyword contained uniq unless unquote until
syn keyword txl_keyword contained upcase-str update url-decode url-encode
syn keyword txl_keyword contained usleep uw-protect vec vec-push
syn keyword txl_keyword contained vec-set-length vecref vector vector-list
syn keyword txl_keyword contained vectorp when where while
syn keyword txl_keyword contained with-saved-vars zerop zip

syn match txr_error "@[\t ]*[*]\?[\t ]*."
syn match txr_nested_error "[^\t `]\+" contained
syn match txr_hashbang "^#!.*"
syn match txr_atat "@[ \t]*@"
syn match txr_comment "@[ \t]*[#;].*"
syn match txr_contin "@[ \t]*\\$"
syn match txr_char "@[ \t]*\\."
syn match txr_char "@[ \t]*\\x[0-9A-Fa-f]\+"
syn match txr_char "@[ \t]*\\[0-9]\+"
syn match txr_variable "@[ \t]*[*]\?[ \t]*[A-Za-z_][A-Za-z0-9_]*"
syn match txr_metanum "@[0-9]\+"
syn match txr_splicevar "@[ \t,*]*[A-Za-z_][A-Za-z0-9_]*"
syn match txr_regdir "@[ \t]*/\(\\/\|[^/]\|\\\n\)*/"

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

syn region txr_bracevar matchgroup=Delimiter start="@[ \t]*[*]\?{" matchgroup=Delimiter end="}" contains=txr_num,txr_ident,txr_string,txr_list,txr_bracket,txr_mlist,txr_mbracket,txr_regex,txr_quasilit,txr_chr,txl_splice,txr_nested_error
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
