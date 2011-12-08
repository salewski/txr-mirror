" VIM Syntax file for txr
" Kaz Kylheku <kaz@kylheku.com>
" 2011-11-01

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

syn case match
syn spell toplevel

setlocal iskeyword=a-z,+,-,*,<,>,=

syn keyword txr_keyword contained skip trailer freeform block accept fail
syn keyword txr_keyword contained next some all none and or
syn keyword txr_keyword contained maybe cases choose gather collect coll until last end
syn keyword txr_keyword contained flatten forget local merge bind set cat output
syn keyword txr_keyword contained repeat rep first last single empty
syn keyword txr_keyword contained define try catch finally throw
syn keyword txr_keyword contained defex throw deffilter filter eof eol do

syn keyword txl_keyword contained progn let syn let* lambda call fun 
syn keyword txl_keyword contained cond if and or
syn keyword txl_keyword contained defvar defun inc dec set push pop flip
syn keyword txl_keyword contained for for* dohash unwind-protect block
syn keyword txl_keyword contained return return-from

syn keyword txl_keyword contained cons make-lazy-cons lcons-fun car cdr
syn keyword txl_keyword contained rplaca rplacd first rest append list
syn keyword txl_keyword contained identity typeof atom null consp listp
syn keyword txl_keyword contained proper-listp length-list mapcar mappend apply
syn keyword txl_keyword contained second third fourth fifth sixth copy-list nreverse
syn keyword txl_keyword contained reverse ldiff flatten memq memqual tree-find some
syn keyword txl_keyword contained all none eq eql equal + - * trunc mod numberp >
syn keyword txl_keyword contained < >= <= max min search-regex match-regex
syn keyword txl_keyword contained make-hash gethash sethash pushhash remhash
syn keyword txl_keyword contained hash-count hashp maphash eval *stdout* *stdin*
syn keyword txl_keyword contained *stderr* format print pprint make-string-input-stream
syn keyword txl_keyword contained make-string-byte-input-stream make-string-output-stream
syn keyword txl_keyword contained get-string-from-stream make-strlist-output-stream
syn keyword txl_keyword contained get-list-from-stream close-stream
syn keyword txl_keyword contained get-line get-char get-byte put-string put-line
syn keyword txl_keyword contained put-char flush-stream open-directory open-file
syn keyword txl_keyword contained open-pipe *user-package* *keyword-package* *system-package*
syn keyword txl_keyword contained make-sym make-package find-package
syn keyword txl_keyword contained intern symbolp symbol-name symbol-package keywordp
syn keyword txl_keyword contained mkstring copy-str upcase-str downcase-str string-extend
syn keyword txl_keyword contained stringp lazy-stringp length-str search-str search-str-tree
syn keyword txl_keyword contained sub-str cat-str split-str split-str-set trim-str
syn keyword txl_keyword contained string-lt int-str chrp chr-isalnum chr-isalpha
syn keyword txl_keyword contained chr-isascii chr-iscntrl chr-isdigit chr-isgraph
syn keyword txl_keyword contained chr-islower chr-isprint chr-ispunct chr-isspace chr-isupper
syn keyword txl_keyword contained chr-isxdigit chr-toupper chr-tolower chr-str
syn keyword txl_keyword contained chr-str-set span-str compl-span-str break-str
syn keyword txl_keyword contained vector vec-get-fill vec-set-fill vecref
syn keyword txl_keyword contained vec-push length-vec size-vec vector-list
syn keyword txl_keyword contained list-vector assoc assq acons acons-new
syn keyword txl_keyword contained aconsq-new alist-remove alist-nremove copy-cons
syn keyword txl_keyword contained copy-alist merge sort find set-diff length

syn match txr_comment "@[ \t]*#.*"
syn match txr_contin "@[ \t]*\\$"
syn match txr_hashbang "^#!.*"
syn match txr_char "@[ \t]*\\."
syn match txr_char "@[ \t]*\\x[0-9A-Fa-f]\+"
syn match txr_char "@[ \t]*\\[0-9]\+"
syn match txr_variable "@[ \t]*[*]\?[A-Za-z_][A-Za-z0-9_]*"
syn match txr_chr "#\\x[A-Fa-f0-9]\+"
syn match txr_chr "#\\[a-zA-Z_][a-zA-Z0-9_]*"

syn match txr_ident "[a-zA-Z0-9!$%&*+\-<=>?\\^_~]\+" contained
syn match txr_num "[+-]\?[0-9]\+" contained

syn region txr_bracevar matchgroup=Delimiter start="@[ \t]*[*]\?{" matchgroup=Delimiter end="}" contains=txr_num,txr_ident,xr_string,txr_list,txr_regex,txr_quasilit,txr_chr

syn region txr_directive matchgroup=Delimiter start="@[ \t]*(" matchgroup=Delimiter end=")" contains=txr_keyword,txr_string,txr_list,txr_meta,txr_quasilit,txr_num,txr_ident,txr_regex,txr_string,txr_variable,txr_chr

syn region txr_list contained matchgroup=Delimiter start="(" matchgroup=Delimiter end=")" contains=txl_keyword,txr_string,txr_regex,txr_num,txr_ident,txr_variable,txr_meta,txr_list,txr_quasilit,txr_chr

syn region txr_meta contained matchgroup=Delimiter start="@[ \t]*(" matchgroup=Delimiter end=")" contains=txl_keyword,txr_string,txr_list,txr_regex,txr_num,txr_ident,txr_variable,txr_quasilit,txr_chrb

syn region txr_string contained oneline start=+"+ skip=+\\\\\|\\"+ end=+"+
syn region txr_quasilit contained oneline start=+`+ skip=+\\\\\|\\`+ end=+`+ contains=txr_directive,txr_variable,txr_bracevar
syn region txr_regex contained oneline start=+/+ skip=+\\\\\|\\/+ end=+/+
syn region txr_regdir oneline start=+@[ \t]*/+ skip=+\\\\\|\\/+ end=+/+

hi def link txr_comment Comment
hi def link txr_hashbang Comment
hi def link txr_contin Comment
hi def link txr_char String
hi def link txr_keyword Keyword
hi def link txl_keyword Keyword
hi def link txr_string String
hi def link txr_chr String
hi def link txr_quasilit String
hi def link txr_regex String
hi def link txr_regdir String
hi def link txr_variable Identifier
hi def link txr_bracevar Identifier
hi def link txr_ident Identifier
hi def link txr_num Number

let b:current_syntax = "lisp"
