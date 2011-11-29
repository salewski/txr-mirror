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
syn keyword txr_keyword contained defex throw deffilter filter eof eol

syn keyword txl_keyword contained let lambda call cond if and or defvar defun
syn keyword txl_keyword contained inc dec push pop gethash list append apply
syn keyword txl_keyword contained cons list atom null consp listp proper-listp
syn keyword txl_keyword contained length mapcar mappend apply

syn keyword txl_keyword second third fourth fifth sixth copy-list nreversei
syn keyword txl_keyword reverse ldiff flatten memq memqual
syn keyword txl_keyword tree-find some all none eq eql equal 

syn keyword txl_keyword contained + - * trunc mod numberp > < >= <= max min
syn keyword txl_keyword contained int-str

syn keyword txl_keyword contained search-regex match-regex

syn keyword txl_keyword contained make-hash gethash sethash pushhash remhash
syn keyword txl_keyword contained hash-count get-hash-userdata 
syn keyword txl_keyword contained set-hash-userdata 

syn keyword txl_keyword contained eval

syn keyword txl_keyword contained *stdout* *stdin* *stderr*
syn keyword txl_keyword contained format print pprint
syn keyword txl_keyword contained make-string-input-stream
syn keyword txl_keyword contained make-string-byte-input-stream
syn keyword txl_keyword contained make-string-output-stream
syn keyword txl_keyword contained get-string-from-stream
syn keyword txl_keyword contained make-strlist-output-stream
syn keyword txl_keyword contained get-list-from-stream
syn keyword txl_keyword contained close-stream
syn keyword txl_keyword contained get-line get-char get-byte
syn keyword txl_keyword contained put-string put-line put-char
syn keyword txl_keyword contained flush-stream open-directory
syn keyword txl_keyword contained open-file open-pipe

set lispwords=let open-file

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
