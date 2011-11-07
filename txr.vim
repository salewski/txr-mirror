" VIM Syntax file for txr
" Kaz Kylheku <kaz@kylheku.com>
" 2011-11-01

" INSTALL-HOWTO:
"
" 1. Create the directory .vim/syntax in your home directory and
"    put this file there.
" 2. In your .vimrc, add this command to associate *.txr files
"    with the txr filetype.
"    :au BufRead,BufNewFile *.txr set filetype=txr
"
" If you want syntax highlighting to be on automatically (for any language)
" you need to add ":syntax on" in your .vimrc also. But you knew that already!

syn case match
syn spell toplevel

syn keyword txr_keyword contained skip trailer freeform block accept fail
syn keyword txr_keyword contained next some all none and or
syn keyword txr_keyword contained maybe cases choose gather collect until last end
syn keyword txr_keyword contained flatten forget local merge bind set cat output
syn keyword txr_keyword contained repeat rep first last single empty
syn keyword txr_keyword contained define try catch finally throw
syn keyword txr_keyword contained defex throw deffilter filter eof eol

syn match txr_at "@[ \t]*@"
syn match txr_comment "@#.*"
syn match txr_contin "@\\$"
syn match txr_hashbang "^#!.*"
syn match txr_char "@[ \t]*\\."
syn match txr_char "@[ \t]*\\x[0-9A-Fa-f]\+"
syn match txr_char "@[ \t]*\\[0-9]\+"
syn match txr_variable "@[ \t]*[*]\?[A-Za-z_][A-Za-z0-9_]*"

syn match txr_ident ":\?[A-Za-z_][A-Za-z0-9_]*" contained
syn match txr_num "[+-]\?[0-9]\+" contained

syn region txr_bracevar matchgroup=Delimiter start="@[ \t]*[*]\?{" matchgroup=Delimiter end="}" contains=txr_ident,txr_num,txr_string,txr_list,txr_regex,txr_quasilit,txr_chr

syn region txr_directive matchgroup=Delimiter start="@[ \t]*(" matchgroup=Delimiter end=")" contains=txr_keyword,txr_string,txr_list,txr_meta,txr_quasilit,txr_num,txr_ident,txr_regex,txr_string,txr_variable,txr_chr

syn region txr_list contained matchgroup=Delimiter start="(" matchgroup=Delimiter end=")" contains=txr_string,txr_regex,txr_ident,txr_num,txr_variable,txr_meta,txr_list,txr_quasilit,txr_chr

syn region txr_meta contained matchgroup=Delimiter start="@(" matchgroup=Delimiter end=")" contains=txr_string,txr_regex,txr_ident,txr_num,txr_variable,txr_quasilit,txr_chr

syn region txr_string contained oneline start=+"+ skip=+\\\\\|\\"+ end=+"+
syn region txr_quasilit contained oneline start=+`+ skip=+\\\\\|\\`+ end=+`+ contains=txr_directive,txr_variable,txr_bracevar
syn region txr_chr contained oneline start=+'+ skip=+\\\\\|\\'+ end=+'+ contains=txr_directive,txr_variable,txr_bracevar
syn region txr_regex contained oneline start=+/+ skip=+\\\\\|\\/+ end=+/+
syn region txr_regdir oneline start=+@/+ skip=+\\\\\|\\/+ end=+/+

hi def link txr_comment Comment
hi def link txr_hashbang Comment
hi def link txr_contin Comment
hi def link txr_char String
hi def link txr_keyword Keyword
hi def link txr_string String
hi def link txr_chr String
hi def link txr_quasilit String
hi def link txr_regex String
hi def link txr_regdir String
hi def link txr_variable Identifier
hi def link txr_bracevar Identifier
hi def link txr_ident Identifier
hi def link txr_num Number
