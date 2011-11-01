" VIM Syntax file for txr
" Kaz Kylheku <kaz@kylheku.com>
" 2011-11-01

syn case match
syn spell toplevel

syn keyword txr_keyword contained skip trailer freeform block accept fail
syn keyword txr_keyword contained next some all none maybe cases choose collect
syn keyword txr_keyword contained flatten forget local merge bind set cat output
syn keyword txr_keyword contained define try defex throw deffilter filter eof eol

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

syn region txr_bracevar matchgroup=Delimiter start="@[ \t]*[*]\?{" matchgroup=Delimiter end="}" contains=txr_ident,txr_num,txr_string,txr_list,txr_regex

syn region txr_directive matchgroup=Delimiter start="@[ \t]*("  skip="|.\{-}|" matchgroup=Delimiter end=")" contains=txr_keyword,txr_string,txr_list,txr_quasilit,txr_num,txr_ident,txr_regex,txr_string

syn region txr_list contained matchgroup=Delimiter start="("  skip="|.\{-}|" matchgroup=Delimiter end=")" contains=txr_string,txr_regex,txr_ident,txr_num

syn region txr_string contained oneline start=+"+ skip=+\\\\\|\\"+ end=+"+
syn region txr_quasilit contained oneline start=+`+ skip=+\\\\\|\\`+ end=+`+ contains=txr_directive,txr_variable,txr_bracevar
syn region txr_regex contained oneline start=+/+ skip=+\\\\\|\\/+ end=+/+
syn region txr_regdir oneline start=+@/+ skip=+\\\\\|\\/+ end=+/+

hi def link txr_comment Comment
hi def link txr_hashbang Comment
hi def link txr_contin Comment
hi def link txr_char String
hi def link txr_keyword Keyword
hi def link txr_string String
hi def link txr_quasilit String
hi def link txr_regex String
hi def link txr_regdir String
hi def link txr_variable Identifier
hi def link txr_bracevar Identifier
hi def link txr_ident Identifier
hi def link txr_num Number
