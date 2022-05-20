(load "../common")

(mtest
  (short-suffix 42) :error
  (short-suffix #\a) :error
  (short-suffix "") nil
  (short-suffix "" 0) 0
  (short-suffix "a") nil
  (short-suffix "a" 0) 0
  (short-suffix ".") nil
  (short-suffix "a.") "."
  (short-suffix "a.b.") "."
  (short-suffix ".c") nil
  (short-suffix "a.c") ".c"
  (short-suffix ".a.c") ".c"
  (short-suffix "a.b.c") ".c"
  (short-suffix "foo.txt.gz") ".gz"
  (short-suffix "txt.gz") ".gz"
  (short-suffix ".gz") nil)

(mtest
  (long-suffix 42) :error
  (long-suffix #\a) :error
  (long-suffix "") nil
  (long-suffix "" 0) 0
  (long-suffix "a") nil
  (long-suffix "a" 0) 0
  (long-suffix ".") nil
  (long-suffix "a.") "."
  (long-suffix "a.b.") ".b."
  (long-suffix ".c") nil
  (long-suffix "a.c") ".c"
  (long-suffix "a.b.c") ".b.c"
  (long-suffix "foo.txt.gz") ".txt.gz"
  (long-suffix ".gz") nil
  (long-suffix ".txt.gz") ".gz"
  (long-suffix "/.txt.gz") ".gz"
  (long-suffix "a/.txt.gz") ".gz"
  (long-suffix "a/.txt.tar.gz") ".tar.gz")

(mtest
  (short-suffix "/") nil
  (short-suffix "a/") nil
  (short-suffix "/.") nil
  (short-suffix "a/.") nil
  (short-suffix ".a/") nil
  (short-suffix ".a/b") nil
  (short-suffix ".a/c.b") ".b"
  (short-suffix ".a/b/") nil
  (short-suffix ".a/b/.b") nil
  (short-suffix ".a/b/.b/") nil
  (short-suffix ".a/b/c.b") ".b"
  (short-suffix ".a/b/c.b/") ".b"
  (short-suffix ".a/b/c.b//") ".b"
  (short-suffix ".a/b/c.b///") ".b"
  (short-suffix ".a/b/c.") "."
  (short-suffix ".a/b/c./") "."
  (short-suffix ".a/b/c.//") "."
  (short-suffix ".a/b/c.///") ".")

(mtest
  (long-suffix "/") nil
  (long-suffix "a/") nil
  (long-suffix "/.") nil
  (long-suffix "a/.") nil
  (long-suffix ".a/") nil
  (long-suffix ".a/b") nil
  (long-suffix ".a/b/") nil
  (long-suffix ".a/b/.b") nil
  (long-suffix ".a/b/.b/") nil
  (long-suffix ".a/b/c.b") ".b"
  (long-suffix ".a/b/c.b/") ".b"
  (long-suffix "a.b/c.d.e") ".d.e"
  (long-suffix "a.b/c.d.e/") ".d.e"
  (long-suffix "a.b/c.d.e/f") nil
  (long-suffix "a.b/c.d.e/f.g.h") ".g.h"
  (long-suffix "a.b/c.d.e//") ".d.e"
  (long-suffix "a.b/c.d.e///") ".d.e"
  (long-suffix "a.b/c.d.") ".d."
  (long-suffix "a.b/c.d./") ".d."
  (long-suffix "a.b/c.d.//") ".d."
  (long-suffix "a.b/c.d.///") ".d."
  (long-suffix "a.b/c.") "."
  (long-suffix "a.b/c./") "."
  (long-suffix "a.b/c.//") "."
  (long-suffix "a.b/c.///") ".")

(mtest
  (trim-short-suffix "") ""
  (trim-short-suffix ".") "."
  (trim-short-suffix "/.") "/."
  (trim-short-suffix ".b") ".b"
  (trim-short-suffix ".a.b") ".a"
  (trim-short-suffix ".a.b.c") ".a.b"
  (trim-short-suffix "/.b") "/.b"
  (trim-short-suffix "/.b/") "/.b/"
  (trim-short-suffix "/.b//") "/.b//"
  (trim-short-suffix "a.b") "a"
  (trim-short-suffix "/a.b") "/a"
  (trim-short-suffix "/a.b/") "/a/"
  (trim-short-suffix "/a.b//") "/a//"
  (trim-short-suffix "a.") "a"
  (trim-short-suffix "/a.") "/a"
  (trim-short-suffix "/a./") "/a/"
  (trim-short-suffix "/a.//") "/a//")

(mtest
  (trim-long-suffix "") ""
  (trim-long-suffix ".") "."
  (trim-long-suffix "/.") "/."
  (trim-long-suffix ".b") ".b"
  (trim-long-suffix ".a.b") ".a"
  (trim-long-suffix ".a.b.c") ".a"
  (trim-long-suffix "/.b") "/.b"
  (trim-long-suffix "/.b/") "/.b/"
  (trim-long-suffix "/.b//") "/.b//"
  (trim-long-suffix "a.b") "a"
  (trim-long-suffix "/a.b") "/a"
  (trim-long-suffix "/a.b/") "/a/"
  (trim-long-suffix "/a.b//") "/a//"
  (trim-long-suffix "/.b.c") "/.b"
  (trim-long-suffix "/.b.c/") "/.b/"
  (trim-long-suffix "/.b.c//") "/.b//"
  (trim-long-suffix "/.b.c.d") "/.b"
  (trim-long-suffix "/.b.c.d/") "/.b/"
  (trim-long-suffix "/.b.c.d//") "/.b//"
  (trim-long-suffix "a.b.c") "a"
  (trim-long-suffix "/a.b.c") "/a"
  (trim-long-suffix "/a.b.c/") "/a/"
  (trim-long-suffix "/a.b.c//") "/a//"
  (trim-long-suffix "a.") "a"
  (trim-long-suffix "/a.") "/a"
  (trim-long-suffix "/a./") "/a/"
  (trim-long-suffix "/a.//") "/a//")

(mtest
  (add-suffix "" "") ""
  (add-suffix "" "a") "a"
  (add-suffix "." "a") ".a"
  (add-suffix "." ".a") "..a"
  (add-suffix "/" ".b") "/.b"
  (add-suffix "//" ".b") "/.b/"
  (add-suffix "//" "b") "/b/"
  (add-suffix "a" "") "a"
  (add-suffix "a" ".b") "a.b"
  (add-suffix "a/" ".b") "a.b/"
  (add-suffix "a//" ".b") "a.b//"

  (add-suffix "c://" "x") "c:/x/"
  (add-suffix "0://" "x") "0:/x/"
  (add-suffix "host://" "x") "host://x"
  (add-suffix "host:///" "x") "host://x/"
  (add-suffix "1234:///" "x") "1234://x/")

(mtest
  (base-name "") ""
  (base-name "/") "/"
  (base-name ".") "."
  (base-name "./") "."
  (base-name "a") "a"
  (base-name "a/") "a"
  (base-name "/a") "a"
  (base-name "/a/") "a"
  (base-name "/a/b") "b"
  (base-name "/a/b/") "b"
  (base-name "/a/b//") "b"
  (base-name "/a/b///") "b")

(mtest
  (base-name "" "") ""
  (base-name "/" "/") "/"
  (base-name "/" "") "/"
  (base-name "." ".") "."
  (base-name "." "") "."
  (base-name "./" "/") "."
  (base-name "a" "a") "a"
  (base-name "a" "") "a"
  (base-name "a.b" ".b") "a"
  (base-name "a.b/" ".b") "a"
  (base-name "a.b/" ".b/") "a.b"
  (base-name "a.b/" "a.b") "a.b")

(mtest
  (path-cat "" "") ""
  (path-cat "" ".") "."
  (path-cat "." "") "."
  (path-cat "." ".") "."
  (path-cat "abc" ".") "abc"
  (path-cat "." "abc") "abc"
  (path-cat "./" ".") "./"
  (path-cat "."  "./") "./"
  (path-cat "abc/" ".") "abc/"
  (path-cat "./" "abc") "abc"
  (path-cat "/" ".") "/"
  (path-cat "/" "abc") "/abc"
  (path-cat "ab/cd" "ef") "ab/cd/ef"
  (path-cat "a" "b" "c") "a/b/c"
  (path-cat "a" "b" "" "c" "/") "a/b/c/")

(mtest
  (path-cat) "."
  (path-cat 3) :error
  (path-cat "") ""
  (path-cat "/") "/"
  (path-cat ".") "."
  (path-cat "" "" "") ""
  (path-cat "." "" "") "."
  (path-cat "" "." "") "."
  (path-cat "" "" ".") "."
  (path-cat "." "." ".") "."
  (path-cat "abc/" "/def/" "g") "abc/def/g"
  (path-cat "abc/" "/def/" "g/") "abc/def/g/"
  (path-cat "" "abc/" "/def/" "g/") "abc/def/g/")

(mtest
  (abs-path-p "") nil
  (abs-path-p "/") t
  (abs-path-p "/abc") t
  (abs-path-p "abc") nil
  (abs-path-p ".") nil)

(if (eql [path-sep-chars 0] #\\)
  (mtest
    (abs-path-p "\\abc") t
    (abs-path-p "a:\\abc") t
    (abs-path-p "0:\\abc") t
    (abs-path-p "AB0:\\abc") t
    (abs-path-p "cd5:\\abc") t
    (abs-path-p "a:/abc") t
    (abs-path-p "0:/abc") t
    (abs-path-p "AB0:/abc") t
    (abs-path-p "cd5:/abc") t)
  (mtest
    (abs-path-p "\\abs") nil
    (abs-path-p "a:\\abc") nil
    (abs-path-p "0:\\abc") nil
    (abs-path-p "AB0:\\abc") nil
    (abs-path-p "cd5:\\abc") nil
    (abs-path-p "a:/abc") nil
    (abs-path-p "0:/abc") nil
    (abs-path-p "AB0:/abc") nil
    (abs-path-p "cd5:/abc") nil))

(mtest
  (portable-abs-path-p "") nil
  (portable-abs-path-p "/") t
  (portable-abs-path-p "/abc") t
  (portable-abs-path-p "\\abc") t
  (portable-abs-path-p "abc") nil
  (portable-abs-path-p ".") nil
  (portable-abs-path-p "\\abc") t
  (portable-abs-path-p "a:\\abc") t
  (portable-abs-path-p "0:\\abc") t
  (portable-abs-path-p "AB0:\\abc") t
  (portable-abs-path-p "cd5:\\abc") t
  (portable-abs-path-p "a:/abc") t
  (portable-abs-path-p "0:/abc") t
  (portable-abs-path-p "AB0:/abc") t
  (portable-abs-path-p "cd5:/abc") t)

(mtest
  (pure-rel-path-p "") t
  (pure-rel-path-p "/") nil
  (pure-rel-path-p "\\") nil
  (pure-rel-path-p "/abc") nil
  (pure-rel-path-p ".") nil
  (pure-rel-path-p "./") nil
  (pure-rel-path-p ".\\") nil
  (pure-rel-path-p ".abc") t
  (pure-rel-path-p ".abc/") t
  (pure-rel-path-p ".abc\\") t
  (pure-rel-path-p ":") t
  (pure-rel-path-p "a:") nil
  (pure-rel-path-p "A:") nil
  (pure-rel-path-p "0:") nil
  (pure-rel-path-p "9:") nil
  (pure-rel-path-p "_:") t
  (pure-rel-path-p "abc") t
  (pure-rel-path-p "abc/def") t
  (pure-rel-path-p "abc/.") t
  (pure-rel-path-p "abc\\def") t
  (pure-rel-path-p "abc\\.") t)

(mtest
  (trim-path-seps "") ""
  (trim-path-seps "/") "/"
  (trim-path-seps "//") "/"
  (trim-path-seps "///") "/"
  (trim-path-seps "a///") "a"
  (trim-path-seps "/a///") "/a")

(mtest
  (trim-path-seps "c:/") "c:/"
  (trim-path-seps "c://") "c:/"
  (trim-path-seps "c:///") "c:/"
  (trim-path-seps "c:a///") "c:a"
  (trim-path-seps "/c:/a///") "/c:/a"
  (trim-path-seps "/c://///") "/c:")

(mtest
  (trim-path-seps "\\") "\\"
  (trim-path-seps "\\\\") "\\"
  (trim-path-seps "\\\\\\") "\\"
  (trim-path-seps "a\\\\\\") "a"
  (trim-path-seps "\\a\\\\\\") "\\a")

(mtest
  (trim-path-seps "c:\\") "c:\\"
  (trim-path-seps "c:\\\\") "c:\\"
  (trim-path-seps "c:\\\\\\") "c:\\"
  (trim-path-seps "c:a\\\\\\") "c:a"
  (trim-path-seps "\\c:\\a\\\\\\") "\\c:\\a"
  (trim-path-seps "\\c:\\\\\\\\\\") "\\c:")

(mtest
  (trim-path-seps "/c:\\") "/c:"
  (trim-path-seps "c:/\\/\\/") "c:/"
  (trim-path-seps "c:a\\\\\\") "c:a"
  (trim-path-seps "\\c:\\a/\\\\\\") "\\c:\\a"
  (trim-path-seps "/c:\\\\\\\\\\") "/c:")
