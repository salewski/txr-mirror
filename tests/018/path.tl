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
  (long-suffix "a/.txt.gz") ".gz")

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
