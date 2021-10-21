(load "../common")

(mtest
  (path-equal "a" "a") t
  (path-equal "a" "b") nil
  (path-equal "/a" "a") nil

  (path-equal "a" "a/") t
  (path-equal "a/" "a/") t

  (path-equal "a/b/../c" "a/c") t

  (path-equal "a" "a/././.") t
  (path-equal "a/." "a/././.") t

  (path-equal "/.." "/") t
  (path-equal "/../a" "/a/") t)
