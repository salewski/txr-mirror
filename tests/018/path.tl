(load "../common")

(mtest
  (short-suffix 42) :error
  (short-suffix #\a) :error
  (short-suffix "") nil
  (short-suffix "" 0) 0
  (short-suffix "a") nil
  (short-suffix "a" 0) 0
  (short-suffix ".") ""
  (short-suffix "a.") ""
  (short-suffix "a.b.") ""
  (short-suffix ".c") "c"
  (short-suffix "a.c") "c"
  (short-suffix "a.b.c") "c"
  (short-suffix "foo.txt.gz") "gz"
  (short-suffix ".gz") "gz")

(mtest
  (long-suffix 42) :error
  (long-suffix #\a) :error
  (long-suffix "") nil
  (long-suffix "" 0) 0
  (long-suffix "a") nil
  (long-suffix "a" 0) 0
  (long-suffix ".") ""
  (long-suffix "a.") ""
  (long-suffix "a.b.") "b."
  (long-suffix ".c") "c"
  (long-suffix "a.c") "c"
  (long-suffix "a.b.c") "b.c"
  (long-suffix "foo.txt.gz") "txt.gz"
  (long-suffix ".gz") "gz"
  (long-suffix ".txt.gz") "txt.gz")
