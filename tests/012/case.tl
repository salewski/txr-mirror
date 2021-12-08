(load "../common")

(mtest
  (caseq 0 (1 :match)) nil
  (caseq 0 ((1) :match)) nil
  (caseq 1 (1 :match)) :match
  (caseq 1 ((1) :match)) :match
  (caseq 1 ((0 1) :match)) :match
  (caseq 1 ((0 2) :match)) nil
  (caseq 1 (t :match)) :match
  (caseq 1 ((t) :match)) nil
  (caseq t ((t) :match)) :match)

(defvar o 1)
(defvar y t)

(mtest
  (caseq* 0 (o :match)) nil
  (caseq* 0 ((o) :match)) nil
  (caseq* 1 (o :match)) :match
  (caseq* 1 ((o) :match)) :match
  (caseq* 1 ((0 o) :match)) :match
  (caseq* 1 ((0 2) :match)) nil
  (caseq* 1 (t :match)) :match
  (caseq* 1 (y :match)) nil
  (caseq* 1 ((t) :match)) nil
  (caseq* t ((t) :match)) :match
  (caseq* t ((y) :match)) :match)

(test (casequal '(a b c d)
        (((a b c d)) :match))
      :match)
