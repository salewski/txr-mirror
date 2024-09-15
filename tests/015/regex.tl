(load "../common")

(defun rum (str regex : include-match) 
  (with-in-string-stream (s str)
    (list (read-until-match regex s include-match)
          (read-until-match regex s include-match))))

(mtest
  (rum "a-b" #/-/) ("a" "b")
  (rum "a-b" #/-/ t) ("a-" "b")
  (rum "a----b" #/-+/) ("a" "b")
  (rum "a----b" #/-+/ t) ("a----" "b")
  (rum "a----b" #/-*/) ("a" "b")
  (rum "a----b" #/-*/ t) ("a----" "b")
  (rum "abc" #/-/) ("abc" nil)
  (rum "abc" #/-/ t) ("abc" nil)
  (rum "a___b___#c" #/_+#/) ("a___b" "c")
  (rum "a___b___#c" #/_+#/ t) ("a___b___#" "c"))
