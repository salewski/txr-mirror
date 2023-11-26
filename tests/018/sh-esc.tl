(load "../common")

(mtest
  (sh-esc "") ""
  (sh-esc "a") "a")

(mtest
  (sh-esc "|") "\"|\""
  (sh-esc "&") "\"&\""
  (sh-esc ";") "\";\""
  (sh-esc "<") "\"<\""
  (sh-esc ">") "\">\""
  (sh-esc "(") "\"(\""
  (sh-esc ")") "\")\""
  (sh-esc " ") "\" \""
  (sh-esc "\t") "\"\t\""
  (sh-esc "\n") "\"\n\""
  (sh-esc "*") "\"*\""
  (sh-esc "?") "\"?\""
  (sh-esc "[") "\"[\""
  (sh-esc "#") "\"#\""
  (sh-esc "~") "\"~\"")

(mtest
  (sh-esc "'") "\"'\"")

(mtest
  (sh-esc "\"") "'\"'"
  (sh-esc "$") "'$'"
  (sh-esc "`") "'`'"
  (sh-esc "\\") "'\\'")

(mtest
  (sh-esc "=") "="
  (sh-esc "%") "%"
  (sh-esc-all "=") "\"=\""
  (sh-esc-all "%") "\"%\"")

(test
  (sh-esc "a\"b'c") "'a\"b'\\''c'")

(mtest
  (sh-esc "|'") "\"|'\""
  (sh-esc "|\"") "'|\"'"
  (sh-esc "'$") "''\\''$'")

(mtest
  (sh-esc-all "|=") "\"|=\""
  (sh-esc-all "'=") "\"'=\""
  (sh-esc-all "\"=") "'\"='")

(mtest
  (sh-esc "|&;<>() \t\n*?[#~") "\"|&;<>() \t\n*?[#~\""
  (sh-esc "\"$`\\") "'\"$`\\'")

(mtest
  (sh-esc-dq "$`\\\"\n'abc()*~") "\\$\\`\\\\\\\"\n'abc()*~"
  (sh-esc-sq "$`\\\"\n'abc()*~") "$`\\\"\n'\\''abc()*~")
