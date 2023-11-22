(load "../common")

;; Passes on Cygwin, but too slow!
(if (eq (os-symbol) :cygwin)
  (exit))

(mtest
  (sys:brace-expand "~/{Downloads,Pictures}/*.{jpg,gif,png}")
  #"~/Downloads/*.jpg ~/Downloads/*.gif ~/Downloads/*.png \
    ~/Pictures/*.jpg ~/Pictures/*.gif ~/Pictures/*.png"
  (sys:brace-expand "It{{em,alic}iz,erat}e{d,}, please.")
  ("Itemized, please." "Itemize, please." "Italicized, please."
  "Italicize, please." "Iterated, please." "Iterate, please.")
  (sys:brace-expand "{,{,gotta have{ ,\\, again\\, }}more }cowbell!")
  ("cowbell!" "more cowbell!" "gotta have more cowbell!"
   "gotta have\\, again\\, more cowbell!")
  (sys:brace-expand "{}} some }{,{\\\\{ edge, edge} \\,}{ cases, {here} \\\\\\\\\\}")
  ("{}} some }{,{\\\\ edge \\,}{ cases, {here} \\\\\\\\\\}"
   "{}} some }{,{\\\\ edge \\,}{ cases, {here} \\\\\\\\\\}"))

(mtest
  (glob* "tests/**/002") ("tests/002")
  (glob* "tests/**/{003,004}") ("tests/003" "tests/004"))

(chdir "tests/002/proc")

(mtest
  (glob* "**")
  ("1" "1/status" "1/tasks" "103" "103/status" "103/tasks" "1068"
   "1068/status" "1068/tasks" "1235" "1235/status" "1235/tasks"
   "1236" "1236/status" "1236/tasks" "15812" "15812/status" "15812/tasks"
   "16" "16/status" "16/tasks" "1620" "1620/status" "1620/tasks"
   "1624" "1624/status" "1624/tasks" "16248" "16248/status" "16248/tasks"
   "16249" "16249/status" "16249/tasks" "1645" "1645/status" "1645/tasks"
   "16598" "16598/tasks" "1665" "1665/status" "1665/tasks" "1698"
   "1698/status" "1698/tasks" "17" "17/status" "17/tasks" "175"
   "175/status" "175/tasks" "1766" "1766/status" "1766/tasks" "1790"
   "1790/status" "1790/tasks" "1791" "1791/status" "1791/tasks"
   "1821" "1821/status" "1821/tasks" "1839" "1839/status" "1839/tasks"
   "1851" "1851/status" "1851/tasks" "186" "186/status" "186/tasks"
   "18614" "18614/tasks" "1887" "1887/status" "1887/tasks" "1902"
   "1902/status" "1902/tasks" "1921" "1921/status" "1921/tasks"
   "1925" "1925/status" "1925/tasks" "1926" "1926/status" "1926/tasks"
   "1927" "1927/status" "1927/tasks" "1928" "1928/status" "1928/tasks"
   "1929" "1929/status" "1929/tasks" "1930" "1930/status" "1930/tasks"
   "1931" "1931/status" "1931/tasks" "1932" "1932/status" "1932/tasks"
   "1936" "1936/status" "1936/tasks" "1963" "1963/status" "1963/tasks"
   "1989" "1989/status" "1989/tasks" "2" "2/status" "2/tasks" "2008"
   "2008/status" "2008/tasks" "2027" "2027/status" "2027/tasks"
   "2041" "2041/status" "2041/tasks" "2052" "2052/status" "2052/tasks"
   "2062" "2062/status" "2062/tasks" "2124" "2124/status" "2124/tasks"
   "2184" "2184/status" "2184/tasks" "2354" "2354/status" "2354/tasks"
   "24134" "24134/tasks" "2551" "2551/status" "2551/tasks" "2579"
   "2579/status" "2579/tasks" "2625" "2625/status" "2625/tasks"
   "2626" "2626/status" "2626/tasks" "2631" "2631/status" "2631/tasks"
   "2634" "2634/status" "2634/tasks" "2636" "2636/status" "2636/tasks"
   "2638" "2638/status" "2638/tasks" "2644" "2644/status" "2644/tasks"
   "2661" "2661/status" "2661/tasks" "2685" "2685/status" "2685/tasks"
   "2689" "2689/status" "2689/tasks" "2691" "2691/status" "2691/tasks"
   "2693" "2693/status" "2693/tasks" "2695" "2695/status" "2695/tasks"
   "2698" "2698/status" "2698/tasks" "2701" "2701/status" "2701/tasks"
   "2707" "2707/status" "2707/tasks" "27121" "27121/status" "27121/tasks"
   "2717" "2717/status" "2717/tasks" "2718" "2718/status" "2718/tasks"
   "2720" "2720/status" "2720/tasks" "2722" "2722/status" "2722/tasks"
   "27243" "27243/status" "27243/tasks" "2726" "2726/status" "2726/tasks"
   "2728" "2728/status" "2728/tasks" "27682" "27682/status" "27682/tasks"
   "27684" "27684/status" "27684/tasks" "27685" "27685/status" "27685/tasks"
   "28" "28/status" "28/tasks" "29" "29/status" "29/tasks" "29840"
   "29840/status" "29840/tasks" "3" "3/status" "3/tasks" "30737"
   "30737/status" "30737/tasks" "31905" "31905/status" "31905/tasks"
   "31907" "31907/status" "31907/tasks" "31908" "31908/status" "31908/tasks"
   "32672" "32672/status" "32672/tasks" "32674" "32674/status" "32674/tasks"
   "32675" "32675/status" "32675/tasks" "4" "4/status" "4/tasks"
   "5" "5/status" "5/tasks" "870" "870/status" "870/tasks")
  (glob "**/")
  ("1/" "103/" "1068/" "1235/" "1236/" "15812/" "16/" "1620/" "1624/"
   "16248/" "16249/" "1645/" "16598/" "1665/" "1698/" "17/" "175/"
   "1766/" "1790/" "1791/" "1821/" "1839/" "1851/" "186/" "18614/"
   "1887/" "1902/" "1921/" "1925/" "1926/" "1927/" "1928/" "1929/"
   "1930/" "1931/" "1932/" "1936/" "1963/" "1989/" "2/" "2008/"
   "2027/" "2041/" "2052/" "2062/" "2124/" "2184/" "2354/" "24134/"
   "2551/" "2579/" "2625/" "2626/" "2631/" "2634/" "2636/" "2638/"
   "2644/" "2661/" "2685/" "2689/" "2691/" "2693/" "2695/" "2698/"
   "2701/" "2707/" "27121/" "2717/" "2718/" "2720/" "2722/" "27243/"
   "2726/" "2728/" "27682/" "27684/" "27685/" "28/" "29/" "29840/"
   "3/" "30737/" "31905/" "31907/" "31908/" "32672/" "32674/" "32675/"
   "4/" "5/" "870/"))

(chdir "../..")

(mtest
  (glob* "**/3*/**")
  ("002/proc/3/" "002/proc/3/status" "002/proc/3/tasks" "002/proc/30737/"
   "002/proc/30737/status" "002/proc/30737/tasks" "002/proc/31905/"
   "002/proc/31905/status" "002/proc/31905/tasks" "002/proc/31907/"
   "002/proc/31907/status" "002/proc/31907/tasks" "002/proc/31908/"
   "002/proc/31908/status" "002/proc/31908/tasks" "002/proc/32672/"
   "002/proc/32672/status" "002/proc/32672/tasks" "002/proc/32674/"
   "002/proc/32674/status" "002/proc/32674/tasks" "002/proc/32675/"
   "002/proc/32675/status" "002/proc/32675/tasks")
  (glob* "**/{3,4,5}*/**")
  ("002/proc/3/" "002/proc/3/status" "002/proc/3/tasks" "002/proc/30737/"
   "002/proc/30737/status" "002/proc/30737/tasks" "002/proc/31905/"
   "002/proc/31905/status" "002/proc/31905/tasks" "002/proc/31907/"
   "002/proc/31907/status" "002/proc/31907/tasks" "002/proc/31908/"
   "002/proc/31908/status" "002/proc/31908/tasks" "002/proc/32672/"
   "002/proc/32672/status" "002/proc/32672/tasks" "002/proc/32674/"
   "002/proc/32674/status" "002/proc/32674/tasks" "002/proc/32675/"
   "002/proc/32675/status" "002/proc/32675/tasks" "002/proc/4/"
   "002/proc/4/status" "002/proc/4/tasks" "002/proc/5/" "002/proc/5/status"
   "002/proc/5/tasks")
  (glob* "**/{3,4,5}*/**" glob-xnobrace)
  nil
  (len (glob* "**/proc/**/**"))
  366)

(if (neq (os-symbol) :solaris)
  (test
    (glob* "002/proc\\/**")
    ("002/proc/1" "002/proc/103" "002/proc/1068" "002/proc/1235" "002/proc/1236"
     "002/proc/15812" "002/proc/16" "002/proc/1620" "002/proc/1624"
     "002/proc/16248" "002/proc/16249" "002/proc/1645" "002/proc/16598"
     "002/proc/1665" "002/proc/1698" "002/proc/17" "002/proc/175"
     "002/proc/1766" "002/proc/1790" "002/proc/1791" "002/proc/1821"
     "002/proc/1839" "002/proc/1851" "002/proc/186" "002/proc/18614"
     "002/proc/1887" "002/proc/1902" "002/proc/1921" "002/proc/1925"
     "002/proc/1926" "002/proc/1927" "002/proc/1928" "002/proc/1929"
     "002/proc/1930" "002/proc/1931" "002/proc/1932" "002/proc/1936"
     "002/proc/1963" "002/proc/1989" "002/proc/2" "002/proc/2008"
     "002/proc/2027" "002/proc/2041" "002/proc/2052" "002/proc/2062"
     "002/proc/2124" "002/proc/2184" "002/proc/2354" "002/proc/24134"
     "002/proc/2551" "002/proc/2579" "002/proc/2625" "002/proc/2626"
     "002/proc/2631" "002/proc/2634" "002/proc/2636" "002/proc/2638"
     "002/proc/2644" "002/proc/2661" "002/proc/2685" "002/proc/2689"
     "002/proc/2691" "002/proc/2693" "002/proc/2695" "002/proc/2698"
     "002/proc/2701" "002/proc/2707" "002/proc/27121" "002/proc/2717"
     "002/proc/2718" "002/proc/2720" "002/proc/2722" "002/proc/27243"
     "002/proc/2726" "002/proc/2728" "002/proc/27682" "002/proc/27684"
     "002/proc/27685" "002/proc/28" "002/proc/29" "002/proc/29840"
     "002/proc/3" "002/proc/30737" "002/proc/31905" "002/proc/31907"
     "002/proc/31908" "002/proc/32672" "002/proc/32674" "002/proc/32675"
     "002/proc/4" "002/proc/5" "002/proc/870")))
