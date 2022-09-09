(load "../common")

(mstest
  '[ . a] "[. a]"
  '[. a] "[. a]"
  '[ . @a] "[. @a]"
  '[. @a] "[. @a]")
