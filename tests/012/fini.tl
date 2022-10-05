(load "../common")

(defstruct base nil
  others
  id
  (:static counter 0)
  (:postinit (me) (set me.id (inc me.counter)))
  (:fini (me)
    (put-line `@(typeof me):@{me.id} finalized`)
    [mapdo call-finalizers me.others]))

(defstruct derived base
  (:fini (me)
    (put-line `@(typeof me):@{me.id} derived fini`))
  (:postfini (me)
    (put-line `@(typeof me):@{me.id} derived postfini`)))

(unwind-protect
  (with-objects ((b (new base others (mapcar (ret (new derived)) (range 1 20)))))
    (put-line "inside with-objects"))
  (put-line "after with-objects"))

(mapcar (ret (new derived)) (range 1 20))
(sys:gc)

(defstruct multi ()
  (:init (me)
    (put-line `@{%fun%}: 1`))
  (:init (me)
    (put-line `@{%fun%}: 2`))
  (:postinit (me)
    (put-line `@{%fun%}: 1`))
  (:postinit (me)
    (put-line `@{%fun%}: 2`))
  (:fini (me)
    (put-line `@{%fun%}: 1`))
  (:fini (me)
    (put-line `@{%fun%}: 2`))
  (:postfini (me)
    (put-line `@{%fun%}: 1`))
  (:postfini (me)
    (put-line `@{%fun%}: 2`)))

(with-objects ((m (new multi))))
