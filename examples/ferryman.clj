(use 'grape.core)

(rule 'setup-ferryman!
  :create
  (pattern
    (node 'tg :label "Thing" :asserts {:kind "'Goat'"})
    (node 'tc :label "Thing" :asserts {:kind "'Grape'"})
    (node 'tw :label "Thing" :asserts {:kind "'Wolf'"})
    (node 's1 :label "Side" :asserts {:name "'This side'"})
    (node 's2 :label "Side" :asserts {:name "'Other side'"})
    (node 'f  :label "Ferry" :asserts {:name "'Ferryman'" :coins "7"})
    (edge :label "is_at" :src 'tg :tar 's1)
    (edge :label "is_at" :src 'tc :tar 's1)
    (edge :label "is_at" :src 'tw :tar 's1)
    (edge :label "is_at" :src 'f :tar 's1)
    ))

(rule 'ferry_one_over!
  :read
  (pattern
    (node 's1 :label "Side")
    (node 's2 :label "Side")
    (node 'f :label "Ferry")
    (node 't :label "Thing")
    (edge 'et :label "is_at" :src 't :tar 's1)
    (edge 'e :label "is_at" :src 'f :tar 's1)
    (condition "f.coins > 0"))
  :delete ['e 'et]
  :create
  (pattern
    (edge :label "is_at" :src 'f :tar 's2)
    (edge :label "is_at" :src 't :tar 's2)
    (assign "f.coins=f.coins-1")))


(rule 'cross_empty!
  :read
  (pattern
    (node 's1 :label "Side")
    (node 's2 :label "Side")
    (node 'f :label "Ferry")
    (edge 'e :label "is_at" :src 'f :tar 's1)
    (condition "f.coins > 0"))
  :delete ['e ]
  :create
  (pattern
    (edge :label "is_at" :src 'f :tar 's2)
    (assign "f.coins=f.coins-1")))


(rule 'wolf-can-eat-goat?
  :read
  (pattern :homo
           (node 't1 :label "Thing" :asserts {:kind "'Wolf'"})
           (node 't2 :label "Thing" :asserts {:kind "'Goat'"})
           (node 's :label "Side")
           (edge :label "is_at" :src 't1 :tar 's)
           (edge :label "is_at" :src 't2 :tar 's)
           (NAC
             (node 'f :label "Ferry")
             (edge :label "is_at" :src 'f :tar 's))))


(rule 'goat-can-eat-grape?
  :read
  (pattern :homo
           (node 't1 :label "Thing" :asserts {:kind "'Goat'"})
           (node 't2 :label "Thing" :asserts {:kind "'Grape'"})
           (node 's :label "Side")
           (edge :label "is_at" :src 't1 :tar 's)
           (edge :label "is_at" :src 't2 :tar 's)
           (NAC
             (node 'f :label "Ferry")
             (edge :label "is_at" :src 'f :tar 's))))

(rule 'all_on_the_other_side?
  :read
  (pattern :homo
           (node 'tg :label "Thing" :asserts {:kind "'Goat'"})
           (node 'tc :label "Thing" :asserts {:kind "'Grape'"})
           (node 'tw :label "Thing" :asserts {:kind "'Wolf'"})
           (node 's2 :label "Side" :asserts {:name "'Other side'"})
           (edge :label "is_at" :src 'tg :tar 's2)
           (edge :label "is_at" :src 'tc :tar 's2)
           (edge :label "is_at" :src 'tw :tar 's2)))



(defn solve_problem! []
  (until 'all_on_the_other_side?
         (transact (choice (apl 'ferry_one_over!)
                           (apl 'cross_empty!))
                   (avoid (apl 'wolf-can-eat-goat?)
                          (apl 'goat-can-eat-grape?)))))


(clear!)



(setup-ferryman!)



(attempt (solve_problem!))
