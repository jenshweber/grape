;; We have to load the grape.core to be able to declare rules. 

(use 'grape.core)

;; Declar rules as normal.

(rule 'testRule!
      {:create
       (pattern
         (node 'tg {:label "Thing" :asserts {:kind "'Goat'"}})
         (node 'tc {:label "Thing" :asserts {:kind "'Grape'"}})
         (node 'tw {:label "Thing" :asserts {:kind "'Wolf'"}})
         (node 's1 {:label "Side" :asserts {:name "'This side'"}})
         (node 's2 {:label "Side" :asserts {:name "'Other side'"}})
         (node 'f  {:label "Ferry" :asserts {:name "'Ferryman'" :coins "7"}})
         (edge 'e1 {:label "is_at" :src 'tg :tar 's1})
         (edge 'e2 {:label "is_at" :src 'tc :tar 's1})
         (edge 'e3 {:label "is_at" :src 'tw :tar 's1})
         (edge 'e4 {:label "is_at" :src 'f :tar 's1})
 )})
