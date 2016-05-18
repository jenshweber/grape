(use 'grape.core)

(gts 'example)

(clear!)


; until control structure

(rule 'setup-likes!
      {:create
       (pattern
        (node 'n1)
        (node 'n2 )
        (node 'n3 )
        (edge 'e1 {:label "likes" :src 'n1 :tar 'n2})
        (edge 'e2 {:label "likes" :src 'n1 :tar 'n3})
        (edge 'e3 {:label "likes" :src 'n2 :tar 'n1})
        (edge 'e4 {:label "likes" :src 'n2 :tar 'n3})
        (edge 'e5 {:label "likes" :src 'n3 :tar 'n2})
        (edge 'e6 {:label "likes" :src 'n3 :tar 'n1}))})

(setup-likes!)

(rule 'dislike_one!
      {:read (pattern
              (node 'n1)
              (node 'n2)
              (edge 'e {:label "likes" :src 'n1 :tar 'n2}))
       :delete ['e]})

(rule 'chain_of_likes?
      {:read (pattern
              (node 'n1)
              (node 'n2 )
              (node 'n3 )
              (edge 'e1 {:label "likes" :src 'n1 :tar 'n2})
              (edge 'e2 {:label "likes" :src 'n2 :tar 'n3})
              (edge 'e3 {:label "likes" :src 'n3 :tar 'n1})
              (NAC 1 :homo
                   (node 'n5)
                   (node 'n6)
                   (edge 'e5 {:label "likes" :src 'n5 :tar 'n6})
                   (edge 'e6 {:label "likes" :src 'n6 :tar 'n5}))
               )})


(attempt (until 'chain_of_likes? (apl 'dislike_one!)))




(rule 'setup3!
      {:create
       (pattern
        (node 'n1 {:label "Kim"})
        (node 'n2 {:label "John"})
        (edge 'e1 {:label "likes" :src 'n1 :tar 'n2}))})

(rule 'KimLikesJohn!
      {:read
       (pattern
        (node 'n1 {:label "Kim"})
        (node 'n2 {:label "John"}))
       :create
       (pattern
        (edge 'e1 {:label "likes" :src 'n1 :tar 'n2}))})

(rule 'JohnLikesKim!
      {:read
       (pattern
        (node 'n1 {:label "Kim"})
        (node 'n2 {:label "John"}))
       :create
       (pattern
        (edge 'e1 {:label "likes" :src 'n2 :tar 'n1}))})

(rule 'likeEachOther?
      {:read
       (pattern
        (node 'n1 {:label "Kim"})
        (node 'n2 {:label "John"})
        (edge 'e1 {:label "likes" :src 'n2 :tar 'n1})
        (edge 'e2 {:label "likes" :src 'n1 :tar 'n2}))})
(clear!)
(setup3!)


(attempt
  (choice (apl 'KimLikesJohn!)
          (apl 'JohnLikesKim!))
  (apl 'likeEachOther?))




(rule 'setup4!
      {:create
       (pattern
        (node 'n1)
        (node 'n2 )
        (node 'n3 )
        (node 'n4 {:label "A"} )
        (node 'n5 )
        (edge 'e1 {:label "relates" :src 'n1 :tar 'n4})
        (edge 'e2 {:label "relates" :src 'n2 :tar 'n4})
        (edge 'e3 {:label "relates" :src 'n3 :tar 'n4}))})

(rule 'relate-one!
      {:read
       (pattern
        (node 'n1)
        (node 'n4 {:label "A"}))
       :create
       (pattern
        (edge 'e1 {:label "relates" :src 'n1 :tar 'n4}))
       })

(rule 'double?
      {:read
       (pattern
        (node 'n1 )
        (node 'n4 {:label "A"} )
        (edge 'e1 {:label "relates" :src 'n1 :tar 'n4})
        (edge 'e2 {:label "relates" :src 'n1 :tar 'n4})
        )})

(clear!)
(setup4!)


(attempt (transact (apl 'relate-one!) (avoid (apl 'double?))))
