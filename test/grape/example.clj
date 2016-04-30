(use 'grape.core)

(gts 'example)

(rule 'create-jens!
      {:create
       (pattern
        (node 'n {:label "Person" :asserts {:name "'Jens'"}}))})

(create-jens!)

(rule 'create-person! ['p]
      {:create
       (pattern
        (node 'n {:label "Person" :asserts {:name "'&p'"}}))})



(create-person! "Flo")



(rule 'parent_of! ['p 'c]
      { :read (pattern
               (node 'f {:label "Person" :asserts {:name "'&c'"}})
               (node 'j {:label "Person" :asserts {:name "'&p'"}}))
        :create (pattern
                 (edge 'e {:label "parent_of" :src 'j :tar 'f } )
                 )})


(parent_of! "Jens" "Flo")



(rule 'works_for! ['e 's]
      { :read (pattern :homo
                       (node 'f {:label "Person" :asserts {:name "'&s'"}})
                       (node 'j {:label "Person" :asserts {:name "'&e'"}}))
        :create (pattern
                 (edge 'd {:label "works_for" :src 'j :tar 'f} ))})


(works_for! "Flo" "Jens")

(works_for! "Jens" "Jens")


(rule 'rewrite_contract!
      {
         :read (pattern :homo
                       (node 'n1)
                       (node 'n2)
                       (edge 'e {:label "works_for" :src 'n1 :tar 'n2}))
        :delete ['e]
        :create (pattern
                 (node 'n3 {:label "Contract" :asserts {:name "'Contract'" :with "n1.name"}})
                 (edge 'e1 {:label "employer" :src 'n3 :tar 'n2})
                 (edge 'e2 {:label "employee" :src 'n3 :tar 'n1}))})

(rewrite_contract!)


(rule 'fire-employee! ['name]
      {
       :read (pattern
              (node 'emp {:label "Person" :asserts {:name "'&name'"}})
              (node 'con)
              (edge 'e {:label "employee" :src 'con :tar 'emp}))
       :delete ['con]})



(fire-employee! "Flo")


(rule 'works_for2! ['e 's]
      { :read (pattern
               (node 'f {:label "Person" :asserts {:name "'&s'"}})
               (node 'j {:label "Person" :asserts {:name "'&e'"}})
               (NAC 1
                (edge 'e1 {:label "works_for" :src 'j :tar 'f})
                )
              )
        :create (pattern
                 (edge 'e2 {:label "works_for" :src 'j :tar 'f} ))
        })


(works_for2! "Flo" "Jens")


(rule 'sole_employer! []
      { :read (pattern
               (node 'f )
               (node 'j )
               (edge 'e1 {:label "works_for" :src 'j :tar 'f})
               (NAC 1
                (node 'f2)
                (edge 'e2 {:label "works_for" :src 'j :tar 'f2})
                )
               (NAC 2
                (edge 'e3 {:label "sole_employer" :src 'j :tar 'f} )))

        :create (pattern
                 (edge 'e4 {:label "sole_employer" :src 'j :tar 'f} ))
        })


(sole_employer!)


(rule 'delete-any-node!
      {:read (pattern (node 'n))
       :delete ['n]})


(while (delete-any-node!))

(defn clear [] (while (delete-any-node!)))


(rule 'let_one_go! ['employer]
      {
       :read (pattern
              (node 'emp {:label "Person" :asserts {:name "'&employer'"}})
              (node 'worker)
              (edge 'e {:label "works_for" :src 'worker :tar 'emp}))
       :delete ['e]})




(rule 'setup!
      {:create
       (pattern
        (node 'emp {:label "Person" :asserts {:name "'Jens'"}})
        (node 'w1 {:label "Person"})
        (node 'w2 {:label "Person"})
        (edge 'e1 {:label "works_for" :src 'w1 :tar 'emp})
        (edge 'e2 {:label "works_for" :src 'w2 :tar 'emp})
        )})


(setup!)

(let_one_go! "Jens")

(defn fire-two!
  [employer]
  (attempt
   (transact
     ['let_one_go! employer]
     ['let_one_go! employer])))

(clear)



(fire-two! "Jens")


(rule 'setup2!
      {:create
       (pattern
        (node 'w1 {:label "Worker"})
        (node 'w2 {:label "Worker"})
        (node 'w3 {:label "Worker"})
        (node 'w4 {:label "Worker"})
        (node 'm1 {:label "Employer" :asserts {:name "'Jens'"}})
        (node 'm2 {:label "Employer" })
        (node 'm3 {:label "Employer" })
        (node 'm4 {:label "Employer" })
        (edge 'e1 {:label "works_for" :src 'w1 :tar 'm2})
        (edge 'e2 {:label "works_for" :src 'w2 :tar 'm3})
        (edge 'e3 {:label "works_for" :src 'w3 :tar 'm4})

        )})


(rule 'hire! ['name]
      {:read (pattern
              (node 'm {:label "Employer" :asserts {:name "'&name'"}})
              (node 'w {:label "Worker"})
              (NAC
               (edge 'e {:label "works_for" :src 'w :tar 'm})))
       :create (pattern
                (edge 'e {:label "works_for" :src 'w :tar 'm}))})


(rule 'promote! ['name]
      {:read (pattern
              (node 'm {:label "Employer" :asserts {:name "'&name'"}})
              (node 'w {:label "Worker"})
              (edge 'e {:label "works_for" :src 'w :tar 'm})
              (NAC
               (node 'm2)
               (edge 'en {:label "works_for" :src 'w :tar 'm2}))
              )
       :delete ['w]
       :create (pattern
                (node 'd {:label "Director"})
                (edge 'f {:label "works_for" :src 'd :tar 'm})
                )})


(def hire_director!
  (fn [employer]
    (transact
     ['hire! employer]
     ['promote! employer])))

(setup2!)


(attempt (hire_director! "Jens"))


(clear)


; until control structure

(rule 'setup2!
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
(clear)
(setup2!)

(rule 'dislike_one
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

(clear)
(setup2!)



(attempt (until 'chain_of_likes? ['dislike_one]))

(use 'grape.visualizer)
(document-rules)


(chain_of_likes?)




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
(clear)
(setup3!)


(attempt
  (choice ['KimLikesJohn!]
          ['JohnLikesKim!])
  ['likeEachOther?])




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

(clear)
(setup4!)


(attempt (transact ['relate-one!] (avoid ['double?])))
