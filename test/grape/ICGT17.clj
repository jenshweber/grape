(use 'grape.core)

(gts 'example)

(rule 'create-person! ['p]
      {:create
       (pattern
        (node 'n {:label "Person" :asserts {:name "'&p'"}}))})



(create-person! "Flo")
(create-person! "Jens")

; add :homo key for homomorhic matching

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



(while (sole_employer!))


(clear!)


(rule 'setup-job-market!
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


(defn hire_director! [employer]
    (transact
     (apl 'hire! employer)
     (apl 'promote! employer)))

(setup-job-market!)


(attempt (hire_director! "Jens"))

(clear!)


(rule 'setup-job-market!
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

(rule 'hire-someone! ['name]
      {:read (pattern
               (node 'm {:label "Employer" :asserts {:name "'&name'"}})
               (node 'w {:label "Worker"})
               (NAC
                 (edge 'e {:label "works_for" :src 'w :tar 'm})))
       :create (pattern
                (edge 'e {:label "works_for" :src 'w :tar 'm}))})

(rule 'train! ['name 'w]
      {:read (pattern
              (node 'm {:label "Employer" :asserts {:name "'&name'"}})
              (node 'w {:label "Worker"}))
       :create (pattern
                (edge 'f {:label "in_training" :src 'm :tar 'w})
                )})


(defn hire_and_train! [employer]
    (transact
      (apl 'hire-someone! employer)
      (bind 'new-hire 'w)
      (apl 'train! employer (consult 'new-hire))))

(setup-job-market!)


(attempt (hire_and_train! "Jens"))


(clear!)





