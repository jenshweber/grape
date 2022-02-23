(use 'grape.core)

(gts 'example)


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
     (apl 'let_one_go! employer)
     (apl 'let_one_go! employer))))

(clear!)



(fire-two! "Jens")


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


