(use 'grape.core)

(gts 'example)



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
              (node 'w {:label "Worker"}))
       :create (pattern
                (edge 'e {:label "works_for" :src 'w :tar 'm}))})
(use 'grape.visualizer)
(document-rule 'train!)

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


