(use 'grape.core)

(rule 'create-jens!
      {:create
       (pattern
        (node 'n {:label "Person" :asserts {:name "Jens"}}))})


(create-jens!)

(rule 'create-person! ['p]
      {:create
       (pattern
        (node 'n {:label "Person" :asserts {:name 'p}}))})



(create-person! {'p "Flo"})



(rule 'parent_of! ['p 'c]
      { :read (pattern
               (node 'f {:label "Person" :asserts {:name 'c}})
               (node 'j {:label "Person" :asserts {:name 'p}}))
        :create (pattern
                 (edge 'e {:label "parent_of" :src 'j :tar 'f } )
                 )})


(parent_of! {'p "Jens" 'c "Flo"})



(rule 'works_for! ['e 's]
      { :read (pattern
                (node 'f {:label "Person" :asserts {:name 's}})
                (node 'j {:label "Person" :asserts {:name 'e}}))
        :create (pattern
                 (edge 'e {:label "works_for" :src 'j :tar 'f} ))})

(works_for! {'e "Flo" 's "Jens"})


(works_for! {'e "Jens" 's "Jens"})




(rule 'rewrite_contract!
      { :read (pattern
               (node 'n1)
               (node 'n2)
               (edge 'e {:label "works_for" :src 'n1 :tar 'n2}))
        :delete ['e]
        :create (pattern
                 (node 'n3 {:label "Contract" :asserts {:name "Contract" :with 'n1.name}})
                 (edge 'e1 {:label "employer" :src 'n3 :tar 'n2})
                 (edge 'e2 {:label "employee" :src 'n3 :tar 'n1}))})


(rewrite_contract!)


(rule 'fire-employee! ['name]
      {
       :read (pattern
              (node 'emp {:label "Person" :asserts {:name 'name}})
              (node 'con)
              (edge 'e {:label "employee" :src 'con :tar 'emp}))
       :delete ['con]})

(fire-employee! {'name "Flo"})

(rule 'delete-any-node!
      {:read (pattern (node 'n))
       :delete ['n]})


(while (delete-any-node!))

(delete-any-node)
