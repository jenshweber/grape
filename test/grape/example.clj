(use 'grape.core)

(rule 'hello!
  {:create (pattern
             (node 'n1 {:label "Hello"}))})

(hello!)

(rule 'hello-grape!
  {
   :read (pattern
           (node 'n1 {:label "Hello"}))
   :create (pattern
             (node 'n2 {:label "Grape"})
             (edge 'e {:label "to" :src 'n1 :tar 'n2}))})

(hello-grape!)

(rule 'hello?
  {:read  (pattern
           (node 'n1 {:label "Hello"}))})

(hello?)


(document-rule 'hello-grape!)


(rule 'find-person
  {:read
   (pattern
     (node 'n {:label "Person"}))})

(rule 'find-parent
  {:read
   (pattern
     (node 'p)
     (node 'c)
     (edge 'e {:label "parent_of" :src 'p :tar 'c } )
     )})

(document-rule 'find-person)

(rule 'create-jens!
      {:create
       (pattern
        (node 'n {:label "Person" :asserts {:name "'Jens'"}}))})



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

(defn clear! [] (while (delete-any-node!)))

(clear!)

