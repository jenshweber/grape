(use 'grape.core)


; create one node



(def create-person (rule {
                          :create (pattern
                                   (node 'n {:label 'Person :asserts {:name "Jens"}}))
                          }))

(apply-rule create-person)


(def create-rel (rule {
                       :read (pattern
                              (node 'f {:label 'Person :asserts {:name "Flo"}})
                              (node 'j {:label 'Person :asserts {:name "Jens"}}))
                       :create (pattern
                                (edge 'e {:label 'dad_of :src 'j :tar 'f} )
                                )
                       }))

(apply-rule create-rel)



(def create-works-for (rule {
                          :read (pattern
                                 (node 'f {:label 'Person :asserts {:name "Flo"}})
                                 (node 'j {:label 'Person :asserts {:name "Jens"}}))
                          :create (pattern
                                   (node 'a {:label 'Person :asserts {:name "Anke"}})
                                   (edge 'e1 {:label 'works_for :src 'f :tar 'a} )
                                   (edge 'e2 {:label 'works_for :src 'j :tar 'j} )
                                   )
                       }))

(apply-rule create-works-for)


(def rewrite-employers (rule  {
                              :read (pattern :homo
                                     (node 'n1)
                                     (node 'n2)
                                     (edge 'e {:label 'works_for :src 'n1 :tar 'n2}))
                              :delete ['e]
                              :create (pattern
                                       (node 'n3 {:label 'Role :asserts {:name 'work}})
                                       (edge 'e1 {:label 'offers :src 'n3 :tar 'n2})
                                       (edge 'e2 {:label 'takes :src 'n3 :tar 'n1}))
                              }))

(apply-rule rewrite-employers)



