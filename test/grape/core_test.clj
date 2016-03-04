(ns grape.core-test
  (:require [clojure.test :refer :all]
            [clojurewerkz.neocons.rest.cypher :as cy]
            [grape.core :refer :all]
            [clojure.string :as str]))


;--------------------------
; testing DSL forms
;--------------------------

(deftest test-node
  (testing "Node form failed (1)"
    (is (= (node 'n1 {:label 'label1 :asserts {:key1 "val1" :key2 "val2"}})
           ['node {:id 'n1 :label 'label1 :asserts {:key1 "val1" :key2 "val2"}}]
           )))
  (testing "Node form failed (2)"
    (is (= (node 'n1)
           ['node {:id 'n1}]
           )))
  )

(deftest test-edge
  (testing "Edge form failed (1)"
    (is (= (edge 'e1 {:label 'label1 :src 's :tar 't :asserts {:key1 "val1" :key2 "val2"}})
           ['edge {:id 'e1 :label 'label1 :src 's :tar 't :asserts {:key1 "val1" :key2 "val2"}}]
           )))
  (testing "Edge form failed (2)"
    (is (= (edge 'e1 {:src 's :tar 't} )
           ['edge {:id 'e1 :src 's :tar 't}]
           )))
  )



;-------------------------- 
; testing translation to Cypher
;--------------------------


(deftest test-asss->cypher
  (testing "Translation of assertion to Cypher failed (1)"
    (is (= (asserts->cypher {:key1 "val1" :key2 "val2"})
           " {key1:\"val1\" key2:\"val2\"}"
           )))
  (testing "Translation of assertion to Cypher failed (2)"
    (is (= (asserts->cypher {})
           ""
           )))
  )




   (deftest test-node->cypher
    (testing "Translation of nodes to Cypher failed"
      (is (= (node->cypher :match (node 'n1 {:label 'label1 :asserts {:key1 "val1" :key2 "val2"}}))
              " MATCH (n1:label1 {key1:\"val1\", key2:\"val2\"})"
              ))
      (is (= (node->cypher :create (node 'n1 {:label 'label1 :asserts {:key1 "val1" :key2 "val2"}})))
              " CREATE (n1:label1 {key1:\"val1\", key2:\"val2\"})"
              ))
      )




 (deftest test-edge->cypher
    (testing "Translation of edges to Cypher failed"
      (is (= (edge->cypher :match (edge 'e1 {:label 'label1 :src 's :tar 't :asserts {:key1 "val1" :key2 "val2"}}) ))
              " MATCH (s)-[e1:label1 {key1:\"val1\", key2:\"val2\"}]->(t)"
              ))
      (is (= (edge->cypher :create (edge 'e1 {:label 'label1 :src 's :tar 't :asserts {:key1 "val1" :key2 "val2"}}) ))
              " CREATE (s)-[e1:label {key1:\"val1\", key2:\"val2\"}]->(t)"
              ))




  (deftest test-pattern->cypher
    (testing "Translation of graph to Cypher failed (1)"
      (is (= (pattern->cypher :match
              (pattern :homo
               (node 'n1 {:label 'Table})
               (node 'n3 {:label 'Attribute})
               (edge 'e2 {:label 'col :src 'n1 :tar 'n3})))
              " MATCH (n1:Table)  MATCH (n3:Attribute)  MATCH (n1)-[e2:col]->(n3) RETURN n1, n3, e2"
              )))
    (testing "Translation of graph to Cypher failed (2)"
      (is (= (pattern->cypher :match
              (pattern
               (node 'n1 {:label 'Table} )
               (node 'n3 {:label 'Attribute} )
               (edge 'e2 {:label 'col :src 'n1 :tar 'n3})
               (edge 'e3 {:label 'mock :src 'n1 :tar 'n3})))
              " MATCH (n1:Table)  MATCH (n3:Attribute)  MATCH (n1)-[e2:col]->(n3)  MATCH (n1)-[e3:mock]->(n3) WHERE ID(n1)<>ID(n3) AND ID(e2)<>ID(e3) RETURN n1, n3, e2, e3"
              ))))



  (deftest test-rule->cypher
    (testing "Translation of rule to Cypher failed (1)"
      (is (= (rule->cypher (rule {
                                  :create (pattern
                                           (node 'n {:label 'label}))}))
              " CREATE (n:label)"
              )))
  )



 ;------------------------------
 ; testing neo4j execution engine
 ;-------------------------------

 (deftest test-match
   ;setup
   (cy/tquery conn "create (n1:_testlabel1)-[e1:_edgelabel]->(n3:_testlabel2)")
   (testing "Execution engine neo4j - match"
     (is (contains?
          (:nodes (match (pattern
                         (node 'n1 {:label '_testlabel1}))))
          "n1"))
     (is (empty? (match (pattern
                         (node 'n1 {:label '_testlabel100})))))
     (is (empty? (match (pattern
                         (node 'n1 {:label '_testlabel1})
                         (node 'n2 {:label '_testlabel1})
                         ))))
     (is (not (empty? (match (pattern :homo
                         (node 'n1 {:label '_testlabel1})
                         (node 'n2 {:labels '_testlabel1})
                         )))))
     )
   ;tear down
   (cy/tquery conn "match (n) detach delete n")
 )




(deftest test-apply-rule
   ;setup
   (cy/tquery conn "match (n) detach delete n")
   (testing "Execution engine - apply-rule failed"
     (is (not (nil?
          (apply-rule (rule {
                    :create (pattern
                             (node 'n {:label 'testlabel
                                       :asserts {:key "value" :key2 "value2"}}))})))))
   ;tear down
   (cy/tquery conn "match (n) detach delete n")
     ))

