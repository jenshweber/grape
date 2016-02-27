(ns grape.core-test
  (:require [clojure.test :refer :all]
            [clojurewerkz.neocons.rest.cypher :as cy]
            [grape.core :refer :all]))


;--------------------------
; testing DSL forms
;--------------------------

(deftest test-node
  (testing "Node form failed (1)"
    (is (= (node 'n1 (labels :label1 :label2) (asserts {:key1 "val1" :key2 "val2"}))
           ['node {:id 'n1, :labels '(:label1 :label2), :ass {:key1 "val1" :key2 "val2"}}]
           )))
  (testing "Node form failed (2)"
    (is (= (node 'n1 (labels :label1 :label2))
           ['node {:id 'n1, :labels '(:label1 :label2), :ass '()}]
           )))
  (testing "Node form failed (3)"
     (is (= (node 'n1)
           ['node {:id 'n1, :labels '(), :ass '()}]
           )))
    )

(deftest test-edge
  (testing "Edge form failed (1)"
    (is (= (edge 'e1 (labels :label1 :label2) 's 't (asserts {:key1 "val1" :key2 "val2"}) )
           ['edge {:id 'e1, :labels '(:label1 :label2), :src 's, :tar 't, :ass {:key1 "val1" :key2 "val2"}}]
           )))
  (testing "Edge form failed (2)"
    (is (= (edge 'e1 (labels :label1 :label2) 's 't )
           ['edge {:id 'e1, :labels '(:label1 :label2), :src 's, :tar 't, :ass '()}]
           )))
  (testing "Edge form failed (3)"
    (is (= (edge 'e1 's 't )
           ['edge {:id 'e1, :labels '(), :src 's, :tar 't, :ass '()}]
           )))
  )



 (deftest test-assertions
  (testing "Assertion form failed (1)"
    (is (= (asserts {:key1 "val1" :key2 "val2"})
           {:key1 "val1" :key2 "val2"}
           )))
  )


 (deftest test-labels
  (testing "Labels form failed (1)"
    (is (= (labels :label1 :label2)
           '(:label1 :label2)
           )))
  )


;-------------------------- 
; testing translation to Cypher
;--------------------------

  (deftest test-asss->cypher
    (testing "Translation of assertion to Cypher failed (1)"
      (is (= (asss->cypher (asserts {:key1 "val1" :key2 "val2"}))
              " {key1:\"val1\" key2:\"val2\"}"
              )))
        (testing "Translation of assertion to Cypher failed (2)"
      (is (= (asss->cypher '())
              ""
              )))
  )

   (deftest test-labels->cypher
    (testing "Translation of labels to Cypher failed (1)"
      (is (= (labels->cypher (labels :label1 :label2))
              ":label1:label2"
              )))
     (testing "Translation of labels to Cypher failed (2)"
      (is (= (labels->cypher (labels :label1))
              ":label1"
              )))
     (testing "Translation of labels to Cypher failed (3)"
      (is (= (labels->cypher '())
              ""
              )))
  )

   (deftest test-node->cypher
    (testing "Translation of nodes to Cypher failed"
      (is (= (node->cypher (node 'n1 (labels :label1 :label2) (asserts {:key1 "val1" :key2 "val2"})))
              "MATCH (n1:label1:label2 {key1:\"val1\" key2:\"val2\"})"
              )))
  )


 (deftest test-edge->cypher
    (testing "Translation of edges to Cypher failed"
      (is (= (edge->cypher (edge 'e1 (labels :label1 :label2) 's 't (asserts {:key1 "val1" :key2 "val2"}) ))
              "MATCH (s)-[e1:label1:label2 {key1:\"val1\" key2:\"val2\"}]->(t)"
              )))
  )



  (deftest test-pattern->cypher
    (testing "Translation of graph to Cypher failed (1)"
      (is (= (pattern->cypher
              (pattern :homo
               (node 'n1 (labels :Table) )
               (node 'n3 (labels :Attribute) )
               (edge 'e2 (labels :col) 'n1 'n3)))
              "MATCH (n1:Table) MATCH (n3:Attribute) MATCH (n1)-[e2:col]->(n3) RETURN n1, n3, e2"
              )))
    (testing "Translation of graph to Cypher failed (2)"
      (is (= (pattern->cypher
              (pattern
               (node 'n1 (labels :Table) )
               (node 'n3 (labels :Attribute) )
               (edge 'e2 (labels :col) 'n1 'n3)
               (edge 'e3 (labels :mock) 'n1 'n3)))
              "MATCH (n1:Table) MATCH (n3:Attribute) MATCH (n1)-[e2:col]->(n3) MATCH (n1)-[e3:mock]->(n3) WHERE ID(n1)<>ID(n3) AND ID(e2)<>ID(e3) RETURN n1, n3, e2, e3"
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
          (first (match (pattern
                         (node 'n1 (labels :_testlabel1)))))
          "n1"))
     (is (empty? (match (pattern
                         (node 'n1 (labels :_testlabel100))))))
     (is (empty? (match (pattern
                         (node 'n1 (labels :_testlabel1))
                         (node 'n2 (labels :_testlabel1))
                         ))))
     (is (not (empty? (match (pattern :homo
                         (node 'n1 (labels :_testlabel1))
                         (node 'n2 (labels :_testlabel1))
                         )))))
     )
   ;tear down
   (cy/tquery conn "MATCH (n1:_testlabel1) MATCH (n2:_testlabel2) detach delete n1,n2")
 )


 (pattern->cypher (pattern
                         (node 'n1 (labels :_testlabel1))
                         (node 'n2 (labels :_testlabel1))))

;   (cy/tquery conn "create (n1:_testlabel1)-[e1:_edgelabel]->(n3:_testlabel2)")

  ;(match (pattern :iso
   ;        (node 'n1 '("_testlabel1"))))


