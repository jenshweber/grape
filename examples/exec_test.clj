(ns grape.exec-test
  (:require [clojure.test :refer :all]
            [clojurewerkz.neocons.rest.cypher :as cy]
            [grape.core :refer :all]
            [clojure.string :as str]
            [grape.exec :refer :all]))


 (deftest test-match
   ;setup
   (cy/tquery conn "create (n1:_testlabel1)-[e1:_edgelabel]->(n3:_testlabel2)")
   (testing "Execution engine neo4j - match"
     (is (contains?
          (:nodes (match {} "" (pattern
                          (node 'n1 {:label "_testlabel1"}))))
          "n1"))
     (is (empty? (match {} "" (pattern
                         (node 'n1 {:label "_testlabel100"})))))
     (is (empty? (match {} ""(pattern
                         (node 'n1 {:label "_testlabel1"})
                         (node 'n2 {:label "_testlabel1"})
                         ))))
     (is (not (empty? (match  {} "" (pattern :homo
                         (node 'n1 {:label "_testlabel1"})
                         (node 'n2 {:label "_testlabel1"})
                         )))))
     )
   ;tear down
   (cy/tquery conn "match (n) detach delete n")
 )



(deftest test-apply-rule
   ;setup
  (cy/tquery conn "match (n) detach delete n")
  (gts)
  (rule 'rulename {:create (pattern
                            (node 'n {:label "testlabel"
                                      :asserts {:key "value" :key2 "value2"}}))})
  (testing "Execution engine - apply-rule failed"
    (is (true? (apply-rule 'rulename))))
   ;tear down
   (cy/tquery conn "match (n) detach delete n"))
