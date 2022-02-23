(ns grape.tx-cypher-test
  (:require [clojure.test :refer :all]
            [grape.core :refer :all]
            [clojure.string :as str]
            [grape.tx-cypher :refer :all]))


;; (deftest test-asss->cypher
;;   (testing "Translation of assertion to Cypher failed (1)"
;;     (is (= (asserts->cypher {} {:key1 "val1" :key2 "val2"})
;;            " {key1:\"val1\", key2:\"val2\"}"
;;            )))
;;   (testing "Translation of assertion to Cypher failed (2)"
;;     (is (= (asserts->cypher {} {})
;;            ""
;;            )))
;;   (testing "Translation of assertion to Cypher failed (2)"
;;     (is (= (asserts->cypher {'p "val3"} {:key1 "val1" :key2 'p})
;;            " {key1:\"val1\", key2:\"val3\"}"
;;            )))

;;   )


;; (deftest test-node->cypher
;;     (testing "Translation of nodes to Cypher failed"
;;       (is (= (node->cypher {} :match (node 'n1 {:label "label1" :asserts {:key1 "val1" :key2 "val2"}}))
;;               " MATCH (n1:label1 {key1:\"val1\", key2:\"val2\"})"))
;;       (is (= (node->cypher {} :create (node 'n1 {:label "label1" :asserts {:key1 "val1" :key2 "val2"}}))
;;               " CREATE (n1:label1 {key1:\"val1\", key2:\"val2\"})"))
;;       (is (= (node->cypher {'p "val3"} :match (node 'n1 {:label 'p :asserts {:key1 "val1" :key2 "val2"}}))
;;               " MATCH (n1:val3 {key1:\"val1\", key2:\"val2\"})"))))




;;  (deftest test-edge->cypher
;;     (testing "Translation of edges to Cypher failed"
;;       (is (= (edge->cypher {} :match (edge 'e1 {:label "label1" :src 's :tar 't :asserts {:key1 "val1" :key2 "val2"}}) ))
;;               " MATCH (s)-[e1:label1 {key1:\"val1\", key2:\"val2\"}]->(t)"
;;               ))
;;       (is (= (edge->cypher {'p "val3"} :create (edge 'e1 {:label "label1" :src 's :tar 't :asserts {:key1 "val1" :key2 "val2"}}) ))
;;               " CREATE (s)-[e1:val3 {key1:\"val1\", key2:\"val2\"}]->(t)"
;;               ))


;;   (deftest test-pattern->cypher
;;     (testing "Translation of graph to Cypher failed (1)"
;;       (is (= (pattern->cypher {} :match
;;               (pattern :homo
;;                (node 'n1 {:label "Table"})
;;                (node 'n3 {:label "Attribute"})
;;                (edge 'e2 {:label "col" :src 'n1 :tar 'n3})))
;;               " MATCH (n1:Table)  MATCH (n3:Attribute)  MATCH (n1)-[e2:col]->(n3) "
;;               )))
;;     (testing "Translation of graph to Cypher failed (2)"
;;       (is (= (pattern->cypher {} :match
;;               (pattern
;;                (node 'n1 {:label "Table"} )
;;                (node 'n3 {:label "Attribute"} )
;;                (edge 'e2 {:label "col" :src 'n1 :tar 'n3})
;;                (edge 'e3 {:label "mock" :src 'n1 :tar 'n3})))
;;               " MATCH (n1:Table)  MATCH (n3:Attribute)  MATCH (n1)-[e2:col]->(n3)  MATCH (n1)-[e3:mock]->(n3) WHERE ID(n1)<>ID(n3) AND ID(e2)<>ID(e3) "
;;               ))))

