(ns grape.core-test
  (:require [clojure.test :refer :all]
            [grape.core :refer :all]
            [clojure.string :as str]))


(deftest test-node
  (testing "Node form failed (1)"
    (is (= (node 'n1 {:label "label1" :asserts {:key1 "val1" :key2 "val2"}})
           ['node {:id 'n1 :label "label1" :asserts {:key1 "val1" :key2 "val2"}}]
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


(deftest test-static-analysis
  (gts 'example)
  (is (thrown? Exception
               (rule 'rule1
                     {:create
                      (pattern
                       (node 'n {:label 'k :asserts {:name "Jens"}}))})))
  (is (not (nil? (rule 'rule1 ['k]
                       {:create
                        (pattern
                         (node 'n {:label 'k :asserts {:name "Jens"}}))}))))
  (is (thrown? Exception
               (rule 'rule1 ['k]
                     {:create
                      (pattern
                       (node 'k {:label "label" :asserts {:name "Jens"}}))})))
   (is (thrown? Exception
               (rule 'rule1 ['k]
                     {:read
                      (pattern
                       (node 'k {:label "label" :asserts {:name "Jens"}}))})))
   (is (thrown? Exception
               (rule 'rule1
                     {:read
                      (pattern
                       (node 'k {:label "label" :asserts {:name "Jens"}}))
                      :create
                      (pattern
                       (node 'k {:label "label" :asserts {:name "Jens"}}))})))
  (is (thrown? Exception
                 (rule 'delete-any-node!
                       {:read (pattern (node 'k))
                        :delete ['n]})))
  (is (thrown? Exception
               (rule 'rule1
                     {:read
                      (pattern
                       (node 'k {:label "label" :asserts {:name 'n.name}}))
                      :create
                      (pattern
                       (node 'n {:label "label" :asserts {:name "Jens"}}))})))
  (is (thrown? Exception
               (rule 'rule1
                     {:read
                      (pattern
                       (node 'k {:label "label" :asserts {:name 'n.name}}))})))
  (is (thrown? Exception
               (rule 'rule1
                     {:create
                      (pattern
                       (node 'n {:label "label" :asserts {:name 'k.name}}))})))
  )
