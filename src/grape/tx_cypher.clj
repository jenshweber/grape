(ns grape.tx-cypher
  (:require
            [clojure.math.combinatorics :as combo]
            [clojure.data.json :as json]
            [schema.core :as s]
            [clojure.string :as str]
            [clojure.set :refer :all]
            [grape.util :refer :all]
            ))



(def opt-atom (atom true))

(defn first-optional? []
  (let [r (deref opt-atom)]
    (swap! opt-atom (fn [_] false))
    r))



(defn path->cypher [e]
  (let [c (second e)
        k  (if (:opt c) (if (first-optional?)
                          " WITH * OPTIONAL MATCH"
                          " , "
                          )
                        " WITH * MATCH") ]
    (str k " (" (:src c) ")"
         (str/join "()" (map (fn [s]
                          (let [dir (subs s 0 1)
                                seg (subs s 1 (count s))]
                            (cond (= dir ">") (str "-[" seg "]->")
                                  (= dir "<") (str "<-[" seg "]-")
                                  :else (str "-[" seg "]-"))))
                          (str/split (:exp c) #" ")))
         "(" (:tar c) ") ")))



(defn filter-elem [k c]
  "filter out specified graph element type k (node or edge)"
  (filter (fn [x] (= k (first x))) c))








