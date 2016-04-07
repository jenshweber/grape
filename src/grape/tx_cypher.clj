(ns grape.tx-cypher
  (:require
            [clojure.math.combinatorics :as combo]
            [clojure.data.json :as json]
            [schema.core :as s]
            [clojure.string :as str]
            [clojure.set :refer :all]
            [grape.util :refer :all]
            ))

(defn graphlabel []
  (str ":_" (name (:_graph_name (eval 'gragra)))))

(defn resolve-const-or-par-ass [scope x]
  (if (symbol? x)
    (if (contains? scope x)
      (str "\"" (scope x) "\"")
      x)
    (str "\"" x "\"")))

(defn resolve-const-or-par-lab [scope x]
  (if (symbol? x)
    (if (contains? scope x)
      (scope x)
      x)
    x ))


(defn asserts->cypher [s as]
  "Translate a map of assertions to a Cypher code fragment"
  (if (empty? as)
    ""
    (str " {" (reduce (partial str-sep ", ") (map (fn [[k v]] (str (name k) ":" (resolve-const-or-par-ass s v) )) as)) "}")
    ))



(defn node->cypher [s m n]
  (let [c (second n)
        k (if (= m :match) " MATCH" " CREATE")]
    (str k " (" (:id c)
         (let [l (:label c)]
           (if (nil? l)
             (graphlabel)
             (str  ":" (resolve-const-or-par-lab s l) (graphlabel))))
         (asserts->cypher s (:asserts c))
         ")"
         )))

(defn edge->cypher [s m e]
  (let [c (second e)
        k (if (= m :match) " MATCH" " CREATE")]
    (str k " (" (:src c) ")-[" (:id c)
         (let [l (:label c)]
           (if (nil? l)
             (graphlabel)
             (str ":" (resolve-const-or-par-lab s l))))
         (asserts->cypher s (:ass c))
         "]->(" (:tar c) ")")))

(defn graphelem->cypher [s m e]
  "Translate a graph element to cipher - either node or edge"
  (let [t (first e)]
    (cond
     (= 'node t) (node->cypher s m e)
     (= 'edge t) (edge->cypher s m e)
     (= 'NAC t) ""
     :else
     (throw (Exception. "Invalid graph element"))
     )))

(defn get-id [e]
  (:id (second e)))


(defn gen-constraint-isomorphism
  [nids eids]
  (if (and (<= (count eids) 1) (<= (count nids) 1))
    ""
    (let [nc (combo/combinations nids 2)
          ec (combo/combinations eids 2)
          f (fn [x] (str "ID("(first x) ")<>ID(" (second x) ")"))
          ni (map f nc)
          ei (map f ec)
          ]
      (str " WHERE "
           (reduce (partial str-sep " AND ") (concat ni ei))
           )))
  )

(defn filter-elem [k c]
  "filter out specified graph element type k (node or edge)"
  (filter (fn [x] (= k (first x))) c))


(defn pattern->cypher [scope m p]
  "translate a graph pattern to cypher matching query"
  (let [s (second p)
        els (:els s)
        sem (:sem s)
        eids (map get-id (filter-elem 'edge els))
        nids (map get-id (filter-elem 'node els ))]
    (if (nil? els)
      ""
      (str
       (reduce (partial str-sep " ") (map (partial graphelem->cypher scope m) els))
       (if (= m :match)
         (str
          (if (= :iso sem)
            (gen-constraint-isomorphism nids eids)
            ""
            )
          " ")
         ""
         )))))

(defn redex->cypher [r]
  "recall a redex by node and edge IDs"
  (let [nnames (keys (:nodes r))
        enames (keys (:edges r))
        idmap (map (fn [[k v]] [k (:id (:metadata v))]) (concat (:nodes r) (:edges r)))]
    (str
     (reduce str (map (fn [x] (str " MATCH (" x ")")) nnames))
     (reduce str (map (fn [x] (str " MATCH ()-[" x "]->()")) enames))
     " WHERE "
     (reduce (partial str-sep " AND") (map (fn [[k v]] (str " id(" k ")=" v)) idmap))
     )
    ))


