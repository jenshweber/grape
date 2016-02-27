(ns grape.core
  (:require [clojurewerkz.neocons.rest :as nr]
            [clojurewerkz.neocons.rest.cypher :as cy]
            [clojure.math.combinatorics :as combo]
            )
  (:use [clojure.string :only (join split)])
  )

(def dburi "http://localhost:7474/db/data/")
(def dbusr "neo4j")
(def dbpw "socke")

(def conn (nr/connect dburi dbusr dbpw))

; -------------------
; Utility functions
; ------------------


(defn str-sep [s x y ] (str x s y))

; -------------------
; DSL forms ---------
; -------------------

(defn labels [& ls]
  ls
  )


(defn asserts [as]
  "DSL form for specifying an assertion."
  as
  )

(defn node
  "DSL form for specifying a node"
  ([id labels as]
   ['node {:id id :labels labels :ass as}])
  ([id labels]
   (node id labels '()))
  ([id]
   (node id '() '())))

(defn edge
  "DSL form for specifying an edge"
  ([id labels src tar ass]
   ['edge {:id id :labels labels :src src :tar tar :ass ass}])
  ([id labels src tar]
   (edge id labels src tar '()))
  ([id src tar]
   (edge id '() src tar '()))
  )


(defn pattern
  "DSL form for specifying a graph pattern (reader)"
  [& xs]
  (if (= :homo (first xs))
    ['pattern {:sem :homo :els (rest xs)}]
    ['pattern {:sem :iso :els  xs}]
    )
)


;(combo/combinations '( "n2" "n3") 2)

;(gen-constraint-isomorphism   '( "n2" "n3" "n4" "n5") )

;---------------------------
; DSL to Cypher translation
;---------------------------


(defn ass->cypher [a]
  "Helper funtion for asss->cypher"
  (str (name (first a)) ":\"" (second a) "\""))

(defn asss->cypher [as]
  "Translate a map of assertions to a Cypher code fragment"
  (if (empty? as)
    ""
    (str " {" (reduce (partial str-sep " ") (map ass->cypher (seq as))) "}")
    ))



(defn labels->cypher [l]
  "translate a list of labels to Cypher"
  (if (empty? l)
    ""
    (str (reduce str l))))


(defn node->cypher [n]
  (let [c (second n)]
    (str "MATCH (" (:id c)
         (labels->cypher (:labels c))
         (asss->cypher (:ass c))

         ")"
         )))

(defn edge->cypher [e]
  (let [c (second e)]
    (str "MATCH (" (:src c) ")-[" (:id c)
         (labels->cypher (:labels c))
         (asss->cypher (:ass c))
         "]->(" (:tar c) ")")))


(edge->cypher (edge 'e1 (labels :label1 :label2) 's 't (asserts {:key1 "val1" :key2 "val2"}) ))

(defn graphelem->cypher [e]
  "Translate a graph element to cipher - either node or edge"
  (let [t (first e)]
    (cond
     (= 'node t) (node->cypher e)
     (= 'edge t) (edge->cypher e)
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


(defn pattern->cypher [p]
  "translate a graph pattern to cypher"
  (let [s (second p)
        els (:els s)
        sem (:sem s)
        eids (map get-id (filter-elem 'edge els))
        nids (map get-id (filter-elem 'node els ))]
    (if (nil? els)
      ""
      (str
       (reduce (partial str-sep " ") (map graphelem->cypher els))
       (if (= :iso sem)
         (gen-constraint-isomorphism nids eids)
         ""
         )
       " RETURN "
       (reduce (partial str-sep ", ") (concat nids eids))
       ))))


;-----------------------
; Execution engine
;-----------------------

(defn match [m]
  (cy/tquery conn (str (pattern->cypher m) " LIMIT 1")
             ))
