(ns grape.core
  (:require [clojurewerkz.neocons.rest :as nr]
            [clojurewerkz.neocons.rest.cypher :as cy]
            [clojure.math.combinatorics :as combo]
            [clojure.data.json :as json]
            )
  (:use [clojure.string :only (join split)])
  )

(def dburi "http://localhost:7474/db/data/")
(def dbusr "neo4j")
(def dbpw "neo4j")

(def conn (nr/connect dburi dbusr dbpw))

; -------------------
; Utility functions
; ------------------


(defn str-sep [s x y ] (str x s y))

; -------------------
; DSL forms ---------
; -------------------


(defn node
  "DSL form for specifying a node"
  ([id rest]
   ['node (assoc rest :id id)])
  ([id ]
   (node id {})))

(defn edge
  "DSL form for specifying an edge"
  [id rest]
   ['edge (assoc rest :id id)])


(defn pattern
  "DSL form for specifying a graph pattern (reader)"
  [& xs]
  (if (= :homo (first xs))
    ['pattern {:sem :homo :els (rest xs)}]
    ['pattern {:sem :iso :els  xs}]
    )
  )


(defn rule
  "DSL form for specifying a graph transformation"
  [m]
  (if (not (contains? m :theory))
    (assoc m :theory 'spo)
    m
    )
  )


;---------------------------
; DSL to Cypher translation
;---------------------------

(defn asserts->cypher [as]
  "Translate a map of assertions to a Cypher code fragment"
  (if (empty? as)
    ""
    (str " {" (reduce (partial str-sep ", ") (map (fn [[k v]] (str (name k) ":\"" v "\"")) as)) "}")
    ))



(defn node->cypher [m n]
  (let [c (second n)
        s (if (= m :match) " MATCH" " CREATE")]
    (str s " (" (:id c)
         (let [l (:label c)]
           (if (nil? l)
             ""
             (str ":" l)))
         (asserts->cypher (:asserts c))
         ")"
         )))

(defn edge->cypher [m e]
  (let [c (second e)
        s (if (= m :match) " MATCH" " CREATE")]
    (str s " (" (:src c) ")-[" (:id c)
         (let [l (:label c)]
           (if (nil? l)
             ""
             (str ":" l)))
         (asserts->cypher (:ass c))
         "]->(" (:tar c) ")")))

(defn graphelem->cypher [m e]
  "Translate a graph element to cipher - either node or edge"
  (let [t (first e)]
    (cond
     (= 'node t) (node->cypher m e)
     (= 'edge t) (edge->cypher m e)
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


(defn pattern->cypher [m p]
  "translate a graph pattern to cypher matching query"
  (let [s (second p)
        els (:els s)
        sem (:sem s)
        eids (map get-id (filter-elem 'edge els))
        nids (map get-id (filter-elem 'node els ))]
    (if (nil? els)
      ""
      (str
       (reduce (partial str-sep " ") (map (partial graphelem->cypher m) els))
       (if (= m :match)
           (str
            (if (= :iso sem)
              (gen-constraint-isomorphism nids eids)
              ""
              )
            " RETURN "
            (reduce (partial str-sep ", ") (concat nids eids)))
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



(defn rule->cypher [r]
  "translate a rule to cypher"
  (str
   (if (contains? r :delete) (str (if (= (:theory r) 'dpo)
                                    " DELETE "
                                    " DETACH DELETE ")
                                  (reduce (partial str-sep ", ") (:delete r)))
     "")
   (if (contains? r :create) (pattern->cypher :create (:create r))
     "")
   ))



;-----------------------
; Execution engine
;-----------------------

(defn match [m]
  "match a pattern in the host graph"
  (if (nil? (:els (second m)))
    '()
    (let [m (cy/tquery conn (str (pattern->cypher :match m) " LIMIT 1"))]
      (if (empty? m)
        m
        (let [r (first m)
              isNode? (fn [x] (re-find #"node" (:self x)))
              nodes (filter (fn [[k v]] (isNode? v)) r)
              edges (filter (fn [[k v]] (not (isNode? v))) r)
              remove-unneeded (fn [x]
                                (zipmap (keys x)
                                        (map (fn [y] (select-keys y [:metadata :data]))
                                             (vals x))))]
          {:nodes (remove-unneeded nodes) :edges (remove-unneeded edges)}
          )))))


(defn apply-rule [r]
  "apply a rule to a host graph"
  (let [reader (:read r)
        redex (if (not (nil? reader))
                (let [m (match reader)]
                  (if (empty? m)
                    nil
                    (redex->cypher m)))
                "")]
    (if (nil? redex)
      nil
      (let [s (str redex
                   (if (contains? r :delete) (str (if (= (:theory r) 'dpo)
                                                    " DELETE "
                                                    " DETACH DELETE ")
                                                  (reduce (partial str-sep ", ") (:delete r)))
                         "")
                   (if (contains? r :create) (pattern->cypher :create (:create r))
                         "")
                   )]
        (do
          (println s)
          (try
            (cy/tquery conn s)
            (catch Exception e
              (str "Exception: " (.getMessage e)))))
          ))))

