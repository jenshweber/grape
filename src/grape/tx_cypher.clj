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


(defn resolve-value [scope s]
  (let [[m par] (re-find #"(?:&)(\w+)" s)]
    (if (nil? par)
      s
      (resolve-value scope (clojure.string/replace s m (scope (symbol par)))))))

(re-find #"(?:&)(\w+)" "hello &par")

(defn asserts->cypher [s as]
  "Translate a map of assertions to a Cypher code fragment"
  (if (empty? as)
    ""
    (str " {" (reduce (partial str-sep ", ") (map (fn [[k v]] (str (name k) ":" (resolve-value s v) )) as)) "}")
    ))



(defn node->cypher [s m n]
  (let [c (second n)
        k (if (= m :match) " MATCH" " CREATE")]
    (str k " (" (:id c)
         (let [l (:label c)]
           (if (nil? l)
             (graphlabel)
             (str  ":" (resolve-value s l) (graphlabel))))
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
             (str ":" (resolve-value s l))))
         (asserts->cypher s (:ass c))
         "]->(" (:tar c) ")")))

(defn graphelem->cypher [s m e]
  "Translate a graph element to cipher - either node or edge"
  (let [t (first e)]
    (cond
      (= 'node t) (node->cypher s m e)
      (= 'edge t) (edge->cypher s m e)
      (= 'NAC t) ""
      (= 'cond t) ""
      (= 'assign t) ""
     :else
     (throw (Exception. "Invalid graph element"))
     )))

(defn get-id [e]
  (:id (second e)))

(defn all-strings [els]
  (map (fn [el] (if (symbol? el)
                  (name el)
                  el)) els))

(defn gen-constraint-isomorphism
  [nids eids]
  (if (and (<= (count eids) 1) (<= (count nids) 1))
    ""
    (let [nc (combo/combinations (distinct (all-strings nids)) 2)
          ec (combo/combinations (distinct (all-strings eids)) 2)
          ;_ (println "\n\n **** " nc " --- " ec " \n\n ")
          f (fn [x] (str "ID("(first x) ")<>ID(" (second x) ")"))
          ni (map f nc)
          ei (map f ec)
          ]
      (reduce (partial str-sep " AND ") (concat ni ei)))))

(defn filter-elem [k c]
  "filter out specified graph element type k (node or edge)"
  (filter (fn [x] (= k (first x))) c))


(defn ids->return [ids]
  (if (empty? ids)
    ""
    (reduce (fn [s id] (str s ", ID(" (name id) ") AS " (name id)))"" ids)))

(defn ids->order [ids]
  (if (empty? ids)
    ""
    (reduce (fn [s id] (str s ", " (name id))) "" ids)))


(defn pattern->cypher
  "translate a graph pattern to cypher matching query"
  ([scope m p excluded]
   (let [s (second p)
         els (:els s)
         sem (:sem s)
         c (filter-elem 'cond els)
         a (filter-elem 'assign els)
         eids (map get-id (filter-elem 'edge els))
         nids (map get-id (filter-elem 'node els ))
         ex_eids (keys (:edges excluded))
         ex_nids (keys (:nodes excluded))]
     (if (nil? els)
       ""
       (str
        (reduce (partial str-sep " ") (map (partial graphelem->cypher scope m) els))
        (if (= m :match)
          (str
            " WHERE "
            (if (= :iso sem)
              (let [st (gen-constraint-isomorphism (concat nids ex_nids) (concat eids ex_eids))]
                (if (empty? st)
                  ""
                  (str st " AND ")))
              "")
            (if (empty? c)
              "1=1"
              (second (first c)))))

         (if (empty? a)
           ""
           (str " SET " (second (first a))))
         " RETURN "
         (let [rstr (str (ids->return nids) (ids->return eids))]
           (if (= 0 (count rstr))
             ""
             (.substring rstr 1 (count rstr) )))
         " ORDER BY "
         (let [rstr (str (ids->order nids) (ids->order eids))]
           (if (= 0 (count rstr))
             ""
             (.substring rstr 1 (count rstr) )))))))
  ([scope m p]
   (pattern->cypher scope m p {:nodes '{} :edges '{}})))



(defn redex->cypher [r]
  "recall a redex by node and edge IDs"
  (let [nodes (:nodes r)
        edges (:edges r)]
    (str
     (reduce str (map (fn [[n i]] (str " MATCH (" n ") WHERE id(" n ")=" i " ")) nodes))
     (reduce str (map (fn [[e i]] (str " MATCH ()-[" e "]->() WHERE id(" e ")=" i " ")) edges)))))


