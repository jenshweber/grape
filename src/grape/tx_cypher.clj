(ns grape.tx-cypher
  (:require
            [clojure.math.combinatorics :as combo]
            [clojure.data.json :as json]
            [schema.core :as s]
            [clojure.string :as str]
            [clojure.set :refer :all]
            [grape.util :refer :all]
            ))


(defn resolve-value [scope s]
  (let [[m par] (re-find #"(?:&)(\w+)" s)]
    (if (nil? par)
      s
      (resolve-value scope (clojure.string/replace s m (str (scope (symbol par))))))))

(re-find #"(?:&)(\w+)" "hello &par")

(defn asserts->cypher [s as]
  "Translate a map of assertions to a Cypher code fragment"
  (if (empty? as)
    ""
    (str " {" (reduce (partial str-sep ", ") (map (fn [[k v]] (str (name k) ":" (resolve-value s v) )) as)) "}")
    ))



(defn node->cypher [s m n]
  (let [c (second n)
        k (if (= m :match) " MATCH"
                           (if (:merge c) " MERGE"
                                          " CREATE"))
        nid (:id c)]
    (str k " (" nid
         (let [l (:label c)]
           (if (nil? l)
             ""
             (str ":" (resolve-value s l))))
         (asserts->cypher s (:asserts c))
         ") "
         (if (= m :match)
           (let [o (:oid c)]
            (if (nil? o)
               " where 1=1 "
               (str " where ID(" nid ")=" (resolve-value s o) " "))))
         )))

(defn edge->cypher [s m e]
  (let [c (second e)
        k (if (= m :match) " MATCH" " MERGE")]
    (str k " (" (:src c) ")-[" (:id c)
         (let [l (:label c)]
           (if (nil? l)
             ""
             (str ":" (resolve-value s l))))
         (asserts->cypher s (:asserts c))
         "]->(" (:tar c) ") "
         (if (= m :match) "where 1=1 "
                          ""))))

(defn path->cypher [e]
  (let [c (second e)
        k  " MATCH"]
    (str k " (" (:src c) ")"
         (str/join "()" (map (fn [s]
                          (let [dir (subs s 0 0)
                                seg (subs s 1 (count s))]
                            (cond (= dir ">") (str "-[" seg "]->")
                                  (= dir "<") (str "<-[" seg "]-")
                                  :else (str "-[" seg "]-"))))
                          (str/split (:exp c) #" ")))
         "(" (:tar c) ") where 1=1 ")))

(defn graphelem->cypher [s m e]
  "Translate a graph element to cipher - either node or edge"
  (let [t (first e)]
    (cond
      (= 'node t) (node->cypher s m e)
      (= 'edge t) (edge->cypher s m e)
      (= 'path t) (path->cypher e)
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
  "translate a read graph pattern to cypher matching query.
  1) scope: provides the actual parameterlist
  2) m: can be :match or :create
  3) p: is the actual pattern
  4) excluded: list of nodes and edges that are excluded from the match"
  ([scope m p excluded]
   (let [s (second p)
         els (:els s)
         sem (:sem s)
         c (filter-elem 'cond els)
         a (filter-elem 'assign els)
         eids (map get-id (filter-elem 'edge els))
         nids (map get-id (filter-elem 'node els ))
         ;_ (println "scope:" scope "   m:" m "   p:" p "    excluded:" excluded "    nids:" nids)
         ]
     (if (or (nil? els) (and (empty? nids) (empty? eids)))
       ""
       (str
        (reduce (partial str-sep " ") (map (partial graphelem->cypher scope m) els))
        (if (= m :match)
          (let [ex_eids (keys (:edges excluded))
                ex_nids (keys (:nodes excluded))]
            (str
              " AND "
              ; add isomorphism condition
              (if (= :iso sem)
                (let [st (gen-constraint-isomorphism (concat nids ex_nids) (concat eids ex_eids))]
                  (if (empty? st)
                    ""
                    (str st " AND ")))
                "")
              ; add custom conditions
              (if (empty? c)
                "1=1"
                (second (first c)))
              ; add graph element id's that have been passed as parameters

              (reduce (partial str-sep " AND ")
                      ""
                      (map (fn [i] (str "ID(" i ")=" (scope i)))
                           (filter (fn [i] (not (nil? (scope i)))) (concat eids nids))))

              (let [rstr (str (ids->return nids) (ids->return eids))
                    ostr (str (ids->order nids) (ids->order eids))]
                (str " RETURN " (.substring rstr 1 (count rstr) )
                     " ORDER BY " (.substring ostr 1 (count ostr) )))))

          ; else (= m :create)
          (let [els2 (:els (second excluded))
                ex_eids (map get-id (filter-elem 'edge els2))
                ex_nids (map get-id (filter-elem 'node els2 ))
                ;_ (println "CREATE CALLED " excluded " *** " ex_nids)
                ]
            (str
              (if (empty? a)
                ""
                (let [rstr (reduce (partial str-sep ", ")
                                   ""
                                   (map (fn [i] (resolve-value scope (second i))) a))]
                  (str " SET " (.substring rstr 1 (count rstr) ))))

              (let [rstr (str (ids->return nids) (ids->return eids) (ids->return ex_nids) (ids->return ex_eids))]
                (if (= 0 (count rstr))
                  " RETURN * "
                  (str " RETURN " (.substring rstr 1 (count rstr) )))))))))))
  ([scope m p]
   (pattern->cypher scope m p {:nodes '{} :edges '{}})))




(defn redex->cypher [r]
  "recall a redex by node and edge IDs"
  (let [nodes (:nodes r)
        edges (:edges r)]
    (str
     (reduce str (map (fn [[n i]] (str " MATCH (" n ") WHERE id(" n ")=" i " ")) nodes))
     (reduce str (map (fn [[e i]] (str " MATCH ()-[" e "]->() WHERE id(" e ")=" i " ")) edges)))))


