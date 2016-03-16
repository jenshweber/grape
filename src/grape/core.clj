(ns grape.core
  (:require [clojurewerkz.neocons.rest :as nr]
            [clojurewerkz.neocons.rest.cypher :as cy]
            [clojure.math.combinatorics :as combo]
            [clojure.data.json :as json]
            [clojure.test :refer :all]
            [clojure.data.codec.base64 :as b64]
            [dorothy.core :as dorothy]))

(def dburi "http://localhost:7474/db/data/")
(def dbusr "neo4j")
(def dbpw "neo4j")

(def conn (nr/connect dburi dbusr dbpw))
(def gragra {})

; -------------------
; Utility functions
; ------------------


(defn str-sep [s x y ] (str x s y))


;--------------------
; Visualization
;--------------------

(defn dot->render [g]
  (dorothy/render g {:format :png}))

(defn dot->image [g]
  (String. (b64/encode (dorothy/render g {:format :png}))))

(defn dorothy->dot [g]
  (dorothy/dot g))

(defn asserts->dot [as]
  "Translate a map of assertions to Dot"
  (if (empty? as)
    ""
    (str (reduce (partial str-sep "\n") (map (fn [[k v]]
                                               (str (name k) "="
                                                    (if (symbol? v)
                                                      v
                                                      (str "'" v "'"))
                                                    )) as)))))

(defn node->dot [n c o]
  (let [p (second n)
        name (:id p)
        l (:label p)
        as (:asserts p)]
    (str " " name " [color=" c " shape=record penwidth=bold  " o " "
         "label=\"{" name (if (nil? l) "" (str ":" l))
         (if (empty? as)
           " "
           (str " | " (asserts->dot as)))
         " }\"]; ")))


(defn edge->dot [e c o]
  (let [p (second e)
        src (name (:src p))
        tar (name (:tar p))
        l (:label p)
        as (:asserts p)]
    (str " " src " -> " tar
         " [color=" c " penwidth=bold len=2 constraint=false fontcolor=" c " " o
         " label=\"" l
         (if (empty? as)
           ""
           (str "\n{" (asserts->dot as) "}"))
         "\" ]"
         )))

(declare pattern->dot)

(defn NAC->dot
  "translate a NAC to dot"
  [nac]
  (let [nacid (second nac)
        c (nth '("blue" "darkviolet" "brown" "dimgrey") nacid)
        p (nth nac 2)]
    (pattern->dot p [] c c " style=dashed ")))

(defn graphelem->dot [d c1 c2 o e]
  "Translate a graph element to dorothy - either node or edge"
  (let [t (first e)
        id (:id (second e))
        c (if (nil? (some #{id} d)) c1 c2)]
    (cond
     (= 'node t) (node->dot e c o)
     (= 'edge t) (edge->dot e c o)
     (= 'NAC t) (NAC->dot e)
     :else
     (throw (Exception. "Invalid graph element"))
     )))


(defn pattern->dot
  "translate a graph pattern to dot"
  [p d c1 c2 o]
  (let [els (:els (second p))]
    (if (nil? els)
      ""
      (reduce str (map (partial graphelem->dot d c1 c2 o) els)))))

(defn rule->dot [rid]
  "translate a rule to dot"
  (let [n (name rid)
        rule (gragra rid)
        r (:read rule)
        d (:delete rule)
        c (:create rule)
        p (:params rule)]
    (str "digraph g {  splines=true overlap=false subgraph cluster0 {"
         "label=\"Rule: " n (str p)\";"
         (pattern->dot r d "black" "red" "")
         (pattern->dot c [] "green" "green" "")
         "}}")))


(defn document-rule [r]
  (dorothy/save! (rule->dot r) (str "doc/images/"(name r) ".png") {:format :png}))

(defn document-rules []
  (map document-rule (keys gragra)))

;---------------------------
; DSL to Cypher translation
;---------------------------


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
             ""
             (str ":" (resolve-const-or-par-lab s l))))
         (asserts->cypher s (:asserts c))
         ")"
         )))

(defn edge->cypher [s m e]
  (let [c (second e)
        k (if (= m :match) " MATCH" " CREATE")]
    (str k " (" (:src c) ")-[" (:id c)
         (let [l (:label c)]
           (if (nil? l)
             ""
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

(defn match [s c m]
  "match a pattern in the host graph. s is a parameterlist, c is an (optional) match context string and m is a pattern"
  (if (nil? (:els (second m)))
    '()
    (let [m (cy/tquery conn (str c " "(pattern->cypher s :match m) " RETURN * LIMIT 1"))]
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

(defn match-nacs [con s nacs]
  (if (empty? nacs)
    false
    (let [nac (first nacs)
          [_ nacid p] nac
          ext (pattern->cypher s :match p)]
      (println ".       [Trying NAC " nacid "]: " con "  ||  " ext )
      (let [m (cy/tquery conn (str con " " ext " RETURN * LIMIT 1"))]
        (if (empty? m)
          (do
            (println ".       [false]")
            (match-nacs con s (rest nacs)))
          (do
            (println ".       [true]")
            true))))))


(defn apply-rule
  "apply a rule to a host graph"
  ([n s]
   (let [r (gragra n)]
     (if (nil? r)
       (throw (Exception. (str "a rule with name " n " does not exist")))
       (do
         (println "[Applying rule:]" (name n))
         (let [reader (:read r)
               redex (if (not (nil? reader))
                     (let [m (match s "" reader)]
                       (if (empty? m)
                         nil
                         (let [nacs (filter (fn [x] (= 'NAC (first x))) (:els (second reader)))
                               con (redex->cypher m)]
                           (if (match-nacs con s nacs)
                             nil
                             con))))
                       "")]
           (if (nil? redex)
             false
             (let [s (str redex
                          (if (contains? r :delete)
                            (str (if (= (:theory r) 'dpo)
                                   " DELETE "
                                   " DETACH DELETE ")
                                 (reduce (partial str-sep ", ") (:delete r)))
                            "")
                          (if (contains? r :create)
                            (pattern->cypher s :create (:create r))
                            "")
                          )]
               (do
                 (println ".   [changes:] "s)
                 (try
                   (cy/tquery conn s)
                   true
                   #_(catch Exception e
                       (do
                         (println (str "Exception: " (.getMessage e)))
                         false )))))))))))
   ([n]
   (apply-rule n {})))

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
  "DSL form for specifying a graph patterns"
  [& xs]
  (if (= :homo (first xs))
    ['pattern {:sem :homo :els (rest xs)}]
    ['pattern {:sem :iso :els  xs}]
    )
  )

(defn NAC
  "DSL form for specifying Negatic Applications Conditions (NACs)"
  [& xs]
  (if (number? (first xs))
    ['NAC (first xs) (apply pattern (rest xs))]
    ['NAC 1 (apply pattern xs)]))

(defn rule
  "DSL form for specifying a graph transformation"
  ([n params prop]
   (do
     (let [r (assoc prop :params params)
           s (if (not (contains? prop :theory))
               (assoc r :theory 'spo)
               r)]
       (def gragra (assoc gragra n s)))
     (let [s (symbol n)]
       (if (= [] params)
         (intern *ns* s (fn [] (apply-rule n)))
         (intern *ns* s (fn [par] (apply-rule n par))))
       (intern *ns* (symbol (str (name n) "-dot")) (fn [] (rule->dot n)))
       ((intern *ns* (symbol (str (name n) "-show")) (fn [] (dot->image (rule->dot n))))))))
  ([n prop]
   (rule n [] prop)))

