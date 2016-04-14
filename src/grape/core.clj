(ns grape.core
  (:require
   [clojure.math.combinatorics :as combo]
   [schema.core :as s]
   [clojure.string :as str]
   [clojure.set :refer :all]
   [grape.visualizer :refer :all]
   [grape.tx-cypher :refer :all]
   [grape.util :refer :all]
   [grape.analysis :refer :all]
   [clojurewerkz.neocons.rest.cypher :as cy]
   [clojurewerkz.neocons.rest :as nr]
   [grape.tx-cypher :refer :all]
   [environ.core :refer [env]]
   ))

; ---------------------------------------------------
; ENGINE
; ---------------------------------------------------

(def dburi (env :db-url))
(def dbusr (env :db-usr))
(def dbpw (env :db-pw))

(def conn (nr/connect dburi dbusr dbpw))

(defn match [s c m]
  "match a pattern in the host graph. s is a parameterlist, c is an (optional) match context string and m is a pattern"
  (if (nil? (:els (second m)))
    '()
    (let [q (str c " "(pattern->cypher s :match m) " RETURN * LIMIT 1")
          _ (print q)
          m (cy/tquery conn q)]
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
          ext (pattern->cypher s :match p)
          _ (print ".       [Trying NAC " nacid "]: " con "  ||  " ext )]
      (let [m (cy/tquery conn (str con " " ext " RETURN * LIMIT 1"))
            _ (println " [" (not (empty? m)) "]")]
        (if (empty? m)
          (match-nacs con s (rest nacs))
          true)))))

(defn rule-exists? [n]
  (not (empty? (:rules (eval 'gragra) n))))





(defn apply-rule
  "apply a rule to a host graph"
  [n aparams]
   (let [r ((:rules (eval 'gragra)) n)
         fparams (:params r)
         _ (println "[Applying rule:]" (name n))]
     (when (nil? r)
       (throw (Exception. (str "a rule with name " n " does not exist"))))
     (when (not (= (count fparams) (count aparams)))
       (throw (Exception. (str "rule " n " called with wrong number of parameters: " (count aparams)))))

     (let [reader (:read r)
           s (zipmap fparams aparams)
           redex (if (not (nil? reader))
                   (let [_ (print ".     [matching RHS] ")
                         m (match s "" reader)
                         _ (println " [match found: " (not (empty? m)) "]")]
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
                        ""))
               _ (println ".     [rewriting graph:] " s)]
           (try
             (cy/tquery conn s)
             true
             #_(catch Exception e
                 (do
                   (println (str "Exception: " (.getMessage e)))
                   false ))))))))


; ---------------------------------------------------
; DSL
; ---------------------------------------------------



(defn gts [n]
  (let [check-syntax (partial check-syntax-generic
                              (str "GTS    :- (gts ID ) \n"
                                   "ID     :- *symbol* \n"
                                   ))]
    (check-syntax (symbol? n) "gts ID should be a symbol."))
  (intern *ns* 'gragra {:_graph_name n :rules {}}))


(defn node
  "DSL form for specifying a node"
  ([id rest]
   (let [check-syntax (partial check-syntax-generic
                               (str "NODE   :- ( node ID <PROP> ) \n"
                                    "PROP   :- { <LABEL> <ASSERT> } \n"
                                    "LABEL  :- :label L \n"
                                    "ASSERT :- :asserts {KEYVAL*} \n"
                                    "KEYVAL :- KEY VALUE \n"
                                    "ID     :- *symbol* \n"
                                    "KEY    :- *keyword* \n"
                                    "L,VALUE:- *symbol* | *string*\n"))]
     (check-syntax (symbol? id) "ID should be a symbol.")
     (check-syntax (valid-schema {(s/optional-key :label) (s/either s/Symbol s/Str)
                                  (s/optional-key :asserts) {s/Keyword (s/either s/Symbol s/Str)}}
                                 rest) ""))
   ['node (assoc rest :id id)])
  ([id]
   (node id {})))

(defn edge
  "DSL form for specifying an edge"
  [id rest]
  (let [check-syntax (partial check-syntax-generic
                              (str "EDGE   :- ( edge ID <PROP> ) \n"
                                   "PROP   :- { <LABEL> <ASSERT> :src ID :tar ID } \n"
                                   "LABEL  :- :label L \n"
                                   "ASSERT :- :asserts {KEYVAL*} \n"
                                   "KEYVAL :- KEY VALUE \n"
                                   "ID     :- *symbol* \n"
                                   "KEY    :- *keyword* \n"
                                   "L,VALUE:- *symbol* | *string*\n"))]
    (check-syntax (symbol? id) "edge ID should be a symbol.")
    (check-syntax (valid-schema {(s/optional-key :label) (s/either s/Symbol s/Str)
                                 :src s/Symbol
                                 :tar s/Symbol
                                 (s/optional-key :asserts) {s/Keyword (s/either s/Symbol s/Str)}}
                                rest) ""))
  ['edge (assoc rest :id id)])

(defn pattern
  "DSL form for specifying a graph patterns"
  [& xs]
  (let [check-syntax (partial check-syntax-generic
                              (str "PATTERN :- ( pattern <MTYPE> ELEM+ ) \n"
                                   "MTYPE   :- :iso | :homo \n"
                                   "ELEM    :- (node ...) | (edge ...) | (NAC ...)"))]
    (let [f (first xs)
          r (if (keyword? f) (rest xs) xs)
          m (if (= f :homo) :homo :iso)]
      (if (symbol? f) (check-syntax (valid-schema (s/either :homo :iso) f) "MTYPE must be :iso or :homo") )
      (check-syntax (valid-children #{'node 'edge 'NAC} r) "ELEM must be node, edge or NAC")
      ['pattern {:sem m :els r}])))


(defn NAC
  "DSL form for specifying Negatic Applications Conditions (NACs)"
  [& xs]
  (let [check-syntax (partial check-syntax-generic
                              (str "NAC :- ( NAC <ID> (pattern ...) ) \n"
                                   "ID  :- *number*"))]
    (let [f (first xs)
          id (if (number? f) f 1)
          r (if (number? f) (rest xs) xs)]
      (check-syntax (valid-children #{'node 'edge 'NAC} r) "ELEM must be node, edge or NAC")
      ['NAC id (apply pattern r)])))


(defn rule
  "DSL form for specifying a graph transformation"
  ([n params prop]
   (let [check-syntax (partial check-syntax-generic
                               (str "RULE          :- ( rule NAME <[PAR+]> { <:theory 'spo|'dpo> <:read PATTERN> <:delete [ID+]> <:create PATTERN> } ) \n"
                                    "NAME, PAR, ID :- *symbol* \n"
                                    "PATTERN       := (pattern ...)"))]
     (check-syntax (symbol? n) "rule name must be a symbol")
     (check-syntax (valid-schema [s/Symbol] params) "rule parameter list must be sequence of symbols")
     (check-syntax (subset? (set (keys prop)) #{:read :delete :create :theory}) "unknown part of rule. Rules can only have :theory :read, :delete and :create parts")
     (if (contains? prop :read) (check-syntax (= 'pattern (first (:read prop))) "'read' part of rule must contain a pattern"))
     (if (contains? prop :create) (check-syntax (= 'pattern (first (:create prop))) "'create' part of rule must contain a pattern"))
     (if (contains? prop :delete) (check-syntax (valid-schema [s/Symbol] (:delete prop)) "'delete' part must specify sequence of symbols"))
     (if (contains? prop :delete) (check-syntax (contains? prop :read) "'delete' part requires 'read' part to be present"))
     (if (contains? prop :theory) (check-syntax (valid-schema (s/either 'spo 'dpo) (:theory prop))))

     (let [r (assoc prop :params params)
           s (if (not (contains? prop :theory))
               (assoc r :theory 'spo)
               r)]
       (validate-rule s)
       (intern *ns* 'gragra (assoc (eval 'gragra) :rules (assoc (:rules (eval 'gragra)) n s) )))
     (let [s (symbol n)]
       (intern *ns* s (fn [& par] (apply-rule n par)))
       (intern *ns* (symbol (str (name n) "-dot")) (fn [] (rule->dot n)))
       ((intern *ns* (symbol (str (name n) "-show")) (fn [] (dot->image (rule->dot n))))))))
  ([n prop]
   (rule n [] prop)))

;-------------------------------------------


(defn transaction
  ([name params steps]
   (let [check-syntax (partial check-syntax-generic
                               (str "TRANSACTION   :- ( transaction NAME <[PAR+]> [STEP+] ) \n"
                                    "NAME, PAR, STEP :- *symbol* \n"))]
     (check-syntax (symbol? name) "transaction name must be a symbol")
     (check-syntax (valid-schema [s/Symbol] params) "transaction parameter list must be sequence of symbols")
     (check-syntax (valid-schema [s/Symbol] steps) "transaction steps must be sequence of symbols"))

   (if (not (reduce (fn [agg t] (and agg (rule-exists? t))) true steps))
     (throw (Exception. (str "one or several rule names used in the transaction do not exist \n")))
     true

   ))
  ([name steps]
   (transaction name [] steps))
   )

;(gts 'example)

;(rule-exists? 'create-jewerns!)


;(transaction 'tx ['step1 'step2])
