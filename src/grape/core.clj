(ns grape.core
  (:require
   [clojure.math.combinatorics :as combo]
   [schema.core :as s]
   [clojure.string :as str]
   [clojure.set :refer :all]
   [grape.visualizer :refer :all]
   [grape.tx-cypher :refer :all]
   [grape.util :refer :all]
   [grape.exec :refer :all]
   [grape.analysis :refer :all]
   ))


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
       (if (= [] params)
         (intern *ns* s (fn [] (apply-rule n)))
         (intern *ns* s (fn [par] (apply-rule n par))))
       (intern *ns* (symbol (str (name n) "-dot")) (fn [] (rule->dot n)))
       ((intern *ns* (symbol (str (name n) "-show")) (fn [] (dot->image (rule->dot n))))))))
  ([n prop]
   (rule n [] prop)))
