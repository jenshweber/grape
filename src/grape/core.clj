(ns grape.core
  (:require
   [clojure.math.combinatorics :as combo]
   [schema.core :as s]
   [clojure.string :as str]
   [clojure.set :refer [subset?]]    
   [grape.visualizer :refer :all]
   [grape.tx-cypher :refer :all]
   [grape.util :refer :all]
   [grape.analysis :refer :all]
   [grape.tx-cypher :refer :all]
   [environ.core :refer [env]]
   [taoensso.timbre :as timbre
    :refer (log trace info warn error fatal report
                logf tracef debugf infof warnf errorf fatalf reportf
                spy get-env log-env)]
   [taoensso.timbre.profiling :as profiling
    :refer (pspy pspy* profile defnp p p*)]
   [dorothy.core :as dorothy]
   [neo4j-clj.core :as db]
   [gorilla-graph.core :as gorillagraph])
   (:import (java.net URI)))

(use 'clojure.walk)

; ---------------------------------------------------
; ENGINE
; ---------------------------------------------------

(def dburi (env :db-url))
(def dbusr (env :db-usr))
(def dbpw (env :db-pw))

(def rules-atom (atom {}))
(def queries-atom (atom {}))
(def constraints-atom (atom '()))

(def debug-atom (atom false))

(defn debug! [b]
  (swap! debug-atom (fn [_] b)))

(defn debug? []
  (deref debug-atom))

(defn debug [f & s]
  (if (debug?)
    (apply f s)))


(defn rules []
  (deref rules-atom))


(defn queries []
  (deref queries-atom))

(defn set-graph! [g] 
  (intern 'grape.core '_ g))

(set-graph! "nil")

(defn add-rule! [n s]
  (swap! rules-atom (fn [c] (assoc c n s))))

(defn add-query! [n s]
  (swap! queries-atom (fn [c] (assoc c n s))))

(def conn (try
         (-> (db/connect (URI. "bolt://localhost:7687")
                       "neo4j"
                       "grape")
             db/get-session)
            (catch Exception e (println (str "Caught exception: " (.toString e))))
            (finally "UNSUCCESSFUL")))



(defn dbquery [q]
  (debug println "DBQuery: " q)
  (let [qf (db/create-query q)
        res (try (qf conn)
                 (catch Exception e (println (str "Caught exception: " (.toString e))))
                 (finally "UNSUCCESSFUL"))]
    (debug println "************* done \n")
    res))


; -------------------------------------------------------
; Viewer
; -------------------------------------------------------

(defn node->view [c d n]
  (let [p (second n
                  )
        handle (name (:id p))
        dhandle (if (str/starts-with? handle "_") "_" handle)
        l (:label p)
        bgcolour (if c  "PaleGreen"
                     (if (some #(= (symbol handle) %) d)
                       "Salmon"
                       "Gainsboro"))
        bordercolour (if c  "#32CD32"
                         (if (some #(= (symbol handle) %) d)
                           "#FF2400"
                           "black"))]
    {:id handle 
     :label (str dhandle (if (nil? l) "" (str ":" l)))
     :shape "box"
     :color {:background bgcolour
             :border bordercolour
             :highlight {:background bgcolour
                         :border bordercolour}}
     :shapeProperties (if (:opt p) {:borderDashes [5 5]} {})}))



(defn edge->view [c d e]
  (let [p (second e)
        handle (name (:id p))
        src (name (:src p))
        tar (name (:tar p))
        l (:label p)
        colour (if c  "#32CD32"
                   (if (some #(= (symbol handle) %) d)
                     "#FF2400"
                     "Black"))]
    {:from src
     :to tar
     :color colour
     :label l
     :shapeProperties (if (:opt p) {:borderDashes [5 5]} {})
     :arrows {:to {:enabled true}}}))



; ---------------------------------------------------
; DSL
; ---------------------------------------------------


(defn resolve-value [scope s]
  (let [[m par] (re-find #"(?:&)(\w+)" s)]
    (if (nil? par)
      s
      (resolve-value scope (clojure.string/replace s m (str (scope (symbol par))))))))
(defn asserts->cypher [s as]
  "Translate a map of assertions to a Cypher code fragment"
  (if (empty? as)
    ""
    (str " {" (reduce (partial str-sep ", ") (map (fn [[k v]] (str (name k) ":" (resolve-value s v))) as)) "}")))

(defn readnode->cypher [n]
  (let [handle (-> n second :id)
        label (-> n second :label)
        opt (-> n second :label)
        ass (-> n second :asserts)]
    (str
     (if (true? opt)
       "optional match("
       "match(")
     handle
     (if (empty? label) ":__Node" (str ":__Node:" label))
     (asserts->cypher [] ass)
     ")"
     "<-[:create]-(:`__Graph`)"
     "<-[:prov*0..]-(_g)"
     " where not (" handle ") <-[:delete]-(:`__Graph`)<-[:prov*0..]-(_g) ")))

(defn readnode->cypher2 [n]
  (let [handle (-> n second :id)]
    (str "create (" handle ")"
         "<-[:read]-(_gn)")))

(defn createnode->cypher [n]
  (let [handle (-> n second :id)
        label (-> n second :label)
        ass (-> n second :asserts)]
    (str " create(" handle ":" label ":__Node"
         (asserts->cypher [] ass)
         ")"
         "<-[:create]-(_gn) ")))


(defn readedge->cypher [e]
  (let [p (second e)]
    (str 
     (if (true? (:opt p))
       " optional match ("
       " match (") 
     (:src p) ")<-["(:id p)"_s:src]-(" (:id p) ":`__Edge`" 
     (if (empty? (:label p)) "" (str ":" (:label p))) ")-["(:id p)"_t:tar]->(" (:tar p) ")"
     " WHERE (" (:id p) ") <-[:create]-(:`__Graph`)<-[:prov*0..]-(_g)"
     " AND NOT (" (:id p) ") <-[:delete]-(:`__Graph`)<-[:prov*0..]-(_g) ")))

(defn createedge->cypher [e]
  (let [p (second e)]
    (str " CREATE ( " (:id p) " :`__Edge`:" (:label p) ") <-[:create]-(_gn)"
         " WITH * CREATE ( " (:tar p) " )<-[:tar]-(" (:id p) ") -[:src]->(" (:src p) " )"
         " SET " (:id p) ".src=ID(" (:src p) "), "
          (:id p) ".tar=ID(" (:tar p) ")")))

(defn delitem->cypher [i]
  (str " create (" i ") <-[:delete]-(_gn) "))

(defn all-ids [els]
  (map (fn [el] (-> el second :id)) els))

(defn gen-constraint-isomorphism
  [nodes edges]
  (if (and (<= (count nodes) 1) (<= (count edges) 1))
    ""
    (let [nc (combo/combinations (distinct (all-ids nodes)) 2)
          ec (combo/combinations (distinct (all-ids edges)) 2)
          ;_ (println "\n\n **** " nc " --- " ec " \n\n ")
          f (fn [x] (str "ID(" (first x) ")<>ID(" (second x) ")"))
          ni (map f nc)
          ei (map f ec)]
      (reduce (partial str-sep " AND ") (concat ni ei)))))

(defn readnodeRet->cypher [n]
  (let [id (-> n second :id)]
    (str id "{.*, "
         "id:ID(" id "), "
         "labels:labels(" id ")} ")))


(defn search [g n pars]
  (let [r ((rules) n)
        nodesToRead (filter-elem 'node (-> r :read second :els))
        nodesToReadStr (apply str (interpose " WITH * " (map readnode->cypher nodesToRead)))
        nodesToReturnStr (apply str (interpose "," (map readnodeRet->cypher nodesToRead)))
        edgesToRead (filter-elem 'edge (-> r :read second :els))
        edgesToReadStr (apply str (interpose " WITH * " (map readedge->cypher edgesToRead)))
        edgesToReturnStr (apply str (interpose "," (map readnodeRet->cypher edgesToRead)))
        qstr (str "MATCH (_g:`__Graph` {uid:\"" g "\"}) "
                  nodesToReadStr
                  " WITH * " edgesToReadStr

                  (if (= :iso (-> r :read second :sem))
                    (let [st (gen-constraint-isomorphism nodesToRead edgesToRead)]
                      (if (empty? st)
                        ""
                        (str " WITH * WHERE " st )))
                    "")

                  " RETURN _g "
                  (if (not (empty? nodesToReturnStr)) (str "," nodesToReturnStr))
                  (if (not (empty? edgesToReturnStr)) (str "," edgesToReturnStr)))]
   (dbquery qstr)))


(defn getHandle [e]  (-> e first name))

(defn getId [e]  (-> e second :id))

(defn nodeRem->cypher [n]
  (let [id   (getId n)
        handle (getHandle n)]
    (str  " MATCH(" handle ") WHERE ID( " handle ")=" id)))

(defn derive [g n pars redex]
  (let [r ((rules) n)
        nodesToRematch (filter (fn [x] (some #(= "__Node" %) (-> x second :labels))) redex)
        nodesToRematchStr (apply str (map nodeRem->cypher nodesToRematch))
        edgesToRematch (filter (fn [x] (some #(= "__Edge" %) (-> x second :labels))) redex)
        edgesToRematchStr (apply str (map nodeRem->cypher edgesToRematch))
        itemsDeleteStr (if (empty? (:delete r))
                         " "
                         (str " WITH * "(apply str (interpose " WITH * " (map delitem->cypher (:delete r))))))
        nodesToRead (filter-elem 'node (-> r :read second :els))
        nodesToReadStr2 (if (empty? nodesToRead)
                          ""
                          (str " WITH * "(apply str (interpose " WITH * " (map readnode->cypher2 nodesToRead)))))
        nodesToCreate (filter-elem 'node (-> r :create second :els))
        nodesToCreateStr (if (empty? nodesToCreate)
                           ""
                           (str " WITH * " (apply str (interpose " WITH * " (map createnode->cypher nodesToCreate))))) 
        edgesToCreate (filter-elem 'edge (-> r :create second :els))
        edgesToCreateStr (if (empty? edgesToCreate)
                           ""
                           (str " WITH * "(apply str (interpose " WITH * " (map createedge->cypher edgesToCreate)))))
        qstr (str "MATCH (_g:`__Graph` {uid:\"" g "\"}) "
                  nodesToRematchStr
                  " WITH * " edgesToRematchStr
                  " CREATE (_gn:`__Graph`{uid: apoc.create.uuid()})-[:prov{rule:\"" n "\"}]->(_g) "
                  nodesToCreateStr
                  nodesToReadStr2
                  edgesToCreateStr
                  itemsDeleteStr
                  " RETURN _gn")]
    (let [g (->
             (dbquery qstr)
             first 
             :_gn :uid
             list)]
      (when (not (empty? g)) (set-graph! g))
      g)))


(defn exec [gs n par]
  (let [g (first gs)
        redexes (search g n par)]
    (if (empty? redexes)
      nil
      (derive g n par (first redexes)))))

(defn exec* [g n par]
  (let [redexes (search g n par)]
    (map (partial derive g n par) redexes)))


(defn exec-query [g n & par]
  (let [r ((queries) n)
        nodesToRead (filter-elem 'node (-> r :read second :els))
        nodesToReadStr (apply str (interpose " WITH * " (map readnode->cypher nodesToRead)))
        edgesToRead (filter-elem 'edge (-> r :read second :els))
        edgesToReadStr (apply str (interpose " WITH * " (map readedge->cypher edgesToRead)))
        nodesToReturnStr (apply str (interpose "," (map #(-> % second :id) nodesToRead)))
        edgesToReturnStr1 (apply str (interpose "," (map #(str (-> % second :id) "_s") edgesToRead)))
        edgesToReturnStr2 (apply str (interpose "," (map #(str (-> % second :id) "_t") edgesToRead)))
        edgesToReturnStr3 (apply str (interpose "," (map #(-> % second :id) edgesToRead)))


        qstr (str "MATCH (_g:`__Graph` {uid:\"" g "\"}) "
                  " WITH * "
                  nodesToReadStr
                  " WITH * " edgesToReadStr
                  (if (= :iso (-> r :read second :sem))
                    (let [st (gen-constraint-isomorphism nodesToRead edgesToRead)]
                      (if (empty? st)
                        ""
                        (str " WITH * WHERE " st)))
                    "")
                  " RETURN _g," nodesToReturnStr ","
                  edgesToReturnStr1 "," edgesToReturnStr2 "," edgesToReturnStr3)]
    (->> (dbquery qstr)
        (map (fn [row] (map (fn [col]
                           (assoc (select-keys (second col) [:data :metadata])
                                  :handle (first col)))
                         row))
          )
         flatten)))

(defn rollback []
  (dbquery "MATCH (gt:`__Graph`)-[:prov*0..]->(gp:`__Graph`) 
   WHERE gt.tag IS NOT NULL  with collect(gp.uid) as transacted 
   match (g:`__Graph`) where g.tag IS NULL and not g.uid in transacted 
   with * OPTIONAL MATCH (g) -[:create]->(i) detach delete g,i")
  true)

(defn commit [g t]
    (let [res (dbquery (str "MATCH (g:`__Graph`{uid:\"" g 
                            "\"}) set g.tag=\"" t "\" return g"))
        id   (if (empty? res) 
               "NOT FOUND"
               (-> res first second first first :data :uid))]
    id))


(defn graph [t]
  (let [res (dbquery (str "MATCH (g:`__Graph`{tag:\"" t "\"}) return g"))
        id   (if (empty? res) 
               "NOT FOUND"
               (-> res first second first first :data :uid))]
    (when (not (= "NOT FOUND" id))
      (set-graph! id))
    id))
      

(defn newgraph []
  "DSL form to create a new graph"
  (let [g (->
         (dbquery (str "create (g:`__Graph` {uid: apoc.create.uuid()}) return g"))
         first :g :uid list)]
    (set-graph! g)
    g))



(defn node-os
  "DSL form for specifying a node"
  [id rest]
  (let [check-syntax (partial check-syntax-generic
                              (str "NODE   :- ( node <HANDLE> <PROP> ) \n"
                                   "PROP   :-  <LABEL> <ASSERT> <OID> <OPT> \n"
                                   "LABEL  :- :label *string* \n"
                                   "ASSERT :- :asserts {KEYVAL*} \n"
                                   "OID    :- :oid *number*"
                                   "KEYVAL :- KEY *string* \n"
                                   "HANDLE :- *symbol* \n"
                                   "KEY    :- *keyword* \n"
                                   "OPT    := :merge true | :opt true"))]
    (check-syntax (symbol? id) "HANDLE should be a symbol."))
  ['node (assoc rest :id id)])

(defn node [& args]
  (let [handle (if (even? (count args))
                 (symbol (random-id))
                 (first args))
        r      (if (even? (count args))
                 args
                 (rest args))]

    (node-os handle (zipmap (take-nth 2 r)
                            (take-nth 2 (rest r))))))

(defn edge-os
  "DSL form for specifying an edge"
  [id rest]
  (let [check-syntax (partial check-syntax-generic
                              (str "(Problem with edg-id " id ") \n"
                                   "EDGE     :- ( edge <HANDLE> <LABEL> LOCATION <ASSERTS> <:opt true>) \n"
                                   "LOCATION :-  :src ID :tar ID  \n"
                                   "LABEL    :- :label L \n"
                                   "ASSERT   :- :asserts {KEYVAL*} \n"
                                   "KEYVAL   :- KEY VALUE \n"
                                   "HANDLE   :- *symbol* \n"
                                   "KEY      :- *keyword* \n"
                                   "L,VALUE  :- *symbol* | *string*\n"))]
    (check-syntax (symbol? id) "edge ID should be a symbol.")
  ;  (check-syntax (valid-schema {(s/optional-key :label) (s/either s/Symbol s/Str)
  ;                               :src s/Symbol
  ;                               :tar s/Symbol
  ;                               (s/optional-key :asserts) {s/Keyword (s/either s/Symbol s/Str)}}
  ;                              rest) "")
    )
  ['edge (assoc rest :id id)])

(defn edge [& args]
  (let [handle (if (even? (count args))
                 (symbol (random-id))
                 (first args))
        r      (if (even? (count args))
                 args
                 (rest args))]
    (edge-os handle (zipmap (take-nth 2 r)
                            (take-nth 2 (rest r))))))

(defn path
  "DSL form for specifying an edge"
  [& args]
  (let [exp    (first args)
        r      (rest args)
        argmap  (zipmap (take-nth 2 r)
                        (take-nth 2 (rest r)))]
    ['path (assoc argmap :exp exp)]))

(defn pattern
  "DSL form for specifying a graph patterns"
  [& xs]
  (let [check-syntax (partial check-syntax-generic
                              (str "PATTERN :- ( pattern <MTYPE> ELEM+ ) \n"
                                   "MTYPE   :- :iso | :homo \n"
                                   "ELEM    :- (node ...) | (edge ...) | (NAC ...) | (condition ...) | (assign ...)"))]
    (let [f (first xs)
          r (if (keyword? f) (rest xs) xs)
          m (if (= f :iso) :iso :homo)]
      (if (symbol? f) (check-syntax (valid-schema (s/either :homo :iso) f) "MTYPE must be :iso or :homo"))
      (check-syntax (valid-children #{'node 'edge 'NAC 'cond 'assign 'path} r) "ELEM must be node, edge, NAC, condition, assign, or path")
      ['pattern {:sem m :els r}])))


(defn condition [c]
  ['cond c])

(defn assign [s]
  ['assign s])

(defn bind
  "Binds a value from a rule application (e) to a name (n)"
  [n e]
  ['__bind n e])

(defn consult [n]
  "resolves value bound to a given name (n)"
  ['__consult n])



(defn NAC
  "DSL form for specifying Negatic Applications Conditions (NACs)"
  [& xs]
  (let [check-syntax (partial check-syntax-generic
                              (str "NAC :- ( NAC <ID> (pattern ...) ) \n"
                                   "ID  :- *number*"))]
    (let [f (first xs)
          id (if (number? f) f 1)
          r (if (number? f) (rest xs) xs)]
      ['NAC id (apply pattern r)])))

(defn rule-os
  "Helper function to create GT Rule"
  [n params prop]
  (let [check-syntax (partial check-syntax-generic
                              (str "RULE          :- ( rule NAME <[PAR+]>  <:theory 'spo|'dpo> <:read PATTERN> <:delete [ID+]> <:create PATTERN>  ) \n"
                                   "NAME, PAR, ID :- *symbol* \n"
                                   "PATTERN       := (pattern ...)"))]
    (check-syntax (symbol? n) "rule name must be a symbol")
    (if (not (empty? params))
      (check-syntax (valid-schema [s/Symbol] params) "rule parameter list must be sequence of symbols"))
    (check-syntax (subset? (set (keys prop)) #{:read :delete :create :theory}) "unknown part of rule. Rules can only have :theory :read, :delete and :create parts")
    (if (contains? prop :read) (check-syntax (= 'pattern (first (:read prop))) "'read' part of rule must contain a pattern"))
    (if (contains? prop :create) (check-syntax (= 'pattern (first (:create prop))) "'create' part of rule must contain a pattern"))
    (if (contains? prop :delete) (check-syntax (valid-schema [s/Symbol] (:delete prop)) "'delete' part must specify sequence of symbols"))
    (if (contains? prop :delete) (check-syntax (contains? prop :read) "'delete' part requires 'read' part to be present"))
    (if (contains? prop :theory) (check-syntax (valid-schema s/Symbol (:theory prop)) "theory must be a symbol"))

    (let [r (assoc prop :params params)
          r2 (if (not (contains? prop :theory))
               (assoc r :theory 'spo)
               r)
          s (if (not (contains? prop :delete))
              (assoc r :delete [])
              r2)]
       ;(validate-rule s)
      (add-rule! n s)
      (intern *ns* (symbol (str (name n))) (fn [g & par] (exec g n par)))
      (intern *ns* (symbol (str (name n) "*")) (fn [g & par] (exec* g n par)))
      (intern *ns* (symbol (str (name n) "-dot")) (fn [] (rule->dot n s)))
      ((intern *ns* (symbol (str (name n) "-show")) (fn [] (show (rule->dot n s))))))))

(defn rule [n & args]
  "DSL form for specifying a graph transformation"
  (if (even? (count args))
    (rule-os n [] (zipmap (take-nth 2 args)
                          (take-nth 2 (rest args))))
    (rule-os n (first args) (zipmap (take-nth 2 (rest args))
                                    (take-nth 2 (rest (rest args)))))))

(defn realLabel [ls]
  (first (filter #(not (or (= % "__Node") (= % "__Edge"))) ls)))





(defn query [n params pat]
  "DSL form for specifying a graph query"
  (let [s {:read pat :params params}]
    (add-query! n s)
    (intern *ns* (symbol (str (name n))) (fn [g & par] (exec-query g n par)))
    (intern *ns* (symbol (str (name n) "-dot")) (fn [] (query->dot n s)))
    ((intern *ns* (symbol (str (name n) "-show")) (fn [] (show (query->dot n s)))))))


(rule 'delete-any-node!
      :read (pattern (node 'n))
      :delete ['n])



(defn occnodes [g kind]
  "return the occurance of kind nodes for graph g"
  (let [qstr (str "match(g:`__Graph` {uid:'" g "'}) -[:" kind "]->(n:`__Node`) RETURN n  ")]
    (->>
     (dbquery qstr)
     (map #(-> % (get-in ["n"]) (select-keys [:metadata :data]))))))

(defn occdelnodes [g]
  (occnodes g "delete"))

(defn occaddnodes [g]
  (occnodes g "create"))

(defn occkeepnodes [g]
  (occnodes g "read"))

(defn occedges [g kind]
  "return the occurance of kind nodes for graph g"
  (->>
   (dbquery (str "match(g:`__Graph` {uid:'" g "'}) -[:" kind "]->(e:`__Edge`) 
                      WITH * match (e)-[:src]->(s)
                      WITH * match (e)-[:tar]->(t)
                      RETURN e,s,t  "))
   (map #(-> % (get-in ["e"])
             (select-keys [:metadata :data])
             (assoc :src (-> % (get-in ["s" :metadata :id])))
             (assoc :tar (-> % (get-in ["t" :metadata :id])))))))


(defn occdeledges [g]
  (occedges g "delete"))

(defn occaddedges [g]
  (occedges g "create"))

(defn occkeepedges [g]
  (occedges g "read"))

(defn occgraphs [g]
  "search for g and its provenance"
  (let [res (->
             (dbquery  (str "match(g:`__Graph` {uid:'" g "'}) "
                             " OPTIONAL MATCH(g)-[e:prov]->(p) return g,e,p"))
             first)]
    {:g (get-in res ["g" :data :uid])
     :p (get-in res ["p" :data :uid])
     :r (get-in res ["e" :data :rule])}))


(defn- occurance [g]
  (assoc (occgraphs g)
         :keepnodes (occkeepnodes g)
         :delnodes (occdelnodes g)
         :addnodes (occaddnodes g)
         :keepedges (occkeepedges g)
         :deledges (occdeledges g)
         :addedges (occaddedges g)))



(defn n->dot [n color]
  (let [id   (-> n (:metadata) (:id))
        label  (-> n (:metadata) (:labels) (realLabel))
        attrs  (->> (:data n)
                    (map #(str (-> % first name) ":" (-> % second pr-str))))
        attrstr (apply str (interpose "\n" attrs))]
    (str id " [shape=record penwidth=bold label=\"{" id ":" label
         (if (not (empty? attrstr))
           (str " | " (str/escape attrstr {\" "'"}))
           "")
         "}\" color=" color " fontcolor=" color " ] ")))


(defn nd->dot [n]
  (n->dot n "red"))

(defn na->dot [n]
  (n->dot n "forestgreen"))
(defn nk->dot [n]
  (n->dot n "black"))

(defn e->dot [e color]
  (let [label  (-> e :metadata :labels realLabel)
        src  (-> e :data :src)
        tar  (-> e :data :tar)]
    (str src " -> " tar "[ label= \"" label "\" 
      color=" color " fontcolor= " color "]")))


(defn ek->dot [e]
  (e->dot e "black"))

(defn ed->dot [e]
  (e->dot e "red"))

(defn ea->dot [e]
  (e->dot e "forestgreen"))

(defn viewocc [g]
  "view the graph occurance"
  (let [o (occurance g)
        g2 (or (:g o) "  EMPTY ")
        g1 (or (:p o) "NOTFOUND")
        rn (-> o :r)]
    (->
     (str "digraph G { subgraph cluster_p  { 
           label = <<b>" (subs g1 0 8) " &#8212;" rn "&#8594; " (subs g2 0 8) "</b>> "
          (apply str (map nk->dot (:keepnodes o))) "\n"
          (apply str (map nd->dot (:delnodes o))) "\n"
          (apply str (map na->dot (:addnodes o))) "\n"
          (apply str (map ek->dot (:keepedges o))) "\n"
          (apply str (map ed->dot (:deledges o))) "\n"
          (apply str (map ea->dot (:addedges o))) "\n"
          " } }")
     show)))

(defn step->dot [r]
  (let [uid1 (get-in r ["g1" :data :uid])
        tag (get-in r ["g1" :data :tag])
        id1  (get-in r ["g1" :metadata :id])]
    (str id1 "[label=\"" (or tag (subs uid1 0 8)) "\"]")))


(defn prov->dot [r]
  (let [rule (get-in r ["e" :data :rule])
        src  (get-in r ["g2" :metadata :id])
        tar  (get-in r ["g1" :metadata :id])]
    (if (or (nil? src) (nil? tar))
      " "
      (str tar " -> " src "[label=\"" rule "\"]"))))

(defn viewproc [g]
  "view the graph process"
  (let [p (dbquery (str "match (g1:`__Graph`)-[:prov*0..]-(:`__Graph`{uid:\"" g "\"}) OPTIONAL match (g2)-[e:prov]->(g1:`__Graph`) return g1,e,g2"))
        nodes (map step->dot p)
        nodeStr (apply str (interpose " \n " nodes))
        edges (map prov->dot p)
        edgeStr (apply str (interpose " \n " edges))
        complete   (str "digraph D { "
                        nodeStr
                        " "
                        edgeStr
                        "}")]
    (show complete)))


(defn result->dot [res]
  (let [nodes (filter (fn [x] (some #(= "__Node" %) (-> x :metadata :labels))) res)
        nodeStr (apply str (map nk->dot nodes))
        edges (filter (fn [x] (some #(= "__Edge" %) (-> x :metadata :labels))) res)
        edgeStr (apply str (map ek->dot edges))
        graphs (filter (fn [x] (some #(= "__Graph" %) (-> x :metadata :labels))) res)]
    (str "digraph g { splines=true overlap=false subgraph cluster0 {"
         "label=\"GRAPH: " (or (-> graphs first :data :tag)
                               (-> graphs first :data :uid)
                               "empty") "\"; \n"
         nodeStr
         " \n "
         edgeStr
         " }} ")))

(defn n->view [n]
  (let [id   (-> n (:metadata) (:id))
        label  (-> n (:metadata) (:labels) (realLabel))
        attrs  (->> (:data n)
                    (map #(str (-> % first name) ":" (-> % second pr-str))))
        attrstr (apply str (interpose "\n" attrs))]
    {:id id
     :label (str id ":" label
                 (if (not (empty? attrstr))
                   (str "\n" (str/escape attrstr {\" "'"}))
                   ""))}))
    
(defn e->view [e]
  (let [label  (-> e :metadata :labels realLabel)
        src  (-> e :data :src)
        tar  (-> e :data :tar)]
    {:from src
     :to tar
     :label label
     :arrows {:to {:enabled true}}}))
    


(defn result->view [res]
  (let [nodes (filter (fn [x] (some #(= "__Node" %) (-> x :metadata :labels))) res)
        nodesV (map n->view nodes)
        nodesVD (reduce (fn [res n] (if (some #(= (:id %) (:id n)) res)
                                      res
                                      (conj res n))) 
                        '() nodesV)
        edges (filter (fn [x] (some #(= "__Edge" %) (-> x :metadata :labels))) res)
        edgesV (map e->view edges)
        graphs (filter (fn [x] (some #(= "__Graph" %) (-> x :metadata :labels))) res)]
    (if (empty? (-> graphs first :data :uid))
      {:nodes [:id 0 :shape "text" :label "(empty)"]}
      {:nodes (vec (concat [{:id 0
                             :label (str "Graph: " (or (-> graphs first :data :tag)
                                                       (-> graphs first :data :uid)))
                             :shape "text"
                             :font {:size 20 :bold true}}]
                           nodesVD))
       :edges edgesV})))



(defn viewquery [res] 
  (-> res result->dot show))

(defn browsequery [res]
  (-> res result->view gorillagraph/view))

(query 'any []
       (pattern (node 'n1)
                (node 'n2 :opt true)
                (edge 'e :src 'n1 :tar 'n2 :opt true)))


(defn viewgraph [g]
  (-> (any g) viewquery))

(defn browsegraph [g]
  (-> (any g) browsequery))

(comment
(rule 'setup-ferryman
      :create
      (pattern
       (node 'tg :label "Thing" :asserts {:kind "'Goat'"})
       (node 'tc :label "Thing" :asserts {:kind "'Grape'"})
       (node 'tw :label "Thing" :asserts {:kind "'Wolf'"})
       (node 's1 :label "Side" :asserts {:name "'This side'"})
       (node 's2 :label "Side" :asserts {:name "'Other side'"})
       (node 'f  :label "Ferry")
       (edge :label "is_at" :src 'tg :tar 's1)
       (edge :label "is_at" :src 'tc :tar 's1)
       (edge :label "is_at" :src 'tw :tar 's1)
       (edge :label "is_at" :src 'f :tar 's1)))


(rule 'ferry_one_over
      :read
      (pattern :iso
               (node 's1 :label "Side")
               (node 's2 :label "Side")
               (node 'f :label "Ferry")
               (node 't :label "Thing")
               (edge 'et :label "is_at" :src 't :tar 's1)
               (edge 'e :label "is_at" :src 'f :tar 's1))
      :delete ['e 'et]
      :create
      (pattern
       (edge :label "is_at" :src 'f :tar 's2)
       (edge :label "is_at" :src 't :tar 's2)))

(rule 'cross_empty
      :read
      (pattern :iso
               (node 's1 :label "Side")
               (node 's2 :label "Side")
               (node 'f :label "Ferry")
               (edge 'e :label "is_at" :src 'f :tar 's1))
      :delete ['e]
      :create
      (pattern
       (edge :label "is_at" :src 'f :tar 's2)))

(query 'wolf-can-eat-goat? []
       (pattern :iso
                (node 't1 :label "Thing" :asserts {:kind "'Wolf'"})
                (node 't2 :label "Thing" :asserts {:kind "'Goat'"})
                (node 's :label "Side")
                (node 's2 :label "Side")
                (edge :label "is_at" :src 't1 :tar 's)
                (edge :label "is_at" :src 't2 :tar 's)
                (node 'f :label "Ferry")
                (edge :label "is_at" :src 'f :tar 's2)))

(query 'goat-can-eat-grape? []
       (pattern :iso
                (node 't1 :label "Thing" :asserts {:kind "'Grape'"})
                (node 't2 :label "Thing" :asserts {:kind "'Goat'"})
                (node 's :label "Side")
                (node 's2 :label "Side")
                (edge :label "is_at" :src 't1 :tar 's)
                (edge :label "is_at" :src 't2 :tar 's)
                (node 'f :label "Ferry")
                (edge :label "is_at" :src 'f :tar 's2)))

(query 'all_on_the_other_side? []
       (pattern
        (node 'tg :label "Thing" :asserts {:kind "'Goat'"})
        (node 'tc :label "Thing" :asserts {:kind "'Grape'"})
        (node 'tw :label "Thing" :asserts {:kind "'Wolf'"})
        (node 's2 :label "Side" :asserts {:name "'Other side'"})
        (edge :label "is_at" :src 'tg :tar 's2)
        (edge :label "is_at" :src 'tc :tar 's2)
        (edge :label "is_at" :src 'tw :tar 's2)))


(defn solveFMP []
  (loop [gs (list (-> (newgraph) setup-ferryman))]
    (let [solutions (filter #(not (empty? (all_on_the_other_side?  %))) gs)]
      (if (empty? solutions)
        (let [safeGs (filter #(and (empty? (wolf-can-eat-goat? %))
                                   (empty? (goat-can-eat-grape? %))) gs)]
          (recur (flatten
                  (concat
                   (map ferry_one_over* safeGs)
                   (map cross_empty* safeGs)))))
        (commit (first solutions) "solution")))))

)


(rule 'hello
      :create (pattern
               (node :label "Hello")))

(rule 'world
      :read 	(pattern
              (node 'h :label "Hello"))
      :create	(pattern
               (node 'w :label "World")
               (edge :src 'h :tar 'w :label "to")))

(rule 'vine
      :read 	(pattern
              (node 'h :label "Hello")
              (node 'w :label "World")
              (edge 'e :src 'h :tar 'w :label "to"))
      :delete ['w 'e]
      :create	(pattern
               (node 'g :label "Grape")
               (edge :src 'h :tar 'g :label "to")))