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
   [clojurewerkz.neocons.rest.cypher :as cy]
   [clojurewerkz.neocons.rest :as nr]
   [clojurewerkz.neocons.rest.transaction :as nt]
   [grape.tx-cypher :refer :all]
   [environ.core :refer [env]]
   [taoensso.timbre :as timbre
    :refer (log trace info warn error fatal report
                logf tracef debugf infof warnf errorf fatalf reportf
                spy get-env log-env)]
   [taoensso.timbre.profiling :as profiling
    :refer (pspy pspy* profile defnp p p*)]
   [dorothy.core :as dorothy]))

(use 'clojure.walk)

; ---------------------------------------------------
; ENGINE
; ---------------------------------------------------

(def dburi (env :db-url))
(def dbusr (env :db-usr))
(def dbpw (env :db-pw))

(def rules-atom (atom {}))
(def ret-atom (atom {}))
(def bindings-atom (atom {}))
(def last-match-atom (atom '()))
(def constraints-atom (atom '()))
(def tx-atom (atom nil))

(def debug-atom (atom false))

(defn debug! [b]
  (swap! debug-atom (fn [_] b)))

(defn debug? []
  (deref debug-atom))

(defn debug [f & s]
  (if (debug?)
    (apply f s)))


(defn tx [] (deref tx-atom))

(defn set-tx [t] (swap! tx-atom (fn [old] t)))

(defn add-violation! [msg f]
  (swap! constraints-atom (fn [old] (conj old [msg f]))))


(defn rules []
  (deref rules-atom))

(defn ret []
  (deref ret-atom))

(defn reset-ret! []
  (swap! ret-atom (fn [c] {})))

(defn update-ret! [i]
  (swap! ret-atom (fn [c] (merge c i))))

(defn bindings []
  (deref bindings-atom))

(defn add-binding! [k v]
  (swap! bindings-atom (fn [c] (assoc c k v))))

(defn add-rule! [n s]
  (swap! rules-atom (fn [c] (assoc c n s))))

(def conn (nr/connect dburi dbusr dbpw))


(defn begintx []
  (set-tx (nt/begin-tx conn)))

(defn committx []
  (nt/commit conn (tx))
  (set-tx nil))

(defn rollbacktx []
  (nt/rollback conn (tx))
  (set-tx nil))

(declare run-transaction)

(defn no-violations-iter [cs]
  (if (empty? cs)
    true
    (let [n (-> cs first first)
          f  (-> cs first second)]
      (if (not (f))
        (no-violations-iter (rest cs))
        (do
          (println "Grape schema constraint failed: " n)
          false)))))

(defn no-violations? []
  (let [lrbuf (deref last-match-atom)
        r  (no-violations-iter (deref constraints-atom))]
    (swap! last-match-atom (fn [_] lrbuf))
    r))


(defn dbquery [q]
  (debug println "DBQuery: " q)
   (cy/query conn q))

(defn dbtquery [q]
  (debug println "DBQuery: " q)
  (cy/tquery conn q))

; ---------------------------------------------------
; DSL
; ---------------------------------------------------



(defn readnode->cypher [n]
  (let [handle (-> n second :id)
        label (-> n second :label)]
    (str "match(" handle ":" label ")"
         "<-[:create]-(:`__Graph`)"
         "<-[:prov*0..]-(_g)"
         " where not (" handle ") <-[:delete]-(:`__Graph`)<-[:prov*0..]-(_g) ")))

(defn readnode->cypher2 [n]
  (let [handle (-> n second :id)]
    (str "create ("handle")" 
         "<-[:read]-(_gn)")))

(defn createnode->cypher [n]
  (let [handle (-> n second :id)
        label (-> n second :label)]
    (str " create(" handle ":" label ":__Node)"
         "<-[:create]-(_gn) " )))


(defn readedge->cypher [e]
  (let [p (second e)]
    (str " match (" (:src p) ")-[:" (:label p) "]->(" (:tar p) ")")))

(defn createedge->cypher [e]
  (let [p (second e)]
    (str " CREATE ( " (:id p) " :`__Edge`:" (:label p) ") <-[:create]-(_gn)"
         " WITH * CREATE ( " (:tar p) " )<-[:tar]-(" (:id p) ") -[:src]->(" (:src p) " )")))

(defn delitem->cypher [i]
    (str " create (" i ") <-[:delete]-(_gn) "))
        

(defn exec [g n & par]
  (let [r ((rules) n)
        itemsDeleteStr (apply str (interpose " WITH * " (map delitem->cypher (:delete r))))
        nodesToRead (filter-elem 'node (-> r :read second :els))
        nodesToReadStr (apply str (interpose " WITH * " (map readnode->cypher nodesToRead)))
        nodesToReadStr2 (apply str (interpose " WITH * " (map readnode->cypher2 nodesToRead)))
        edgesToRead (filter-elem 'edge (-> r :read second :els))
        edgesToReadStr (apply str (interpose " WITH * " (map readedge->cypher edgesToRead)))
        nodesToCreate (filter-elem 'node (-> r :create second :els))
        nodesToCreateStr (apply str (interpose " WITH * " (map createnode->cypher nodesToCreate)))
        edgesToCreate (filter-elem 'edge (-> r :create second :els))
        edgesToCreateStr (apply str (interpose " WITH * " (map createedge->cypher edgesToCreate)))
        qstr (str "MATCH (_g:`__Graph` {uid:\"" g "\"}) "
                  nodesToReadStr
                  " WITH * " edgesToReadStr
                  " CREATE (_gn:`__Graph`{uid: apoc.create.uuid()})-[:prov{rule:\"" n "\"}]->(_g) "
                  nodesToCreateStr
                  " WITH * "
                  nodesToReadStr2
                  " WITH * "
                  edgesToCreateStr
                  " WITH * "
                  itemsDeleteStr
                  " RETURN _gn")]
    (->
     (dbquery qstr)
     first second first first :data :uid))) 



(defn start []
  "DSL form to create a new graph"
  (->
   (dbquery (str "create (g:`__Graph` {uid: apoc.create.uuid()}) return g"))
   first second first first :data :uid))



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
          s (if (not (contains? prop :theory))
              (assoc r :theory 'spo)
              r)]
       ;(validate-rule s)
      (add-rule! n s)
      (intern *ns* (symbol (str (name n))) (fn [g & par] (exec g n par))) 
      (intern *ns* (symbol (str (name n) "-dot")) (fn [] (rule->dot n s)))
      ((intern *ns* (symbol (str (name n) "-show")) (fn [] (show (rule->dot n s))))))))

(defn rule [n & args]
  "DSL form for specifying a graph transformation"
  (if (even? (count args))
    (rule-os n [] (zipmap (take-nth 2 args)
                          (take-nth 2 (rest args))))
    (rule-os n (first args) (zipmap (take-nth 2 (rest args))
                                    (take-nth 2 (rest (rest args)))))))




(defn document-rule [r]
  (let [dir (java.io.File. "doc/images")]
    (when (not (.exists dir))
      (.mkdirs dir)))
  (dorothy/save! (rule->dot r (r (rules))) (str "doc/images/" (name r) ".png") {:format :png}))

(defn document-rules []
  (map document-rule (keys (rules))))

(rule 'delete-any-node!
      :read (pattern (node 'n))
      :delete ['n])

(defn clear! []
  (while (delete-any-node!)))



(defn occnodes [g kind]
  "return the occurance of kind nodes for graph g"
  (let [qstr (str "match(g:`__Graph` {uid:'" g "'}) -[:" kind "]->(n:`__Node`) RETURN n  ")]
    (->>
     (dbtquery qstr)
     (map #(-> % (get-in ["n"]) (select-keys [:metadata :data]))))))

(defn occdelnodes [g]
  (occnodes g "delete")
  )

(defn occaddnodes [g]
  (occnodes g "create")
  )

(defn occkeepnodes [g]
  (occnodes g "read")
  )

(defn occedges [g kind]
  "return the occurance of kind nodes for graph g"
  (->>
   (dbtquery (str "match(g:`__Graph` {uid:'" g "'}) -[:" kind "]->(e:`__Edge`) 
                      WITH * match (e)-[:src]->(s)
                      WITH * match (e)-[:tar]->(t)
                      RETURN e,s,t  "))
   (map #(-> % (get-in ["e"])
             (select-keys [:metadata :data])
             (assoc :src (-> % (get-in ["s" :metadata :id]))) 
             (assoc :tar (-> % (get-in ["t" :metadata :id])))))))

(def _g "64192761-908b-4f2f-9ed9-a4a268cc7370")

(defn occdeledges [g]
  (occedges g "delete")
  )

(defn occaddedges [g]
  (occedges g "create")
  )

(defn occkeepedges [g]
  (occedges g "read")
  )

(defn occgraphs [g]
  "search for g and its provenance"
  (let [res (->
             (dbtquery  (str "match(g:`__Graph` {uid:'" g "'}) "
             " OPTIONAL MATCH(g)-[e:prov]->(p) return g,e,p"))
             first)]
    {:g (get-in res ["g" :data :uid] )
     :p (get-in res ["p" :data :uid] )
     :r (get-in res ["e" :data :rule] ) }))
  

(defn- occurance [g]
  (assoc (occgraphs g)
         :keepnodes (occkeepnodes g)
         :delnodes (occdelnodes g)
         :addnodes (occaddnodes g)
         :keepedges (occkeepedges g)
         :deledges (occdeledges g)
         :addedges (occaddedges g)))

(defn realLabel [ls]
  (first (filter #(not (or (= % "__Node") (= % "__Edge"))) ls)))

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
               "}\" 
              color=" color " fontcolor=" color " ] ")))


(defn nd->dot [n]
  (n->dot n "red"))

(defn na->dot [n]
  (n->dot n "forestgreen"))
(defn nk->dot [n]
  (n->dot n "black"))

(defn e->dot [e color]
  (let [id   (-> e (:metadata) (:id))
        label  (-> e (:metadata) (:labels) (realLabel))
        src  (-> e (:src))
        tar  (-> e (:tar))
        ]
    (str src " -> " tar "[ label= \"" label "\" 
      color=" color " fontcolor= " color "]")))


(defn ek->dot [e]
  (e->dot e "black"))

(defn ed->dot [e]
  (e->dot e "red"))

(defn ea->dot [e]
  (e->dot e "forestgreen"))

(defn view [g]
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
     show
     )))

(defn step->dot [r]
  (let [uid1 (get-in r ["g1" :data :uid])
        id1  (get-in r ["g1" :metadata :id])]
    (str id1 "[label=\"" (subs uid1 0 8) "\"]")))


(defn prov->dot [r]
  (let [rule (get-in r ["e" :data :rule])
        src  (get-in r ["g2" :metadata :id])
        tar  (get-in r ["g1" :metadata :id])]
    (if (or (nil? src) (nil? tar))
      " "
      (str tar " -> " src "[label=\"" rule "\"]"))))

(defn viewproc [g]
  "view the graph process"
  (let [p (dbtquery (str "match (g1:`__Graph`)-[:prov*0..]-(:`__Graph`{uid:\"" g "\"}) OPTIONAL match (g2)-[e:prov]->(g1:`__Graph`) return g1,e,g2"))
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







(comment

  (rule 'hello
        :create (pattern
                 (node :label "Hello")))


  (rule 'hello2
        :read (pattern
               (node 'n1 :label "Hello"))
        :create (pattern
                 (node :label "World")))


  (rule 'killHello
        :read (pattern
               (node 'n1 :label "Hello"))
        :delete ['n1])


  (rule 'hello3
        :read  (pattern
                (node 'n1 :label "Hello")
                (node 'n2 :label "World"))
        :create (pattern
                 (edge 'e :label "to" :src 'n1 :tar 'n2)))
  )
  
