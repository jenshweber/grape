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
    [clojurewerkz.neocons.rest.transaction :as nt]
    [grape.tx-cypher :refer :all]
    [environ.core :refer [env]]
    [taoensso.timbre :as timbre
     :refer (log  trace  debug  info  warn  error  fatal  report
                  logf tracef debugf infof warnf errorf fatalf reportf
                  spy get-env log-env)]
    [taoensso.timbre.profiling :as profiling
     :refer (pspy pspy* profile defnp p p*)]
    ))

; ---------------------------------------------------
; ENGINE
; ---------------------------------------------------

(def dburi (env :db-url))
(def dbusr (env :db-usr))
(def dbpw (env :db-pw))

(def conn (nr/connect dburi dbusr dbpw))

(defn begintx []
  (intern *ns* 'tx (nt/begin-tx conn)))

(defn committx []
  (nt/commit conn (eval 'tx)))

(defn rollbacktx []
  (nt/rollback conn (eval 'tx)))

(defn sort-graph-elms [x nids eids]
  {:nodes (select-keys x nids)
   :edges  (select-keys x eids)})

(defn tabelize [res]
  (let [res2 (first (second res))
        cols (:columns res2)
        rows (map (fn [x] (:row x)) (:data res2))
        tab (cy/tableize cols rows )]
    tab))

(defnp dbquery
  ([q p]
   (let [_ (println "DBQuery: " q)
         res (nt/execute conn (eval 'tx) [(nt/statement q)])
         els (:els (second p))
         eids (map name (map get-id (filter-elem 'edge els)))
         nids (map name (map get-id (filter-elem 'node els )))]
     (intern *ns* 'tx (first res))
     (let [tab (tabelize res)
           ret (map (fn [x] (sort-graph-elms x nids eids)) tab)
;           _ (println "Current bindings: " (eval '_ret))
;           _ (println "new returns:" (first tab))
           ]
       (intern *ns* '_ret (merge (eval '_ret) (first tab) ))
       ret)))
  ([q]
   (dbquery q '())))


(defn return-id [l]
  ((eval '_ret) (name l)))

(defn return-node [l]
  (return-id l))

(defn return-edge [l]
  (return-id l))

(defn return-node-props [par]
  (let [i (return-id par)
        _ (begintx)
        res (nt/execute conn (eval 'tx) [(nt/statement (str "MATCH (n) WHERE ID(n)=" i " RETURN *"))])
        tab (tabelize res)]
    (first (vals (first tab)))))

(defn return-edge-props [par]
  (let [i (return-id par)
        _ (begintx)
        res (nt/execute conn (eval 'tx) [(nt/statement (str "MATCH ()-[e]->() WHERE ID(e)=" i " RETURN *"))])
        tab (tabelize res)]
    (first (vals (first tab)))))


(defnp match [s c m]
  "match a pattern in the host graph. s is a parameterlist, c is an (optional) match context string and m is a pattern"
  (if (nil? (:els (second m)))
    '()
    (let [q (str c " "(pattern->cypher s :match m))
          ;          _ (print q)
          m (if (str/blank? q)
              true
              (dbquery q m))
              ;          _ (println "\nRESULT " m)
          ]
      m)))

(defnp match-nacs [m s nacs]
  (if (empty? nacs)
    false
    (let [nac (first nacs)
          con (redex->cypher m)
          [_ nacid p] nac
          ext (pattern->cypher s :match p m)
          ;_ (print ".       [Trying NAC " nacid "]: " con "  ||  " ext )
          ]
      (let [res (dbquery (str con " " ext))
            ; _ (println " [" (not (empty? res)) "]")
            ]
        (if (empty? res)
          (match-nacs m s (rest nacs))
          true)))))

(defn rule-exists? [n]
  (not (empty? (:rules (eval 'gragra) n))))

(defnp check [ms s nacs]
  "check nacs in context of prior matches"
  (remove (fn [x] (nil? x))
          (map (fn [m] (if (match-nacs m s nacs)
                         nil
                         m))
               ms)))

(defnp check-only [s nacs]
  "check nacs in context of no readers"
  (match-nacs '() s nacs))


(defnp drop-n [coll n]
  (if (zero? n)
    coll
    (drop-n (rest coll) (dec n))))

(declare run-transaction)


(defn swap [mps e]
  (concat (take (dec (count mps)) mps) (list (assoc (last mps) :btp e))))


(defn resolve-consults [params]
  (map (fn [p] (if (and (vector? p) (= '__consult (first p)))
                 ((eval '_bindings) (second p))
                 p))
       params))

(defnp run-transaction [steps mps ctr]
  (if (empty? steps)
    [true mps  ctr]
    (let [;_ (println "working on " mps)
           [n & aparams] (first steps)
           ;_ (if (< (inc ctr) (count mps))
           ;    (println "Redoing: " n "with aparams " aparams  )
           ;    (println "Current step: " n "with aparams " aparams  ))
           ctr (inc ctr)
           new (= (count mps) ctr)
           mps (if new
                 (concat mps (list {:name n :btp 0 :max 0}))
                 mps)
           btp (:btp (nth mps ctr))]


      (cond (= '__until n)
            ;;
            ;; UNTIL
            ;;
            (do ;(println "UNTIL")
              (cond (zero? btp) (let [;_ (print "      testing until condition: ")
                                       [res m c] (run-transaction (list (first aparams)) '() -1)
                                       ;_ (println (true? res))
                                       ]
                                  (if (true? res)
                                    (run-transaction (rest steps) (swap mps 2) ctr) ;; until met
                                    (let [r (run-transaction (concat (second aparams) steps) (swap mps 1) ctr)]
                                      r)))
                    ;; until not met
                    (= btp 1) (run-transaction (concat (second aparams) steps) mps ctr) ;; not met
                    (= btp 2) (run-transaction (rest steps) mps ctr))) ;; met
            ;;
            ;; CHOICE
            ;;
            (= '__choice n) (let [;_ (println "CHOICE")
                                   aparams (first aparams)
                                   mps (if new
                                         (concat (take (dec (count mps)) mps) (list {:name '__choice :btp 0 :max (dec (count aparams))} ))
                                         mps)]
                              (run-transaction (concat (list (nth aparams btp)) (rest steps)) mps ctr))
            ;;
            ;; AVOID
            ;;
            (= '__avoid n) (let [;_ (println "AVOID")
                                  res (if new
                                        (reduce (fn [agg n]
                                                  (let [[r m c] (run-transaction (list n) '() -1)
                                                        ;_ (println "avoid cond check: " r)
                                                        ]
                                                    (or agg (true? r))))
                                                false (first aparams))
                                        false)]
                             (if (true? res)
                               [false mps ctr]
                               (run-transaction (rest steps) mps ctr)))

            ;;
            ;; TRANSACT
            ;;

            (= '__transact n) (let [;_ (println "TRANSACT, now continuing with " (first aparams))
                                     [res n-mps n-ctr] (run-transaction (first aparams) mps ctr )]
                                (if (true? res)
                                  (let [mps2 (if (> (count n-mps) (count mps)) n-mps mps)]
                                    (run-transaction (rest steps) mps2 n-ctr))
                                  [res n-mps n-ctr]))
            ;;
            ;; BIND
            ;;
            (= '__bind n) (let [k (first aparams)
                                v (return-id (second aparams))]
                           ; (println " binding node id " v)
                            (intern *ns* '_bindings (assoc (eval '_bindings) k v))

                            (if new
                              (run-transaction (rest steps) (concat mps (list {:name n :btp 0 :max 0})) ctr)
                              (run-transaction (rest steps) mps ctr)))

            :else

            (let [;_ (println "RULE")
                   r ((:rules (eval 'gragra)) n)
                   fparams (:params r)
                   aparams (resolve-consults aparams)]
              (when (nil? r)
                (throw (Exception. (str "rule '" n "' does not exist \n"))))
              (when (not (= (count fparams) (count aparams)))
                (throw (Exception. (str "Wrong number of arguments for rule '" n "' (" (count aparams)" instead of " (count fparams) ")" aparams " \n"))))

              ; match


              (let [reader (:read r)
                    s (zipmap fparams aparams)
                    matches (if (nil? reader)
                              nil
                              (let [;_ (print ".     [matching LHS] ")
                                     cm (match s "" reader)
                                     ;  _ (println ".       [matches found before NACs check: " (count cm) "]")
                                     nacs (filter (fn [x] (= 'NAC (first x))) (:els (second reader)))

                                     mc (if (true? cm) ; empty reader
                                          (if (check-only s nacs)
                                            '()
                                            true)
                                          (check cm s nacs))
                                     ;  _ (println ".         [matches remaining after NACs check: " (count mc) "]" )
                                     mt (if (true? mc)
                                          true
                                          (drop-n mc btp))
                                     ;_ (println ".           [matches remaining after backtracking point (" btp "):" (count mt) "]")])
                                     ]
                                mt))]

                (if (and (not (nil? matches)) (not (true? matches))(zero? (count matches)))
                  (do
                    ;(println ".        [Failure] " [false mps ctr])
                    [false mps ctr])
                  (let [redex (if (or (nil? matches) (true? matches))
                                ""
                                (redex->cypher (first matches)))
                        s (str redex
                               (if (contains? r :delete)
                                 (str (if (= (:theory r) 'dpo)
                                        " DELETE "
                                        " DETACH DELETE ")
                                      (reduce (partial str-sep ", ") (:delete r)))
                                 "")
                               (if (contains? r :create)
                                 (pattern->cypher s :create (:create r) (:read r))
                                 ""))
                        ;_ (print ".     [attempting to rewrite graph:] ")
                        ]

                    (try
                      (when (or (contains? r :create) (contains? r :delete))
                        (do
                          ;(println "MODIFY GRAPH: " s)
                          (dbquery s)))
                      ;(println s "\n.        [Success] mps:" mps " ctr:" ctr)
                      (let [m (if (or (nil? matches) (true? matches)) 0 (count matches))
                            mps (if new
                                  (concat (take (dec (count mps)) mps) (list {:name n :btp 0 :max (dec m)}))
                                  mps)]
                        (run-transaction (rest steps) mps ctr))

                      (catch Exception e
                        (let [msg (.getMessage e)]
                          (println (str ".      [Failed]: " msg))
                          (throw e)
                          [false mps ctr])))))))))))

(defnp track-back [mps ctr]
  (let [n-ctr (dec ctr)]
    (if (< n-ctr 0)
      [mps n-ctr]
      (let [step (nth mps n-ctr)]
        (if (or (some #{(:name step)} ['__transact '__avoid '__until]) (= (:btp step) (:max step)))
          (track-back mps n-ctr)
          (let [n-mps (concat (take n-ctr mps) (list (assoc step :btp (inc (:btp step)))))]
            [n-mps n-ctr]))))))

(defnp transact-iter
  [steps mps iter]
  (begintx)
  (intern *ns* '_ret {})
  (let [_ (println "Transact-iteration: " iter)
        ;_ (println "mps: " (map (fn [s] [(:name s) (:btp s) (:max s)]) mps))
        ;_ (Thread/sleep 500)
        [res r-mps ctr] (run-transaction steps mps -1)]
    (if (true? res)
      (do
        (committx)
        true)
      (do
        (when (< iter 500)
          (do
            (rollbacktx)
            (let [ [n-mps n-ctr] (track-back r-mps ctr)
                   ;_ (println "transaction failed at ctr:" ctr "  !! " r-mps)
                   ;_ (println " new ctr: " n-ctr "  || " n-mps)
                   ]
              (if (< n-ctr 0)
                false
                (transact-iter steps n-mps (inc iter))))))))))





; ---------------------------------------------------
; DSL
; ---------------------------------------------------


(defn transact
  [& steps]
  ['__transact steps])


(defn attempt
  [& steps]
  (transact-iter steps '() 0))

(defn until [test & steps]
  ['__until [test] steps])

(defn choice [ & steps]
  ['__choice steps])

(defn avoid [& steps]
  ['__avoid steps])

(defn apl [r & pars]
  (vec (concat [r] pars)))


(defn node
  "DSL form for specifying a node"
  ([id rest]
   (let [check-syntax (partial check-syntax-generic
                               (str "NODE   :- ( node ID <PROP> ) \n"
                                    "PROP   :- { <LABEL> <ASSERT> } \n"
                                    "LABEL  :- :label *string* \n"
                                    "ASSERT :- :asserts {KEYVAL*} \n"
                                    "KEYVAL :- KEY *string* \n"
                                    "ID     :- *symbol* \n"
                                    "KEY    :- *keyword* \n"))]
     (check-syntax (symbol? id) "ID should be a symbol."))
   ['node (assoc rest :id id)])
  ([id]
   (node id {})))

(defn edge
  "DSL form for specifying an edge"
  [id rest]
  (let [check-syntax (partial check-syntax-generic
                              (str "(Problem with edg-id " id ") \n"
                                   "EDGE   :- ( edge ID <PROP> ) \n"
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
  (let [
         check-syntax (partial check-syntax-generic
                               (str "PATTERN :- ( pattern <MTYPE> ELEM+ ) \n"
                                    "MTYPE   :- :iso | :homo \n"
                                    "ELEM    :- (node ...) | (edge ...) | (NAC ...) | (condition ...) | (assign ...)"))]
    (let [f (first xs)
          r (if (keyword? f) (rest xs) xs)
          m (if (= f :homo) :homo :iso)]
      (if (symbol? f) (check-syntax (valid-schema (s/either :homo :iso) f) "MTYPE must be :iso or :homo") )
      (check-syntax (valid-children #{'node 'edge 'NAC 'cond 'assign} r) "ELEM must be node, edge, NAC, condition or set")
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
     (if (contains? prop :theory) (check-syntax (valid-schema s/Symbol (:theory prop)) "theory must be a symbol"))

     (let [r (assoc prop :params params)
           s (if (not (contains? prop :theory))
               (assoc r :theory 'spo)
               r)]
       ;(validate-rule s)
       (intern *ns* 'gragra (assoc (eval 'gragra) :rules (assoc (:rules (eval 'gragra)) n s) )))
     (let [s (symbol n)]
       (intern *ns* s (fn [& par] (attempt (transact (cons s par) ))))

       (intern *ns* (symbol (str (name n) "-dot")) (fn [] (rule->dot n)))
       ((intern *ns* (symbol (str (name n) "-show")) (fn [] (dot->image (rule->dot n))))))))
  ([n prop]
   (rule n [] prop)))

(defn loadRules [module] 
    (try 
        (let [] 
            (println (str "Attempting to load: " module)) 
            (load-file module)
        )(catch Exception e (println (str "failed to load: " module)))

    )
)

(defn inner_gts [n]
  (let [check-syntax (partial check-syntax-generic
                              (str "GTS    :- (gts ID ) \n"
                                   "ID     :- *symbol* \n"
                                   ))]
    (check-syntax (symbol? n) "gts ID should be a symbol."))
  (intern *ns* 'gragra {:_graph_name n :rules {}})
  (intern *ns* '_ret)
  (intern *ns* '_bindings {})
  (rule 'delete-any-node!
      {:read (pattern (node 'n))
       :delete ['n]})
  ;(loadRules modules)     
  (intern *ns* 'clear! (fn [] (while ((eval 'delete-any-node!)))))
  (println "end of inner_gts")
)

(defn gts "Makes a new GTS system" 
    ([n] (let [] (println "[n]") (inner_gts n)))
    ([n modules] (let [] 
            (println (str "[n modules] -> " modules) ) 
            (println modules) 
            (inner_gts n) 
            (dorun (map loadRules modules))
     ))
)

