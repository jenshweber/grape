(ns grape.core
  (:require
   [clojure.math.combinatorics :as combo]
   [clojure.string :as str]
   [clojure.set :refer [subset? difference]]
   [grape.tx-cypher :refer :all]
   [grape.util :refer :all]
   [grape.analysis :refer :all]
   [grape.tx-cypher :refer :all]
   [environ.core :refer [env]]
   [neo4j-clj.core :as db]
   [schema.core :as s]
   [dorothy.core :as dorothy]
   [clojure.data.codec.base64 :as b64]
   [gorilla-repl.html :as html]
   [taoensso.tufte :as tufte :refer (defnp p profiled profile)])
  (:import (java.net URI)))

(use 'clojure.walk)


; ---------------------------------------------------
; GraphViz Visualization
; ---------------------------------------------------

(import javax.imageio.ImageIO)

(def ctr-atom (atom 0))

(defn reset-ctr! []
  (swap! ctr-atom (fn [c] 0)))

(defn get-ctr! []
  (swap! ctr-atom (fn [c] (+ 1 c)))
  (deref ctr-atom))

(defn dot->render [g]
  (dorothy/render g {:format :png}))

(defn dot->image [g]
  (String. (b64/encode (dorothy/render g {:format :png}))))

;; (defn show [g]
;;   (image/image-view
;;    (ImageIO/read
;;     (io/input-stream
;; ;  (dorothy/save! (dorothy/render  g {:format :png}) "out.png")
;;      (dorothy/render  g {:format :png})))))

(defn show [g]
  (html/html-view
 ;      (str "<img style=\"width:100%\">"                  
   (dorothy/render  g {:format :svg})
   ;         "</img>")
   ))



(defn asserts->dot [as]
  "Translate a map of assertions to Dot"
  (if (empty? as)
    ""
    (str/join "; " (map (fn [[k v]]
                          (str (name k) "=" (if (str/starts-with? v "&")
                                              (subs v 1 (count v))
                                              v)))
                        as))))

(defn node->dot [n c o]
  (let [p (second n)
        handle (name (:id p))
        dhandle (if (str/starts-with? handle "_") "" handle)
        l (:label p)
        oid (:oid p)
        as (:asserts p)
        o (if (:opt p) " style=dashed "
              (if (:merge p)
                " style=diagonals "
                o))]
    (str " "
         handle " [color=" c " shape=record penwidth=bold  " o " "
         "label=\"{"
         dhandle (if (nil? l) "" (str ":" l))
         (if (nil? oid) "" (str ":OID(" oid ")"))
         (if (empty? as)
           " "
           (str " | " (asserts->dot as)))
         " }\"]; ")))


(defn edge->dot [e c o]
  (let [p (second e)
        src (name (:src p))
        tar (name (:tar p))
        l (:label p)
        as (:asserts p)
        o (if (:opt p) " style=dashed " o)]
    (str " " src " -> " tar
         " [color=" c " penwidth=bold len=2  fontcolor=" c " " o
         " label=\"" l
         (if (empty? as)
           ""
           (str "\n{" (asserts->dot as) "}"))
         "\" ]")))

(defn path->dot [e c o]
  (let [p (second e)
        src (name (:src p))
        tar (name (:tar p))]
    (str " " src " -> " tar
         " [color=\"black:invis:black\" len=8 fontcolor=" c " " o
         " label=\"" (:exp p) "\" ]")))

(declare pattern->dot)

(defn NAC->dot
  "translate a NAC to dot"
  [nac]
  (let [c "darkviolet"
        p (nth nac 2)]
    (pattern->dot p [] c c " style=filled arrowhead=normal fillcolor=violet")))

(defn cond->dot
  "translate a condition to dot"
  [c]
  (str " "
       ;(get-ctr!)
       " cond [ shape=oval penwidth=0 style=filled fillcolor=khaki1 label=\"" (second c) "\"]"))

(defn assign->dot
  "translate an assignment to dot"
  [c]
  (str " "
       (get-ctr!)
       " [color=seagreen1 style=filled shape=none label=\"Assign: " (second c) "\"]"))


(defn graphelem->dot [d c1 c2 o e]
  "Translate a graph element to dorothy - either node or edge"
  (let [t (first e)
        id (:id (second e))
        c (if (nil? (some #{id} d)) c1 c2)]
    (cond
      (= 'node t) (node->dot e c o)
      (= 'edge t) (edge->dot e c o)
      (= 'path t) (path->dot e c o)
      (= 'NAC t) (NAC->dot e)
      (= 'cond t) (cond->dot e)
      (= 'assign t) (assign->dot e)

      :else
      (throw (Exception. "Invalid graph element")))))


(defn pattern->dot
  "translate a graph pattern to dot"
  [p d c1 c2 o]
  (let [m    (second p)
        els  (:els m)
        flag (if (= :iso (:sem m))
               (str (random-id) " [ fontcolor=blue shape=none fontsize=\"30pt\" label=<&#x21a3;>] "))]
    (if (nil? els)
      ""
      (str flag (reduce str (map (partial graphelem->dot d c1 c2 o) els))))))


(defn rule->dot [rid rule]
  "translate a rule to dot"
  (reset-ctr!)
  (let [n (name rid)
        r (:read rule)
        d (:delete rule)
        c (:create rule)
        p (:params rule)
        g (:gcond rule)]
    (str "digraph g { plines=true overlap=false subgraph cluster0 {  "
         "label=\"Rule: " n " " (str p) " \";"
         (when (not (empty? g))
           (str " _cond [shape=egg style=filled penwidth=0 fontcolor=black fillcolor=khaki1 label=\""
                (apply str (interpose "," (map name g)))
                " \"] "))
         (pattern->dot r d "black" "red" "")
         (pattern->dot c [] "x11green" "x11green" "")
         "}}")))

(defn query->dot [rid query]
  "translate a query to dot"
  (let [n (name rid)
        r (:read query)
        p (:params query)]
    (str "digraph g {  splines=true overlap=false subgraph cluster0 {"
         "label=\"Query: " n (if (empty? p) "" (str p)) "\";"
         (pattern->dot r [] "black" "black" "")
         "}}")))


(defn constraint->dot [cid con]
  "translate a constraint to dot"
  (let [n (name cid)
        i (:if con)
        t (:then con)
        p (:params con)]
    (str "digraph g {  splines=true overlap=false subgraph cluster0 {  "
         "label=\"Constraint: " n " " (str p) "\";"
   ;      " _cond [fontsize=24 fontcolor=blue shape=none label=\"&#x21aa;\"] "
         (pattern->dot i [] "black" "black" "")
         (pattern->dot t [] "blue" "blue" "")
         "}}")))


; ---------------------------------------------------
; ENGINE
; ---------------------------------------------------

(def dburi (env :db-url))
(def dbusr (env :db-usr))
(def dbpw (env :db-pw))

(def rules-atom (atom {}))
(def queries-atom (atom {}))
(def constraints-atom (atom '{}))
(def unit-stack-atom (atom ()))
(def unit-policy-atom (atom {:check true, :fail "SILENT", :stack-size 100}))
(def suppressed-atom (atom '()))

(def debug-atom (atom false))

(defn debug! [b]
  (swap! debug-atom (fn [_] b)))

(defn debug? []
  (deref debug-atom))

(defn debug [f & s]
  (if (debug?)
    (apply f s)))


(def checkeq-atom (atom true))

(defn checkeq! [b]
  (swap! checkeq-atom (fn [_] b)))

(defn checkeq? []
  (deref checkeq-atom))

(def trackreads-atom (atom true))

(defn trackreads! [b]
  (swap! trackreads-atom (fn [_] b)))

(defn trackreads? []
  (deref trackreads-atom))

(defn rules []
  (deref rules-atom))

(defn queries []
  (deref queries-atom))

(defn constraints []
  (deref constraints-atom))

(defn suppress [& args]
  (swap! suppressed-atom (fn [_] args)))

(defn suppressed []
  (deref suppressed-atom))

(defn set-grape! [g]
  (intern 'grape.core '_ g))

(set-grape! "nil")

(defn add-rule! [n s]
  (swap! rules-atom (fn [c] (assoc c n s))))

(defn add-query! [n s]
  (swap! queries-atom (fn [c] (assoc c n s))))

(defn add-empty-unit-frame [name]
  (let [ss ((deref unit-policy-atom) :stack-size)
        new-elem (list {:name name :pre nil :post nil})]
    (swap! unit-stack-atom
           (fn [S] (concat
                    new-elem
                    (if
                     (>= (count S) ss)
                      (butlast S) ;; drop off last element
                      S))))))

(defn unit-stack [] (deref unit-stack-atom))
(defn unit-stack-reset[] (swap! unit-stack-atom (fn [_] ())))
(defn last-unit [] (first (deref unit-stack-atom)))

(defn pre? [] ((last-unit) :pre))

(defn post? [] ((last-unit) :post))

(defn unit-set-pre [result]
  (let [new-elem (list (assoc (last-unit) :pre result))]
    (swap! unit-stack-atom
           (fn [S] (concat new-elem (rest S))))))

(defn unit-set-post [result]
  (let [new-elem (list (assoc (last-unit) :post result))]
    (swap! unit-stack-atom
           (fn [S] (concat new-elem (rest S))))))

(defn unit-policy? [] (deref unit-policy-atom))

(defn set-unit-failure-policy 
  "Set the failure policy for unit pre- and post-conditions.
  If set to SILENT, then unit execution proceeds, but flags
  are set (access via unit-pre? and unit-post?).
  If set to FAIL, then if pre- or post-conditions are violated,
  an exception will be thrown.
  Default behaviour is SILENT."
  [policy]
  (swap! unit-policy-atom (fn [p] (assoc p :fail policy))))

(defn set-unit-check-policy
  "Set the policy for checking pre- and post-conditions
  for a unit. Setting to true will enable checking and setting
  to false will disable checking. Disabling checking improves
  performance. Checking is enabled by default. If a condition check
  fails, then the unit failure policy is followed."
  [policy]
  (swap! unit-policy-atom (fn [p] (assoc p :check policy))))

(defn unit-should-check? [] ((deref unit-policy-atom) :check))

(defn unit-should-fail? [] 
  (and 
  (unit-should-check?)
  (= ((deref unit-policy-atom) :fail) "FAIL")))

(defn set-unit-stack-size [ss]
  (do
    (swap! unit-stack-atom (fn [S] (take ss S)))
    (swap! unit-policy-atom (fn [P] (assoc P :stack-size ss)))))

(defn add-constraint! [n s]
  (swap! constraints-atom (fn [c] (assoc c n s))))

(def conn
  (do
    (try
      (-> (db/connect (URI. "bolt://neo4j:7687")
                      "neo4j"
                      "grapevine")
          db/get-session)
      (catch Exception e (println (str "Caught exception: " (.toString e))))
      (finally "UNSUCCESSFUL"))))

(defn dbquery [q]
  (debug println "DBQuery: " q)
  (let [qf (db/create-query q)
        res (try (qf conn)
                 (catch Exception e (println (str "Caught exception: " (.toString e))))
                 (finally "UNSUCCESSFUL"))]
    (debug println "************* done \n")
    res))

(dbquery "CREATE CONSTRAINT IF NOT EXISTS FOR (g:__Graph) REQUIRE g.uid IS UNIQUE")
(dbquery "CREATE CONSTRAINT IF NOT EXISTS FOR (g:__Graph) REQUIRE g.tag IS UNIQUE")
(dbquery "create index if not exists for  (g:__Graph) on g._fp")
(dbquery "create index if not exists for  (g:Graph) on g.uid")

; ---------------------------------------------------
; DSL
; ---------------------------------------------------


(defn resolve-value [scope s]
  (if (empty? s) ""
      (let [[m par] (re-find #"(?:&)(\w+)" s)]
        (if (nil? par)
          s
          (resolve-value scope (clojure.string/replace s m (str (scope (symbol par)))))))))

(defn asserts->cypher [s as]
  "Translate a map of assertions to a Cypher code fragment"
  (if (empty? as)
    ""
    (str " {" (reduce (partial str-sep ", ") (map (fn [[k v]] (str (name k) ":" (resolve-value s v))) as)) "}")))

(defn readnode->cypher [n pars]
  (let [handle (-> n second :id)
        label (resolve-value pars (-> n second :label))
        opt (-> n second :opt)
        ass (-> n second :asserts)]
    (str
     (if (true? opt)
       "optional "
       " ")
      "match ("
     handle
     (if (empty? label) ":__Node" (str ":__Node:" label))
     (asserts->cypher pars ass)
     ")"
     "WHERE ID(" handle ") IN _g.active ")))

(defn conditions->cypher [c scope]
  (resolve-value scope (second c)))

(defn readnode->cypher2 [n]
  (let [
        handle (-> n second :id)]
    (str  " create (" handle ")"
          "<-[:read{handle:'" handle "'}]-(_gn) ")))

(defn createnode->cypher [n pars]
  (let [handle (-> n second :id)
        label (-> n second :label)
        ass (-> n second :asserts)]
    (str " create (" handle ":" label ":__Node"
         (asserts->cypher pars ass)
         ")"
         "<-[:create{handle:'" handle "'}]-(_gn)  "
         (if (checkeq?)
           (str "set " handle "._fp=apoc.hashing.fingerprint(" handle ") ")
           " "))))


(defn readedge->cypher [e]
  (let [p (second e)]
    (str
     (if (true? (:opt p))
       " optional match ("
       " match (") 
     (:src p) ")<-[" (:id p) "_s:src]-(" (:id p) ":`__Edge`"
     (if (empty? (:label p)) "" (str ":" (:label p))) ")-[" (:id p) "_t:tar]->(" (:tar p) ")"
     " WHERE ID(" (:id p) ") IN _g.active ")))

(defn createedge->cypher [enodes e]
  (let [p (second e)
        handle (:id p)
        source (:src p)
        target (:tar p)]
    (if (and (some #(= source %) enodes)
             (some #(= target %) enodes))
      (str
       " CREATE ( " handle " :`__Edge`:" (:label p) ") <-[:create{handle:'" handle "'}]-(_gn)"
       " CREATE ( " target " )<-[:tar]-(" handle ") -[:src]->(" source " )"

       " SET " handle ".src= ID(" source "), "
       handle ".tar=ID(" target ") "
       (if (checkeq?)
         (str
          ", "
          handle "._fps=" source "._fp, "
          handle "._fpt=" target "._fp, "
          handle "._fpd= apoc.coll.different([ ID( " source "), ID(" target ")]) "
          " SET "
          handle "._fp=apoc.hashing.fingerprint("  handle
          ", ['tar', 'src']) ")
         " "))
      " ")))

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
          f (fn [x] (str " ID(" (first x) ")<> ID(" (second x) ")"))
          ni (map f nc)
          ei (map f ec)]
      (reduce (partial str-sep " AND ") (concat ni ei)))))

(defn- exclothers [others ditem]
  (map #(str " ( ID(" ditem ")<> ID(" % ") OR " ditem " IS NULL OR " % " IS NULL)") others))

(defn gen-identification-condition [itemsToDelete nodes edges]
  (if (empty? itemsToDelete)
    ""
    (let [others (difference (set (concat (all-ids nodes) (all-ids edges))) (set itemsToDelete))
          ecls (map #(exclothers others %) itemsToDelete)
          cstr (apply str (interpose " AND " (flatten ecls)))]
      (if (empty? cstr) "" (str " WITH * WHERE " cstr)))))

(defn readnodeRet->cypher [n]
  (let [id (-> n second :id)]
    (str id "{.*, "
         "id: ID(" id "), "
         "labels:labels(" id ")} ")))

(defn third [v] (nth v 2))

(defn gen-constraint-new
  [newitems olditems]
  (if (or (< (count newitems) 1) (< (count olditems) 1))
    '()
    (let [ec (combo/cartesian-product (all-ids newitems) (all-ids olditems))
          ;_ (println "\n\n **** " nc " --- " ec " \n\n ")
          f (fn [x] (str " ID(" (first x) ")<> ID(" (second x) ")"))]
      (map f ec))))

(defn nac->cypher [nac bnodes bedges scope]
  (if (nil? nac)
    " "
    (let [nnodes (filter-elem 'node (-> nac third second :els))
          nedges (filter-elem 'edge (-> nac third second :els))
          nnodesStr (apply str (interpose " WITH * " (map #(readnode->cypher % scope) nnodes)))
          nedgesStr (apply str (interpose " WITH * " (map readedge->cypher nedges)))
          cstr (apply str (interpose " AND " (concat (gen-constraint-new nnodes bnodes)

                                                     (gen-constraint-new nedges bedges))))
          wstr (apply str
                      (interpose ", "
                                 (concat (list "_g")
                                  (map (fn [x] (-> x second :id str))
                                       (concat bnodes bedges)))))]
      (str " WITH * call { with " wstr " "
           nnodesStr " WITH * " nedgesStr
           (if (empty? cstr)
             " "
             (str " with * where  " cstr))
           " return  count(*) as _nac } with * where _nac=0 "))))



(defn- search-if [g n pars]
  (let [r ((constraints) n)
        scope (zipmap (:params r) pars)
        nodesToRead (filter-elem 'node (-> r :if second :els))
        nodesToReadStr (apply str (interpose " WITH * " (map #(readnode->cypher % scope) nodesToRead)))
        nodesToReturnStr (apply str (interpose "," (map readnodeRet->cypher nodesToRead)))
        edgesToRead (filter-elem 'edge (-> r :if second :els))
        edgesToReadStr (apply str (interpose " WITH * " (map readedge->cypher edgesToRead)))
        edgesToReturnStr (apply str (interpose "," (map readnodeRet->cypher edgesToRead)))
        conditions (filter-elem 'cond (-> r :if second :els))
        conditionsStr (apply str (interpose " AND " (map #(conditions->cypher % scope) conditions)))
        qstr (str "MATCH (_g:`__Graph` {uid:\"" g "\"}) "
                  " WITH _g "
                  nodesToReadStr
                  " WITH * " edgesToReadStr

                  (let [st (gen-constraint-isomorphism nodesToRead edgesToRead)]
                    (if (empty? st)
                      ""
                      (str " WITH * WHERE " st)))

                  (if (empty? conditionsStr)
                    ""
                    (str " WITH * WHERE " conditionsStr))

                  " RETURN _g "
                  (if (not (empty? nodesToReturnStr)) (str "," nodesToReturnStr))
                  (if (not (empty? edgesToReturnStr)) (str "," edgesToReturnStr)))]
    (p ::search (dbquery qstr))))

(defn omit_ [s]
  (if (str/starts-with? s "_") "" s))

(defn getHandle [e]  (-> e first name omit_))

(defn getHandle- [e]  (-> e first name))

(defn getId [e]  (-> e second :id))

(defn getSrc [e]  (-> e second :src))
(defn getTar [e]  (-> e second :tar))

(defn getTag [e]  (-> e second :tag))

(defn getLabels [e]  (-> e second :labels))

(defn getAttrs [e]  (apply dissoc
                           (-> e second
                               (dissoc :labels :id :src :tar :handle
                                       :_fp :_fps :_fpt :_fpd))
                           (suppressed)))


(defn nodeRem->cypher [n]
  (let [id   (getId n)
        handle (getHandle- n)]
    (str  " MATCH(" handle ") WHERE ID( " handle ")=" id)))

(defn check-one-then [g n pars redex]
  (let [r ((constraints) n)
        scope (zipmap (:params r) pars)
        nodesToRematch (filter (fn [x] (some #(= "__Node" %) (-> x second :labels))) redex)
        nodesToRematchStr (apply str (map nodeRem->cypher nodesToRematch))
        ifnodes (filter-elem 'node (-> r :if second :els))
        ifedges (filter-elem 'edge (-> r :if second :els))
        edgesToRematch (filter (fn [x] (some #(= "__Edge" %) (-> x second :labels))) redex)
        edgesToRematchStr (apply str (map nodeRem->cypher edgesToRematch))
        nodesToRead (filter-elem 'node (-> r :then second :els))
        nodesToReadStr (if (empty? nodesToRead)
                         ""
                         (str " WITH * " (apply str (interpose " WITH * " (map #(readnode->cypher % scope) nodesToRead)))))
        edgesToRead (filter-elem 'edge (-> r :then second :els))
        edgesToReadStr (apply str (interpose " WITH * " (map readedge->cypher edgesToRead)))
        conditions (filter-elem 'cond (-> r :then second :els))
        conditionsStr (apply str (interpose " AND " (map #(conditions->cypher % scope) conditions)))

        qstr (str "MATCH (_g:`__Graph` {uid:\"" g "\"}) WITH * "
                  nodesToRematchStr
                  " WITH * " edgesToRematchStr
                  " WITH * "
                  nodesToReadStr
                  " "
                  edgesToReadStr
                  " "
                  (let [st (gen-constraint-isomorphism
                            (concat nodesToRead ifnodes)
                            (concat edgesToRead ifedges))]
                    (if (empty? st)
                      ""
                      (str " WITH * WHERE " st)))

                  (if (empty? conditionsStr)
                    ""
                    (str " WITH * WHERE " conditionsStr))
                  " RETURN _g {.uid}")]
    (not (empty? (dbquery qstr)))))







(defn search-and-derive [g n limit pars]
  (let [_ (debug println "*** SEARCH & DERIVE ")
        r ((rules) n)
        scope (zipmap (:params r) pars)
        nodesToRead (filter-elem 'node (-> r :read second :els))
        nodesToReadStr (apply str (interpose " WITH * " (map #(readnode->cypher % scope) nodesToRead)))
        nodesToReadStr2 (apply str (interpose " WITH * " (map readnode->cypher2 nodesToRead)))

        edgesToRead (filter-elem 'edge (-> r :read second :els))
        edgesToReadStr (apply str (interpose " WITH * " (map readedge->cypher edgesToRead)))
        conditions (filter-elem 'cond (-> r :read second :els))
        conditionsStr (apply str (interpose " AND " (map #(conditions->cypher % scope) conditions)))
        nacToCheck (nac->cypher (first (filter-elem 'NAC (-> r :read second :els)))
                                nodesToRead
                                edgesToRead
                                scope)
        itemsToDelete (:delete r)
        qstr (str "MATCH (_g:`__Graph` {uid:\"" g "\"}) "
                  " WITH * "
                  nodesToReadStr
                  " WITH * " edgesToReadStr

                  (if (some #{'INJ} (:gcond r))
                    (let [st (gen-constraint-isomorphism nodesToRead edgesToRead)]
                      (if (empty? st)
                        ""
                        (str " WITH * WHERE " st)))

                    (if (or (some #{'ID} (:gcond r)) (some #{'GLUE} (:gcond r)))
                      (gen-identification-condition itemsToDelete nodesToRead edgesToRead)
                      ""))

                  nacToCheck

                  (if (empty? conditionsStr)
                    ""
                    (str " WITH * WHERE " conditionsStr " "))
                  
                  (if (> limit 0)
                    (str " WITH  * LIMIT " limit " ")
                    " "
                  ))

        ; **** SEARCH PART DONE

        itemsDeleteStr (if (empty? (:delete r))
                         " "
                         (str " WITH * " (apply str (interpose " WITH * " (map delitem->cypher (:delete r))))))
        nodesToCreate (filter-elem 'node (-> r :create second :els))
        nodesToCreateStr (if (empty? nodesToCreate)
                           ""
                           (str " WITH * " (apply str (interpose " WITH * " (map #(createnode->cypher % scope) nodesToCreate)))))
        edgesToCreate (filter-elem 'edge (-> r :create second :els))
        existingNodes (concat (map #(-> % second :id) nodesToRead)
                              (map #(-> % second :id) nodesToCreate))
        edgesToCreateStr (if (empty? edgesToCreate)
                           ""
                           (str " WITH * " (apply str (interpose " WITH * " (map (partial createedge->cypher existingNodes) edgesToCreate)))))

        createdEls (concat (map #(-> % second :id) nodesToCreate)
                           (map #(-> % second :id) edgesToCreate))
        createdElsIDStr (apply str (interpose "," (map #(str "ID(" % ")") createdEls)))

        deletedElsIDStr (apply str (interpose "," (map #(str "ID(" % ")") (:delete r))))

        qstr2 (str "with * CREATE (_gn:`__Graph`{uid: randomUUID()})-[:prov{rule:\"" n "\"}]->(_g) "
                   " WITH * "
                   (if (trackreads?)
                     nodesToReadStr2
                     " ")
                   nodesToCreateStr
                   edgesToCreateStr
                   itemsDeleteStr

                   (if (empty? (:delete r)) ""
                       (if (or (some #{'DANG} (:gcond r)) (some #{'GLUE} (:gcond r)))
                         (str
                          " with * call { with _gn match(_gn)-[:delete]->(_nd:`__Node`)<-[:tar]-(_e1) RETURN collect(_e1) as _cont UNION "
                          " match(_gn)-[:delete]->(_nd:`__Node`)<-[:src]-(_e2) RETURN collect(_e2) as _cont } "
                          " with * call { with _gn match(_gn)-[:delete]->(_ed:`__Edge`) RETURN collect(_ed) as _deled } "
                          " with * match(_gn) where _deled = _cont ")
                         (str
                          " with * call { with _gn match(_gn)-[:delete]->(_nd:`__Node`)<-[:tar]-(_e) "
                          " merge (_gn)-[:delete]->(_e) } "
                          " with * call { with _gn match(_gn)-[:delete]->(_nd:`__Node`)<-[:src]-(_e) "
                          " merge (_gn)-[:delete]->(_e) } ")))

                   (str " with * "
                        " set _gn.active = apoc.coll.unionAll(apoc.coll.removeAll(_g.active,[" deletedElsIDStr "]),[" createdElsIDStr "]) ")
                   (if (checkeq?)
                     (str
                      "with _gn optional match (_el) where ID(_el) in _gn.active "
                      " with _gn, collect (_el._fp) as _fps "
                      " set _gn._fps = apoc.coll.sort(_fps)"
                      " with _gn set _gn._fp=apoc.hashing.fingerprint (_gn, ['uid', 'active']) "
                      "with _gn optional match (_gconf:`__Graph`{`_fp`:_gn.`_fp`}) "
                      " where  ID(_gn) <> ID(_gconf) and exists ((_gconf)-[:prov*0..]->()<-[:prov*0..]-(_gn) )"
                      " create (_gn)-[:conf]->(_gconf)")
                     " ")


                   " RETURN distinct _gn {.uid}")]
    (->>
     (dbquery (str qstr qstr2))
     (map :_gn)
     (map :uid))))


(defn search-and-derive-par [g n limit pars]
  (let [_ (debug println "*** SEARCH & DERIVE PARALLEL")
        r ((rules) n)
        scope (zipmap (:params r) pars)
        nodesToRead (filter-elem 'node (-> r :read second :els))
        nodesToReadStr (apply str (interpose " WITH * " (map #(readnode->cypher % scope) nodesToRead)))
        nodesToReadStr2 (apply str (interpose " WITH * " (map readnode->cypher2 nodesToRead)))

        edgesToRead (filter-elem 'edge (-> r :read second :els))
        edgesToReadStr (apply str (interpose " WITH * " (map readedge->cypher edgesToRead)))
        conditions (filter-elem 'cond (-> r :read second :els))
        conditionsStr (apply str (interpose " AND " (map #(conditions->cypher % scope) conditions)))
        nacToCheck (nac->cypher (first (filter-elem 'NAC (-> r :read second :els)))
                                nodesToRead
                                edgesToRead
                                scope)
        itemsToDelete (:delete r)
        qstr (str "MATCH (_g:`__Graph` {uid:\"" g "\"}) "
                  " WITH * "
                  nodesToReadStr
                  " WITH * " edgesToReadStr

                  (if (some #{'INJ} (:gcond r))
                    (let [st (gen-constraint-isomorphism nodesToRead edgesToRead)]
                      (if (empty? st)
                        ""
                        (str " WITH * WHERE " st)))

                    (if (or (some #{'ID} (:gcond r)) (some #{'GLUE} (:gcond r)))
                      (gen-identification-condition itemsToDelete nodesToRead edgesToRead)
                      ""))

                  nacToCheck

                  (if (empty? conditionsStr)
                    ""
                    (str " WITH * WHERE " conditionsStr " "))

                  (if (> limit 0)
                    (str " WITH  * LIMIT " limit " ")
                    " "))

        ; **** SEARCH PART DONE

        itemsDeleteStr (if (empty? (:delete r))
                         " "
                         (str " WITH * " (apply str (interpose " WITH * " (map delitem->cypher (:delete r))))))
        nodesToCreate (filter-elem 'node (-> r :create second :els))
        nodesToCreateStr (if (empty? nodesToCreate)
                           ""
                           (str " WITH * " (apply str (interpose " WITH * " (map #(createnode->cypher % scope) nodesToCreate)))))
        edgesToCreate (filter-elem 'edge (-> r :create second :els))
        existingNodes (concat (map #(-> % second :id) nodesToRead)
                              (map #(-> % second :id) nodesToCreate))
        edgesToCreateStr (if (empty? edgesToCreate)
                           ""
                           (str " WITH * " (apply str (interpose " WITH * " (map (partial createedge->cypher existingNodes) edgesToCreate)))))

        qstr2 (str "with * CREATE (_gn:`__Graph`{uid: randomUUID()})-[:prov{rule:\"" n "\"}]->(_g) "
                   " WITH * "
                   (if (trackreads?)
                     nodesToReadStr2
                     " ")
                   nodesToCreateStr
                   edgesToCreateStr
                   itemsDeleteStr

                   (if (empty? (:delete r)) ""
                       (if (or (some #{'DANG} (:gcond r)) (some #{'GLUE} (:gcond r)))
                         (str
                          " with * call { with _gn match(_gn)-[:delete]->(_nd:`__Node`)<-[:tar]-(_e1) RETURN collect(_e1) as _cont UNION "
                          " match(_gn)-[:delete]->(_nd:`__Node`)<-[:src]-(_e2) RETURN collect(_e2) as _cont } "
                          " with * call { with _gn match(_gn)-[:delete]->(_ed:`__Edge`) RETURN collect(_ed) as _deled } "
                          " with * match(_gn) where _deled = _cont ")
                         (str
                          " with * call { with _gn match(_gn)-[:delete]->(_nd:`__Node`)<-[:tar]-(_e) "
                          " merge (_gn)-[:delete]->(_e) } "
                          " with * call { with _gn match(_gn)-[:delete]->(_nd:`__Node`)<-[:src]-(_e) "
                          " merge (_gn)-[:delete]->(_e) } ")))

; start of merging 
                   "with distinct collect(ID(_gn)) as _gids, _g as _gp "
                   " call { with _gp create (_gn:__Graph {uid: randomUUID()})"
                   "-[:prov{rule:'" (str n "*" (if (> limit 0) (str "<n" limit) "")) "'}]->(_gp)"
                   "return _gn } "
                   "  with * match (_g:__Graph) where ID(_g) IN _gids  "
              " and not EXISTS {match (g1:`__Graph`)-[:read]->(e)<-[:delete]-(g2:`__Graph`) where ID(g1) in _gids " 
              "                 and ID(g2) in _gids and g1 <> g2 } "
                   " with  _g, _gn, _gp call { with  _gn, _gp merge (_gn)-[:prov{rule:'" (str n "*" (if (> limit 0) (str "<n" limit) "")) "'}]->(_gp) }"
                   (if (trackreads?)
                     (str
                      " with * call { with _g, _gn match (_g)-[:read]->(read) "
                      "                                with distinct _gn, read create (_gn)-[:read]->(read)}   ")
                     " ")
                   " with * call { with _g, _gn match (_g)-[:delete]->(deleted) "
                   (if (trackreads?)
                     "                   with distinct _gn, deleted create (_gn)-[:delete]->(deleted) return collect(ID(deleted)) as dels }"
                     "                   with distinct deleted return collect(ID(deleted)) as dels }")
                   " with  _g, _gn, _gp, dels call {with _g, _gn  match (_g)-[:create]->(created) "
                   (if (trackreads?)
                     "                   with distinct _gn, created create (_gn)-[:create]->(created) return collect(ID(created)) as creas }"
                     "                   with created return collect(ID(created)) as creas }")
                   " with _g, _gn, _gp, dels, creas call { with _g detach delete _g }"
                   " with _gn, _gp, collect(dels) as delss, collect(creas) as creass "
                   " set _gn.active=apoc.coll.unionAll(apoc.coll.removeAll(_gp.active, "
                   "                              apoc.coll.flatten(delss)),apoc.coll.flatten(creass)) "
                   (if (checkeq?)
                     (str
                      " with _gn optional match (_gn) -[:prov*0..]->()-[:create]->(_el) "
                      " with _gn, collect (_el._fp) as _fps "
                      " with _gn, _fps "
                      " set _gn._fps = apoc.coll.sort(_fps) "
                      " with _gn set _gn._fp=apoc.hashing.fingerprint (_gn, ['uid', 'active']) ")
                     " ")

                   " RETURN distinct _gn {.uid}")]
    (->>
     (dbquery (str qstr qstr2))
     (map :_gn)
     (map :uid))))




(defn- get-inv [g]
  (debug println "*** CHECK INVARIANTS ")
  (->
   (dbquery (str "MATCH (g:`__Graph`{uid:\"" (first g) "\"})-[:prov*0..]->(gs) "
                 " with gs optional match (gs)-[_inve]->(inve) "
                 " with * optional match (gs)-[:_inva]->(inva) "
                 " return collect(inve.name) as enforced, "
                 " collect(inva.name) as asserted "))
   first))


(defn enforce- [g constrs]
  (loop [gc g
         cs constrs]
    (if (empty? cs)
      gc
      (recur ((first cs) gc) (rest cs)))))

(defn- check-invariants [gr]
  (let [invs (get-inv gr)
        ge (enforce- gr (map #(-> % symbol eval)
                             (:enforced invs)))]
    (doseq [g ge]
      (doseq [c (:asserted invs)]
        (if (empty? ((eval (symbol c)) (list g)))
          (do
            (set-grape! (list g))
            (throw (AssertionError.
                    (str "Asserted invariant '" c "' violated for graph " g)))))))
    ge))


(defn removeConfluent- [g]
  (not (empty? (dbquery (str "MATCH (g:`__Graph` {uid:'" g "'})"
                             "-[:conf]->(g2)-[:prov*0..]->(g0)<-[:prov*0..]-(g) "
                             " with * match (g) -[:create]->(n) detach delete g,n "
    ;                         " WHERE EXISTS ((g)-[:prov*1..]->(g2)) "
                             " return g2.uid")))))

(defn removeConfluent-loc- [g gr]
  (not (empty? (dbquery (str "MATCH (g:`__Graph` {uid:'" g "'})"
                             "<-[:conf]-(g2:`__Graph`) where g2.uid in [" gr "] "
                             " with * match (g) -[:create]->(n) detach delete g,n "
                             " return g2.uid")))));


(defn removeConfluent [gr]
  (filter #(not (removeConfluent- %)) gr))


(defn removeConfluent-loc [gr]
  (let [grs (apply str (interpose " , " (map #(str "'" % "'") gr)))]
    (filter #(not (removeConfluent-loc- % grs)) gr)))


;(defn- exec- [g n par]
;  (let [redexes (search g n par)]
;    (let [gns (map (partial derive- g n par) redexes)
;          gn (reduce concat gns)]
;      gn)))


(defn exec-all [gs n limit par]
  (let [res (map #(search-and-derive % n limit  par) gs)
        gn (reduce concat res)
        gne (check-invariants gn)]
    (if (not (empty? gne))
      (set-grape! gne))
    gne))

(defn exec= [gs n limit par]
  (let [_ (debug println "\n\n***************************"
                 "\n**** EXECUTING RULE: " n " - non-distinct mode (=)"
                 "\n***************************\n")
        gne (exec-all gs n limit par)]
    gne))

(defn exec [gs n par]
  (let [_ (debug println "\n\n***************************"
                 "\n**** EXECUTING RULE: " n " - distinct mode "
                 "\n***************************\n")
        m (checkeq?)
        _  (checkeq! true)
        gne (exec-all gs n 0 par)]
    (checkeq! m)
    (removeConfluent-loc gne)
    ))

(defn meld [gs]
  (let [
        gss (str "[ "
                 (apply str (interpose ", " (map (fn [x] (str "\"" x "\"")) gs)))
                 "]")
        qstr (str
              " create (_gn:__Graph {uid: randomUUID()}) "
              " with * "
              " match (_g:`__Graph`)-[_p:prov]->(_gp) where _g.uid in " gss
           ;   " and not EXISTS {match (g1:`__Graph`)-[:read]->(e)<-[:delete]-(g2:`__Graph`) where g1.uid in " gss
           ;   "                 and g2.uid in " gss " and g1 <> g2 } "
              " with * merge (_gn)-[:prov{rule:_p.rule + '*'}]->(_gp) "
              " with distinct _g, _gn, _gp call { with _g, _gn match (_g)-[:read]->(read) "
              "                                with distinct _gn, read create (_gn)-[:read]->(read)}   "
              " with  _g, _gn, _gp call { with _g, _gn match (_g)-[:delete]->(deleted) "
              "                                with distinct _gn, deleted create (_gn)-[:delete]->(deleted) return collect(ID(deleted)) as dels }"
              " with  _g, _gn, _gp, dels call {with _g, _gn  match (_g)-[:create]->(created) "
              "                                with distinct _gn, created create (_gn)-[:create]->(created) return collect(ID(created)) as creas }"
              " with _g, _gn, _gp, dels, creas call { with _g detach delete _g }"
              " with _gn, _gp, collect(dels) as delss, collect(creas) as creass "
              " set _gn.active=apoc.coll.unionAll(apoc.coll.removeAll(_gp.active, "
              "                              apoc.coll.flatten(delss)),apoc.coll.flatten(creass)) "
              (if (checkeq?)
                (str
                 " with _gn optional match (_gn) -[:prov*0..]->()-[:create]->(_el) "
                 " with _gn, collect (_el._fp) as _fps "
                 " with _gn, _fps "
                 " set _gn._fps = apoc.coll.sort(_fps) "
                 " with _gn set _gn._fp=apoc.hashing.fingerprint (_gn, ['uid']) ")
                " ")
              " return distinct _gn ")]
    (-> (dbquery qstr) first :_gn :uid list)))

;(defn- exec*- [g n par]
;  (let [gns (search-and-derive g n par true)]
;    gns))



(defn- exec*- [g n limit par]
  (let [gns (search-and-derive-par g n limit par)]
    gns))




(defn exec* [gs n limit par]
  (let [_ (debug println "\n\n***************************"
                 "\n**** EXECUTING RULE: " n " - parallel mode (*)"
                 "\n***************************\n")
        res (map #(exec*- % n limit par) gs)
        gn (reduce concat res)
        gne (check-invariants gn)]
    (if (not (empty? gne))
      (set-grape! gne))
    gne))

(defn- exec-query- [g n pars]
  (let [r ((queries) n)
        _  (debug println "*** exec query- ")
        scope (zipmap (:params r) pars)
        nodesToRead (filter-elem 'node (-> r :read second :els))
        nodesToReadStr (apply str (interpose " WITH * " (map #(readnode->cypher % scope) nodesToRead)))
        edgesToRead (filter-elem 'edge (-> r :read second :els))
        edgesToReadStr (apply str (interpose " WITH * " (map readedge->cypher edgesToRead)))
        nodesToReturnStr (apply str (interpose "," (map readnodeRet->cypher nodesToRead)))
        edgesToReturnStr (apply str (interpose "," (map readnodeRet->cypher edgesToRead)))


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
                  " RETURN distinct _g{.uid}," nodesToReturnStr
                  (if (empty? edgesToReturnStr)
                    ""
                    (str "," edgesToReturnStr)))
        _ (debug println "*** exec query-: " qstr)]
;    (apply concat (->> (dbquery qstr)
;         (map #(into [] %))))))
    (p ::query (dbquery qstr))))

(defn exec-query [gs n par]
  (let [_  (debug println "*** exec query " gs)
        res (map #(exec-query- % n par) gs)
 ;       res (list (exec-query- (first gs) n par))
        ]
    (remove empty? res)))



(defn- check-constraint [g n par]
  (loop [rs (search-if g n par)]
    (if (empty? rs)
      true
      (let [c (check-one-then g n par (first rs))]
        (if c (recur (rest rs))
            false)))))

(defn filter-constraint [gs n par]
  (let [res (map #(if (check-constraint % n par) % nil) gs)]
    (remove empty? res)))

(defn filter-constraint-neg [gs n par]
  (let [res (map #(if (check-constraint % n par) nil %) gs)]
    (remove empty? res)))


(defn || [g & rs]
  (let [res (flatten (map #(% g) rs))]
    (set-grape! res)
    res))

(defn rollback []
  (dbquery " call apoc.periodic.iterate ( \"MATCH (gt:`__Graph`)-[:prov*0..]->(gp:`__Graph`) 
   WHERE gt.tag IS NOT NULL  with collect(gp.uid) as transacted 
   with transacted 
            optional match (g:`__Graph`) -[:create]->(i) where not g.uid in transacted return i\",
   \" detach delete i\" ,
           {batchSize:1000, parallel:false}) yield batches
CALL {
           MATCH (gt:`__Graph`)-[:prov*0..]->(gp:`__Graph`) 
   WHERE gt.tag IS NOT NULL  with collect(gp.uid) as transacted 
   with transacted 
             optional match (g:`__Graph`)where not g.uid in transacted 
                with g OPTIONAL MATCH (g)-[:inve]->(e) detach delete e
   with g OPTIONAL MATCH (g)-[:inva]->(a) detach delete a,g }

 ")
  true)

(defn deleteall []
  (dbquery " call apoc.periodic.iterate ( \"MATCH (i) return i\",
   \" detach delete i\" ,
           {batchSize:1000, parallel:false})
 ")
  true)

(defn commit [gs t]
  (if (> (count gs) 1) (println "*** Warning: Commit called with grape that contains multiple graphs."))
    (let [res (dbquery (str "MATCH (g:`__Graph`{uid:\"" (first gs)
                            "\"}) set g.tag=\"" t "\" return g"))
          id   (if (empty? res)
                 nil
                 (-> res first :g :uid))]
      (list id)))

(defn uncommit [t]
  (let [res (dbquery (str "MATCH (g:`__Graph`{tag:\"" t
                          "\"}) remove g.tag return g"))
        id   (if (empty? res)
               "NOT FOUND"
               (-> res first :g :uid))]
    id))


(defn grape [t]
  (let [res (dbquery (str "MATCH (g:`__Graph`{tag:\"" t "\"}) return g"))
        id   (if (empty? res)
               "NOT FOUND"
               (-> res first :g :uid))]
    (when (not (= "NOT FOUND" id))
      (set-grape! (list id)))
    (list id)))


(defn newgrape []
  "DSL form to create a new graph"
  (let [g (->
           (dbquery (str "create (g:`__Graph` {uid: randomUUID(), active:[]}) return g"))
           first :g :uid list)]
    (set-grape! g)
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


(defn node- [h args]
  (let [s (str/split (str h) #":")
        s1 (first s)
        s2 (second s)
        as (or (some #(if (map? %) %) args) {})
        op (or (some #(if (= :opt %) {:opt true}) args) {})
        id (if (or (empty? s1) (= "_" s1))
             (symbol (random-id))
             (symbol s1))
        lab (if (empty? s2) {} {:label s2})]
    (node-os id (merge {:asserts as} op lab))))


(defmacro node [h & args]
  (list 'node- (list 'quote h) (list 'quote args)))

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

(defn edge- [h src tar args]
  (let [s (str/split (str h) #":")
        s1 (first s)
        s2 (second s)
        id (if (or (empty? s1) (= "_" s1))
             (symbol (random-id))
             (symbol s1))
        as (or (some #(if (map? %) %) args) {})
        op (or (some #(if (= :opt %) {:opt true}) args) {})]
    (edge-os id  (merge {:asserts as :label s2 :src src :tar tar} op))))

(defmacro edge [h s t & args]
  (list 'edge- (list 'quote h)
        (list 'quote s)
        (list 'quote t)
        (list 'quote args)))

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


(defn read [& xs]
  {:read (apply pattern xs)})

(defn create [& xs]
  {:create (apply pattern xs)})

(defmacro delete [& xs]
  {:delete (list 'vec (list 'quote xs))})

(defmacro global [& xs]
  {:gcond (list 'vec (list 'quote xs))})

(defn condition [c]
  ['cond c])

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
                              (str "RULE          :- ( rule NAME <[PAR+]>  <:read PATTERN> <:delete [ID+]> <:create PATTERN>  ) \n"
                                   "NAME, PAR, ID :- *symbol* \n"
                                   "PATTERN       := (pattern ...)"))]
    (check-syntax (symbol? n) "rule name must be a symbol")
    (if (not (empty? params))
      (check-syntax (valid-schema [s/Symbol] params) "rule parameter list must be sequence of symbols"))
    (check-syntax (subset? (set (keys prop)) #{:read :delete :create :gcond}) "unknown part of rule. Rules can only have :theory :read, :delete and :create parts")
    (if (contains? prop :read) (check-syntax (= 'pattern (first (:read prop))) "'read' part of rule must contain a pattern"))
    (if (contains? prop :create) (check-syntax (= 'pattern (first (:create prop))) "'create' part of rule must contain a pattern"))
    (if (contains? prop :delete) (check-syntax (valid-schema [s/Symbol] (:delete prop)) "'delete' part must specify sequence of symbols"))
    (if (contains? prop :delete) (check-syntax (contains? prop :read) "'delete' part requires 'read' part to be present"))

    (let [r (assoc prop :params params)
          r2 (if (not (contains? prop :gcond))
               (assoc r :gcond [])
               r)
          s (if (not (contains? prop :delete))
              (assoc r :delete [])
              r2)]
       ;(validate-rule s)
      (add-rule! n s)
      (intern *ns* (symbol (str (name n))) (fn [g & par] (exec= g n 0 par)))
      (intern *ns* (symbol (str (name n) "<1")) (fn [g & par] (exec= g n 1 par)))
      (intern *ns* (symbol (str (name n) "<n")) (fn [g l & par] (exec= g n l par)))
      (intern *ns* (symbol (str (name n) "#")) (fn [g & par] (exec g n par)))
      (intern *ns* (symbol (str (name n) "*")) (fn [g & par] (exec* g n 0 par)))
      (intern *ns* (symbol (str (name n) "*<n")) (fn [g l & par] (exec* g n l par)))
      (intern *ns* (symbol (str (name n) "-dot")) (fn [] (rule->dot n s)))
      ((intern *ns* (symbol (str (name n) "-show")) (fn [] (show (rule->dot n s))))))))


(defn realLabel [ls]
  (first (filter #(not (or (= % "__Node") (= % "__Edge"))) ls)))

(defmacro rule [n pars & args]
  (list 'rule-os
        (list 'quote n)
        (list 'quote pars)
        (list 'apply 'merge (list 'quote (map eval args)))))

(defn unit-os
  "Helper function for defining a unit"
  [n params spec]
  (intern 
    *ns* 
    (symbol (str (name n))) 
    spec))

(defn extract-clause [L sym] (rest (first (filter #(= (first %1) sym) L))))

(defmacro unit 
  "Makes a new graph transformation unit and stores the
  name of the unit in the current namespace such that it
  can be called like a Clojure function on an existing GRAPE."
  [n params & args] 
  (let [
    pre (extract-clause args 'pre)
    post (extract-clause args 'post)
    prog (extract-clause args 'prog)
    pre (if (> (count pre) 0) pre (list (list 'fn ['x] 'true)))
    post (if (> (count post) 0) post (list (list 'fn ['x] 'true)))
    Fn (list 'unit-os 
        (list 'quote n)
        (list 'quote params)
        (list 'fn 

          (vec (concat (list '__G) params))

          (list 'if (list 'unit-should-check?)
            
            ;; CASE: unit condition checking enabled
            (list 'do
            (list 'add-empty-unit-frame (list 'quote n))
            (list 'if
              (list 'every? 'true? (list (concat (list 'juxt) pre) '__G))
              (list 'do 
                (list 'unit-set-pre 'true) 
                (list 'let [
                  '__ret (concat (list '->) (list '__G) prog)
                  '__post-ret (list 'every? 'true? (list (concat (list 'juxt) post) '__ret))] 
                  (list 'if '__post-ret
                    ;; CASE: post condition passed
                    (list 'do 
                      (list 'unit-set-post 'true)
                      '__ret)
                    ;; CASE: post condition failed
                    (list 'do
                      (list 'unit-set-post 'false)
                      (list 'if (list 'unit-should-fail?)
                        (list 'throw (list 'AssertionError. (list 'str "Post-condition for unit " (list 'quote n) " failed!")))
                        '__ret)))))
              (list 'do 
                (list 'unit-set-pre 'false) 
                (list 'if (list 'unit-should-fail?) 
                  (list 'throw (list 'AssertionError. (list 'str "Pre-condition for unit " (list 'quote n) " failed!")))
                  '__G)))))
              
              ;; CASE: unit condition checking disabled
              (concat (list '->) (list '__G) prog)))]
  Fn
  ))

(defn try-skip
  "Attempt to execute unit u on GRAPE G.
   If the unit's execution fails, then return the original GRAPE G. 
   If the unit's execution succeeds, then return the new GRAPE. 
   Unit failure is only detected if unit failure policy is set to FAIL;
   otherwise, unit failures are ignored."
  [G u]
  (try (u G) (catch AssertionError _ G)))

(defn try-empty
  "Attempt to execute unit u on GRAPE G.
   If the unit's execution fails, then returns an empty GRAPE (an empty list).
   If the unit's execution succeeds, then return the resulting GRAPE.
   Only works if unit failure policy is set to FAIL; otherwise failures
   are ignored."
  [G u]
  (try (u G) (catch AssertionError _ ())))

(defn try-resume
  "Attempt to execute unit u on GRAPE G.
   If the unit's execution fails, then disregard the failure
   and return the result anyways."
  [G u]
  (let [og-policy ((deref unit-policy-atom) :fail)]
    (set-unit-failure-policy "SILENT")
    (let [G2 (u G)]
      (set-unit-failure-policy og-policy)
      G2)))

(defmacro exists-graph? 
  "Takes a list of graph constraints.
   Produces a function takes a GRAPE returns true if there exists at least 
   one graph in the GRAPE that satisfies the conjunction of all conditions; 
   otherwise returns false."
  [& conds] 
  `#(> (count (-> %1 ~@conds)) 0))

(defmacro forall-graphs? 
  "Takes a list of graph constraints.
  Produces a function that takes a GRAPE and returns true if all graphs
  in the GRAPE satisfy the conjunciton of all conditions; otherwise 
  returns false."
  [& conds] 
  `#(= (count (-> %1 ~@conds)) (count %1)))

(defmacro empty-grape?
  "Produces a function that takes a GRAPE and returns true if the GRAPE
  is empty and false otherwise."
  [] 
  `#(= (count %1) 0))

(defmacro empty-graph?
  "Produces a function that takes a GRAPE and returns true if all graphs
  in the GRAPE are the empty graph and false otherwise."
  [] 
  `#(> (count (-> %1 __has-node-)) 0))

(defn query- [n params pat]
  "DSL form for specifying a graph query"
  (let [s {:read pat :params params}]
    (add-query! n s)
    (intern *ns* (symbol (str (name n))) (fn [g & par] (exec-query g n par)))
    (intern *ns* (symbol (str (name n) "-dot")) (fn [] (query->dot n s)))
    ((intern *ns* (symbol (str (name n) "-show")) (fn [] (show (query->dot n s)))))))

(defmacro query [n params & cs]
  (list 'query-
        (list 'quote n)
        (list 'quote params)
        (list 'quote (apply pattern (map eval cs)))))


(defn constraint-
  [n params ip tp]
  (let [s {:if ip :then tp :params params}]
    (add-constraint! n s)
    (intern *ns* (symbol (str (name n))) (fn [g & par] (filter-constraint g n par)))
    (intern *ns* (symbol (str (name n) "-")) (fn [g & par] (filter-constraint-neg g n par)))

    (intern *ns* (symbol (str (name n) "-dot")) (fn [] (constraint->dot n s)))
    ((intern *ns* (symbol (str (name n) "-show")) (fn [] (show (constraint->dot n s)))))))

(defmacro constraint [n params & args]
  (list 'constraint- (list 'quote n) (list 'quote params) []
        (list 'apply 'pattern (list 'quote (map eval args)))))

(defn IF [& xs]
  (apply pattern xs))

(defn THEN [& xs]
  (apply pattern xs))

(defmacro cond-constraint [n params ip tp]
  (list 'constraint- (list 'quote n) (list 'quote params)
        (list 'quote (eval ip))
        (list 'quote (eval tp))))

(defn constraint-clause- [n cs]
  (intern *ns* (symbol (str (name n))) (fn [g] (apply (partial || g) cs))))

(defmacro constraint-clause [n cs]
  (list 'constraint-clause- (list 'quote n) cs))

(defn add-inv [g c t]
  (dbquery (str "MATCH (g:`__Graph`{uid:\"" g "\"})"
                " create (g)-[:_" t "]->(i:`__Inv`{name:\"" c "\"})")))

(defn del-inv [g c t]
  (dbquery (str "MATCH (g:`__Graph`{uid:\"" g "\"})"
                " -[e:_" t "]->(i:`__Inv`{name:\"" c "\"}) delete e,i")))

(defn- attach-schema [constrs]
  (fn [g]
    (let [g- (enforce- g (vec (eval constrs)))]
      (doseq [i g-]
        (doseq [c constrs]
          (add-inv i c "inve")))
      g-)))

(defn- drop-schema [constrs]
  (fn [g]
    (doseq [i g]
      (doseq [c constrs]
        (del-inv i c "inve")))
    g))

(defmacro schema [n constrs]
  (let [cs (vec (map #(if (symbol? %) % (eval %)) constrs))]
    (intern *ns* (symbol (str (name n) "-drop")) (drop-schema cs))
    (intern *ns* (symbol (str (name n))) (attach-schema cs))))

(defn schema-list [gs]
  (map #(dbquery (str "  MATCH (g:`__Graph`{uid:\"" % "\"}) "
                      " with g optional match (g) -[:_inve]->(i) return g.uid as graph, collect (i.name) as constraints "))
       gs))

(defn- cons-nodetypes [g nts]
  (let [ntsr (str "[" (apply str (interpose ", " (map #(str "'" % "'") nts))) "]")
        qstr (str "MATCH (_g:`__Graph` {uid:\"" g "\"}) "
                  " WITH * match (n:__Node) WHERE ID(n) in _g.active AND apoc.coll.intersection(labels(n), " ntsr ")=[] return n")]
    (if (empty? (dbquery qstr))
      g
      nil)))

(defn- cons-edgetypes [g nts]
  (let [ntsr (str "[" (apply str (interpose ", " (map #(str "'" % "'") nts))) "]")
        qstr (str "MATCH (_g:`__Graph` {uid:\"" g "\"}) "
                  " WITH * match (n:__Edge) where ID(n) in _g.active AND  apoc.coll.intersection(labels(n), " ntsr ")=[] return n")]
    (if (empty? (dbquery qstr))
      g
      nil)))

(defn- cons-srctypes [g e nts]
  (let [ntsr (str "[" (apply str (interpose ", " (map #(str "'" % "'") nts))) "]")
        qstr (str "MATCH (_g:`__Graph` {uid:\"" g "\"})  "
                  " WITH * match (n:__Edge:" e ")-[:src]->(s) where ID(n) in _g.active AND  apoc.coll.intersection(labels(s), " ntsr ")=[] return n")]
    (if (empty? (dbquery qstr))
      g
      nil)))

(defn- cons-tartypes [g e nts]
  (let [ntsr (str "[" (apply str (interpose ", " (map #(str "'" % "'") nts))) "]")
        qstr (str "MATCH (_g:`__Graph` {uid:\"" g "\"})  "
                  " WITH * match (n:__Edge:" e ")-[:tar]->(s) where ID(n) in _g.active AND apoc.coll.intersection(labels(s), " ntsr ")=[] return n")]
    (if (empty? (dbquery qstr))
      g
      nil)))

(defn- cons-tarunique [g e]
  (let [qstr (str "MATCH (_g:`__Graph` {uid:\"" g "\"}) "
                  " match (n2)<-[:tar]-(e2:__Edge:" e ")-[:src]->(o:__Node)"
                  "<-[:src]-(e1:__Edge:" e ")-[:tar]->(n) where ID (o) in _g.active "
                  " AND ID (e1) in _g.active AND ID (e2) in _g.active and ID (n) <>ID (n2) return n")]
    (if (empty? (dbquery qstr))
      g
      nil)))


(defn- cons-srcunique [g e]
  (let [qstr (str "MATCH (_g:`__Graph` {uid:\"" g "\"}) "
                  " match (n2)<-[:src]-(e2:__Edge:" e ")-[:tar]->(o:__Node)"
                  "<-[:tar]-(e1:__Edge:" e ")-[:src]->(n) where ID (o) in _g.active "
                  " AND ID (e1) in _g.active AND ID (e2) in _g.active and ID (n) <>ID (n2) return n")]
    (if (empty? (dbquery qstr))
      g
      nil)))


(defn- cons-all [c gs nts]
  (remove nil?
          (map #(c % nts) gs)))

(defn- cons-all-edge [c gs e nts]
  (remove nil?
          (map #(c % e nts) gs)))


(defn- cons-all-edge-card [c gs e]
  (remove nil?
          (map #(c % e) gs)))

(defn- cons-all-unique [c gs l a]
  (remove nil?
          (map #(c % l a) gs)))

(defmacro nodelabels [& args]
  (let [n (if (= (count args) 2) (first args) (symbol "-nodelabs"))
        nts (if (= (count args) 2) (second args) (first args))]
    (intern *ns* (symbol (str (name n))) (fn [g] (cons-all cons-nodetypes g (map #(str (name %)) nts))))
    (list 'quote n)))

(defmacro edgelabels [& args]
  (let [n (if (= (count args) 2) (first args) (symbol "-edgelabs"))
        nts (if (= (count args) 2) (second args) (first args))]
    (intern *ns* (symbol (str (name n))) (fn [g] (cons-all cons-edgetypes g (map #(str (name %)) nts))))
    (list 'quote n)))

(defmacro sourceroles [& args]
  (let [nts (if (= (count args) 3) (third args) (second args))
        e (if (= (count args) 3) (second args) (first args))
        n (if (= (count args) 3) (first args) (symbol (str "-" e "-sources")))]
    (intern *ns* (symbol (str (name n))) (fn [g] (cons-all-edge cons-srctypes g (str (name e)) (map #(str (name %)) nts))))
    (list 'quote n)))

(defmacro targetroles [& args]
  (let [nts (if (= (count args) 3) (third args) (second args))
        e (if (= (count args) 3) (second args) (first args))
        n (if (= (count args) 3) (first args) (symbol (str "-" e "-targets")))]
    (intern *ns* (symbol (str (name n))) (fn [g] (cons-all-edge cons-tartypes g (str (name e)) (map #(str (name %)) nts))))
    (list 'quote n)))

(defmacro to-one [& args]
  (let [e (if (= (count args) 2) (second args) (first args))
        n (if (= (count args) 2) (first args) (symbol (str "-" e "-toone")))]
    (intern *ns* (symbol (str (name n))) (fn [g] (cons-all-edge-card cons-tarunique g (str (name e)))))
    (list 'quote n)))

(defmacro from-one [& args]
  (let [e (if (= (count args) 2) (second args) (first args))
        n (if (= (count args) 2) (first args) (symbol (str "-" e "-fromone")))]
    (intern *ns* (symbol (str (name n))) (fn [g] (cons-all-edge-card cons-srcunique g (str (name e)))))
    (list 'quote n)))

(defn- cons-unique [g l a]
  (let [qstr1 (str "CREATE INDEX IF NOT EXISTS "
                   "FOR (n:" l ") "
                   "ON (n." a ") ")
        qstr2 (str "MATCH (_g:`__Graph` {uid:\"" g "\"})"
                   " WITH * match(n1:" l ") WHERE ID(n1) in _g.active "
                   " WITH * match(n2:" l ") WHERE ID(n2) in _g.active  and ID(n1) <> ID(n2) "
                   " AND n1." a " = n2." a " return n1")]
    (dbquery qstr1)
    (if (empty? (dbquery qstr2))
      g
      nil)))




(defmacro unique [& args]
  (let [lab (if (= (count args) 3) (second args) (first args))
        at (if (= (count args) 3) (third args) (second args))
        n (if (= (count args) 3) (first args) (symbol (str "-unique-" lab "-" at)))]
    (intern *ns* (symbol (str (name n))) (fn [g] (cons-all-unique cons-unique g
                                                                  (str (name lab))
                                                                  (str (name at)))))
    (list 'quote n)))

(defmacro enforce [n cl]
  (intern *ns* n
          (eval
           (list 'fn ['g] (concat (list (first cl) 'g) (vec (rest cl))))))
  (list 'quote n))



(defmacro assertion [g & constrs]

  (list 'let ['g- (list 'enforce- g (vec constrs))]
        (list 'doseq ['i 'g-]
              (list ['c (list 'quote constrs)] 'doseq
                    (list 'add-inv 'i 'c "inva")))
        'g-))






(defn occnodes [g kind]
  "return the occurance of kind nodes for graph g"
  (let [qstr (str "match(g:`__Graph` {uid:'" g "'}) -[e:" kind "]->(n:`__Node`) "
                  " RETURN n{.*, labels: labels(n), id: ID(n), handle:e.handle}  ")]
    (map first (dbquery qstr))))

(defn occdelnodes [g]
  (occnodes g "delete"))

(defn occaddnodes [g]
  (occnodes g "create"))

(defn occkeepnodes [g]
  (occnodes g "read"))

(defn occedges [g kind]
  "return the occurance of kind nodes for graph g"
  (let [qstr (str "match(g:`__Graph` {uid:'" g "'}) -[:" kind "]->(e:`__Edge`) 
                      WITH * match (e)-[:src]->(s)
                      WITH * match (e)-[:tar]->(t)
                      RETURN e{.*, labels: labels(e), src: ID(s), tar: ID(t)}  ")]
    (map first (dbquery qstr))))



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
    {:g (-> res :g :uid)
     :p (-> res :p :uid)
     :r (-> res :e :rule)}))


(defn occ- [g]
  (assoc (occgraphs g)
         :keepnodes (occkeepnodes g)
         :delnodes (occdelnodes g)
         :addnodes (occaddnodes g)
         :keepedges (occkeepedges g)
         :deledges (occdeledges g)
         :addedges (occaddedges g)))

(defn occ [gs]
  (map occ- gs))

(defn n->dot [n color pref]
  (let [handle   (getHandle n)
        id (getId n)
        label  (-> n getLabels realLabel)
        attrs  (->> (getAttrs n)
                    (map #(str (-> % first name) ": " (-> % second pr-str))))
        attrstr (apply str (interpose " | " attrs))]
    (str "\"" pref id "\" [shape=Mrecord penwidth=bold style=filled fillcolor=aliceblue label=\"{" handle ":" label
         "(" id ")"
         (if (not (empty? attrstr))
           (str " | " (str/escape attrstr {\" "'"}))
           "")
         "}\" color=" color " fontcolor=" color " ] ")))


(defn nd->dot [n]
  (n->dot n "red" ""))

(defn na->dot [n]
  (n->dot n "x11green" ""))
(defn nk->dot [n]
  (n->dot n "black" ""))

(defn e->dot [e color pref]
  (let [label  (-> e getLabels realLabel)
        src  (getSrc e)
        tar  (getTar e)
        attrs  (->> (getAttrs e)
                    (map #(str (-> % first name) ":" (-> % second pr-str))))
        attrstr (apply str (interpose ", " attrs))]
    (str "\"" pref src "\" -> \"" pref tar "\" [ label= \"" label
         (if (not (empty? attrstr))
           (str "{" (str/escape attrstr {\" "'"}) "}")
           "")
         "\" color=" color " fontcolor= " color "]")))


(defn ek->dot [e]
  (e->dot e "black" ""))

(defn ed->dot [e]
  (e->dot e "red" ""))

(defn ea->dot [e]
  (e->dot e "x11green" ""))


(defn viewocc-- [g]
  "view the graph occurance"
  (let [o (occ- g)
        g2 (or (:g o) "  EMPTY ")
        rn (-> o :r)]
    (str " subgraph \"cluster_" g2 "\"  { "
         " \"" g2 "\" [shape=point style=invis]"
         "  label = < &#8212;<b>" rn "</b>&#8594; > "
         (apply str (map #(n->dot % "black" g2) (:keepnodes o))) "\n"
         (apply str (map #(n->dot % "red" g2) (:delnodes o))) "\n"
         (apply str (map #(n->dot % "x11green" g2) (:addnodes o))) "\n"
         (apply str (map #(e->dot % "black" g2) (:keepedges o))) "\n"
         (apply str (map #(e->dot % "red" g2) (:deledges o))) "\n"
         (apply str (map #(e->dot % "x11green" g2) (:addedges o))) "\n"
         " } ")))


(defn viewoccrels-- [g]
  "view the graph occurance"
  (let [o (occ- g)
        g2 (or (:g o) "  EMPTY ")
        g1 (or (:p o) "NOTFOUND")]
    (str " \"" g1 "\" [shape=point style=invis]"
         " \"" g1 "\" -> \"" g2 "\" [ltail=\"cluster_" g1 "\" lhead=\"cluster_" g2 "\" ]; ")))

(defn viewocc- [g]
  (apply str (map viewocc-- g)))

(defn viewoccrels- [g]
  (apply str (map viewoccrels-- g)))


(defn viewsteps [gs]
  (let [clusters (map viewocc- gs)
        rels     (map viewoccrels- gs)]
    (-> (str "digraph G { "
             "compound=true; "
             (apply str (concat clusters rels))
             "}")
        show)))

(defn step->dot [r]
  (let [uid1 (-> r :g1 :uid)
        tag (-> r :g1 :tag)
        id1  (-> r :g1 :id)
        orig    (-> r :gc nil?)]
    (str id1 "[label=\"" (or tag (subs uid1 0 8)) "\""
         "]")))


(defn prov->dot [r]
  (let [rule (-> r :e :rule)
        src  (-> r :g2 :id)
        tar  (-> r :g1 :id)]
    (if (or (nil? src) (nil? tar))
      " "
      (str tar " -> " src "[label=\"" rule "\"]"))))

(defn conf->dot [r]
  (let [src  (-> r :g1 :id)
        tar  (-> r :gc :id)]
    (if (nil? tar)
      " "
      (str src " -> " tar "[constraint=false style=dashed color=blue]"))))


(defn history [gs]
  (let [g (first gs)
        qstr (str "match(g:`__Graph`{uid:\"" g "\"})
                     -[:prov*0..]->(g0:`__Graph`) 
                     where not exists( (g0)-[:prov]->() ) 
                     with g0 create (gn:`__Graph`{uid:randomUUID()}) with g0,gn  
                     match (g0)<-[:prov*0..]-(g1:`__Graph`) 
                     with g0,g1,gn 
                     create(g1g:Graph:`__Node`{uid:g1.uid, tag:g1.tag})<-[:create]-(gn)                          
                     with collect(distinct gn) as gnew 
                     call {with gnew with head(gnew) as gn 
                           match(gn)-[:create]->(g1g:Graph) 
                          with * match(g1:`__Graph`{uid:g1g.uid})-[p:prov]->(g2:`__Graph`) 
                          with * match(g2g:Graph {uid:g2.uid})<-[:create]-(gn) 
                          with * create (gn)-[:create]->(e:__Edge:occ{rule:p.rule})-[:src]->(g2g) 
                          with * create(e)-[:tar]->(g1g)
                          with * call { with g1 match (g1)-[p:prov*1..]->(gx:`__Graph`) return count(*) as steps}
                          set e.src= ID(g2g), e.tar= ID(g1g),
                              e.step=steps
                                                } 
                     with * 
                       call {with gnew with head(gnew) as gn
                              match(gn)-[:create]->(el)
                              with gn, collect(ID(el)) as elids
                              set gn.active = elids}
                     with *
                     return head(gnew).uid as gn")]
    (map :gn (dbquery qstr))))

(defn history-old [gs]
  (let [g (first gs)
        qstr (str "match(g:`__Graph`{uid:\"" g "\"})
                     -[:prov*0..]->(g0:`__Graph`) 
                     where not exists( (g0)-[:prov]->() ) 
                     with g0 create (gn:`__Graph`{uid:randomUUID()}) with g0,gn  
                     match (g0)<-[:prov*0..]-(g1:`__Graph`) 
                     with g0,g1,gn 
                     create(g1g:Graph:`__Node`{uid:g1.uid, tag:g1.tag})<-[:create]-(gn) 
                     with collect(distinct gn) as gnew 
                     call {with gnew with head(gnew) as gn 
                           match(gn)-[:create]->(g1g:Graph) 
                          with * match(g1:`__Graph`{uid:g1g.uid})-[p:prov]->(g2:`__Graph`) 
                          with * match(g2g:Graph {uid:g2.uid})<-[:create]-(gn) 
                          with * create (gn)-[:create]->(e:__Edge:occ{rule:p.rule})-[:src]->(g2g) 
                          with * create(e)-[:tar]->(g1g)
                          with * call { with g1 match (g1)-[p:prov*1..]->(gx:`__Graph`) return count(*) as steps}
                          set e.src= ID(g2g), e.tar= ID(g1g),
                              e.step=steps
                                                } 
                     with *
                     return head(gnew).uid as gn")]
    (map :gn (dbquery qstr))))

(defn- trace [g]
  (let [qstr (str "match(g:`__Graph`{uid:\"" g "\"})
                     with * create (gn:`__Graph`{uid:randomUUID()}) 
                     with * match (g)-[:prov*0..]->(g1:`__Graph`) 
                     with * 
                     create(g1g:Graph:`__Node`{uid:g1.uid, tag:g1.tag})<-[:create]-(gn) 
                     with collect(distinct gn) as gnew 
                     call {with gnew with head(gnew) as gn 
                           match(gn)-[:create]->(g1g:Graph) 
                          with * match(g1:`__Graph`{uid:g1g.uid})-[p:prov]->(g2:`__Graph`) 
                          with * match(g2g:Graph {uid:g2.uid})<-[:create]-(gn) 
                          with * create (gn)-[:create]->(e:__Edge:occ{rule:p.rule})-[:src]->(g2g) 
                          with * create(e)-[:tar]->(g1g)
                          with * call { with g1 match (g1)-[p:prov*1..]->(gx:`__Graph`) return count(*) as steps}
                          set e.src= ID(g2g), e.tar= ID(g1g),
                              e.step=steps
                                                } 
                     with *
                     return head(gnew).uid as gn")]
    (map :gn (dbquery qstr))))

(defn traces [gs]
  (flatten (map trace gs)))

(defn result->dot [res]
  (let [nodes (filter (fn [x] (some #(= "__Node" %) (getLabels x))) res)
        nodeStr (apply str (map nk->dot nodes))
        edges (filter (fn [x] (some #(= "__Edge" %) (getLabels x))) res)
        edgeStr (apply str (map ek->dot edges))
        graphs (filter (fn [x] (empty? (getLabels x))) res)]
    ;graph [size=\"11,8.5\"] 
    (str "digraph g { splines=true overlap=false subgraph cluster0 { margin=0 "
         "label=\"GRAPH: " (or (-> graphs first  getTag)
                               (-> graphs first second :uid)) "\"; \n"
         nodeStr
         " \n "
         edgeStr
         (if (empty? (str nodeStr edgeStr)) "e [label=\"empty graph\" shape=none]" "")
         " graph[style=dotted]; } } ")))

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
    (if (empty? (-> graphs first getId))
      {:nodes [:id 0 :shape "text" :label "(empty)"]}
      {:nodes (vec (concat [{:id 0
                             :label (str "Graph: " (or (-> graphs first :data :tag)
                                                       (-> graphs first :data :uid)))
                             :shape "text"
                             :font {:size 14 :bold true}}]
                           nodesVD))
       :edges edgesV})))



(defn viewquery [ress]
  (let [ses (map #(apply concat %) ress)]
    (map #(-> % distinct result->dot show) ses)))


(query _any? []
       (node _1 :opt)
       (node _2 :opt)
       (edge e:__Edge _1 _2 :opt))



(declare _any?)

(defn view [gs]
  (debug println "** View")
  (viewquery (_any? gs)))


(defn- merge-grapes [ds d]
  (let [curg0 (-> ds first :before)
        curg1 (-> ds first :after)]
    (if (= curg0 (:before d))
      (conj (rest ds) {:before curg0 :after (conj curg1 (:after d))})
      (conj ds {:before (:before d) :after (list (:after d))}))))

(defn steps [g]
  (let [res (dbquery (str "match (g:`__Graph`{uid:\"" (first g) "\"})
                     -[:create]->(g1:Graph)<-[:tar]-(e:`__Edge`:occ)-[:src]->(g0:Graph) 
                     return g1.uid as after, g0.uid as before, e.step as i order by i"))
        gr (reduce merge-grapes '() res)]
    (map :after (reverse gr))))






(defmacro ->*?# [start test & ops]
  (list 'let ['m (list 'checkeq?)
              '_ (list 'checkeq! true)
              'res
              (list 'loop ['g start]
                    (list 'if (list 'empty? 'g)
                          'g
                          (list 'let ['s (list test 'g)]
                                (list 'if (list 'empty? 's)
                                      (list 'recur (concat (list '-> 'g)
                                                           ops
                                                           (list 'removeConfluent)))
                                      's))))]
        (list 'checkeq! 'm)
        (list 'set-grape! 'res)
        'res))

; recurse as long as possible (ralap)
(defmacro ->* [start & ops]
  (list 'let ['res
              (list 'loop ['g start
                           'gp '()]
                    (list 'if (list 'empty? 'g)
                          'gp
                          (list 'recur (concat (list '-> 'g)
                                               ops) 'g)))]
        (list 'set-grape! 'res)
        'res))


; recurse n times
(defmacro ->n* [start nm & ops]
  (list 'let ['res
              (list 'loop ['g start
                           'n 0]
                    (list 'if (list 'or (list 'empty? 'g)
                                    (list '= 'n nm))
                          'g
                          (list 'recur (concat (list '-> 'g)
                                               ops) (list 'inc 'n))))]
        (list 'set-grape! 'res)
        'res))



(defmacro ->*? [start test & ops]
  (list 'loop ['g start]
        (list 'if (list 'empty? 'g)
              'g
              (list 'let ['s (list test 'g)]
                    (list 'if (list 'empty? 's)
                          (list 'recur (concat (list '-> 'g)
                                               ops))
                          's)))))

(defn harvest [gs tag]
  (let [qstr (str
              "CALL {OPTIONAL MATCH (v:__View{name:\"" tag "\"})-[]-(n) detach delete v,n} "
              "CREATE (v:__View{name:\"" tag "\"}) "
              " WITH v "
              " MATCH (_g:`__Graph` {uid:\"" (first gs) "\"}) "
              " WITH * match(n:__Node) WHERE ID(n) IN _g.active "
              " WITH distinct n,_g,v CALL apoc.refactor.cloneNodes([n], true, [\"_fp\"]) YIELD output "
        ;        " WITH v, output, _active SET output.name=\"c\" "
              " WITH v, output, _g CREATE (v)-[:c]->(output) "

              " WITH * MATCH (output)-[gr]-(:__Graph) DELETE gr "

              " WITH * REMOVE output:__Node "

              " WITH _g,v match (v)-[:c]->(n1)<-[:src]-(e:__Edge)-[:tar]->(n2)<-[:c]-(v) WHERE ID(e) IN _g.active "
              " WITH distinct e, v "
              " CALL apoc.refactor.cloneNodes([e], true, [\"_fp\",\"_fpd\", \"_fps\", \"_fpt\", \"src\", \"tar\"]) "
              " YIELD output "
              " WITH output, v "
  ;              " set output.name=\"n\" "
              " WITH output, v "
              " MATCH (output)-[gr]-(:__Graph) DELETE gr "
              " with output, v "
              " MATCH (output)-[s1:src]->()<-[]-(:__Graph) DELETE s1 "
              " with output, v "
              " MATCH (output)-[t1:tar]->()<-[]-(:__Graph) DELETE t1 "
              " with output, v "
              " REMOVE output:__Edge "

              " WITH v "
              " MATCH (e:__Edge)-[s:src]->(vn1)<-[:c]-(v) delete s "
              " WITH v "
              " MATCH (e:__Edge)-[t:tar]->(vn2)<-[:c]-(v) delete t "
              " WITH v "
              " CALL { with v "
              " MATCH (v)-[]->(n1)<-[s:src]-(es)-[:tar]->(n2) "
              " delete s create (n1)-[:src]->(es)} "
              " WITH v "
              " MATCH (v)-[:c]->(n1)-[s:src]->(es)-[:tar]->(n2) "
              " WITH distinct es, v "
              " CALL apoc.refactor.collapseNode([es],head(labels(es))) "
              " YIELD input, output, error "
              " RETURN v")
        ret   (dbquery qstr)]
    (if (empty? ret) nil tag)))

(defn plant [tag]
  (let [qstr (str
              "CREATE (_gn:`__Graph`{uid: randomUUID()}) "
              "WITH _gn "
              "MATCH (v:`__View`{name:\"" tag "\"}) "
              "with * OPTIONAL MATCH (v)-[:c]->(n) "
              "CALL apoc.refactor.cloneNodes([n], true) YIELD output as nc "
              "with * create (_gn)-[:create]->(nc) "
              "with * match(v)-[e:c]->(nc) delete e "
              "with * CALL { with _gn match (_gn)-[:create]->(nn) "
              "              return collect ( ID(nn)) as nids} "
              "with nids, _gn "
              "match (n1)-[e]->(n2) where ID(n1) IN nids and ID(n2) IN nids "
              "CALL apoc.refactor.extractNode(e,[TYPE(e), '__Edge'],'tar','src') "
              "YIELD output as ne "
              "SET n1:__Node SET n2:__Node "
              "return distinct _gn ")
        g   (-> (dbquery qstr) first :_gn :uid)
        qstr2 (str
               "match (_gn:`__Graph`{uid:\"" g "\"})-[:create]->(n1:__Node)-[]-(e:__Edge) "
               "merge ((_gn)-[:create]->(e)) "
               "with _gn "
               "match (_gn)-[:create]->(n1:__Node)-[e:src]->(:__Edge) "
               "CALL apoc.refactor.invert(e) YIELD output "
               "with _gn "
               "match (_gn)-[:create]->(n1:__Node)-[e]-(n2) "
               "where not exists ((_gn)-[:create]->(n2)) "
               "delete e "
               "with _gn "
               "match (_gn)-[:create]->(n:__Node) "
               "  set n._fp=apoc.hashing.fingerprint(n) "
               "with _gn "
               "match (_gn)-[:create]->(ns:__Node)<-[:src]-(e:__Edge)-[:tar]->(nt) "
               " SET e.src= ID(ns), e.tar= ID(nt), e._fps=ns._fp, e._fpt=nt._fp, "
               " e._fpd= apoc.coll.different([ ID(ns), ID(nt)]) "
               " SET e._fp=apoc.hashing.fingerprint( e, ['tar', 'src']) "

               "with _gn optional match (_gn) -[:prov*0..]->()-[:create]->(_el) "
               " with _gn, collect (_el._fp) as _fps, collect( ID(_el)) as els "
               " set _gn._fps = apoc.coll.sort(_fps), _gn.active=els "
               " with _gn set _gn._fp=apoc.hashing.fingerprint (_gn, ['uid']) ")]
    (dbquery qstr2)
    (list g)))


;; =========================================
;; Built-in rules and conditions that are used
;; elsewhere in grape.core.
;; =========================================

;; used by empty-graph? to check if a graph is indeed empty.
(constraint __has-node [] (node n))

