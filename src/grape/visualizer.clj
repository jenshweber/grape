(ns grape.visualizer
  (:require
    [clojure.data.json :as json]
    [schema.core :as s]
    [clojure.string :as str]
    [clojure.set :refer :all]
    [dorothy.core :as dorothy]
    [clojure.data.codec.base64 :as b64]
    [grape.util :refer :all]
    [clojure.java.io :as io]))

(import javax.imageio.ImageIO)

(use 'gorilla-repl.image)

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

(defn show [g]
  (image-view
    (ImageIO/read
      (io/input-stream
;  (dorothy/save! (dorothy/render  g {:format :png}) "out.png")
        (dorothy/render  g {:format :png}))
))
)


(defn asserts->dot [as]
  "Translate a map of assertions to Dot"
  (if (empty? as)
    ""
    (str/join "; " (map (fn [[k v]]
                          (str (name k) "=" (if (str/starts-with? v "&")
                                              (subs v 1 (count v) )
                                              v)))
                               as))))

(defn node->dot [n c o]
  (let [p (second n)
        handle (name (:id p))
        dhandle (if (str/starts-with? handle "_") "_" handle)
        l (:label p)
        oid (:oid p)
        as (:asserts p)
        o (if (:opt p) " style=dashed "
                       (if (:merge p)
                         " style=diagonals "
                         o))
        ]
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
         "\" ]"
         )))

(defn path->dot [e c o]
  (let [p (second e)
        src (name (:src p))
        tar (name (:tar p))]
    (str " " src " -> " tar
         " [color=\"black:invis:black\" len=8 fontcolor=" c " " o
         " label=\"" (:exp p) "\" ]"
         )))

(declare pattern->dot)

(defn NAC->dot
  "translate a NAC to dot"
  [nac]
  (let [nacid (second nac)
        c (nth '("blue" "darkviolet" "brown" "dimgrey") nacid)
        p (nth nac 2)]
    (pattern->dot p [] c c " style=filled arrowhead=normalicurvecurve ")))

(defn cond->dot
  "translate a condition to dot"
  [c]
  (str " "
       ;(get-ctr!)
       " cond [color=lightgrey style=filled shape=house label=\"" (second c) "\"]"))

(defn assign->dot
  "translate an assignment to dot"
  [c]
  (str " "
       (get-ctr!)
       " [color=seagreen1 style=filled shape=invhouse label=\"" (second c) "\"]"))


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
     (throw (Exception. "Invalid graph element"))
     )))


(defn pattern->dot
  "translate a graph pattern to dot"
  [p d c1 c2 o]
  (let [m    (second p)
        els  (:els m)
        flag (if (= :iso (:sem m))
               (str (random-id) " [color=\".7 .3 1.0\" shape=round style=filled label=\"ISO\"] " ""))]
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
        p (:params rule)]
    (str "digraph g {  splines=true overlap=false subgraph cluster0 {"
         "label=\"Rule: " n (if (empty? p) "" (str p)) "\";"
         (pattern->dot r d "black" "red" "")
         (pattern->dot c [] "green" "green" "")
         "}}")))










