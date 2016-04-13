(ns grape.exec
  (:require
   [schema.core :as s]
   [clojure.string :as str]
   [clojure.set :refer :all]
   [grape.util :refer :all]
   [clojurewerkz.neocons.rest.cypher :as cy]
   [clojurewerkz.neocons.rest :as nr]
   [grape.tx-cypher :refer :all]
   [environ.core :refer [env]]
   ))

(def dburi (env :db-url))
(def dbusr (env :db-usr))
(def dbpw (env :db-pw))

(def conn (nr/connect dburi dbusr dbpw))

(defn match-pattern [s c m]
  "match a pattern in the host graph. s is a parameterlist, c is an (optional) match context string and m is a pattern"
  (if (nil? (:els (second m)))
    '()
    (let [q (str c " "(pattern->cypher s :match m) " RETURN *")
          _ (print q)
          m (cy/tquery conn q)]
      (if (empty? m)
        m
        (let [isNode? (fn [x] (re-find #"node" (:self x)))
              remove-unneeded (fn [x]
                                (zipmap (keys x)
                                        (map (fn [y] (select-keys y [:metadata :data]))
                                             (vals x))))]
          (map (fn [x] {:nodes (remove-unneeded (filter (fn [[k v]] (isNode? v)) x))
                        :edges (remove-unneeded (filter (fn [[k v]] (not (isNode? v))) x))}) m)
          )))))



(defn match-nacs [s nacs m]
  "attempt to match the nacs. s is a scope of parameters, nacs a set of nacs and m a match"
  (if (empty? nacs)
    false
    (let [nac (first nacs)
          con (redex->cypher m)
          [_ nacid p] nac
          ext (pattern->cypher s :match p)
          _ (print ".       [Trying NAC " nacid "]: " con "  ||  " ext )]
      (let [m (cy/tquery conn (str con " " ext " RETURN * LIMIT 1"))
            _ (println " [" (not (empty? m)) "]")]
        (if (empty? m)
          (match-nacs s (rest nacs) m)
          true)))))

(defn match [n s]
  "match a rule to host graph"
  (let [r ((:rules (eval 'gragra)) n)
         _ (println "[Matching rule:]" (name n))]
    (if (nil? r)
       (throw (Exception. (str "a rule with name " n " does not exist")))
       (let [reader (:read r)
             matchset (if (not (nil? reader))
                        (let [_ (print ".     [matching RHS] ")
                              ms (match-pattern s "" reader)
                              _ (println " [matches found (prior to NACs): " (count ms) "]")
                              nacs (filter (fn [x] (= 'NAC (first x))) (:els (second reader)))
                              ms2 (remove (fn [m] (match-nacs s nacs m)) ms)
                              _ (println " [matches found (after NACs): " (count ms2) "]")
                              ]
                          ms2)
                        '(()))]
         {:name n :matchset matchset :scope s}
         ))))

(defn change [mm]
  "apply a rule, based on a matchset mm"
  (let [n  (:name mm)
        ms (:matchset mm)
        s (:scope mm)
        r ((:rules (eval 'gragra)) n)] ; rule
    (if (empty? ms)
      false
      (let [m (first ms)
            redex (if (empty? m)  ; is the LHS empty?
                    ""
                    (redex->cypher m))
            q (str redex
                   (if (contains? r :delete)
                     (str (if (= (:theory r) 'dpo)
                            " DELETE "
                            " DETACH DELETE ")
                          (reduce (partial str-sep ", ") (:delete r)))
                     "")
                   (if (contains? r :create)
                     (pattern->cypher s :create (:create r))
                     ""))
            _ (println ".     [attempting to change graph:] " q)]

        (try
          (cy/tquery conn q)
          (println ".                  [success]")
          true
          (catch Exception e
            (do
              (println (str "         [fail] " (.getMessage e)))
              (change (assoc mm :matchset (rest ms))))))))))


(defn apply-rule
  ([n s]
   "apply a rule with name n (symbol) and parameters s (map) to a host graph"
   (change (match n s)))
  ([n]
   (apply-rule n {})))

