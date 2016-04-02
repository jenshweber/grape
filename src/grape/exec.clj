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


(defn apply-rule
  "apply a rule to a host graph"
  ([n s]
   (let [r ((eval 'gragra) n)
         _ (println "[Applying rule:]" (name n))]
     (if (nil? r)
       (throw (Exception. (str "a rule with name " n " does not exist")))
       (let [reader (:read r)
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
                     false )))))))))
  ([n]
   (apply-rule n {})))

