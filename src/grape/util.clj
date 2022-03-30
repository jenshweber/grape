(ns grape.util
  (:require
    [clojure.test :refer :all]
    [clojure.set :refer :all]
    [nano-id.custom :refer [generate]]
    ))



(defn str-sep [s x y ] (str x s y))

(defn implies
  "Logical implication"
  [p q]
  (or (not p) q))

(defn in?
  "true if coll contains elm"
  [coll elm]
  (some #(= elm %) coll))

(defn random-id []
  (symbol
    (str "_"
      ((generate "ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz") 10))))

