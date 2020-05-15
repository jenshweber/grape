(ns grape.util
  (:require
    [clojure.data.json :as json]
    [clojure.test :refer :all]
    [schema.core :as s]
    [clojure.string :as str]
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
    ((generate "ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz") 10)))
