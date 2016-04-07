(ns grape.analysis
  (:require
   [schema.core :as s]
   [clojure.string :as str]
   [clojure.set :refer :all]
   [grape.util :refer :all]
   ))



(defn check-syntax-generic [s c m]
  (if (not c)
    (throw (Exception. (str "Grape syntax error: " m
                            "\n Expected syntax: \n" s

                            "\n ... where <> denotes an optional element, | denotes an alternative choice, and N+ denotes a list of elements \n\n")))))

(defn check-stat-sem [c m]
  (if (not c)
    (throw (Exception. (str "Grape static analysis error: " m "\n\n")))))


(defn valid-schema [s d]
  (nil? (s/check s d)))

(defn valid-children [allowed d]
  (let [c (reduce (fn [l x] (cons (first x) l)) '() d) ]
    (subset? (set c) allowed)))


(defn pattern-elems [p ks]
  (reduce (fn [es e]
            (if (some #{(first e)} ks)
              (cons e es)
              es))
          '()
          (:els (if (= 'pattern (first p))
                  (second p)
                  (nth p 2)))))


(defn ids [els]
  (map (fn [x] (:id (second x))) els ))

(defn appls [els]
  (remove nil?
          (concat
           (map (fn [x] (:src (second x))) els )
           (map (fn [x] (:tar (second x))) els )
           (map (fn [x] (if (symbol? x)
                          (first (str/split (name x) #"\."))
                          nil))
                (flatten (remove nil?
                                 (map (fn [x] (vals (:asserts (second x)))) els)))) ; pull out the assert refs
           (map (fn [x] (if (symbol? (:label (second x)))
                          (first (str/split (name (:label (second x))) #"\.")) ; pull out the label refs
                          nil)) els)
           )))


(defn build-symtable [t st n]
  (let [k (keyword n)]
    (if (contains? st k)
      (assoc (dissoc st k) k (cons t (k st)))
      (assoc st k (list t)))))


(defn add-graph-elements-to-symtable [st k p]
  (let [ne (pattern-elems p '(node edge))
        st1 (reduce (partial build-symtable (keyword (str (name k) "-decl")))  st (ids ne))
        st2 (reduce (partial build-symtable (keyword (str (name k) "-appl")))  st1 (appls ne)) ]
    st2))



(defn validate-st-entry [[s e]]
  (let [i (name s)]
    (check-stat-sem (implies (in? e :create-appl)
                             (or (in? e :create-decl)
                                 (in? e :read-decl)
                                 (in? e :par)))
                    (str "identifier " i " is used but not declared"))
    (check-stat-sem (not (or (and (some #{:create-decl} e) (some #{:par} e))
                             (and (some #{:read-decl} e) (some #{:par} e))
                             (and (some #{:read-decl} e) (some #{:create-decl} e))))
                    (str "identifier " i " can be declared only once: as a formal parameter, or as a graph element in either 'read' or 'create' part of the rule."))
    (check-stat-sem (implies (some #{:delete} e) (some #{:read-decl} e))
                    (str "deleted element " i " must be declared in 'read' part of the rule"))
    (check-stat-sem (implies (in? e :read-appl) (or (in? e :read-decl) (in? e :par )))
                    (str "identifier " i " referenced in 'read' part should be declared in that part or defined as formal parameter"))
    (check-stat-sem (implies (in? e :create-appl) (or (in? e :read-decl) (in? e :create-decl) (in? e :par )))
                    (str "identifier " i " referenced in 'create' part should be declared in that part or defined as formal parameter"))

    ))



(defn validate-rule [r]
  "validate static semantics of rule"
  (let [st1 (reduce (partial build-symtable :par) {} (:params r))
        st2 (add-graph-elements-to-symtable st1 :read (:read r))
        st3 (add-graph-elements-to-symtable st2 :create (:create r))
        st4 (reduce (partial build-symtable :delete) st3 (:delete r))
        nacs (reduce (fn [xs x] (if (= 'NAC (first x)) (cons (nth x 2) xs) xs)) '() (:els (second (:read r))))
        st5 (reduce (fn [st n] (add-graph-elements-to-symtable st :nac n)) st4 nacs)
        ]
    (doall (map validate-st-entry st5))
    ))

