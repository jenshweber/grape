(defproject leadlab/grape "0.1.0-SNAPSHOT"
  :description "Graph Rewriting And Persistence Engine"
  :url "http://jenshweber.github.io/grape/"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/math.combinatorics "0.1.1"]
                 [org.clojure/data.json "0.2.6"]
                 [dorothy "0.0.6"]
                 [org.clojure/data.codec "0.1.0"]
                 [prismatic/schema "1.0.5"]
                 [clojurewerkz/neocons "3.1.0"]
                 [com.taoensso/timbre "4.3.1"]
                 [environ "1.0.2"]]
  :plugins [[lein-environ "1.0.2"]]
  :profiles {:dev {}}
  :jvm-opts ["-Xmx1g" "-server"]
)

