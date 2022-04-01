(defproject leadlab/grape "0.3.0-SNAPSHOT"
  :description "Graph Rewriting And Persistence Engine"
  :url "http://jenshweber.github.io/grape/"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [org.clojure/math.combinatorics "0.1.1"]
                 [org.clojure/data.json "0.2.6"]
                 [dorothy "0.0.6"]
                 [org.clojure/data.codec "0.1.0"]
                 [environ "1.0.2"]
                 [nano-id "0.9.3"]
                 [proto-repl "0.3.1"]
                 [prismatic/schema "1.0.5"]
                 [gorilla-graph "0.1.0-SNAPSHOT"]
                 [gorillalabs/neo4j-clj "4.1.2"]
                 [org.neo4j.driver/neo4j-java-driver "4.1.1"]
                 [com.taoensso/tufte "2.2.0"]
                 [org.clojars.benfb/gorilla-repl "0.6.0"]]
  :plugins [[org.clojars.benfb/lein-gorilla "0.6.0"]
            [lein-environ "1.1.0"]]
  :profiles {:dev {}}
  :test-path "test/"
  :jvm-opts ["-Xmx1g" "-server"]
  :deploy-repositories [["clojars" {:url "https://clojars.org/repo"
                                    :username :env/clojars_user
                                    :password  :env/clojars_pass
                                    :sign-releases false}]])
