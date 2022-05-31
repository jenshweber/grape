(defproject org.clojars.jenshweber/grape "0.4.0-SNAPSHOT"
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
                 [gorillalabs/neo4j-clj "4.1.2"]
                 [org.neo4j.driver/neo4j-java-driver "4.1.1"]
                 [com.taoensso/tufte "2.2.0"]
                 [http-kit "2.4.0-alpha6" :exclusions [ring/ring-core]]
                 [ring/ring-json "0.5.0" :exclusions [org.clojure/clojure]]
                 [cheshire "5.9.0"]
                 [compojure "1.6.1" :exclusions [ring/ring-core ring/ring-json]]
                 [gorilla-renderable "2.0.0"]
                 [gorilla-plot "0.1.4" :exclusions [org.clojure/clojure]]
                 [grimradical/clj-semver "0.2.0" :exclusions [org.clojure/clojure]]
                 [cider/cider-nrepl "0.25.6" :exclusions [org.clojure/clojure]]
                 [nrepl/nrepl "0.7.0"]]
  :main ^:skip-aot gorilla-repl.core
  :plugins []
  :profiles {:dev {}}
  :test-path "test/"
  :jvm-opts ["-Xmx1g" "-server"])
