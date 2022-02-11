(defproject grapepress "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [leadlab/grape "0.3.0-SNAPSHOT"]]
  :plugins [[lein-environ "1.1.0"]
            [org.clojars.benfb/lein-gorilla "0.7.0"]]
  :profiles {:dev {}}
  :repl-options {:init-ns grapepress.core})
