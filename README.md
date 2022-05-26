[![Clojars Project](https://img.shields.io/clojars/v/leadlab/grape.svg)](https://clojars.org/leadlab/grape)
![logo](https://raw.githubusercontent.com/jenshweber/grape/master/resources/logo.jpg)
# Grape and GrapePress

**_Grape_** is a **G**raph **R**ewriting and **P**ersistence **E**ngine for Clojure.  _**GrapePress**_ is a computational notebook that utilizes _Grape_.

# Installation

## Quickstart - using Docker

Assuming you have Docker installed:

``docker run --name grapepress -p 8999:8999 -p 7474:7474 -p 7687:7687  jenshweber/grape-latest``

Then navigate to http://127.0.0.1:8999/worksheet.html?filename=welcome.clj

Creating further notebooks works the same. If you run notebooks concurrently, you need to update the port mappings. 

## Slowstart - using a local installation

### Prerequisites

* **Clojure** (including Leiningen, requires JDK). Eric Normand has written very nice instructions [here](https://purelyfunctional.tv/guide/how-to-install-clojure/)
* **Neo4J** Grape requires the graph database Neo4j. (Important: Grape has not been migrated to Neo4J v. 4 yet. Please use the latest v. 3 release.) The community edition (free) can be downloaded here: https://neo4j.com/download/

* add the following line to your neo4j.conf:
```
cypher.lenient_create_relationship=true
```

* **Graphviz** Grape uses Graphviz for visualization. [installation](https://graphviz.gitlab.io/download/)

### Create a Grape project
* **Create Clojure project** For example with Leiningen ``lein new grapetest``
* **Add profiles.clj** Create a new file ``profiles.clj`` to contain your neo4j connection info:

```clojure
{:dev {
       :env {:db-url "http://localhost:7474/db/data/"
             :db-usr "<your neo4j user name>"
             :db-pw "<your neo4j password>"}}}
```

* **Edit project.clj** Add the `grape` dependency to your project.clj. Add the `lein-environ` plugin, so that Leiningen can source environment variables from your profiles.clj file. If you want to use the browser-based REPL (Gorilla) for developing your graph transformation rules (recommended) also add the `lein-gorilla` plugin. Your project.clj file will look similar to:

```clojure
(defproject grapetest "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [leadlab/grape "X.X.X"]]
  :plugins [[lein-environ "1.1.0"]
            [org.clojars.benfb/lein-gorilla "0.6.0"]]
  :profiles {:dev {}}
  :repl-options {:init-ns grapetest.core})
```
* **Start it up** Make sure the Neo4j database is running. Start a Gorilla REPL with `lein gorilla`. Open the indicated work sheet. Enter `(use 'grape.core)` to import the Grape and connect to Neo4J. (This may take a few seconds. If you are getting an exception, your database is not running or something is wrong with the connection details.)

* **Load grape** enter `(use 'grape.core)` in the Web repl to load Grape

* **Create a rule** Enter the following to create a rule that creates a node labeled "Hello". (You should see a visualization of the rule after you entered it.)

```clojure
(rule 'hello!
  :create (pattern
             (node 'n1 :label "Hello")))
```

* Execute the rule by calling it: `(hello!)`

* Use the Neo4J browser (http://localhost:7474/browser/) to see that a node was indeed created in the graph database. (enter a simple cipher query: `match (n) return n;`

* Enter another rule that matches the existing "Hello" node and links it to a newly created "Grape" node.

```clojure
(rule 'hello-grape!
   :read (pattern
           (node 'n1 :label "Hello"))
   :create (pattern
             (node 'n2 :label "Grape")
             (edge :label "to" :src 'n1 :tar 'n2)))
```

* Execute that rule `(hello-grape!)` and check with ```(-> any? matches view)``` that the graph was indeed extended.

# Tutorial

Grape comes with an "executable" tutorial worksheet for Gorilla REPL. (Load tutorial.clj into Gorilla REPL.)

A view-only version of that worksheet is available here:
http://viewer.gorilla-repl.org/view.html?source=gist&id=5e8e15ced9089e569a3ef271fc4b4041



Copyright © 2016-22 Jens Weber

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
