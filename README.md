[![Clojars Project](https://img.shields.io/clojars/v/leadlab/grape.svg)](https://clojars.org/leadlab/grape)

# Grape - Graph Rewriting and Persistence Engine 

Persistent Graph Rewriting for Clojure.

# News

* Release 0.2.1 - Path expressions added

* Release 0.2 - Syntax simplification. Removed the need for a lot of curly braces in Grape rules. Also, node and edge handles are optional now. Makes rules simpler.

* Release 0.1.1 - added mechanism to "pass on" nodes from one rule to another

# Installation

Below is a description of two alternative ways to install Grape. The "Quickstart" way uses a prepared VM (with much of the installation work already done for you), while the "Slowstart" way describes a local installation.

## Quickstart - using a Vagrant VM image

Grape can be installed using a prepared Vagrant VM. This assumes that you have [Vagrant installed](https://www.vagrantup.com/docs/installation).

* Download and initialize the VM with `vagrant init jenshweber/grape`
* Edit the `Vagrantfile` that was created by adding these port forwards: (feel free to forward to a different port if these ports collide with existing ones)

```
  config.vm.network "forwarded_port", guest: 7687, host: 7687
  config.vm.network "forwarded_port", guest: 7474, host: 7474
  config.vm.network "forwarded_port", guest: 8080, host: 8080
```

You may also want to allocate some additional resources:

```
  config.vm.provider "virtualbox" do |vb|
    vb.memory = "4096"
    vb.cpus = "3"
  end
```

* Download and start the VM: `vagrant up`. (This will take some time to download the VM on the first call.)

* Open a shell at the VM `vagrant ssh` 

* Then `cd grapetest` and start a Grape REPL with `lein gorilla :ip 0.0.0.0 :port 8080`

* Now just navigate to http://0.0.0.0:8080/worksheet.html?filename=welcome.clj

* You can get the Neo4J browser at http://localhost:7474 (username neo4j, password grape)

## Slowstart - using a local installation

### Prerequisites

* **Clojure** (including Leiningen, requires JDK). Eric Normand has written very nice instructions [here](https://purelyfunctional.tv/guide/how-to-install-clojure/)
* **Neo4J** Grape requires the graph database Neo4j. (Important: Grape has not been migrated to Neo4J v. 4 yet. Please use the latest v. 3 release.) The community edition (free) can be downloaded here: https://neo4j.com/download/
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
  {:create (pattern
             (node 'n1 {:label "Hello"}))})
```

* Execute the rule by calling it: `(hello!)`

* Use the Neo4J browser (http://localhost:7474/browser/) to see that a node was indeed created in the graph database. (enter a simple cipher query: `match (n) return n;`

* Enter another rule that matches the existing "Hello" node and links it to a newly created "Grape" node.

```clojure
(rule 'hello-grape!
  {
   :read (pattern
           (node 'n1 {:label "Hello"}))
   :create (pattern
             (node 'n2 {:label "Grape"})
             (edge 'e {:label "to" :src 'n1 :tar 'n2}))})
```

* Execute that rule `(hello-grape!)` and check with the Neo4J browser that the graph was indeed extended.

# Tutorial

Grape comes with an "executable" tutorial worksheet for Gorilla REPL. (Load tutorial.clj into Gorilla REPL.)

A view-only version of that worksheet is available here:
http://viewer.gorilla-repl.org/view.html?source=github&user=jenshweber&repo=grape&path=tutorial.clj



Copyright © 2016-20 Jens Weber

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
