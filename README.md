![logo](https://raw.githubusercontent.com/jenshweber/grape/fgrape/resources/gv-logo.png)
# GrapeVine

**_GrapeVine_** is a functional **G**raph **R**ewriting and **P**ersistence **E**ngine for Clojure.  _**GrapeVine**_ can be used with or without the integrated computational notebook (based on Gorilla REPL). 

**_GrapeVine_** is a fundamentally new release of _Grape_ and _GrapePress_ with the difference that **_GrapeVine_** empraces functional graph transformations, while  _Grape_ and _GrapePress_ used stateful computation. (If you are looking for the old _Grape_ / _GrapePress_ please swith to branch "grape-legacy".)

# Installation

## Quickstart - using Docker

The easiest way to install **_GrapeVine_** is by using Docker. If you don't have Docker, please install it first.

 **_GrapeVine_** is based on the Neo4J graph database. We will run Neo4J in _one_ Docker container and _GrapeVine_ in a another container. The installation takes three steps:
 
 ### Step 1. Create a network between the two Docker containers:    
 ``docker network create grapenet`` 
 
 > This creates a network called "grapenet". Of course, you can give it a different name. If you do, make sure to use it below, too.

### Step 2: Start Neo4J:

```
docker run --name neo4j -p 7474:7474 -p 7687:7687 --net grapenet -d -v $HOME/neo4j/data:/data -v $HOME/neo4j/logs:/logs -v $HOME/neo4j/import:/var/lib/neo4j/import -v $HOME/neo4j/plugins:/plugins -e NEO4JLABS_PLUGINS=\[\"apoc\"\] -e NEO4J_apoc_export_file_enabled=true -e NEO4J_AUTH=none -e NEO4J_apoc_import_file_use__neo4j__config=true -e NEO4J_apoc_import_file_enabled=true -e NEO4J_cypher_lenient__create__relationship=true -e NEO4JLABS_PLUGINS=\[\"apoc\"\] neo4j
 ```

> This runs a Neo4J container (and downloads it if need be). The command also connects the container to the network we created. Note that the command switches off authentication (to the Neo4J database). This may not want to be what you want if you install this on a public system. If you do want authentication, you may need to edit the password in the _GrapeVine_ profiles.clj file.

After the above command, you should be able to access the Neo4J browser at http://localhost:7474/browser/

### Step 3. Start _GrapeVine_:
 
```
docker run -it -v ~/grapevine:/usr/src/app/book \
--net grapenet \
--name grapevine \
-p 8999:8999 -p 62222:62222 \
jenshweber/grapevine
```

> The above command starts a _GrapeVine_ constainer and mounts the `~/grapevine` directory on the host machine to the `book` directory in the _GrapeVine_ container. You can save your worksheets in that directory and exchange it with the host.

## Slow(er) Start - without using Docker

If you don't want to use Docker, you need to install Clojure, Leiningen and Graphviz before starting _GrapeVine_ with "lein run".

# Tutorial

_GrapeVine_ comes with an "executable" tutorial worksheet in the "help" directory. Simply load it by hitting control-g control-l.

Here is a [read-only version of the tutorial](https://web.uvic.ca/~jens/gv/view.html?source=github&user=jenshweber&repo=grape&path=help/tutorial1.clj).



Copyright © 2016-23 Jens Weber

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
