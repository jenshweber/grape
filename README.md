![logo](https://raw.githubusercontent.com/jenshweber/grape/fgrape/resources/gv-logo.png)
# GrapeVine

**_GrapeVine_** is a functional **G**raph **R**ewriting and **P**ersistence **E**ngine for Clojure.  _**GrapeVine**_ can be used with or without the integrated computational notebook (based on Gorilla REPL). 

**_GrapeVine_** is a fundamentally new release of _Grape_ and _GrapePress_ with the difference that **_GrapeVine_** empraces functional graph transformations, while  _Grape_ and _GrapePress_ used stateful computation. (If you are looking for the old _Grape_ / _GrapePress_ please swith to branch "grape-legacy".)

# Installation

## Quickstart - using Docker

The easiest way to install **_GrapeVine_** is by using Docker. If you don't have Docker, please install it first.

Start **_GrapeVine_** with 
```
 docker compose up
```

> The default configuration will mount the `~/grapevine` directory on the host machine to the `book` directory in the _GrapeVine_ container. You can save your worksheets in that directory and exchange it with the host.

## Slow Start - or for developers

If you don't want to use Docker, you need to install Clojure, Leiningen and Graphviz before starting _GrapeVine_ with "lein run". See the Wiki for details.

# Tutorial

_GrapeVine_ comes with an "executable" tutorial worksheet in the "help" directory. Simply load it by hitting control-g control-l.

Here is a [read-only version of the tutorial](https://web.uvic.ca/~jens/gv/view.html?source=github&user=jenshweber&repo=grape&path=help/tutorial.clj) 
and here is [part 2](https://web.uvic.ca/~jens/gv/view.html?source=github&user=jenshweber&repo=grape&path=help/tutorial2.clj) .


Copyright © 2016-23 Jens Weber

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
