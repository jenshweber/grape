;; gorilla-repl.fileformat = 1

;; **
;;; ![logo](https://raw.githubusercontent.com/jenshweber/grape/master/resources/logo.jpg)
;;; 
;;; # Welcome!
;;; 
;;; @@GRAPEpress@@ is a computational notebook for graph transformations (among other things). @@GRAPEpress@@ is built on top of Gorilla.
;;; 
;;; A computational notebook consists of mulltiple _worksheets_ (you are looking at one). Each worksheet consists of _static_ and _executable_ _segments_. 
;;; 
;;; ## Static segments
;;; Static segments can contain text using markdown, html, @@\LaTeX@@, images and fancy things like Mermaid diagrams that can be edited online (see below, you may want to right-click links and open in a new window).
;;; [![Mermaid diagrams](https://mermaid.ink/img/pako:eNptkU1PwzAMhv9K5BOI9Q9EXBDbJA6cdkOVkJt4ndV8QD40weh_Jy1rGR0-OY_tV2_sEyivCSQogzGuGduAtnaixINji0bcf1WVWGfVXdMtx8M1faEm4B8sxR27JLClJd6lwK4VLTlN4bI4jMRntCW9uV0ULCaa4Gh7tHf6AWIWbQi7R298mAvxyHYaLM_3jKqb3v2l3vCxWa8avEf-pCe3JUozVug2mP6dH1fwa6jx3giOr0c2eoYhu8XsFLACS8Ei63KXUaWGdCBLNciSatpjNqmG2vWlNb_pso-N5uQDyD2aSCvAnPzuwymQKWSams7nPXf130Plmog)](https://mermaid.live/edit#pako:eNptkU1PwzAMhv9K5BOI9Q9EXBDbJA6cdkOVkJt4ndV8QD40weh_Jy1rGR0-OY_tV2_sEyivCSQogzGuGduAtnaixINji0bcf1WVWGfVXdMtx8M1faEm4B8sxR27JLClJd6lwK4VLTlN4bI4jMRntCW9uV0ULCaa4Gh7tHf6AWIWbQi7R298mAvxyHYaLM_3jKqb3v2l3vCxWa8avEf-pCe3JUozVug2mP6dH1fwa6jx3giOr0c2eoYhu8XsFLACS8Ei63KXUaWGdCBLNciSatpjNqmG2vWlNb_pso-N5uQDyD2aSCvAnPzuwymQKWSams7nPXf130Plmog)
;;; 
;;; Further info can be found on the [Gorilla web site](http://gorilla-repl.org/start.html).
;;; 
;;; ## Executable segments
;;; 
;;; Executable code is written in Clojure. There are several renderers for visualizing computational results.
;;; 
;;; Graph transformations are written in the [GRAPE language](http://viewer.gorilla-repl.org/view.html?source=github&user=jenshweber&repo=grape&path=tutorial.clj), an internal DSL to Clojure.
;;; 
;;; Shift + enter evaluates code. 
;;; 
;;; ## Graph persistence
;;; Graphs are persistent and shared across all worksheets in the same book (container). (Use ``(browse)`` do view the graph and ``(clear!)`` do delete it.)
;;; 
;;; @@GRAPEpress@@ uses the graph database Neo4J underneath. The [Neo4J graph browser](http://localhost:7474/browser/) is also available. (login: neo4j / password: grape)
;;; 
;;; ## Commands
;;; 
;;; Press ctrl+g twice in quick succession or click the menu icon (upper-right corner) for an overview on commands.
;;; 
;;; ## Namespaces
;;; It's a good habit to run each worksheet in its own namespace: feel free to use the declaration we've provided below if you'd like.
;; **

;; @@
(ns worksheet1
  (:require [gorilla-plot.core :as plot]
            [grape.core :refer :all]))
;; @@
