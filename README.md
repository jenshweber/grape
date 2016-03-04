# Grape - Graph Rewriting and Persistence Engine 

A Clojure library designed to provide support for graph rewriting based on a persistent graph store.

## Rule definition

Graph rewriting rules are defined using the 'rule' form. Rules consist of three parts:
- the _read_ specifies the graph pattern to be matched in the host graph
- the _delete_ part specifies which graph elements from the matched host graph should be deleted
- the _create_ part specifies which graph elements should be created when the rule is applied

### Example 1: A simple rule to create one node
The following rule creates only one node. It has an empty _read_ and _delete_ part, so it matches any host graph and deletes nothing.

```clojure
(rule { :create 
          (pattern 
            (node 'n {:label 'Person :asserts {:name "Jens"}}))}))
```
The 'node' form is used to specify the node to be created. Note that Grape currently supports only one (optional) type label for nodes, but multiple (optional) property assigments (assert).

### Example 2: A simple rule with a _reader_
The the next rule has a _read_ as well as a _create_ part. Two nodes of type _Person_ are matched and connected with a newly created edge.

```clojure
(rule {:read (pattern 
                (node 'f {:label 'Person :asserts {:name "Flo"}})
                (node 'j {:label 'Person :asserts {:name "Jens"}}))
       :create (pattern 
                  (edge 'e {:label 'parent_of :src 'j :tar 'f} ))}))
```

### Example 3: This example create a bit more

```clojure
(rule {:read (pattern 
                (node 'f {:label 'Person :asserts {:name "Flo"}})
                (node 'j {:label 'Person :asserts {:name "Jens"}}))
       :create (pattern 
                (node 'a {:label 'Person :asserts {:name "Anke"}})
                (edge 'e1 {:label 'works_for :src 'f :tar 'a} )
                (edge 'e2 {:label 'works_for :src 'j :tar 'j} )
               )}))
```
### Example 4: A rule with _delete_

This rule also deletes matched graph elements.

```clojure
(rule  { :read (pattern
                  (node 'n1)
                  (node 'n2)
                  (edge 'e {:label 'works_for :src 'n1 :tar 'n2}))
         :delete ['e]
         :create (pattern
                  (node 'n3 {:label 'Role :asserts {:name 'work}})
                  (edge 'e1 {:label 'offers :src 'n3 :tar 'n2})
                  (edge 'e2 {:label 'takes :src 'n3 :tar 'n1}))}))
```

## Rule semantics

Grape will by default attempt to find an isomorphic match of the _reader_ pattern in the host graph. Consider the application of the our above rewrite rule (example 4) on the followig host graph.
![graph1](https://cloud.githubusercontent.com/assets/1776629/13522111/3d4e4956-e1a2-11e5-9d07-043e0ede3860.png)
The rule replaces 'works_for' edges with a node and two new edges. Given the (default) isomorphic matching semantics, the rule is applicable only once, namely on the edge between _Flo_ and _Anke_. The following figure shows the result of that rule application.
![graph2](https://cloud.githubusercontent.com/assets/1776629/13522256/4c579154-e1a3-11e5-9e50-e8cf62cd04ed.png)
IF we want to allow homomorphic matches, we can choose that option in the pattern definition of the rule's _read_ clause. The following modification of example rule 4 does the trick.

```clojure
(rule  { :read (pattern :homo
                  (node 'n1)
                  (node 'n2)
                  (edge 'e {:label 'works_for :src 'n1 :tar 'n2}))
         :delete ['e]
         :create (pattern
                  (node 'n3 {:label 'Role :asserts {:name 'work}})
                  (edge 'e1 {:label 'offers :src 'n3 :tar 'n2})
                  (edge 'e2 {:label 'takes :src 'n3 :tar 'n1}))}))
```


## License

Copyright © 2016 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
