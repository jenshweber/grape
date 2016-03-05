# Grape - Graph Rewriting and Persistence Engine 

A Clojure library designed to provide support for graph rewriting based on a persistent graph store.

## Usage
Import the Grape library.
```clojure
(use 'grape.core)
```
Graph rewriting rules are defined using the 'rule' form. Rules consist of three parts:
- the _read_ specifies the graph pattern to be matched in the host graph
- the _delete_ part specifies which graph elements from the matched host graph should be deleted
- the _create_ part specifies which graph elements should be created when the rule is applied

### Example 1: A simple rule to create one node
The following rule creates only one node. It has an empty _read_ and _delete_ part, so it matches any host graph and deletes nothing.

```clojure
(rule 'create-jens 
      {:create 
          (pattern 
            (node 'n {:label "Person" :asserts {:name "Jens"}}))}))
```
The 'node' form is used to specify the node to be created. Note that Grape currently supports only one (optional) type label for nodes, but multiple (optional) property assigments (assert).

Rules can be applied using the _apply-rule_ form. The above rule can be applied like so:
```clojure
(apply-rule 'create-jens)
```
### Example 2: Parameterized rules
Our first example rule was not very versatile, since it could not generate different _persons_. This can be improved by using _parameterized_ rules. The following rule is more generic, as it takes the name of the person to be created as a parameter (p).
```clojure
(rule 'create-person ['p]
      {:create 
       (pattern 
        (node 'n {:label "Person" :asserts {:name 'p}}))})
```
Formal parameters must be actualized when the rule is applied. The rule application below creates a Person node with name "Flo".
```clojure
(apply-rule 'create-person {'p "Flo"})
```

### Example 3: A rule with a _reader_
The the next rule has a _read_ as well as a _create_ part. It matches two Person nodes with the names given as formal parameters and creates a _parent-of_ relationship between them.

```clojure
(rule 'parent_of ['p 'c]
      { :read (pattern 
               (node 'f {:label "Person" :asserts {:name 'c}})
               (node 'j {:label "Person" :asserts {:name 'p}}))
        :create (pattern 
                 (edge 'e {:label "parent_of" :src 'j :tar 'f} )
                 )})

(apply-rule 'parent_of {'p "Jens" 'c "Flo"})
```

### Example 4: Isomorphic vs homomorphic rules
The following rule is similar to Example 3.

```clojure
(rule 'works_for ['e 's] 
      { :read (pattern 
                (node 'f {:label "Person" :asserts {:name 's}})
                (node 'j {:label "Person" :asserts {:name 'e}}))
        :create (pattern 
                 (edge 'e {:label "works_for" :src 'j :tar 'f} ))})
```
It works fine when the _read_ part maps the two nodes in the pattern to two nodes in the host graph, for example:
```clojure
(apply-rule 'works_for {'e "Flo" 's "Jens"})
```
However, we cannot use it to express sitations where a person is self-employed, e.g.,
```clojure
(apply-rule 'works_for {'e "Jens" 's "Jens"})
```
(Note: We are assuming here that there is only one person with name "Jens", i.e., that the person's name is a unique identifier. In that case the above rule application will not find a valid match (and return _nil_). This is because Grape's rule matching engine will search for _isomorphic_ matches of the _read_ pattern in the host graph. This means that the nodes / edges in the read pattern must match to _distinct_ nodes / edges in the host graph. This matching semantics can be changed to _homomorphic_ matches by adding the :homo keyword to the definition of the reader pattern:
```clojure
(rule 'works_for ['e 's] 
      { :read (pattern :homo
                (node 'f {:label "Person" :asserts {:name 's}})
                (node 'j {:label "Person" :asserts {:name 'e}}))
        :create (pattern 
                 (edge 'e {:label "works_for" :src 'j :tar 'f} ))})
```
The above rule allows us to express our "self-employment" example.

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

# Matching
Grape will by default attempt to find an isomorphic match of the _reader_ pattern in the host graph. Consider the application of the our above rewrite rule (example 4) on the followig host graph.

![graph1](https://cloud.githubusercontent.com/assets/1776629/13522111/3d4e4956-e1a2-11e5-9d07-043e0ede3860.png)

The rule replaces 'works_for' edges with a node and two new edges. Given the (default) isomorphic matching semantics, the rule is applicable only once, namely on the edge between _Flo_ and _Anke_. The following figure shows the result of that rule application.

![graph2](https://cloud.githubusercontent.com/assets/1776629/13522256/4c579154-e1a3-11e5-9e50-e8cf62cd04ed.png)

If we want to allow homomorphic matches, we can choose that option in the pattern definition of the rule's _read_ clause. The following modification of example rule 4 does the trick.

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
The homomorphic option allows nodes _n1_ and _n2_ in the reader pattern to map to the _same_ node in the host graph, resulting in the following rewritten graph:

![graph3](https://cloud.githubusercontent.com/assets/1776629/13522361/f6b28d48-e1a3-11e5-8e06-121a7efda890.png)

# Deletions
Grape supports different semantics when it comes to node deletions. Graph rewriting may be based on different theories. The most well known are based on category theory. The so-called Double Pushout (DPO) approach does not allow "dangling" edges when nodes are deleted. The so-called Single Pushout (SPO) approach resolves edges that are left "dangling" when nodes are deleted by deleting those edges as well. Grape supports both approaches, with SPO being the default. The approach used for executing a partiular rule can be specified with the _:theory_ keyword. For example, the DPO rule below applied to the most recent graph above will through an exception as it would result in dangling edges when node _f_ is deleted.
```clojure
(rule {:theory 'dpo
       :read (pattern 
               (node 'f {:label 'Person :asserts {:name "Flo"}}))
       :delete ['f]})
```


## License

Copyright © 2016 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
