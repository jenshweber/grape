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

### Example 5: A rule with _delete_

This rule also deletes matched graph elements. In this case it replaces a "works_for" edge with a new "Contract" node and two edges.

```clojure
(rule 'rewrite_contracts 
      { :theory
        :read (pattern
               (node 'n1)
               (node 'n2)
               (edge 'e {:label "works_for" :src 'n1 :tar 'n2}))
        :delete ['e]
        :create (pattern
                 (node 'n3 {:label "Contract" :asserts {:name "contract"}})
                 (edge 'e1 {:label "employer" :src 'n3 :tar 'n2})
                 (edge 'e2 {:label "employee" :src 'n3 :tar 'n1}))})
```



Copyright © 2016 Jens Weber

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
