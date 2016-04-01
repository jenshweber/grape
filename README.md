# Grape - Graph Rewriting and Persistence Engine 

A Clojure library designed to provide support for graph rewriting based on a persistent graph store.

## Usage
Import the Grape library.
```clojure
(use 'grape.core)
```
Graph rewriting rules are defined using the ```rule``` form. Rules consist of three parts:
- the ```:read``` specifies the graph pattern to be matched in the host graph
- the ```:delete``` part specifies which graph elements from the matched host graph should be deleted
- the ```:create``` part specifies which graph elements should be created when the rule is applied

### Example 1: A simple rule to create one node
The following rule creates only one node. It has an empty ```:read``` and ```delete``` part, so it matches any host graph and deletes nothing.

```clojure
(rule 'create-jens! 
      {:create 
          (pattern 
            (node 'n {:label "Person" :asserts {:name "Jens"}}))}))
```
The ```node``` form is used to specify the node to be created. Grape currently supports only one (optional) type label for nodes, but multiple (optional) property assigments (assert).
The visual representation of the above rule is given in the image below. Here we use the popular "inline" notation of rewrite rules, where green coloured shapes mark those graph elements that are being created, i.e., graph elements that appear on the right hand side of the rule, but not on the left-hand side.

![createJens](https://raw.githubusercontent.com/jenshweber/grape/master/doc/images/create-jens!.png)

Grape supports automatic generation of rule visualizations based on the popular Graphviz tool. Each rule definition automatically creates a function to emit the rule in Graphviz (dot) format. The name of that function is <rulename>-dot. For example, the following function call will return the visual prepresentation of the above example rule:

```clojure
(create-jens!-dot)
```
Visual representations can also be saved as image files to the file system by calling function ```document-rule``` for a defined rule, or ```document-rules``` for all defined rules:
```clojure
(document-rule 'create-jens!) ; saves a PNG visual representation of rule 'createJens!
(document-rules) ; saves PNG visual representations for all defined rules
```
Indeed, if Lighttable is used as the IDE, the visual rule representation can be "inlined" within the IDE. This function requires the NerdyPainter plugin.

#### Applying a rule ####

Defining a rule results in the creation of a new function with the name of the rule. The rule can be applied by calling that function:
```clojure
(create-jens!)
```
The call to the rule function returns ```true``` if (and only if) the rule application succeeds. 

### Example 2: Parameterized rules
Our first example rule was not very versatile, since it could not generate different _persons_. This can be improved by using _parameterized_ rules. The following rule is more generic, as it takes the name of the person to be created as a parameter (p).
```clojure
(rule 'create-person! ['p]
      {:create 
       (pattern 
        (node 'n {:label "Person" :asserts {:name 'p}}))})
```
![createJens](https://raw.githubusercontent.com/jenshweber/grape/master/doc/images/create-person!.png)

Formal parameters must be actualized when the rule is applied. The rule application below creates a Person node with name "Flo".
```clojure
(create-person! {'p "Flo"})
```

### Example 3: A rule with a _reader_
The the next rule has a _read_ as well as a _create_ part. It matches two Person nodes with the names given as formal parameters and creates a _parent-of_ relationship between them.

```clojure
(rule 'parent_of! ['p 'c]
      { :read (pattern 
               (node 'f {:label "Person" :asserts {:name 'c}})
               (node 'j {:label "Person" :asserts {:name 'p}}))
        :create (pattern 
                 (edge 'e {:label "parent_of" :src 'j :tar 'f} )
                 )})

(parent_of! {'p "Jens" 'c "Flo"})
```
![createJens](https://raw.githubusercontent.com/jenshweber/grape/master/doc/images/parent_of!.png)

### Example 4: Isomorphic vs homomorphic rules
The following rule is similar to Example 3.

```clojure
(rule 'works_for! ['e 's] 
      { :read (pattern 
                (node 'f {:label "Person" :asserts {:name 's}})
                (node 'j {:label "Person" :asserts {:name 'e}}))
        :create (pattern 
                 (edge 'e {:label "works_for" :src 'j :tar 'f} ))})
```
![createJens](https://raw.githubusercontent.com/jenshweber/grape/master/doc/images/works_for!.png)

It works fine when the _read_ part maps the two nodes in the pattern to two nodes in the host graph, for example:
```clojure
(works_for! {'e "Flo" 's "Jens"})
```
However, we cannot use it to express sitations where a person is self-employed, e.g.,
```clojure
(works_for! {'e "Jens" 's "Jens"})
```
(Note: We are assuming here that there is only one person with name "Jens", i.e., that the person's name is a unique identifier. In that case the above rule application will not find a valid match (and return ```nil```). This is because Grape's rule matching engine will search for _isomorphic_ matches of the _read_ pattern in the host graph. This means that the nodes / edges in the read pattern must match to _distinct_ nodes / edges in the host graph. This matching semantics can be changed to _homomorphic_ matches by adding the :homo keyword to the definition of the reader pattern:
```clojure
(rule 'works_for! ['e 's] 
      { :read (pattern :homo
                (node 'f {:label "Person" :asserts {:name 's}})
                (node 'j {:label "Person" :asserts {:name 'e}}))
        :create (pattern 
                 (edge 'e {:label "works_for" :src 'j :tar 'f} ))})
```
The above rule allows us to express our "self-employment" example.

### Example 5: A rule with _delete_

The following rule also deletes matched graph elements. In this case it replaces a "works_for" edge with a new "Contract" node and two edges.

```clojure
rule 'rewrite_contract!
      { :read (pattern
               (node 'n1)
               (node 'n2)
               (edge 'e {:label "works_for" :src 'n1 :tar 'n2}))
        :delete ['e]
        :create (pattern
                 (node 'n3 {:label "Contract" :asserts {:name "Contract" :with 'n1.name}})
                 (edge 'e1 {:label "employer" :src 'n3 :tar 'n2})
                 (edge 'e2 {:label "employee" :src 'n3 :tar 'n1}))})
```
![createJens](https://raw.githubusercontent.com/jenshweber/grape/master/doc/images/rewrite_contract!.png)
Another interesting aspect abot the above rule is that the _create_ part of the rule copies an attribute from a graph element matched in the _read_ part of the rule (```n1.name```).

### Example 6: Dealing with "dangling" edges
Consider the following rule whose purpose it is to "fire" an employee with a given name (by deleting the contract node). 
```clojure
(rule 'fire-employee! ['name] 
      {:read (pattern
              (node 'emp {:label "Person" :asserts {:name 'name}})
              (node 'con)
              (edge 'e {:label "employee" :src 'con :tar 'emp}))
       :delete ['con]})
```
![createJens](https://raw.githubusercontent.com/jenshweber/grape/master/doc/images/fire-employee!.png)

But what happens to the 'employee' edge _e_ when the contract node _con_ is deleted? It can't be left "dangling", as that would result in an invalid graph. Graph transformation systems may be based on different theoretical foundations. Algebraic theories for graph transformation systems may be based on different approaches, including the so-called double pushout (DPO) approach and the single pushout (SPO) approach. We won't dive into the theory here, but what is important at this point is that these approaches differ in their treatment of "dangling" edges during node deletion. DPO rules will disallow dangling edges while SPO rules resolve dangling edges by also deleting them from the host graph. The default rule semantics is SPO in Grape. However, a different rule semantics can be specified. The following rule is identical to the previous but specifies DPO semantics. Applying it to our host graph will not be allowed if the application would cause any dangling edges.
```clojure
(rule 'fire-employee! ['name] 
      {:theory 'dpo
       :read (pattern
              (node 'emp {:label "Person" :asserts {:name 'name}})
              (node 'con)
              (edge 'e {:label "employee" :src 'con :tar 'emp}))
       :delete ['con]})
```
### Example 7: Rules with Negative Application Conditions (NACs)
Negative applications conditions (NACs) are conditions that, if met, inhibit a rule from being applied. Consider the ```works_for!``` rule from Example 4. You may want to specify that a 'works_for' edge is created between two persons _only_ if there isn't already such an edge in the graph. A NAC can be used to accomplish this, as seen in the following rule:
```clojure
(rule 'works_for2! ['e 's]
      { :read (pattern
               (node 'f {:label "Person" :asserts {:name 's}})
               (node 'j {:label "Person" :asserts {:name 'e}})
               (NAC 1
                (edge 'e1 {:label "works_for" :src 'j :tar 'f} )))
        :create (pattern
                 (edge 'e2 {:label "works_for" :src 'j :tar 'f} ))
        })
````
NACs are specified using 'NAC' forms, which essentially specifiy graph patterns that, if matched in the context of the _read_ part, will inhibit the application of the rule. Grape allows multiple NACs per rule. The visual representation of NAC's uses dashed borders, with different NACs rendered in different colours. 

![works_for1!](https://raw.githubusercontent.com/jenshweber/grape/master/doc/images/works_for2!.png)

### Example 8: Repeatedly applying a rule
A rule can be applied repeatedly using the _while_ form. Given the following example rule that deletes any node, we can simply delete the entire graph using _while_:
```clojure
(rule 'delete-any-node!
      {:read (pattern (node 'n))
       :delete ['n]})

(while (delete-any-node!))
```
![createJens](https://raw.githubusercontent.com/jenshweber/grape/master/doc/images/delete-any-node!.png)

## Syntax checks and static analysis
Grape implements checks for syntactical ans static semantical correctness and will through exceptions if errors are found during rule definition. For example the following rule is considered incorrect with respect to Grape's syntax definition, as the rule name is a string and not a symbol:
```clojure
 (rule "testrule"
       {:create
        (pattern
         (node 'n {:label "Person" :asserts {:name "Jens"}}))})
```
An exception with the following message will be thrown in this case:
```
Grape syntax error: rule name must be a symbol
 Expected syntax: 
RULE          :- ( rule NAME <[PAR+]> { <:theory 'spo|'dpo> <:read PATTERN> <:delete [ID+]> <:create PATTERN> } ) 
NAME, PAR, ID :- *symbol* 
PATTERN       := (pattern ...)
 ... where <> denotes an optional element, | denotes an alternative choice, and N+ denotes a list of elements
```
Likewise, here is an example for a syntactically correct rule that has problems with respect to static semantics. (In this case an undeclared identifier is referenced.)
```clojure
(rule 'testrule
       {:create
        (pattern
         (node 'n {:label "Person" :asserts {:name 'id}}))})
```
The exception thrown may look as follows:
```
Grape static analysis error: identifier id is used but not declared
```
Note, though, that Grape is schema-less, i.e., there is no need / ability to define a graph schema type for rules. Thus, Grape has no means of checking whether rule definitions are compliant to a particular graph class.

Copyright © 2016 Jens Weber

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
