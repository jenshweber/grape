;; gorilla-repl.fileformat = 1

;; **
;;; 
;; **

;; **
;;; # Tutorial - Part 2: Taking control
;;; 
;;; This is the second part of the _GrapeVine_ tutorial. Readers unfamiliar with the [first part](worksheet.html?filename=help/tutorial1.clj) should start there.
;;; 
;;; 
;;; 
;; **

;; @@
(ns tutorial2
  (:require [grape.core :refer :all]))
;; @@

;; **
;;; # Graph Constraints
;;; 
;;; A _graph constraint_ defines a (Boolean) condition on the state of graphs. 
;;; 
;;; ## Basic constraints
;;; 
;;; The most basic form of a constraint is just called that, a _basic constraint_. It simply describes a pattern to be present in a given graph. The meaning of a basic constraintcan @@C@@ can be described as @@\exists C@@. 
;;; 
;;; Consider the following basic constraint, which matches a `Hello` node that is connected to a `Grape` node via a `to` edge.
;; **

;; @@
(constraint hello-to->grape! []
            (node h:Hello)
            (node p:Grape)
            (edge :to h p))
;; @@

;; **
;;; - _Note: By convention, names of constrains should end with a exclamation mark._
;;; - _Another note: Graph constraints always use injective matching (as indicated by the hooked arrow)._
;; **

;; **
;;; 
;; **

;; **
;;; ## Using constraints to filter _grapes_
;;; 
;;; Like rules, constraints can be applied to _grapes_ and "sieve" out those graphs that do not satisfy the constraint. Applications can be positive or negated.
;;; 
;;; Let's try:
;; **

;; @@
(hello-to->grape! (newgrape))
;; @@

;; **
;;; As expected, the above pattern cannot be found in an empty graph.
;;; 
;;; Let's apply the constraint in **negated form**. (i.e., we assert that the "hello-to->grape!" pattern must _not_ exist). This is done by appending a minus sign (**-**) to the call of the constraint.
;; **

;; @@
(hello-to->grape!- (newgrape))
;; @@

;; **
;;; We we see above that the empty graph satisfies this negated constraint.
;;; 
;;; Conversely, let's generate a graph that satisfies the positive constraint (see below).
;;; 
;;; > _ Note: we will use the rules again from the first part of the tutorial._
;; **

;; @@
(rule hello []
      (create
       (node :Hello)))

(rule world []
      (read
       (node h:Hello))
      (create
       (node w:World)
       (edge :to h w)))

(rule vine []
      (read
       (node h:Hello)
       (node w:World)
       (edge e:to h w))
      (delete w e)
      (create
       (node g:Grape {:calories "104"})
       (edge :to h g)))

(-> (newgrape) hello world vine hello-to->grape!)
;; @@

;; **
;;; Observe that testing a graph constraint only returns a graph _if_ it satisfies the constraint.
;;; 
;;; > **_Graph constraints act as filters on _grapes_. They filter out all graphs that do not satisfy the constraint._**
;;; 
;;; Let's see another example with the `rel` rule we have seen earlier (part 1). As previously noted, applying that rule to a graph with two `Hello` nodes produces two unique graphs (four if we use a starred rule application).
;;; 
;; **

;; @@
(rule rel []
      (read
       (node h:Hello)
       (node h2:Hello))
      (create
       (edge e:rel h h2)))

(-> (newgrape) hello hello rel view)
;; @@

;; **
;;; Now let's consider the following constraint `self-related!`, which matches a node that has a relationship with itself:
;; **

;; @@
(constraint self-related! []
            (node h)
            (edge e:rel h h))
;; @@

;; **
;;; As we can see below, applying the graph constraint filters out those graphs that do not contain a "self-related" node:
;; **

;; @@
(-> (newgrape) hello hello rel self-related! view)
;; @@

;; **
;;; Conversely, applying the negated constraint exclude graphs that satisfy the constraint:
;; **

;; @@
(-> (newgrape) hello hello rel self-related!- view)
;; @@

;; **
;;; 
;; **

;; **
;;; ## Conditional Constraints
;;; 
;;; Conditional constraints have the general form **if @@X@@ then @@C@@**, where @@X@@ is a graph pattern and @@C@@ is an _extension_ of that graph pattern. 
;;; 
;;; Below is an example for a conditional constraint. A graph satisfies this constraint if (and only if) all `Hello` nodes that can be found are connected to a `Grape` node by a `to` edge.
;;; 
;;; As we can see, the pattern that belongs to the "if" part is represented in black colour.
;; **

;; @@
(cond-constraint if-hello-then-to-grape! []
                 (IF   (node h:Hello))
                 (THEN (node p:Grape)
                       (edge :to h p)))
;; @@

;; **
;;; An empty graph satisfies this constraint:
;; **

;; @@
(-> (newgrape) if-hello-then-to-grape!)
;; @@

;; **
;;; The following graph does not:
;; **

;; @@
(-> (newgrape) hello world if-hello-then-to-grape!)
;; @@

;; **
;;; But this one does:
;; **

;; @@
(-> (newgrape) hello world vine if-hello-then-to-grape! view)
;; @@

;; **
;;; ## Atomic constraints
;;; It should be clear that basic constraints are (special cases) of conditional constraints, where the `IF` part is an empty pattern. Following the terminology introduced by [Orejas et al.](https://dl.acm.org/doi/10.5555/1792838.1792857), we simply refer to an _atomic constraint_ if we do not wish to distinguish between a constraint in either basic or conditional form. 
;;; 
;;; ## Constraints with parameters
;;; 
;;; Similar to rules and queries, constraints can have parameters. This makes them more versatile. Consider the example below, where the node type is parameterized. The parameter (`lab`) determines the type of node `h` in the constraint.
;; **

;; @@
(constraint multiple-to! [lab]
            (node h:&lab)
            (node n1)
            (node n2)
            (edge :to h n1)
            (edge :to h n2))
;; @@

;; **
;;; Now, let's use the parameterized constraint to ensure that a graph has a `Hello` node with two `to` edges to other nodes:
;; **

;; @@
(-> (newgrape) hello world world (multiple-to! "Hello"))
;; @@

;; **
;;; ## Constraint clauses
;;; 
;;; In mathematical logic, a _clause_ is a formula of the form @@L_1 \vee ... \vee L_n@@, where each @@L_i@@ is a positive or negative literal. 
;;; 
;;; _GrapeVine_ provides the `constraint-clause` form for defining constraints in clausal form, where each literal is a positive or negative atomic constraint. For example, the form below defines a constraint clause (named `self-or-not-hello-to-grape!`) as a disjunction of the two atomic constraints `self-related!` and `if-hello-then-to-grape!`).
;; **

;; @@
(constraint-clause  self-or-not-hello-to-grape!
                    [self-related! if-hello-then-to-grape!])
;; @@

;; **
;;; Constraints defined in clausal form can be applied to _grapes_ in the same way as atomic constraints, except that their negation is not defined. See below for an example.
;; **

;; @@
(-> (newgrape) hello world vine self-or-not-hello-to-grape!)
;; @@

;; **
;;; # Schemas
;;; 
;;; In contrast to most other graph transformation tools, @@GrapeVine@@ graphs do not need to be typed. We believe that schema-less programming promotes exploration, rapid prototyping, and evolution. 
;;; 
;;; However, it is quite possible to define _graph schemas_ if consistency constraints should be enforced. It is up to the user to choose how much or how little schema they desire. 
;;; 
;;; Schemas are defined using the __`schema`__ form
;;; 
;;; Syntax: __`(schema name [c1 ... cn])`__, where 
;;; * `name` is a name for the schema, and
;;; * `c1` .. `cn` are constraints.
;;; 
;;; Let's consider a simple example. We define a constraint that forbids the existence of a `Hello` node with multiple outgoing `to` edges. We do so by defining a basic constraint and then using its negation:
;; **

;; @@
(constraint multiple-to! []
            (node h:Hello)
            (node n1)
            (node n2)
            (edge :to h n1)
            (edge :to h n2))
;; @@

;; **
;;; Let's define a schema and call it `myschema`:
;; **

;; @@
(schema myschema [multiple-to!-])
;; @@

;; **
;;; At this point, the schema has been defined, but it has not been attached to any graph yet. Let's define two graphs and attach the schema to one of them:
;; **

;; @@
(def g1 (newgrape))
(def g2 (newgrape))
(myschema g2)
;; @@

;; **
;;; The above command has attached our schema to graph `g2`.
;;; 
;;; Note that a schema is a function on graphs (@@grapes@@). So, we could also just write the following:
;; **

;; **
;;; 
;; **

;; @@
(def g2 (-> (newgrape) myschema))
;; @@

;; **
;;; 
;; **

;; **
;;; Now let's see the effect of the schema constraint. The following derivation should not be allowed in the schema that's attached to graph `g2` since it would create a `Hello` node connected to two `World` nodes:
;; **

;; @@
(-> g2 hello world world)
;; @@

;; **
;;; Indeed, the above operation returns no graph (an empty _grape_). 
;;; 
;;; Of course, applying the same to our schemaless graph `g1` works without a problem:
;; **

;; @@
(-> g1 hello world world view)
;; @@

;; **
;;; >_Note: Schema constraints are "inherited" during derivation. In other words, a graph that was created as a result of a rule application has the same schema as the graph it was derived from. _
;; **

;; **
;;; 
;; **

;; **
;;; 
;; **

;; **
;;; ## Listing and Dropping Schema Constraints
;;; 
;;; We can list the schema constraints defined on a graph using the `__schema-list__` form. For example:
;; **

;; @@
(schema-list g2)
;; @@

;; **
;;; while...
;; **

;; @@
(schema-list g1)
;; @@

;; **
;;; We can drop constraints by adding a `__drop__` to the schema name.
;; **

;; @@
(myschema-drop g2)
;; @@

;; @@
(schema-list g2)
;; @@

;; **
;;; > __Note__: Constraints (and rules) are currently not "stored procedures" in the Neo4J database. The schema is stored in the database only with the names of the constraints. This means that you need to (re)evaluate the declaration of any constraints that are referenced in a graph's schema prior to working with the graph.
;;; 
;;; ## Using parameterized constraints in schema definitions
;;; 
;;; Parameterized constraints can also be used in schema definitions, but their parameters must be actualized and they must be given unique names. The __`enforce`__ form is used for this purpose. Consider the following example of a parameterizable version of the `multiple-to` constraint, which takes the label of node `h` as a parameter:
;; **

;; @@
(constraint multiple-to! [lab]
            (node h:&lab)
            (node n1)
            (node n2)
            (edge :to h n1)
            (edge :to h n2))
;; @@

;; **
;;; The following schema uses the (negated) parameterized `multiple-to!` constraint with parameter `Hello` and names it `hello-to-one`.
;; **

;; @@
(schema myschema [(enforce hello-to-one (multiple-to!- "Hello"))])
;; @@

;; **
;;; ## Predefined schema constraints
;;; 
;;; While it is possible to define schema constraints using the above constraint forms, _GrapeVine_ comes with a list of _predefined_ constraint types, which make schema definition more convinient.
;;; 
;;; Consider for example a constraint that restricts the allowed node types. Defining such a constraint using the above means is possible but combersome. Database schema definitions often go beyond restrictions but also define efficient search structures like B+ trees for unique keys (as a side-effect of defining constraints). 
;;; 
;;; The following predefined constraint types are available:
;;; 
;;; ### Nodes and Edge Labels
;;; 
;;; We can restrict the allowed node and edge labels with the `__nodelabel__` and `__edgelabel__` forms, respectively. 
;;; 
;;; Syntax: __`(nodelabels conname [l1 ... ln])`__ and __`(nodelabels conname [l1 ... ln])`__, where 
;;; * `conname` is an _optional_ name for the constraint, and
;;; * `l1` .. `ln` are names of (allowed) labels
;;; 
;;; The following example defines a schema that allows only two node labels (`Hello` and `World`) and one edge label (`to`), attaches it to a graph and demonstrates its behaviour.
;; **

;; @@
(schema myschema [(nodelabels [Hello World])
                  (edgelabels	[to])])

(-> (newgrape) myschema hello world view)
;; @@

;; **
;;; Now, the following should not be allowed, as it creates a node of type `Grape`.
;; **

;; @@
(vine _)
;; @@

;; **
;;; ### Roles
;;; 
;;; The roles that nodes can play in relationships (as sources and targets of edges) can be restricted with the __`sourceroles`__ and __`targetroles`__ forms.
;;; 
;;; Syntax: __`(sourceroles conname edgelabel [l1 l2 ... ln])`__ and __`(targetroles conname edgelabel [l1 l2 ... ln])`__, where
;;; * `conname` is an *optional* name for the constraint,
;;; * `edgelabel` is an edge label, and
;;; * `l1` .. `ln` are labels of nodes that are allowed to play the source or target role of that edge, respectively
;;; 
;;; 
;; **

;; @@
(schema myschema [(sourceroles to [Hello])
                  (targetroles to [World])])

;; @@

;; @@
(-> (newgrape) myschema hello world view)
;; @@

;; **
;;; Now, the following should not be allowed, since a `Grape` node cannot play the target role for a `to` edge.
;; **

;; @@
(vine _)
;; @@

;; **
;;; ### Multiplicity
;;; 
;;; We can constrain the cardinality of relationships (edges) to have a unique target ("to one") and or unique source ("from one").
;;; 
;;; Syntax: __`(to-one conname edgelabel)`__ and __`(from-one conname edgelabel)`__, where
;;; * `conname` is an *optional* name for the constraint, and
;;; * `edgelabel` is an edge label
;; **

;; @@
(schema myschema [(to-one to)])
;; @@

;; @@
(-> (newgrape) myschema hello world vine view)
;; @@

;; **
;;; But the following does not work:
;; **

;; @@
(-> (newgrape) myschema hello world world)
;; @@

;; **
;;; ### Attribute uniqueness
;;; 
;;; We can define attributes to be unique for all nodes of a particular label:
;;; 
;;; Syntax: __`(unique conname nodelabel attname)`__, where
;;; * `conname` is an *optional* name for the constraint, 
;;; * `nodelabel` is a node label, and
;;; * `attname` is an attribute name
;; **

;; @@
(rule add-person [s]
      (create (node :Person {:SIN "'&s'"})))
;; @@

;; @@
(schema myschema [(unique Person SIN)])


(-> (newgrape) myschema (add-person 123) (add-person 123) view)
;; @@

;; **
;;; # Transactions 
;;; 
;;; The classical notion of transactions revolves around the ACID (atomic, consistent, isolated, durability) properties. We discuss these properties below.
;;; 
;;; ## Durability
;;; 
;;; Since _GrapeVine_ uses a database, all graph productions are automatically made durable. However, not all graphs may be of interest. Moreover, identifying graphs with automatically generated IDs is inconvenient. 
;;; 
;;; Graphs can therefore be _tagged_ with meaningful names using the `commit` form. In the following example, we tag the resulting graph with the name "_mygraph_"
;;; 
;;; > **Note: `commit` tags an _individual_ graph, that needs to be selected from a _grape_.** (In the example below, we use commit the first (only) graph in the _grape_.)
;;; 
;; **

;; @@
(-> (newgrape) hello world vine)
(commit (first _) "mygraph")
;; @@

;; **
;;; Once tagged, we can simply call the `grape` form to recall the graph by its semantic name, for example:
;; **

;; @@
(-> (grape "mygraph") view)
;; @@

;; **
;;; The names given to graphs using the commit form must be unique. Trying to tag another graph with the same name will result in an error:
;; **

;; @@
(-> (newgrape) hello first (commit "mygraph"))
;; @@

;; **
;;; Committed graphs can be "uncommitted" using the `uncommit` form:
;; **

;; @@
(uncommit "mygraph")
;; @@

;; **
;;; ### Rollbacks
;;; We may want to roll back uninteresting graph derivations, for example to free up resources or to prune the graph derivation history. We can do so by using the `rollback` form. **Warning**: calling rollback will remove _all_ graphs that are not in the trace of any _committed_ (tagged) graph. (See the first part of this tutorial for a definition of the term "trace".)
;;; 
;;; The following example illustrates this behaviour. In the code block below, we create a graph history with four different branches, originating from the same empty start graph. We commit a tag for the end-result of one of these derivations.
;; **

;; @@
(def start (newgrape))

(-> start hello world vine)
(-> start hello world vine)
(-> start hello world vine)
(-> start hello world vine)
(commit (first _) "mygraph")
(-> _ history view)
;; @@

;; **
;;; Calling `rollback` will remove all graphs that "do not lead up to" a committed (tagged) graph in the graph process. (see below)
;; **

;; @@
(rollback)
(-> _ history view)
;; @@

;; **
;;; # Control structures
;;; 
;;; Control structures are the primitives we use to control the application of graph transformations, i.e., to write _graph programs_. 
;;; 
;;; Since _GrapeVine_ is embedded into the Clojure language, all the regular Clojure control structures are available to program with graph transformations. The _GrapeVine_ approach is to leverage these existing control structures as much as possible rather than introducing new "syntactic sugar".
;;; 
;;; We have already seen that we use simple function calls for invoking graph transformations. Calling a rule. We have also seen that the Clojure _threading macro_ (->) is well-suited for _sequential composition_ of graph transformations, tests and queries. Of course, we could always use alternative Clojure notation (as we did before as well), e.g, we could use the classical way of nesting function calls (i.e., (f1 (f2)))
;;; 
;;; ## Sequences
;;; 
;;; Since transformations (and constraints) are functions mapping _grapes_ to _grapes_, we have already seen that sequential composition is as simple as making nested function calls, e.g., `f1(f2(..)))`. Alternatively, (as we have also seen), we can make use of the Clojure _threading macro_ (->) to avoid too many brackets, e.g., `(-> f1 f2 ..)`.
;;; 
;;; ## Alternatives
;;; 
;;; Again, programmers can use the regular Clojure constructs to define alternative behaviour. However, _GrapeVine_ offers an additional macro for convenience. This `<code>&#124;&#124;</code>` macro defines a function from _grapes_ to _grapes_. It returns a grape with the union of all graphs that can be produced by _any_ of the specified operations when performed on graphs in the input grape. This means that applying `(`<code>&#124;&#124;</code>`o1 .. on)` to an input _grape_ will produce an output _grape_ containing all graphs that can be produced by applying any operation in `o1 .. on`.
;;; 
;;; ## Loops
;;; 
;;; In addition to the regular Clojure looping constructs, _GrapeVine_ provides a looping macro `->*`, which works similar to a "repeat until" loop. The general form of this macro is `(->* c o1 .. on)`, where `c` is a constraint and `o1 .. on` are other operations (e.g., transformation or constraints). The looping macro is a recursive function from _grapes_ to _grapes_. It recursively applies the sequence of `o1 .. on` operations until a graph is found that satisfies `c` if the _grape_ is empty. 
;;; 
;;; > _**Note: the `->*` automatically removes graphs that are identical (up to isomorphism) to graphs that have been explored before.** This is helpful to limit the state space during graph exploration._
;;; 
;;; A second looping macro is available to use if the above equivalence check is not desired. It's form is `(->*! c o1 .. on)` and its semantics is the same as above, without the restriction regarding graph similarity.
;;; 
;; **

;; **
;;; ## An example: Ferryman
;;; 
;;; Consider the well-known [Wolf-Goat-Cabbage (well, Wolf-Goat-Grape problem)](https://en.wikipedia.org/wiki/Wolf,_goat_and_cabbage_problem) as an example.
;;; 
;;; ![](https://www.mathworks.com/matlabcentral/mlc-downloads/downloads/submissions/55054/versions/1/screenshot.gif)
;;; 
;;; Below is specification of the problem. Rule `setup-ferryman` creates the starting situation. The ferryman can either take one item to the other side  (rule `ferry_one_over`) or he can cross without cargo (rule `cross_empty`). 
;;; 
;;; Then we have defined two graph constraints. The first one for testing the dangerious situation where a thing can be eaten (if unattended) and the second one to check whether we have solved the ridle (all on the other side).
;; **

;; @@
(list

 (rule setup-ferryman []
       (create
        (node tg:Thing {:kind "'Goat'"})
        (node tc:Thing {:kind "'Grape'"})
        (node tw:Thing {:kind "'Wolf'"})
        (node s1:Side {:name "'This side'"})
        (node s2:Side {:name "'Other side'"})
        (node f:Ferry)
        (edge :is_at tg s1)
        (edge :is_at tc s1)
        (edge :is_at tw s1)
        (edge :is_at f s1)))

 (rule ferry_one_over []
       (read :iso
             (node s1:Side)
             (node s2:Side)
             (node f:Ferry)
             (node t:Thing)
             (edge et:is_at t s1)
             (edge e:is_at f s1))
       (delete e et)
       (create
        (edge :is_at f s2)
        (edge :is_at t s2)))

 (rule cross_empty []
       (read :iso
             (node s1:Side)
             (node s2:Side)
             (node f:Ferry)
             (edge e:is_at f s1))
       (delete e)
       (create
        (edge :is_at f s2)))

 (constraint can-eat! [t1 t2]
             (node t1:Thing {:kind "'&t1'"})
             (node t2:Thing {:kind "'&t2'"})
             (node s:Side)
             (node s2:Side)
             (edge :is_at t1 s)
             (edge :is_at t2 s)
             (node f:Ferry)
             (edge :is_at f s2))

 (constraint all_on_the_other_side! []
             (node tg:Thing {:kind "'Goat'"})
             (node tc:Thing {:kind "'Grape'"})
             (node tw:Thing {:kind "'Wolf'"})
             (node s2:Side {:name "'Other side'"})
             (edge :is_at tg s2)
             (edge :is_at tc s2)
             (edge :is_at tw s2)))

;; @@

;; **
;;; Below is a program that demonstrates the use of the above control structures to solve the ferryman problem:
;;; 
;;; We begin as usual with creating our start graph (grape) (line 1). 
;;; 
;;; Line 2 uses are default **looping** macro. It first takes a constraint to check whether the goal has already reached for _some_ graph in the _grape_. If no graph in the _grape_ satisfies the constraint, the loop continues. (In other words, the looping macro has an "until" semantics.).
;;; 
;;; Line 3 explores all possible ferry move alternatives. 
;;; 
;;; Finally, the (negative) constraint checks in lines 3 and 4 filter out the dangerous situations.
;; **

;; @@
(-> (newgrape) setup-ferryman
    (->* all_on_the_other_side!
         (|| ferry_one_over* cross_empty*)
         (can-eat!- "Wolf" "Goat")
         (can-eat!- "Goat" "Grape")))
;; @@

;; **
;;; We can confirm that a solutions was indeed found.
;; **

;; @@
(view _)
;; @@

;; **
;;; 
;; **

;; **
;;; It should be clear that the above program does a _breadth-first exploration_ of the possible moves for the ferryman until a solution is found. 
;;; 
;;; What if there _is_ no solution? Let's modify the program and only allow the ferryman to cross _with_ cargo (i.e., no empty ferries). A naive breath-first exploration would run forever, since moves with the goat are always safe but will never achieve the desired solution. The result would be an infinite graph process. The program should never terminate (disregarding resource limitations). Let's try this out:
;; **

;; @@
(-> (newgrape) setup-ferryman
    (->* all_on_the_other_side!
         ferry_one_over*
         (can-eat!- "Wolf" "Goat")
         (can-eat!- "Goat" "Grape")))
;; @@

;; **
;;; As we can see, the program does indeed terminate and returns an empty _grape_ (no solution). The reason for this is that the default _GrapeVine_ looping macro compares graphs in the graph history for previously seen graphs and does not further explore graphs that are similar with graphs that have been seen before.
;;; 
;;; > _Note: Feel free to modify the above program to use the alternative looping macro, which does not filter previously explored graphs. However, please be prepared that your program will run forever. There is currently no way to interrupt running programs in GrapePress, so you will have to restart the REPL (using `lein`) and reload the browser. (We hope to find a better solution for this in the future.)_
;;; 
;;; Let's have a look at the graph history for the above program:
;; **

;; **
;;; 
;; **

;; @@
(def start (-> (newgrape) setup-ferryman))
(->* start all_on_the_other_side!
     ferry_one_over*
     (can-eat!- "Wolf" "Goat")
     (can-eat!- "Goat" "Grape"))
(-> start history steps viewsteps)

;; @@

;; **
;;; Indeed, there are three possible "cargo" moves of the ferry from the initial state. Two of them are "unsafe" and one of the (transporting the goat) is safe. From there, the only possible move is to transport the goat again. At that point, the resulting graph is identical to the start (up to isomorphism). 
;;; 
;;; Let's look at the history of the original program again (which _has_ a solution). Below we see that the breadth-first search does not explore graphs that have already been seen on the process (up to isomorphism).
;; **

;; @@
(-> (newgrape) setup-ferryman
    (->* all_on_the_other_side!
         (|| ferry_one_over* cross_empty*)
         (can-eat!- "Wolf" "Goat")
         (can-eat!- "Goat" "Grape")))

(-> _ history view)
;; @@

;; **
;;; Viewing the details on the full history graph creates a big image. Rather, we are just interested in the trace that leads to the solution:
;; **

;; @@
(-> _ traces steps viewsteps)
;; @@

;; **
;;; ### Solving the Ferryman Problem with Schema Constraints
;;; 
;;; Alternatively, we can solve the Ferryman Problem using a schema that forbids dangerous situations. We define avoidance of the two dangerous moves as schema constraints.
;; **

;; @@
(schema safe-moves [(enforce goat-is-safe  (can-eat!- "Wolf" "Goat"))
                    (enforce grape-is-safe (can-eat!- "Goat" "Grape"))])
;; @@

;; **
;;; 
;; **

;; @@
(-> (newgrape)
    safe-moves
    setup-ferryman
    (->* all_on_the_other_side! (|| ferry_one_over* cross_empty*)))
;; @@

;; @@

;; @@
