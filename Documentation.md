# Grapheme Documentation

## Basic Syntax

### Scheme Direvations

Basic syntactic keywords:

* `quote`
* `if`
* `lambda`
* `set!`
* `define`

The same definition as [formal Scheme syntax](http://people.csail.mit.edu/jaffer/r5rs_9.html#SEC72), except that the `if` in grapheme can only take three arguments, i.e. you have to define else-expressions.

The `define` keyword can either be used to define variable, functions (just like Scheme), or used to define graphs. Graph definition syntax:

```
<graph definition> --> (define <variable> <graph descriptor>)
<graph descriptor> --> '((<vertex>* ) ((<vertex> <vertex> <letters|digits>)* ))
<vertex> --> <variable>| #<variable>

```

The first list of graph descriptor represents the vertex list, where `#` preceded vertexes denode the accept states in a automaton. The second list represents the arcs: *(from, to, trans)* for automatons, or *(from, to, weight)* for normal graphs.

### Other primitives

* **numeric operations:** `+` `-` `*` `/` `mod` `quotient` `remainder`.
  
	Where `/` and `mod` have the same behavior as C/C++'s `/` and `%`; while `quotient` and `remainder` have the same behavior as Python's `/` and `%`.
	
* **numeric bool operations:** `=` `<` `>` `/=` `>=` `<=`
* **bool operations:** `&&` `||`
* **string bool operations:** `string=?` `string>?` `string<?` `string<=?` `string>=?`
* **atom bool operations:** `atom>?` `atom<?`
* **list operations:** `car` `cdr` `cons` `list?`
* **atom operations:** Originally in Scheme, we cannot modify an atom. But we may need to modify atoms in Grapheme. So we defined the following primitive operations for atoms.

	```
	(sflip <atom>)
	(eqs? <atom> <atom>)
	(acc? <atom>)
	(atomtolist <atom>)
	(listtoatom <list>)
	```
	
	where:
	* `sflip`: This operation only make sense when the atom is a vertex in an automaton, which flips the acceptablily of the node.
	* `eqs?`: This operation returns true if the two atoms are the same with no regard of the preceding "#". This is helpful when user gives an accept vertex with "#" in vertex list and without "#" in arc list.
	* `acc?`: This operation tells if the atom is an accept state, i.e., preceded by "#".
	* `atomtolist`: Convert an atom to a list, which is a helpful operation in graph programming when we want to deal with compound vertexes/edges.
	* `listtoatom`: Convert a list to an atom.
* **other operations:**
	* `eqv?`: A universal equal operation.
	* `apply`: In general, `(apply op args) = (op args)`, which is useful in graph programming.
	* `load`: Using `(load "file name")` to load a Grapheme file.
	* `paint`: Using `(paint G)` to paint a graph G into the GUI.
	


## Standard Library

The following functions are defined in our standard library. You can use `(load "grapheme.scm")` to load the library can use the functions.

### Extended Contionals

As introduced in last section, our `if` form is: `(if <test-expr> <then-expr> <else-expr>)`. If `<test-expr>` produces any value other than `#f`, then `<then-expr>` is evaluated, and its results are the result for the if form. Otherwise, `<else-expr>` is evaluated, and its results are the result for the if form.

For more intuitive programming needs, we also defined `not`, `and` and `or`.

```
(not <expr>)
(and <expr>*)
(or <expr>*)
```
Where:

* `not`: Takes an expression, return the opposite value.
* `and`: If no `<expr>`s are provided, then result is `#t`. If a single `<expr>` is provided, then it is in tail position, so the results of the and expression are the results of the `<expr>`. Otherwise, the first `<expr>` is evaluated. If it produces `#f`, the result of the and expression is `#f`. Otherwise, the result is the same as an `and` expression with the remaining `<expr>`s in tail position with respect to the original `and` form.
* `or`: If no `<expr>`s are provided, then result is `#f`. If a single `<expr>` is provided, then it is in tail position, so the results of the `or` expression are the results of the `<expr>`. Otherwise, the first `<expr>` is evaluated. If it produces a value other than `#f`, that result is the result of the `or` expression. Otherwise, the result is the same as an `or` expression with the remaining `<expr>`s in tail position with respect to the original `or` form.

### Functional Attributes


```
(flip <proc>)
(curry <proc> <arg>)
(compose <proc> <proc>)
```
Where:

* `flip`: Flip takes `<proc>`'s (first) two arguments in the reverse order of proc.
* `curry`: Curry converts an uncurried function to a curried function.
* `compose`: Function composition.

### Pair Constructors and Selectors

```
(null? <variable>)
(list <variable>*)
```
Where:

* `null?`: Returns `#t` if `<variable>` is the empty list, `#f` otherwise.
* `list`: Returns a newly allocated list containing the `<variable>`s as its elements.

### List Operations


* `length`: 

	```
	(length lst) → exact-nonnegative-integer?
 		lst : list? 
	```
	Returns the number of elements in `lst`.
* `list-ref`:

	```
	(list-ref lst pos) → any/c
 		lst : pair? 
  		pos : exact-nonnegative-integer?
	```
	Returns the element of `lst` at position `pos`, where the list’s first element is position 0. If the list has `pos` or fewer elements, then the `exn:fail:contract exception` is raised.

	The `lst` argument need not actually be a list; `lst` must merely start with a chain of at least `(add1 pos)` pairs.

* `append`:

	```
	(append lst ...) → list?
 		lst : list? 
	```
	When given all list arguments, the result is a list that contains all of the elements of the given lists in order. The last argument is used directly in the tail of the result.
* `reverse`:

	```
	(reverse lst) → list?
  		lst : list? 
	```
	Returns a list that has the same elements as `lst`, but in reverse order.

### List Iterations

* `map`:

	```
	(map proc lst) → list?
		proc : procedure?
		lst : list? 
	```
	Applies proc to the elements of the lst from the first element to the last. 
* `for-each`:

	```
	(for-each proc lst) → void?
		proc : procedure? 
		lst : list? 
	```
	Similar to `map`, but `proc` is called only for its effect, and its result (which can be any number of values) is ignored.
* `foldl`:
	
	```
	(foldl proc init lst) → any/c
		proc : procedure? 
		init : any/c 
  		lst : list?
	```
	Like `map`, `foldl` applies a procedure to the elements of a list. Whereas `map` combines the return values into a list, `foldl` combines the return values in an arbitrary way that is determined by `proc`.
	
	The `proc` is initially invoked with the first item the list, and the final argument is `init`. In subsequent invocations of `proc`, the last argument is the return value from the previous invocation of `proc`. The input `lst`s are traversed from left to right, and the result of the whole foldl application is the result of the last application of proc. If the `lst`s are empty, the result is `init`.
	
	Specially, the `proc` should take `init` (which could be seen as an accumulator) as its first argument and the element processed now from the list as the second one.
* `foldr`:
	
	```
	(foldr proc init lst) → any/c
		proc : procedure? 
  		init : any/c 
  		lst : list? 
	```
	Like `foldl`, but the lists are traversed from right to left.
	
	The order of arguments `proc` receives here is the inverse of that in `foldl`.
* `filter`:

	```
	(filter pred lst) → list?
 		pred : procedure? 
  		lst : list? 
	```
	Returns a list with the elements of lst for which `pred` produces a true value. The `pred` procedure is applied to each element from first to last.
* `filter-not`:

	```
	(filter-not pred lst) → list?
		pred : (any/c . -> . any/c) 
  		lst : list? 
	```
	Like `filter`, but the meaning of the `pred` predicate is reversed: the result is a list of all items for which pred returns `#f`.
* `sort`:
	
	```
	(sort lst proc) → list?
		lst : list?
		proc : procedure?
	```
	Returns the a list where elements in `lst` are sorted by the procedure `proc`.
* `remove`:

	```
	(remove v lst [proc]) → list?
 		v : any/c 
  		lst : list? 
  		proc : procedure? = equal? 
	```
	Returns a list that is like `lst`, omitting the first element of lst that is equal to `v` using the comparison procedure `proc` (which must accept two arguments).
* `remq`:

	```
	(remq v lst) → list?
		v : any/c 
  		lst : list? 
	```
	Returns `(remove v lst eq?)`.
* `remove*`:

	```
	(remove* v-lst lst [proc]) → list? 
 		v-lst : list? 
  		lst : list? 
  		proc : procedure? = equal? 
	```
	Like `remove`, but removes from `lst` every instance of every element of `v-lst`.
* `remq*`:

	```
	(remq* v-lst lst) → list?
  		v-lst : list? 
  		lst : list? 
	```
	Returns `(remove* v-lst lst eq?)`.
* `member`:

	```
	(member v lst [is-equal?]) → (or/c list? '())
  		v : any/c 
  		lst : list? 
  		is-equal? : (any/c any/c -> any/c) = equal?
	```
	Locates the first element of `lst` that is `equal?` to `v`. If such an element exists, the tail of `lst` starting with that element is returned. Otherwise, the result is `'()`.
* `memv`:

	```
	(memv v lst) → (or/c list? '())
  		v : any/c 
  		lst : list? 
	```
	Like `member`, but finds an element using `eqv?`.
* `memf`:

	```
	(memf proc lst) → (or/c list? '())
  		proc : procedure? 
  		lst : list? 
	```
	Like `member`, but finds an element using the predicate `proc`; an element is found when `proc` applied to the element returns a true value.
* `findf`:

	```
	(findf proc lst) → any/c
  		proc : procedure? 
  		lst : list?
	```
	Like `memf`, but returns the element or `#f` instead of a tail of lst or `#f`.
* `flatten`:

	```
	(flatten v) → list?
  		v : any/c
	```
	Flattens an arbitrary S-expression structure of pairs into a list. More precisely, `v` is treated as a binary tree where pairs are interior nodes, and the resulting list contains all of the non-null leaves of the tree in the same order as an inorder traversal.
* `remove-duplicates`:

	```
	(remove-duplicates   lst [same?])   →   list? 
  		lst : list? 
  		same? : (any/c any/c . -> . any/c) = equal? 
	```
	Returns a list that has all items in `lst`, but without duplicate items, where `same?` determines whether two elements of the list are equivalent. The resulting list is in the same order as `lst`, and for any item that occurs multiple times, the first one is kept.
* `count`:

	```
	(count arg lst [is-equal?]) → exact-nonnegative-integer?
		arg : any/c
  		lst : list? 
  		is-equal? : (any/c any/c -> any/c) = equal?
	```
	Returns `(length (filter (curry is-equal? arg) lst ...))`, but without building the intermediate list.
* `range`:

	```
	(range start end [step]) → list? 
  		start : exact-integer? 
  		end : exact-integer? 
  		step : exact-integer? = 1 
	```
	The resulting list holds numbers starting at `start` and whose successive elements are computed by adding `step` to their predecessor until `end` (excluded) is reached. If no starting point is provided, 0 is used. If no step argument is provided, 1 is used.
* `make-list`:

	```
	(make-list k v) → list?
  		k : exact-nonnegative-integer? 
  		v : any/c 
	```
	Returns a newly constructed list of length `k`, holding `v` in all positions.
* `take`:

	```
	(take lst pos) → list?
  		lst : any/c 
  		pos : exact-nonnegative-integer?
	```
	Returns a fresh list whose elements are the first `pos` elements of `lst`. 

	The `lst` argument need not actually be a list; `lst` must merely start with a chain of at least `pos` pairs.
* `drop`:

	```
	(drop lst pos) → any/c
  		lst : any/c 
  		pos : exact-nonnegative-integer? 
	```
	Returns the list after the first `pos` elements of `lst`.

	The `lst` argument need not actually be a list; `lst` must merely start with a chain of at least `pos` pairs.
* `zip`:

	```
	(zip lst ...+) → list?
  		lst : list?
	```
	All `lst`s must have the same number of elements. The `n`th element of result is a list containing `n`th elements from each `lst`s in order.
	
### Graph Operations

* `vertexes`:

	```
	(vertexes G) → list
		G : pair?
	```
	Returns the vertex list of the graph `G`.
* `arcs`:

	```
	(arcs G) → list
		G : pair?
	```
	Returns the arc list of the graph `G`.
* `vertex`:

	```
	(vertex G pos) → any/c
		G : pair?
		pos : exact-nonnegative-integer?
	```
	Returns the vertex at `pos` of `G`'s vertex list.
* `arc`:

	```
	(arc G pos) → any/c
		G : pair?
		pos : exact-nonnegative-integer?
	``` 
	Returns the arc at `pos` of `G`'s arc list.
* `relabel`:

	```
	(relabel G) → pair?
		G : pair?
	```
	Returns the minimum labeled graph that is equivalent to graph `G`. 
* `lex`:

	```
	(lex G) → list?
		G : pair?
	```
	Returns the list of the lexicon used in the Automaton represented by `G`.

## Sample Programs

To give an example of programming a graph, we provide several sample programs. You can observe the usage of the functions and operations in these programs.

* **NFAtoDFA**: The function takes an NFA, and returns the equivalent DFA.
* **DFA-minimize**: The function takes a DFA, and returns the minimized equivalent DFA.
* **NFAtoREG**: The function takes an NFA, and returns the regular expression decoded for the NFA.
 	
