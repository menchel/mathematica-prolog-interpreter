🧠 Core Concepts
1. Terms
These are the building blocks of Prolog:

Atoms: lowercase identifiers, e.g. apple, 'Hello World'

Numbers: integers and floats, e.g. 42, 3.14

Variables: start with uppercase or _, e.g. X, Result, _Temp

Compound Terms: functor with arguments, e.g. father(john, mary)

Lists: syntactic sugar for terms, e.g. [a, b, c] is .(a, .(b, .(c, [])))

Anonymous Variable: _, matches anything but doesn't bind

🏗 Syntax Elements
Fact: parent(john, mary).

Rule: ancestor(X, Y) :- parent(X, Y).

Query: ?- ancestor(john, Y).

Conjunction: A, B (logical AND)

Disjunction: A ; B (logical OR)

Negation (as failure): \+ Goal

🔄 Control Constructs
Backtracking: Automatic exploration of alternatives

Cut: ! — prevents backtracking past this point

Fail: fail — forces failure

True: true — always succeeds

📚 Built-in Predicates (typically in plain Prolog)
Unification and Comparison
=, \=: unification, negated unification

==, \==: term identity

is: arithmetic evaluation, e.g. X is 2 + 3

Arithmetic comparisons: =:=, =\=, <, >, =<, >=

Arithmetic Functions
+, -, *, /, //, mod, abs, min, max, *
