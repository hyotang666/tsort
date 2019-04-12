(defpackage :tsort.spec(:use :cl :tsort :jingoh))
(in-package :tsort.spec)
(setup :tsort)

;;;;
(requirements-about tsort)

#|
TSORT sorts DAG (i.e. Directed Asyclic Graph).
In this library, DAG is represented by list of list.
Let's say inner list is node.
Node's CAR is subject, Node's CDR is directions.
Node (A B C) represents A has direction to B and to C.
|#
#?(tsort '((a b c)(b)(c))) => (A B C)
, :test equal

#|
When keyword parameter :GROUP is specified T,
same rank subjects are listed.
|#
#?(tsort '((a b c)(b)(c))
	 :group T)
=> ((A) (B C))
, :test equal

#|
TSORT support :TEST keyword parameter.
The default is #'CL:EQL.
|#
#?(tsort '(("a" "b" "c")("b")("c")))
:signals UNCOMPLETE-GRAPH
#?(tsort '(("a" "b" "c")("b")("c")):test #'string=)
=> ("a" "b" "c")
, :test equal

#|
TSORT support :KEY keyword parameter.
The default is #'CL:IDENTITY.
|#
#?(tsort '(("1" "2" "3")("2")("3")) :key #'parse-integer)
=> ("1" "2" "3")
, :test equal

#|
Result order is node's left to right order.
|#
#?(tsort '((1 2 3)(3)(2)))
=> (1 3 2)
,:test equal
#?(tsort '((3)(2)(1 2 3)))
=> (1 3 2)
,:test equal

#|
Unlike CL:SORT, TSORT is not destructive.
|#
#?(defvar *graph* '((a b c)(b)(c)))
=> *GRAPH*
,:lazy NIL
#?(tsort *graph*) => (A B C)
,:test equal
#?*graph* => ((A B C)(B)(C))
,:test equal

#|
When invalid graph comes signals invalid-graph.
|#
#?(tsort :not-dag) :signals INVALID-GRAPH
#?(tsort '(:not-node)) :signals INVALID-GRAPH
#?(tsort '((dotted . node))) :signals INVALID-GRAPH
#?(tsort '((dotted) . graph)) :signals INVALID-GRAPH

#|
When uncomplete-graph comes, signals an error of type UNCOMPLETE-GRAPH.
|#
#?(tsort '((no enough directions))) :signals UNCOMPLETE-GRAPH

#|
When cyclic graph comes, signals an error of type RECURSIVE-GRAPH.
|#
#?(tsort '((this this))) :signals RECURSIVE-GRAPH

#|
When each reference graph comes, signals an error of type trampoline-graph
|#
#?(tsort '((from to)(to from))) :signals TRAMPOLINE-GRAPH

#|
All conditions are subtype of graph-error.
|#
#?(every #`(subtypep $c 'graph-error) '(INVALID-GRAPH UNCOMPLETE-GRAPH RECURSIVE-GRAPH TRAMPOLINE-GRAPH))
=> T
