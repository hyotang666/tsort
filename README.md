# TSORT 0.0.0

## What is this?
[Topological sorting].

[Topological sorting] is familiar for common-lisp programmers because it is used [inside of CLOS].

## Alternatives and differences.
Please tell me if such libraries exist.

### Related works.
If you want other types of sortings, [zsort] may help you.

## Usage

```lisp
(tsort:tsort '((a b c) (b) (c d) (d)))
=> (A C B D)
```

Tsort provides the helper function `MAKE-DAG` to generate a directed acyclic graph.

A first argument is an object as the root node.

The second argument is a function-designator that accepts a node object then returns a list of node objects as edges from the node.

```lisp
;; Case in SBCL.
* (tsort:make-dag (find-class 'string) #'c2mop:class-direct-superclasses)

((#<BUILT-IN-CLASS COMMON-LISP:STRING> #<BUILT-IN-CLASS COMMON-LISP:VECTOR>) ;; STRING inherits VECTOR.
 (#<BUILT-IN-CLASS COMMON-LISP:VECTOR> #<BUILT-IN-CLASS COMMON-LISP:ARRAY>   ;; VECTOR inherits ARRAY and SEQUENCE.
  #<SB-PCL:SYSTEM-CLASS COMMON-LISP:SEQUENCE>)
 (#<SB-PCL:SYSTEM-CLASS COMMON-LISP:SEQUENCE>                                ;; SEQUENCE inherits T.
  #<SB-PCL:SYSTEM-CLASS COMMON-LISP:T>)
 (#<BUILT-IN-CLASS COMMON-LISP:ARRAY> #<SB-PCL:SYSTEM-CLASS COMMON-LISP:T>)  ;; ARRAY inherits T.
 (#<SB-PCL:SYSTEM-CLASS COMMON-LISP:T>))                                     ;; T inherits nothing.

 * (tsort:tsort *)

;; Sorted by from most concrete class to the most abstract class order.
(#<BUILT-IN-CLASS COMMON-LISP:STRING> #<BUILT-IN-CLASS COMMON-LISP:VECTOR>
 #<SB-PCL:SYSTEM-CLASS COMMON-LISP:SEQUENCE>
 #<BUILT-IN-CLASS COMMON-LISP:ARRAY> #<SB-PCL:SYSTEM-CLASS COMMON-LISP:T>)
```

You can specify to list up same level nodes with keyword argument `:GROUP`.

```lisp
* (tsort:tsort ** :group t)

((#<BUILT-IN-CLASS COMMON-LISP:STRING>) (#<BUILT-IN-CLASS COMMON-LISP:VECTOR>)
 (#<SB-PCL:SYSTEM-CLASS COMMON-LISP:SEQUENCE>
  #<BUILT-IN-CLASS COMMON-LISP:ARRAY>)
 (#<SB-PCL:SYSTEM-CLASS COMMON-LISP:T>))
 ```

Tsort is used for general purposes where DAG is used.
So you can say ***"Where DAG is used?"*** instead of "What is the tsort for?".

The most familiar case is CLOS as you saw above.
One more familiar common lisp programmer is [asdf] system dependencies.

```lisp
* (tsort:make-dag (asdf:find-system :hunchentoot)
                  (lambda (s) (mapcar #'asdf:find-system (asdf:system-depends-on s))))

((#<ASDF/SYSTEM:SYSTEM "hunchentoot"> #<ASDF/SYSTEM:SYSTEM "chunga">
  #<ASDF/SYSTEM:SYSTEM "cl-base64"> #<ASDF/SYSTEM:SYSTEM "cl-fad">
  #<ASDF/SYSTEM:SYSTEM "cl-ppcre"> #<ASDF/SYSTEM:SYSTEM "flexi-streams">
  #<ASDF/SYSTEM:SYSTEM "cl+ssl"> #<ASDF/SYSTEM:SYSTEM "md5">
  #<ASDF/SYSTEM:SYSTEM "alexandria"> #<ASDF/SYSTEM:SYSTEM "rfc2388">
  #<ASDF/SYSTEM:SYSTEM "trivial-backtrace"> #<ASDF/SYSTEM:SYSTEM "usocket">
  #<ASDF/SYSTEM:SYSTEM "bordeaux-threads">)
 (#<ASDF/SYSTEM:SYSTEM "bordeaux-threads"> #<ASDF/SYSTEM:SYSTEM "alexandria">)
 (#<ASDF/SYSTEM:SYSTEM "chunga"> #<ASDF/SYSTEM:SYSTEM "trivial-gray-streams">)
 (#<ASDF/SYSTEM:SYSTEM "alexandria">)
 (#<ASDF/SYSTEM:SYSTEM "trivial-gray-streams">)
 (#<ASDF/SYSTEM:SYSTEM "usocket"> #<ASDF/SYSTEM:SYSTEM "split-sequence">
  #<ASDF/OPERATE:REQUIRE-SYSTEM "sb-bsd-sockets">)
 (#<ASDF/SYSTEM:SYSTEM "cl-base64">)
 (#<ASDF/OPERATE:REQUIRE-SYSTEM "sb-bsd-sockets">)
 (#<ASDF/SYSTEM:SYSTEM "cl-fad"> #<ASDF/OPERATE:REQUIRE-SYSTEM "sb-posix">
  #<ASDF/SYSTEM:SYSTEM "bordeaux-threads"> #<ASDF/SYSTEM:SYSTEM "alexandria">)
 (#<ASDF/SYSTEM:SYSTEM "split-sequence">)
 (#<ASDF/SYSTEM:SYSTEM "trivial-backtrace">) (#<ASDF/SYSTEM:SYSTEM "rfc2388">)
 (#<ASDF/SYSTEM:SYSTEM "md5"> #<ASDF/OPERATE:REQUIRE-SYSTEM "sb-rotate-byte">)
 (#<ASDF/OPERATE:REQUIRE-SYSTEM "sb-posix">)
 (#<ASDF/OPERATE:REQUIRE-SYSTEM "sb-rotate-byte">)
 (#<ASDF/SYSTEM:SYSTEM "cl-ppcre">)
 (#<ASDF/SYSTEM:SYSTEM "cl+ssl"> #<ASDF/SYSTEM:SYSTEM "cl+ssl/config">
  #<ASDF/SYSTEM:SYSTEM "cffi"> #<ASDF/SYSTEM:SYSTEM "trivial-gray-streams">
  #<ASDF/SYSTEM:SYSTEM "flexi-streams">
  #<ASDF/OPERATE:REQUIRE-SYSTEM "sb-posix">
  #<ASDF/SYSTEM:SYSTEM "bordeaux-threads">
  #<ASDF/SYSTEM:SYSTEM "trivial-garbage"> #<ASDF/SYSTEM:SYSTEM "uiop">
  #<ASDF/SYSTEM:SYSTEM "usocket"> #<ASDF/SYSTEM:SYSTEM "alexandria">
  #<ASDF/SYSTEM:SYSTEM "trivial-features">)
 (#<ASDF/SYSTEM:SYSTEM "trivial-features">)
 (#<ASDF/SYSTEM:SYSTEM "flexi-streams">
  #<ASDF/SYSTEM:SYSTEM "trivial-gray-streams">)
 (#<ASDF/SYSTEM:SYSTEM "cl+ssl/config"> #<ASDF/SYSTEM:SYSTEM "cffi">)
 (#<ASDF/SYSTEM:SYSTEM "cffi"> #<ASDF/SYSTEM:SYSTEM "uiop">
  #<ASDF/SYSTEM:SYSTEM "alexandria"> #<ASDF/SYSTEM:SYSTEM "trivial-features">
  #<ASDF/SYSTEM:SYSTEM "babel">)
 (#<ASDF/SYSTEM:SYSTEM "babel"> #<ASDF/SYSTEM:SYSTEM "trivial-features">
  #<ASDF/SYSTEM:SYSTEM "alexandria">)
 (#<ASDF/SYSTEM:SYSTEM "trivial-garbage">) (#<ASDF/SYSTEM:SYSTEM "uiop">))

 * (nreverse (tsort:tsort * :group t))

((#<ASDF/SYSTEM:SYSTEM "alexandria">
  #<ASDF/SYSTEM:SYSTEM "trivial-gray-streams">
  #<ASDF/SYSTEM:SYSTEM "cl-base64">
  #<ASDF/OPERATE:REQUIRE-SYSTEM "sb-bsd-sockets">
  #<ASDF/SYSTEM:SYSTEM "split-sequence">
  #<ASDF/SYSTEM:SYSTEM "trivial-backtrace"> #<ASDF/SYSTEM:SYSTEM "rfc2388">
  #<ASDF/OPERATE:REQUIRE-SYSTEM "sb-posix">
  #<ASDF/OPERATE:REQUIRE-SYSTEM "sb-rotate-byte">
  #<ASDF/SYSTEM:SYSTEM "cl-ppcre"> #<ASDF/SYSTEM:SYSTEM "trivial-features">
  #<ASDF/SYSTEM:SYSTEM "trivial-garbage"> #<ASDF/SYSTEM:SYSTEM "uiop">)
 (#<ASDF/SYSTEM:SYSTEM "bordeaux-threads"> #<ASDF/SYSTEM:SYSTEM "chunga">
  #<ASDF/SYSTEM:SYSTEM "usocket"> #<ASDF/SYSTEM:SYSTEM "md5">
  #<ASDF/SYSTEM:SYSTEM "flexi-streams"> #<ASDF/SYSTEM:SYSTEM "babel">)
 (#<ASDF/SYSTEM:SYSTEM "cl-fad"> #<ASDF/SYSTEM:SYSTEM "cffi">)
 (#<ASDF/SYSTEM:SYSTEM "cl+ssl/config">) (#<ASDF/SYSTEM:SYSTEM "cl+ssl">)
 (#<ASDF/SYSTEM:SYSTEM "hunchentoot">))
 ```

This means the systems in the first list depend on nothing, so these systems can be compiled in parallel ideally.
The systems in the second list depend on the systems in the first list.
So it can be compiled in parallel after the first systems are compiled, and so on.

## From developer

### Product's goal

### License
MIT

### Developped
SBCL
### Tested
SBCL/2.2.0
CCL/1.12.1
CLISP/2.49
ECL/21.2.1
ABCL/1.8.0
allegro/10.1
CMUCL/21D

<!-- Links -->

[Topological sorting]:https://en.wikipedia.org/wiki/Topological_sorting
[zsort]:https://github.com/jorgetavares/zsort
[inside of CLOS]:https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node275.html#SECTION003215100000000000000
[asdf]:https://gitlab.common-lisp.net/asdf/asdf/
