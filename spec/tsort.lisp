(defpackage :tsort.spec
  (:use :cl :jingoh :tsort))
(in-package :tsort.spec)
(setup :tsort)

(requirements-about TSORT :test equal)

;;;; Description:
; Do topological sort.
#?(tsort '((a b c)(b)(c)))
=> (A B C)

#+syntax
(TSORT graph &key (test #'eql) (key #'identity) group) ; => result

;;;; Arguments and Values:

; graph := DAG structured list.
; otherwise error.
#?(tsort '((a b c)(b) . c)) :signals error

; test := function
; The default is `CL:EQL`
#?(tsort (list (list "a" "b" "c")(list (string :b))(list "c")))
:signals uncomplete-graph
#?(tsort'(("a" "b" "c")("b")("c")) :test #'string=)
=> ("a" "b" "c")

; key := function
; The default is `CL:IDENTITY`
#?(tsort (list (list "1" "2" "3")(list (string #\2))(list (string #\3))))
:signals uncomplete-graph
#?(tsort (list (list "1" "2" "3")(list (string #\2))(list (string #\3)))
	 :key #'parse-integer)
=> ("1" "2" "3")

; group := boolean
; Specify engroup same rank.
; The default is NIL.
#?(tsort '((a b c)(b)(c))) => (a b c)
#?(tsort '((a b c)(b)(c))
	 :group t)
=> ((A)(B C))

; result := sorted list.

;;;; Affected By:
; none

;;;; Side-Effects:
; none
#?(let((graph '((a b c)(b)(c))))
    (tsort graph)
    graph)
=> ((A B C)(B)(C))

;;;; Notes:
; nil is valid graph.
#?(tsort nil) => NIL

;;;; Exceptional-Situations:
; each node must proper-list
#?(tsort '((A B C)B (C))) :signals invalid-graph
#?(tsort '((A B . C)(B)(C))) :signals type-error

(requirements-about GRAPH-ERROR)

;;;; Description:
;;;; Class Precedence List: (case in SBCL)
; graph-error simple-error simple-condition error serious-condition condition slot-object t

;;;; Effective Slots:

; FORMAT-CONTROL [Type] T
; [READER] simple-condition-format-control

; FORMAT-ARGUMENTS [Type] T
; [READER] simple-condition-format-arguments

;;;; Notes:

(requirements-about RECURSIVE-GRAPH)

;;;; Description:
;;;; Class Precedence List: (case in SBCL)
; recursive-graph graph-error simple-error simple-condition error serious-condition condition slot-object t

;;;; Effective Slots:

; FORMAT-CONTROL [Type] T
; [READER] simple-condition-format-control

; FORMAT-ARGUMENTS [Type] T
; [READER] simple-condition-format-arguments

;;;; Notes:

(requirements-about TRAMPOLINE-GRAPH)

;;;; Description:
;;;; Class Precedence List: (case in SBCL)
; trampoline-graph graph-error simple-error simple-condition error serious-condition condition slot-object t

;;;; Effective Slots:

; FORMAT-CONTROL [Type] T
; [READER] simple-condition-format-control

; FORMAT-ARGUMENTS [Type] T
; [READER] simple-condition-format-arguments

;;;; Notes:

(requirements-about UNCOMPLETE-GRAPH)

;;;; Description:
;;;; Class Precedence List: (case in SBCL)
; uncomplete-graph graph-error simple-error simple-condition error serious-condition condition slot-object t

;;;; Effective Slots:

; FORMAT-CONTROL [Type] T
; [READER] simple-condition-format-control

; FORMAT-ARGUMENTS [Type] T
; [READER] simple-condition-format-arguments

;;;; Notes:

(requirements-about INVALID-GRAPH)

;;;; Description:
;;;; Class Precedence List: (case in SBCL)
; invalid-graph graph-error simple-error simple-condition error serious-condition condition slot-object t

;;;; Effective Slots:

; FORMAT-CONTROL [Type] T
; [READER] simple-condition-format-control

; FORMAT-ARGUMENTS [Type] T
; [READER] simple-condition-format-arguments

;;;; Notes:
