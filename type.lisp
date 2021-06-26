(in-package :tsort)

;;; Actually these are not needed, but for explaining implementation.

(deftype graph () "Graph represented by list." 'nodes)

(deftype node ()
  "Element of GRAPH, represented by list."
  '(cons subject directions))

(deftype nodes () "List which contains only node." 'list)

(deftype subject ()
  "Identifier for NODE. We can say this is node's name, roughly."
  '(not null))

(deftype subjects () "List which contains subject only." 'list)

(deftype direction () 'subjects)

(deftype directions () "List which contains direction only." 'list)