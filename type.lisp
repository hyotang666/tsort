(in-package :tsort)

;;; Actually these are not needed, but for explaining implementation.

(deftype graph()
  "Graph represented by list."
  'NODES)

(deftype node()
  "Element of GRAPH, represented by list."
  '(CONS SUBJECT DIRECTIONS))

(deftype nodes()
  "List which contains only node."
  'list)

(deftype subject()
  "Identifier for NODE. We can say this is node's name, roughly."
  '(NOT NULL))

(deftype subjects()
  "List which contains subject only."
  'list)

(deftype direction()
  'SUBJECTS)

(deftype directions()
  "List which contains direction only."
  'list)

