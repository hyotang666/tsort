(in-package :cl-user)

(defpackage :tsort
  (:use :cl)
  (:export ;;;; main api
           #:tsort
           ;;;; conditions
           ;; super condition
           #:graph-error
           ;; sub conditions
           #:recursive-graph
           #:trampoline-graph
           #:uncomplete-graph
           #:invalid-graph
           ;; Helper
           #:make-dag))

(in-package :tsort)

(declaim (optimize speed))

;;;; TYPES
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

;;;; CONDITIONS

(define-condition graph-error (simple-error) ())

(define-condition recursive-graph (graph-error)
  ()
  (:report
   (lambda (c *standard-output*)
     (format t "~S: Recursive graph: ~S" 'tsort
             (car (simple-condition-format-arguments c))))))

(define-condition trampoline-graph (graph-error)
  ()
  (:report
   (lambda (c *standard-output*)
     (apply #'format t "~S: Trampoline graph.~%Especially ~S and ~S." 'tsort
            (simple-condition-format-arguments c)))))

(define-condition uncomplete-graph (graph-error)
  ()
  (:report
   (lambda (c *standard-output*)
     (format t "~S: Uncomplete graph.~%Especially lack of ~S." 'tsort
             (simple-condition-format-arguments c)))))

(define-condition invalid-graph (graph-error)
  ()
  (:report
   (lambda (c *standard-output*)
     (format t "~S: Each node must proper-list, but ~S." 'tsort
             (simple-condition-format-arguments c)))))

(defun report-error (rest &key (test #'eql) (key #'identity))
  (let ((arg (cyclic-graph-p rest :test test :key key)))
    (if arg
        (if (eq (first arg) (second arg)) ; it is self recursive.
            (error 'recursive-graph :format-arguments arg)
            (error 'trampoline-graph :format-arguments arg))
        (error 'uncomplete-graph :format-arguments (lack rest)))))

(declaim
 (ftype (function
         (list &key (:test (or symbol function)) (:key (or symbol function)))
         (values list &optional))
        cyclic-graph-p))

(defun cyclic-graph-p
       (rest
        &key (test #'eql) (key #'identity)
        &aux (test (coerce test 'function)) (key (coerce key 'function)))
  (loop :for node1 :in rest
        :for (subject1 . direction1) = node1
        :if (find subject1 rest
                  :test (lambda (subject1 node)
                          (destructuring-bind
                              (subject2 . direction2)
                              node
                            (and (find subject1 (the list direction2)
                                       :test test
                                       :key key)
                                 (find subject2 (the list direction1)
                                       :test test
                                       :key key)))))
          :collect it :into node2
          :and :do (return (cons node1 node2))))

(defun lack (nodes)
  (loop :for (subject . direction) :in nodes
        :collect subject :into subjects
        :append direction :into directions
        :finally (return (nset-difference directions subjects))))

;;;; TSORT

(defun tsort (graph &key (test #'eql) (key #'identity) group)
  (let ((engrouper
         (if group
             #'cons
             #'append))
        (test (coerce test 'function))
        (key (coerce key 'function)))
    (labels ((target (list)
               (lambda (x) (find (funcall key x) list :test test :key key)))
             (rec (g &optional acc)
               (multiple-value-bind (indies rest)
                   (split g)
                 (if (endp rest)
                     (funcall engrouper indies acc)
                     (if indies
                         (rec
                           (trestrul:remove-leaf-if (target indies) rest
                                                    :keep nil)
                           (funcall engrouper indies acc))
                         (report-error rest :test test))))))
      (declare (ftype (function (list) (values function &optional)) target))
      (rec graph))))

(defun split (graph)
  (labels ((independent-node-p (node)
             (typecase node
               ((or (and atom (not null)) ; Non NIL ATOM.
                    (cons t (and atom (not null)))) ; Dotted.
                (error 'invalid-graph :format-arguments 'node))
               ((cons t null) t)
               (otherwise nil))))
    (handler-bind (((and error (not invalid-graph))
                    (lambda (condition)
                      (declare (ignore condition))
                      (error 'invalid-graph :format-arguments 'graph))))
      (loop :for node :in graph
            :if (independent-node-p node)
              :collect (car node) :into indies
            :else
              :collect node :into depends
            :finally (return (values indies depends))))))

;;;; MAKE-DAG

(defun make-dag (node edge-generator)
  "Make list of lists as Directed Acyclic Graph from NODE.
  EDGE-GENERATOR should have signature (FUNCTION (NODE) LIST).
  All elements of returned LIST must be NODE."
  (setf edge-generator (coerce edge-generator 'function))
  (let ((table (make-hash-table :test #'eq)))
    (labels ((rec (nodes)
               (if (endp nodes)
                   (loop :for node :being :each :hash-key :of table :using
                              (:hash-value edges)
                         :collect (cons node edges))
                   (body (car nodes) (cdr nodes))))
             (body (subject rest)
               (let ((edges (funcall edge-generator subject)))
                 (setf (gethash subject table) edges)
                 (rec (union rest edges :test #'eq)))))
      (rec (list node)))))
