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
           #:invalid-graph))

(in-package :tsort)

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
  (let ((arg (circler-graph-p rest :test test :key key)))
    (if arg
        (if (eq (first arg) (second arg)) ; it is self recursive.
            (error 'recursive-graph :format-arguments arg)
            (error 'trampoline-graph :format-arguments arg))
        (error 'uncomplete-graph :format-arguments (lack rest)))))

(defun circler-graph-p (rest &key (test #'eql) (key #'identity))
  (loop :for node1 :in rest
        :for (subject1 . direction1) = node1
        :if (find subject1 rest
                  :test (lambda (subject1 node)
                          (destructuring-bind
                              (subject2 . direction2)
                              node
                            (and (find subject1 direction2 :test test :key key)
                                 (find subject2 direction1
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
             #'append)))
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
      (rec graph))))

(macrolet ((! (n form)
             `(handler-case ,form
                (error ()
                  (error 'invalid-graph
                         :format-arguments ,(aref #(node graph tree) n))))))
  (defun split (graph)
    (labels ((independent-node-p (node)
               (! 0 (endp (! 0 (cdr node)))))) ; node may dotted. node may atom.
      (loop :for node :in graph
                 ;; to catch up dotted list error.
                 :by #'(lambda (l) (! 1 (cdr l)))
            :if (independent-node-p node)
              :collect (car node) :into indies
            :else
              :collect node :into depends
            :finally (return (values indies depends))))))