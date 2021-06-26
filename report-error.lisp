(in-package :tsort)

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