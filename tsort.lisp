(in-package :tsort)

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
    (cl-utilities:with-collectors (indies depends)
      (labels ((rec (nodes) ; recurse to catch up dotted list error.
                 (unless (! 1 (endp nodes)) ; graph may dotted.
                   (let ((node (car nodes)))
                     (if (independent-node-p node)
                         (indies (car node))
                         (depends node)))
                   (rec (cdr nodes))))
               (independent-node-p (node)
                 (! 0 (endp (! 0 (cdr node))))) ; node may dotted. node may
                                                ; atom.
               )
        (rec graph)))))
