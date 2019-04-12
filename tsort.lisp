(in-package :tsort)
(named-readtables:in-readtable with-package:syntax)

(defun tsort(graph &key (test #'eql)(key #'identity)group)
  (let((engrouper(if group
		   #'cons
		   #'append)))
    (labels((TARGET(list)
	      (lambda(x)
		(find (funcall key x)
		      list :test test :key key)))
	    (REC(g &optional acc)
	      (multiple-value-bind(indies rest)(split g)
		(if(endp rest)
		  (funcall engrouper indies acc)
		  (if indies
		    (REC (trestrul:remove-leaf-if (TARGET indies)
						  rest :keep nil)
			 (funcall engrouper indies acc))
		    (report-error rest :test test))))))
      (REC graph))))

#@(:cl-utilities #:With-collectors)

(macrolet((!(n form)
	    `(HANDLER-CASE,form
	       (ERROR()(ERROR 'INVALID-GRAPH
			      :FORMAT-ARGUMENTS,(aref #(node graph tree)n))))))


  (defun split(graph)
    (With-collectors(INDIES DEPENDS)
      (labels((REC(nodes) ; recurse to catch up dotted list error.
		(unless(! 1(endp nodes)) ; graph may dotted.
		  (let((node(car nodes)))
		    (if(INDEPENDENT-NODE-P node)
		      (INDIES(car node))
		      (DEPENDS node)))
		  (REC(cdr nodes))))
	      (INDEPENDENT-NODE-P(node)
		(! 0(endp(! 0(cdr node))))) ; node may dotted. node may atom.
	      )
	(REC graph)))))
