; vim: ft=lisp et
(in-package :asdf)
(defsystem :tsort
  :version "0.0.4"
  :depends-on
  (
   "cl-utilities" ; Utility especially for collectors.
   "trestrul" ; Utilities for TREe STRUctured List.
   )
  :components((:file "tsort")))
;; Perform method below is added by JINGOH.GENERATOR.
(defmethod component-depends-on((o test-op) (c (eql (find-system "tsort"))))
  (append (call-next-method)'((test-op "tsort.test"))))
(defmethod operate :around ((o test-op)(c (eql (find-system "tsort")))
                            &key ((:compile-print *compile-print*))
                            ((:compile-verbose *compile-verbose*))
                            &allow-other-keys)
  (call-next-method))
