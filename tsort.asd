; vim: ft=lisp et
(in-package :asdf)
(defsystem :tsort
  :version "0.0.1"
  :depends-on
  (
   "cl-utilities" ; Utility especially for collectors.
   "with-package" ; Temporally using package.
   "named-readtables" ; Manage readtables.
   "trestrul" ; Utilities for TREe STRUctured List.
   )
  :components((:file "package")
              (:file "type" :depends-on ("package"))
              (:file "report-error" :depends-on("package"))
              (:file "tsort" :depends-on("report-error"))
              ))
;; Perform method below is added by JINGOH.GENERATOR.
(defmethod component-depends-on((o test-op) (c (eql (find-system "tsort"))))
  (append (call-next-method)'((test-op "tsort.test"))))
(defmethod operate :around ((o test-op)(c (eql (find-system "tsort")))
                            &key ((:compile-print *compile-print*))
                            ((:compile-verbose *compile-verbose*))
                            &allow-other-keys)
  (call-next-method))
