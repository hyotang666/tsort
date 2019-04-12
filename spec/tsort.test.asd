; vim: ft=lisp et
(in-package :asdf)
(defsystem :tsort.test
  :depends-on
  (:jingoh "tsort")
  :components
  ((:file "tsort"))
  :perform
  (test-op (o c) (symbol-call :jingoh :examine :tsort)))
