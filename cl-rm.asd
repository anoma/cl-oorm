(asdf:defsystem :cl-rm
  :depends-on (:ironclad :serapeum :closer-mop :trivial-utf-8)
  :version "0.1.0"
  :description "Common Lisp Objects to the Resource Machine"
  :license "MIT"
  :pathname "src/"
  :serial t
  :components
  ((:file package)
   (:file utils)
   (:file mixins)
   (:file cl-rm)
   (:module user
    :serial t
    :description "RM user code"
    :components ((:file package)
                 (:file numbers))))
    :in-order-to ((asdf:test-op (asdf:test-op :cl-rm/test))))

(asdf:defsystem :cl-rm/test
  :depends-on (:cl-rm :parachute)
  :description "Testing Cl OO"
  :pathname "test/"
  :serial t
  :components
  ((:file package)
   (:file run-tests)
   (:file numbers))
  :perform (asdf:test-op (o s)
                         (uiop:symbol-call :cl-rm.test :run-tests-error)))
