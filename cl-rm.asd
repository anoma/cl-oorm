(asdf:defsystem :cl-rm
  :depends-on (:ironclad :serapeum :closer-mop :trivial-utf-8 :fset :flexi-streams)
  :version "0.1.0"
  :description "Common Lisp Objects to the Resource Machine"
  :license "MIT"
  :pathname "src/"
  :serial t
  :components
  ((:file package)
   (:file utils)
   (:file mixins)
   (:module cl-rm
    :serial t
    :description "RM Base and Meta Model"
    :components ((:file package)
                 (:file env)
                 (:file cl-rm)))
   (:module user
    :serial t
    :description "RM user code"
    :depends-on (cl-rm)
    :components ((:file package)
                 (:file numbers)
                 (:file unique)
                 (:file fixed-supply)
                 (:file ownership)
                 (:file spacebucks))))
    :in-order-to ((asdf:test-op (asdf:test-op :cl-rm/test))))

(asdf:defsystem :cl-rm/test
  :depends-on (:cl-rm :parachute)
  :description "Testing Cl OO"
  :pathname "test/"
  :serial t
  :components
  ((:file package)
   (:file run-tests)
   (:file helpers)
   (:file numbers)
   (:file fixed-supply)
   (:file environment)
   (:file ownership))
  :perform (asdf:test-op (o s)
                         (uiop:symbol-call :cl-rm.test :run-tests-error)))

(asdf:defsystem :cl-rm/moldable
  :depends-on (:cl-rm/test :clog-moldable-inspector :hyperdoc/explorer)
  :pathname "view/"
  :serial t
  :components
  ((:file package)
   (:file views)))
