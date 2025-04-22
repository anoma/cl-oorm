(in-package :cl-rm.test)

(define-test cl-rm-ownership
  :parent cl-rm-test-suite)

(defparameter *alice-key* (ironclad:generate-key-pair :ed25519))
(defparameter *bob-key*   (ironclad:generate-key-pair :ed25519))
(defparameter *alice-public*
  (ironclad:make-public-key :ed25519 :y (ironclad:ed25519-key-y *alice-key*)))

(defclass only-owned (ownership-mixin)
  ((value :initarg :value :accessor value)))

(defmethod always-true    ((object only-owned)) t)
(defmethod holds-on-use   ((object only-owned) (instance instance)) t)
(defmethod holds-on-intro ((object only-owned) (instance instance)) t)

(defexample alice-1
  (make-instance 'only-owned :value 1 :owner *alice-public*))

(defexample signed-1
  (let ((cl-rm.env:*environment* (cl-rm.env:empty-environment :operation 'foo)))
    (cl-rm.env:put-private-key cl-rm.env:*environment* *alice-key*)
    (cl-rm.user:try-signing (alice-1))
    cl-rm.env:*environment*))

(defexample Properly-signed-away
  (transact (drop-all (alice-1)) :keys (list *alice-key*)))

(defexample improperly-signed-away
  (transact (drop-all (alice-1)) :keys (list *bob-key*)))


(define-test signed-actually-signs
  :parent cl-rm-ownership
  (let ((signed (signed-1)))
    (true (cl-rm.env:lookup-private-key signed (owner (alice-1))))
    (true (ironclad:verify-signature *alice-public*
                                     (cl-rm.utils:symbol-to-bytes 'foo)
                                     (cl-rm.env:lookup-metadata signed (alice-1) :signature)))))

(define-test ownership-resource-logic-works
  :parent cl-rm-ownership
  (true (verify-compliance-unit (properly-signed-away)))
  (false (verify-compliance-unit (improperly-signed-away))))
