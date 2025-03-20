(in-package :cl-rm.test)

(define-test cl-rm-numbers
  :parent cl-rm-test-suite)

(defun transacted-add ()
  (transact (add (counted 3) (counted 5))))

(defun invalid-number ()
  (let ((obj (obj->resource (counted 4))))
    (setf (data obj) (list "hi"))
    (resource->obj obj)))

(define-test addative
  :parent cl-rm-numbers
  (let ((add (transacted-add)))
    (of-type compliance-unit add)
    (true (verify-compliance-unit add))))

(define-test invalid-predicates
  :parent cl-rm-numbers
  (false (verify-compliance-unit (transact (invalid-number)))))
