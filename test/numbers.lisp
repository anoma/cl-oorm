(in-package :cl-rm.test)

(define-test cl-rm-numbers
  :parent cl-rm-test-suite)

(defun transacted-add ()
  (transact (add (counted 3) (counted 5))))

(define-test addative
  :parent cl-rm-numbers
  (let ((add (transacted-add)))
    (of-type compliance-unit add)
    (true (verify-compliance-unit add))))
