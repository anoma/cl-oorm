(in-package :cl-rm.test)

(define-test cl-rm-environment
  :parent cl-rm-test-suite)

(defun single-environment ()
  (let ((env (cl-rm.env:empty-environment)))
    (cl-rm.env:put-metadata env 1 :signature (list 0 0))
    env))

(define-test environment-works-as-expected
  :parent cl-rm-environment
  (is equalp
      (cl-rm.env:lookup-metadata (single-environment) 1 :signature)
      (list 0 0)))
