(defpackage #:cl-rm.test
  (:export :run-tests :run-tests-error)
  (:shadow :delete)
  (:use :cl-rm :cl-rm.user :common-lisp :parachute))

(in-package #:cl-rm.test)

(define-test cl-rm-test-suite
  :serial nil)
