(in-package :cl-rm.user)

;; We define the most basic of types, a wrapper over a number, and a
;; version with a class slot

(defclass integer-obj (cl-rm.mixins:pointwise-mixin)
  ((data :initarg :data :accessor data :type integer)))

;; We can do this via a meta class but I want to see how this works
(defclass counted-integer (integer-obj)
  ((counter :initarg :counter
            :accessor counter
            :allocation :class
            :initform 0)))

(-> mk-integer (integer) integer-obj)
(defun mk-integer (x)
  (values (make-instance 'integer-obj :data x)))

(-> counted (integer) counted-integer)
(defun counted (x)
  (values (make-instance 'counted-integer :data x)))

;;; #############################################################################
;;;                              Meta Model Code                                #
;;; #############################################################################

(define-generic-print integer-obj)

(defmethod holds-on-use   ((object integer-obj) (instance instance)) t)
(defmethod holds-on-intro ((object integer-obj) (instance instance)) t)
(defmethod always-true    ((object integer-obj))
  (integerp (data object)))


;;; #############################################################################
;;;                             Basic Operations                                #
;;; #############################################################################

(defun add (arg1 &rest argn)
  (reduce #'add-2 argn :initial-value arg1))

;; Let us make our first operation
(defmethod add-2 ((int1 integer-obj)
                  (int2 integer-obj))
  ;; if something is a subclass make that instead
  (copy-instance int1 :data (+ (data int1) (data int2))))
