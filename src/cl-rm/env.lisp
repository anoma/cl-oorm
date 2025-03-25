(in-package :cl-rm.env)

;;; #############################################################################
;;;                                  Types                                      #
;;; #############################################################################

(defclass compilation-environment ()
  ((consumed :initarg :consumed :accessor consumed :type list :initform nil)
   (created  :initarg :created  :accessor created  :type list :initform nil)
   (comp-operation :initarg :operation
                   :accessor operation
                   :type symbol
                   :documentation "I am the top level operation that is being compiled")
   (transaction-data :initarg :environment
                     :accessor environment
                     :type hash-table
                     :initform (make-hash-table)
                     :documentation "I am included in action app-data,
in particular if you want to include data to a particular resource then
set a map for the resource such that:

env ⟶ (kind resource) → data-to-be-passed in")))

;; Currently these are not hooked-up to transaction
(defun empty-environment (&key operation)
  (make-instance 'compilation-environment :operation operation))

;;; #############################################################################
;;;                                Operations                                   #
;;; #############################################################################

;;; #############################################################################
;;;                                  Global                                     #
;;; #############################################################################

(defparameter *environment* (empty-environment)
  "I am the current environment for compiling a transaction")

(defun flush-environment ()
  (setf *environment* (empty-environment)))

(defun emit-created (object)
  (push object (created *environment*)))

(defun emit-consumed (object)
  (push object (consumed *environment*)))
