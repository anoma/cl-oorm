(in-package :cl-rm.env)

;; Currently these are not hooked-up to transaction
(defun empty-environment ()
  (list nil nil))

(defparameter *current-environment* (empty-environment)
  "I am the current environment for compiling a transaction")

(defparameter *top-level-action* (list nil (make-hash-table))
  "I am the top level transaction environment.

My structure is as follows:

1. a map from owner → signature
2. A top level action to sign over")

(defun flush-environment ()
  (setf *current-environment* (empty-environment)))

(defun top-level-action ()
  (car *top-level-action*))

(defun signed-action (key)
  (gethash key (cadr *top-level-action*)))

(defun emit-created (object)
  (push object (car *current-environment*)))

(defun emit-consumed (object)
  (push object (cadr *current-environment*)))

(defun current-created  () (car *current-environment*))
(defun current-consumed () (cadr *current-environment*))
