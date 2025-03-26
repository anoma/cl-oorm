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
                     ;; we have to use fset as we need our slotwise equality
                     :initform (fset:empty-map)
                     :documentation "I am included in action app-data,
in particular if you want to include data to a particular resource then
set a map for the resource such that:

env ⟶ (kind resource) → data-to-be-passed in

Non resources can also use it, with their data being pruned before the
transaction is made")))

;; Currently these are not hooked-up to transaction
(defun empty-environment (&key operation)
  (make-instance 'compilation-environment :operation operation))

;;; #############################################################################
;;;                                Operations                                   #
;;; #############################################################################

;; We want to expose functions that makes working over things easier
(-> put-metadata (compilation-environment t t t) fset:map)
(defun put-metadata (env data key value)
  "Put any data into a specified key into the environment"
  (let ((res   (obj->resource data))
        (table (environment env)))
    (setf (environment env)
          (fset:with table
                     res
                     (fset:with (lookup-metadata-table env data) key value)))))

(-> lookup-metadata-table (compilation-environment t) fset:map)
(defun lookup-metadata-table (env data)
  "I lookup the metadata table of a particular value"
  (or (fset:lookup (environment env) (obj->resource data))
      (fset:empty-map)))

(-> lookup-metadata (compilation-environment t t) t)
(defun lookup-metadata (env data key)
  "I lookup the metadata table of a particular value"
  (fset:lookup (lookup-metadata-table env data) key))

;; These functions use the API but may change, so we encapsulate the
;; lookup here

(-> put-private-key (compilation-environment ironclad:ed25519-private-key) fset:map)
(defun put-private-key (env private-key)
  "Puts the private key as metadata in the public key. We don't add this
to any resources"
  (put-metadata
   env
   (ironclad:make-public-key :ed25519 :y (ironclad:ed25519-key-y private-key))
   :private private-key))

(-> lookup-private-key
    (compilation-environment ironclad:ed25519-public-key)
    (or nil ironclad:ed25519-private-key))
(defun lookup-private-key (env pub)
  "Puts the private key as metadata in the public key. We don't add this
to any resources"
  (lookup-metadata env pub :private))

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


