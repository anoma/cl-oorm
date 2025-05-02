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

(defparameter *environment* (empty-environment)
  "I am the current environment for compiling a transaction")

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
    (or null ironclad:ed25519-private-key))
(defun lookup-private-key (env pub)
  "Puts the private key as metadata in the public key. We don't add this
to any resources"
  (lookup-metadata env pub :private))

;; could be tail recursive...
(-> compute-all-related (compilation-environment) compilation-environment)
(defun compute-all-related (env)
  (let ((*environment* (cl-rm.utils:copy-instance env :consumed nil :created nil))
        (related       (related-objects env)))
    (if (not related)
        env
        (progn
          (mapcar #'emit related)
          ;; Slower than it needs to be, we just need to compose the two fields really
          (union-envs env
                      (compute-all-related (remove-duplicate-resources *environment*)))))))

(-> compute-all-related-with (compilation-environment &key (:arguments list) (:results list))
    compilation-environment)
(defun compute-all-related-with (env &key arguments results)
  (compute-all-related
   ;; We are filtering out resources that are in the inputs that
   ;; emit themselves. This isn't full proof as we really should
   ;; use remove-duplicates, however this has the issue of
   ;; removing (+ 1 1).... I think I need to implement a better
   ;; system for function application to better see what
   ;; arguments it takes
   ;;
   ;; A more robust system is to mark what slot I care about and
   ;; what positions do I need to apply this in, that should
   ;; work generically
   (cl-rm.utils:copy-instance env :consumed (append arguments
                                                    (remove-if (lambda (x) (member x arguments))
                                                               (consumed env)))
                                  :created (append results
                                                   (remove-if (lambda (x) (member x results))
                                                              (created env))))))

(-> to-compliance-unit (compilation-environment) cl-rm:compliance-unit)
(defun to-compliance-unit (env)
  (let ((consumed (mapcar #'obj->resource (consumed env)))
        (created  (mapcar #'obj->resource (created env))))
    (labels ((create (object tag)
               (make-instance 'cl-rm:instance
                              :created created
                              :consumed consumed
                              :consumed-p tag
                              ;; modeling of tag not online
                              :tag object
                              :environment (cl-rm.env:lookup-metadata-table
                                            cl-rm.env:*environment*
                                            object))))
      (cl-rm:make-compliance-unit
       ;; Order is: function, output, created, inputs, consumed
       (append (mapcar (lambda (c) (create c nil)) created)
               (mapcar (lambda (c) (create c t)) consumed))))))
;;; #############################################################################
;;;                                  Global                                     #
;;; #############################################################################



(defun flush-environment ()
  (setf *environment* (empty-environment)))

(defun emit-created (object)
  (push object (created *environment*)))

(defun emit-consumed (object)
  (push object (consumed *environment*)))


;;; #############################################################################
;;;                                  Helpers                                    #
;;; #############################################################################

(-> related-objects (compilation-environment) list)
(defun related-objects (env)
  (append (mapcan #'related-use (consumed env))
          (mapcan #'related-create (created env))))

(-> remove-duplicate-resources (compilation-environment) compilation-environment)
(defun remove-duplicate-resources (env)
  (cl-rm.utils:copy-instance env :consumed (remove-duplicates (consumed env))
                                 :created (remove-duplicates (created env))))

(-> union-envs (compilation-environment compilation-environment) compilation-environment)
(defun union-envs (env1 env2)
  (make-instance 'compilation-environment
                 :operation (operation env1)
                 :environment (fset:map-union (environment env1)
                                              (environment env2)
                                              (lambda (_ y) (declare (ignorable _)) y))
                 :created  (append (created env1) (created env2))
                 :consumed (append (consumed env1) (consumed env2))))
