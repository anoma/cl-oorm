(in-package :cl-rm)

;;; #############################################################################
;;;                                  Types                                      #
;;; #############################################################################

;; We are mostly ignoring details about references for now
(defclass resource (cl-rm.mixins:pointwise-mixin)
  ((label :initarg :label :accessor label :type list)
   (data  :initarg :data  :accessor data :type list)
   ;; Think of a better init form for the compilation
   (logic :initarg :logic
          :accessor logic
          :initform t)
   (quantity :accessor quantity
             :initarg :quantity
             :type integer
             :initform 1)
   (nonce :accessor nonce
          :initarg :nonce
          :type (simple-vector 256)
          :initform (make-array 256 :element-type 'unsigned-byte))
   (ephemeral :initarg :ephmeral-p
              :accessor ephemeral-p
              :initform nil
              :type boolean
              :documentation "I represent if a resource is ephmeral")
   (nullifier-key-commitment
    :accessor nullifier-commitment
    :initarg :nullifier-commitment
    :initform nil)
   (randseed :accessor randseed
             :initarg :randseed
             :type integer
             :initform 0))
  (:documentation "I am a resource"))

;; I can form the rest as well though the exact machinery around
;; forming actions is more annoying to do properly

;; Our instance is a rough approximation of a proving context for an
;; individual resource... This not completely accurate
(defclass instance ()
  ((tag :initarg :tag :accessor tag :initform nil)
   (consumed-p :initarg :consumed-p :accessor consumed-p :type boolean :initform t)
   (consumed   :initarg :consumed :accessor consumed :type list :initform nil)
   (created    :initarg :created  :accessor created  :type list :initform nil)
   (environemnt :initarg :environment :accessor environment :initform nil
                :documentation "I am the specific app data for the instance"))
  (:documentation "A not very accurate instance"))


(defclass compliance-unit ()
  ((proof    :initarg :proof :accessor proof :initform nil)
   (vk       :initarg :verifying-key :accessor verifying-key :initform nil)
   (instances :initarg :instances :accessor instances :initform nil))
  (:documentation "In a transparent case the only real field is instance"))


;; Kinds of resources

(defclass method-resource ()
  ;; type is funcallable to be more accurate.
  ;; If we have more outputs, then we'd care about that as a field
  ((gf :initarg :gf :accessor gf :type symbol)
   (num-args :initarg :num-args :accessor num-args :type fixnum)))

;;; #############################################################################
;;;                               Constructors                                  #
;;; #############################################################################

(-> make-compliance-unit (list) compliance-unit)
(defun make-compliance-unit (instances)
  (values (make-instance 'compliance-unit :instances instances)))

;;; #############################################################################
;;;                                 Protocols                                   #
;;; #############################################################################

(defgeneric resource-logic (object instance any)
  (:documentation "I run the resource resource-logic for a given type"))

(defgeneric obj->resource (object)
  (:documentation "I turn an object into a resource"))

(defgeneric verify (proof instance key)
  (:documentation "Generic verify in a proving system"))

(defgeneric delete (object)
  (:documentation "I delete the given object")
  (:method ((object standard-object)) t)
  (:method ((n integer)) t)
  (:method ((s string)) t))

;;; #############################################################################
;;;                                 Instances                                   #
;;; #############################################################################

(cl-rm.utils:define-generic-print resource)
(cl-rm.utils:define-generic-print instance)
(cl-rm.utils:define-generic-print compliance-unit)

(defmethod obj->resource ((x standard-object))
  (let ((class (class-of x)))
    (make-instance
     'resource
     :data (cl-rm.utils:instance-values x)
     :logic #'resource-logic
     ;; Label is a reference to the slot values that we need to refer
     ;; to, however since we recreate the value and the data is
     ;; available Ill elide this detail, what we want is to basically
     ;; have a reference to the specific class values in some way
     ;; which can change, not sure what this looks like. However we
     ;; can always retrieve the class this way
     :label class)))

(defmethod obj->resource ((x number))
  (make-instance 'resource :data (list x)
                           :logic #'resource-logic
                           :label 'built-in-class))

(defmethod obj->resource ((r resource))
  r)

(defmethod resource-logic ((object method-resource) (instance instance) consumed?)
  (if consumed?
      t
      (cl-rm.utils:obj-equalp
       ;; We check the output is equal to the work
       (cadr (created instance))
       ;; We assume all inputs are stored next to each other at the start
       ;; of the consumed
       (apply (gf object)
              (serapeum:take (num-args object) (consumed instance))))))

(defmethod resource-logic ((object integer) (instance instance) any)
  t)

;;; #############################################################################
;;;                                    API                                      #
;;; #############################################################################

;; #############################################################################
;;                                    Kinds                                    #
;; #############################################################################

(-> kind (resource) integer)
(defun kind (resource)
  (manual-kind (logic resource) (label resource)))

(-> manual-kind (function t) integer)
(defun manual-kind (logic label)
  (sxhash (list logic label)))

;; We should define this over both compliance-units and transactions
(defmethod kind-balance ((comp compliance-unit))
  (let* ((instances (instances comp))
         (consumed (mapcar #'tag  (remove-if #'consumed-p instances)))
         (created  (mapcar #'tag (remove-if-not #'consumed-p instances))))
    (flet ((sets-of-kind (resources default-value)
             (reduce (lambda (s1 s2)
                       (fset:map-union s1 s2 #'+))
                     (mapcar (lambda (x)
                               (fset:with (fset:empty-map) (kind x) default-value))
                             resources))))
      (fset:map-union (sets-of-kind consumed 1) (sets-of-kind created -1) #'+))))

(-> resource->obj ((or null resource)) t)
(defun resource->obj (x)
  (when x
    (case (label x)
      (built-in-class
       (car (data x)))
      (t
       (cl-rm.utils:list-to-class (c2mop:ensure-finalized (label x))
                                  (data x))))))

(defmethod verify ((resource resource) (instance instance) consumed?)
  ;; time for the fun
  (assure boolean
    (funcall (logic resource)
             (resource->obj resource)
             (cl-rm.utils:copy-instance instance
                                        :consumed (mapcar #'resource->obj (consumed instance))
                                        :created (mapcar #'resource->obj (created instance)))
             ;; we pass consumed? because we don't reify creation into
             ;; the model, thus it has to be done in an ham-fisted
             ;; manner
             consumed?)))

;; Now let us verify our compliance unit
(-> verify-compliance-unit (compliance-unit) boolean)
(defun verify-compliance-unit (compliance)
  (null (failed-compliance-unit compliance)))

(-> failed-compliance-unit (compliance-unit) list)
(defun failed-compliance-unit (compliance)
  (let ((instances (instances compliance)))
    (remove-if (lambda (instance)
                 (verify (tag instance) instance (consumed-p instance)))
               instances)))


;;; #############################################################################
;;;                           Transaction Environment                           #
;;; #############################################################################

;; Current the transaction environment is quite simple and naive this
;; needs a proper environment around it.

(defmacro transact (expression &key (keys nil))
  ;; poor man's stepper
  (let ((rest (gensym "CDR")))
    `(let ((cl-rm.env:*environment*
             (cl-rm.env:empty-environment :operation ',(car expression))))
       ;; Insert the private keys now
       (mapcar (lambda (k) (cl-rm.env:put-private-key cl-rm.env:*environment* k)) ,keys)
       (let ((,rest (list ,@(cdr expression))))
         (mapcar #'delete ,rest)
         (transact-expression (list* ',(car expression) ,rest)
                              (apply #',(car expression) ,rest))))))

;; With a better environment we need a better way of expressing the
;; arguments
(defun transact-expression (expression result)
  (let* ((consumed (cdr expression))
         ;; just using the car isn't the most elegant
         (function (make-instance 'method-resource
                                  :gf (car expression)
                                  :num-args (length consumed)))
         (results  (list function result))
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
         (full-consumed
           (mapcar #'obj->resource
                   (append consumed
                           (remove-if (lambda (x)
                                        (member x consumed))
                                      (consumed cl-rm.env:*environment*)))))
         (full-created
           (mapcar #'obj->resource
                   (append results
                           (remove-if (lambda (x) (member x results))
                                      (created cl-rm.env:*environment*))))))
    (labels ((create-consumed (object)
               (make-instance 'instance
                              :created full-created
                              :consumed full-consumed
                              :consumed-p t
                              ;; modeling of tag not online
                              :tag object
                              :environment (cl-rm.env:lookup-metadata-table
                                            cl-rm.env:*environment*
                                            object)))
             (create-output (object)
               (let ((obj (create-consumed object)))
                 (setf (consumed-p obj) nil)
                 obj)))
      (make-compliance-unit
       ;; Order is: function, output, created, inputs, consumed
       (append (mapcar #'create-output full-created)
               (mapcar #'create-consumed full-consumed))))))
