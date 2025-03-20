(in-package :cl-rm)

;;; #############################################################################
;;;                                  Types                                      #
;;; #############################################################################

;; We are mostly ignoring details about references for now
(defclass resource ()
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
  ((tag :initarg :tag :accessor tag)
   (consumed-p :initarg :consumed-p :accessor consumed-p :type boolean :initform t)
   (consumed   :initarg :consumed :accessor consumed :type list :initform nil)
   (created    :initarg :created  :accessor created  :type list :initform nil)))


(defclass compliance-unit ()
  ((proof    :initarg :proof :accessor proof :initform nil)
   (vk       :initarg :verifying-key :accessor verifying-key :initform nil)
   (instances :initarg :instances :accessor instances :initform nil))
  (:documentation "In a transparent case the only real field is instance"))


;; Kinds of resources

(defclass method-resource ()
  ;; type is funcallable to be more accurate.
  ;; If we have more outputs, then we'd care about that as a field
  ((gf :initarg :gf :accessor gf :type symbol)))

;;; #############################################################################
;;;                               Constructors                                  #
;;; #############################################################################

(-> make-compliance-unit (list) compliance-unit)
(defun make-compliance-unit (instances)
  (values (make-instance 'compliance-unit :instances instances)))

;;; #############################################################################
;;;                                 Protocols                                   #
;;; #############################################################################

(defgeneric obj-resource-logic (object instance any)
  (:documentation "I run the resource obj-resource-logic for a given type"))

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

(defmethod obj->resource ((x standard-object))
  (let ((class (class-of x)))
    (make-instance
     'resource
     :data (cl-rm.utils:instance-values x)
     :logic #'obj-resource-logic
     ;; Label is a reference to the slot values that we need to refer
     ;; to, however since we recreate the value and the data is
     ;; available Ill elide this detail, what we want is to basically
     ;; have a reference to the specific class values in some way
     ;; which can change, not sure what this looks like. However we
     ;; can always retrieve the class this way
     :label class)))

(defmethod obj->resource ((x number))
  (make-instance 'resource :data (list x)
                           :logic #'obj-resource-logic
                           :label 'built-in-class))

(defmethod obj-resource-logic ((object method-resource) (instance instance) any)
  (cl-rm.utils:obj-equalp
   ;; We check the output is equal to the work
   (resource->obj (cadr (created instance)))
   (apply (gf object)
          (mapcar #'resource->obj (consumed instance)))))

(defmethod obj-resource-logic ((object integer) (instance instance) any)
  t)

;;; #############################################################################
;;;                                    API                                      #
;;; #############################################################################

(-> resource->obj (resource) t)
(defun resource->obj (x)
  (case (label x)
    (built-in-class
     (car (data x)))
    (t
     (cl-rm.utils:list-to-class (c2mop:ensure-finalized (label x))
                                (data x)))))

(defmethod verify ((resource resource) (instance instance) consumed?)
  ;; time for the fun
  (assure boolean
    (funcall (logic resource)
             (resource->obj resource)
             instance
             ;; we pass consumed? because we don't reify creation into
             ;; the model, thus it has to be done in an ham-fisted
             ;; manner
             consumed?)))

(defmacro transact (expression)
  `(transact-expression (list ',(car expression)
                              ,@(cdr expression))
                        ,expression))

(defun transact-expression (expression result)
  (let ((consumed (mapcar #'obj->resource (cdr expression)))
        (output   (obj->resource result))
        ;; just using the car isn't the most elegant
        (function (obj->resource
                   (make-instance 'method-resource :gf (car expression)))))
    (labels ((create-consumed (object)
               (make-instance 'instance
                              :created (list function output)
                              :consumed consumed
                              :consumed-p t
                              ;; modeling of tag not online
                              :tag object))
             (create-output (object)
               (let ((obj (create-consumed object)))
                 (setf (consumed-p obj) nil)
                 obj)))
      (make-compliance-unit
       ;; Order is: function, output, inputs
       (list* (create-output function)
              (create-output output)
              (mapcar #'create-consumed consumed))))))

;; Now let us verify our compliance unit
(-> verify-compliance-unit (compliance-unit) boolean)
(defun verify-compliance-unit (compliance)
  (let ((instances (instances compliance)))
    (every (lambda (instance)
             (verify (tag instance) instance (consumed-p instance)))
           instances)))
