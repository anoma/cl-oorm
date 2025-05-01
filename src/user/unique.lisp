(in-package :cl-rm.user)

;;; #############################################################################
;;;                                  Types                                      #
;;; #############################################################################

(defclass unique-mixin ()
  ((already-made :initarg :already-made
                 :accessor already-made?
                 :initform nil
                 :documentation
                 "I deal with if any particular object is already made
In particular I don't enforce that my slot is a boolean, leaving it
open for developers to decide if they want to set it to a specific
value to change my effects"))
  (:documentation "I am the unique-mixin. Inherit from me if you want your data to be
unique.

Every-time I am created within a transaction, I will be remembered.

Further I offer an API on how to `use' myself along with related data
that must also be included in the environment."))

(deftype emitted-kind ()
  `(member :create :consumed))

(defclass emitter ()
  ((emitted :initarg :emitted :accessor emitted)
   (kind-of :initarg :kind-of :accessor kind-of :type emitted-kind :initform :create
            :documentation "I can either be `:create' or `:use'"))
  (:documentation "I provide a decent entry-point for making an emitted object.
To make your own just implement `emit'"))

;;; #############################################################################
;;;                                Public API                                   #
;;; #############################################################################

;; Maybe we can keep these as functions that always go off?
(defgeneric use (unique)
  (:documentation "Emits the unique object being used into the transaction

I am important to call whenever an operation uses the unique data."))
(defgeneric create (unique)
  (:documentation "Emits the unique object being used into the transaction"))

;; We need to keep these methods though
(defgeneric related-use (unique)
  (:documentation "I list extra objects that need to be put into the transaction space called by `use'.

Please return a list of objects that can be called with `emit'. It is
customary to wrap your object in `emitter' if you want the default behaviour.")
  (:method ((u unique-mixin)) nil))

(defgeneric related-create (unique)
  (:documentation "I list extra objects that need to be put into transaction space called by `create'

Please return a list of objects that can be called with `emit'. It is
customary to wrap your object in `emitter' if you want the default behaviour.")
  (:method ((u unique-mixin)) nil))

(defgeneric emit (object)
  (:documentation "I emit the object into the environment"))

(defun emit-create (object)
  (make-instance 'emitter :emitted object))

(defun emit-consume (object)
  (make-instance 'emitter :emitted object :kind-of :consumed))

;;; #############################################################################
;;;                                 Instances                                   #
;;; #############################################################################

(defmethod use ((object unique-mixin))
  (cl-rm.env:emit-consumed object)
  (mapcar #'emit (related-use object)))

(defmethod create ((object unique-mixin))
  (cl-rm.env:emit-created object)
  (mapcar #'emit (related-create object)))

(defmethod emit ((object emitter))
  (ecase-of emitted-kind (kind-of object)
    (:create   (cl-rm.env:emit-created (emitted object)))
    (:consumed (cl-rm.env:emit-consumed (emitted object)))))

(defmethod cl-rm:delete :after ((object unique-mixin))
  (use object))

(defmethod initialize-instance :after ((instance unique-mixin) &key &allow-other-keys)
  ;; We should abstract, user code should not care about this!!!
  (unless (already-made? instance)
    (setf (already-made? instance) t)
    (create instance)))

;; We care about copying the instance also initializing So this isn't
;; magic just a consequence of copy not calling initialize but
;; re-initialize
(defmethod copy-instance :after ((object unique-mixin)
                                 &rest initargs &key &allow-other-keys)
  (apply #'initialize-instance object initargs))

(defmethod print-object ((obj emitter) stream)
  (print-unreadable-object (obj stream :type nil)
    (format stream "~A ~A" (kind-of obj) (emitted obj))))
