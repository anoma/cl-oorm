(in-package :cl-rm.relation)

;;; #############################################################################
;;;                                  Types                                      #
;;; #############################################################################

(deftype emitted-kind ()
  `(member :create :consumed))

(defclass emitter ()
  ((emitted :initarg :emitted :accessor emitted)
   (kind-of :initarg :kind-of :accessor kind-of :type emitted-kind :initform :create
            :documentation "I can either be `:create' or `:use'"))
  (:documentation "I provide a decent entry-point for making an emitted object.
To make your own just implement `emit'"))

;;; #############################################################################
;;;                                Protocols                                    #
;;; #############################################################################

;; We need to keep these methods though
(defgeneric related-use (unique)
  (:documentation "I list extra objects that need to be put into the transaction space called by `use'.

Please return a list of objects that can be called with `emit'. It is
customary to wrap your object in `emitter' if you want the default behaviour.")
  (:method ((o standard-object)) nil)
  (:method ((i integer)) nil)
  (:method ((s string)) nil)
  (:method ((l list)) nil))

(defgeneric related-create (unique)
  (:documentation "I list extra objects that need to be put into transaction space called by `create'

Please return a list of objects that can be called with `emit'. It is
customary to wrap your object in `emitter' if you want the default behaviour.")
  (:method ((o standard-object)) nil)
  (:method ((i integer)) nil)
  (:method ((s string)) nil)
  (:method ((l list)) nil))

(defgeneric emit (object)
  (:documentation "I emit the object into the environment"))

;;; #############################################################################
;;;                                Instances                                    #
;;; #############################################################################

(defmethod emit ((object emitter))
  (ecase-of emitted-kind (kind-of object)
    (:create   (cl-rm.env:emit-created (emitted object)))
    (:consumed (cl-rm.env:emit-consumed (emitted object)))))

(defmethod print-object ((obj emitter) stream)
  (print-unreadable-object (obj stream :type nil)
    (format stream "~A ~A" (kind-of obj) (emitted obj))))

;;; #############################################################################
;;;                                 Helpers                                     #
;;; #############################################################################

(defun emit-create (object)
  (make-instance 'emitter :emitted object))

(defun emit-consume (object)
  (make-instance 'emitter :emitted object :kind-of :consumed))
