(in-package :cl-rm.user)

;;; #############################################################################
;;;                                  Types                                      #
;;; #############################################################################

(defclass fixed-supply-mixin (unique-mixin)
  ((supply-quantity :initarg :supply-quantity :accessor quantity :type integer))
  (:documentation "I maintain the invariant that things are of a fixed supply"))

(defclass fixed-supply-intent (cl-rm.mixins:pointwise-mixin) ; just for equality
  ((quantity         :initarg :quantity        :accessor quantity :type integer)
   (class-assurance  :initarg :class-assurance :accessor class-assurance :type symbol)
   (should-create?   :initarg :should-create? :accessor should-create? :type boolean))
  (:documentation
   "I represent an intent that assures fixed supply is maintained for a particular class.
In particular I assure that x number of tokens are created or burned"))

;;; #############################################################################
;;;                                Instances                                    #
;;; #############################################################################

(cl-rm.utils:define-generic-print fixed-supply-intent)

(defmethod related-create ((fixed fixed-supply-mixin))
  (cl-rm.env:emit-created (make-fixed-supply-intent fixed nil)))

(defmethod related-use ((fixed fixed-supply-mixin))
  (cl-rm.env:emit-created (make-fixed-supply-intent fixed t)))

(-> make-fixed-supply-intent (fixed-supply-mixin boolean) fixed-supply-intent)
(defun make-fixed-supply-intent (fixed-supply create)
  "Makes a `fixed-supply-intent' on the given instance, the second argument is
for if we should create or consume"
  (values (make-instance 'fixed-supply-intent
                         :quantity (quantity fixed-supply)
                         :class-assurance (class-name (class-of fixed-supply))
                         :should-create? create)))

(defmethod always-true ((object fixed-supply-intent)) t)
(defmethod holds-on-intro ((object fixed-supply-intent) (instance instance)) t)
(defmethod holds-on-use ((object fixed-supply-intent) (instance instance))
  (some (lambda (finding)
          (and (eq (class-name-of finding) (class-assurance object))
               (= (quantity finding) (quantity object))))
        (if (should-create? object)
            (created instance)
            (consumed instance))))

(defmethod holds-on-use :around ((object fixed-supply-mixin) (instance instance))
  (and (call-next-method)
       (fixed-supply-holds object instance t)))
(defmethod holds-on-intro :around ((object fixed-supply-mixin) (instance instance))
  (and (call-next-method)
       (fixed-supply-holds object instance nil)))

(-> fixed-supply-holds (fixed-supply-mixin instance boolean) boolean)
(defun fixed-supply-holds (object instance using?)
  (some (lambda (finding)
          (and (eq (class-name-of finding) 'fixed-supply-intent)
               (eq (class-assurance finding) (class-name (class-of object)))
               (= (quantity finding) (quantity object))
               (eq using? (should-create? finding))))
        (created instance)))

;;; #############################################################################
;;;                                   API                                       #
;;; #############################################################################

(defmethod split ((fixed fixed-supply-mixin) number)
  (when (> (quantity fixed) number)
    (use fixed)
    (list (copy-instance fixed :supply-quantity number)
          (copy-instance fixed :supply-quantity (- (quantity fixed) number)))))
