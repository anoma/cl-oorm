(in-package :cl-rm.user)

;;; #############################################################################
;;;                                  Types                                      #
;;; #############################################################################

;; Without more scaffolding we can't do much... we want to maintain on
;; creation or deletion something exists, but these apis need to exist
;; in transact
(defclass fixed-supply-mixin ()
  ((supply-quantity :initarg :supply-quantity
                    :accessor quantity :type integer)
   ;; Use after and before methods but use (list class-name-of-action symbol)
   ;; This can be simplified by swapping the method order and we can
   ;; go back to a bool
   (already-global :initarg :already-global
                   :accessor global? :type boolean
                   :initform nil))
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

(defmethod initialize-instance :after ((instance fixed-supply-mixin) &key &allow-other-keys)
  ;; We should abstract, user code should not care about this!!!
  (unless (global? instance)
    (setf (global? instance) t)
    (cl-rm.env:emit-created instance)
    (cl-rm.env:emit-created (make-fixed-supply-intent instance nil))))

;; We care about copying the instance also initializing So this isn't
;; magic just a consequence of copy not calling initialize but
;; re-initialize
(defmethod copy-instance :after ((object fixed-supply-mixin)
                                 &rest initargs &key &allow-other-keys)
  (apply #'initialize-instance object initargs))

(defmethod cl-rm:delete :after ((object fixed-supply-mixin))
  (use object))

(cl-rm.utils:define-generic-print fixed-supply-intent)

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
               (eq using? (should-create? finding))))
        (created instance)))

;;; #############################################################################
;;;                                   API                                       #
;;; #############################################################################


;; We make useage explicit, kinda annoying but it is what it is.
(defmethod use ((fixed fixed-supply-mixin))
  (cl-rm.env:emit-consumed fixed)
  (cl-rm.env:emit-created (make-fixed-supply-intent fixed t)))

(defmethod split ((fixed fixed-supply-mixin) number)
  (when (> (quantity fixed) number)
    (use fixed)
    (list (copy-instance fixed :supply-quantity number)
          (copy-instance fixed :supply-quantity (- (quantity fixed) number)))))
