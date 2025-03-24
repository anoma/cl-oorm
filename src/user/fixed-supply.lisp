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
    (emit-created instance)
    (emit-created (make-fixed-supply-intent instance nil))))

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
  (values (make-instance 'fixed-supply-intent
                         :quantity (quantity fixed-supply)
                         :class-assurance (class-name (class-of fixed-supply))
                         :should-create? create)))

(defmethod resource-logic ((object fixed-supply-intent) (instance instance) consumed?)
  (if consumed?
      (let* ((class (find-class (class-assurance object)))
             (kind-wanted (manual-kind #'resource-logic class)))
        (true (find-if
               (lambda (resource)
                 (and (= kind-wanted (kind resource))
                      (= (quantity (resource->obj resource))
                         (quantity object))))
               (if (should-create? object)
                   (created instance)
                   (consumed instance)))))
      t))

;; This code is very low level sadly
(defmethod resource-logic :around ((object fixed-supply-mixin) (instance instance) consumed?)
  (let* ((class (class-of object))
         (kind-want (manual-kind #'resource-logic (find-class 'fixed-supply-intent))))
    (and (call-next-method)
         (true (find-if
                (lambda (resource)
                  (and (= kind-want (kind resource))
                       (let ((found (resource->obj resource)))
                         (and (eq (class-assurance found) (class-name class))
                              (if consumed?
                                  (should-create? found)
                                  (not (should-create? found)))))))
                (created instance))))))

;;; #############################################################################
;;;                                   API                                       #
;;; #############################################################################


;; We make useage explicit, kinda annoying but it is what it is.
(defmethod use ((fixed fixed-supply-mixin))
  (emit-consumed fixed)
  (emit-created (make-fixed-supply-intent fixed t)))

(defmethod split ((fixed fixed-supply-mixin) number)
  (when (> (quantity fixed) number)
    (use fixed)
    (list (copy-instance fixed :supply-quantity number)
          (copy-instance fixed :supply-quantity (- (quantity fixed) number)))))
