(in-package #:cl-rm.utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun symbol-to-keyword (symbol)
  "Turns a [symbol] into a [keyword]"
  (intern (symbol-name symbol) :keyword))

(defmacro define-generic-print (type)
  `(defmethod print-object ((obj ,type) stream)
     (pprint-logical-block (stream nil)
       (print-unreadable-object (obj stream :type t)
         (format stream "~2I~{~_~A~^ ~}" (to-list obj))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; API for Generic Traversal Protocol
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric pointwise-slots (obj)
  (:documentation "Works like C2MOP:COMPUTE-SLOTS however on the object
                   rather than the class")
  ;; should we specialize it on pointwise-mixin instead? Should all
  ;; objects be able to give their pointwise slots?
  (:method ((object standard-object))
    (c2mop:compute-slots (class-of object))))

(defgeneric obj-equalp (object1 object2)
  (:documentation "Compares objects with pointwise equality. This is a
                   much weaker form of equality comparison than
                   STANDARD-OBJECT EQUALP, which does the much
                   stronger pointer quality"))

(defgeneric to-pointwise-list (obj)
  (:documentation "Turns a given object into a pointwise LIST. listing
                   the KEYWORD slot-name next to their value.")
  (:method ((obj standard-object))
    (mapcar (lambda (x)
              (cons (symbol-to-keyword x)
                    (slot-value obj x)))
            (mapcar #'c2mop:slot-definition-name
                    (pointwise-slots obj)))))

(defgeneric copy-instance (object  &rest initargs &key &allow-other-keys)
  (:documentation
   "Makes and returns a shallow copy of OBJECT.

  An uninitialized object of the same class as OBJECT is allocated by
  calling ALLOCATE-INSTANCE.  For all slots returned by
  CLASS-SLOTS, the returned object has the
  same slot values and slot-unbound status as OBJECT.

  REINITIALIZE-INSTANCE is called to update the copy with INITARGS."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; API for List Class Traversal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun list-to-class (class list)
  (apply #'copy-instance
         (allocate-instance class)
         (mapcan (lambda (slot value)
                   (list (car (c2mop:slot-definition-initargs slot)) value))
                 (instance-slots class)
                 list)))

(defun to-list (object)
  "Turns an object into a list"
  (mapcar #'cdr (to-pointwise-list object)))

;; Bad way to define these but w/e
(defgeneric instance-values (object)
  (:documentation "Gets all the instance values of a list")
  (:method ((object standard-object))
    (mapcar (lambda (x) (slot-value object x))
            (mapcar #'c2mop:slot-definition-name
                    (instance-slots (class-of object))))))

(defgeneric class-values (object)
  (:documentation "Gets all the class values of a list")
  (:method ((object standard-object))
    (mapcar (lambda (x) (slot-value object x))
            (mapcar #'c2mop:slot-definition-name
                    (remove-if-not
                     (lambda (x) (eq :class (c2mop:slot-definition-allocation x)))
                     (pointwise-slots object))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun instance-slots (class)
  (remove-if-not (lambda (x)
                   (eq :instance
                       (c2mop:slot-definition-allocation x)))
                 (c2mop:compute-slots class)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Instances
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod obj-equalp ((obj1 standard-object) (obj2 standard-object))
  (equalp obj1 obj2))

(defmethod obj-equalp ((obj1 list) (obj2 list))
  (or (eq obj1 obj2)
      (and (consp obj1)
           (consp obj2)
           (obj-equalp (car obj1) (car obj2))
           (obj-equalp (cdr obj1) (cdr obj2)))))

;; I should implement it for arrays as well!
(defmethod obj-equalp ((obj1 t) (obj2 t))
  (equalp obj1 obj2))

(defmethod copy-instance ((object standard-object) &rest initargs &key &allow-other-keys)
  (let* ((class (class-of object))
         (copy (allocate-instance class)))
    (dolist (slot (c2mop:class-slots class))
      ;; moved the mapcar into a let, as allocation wise, CCL
      ;; performed better this way.
      (let ((slot-name (c2mop:slot-definition-name slot)))
        (when (slot-boundp object slot-name)
          (setf (slot-value copy slot-name)
                (slot-value object slot-name)))))
    (values
     (apply #'reinitialize-instance copy initargs))))
