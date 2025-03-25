(in-package :cl-rm.mixins)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pointwise Mixins
;; These just control equality and how slots are computed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass pointwise-mixin () ()
  (:documentation "Provides the service of giving point wise
                   operations to classes"))

(defclass direct-pointwise-mixin (pointwise-mixin) ()
  (:documentation "Works like POINTWISE-MIXIN, however functions on
                   [POINTWISE-MIXIN] will only operate on direct-slots
                   instead of all slots the class may contain.

                   Further all `DIRECT-POINTWISE-MIXIN`'s are [POINTWISE-MIXIN]'s"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Instance methods for the operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod pointwise-slots ((object direct-pointwise-mixin))
  "Works like the normal POINTWISE-SLOTS however we only work on
   direct slot values"
  (c2mop:class-direct-slots (class-of object)))

(defmethod fset:compare ((x pointwise-mixin) (y pointwise-mixin))
  (fset:compare (to-pointwise-list x)
                (to-pointwise-list y)))

(defmethod obj-equalp ((obj1 pointwise-mixin) (obj2 pointwise-mixin))
  (and (c2mop:subclassp (type-of obj1) (type-of obj2))
       (obj-equalp (to-pointwise-list obj1)
                   (to-pointwise-list obj2))))
