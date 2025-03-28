(in-package :cl-rm.user)

;; Mainly here for example, "tokens" are trivial

(defclass spacebucks (ownership-mixin fixed-supply-mixin) ())

(defmethod always-true    ((object spacebucks)) t)
(defmethod holds-on-use   ((object spacebucks) (instance instance)) t)
(defmethod holds-on-intro ((object spacebucks) (instance instance)) t)
