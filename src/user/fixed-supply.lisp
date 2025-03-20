(in-package :cl-rm.user)

;; Without more scaffolding we can't do much... we want to maintain on
;; creation or deletion something exists, but these apis need to exist
;; in transact
(defclass fixed-supply-mixin ()
  ((supply-quantity :initarg :supply-quantity
                    :accessor quantity :type integer)))
