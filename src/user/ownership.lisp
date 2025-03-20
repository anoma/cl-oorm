(in-package :cl-rm.user)

;; Without more scaffolding we can't do much, as we want to sign over
;; numbers for validity
(defclass ownership-mixin ()
  ((owner :initarg :owner :accessor owner :type ironclad:ed25519-public-key)))
