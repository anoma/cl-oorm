(in-package :cl-rm.user)

;; Without more scaffolding we can't do much, as we want to sign over
;; numbers for validity
(defclass ownership-mixin ()
  ((owner :initarg :owner :accessor owner :type ironclad:ed25519-public-key)))

(defmethod cl-rm:delete :after ((object ownership-mixin))
  ;; We want to instantiate the signing in the env if we have it
  (try-signing object))

(-> try-signing (ownership-mixin) t)
(defun try-signing (object)
  ;; We store the key here for now, not great but w/e
  (let* ((env cl-rm.env:*environment*)
         (priv (cl-rm.env:lookup-private-key env (owner object))))
    (when priv
      (~>> env
           operation
           cl-rm.utils:symbol-to-bytes
           (ironclad:sign-message priv)
           (cl-rm.env:put-metadata env object :signature)))))

(defmethod resource-logic :around ((object ownership-mixin) (instance instance) consumed?)
  (let ((sig (fset:lookup (environment instance) :signature)))
    (and (call-next-method)
         (or (not consumed?)
             (and sig
                  (true (find-if (lambda (r)
                                   (and (c2mop:subclassp (class-of r) 'method-resource)
                                        (ironclad:verify-signature
                                         (owner object)
                                         (cl-rm.utils:symbol-to-bytes (gf r))
                                         sig)))
                                 (created instance))))))))
