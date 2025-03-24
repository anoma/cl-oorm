(in-package :cl-rm.test)

(define-test cl-rm-fixed-supply
  :parent cl-rm-test-suite)


;; Our class we shall be testing against
(defclass only-fixed (fixed-supply-mixin) ())

;; We should be abstract but I'm testing directly against the mixin
(defmethod resource-logic ((object only-fixed) (instance instance) consumed?) t)

(defun 1000-supply ()
  (make-instance 'only-fixed :supply-quantity 1000))

(defun fixed-supply-transaction ()
  (transact (quantity (1000-supply))))


(define-test environment-is-correct
  :parent cl-rm-fixed-supply
  (let* ((*current-environment* (empty-environment))
         (supply (1000-supply)))
    (is = (length (current-created)) 2)
    (true (find-if (lambda (o)
                     (cl-rm.utils:obj-equalp
                      o
                      (make-instance 'fixed-supply-intent
                                     :should-create? nil
                                     :quantity 1000
                                     :class-assurance 'only-fixed)))
                   (current-created)))
    (true (find-if (lambda (o) (eq o supply)) (current-created))
          "The object should be exactly the same as ours")))

(define-test validated-correctly
  :parent cl-rm-fixed-supply
  (true (verify-compliance-unit (fixed-supply-transaction))))
