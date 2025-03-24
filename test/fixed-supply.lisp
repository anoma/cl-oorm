(in-package :cl-rm.test)

(define-test cl-rm-fixed-supply
  :parent cl-rm-test-suite)


;; Our class we shall be testing against
(defclass only-fixed (fixed-supply-mixin) ())

;; We should be abstract but I'm testing directly against the mixin
(defmethod resource-logic ((object only-fixed) (instance instance) consumed?) t)

(defun 1000-supply ()
  (make-instance 'only-fixed :supply-quantity 1000))

(defun 1000-intent ()
  "I am the intent that gets created when creating a fixed supply intent"
  (make-instance 'fixed-supply-intent
                 :should-create? nil
                 :quantity 1000
                 :class-assurance 'only-fixed))

(defun fixed-supply-transaction ()
  (transact (quantity (1000-supply))))


(define-test environment-is-correct
  :parent cl-rm-fixed-supply
  (let* ((*current-environment* (empty-environment))
         (supply (1000-supply)))
    (is = (length (current-created)) 2)
    (true (find-if (lambda (o)
                     (cl-rm.utils:obj-equalp o (1000-intent)))
                   (current-created)))
    (true (find-if (lambda (o) (eq o supply)) (current-created))
          "The object should be exactly the same as ours")))

(define-test validated-correctly
  :parent cl-rm-fixed-supply
  (true (verify-compliance-unit (fixed-supply-transaction))))

(define-test kind-is-expected
  :parent cl-rm-fixed-supply
  (let* ((transact (fixed-supply-transaction))
         (supply (1000-supply))
         (kinds (cl-rm:kind-balance transact)))
    (flush-environment)
    ;; This create a balance of
    ;; TEST> (cl-rm::kind-balance (fixed-supply-transaction))
    ;; #{|
    ;;    (37152276167171639 0)      ; only-fixed
    ;;    (931098238869694181 1)     ; method
    ;;    (1762477709553014044 1)    ; built-in-class
    ;;    (3821453031763966642 2) |} ; fixed-supply-intent
    (is = 1 (fset:lookup kinds (kind (obj->resource 5)))
        "The output should be created, and be unbalanced at 1")
    (is = 0 (fset:lookup kinds (kind (obj->resource supply)))
        "Supply should be 0 as it's created and consumed")
    (is = 1 (fset:lookup kinds
                         (kind (obj->resource
                                (make-instance 'method-resource :gf 'a :num-args 1))))
        "The method should be created and not consumed")
    (is = 2 (fset:lookup kinds
                         (kind (obj->resource (1000-intent))))
        "The intent is added twice once for create and once for destroy")))
