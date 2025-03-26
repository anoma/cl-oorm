(defpackage #:cl-rm.user
  (:documentation "User code for the system")
  (:shadow :@ :take :delete)
  (:export
   ;; Integer API

   :interger-obj :data
   :counted-integer :counter

   :mk-integer :counted

   :add :add-2

   ;; Ownership API
   :ownership-mixin :owner
   :try-signing
   ;; Fixed-Supply API
   :fixed-supply-mixin :quantity
   :fixed-supply-intent :class-assurance :should-create?
   :split :use)
  (:use #:cl-rm.utils #:cl-rm
        #:common-lisp #:serapeum #:ironclad))
