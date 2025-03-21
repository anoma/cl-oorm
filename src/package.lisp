(defpackage #:cl-rm.utils
  (:documentation "Utility Functions for the codebase")
  (:use #:common-lisp #:serapeum #:ironclad)
  (:export
   ;; Generic Utility Functions
   :symbol-to-keyword
   :define-generic-print
   ;; ###################################
   ;; Generic Data Processing Protocols #
   ;; ###################################
   ;; Point-wise
   :pointwise-slots
   :obj-equalp
   :to-pointwise-list
   :copy-instance
   ;; list Traversals
   :list-to-class
   :to-list
   :instance-values
   :class-values))

(defpackage #:cl-rm.mixins
  (:documentation "Mixins for the codebase")
  (:use #:common-lisp #:serapeum #:cl-rm.utils)
  (:export :direct-pointwise-mixin :pointwise-mixin))

(defpackage #:cl-rm
  (:documentation "A resource machine implementation and exploration")
  (:shadow :@ :take :delete)
  (:use #:common-lisp #:serapeum #:ironclad)
  (:export
   ;; Types
   :resource
   :label :data :logic :quantity :nonce :ephmeral-p :nullifier-commitment :randseed

   :instance :tag :consumed-p :consumed :created
   :compliance-unit :proof :verifying-key :instances
   :method-resource :gf
   ;; Constructors
   :make-compliance-unit

   ;; Meta Model
   :obj->resource
   :resource->obj
   :verify-compliance-unit

   ;;
   ;; API, I think this is the main public part
   ;;
   ;; Model Meta Protocols (because we can't hack the MOP)
   :delete
   :resource-logic     ; this exists as the base predicate for now
   :kind
   ;; Normal API
   :verify
   :transact

   ;; Environmental manipulation functions
   :top-level-action :signed-action
   :emit-created :emit-consumed
   :current-created :current-consumed

   ;; Useful to expose for testing, should not be used by users
   :*current-environment* :*top-level-action*
   :empty-environment :flush-environment))
