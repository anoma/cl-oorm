;; We can have the other packages define this for us
(defpackage #:cl-rm.generics
  (:use #:common-lisp #:serapeum)
  (:export
   :consumed :created
   :tag

   :resource-logic         ; this exists as the base predicate for now
   :kind
   :manual-kind
   :kind-balance

   ;; Normal API
   :verify
   :delete
   ;; Meta Model
   :obj->resource
   :resource->obj
   ;; Environmental
   :environment
   :operation))

(defpackage #:cl-rm.env
  (:documentation "I hold the environmental model for the compilation model over the RM")
  (:use #:common-lisp #:serapeum #:cl-rm.generics)
  (:export
 ;; Environmental manipulation functions
   :top-level-action :signed-action
   :emit-created :emit-consumed

   ;; Useful to expose for testing, should not be used by users
   :*environment*
   :empty-environment :flush-environment

   ;; useful for putting things in the metadata table and lookup
   :put-metadata
   :lookup-metadata-table :lookup-metadata
   ;; Private key helper functions
   :lookup-private-key :put-private-key))

(uiop:define-package #:cl-rm
  (:documentation "A resource machine implementation and exploration")
  (:shadow :@ :take :delete)
  (:use #:common-lisp #:serapeum #:ironclad)
  (:use-reexport #:cl-rm.generics)
  (:export
   ;; Types
   :resource
   :label :data :logic :quantity :nonce :ephmeral-p :nullifier-commitment :randseed

   :instance :tag :consumed-p
   :compliance-unit :proof :verifying-key :instances
   :method-resource :gf
   ;; Constructors
   :make-compliance-unit

   :verify-compliance-unit
   :failed-compliance-unit

   ;;
   ;; API, I think this is the main public part
   ;;
   ;; Model Meta Protocols (because we can't hack the MOP)
   :transact))


