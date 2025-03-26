(defpackage #:cl-rm.utils
  (:documentation "Utility Functions for the codebase")
  (:use #:common-lisp #:serapeum #:ironclad)
  (:export
   ;; Generic Utility Functions
   :symbol-to-keyword
   :symbol-to-bytes
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
