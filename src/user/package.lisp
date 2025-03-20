(defpackage #:cl-rm.user
  (:documentation "User code for the system")
  (:shadow :@ :take :delete)
  (:use #:cl-rm.utils #:cl-rm
        #:common-lisp #:serapeum #:ironclad))
