(in-package :cl-user)

(defpackage :cl-braces.runtime.value
  (:nicknames :runtime.value)
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:export
   #:<value>
   #:make-nil
   #:nilp
   #:make-bool
   #:truep
   #:falsep
   #:boolp
   #:make-int
   #:intp
   #:int-value
   #:<closure>
   #:make-closure
   #:closurep
   #:closure-up-values
   #:closure-arity
   #:closure-registers-used
   #:closure-function-label
   #:box
   #:unbox
   #:<arity>
   #:arity-exactly
   #:arity-at-least
   #:arity-kind
   #:arity-value))
