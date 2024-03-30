(in-package :cl-user)

(defpackage :cl-braces.support
  (:nicknames :support)
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:export
   #:todo!
   #:unreachable!
   #:pry
   #:returning
   #:domap
   #:define-enum
   #:debug
   #:debug-print
   #:list-of
   #:non-empty-list-of
   #:to-plist
   #:copy-instance
   #:id-generator
   #:sequential-id-generator
   #:make-id-generator
   #:has-id-mixin
   #:id-of
   #:next-id))
