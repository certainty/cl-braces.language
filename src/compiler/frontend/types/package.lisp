(in-package :cl-user)

(defpackage :cl-braces.compiler.frontend.types
  (:nicknames :frontend.types :types)
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:shadow :type-of)
  (:export
   :check
   :make-type-checker
   :make-type-map
   :fetch-type
   :type-map
   :reference-type-p
   :value-tpe-p
   :type-id
   :type-name))
