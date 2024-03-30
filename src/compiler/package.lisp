(in-package :cl-user)

(defpackage :cl-braces.compiler
  (:nicknames :compiler)
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:export
   #:compile
   #:make-workspace)
  (:shadow :compile))

(defpackage :cl-braces.compiler.symbols
  (:nicknames :symbols)
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:export
   #:id
   #:name
   #:location
   #:package-name*
   #:exportedp
   #:scope
   #:denotation
   #:symbol-table
   #:make-symbol-table
   #:denotes-variable-p
   #:denotes-function-p
   #:denotes-type-p
   #:place-holder-p
   #:add-symbol
   #:scope-t

   #:find-by-id
   #:find-by-name
   #:filter-by-denotation
   #:denotes-any
   #:closest-scope))

(defpackage :cl-braces.compiler.examples
  (:nicknames :compiler.examples)
  (:use :cl :cl-braces.support))
