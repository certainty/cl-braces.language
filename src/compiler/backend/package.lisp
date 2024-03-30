(in-package :cl-user)

(defpackage :cl-braces.compiler.backend.codegen
  (:nicknames :compiler.backend.codegen :codegen)
  (:use :cl :cl-braces.support)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:import-from :cl-braces.bytecode :addr :reg :register :address)
  (:export
   #:make-generator
   #:bytecode-generator
   #:generate
   #:finalize-chunk
   #:generate))
