(in-package :cl-user)

(defpackage :cl-braces.sourcecode
  (:nicknames :sourcecode)
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:export
   #:open-source-input
   #:close-source-input
   #:source-input
   #:string-input
   #:source-input-stream
   #:open-input
   #:input-designator
   #:call-with-input
   #:with-input))

(defpackage :cl-braces.sourcecode.location
  (:nicknames :sourcecode.location :location)
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:export
   #:source-location
   #:offset
   #:line
   #:line++
   #:column
   #:column++
   #:make-source-location))

(defpackage :cl-braces.sourcecode.span
  (:nicknames :sourcecode.span :span)
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:export
   #:source-span
   #:make-span
   #:from
   #:to
   #:for
   #:span-map
   #:make-span-map
   #:fetch-span))
