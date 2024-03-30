(in-package :cl-user)

(defpackage :cl-braces.vm.machine
  (:nicknames :vm.machine :machine)
  (:use :cl :cl-braces.support)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->)
  (:import-from :cl-braces.bytecode)
  (:export
   #:run
   #:make-machine
   #:execute))
