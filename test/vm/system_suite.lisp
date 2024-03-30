(in-package :cl-braces.tests.system)

(defun compile-and-run (module-name)
  (let ((chunk (helper:compile-fixture-module module-name)))
    (machine:run vm chunk)))

(define-test system-test ()
  (assert-no-signal
   'error
   (compile-and-run "simple")))
