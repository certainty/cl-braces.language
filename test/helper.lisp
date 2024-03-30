(in-package :cl-braces.tests.helper)

(defun compile-fixture-module (module-name)
  (let* ((test-root (asdf:system-source-directory "cl-braces.language/tests"))
         (ws (compiler:make-workspace module-name (merge-pathnames (format nil "test/fixtures/~a" module-name) test-root) :fail-fast-p t)))
    (compiler:compile ws)))
