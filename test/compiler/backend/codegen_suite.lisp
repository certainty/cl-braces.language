(in-package :tests.backend.codegen)

(define-test codegen-smoke-test ()
  (let ((chunk (helper:compile-fixture-module "valid_module")))
    (snapshots:assert-snapshot-equals "valid_module.snapshot" (bytecode:chunk-code chunk))))
